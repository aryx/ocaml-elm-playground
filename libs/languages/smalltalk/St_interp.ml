(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_interp.mli *)

module M = St_memory
module B = St_bytecode
module C = St_class

type oop = M.oop
type host = {
  transcript : string -> unit;
  milliseconds : unit -> int;
  inspect : oop -> unit;
  mouse : unit -> int * int * int;
}
type process_state = Runnable | Suspended of string | Finished of oop | Terminated
type process = { id : int; mutable top : oop; mutable state : process_state }

type vm = {
  m : M.t;
  mutable host : host;
  prims : primitive option array;
  mutable extra_roots : unit -> oop list;
  mutable processes : process list; (* the ones alive: the collector's roots *)
  mutable next_id : int;
  (* the registers, cached from the active context *)
  mutable active : oop;
  mutable slots : oop array; (* the active context's fields *)
  mutable home : oop;
  mutable temps : oop array; (* the home's fields *)
  mutable meth : oop;
  mutable lits : oop array; (* the method's fields: the header, then literal i at i + 1 *)
  mutable code : Bytes.t;
  mutable receiver : oop;
  mutable ip : int;
  mutable sp : int; (* the index in slots of the top of the stack *)
  mutable stop : string option;
  mutable finished : oop option;
  (* the method cache *)
  cache_cls : int array;
  cache_sel : int array;
  cache_meth : int array;
  mutable hits : int;
  mutable misses : int;
  mutable count : int;
}

and primitive = vm -> int -> bool

exception Fatal of string

let c_sender = 0
let c_ip = 1
let c_sp = 2
let c_method = 3
let c_receiver = 5
let c_home = 5
let c_temps = 6

let cache_size = 1024

let create (m : M.t) (host : host) : vm =
  {
    m;
    host;
    prims = Array.make 512 None;
    extra_roots = (fun () -> []);
    processes = [];
    next_id = 1;
    active = M.nil;
    slots = [||];
    home = M.nil;
    temps = [||];
    meth = M.nil;
    lits = [||];
    code = Bytes.empty;
    receiver = M.nil;
    ip = 0;
    sp = 0;
    stop = None;
    finished = None;
    cache_cls = Array.make cache_size (-1);
    cache_sel = Array.make cache_size (-1);
    cache_meth = Array.make cache_size 0;
    hits = 0;
    misses = 0;
    count = 0;
  }

let memory (vm : vm) : M.t = vm.m
let host (vm : vm) : host = vm.host
let set_host (vm : vm) (h : host) : unit = vm.host <- h
let primitives (vm : vm) = vm.prims
let set_extra_roots (vm : vm) (f : unit -> oop list) : unit = vm.extra_roots <- f

let flush_cache (vm : vm) : unit =
  Array.fill vm.cache_cls 0 cache_size (-1);
  Array.fill vm.cache_sel 0 cache_size (-1)

let cache_stats (vm : vm) : int * int = (vm.hits, vm.misses)
let bytecodes_run (vm : vm) : int = vm.count

(*****************************************************************************)
(* The registers *)
(*****************************************************************************)

let is_block_context (vm : vm) (ctx : oop) : bool = M.class_of vm.m ctx = (M.known vm.m).block_context
let context_home (vm : vm) (ctx : oop) : oop = if is_block_context vm ctx then M.fetch vm.m ctx c_home else ctx
let context_method (vm : vm) (ctx : oop) : oop = M.fetch vm.m (context_home vm ctx) c_method

let load (vm : vm) (ctx : oop) : unit =
  let slots = M.fields vm.m ctx in
  vm.active <- ctx;
  vm.slots <- slots;
  let home = if is_block_context vm ctx then slots.(c_home) else ctx in
  vm.home <- home;
  vm.temps <- M.fields vm.m home;
  vm.meth <- vm.temps.(c_method);
  vm.lits <- M.fields vm.m vm.meth;
  vm.code <- B.bytecodes vm.m vm.meth;
  vm.receiver <- vm.temps.(c_receiver);
  vm.ip <- M.int_of slots.(c_ip);
  vm.sp <- M.int_of slots.(c_sp)

let save (vm : vm) : unit =
  if vm.active <> M.nil then begin
    vm.slots.(c_ip) <- M.of_int vm.ip;
    vm.slots.(c_sp) <- M.of_int vm.sp
  end

let active_context (vm : vm) : oop = vm.active
let home_context (vm : vm) : oop = vm.home
let ip (vm : vm) : int = vm.ip

let activate_context (vm : vm) (ctx : oop) : unit =
  save vm;
  load vm ctx

(*****************************************************************************)
(* The stack *)
(*****************************************************************************)

let push (vm : vm) (v : oop) : unit =
  vm.sp <- vm.sp + 1;
  vm.slots.(vm.sp) <- v

let stack (vm : vm) (i : int) : oop = vm.slots.(vm.sp - i)
let pop (vm : vm) (n : int) : unit = vm.sp <- vm.sp - n

let pop_top (vm : vm) : oop =
  let v = vm.slots.(vm.sp) in
  vm.sp <- vm.sp - 1;
  v

let bool (vm : vm) (b : bool) : oop = if b then (M.known vm.m).true_ else (M.known vm.m).false_
let request_suspend (vm : vm) (label : string) : unit = vm.stop <- Some label

(*****************************************************************************)
(* Sends *)
(*****************************************************************************)

let lookup (vm : vm) (cls : oop) (sel : oop) : oop option =
  let h = ((cls lxor (sel lsl 3)) lsr 1) land (cache_size - 1) in
  if vm.cache_cls.(h) = cls && vm.cache_sel.(h) = sel then begin
    vm.hits <- vm.hits + 1;
    Some vm.cache_meth.(h)
  end
  else begin
    vm.misses <- vm.misses + 1;
    match C.lookup vm.m cls sel with
    | Some meth ->
        vm.cache_cls.(h) <- cls;
        vm.cache_sel.(h) <- sel;
        vm.cache_meth.(h) <- meth;
        Some meth
    | None -> None
  end

(* a new MethodContext for [meth], its receiver and arguments taken off
 * the stack, and made active *)
let activate_method (vm : vm) (meth : oop) (nargs : int) : unit =
  let h = B.header vm.m meth in
  if h.num_args <> nargs then raise (Fatal "wrong number of arguments");
  let a = Array.make (c_temps + h.frame_size) M.nil in
  a.(c_sender) <- vm.active;
  a.(c_ip) <- M.of_int 0;
  a.(c_sp) <- M.of_int (c_temps + h.num_temps - 1);
  a.(c_method) <- meth;
  a.(c_receiver) <- vm.slots.(vm.sp - nargs);
  for i = 0 to nargs - 1 do
    a.(c_temps + i) <- vm.slots.(vm.sp - nargs + 1 + i)
  done;
  let ctx = M.alloc vm.m ~cls:(M.known vm.m).method_context (M.Pointers a) in
  vm.sp <- vm.sp - nargs - 1;
  save vm;
  load vm ctx

let rec execute (vm : vm) (meth : oop) (nargs : int) : unit =
  let h = B.header vm.m meth in
  let ok = h.primitive <> 0 && match vm.prims.(h.primitive) with Some p -> p vm nargs | None -> false in
  if not ok then activate_method vm meth nargs

and send_to_class (vm : vm) (cls : oop) (sel : oop) (nargs : int) : unit =
  match lookup vm cls sel with
  | Some meth -> execute vm meth nargs
  | None ->
      (* a Message with the selector and the arguments, sent to the
       * receiver with #doesNotUnderstand: *)
      let args = Array.init nargs (fun i -> vm.slots.(vm.sp - nargs + 1 + i)) in
      let msg = M.alloc vm.m ~cls:(M.known vm.m).message (M.Pointers [| sel; M.new_array vm.m args |]) in
      pop vm nargs;
      push vm msg;
      let dnu = M.symbol vm.m "doesNotUnderstand:" in
      if sel = dnu then raise (Fatal ("recursive doesNotUnderstand: in " ^ C.name vm.m cls));
      send_to_class vm cls dnu 1

let send (vm : vm) (sel : oop) (nargs : int) : unit = send_to_class vm (M.class_of vm.m (stack vm nargs)) sel nargs

let super_send (vm : vm) (sel : oop) (nargs : int) : unit =
  send_to_class vm (C.superclass vm.m (B.method_class vm.m vm.meth)) sel nargs

(*****************************************************************************)
(* Returns *)
(*****************************************************************************)

let dead (vm : vm) (ctx : oop) : bool = M.fetch vm.m ctx c_ip = M.nil

let kill (vm : vm) (ctx : oop) : unit =
  M.store vm.m ctx c_sender M.nil;
  M.store vm.m ctx c_ip M.nil

let cannot_return (vm : vm) (v : oop) : unit =
  push vm vm.active;
  push vm v;
  send vm (M.symbol vm.m "cannotReturn:") 1

(* from the home's method: to the home's sender *)
let return_from_method (vm : vm) (v : oop) : unit =
  let home = vm.home in
  let target = M.fetch vm.m home c_sender in
  if dead vm home then cannot_return vm v
  else if target = M.nil then begin
    (* the bottom of the process *)
    kill vm home;
    if vm.active <> home then kill vm vm.active;
    vm.finished <- Some v
  end
  else if dead vm target then cannot_return vm v
  else begin
    kill vm home;
    if vm.active <> home then kill vm vm.active;
    load vm target;
    push vm v
  end

(* from a block, to its caller *)
let return_from_block (vm : vm) (v : oop) : unit =
  let caller = vm.slots.(c_sender) in
  if caller = M.nil || dead vm caller then cannot_return vm v
  else begin
    kill vm vm.active;
    load vm caller;
    push vm v
  end

(*****************************************************************************)
(* The bytecodes *)
(*****************************************************************************)

let special_arity = [| 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 2; 0; 0; 1; 0; 1; 0; 1; 0; 1; 1; 0; 1; 0; 0 |]

let floor_div a b = if (a < 0) <> (b < 0) && a mod b <> 0 then (a / b) - 1 else a / b
let floor_mod a b = a - (b * floor_div a b)

(* the arithmetic special selectors on two SmallIntegers, done by the
 * bytecode (Blue Book: "primitive" in the interpreter's own loop) *)
let arith (vm : vm) (i : int) : bool =
  let a = stack vm 1 and b = stack vm 0 in
  if not (M.is_int a && M.is_int b) then false
  else
    let x = M.int_of a and y = M.int_of b in
    let int r = if M.fits r then begin pop vm 2; push vm (M.of_int r); true end else false in
    let boolean r = pop vm 2; push vm (bool vm r); true in
    match i with
    | 0 -> int (x + y)
    | 1 -> int (x - y)
    | 2 -> boolean (x < y)
    | 3 -> boolean (x > y)
    | 4 -> boolean (x <= y)
    | 5 -> boolean (x >= y)
    | 6 -> boolean (x = y)
    | 7 -> boolean (x <> y)
    | 8 ->
        (* claude: the product checked in floats, exact below 2^53, so
         * that 32-bit ints on the web cannot wrap unseen *)
        let p = float_of_int x *. float_of_int y in
        if Float.abs p <= 1073741823. then int (x * y) else false
    | 9 -> if y <> 0 && x mod y = 0 then int (x / y) else false
    | 10 -> if y <> 0 then int (floor_mod x y) else false
    | 13 -> if y <> 0 then int (floor_div x y) else false
    | 14 -> int (x land y)
    | 15 -> int (x lor y)
    | _ -> false

let jump_if (vm : vm) (cond : bool) (off : int) : unit =
  let v = pop_top vm in
  let k = M.known vm.m in
  if v = (if cond then k.true_ else k.false_) then vm.ip <- vm.ip + off
  else if v = (if cond then k.false_ else k.true_) then ()
  else begin
    push vm v;
    send vm (M.symbol vm.m "mustBeBoolean") 0
  end

let next_byte (vm : vm) : int =
  let b = Char.code (Bytes.unsafe_get vm.code vm.ip) in
  vm.ip <- vm.ip + 1;
  b

let step (vm : vm) : unit =
  let m = vm.m in
  let b = next_byte vm in
  if b < 16 then push vm (M.fields m vm.receiver).(b)
  else if b < 32 then push vm vm.temps.(c_temps + b - 16)
  else if b < 64 then push vm vm.lits.(1 + b - 32)
  else if b < 96 then push vm (M.fetch m vm.lits.(1 + b - 64) 1)
  else if b < 104 then (M.fields m vm.receiver).(b - 96) <- pop_top vm
  else if b < 112 then vm.temps.(c_temps + b - 104) <- pop_top vm
  else if b < 120 then begin
    let k = M.known m in
    push vm
      (match b with
      | 112 -> vm.receiver
      | 113 -> k.true_
      | 114 -> k.false_
      | 115 -> M.nil
      | _ -> M.of_int (b - 117))
  end
  else if b >= 208 then begin
    let nargs = (b - 208) / 16 in
    send vm vm.lits.(1 + (b land 15)) nargs
  end
  else if b >= 176 then begin
    let i = b - 176 in
    let k = M.known m in
    if i < 16 && arith vm i then ()
    else if i = 22 then begin
      let r = stack vm 0 = stack vm 1 in
      pop vm 2;
      push vm (bool vm r)
    end
    else if i = 23 then begin
      let c = M.class_of m (stack vm 0) in
      pop vm 1;
      push vm c
    end
    else send vm k.special_selectors.(i) special_arity.(i)
  end
  else
    match b with
    | 120 -> return_from_method vm vm.receiver
    | 121 -> return_from_method vm (M.known m).true_
    | 122 -> return_from_method vm (M.known m).false_
    | 123 -> return_from_method vm M.nil
    | 124 -> return_from_method vm (pop_top vm)
    | 125 -> return_from_block vm (pop_top vm)
    | 128 | 129 | 130 -> (
        let e = next_byte vm in
        let i = e land 63 in
        match (b, e lsr 6) with
        | 128, 0 -> push vm (M.fields m vm.receiver).(i)
        | 128, 1 -> push vm vm.temps.(c_temps + i)
        | 128, 2 -> push vm vm.lits.(1 + i)
        | 128, _ -> push vm (M.fetch m vm.lits.(1 + i) 1)
        | _, kind ->
            let v = stack vm 0 in
            if b = 130 then pop vm 1;
            (match kind with
            | 0 -> (M.fields m vm.receiver).(i) <- v
            | 1 -> vm.temps.(c_temps + i) <- v
            | 3 -> M.store m vm.lits.(1 + i) 1 v
            | _ -> raise (Fatal "store into a literal")))
    | 131 ->
        let e = next_byte vm in
        send vm vm.lits.(1 + (e land 31)) (e lsr 5)
    | 132 ->
        let nargs = next_byte vm in
        let i = next_byte vm in
        send vm vm.lits.(1 + i) nargs
    | 133 ->
        let e = next_byte vm in
        super_send vm vm.lits.(1 + (e land 31)) (e lsr 5)
    | 134 ->
        let nargs = next_byte vm in
        let i = next_byte vm in
        super_send vm vm.lits.(1 + i) nargs
    | 135 -> pop vm 1
    | 136 -> push vm (stack vm 0)
    | 137 -> push vm vm.active
    | _ when b >= 144 && b <= 151 -> vm.ip <- vm.ip + (b - 143)
    | _ when b >= 152 && b <= 159 -> jump_if vm false (b - 151)
    | _ when b >= 160 && b <= 167 ->
        let e = next_byte vm in
        vm.ip <- vm.ip + (((b - 164) * 256) + e)
    | _ when b >= 168 && b <= 171 ->
        let e = next_byte vm in
        jump_if vm true (((b - 168) * 256) + e)
    | _ when b >= 172 && b <= 175 ->
        let e = next_byte vm in
        jump_if vm false (((b - 172) * 256) + e)
    | _ -> raise (Fatal (Printf.sprintf "unknown bytecode %d" b))

(*****************************************************************************)
(* Processes *)
(*****************************************************************************)

let collect (vm : vm) : unit =
  save vm;
  let roots = (vm.active :: List.map (fun p -> p.top) vm.processes) @ vm.extra_roots () in
  ignore (M.gc vm.m ~roots);
  flush_cache vm

let new_process (vm : vm) (ctx : oop) : process =
  let p = { id = vm.next_id; top = ctx; state = Runnable } in
  vm.next_id <- vm.next_id + 1;
  vm.processes <- p :: vm.processes;
  p

let spawn_method (vm : vm) (meth : oop) (receiver : oop) : process =
  let h = B.header vm.m meth in
  let a = Array.make (c_temps + h.frame_size) M.nil in
  a.(c_ip) <- M.of_int 0;
  a.(c_sp) <- M.of_int (c_temps + h.num_temps - 1);
  a.(c_method) <- meth;
  a.(c_receiver) <- receiver;
  new_process vm (M.alloc vm.m ~cls:(M.known vm.m).method_context (M.Pointers a))

(* a process sending one message: its bottom context runs a little
 * method made for it, "push the receiver and the arguments, send,
 * return the answer" -- so that the send goes through everything a
 * send does: primitives, doesNotUnderstand: *)
let spawn (vm : vm) (receiver : oop) (selector : string) (args : oop list) : process =
  let n = List.length args in
  let code = Bytes.create (n + 4) in
  for i = 0 to n do
    Bytes.set code i (Char.chr (16 + i))
  done;
  Bytes.set code (n + 1) (Char.chr 131);
  Bytes.set code (n + 2) (Char.chr (n lsl 5));
  Bytes.set code (n + 3) (Char.chr 124);
  let header = { B.primitive = 0; num_args = n + 1; num_temps = n + 1; frame_size = (2 * n) + 4 } in
  let sel = M.symbol vm.m selector in
  let meth =
    B.new_method vm.m ~header ~literals:[| sel |] ~bytecodes:code ~selector:(M.symbol vm.m "send") ~cls:M.nil
      ~source:"" ~pcmap:[ (n + 1, 0, 0) ]
      ~temp_names:(List.init (n + 1) (fun i -> if i = 0 then "receiver" else "arg" ^ string_of_int i))
  in
  let p = spawn_method vm meth M.nil in
  let a = M.fields vm.m p.top in
  List.iteri (fun i v -> a.(c_temps + i) <- v) (receiver :: args);
  p

let forget (vm : vm) (p : process) : unit = vm.processes <- List.filter (fun q -> q != p) vm.processes

let run ?stop_when (vm : vm) (p : process) ~(budget : int) : unit =
  match p.state with
  | Suspended _ | Finished _ | Terminated -> ()
  | Runnable ->
      vm.stop <- None;
      vm.finished <- None;
      load vm p.top;
      let n = ref 0 in
      (try
         while !n < budget && vm.stop = None && vm.finished = None do
           match stop_when with
           | Some f when !n > 0 && f vm -> vm.stop <- Some "Step"
           | _ ->
               step vm;
               incr n;
               if !n land 1023 = 0 && M.allocated vm.m > 100_000 + (M.live vm.m / 2) then collect vm
         done
       with
      | Fatal msg -> vm.stop <- Some ("Virtual machine: " ^ msg)
      | Invalid_argument msg -> vm.stop <- Some ("Virtual machine: " ^ msg));
      vm.count <- vm.count + !n;
      (match vm.finished with
      | Some v ->
          p.state <- Finished v;
          p.top <- M.nil;
          forget vm p
      | None -> (
          save vm;
          p.top <- vm.active;
          match vm.stop with Some label -> p.state <- Suspended label | None -> ()));
      vm.active <- M.nil

let resume (p : process) : unit = match p.state with Suspended _ -> p.state <- Runnable | _ -> ()
let suspend (p : process) (label : string) : unit = match p.state with Runnable -> p.state <- Suspended label | _ -> ()
let terminate (vm : vm) (p : process) : unit =
  p.state <- Terminated;
  forget vm p

let finish (vm : vm) (p : process) ~(budget : int) : (oop, string) result =
  run vm p ~budget;
  let r =
    match p.state with
    | Finished v -> Ok v
    | Suspended label -> Error label
    | Runnable -> Error "Too long: stopped"
    | Terminated -> Error "Terminated"
  in
  forget vm p;
  r

let call (vm : vm) ?(budget = 20_000_000) (receiver : oop) (selector : string) (args : oop list) : (oop, string) result =
  finish vm (spawn vm receiver selector args) ~budget

let print_string (vm : vm) (o : oop) : string =
  match call vm ~budget:5_000_000 o "printString" [] with
  | Ok s when M.class_of vm.m s = (M.known vm.m).string -> M.string_of vm.m s
  | Ok _ -> "<printString not a String>"
  | Error e -> "<printString failed: " ^ e ^ ">"

let evaluate (vm : vm) ?(budget = 20_000_000) ?(receiver = M.nil) (text : string) : (oop, string) result =
  match St_compile.compile_doit vm.m ~receiver_class:(M.class_of vm.m receiver) text with
  | meth -> finish vm (spawn_method vm meth receiver) ~budget
  | exception St_compile.Error (_, msg) -> Error msg
