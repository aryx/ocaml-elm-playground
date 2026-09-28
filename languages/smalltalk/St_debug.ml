(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_debug.mli *)

module M = St_memory
module B = St_bytecode
module C = St_class
module I = St_interp

type oop = M.oop
type frame = { ctx : oop; label : string; source : string; highlight : (int * int) option }

(*****************************************************************************)
(* Reading the stack *)
(*****************************************************************************)

let dead (vm : I.vm) (ctx : oop) : bool = M.fetch (I.memory vm) ctx I.c_ip = M.nil

let label (vm : I.vm) (ctx : oop) : string =
  let m = I.memory vm in
  let home = I.context_home vm ctx in
  let meth = M.fetch m home I.c_method in
  let rcls = M.class_of m (M.fetch m home I.c_receiver) in
  let mcls = B.method_class m meth in
  let sel = M.string_of m (B.selector m meth) in
  let base =
    if mcls = rcls || mcls = M.nil then C.name m rcls ^ ">>" ^ sel else C.name m rcls ^ "(" ^ C.name m mcls ^ ")>>" ^ sel
  in
  if I.is_block_context vm ctx then "[] in " ^ base else base

(* the send in progress: at the top of a stepped process, the one about
 * to be made; elsewhere, the last one made *)
let highlight (vm : I.vm) (ctx : oop) ~(next : bool) : (int * int) option =
  let m = I.memory vm in
  let ip = M.fetch m ctx I.c_ip in
  if ip = M.nil then None
  else
    let ip = M.int_of ip in
    let map = B.pcmap m (I.context_method vm ctx) in
    let pick =
      if next then List.find_opt (fun (pc, _, _) -> pc >= ip) map
      else List.fold_left (fun acc ((pc, _, _) as e) -> if pc < ip then Some e else acc) None map
    in
    match pick with Some (_, a, b) when b > a -> Some (a, b) | _ -> None

let frames ?(stepping = false) (vm : I.vm) (p : I.process) : frame list =
  let m = I.memory vm in
  let rec go ctx top acc =
    if ctx = M.nil || List.length acc > 200 then List.rev acc
    else
      let f =
        {
          ctx;
          label = label vm ctx;
          source = B.source m (I.context_method vm ctx);
          highlight = highlight vm ctx ~next:(top && stepping);
        }
      in
      go (M.fetch m ctx I.c_sender) false (f :: acc)
  in
  go p.top true []

let variables (vm : I.vm) (ctx : oop) : (string * oop) list =
  let m = I.memory vm in
  let home = I.context_home vm ctx in
  let names = B.temp_names m (M.fetch m home I.c_method) in
  let a = M.fields m home in
  ("self", a.(I.c_receiver))
  :: List.mapi (fun i n -> (n, if I.c_temps + i < Array.length a then a.(I.c_temps + i) else M.nil)) names
  |> List.filter (fun (n, _) -> n <> "" && n.[0] <> ' ')

let fields (vm : I.vm) (o : oop) : (string * oop) list =
  let m = I.memory vm in
  if M.is_int o then []
  else
    let k = M.known m in
    let cls = M.class_of m o in
    let names = C.inst_var_names m cls in
    let limit = 200 in
    match M.body m o with
    | M.Pointers a ->
        let n = List.length names in
        List.mapi (fun i name -> (name, a.(i))) (List.filteri (fun i _ -> i < Array.length a) names)
        @ List.init (min limit (max 0 (Array.length a - n))) (fun i -> (string_of_int (i + 1), a.(n + i)))
    | M.Bytes b ->
        let string = cls = k.string || cls = k.symbol in
        List.init (min limit (Bytes.length b)) (fun i ->
            let c = Char.code (Bytes.get b i) in
            (string_of_int (i + 1), if string then k.characters.(c) else M.of_int c))
    | M.Method (a, _) ->
        Array.sub a 0 (Array.length a - B.trailer_size)
        |> Array.to_list
        |> List.mapi (fun i v -> ((if i = 0 then "header" else "literal " ^ string_of_int i), v))
    | M.Float _ | M.Free -> []

(*****************************************************************************)
(* Moving on *)
(*****************************************************************************)

let budget = 20_000_000

let run_until (vm : I.vm) (p : I.process) (cond : I.vm -> bool) : unit =
  I.resume p;
  I.run vm p ~budget ~stop_when:cond;
  (* claude: a step that never came back, an endless loop: stopped
   * anyway, rather than left running under the debugger's feet *)
  match p.state with I.Runnable -> I.suspend p "Step: still running" | _ -> ()

(* stop in [ctx] before its next send once the current one is done, or
 * as soon as ctx has returned. The current one: in a context below the
 * top, the send in progress, done when ctx is active again at its ip;
 * at the top, the next send ahead. *)
let over_condition (vm : I.vm) (p : I.process) (ctx : oop) : I.vm -> bool =
  let m = I.memory vm in
  let meth = I.context_method vm ctx in
  let code = B.bytecodes m meth in
  let pcs = List.map (fun (pc, _, _) -> pc) (B.pcmap m meth) in
  let ip0 = match M.fetch m ctx I.c_ip with ip when M.is_int ip -> M.int_of ip | _ -> 0 in
  let after =
    if p.top <> ctx then Some ip0
    else Option.map (fun s1 -> s1 + B.length_at code s1) (List.find_opt (fun pc -> pc >= ip0) pcs)
  in
  match after with
  | None -> fun vm -> dead vm ctx
  | Some after ->
      let passed = ref false in
      fun vm ->
        if dead vm ctx then true
        else if I.active_context vm = ctx then begin
          let ip = I.ip vm in
          if ip = after then passed := true;
          !passed && List.mem ip pcs
        end
        else false

let step (vm : I.vm) (p : I.process) (ctx : oop) : unit = if not (dead vm ctx) then run_until vm p (over_condition vm p ctx)

let step_into (vm : I.vm) (p : I.process) (ctx : oop) : unit =
  if not (dead vm ctx) then begin
    let over = over_condition vm p ctx in
    let m = I.memory vm in
    run_until vm p (fun vm ->
        let a = I.active_context vm in
        (a <> ctx && M.fetch m a I.c_sender = ctx) || over vm)
  end

let restart (vm : I.vm) (p : I.process) (ctx : oop) : bool =
  let m = I.memory vm in
  let ctx = I.context_home vm ctx in
  if dead vm ctx then false
  else begin
    let old = M.fetch m ctx I.c_method in
    let cls = B.method_class m old in
    let meth = if cls = M.nil then old else Option.value (C.local_method m cls (B.selector m old)) ~default:old in
    let h = B.header m meth in
    let size = I.c_temps + h.frame_size in
    if M.size m ctx < size then begin
      (* a bigger context, and every reference to the old one follows *)
      let a = Array.make size M.nil in
      Array.blit (M.fields m ctx) 0 a 0 (min size (M.size m ctx));
      let bigger = M.alloc m ~cls:(M.known m).method_context (M.Pointers a) in
      M.become m ctx bigger
    end;
    let a = M.fields m ctx in
    a.(I.c_method) <- meth;
    a.(I.c_ip) <- M.of_int 0;
    a.(I.c_sp) <- M.of_int (I.c_temps + h.num_temps - 1);
    for i = I.c_temps + h.num_args to Array.length a - 1 do
      a.(i) <- M.nil
    done;
    p.top <- ctx;
    I.flush_cache vm;
    true
  end

let proceed (p : I.process) : unit = I.resume p

let not_understood (vm : I.vm) (p : I.process) : (oop * string * oop) option =
  let m = I.memory vm in
  let top = p.top in
  if top = M.nil || I.is_block_context vm top then None
  else
    let meth = M.fetch m top I.c_method in
    if M.string_of m (B.selector m meth) <> "doesNotUnderstand:" then None
    else
      let msg = M.fetch m top I.c_temps in
      let receiver = M.fetch m top I.c_receiver in
      let sender = M.fetch m top I.c_sender in
      if M.is_int msg || M.class_of m msg <> (M.known m).message then None
      else Some (M.class_of m receiver, M.string_of m (M.fetch m msg 0), sender)

let template (selector : string) : string =
  let n = St_ast.arity selector in
  let pattern =
    if n = 0 then selector
    else if not (String.contains selector ':') then selector ^ " anObject"
    else
      String.split_on_char ':' selector |> List.filter (( <> ) "")
      |> List.mapi (fun i k -> k ^ ": arg" ^ string_of_int (i + 1))
      |> String.concat " "
  in
  pattern ^ "\n\t\"A method for " ^ selector ^ ", to write here.\"\n\t^self"
