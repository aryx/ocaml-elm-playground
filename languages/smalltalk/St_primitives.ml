(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_primitives.mli *)

module M = St_memory
module C = St_class
module I = St_interp

type oop = M.oop

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* the receiver and the arguments, as a send left them on the stack *)
let rcvr vm n = I.stack vm n
let arg vm n i = I.stack vm (n - 1 - i)

(* success: the receiver and the arguments replaced by the answer *)
let answer vm n (v : oop) : bool =
  I.pop vm (n + 1);
  I.push vm v;
  true

let is_bytes (m : M.t) (o : oop) : bool = match M.body m o with M.Bytes _ -> true | _ -> false
let is_string (m : M.t) (o : oop) : bool = let k = M.known m in (not (M.is_int o)) && (M.class_of m o = k.string || M.class_of m o = k.symbol)

(* the named fields of an object, which its indexed ones come after *)
let named (m : M.t) (o : oop) : int = fst (C.format m (M.class_of m o))

let is_indexable (m : M.t) (o : oop) : bool =
  (not (M.is_int o)) && match snd (C.format m (M.class_of m o)) with C.Indexable | C.Byte_indexable -> true | _ -> false

(*****************************************************************************)
(* Numbers *)
(*****************************************************************************)

let floor_div a b = if (a < 0) <> (b < 0) && a mod b <> 0 then (a / b) - 1 else a / b

let int_op (f : int -> int -> [ `Int of int | `Bool of bool | `Fail ]) : I.primitive =
 fun vm n ->
  let a = rcvr vm n and b = arg vm n 0 in
  if M.is_int a && M.is_int b then
    match f (M.int_of a) (M.int_of b) with
    | `Int r when M.fits r -> answer vm n (M.of_int r)
    | `Bool r -> answer vm n (I.bool vm r)
    | `Int _ | `Fail -> false
  else false

let small_ints =
  [
    (1, fun a b -> `Int (a + b));
    (2, fun a b -> `Int (a - b));
    (3, fun a b -> `Bool (a < b));
    (4, fun a b -> `Bool (a > b));
    (5, fun a b -> `Bool (a <= b));
    (6, fun a b -> `Bool (a >= b));
    (7, fun a b -> `Bool (a = b));
    (8, fun a b -> `Bool (a <> b));
    (9, fun a b -> if Float.abs (float_of_int a *. float_of_int b) <= 1073741823. then `Int (a * b) else `Fail);
    (10, fun a b -> if b <> 0 && a mod b = 0 then `Int (a / b) else `Fail);
    (11, fun a b -> if b <> 0 then `Int (a - (b * floor_div a b)) else `Fail);
    (12, fun a b -> if b <> 0 then `Int (floor_div a b) else `Fail);
    (13, fun a b -> if b <> 0 then `Int (a / b) else `Fail);
    (14, fun a b -> `Int (a land b));
    (15, fun a b -> `Int (a lor b));
    (16, fun a b -> `Int (a lxor b));
    ( 17,
      fun a b ->
        if b >= 0 then if b < 31 && (a lsl b) asr b = a && M.fits (a lsl b) then `Int (a lsl b) else `Fail
        else `Int (a asr min 31 (-b)) );
  ]

(* a Float, or a SmallInteger taken as one *)
let float_arg (m : M.t) (o : oop) : float option =
  if M.is_int o then Some (float_of_int (M.int_of o))
  else match M.body m o with M.Float f when M.class_of m o = (M.known m).float -> Some f | _ -> None

let float_op (f : float -> float -> [ `Float of float | `Bool of bool | `Fail ]) : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  match (M.body m (rcvr vm n), float_arg m (arg vm n 0)) with
  | M.Float a, Some b -> (
      match f a b with
      | `Float r -> answer vm n (M.new_float m r)
      | `Bool r -> answer vm n (I.bool vm r)
      | `Fail -> false)
  | _ -> false

let floats =
  [
    (41, fun a b -> `Float (a +. b));
    (42, fun a b -> `Float (a -. b));
    (43, fun a b -> `Bool (a < b));
    (44, fun a b -> `Bool (a > b));
    (45, fun a b -> `Bool (a <= b));
    (46, fun a b -> `Bool (a >= b));
    (47, fun a b -> `Bool (a = b));
    (48, fun a b -> `Bool (a <> b));
    (49, fun a b -> `Float (a *. b));
    (50, fun a b -> if b = 0. then `Fail else `Float (a /. b));
  ]

let float_fun (f : float -> float) : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  match M.body m (rcvr vm n) with M.Float a -> answer vm n (M.new_float m (f a)) | _ -> false

(* printed as Smalltalk-80 printed a Float: at least one digit after
 * the point *)
let float_string (f : float) : string =
  if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.1f" f
  else
    let s = Printf.sprintf "%.15g" f in
    let s = if float_of_string s = f then s else Printf.sprintf "%.17g" f in
    if String.contains s '.' || String.contains s 'e' || String.contains s 'n' || String.contains s 'i' then s else s ^ ".0"

(*****************************************************************************)
(* Indexing *)
(*****************************************************************************)

(* an indexed field, from 1, after the named ones *)
let index_of vm (o : oop) (i : oop) : int option =
  let m = I.memory vm in
  if not (M.is_int i && is_indexable m o) then None
  else
    let i = M.int_of i in
    let base = named m o in
    if i >= 1 && base + i <= M.size m o then Some (base + i - 1) else None

let at : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n in
  match index_of vm o (arg vm n 0) with
  | None -> false
  | Some j -> (
      match M.body m o with
      | M.Pointers a -> answer vm n a.(j)
      | M.Bytes b -> answer vm n (M.of_int (Char.code (Bytes.get b j)))
      | _ -> false)

let at_put : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n and v = arg vm n 1 in
  match index_of vm o (arg vm n 0) with
  | None -> false
  | Some j -> (
      match M.body m o with
      | M.Pointers a ->
          a.(j) <- v;
          answer vm n v
      | M.Bytes b when M.is_int v && M.int_of v >= 0 && M.int_of v < 256 ->
          Bytes.set b j (Char.chr (M.int_of v));
          answer vm n v
      | _ -> false)

let size : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n in
  if M.is_int o then false else if is_indexable m o then answer vm n (M.of_int (M.size m o - named m o)) else answer vm n (M.of_int 0)

let string_at : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n in
  match (M.body m o, arg vm n 0) with
  | M.Bytes b, i when M.is_int i && M.int_of i >= 1 && M.int_of i <= Bytes.length b ->
      answer vm n (M.known m).characters.(Char.code (Bytes.get b (M.int_of i - 1)))
  | _ -> false

let char_value (m : M.t) (c : oop) : int option =
  if (not (M.is_int c)) && M.class_of m c = (M.known m).character then
    let v = M.fetch m c 0 in
    if M.is_int v && M.int_of v >= 0 && M.int_of v < 256 then Some (M.int_of v) else None
  else None

let string_at_put : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n and c = arg vm n 1 in
  match (M.body m o, arg vm n 0, char_value m c) with
  | M.Bytes b, i, Some v when M.class_of m o <> (M.known m).symbol && M.is_int i && M.int_of i >= 1 && M.int_of i <= Bytes.length b ->
      Bytes.set b (M.int_of i - 1) (Char.chr v);
      answer vm n c
  | _ -> false

let replace : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n and start = arg vm n 0 and stop = arg vm n 1 and repl = arg vm n 2 and rstart = arg vm n 3 in
  if not (List.for_all M.is_int [ start; stop; rstart ] && is_indexable m o && is_indexable m repl) then false
  else
    let start = M.int_of start and stop = M.int_of stop and rstart = M.int_of rstart in
    let len = stop - start + 1 in
    let bo = named m o and br = named m repl in
    (* claude: only between Arrays and Strings, whose index 1 is their
     * first indexed field: an OrderedCollection's elements start at its
     * firstIndex, which its Smalltalk at: knows and this does not *)
    if bo <> 0 || br <> 0 then false
    else if len < 0 || start < 1 || rstart < 1 || bo + stop > M.size m o || br + rstart + len - 1 > M.size m repl then false
    else
      match (M.body m o, M.body m repl) with
      | M.Pointers a, M.Pointers r ->
          Array.blit r (br + rstart - 1) a (bo + start - 1) len;
          answer vm n o
      | M.Bytes a, M.Bytes r ->
          Bytes.blit r (rstart - 1) a (start - 1) len;
          answer vm n o
      | _ -> false

(*****************************************************************************)
(* Objects and classes *)
(*****************************************************************************)

let instantiate vm (cls : oop) (k : int) : oop option =
  let m = I.memory vm in
  if M.is_int cls || cls = M.nil || not (C.is_meta m (M.class_of m cls)) then None
  else
    let named, kind = C.format m cls in
    match kind with
    | C.Fixed when k = 0 -> Some (M.alloc m ~cls (M.Pointers (Array.make named M.nil)))
    | C.Indexable -> Some (M.alloc m ~cls (M.Pointers (Array.make (named + k) M.nil)))
    | C.Byte_indexable -> Some (M.alloc m ~cls (M.Bytes (Bytes.make k '\000')))
    | C.Float_kind when k = 0 -> Some (M.alloc m ~cls (M.Float 0.))
    | _ -> None

let new_ : I.primitive = fun vm n -> match instantiate vm (rcvr vm n) 0 with Some o -> answer vm n o | None -> false

let new_size : I.primitive =
 fun vm n ->
  let k = arg vm n 0 in
  if M.is_int k && M.int_of k >= 0 then match instantiate vm (rcvr vm n) (M.int_of k) with Some o -> answer vm n o | None -> false
  else false

let inst_var_at : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n and i = arg vm n 0 in
  match M.body m o with
  | M.Pointers a when M.is_int i && M.int_of i >= 1 && M.int_of i <= Array.length a -> answer vm n a.(M.int_of i - 1)
  | _ -> false

let inst_var_at_put : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n and i = arg vm n 0 and v = arg vm n 1 in
  match M.body m o with
  | M.Pointers a when M.is_int i && M.int_of i >= 1 && M.int_of i <= Array.length a ->
      a.(M.int_of i - 1) <- v;
      answer vm n v
  | _ -> false

let shallow_copy : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n in
  if M.is_int o || o = M.nil then answer vm n o
  else
    let b =
      match M.body m o with
      | M.Pointers a -> M.Pointers (Array.copy a)
      | M.Bytes b -> M.Bytes (Bytes.copy b)
      | M.Method (a, b) -> M.Method (Array.copy a, Bytes.copy b)
      | b -> b
    in
    answer vm n (M.alloc m ~cls:(M.class_of m o) b)

(*****************************************************************************)
(* Blocks and perform *)
(*****************************************************************************)

(* BlockContext: 0 caller 1 ip 2 sp 3 argument count 4 initial ip 5 home *)
let block_copy : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let ctx = rcvr vm n and nargs = arg vm n 0 in
  let home = I.context_home vm ctx in
  let size = M.size m home in
  let a = Array.make size M.nil in
  let initial = M.of_int (I.ip vm + 2) in
  a.(1) <- initial;
  a.(2) <- M.of_int (I.c_temps - 1);
  a.(3) <- nargs;
  a.(4) <- initial;
  a.(5) <- home;
  answer vm n (M.alloc m ~cls:(M.known m).block_context (M.Pointers a))

let start_block vm (blk : oop) (args : oop list) : bool =
  let m = I.memory vm in
  if M.is_int blk || M.class_of m blk <> (M.known m).block_context then false
  else
    let a = M.fields m blk in
    if M.int_of a.(3) <> List.length args then false
    else begin
      List.iteri (fun i v -> a.(I.c_temps + i) <- v) args;
      a.(2) <- M.of_int (I.c_temps + List.length args - 1);
      a.(1) <- a.(4);
      I.pop vm (List.length args + 1);
      a.(0) <- I.active_context vm;
      I.activate_context vm blk;
      true
    end

let value : I.primitive = fun vm n -> start_block vm (rcvr vm n) (List.init n (fun i -> arg vm n i))

let value_with_arguments : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let args = arg vm n 0 in
  if M.is_int args || M.class_of m args <> (M.known m).array then false
  else begin
    let l = Array.to_list (M.fields m args) in
    let blk = rcvr vm n in
    (* the Array's elements where value: would have its arguments *)
    I.pop vm 1;
    List.iter (I.push vm) l;
    if start_block vm blk l then true
    else begin
      I.pop vm (List.length l);
      I.push vm args;
      false
    end
  end

let perform : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let sel = arg vm n 0 in
  if M.is_int sel || M.class_of m sel <> (M.known m).symbol || St_ast.arity (M.string_of m sel) <> n - 1 then false
  else begin
    let args = List.init (n - 1) (fun i -> arg vm n (i + 1)) in
    I.pop vm n;
    List.iter (I.push vm) args;
    I.send vm sel (n - 1);
    true
  end

let perform_with_arguments : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let sel = arg vm n 0 and args = arg vm n 1 in
  if M.is_int args || M.class_of m args <> (M.known m).array || M.is_int sel then false
  else
    let l = Array.to_list (M.fields m args) in
    if St_ast.arity (M.string_of m sel) <> List.length l then false
    else begin
      I.pop vm 2;
      List.iter (I.push vm) l;
      I.send vm sel (List.length l);
      true
    end

(*****************************************************************************)
(* Strings, the system *)
(*****************************************************************************)

let string_op (f : string -> string -> [ `Bool of bool ]) : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let a = rcvr vm n and b = arg vm n 0 in
  if is_string m a && is_string m b then match f (M.string_of m a) (M.string_of m b) with `Bool r -> answer vm n (I.bool vm r)
  else false

let define kind : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let sup = rcvr vm n and name = arg vm n 0 and ivs = arg vm n 1 and cvs = arg vm n 2 and cat = arg vm n 4 in
  if not (List.for_all (is_string m) [ name; ivs; cvs; cat ]) then false
  else
    let words o = String.split_on_char ' ' (M.string_of m o) |> List.concat_map (String.split_on_char '\t') |> List.filter (( <> ) "") in
    let cls, changed =
      C.define_class m ~superclass:sup ~name:(M.string_of m name) ~kind ~inst_vars:(words ivs) ~class_vars:(words cvs)
        ~category:(M.string_of m cat)
    in
    if changed then List.iter (fun (what, msg) -> (I.host vm).transcript (what ^ ": " ^ msg ^ "\n")) (St_compile.recompile m cls);
    I.flush_cache vm;
    answer vm n cls

let compile : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let cls = rcvr vm n and src = arg vm n 0 and cat = arg vm n 1 in
  if not (is_string m src && is_string m cat) then false
  else
    match St_compile.compile_and_install m ~cls ~category:(M.string_of m cat) (M.string_of m src) with
    | sel ->
        I.flush_cache vm;
        answer vm n (M.symbol m sel)
    | exception St_compile.Error (_, msg) -> answer vm n (M.new_string m msg)

let as_number : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let s = rcvr vm n in
  if not (is_string m s) then false
  else
    match St_parse.parse_literal (String.trim (M.string_of m s)) with
    | Some ((St_ast.L_int _ | St_ast.L_large _ | St_ast.L_float _) as l) -> answer vm n (St_compile.literal_object m l)
    | _ -> answer vm n M.nil

(*****************************************************************************)
(* Large integers' help *)
(*****************************************************************************)

(* a SmallInteger as a LargePositiveInteger or a LargeNegativeInteger:
 * its magnitude's four bytes, least significant first -- in OCaml,
 * because -2^30's magnitude is not a SmallInteger, so Smalltalk could
 * not compute it without a LargeInteger already *)
let as_large : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n in
  if not (M.is_int o) then false
  else
    let v = M.int_of o in
    let mag = abs v in
    let b = Bytes.init 4 (fun i -> Char.chr ((mag lsr (8 * i)) land 255)) in
    let k = M.known m in
    answer vm n (M.alloc m ~cls:(if v < 0 then k.large_negative else k.large_positive) (M.Bytes b))

(* the leading zero bytes dropped, and a SmallInteger if it fits *)
let normalize : I.primitive =
 fun vm n ->
  let m = I.memory vm in
  let o = rcvr vm n in
  match M.body m o with
  | M.Bytes b ->
      let len = ref (Bytes.length b) in
      while !len > 0 && Bytes.get b (!len - 1) = '\000' do decr len done;
      let neg = M.class_of m o = (M.known m).large_negative in
      let bytes = List.init !len (fun i -> Char.code (Bytes.get b i)) in
      if !len = 0 then answer vm n (M.of_int 0)
      else (
        match St_lexer.small_of_bytes neg bytes with
        | Some v -> answer vm n (M.of_int v)
        | None ->
            if !len = Bytes.length b then answer vm n o
            else answer vm n (M.alloc m ~cls:(M.class_of m o) (M.Bytes (Bytes.sub b 0 !len))))
  | _ -> false

(*****************************************************************************)
(* The table *)
(*****************************************************************************)

let install (vm : I.vm) : unit =
  let t = I.primitives vm in
  let set i p = t.(i) <- Some p in
  List.iter (fun (i, f) -> set i (int_op f)) small_ints;
  set 18 (fun vm n -> let m = I.memory vm in answer vm n (M.alloc m ~cls:(M.known m).point (M.Pointers [| rcvr vm n; arg vm n 0 |])));
  set 40 (fun vm n -> let o = rcvr vm n in if M.is_int o then answer vm n (M.new_float (I.memory vm) (float_of_int (M.int_of o))) else false);
  List.iter (fun (i, f) -> set i (float_op f)) floats;
  set 51 (fun vm n ->
      match M.body (I.memory vm) (rcvr vm n) with
      | M.Float f when Float.is_finite f && M.fits (truncate f) && Float.abs f < 1073741824. -> answer vm n (M.of_int (truncate f))
      | _ -> false);
  set 55 (float_fun Float.sqrt);
  set 56 (float_fun Float.sin);
  set 57 (float_fun Float.atan);
  set 58 (float_fun Float.log);
  set 59 (float_fun Float.exp);
  set 60 at;
  set 61 at_put;
  set 62 size;
  set 63 string_at;
  set 64 string_at_put;
  set 68 (fun vm n ->
      let m = I.memory vm in
      let o = rcvr vm n and i = arg vm n 0 in
      match M.body m o with
      | M.Method (a, _) when M.is_int i && M.int_of i >= 1 && M.int_of i <= Array.length a -> answer vm n a.(M.int_of i - 1)
      | _ -> false);
  set 69 (fun vm n ->
      let m = I.memory vm in
      let o = rcvr vm n and i = arg vm n 0 and v = arg vm n 1 in
      match M.body m o with
      | M.Method (a, _) when M.is_int i && M.int_of i >= 1 && M.int_of i <= Array.length a ->
          a.(M.int_of i - 1) <- v;
          answer vm n v
      | _ -> false);
  set 70 new_;
  set 71 new_size;
  set 72 (fun vm n ->
      let a = rcvr vm n and b = arg vm n 0 in
      if M.is_int a || M.is_int b || a = M.nil || b = M.nil then false
      else begin
        M.become (I.memory vm) a b;
        I.flush_cache vm;
        answer vm n a
      end);
  set 73 inst_var_at;
  set 74 inst_var_at_put;
  set 75 (fun vm n -> let o = rcvr vm n in answer vm n (if M.is_int o then o else M.of_int (o lsr 1)));
  set 80 block_copy;
  set 81 value;
  set 82 value_with_arguments;
  set 83 perform;
  set 84 perform_with_arguments;
  set 105 replace;
  set 110 (fun vm n -> answer vm n (I.bool vm (rcvr vm n = arg vm n 0)));
  set 111 (fun vm n -> answer vm n (M.class_of (I.memory vm) (rcvr vm n)));
  set 120 (fun vm n ->
      let v = arg vm n 0 in
      if M.is_int v && M.int_of v >= 0 && M.int_of v < 256 then answer vm n (M.known (I.memory vm)).characters.(M.int_of v) else false);
  set 122 (fun vm n ->
      let m = I.memory vm in
      let s = rcvr vm n in
      if is_string m s then answer vm n (M.symbol m (M.string_of m s)) else false);
  set 123 (string_op (fun a b -> `Bool (a = b)));
  set 124 (string_op (fun a b -> `Bool (a < b)));
  set 125 (fun vm n ->
      let m = I.memory vm in
      let s = rcvr vm n in
      if is_string m s then answer vm n (M.of_int (Hashtbl.hash (M.string_of m s) land 0x3FFFFFF)) else false);
  set 130 (fun vm n ->
      let m = I.memory vm in
      match M.body m (rcvr vm n) with M.Float f -> answer vm n (M.new_string m (float_string f)) | _ -> false);
  set 140 (fun vm n ->
      let m = I.memory vm in
      let s = arg vm n 0 in
      if is_string m s then begin
        (I.host vm).transcript (M.string_of m s);
        answer vm n (rcvr vm n)
      end
      else false);
  set 141 (fun vm n ->
      let m = I.memory vm in
      let s = arg vm n 0 in
      I.request_suspend vm (if is_string m s then M.string_of m s else "Halt");
      answer vm n M.nil);
  set 142 compile;
  set 143 (define C.Fixed);
  set 151 (define C.Indexable);
  set 152 (define C.Byte_indexable);
  set 144 (fun vm n -> answer vm n (M.of_int ((I.host vm).milliseconds () land 0x3FFFFFFF)));
  set 145 (fun vm n ->
      let m = I.memory vm in
      answer vm n (M.new_array m (Array.of_list (M.instances m (rcvr vm n)))));
  set 146 (fun vm n ->
      let before = M.live (I.memory vm) in
      I.collect vm;
      answer vm n (M.of_int (before - M.live (I.memory vm))));
  set 147 (fun vm n -> let m = I.memory vm in answer vm n (I.bool vm (C.lookup m (rcvr vm n) (arg vm n 0) <> None)));
  set 148 (fun vm n -> let m = I.memory vm in answer vm n (I.bool vm (C.local_method m (rcvr vm n) (arg vm n 0) <> None)));
  set 149 (fun vm n ->
      let m = I.memory vm in
      answer vm n (M.new_array m (Array.of_list (List.map (M.symbol m) (C.selectors m (rcvr vm n))))));
  set 150 as_number;
  set 153 shallow_copy;
  set 154 as_large;
  set 155 normalize;
  set 156 (fun vm n ->
      let m = I.memory vm in
      match M.body m (rcvr vm n) with M.Method _ -> answer vm n (St_bytecode.selector m (rcvr vm n)) | _ -> false);
  set 157 (fun vm n ->
      let m = I.memory vm in
      match M.body m (rcvr vm n) with M.Method _ -> answer vm n (St_bytecode.method_class m (rcvr vm n)) | _ -> false);
  set 90 (fun vm n ->
      let m = I.memory vm in
      let x, y, _ = (I.host vm).mouse () in
      answer vm n (M.alloc m ~cls:(M.known m).point (M.Pointers [| M.of_int x; M.of_int y |])));
  set 91 (fun vm n ->
      let _, _, b = (I.host vm).mouse () in
      answer vm n (M.of_int b));
  set 96 (fun vm n -> if St_bitblt.copy_bits (I.memory vm) (rcvr vm n) then answer vm n (rcvr vm n) else false);
  set 158 (fun vm n ->
      (I.host vm).inspect (rcvr vm n);
      answer vm n (rcvr vm n))
