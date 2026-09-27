(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_compile.mli *)

open St_ast
module M = St_memory
module B = St_bytecode
module C = St_class

type oop = M.oop

exception Error of int * string

(*****************************************************************************)
(* Code, in pieces *)
(*****************************************************************************)

(* the bytecodes of a piece, and its sends (pc, start, stop): a
 * conditional's branches are compiled first, into pieces of their own,
 * so that the jump over them knows how long it is *)
type code = { buf : Buffer.t; mutable sends : (int * int * int) list }

let new_code () : code = { buf = Buffer.create 64; sends = [] }
let pc (c : code) : int = Buffer.length c.buf
let emit (c : code) (b : int) : unit = Buffer.add_char c.buf (Char.chr b)

let append (dst : code) (src : code) : unit =
  let off = pc dst in
  Buffer.add_buffer dst.buf src.buf;
  dst.sends <- List.map (fun (p, a, b) -> (p + off, a, b)) src.sends @ dst.sends

(*****************************************************************************)
(* The compiler's state *)
(*****************************************************************************)

type st = {
  m : M.t;
  cls : oop;
  declare : bool;
  inst_vars : string list;
  mutable literals : oop list; (* reversed *)
  mutable nlits : int;
  mutable scope : (string * int) list; (* the temporaries in sight, innermost first *)
  mutable args : string list; (* which of them cannot be stored into *)
  mutable ntemps : int;
  mutable names : string list; (* the temporaries', by index, reversed *)
  mutable depth : int;
  mutable max_depth : int;
}

let error ((start, _) : pos) (msg : string) = raise (Error (start, msg))

let push_depth (st : st) (n : int) : unit =
  st.depth <- st.depth + n;
  if st.depth > st.max_depth then st.max_depth <- st.depth

(* the index of a literal, the same one for the same oop (a symbol, an
 * Association, a SmallInteger) *)
let literal (st : st) (o : oop) : int =
  let rec find i = function [] -> None | x :: rest -> if x = o then Some i else find (i - 1) rest in
  match find (st.nlits - 1) st.literals with
  | Some i -> i
  | None ->
      st.literals <- o :: st.literals;
      st.nlits <- st.nlits + 1;
      st.nlits - 1

let new_temp (st : st) (pos : pos) (name : string) : int =
  let i = st.ntemps in
  if i >= 63 then error pos "Too many temporaries";
  st.ntemps <- st.ntemps + 1;
  st.names <- name :: st.names;
  st.scope <- (name, i) :: st.scope;
  i

(*****************************************************************************)
(* Literals *)
(*****************************************************************************)

let rec literal_object (m : M.t) (l : literal) : oop =
  let k = M.known m in
  match l with
  | L_int i -> M.of_int i
  | L_large (neg, bytes) ->
      let b = Bytes.of_string (String.init (List.length bytes) (fun i -> Char.chr (List.nth bytes i))) in
      M.alloc m ~cls:(if neg then k.large_negative else k.large_positive) (M.Bytes b)
  | L_float f -> M.new_float m f
  | L_char c -> k.characters.(Char.code c)
  | L_string s -> M.new_string m s
  | L_symbol s -> M.symbol m s
  | L_array l -> M.new_array m (Array.of_list (List.map (literal_object m) l))
  | L_nil -> M.nil
  | L_true -> k.true_
  | L_false -> k.false_

(*****************************************************************************)
(* Pushes and stores *)
(*****************************************************************************)

(* 128 jjkkkkkk, for an index past the short forms *)
let extended (st : st) (c : code) (op : int) (kind : int) (i : int) (pos : pos) : unit =
  if i > 63 then error pos "Too many literals or variables";
  emit c op;
  emit c ((kind lsl 6) lor i);
  ignore st

let push_literal_index (st : st) (c : code) (i : int) (pos : pos) : unit =
  if i < 32 then emit c (32 + i) else extended st c 128 2 i pos

let push_literal (st : st) (c : code) (l : literal) (pos : pos) : unit =
  (match l with
  | L_int -1 -> emit c 116
  | L_int 0 -> emit c 117
  | L_int 1 -> emit c 118
  | L_int 2 -> emit c 119
  | _ -> push_literal_index st c (literal st (literal_object st.m l)) pos);
  push_depth st 1

type var = Temp of int | Inst of int | Assoc of oop

let resolve (st : st) (name : string) (pos : pos) : var =
  match List.assoc_opt name st.scope with
  | Some i -> Temp i
  | None -> (
      let rec index i = function [] -> None | v :: rest -> if v = name then Some i else index (i + 1) rest in
      (* the last of two same names wins: a subclass's own *)
      match index 0 (List.rev st.inst_vars) with
      | Some i -> Inst (List.length st.inst_vars - 1 - i)
      | None -> (
          match C.class_var st.m st.cls name with
          | Some a -> Assoc a
          | None -> (
              match C.global st.m name with
              | Some a -> Assoc a
              | None ->
                  if st.declare then Assoc (C.declare_global st.m name M.nil)
                  else error pos ("Undeclared variable " ^ name))))

let push_var (st : st) (c : code) (name : string) (pos : pos) : unit =
  (match name with
  | "self" | "super" -> emit c 112
  | "true" -> emit c 113
  | "false" -> emit c 114
  | "nil" -> emit c 115
  | "thisContext" -> emit c 137
  | _ -> (
      match resolve st name pos with
      | Temp i -> if i < 16 then emit c (16 + i) else extended st c 128 1 i pos
      | Inst i -> if i < 16 then emit c i else extended st c 128 0 i pos
      | Assoc a ->
          let i = literal st a in
          if i < 32 then emit c (64 + i) else extended st c 128 3 i pos));
  push_depth st 1

(* store the top into a variable, popping it or not *)
let store_var (st : st) (c : code) (name : string) (pos : pos) ~(pop : bool) : unit =
  if List.mem name [ "self"; "super"; "true"; "false"; "nil"; "thisContext" ] || List.mem name st.args then
    error pos ("Cannot store into " ^ name);
  (match resolve st name pos with
  | Temp i -> if pop && i < 8 then emit c (104 + i) else extended st c (if pop then 130 else 129) 1 i pos
  | Inst i -> if pop && i < 8 then emit c (96 + i) else extended st c (if pop then 130 else 129) 0 i pos
  | Assoc a -> extended st c (if pop then 130 else 129) 3 (literal st a) pos);
  if pop then push_depth st (-1)

(*****************************************************************************)
(* Sends and jumps *)
(*****************************************************************************)

let send (st : st) (c : code) (sel : string) (nargs : int) ~(super : bool) (pos : pos) : unit =
  let start, stop = pos in
  c.sends <- (pc c, start, stop) :: c.sends;
  let rec find i =
    if i >= Array.length B.special_selectors then None else if B.special_selectors.(i) = sel then Some i else find (i + 1)
  in
  let special = if super then None else find 0 in
  (match special with
  | Some i -> emit c (176 + i)
  | None ->
      let i = literal st (M.symbol st.m sel) in
      if super then
        if nargs < 8 && i < 32 then begin
          emit c 133;
          emit c ((nargs lsl 5) lor i)
        end
        else begin
          emit c 134;
          emit c nargs;
          emit c i
        end
      else if nargs <= 2 && i < 16 then emit c (208 + (nargs * 16) + i)
      else if nargs < 8 && i < 32 then begin
        emit c 131;
        emit c ((nargs lsl 5) lor i)
      end
      else begin
        if i > 255 then error pos "Too many literals";
        emit c 132;
        emit c nargs;
        emit c i
      end);
  push_depth st (-nargs)

let jump_size (n : int) : int = if n >= 1 && n <= 8 then 1 else 2

let jump (c : code) (n : int) : unit =
  if n >= 1 && n <= 8 then emit c (143 + n)
  else begin
    emit c (164 + (n asr 8));
    emit c (n land 255)
  end

let jump_on_false (c : code) (n : int) : unit =
  if n >= 1 && n <= 8 then emit c (151 + n)
  else begin
    emit c (172 + (n lsr 8));
    emit c (n land 255)
  end

let jump_on_true (c : code) (n : int) : unit =
  emit c (168 + (n lsr 8));
  emit c (n land 255)

(* backwards, always the long form, 2 bytes *)
let jump_back (c : code) (target : int) : unit =
  let off = target - (pc c + 2) in
  emit c (164 + (off asr 8));
  emit c (off land 255)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

let is_block0 (x : expr) : bool = match x.e with Block ([], _, _) -> true | _ -> false

let rec gen (st : st) (c : code) (x : expr) : unit =
  match x.e with
  | Lit l -> push_literal st c l x.pos
  | Var v -> push_var st c v x.pos
  | Assign (v, y) ->
      gen st c y;
      store_var st c v x.pos ~pop:false
  | Send (r, sel, args) -> gen_send st c r sel args x.pos
  | Cascade (r, msgs) ->
      let super = match r.e with Var "super" -> true | _ -> false in
      gen st c r;
      let n = List.length msgs in
      List.iteri
        (fun i (sel, args, pos) ->
          if i < n - 1 then begin
            emit c 136;
            push_depth st 1
          end;
          List.iter (gen st c) args;
          send st c sel (List.length args) ~super pos;
          if i < n - 1 then begin
            emit c 135;
            push_depth st (-1)
          end)
        msgs
  | Block (args, temps, body) -> gen_block st c args temps body x.pos

and gen_send (st : st) (c : code) (r : expr) (sel : string) (args : expr list) (pos : pos) : unit =
  let d = st.depth in
  match (sel, args) with
  | ("ifTrue:ifFalse:" | "ifFalse:ifTrue:"), [ a; b ] when is_block0 a && is_block0 b ->
      let yes, no = if sel = "ifTrue:ifFalse:" then (a, b) else (b, a) in
      gen st c r;
      push_depth st (-1);
      let c1 = new_code () in
      inline_block st c1 yes;
      st.depth <- d;
      let c2 = new_code () in
      inline_block st c2 no;
      jump_on_false c (pc c1 + jump_size (pc c2));
      append c c1;
      jump c (pc c2);
      append c c2
  | ("ifTrue:" | "ifFalse:"), [ a ] when is_block0 a ->
      gen st c r;
      push_depth st (-1);
      let c1 = new_code () in
      inline_block st c1 a;
      if sel = "ifTrue:" then jump_on_false c (pc c1 + 1) else jump_on_true c (pc c1 + 1);
      append c c1;
      jump c 1;
      emit c 115
  | ("and:" | "or:"), [ a ] when is_block0 a ->
      gen st c r;
      push_depth st (-1);
      let c1 = new_code () in
      inline_block st c1 a;
      if sel = "and:" then jump_on_false c (pc c1 + 1) else jump_on_true c (pc c1 + 1);
      append c c1;
      jump c 1;
      emit c (if sel = "and:" then 114 else 113)
  | ("whileTrue:" | "whileFalse:"), [ a ] when is_block0 r && is_block0 a ->
      let start = pc c in
      inline_block st c r;
      push_depth st (-1);
      let c1 = new_code () in
      inline_block st c1 a;
      emit c1 135;
      push_depth st (-1);
      if sel = "whileTrue:" then jump_on_false c (pc c1 + 2) else jump_on_true c (pc c1 + 2);
      append c c1;
      jump_back c start;
      emit c 115;
      push_depth st 1
  | ("whileTrue" | "whileFalse"), [] when is_block0 r ->
      let start = pc c in
      inline_block st c r;
      push_depth st (-1);
      if sel = "whileTrue" then jump_on_false c 2 else jump_on_true c 2;
      jump_back c start;
      emit c 115;
      push_depth st 1
  | "to:do:", [ stop; ({ e = Block ([ _ ], _, _); _ } as b) ] -> gen_to_do st c r stop 1 b pos
  | "to:by:do:", [ stop; { e = Lit (L_int step); _ }; ({ e = Block ([ _ ], _, _); _ } as b) ] when step <> 0 ->
      gen_to_do st c r stop step b pos
  | _ ->
      let super = match r.e with Var "super" -> true | _ -> false in
      gen st c r;
      List.iter (gen st c) args;
      send st c sel (List.length args) ~super pos

(* "1 to: n do: [:i | ...]" as a loop over a temporary, no block made:
 * Squeak's compiler does it; the Blue Book's sent to:do: to the number.
 * The limit is computed once, into a hidden temporary. *)
and gen_to_do (st : st) (c : code) (start : expr) (stop : expr) (step : int) (b : expr) (pos : pos) : unit =
  match b.e with
  | Block ([ var ], temps, body) ->
      let saved_scope = st.scope and saved_args = st.args in
      let i = new_temp st b.pos var in
      let limit = new_temp st b.pos " limit" in
      let store_temp t =
        if t < 8 then emit c (104 + t) else extended st c 130 1 t pos;
        push_depth st (-1)
      in
      let push_temp (cc : code) t =
        if t < 16 then emit cc (16 + t) else extended st cc 128 1 t pos;
        push_depth st 1
      in
      gen st c start;
      store_temp i;
      gen st c stop;
      store_temp limit;
      st.args <- var :: st.args;
      List.iter (fun t -> ignore (new_temp st b.pos t)) temps;
      let top = pc c in
      push_temp c i;
      push_temp c limit;
      send st c (if step > 0 then "<=" else ">=") 1 ~super:false pos;
      push_depth st (-1);
      let c1 = new_code () in
      statements st c1 body ~value:false;
      push_temp c1 i;
      push_literal st c1 (L_int step) pos;
      send st c1 "+" 1 ~super:false pos;
      if i < 8 then emit c1 (104 + i) else extended st c1 130 1 i pos;
      push_depth st (-1);
      jump_on_false c (pc c1 + 2);
      append c c1;
      jump_back c top;
      emit c 115;
      push_depth st 1;
      st.scope <- saved_scope;
      st.args <- saved_args
  | _ -> assert false

(* a literal block's statements, in place: its temporaries are the
 * method's, its value the last statement's *)
and inline_block (st : st) (c : code) (b : expr) : unit =
  match b.e with
  | Block (_, temps, body) ->
      let saved = st.scope in
      List.iter (fun t -> ignore (new_temp st b.pos t)) temps;
      statements st c body ~value:true;
      st.scope <- saved
  | _ -> gen st c b

(* push thisContext, push the arguments' count, send blockCopy:, jump
 * over the body; the body pops its arguments into their temporaries *)
and gen_block (st : st) (c : code) (args : string list) (temps : string list) (body : stmt list) (pos : pos) : unit =
  emit c 137;
  push_depth st 1;
  push_literal st c (L_int (List.length args)) pos;
  emit c 200;
  push_depth st (-1);
  let saved_scope = st.scope and saved_args = st.args and outer = st.depth in
  let indexes = List.map (fun a -> new_temp st pos a) args in
  st.args <- args @ st.args;
  List.iter (fun t -> ignore (new_temp st pos t)) temps;
  (* the block's own stack: its arguments, pushed by value: *)
  st.depth <- 0;
  push_depth st (List.length args);
  let b = new_code () in
  List.iter
    (fun i ->
      if i < 8 then emit b (104 + i) else extended st b 130 1 i pos;
      push_depth st (-1))
    (List.rev indexes);
  statements st b body ~value:true;
  emit b 125;
  st.scope <- saved_scope;
  st.args <- saved_args;
  st.depth <- outer;
  let n = pc b in
  if n > 1023 then error pos "Block too long";
  emit c (164 + (n asr 8));
  emit c (n land 255);
  append c b

(* the statements; with [~value], the last one's value is left on the
 * stack (nil if there are none) *)
and statements (st : st) (c : code) (body : stmt list) ~(value : bool) : unit =
  let rec go = function
    | [] -> if value then begin emit c 115; push_depth st 1 end
    | [ Return (x, _) ] ->
        gen_return st c x;
        (* what follows a return is never run, but the branches that
         * contain it must still look as if they pushed their value *)
        if value then push_depth st 1
    | Return (x, _) :: _ -> gen_return st c x
    | [ Expr x ] when value -> gen st c x
    | Expr x :: rest ->
        effect st c x;
        if rest = [] && value then begin
          emit c 115;
          push_depth st 1
        end
        else go rest
  in
  go body

and gen_return (st : st) (c : code) (x : expr) : unit =
  match x.e with
  | Var "self" -> emit c 120
  | Var "true" -> emit c 121
  | Var "false" -> emit c 122
  | Var "nil" -> emit c 123
  | _ ->
      gen st c x;
      emit c 124;
      push_depth st (-1)

(* an expression whose value is not wanted *)
and effect (st : st) (c : code) (x : expr) : unit =
  match x.e with
  | Assign (v, y) ->
      gen st c y;
      store_var st c v x.pos ~pop:true
  | _ ->
      gen st c x;
      emit c 135;
      push_depth st (-1)

(*****************************************************************************)
(* Methods *)
(*****************************************************************************)

let compile (m : M.t) ~(cls : oop) ~(source : string) ?(declare = false) (meth : method_) : oop =
  let st =
    {
      m;
      cls;
      declare;
      inst_vars = C.inst_var_names m cls;
      literals = [];
      nlits = 0;
      scope = [];
      args = meth.args;
      ntemps = 0;
      names = [];
      depth = 0;
      max_depth = 0;
    }
  in
  List.iter (fun a -> ignore (new_temp st (0, 0) a)) meth.args;
  List.iter (fun t -> ignore (new_temp st (0, 0) t)) meth.temps;
  let c = new_code () in
  (* the statements, then return self unless the last one returned *)
  let returns = match List.rev meth.body with Return _ :: _ -> true | _ -> false in
  statements st c meth.body ~value:false;
  if not returns then emit c 120;
  let header =
    {
      B.primitive = Option.value meth.primitive ~default:0;
      num_args = List.length meth.args;
      num_temps = st.ntemps;
      frame_size = min 255 (st.ntemps + st.max_depth + 1);
    }
  in
  B.new_method m ~header
    ~literals:(Array.of_list (List.rev st.literals))
    ~bytecodes:(Buffer.to_bytes c.buf) ~selector:(M.symbol m meth.selector) ~cls ~source ~pcmap:(List.rev c.sends)
    ~temp_names:(List.rev st.names)

let compile_and_install (m : M.t) ~(cls : oop) ~(category : string) ?(declare = false) (source : string) : string =
  let meth =
    try St_parse.parse_method source with St_parse.Error (pos, msg) -> raise (Error (pos, msg))
  in
  let cm = compile m ~cls ~source ~declare meth in
  C.install m cls (M.symbol m meth.selector) cm ~category;
  meth.selector

let rec recompile (m : M.t) (cls : oop) : (string * string) list =
  let errors =
    C.selectors m cls
    |> List.filter_map (fun sel ->
           match C.local_method m cls (M.symbol m sel) with
           | None -> None
           | Some meth -> (
               let source = B.source m meth in
               let category = Option.value (C.category_of m cls sel) ~default:"as yet unclassified" in
               try
                 ignore (compile_and_install m ~cls ~category source);
                 None
               with Error (_, msg) -> Some (C.name m cls ^ ">>" ^ sel, msg)))
  in
  errors @ List.concat_map (recompile m) (C.subclasses m cls)

let compile_doit (m : M.t) ~(receiver_class : oop) (source : string) : oop =
  let meth = try St_parse.parse_doit source with St_parse.Error (pos, msg) -> raise (Error (pos, msg)) in
  (* the last statement's value is the answer: a Workspace's print it *)
  let body =
    match List.rev meth.body with
    | Expr x :: rest -> List.rev (Return (x, x.pos) :: rest)
    | _ -> meth.body
  in
  compile m ~cls:receiver_class ~source ~declare:true { meth with body }
