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

(* a temporary's name where it is declared: its block's place in the
 * text (the method's is (0, 0)), and the name *)
type key = pos * string

(* a context's temporaries as the compiler lays them out: the method's,
 * or -- with closures -- a block's, each activation having its own *)
type frame = {
  block : pos; (* whose *)
  outer : frame option;
  mutable nslots : int; (* arguments, copied values, temporaries *)
  mutable vector : int; (* the slot of its temp vector, -1 without *)
  mutable nremote : int; (* the temporaries in the vector *)
  copied : (copy * int) list; (* what a block copies when it is made, and the slot of each *)
}

(* an outer temporary's value, or an outer frame's temp vector *)
and copy = Value of key | Vector of pos

(* where a temporary is: a slot of its frame, or an index in its
 * frame's temp vector *)
type place = Direct of int | Remote of int
type temp = { key : key; frame : frame; place : place }

(* what the first pass of the closure compiler learns for the second
 * (St_compile.mli) *)
type facts = {
  learning : bool; (* the first pass *)
  captured : (key, unit) Hashtbl.t; (* used by a block inside the one that declares it *)
  changed : (key, unit) Hashtbl.t; (* assigned when a block may already hold it *)
  free : (pos, key list) Hashtbl.t; (* a block's outer temporaries, in the order met *)
  uninline : (pos, unit) Hashtbl.t; (* the to:do: loops whose variable is captured *)
  mutable again : bool; (* a loop was added to uninline: learn again *)
}

type st = {
  m : M.t;
  cls : oop;
  declare : bool;
  inst_vars : string list;
  facts : facts option; (* closures, or the Blue Book's blocks *)
  mutable literals : oop list; (* reversed *)
  mutable nlits : int;
  mutable scope : (string * temp) list; (* the temporaries in sight, innermost first *)
  mutable args : string list; (* which of them cannot be stored into *)
  mutable frame : frame; (* the one being compiled *)
  mutable names : string list; (* the method's temporaries', by index, reversed *)
  mutable depth : int;
  mutable max_depth : int;
  mutable need : int; (* the biggest frame a closure asks for: its slots and its stack *)
  mutable loops : int; (* the inlined loops around *)
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

let new_slot (st : st) (pos : pos) (name : string) : int =
  let f = st.frame in
  let i = f.nslots in
  if i >= 63 then error pos "Too many temporaries";
  f.nslots <- i + 1;
  (match f.outer with None -> st.names <- name :: st.names | Some _ -> ());
  i

(* a temporary declared by the block at [pos], in the frame being
 * compiled: in a slot, or in the frame's temp vector (made when its
 * first temporary is) if a block captures it and it changes *)
let new_temp (st : st) (pos : pos) (name : string) : unit =
  let f = st.frame in
  let key = (pos, name) in
  let remote =
    match st.facts with
    | Some facts -> (not facts.learning) && Hashtbl.mem facts.captured key && Hashtbl.mem facts.changed key
    | None -> false
  in
  let place =
    if remote then begin
      if f.vector < 0 then f.vector <- new_slot st pos " vector";
      if f.nremote >= 127 then error pos "Too many temporaries";
      f.nremote <- f.nremote + 1;
      Remote (f.nremote - 1)
    end
    else Direct (new_slot st pos name)
  in
  st.scope <- (name, { key; frame = f; place }) :: st.scope

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

(* a temporary in a temp vector: its index there, and the slot of the
 * vector *)
type var = Temp of int | Remote_temp of int * int | Inst of int | Assoc of oop

(* the first pass: [v] is used in frame [f], inside its own; every
 * block from f out to v's own has to copy it *)
let capture (facts : facts) (f : frame) (v : temp) : unit =
  Hashtbl.replace facts.captured v.key ();
  let rec up (g : frame) =
    if g != v.frame then begin
      let free = Option.value (Hashtbl.find_opt facts.free g.block) ~default:[] in
      if not (List.mem v.key free) then Hashtbl.replace facts.free g.block (free @ [ v.key ]);
      Option.iter up g.outer
    end
  in
  up f

let copied_slot (st : st) (c : copy) : int = Option.value (List.assoc_opt c st.frame.copied) ~default:0

(* a temporary as the frame being compiled reaches it: its own, or
 * through what the block copied *)
let temp_var (st : st) (v : temp) : var =
  let f = st.frame in
  if v.frame == f then match v.place with Direct i -> Temp i | Remote j -> Remote_temp (j, f.vector)
  else begin
    (match st.facts with Some facts when facts.learning -> capture facts f v | _ -> ());
    match v.place with
    | Direct _ -> Temp (copied_slot st (Value v.key))
    | Remote j -> Remote_temp (j, copied_slot st (Vector v.frame.block))
  end

let resolve (st : st) (name : string) (pos : pos) : var =
  match List.assoc_opt name st.scope with
  | Some v -> temp_var st v
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
      | Remote_temp (j, vector) ->
          emit c 140;
          emit c j;
          emit c vector
      | Inst i -> if i < 16 then emit c i else extended st c 128 0 i pos
      | Assoc a ->
          let i = literal st a in
          if i < 32 then emit c (64 + i) else extended st c 128 3 i pos));
  push_depth st 1

(* store the top into a variable, popping it or not. The first pass
 * notes a temporary that changes when a block may already hold its
 * value: assigned from a block inside its own, or after a block that
 * uses it, or in a loop *)
let store (st : st) (c : code) (name : string) (pos : pos) ~(pop : bool) : unit =
  (match (st.facts, List.assoc_opt name st.scope) with
  | Some facts, Some v when facts.learning ->
      if v.frame != st.frame || st.loops > 0 || Hashtbl.mem facts.captured v.key then Hashtbl.replace facts.changed v.key ()
  | _ -> ());
  (match resolve st name pos with
  | Temp i -> if pop && i < 8 then emit c (104 + i) else extended st c (if pop then 130 else 129) 1 i pos
  | Remote_temp (j, vector) ->
      emit c (if pop then 142 else 141);
      emit c j;
      emit c vector
  | Inst i -> if pop && i < 8 then emit c (96 + i) else extended st c (if pop then 130 else 129) 0 i pos
  | Assoc a -> extended st c (if pop then 130 else 129) 3 (literal st a) pos);
  if pop then push_depth st (-1)

(* what the program may store into: not an argument *)
let store_var (st : st) (c : code) (name : string) (pos : pos) ~(pop : bool) : unit =
  if List.mem name [ "self"; "super"; "true"; "false"; "nil"; "thisContext" ] || List.mem name st.args then
    error pos ("Cannot store into " ^ name);
  store st c name pos ~pop

let push_temp (st : st) (c : code) (i : int) (pos : pos) : unit =
  if i < 16 then emit c (16 + i) else extended st c 128 1 i pos;
  push_depth st 1

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

(* a to:do: whose block captures the loop's variable is sent, not
 * inlined: each turn then has its own *)
let loop_inlined (st : st) (b : expr) : bool = match st.facts with Some f -> not (Hashtbl.mem f.uninline b.pos) | None -> true

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
      st.loops <- st.loops + 1;
      inline_block st c r;
      push_depth st (-1);
      let c1 = new_code () in
      inline_block st c1 a;
      emit c1 135;
      push_depth st (-1);
      if sel = "whileTrue:" then jump_on_false c (pc c1 + 2) else jump_on_true c (pc c1 + 2);
      append c c1;
      st.loops <- st.loops - 1;
      jump_back c start;
      emit c 115;
      push_depth st 1
  | ("whileTrue" | "whileFalse"), [] when is_block0 r ->
      let start = pc c in
      st.loops <- st.loops + 1;
      inline_block st c r;
      st.loops <- st.loops - 1;
      push_depth st (-1);
      if sel = "whileTrue" then jump_on_false c 2 else jump_on_true c 2;
      jump_back c start;
      emit c 115;
      push_depth st 1
  | "to:do:", [ stop; ({ e = Block ([ _ ], _, _); _ } as b) ] when loop_inlined st b -> gen_to_do st c r stop 1 b pos
  | "to:by:do:", [ stop; { e = Lit (L_int step); _ }; ({ e = Block ([ _ ], _, _); _ } as b) ]
    when step <> 0 && loop_inlined st b ->
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
      let limit = " limit" in
      new_temp st b.pos var;
      new_temp st b.pos limit;
      gen st c start;
      store st c var pos ~pop:true;
      gen st c stop;
      store st c limit pos ~pop:true;
      st.args <- var :: st.args;
      List.iter (new_temp st b.pos) temps;
      let top = pc c in
      push_var st c var pos;
      push_var st c limit pos;
      send st c (if step > 0 then "<=" else ">=") 1 ~super:false pos;
      push_depth st (-1);
      st.loops <- st.loops + 1;
      let c1 = new_code () in
      statements st c1 body ~value:false;
      push_var st c1 var pos;
      push_literal st c1 (L_int step) pos;
      send st c1 "+" 1 ~super:false pos;
      store st c1 var pos ~pop:true;
      st.loops <- st.loops - 1;
      jump_on_false c (pc c1 + 2);
      append c c1;
      jump_back c top;
      emit c 115;
      push_depth st 1;
      st.scope <- saved_scope;
      st.args <- saved_args;
      (* claude: the loop's variable held by a block: learn again, the
       * loop sent this time *)
      (match st.facts with
      | Some f when f.learning && Hashtbl.mem f.captured (b.pos, var) ->
          Hashtbl.replace f.uninline b.pos ();
          f.again <- true
      | _ -> ())
  | _ -> assert false

(* a literal block's statements, in place: its temporaries are the
 * method's, its value the last statement's *)
and inline_block (st : st) (c : code) (b : expr) : unit =
  match b.e with
  | Block (_, temps, body) ->
      let saved = st.scope in
      List.iter (new_temp st b.pos) temps;
      statements st c body ~value:true;
      st.scope <- saved
  | _ -> gen st c b

and gen_block (st : st) (c : code) (args : string list) (temps : string list) (body : stmt list) (pos : pos) : unit =
  match st.facts with None -> gen_block_context st c args temps body pos | Some facts -> gen_closure st facts c args temps body pos

(* the Blue Book's: push thisContext, push the arguments' count, send
 * blockCopy:, jump over the body; the body pops its arguments into
 * their temporaries *)
and gen_block_context (st : st) (c : code) (args : string list) (temps : string list) (body : stmt list) (pos : pos) : unit =
  emit c 137;
  push_depth st 1;
  push_literal st c (L_int (List.length args)) pos;
  emit c 200;
  push_depth st (-1);
  let saved_scope = st.scope and saved_args = st.args and outer = st.depth in
  List.iter (new_temp st pos) args;
  List.iter (new_temp st pos) temps;
  (* the block's own stack: its arguments, pushed by value: *)
  st.depth <- 0;
  push_depth st (List.length args);
  let b = new_code () in
  List.iter (fun a -> store st b a pos ~pop:true) (List.rev args);
  st.args <- args @ st.args;
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

(* a closure (Squeak, 2008): push what the block copies, then "push
 * closure" (143) with their count, the arguments' and the body's
 * length. The body is a frame of its own: the arguments, the copied
 * values, then its temporaries, which it pushes itself (nil, or its
 * temp vector) before its statements *)
and gen_closure (st : st) (facts : facts) (c : code) (args : string list) (temps : string list) (body : stmt list) (pos : pos) : unit =
  let outer = st.frame and nargs = List.length args and depth = st.depth in
  if nargs > 15 then error pos "Too many arguments";
  let temp_of key = snd (List.find (fun (_, (v : temp)) -> v.key = key) st.scope) in
  let copies =
    List.fold_left
      (fun acc key ->
        let cp = match (temp_of key).place with Remote _ -> Vector (temp_of key).frame.block | Direct _ -> Value key in
        if List.mem cp acc then acc else acc @ [ cp ])
      []
      (if facts.learning then [] else Option.value (Hashtbl.find_opt facts.free pos) ~default:[])
  in
  let ncopied = List.length copies in
  if ncopied > 15 then error pos "Too many variables used by a block";
  List.iter
    (fun cp ->
      let slot =
        match cp with
        | Value key -> ( match temp_var st (temp_of key) with Temp i -> i | _ -> assert false)
        | Vector block -> if outer.block = block then outer.vector else copied_slot st cp
      in
      push_temp st c slot pos)
    copies;
  let f = { block = pos; outer = Some outer; nslots = 0; vector = -1; nremote = 0; copied = List.mapi (fun i cp -> (cp, nargs + i)) copies } in
  let saved_scope = st.scope and saved_args = st.args and max_depth = st.max_depth and loops = st.loops in
  st.frame <- f;
  List.iter (new_temp st pos) args;
  f.nslots <- nargs + ncopied;
  st.args <- args @ st.args;
  List.iter (new_temp st pos) temps;
  st.depth <- 0;
  st.max_depth <- 0;
  st.loops <- 0;
  let b = new_code () in
  statements st b body ~value:true;
  emit b 125;
  (* its temporaries, known now that the body is compiled *)
  let p = new_code () in
  for slot = nargs + ncopied to f.nslots - 1 do
    if slot = f.vector then begin
      emit p 138;
      emit p f.nremote
    end
    else emit p 115
  done;
  append p b;
  st.need <- max st.need (f.nslots + st.max_depth);
  st.frame <- outer;
  st.scope <- saved_scope;
  st.args <- saved_args;
  st.max_depth <- max_depth;
  st.loops <- loops;
  st.depth <- depth;
  push_depth st 1;
  let n = pc p in
  if n > 65535 then error pos "Block too long";
  emit c 143;
  emit c ((ncopied lsl 4) lor nargs);
  emit c (n lsr 8);
  emit c (n land 255);
  append c p

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
        for_effect st c x;
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

(* an expression whose value is not wanted (claude: for_effect, effect
 * being a keyword since OCaml 5.3) *)
and for_effect (st : st) (c : code) (x : expr) : unit =
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

(* a method's bytecodes, and what the compiler knows at the end *)
let generate (m : M.t) ~(cls : oop) ~(declare : bool) (facts : facts option) (meth : method_) : st * code =
  let frame = { block = (0, 0); outer = None; nslots = 0; vector = -1; nremote = 0; copied = [] } in
  let st =
    {
      m;
      cls;
      declare;
      inst_vars = C.inst_var_names m cls;
      facts;
      literals = [];
      nlits = 0;
      scope = [];
      args = meth.args;
      frame;
      names = [];
      depth = 0;
      max_depth = 0;
      need = 0;
      loops = 0;
    }
  in
  List.iter (new_temp st (0, 0)) meth.args;
  List.iter (new_temp st (0, 0)) meth.temps;
  let c = new_code () in
  (* the statements, then return self unless the last one returned *)
  let returns = match List.rev meth.body with Return _ :: _ -> true | _ -> false in
  statements st c meth.body ~value:false;
  if not returns then emit c 120;
  if frame.vector < 0 then (st, c)
  else begin
    (* the method's temp vector, made before anything else *)
    let p = new_code () in
    emit p 138;
    emit p frame.nremote;
    if frame.vector < 8 then emit p (104 + frame.vector) else extended st p 130 1 frame.vector (0, 0);
    st.max_depth <- max st.max_depth 1;
    append p c;
    (st, p)
  end

let compile (m : M.t) ~(cls : oop) ~(source : string) ?(declare = false) (meth : method_) : oop =
  let facts =
    if (M.known m).block_closure = M.nil then None
    else begin
      (* the first pass, again as long as it finds a loop to send *)
      let uninline = Hashtbl.create 4 in
      let rec learn () =
        let f =
          { learning = true; captured = Hashtbl.create 16; changed = Hashtbl.create 16; free = Hashtbl.create 16; uninline; again = false }
        in
        ignore (generate m ~cls ~declare (Some f) meth);
        if f.again then learn () else f
      in
      Some { (learn ()) with learning = false }
    end
  in
  let st, c = generate m ~cls ~declare facts meth in
  let ntemps = st.frame.nslots in
  let header =
    {
      B.primitive = Option.value meth.primitive ~default:0;
      num_args = List.length meth.args;
      num_temps = ntemps;
      frame_size = min 255 (max (ntemps + st.max_depth) st.need + 1);
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
