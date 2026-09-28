(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Pascal_lexer
open Pcode

(* See Pascal_compile.mli *)

type error = { line : int; col : int; message : string }

exception Failed of error

(*****************************************************************************)
(* Types and names *)
(*****************************************************************************)

type ty =
  | Integer
  | Boolean
  | Character
  | Text (* a string constant: only written *)
  | Subrange of ty * int * int
  | Array of int * int * ty (* its index's bounds, its elements *)
  | Record of (string * int * ty) list (* the fields, their offsets *)

let rec size (t : ty) : int =
  match t with
  | Array (lo, hi, elt) -> (hi - lo + 1) * size elt
  | Record fields -> List.fold_left (fun n (_, _, t) -> n + size t) 0 fields
  | Text -> 0
  | Integer | Boolean | Character | Subrange _ -> 1

(* a subrange is its base type, with bounds to check *)
let rec base (t : ty) : ty = match t with Subrange (b, _, _) -> base b | t -> t

let rec type_name (t : ty) : string =
  match t with
  | Integer -> "integer"
  | Boolean -> "boolean"
  | Character -> "char"
  | Text -> "string"
  | Subrange (_, lo, hi) -> Printf.sprintf "%d..%d" lo hi
  | Array (lo, hi, elt) -> Printf.sprintf "array[%d..%d] of %s" lo hi (type_name elt)
  | Record _ -> "record"

let scalar (t : ty) : bool = match base t with Integer | Boolean | Character -> true | _ -> false

type param = { pname : string; pty : ty; by_ref : bool }

type proc = {
  plevel : int; (* the level of its body: one more than where it is declared *)
  label : int;
  params : param list;
  result : ty option; (* a function's *)
  mutable defined : bool; (* its body compiled; false after a forward *)
  at : int * int; (* where its name was declared, for the error when its body never comes *)
}

type entry =
  | Const of ty * int
  | Const_text of string
  | Var of { vty : ty; vlevel : int; offset : int; var_param : bool }
  | Proc of proc
  | Type of ty
  | Standard of string (* write, abs, ...: compiled each its own way *)

(*****************************************************************************)
(* The compiler's state *)
(*****************************************************************************)

type state = {
  toks : token array;
  mutable pos : int;
  mutable last : token; (* the last token read: its line the instructions' *)
  mutable code : instr array;
  mutable lines : int array;
  mutable n : int; (* instructions emitted *)
  mutable labels : int array; (* a label's address, -1 until placed *)
  mutable nlabels : int;
  mutable scopes : (string * entry) list list; (* the innermost first *)
  mutable level : int;
  mutable frame : int; (* the words the current frame takes so far *)
  mutable functions : proc list; (* the functions whose body is being compiled *)
  (* the debugger's information (Pcode.mli): the statements' addresses
     and lines, the procedures compiled, and the one being compiled *)
  mutable marks : (int * int) list;
  mutable procedures : (int * Pcode.procedure) list;
  mutable nprocedures : int;
  mutable current : int;
}

let tok (s : state) : token = s.toks.(s.pos)
let kind (s : state) : kind = (tok s).kind
let advance (s : state) : unit =
  s.last <- tok s;
  if s.pos < Array.length s.toks - 1 then s.pos <- s.pos + 1

let fail (s : state) (message : string) : 'a =
  let t = tok s in
  raise (Failed { line = t.line; col = t.col; message })

let is_symbol (s : state) (sym : string) : bool = kind s = Symbol sym
let is_keyword (s : state) (k : string) : bool = kind s = Keyword k

let expect_symbol (s : state) (sym : string) : unit =
  if is_symbol s sym then advance s else fail s (Printf.sprintf "'%s' expected, not %s" sym (Pascal_lexer.show (kind s)))

let expect_keyword (s : state) (k : string) : unit =
  if is_keyword s k then advance s else fail s (Printf.sprintf "%s expected, not %s" k (Pascal_lexer.show (kind s)))

let name (s : state) : string =
  match kind s with
  | Name n ->
      advance s;
      n
  | k -> fail s ("identifier expected, not " ^ Pascal_lexer.show k)

(* code *)

let emit (s : state) (i : instr) : unit =
  if s.n = Array.length s.code then begin
    let grow a fill = Array.append a (Array.make (Array.length a + 16) fill) in
    s.code <- grow s.code Stp;
    s.lines <- grow s.lines 0
  end;
  s.code.(s.n) <- i;
  s.lines.(s.n) <- s.last.line;
  s.n <- s.n + 1

let new_label (s : state) : int =
  if s.nlabels = Array.length s.labels then s.labels <- Array.append s.labels (Array.make (s.nlabels + 16) (-1));
  s.nlabels <- s.nlabels + 1;
  s.nlabels - 1

let place (s : state) (l : int) : unit = s.labels.(l) <- s.n

(* names *)

let lookup (s : state) (n : string) : entry =
  match List.find_map (List.assoc_opt n) s.scopes with
  | Some e -> e
  | None ->
      (* where the name is, read already or not *)
      let t = if s.last.kind = Name n then s.last else tok s in
      raise (Failed { line = t.line; col = t.col; message = "Unknown identifier " ^ n })

let declare (s : state) (n : string) (e : entry) : unit =
  match s.scopes with
  | scope :: outer ->
      if List.mem_assoc n scope then fail s ("Duplicate identifier " ^ n);
      s.scopes <- ((n, e) :: scope) :: outer
  | [] -> assert false

(* a word (or more) of the current frame, for a variable or a
   compiler's temporary: the for loop's limit, the case's selector *)
let allocate (s : state) (t : ty) : int =
  let o = s.frame in
  s.frame <- s.frame + size t;
  o

(* [at]: where the value checked starts, where the error is shown *)
let check_type ?at (s : state) ~(expected : ty) (got : ty) : unit =
  let ok =
    match (base expected, base got) with
    | (Integer | Boolean | Character), _ -> base expected = base got
    | e, g -> e = g
  in
  if not ok then
    let t = Option.value at ~default:(tok s) in
    raise (Failed { line = t.line; col = t.col; message = Printf.sprintf "Type mismatch: %s expected, not %s" (type_name expected) (type_name got) })

(* a value about to be stored in a subrange: checked at run time *)
let range_check (s : state) (t : ty) : unit = match t with Subrange (_, lo, hi) -> emit s (Chk (lo, hi)) | _ -> ()

(*****************************************************************************)
(* Constants and types *)
(*****************************************************************************)

let constant (s : state) : ty * int =
  let sign = if is_symbol s "-" then (advance s; -1) else (if is_symbol s "+" then advance s; 1) in
  match kind s with
  | Int n ->
      advance s;
      (Integer, sign * n)
  | Char c when sign = 1 ->
      advance s;
      (Character, Char.code c)
  | Name n -> (
      match lookup s n with
      | Const (t, v) ->
          advance s;
          if sign = -1 && base t <> Integer then fail s "Integer constant expected";
          (t, sign * v)
      | _ -> fail s "Constant expected")
  | k -> fail s ("Constant expected, not " ^ Pascal_lexer.show k)

let rec type_ (s : state) : ty =
  match kind s with
  | Keyword "array" ->
      advance s;
      expect_symbol s "[";
      (* array[1..3, 1..4] of t is array[1..3] of array[1..4] of t *)
      let rec bounds acc =
        let b = index_type s in
        if is_symbol s "," then (advance s; bounds (b :: acc)) else List.rev (b :: acc)
      in
      let bs = bounds [] in
      expect_symbol s "]";
      expect_keyword s "of";
      let elt = type_ s in
      List.fold_right (fun (lo, hi) t -> Array (lo, hi, t)) bs elt
  | Keyword "record" ->
      advance s;
      let rec fields offset acc =
        if is_keyword s "end" then List.rev acc
        else
          let names = name_list s in
          expect_symbol s ":";
          let t = type_ s in
          let acc, offset =
            List.fold_left
              (fun (acc, offset) n ->
                if List.exists (fun (m, _, _) -> m = n) acc then fail s ("Duplicate field " ^ n);
                ((n, offset, t) :: acc, offset + size t))
              (acc, offset) names
          in
          if is_symbol s ";" then advance s;
          fields offset acc
      in
      let fs = fields 0 [] in
      expect_keyword s "end";
      Record fs
  | Name n when (match lookup s n with Type _ -> true | _ -> false) -> (
      advance s;
      match lookup s n with Type t -> t | _ -> assert false)
  | _ ->
      (* a subrange: lo..hi *)
      let t, lo = constant s in
      expect_symbol s "..";
      let t', hi = constant s in
      check_type s ~expected:t t';
      if hi < lo then fail s "Lower bound greater than upper bound";
      Subrange (base t, lo, hi)

(* an array's index: a subrange, written or named, or char, or boolean *)
and index_type (s : state) : int * int =
  match type_ s with
  | Subrange (_, lo, hi) -> (lo, hi)
  | Character -> (0, 255)
  | Boolean -> (0, 1)
  | _ -> fail s "an array's index must be a subrange (1..10)"

and name_list (s : state) : string list =
  let first = name s in
  if is_symbol s "," then (advance s; first :: name_list s) else [ first ]

(*****************************************************************************)
(* Variables: where they are *)
(*****************************************************************************)

(* A designator (a, a[i], r.f) is either a variable the instructions
   reach directly, lod d,o and str d,o, or an address computed on the
   stack, for ind and sto *)
type place = Direct of int * int | Address

let to_address (s : state) (p : place) : unit = match p with Direct (d, o) -> emit s (Lda (d, o)) | Address -> ()

(* the selectors after a variable's name: [i, j] and .f *)
let rec selectors (s : state) (t : ty) (p : place) : ty * place =
  if is_symbol s "[" then begin
    advance s;
    let rec indices t =
      match t with
      | Array (lo, hi, elt) ->
          if not (scalar (expression s)) then fail s "An index is an integer, a character or a boolean";
          emit s (Chk (lo, hi));
          if lo <> 0 then emit s (Inc (-lo));
          emit s (Ixa (size elt));
          if is_symbol s "," then (advance s; indices elt) else elt
      | _ -> fail s "Array expected"
    in
    to_address s p;
    let elt = indices t in
    expect_symbol s "]";
    selectors s elt Address
  end
  else if is_symbol s "." then begin
    advance s;
    match t with
    | Record fields -> (
        let f = name s in
        match List.find_opt (fun (n, _, _) -> n = f) fields with
        | Some (_, offset, ft) ->
            to_address s p;
            if offset <> 0 then emit s (Inc offset);
            selectors s ft Address
        | None -> fail s ("Unknown field " ^ f))
    | _ -> fail s "Record expected"
  end
  else (t, p)

(* a variable named [n], already read: its type and place *)
and variable (s : state) (n : string) : ty * place =
  match lookup s n with
  | Var v ->
      let d = s.level - v.vlevel in
      if v.var_param then begin
        (* a var parameter holds the address of the variable given *)
        emit s (Lod (d, v.offset));
        selectors s v.vty Address
      end
      else selectors s v.vty (Direct (d, v.offset))
  | _ -> fail s "Variable identifier expected"

(* the value of a variable, on the stack *)
and load (s : state) (t : ty) (p : place) : unit =
  match (p, size t) with
  | Direct (d, o), 1 -> emit s (Lod (d, o))
  | Address, 1 -> emit s (Ind 0)
  | p, n ->
      to_address s p;
      emit s (Ldm n)

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* expression = simple [relation simple] *)
and expression (s : state) : ty =
  let t = simple s in
  let relation = function
    | Symbol "=" -> Some Equ | Symbol "<>" -> Some Neq | Symbol "<" -> Some Les
    | Symbol "<=" -> Some Leq | Symbol ">" -> Some Grt | Symbol ">=" -> Some Geq | _ -> None
  in
  match relation (kind s) with
  | Some op ->
      advance s;
      let at = tok s in
      let t' = simple s in
      if not (scalar t) then fail s "Only integers, characters and booleans can be compared";
      check_type ~at s ~expected:t t';
      emit s op;
      Boolean
  | None -> t

(* simple = [+|-] term {(+ | - | or) term} *)
and simple (s : state) : ty =
  let negate = is_symbol s "-" in
  if negate || is_symbol s "+" then advance s;
  let t = term s in
  if negate then begin
    check_type s ~expected:Integer t;
    emit s Ngi
  end;
  let rec more t =
    let op = match kind s with Symbol "+" -> Some (Adi, Integer) | Symbol "-" -> Some (Sbi, Integer) | Keyword "or" -> Some (Ior, Boolean) | _ -> None in
    match op with
    | Some (i, operand) ->
        check_type s ~expected:operand t;
        advance s;
        (let at = tok s in check_type ~at s ~expected:operand (term s));
        emit s i;
        more operand
    | None -> t
  in
  more t

(* term = factor {(times | div | mod | and) factor} *)
and term (s : state) : ty =
  let t = factor s in
  let rec more t =
    let op = match kind s with Symbol "*" -> Some (Mpi, Integer) | Keyword "div" -> Some (Dvi, Integer) | Keyword "mod" -> Some (Mod, Integer) | Keyword "and" -> Some (And, Boolean) | Symbol "/" -> fail s "No reals in this Pascal: div divides integers" | _ -> None in
    match op with
    | Some (i, operand) ->
        check_type s ~expected:operand t;
        advance s;
        (let at = tok s in check_type ~at s ~expected:operand (factor s));
        emit s i;
        more operand
    | None -> t
  in
  more t

and factor (s : state) : ty =
  match kind s with
  | Int n ->
      advance s;
      emit s (Ldc n);
      Integer
  | Char c ->
      advance s;
      emit s (Ldc (Char.code c));
      Character
  | String _ -> fail s "A string can only be written (write)"
  | Symbol "(" ->
      advance s;
      let t = expression s in
      expect_symbol s ")";
      t
  | Keyword "not" ->
      advance s;
      (let at = tok s in check_type ~at s ~expected:Boolean (factor s));
      emit s Not;
      Boolean
  | Name n -> (
      advance s;
      match lookup s n with
      | Const (t, v) ->
          emit s (Ldc v);
          t
      | Const_text _ -> fail s "A string can only be written (write)"
      | Var _ ->
          let t, p = variable s n in
          load s t p;
          t
      | Proc ({ result = Some t; _ } as p) ->
          call s n p;
          t
      | Proc _ -> fail s (n ^ " is a procedure: it has no value")
      | Standard f -> standard_function s f
      | Type _ -> fail s "Error in expression")
  | k -> fail s ("Error in expression: " ^ Pascal_lexer.show k)

(* a call: the mark (its static link [d] levels out), the arguments,
   then cup *)
and call (s : state) (n : string) (p : proc) : unit =
  emit s (Mst (s.level - (p.plevel - 1)));
  let words = ref 0 in
  let args = if is_symbol s "(" then (advance s; true) else false in
  List.iteri
    (fun i (param : param) ->
      if i > 0 then expect_symbol s ",";
      if not args then fail s (Printf.sprintf "%s needs %d arguments" n (List.length p.params));
      if param.by_ref then begin
        (* var: the variable's address, and exactly its type *)
        let t, pl = variable s (name s) in
        if t <> param.pty then fail s (Printf.sprintf "Type mismatch: a var parameter must be %s" (type_name param.pty));
        to_address s pl;
        incr words
      end
      else begin
        (let at = tok s in check_type ~at s ~expected:param.pty (expression s));
        range_check s param.pty;
        words := !words + size param.pty
      end)
    p.params;
  if args then begin
    if p.params = [] then fail s (n ^ " takes no arguments");
    expect_symbol s ")"
  end;
  emit s (Cup (!words, p.label))

and one_argument (s : state) : ty =
  expect_symbol s "(";
  let t = expression s in
  expect_symbol s ")";
  t

and standard_function (s : state) (f : string) : ty =
  match f with
  | "abs" | "sqr" ->
      (let at = tok s in check_type ~at s ~expected:Integer (one_argument s));
      emit s (if f = "abs" then Abi else Sqi);
      Integer
  | "odd" ->
      (let at = tok s in check_type ~at s ~expected:Integer (one_argument s));
      emit s Odd;
      Boolean
  | "ord" ->
      let t = one_argument s in
      if not (scalar t) then fail s "ord of an integer, a character or a boolean";
      Integer
  | "chr" ->
      (let at = tok s in check_type ~at s ~expected:Integer (one_argument s));
      emit s (Chk (0, 255));
      Character
  | "succ" | "pred" ->
      let t = one_argument s in
      if not (scalar t) then fail s (f ^ " of an integer, a character or a boolean");
      emit s (Inc (if f = "succ" then 1 else -1));
      base t
  | "random" ->
      (let at = tok s in check_type ~at s ~expected:Integer (one_argument s));
      emit s (Csp Rnd);
      Integer
  | "eoln" ->
      emit s (Csp Eol);
      Boolean
  | _ -> fail s (f ^ " is a procedure: it has no value")

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

(* where a statement's code begins, and its line: where the debugger's
   steps stop *)
let mark (s : state) (line : int) : unit = s.marks <- (s.n, line) :: s.marks

let rec vtype (t : ty) : Pcode.vtype =
  match t with
  | Integer | Text -> Vint
  | Boolean -> Vbool
  | Character -> Vchar
  | Subrange (b, _, _) -> vtype b
  | Array (lo, hi, elt) -> Varray (lo, hi, vtype elt)
  | Record fields -> Vrecord (List.map (fun (n, o, t) -> (n, o, vtype t)) fields)

let rec statement (s : state) : unit =
  (match kind s with
  | Name _ | Keyword ("if" | "while" | "repeat" | "for" | "case") -> mark s (tok s).line
  | _ -> ());
  match kind s with
  | Name n -> (
      advance s;
      match lookup s n with
      | Var _ -> assignment s n
      | Proc ({ result = Some t; _ } as p) when is_symbol s ":=" ->
          (* a function's name assigned: its result, in its frame's
             first word *)
          if not (List.memq p s.functions) then fail s ("Cannot assign to function " ^ n ^ " outside of it");
          advance s;
          (let at = tok s in check_type ~at s ~expected:t (expression s));
          range_check s t;
          emit s (Str (s.level - p.plevel, 0))
      | Proc ({ result = None; _ } as p) -> call s n p
      | Proc _ -> fail s (n ^ " is a function: its value must be used")
      | Standard f -> standard_procedure s f
      | _ -> fail s ("Statement expected, not " ^ n))
  | Keyword "begin" -> compound s
  | Keyword "if" ->
      advance s;
      (let at = tok s in check_type ~at s ~expected:Boolean (expression s));
      expect_keyword s "then";
      let otherwise = new_label s in
      emit s (Fjp otherwise);
      statement s;
      if is_keyword s "else" then begin
        advance s;
        let finish = new_label s in
        emit s (Ujp finish);
        place s otherwise;
        statement s;
        place s finish
      end
      else place s otherwise
  | Keyword "while" ->
      advance s;
      let top = new_label s and finish = new_label s in
      place s top;
      (let at = tok s in check_type ~at s ~expected:Boolean (expression s));
      expect_keyword s "do";
      emit s (Fjp finish);
      statement s;
      emit s (Ujp top);
      place s finish
  | Keyword "repeat" ->
      advance s;
      let top = new_label s in
      place s top;
      statements s;
      expect_keyword s "until";
      (let at = tok s in check_type ~at s ~expected:Boolean (expression s));
      emit s (Fjp top)
  | Keyword "for" -> for_loop s
  | Keyword "case" -> case s
  | _ -> () (* the empty statement: begin end, or a ; too many *)

and statements (s : state) : unit =
  statement s;
  if is_symbol s ";" then begin
    advance s;
    statements s
  end

and compound (s : state) : unit =
  expect_keyword s "begin";
  statements s;
  expect_keyword s "end"

and assignment (s : state) (n : string) : unit =
  let t, p = variable s n in
  expect_symbol s ":=";
  if size t = 1 then begin
    let at = tok s in
    let t' = expression s in
    check_type ~at s ~expected:t t';
    range_check s t;
    match p with Direct (d, o) -> emit s (Str (d, o)) | Address -> emit s Sto
  end
  else begin
    (* an array or a record: its words, all of them *)
    to_address s p;
    (let at = tok s in check_type ~at s ~expected:t (expression s));
    emit s (Stm (size t))
  end

(* for v := a to b do s: the limit kept in a temporary, computed once *)
and for_loop (s : state) : unit =
  advance s;
  let v = name s in
  let t, p = variable s v in
  let d, o = match p with Direct (d, o) when scalar t -> (d, o) | _ -> fail s "A for loop's variable must be a simple variable" in
  expect_symbol s ":=";
  (let at = tok s in check_type ~at s ~expected:t (expression s));
  range_check s t;
  emit s (Str (d, o));
  let up = if is_keyword s "to" then true else if is_keyword s "downto" then false else fail s "to or downto expected" in
  advance s;
  let limit = allocate s Integer in
  (let at = tok s in check_type ~at s ~expected:t (expression s));
  emit s (Str (0, limit));
  expect_keyword s "do";
  let top = new_label s and finish = new_label s in
  place s top;
  emit s (Lod (d, o));
  emit s (Lod (0, limit));
  emit s (if up then Leq else Geq);
  emit s (Fjp finish);
  statement s;
  emit s (Lod (d, o));
  emit s (Inc (if up then 1 else -1));
  emit s (Str (d, o));
  emit s (Ujp top);
  place s finish

(* case e of 1, 2: s; 3: s' else s'' end: the selector kept in a
   temporary, compared with each label in turn (Pascal-P jumped
   through a table, xjp: an exercise) *)
and case (s : state) : unit =
  advance s;
  let t = expression s in
  if not (scalar t) then fail s "case of an integer, a character or a boolean";
  let selector = allocate s Integer in
  emit s (Str (0, selector));
  expect_keyword s "of";
  let finish = new_label s in
  let rec arms () =
    if is_keyword s "end" then ()
    else if is_keyword s "else" then begin
      advance s;
      statements s
    end
    else begin
      let rec labels first =
        let lt, v = constant s in
        check_type s ~expected:t lt;
        emit s (Lod (0, selector));
        emit s (Ldc v);
        emit s Equ;
        if not first then emit s Ior;
        if is_symbol s "," then (advance s; labels false)
      in
      labels true;
      expect_symbol s ":";
      let next = new_label s in
      emit s (Fjp next);
      statement s;
      emit s (Ujp finish);
      place s next;
      if is_symbol s ";" then advance s;
      arms ()
    end
  in
  arms ();
  expect_keyword s "end";
  place s finish

and standard_procedure (s : state) (f : string) : unit =
  match f with
  | "write" | "writeln" ->
      if is_symbol s "(" then begin
        advance s;
        let rec args first =
          if not first then expect_symbol s ",";
          write_argument s;
          if not (is_symbol s ")") then args false
        in
        args true;
        expect_symbol s ")"
      end;
      if f = "writeln" then emit s (Csp Wln)
  | "read" | "readln" ->
      if is_symbol s "(" then begin
        advance s;
        let rec args first =
          if not first then expect_symbol s ",";
          let t, p = variable s (name s) in
          to_address s p;
          (match base t with
          | Integer -> emit s (Csp Rdi)
          | Character -> emit s (Csp Rdc)
          | _ -> fail s "Only integers and characters can be read");
          if not (is_symbol s ")") then args false
        in
        args true;
        expect_symbol s ")"
      end;
      if f = "readln" then emit s (Csp Rln)
  | _ -> fail s (f ^ " is a function: its value must be used")

(* write's argument: a value, and after a colon the width it is
   right-aligned in *)
and write_argument (s : state) : unit =
  let width () = if is_symbol s ":" then (advance s; (let at = tok s in check_type ~at s ~expected:Integer (expression s))) else emit s (Ldc 0) in
  let text t = width (); emit s (Csp (Wrs t)) in
  match kind s with
  | String t ->
      advance s;
      text t
  | Name n when (match lookup s n with Const_text _ -> true | _ -> false) -> (
      advance s;
      match lookup s n with Const_text t -> text t | _ -> assert false)
  | _ -> (
      let t = expression s in
      width ();
      match base t with
      | Integer -> emit s (Csp Wri)
      | Character -> emit s (Csp Wrc)
      | Boolean -> emit s (Csp Wrb)
      | _ -> fail s "Only integers, characters, booleans and strings can be written")

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

(* the declarations, then the body at the label [entry]: its frame's
   size known only at the end, ent patched then *)
let rec block (s : state) ~(entry : int) ~(id : int) ~(parent : int) ~(pname : string) ~(params : string list) ~(finish : instr) : unit =
  if is_keyword s "const" then begin
    advance s;
    let rec consts () =
      match kind s with
      | Name n ->
          advance s;
          expect_symbol s "=";
          (match kind s with
          | String t ->
              advance s;
              declare s n (Const_text t)
          | _ ->
              let t, v = constant s in
              declare s n (Const (t, v)));
          expect_symbol s ";";
          consts ()
      | _ -> ()
    in
    consts ()
  end;
  if is_keyword s "type" then begin
    advance s;
    let rec types () =
      match kind s with
      | Name n ->
          advance s;
          expect_symbol s "=";
          declare s n (Type (type_ s));
          expect_symbol s ";";
          types ()
      | _ -> ()
    in
    types ()
  end;
  if is_keyword s "var" then begin
    advance s;
    let rec vars () =
      match kind s with
      | Name _ ->
          let names = name_list s in
          expect_symbol s ":";
          let t = type_ s in
          List.iter (fun n -> declare s n (Var { vty = t; vlevel = s.level; offset = allocate s t; var_param = false })) names;
          expect_symbol s ";";
          vars ()
      | _ -> ()
    in
    vars ()
  end;
  while is_keyword s "procedure" || is_keyword s "function" do
    procedure s
  done;
  place s entry;
  let ent = s.n in
  (* the debugger stops at the begin, and at the end *)
  mark s (tok s).line;
  emit s (Ent 0);
  compound s;
  s.code.(ent) <- Ent s.frame;
  mark s s.last.line;
  emit s finish;
  let variables =
    List.rev
      (List.filter_map
         (fun (vname, e) ->
           match e with
           | Var v -> Some { Pcode.vname; offset = v.offset; vtype = vtype v.vty; by_ref = v.var_param; param = List.mem vname params }
           | _ -> None)
         (List.hd s.scopes))
  in
  let info = { Pcode.pname; level = s.level; parent; first = ent; last = s.n - 1; variables } in
  s.procedures <- (id, info) :: s.procedures

(* procedure p(a: integer; var b: t); block; -- or forward; *)
and procedure (s : state) : unit =
  let is_function = is_keyword s "function" in
  advance s;
  let at = ((tok s).line, (tok s).col) in
  let n = name s in
  let earlier = match List.assoc_opt n (List.hd s.scopes) with Some (Proc p) when not p.defined -> Some p | _ -> None in
  let p =
    match earlier with
    | Some p -> p (* its heading was given before, with forward *)
    | None ->
        let params =
          if is_symbol s "(" then begin
            advance s;
            let rec groups () =
              let by_ref = is_keyword s "var" in
              if by_ref then advance s;
              let names = name_list s in
              expect_symbol s ":";
              let t = match kind s with Name tn -> (match lookup s tn with Type t -> advance s; t | _ -> fail s "Type identifier expected") | _ -> fail s "Type identifier expected" in
              let ps = List.map (fun pname -> { pname; pty = t; by_ref }) names in
              if is_symbol s ";" then (advance s; ps @ groups ()) else ps
            in
            let ps = groups () in
            expect_symbol s ")";
            ps
          end
          else []
        in
        let result =
          if is_function then begin
            expect_symbol s ":";
            match kind s with
            | Name tn -> (
                match lookup s tn with
                | Type t when scalar t -> advance s; Some t
                | _ -> fail s "A function returns an integer, a character or a boolean")
            | _ -> fail s "Type identifier expected"
          end
          else None
        in
        let p = { plevel = s.level + 1; label = new_label s; params; result; defined = false; at } in
        declare s n (Proc p);
        p
  in
  expect_symbol s ";";
  if is_keyword s "forward" then begin
    if earlier <> None then fail s ("Duplicate forward " ^ n);
    advance s;
    expect_symbol s ";"
  end
  else begin
    p.defined <- true;
    (* the body: a scope of its own, one level in, the parameters after
       the mark *)
    let saved_frame = s.frame and saved_functions = s.functions in
    s.level <- s.level + 1;
    s.frame <- Pcode.mark;
    s.scopes <- [] :: s.scopes;
    if is_function then s.functions <- p :: s.functions;
    List.iter
      (fun (param : param) ->
        let offset = allocate s (if param.by_ref then Integer else param.pty) in
        declare s param.pname (Var { vty = param.pty; vlevel = s.level; offset; var_param = param.by_ref }))
      p.params;
    let id = s.nprocedures in
    s.nprocedures <- id + 1;
    let parent = s.current in
    s.current <- id;
    block s ~entry:p.label ~id ~parent ~pname:n ~params:(List.map (fun (q : param) -> q.pname) p.params) ~finish:(if is_function then Retf else Retp);
    s.current <- parent;
    s.scopes <- List.tl s.scopes;
    s.level <- s.level - 1;
    s.frame <- saved_frame;
    s.functions <- saved_functions;
    expect_symbol s ";"
  end

let standard_names =
  [ ("integer", Type Integer); ("boolean", Type Boolean); ("char", Type Character);
    ("true", Const (Boolean, 1)); ("false", Const (Boolean, 0)); ("maxint", Const (Integer, 32767)) ]
  @ List.map (fun f -> (f, Standard f))
      [ "write"; "writeln"; "read"; "readln"; "abs"; "sqr"; "odd"; "ord"; "chr"; "succ"; "pred"; "random"; "eoln" ]

(* program name (input, output); block. *)
let program (s : state) : Pcode.program =
  expect_keyword s "program";
  let pname = name s in
  if is_symbol s "(" then begin
    advance s;
    ignore (name_list s);
    expect_symbol s ")"
  end;
  expect_symbol s ";";
  let main = new_label s in
  emit s (Ujp main);
  s.scopes <- [] :: s.scopes;
  s.nprocedures <- 1;
  block s ~entry:main ~id:0 ~parent:(-1) ~pname ~params:[] ~finish:Stp;
  expect_symbol s ".";
  (* forward procedures never given a body *)
  List.iter
    (fun scope ->
      List.iter
        (fun (n, e) ->
          match e with
          | Proc { defined = false; at = line, col; _ } -> raise (Failed { line; col; message = n ^ " was declared forward, but its body never came" })
          | _ -> ())
      scope)
    s.scopes;
  (* the labels replaced by their addresses *)
  let at l = s.labels.(l) in
  let code =
    Array.map (function Ujp l -> Ujp (at l) | Fjp l -> Fjp (at l) | Cup (n, l) -> Cup (n, at l) | i -> i) (Array.sub s.code 0 s.n)
  in
  let statements = Array.make s.n (-1) in
  (* the first mark of an address wins: a statement's, not those of
     the statements nested in it that start at the same place *)
  List.iter (fun (a, l) -> if a < s.n then statements.(a) <- l) s.marks;
  let procedures = Array.of_list (List.map snd (List.sort compare s.procedures)) in
  { code; lines = Array.sub s.lines 0 s.n; statements; procedures }

let compile (text : string) : (Pcode.program, error) result =
  match Pascal_lexer.tokens text with
  | exception Pascal_lexer.Error (line, col, message) -> Error { line; col; message }
  | toks -> (
      let s =
        { toks = Array.of_list toks; pos = 0; last = List.hd toks; code = Array.make 64 Stp; lines = Array.make 64 0; n = 0; labels = Array.make 16 (-1); nlabels = 0;
          scopes = [ standard_names ]; level = 0; frame = Pcode.mark; functions = [];
          marks = []; procedures = []; nprocedures = 0; current = 0 }
      in
      match program s with p -> Ok p | exception Failed e -> Error e)
