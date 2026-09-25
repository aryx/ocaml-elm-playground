(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Basic_parse.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type op = Add | Sub | Mul | Div
type expr = Num of int | Var of char | Neg of expr | Bin of op * expr * expr | Rnd of expr | Abs of expr
type relop = Eq | Ne | Lt | Le | Gt | Ge
type item = Str of string | Expr of expr
type sep = Semi | Comma | Newline

type stmt =
  | Print of (item * sep) list
  | Input of char list
  | Let of char * expr
  | If of expr * relop * expr * stmt
  | Goto of expr
  | Gosub of expr
  | Return
  | End
  | Rem
  | List
  | Run
  | New
  | Bye

type line = Numbered of int * stmt option | Direct of stmt

(*****************************************************************************)
(* Characters *)
(*****************************************************************************)

let capitals (s : string) : string =
  let inside = ref false in
  String.map
    (fun c ->
      if c = '"' then inside := not !inside;
      if !inside then c else Char.uppercase_ascii c)
    s

(* the line being read, and where *)
type p = { s : string; mutable i : int }

exception Syntax of string

let fail (msg : string) = raise (Syntax msg)

let skip (p : p) : unit =
  while p.i < String.length p.s && p.s.[p.i] = ' ' do
    p.i <- p.i + 1
  done

(* the next character that isn't a space, not consumed *)
let peek (p : p) : char option =
  skip p;
  if p.i < String.length p.s then Some p.s.[p.i] else None

let at_end (p : p) : bool = peek p = None

let accept (p : p) (c : char) : bool =
  if peek p = Some c then begin
    p.i <- p.i + 1;
    true
  end
  else false

let expect (p : p) (c : char) : unit = if not (accept p c) then fail (Printf.sprintf "%c EXPECTED" c)

(* a keyword, spaces allowed between its letters as in 1976 (G O T O
   is GOTO); nothing consumed if it isn't there *)
let keyword (p : p) (k : string) : bool =
  let start = p.i in
  let ok = String.for_all (fun c -> accept p c) k in
  if not ok then p.i <- start;
  ok

let is_digit c = c >= '0' && c <= '9'
let is_letter c = c >= 'A' && c <= 'Z'

(* at most 32767: the numbers are 16 bits, as the 1976 machines' were *)
let number (p : p) : int =
  skip p;
  let start = p.i in
  while p.i < String.length p.s && is_digit p.s.[p.i] do
    p.i <- p.i + 1
  done;
  if p.i = start then fail "NUMBER EXPECTED";
  match int_of_string_opt (String.sub p.s start (p.i - start)) with
  | Some n when n <= 32767 -> n
  | _ -> fail "NUMBER TOO BIG"

let var (p : p) : char =
  match peek p with
  | Some c when is_letter c ->
      p.i <- p.i + 1;
      c
  | _ -> fail "VARIABLE EXPECTED"

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

let rec expr (p : p) : expr =
  let first =
    if accept p '-' then Neg (term p)
    else begin
      ignore (accept p '+');
      term p
    end
  in
  let rec more acc =
    if accept p '+' then more (Bin (Add, acc, term p)) else if accept p '-' then more (Bin (Sub, acc, term p)) else acc
  in
  more first

and term (p : p) : expr =
  let rec more acc =
    if accept p '*' then more (Bin (Mul, acc, factor p)) else if accept p '/' then more (Bin (Div, acc, factor p)) else acc
  in
  more (factor p)

and factor (p : p) : expr =
  let call f =
    expect p '(';
    let e = expr p in
    expect p ')';
    f e
  in
  match peek p with
  | Some '(' ->
      p.i <- p.i + 1;
      let e = expr p in
      expect p ')';
      e
  | Some c when is_digit c -> Num (number p)
  | Some _ when keyword p "RND" -> call (fun e -> Rnd e)
  | Some _ when keyword p "ABS" -> call (fun e -> Abs e)
  | Some c when is_letter c -> Var (var p)
  | _ -> fail "EXPRESSION EXPECTED"

let relop (p : p) : relop =
  if accept p '<' then if accept p '>' then Ne else if accept p '=' then Le else Lt
  else if accept p '>' then if accept p '=' then Ge else if accept p '<' then Ne else Gt
  else if accept p '=' then Eq
  else fail "RELATION EXPECTED"

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

let string_lit (p : p) : string =
  expect p '"';
  match String.index_from_opt p.s p.i '"' with
  | Some j ->
      let s = String.sub p.s p.i (j - p.i) in
      p.i <- j + 1;
      s
  | None -> fail "UNTERMINATED STRING"

let rec print_items (p : p) : (item * sep) list =
  if at_end p then []
  else
    let item = if peek p = Some '"' then Str (string_lit p) else Expr (expr p) in
    if accept p ';' then (item, Semi) :: print_items p
    else if accept p ',' then (item, Comma) :: print_items p
    else [ (item, Newline) ]

let rec vars (p : p) : char list =
  let v = var p in
  if accept p ',' then v :: vars p else [ v ]

let assignment (p : p) : stmt =
  let v = var p in
  expect p '=';
  Let (v, expr p)

let rec statement (p : p) : stmt =
  if keyword p "PRINT" then Print (print_items p)
  else if keyword p "INPUT" then Input (vars p)
  else if keyword p "LET" then assignment p
  else if keyword p "IF" then begin
    let a = expr p in
    let r = relop p in
    let b = expr p in
    if not (keyword p "THEN") then fail "THEN EXPECTED";
    (* THEN 100: a GOTO without the word *)
    let s = match peek p with Some c when is_digit c -> Goto (Num (number p)) | _ -> statement p in
    If (a, r, b, s)
  end
  else if keyword p "GOTO" then Goto (expr p)
  else if keyword p "GOSUB" then Gosub (expr p)
  else if keyword p "RETURN" then Return
  else if keyword p "END" then End
  else if keyword p "REM" then begin
    p.i <- String.length p.s;
    Rem
  end
  else if keyword p "LIST" then List
  else if keyword p "RUN" then Run
  else if keyword p "NEW" then New
  else if keyword p "BYE" then Bye
  (* the LET left out, as Palo Alto allowed: X = 5 *)
  else match peek p with Some c when is_letter c -> assignment p | _ -> fail "SYNTAX"

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

let parse_line (line : string) : (line, string) result =
  let p = { s = capitals line; i = 0 } in
  try
    let l =
      match peek p with
      | Some c when is_digit c ->
          let n = number p in
          if at_end p then Numbered (n, None) else Numbered (n, Some (statement p))
      | _ -> Direct (statement p)
    in
    if at_end p then Ok l else fail "SYNTAX"
  with Syntax msg -> Error msg
