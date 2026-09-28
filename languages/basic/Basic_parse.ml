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

type op = Add | Sub | Mul | Div | Pow | Eq | Ne | Lt | Le | Gt | Ge | And | Or

type expr =
  | Num of float
  | Str of string
  | Var of string
  | Index of string * expr list
  | Call of string * expr list
  | Fn of string * expr
  | Neg of expr
  | Not of expr
  | Bin of op * expr * expr

type item = Expr of expr | Tab of expr | Spc of expr
type sep = Semi | Comma | Newline
type lvalue = Scalar of string | Elem of string * expr list
type datum = D_num of float | D_str of string

type stmt =
  | Print of (item * sep) list
  | Input of string option * lvalue list
  | Let of lvalue * expr
  | If of expr
  | Goto of expr
  | Gosub of expr
  | On of expr * bool * int list
  | Return
  | For of string * expr * expr * expr option
  | Next of string list
  | Dim of (string * expr list) list
  | Data of datum list
  | Read of lvalue list
  | Restore
  | Def of string * string * expr
  | End
  | Stop
  | Rem
  | List
  | Run
  | New
  | Bye
  | Fp
  | Int
  | Catalog
  | Load of string
  | Save of string
  | Run_file of string

type line = Numbered of int * stmt list option | Direct of stmt list

let functions = [ "INT"; "ABS"; "SGN"; "SQR"; "RND"; "SIN"; "COS"; "TAN"; "ATN"; "EXP"; "LOG"; "LEN"; "VAL"; "ASC"; "CHR$"; "STR$"; "LEFT$"; "RIGHT$"; "MID$" ]

(* the words the cruncher finds anywhere, even inside a name *)
let keywords =
  [ "PRINT"; "INPUT"; "LET"; "IF"; "THEN"; "GOTO"; "GOSUB"; "ON"; "RETURN"; "FOR"; "TO"; "STEP"; "NEXT"; "DIM"; "DATA"; "READ";
    "RESTORE"; "DEF"; "FN"; "END"; "STOP"; "REM"; "AND"; "OR"; "NOT"; "TAB("; "SPC(" ]
  @ functions

let is_string (name : string) : bool = name <> "" && name.[String.length name - 1] = '$'

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

(* the end of a statement: the line's, or a ":" *)
let at_stmt_end (p : p) : bool = match peek p with None | Some ':' -> true | _ -> false

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

(* whether a keyword starts here, nothing consumed *)
let looking_at_keyword (p : p) : bool =
  List.exists
    (fun k ->
      let start = p.i in
      let found = keyword p k in
      p.i <- start;
      found)
    keywords

let is_digit c = c >= '0' && c <= '9'
let is_letter c = c >= 'A' && c <= 'Z'

(*****************************************************************************)
(* Numbers and names *)
(*****************************************************************************)

(* digits, a point, more digits, an exponent: 12, 3.5, .25, 1E6, 2.5E-3 *)
let number (p : p) : float =
  skip p;
  let start = p.i in
  let digits () =
    while p.i < String.length p.s && is_digit p.s.[p.i] do
      p.i <- p.i + 1
    done
  in
  digits ();
  if p.i < String.length p.s && p.s.[p.i] = '.' then begin
    p.i <- p.i + 1;
    digits ()
  end;
  if p.i < String.length p.s && p.s.[p.i] = 'E' then begin
    let before = p.i in
    p.i <- p.i + 1;
    if p.i < String.length p.s && (p.s.[p.i] = '+' || p.s.[p.i] = '-') then p.i <- p.i + 1;
    let digits_start = p.i in
    digits ();
    (* an E not followed by digits isn't an exponent *)
    if p.i = digits_start then p.i <- before
  end;
  match float_of_string_opt (String.sub p.s start (p.i - start)) with
  | Some f when p.i > start -> f
  | _ -> fail "NUMBER EXPECTED"

let line_number (p : p) : int =
  let f = number p in
  if Float.is_integer f && f >= 0. && f <= 63999. then int_of_float f else fail "BAD LINE NUMBER"

(* a letter, then letters and digits until a keyword starts (the
   cruncher's reading), then a $ for a string's *)
let name (p : p) : string =
  match peek p with
  | Some c when is_letter c && not (looking_at_keyword p) ->
      let start = p.i in
      p.i <- p.i + 1;
      while
        p.i < String.length p.s
        && (is_letter p.s.[p.i] || is_digit p.s.[p.i])
        && not (looking_at_keyword p)
      do
        p.i <- p.i + 1
      done;
      if p.i < String.length p.s && p.s.[p.i] = '$' then p.i <- p.i + 1;
      String.sub p.s start (p.i - start)
  | _ -> fail "VARIABLE EXPECTED"

let string_lit (p : p) : string =
  expect p '"';
  match String.index_from_opt p.s p.i '"' with
  | Some j ->
      let s = String.sub p.s p.i (j - p.i) in
      p.i <- j + 1;
      s
  | None ->
      (* Microsoft allowed a string left open at the end of the line *)
      let s = String.sub p.s p.i (String.length p.s - p.i) in
      p.i <- String.length p.s;
      s

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)

(* from the loosest to the tightest: OR, AND, NOT, comparisons, + -,
   * /, unary -, ^ *)
let rec expr (p : p) : expr =
  let rec more acc = if keyword p "OR" then more (Bin (Or, acc, conj p)) else acc in
  more (conj p)

and conj (p : p) : expr =
  let rec more acc = if keyword p "AND" then more (Bin (And, acc, negation p)) else acc in
  more (negation p)

and negation (p : p) : expr = if keyword p "NOT" then Not (negation p) else comparison p

and comparison (p : p) : expr =
  let relop () =
    if accept p '<' then Some (if accept p '>' then Ne else if accept p '=' then Le else Lt)
    else if accept p '>' then Some (if accept p '=' then Ge else if accept p '<' then Ne else Gt)
    else if accept p '=' then Some (if accept p '<' then Le else if accept p '>' then Ge else Eq)
    else None
  in
  let rec more acc = match relop () with Some op -> more (Bin (op, acc, sum p)) | None -> acc in
  more (sum p)

and sum (p : p) : expr =
  let rec more acc =
    if accept p '+' then more (Bin (Add, acc, product p)) else if accept p '-' then more (Bin (Sub, acc, product p)) else acc
  in
  more (product p)

and product (p : p) : expr =
  let rec more acc =
    if accept p '*' then more (Bin (Mul, acc, unary p)) else if accept p '/' then more (Bin (Div, acc, unary p)) else acc
  in
  more (unary p)

(* -2^2 is -4: the power binds tighter than the sign *)
and unary (p : p) : expr = if accept p '-' then Neg (unary p) else if accept p '+' then unary p else power p

and power (p : p) : expr =
  let rec more acc = if accept p '^' then more (Bin (Pow, acc, if accept p '-' then Neg (atom p) else atom p)) else acc in
  more (atom p)

and args (p : p) : expr list =
  expect p '(';
  let rec rest acc = if accept p ',' then rest (expr p :: acc) else List.rev acc in
  let first = expr p in
  let l = rest [ first ] in
  expect p ')';
  l

and atom (p : p) : expr =
  match peek p with
  | Some '(' ->
      p.i <- p.i + 1;
      let e = expr p in
      expect p ')';
      e
  | Some '"' -> Str (string_lit p)
  | Some c when is_digit c || c = '.' -> Num (number p)
  | Some _ when keyword p "FN" ->
      let f = name p in
      (match args p with [ e ] -> Fn (f, e) | _ -> fail "ONE ARGUMENT EXPECTED")
  | Some _ -> (
      match List.find_opt (fun f -> keyword p f) functions with
      | Some f -> Call (f, args p)
      | None ->
          let v = name p in
          if peek p = Some '(' then Index (v, args p) else Var v)
  | None -> fail "EXPRESSION EXPECTED"

(*****************************************************************************)
(* Statements *)
(*****************************************************************************)

let lvalue (p : p) : lvalue =
  let v = name p in
  if peek p = Some '(' then Elem (v, args p) else Scalar v

let rec comma_list (f : p -> 'a) (p : p) : 'a list =
  let x = f p in
  if accept p ',' then x :: comma_list f p else [ x ]

(* PRINT's items and separators; an item right after another (PRINT
   "X"A) is joined as by ";" *)
let rec print_items (p : p) : (item * sep) list =
  if at_stmt_end p then []
  else if accept p ',' then (Expr (Str ""), Comma) :: print_items p
  else if accept p ';' then print_items p
  else
    let item =
      if keyword p "TAB(" then begin
        let e = expr p in
        expect p ')';
        Tab e
      end
      else if keyword p "SPC(" then begin
        let e = expr p in
        expect p ')';
        Spc e
      end
      else Expr (expr p)
    in
    if accept p ',' then (item, Comma) :: print_items p
    else if accept p ';' then (item, Semi) :: print_items p
    else if at_stmt_end p then [ (item, Newline) ]
    else (item, Semi) :: print_items p

(* a DATA item: a number, a quoted string, or anything up to the next
   comma, trimmed *)
let datum (p : p) : datum =
  if peek p = Some '"' then D_str (string_lit p)
  else begin
    let start = p.i in
    while p.i < String.length p.s && p.s.[p.i] <> ',' && p.s.[p.i] <> ':' do
      p.i <- p.i + 1
    done;
    let s = String.trim (String.sub p.s start (p.i - start)) in
    match float_of_string_opt s with Some f when s <> "" -> D_num f | _ -> D_str s
  end

(* a statement, and for IF what THEN starts: the list is what the line
   goes on with *)
let rec statement (p : p) : stmt list =
  (* the commands, alone on their line *)
  let command k c = if keyword p k && at_end p then Some c else None in
  let commands = [ ("LIST", List); ("RUN", Run); ("NEW", New); ("BYE", Bye); ("FP", Fp); ("INT", Int) ] in
  let start = p.i in
  (* and the disk's, followed by a file's name *)
  let file k c =
    p.i <- start;
    if keyword p k && not (at_end p) then begin
      let name = String.trim (String.sub p.s p.i (String.length p.s - p.i)) in
      p.i <- String.length p.s;
      Some (c name)
    end
    else None
  in
  let files = [ ("LOAD", fun n -> Load n); ("SAVE", fun n -> Save n); ("RUN", fun n -> Run_file n) ] in
  match
    match List.find_map (fun (k, c) -> p.i <- start; command k c) (("CATALOG", Catalog) :: commands) with
    | Some c -> Some c
    | None -> List.find_map (fun (k, c) -> file k c) files
  with
  | Some c -> [ c ]
  | None ->
      p.i <- start;
      if keyword p "PRINT" then [ Print (print_items p) ]
      else if accept p '?' then [ Print (print_items p) ]
      else if keyword p "INPUT" then begin
        let prompt =
          if peek p = Some '"' then begin
            let s = string_lit p in
            if not (accept p ';' || accept p ',') then fail "; EXPECTED";
            Some s
          end
          else None
        in
        [ Input (prompt, comma_list lvalue p) ]
      end
      else if keyword p "IF" then begin
        let c = expr p in
        if keyword p "THEN" then
          match peek p with Some d when is_digit d -> [ If c; Goto (Num (float_of_int (line_number p))) ] | _ -> If c :: statement p
        else if keyword p "GOTO" then [ If c; Goto (Num (float_of_int (line_number p))) ]
        else fail "THEN EXPECTED"
      end
      else if keyword p "GOTO" then [ Goto (expr p) ]
      else if keyword p "GOSUB" then [ Gosub (expr p) ]
      else if keyword p "ON" then begin
        let e = expr p in
        let sub = if keyword p "GOSUB" then true else if keyword p "GOTO" then false else fail "GOTO EXPECTED" in
        [ On (e, sub, comma_list line_number p) ]
      end
      else if keyword p "RETURN" then [ Return ]
      else if keyword p "FOR" then begin
        let v = name p in
        expect p '=';
        let a = expr p in
        if not (keyword p "TO") then fail "TO EXPECTED";
        let b = expr p in
        let step = if keyword p "STEP" then Some (expr p) else None in
        [ For (v, a, b, step) ]
      end
      else if keyword p "NEXT" then [ Next (if at_stmt_end p then [] else comma_list name p) ]
      else if keyword p "DIM" then
        [ Dim (comma_list (fun p -> let v = name p in (v, args p)) p) ]
      else if keyword p "DATA" then [ Data (comma_list datum p) ]
      else if keyword p "READ" then [ Read (comma_list lvalue p) ]
      else if keyword p "RESTORE" then [ Restore ]
      else if keyword p "DEF" then begin
        if not (keyword p "FN") then fail "FN EXPECTED";
        let f = name p in
        expect p '(';
        let x = name p in
        expect p ')';
        expect p '=';
        [ Def (f, x, expr p) ]
      end
      else if keyword p "END" then [ End ]
      else if keyword p "STOP" then [ Stop ]
      else if keyword p "REM" then begin
        p.i <- String.length p.s;
        [ Rem ]
      end
      else begin
        (* the LET left out, as every BASIC after Dartmouth's allowed *)
        ignore (keyword p "LET");
        let v = lvalue p in
        expect p '=';
        [ Let (v, expr p) ]
      end

(* statements separated by ":" *)
let rec statements (p : p) : stmt list =
  let first = statement p in
  if accept p ':' then first @ statements p else if at_end p then first else fail "SYNTAX"

(*****************************************************************************)
(* Lines *)
(*****************************************************************************)

let parse_line (line : string) : (line, string) result =
  let p = { s = capitals line; i = 0 } in
  try
    match peek p with
    | Some c when is_digit c ->
        let n = line_number p in
        Ok (if at_end p then Numbered (n, None) else Numbered (n, Some (statements p)))
    | _ -> Ok (Direct (statements p))
  with Syntax msg -> Error msg
