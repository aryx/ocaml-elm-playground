(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Formula.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type cell = int * int

type expr =
  | Number of float
  | Ref of cell
  | Range of cell * cell
  | Unary of char * expr
  | Binop of char * expr * expr
  | Call of string * expr list

type content = Formula of expr | Invalid of string | Value of float | Text of string | Blank

(*****************************************************************************)
(* How a cell is named *)
(*****************************************************************************)

(* A column is base 26 with no zero: A..Z, then AA..AZ, BA.. -- which
 * is why the column after Z is AA and not BA, and why this is a loop
 * and not a division. *)
let name_of_cell ((col, row) : cell) =
  let rec letters n acc =
    let acc = String.make 1 (Char.chr (Char.code 'A' + (n mod 26))) ^ acc in
    if n < 26 then acc else letters ((n / 26) - 1) acc
  in
  letters col "" ^ string_of_int (row + 1)

let cell_of_name (s : string) : cell option =
  let n = String.length s in
  let rec split i = if i < n && s.[i] >= 'A' && s.[i] <= 'Z' then split (i + 1) else i in
  let i = split 0 in
  if i = 0 || i = n then None
  else
    let col = ref 0 in
    String.iter (fun c -> col := (!col * 26) + (Char.code c - Char.code 'A') + 1) (String.sub s 0 i);
    match int_of_string_opt (String.sub s i (n - i)) with
    | Some row when row >= 1 -> Some (!col - 1, row - 1)
    | _ -> None

(*****************************************************************************)
(* The tokens *)
(*****************************************************************************)

type token = TNum of float | TName of string | TOp of char | TOpen | TClose | TComma | TColon

exception Bad of string

let tokens (s : string) : token list =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      match s.[i] with
      | ' ' | '\t' -> go (i + 1) acc
      | '(' -> go (i + 1) (TOpen :: acc)
      | ')' -> go (i + 1) (TClose :: acc)
      | ',' -> go (i + 1) (TComma :: acc)
      | ':' -> go (i + 1) (TColon :: acc)
      | ('+' | '-' | '*' | '/') as c -> go (i + 1) (TOp c :: acc)
      | c when (c >= '0' && c <= '9') || c = '.' ->
          let j = ref i in
          while !j < n && ((s.[!j] >= '0' && s.[!j] <= '9') || s.[!j] = '.') do
            incr j
          done;
          let text = String.sub s i (!j - i) in
          (match float_of_string_opt text with
          | Some f -> go !j (TNum f :: acc)
          | None -> raise (Bad (Printf.sprintf "%S is not a number" text)))
      | c when (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ->
          let j = ref i in
          while
            !j < n
            && ((s.[!j] >= 'A' && s.[!j] <= 'Z')
               || (s.[!j] >= 'a' && s.[!j] <= 'z')
               || (s.[!j] >= '0' && s.[!j] <= '9'))
          do
            incr j
          done;
          go !j (TName (String.uppercase_ascii (String.sub s i (!j - i))) :: acc)
      | c -> raise (Bad (Printf.sprintf "%C has no meaning here" c))
  in
  go 0 []

(*****************************************************************************)
(* One function per rule of the grammar *)
(*****************************************************************************)

let parse (s : string) : (expr, string) result =
  let rest = ref [] in
  let peek () = match !rest with [] -> None | t :: _ -> Some t in
  let eat () = match !rest with [] -> raise (Bad "it stops too soon") | t :: r -> rest := r; t in
  let expect t what = if eat () <> t then raise (Bad (Printf.sprintf "expected %s" what)) in
  let name_as_cell name =
    match cell_of_name name with
    | Some c -> c
    | None -> raise (Bad (Printf.sprintf "%s is not a cell" name))
  in
  (* expr ::= term (('+' | '-') term)* *)
  let rec expr () =
    let left = ref (term ()) in
    let rec go () =
      match peek () with
      | Some (TOp (('+' | '-') as c)) ->
          ignore (eat ());
          left := Binop (c, !left, term ());
          go ()
      | _ -> ()
    in
    go ();
    !left
  (* term ::= factor (('*' | '/') factor)* -- lower down, so it binds
     tighter: that is where precedence comes from *)
  and term () =
    let left = ref (factor ()) in
    let rec go () =
      match peek () with
      | Some (TOp (('*' | '/') as c)) ->
          ignore (eat ());
          left := Binop (c, !left, factor ());
          go ()
      | _ -> ()
    in
    go ();
    !left
  and factor () =
    match peek () with
    | Some (TOp '-') ->
        ignore (eat ());
        Unary ('-', factor ())
    | Some (TOp '+') ->
        ignore (eat ());
        factor ()
    | _ -> atom ()
  and atom () =
    match eat () with
    | TNum f -> Number f
    | TOpen ->
        let e = expr () in
        expect TClose ")";
        e
    | TName name -> (
        match peek () with
        (* a call: SUM(A1:A9, B1) *)
        | Some TOpen ->
            ignore (eat ());
            let args = ref [] in
            (if peek () <> Some TClose then
               let rec go () =
                 args := expr () :: !args;
                 if peek () = Some TComma then (
                   ignore (eat ());
                   go ())
               in
               go ());
            expect TClose ")";
            Call (name, List.rev !args)
        (* a range: A1:B9 *)
        | Some TColon ->
            ignore (eat ());
            let upto = match eat () with
              | TName n -> name_as_cell n
              | _ -> raise (Bad "a range ends at a cell")
            in
            Range (name_as_cell name, upto)
        | _ -> Ref (name_as_cell name))
    | TOp c -> raise (Bad (Printf.sprintf "%C where a value was expected" c))
    | TClose -> raise (Bad "one ) too many")
    | TComma | TColon -> raise (Bad "a stray , or :")
  in
  try
    rest := tokens s;
    if !rest = [] then Error "an empty formula"
    else
      let e = expr () in
      if !rest <> [] then Error "there is something after the end of the formula" else Ok e
  with Bad msg -> Error msg

(*****************************************************************************)
(* What a cell holds *)
(*****************************************************************************)

let content_of (s : string) : content =
  let trimmed = String.trim s in
  if trimmed = "" then Blank
  else if trimmed.[0] = '=' then
    match parse (String.sub trimmed 1 (String.length trimmed - 1)) with
    | Ok e -> Formula e
    | Error msg -> Invalid msg
  else match float_of_string_opt trimmed with Some f -> Value f | None -> Text trimmed

(* printed back out with the parentheses it needs and no others:
 * a child binds looser than its parent only if it is a sum inside a
 * product *)
let to_string (e : expr) : string =
  let prec = function Binop (('+' | '-'), _, _) -> 1 | Binop (('*' | '/'), _, _) -> 2 | _ -> 3 in
  let rec go e =
    match e with
    | Number f -> if Float.is_integer f then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f
    | Ref c -> name_of_cell c
    | Range (a, b) -> name_of_cell a ^ ":" ^ name_of_cell b
    | Unary (c, e) -> Printf.sprintf "%c%s" c (wrap e 3)
    | Binop (c, a, b) ->
        let p = prec e in
        Printf.sprintf "%s%c%s" (wrap a p) c (wrap b (p + 1))
    | Call (name, args) -> Printf.sprintf "%s(%s)" name (String.concat "," (List.map go args))
  and wrap e p = if prec e < p then "(" ^ go e ^ ")" else go e in
  go e

let shift ((dc, dr) : int * int) (e : expr) : expr =
  let move (c, r) = (max 0 (c + dc), max 0 (r + dr)) in
  let rec go = function
    | Number f -> Number f
    | Ref c -> Ref (move c)
    | Range (a, b) -> Range (move a, move b)
    | Unary (c, e) -> Unary (c, go e)
    | Binop (c, a, b) -> Binop (c, go a, go b)
    | Call (name, args) -> Call (name, List.map go args)
  in
  go e

let refs (e : expr) : cell list =
  let out = ref [] in
  let rec go = function
    | Number _ -> ()
    | Ref c -> out := c :: !out
    | Range ((c1, r1), (c2, r2)) ->
        for col = min c1 c2 to max c1 c2 do
          for row = min r1 r2 to max r1 r2 do
            out := (col, row) :: !out
          done
        done
    | Unary (_, e) -> go e
    | Binop (_, a, b) -> go a; go b
    | Call (_, args) -> List.iter go args
  in
  go e;
  List.sort_uniq compare !out
