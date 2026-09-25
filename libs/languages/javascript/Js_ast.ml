(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Js_ast.mli *)

type expr =
  | Number of float
  | String of string
  | Bool of bool
  | Null
  | Name of string
  | This
  | Array of expr list
  | Object of (string * expr) list
  | Function of func
  | Unary of string * expr
  | Update of string * bool * expr
  | Binary of string * expr * expr
  | Logical of string * expr * expr
  | Assign of string * expr * expr
  | Conditional of expr * expr * expr
  | Member of expr * string
  | Index of expr * expr
  | Call of expr * expr list

and func = { name : string option; params : string list; body : stmt list; arrow : bool }
and stmt = { line : int; stmt : statement }

and statement =
  | Expr of expr
  | Let of let_kind * (string * expr option) list
  | Function_decl of func
  | Return of expr option
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | For of stmt option * expr option * expr option * stmt
  | For_of of let_kind * string * expr * stmt
  | Break
  | Continue
  | Throw of expr
  | Try of stmt list * string * stmt list
  | Block of stmt list
  | Empty

and let_kind = Let_kind | Const_kind | Var_kind

type program = stmt list

(*****************************************************************************)
(* Numbers *)
(*****************************************************************************)

(* the shortest of 15, 16 and 17 significant digits that reads back as
   the same float: 17 always do (a double's 53 bits), fewer usually do,
   and a person wants 0.1, not 0.10000000000000001 *)
let number_to_string (f : float) : string =
  if Float.is_nan f then "NaN"
  else if f = Float.infinity then "Infinity"
  else if f = Float.neg_infinity then "-Infinity"
  else if Float.is_integer f && Float.abs f < 1e21 then Printf.sprintf "%.0f" f
  else
    let shortest = List.find (fun p -> float_of_string (Printf.sprintf "%.*g" p f) = f) [ 15; 16; 17 ] in
    Printf.sprintf "%.*g" shortest f

(*****************************************************************************)
(* Printing *)
(*****************************************************************************)

let kind_to_string (k : let_kind) : string = match k with Let_kind -> "Let" | Const_kind -> "Const" | Var_kind -> "Var"
let list (f : 'a -> string) (xs : 'a list) : string = String.concat ", " (List.map f xs)

let rec expr_to_string (e : expr) : string =
  let p = Printf.sprintf in
  match e with
  | Number f -> number_to_string f
  | String s -> p "%S" s
  | Bool b -> string_of_bool b
  | Null -> "null"
  | Name x -> x
  | This -> "this"
  | Array es -> p "[%s]" (list expr_to_string es)
  | Object kvs -> p "{%s}" (list (fun (k, v) -> k ^ ": " ^ expr_to_string v) kvs)
  | Function { arrow = true; params; body = [ { stmt = Return (Some e); _ } ]; _ } ->
      p "(%s) => %s" (String.concat ", " params) (expr_to_string e)
  | Function f -> func_to_string f
  | Unary (("typeof" as op), e) -> p "(%s %s)" op (expr_to_string e)
  | Unary (op, e) -> p "(%s%s)" op (expr_to_string e)
  | Update (op, true, e) -> p "(%s%s)" op (expr_to_string e)
  | Update (op, false, e) -> p "(%s%s)" (expr_to_string e) op
  | Binary (op, a, b) | Logical (op, a, b) | Assign (op, a, b) -> p "(%s %s %s)" (expr_to_string a) op (expr_to_string b)
  | Conditional (c, a, b) -> p "(%s ? %s : %s)" (expr_to_string c) (expr_to_string a) (expr_to_string b)
  | Member (o, x) -> p "(%s.%s)" (expr_to_string o) x
  | Index (o, i) -> p "(%s[%s])" (expr_to_string o) (expr_to_string i)
  | Call (f, args) -> p "(%s(%s))" (expr_to_string f) (list expr_to_string args)

and func_to_string (f : func) : string =
  Printf.sprintf "%s%s [%s] [%s]"
    (if f.arrow then "Arrow" else "Function")
    (match f.name with Some n -> " " ^ n | None -> "")
    (String.concat "; " f.params) (body_to_string f.body)

and body_to_string (body : stmt list) : string = String.concat "; " (List.map stmt_to_string body)

and stmt_to_string (s : stmt) : string =
  let p = Printf.sprintf in
  let e = expr_to_string in
  let opt f x = match x with Some x -> f x | None -> "none" in
  match s.stmt with
  | Expr x -> "Expr " ^ e x
  | Let (k, decls) ->
      kind_to_string k ^ " " ^ list (fun (x, init) -> match init with Some v -> x ^ " " ^ e v | None -> x) decls
  | Function_decl f -> func_to_string f
  | Return None -> "Return"
  | Return (Some x) -> "Return " ^ e x
  | If (c, a, None) -> p "If (%s, %s)" (e c) (stmt_to_string a)
  | If (c, a, Some b) -> p "If (%s, %s, %s)" (e c) (stmt_to_string a) (stmt_to_string b)
  | While (c, b) -> p "While (%s, %s)" (e c) (stmt_to_string b)
  | For (init, test, update, b) ->
      p "For (%s, %s, %s, %s)" (opt stmt_to_string init) (opt e test) (opt e update) (stmt_to_string b)
  | For_of (k, x, xs, b) -> p "For_of (%s %s, %s, %s)" (kind_to_string k) x (e xs) (stmt_to_string b)
  | Break -> "Break"
  | Continue -> "Continue"
  | Throw x -> "Throw " ^ e x
  | Try (body, x, handler) -> p "Try [%s] catch %s [%s]" (body_to_string body) x (body_to_string handler)
  | Block body -> p "Block [%s]" (body_to_string body)
  | Empty -> "Empty"
