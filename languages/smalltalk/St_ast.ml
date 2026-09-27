(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See St_ast.mli *)

type pos = int * int

type literal =
  | L_int of int
  | L_large of bool * int list
  | L_float of float
  | L_char of char
  | L_string of string
  | L_symbol of string
  | L_array of literal list
  | L_nil
  | L_true
  | L_false

type expr = { e : desc; pos : pos }

and desc =
  | Lit of literal
  | Var of string
  | Assign of string * expr
  | Send of expr * string * expr list
  | Cascade of expr * (string * expr list * pos) list
  | Block of string list * string list * stmt list

and stmt = Expr of expr | Return of expr * pos

type method_ = { selector : string; args : string list; temps : string list; primitive : int option; body : stmt list }

let arity (sel : string) : int =
  let colons = String.fold_left (fun n c -> if c = ':' then n + 1 else n) 0 sel in
  if colons > 0 then colons
  else if sel <> "" && not ((sel.[0] >= 'a' && sel.[0] <= 'z') || (sel.[0] >= 'A' && sel.[0] <= 'Z')) then 1
  else 0

let rec show_literal = function
  | L_int i -> string_of_int i
  | L_large (neg, _) -> if neg then "-<large>" else "<large>"
  | L_float f -> Printf.sprintf "%g" f
  | L_char c -> Printf.sprintf "$%c" c
  | L_string s ->
      let b = Buffer.create 16 in
      Buffer.add_char b '\'';
      String.iter (fun c -> if c = '\'' then Buffer.add_string b "''" else Buffer.add_char b c) s;
      Buffer.add_char b '\'';
      Buffer.contents b
  | L_symbol s -> "#" ^ s
  | L_array l -> "#(" ^ String.concat " " (List.map show_literal l) ^ ")"
  | L_nil -> "nil"
  | L_true -> "true"
  | L_false -> "false"

(* "at:put:" with [a; b] -> "at: a put: b" *)
let show_message (sel : string) (args : string list) : string =
  match args with
  | [] -> sel
  | [ a ] when arity sel = 1 && not (String.contains sel ':') -> sel ^ " " ^ a
  | _ ->
      let parts = String.split_on_char ':' sel |> List.filter (( <> ) "") in
      String.concat " " (List.map2 (fun p a -> p ^ ": " ^ a) parts args)

let rec show_expr (x : expr) : string =
  match x.e with
  | Lit l -> show_literal l
  | Var v -> v
  | Assign (v, x) -> v ^ " := " ^ show_expr x
  | Send (r, sel, args) -> "(" ^ show_expr r ^ " " ^ show_message sel (List.map show_expr args) ^ ")"
  | Cascade (r, msgs) ->
      "(" ^ show_expr r ^ " "
      ^ String.concat "; " (List.map (fun (sel, args, _) -> show_message sel (List.map show_expr args)) msgs)
      ^ ")"
  | Block (args, temps, body) ->
      let args = String.concat "" (List.map (fun a -> ":" ^ a ^ " ") args) in
      let args = if args = "" then "" else args ^ "| " in
      let temps = if temps = [] then "" else "| " ^ String.concat " " temps ^ " | " in
      "[" ^ args ^ temps ^ String.concat ". " (List.map show_stmt body) ^ "]"

and show_stmt = function Expr x -> show_expr x | Return (x, _) -> "^" ^ show_expr x
