(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Sexpr.mli *)

type span = { start : int; stop : int }

type t = { datum : datum; span : span }

and datum =
  | Int of int
  | Float of float
  | Str of string
  | Sym of string
  | Char of int
  | Bool of bool
  | List of t list * t option
  | Vector of t list

let make (datum : datum) (span : span) : t = { datum; span }
let sym (name : string) (span : span) : t = { datum = Sym name; span }
let nowhere = { start = 0; stop = 0 }

let rec to_string (x : t) : string =
  match x.datum with
  | Int n -> string_of_int n
  | Float f -> Printf.sprintf "%g" f
  | Str s -> Printf.sprintf "%S" s
  | Sym s -> s
  | Char c -> Printf.sprintf "#\\%c" (Char.chr (c land 255))
  | Bool b -> if b then "#t" else "#f"
  | List ([ { datum = Sym "quote"; _ }; x ], None) -> "'" ^ to_string x
  | List (xs, tail) ->
      let tail = match tail with Some t -> " . " ^ to_string t | None -> "" in
      "(" ^ String.concat " " (List.map to_string xs) ^ tail ^ ")"
  | Vector xs -> "#(" ^ String.concat " " (List.map to_string xs) ^ ")"
