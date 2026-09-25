(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lisp.mli *)

type t = Int of int | Str of string | Sym of string | Cons of t * t | Subr of string

let nil = Sym "nil"
let t = Sym "t"
let of_bool b = if b then t else nil
let truthy v = v <> nil
let list (xs : t list) : t = List.fold_right (fun x acc -> Cons (x, acc)) xs nil

let rec to_list (v : t) : t list option =
  match v with
  | Sym "nil" -> Some []
  | Cons (x, rest) -> Option.map (fun xs -> x :: xs) (to_list rest)
  | _ -> None

let quote_string (s : string) : string =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let rec show ~(readably : bool) (v : t) : string =
  match v with
  | Int n -> string_of_int n
  | Str s -> if readably then quote_string s else s
  | Sym s -> s
  | Subr name -> "#<subr " ^ name ^ ">"
  | Cons (Sym "quote", Cons (x, Sym "nil")) -> "'" ^ show ~readably x
  | Cons (Sym "function", Cons (x, Sym "nil")) -> "#'" ^ show ~readably x
  | Cons _ ->
      (* the elements, and a dot before a last cdr that isn't nil *)
      let rec elements (v : t) =
        match v with
        | Sym "nil" -> []
        | Cons (x, rest) -> show ~readably x :: elements rest
        | last -> [ "."; show ~readably last ]
      in
      "(" ^ String.concat " " (elements v) ^ ")"

let print = show ~readably:true
let princ = show ~readably:false
