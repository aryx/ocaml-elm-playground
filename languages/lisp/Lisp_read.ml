(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Lisp

(* See Lisp_read.mli *)

exception Error of string

(* the tree into Emacs's values: a character is its code, and a list is
   conses ending with nil or with the dotted tail *)
let rec of_sexpr (x : Sexpr.t) : Lisp.t =
  match x.datum with
  | Int n | Char n -> Int n
  | Str s -> Str s
  | Sym s -> Sym s
  | List (xs, tail) -> List.fold_right (fun x rest -> Cons (of_sexpr x, rest)) xs (match tail with Some t -> of_sexpr t | None -> nil)
  (* the Emacs dialect of the reader makes none of these *)
  | Float _ | Bool _ | Vector _ -> raise (Error "not Emacs Lisp")

let read (s : string) (pos : int) : Lisp.t * int =
  match Sexpr_read.read Emacs s pos with
  | x, j -> (of_sexpr x, j)
  | exception Sexpr_read.Error (msg, _) -> raise (Error msg)

let read_all (s : string) : Lisp.t list =
  match Sexpr_read.read_all Emacs s with xs -> List.map of_sexpr xs | exception Sexpr_read.Error (msg, _) -> raise (Error msg)

let only_blank (s : string) (pos : int) : bool = Sexpr_read.only_blank Emacs s pos
