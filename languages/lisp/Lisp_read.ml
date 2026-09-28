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

(*****************************************************************************)
(* Characters *)
(*****************************************************************************)

let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

(* what ends a symbol or a number *)
let is_delimiter c = is_space c || c = '(' || c = ')' || c = '"' || c = ';' || c = '\''

(* spaces and comments *)
let rec skip (s : string) (i : int) : int =
  if i >= String.length s then i
  else if is_space s.[i] then skip s (i + 1)
  else if s.[i] = ';' then match String.index_from_opt s i '\n' with Some j -> skip s (j + 1) | None -> String.length s
  else i

let only_blank s pos = skip s pos >= String.length s

(* [escape s i]: the character written after a backslash at [i - 1],
   and where the text goes on: \n, \C-a, \M-x ... *)
let rec escape (s : string) (i : int) : string * int =
  let n = String.length s in
  if i >= n then raise (Error "end of input after \\")
  else
    match s.[i] with
    | 'n' -> ("\n", i + 1)
    | 't' -> ("\t", i + 1)
    | 'r' -> ("\r", i + 1)
    | 'e' -> ("\x1b", i + 1)
    | 's' when not (i + 1 < n && s.[i + 1] = '-') -> (" ", i + 1)
    | ('C' | 'M') as m when i + 1 < n && s.[i + 1] = '-' ->
        let c, j = if i + 2 < n && s.[i + 2] = '\\' then escape s (i + 3) else if i + 2 < n then (String.make 1 s.[i + 2], i + 3) else raise (Error "end of input after \\C-") in
        if m = 'M' then ("\x1b" ^ c, j)
        else
          (* Control clears the top three bits of a letter: C-a is 1, C-@
             is 0; C-? is DEL, as terminals have it *)
          let k = Char.code c.[0] in
          let code = if c = "?" then 127 else if c = " " then 0 else k land 0x1f in
          (String.make 1 (Char.chr code), j)
    | c -> (String.make 1 c, i + 1)

(*****************************************************************************)
(* The reader *)
(*****************************************************************************)

let atom (text : string) : Lisp.t =
  match int_of_string_opt text with
  | Some n when text <> "+" && text <> "-" -> Int n
  | _ -> Sym text

let rec read (s : string) (pos : int) : Lisp.t * int =
  let n = String.length s in
  let i = skip s pos in
  if i >= n then raise (Error "end of input")
  else
    match s.[i] with
    | '(' -> read_list s (i + 1) []
    | ')' -> raise (Error "unexpected )")
    | '\'' ->
        let v, j = read s (i + 1) in
        (list [ Sym "quote"; v ], j)
    | '#' when i + 1 < n && s.[i + 1] = '\'' ->
        let v, j = read s (i + 2) in
        (list [ Sym "function"; v ], j)
    | '"' ->
        let b = Buffer.create 16 in
        let rec go j =
          if j >= n then raise (Error "end of input in a string")
          else if s.[j] = '"' then j + 1
          else if s.[j] = '\\' then begin
            let c, k = escape s (j + 1) in
            Buffer.add_string b c;
            go k
          end
          else begin
            Buffer.add_char b s.[j];
            go (j + 1)
          end
        in
        let j = go (i + 1) in
        (Str (Buffer.contents b), j)
    | '?' when i + 1 < n ->
        let c, j = if s.[i + 1] = '\\' then escape s (i + 2) else (String.make 1 s.[i + 1], i + 2) in
        (Int (Char.code c.[String.length c - 1]), j)
    | _ ->
        let j = ref i in
        while !j < n && not (is_delimiter s.[!j]) do
          incr j
        done;
        (atom (String.sub s i (!j - i)), !j)

(* the elements read so far, backwards, until ")"; a "." before the
   last one makes it the list's end instead of nil: (a . b) *)
and read_list (s : string) (pos : int) (acc : Lisp.t list) : Lisp.t * int =
  let i = skip s pos in
  if i >= String.length s then raise (Error "end of input in a list")
  else if s.[i] = ')' then (list (List.rev acc), i + 1)
  else if s.[i] = '.' && i + 1 < String.length s && is_delimiter s.[i + 1] && acc <> [] then begin
    let last, j = read s (i + 1) in
    let j = skip s j in
    if j < String.length s && s.[j] = ')' then (List.fold_left (fun rest x -> Cons (x, rest)) last acc, j + 1)
    else raise (Error "more than one value after a dot")
  end
  else
    let v, j = read s i in
    read_list s j (v :: acc)

let read_all (s : string) : Lisp.t list =
  let rec go pos acc = if only_blank s pos then List.rev acc else let v, j = read s pos in go j (v :: acc) in
  go 0 []
