(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Sexpr

(* See Sexpr_read.mli *)

type dialect = Emacs | Scheme

exception Error of string * int

(*****************************************************************************)
(* Characters *)
(*****************************************************************************)

let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r'

(* what ends a symbol or a number *)
let is_delimiter (d : dialect) (c : char) : bool =
  is_space c || c = '(' || c = ')' || c = '"' || c = ';' || c = '\''
  || (d = Scheme && (c = '[' || c = ']' || c = '`' || c = ','))

(* [escape d s i]: the character written after a backslash at [i - 1],
   and where the text goes on: \n, and Emacs's \C-a, \M-x ... *)
let rec escape (d : dialect) (s : string) (i : int) : string * int =
  let n = String.length s in
  if i >= n then raise (Error ("end of input after \\", i))
  else
    match s.[i] with
    | 'n' -> ("\n", i + 1)
    | 't' -> ("\t", i + 1)
    | 'r' -> ("\r", i + 1)
    | 'e' when d = Emacs -> ("\x1b", i + 1)
    | 's' when d = Emacs && not (i + 1 < n && s.[i + 1] = '-') -> (" ", i + 1)
    | ('C' | 'M') as m when d = Emacs && i + 1 < n && s.[i + 1] = '-' ->
        let c, j =
          if i + 2 < n && s.[i + 2] = '\\' then escape d s (i + 3)
          else if i + 2 < n then (String.make 1 s.[i + 2], i + 3)
          else raise (Error ("end of input after \\C-", i))
        in
        if m = 'M' then ("\x1b" ^ c, j)
        else
          (* Control clears the top three bits of a letter: C-a is 1, C-@
             is 0; C-? is DEL, as terminals have it *)
          let k = Char.code c.[0] in
          let code = if c = "?" then 127 else if c = " " then 0 else k land 0x1f in
          (String.make 1 (Char.chr code), j)
    | c -> (String.make 1 c, i + 1)

(*****************************************************************************)
(* Atoms *)
(*****************************************************************************)

let atom (d : dialect) (text : string) : datum =
  match int_of_string_opt text with
  | Some n when text <> "+" && text <> "-" -> Int n
  | _ -> (
      let has_digit = String.exists (fun c -> c >= '0' && c <= '9') text in
      match float_of_string_opt text with
      | Some f when d = Scheme && has_digit && not (String.contains text '_') -> Float f
      | _ -> Sym text)

(* Scheme's #\a, #\space, #\newline *)
let char_named (name : string) (pos : int) : int =
  match name with
  | "space" -> 32
  | "newline" | "linefeed" -> 10
  | "tab" -> 9
  | "nul" | "null" -> 0
  | "return" -> 13
  | _ when String.length name = 1 -> Char.code name.[0]
  | _ -> raise (Error ("bad character #\\" ^ name, pos))

(*****************************************************************************)
(* The reader *)
(*****************************************************************************)

let span start stop = { start; stop }

let rec skip (d : dialect) (s : string) (i : int) : int =
  let n = String.length s in
  if i >= n then i
  else if is_space s.[i] then skip d s (i + 1)
  else if s.[i] = ';' then match String.index_from_opt s i '\n' with Some j -> skip d s (j + 1) | None -> n
  else if d = Scheme && s.[i] = '#' && i + 1 < n && s.[i + 1] = '|' then skip d s (block_comment s (i + 2) 1)
  else if d = Scheme && s.[i] = '#' && i + 1 < n && s.[i + 1] = ';' then
    let _, j = read d s (i + 2) in
    skip d s j
  else i

(* the position after the |# closing a #| opened [depth] times *)
and block_comment (s : string) (i : int) (depth : int) : int =
  let n = String.length s in
  if i + 1 >= n then raise (Error ("end of input in a #| comment", i))
  else if s.[i] = '|' && s.[i + 1] = '#' then if depth = 1 then i + 2 else block_comment s (i + 2) (depth - 1)
  else if s.[i] = '#' && s.[i + 1] = '|' then block_comment s (i + 2) (depth + 1)
  else block_comment s (i + 1) depth

and read (d : dialect) (s : string) (pos : int) : Sexpr.t * int =
  let n = String.length s in
  let i = skip d s pos in
  let peek k = if i + k < n then Some s.[i + k] else None in
  (* 'x and its kin: (quote x), spanning the prefix too *)
  let prefixed name len =
    let v, j = read d s (i + len) in
    (make (List ([ sym name (span i (i + len)); v ], None)) (span i j), j)
  in
  if i >= n then raise (Error ("end of input", i))
  else
    match s.[i] with
    | '(' -> read_list d s i (i + 1) ')' []
    | '[' when d = Scheme -> read_list d s i (i + 1) ']' []
    | (')' | ']') as c -> raise (Error (Printf.sprintf "unexpected %c" c, i))
    | '\'' -> prefixed "quote" 1
    | '`' when d = Scheme -> prefixed "quasiquote" 1
    | ',' when d = Scheme -> if peek 1 = Some '@' then prefixed "unquote-splicing" 2 else prefixed "unquote" 1
    | '#' when d = Emacs && peek 1 = Some '\'' -> prefixed "function" 2
    | '#' when d = Scheme && peek 1 = Some '(' ->
        let v, j = read_list d s i (i + 2) ')' [] in
        let xs = match v.datum with List (xs, None) -> xs | _ -> raise (Error ("a dot in a vector", i)) in
        (make (Vector xs) v.span, j)
    | '#' when d = Scheme && peek 1 = Some '\\' ->
        (* #\a, #\(, #\space: one character, or a name *)
        let j = ref (i + 3) in
        while !j < n && not (is_delimiter d s.[!j]) do
          incr j
        done;
        if i + 2 >= n then raise (Error ("end of input after #\\", i));
        (make (Char (char_named (String.sub s (i + 2) (!j - i - 2)) i)) (span i !j), !j)
    | '"' ->
        let b = Buffer.create 16 in
        let rec go j =
          if j >= n then raise (Error ("end of input in a string", i))
          else if s.[j] = '"' then j + 1
          else if s.[j] = '\\' then begin
            let c, k = escape d s (j + 1) in
            Buffer.add_string b c;
            go k
          end
          else begin
            Buffer.add_char b s.[j];
            go (j + 1)
          end
        in
        let j = go (i + 1) in
        (make (Str (Buffer.contents b)) (span i j), j)
    | '?' when d = Emacs && i + 1 < n ->
        let c, j = if s.[i + 1] = '\\' then escape d s (i + 2) else (String.make 1 s.[i + 1], i + 2) in
        (make (Char (Char.code c.[String.length c - 1])) (span i j), j)
    | _ -> (
        let j = ref i in
        while !j < n && not (is_delimiter d s.[!j]) do
          incr j
        done;
        let text = String.sub s i (!j - i) in
        match (d, text) with
        | Scheme, ("#t" | "#true") -> (make (Bool true) (span i !j), !j)
        | Scheme, ("#f" | "#false") -> (make (Bool false) (span i !j), !j)
        | _ -> (make (atom d text) (span i !j), !j))

(* the elements read so far, backwards, until [close]; a "." before the
   last one makes it the list's tail: (a . b) *)
and read_list (d : dialect) (s : string) (start : int) (pos : int) (close : char) (acc : Sexpr.t list) : Sexpr.t * int =
  let n = String.length s in
  let i = skip d s pos in
  if i >= n then raise (Error ("end of input in a list", start))
  else if s.[i] = close then (make (List (List.rev acc, None)) (span start (i + 1)), i + 1)
  else if s.[i] = ')' || s.[i] = ']' then raise (Error (Printf.sprintf "expected a %c to close %c, found %c" close s.[start] s.[i], i))
  else if s.[i] = '.' && i + 1 < n && is_delimiter d s.[i + 1] && acc <> [] then begin
    let last, j = read d s (i + 1) in
    let j = skip d s j in
    if j < n && s.[j] = close then (make (List (List.rev acc, Some last)) (span start (j + 1)), j + 1)
    else raise (Error ("more than one value after a dot", j))
  end
  else
    let v, j = read d s i in
    read_list d s start j close (v :: acc)

let only_blank (d : dialect) (s : string) (pos : int) : bool = skip d s pos >= String.length s

let read_all (d : dialect) (s : string) : Sexpr.t list =
  let rec go pos acc = if only_blank d s pos then List.rev acc else let v, j = read d s pos in go j (v :: acc) in
  go 0 []
