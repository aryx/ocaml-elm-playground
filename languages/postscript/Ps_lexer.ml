(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ps_lexer.mli *)

type kind =
  | Int of int
  | Real of float
  | Name of string
  | Literal of string
  | String of string
  | Open_brace
  | Close_brace

type token = { kind : kind; start : int; stop : int }

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012' || c = '\000'
let is_delimiter c = String.contains "()<>[]{}/%" c

(* a regular token's end: up to a space or a delimiter *)
let rec word_end s i = if i < String.length s && not (is_space s.[i] || is_delimiter s.[i]) then word_end s (i + 1) else i

(* a number if it reads as one, else a name: 3, -7, .5, 1e3 *)
let number_or_name (w : string) : kind =
  match int_of_string_opt w with
  | Some n when w.[0] <> '0' || String.length w = 1 || w.[0] = '-' -> Int n
  | _ -> (
      let numeric = String.for_all (fun c -> (c >= '0' && c <= '9') || String.contains "+-.eE" c) w in
      let has_digit = String.exists (fun c -> c >= '0' && c <= '9') w in
      match float_of_string_opt w with Some f when numeric && has_digit -> Real f | _ -> Name w)

(* a string from after its "(" : balanced parentheses, and escapes *)
let string_body s i : (string * int, string) result =
  let buf = Buffer.create 16 in
  let n = String.length s in
  let rec go i depth =
    if i >= n then Error "a string with no closing )"
    else
      match s.[i] with
      | ')' when depth = 0 -> Ok (Buffer.contents buf, i + 1)
      | ')' -> Buffer.add_char buf ')'; go (i + 1) (depth - 1)
      | '(' -> Buffer.add_char buf '('; go (i + 1) (depth + 1)
      | '\\' when i + 1 < n ->
          (match s.[i + 1] with
          | 'n' -> Buffer.add_char buf '\n'
          | 't' -> Buffer.add_char buf '\t'
          | c -> Buffer.add_char buf c);
          go (i + 2) depth
      | c -> Buffer.add_char buf c; go (i + 1) depth
  in
  go i 0

let rec next (s : string) (i : int) : (token, string) result option =
  let n = String.length s in
  if i >= n then None
  else if is_space s.[i] then next s (i + 1)
  else
    let tok kind stop = Some (Ok { kind; start = i; stop }) in
    match s.[i] with
    | '%' -> ( match String.index_from_opt s i '\n' with Some j -> next s (j + 1) | None -> None)
    | '{' -> tok Open_brace (i + 1)
    | '}' -> tok Close_brace (i + 1)
    | '[' | ']' -> tok (Name (String.make 1 s.[i])) (i + 1)
    | '(' -> ( match string_body s (i + 1) with Ok (str, stop) -> tok (String str) stop | Error e -> Some (Error e))
    | '/' ->
        let j = word_end s (i + 1) in
        tok (Literal (String.sub s (i + 1) (j - i - 1))) j
    | _ ->
        let j = max (i + 1) (word_end s i) in
        tok (number_or_name (String.sub s i (j - i))) j

let tokens (s : string) : (kind list, string) result =
  let rec go i acc =
    match next s i with
    | None -> Ok (List.rev acc)
    | Some (Error e) -> Error e
    | Some (Ok t) -> go t.stop (t.kind :: acc)
  in
  go 0 []
