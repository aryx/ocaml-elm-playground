(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Js_lexer.mli *)

type kind = Keyword of string | Name of string | Number of float | String of string | Punct of string | Eof
type token = { kind : kind; line : int; newline_before : bool }

exception Error of int * string

let keywords =
  (* not "of": a name everywhere but in for (x of xs), where the parser
   * looks for it *)
  [ "let"; "const"; "var"; "function"; "return"; "if"; "else"; "while"; "for"; "in"; "break"; "continue";
    "throw"; "try"; "catch"; "finally"; "new"; "typeof"; "true"; "false"; "null"; "this"; "class"; "delete";
    "do"; "switch"; "case"; "default"; "void"; "instanceof" ]

(* the operators, the longest first: the longest match *)
let puncts3 = [ "==="; "!=="; "..."; "**=" ]

let puncts2 =
  [ "=="; "!="; "<="; ">="; "&&"; "||"; "=>"; "++"; "--"; "+="; "-="; "*="; "/="; "%="; "**"; "??"; "?." ]

let puncts1 = "{}()[];,.<>+-*/%=!?:&|^~"

let is_digit c = c >= '0' && c <= '9'
let is_name_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
let is_name_char c = is_name_start c || is_digit c

(* a code point as UTF-8's bytes: a string's \u escape *)
let utf_8 (cp : int) : string =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int cp);
  Buffer.contents b

let tokenize (s : string) : token list =
  let n = String.length s in
  let line = ref 1 and newline = ref false and tokens = ref [] in
  let emit kind at_line =
    tokens := { kind; line = at_line; newline_before = !newline } :: !tokens;
    newline := false
  in
  let error msg = raise (Error (!line, msg)) in
  let sub i j = String.sub s i (j - i) in
  let rec go i =
    if i >= n then ()
    else
      match s.[i] with
      | '\n' ->
          incr line;
          newline := true;
          go (i + 1)
      | ' ' | '\t' | '\r' -> go (i + 1)
      | '/' when i + 1 < n && s.[i + 1] = '/' ->
          let rec eol j = if j >= n || s.[j] = '\n' then j else eol (j + 1) in
          go (eol i)
      | '/' when i + 1 < n && s.[i + 1] = '*' ->
          let start = !line in
          let rec close j =
            if j + 1 >= n then raise (Error (start, "a comment /* never closed"))
            else if s.[j] = '*' && s.[j + 1] = '/' then j + 2
            else (
              if s.[j] = '\n' then (
                incr line;
                newline := true);
              close (j + 1))
          in
          go (close (i + 2))
      | c when is_digit c || (c = '.' && i + 1 < n && is_digit s.[i + 1]) -> go (number i)
      | c when is_name_start c ->
          let j = ref i in
          while !j < n && is_name_char s.[!j] do incr j done;
          let w = sub i !j in
          emit (if List.mem w keywords then Keyword w else Name w) !line;
          go !j
      | ('"' | '\'') as q -> go (string i q)
      | '`' -> error "template literals are not supported here (an exercise: notes_javascript.md)"
      | _ -> (
          let starts p = i + String.length p <= n && sub i (i + String.length p) = p in
          match List.find_opt starts (puncts3 @ puncts2) with
          | Some p ->
              emit (Punct p) !line;
              go (i + String.length p)
          | None ->
              if String.contains puncts1 s.[i] then (
                emit (Punct (String.make 1 s.[i])) !line;
                go (i + 1))
              else error (Printf.sprintf "unexpected character %C" s.[i]))
  (* digits, a fraction, an exponent; or 0x and hexadecimal digits *)
  and number i =
    if i + 1 < n && s.[i] = '0' && (s.[i + 1] = 'x' || s.[i + 1] = 'X') then (
      let j = ref (i + 2) in
      while !j < n && (is_digit s.[!j] || (Char.lowercase_ascii s.[!j] >= 'a' && Char.lowercase_ascii s.[!j] <= 'f')) do
        incr j
      done;
      emit (Number (float_of_int (int_of_string (sub i !j)))) !line;
      !j)
    else
      let j = ref i in
      let digits () = while !j < n && is_digit s.[!j] do incr j done in
      digits ();
      if !j < n && s.[!j] = '.' then (
        incr j;
        digits ());
      if !j < n && (s.[!j] = 'e' || s.[!j] = 'E') then (
        incr j;
        if !j < n && (s.[!j] = '+' || s.[!j] = '-') then incr j;
        digits ());
      if !j < n && is_name_start s.[!j] then error (Printf.sprintf "a number followed by %C" s.[!j]);
      emit (Number (float_of_string (sub i !j))) !line;
      !j
  (* a string between [q]s, its escapes decoded; no newline inside *)
  and string i q =
    let b = Buffer.create 16 in
    let rec go j =
      if j >= n || s.[j] = '\n' then error "a string never closed on its line"
      else if s.[j] = q then j + 1
      else if s.[j] = '\\' && j + 1 < n then (
        match s.[j + 1] with
        | 'n' -> Buffer.add_char b '\n'; go (j + 2)
        | 't' -> Buffer.add_char b '\t'; go (j + 2)
        | 'r' -> Buffer.add_char b '\r'; go (j + 2)
        | '0' -> Buffer.add_char b '\000'; go (j + 2)
        | 'u' when j + 5 < n -> (
            match int_of_string_opt ("0x" ^ sub (j + 2) (j + 6)) with
            | Some cp -> Buffer.add_string b (utf_8 cp); go (j + 6)
            | None -> error "a \\u escape needs four hexadecimal digits")
        (* a backslash, a quote, and any other character: itself *)
        | c -> Buffer.add_char b c; go (j + 2))
      else (
        Buffer.add_char b s.[j];
        go (j + 1))
    in
    let at = !line in
    let j = go (i + 1) in
    emit (String (Buffer.contents b)) at;
    j
  in
  go 0;
  emit Eof !line;
  List.rev !tokens

let to_string (k : kind) : string =
  match k with
  | Keyword w -> "Keyword " ^ w
  | Name w -> "Name " ^ w
  | Number f -> "Number " ^ if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f
  | String s -> "String " ^ Printf.sprintf "%S" s
  | Punct p -> "Punct " ^ p
  | Eof -> "Eof"
