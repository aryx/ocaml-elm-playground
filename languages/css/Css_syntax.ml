(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Css_syntax.mli *)

type token =
  | Ident of string
  | Function of string
  | At_keyword of string
  | Hash of string
  | String of string
  | Url of string
  | Delim of char
  | Number of float
  | Percentage of float
  | Dimension of float * string
  | Whitespace
  | Colon
  | Semicolon
  | Comma

type component = Token of token | Block of char * component list | Func of string * component list
type declaration = { name : string; value : component list; important : bool }

type rule =
  | Style_rule of { prelude : component list; declarations : declaration list }
  | At_rule of { name : string; prelude : component list; block : component list option }

(*****************************************************************************)
(* Tokens *)
(*****************************************************************************)

(* the tokenizer's brackets, before blocks are made of them *)
type raw = T of token | Open of char | Close of char

let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\012'
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_name_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || Char.code c >= 0x80
let is_name c = is_name_start c || is_digit c || c = '-'

let utf_8 (cp : int) : string =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (if cp = 0 || cp > 0x10FFFF then Uchar.rep else Uchar.of_int cp);
  Buffer.contents b

let raw_tokens (s : string) : raw list =
  let n = String.length s in
  let out = ref [] in
  let emit t = out := t :: !out in
  let at i = if i < n then s.[i] else '\000' in
  (* a backslash's escape at [i] (the backslash): its text, and after *)
  let escape i =
    if i + 1 >= n then ("\xEF\xBF\xBD", i + 1)
    else if is_hex s.[i + 1] then (
      let j = ref (i + 1) in
      while !j < n && !j < i + 7 && is_hex s.[!j] do incr j done;
      let cp = int_of_string ("0x" ^ String.sub s (i + 1) (!j - i - 1)) in
      (* one space after a hex escape belongs to it *)
      let j = if !j < n && is_space s.[!j] then !j + 1 else !j in
      (utf_8 cp, j))
    else (String.make 1 s.[i + 1], i + 2)
  in
  let valid_escape i = at i = '\\' && at (i + 1) <> '\n' && i + 1 < n in
  let starts_name i =
    match at i with
    | '-' -> is_name_start (at (i + 1)) || at (i + 1) = '-' || valid_escape (i + 1)
    | '\\' -> valid_escape i
    | c -> is_name_start c
  in
  let starts_number i =
    match at i with
    | '+' | '-' -> is_digit (at (i + 1)) || (at (i + 1) = '.' && is_digit (at (i + 2)))
    | '.' -> is_digit (at (i + 1))
    | c -> is_digit c
  in
  let name i =
    let b = Buffer.create 16 in
    let rec go i =
      if i < n && is_name s.[i] then (Buffer.add_char b s.[i]; go (i + 1))
      else if valid_escape i then (let e, j = escape i in Buffer.add_string b e; go j)
      else i
    in
    let j = go i in
    (Buffer.contents b, j)
  in
  let number i =
    let j = ref i in
    if at !j = '+' || at !j = '-' then incr j;
    while is_digit (at !j) do incr j done;
    if at !j = '.' && is_digit (at (!j + 1)) then (
      incr j;
      while is_digit (at !j) do incr j done);
    if (at !j = 'e' || at !j = 'E') && (is_digit (at (!j + 1)) || ((at (!j + 1) = '+' || at (!j + 1) = '-') && is_digit (at (!j + 2)))) then (
      j := !j + 2;
      while is_digit (at !j) do incr j done);
    (float_of_string (String.sub s i (!j - i)), !j)
  in
  let str i q =
    let b = Buffer.create 16 in
    let rec go i =
      if i >= n then (T (String (Buffer.contents b)), i)
      else if s.[i] = q then (T (String (Buffer.contents b)), i + 1)
      (* a newline in a string: the string is bad, dropped *)
      else if s.[i] = '\n' then (T Whitespace, i)
      else if s.[i] = '\\' then
        if at (i + 1) = '\n' then go (i + 2) else (let e, j = escape i in Buffer.add_string b e; go j)
      else (Buffer.add_char b s.[i]; go (i + 1))
    in
    go (i + 1)
  in
  (* url(...) unquoted, after its "(": up to ")", spaces around dropped *)
  let url i =
    let b = Buffer.create 32 in
    let rec skip i = if i < n && is_space s.[i] then skip (i + 1) else i in
    let rec go i =
      if i >= n || s.[i] = ')' then (T (Url (Buffer.contents b)), min n (i + 1))
      (* spaces only before the ")": skipped *)
      else if is_space s.[i] then go (skip i)
      else if valid_escape i then (let e, j = escape i in Buffer.add_string b e; go j)
      else (Buffer.add_char b s.[i]; go (i + 1))
    in
    go (skip i)
  in
  let rec go i =
    if i >= n then ()
    else
      let c = s.[i] in
      if c = '/' && at (i + 1) = '*' then
        let rec close j = if j + 1 >= n then n else if s.[j] = '*' && s.[j + 1] = '/' then j + 2 else close (j + 1) in
        go (close (i + 2))
      else if is_space c then (
        let j = ref i in
        while !j < n && is_space s.[!j] do incr j done;
        emit (T Whitespace);
        go !j)
      else if c = '"' || c = '\'' then (let t, j = str i c in emit t; go j)
      else if c = '#' && (is_name (at (i + 1)) || valid_escape (i + 1)) then (let h, j = name (i + 1) in emit (T (Hash h)); go j)
      else if c = '@' && starts_name (i + 1) then (let a, j = name (i + 1) in emit (T (At_keyword (String.lowercase_ascii a))); go j)
      else if starts_number i then (
        let f, j = number i in
        if starts_name j then (let u, k = name j in emit (T (Dimension (f, String.lowercase_ascii u))); go k)
        else if at j = '%' then (emit (T (Percentage f)); go (j + 1))
        else (emit (T (Number f)); go j))
      else if starts_name i then (
        let w, j = name i in
        if at j = '(' then
          if String.lowercase_ascii w = "url" then (
            let k = ref (j + 1) in
            while !k < n && is_space s.[!k] do incr k done;
            (* url("x"): a function whose argument is a string *)
            if at !k = '"' || at !k = '\'' then (emit (T (Function w)); go (j + 1))
            else (let t, k = url (j + 1) in emit t; go k))
          else (emit (T (Function w)); go (j + 1))
        else (emit (T (Ident w)); go j))
      else if c = '<' && i + 3 < n && String.sub s i 4 = "<!--" then go (i + 4)
      else if c = '-' && i + 2 < n && String.sub s i 3 = "-->" then go (i + 3)
      else (
        (match c with
        | '(' | '[' | '{' -> emit (Open c)
        | ')' | ']' | '}' -> emit (Close c)
        | ':' -> emit (T Colon)
        | ';' -> emit (T Semicolon)
        | ',' -> emit (T Comma)
        | c -> emit (T (Delim c)));
        go (i + 1))
  in
  go 0;
  List.rev !out

let tokenize (s : string) : token list =
  List.map
    (function T t -> t | Open c -> Delim c | Close c -> Delim c)
    (raw_tokens s)

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

let closer (c : char) : char = match c with '(' -> ')' | '[' -> ']' | _ -> '}'

(* the components up to [stop] (a closing bracket) or the end: blocks
 * and functions made by matching brackets; a stray closer is a Delim *)
let rec components (toks : raw list) ~(stop : char option) : component list * raw list =
  match toks with
  | [] -> ([], [])
  | Close c :: rest when Some c = stop -> ([], rest)
  | Close c :: rest ->
      let cs, rest = components rest ~stop in
      (Token (Delim c) :: cs, rest)
  | Open c :: rest ->
      let inside, rest = components rest ~stop:(Some (closer c)) in
      let cs, rest = components rest ~stop in
      (Block (c, inside) :: cs, rest)
  | T (Function f) :: rest ->
      let args, rest = components rest ~stop:(Some ')') in
      let cs, rest = components rest ~stop in
      (Func (f, args) :: cs, rest)
  | T t :: rest ->
      let cs, rest = components rest ~stop in
      (Token t :: cs, rest)

(*****************************************************************************)
(* Rules and declarations *)
(*****************************************************************************)

let trim (cs : component list) : component list =
  let rec left = function Token Whitespace :: r -> left r | l -> l in
  List.rev (left (List.rev (left cs)))

let split_on (sep : token) (cs : component list) : component list list =
  let rec go acc cur = function
    | [] -> List.rev (List.rev cur :: acc)
    | Token t :: rest when t = sep -> go (List.rev cur :: acc) [] rest
    | c :: rest -> go acc (c :: cur) rest
  in
  go [] [] cs

let declarations_of_block (cs : component list) : declaration list =
  split_on Semicolon cs
  |> List.filter_map (fun d ->
         match trim d with
         | Token (Ident name) :: rest -> (
             match trim rest with
             | Token Colon :: value -> (
                 let value = trim value in
                 (* "! important" at its end *)
                 match List.rev value with
                 | Token (Ident imp) :: rest when String.lowercase_ascii imp = "important" -> (
                     match trim rest with
                     | Token (Delim '!') :: v -> Some { name = String.lowercase_ascii name; value = trim (List.rev v); important = true }
                     | _ -> Some { name = String.lowercase_ascii name; value; important = false })
                 | _ -> if value = [] then None else Some { name = String.lowercase_ascii name; value; important = false })
             | _ -> None)
         | _ -> None)

let rec rules_of_block (cs : component list) : rule list =
  match cs with
  | [] -> []
  | Token Whitespace :: rest -> rules_of_block rest
  | Token (At_keyword name) :: rest ->
      (* its prelude up to a ";" or a { } block *)
      let rec prelude acc = function
        | [] -> (List.rev acc, None, [])
        | Token Semicolon :: r -> (List.rev acc, None, r)
        | Block ('{', inside) :: r -> (List.rev acc, Some inside, r)
        | c :: r -> prelude (c :: acc) r
      in
      let p, block, rest = prelude [] rest in
      At_rule { name; prelude = trim p; block } :: rules_of_block rest
  | _ -> (
      (* a qualified rule: its prelude up to its { } block; none, and
       * what is left is dropped *)
      let rec prelude acc = function
        | [] -> None
        | Block ('{', inside) :: r -> Some (List.rev acc, inside, r)
        | c :: r -> prelude (c :: acc) r
      in
      match prelude [] cs with
      | Some (p, inside, rest) -> Style_rule { prelude = trim p; declarations = declarations_of_block inside } :: rules_of_block rest
      | None -> [])

let components_of (s : string) : component list = fst (components (raw_tokens s) ~stop:None)
let parse_stylesheet (s : string) : rule list = rules_of_block (components_of s)
let parse_declarations (s : string) : declaration list = declarations_of_block (components_of s)

(*****************************************************************************)
(* Back to text *)
(*****************************************************************************)

let number (f : float) : string = if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f else Printf.sprintf "%.6g" f

let rec to_string (cs : component list) : string =
  let b = Buffer.create 32 in
  let rec add (c : component) =
    match c with
    | Token t ->
        Buffer.add_string b
          (match t with
          | Ident s -> s
          | Function s -> s ^ "("
          | At_keyword s -> "@" ^ s
          | Hash s -> "#" ^ s
          | String s -> "\"" ^ String.concat "\\\"" (String.split_on_char '"' s) ^ "\""
          | Url s -> "url(" ^ s ^ ")"
          | Delim c -> String.make 1 c
          | Number f -> number f
          | Percentage f -> number f ^ "%"
          | Dimension (f, u) -> number f ^ u
          | Whitespace -> " "
          | Colon -> ":"
          | Semicolon -> ";"
          | Comma -> ",")
    | Block (o, inside) ->
        Buffer.add_char b o;
        List.iter add inside;
        Buffer.add_char b (closer o)
    | Func (name, args) ->
        Buffer.add_string b (name ^ "(");
        Buffer.add_string b (to_string args);
        Buffer.add_char b ')'
  in
  List.iter add cs;
  Buffer.contents b
