(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Jsonnet_lexer.mli *)

type kind = Keyword of string | Id of string | Number of float | String of string | Op of string | Punct of string | Eof
type token = { kind : kind; line : int }

exception Error of int * string

let keywords =
  [ "assert"; "else"; "error"; "false"; "for"; "function"; "if"; "import"; "importstr"; "importbin"; "in"; "local"; "null"; "tailstrict";
    "then"; "self"; "super"; "true" ]

let to_string = function
  | Keyword k -> k
  | Id x -> x
  | Number n -> Printf.sprintf "%g" n
  | String s -> Printf.sprintf "%S" s
  | Op o | Punct o -> o
  | Eof -> "the end"

let is_op_char c = String.contains "!$:~+-&|^=<>*/%" c
let is_digit c = c >= '0' && c <= '9'
let is_id_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_id_char c = is_id_start c || is_digit c

(* a code point as UTF-8 *)
let add_utf8 (b : Buffer.t) (cp : int) : unit =
  if cp < 0x80 then Buffer.add_char b (Char.chr cp)
  else if cp < 0x800 then begin
    Buffer.add_char b (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else if cp < 0x10000 then begin
    Buffer.add_char b (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else begin
    Buffer.add_char b (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F)))
  end

let tokenize (s : string) : token list =
  let n = String.length s in
  let i = ref 0 and line = ref 1 in
  let out = ref [] in
  let emit kind l = out := { kind; line = l } :: !out in
  let peek k = if !i + k < n then s.[!i + k] else '\000' in
  let fail msg = raise (Error (!line, msg)) in
  let advance () = (if s.[!i] = '\n' then incr line); incr i in
  (* a quoted string's body, [q] its quote, escapes decoded *)
  let quoted q =
    let b = Buffer.create 16 in
    advance ();
    let fin = ref false in
    while not !fin do
      if !i >= n then fail "a string not closed";
      let c = s.[!i] in
      if c = q then (advance (); fin := true)
      else if c = '\\' then begin
        advance ();
        if !i >= n then fail "a string not closed";
        let e = s.[!i] in
        advance ();
        match e with
        | '"' | '\'' | '\\' | '/' -> Buffer.add_char b e
        | 'b' -> Buffer.add_char b '\b'
        | 'f' -> Buffer.add_char b '\012'
        | 'n' -> Buffer.add_char b '\n'
        | 'r' -> Buffer.add_char b '\r'
        | 't' -> Buffer.add_char b '\t'
        | 'u' ->
            if !i + 4 > n then fail "\\u needs four hex digits";
            let hex = String.sub s !i 4 in
            let cp = try int_of_string ("0x" ^ hex) with _ -> fail ("\\u" ^ hex) in
            i := !i + 4;
            add_utf8 b cp
        | c -> fail (Printf.sprintf "an unknown escape \\%c" c)
      end
      else (Buffer.add_char b c; advance ())
    done;
    Buffer.contents b
  in
  (* @'...': no escapes, the quote doubled *)
  let verbatim q =
    let b = Buffer.create 16 in
    advance ();
    let fin = ref false in
    while not !fin do
      if !i >= n then fail "a string not closed";
      if s.[!i] = q then if peek 1 = q then (Buffer.add_char b q; advance (); advance ()) else (advance (); fin := true)
      else (Buffer.add_char b s.[!i]; advance ())
    done;
    Buffer.contents b
  in
  (* |||: after its line, the lines indented as the first *)
  let text_block () =
    i := !i + 3;
    let chomp = peek 0 = '-' in
    if chomp then incr i;
    while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
    if !i >= n || s.[!i] <> '\n' then fail "text block: ||| must end its line";
    advance ();
    let indent_of k = let j = ref k in while !j < n && (s.[!j] = ' ' || s.[!j] = '\t') do incr j done; !j - k in
    (* blank lines before the first line keep their newline *)
    let b = Buffer.create 64 in
    while !i < n && s.[!i] = '\n' do Buffer.add_char b '\n'; advance () done;
    let ind = indent_of !i in
    if ind = 0 then fail "text block: its first line must be indented";
    let prefix = String.sub s !i ind in
    let fin = ref false in
    while not !fin do
      if !i >= n then fail "text block not closed";
      if s.[!i] = '\n' then (Buffer.add_char b '\n'; advance ())
      else if !i + ind <= n && String.sub s !i ind = prefix then begin
        i := !i + ind;
        while !i < n && s.[!i] <> '\n' do Buffer.add_char b s.[!i]; incr i done;
        if !i < n then (Buffer.add_char b '\n'; advance ())
      end
      else begin
        (* indented less: the end, ||| *)
        while !i < n && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
        if !i + 3 <= n && String.sub s !i 3 = "|||" then (i := !i + 3; fin := true) else fail "text block: a line indented less, not |||"
      end
    done;
    let text = Buffer.contents b in
    if chomp && String.length text > 0 && text.[String.length text - 1] = '\n' then String.sub text 0 (String.length text - 1) else text
  in
  while !i < n do
    let c = s.[!i] in
    let l = !line in
    if c = ' ' || c = '\t' || c = '\r' || c = '\n' then advance ()
    else if c = '#' || (c = '/' && peek 1 = '/') then (while !i < n && s.[!i] <> '\n' do incr i done)
    else if c = '/' && peek 1 = '*' then begin
      i := !i + 2;
      while !i < n && not (s.[!i] = '*' && peek 1 = '/') do advance () done;
      if !i >= n then fail "a comment not closed";
      i := !i + 2
    end
    else if is_digit c then begin
      let st = !i in
      while !i < n && is_digit s.[!i] do incr i done;
      if peek 0 = '.' && is_digit (peek 1) then (incr i; while !i < n && is_digit s.[!i] do incr i done);
      if peek 0 = 'e' || peek 0 = 'E' then begin
        incr i;
        if peek 0 = '+' || peek 0 = '-' then incr i;
        if not (is_digit (peek 0)) then fail "a number's exponent";
        while !i < n && is_digit s.[!i] do incr i done
      end;
      emit (Number (float_of_string (String.sub s st (!i - st)))) l
    end
    else if is_id_start c then begin
      let st = !i in
      while !i < n && is_id_char s.[!i] do incr i done;
      let w = String.sub s st (!i - st) in
      emit (if List.mem w keywords then Keyword w else Id w) l
    end
    else if c = '"' || c = '\'' then emit (String (quoted c)) l
    else if c = '@' && (peek 1 = '"' || peek 1 = '\'') then (incr i; emit (String (verbatim s.[!i])) l)
    else if c = '|' && peek 1 = '|' && peek 2 = '|' then emit (String (text_block ())) l
    else if String.contains "{}[](),;." c then (emit (Punct (String.make 1 c)) l; incr i)
    else if c = '$' then (emit (Punct "$") l; incr i)
    else if is_op_char c then begin
      let st = !i in
      let stop () =
        !i >= n || (not (is_op_char s.[!i])) || s.[!i] = '$'
        || (s.[!i] = '/' && (peek 1 = '/' || peek 1 = '*'))
        || (s.[!i] = '|' && peek 1 = '|' && peek 2 = '|' && !i > st)
      in
      while not (stop ()) do incr i done;
      (* less its last + - ~ ! while longer than one *)
      while !i - st > 1 && String.contains "+-~!" s.[!i - 1] do decr i done;
      emit (Op (String.sub s st (!i - st))) l
    end
    else fail (Printf.sprintf "an unexpected character %C" c)
  done;
  List.rev ({ kind = Eof; line = !line } :: !out)
