(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Pascal_lexer.mli *)

type kind = Keyword of string | Name of string | Int of int | Char of char | String of string | Symbol of string | Eof
type token = { kind : kind; line : int; col : int }

exception Error of int * int * string

let keywords =
  [ "and"; "array"; "begin"; "case"; "const"; "div"; "do"; "downto"; "else"; "end"; "for"; "forward"; "function"; "if"; "mod";
    "not"; "of"; "or"; "procedure"; "program"; "record"; "repeat"; "then"; "to"; "type"; "until"; "var"; "while" ]

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'

let tokens (text : string) : token list =
  let n = String.length text in
  (* where we are, and the line and column of it *)
  let i = ref 0 and line = ref 1 and bol = ref 0 in
  let col () = !i - !bol + 1 in
  let advance () =
    if text.[!i] = '\n' then begin
      incr line;
      bol := !i + 1
    end;
    incr i
  in
  let peek k = if !i + k < n then text.[!i + k] else '\000' in
  let acc = ref [] in
  let emit kind l c = acc := { kind; line = l; col = c } :: !acc in
  while !i < n do
    let l = !line and c = col () in
    let ch = text.[!i] in
    if ch = ' ' || ch = '\t' || ch = '\n' || ch = '\r' then advance ()
    else if ch = '{' then begin
      while !i < n && text.[!i] <> '}' do
        advance ()
      done;
      if !i >= n then raise (Error (l, c, "comment not closed")) else advance ()
    end
    else if ch = '(' && peek 1 = '*' then begin
      advance ();
      advance ();
      while !i < n && not (text.[!i] = '*' && peek 1 = ')') do
        advance ()
      done;
      if !i >= n then raise (Error (l, c, "comment not closed"))
      else begin
        advance ();
        advance ()
      end
    end
    else if is_letter ch then begin
      let start = !i in
      while !i < n && (is_letter text.[!i] || is_digit text.[!i]) do
        advance ()
      done;
      let word = String.lowercase_ascii (String.sub text start (!i - start)) in
      emit (if List.mem word keywords then Keyword word else Name word) l c
    end
    else if is_digit ch then begin
      let start = !i in
      while !i < n && is_digit text.[!i] do
        advance ()
      done;
      match int_of_string_opt (String.sub text start (!i - start)) with
      | Some k when k <= 32767 -> emit (Int k) l c
      | _ -> raise (Error (l, c, "integer constant out of range (32767 at most)"))
    end
    else if ch = '\'' then begin
      (* a quote doubled is a quote *)
      let b = Buffer.create 16 in
      advance ();
      let rec go () =
        if !i >= n || text.[!i] = '\n' then raise (Error (l, c, "string not closed"))
        else if text.[!i] = '\'' && peek 1 = '\'' then begin
          Buffer.add_char b '\'';
          advance ();
          advance ();
          go ()
        end
        else if text.[!i] = '\'' then advance ()
        else begin
          Buffer.add_char b text.[!i];
          advance ();
          go ()
        end
      in
      go ();
      let s = Buffer.contents b in
      emit (if String.length s = 1 then Char s.[0] else String s) l c
    end
    else
      let two = String.init 2 (fun k -> peek k) in
      if List.mem two [ ":="; "<="; ">="; "<>"; ".." ] then begin
        advance ();
        advance ();
        emit (Symbol two) l c
      end
      else if String.contains "+-*/=<>()[].,;:^" ch then begin
        advance ();
        emit (Symbol (String.make 1 ch)) l c
      end
      else raise (Error (l, c, Printf.sprintf "unexpected character %C" ch))
  done;
  List.rev ({ kind = Eof; line = !line; col = col () } :: !acc)

let show (k : kind) : string =
  match k with
  | Keyword w | Name w -> w
  | Int n -> string_of_int n
  | Char c -> Printf.sprintf "'%c'" c
  | String s -> "'" ^ s ^ "'"
  | Symbol s -> "'" ^ s ^ "'"
  | Eof -> "the end of the text"
