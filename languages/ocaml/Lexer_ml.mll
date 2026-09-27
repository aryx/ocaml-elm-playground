(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lexer_ml.mli.
 *
 * After ocaml-light's parsing/lexer.mll (OCaml 1.07's, Xavier Leroy),
 * with what OCaml gained since (labels, {|quoted strings|}, let*,
 * attributes, numbers with _ and suffixes), and a visualizer's
 * changes: comments are tokens, a token's value is its text, and
 * nothing raises -- an unclosed comment or string runs to the end of
 * the file, a stray character is an Error token.
 *)

{
open Token_ml

let keywords =
  [ "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done";
    "downto"; "else"; "end"; "exception"; "external"; "false"; "for";
    "fun"; "function"; "functor"; "if"; "in"; "include"; "inherit";
    "initializer"; "lazy"; "let"; "match"; "method"; "module"; "mutable";
    "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec";
    "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val";
    "virtual"; "when"; "while"; "with";
    "mod"; "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr" ]

let keyword_table : (string, unit) Hashtbl.t =
  let h = Hashtbl.create 64 in
  List.iter (fun k -> Hashtbl.replace h k ()) keywords;
  h
}

(* the \223-xxx are Latin-1's accented letters, as in OCaml 1.07 *)
let lowercase = ['a'-'z' '_' '\223'-'\246' '\248'-'\255']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let digit = ['0'-'9']
let hex = ['0'-'9' 'A'-'F' 'a'-'f']
let int_suffix = ['l' 'L' 'n']

rule token = parse
  | [' ' '\t' '\n' '\r' '\012']+ { `Space }
  | "(*" { comment 1 lexbuf; `Tok Comment }

  | "~" lowercase identchar* ':'? { `Tok Label }
  | "?" lowercase identchar* ':'? { `Tok Label }

  (* let*, and+: binding operators, one keyword *)
  | ("let" | "and") ['$' '&' '*' '+' '-' '/' '<' '=' '>' '@' '^' '|'] symbolchar* { `Tok Keyword }

  | lowercase identchar* { `Tok (if Hashtbl.mem keyword_table (Lexing.lexeme lexbuf) then Keyword else Lident) }
  | uppercase identchar* { `Tok Uident }

  | digit (digit | '_')* int_suffix?
  | '0' ['x' 'X'] hex (hex | '_')* int_suffix?
  | '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']* int_suffix?
  | '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']* int_suffix?
      { `Tok Int }
  | digit (digit | '_')* ('.' (digit | '_')*)? (['e' 'E'] ['+' '-']? digit (digit | '_')*)?
  | '0' ['x' 'X'] hex (hex | '_')* ('.' (hex | '_')*)? (['p' 'P'] ['+' '-']? digit (digit | '_')*)?
      { `Tok Float }

  | "\"" { string lexbuf; `Tok String }
  | "{" (lowercase* as delim) "|" { quoted delim lexbuf; `Tok String }

  | "'" [^ '\\' '\'' '\n' '\r'] "'"
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
  | "'\\" digit digit digit "'"
  | "'\\x" hex hex "'"
  | "'\\o" ['0'-'3'] ['0'-'7'] ['0'-'7'] "'"
      { `Tok Char }
  | "'" lowercase identchar* { `Tok Type_var }

  | "#" [' ' '\t']* digit+ [^ '\n' '\r']* { `Tok Directive }

  | "[@@@" | "[@@" | "[@" | "[%%" | "[%" | "[|" | "|]" | "[<" | "[>"
  | "(" | ")" | "[" | "]" | "{" | "}" | "," | ";" | ";;" | "'" | "#" | "`"
      { `Tok Punctuation }
  | symbolchar+ { `Tok Operator }

  | eof { `Eof }
  (* a UTF-8 character, whole, outside a string or a comment *)
  | ['\128'-'\255']+ | _ { `Tok Error }

(* after a "(*", [depth] of them open *)
and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth > 1 then comment (depth - 1) lexbuf }
  (* as OCaml's lexer: strings and characters in a comment are read as
   * such, so that "*)" in a string does not close it, nor '"' open one *)
  | "\"" { string lexbuf; comment depth lexbuf }
  | "{" (lowercase* as delim) "|" { quoted delim lexbuf; comment depth lexbuf }
  | "''"
  | "'" [^ '\\' '\'' '\n' '\r'] "'"
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
  | "'\\" digit digit digit "'"
      { comment depth lexbuf }
  | eof { () }
  | _ { comment depth lexbuf }

and string = parse
  | "\"" { () }
  | "\\" _ { string lexbuf }
  | eof { () }
  | _ { string lexbuf }

and quoted delim = parse
  | "|" (lowercase* as d) "}" { if d <> delim then quoted delim lexbuf }
  | eof { () }
  | _ { quoted delim lexbuf }

{
let tokens (src : string) : Token_ml.t list =
  let lexbuf = Lexing.from_string src in
  (* claude: lines found afterwards from the offsets, rather than by
   * counting newlines in every rule *)
  let line = ref 1 and bol = ref 0 and scanned = ref 0 in
  let place (ofs : int) : int * int =
    while !scanned < ofs do
      if src.[!scanned] = '\n' then (incr line; bol := !scanned + 1);
      incr scanned
    done;
    (!line, ofs - !bol)
  in
  let rec go acc =
    (* claude: where the last token ended, not lexeme_start after: the
     * comment and string rules, called from an action, move it *)
    let offset = Lexing.lexeme_end lexbuf in
    match token lexbuf with
    | `Eof -> List.rev acc
    | `Space -> go acc
    | `Tok kind ->
        let text = String.sub src offset (Lexing.lexeme_end lexbuf - offset) in
        let line, col = place offset in
        go ({ kind; text; offset; line; col } :: acc)
  in
  go []
}
