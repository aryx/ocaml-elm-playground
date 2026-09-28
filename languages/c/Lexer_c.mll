(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Lexer_c.mli.
 *
 * C99's lexical grammar, with a visualizer's changes as in Lexer_ml:
 * comments are tokens, a token's value is its text, nothing raises. The
 * rules below know nothing of lines; the preprocessor's lines are the
 * driver's ([tokens]), which sees the newlines go by.
 *)

{
open Token_c

(* C99's, Plan 9's signof, and gcc's that the corpora use *)
let keywords =
  [ "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do";
    "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if";
    "inline"; "int"; "long"; "register"; "restrict"; "return"; "short";
    "signed"; "sizeof"; "static"; "struct"; "switch"; "typedef"; "union";
    "unsigned"; "void"; "volatile"; "while"; "_Bool"; "signof"; "typeof";
    "__typeof__"; "__inline"; "__inline__"; "__attribute__"; "__asm__";
    "__asm"; "asm"; "__volatile__"; "__restrict"; "__extension__";
    "__const"; "__signed__"; "__declspec" ]

(* claude: back to [pos] (an index in the buffer): where the scanning
 * goes on, and where lexeme_end says the token ended *)
let back_to (lexbuf : Lexing.lexbuf) (pos : int) : unit =
  let n = lexbuf.lex_curr_pos - pos in
  lexbuf.lex_curr_pos <- pos;
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - n }

let keyword_table : (string, unit) Hashtbl.t =
  let h = Hashtbl.create 64 in
  List.iter (fun k -> Hashtbl.replace h k ()) keywords;
  h
}

(* Plan 9's C takes UTF-8 in names: Point δ; *)
let letter = ['a'-'z' 'A'-'Z' '_' '\128'-'\255']
let ident = letter (letter | ['0'-'9'])*
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let int_suffix = ['u' 'U' 'l' 'L']*
let exponent = ['e' 'E'] ['+' '-']? digit+
let float_suffix = ['f' 'F' 'l' 'L']?
let blank = [' ' '\t' '\r' '\012' '\011']

rule token = parse
  | blank+ { `Space }
  | '\n' { `Newline }
  (* a line continued: a space, but the directive goes on *)
  | '\\' blank* '\n' { `Space }
  | "/*" { comment lexbuf; `Tok Comment }
  | "//" [^ '\n']* { `Tok Comment }

  (* a directive's start, if the driver says a line starts here *)
  | '#' blank* ident? { `Hash }
  | "##" { `Tok Operator }

  | ident { `Tok (if Hashtbl.mem keyword_table (Lexing.lexeme lexbuf) then Keyword else Ident) }

  | '0' ['x' 'X'] hex+ int_suffix
  | digit+ int_suffix
      { `Tok Int }
  | digit+ '.' digit* exponent? float_suffix
  | '.' digit+ exponent? float_suffix
  | digit+ exponent float_suffix
      { `Tok Float }

  | 'L'? '\'' { char lexbuf; `Tok Char }
  | 'L'? '"' { string lexbuf; `Tok String }

  | "..." | "<<=" | ">>=" | "->" | "++" | "--" | "<<" | ">>" | "<=" | ">="
  | "==" | "!=" | "&&" | "||" | "+=" | "-=" | "*=" | "/=" | "%=" | "&="
  | "^=" | "|=" | ['+' '-' '*' '/' '%' '<' '>' '=' '!' '~' '&' '|' '^' '?' ':' '.']
      { `Tok Operator }
  | ['(' ')' '[' ']' '{' '}' ',' ';'] { `Tok Punctuation }

  | eof { `Eof }
  (* a UTF-8 character, whole, outside a string or a comment *)
  | ['\128'-'\255']+ | _ { `Tok Error }

(* after a "/*": C's comments do not nest *)
and comment = parse
  | "*/" { () }
  | eof { () }
  | _ { comment lexbuf }

(* after a quote, to the closing one or the line's end *)
and string = parse
  | '"' | eof { () }
  | '\\' _ { string lexbuf }
  | '\n' { back_to lexbuf (lexbuf.lex_curr_pos - 1) }
  | _ { string lexbuf }

and char = parse
  | '\'' | eof { () }
  | '\\' _ { char lexbuf }
  | '\n' { back_to lexbuf (lexbuf.lex_curr_pos - 1) }
  | _ { char lexbuf }

(* after #include: <file.h>, one token *)
and include_file = parse
  | blank* ('<' [^ '>' '\n']* '>' as f) { Some f }
  | "" { None }

{
let tokens (src : string) : Token_c.t list =
  let lexbuf = Lexing.from_string src in
  (* claude: lines found afterwards from the offsets, as in Lexer_ml *)
  let line = ref 1 and bol = ref 0 and scanned = ref 0 in
  let place (ofs : int) : int * int =
    while !scanned < ofs do
      if src.[!scanned] = '\n' then (incr line; bol := !scanned + 1);
      incr scanned
    done;
    (!line, ofs - !bol)
  in
  (* in a directive; and only spaces since the last newline *)
  let pp = ref false and line_start = ref true in
  let rec go acc =
    let offset = Lexing.lexeme_end lexbuf in
    let tok kind =
      let text = String.sub src offset (Lexing.lexeme_end lexbuf - offset) in
      let line, col = place offset in
      { kind; text; offset; line; col; pp = !pp }
    in
    match token lexbuf with
    | `Eof -> List.rev acc
    | `Space -> go acc
    | `Newline ->
        pp := false;
        line_start := true;
        go acc
    | `Hash when !line_start ->
        pp := true;
        line_start := false;
        let t = tok Directive in
        let word = String.trim (String.sub t.text 1 (String.length t.text - 1)) in
        let acc = t :: acc in
        if word = "include" || word = "import" then
          match include_file lexbuf with
          | Some f ->
              let offset = Lexing.lexeme_end lexbuf - String.length f in
              let line, col = place offset in
              go ({ kind = String; text = f; offset; line; col; pp = true } :: acc)
          | None -> go acc
        else go acc
    | `Hash ->
        (* in a macro's body: # then a name, two tokens *)
        back_to lexbuf (lexbuf.lex_start_pos + 1);
        go (tok Operator :: acc)
    | `Tok kind ->
        let t = tok kind in
        if kind <> Comment then line_start := false;
        go (t :: acc)
  in
  go []
}
