(* Js_lexer: JavaScript's text cut into tokens.

   The first stage of the engine (notes_javascript.md section 1):
   characters in, tokens out -- a keyword (let, function), a name (n,
   document), a number, a string (its escapes decoded), a punctuation
   or an operator. Spaces and comments (// to the end of the line,
   /* ... */) are dropped, with one exception: each token remembers
   whether a **newline** came before it, the one fact about spacing the
   grammar needs (a newline may end a statement; `return` alone on its
   line returns nothing: Js_parse.mli).

   The one real decision is the **longest match**: "===" is one token,
   not "==" then "=", and "=>" one, not "=" then ">". So the operators
   are tried three characters first, then two, then one.

   Worked example (the tests'):

     let s = "a" + 'b'; // two strings
     x=>x===1

     Keyword let  Name s  Punct =  String "a"  Punct +  String "b"  Punct ;
     Name x (a newline before)  Punct =>  Name x  Punct ===  Number 1

   Not read (the plan's out of scope): regular expression literals (a
   '/' is always division here: telling /re/ from a division needs the
   parser's help, one of JavaScript's lexing traps), template literals
   (`...`, an error saying so), BigInt (10n), numeric separators
   (1_000), and identifiers beyond ASCII letters, digits, _ and $.

   Reference: ECMAScript, section 12 (lexical grammar): 12.7 names and
   keywords, 12.8 punctuators, 12.9.3 numbers, 12.9.4 strings. *)

type kind =
  | Keyword of string (* let, const, var, function, return, if, ... *)
  | Name of string
  | Number of float
  | String of string (* decoded: "a\nb" is three characters *)
  | Punct of string (* an operator or a punctuation: "===", "{" *)
  | Eof

type token = {
  kind : kind;
  line : int; (* from 1 *)
  newline_before : bool; (* a line ended between this token and the one before *)
}

(* a mistake in the text, and its line: an unterminated string or
 * comment, a character that starts no token *)
exception Error of int * string

(* the words that are not names *)
val keywords : string list

(* the tokens of a script, ending with Eof; Error on a mistake *)
val tokenize : string -> token list

(* a token as the notes and the tests write it: Keyword let, Name s,
 * String "a" (quoted, escaped), Number 1, Punct ===, Eof *)
val to_string : kind -> string
