(* Pascal_lexer: Pascal's text cut into tokens.

   Pascal (Niklaus Wirth, ETH Zurich, 1970) was designed to be compiled
   in one pass by a small compiler, and its lexical level shows it:
   a handful of token kinds, no preprocessor, keywords reserved.

   - names and keywords are the same characters, a letter then letters
     and digits, and **case doesn't count**: Begin, BEGIN and begin are
     one keyword, WriteLn and writeln one name (kept here in lower
     case);
   - numbers are integers (no reals in this Pascal);
   - a quoted text, 'it''s' (a quote doubled inside), is a character
     when it is one character long and a string otherwise;
   - the symbols of two characters are := <= >= <> and .. (1..10), the
     others one;
   - comments are { ... } or the same between a parenthesis-star and a
     star-parenthesis, the latter for keyboards without braces (the
     CDC 6600 Wirth wrote the first compiler on had none).

   Each token keeps its line and column, from 1: the compiler stops at
   the first error and says where, and TinyTurboPascal puts the cursor
   there, as Turbo Pascal did.

   Worked example (the tests'):

     x := a[1] + 'z'; { done }

     Name x  Symbol :=  Name a  Symbol [  Int 1  Symbol ]  Symbol +
     Char z  Symbol ;

   Reference: Kathleen Jensen and Niklaus Wirth, "Pascal User Manual and
   Report" (Springer, 1974), chapter 1 of the Report. *)

type kind =
  | Keyword of string (* program, begin, if, div, and, ... *)
  | Name of string (* in lower case *)
  | Int of int
  | Char of char
  | String of string (* two characters or more, or none *)
  | Symbol of string (* := + ( .. and the others *)
  | Eof

type token = { kind : kind; line : int; col : int }

exception Error of int * int * string

(* [tokens text]: all of them, Eof last; Error at a character that
   starts none, or a comment or a string left open *)
val tokens : string -> token list

(* the reserved words, in lower case: what an editor colours *)
val keywords : string list

(* how a token is written, for error messages: "':='", "begin", "12" *)
val show : kind -> string
