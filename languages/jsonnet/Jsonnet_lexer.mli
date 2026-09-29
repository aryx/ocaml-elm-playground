(* Jsonnet_lexer: jsonnet's text cut into tokens (the spec's "Lexing",
   https://jsonnet.org/ref/spec.html).

   Its own, not JavaScript's (Js_lexer, which Json reads with): jsonnet
   has what JavaScript has not -- # comments, text blocks between |||
   lines, verbatim strings (@'...', a quote doubled), operators made of
   any run of !$:~+-&|^=<>*/% (:: and +: among them) -- and has not
   JavaScript's regular expressions, which make a / ambiguous.

   An operator is the longest run of those characters, stopping before a
   comment, less its last characters while they are + - ~ or ! (so a*-b
   is a * -b, and {a:-1} is a : -1).

   A text block: |||, the end of the line, then lines indented as the
   first one is, that indentation taken off, up to a line indented less,
   which must be |||; |||- drops the last newline.

   Worked example (the tests'):

     local x = 'a'; { [x]+:: @'it''s' }

     Keyword local, Id x, Op =, String a, Punct ;, Punct {, Punct [,
     Id x, Punct ], Op +::, String it's, Punct }, Eof *)

type kind =
  | Keyword of string (* local, function, if, self, ... *)
  | Id of string
  | Number of float
  | String of string (* decoded *)
  | Op of string (* +, ==, ::, +:, ... *)
  | Punct of string (* { } [ ] ( ) , ; . $ *)
  | Eof

type token = { kind : kind; line : int }

exception Error of int * string

val keywords : string list
val tokenize : string -> token list
val to_string : kind -> string
