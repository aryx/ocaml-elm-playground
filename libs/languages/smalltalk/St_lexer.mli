(* St_lexer: Smalltalk-80's text cut into tokens.

   Smalltalk has almost no syntax, and its lexer shows it: names,
   keywords (a name and a colon, "at:"), binary selectors (runs of
   "+-*/\<>=~@%&?,"), literals, and a handful of punctuation. There is
   no keyword of the language itself: "self", "true", "ifTrue:" are an
   ordinary name and an ordinary keyword, which the compiler gives a
   meaning to (St_compile.mli).

   The literals (Blue Book, chapter 2):

     3  -7  16r1F  2r1010  1e10       integers, any size
     3.14  1.5e3                      floats
     $a  $                            characters (the second one a space)
     'it''s'                          strings, a quote doubled
     #foo  #at:put:  #+  #'two words' symbols
     #(1 $a 'x' foo #bar (1 2))       arrays of literals, the names
                                      inside read as symbols

   Three traps, each decided the way Smalltalk-80 did:

   - "x:=1" is the name x then an assignment, not the keyword "x:";
   - "3-4" is 3 minus 4, but "3 - -4" subtracts minus four: a minus
     sign followed by a digit is part of the number only where an
     operand is expected, i.e. not after a name, a literal, ")" or "]";
   - "_" alone is the assignment, the Alto's left arrow (its font drew
     the ASCII underscore as a left arrow, "←", also read here), so
     "a_b" assigns b to a, as it did in 1980.

   Comments are in double quotes and dropped. Every token knows where
   it starts and stops in the text, for the compiler's errors and the
   debugger's highlighting.

   Worked example (the tests'):

     x := #(1 $a) at: 2. ^x
     Name x  Assign  Array_start  Int 1  Char a  Rparen  Keyword at:
     Int 2  Period  Caret  Name x  Eof *)

type kind =
  | Name of string
  | Keyword of string (* "at:" *)
  | Binary of string (* "+", "<=", "," ... *)
  | Int of int (* in SmallInteger's range *)
  | Large of bool * int list (* negative?, the magnitude's bytes, least significant first *)
  | Float of float
  | Char of char
  | String of string
  | Symbol of string
  | Array_start (* #( *)
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Caret
  | Assign (* := or _ *)
  | Period
  | Semicolon
  | Bar
  | Colon (* before a block's argument: [:x | ...] *)
  | Eof

type token = { kind : kind; start : int; stop : int (* byte offsets, [start, stop) *) }

(* a mistake in the text at an offset: an unterminated string or
 * comment, a character that starts no token *)
exception Error of int * string

(* SmallInteger's range: 31 bits, as Squeak's, so that the same values
 * fit a 32-bit JavaScript integer once tagged (St_memory.mli) *)
val min_small : int
val max_small : int

(* the tokens of a text, ending with Eof *)
val tokenize : string -> token array

(* a token as the tests write it *)
val to_string : kind -> string

(* [small_of_bytes neg bytes]: a magnitude's bytes as an integer, if it
 * fits SmallInteger's range *)
val small_of_bytes : bool -> int list -> int option
