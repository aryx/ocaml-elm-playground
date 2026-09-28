(* Ps_lexer: PostScript's tokens.
 *
 * A PostScript program has almost no syntax, and that is the design.
 * It is a sequence of tokens, each either data, pushed on the operand
 * stack, or a name, looked up and executed -- postfix, as a Forth
 * program (Charles Moore, 1970) and an HP calculator are, which is
 * where it comes from (Warnock's JaM at Xerox PARC, 1978, before it):
 *
 *   3 4 add 2 mul      ->  push 3, push 4, add pops them and pushes 7,
 *                          push 2, mul: 14 on the stack
 *
 * The tokens:
 *
 *   42  -7  3.14  1e3        numbers, integers and reals
 *   add  moveto  [  ]        executable names: [ and ] are names too,
 *                            delimiters that need no space around them
 *   /size                    a literal name: the name itself, pushed
 *                            (what def is given to name a value)
 *   (Hello \(world\))        a string, its parentheses balanced or
 *                            escaped
 *   {  }                     a procedure's braces: what is between
 *                            them is not executed but kept, an array
 *                            to be executed later (Ps_machine)
 *   % to the end of line     a comment
 *
 * Worked example, checked by the tests: "/sq {dup mul} def" is a
 * literal name, an open brace, two names, a close brace, a name. *)

type kind =
  | Int of int
  | Real of float
  | Name of string
  | Literal of string
  | String of string
  | Open_brace
  | Close_brace

(* a token and where it is in the text: [start] its first character,
 * [stop] the one after its last *)
type token = { kind : kind; start : int; stop : int }

(* the token starting at or after a place in the text, or None at the
 * end; Error for a string never closed *)
val next : string -> int -> (token, string) result option

(* all of them, for the tests *)
val tokens : string -> (kind list, string) result
