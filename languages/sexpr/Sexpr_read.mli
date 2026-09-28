(* Sexpr_read: text into s-expressions, for Emacs Lisp and Scheme.

   Lisp's syntax is its data's printed form, so its parser is the
   smallest there is: an atom is a number, a string or a symbol, and a
   list is "(" then values then ")". No precedence, no grammar of
   statements: that is what the parentheses buy.

       expr ::= number | "string" | symbol
              | ( expr* ) | ( expr+ . expr )
              | ' expr                          (quote expr)

   and each dialect's additions:

       Emacs Lisp   ?c                  a character, ?a ?\n ?\C-a
                    #' expr             (function expr)
                    "\C-x\M-x"          keys in strings: Control-X, then
                                        Escape and x (a terminal's Meta)
       Scheme       #t #f               the booleans (#true #false)
                    #\a #\space         a character
                    [ expr* ]           a list, as ( ) -- Racket's, and
                                        DrScheme's, for cond's clauses
                                        and let's bindings; a [ closes
                                        with a ], a ( with a )
                    #( expr* )          a vector
                    ` , ,@              quasiquote, unquote,
                                        unquote-splicing
                    #| ... |#  #;expr   a block comment (nested), and a
                                        datum commented out

   ; starts a comment to the end of the line in both. Numbers are
   integers, and in Scheme also decimals (1.5, 2e3).

   Worked example (in the tests):

       read Emacs {|(defun double (x) (+ x x)) ; twice|} 0
         = (defun double (x) (+ x x)), spanning 0-26, and 26, just
           after its last parenthesis

   References: John McCarthy, "Recursive Functions of Symbolic
   Expressions" (CACM, 1960); the GNU Emacs Lisp Reference Manual,
   "Read Syntax"; R5RS, section 7.1.2, "External representations"; the
   Racket Reference, "The Reader". *)

type dialect = Emacs | Scheme

(* what went wrong, and where: the position in the text *)
exception Error of string * int

(* [read dialect s pos]: the expression whose text starts at or after
   [pos] (spaces and comments skipped), and the position just after it;
   Error if there is none, or it is cut short *)
val read : dialect -> string -> int -> Sexpr.t * int

(* every expression in [s], one after the other (a file of
   definitions) *)
val read_all : dialect -> string -> Sexpr.t list

(* [only_blank dialect s pos]: whether nothing but spaces and comments
   is left *)
val only_blank : dialect -> string -> int -> bool
