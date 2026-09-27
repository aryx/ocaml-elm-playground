(* Lisp_read: text into Lisp values, the reader.

   Lisp's syntax is its data's printed form, so its parser is the
   smallest there is: an atom is a number, a string or a symbol, and a
   list is "(" then values then ")". No precedence, no grammar of
   statements: that is what the parentheses buy.

       expr ::= integer | "string" | ?c | symbol
              | ( expr* ) | ( expr+ . expr )
              | ' expr          (quote expr)
              | #' expr         (function expr)

   ; starts a comment to the end of the line. Strings know \n, \t, \e
   (Escape), a backslash before a quote or a backslash, and Emacs's
   key notations: \C-a is Control-A (byte 1) and \M-x is Meta-x,
   which a terminal sends as Escape then x -- so {|"\C-x\C-s"|} is the
   two bytes of C-x C-s, what global-set-key takes. ?a is the character a, the integer 97 (?\n, ?\s for a
   space, ?\C-a).

   Worked example (in the tests):

       read {|(defun double (x) (+ x x)) ; twice|} 0
         = (defun double (x) (+ x x)), and 26, just after its last
           parenthesis *)

exception Error of string

(* [read s pos]: the value whose text starts at or after [pos] (spaces
   and comments skipped), and the position just after it; Error if
   there is none, or it is cut short *)
val read : string -> int -> Lisp.t * int

(* every value in [s], one after the other (a file of definitions) *)
val read_all : string -> Lisp.t list

(* [only_blank s pos]: whether nothing but spaces and comments is left *)
val only_blank : string -> int -> bool
