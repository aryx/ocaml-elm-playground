(* Scheme_step: DrScheme's stepper, Beginning Student's evaluation
   shown as algebra.

   HtDP teaches that running a program is calculating, the way a
   student simplifies (2 + 3) * 4 at school: each step replaces one
   expression, the *redex*, by a simpler one, until a value is left.
   The stepper (John Clements, Matthew Flatt and Matthias Felleisen,
   "Modeling an Algebraic Stepper", ESOP 2001) shows each step, the
   redex highlighted before (green in DrScheme) and what replaced it
   after (purple):
{|
       (define (sq x) (* x x))
       (+ (sq 3) 1)

       (+ (sq 3) 1)      -->   (+ (* 3 3) 1)       a call: the body,
           ^^^^^^                 ^^^^^^^          x replaced by 3
       (+ (* 3 3) 1)     -->   (+ 9 1)             a built-in
       (+ 9 1)           -->   10
|}

   The rules are Beginning Student's semantics, a *substitution*
   model: no store, no environment -- a function's parameters are
   replaced by the argument values in its body's text, a constant's
   name by its value, (cond [#f a] ...) loses its first clause. The
   next redex is always the leftmost innermost one not yet a value,
   which is the order the machine (Scheme_eval.mli) evaluates in, so
   the stepper and Execute agree.

   It knows Beginning Student only: define, define-struct, cond, if,
   and, or, calls, quote; no lambda, local, set! -- for those the
   substitution model needs more (the Advanced Student stepper shows
   the store too). A (big-bang ...) at the top is left out: a world
   runs in time, not in steps. *)

(* a step: the form being reduced, before and after, as text, and
   where the redex and its replacement are in each: DrScheme's two
   panes *)
type step = { before : string; redex : int * int; after : string; contractum : int * int }

(* [steps ~max text]: every step of the program [text], in order, at
   most [max] of them (1000 by default), and the error that stopped
   it, if one did (the last steps before it are kept) *)
val steps : ?max:int -> string -> step list * string option
