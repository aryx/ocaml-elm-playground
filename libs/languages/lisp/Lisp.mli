(* Lisp: the values of a small Emacs Lisp, and how they print.

   Lisp (John McCarthy, MIT, 1958) has one data structure for its
   programs and its data: the list, built from pairs, the "cons cells".
   A program is a list whose first element says what to do, so a
   program can build a program, read one from text (Lisp_read.mli) and
   run it (Lisp_eval.mli), and that is why an editor written in it can
   be changed while it runs, which was Emacs's point (Richard
   Stallman, "EMACS: The Extensible, Customizable Self-Documenting
   Display Editor", MIT AI Memo 519a, 1981).

       (+ 1 2)   is   Cons (Sym "+", Cons (Int 1, Cons (Int 2, nil)))

       [+|.]-->[1|.]-->[2|nil]

   The dialect is Emacs Lisp's, much reduced:
   - nil is the symbol nil, the empty list and false, all three; t is
     true, and so is anything but nil;
   - characters are integers: ?a is 97, as in Emacs;
   - a function is data too, the list (lambda (x) body...), and a
     built-in function is a [Subr], named so that it can be printed;
   - no floats, vectors or hash tables, and a cons can't be changed in
     place (no setcar): the evaluator threads its state, and a value
     is never mutated. *)

type t =
  | Int of int
  | Str of string
  | Sym of string
  | Cons of t * t
  | Subr of string (* a built-in function, by name: #<subr car> *)

val nil : t
val t : t

(* [of_bool b]: t or nil *)
val of_bool : bool -> t

(* nil is false, anything else true *)
val truthy : t -> bool

(* [list [a; b]] is (a b), and [to_list] the elements of a proper list
   (None for a dotted one, (a . b)) *)
val list : t list -> t
val to_list : t -> t list option

(* how prin1 writes a value, so that read gives it back: strings in
   quotes, (quote x) as 'x *)
val print : t -> string

(* how princ writes it, for people: strings without their quotes *)
val princ : t -> string
