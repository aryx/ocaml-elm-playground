(* Sexpr: s-expressions as read, before a Lisp gives them a meaning.

   Every Lisp writes its programs as its data's printed form, so the
   Lisps share their syntax and differ in their values: Emacs Lisp's
   nil is false and the empty list at once, Scheme's #f and '() are
   two things; Scheme's closures and mutable pairs have no Emacs
   counterpart. So the reader is shared (Sexpr_read.mli) and gives a
   neutral tree, this one, which each dialect turns into its own
   values -- languages/lisp's Lisp_read for Emacs, languages/scheme's
   for Scheme.

   What the tree adds to the text is where each part of it came from,
   its [span]: DrScheme paints the expression that failed in pink, and
   its stepper the one about to be reduced, and both need to know which
   characters those were.

       (+ 1 2)   is   List ([Sym "+" at 1-2; Int 1 at 3-4; Int 2 at 5-6], None)
                      at 0-7 *)

(* the characters [start] to [stop] (excluded) of the text read *)
type span = { start : int; stop : int }

type t = { datum : datum; span : span }

and datum =
  | Int of int
  | Float of float
  | Str of string
  | Sym of string
  | Char of int (* Emacs's ?a, Scheme's #\a: the character's code *)
  | Bool of bool (* Scheme's #t and #f; Emacs has none *)
  | List of t list * t option (* (a b), or (a b . c) with its tail *)
  | Vector of t list (* Scheme's #(a b) *)

(* [make d span] and [sym name span] *)
val make : datum -> span -> t
val sym : string -> span -> t

(* the span of none of the text, for a tree made by a program *)
val nowhere : span

(* the tree printed back, spans forgotten, as Scheme writes it: for
   tests and error messages *)
val to_string : t -> string
