(* Lisp_prelude: the part of the language written in itself.

   when, unless, dolist, dotimes, push, pop: in Emacs they are macros
   of subr.el, not the evaluator's, and here too. A macro is a function
   from code to code, called with its arguments unevaluated:

       (when c a b)   expands to   (if c (progn a b))

   so a language's control structures can be added to it by a program
   -- the reason Lisp has no fixed syntax. There is no backquote in
   this reader, so the expansions are built with list and cons, the
   way macros were written before it (MacLisp, 1970s). dolist's hidden
   variable is subr.el's own name for it, --dolist-tail--: with dynamic
   scope and no gensym, a name nobody would pick is the only
   hygiene. *)

(* the source, loaded by Lisp_eval.create *)
val text : string
