(* Scheme_prelude: the part of the language written in itself.

   map, filter, foldl, foldr, for-each, andmap, ormap, build-list,
   sort: each takes a procedure and calls it, so written in OCaml each
   would need the machine's help to call back into Scheme; written in
   Scheme they are a few lines each, and the stepper and the Break
   button see inside them like any other code. (Racket's own are
   Racket, too.) And HtDP's names for constants: empty, true, false,
   pi. *)

(* the source, loaded by Scheme_eval.create *)
val text : string
