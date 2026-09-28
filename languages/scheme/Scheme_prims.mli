(* Scheme_prims: the built-in procedures that are functions, values in
   and a value out -- arithmetic, lists, strings, characters, HtDP's
   structures' kin and images.

   The ones that need the machine are the machine's
   (Scheme_eval.mli): call/cc and apply, which call; display, which
   prints; random, whose seed is in the machine's state. And the ones
   that can be written in Scheme are (Scheme_prelude.mli): map,
   filter, foldl, sort...

   Numbers are integers and reals, not Racket's whole tower: no
   bignums (an integer overflows past 2^62), no exact fractions --
   (/ 1 3) is 0.333333333333333, where DrScheme says 1/3.

   An error names the procedure and what it expected, DrScheme v20x's
   way:

       (car 5)    car: expects argument of type <pair>; given 5 *)

exception Error of string

(* [apply name args]: the built-in [name] on its arguments; Error
   when they don't suit it; Not_found when it is not one of [names] *)
val apply : string -> Scheme.t list -> Scheme.t

(* every built-in here, for the global environment *)
val names : string list
