(* Undo: the states before, to go back to.

   A puzzle is solved by trying: a push too far, and the level can't be
   solved anymore; undo, and try another way. In a pure program, undo is
   free: every state is a value, nothing is ever changed in place, so
   keeping the old ones is keeping a list -- the Elm way, where an
   editor's undo is the list of past models. Programs that change their
   state in place must record what each change did, to undo it (the
   "command" pattern); here there's nothing to undo, only a state to go
   back to.

        record c    [c; b; a]  ->  undo  ->  [b; a]  ->  undo  ->  [a]
        (now first)                                     (the start stays)

   Part of the puzzle kit (gamekits/puzzle/), with Push.mli; used by
   games/TinySokoban and games/TinyBabaIsYou. *)

(* the state now, and the ones before it, the last one first *)
type 'a t = { now : 'a; past : 'a list }

(* a history starting with this state *)
val start : 'a -> 'a t

(* [record a h]: [a] the state now, the old one remembered *)
val record : 'a -> 'a t -> 'a t

(* back to the state before; at the start, unchanged. E.g. start a,
 * record b, record c, undo: b now, a before it. *)
val undo : 'a t -> 'a t
