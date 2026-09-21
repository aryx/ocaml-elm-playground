(* The versions you can go back to, and forward to again.
 *
 *   record c on [b; a]   ->  now c, past [b; a], future []
 *   undo                 ->  now b, past [a],    future [c]
 *   undo                 ->  now a, past [],     future [b; c]
 *   record d             ->  now d, past [a],    future []      <- c and b
 *                                                                  are gone
 *
 * In a language where a state is a value, undo is *keeping the old
 * ones*, and this module is a list and its mirror. That is worth
 * saying because the famous answer is the other one: the **command
 * pattern** (the Gang of Four's, 1994 -- and Smalltalk's before
 * them), where a program that changes its state in place records what
 * each change *did* and how to put it back. Every undo bug anybody
 * has ever had is in that "how to put it back": the inverse of an
 * edit is easy to write and easy to write wrong, and it has to stay
 * right as the program grows. Here there is no inverse to write.
 *
 * The two costs, since nothing is free: a version is kept whole, so
 * the memory is the sum of the versions (which is why [start ?limit]
 * caps them, as every editor does), and it only works while the
 * states really are values -- share a mutable array between two of
 * them and undo will go back to a version that has been changed
 * behind its back.
 *
 * In this repository there are three of these, and the differences
 * are the interesting part:
 *
 *   kits/puzzle/Undo     a game's: one way, no redo, no names. A
 *                        Sokoban player wants the move before, and
 *                        never a menu
 *   gui/Text_edit        a text's own, over its pieces: the same idea
 *                        specialised, so that a version costs a list
 *                        of pieces rather than a copy of the text
 *   this one             an application's: any value, both ways, with
 *                        a name per edit, because a menu says "Undo
 *                        Add Circle" and an editor's whole document
 *                        is the state
 *)

type 'a t

(* [start ?limit v]: a history holding [v], with nothing behind it.
 * [limit] is how many versions to keep (100 by default): the oldest
 * are forgotten, which is what makes an editor's memory bounded. *)
val start : ?limit:int -> 'a -> 'a t

val now : 'a t -> 'a

(* [record ?name v t]: [v] is the state now. [name] says what the edit
 * was, for the menu that offers to undo it. Recording makes the
 * future unreachable -- the branch you did not take is gone, in this
 * as in every editor that is not a version control system. *)
val record : ?name:string -> 'a -> 'a t -> 'a t

(* [amend v t]: [v] is the state now, *without* a new version -- for
 * what changes the state but is not an edit: the selection moving,
 * or the second keystroke of a word being typed, which should be
 * undone together with the first. *)
val amend : 'a -> 'a t -> 'a t

val undo : 'a t -> 'a t
val redo : 'a t -> 'a t
val can_undo : 'a t -> bool
val can_redo : 'a t -> bool

(* what the menu says: the name of the edit that undo would take back,
 * and of the one redo would put back *)
val undo_name : 'a t -> string option
val redo_name : 'a t -> string option

(* how many versions are behind and ahead *)
val undos : 'a t -> int
val redos : 'a t -> int
