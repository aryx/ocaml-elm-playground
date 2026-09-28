(* Gap_buffer: Emacs's text, a sequence of bytes with a hole where the
   editing happens.

   A text editor inserts where the cursor is, again and again. Keeping
   the text in one array would move everything after the cursor at each
   key. The gap buffer (TECO and Emacs's, from the 1960s and 1970s; the
   GNU Emacs sources' insdel.c) keeps a hole, the gap, at the cursor:

       T h e   c a t _ _ _ _ _ _ s a t .      "The cat sat."
                     ^ gap        after it

   Typing fills the gap from its left, a byte copied and nothing moved;
   deleting backwards widens it. Moving the edit point elsewhere moves
   the gap there, copying the bytes it passes over from one side to the
   other: typing is cheap, jumping costs the distance.

   Here it is a value, since the editor's model is one (Tui.mli): two
   stacks instead of one array, the text before the gap and the text
   after it, the latter reversed, so that the byte right after the gap
   is the top of its stack:

       before: T h e _ c a t           (grows to the right)
       after:  . t a s _                (reversed: its top, the space,
                                         is the byte after the gap)

   Each stack is an array filled up to its [length]. Pushing writes past
   the end, where no version of the stack reads -- every version reads
   only its own length's worth -- so a push is done in place when the
   array's used part ends there, and on a copy otherwise (the array is
   shared with a newer version that pushed already). So an old version
   of the text stays readable, what undo and the tests need, and typing
   stays a byte written. Popping only shortens a version's length.

   Positions are counted from 0 here; Lisp's point starts at 1
   (Emacs_editor.mli adds it). *)

type t

val of_string : string -> t
val to_string : t -> string
val length : t -> int

(* [get t i]: the byte at [i], from 0 *)
val get : t -> int -> char

(* [sub t i j]: the bytes from [i] to [j], [j] excluded *)
val sub : t -> int -> int -> string

(* [insert t pos s]: [s] at [pos], the gap moved there first *)
val insert : t -> int -> string -> t

(* [delete t i j]: the bytes from [i] to [j] taken out *)
val delete : t -> int -> int -> t

(* where the gap is: the position of the last change, for the tests *)
val gap : t -> int

(* [index_from t pos c] and [rindex_before t pos c]: the first [c] at
   or after [pos], and the last one before [pos] *)
val index_from : t -> int -> char -> int option
val rindex_before : t -> int -> char -> int option
