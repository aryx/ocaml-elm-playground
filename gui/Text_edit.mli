(* A text being edited: the piece table, a caret, a selection, and
 * every earlier version of itself (notes_gui.md section 7).
 *
 * A text editor's data structure is the whole lesson, and there are
 * three answers:
 *
 *   a string       "Hello world"      insert in the middle: copy it all
 *   a gap buffer   "Hello[    ] world"  insert at the gap: free
 *                  (Emacs, 1976; one cursor, cheap while you type in
 *                   one place, and the gap must be moved when you
 *                   click somewhere else)
 *   a piece table  the ORIGINAL text, never touched, an APPEND
 *                  buffer, only ever added to, and a list of pieces
 *                  saying what to read from where
 *
 * The piece table is the one Word used, and the one VS Code went back
 * to in 2018 (as a tree of pieces). Insert "there " after "Hello ":
 *
 *   original: "Hello world"      added: "there "
 *   pieces:   [original 0..6] [added 0..6] [original 6..11]
 *   reads as: "Hello "        + "there "  + "world"
 *
 * Nothing was copied and nothing was overwritten: an edit is a new
 * *list*, and the two buffers are shared. Which is the property worth
 * the whole structure, and the reason this module is here rather than
 * a gap buffer:
 *
 *   **undo is keeping the old list.** No inverse operations, no
 *   journal of what-was-deleted, no copying the document. The past is
 *   a list of piece lists, the future is the half you popped off, and
 *   a document is a value (which is what appkits/document is built
 *   on, and what lets Inspect scrub an application as it scrubs a
 *   game).
 *
 * The append buffer is mutable and shared by every version, and that
 * is safe for exactly one reason: it is only ever *appended to*, so
 * a piece written down by an old version still says what it said.
 * That one-way rule is what makes the rest a value.
 *
 * Worked example, the piece count -- which is the number to watch,
 * since it is what all the reading costs are in terms of:
 *
 *   of_string "Hello world"          1 piece
 *   insert "there " at 6             3 pieces  (split, then the new one)
 *   type "x" five times at the end   4 pieces, not 8: the first x
 *                                    adds one, and the four after it
 *                                    extend it instead of adding
 *   delete 5 in the middle           4 pieces
 *
 * What it deliberately does not do: coalescing several keystrokes
 * into one undo step (every edit is its own; a real editor groups a
 * word or a pause), a piece *tree* rather than a list (VS Code's,
 * which is what makes a big file's edits O(log n) rather than O(n)),
 * and anything about styles -- a run of bold is another table, and
 * that is TinyWord's problem, not this one.
 *)

type t

(* a text with one piece, a caret at 0, and no history *)
val of_string : string -> t

val to_string : t -> string
val length : t -> int

(* how many pieces the table has: the worked example above, and what
 * the tests watch *)
val pieces : t -> int

(* {1 The caret and the selection}
 *
 * A caret is a selection of length zero, so there is one idea here
 * and not two: the [anchor] is where the selection started and the
 * [caret] is where it ends -- which may be *before* the anchor, when
 * a selection was dragged backwards. *)

val caret : t -> int
val anchor : t -> int

(* the two in order, whichever way round they are *)
val range : t -> int * int
val selected : t -> string

(* [at pos t]: the caret at [pos], nothing selected *)
val at : int -> t -> t

(* [select ~anchor ~caret t]: both ends at once *)
val select : anchor:int -> caret:int -> t -> t

(* [to_ pos t]: the caret moved to [pos], the anchor left where it
 * is -- which is what shift-clicking and shift-arrows do *)
val to_ : int -> t -> t

(* {1 Editing} *)

(* [insert s t]: [s] at the caret, replacing the selection if there is
 * one *)
val insert : string -> t -> t

(* backspace and delete: with a selection, both delete it *)
val delete_backward : t -> t
val delete_forward : t -> t

(* [delete ~from ~len t]: for a caller that knows what it wants gone *)
val delete : from:int -> len:int -> t -> t

(* {1 The past} *)

(* the versions it can go back to, and forward to again *)
val undo : t -> t
val redo : t -> t
val undos : t -> int
val redos : t -> int

(* {1 What a view needs} *)

(* [lines ~width t]: the text broken into lines of at most [width]
 * characters, each with the offset it starts at -- greedy word wrap
 * (break at the last space that fits, and mid-word only when a word
 * is longer than the line), plus the breaks the text asks for itself
 * with "\n".
 *
 * Greedy is what browsers and most editors do, and it is what leaves
 * the rivers and the lonely short lines that Knuth and Plass's
 * algorithm exists to fix (appkits/typeset, and
 * examples/TypesetParagraph to see the difference). *)
val lines : width:int -> t -> (int * string) list

(* [place ~width t pos]: which line [pos] is on, and how far along it,
 * both counted in characters -- for drawing the caret *)
val place : width:int -> t -> int -> int * int

(* [offset ~width t ~line ~column]: the other way round, for a click *)
val offset : width:int -> t -> line:int -> column:int -> int
