(* How the block languages look (TinyScratch, TinySnap): the blocks
 * drawn from Block_layout's pieces -- the jigsaw's notches and tabs,
 * round reporters, pointed predicates, Snap!'s grey rings -- and the
 * stage: the pen's ink, the sprites' costumes, their speech bubbles.
 *
 * A theme is a dialect's colours. Snap!'s adds its "zebra colouring"
 * (Snap! 4, 2015): a block nested in a slot of a block of its own
 * colour is drawn lighter, so that the green of three nested
 * operators still shows where each begins. *)

open Playground

type theme = { colors : Scratch_blocks.category -> float * float * float; zebra : bool }

val scratch2 : theme
val snap : theme

(* the blocks' text: its size, its width (Hershey's, the software
   renderer's font), a word from its left edge *)
val font_size : float
val measure : string -> float
val text : color -> float -> string -> float * float -> shape
val label : color -> string -> float * float -> shape

(* the pieces as shapes; [caret], the slot being typed in and what is
   typed in it *)
val pieces_shapes : theme -> ?caret:Block_layout.path * string -> Block_layout.piece list -> shape list

(* where the stage is drawn: its middle on the screen and its scale *)
type frame = { cx : float; cy : float; k : float }

val of_stage : frame -> float * float -> float * float
val to_stage : frame -> float * float -> float * float

(* a sprite's costume, facing right, in stage units round its middle
   (the cat, the pencil whose tip is its middle, Snap!'s turtle) *)
val costume : ?flip:bool -> Scratch_run.sprite -> shape list

(* the stage: white, the ink, the sprites, their bubbles *)
val stage_shapes : frame -> Scratch_run.t -> shape list

val segment : color -> float -> float * float -> float * float -> shape
