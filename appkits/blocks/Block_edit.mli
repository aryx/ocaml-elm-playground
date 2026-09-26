(* The program changed by the mouse: a stack pulled out of a script,
 * dropped where it snaps or left alone as a script of its own; a
 * reporter pulled out of a slot, dropped in another; a slot typed in.
 *
 * Scratch's drag takes a block *and every block under it*: a stack is
 * a list, and pulling the second of five blocks leaves the first
 * where it was and carries four. That is the whole editing model --
 * no selection, no cut and paste, no text cursor in the program --
 * and why it could be learnt by eight-year-olds without a manual.
 *
 * Worked example: a script "when flag clicked, move, turn"; the move
 * taken ([At 1]) carries "move, turn" and leaves the hat alone; dropped
 * [Below] the hat, the script is as it was. *)

type dragged = Stack of Scratch_blocks.block list | Reporter of Scratch_blocks.block

(* what is at a path: a stack's block and those under it, or the
   reporter in a slot; the scripts without it (an emptied script gone,
   an emptied slot back to nothing typed) *)
val take : Scratch_blocks.script list -> Block_layout.path -> (dragged * Scratch_blocks.script list) option

(* a stack put where it snapped (above a script, the script's corner
   moving up by its height, so that what was there stays put) *)
val drop : measure:(string -> float) -> Scratch_blocks.script list -> Scratch_blocks.block list -> Block_layout.target -> Scratch_blocks.script list

(* a reporter put in a slot *)
val drop_in_slot : Scratch_blocks.script list -> Block_layout.path -> Scratch_blocks.block -> Scratch_blocks.script list

(* a script of its own, its top-left there *)
val alone : Scratch_blocks.script list -> float * float -> Scratch_blocks.block list -> Scratch_blocks.script list

(* what is typed in a slot, and set there *)
val text : Scratch_blocks.script list -> Block_layout.path -> string option
val set_text : Scratch_blocks.script list -> Block_layout.path -> string -> Scratch_blocks.script list
