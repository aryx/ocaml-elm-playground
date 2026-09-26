(* Where blocks go: the scripts area laid out, and where a dragged
 * stack may snap.
 *
 * A block's size is what it says: the width of its words and slots
 * side by side, a slot as wide as what is typed in it or the reporter
 * dropped on it, so that blocks nest like parentheses do, and a C
 * block as tall as the stacks in its mouths. The layout is computed
 * from the program each time it is drawn -- a script is only its
 * blocks and its top-left corner, nothing about sizes is stored -- the
 * way a text editor lays out lines from the text.
 *
 * Coordinates are the Playground's, y up: a script's (x, y) is the
 * top-left of its first block, and the blocks go down from there.
 *
 *   (x, y) +-----------------+      each block below the last, the
 *          | when flag clicked|     height of the one above lower;
 *          +--+  +-----------+      a mouth [arm] to the right, as
 *          | forever         |      tall as its stack (or [arm] when
 *          |  +--------------+      empty); a bottom arm under it
 *          |  | move (10) steps
 *          |  +--------------+
 *          +-----------------+
 *
 * A place is named by a path: a script, then the blocks' indices
 * down the stacks and into the mouths, and into the arguments for a
 * slot or a reporter in one: [At 1; Mouth 0; At 0; Arg 0] is the first
 * slot of the first block in the mouth of the second block.
 *
 * Where a dragged stack snaps: below any block that has a bottom
 * (not a cap: "forever", "stop"), in any mouth, and above a script
 * that has no hat, when its top-left corner (its bottom-left, above a
 * script) is within [snap_distance] of the place; not a stack that
 * starts with a hat, which can only start a script, nor one that ends
 * with a cap anywhere a block would come after it.
 *
 * The width of a text is the caller's, [measure]: the layout knows no
 * font.
 *
 * Worked example: with characters 7 wide, "move (10) steps" is a line
 * of "move" (28), a slot for "10" (24, the narrowest a slot is),
 * "steps" (35), 4 apart: 95, and 8 on each side, a block 111 wide and
 * 28 high (the slot 18, the line padded to the least a stack block
 * is). *)

type step = At of int | Mouth of int | Arg of int
type path = { script : int; steps : step list }

type piece =
  | Body of { path : path; spec : Scratch_blocks.spec; x : float; y : float; w : float; h : float; mouths : (float * float) list (* each mouth's top and height *) }
  | Label of { x : float; y : float; text : string } (* its left, its middle *)
  | Slot of { path : path; part : Scratch_blocks.part; x : float; y : float; w : float; h : float; text : string } (* its top-left *)

type target = Below of path | Above of int | In_mouth of path * int

val arm : float
val snap_distance : float

(* a block's width and height (its C mouths' stacks included); a
   stack's height *)
val size : measure:(string -> float) -> Scratch_blocks.block -> float * float
val height : measure:(string -> float) -> Scratch_blocks.block list -> float

(* the pieces of the scripts, a parent before what is in it, and the
   places a stack can go, each with the point to match *)
val layout : measure:(string -> float) -> Scratch_blocks.script list -> piece list * (target * (float * float)) list

(* where the stack whose top-left is at [at] snaps, if anywhere *)
val snap : measure:(string -> float) -> Scratch_blocks.script list -> Scratch_blocks.block list -> at:float * float -> target option

(* the block under a point (the innermost), and the empty slot *)
val block_at : piece list -> float * float -> path option
val slot_at : piece list -> float * float -> path option
