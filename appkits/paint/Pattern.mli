(* A pattern: 8 by 8 dots, repeated like tiles over the whole picture --
 * how a screen of black and white dots painted "grey", "bricks" and
 * "sky". It is 8 bytes, one per row, the leftmost dot highest, as
 * QuickDraw stored it:
 *
 *   grey    0xAA  #.#.#.#.        a checkerboard: from arm's length,
 *           0x55  .#.#.#.#        half black is grey
 *           0xAA  #.#.#.#.
 *           ...
 *
 * The tiles are laid from the picture's own (0, 0), not from where
 * the painting starts: dot (x, y) is painted with the pattern's dot
 * (x mod 8, y mod 8). So two areas painted separately in the same
 * pattern join without a seam -- the property every paint program
 * since has kept, and the reason a pattern is not an image.
 *
 * The palette below is MacPaint's in spirit (it had 38), not its
 * exact table. *)

type t = string (* 8 bytes, a row each *)

(* whether the pattern is black at a dot of the picture *)
val black : t -> int -> int -> bool

(* [make rows]: from 8 bytes, e.g. [make [| 0xAA; 0x55; ... |]] *)
val make : int array -> t

val solid : t
val white : t
val grey : t

(* the palette, in the order it is shown *)
val palette : t list
