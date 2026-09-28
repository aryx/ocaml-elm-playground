(* A picture as a grid of bits: one bit per dot, 1 for black and 0 for
 * white -- the Macintosh's screen (512 by 342 dots, 1984) and the
 * picture MacPaint edits (576 by 720, a page at 72 dots an inch).
 *
 * The bits of a row are packed eight to a byte, the leftmost dot in the
 * byte's highest bit, as QuickDraw laid them out:
 *
 *   dots     x = 0 1 2 3 4 5 6 7 | 8 9 ...
 *   byte 0   bit 7 6 5 4 3 2 1 0 | byte 1 ...
 *
 *   a row of 10 dots  #.##....|#.         = bytes 0xB0 0x80
 *
 * so a row takes (width + 7) / 8 bytes, its "row bytes" (QuickDraw
 * wanted that number even too, for the 68000's 16-bit words; nothing
 * here does). Packing is what made a full-screen picture fit in the
 * 128 KB Macintosh at all: 21,888 bytes, where a byte per dot would
 * have been 175,104 -- more than the machine.
 *
 * (0, 0) is the top-left dot and y goes down, as on every screen; the
 * application turns the picture over when it draws it.
 *
 * A bitmap is a value, like everything an application keeps in its
 * undo history -- but setting a dot changes it in place, since copying
 * 20 KB per dot would be absurd. The rule that reconciles the two is
 * the one Bytes and String have: change only a bitmap you have just
 * made, and hand it on unchanged ever after. [change] is that rule as a
 * function. *)

(*****************************************************************************)
(* {1 The dots} *)
(*****************************************************************************)

type t

(* all white *)
val create : width:int -> height:int -> t
val width : t -> int
val height : t -> int

(* [get b x y]: whether the dot is black; outside the picture, white *)
val get : t -> int -> int -> bool

(* [set b x y black]: in place, on a bitmap nobody else holds; outside
 * the picture, nothing *)
val set : t -> int -> int -> bool -> unit

val copy : t -> t

(* [change b f]: a copy of [b] changed by [f] -- the one way to edit a
 * bitmap that may be in a history *)
val change : t -> (t -> unit) -> t

(*****************************************************************************)
(* {1 Selections} *)
(*****************************************************************************)

(* [sub b ~x ~y ~w ~h]: the dots of a rectangle, as a bitmap of its own
 * (white where it falls outside [b]) -- what a selection lifts *)
val sub : t -> x:int -> y:int -> w:int -> h:int -> t

(* [blit ~src ~dst ~x ~y]: [src] put down on [dst] with its top-left at
 * ([x], [y]), white dots included -- MacPaint pasted opaque; in place,
 * clipped *)
val blit : src:t -> dst:t -> x:int -> y:int -> unit

(*****************************************************************************)
(* {1 Looking at it} *)
(*****************************************************************************)

(* how many dots are black, for tests *)
val count : t -> int

(* [rectangles b]: the black dots as (x, y, w, h) rectangles, for
 * drawing with a toolkit that has rectangles but no bitmaps. Each row's
 * runs of black, and a run exactly under a run of the row above makes
 * that rectangle taller, so that a solid area is one rectangle:
 *
 *   ####..      (0,0,4,2) -- rows 0 and 1 have the same run
 *   ####..
 *   ..##..      (2,2,2,1)
 *
 * A grey is the worst case: its runs are single dots that never line
 * up from one row to the next, one rectangle per black dot. *)
val rectangles : t -> (int * int * int * int) list

(*****************************************************************************)
(* {1 Saving} *)
(*****************************************************************************)

(* The picture written down, and read back: its size, then every row
 * compressed by Packbits, as MacPaint wrote its documents (which had a
 * fixed size, and 512 bytes of patterns in front instead). *)
val to_string : t -> string
val of_string : string -> t

(* the bits of a row, packed: what to_string compresses *)
val row : t -> int -> Bytes.t
