(* Indexed: a picture of colour numbers.

   The Amiga (1985) had no room for a colour a pixel: its pictures of
   320 by 200 held, for each pixel, the *number* of a colour, 0 to 31,
   and a palette of 32 said what each number looked like. Painting is
   then writing numbers, and the palette is a separate thing, changed
   without touching the picture -- which is what Deluxe Paint's colour
   cycling plays with (Cycling.mli).

   Every operation draws on a copy ([change]), so that an old picture
   stays as it was: the undo keeps it.

   A brush is dots of the current colour (the built-in round and square
   ones), or a piece cut from the picture, Deluxe Paint's invention:
   select a rectangle, and it becomes the brush, painted with its own
   colours, the background colour of the moment it was cut transparent
   -- so a tree drawn once on the sky, the sky's colour the background,
   is stamped into a forest.

   Symmetry, Deluxe Paint's too: each dot painted [order] times, turned
   around a centre by 360 / order degrees, a kaleidoscope.

   Worked example (in the tests): the line from (0, 0) to (4, 2) is the
   five dots (0, 0) (1, 1) (2, 1) (3, 2) (4, 2), Bresenham's (1965) --
   at x 1 and x 3 the true line passes exactly between two dots, and
   this one takes the lower, y down; a
   fill from a dot fills the dots of the same number that touch it,
   side by side, and stops at any other. *)

type t = { width : int; height : int; pixels : Bytes.t }

val create : int -> int -> int -> t
val get : t -> int -> int -> int

(* [change t f]: a copy of [t], which [f] draws on *)
val change : t -> (t -> unit) -> t

(* the drawing, on a picture [change] gave: outside it, nothing *)
val dot : t -> int -> int -> int -> unit

(* a piece, and its colour that is not painted *)
type brush = Dots of (int * int) list | Piece of t * int

val round : int -> brush
val square : int -> brush

(* [stamp t brush colour (x, y)]: the brush centred there *)
val stamp : t -> brush -> int -> int * int -> unit

val line_dots : int * int -> int * int -> (int * int) list

(* the brush stamped along a line *)
val line : t -> brush -> int -> int * int -> int * int -> unit

val frame_rect : t -> int -> int * int -> int * int -> unit
val fill_rect : t -> int -> int * int -> int * int -> unit
val frame_ellipse : t -> int -> int * int -> int * int -> unit
val fill_ellipse : t -> int -> int * int -> int * int -> unit

(* the bucket *)
val fill : t -> int -> int * int -> unit

(* [cut t (x0, y0) (x1, y1)]: the piece of picture between the corners,
   both included, as a brush's *)
val cut : t -> int * int -> int * int -> t

(* [symmetric ~order ~centre p]: p and its turns around centre *)
val symmetric : order:int -> centre:int * int -> int * int -> (int * int) list
