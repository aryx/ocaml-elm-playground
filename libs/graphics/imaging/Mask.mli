(* Mask: a selection, how much of each pixel is chosen.

   A byte a pixel: 255 chosen, 0 not, and between them partly -- the
   anti-aliased edge of an ellipse, a feathered selection's soft
   border. Every operation of an editor is then applied through the
   mask (Composite.through): the new pixels where it is 255, the old
   where it is 0, mixed between. Photoshop 1.0 already kept selections
   this way, which is what made its feathering possible where
   MacPaint's selections were only in or out.

   The ways of choosing, Photoshop's selection tools:
   - rectangle and ellipse (the marquee): the ellipse's edge pixels
     covered in part, measured by sampling each at 4 by 4 points;
   - polygon (the lasso): the pixels whose centre is inside, by the
     even-odd rule -- a ray from the point crossing the outline an odd
     number of times;
   - wand (the magic wand): the pixels whose colour is within
     [tolerance] of the clicked one, reached from it through such
     pixels (contiguous, a flood fill) or anywhere in the picture;
   - feather: the mask blurred (Gaussian.mli), its edge made a ramp.

   And combined: union (Shift held), subtract (Option), intersect.

   Worked example (in the tests): on a 4 by 4 image, the rectangle
   from (1, 1) to (3, 3) (the end excluded) chooses 4 pixels; its
   inverse the other 12. *)

type t = { width : int; height : int; alpha : Bytes.t }

val empty : int -> int -> t
val all : int -> int -> t
val get : t -> int -> int -> int

(* [rectangle w h (x0, y0) (x1, y1)]: from the first corner, included,
   to the second, excluded, in either order *)
val rectangle : int -> int -> int * int -> int * int -> t

(* [ellipse w h (x0, y0) (x1, y1)]: inside that box *)
val ellipse : int -> int -> int * int -> int * int -> t

val polygon : int -> int -> (float * float) list -> t
val wand : ?contiguous:bool -> tolerance:int -> Pixels.image -> int -> int -> t
val feather : radius:float -> t -> t
val invert : t -> t
val union : t -> t -> t
val subtract : t -> t -> t
val intersect : t -> t -> t
val is_empty : t -> bool

(* the smallest rectangle holding every chosen pixel, (x0, y0, x1, y1)
   the end excluded; None for an empty mask *)
val bounds : t -> (int * int * int * int) option

(* the marching ants: the edges between a chosen pixel (128 or more)
   and one not, as unit segments ((x0, y0), (x1, y1)) on the pixels'
   corners *)
val edges : t -> ((int * int) * (int * int)) list
