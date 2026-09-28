(* Pixels: reading and writing an Rgba_image's pixels, what every
   operation of this library does, and making the new image it
   returns.

   A pixel is four bytes, red green blue alpha, at 4 (y width + x):

       x = 0     x = 1           the row y, then the next
       R G B A   R G B A   ...

   An operation never changes the image it is given: it copies it and
   changes the copy, so that the old one stays -- an editor's undo is
   the image kept -- and a new picture is a new value, which is how the
   playground's bitmap shape knows to draw it again. *)

type image = Rgba_image.t

val copy : image -> image

(* [get img x y c]: channel [c] (0 red, 1 green, 2 blue, 3 alpha) of
   the pixel at (x, y), 0 to 255; outside the image, the nearest pixel
   (edges clamped: what a filter reads beyond the border) *)
val get : image -> int -> int -> int -> int

val set : image -> int -> int -> int -> int -> unit

val clamp : int -> int

(* the luminance of a colour, 0 to 255, Rec. 601's weights (299 red,
   587 green, 114 blue per thousand): the eye is most sensitive to
   green, least to blue, so pure green looks brighter than pure blue *)
val luminance : int -> int -> int -> int

(* [map f img]: each pixel replaced by [f r g b a], alpha kept by f *)
val map : (int -> int -> int -> int -> int * int * int * int) -> image -> image
