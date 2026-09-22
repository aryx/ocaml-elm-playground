(* A decoded picture: what the image readers of graphics/images/ give,
 * whatever the file's format (PNG, GIF, JPEG, ...) and whatever its
 * pixels were there (a palette, gray, RGB), and what the backends draw
 * from.

   width x height pixels, row by row from the top, 4 bytes per pixel,
   red, green, blue, alpha (0 = transparent, 255 = opaque; straight,
   not premultiplied), no padding between rows:

     width = 3                 rgba (row 0, then row 1):
     +-----+-----+-----+       FF 00 00 FF  00 FF 00 FF  00 00 FF FF
     | red |green|blue |       00 00 00 00  FF FF FF FF  FF FF FF 80
     +-----+-----+-----+       ^ transparent             ^ half-opaque white
     |     |white|white|
     +-----+-----+-----+

   The layout of Blit.image (graphics/core/) and Texture.image
   (graphics/3d/), and what an OpenGL texture upload wants as is (a
   Bigarray, not Bytes, for that). See notes_images.md, section 1. *)

type t = {
  width : int;
  height : int;
  rgba : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

(* [create ~width ~height]: every pixel transparent black (all 0) *)
val create : width:int -> height:int -> t
