(* Yuv: the pixels video is made of -- brightness apart from color, and
 * the color at a quarter of the resolution.

   The eye sees brightness finely and color coarsely. So video keeps a
   pixel not as red, green and blue but as **luma** Y, the brightness,
   and two **color differences**, Cb (blue less the brightness) and Cr
   (red less it) -- the recipe of ITU-R BT.601 (1982), digital
   television's, which JPEG took too:

     Y  =  0.299 R + 0.587 G + 0.114 B
     Cb = 128 + 0.564 (B - Y)       = 128 - 0.1687 R - 0.3313 G + 0.5 B
     Cr = 128 + 0.713 (R - Y)       = 128 + 0.5 R - 0.4187 G - 0.0813 B

   and back (Jpeg.mli's):

     R = Y + 1.402 (Cr - 128)
     G = Y - 0.344136 (Cb - 128) - 0.714136 (Cr - 128)
     B = Y + 1.772 (Cb - 128)

   Two ranges. JPEG uses all of 0-255 ([Full]); video keeps Y in
   16-235 and Cb, Cr in 16-240 ([Studio]), the room around them left
   for the analog signal's overshoots -- so black is 16, not 0, and a
   video's pixels shown as a JPEG's look washed out, the classic bug.
   Worked example, pure red (255, 0, 0):

     Full:    Y 76, Cb 85, Cr 255     dark (red is dim to the eye) and
     Studio:  Y 81, Cb 90, Cr 240     all red difference

   and white is (255, 128, 128) full, (235, 128, 128) studio.

   Then the color is **subsampled**. In **4:2:0** one Cb and one Cr
   stand for a 2 x 2 square of Y, their average:

     Y:  a b        Cb: one, (Cb(a) + Cb(b) + Cb(c) + Cb(d)) / 4
         c d        Cr: likewise

   A W x H frame is W x H bytes of Y and (W/2) x (H/2) of each color:
   1.5 bytes a pixel instead of 3 -- half the data, before any
   compression, and hard to see. Where it shows: a sharp edge between
   two colors. A 2 x 2 checker of red and blue keeps its four Y values
   (76, 76, 29, 29: the brightnesses) and becomes one color, Cb 170 and
   Cr 181, purple. Back to pixels, each color sample is copied to its
   2 x 2 square (the nearest; interpolating between samples is better
   and an exercise). **4:4:4** keeps every color sample.

   The names are the ratios of a 4-pixel-wide strip in sampling rate:
   4 of Y, then how many color samples on its first row and its second
   (4:2:2: half across; 4:2:0: half across, none on the second row).
   See notes_video.md, section 2. *)

type range = Full | Studio

(* [of_rgb range (r, g, b)]: (y, cb, cr), each rounded and clamped *)
val of_rgb : range -> int * int * int -> int * int * int

(* [to_rgb range (y, cb, cr)]: (r, g, b), each rounded and clamped *)
val to_rgb : range -> int * int * int -> int * int * int

type chroma = C420 | C444

(* a picture as three planes, each row by row from the top: Y at
 * width x height, Cb and Cr at [chroma_size] *)
type planes = { width : int; height : int; chroma : chroma; y : Bytes.t; cb : Bytes.t; cr : Bytes.t }

(* [chroma_size chroma ~width ~height]: the Cb and Cr planes' size;
 * in 4:2:0 an odd width or height rounds up (the last column or row's
 * color samples stand for one or two pixels only) *)
val chroma_size : chroma -> width:int -> height:int -> int * int

(* [of_image range chroma img]: the planes of a picture (its alpha
 * ignored: video has none) *)
val of_image : range -> chroma -> Rgba_image.t -> planes

(* [to_image range planes]: back to pixels, opaque *)
val to_image : range -> planes -> Rgba_image.t
