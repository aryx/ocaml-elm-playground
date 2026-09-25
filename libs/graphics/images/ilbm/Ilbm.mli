(* Ilbm: the Amiga's pictures, IFF ILBM (Electronic Arts, 1985).

   IFF, the "Interchange File Format" Jerry Morrison designed at
   Electronic Arts for Deluxe Paint and Deluxe Music, is a file of
   chunks: four letters naming the chunk, its length in four bytes
   (big-endian, the 68000's order), its data, padded to an even length.
   A reader skips the chunks it doesn't know, so a format can grow
   without breaking its old readers -- the idea Microsoft's RIFF (1991:
   WAV, AVI) copied byte for byte, little-endian, and PNG's chunks after
   it:

       FORM <length> ILBM            a form, of the kind ILBM
         BMHD <20>  width, height, planes, compression...
         CMAP <3n>  the palette: red, green, blue
         CRNG <8>   a colour-cycling range (Deluxe Paint's), and more
         BODY <...> the pixels

   The pixels are an ILBM's "interleaved bitmap": not a byte a pixel
   but *bitplanes*, as the Amiga's chips read them -- the pixel's colour
   number spread over the planes, one bit in each. A picture of 32
   colours has 5 planes; each row is stored as its 5 plane-rows one
   after the other, each a bit a pixel, padded to 16 bits:

       colour 5 = 00101: plane 0 has a 1, plane 1 a 0, plane 2 a 1,
       planes 3 and 4 a 0

   Worked example (in the tests): a 16 by 1 picture, all colour 0 but
   pixel 1 of colour 5: plane 0's row is 0x40 0x00 (the second bit from
   the left), plane 1's 0x00 0x00, plane 2's 0x40 0x00.

   Each plane-row is compressed on its own with ByteRun1, which is
   MacPaint's PackBits (Packbits.mli) under another name.

   Deluxe Paint's CRNG chunk is a range of the palette that turns, a
   colour moving to the next at [rate] (16384 is 60 steps a second): the
   "colour cycling" that animated waterfalls without changing a pixel
   (Cycling.mli).

   Reference: Jerry Morrison, "EA IFF 85, Standard for Interchange
   Format Files" and "ILBM IFF Interleaved Bitmap" (Electronic Arts,
   1985). *)

type range = { low : int; high : int; rate : int; active : bool; reverse : bool }

type t = {
  width : int;
  height : int;
  planes : int; (* 2 ^ planes colours *)
  pixels : Bytes.t; (* a colour number a pixel, row by row *)
  palette : (int * int * int) array;
  ranges : range list;
}

(* how many steps a second a range turns *)
val steps_per_second : range -> float

(* [plane_rows t y]: row [y]'s plane-rows, uncompressed: the bits as
   BODY holds them *)
val plane_rows : t -> int -> Bytes.t list

val encode : t -> string

(* Failure on a file that isn't an ILBM *)
val decode : string -> t

(* the colours, through the palette *)
val to_rgba : t -> Rgba_image.t
