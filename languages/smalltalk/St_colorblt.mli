(* St_colorblt: BitBlt in colour, Squeak's (Dan Ingalls again, 1996:
   "Back to the Future", section "BitBlt"; notes_squeak.md section 3).

   St_bitblt's Form has one bit a pixel. Squeak's has a depth, the
   bits of a pixel, its fourth field (nil, or none: 1):

     depth 1    a bit, 1 black                 rows padded to 16 bits
     depth 8    a byte, a colour's number in   rows padded to 32 bits
                a palette (Color.st's: 0
                transparent, then 6 by 6 by 6
                reds, greens and blues)
     depth 32   four bytes: alpha, red,        a row 4 * width bytes
                green, blue; alpha 0
                transparent, 255 opaque

   BitBlt is the same primitive, the same fourteen fields, a pixel now
   a number of 8 or 32 bits instead of one bit. Three things are new:

   - the 16 rules work on every bit of the two pixels: rule 3 still
     stores, rule 6 still reverses. And rules past 15, which are no
     longer functions of bits but arithmetic on a pixel's parts:

       rule 24  alpha blend: the source over the destination, as much
                as the source's alpha says (depth 32 only). For each
                of red, green and blue, a the source's alpha:

                  result = (s * a + d * (255 - a) + 127) / 255

                red (255, 0, 0) of alpha 128 over white: 255, then
                (0 * 128 + 255 * 127 + 127) / 255 = 127 twice: pink.
                The result's alpha: a + d's alpha * (255 - a) / 255.
       rule 25  paint: the source where it is not 0, the destination
                where it is -- a sprite, 0 its transparent colour.

   - the colour map, a fifteenth field: a table (a ByteArray, an entry
     for each value a source pixel may have, each a destination's
     pixel: 1 byte, 4 at depth 32) through which every source pixel
     goes first. It is how a Form of one depth is drawn on a Form of
     another, and how text gets a colour: the glyphs have one bit a
     pixel, the map says what 0 and 1 become -- nothing, and the ink.
     The source of a map has a depth of 1 or 8 (256 entries at most);
     with no map, the two Forms have the same depth.

   - the halftone is a Form of the destination's depth, of any size,
     repeated over it: a single pixel is a plain colour, and with no
     source, rule 3 fills a rectangle with it, rule 24 tints it.

   And the rectangle is clipped to the source too: what is outside the
   source Form is not drawn (St_bitblt reads white there).

   [blit ~simple:true] is the definition, a pixel at a time. What runs
   does the cases that are most of a screen's drawing faster: a
   rectangle filled with one colour and a Form stored as it is, a row
   at a time; a glyph, whose zeros are skipped a byte at a time. A
   test checks that they agree.

   A pixel of 32 bits is an OCaml int: negative under js_of_ocaml,
   whose ints have 32 bits, so the code only shifts and masks it, and
   never compares two of them. *)

type oop = St_memory.oop

(* a Form's bits (each row [stride] bytes), width, height and depth *)
type form = { bits : Bytes.t; w : int; h : int; stride : int; depth : int }

(* the bytes of a row: [stride ~depth w] *)
val stride : depth:int -> int -> int

(* a pixel read and written, at any depth; inside the Form *)
val get : form -> int -> int -> int
val put : form -> int -> int -> int -> unit

(* [combine ~rule ~depth s d]: the source's pixel on the
 * destination's. Rules 0 to 15, 24 (depth 32) and 25. *)
val combine : rule:int -> depth:int -> int -> int -> int

(* as St_bitblt.blit: the destination's pixels of the rectangle (x0 and
 * y0 in, x1 and y1 out; inside the Form, and what (sx, sy) puts on it
 * inside the source), each combined with the source's pixel -- through
 * [map], an entry a pixel, if any; no source, all ones -- and-ed with
 * the halftone's. [~simple]: a pixel at a time, the definition. *)
val blit :
  ?simple:bool ->
  dest:form ->
  source:form option ->
  map:int array option ->
  halftone:form option ->
  rule:int ->
  dx:int ->
  dy:int ->
  sx:int ->
  sy:int ->
  int * int * int * int ->
  unit

(* the primitive 96, copyBits, of a system whose Forms may have a
 * depth: St_bitblt's when every Form has one bit a pixel, no map and
 * a rule under 16. False if a field is not what it should, or the
 * depths do not go together. *)
val copy_bits : St_memory.t -> oop -> bool

(* a Form's width, height and pixels, for the host: red, green, blue
 * and alpha, each 0 to 255, at any depth -- a bit black or white, a
 * number of the palette looked up *)
val form : St_memory.t -> oop -> (int * int * (int -> int -> int * int * int * int)) option
