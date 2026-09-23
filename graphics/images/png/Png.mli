(* PNG: predict each pixel from its neighbors, then DEFLATE the
   differences.

   PNG was designed in the open, fast, for a reason: at the end of 1994
   Unisys and CompuServe announced that software writing GIFs owed a
   license for GIF's LZW compression, patented. In January 1995 Thomas
   Boutell posted a draft of a replacement on Usenet; the group that
   formed around it named it PNG ("PNG's Not GIF", officially Portable
   Network Graphics) and fixed what GIF lacked while they were at it:
   true color, 16 bits a channel, a real alpha channel, a signature
   that detects broken transfers, a CRC a chunk, and patent-free
   compression, DEFLATE (Inflate.mli). A W3C Recommendation in 1996,
   RFC 2083 in 1997, ISO/IEC 15948 in 2003; the third edition (2025)
   adds APNG, animated PNG, and HDR.

   The file (numbers big-endian, unlike GIF's):

     89 50 4E 47 0D 0A 1A 0A        the signature
     then chunks:  length(4) type(4) data(length) CRC-32(4) of type + data
       IHDR  width(4) height(4) bit depth(1) color type(1)
             compression 0, filter 0, interlace 0 or 1
       PLTE  the palette, 3 bytes a color (color type 3)
       tRNS  transparency: an alpha per palette entry, or the one gray
             or RGB value that is transparent
       IDAT  the pixels, compressed; several IDATs are one zlib stream
             (Zlib.mli), cut anywhere
       IEND  the end

   The signature is a small lesson in 1990s file transfers: 89 has the
   high bit set (a 7-bit link strips it), "PNG" for the humans, 0D 0A
   (CR LF) and 0A (LF) catch a transfer that converts line endings, 1A
   stops DOS's "type" from printing the rest. A chunk type starting with
   an upper-case letter is critical (a decoder must understand it),
   lower-case ancillary (gAMA, tEXt, ...: skip it) -- so old decoders
   read new files.

   Color types: 0 gray, 2 RGB, 3 palette, 4 gray + alpha, 6 RGBA; bit
   depths 1, 2, 4 (several pixels a byte), 8 and 16.

   The filters are PNG's real idea, before DEFLATE: each row starts with
   a byte choosing a *filter*, which replaced each byte x by its
   difference, mod 256, from a prediction made from its neighbors (the
   same channel of the pixel to the left, above, above-left):

       c  b
       a  x        0 None     x
                   1 Sub      x - a
                   2 Up       x - b
                   3 Average  x - floor ((a + b) / 2)
                   4 Paeth    x - (which of a, b, c is closest to a + b - c)

   On a gradient 10 20 30 40 50, Sub gives 10 10 10 10 10: DEFLATE sees
   one byte repeated instead of five different ones. Alan Paeth's
   predictor (1991) guesses the plane through the three neighbors, p = a
   + b - c, and takes the neighbor nearest to it (ties: a, then b, then
   c). The worked example:

     a = 100, b = 120, c = 90:  p = 130;  |p-a| = 30, |p-b| = 10,
     |p-c| = 40, so b = 120  (the values grow downward: up guesses best)

   Interlacing (Adam7, after Adam Costello, 1995): seven passes, each
   pixel of an 8 x 8 tile in the pass numbered below, each pass a small
   picture filtered and compressed on its own, so a browser drew a
   coarse picture after 1/64 of the data:

     1 6 4 6 2 6 4 6
     7 7 7 7 7 7 7 7
     5 6 5 6 5 6 5 6
     7 7 7 7 7 7 7 7
     3 6 4 6 3 6 4 6
     7 7 7 7 7 7 7 7
     5 6 5 6 5 6 5 6
     7 7 7 7 7 7 7 7

   Read here: every color type, bit depth and filter, Adam7, tRNS; each
   chunk's CRC checked. 16-bit channels keep their high byte; 1-, 2-
   and 4-bit gray is scaled to 0..255 (2-bit 3 is 255). Ignored, as
   stb_image ignores them: color management (gAMA, cHRM, sRGB, iCCP),
   the background (bKGD), text, APNG's extra frames (the default image
   is shown).

   References: Portable Network Graphics (PNG) Specification (W3C,
   second edition 2003 = ISO/IEC 15948); Alan Paeth, "Image File
   Compression Made Easy", in Graphics Gems II (1991); Greg Roelofs,
   PNG: The Definitive Guide (O'Reilly, 1999). *)

(* the 8 bytes every PNG file starts with *)
val signature : string

(* [decode s]: the picture of the PNG file [s]. Raises Failure on
 * anything wrong with it (the signature, a CRC, the compressed data, an
 * unknown critical chunk, a palette index out of the palette, ...). *)
val decode : string -> Rgba_image.t

(* [chunks s]: the chunks of [s], (type, data), up to IEND, their CRCs
 * checked *)
val chunks : string -> (string * string) list

(* [paeth a b c]: Paeth's prediction, from left, up and up-left *)
val paeth : int -> int -> int -> int
