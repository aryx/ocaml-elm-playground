(* GIF: 256 colors, LZW, and animation as an afterthought.

   CompuServe, the online service, needed pictures its members'
   different computers could all show over slow modems: GIF87a (June
   1987, Steve Wilhite's team), a palette of up to 256 colors, the
   pixels compressed by LZW (Lzw.mli), rows interlaced so a modem showed
   a blurry picture first. GIF89a (1989) added the graphic control
   extension -- a transparent color, a delay, a disposal -- meant for
   slideshows; Netscape Navigator 2.0 (1995) added the looping
   extension, and the animated GIF became the web's animation format
   for the next thirty years (elm-lang.org's Mario included).

   The file (numbers little-endian, unlike PNG's):

     "GIF89a"                        or "GIF87a"
     logical screen                  width(2) height(2) flags(1)
                                     background(1) aspect(1)
     [global palette]                3 * 2^(1 + (flags land 7)) bytes,
                                     if flags land 0x80
     blocks, each starting with a byte:
       0x21 0xF9  graphic control, for the next frame: 04 packed(1)
                  delay(2, in 1/100 s) transparent index(1) 00;
                  packed = disposal (bits 2-4), transparent? (bit 0)
       0x21 ...   other extensions (comments, NETSCAPE2.0's looping,
                  ...): skipped
       0x2C       a frame: x(2) y(2) width(2) height(2) flags(1)
                  [local palette] (same rule; flags land 0x40:
                  interlaced) min_code_size(1) sub-blocks of LZW data
       0x3B       the end
     sub-blocks: (a size byte, that many bytes) until a size of 0

   The pixels are indices into a palette -- so a GIF photograph has to
   choose its 256 colors, and a sprite never notices -- and one index
   may be transparent, fully: no half-opacity (PNG's alpha is what GIF
   lacked).

   A frame is often a patch, not a full picture: mario/walk/left.gif is
   35 x 35, its first frame full, the next seven 16 x 26 or so at an
   offset, what changes around Mario's body:

          0         9                  25       35
        0 +-----------------------------------+
          |          full picture (35x35)     |
        5 |         +----------------+        |
          |         |  frame 2 patch |        |
          |         |   (16x26)      |        |
       31 |         +----------------+        |
       35 +-----------------------------------+

   Each patch is drawn over the picture so far (its transparent pixels
   letting it show through), shown for its delay, then *disposed of*
   before the next: left there (0, 1), cleared to transparent (2: the
   spec says "to the background color", browsers clear it), or put back
   as it was before the patch (3). So [animation] keeps a canvas of the
   screen's size, draws each patch on it, and takes a snapshot of it:
   one full picture a frame.

   Delays are in 1/100 s, and old GIFs (Mario's among them) say 0: taken
   literally, "as fast as possible". Browsers make a delay of 0 or 1 a
   tenth of a second, and so do we, so the native version looks like
   the web one.

   Interlacing: a frame's rows stored in four passes -- every 8th row
   from row 0, every 8th from 4, every 4th from 2, every 2nd from 1.

   Not read: the background color and the pixel aspect ratio (as
   browsers), the loop count (animations loop forever, as browsers
   do), text and plain-text extensions. The palette index past the end
   of the palette: black. A file missing its final 0x3B: accepted, as
   browsers do.

   References: CompuServe, "Graphics Interchange Format, Version 89a"
   (1990); Terry Welch, "A Technique for High-Performance Data
   Compression", IEEE Computer 17 (1984). *)

(* a frame, before composition: a patch of the picture *)
type frame = {
  x : int;
  y : int;
  (* the patch's pixels, the transparent index with alpha 0 *)
  patch : Rgba_image.t;
  (* in seconds *)
  delay : float;
  (* 0 or 1 leave the patch, 2 clear it, 3 restore what was there *)
  disposal : int;
}

(* [frames s]: the logical screen's size and the frames of the GIF file
 * [s]. Raises Failure if [s] isn't a GIF, or a corrupt one. *)
val frames : string -> (int * int) * frame list

(* [animation s]: the full pictures of the GIF file [s], composed as a
 * browser does, each with its delay in seconds *)
val animation : string -> (Rgba_image.t * float) list

(* [decode s]: the first full picture of [s], what a still viewer
 * shows *)
val decode : string -> Rgba_image.t
