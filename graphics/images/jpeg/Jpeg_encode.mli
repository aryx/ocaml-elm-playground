(* Jpeg_encode: a baseline JPEG written -- Jpeg.mli's pipeline, run
 * forwards.

     RGB -> YCbCr -> subsample -> 8x8 blocks -> DCT -> quantize -> zigzag -> runs -> Huffman

   Every step but one is exact, or nearly (rounding): the **quantization**
   is where the file gets small and the picture loses, and it is all the
   encoder decides. Each of a block's 64 coefficients is divided by an
   entry of a table and rounded; the tables are the standard's own
   examples (T.81 Annex K, from Lohscheller's 1984 visibility
   experiments), large for high frequencies, larger for color:

     luminance (natural order)            chrominance
     16 11 10 16  24  40  51  61          17 18 24 47 99 99 99 99
     12 12 14 19  26  58  60  55          18 21 26 66 99 99 99 99
     14 13 16 24  40  57  69  56          24 26 56 99 99 99 99 99
     14 17 22 29  51  87  80  62          47 66 99 99 99 99 99 99
     18 22 37 56  68 109 103  77          99 99 99 99 99 99 99 99
     24 35 55 64  81 104 113  92          99 ...
     49 64 78 87 103 121 120 101
     72 92 95 98 112 100 103  99

   and **quality**, 1 to 100, is only the IJG's (libjpeg's) way of
   scaling them, which every program since has copied: a scale of
   5000 / q percent below 50, 200 - 2q above, each entry
   (entry * scale + 50) / 100 kept within 1-255. Worked example, the
   luminance's first entry, 16: quality 50 keeps it, 16; 75 halves it,
   8; 25 doubles it, 32; 100 makes every entry 1 -- the rounding of the
   DCT alone, the best a JPEG can do, and still not exact.

   The Huffman codes are the standard's example tables too (Annex K.3),
   the ones libjpeg writes unless asked to optimize: every DC size 0-11,
   every AC (run, size) pair, and end-of-block and sixteen-zeros --
   complete, so any picture can be coded without counting its symbols
   first. Counting them and building the best codes (Huffman's
   algorithm, libjpeg's -optimize) is an exercise: some 5-10% smaller.

   Written: JFIF (APP0), two quantization tables, a baseline frame of 3
   components (Y at 2 x 2 for 4:2:0, 1 x 1 for 4:4:4; Cb, Cr 1 x 1),
   four Huffman tables, one interleaved scan. The picture's edges are
   padded to whole MCUs by repeating the last row and column, as libjpeg
   does (a padding of zeros would ring into the picture). Alpha is
   dropped: JPEG has none. See notes_images.md, sections 8 and 9. *)

(* the standard's tables, natural order (row by row) *)
val luminance_table : int array
val chrominance_table : int array

(* [scaled ~quality table]: the table at that quality, the IJG's way *)
val scaled : quality:int -> int array -> int array

(* [encode img]: a JPEG of [img], at [quality] (75 by default, libjpeg's
 * too) and [subsampling] (4:2:0 by default) *)
val encode : ?quality:int -> ?subsampling:[ `S420 | `S444 ] -> Rgba_image.t -> string
