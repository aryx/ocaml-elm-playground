(* LZW: GIF's compression, a dictionary built while reading.

   Huffman codes (Huffman.mli) give short codes to common symbols. A
   picture repeats whole *sequences* -- a row of sky, a sprite's outline
   -- and Lempel and Ziv's second method (LZ78, 1978), made fast by
   Terry Welch at Sperry (LZW, 1984), gives a code to every sequence
   already seen. The dictionary is never sent: the decoder builds the
   same one from the codes alone. GIF (1987) took it; then Unisys, which
   Sperry had become, enforced its patent on it (1994), and the web
   answered with PNG (Png.mli). The patent expired in 2003-2004.

   GIF's variant, for pictures of up to 256 colors:

   - the dictionary starts with one entry per color (4 colors: codes 0
     to 3), then two special codes: *clear* = 1 lsl min_code_size (4:
     start again with a fresh dictionary) and *end* = clear + 1 (5);
     new entries from end + 1 (6) on;
   - codes have a variable width: min_code_size + 1 bits at first (3),
     one more when the next code to add no longer fits (after adding
     code 7, the next is 8 = 2^3), up to 12 bits (4096 entries: the
     dictionary is full, and stays so until a clear);
   - bits are packed least significant first.

   The encoder keeps the longest sequence w still in the dictionary;
   when w + the next pixel isn't, it sends w's code and adds w + pixel.
   The decoder adds each entry one step later: reading a code (but the
   first after a clear), it adds the previous code's sequence + the
   first pixel of this one. Except in one case, called KwKwK: the code
   read is the very one the encoder has just added, which the decoder
   doesn't know yet -- that only happens when the new sequence is the
   previous one plus its own first pixel, so that's what it is.

   The worked example (notes_images.md section 4), six pixels of color
   1, 4 colors, min_code_size 2:

     encoder                     emits  adds
     w = 1
     pixel 1: "11" not known     1      6 = "11"    w = 1
     pixel 1: "11" known                            w = 11
     pixel 1: "111" not known    6      7 = "111"   w = 1
     pixel 1: "11" known                            w = 11
     pixel 1: "111" known                           w = 111
     end of pixels               7
     stream: clear(4) 1 6 7 end(5)

     decoder     width  code  output   adds
     (clear)     3      1     1
                 3      6     1 1      6 = 1 + 1   (6 unknown: KwKwK)
                 3      7     1 1 1    7 = 11 + 1  (7 unknown: KwKwK)
         the next code, 8, needs 4 bits
                 4      5     (end)

   Packed least significant bit first, 4 in 3 bits, 1, 6, 7, then 5
   in 4 bits: 16 bits, the two bytes 8C 5F.

   Where LZW decoders go wrong, each worth a test: the clear code is 1
   lsl min_code_size (imagelib, the pure-OCaml library this repository
   once used, computes 1 lsl (min_code_size - 1), see Image_decode.ml's
   survey); when the width grows (after adding, when the next code
   reaches 2^width; an encoder growing it one code early -- TIFF's
   "early change" -- is another format); the KwKwK case; a full
   dictionary without a clear.

   Reference: Terry Welch, "A Technique for High-Performance Data
   Compression", IEEE Computer 17 (1984); Jacob Ziv and Abraham Lempel,
   "Compression of Individual Sequences via Variable-Rate Coding", IEEE
   Transactions on Information Theory 24 (1978); the GIF89a
   specification (CompuServe, 1990), appendix F. *)

(* [decode ~min_code_size data ~npixels]: the [npixels] color indices
 * of the LZW stream [data] (a frame's sub-blocks, joined). Stops at the
 * end code, or when [npixels] are decoded, or when [data] runs out
 * (the pixels not reached are 0: some encoders stop early). Raises
 * Failure on a code that can't be there. *)
val decode : min_code_size:int -> string -> npixels:int -> Bytes.t
