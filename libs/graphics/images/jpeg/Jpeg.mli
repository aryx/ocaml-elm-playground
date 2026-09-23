(* JPEG: throw away what the eye doesn't see, then compress the rest.

   The Joint Photographic Experts Group, formed in 1986 by ISO and the
   CCITT, compared a dozen proposals and chose the discrete cosine
   transform (Dct.mli) with quantization and Huffman codes: the standard
   is ITU-T T.81 (1992). It defines the coding, not a file; the file
   everybody writes is JFIF (Eric Hamilton, C-Cube, 1992), cameras
   write Exif around the same data, and the Independent JPEG Group's
   free libjpeg (Tom Lane, 1991) is what made the web's photographs
   JPEGs.

   The encoder, which the decoder runs backwards:

     RGB -> YCbCr -> subsample -> 8x8 blocks -> DCT -> quantize -> zigzag -> runs -> Huffman
            brightness  color at    (-128)             divide and  low          of
            and color   half size                      round: the  frequencies  zeros
                                                       loss        first

   YCbCr: brightness and two color differences, as in color TV (JFIF's
   constants, from ITU-R BT.601):

     R = Y + 1.402 (Cr - 128)
     G = Y - 0.344136 (Cb - 128) - 0.714136 (Cr - 128)
     B = Y + 1.772 (Cb - 128)

   Subsampling: the eye sees color less sharply than brightness, so Cb
   and Cr are often kept at half the width (4:2:2), or half the width
   and height (4:2:0). A component's sampling factors (h, v) say how
   many of its blocks cover the area the largest factors cover; that
   area is a minimum coded unit, an MCU. In 4:2:0, Y is 2 x 2 and Cb,
   Cr 1 x 1: an MCU is 16 x 16 pixels, 4 Y blocks then one Cb and one
   Cr, 384 samples instead of 768. The decoder makes the color planes
   full size again: `Box repeats each sample (the simple way); `Triangle
   weighs the two nearest samples 3/4 and 1/4 (libjpeg's "fancy
   upsampling", smoother on color edges, and the default).

   Quantization, the loss: each coefficient was divided by an entry of
   an 8 x 8 table (large ones for high frequencies) and rounded; the
   decoder multiplies back. The tables are in the file ("quality" is
   only the encoder's way of scaling them).

   Zigzag and runs: after quantization most high frequencies are 0; read
   in a zigzag from the top-left, the zeros come last, in a run:

      0  1  5  6 14 15 27 28      the order of the 64 coefficients
      2  4  7 13 16 26 29 42      in the file
      3  8 12 17 25 30 41 43
      9 11 18 24 31 40 44 53
     10 19 23 32 39 45 52 54
     20 22 33 38 46 51 55 60
     21 34 37 47 50 56 59 61
     35 36 48 49 57 58 62 63

   The DC coefficient is sent as the difference from the previous
   block's of the same component; each nonzero AC one as a Huffman
   symbol (the run of zeros before it, its size in bits) then its bits.
   Symbol 0x00 ends the block (the rest are zeros), 0xF0 is 16 zeros. A
   value of size s is sent in s bits, the negatives shifted -- size 2 is
   -3, -2, 2, 3 as 00, 01, 10, 11:

     extend v s = if v < 2^(s-1) then v - 2^s + 1 else v

   The file, a series of markers, FF then a byte:

     FF D8  SOI   start
     FF E0  APP0  JFIF (other APPn: Exif, ICC profiles, ...: skipped)
     FF DB  DQT   quantization tables (in zigzag order)
     FF C0  SOF0  baseline frame: size, components, sampling factors
     FF C4  DHT   Huffman tables: 16 counts, then the symbols (Huffman.mli)
     FF DD  DRI   a restart every n MCUs
     FF DA  SOS   start of scan: the Huffman-coded data follows
     FF D0-D7 RST restart: DC predictions reset, bits realigned
     FF D9  EOI   end

   Inside a scan a data byte FF is written FF 00 (byte stuffing), so no
   marker appears by accident. Restart markers let a decoder find its
   way again after corrupt data.

   Read here: baseline and extended sequential Huffman JPEGs (SOF0,
   SOF1) of 8-bit samples, gray or YCbCr, any sampling factors, any
   number of scans, restart intervals. Refused, with a message saying
   so: progressive JPEGs (SOF2, a common one on the web: the next
   exercise, notes_images.md section 11), lossless, hierarchical and
   arithmetic-coded ones, 12-bit samples, CMYK (4 components). Adobe's
   RGB JPEGs (APP14) are read as YCbCr, as the rare files they are.

   The Huffman codes are decoded a bit at a time (Huffman.mli): plenty
   for the playground's textures, slow for a camera's 12 megapixels.

   References: ITU-T Recommendation T.81 = ISO/IEC 10918-1, "Digital
   Compression and Coding of Continuous-Tone Still Images" (1992);
   Gregory Wallace, "The JPEG Still Picture Compression Standard",
   Communications of the ACM 34 (1991); Eric Hamilton, "JPEG File
   Interchange Format, Version 1.02" (C-Cube Microsystems, 1992);
   William Pennebaker and Joan Mitchell, JPEG Still Image Data
   Compression Standard (1993); Martin Fiedler's NanoJPEG, a baseline
   decoder in one short C file. *)

(* [decode s]: the picture of the JPEG file [s], its blocks
 * inverse-transformed by [idct] (Dct.idct_aan by default) and its
 * color upsampled by [upsampling] (`Triangle by default). With [keep]
 * (64 by default), only the first [keep] coefficients of each block,
 * in zigzag order, are kept, the others made 0: 1 is each block's
 * average, a mosaic; a dozen, most of the picture -- what quantization
 * bets on (examples/ImageJpeg.ml shows it). Raises Failure on a
 * corrupt file, or one using what isn't read here. *)
val decode :
  ?idct:(float array -> float array) ->
  ?upsampling:[ `Box | `Triangle ] ->
  ?keep:int ->
  string ->
  Rgba_image.t

(* [zigzag.(k)]: where the [k]th coefficient of the file goes in a
 * block, row by row: zigzag.(2) = 8, the first of the second row *)
val zigzag : int array

(* [extend v s]: the value of the [s] bits [v] *)
val extend : int -> int -> int
