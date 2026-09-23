(* Mpeg1: MPEG-1 video (ISO/IEC 11172-2, 1993) -- a frame predicted
 * from others by moving blocks, only the difference sent.

   FLI stores a frame as the pixels that changed (Fli.mli); a camera
   that pans changes them all. MPEG-1's idea: the pixels didn't change,
   they **moved**. So a frame is cut into **macroblocks** of 16 x 16
   pixels (four 8 x 8 blocks of Y, one of Cb and one of Cr: 4:2:0,
   Yuv.mli), and each is either

     intra       coded alone, as JPEG would (Jpeg.mli): the DCT of each
                 block, quantized, zigzag, (run, level) codes (Vlc.mli)
     predicted   copied from a frame already decoded, **moved** by a
                 motion vector, plus the DCT of what still differs, the
                 **residual** -- often nothing: a few bits

   and the pictures come in three kinds:

     I  intra: every macroblock alone; where a player can start
     P  predicted from the last I or P, forward
     B  bidirectional: from the I or P before, the one after, or the
        average of both -- the best predictions, and so a B is sent
        after the P it needs, and the decoder **reorders**:

          display order:   I0  B1  B2  P3  B4  B5  P6
          decode order:    I0  P3  B1  B2  P6  B4  B5

        a decoder holds the last reference (I or P) back, shows the Bs
        as they come, and the held one when the next reference comes.

   **Half a pixel**: a vector counts half pixels; between two pixels
   the prediction is their average, rounded up, (a + b + 1) / 2; between
   four, (a + b + c + d + 2) / 4. Worked example: the row 10 20 30 40
   moved by 3 half pixels (1.5 to the left, the prediction of pixel x
   the pixels at x + 1 and x + 2): 25 35, then the edge. The color's
   vector is half the brightness's (half the resolution), rounded
   towards 0: 3 half pixels become 1.

   A vector is sent as its difference from the one before (the
   neighbour's, usually the same: "1", one bit, Vlc.mli), in f_code
   ranges; a skipped macroblock -- none sent at all -- is, in a P
   picture, the one of the reference at the same place, and in a B, the
   previous macroblock's prediction again.

   **Quantization**, JPEG's: each coefficient multiplied back by the
   macroblock's quantizer_scale (1-31, the encoder's knob, sent per
   slice or macroblock) and a matrix entry (the intra one given, or the
   default; for predicted blocks, 16 everywhere): intra 2 level q m /
   16, predicted (2 level + sign) q m / 16, then made odd (the IDCT
   mismatch control: an even value moved one towards 0), within
   -2048..2047. An intra block's DC is the difference from the previous
   block's, as in JPEG, in units of 8.

   The bitstream, layer in layer, each starting with a start code
   (Bits.mli): a sequence header (the size, the frame rate, the
   matrices), groups of pictures, a picture header (its kind, its place
   in display order, the vectors' ranges), slices (a row of macroblocks,
   or part of one: where a decoder resynchronizes after an error, and
   where the predictions start over), macroblocks.

   Read here: MPEG-1 video elementary streams (.m1v, what a .mpg holds
   inside its packets; the packets, the system stream, not read), I, P
   and B pictures, the matrices, skipped macroblocks, both vector
   precisions. Not read: D pictures (DC only, for fast search, never
   used). The IDCT is Dct.mli's floating-point one, so a pixel can
   differ by one from another decoder's (the standard allows it, within
   the IEEE 1180 limits).

   References: ISO/IEC 11172-2 (1993); Didier Le Gall, "MPEG: a video
   compression standard for multimedia applications", CACM 34(4)
   (1991); Dominic Szablewski's pl_mpeg (2019), a decoder in one file
   of C, and jsmpeg. See notes_video.md, section 5. *)

type kind = I | P | B

(* how a macroblock was coded, for the analyzer *)
type how = Intra | Forward | Backward | Both | Zero (* P, no vector: the same place *) | Skipped

(* a picture, as decoded: its kind, and each macroblock's coding and
 * vectors (in half pixels; (0, 0) when there is none), row by row *)
type info = { kind : kind; macroblocks : (how * (int * int) * (int * int)) array; mb_width : int }

type header = { width : int; height : int; rate : int * int; kinds : kind array (* display order *) }

(* [of_string s]: the header, the frames in display order, decoded
 * forward only (Movie.sequential), and each frame's info (decoding it
 * if need be). With [residual], each frame shown is instead **what was
 * sent** for it: the intra macroblocks as they are, the predicted ones
 * as their residual on gray (128), the skipped ones gray -- the
 * prediction switched off, for the analyzer (the references are still
 * the true pictures). Raises Failure if [s] isn't an MPEG-1 video
 * stream. *)
val of_string : ?residual:bool -> string -> header * Movie.t * (int -> info)

(* the half-pixel prediction of Mpeg1.mli's worked example: [predict
 * row x v] the prediction of pixel [x] of [row] moved by [v] half
 * pixels *)
val predict : int array -> int -> int -> int

(* The decoder's arithmetic, for the encoder (Mpeg1_encode.mli): an
 * encoder predicts from what the decoder will have, not from the
 * source, or the two drift apart a little more every P frame. *)

(* the intra matrix, natural order *)
val default_intra : int array

(* [prediction plane ~stride ~rows ~x ~y ~size v]: the [size] x [size]
 * square at (x, y) of [plane], moved by [v] half pixels *)
val prediction : Bytes.t -> stride:int -> rows:int -> x:int -> y:int -> size:int -> int * int -> int array

(* [dequantize ~intra ~q ~m level]: a coefficient back from its level *)
val dequantize : intra:bool -> q:int -> m:int -> int -> int

(* the frame rates' codes: rate (num, den) at index code *)
val picture_rates : (int * int) array
