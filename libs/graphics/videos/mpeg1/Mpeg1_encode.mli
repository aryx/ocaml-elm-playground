(* Mpeg1_encode: an MPEG-1 video encoder, I and P pictures -- Mpeg1.mli
 * run forwards, with the decisions a decoder never sees.

   A decoder obeys the stream; an encoder writes it, and chooses. For
   each macroblock of a P picture:

     where did it come from?   motion estimation (Motion.mli): the
                               vector whose prediction differs least
     is the prediction good?   its SAD against the macroblock's own
                               variation: much worse, code it intra
     what's left?              the residual's DCT, quantized; the blocks
                               with a level left are the coded ones
     can it be said shorter?   no vector and nothing left: **skipped**,
                               not a bit (a still background); no vector
                               but something left: "01", no vector sent;
                               a vector but nothing left: "001"

   and, the one rule an encoder must not break, it predicts from **what
   the decoder will have** -- its own reconstruction, the same
   dequantization, IDCT and rounding (Mpeg1.mli's, used here) -- never
   the source: from the source, the decoder's small differences add up,
   P frame after P frame, until the picture drifts away.

   Written: a sequence header (default matrices, variable bit rate),
   groups of pictures of [gop] frames (an I, then Ps), a slice per row of
   macroblocks, one quantizer_scale ([quantizer], 1-31: the knob of size
   against quality, JPEG's quality), the vectors' f_code from the search
   range. Not written: B pictures (the reordering, and a search both
   ways: the exercise), a quantizer per macroblock (rate control: a
   bitrate asked for, the quantizer chosen to meet it), custom matrices.

   The tests: our decoder reads what this writes exactly as the encoder
   reconstructed it; ffmpeg reads it too; and on our clip, its size and
   PSNR against ffmpeg's encoder's, full search against logarithmic. *)

type stats = {
  macroblocks : int;
  intra : int; (* in P pictures: the prediction too poor *)
  skipped : int;
  candidates : int; (* the SADs the motion search computed: its cost *)
}

(* [encode ~rate frames]: the stream of [frames] (all of the same size)
 * at [rate] (frames a second, one of MPEG-1's: 25/1, 30000/1001, ...),
 * with [quantizer] (5 by default), a group every [gop] frames (12),
 * motion searched by [search] (Full) within [range] pixels (10); and
 * what the encoder decided *)
val encode : ?quantizer:int -> ?gop:int -> ?search:Motion.search -> ?range:int -> rate:int * int -> Rgba_image.t list -> string * stats
