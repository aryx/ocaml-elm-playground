(* Progressive JPEG: the same picture sent in several passes, each
   sharpening the one before.

   A baseline file (Jpeg.mli) sends each block whole, one after the
   other: a picture arriving slowly is drawn from the top, a strip at a
   time. A progressive one sends every block a little at a time, in
   passes over the whole picture called scans: a first scan gives
   every block's average, and the picture is there at once, in large
   squares; the next ones add detail everywhere. The standard has it
   from the start (ITU-T T.81, 1992, Annex G), with two ways to cut a
   block's 64 coefficients into passes, used together:

     spectral selection          a scan carries the coefficients from
                                 Ss to Se only, in zigzag order: 0
                                 alone (the DC), then 1 to 5, 6 to 63...
     successive approximation    a scan carries them without their Al
                                 lowest bits; later scans add one bit
                                 each (Ah is the bit the scan before
                                 stopped at)

   So a scan is one of four kinds, by its header (Ss, Se, Ah, Al):

     DC, first      Ss = 0, Ah = 0   each block's DC coefficient, as in
                                     baseline (a difference from the
                                     block before), shifted left by Al
     DC, refining   Ss = 0, Ah > 0   one bit a block: its bit Al
     AC, first      Ss > 0, Ah = 0   runs of zeros and values, as in
                                     baseline, between Ss and Se; and
                                     a run of blocks with nothing more
                                     in the band (below)
     AC, refining   Ss > 0, Ah > 0   one more bit for each coefficient
                                     already not zero, and the ones
                                     that become so (+1 or -1 at bit
                                     Al), found by runs of zeros

   An AC scan is of one component (a DC scan may carry the three,
   interleaved as in baseline). What baseline's end-of-block says of
   one block, an AC scan can say of many: the symbols 0x00, 0x10...
   0xE0 are EOB0 to EOB14, "this block and the 2^r - 1 + (r more bits)
   after it have nothing more in this band" (the end-of-band run).
   High frequencies are zero in most blocks: a scan of them is mostly
   such runs.

   The refining AC scan is the subtle one. A coefficient that is zero
   so far and one that is not are told apart by the decoder (it has
   the earlier scans), so their bits are mixed in one stream: a symbol
   (r, 1) says "r coefficients still zero, then one that becomes +1 or
   -1 (a sign bit follows)", and *between* them, each coefficient
   already not zero is passed with one bit, its correction. So the
   run counts only the zeros, and the bits of the others come as they
   are met. An end-of-band run does the same to the end of the band:
   corrections only.

   The decoder therefore keeps every block's coefficients (integers,
   in zigzag order) until the last scan, and only then does what
   baseline does at each block: multiply by the quantization table,
   the inverse transform (Dct). That is Jpeg's; this module is one
   function, a block's part of a scan.

   cs-history:
   Made for pictures looked at as they come over a slow line -- the
   telephone's, in 1992, then the web's modems: a progressive picture
   could be judged at a tenth of its bytes. The Independent JPEG
   Group's library wrote them from its version 6 (1995). They came
   back twenty years later for another reason: the scans compress a
   little better than baseline's blocks (the zeros of a band are
   together), and Mozilla's mozjpeg (2014) makes every JPEG
   progressive for that. Many of the web's photographs are, today.

   Reference: ITU-T Recommendation T.81 (1992), Annex G; its figures
   G.3 to G.7 are the procedures below. *)

(* a scan's header: the band (first and last coefficient, in zigzag
 * order), and the bits: [ah] 0 for a first scan, else the bit the
 * coefficients were known down to; [al] the bit this scan gives *)
type scan = { ss : int; se : int; ah : int; al : int }

(* [block scan ~bit ~receive ~dc ~ac ~pred ~run coefs at]: one block's
 * part of the scan, read with [bit] and [receive] (n bits as a
 * number), into [coefs] from index [at] (64 integers, zigzag order).
 * [dc] and [ac] give the scan's Huffman tables when asked; [pred] is
 * the component's DC so far, [run] the blocks still to skip in an
 * end-of-band run: both the caller's, set to 0 at a scan's start and
 * at a restart marker *)
val block :
  scan ->
  bit:(unit -> int) ->
  receive:(int -> int) ->
  dc:(unit -> Huffman.t) ->
  ac:(unit -> Huffman.t) ->
  pred:int ref ->
  run:int ref ->
  int array ->
  int ->
  unit
