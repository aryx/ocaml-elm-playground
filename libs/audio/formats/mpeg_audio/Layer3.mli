(* Layer III (MP3): the subbands split again, Huffman codes, and a
   reservoir of bits.

   Layer III is Layer II's filterbank (Polyphase.mli) with three ideas
   on top, each winning bits where Layer II can't (Fraunhofer IIS, with
   AT&T, Thomson and CNET: ASPEC, 1990; the name "MP3", 1995):

     1. finer frequencies: each of the 32 subbands split in 18 by an
        MDCT (Imdct.mli): 576 lines a granule (half a frame), or 3
        times 192 in short blocks, when an attack needs finer time;
     2. nonuniform quantization and Huffman codes: each line a small
        integer is, its value is^(4/3) (big values coarser: the ear
        hears loud sounds less precisely), in scalefactor bands
        (Layer3_tables.mli) each with its loudness, the integers sent
        with the table that fits them best;
     3. the **bit reservoir**: a frame's bytes are fixed by the bitrate,
        but a quiet frame needs fewer than a loud one, so a frame may
        start its data in the bytes the frames before left unused --
        main_data_begin bytes back, up to 511:

          | hdr side | data of 1 ....| hdr side |.. 1 | data of 2 .. |
                                                  ^
                          main_data_begin of frame 2 points back here

   A frame (MPEG-1) is 2 granules of 576 samples per channel. After
   the header, its **side information** (17 bytes mono, 32 stereo),
   then the main data, each granule and channel's:

     part 2   the scalefactors: 21 long bands' or 12 x 3 short bands',
              each of slen bits (scalefac_compress); scfsi lets the
              second granule reuse the first's, 4 groups of bands
     part 3   the Huffman codes: big_values pairs, in 3 regions each
              with its table (table_select), then count1 quadruples of
              0s and 1s until part2_3_length bits are used; the rest of
              the 576 lines are zeros

   Then the decoder, per granule and channel:

     Huffman -> is[576] -> requantize: sign(is) |is|^(4/3) 2^(gain/4)
     2^(-scalefactor ...) -> stereo (mid/side) -> reorder (short
     blocks: by window then line, to line then window) -> alias
     reduction (the 32 subbands overlap, 8 butterflies at each border)
     -> IMDCT, window, overlap-add -> 18 time slots of 32 subbands ->
     Polyphase

   **Joint stereo**: mid/side (the mode extension's bit 2): the
   channels sent as M = (L + R) / sqrt 2 and S = (L - R) / sqrt 2 --
   S small when the channels are alike, cheaper to code. Intensity
   stereo (bit 1: the high bands as one channel and a direction, what
   Layer II's joint stereo is) isn't done yet: no encoder we have
   writes it (LAME doesn't), so none of our files could test it -- an
   exercise, 2.4.3.4.9.3, and ISO/IEC 13818-3 for MPEG-2's variant.

   **MPEG-2's lower sample rates** (22,050, 24,000 and 16,000 Hz, and
   MPEG-2.5's 11,025 to 8,000: speech, streams): one granule a frame,
   576 samples; a smaller side information (8 bits of main_data_begin,
   no scfsi); and the scalefactors coded another way, scalefac_compress
   of 9 bits choosing how many bits each of 4 partitions of bands gets
   (ISO/IEC 13818-3, 2.4.3.2).

   References: ISO/IEC 11172-3 (1993), 2.4.1.7, 2.4.2.7, 2.4.3.4;
   Karlheinz Brandenburg and Gerhard Stoll, "ISO-MPEG-1 Audio: A
   Generic Standard for Coding of High-Quality Digital Audio", JAES
   42 (1994); Rassol Raissi, "The Theory Behind Mp3" (2002). *)

(* a granule's channel, as the side information says *)
type granule = {
  part2_3_length : int; (* the bits of its scalefactors and codes *)
  big_values : int; (* the pairs *)
  global_gain : int;
  scalefac_compress : int;
  block_type : int; (* 0 long, 1 start, 2 short, 3 stop *)
  mixed : bool; (* short blocks, but the 2 lowest subbands long *)
  table_select : int array; (* the regions' tables *)
  subblock_gain : int array; (* the 3 short windows' *)
  region0_count : int;
  region1_count : int;
  preflag : bool;
  scalefac_scale : bool;
  count1_table : int; (* A or B *)
}

type side_info = {
  main_data_begin : int;
  scfsi : bool array array; (* per channel, 4 groups of bands *)
  granules : granule array array; (* per granule, per channel *)
}

(* [side_info header bits]: [bits] just past the header (and its CRC) *)
val side_info : Mpeg_audio_header.t -> Bits.t -> side_info

(* what a stream carries from frame to frame: the reservoir, the
 * overlap of the IMDCT, the scalefactors scfsi reuses *)
type state

val create : unit -> state

(* [decode state header file at]: the frame at byte [at] of [file], its
 * subband samples: per channel, 18 time slots a granule of 32 bands,
 * slot [t]'s band [sb] at [t * 32 + sb]; zeros if the reservoir lacks
 * the bytes it borrows *)
val decode : state -> Mpeg_audio_header.t -> string -> int -> float array array
