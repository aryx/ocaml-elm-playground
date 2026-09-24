(* Layer III's tables, as ISO/IEC 11172-3 prints them (and 13818-3 for
   MPEG-2's lower sample rates).

   **The Huffman codes** (Table B.7). Layer III's quantized spectrum is
   mostly small numbers, and mostly zeros towards the high frequencies;
   it is sent in pairs (x, y) (the "big values") then quadruples of 0s
   and 1s ("count1"), each pair one code of a table the encoder picks
   among 32, per region of the spectrum -- the table whose statistics
   fit best. A table of size n codes x and y from 0 to n - 1; table 13's
   16 x 16 for loud music, table 1's 2 x 2 for near silence. Values of
   15 and more, in the 16 x 16 tables, are sent as 15 then the rest in
   "linbits" plain bits (the 8 tables made from table 16, 8 from table
   24, differ only by that). The code gives the magnitude; a sign bit
   follows each value that isn't 0.

   Unlike JPEG's, these codes are fixed, and not canonical (Huffman.mli):
   they are listed code by code, as the standard does, and read with
   Vlc (Vlc.mli), the codes of MPEG-1 video's tables too.

   **The scalefactor bands** (Table B.8): the spectrum's 576 lines (192
   in each of a short block's 3 windows) grouped in bands about as wide
   as the ear's *critical bands* -- narrow in the low frequencies, wide
   in the high ones: 4 lines (about 77 Hz at 44,100) at the bottom, 158
   at the top. Each band gets a scalefactor: its loudness, what the
   encoder shapes its noise with. *)

(* each table's codes, (x, y) row after row *)
val t1 : string array
val t2 : string array
val t3 : string array
val t5 : string array
val t6 : string array
val t7 : string array
val t8 : string array
val t9 : string array
val t10 : string array
val t11 : string array
val t12 : string array
val t13 : string array
val t15 : string array
val t16 : string array
val t24 : string array

(* by table_select, 0 to 31: the codes, the size, the linbits; None for
 * table 0 (zeros, no bits sent) and the unused 4 and 14 *)
val pairs : (string array * int * int) option array

(* count1's quadruples (v, w, x, y) of 0s and 1s, by v * 8 + w * 4 + x * 2
 * + y: table A, a Huffman code, and B, 4 plain bits, each inverted *)
val quad_a : string array
val quad_b : string array

(* [long_bands rate]: the 22 long bands' starts, and 576; [short_bands
 * rate]: the 13 short bands', and 192 *)
val long_bands : int -> int array
val short_bands : int -> int array

(* the preemphasis of the 22 long bands (Table B.6) *)
val pretab : int array

(* by scalefac_compress, 0 to 15: the bits of each low and high band's
 * scalefactor (MPEG-1) *)
val slen : (int * int) array

(* MPEG-2's scalefactors, in 4 partitions: how many in each, by coding
 * (0 to 5) and block (long, short, mixed) *)
val nr_of_sfb : int array array array
