(* Mpeg1_vlc: MPEG-1's variable-length codes -- the standard's tables
   (ISO/IEC 11172-2, annex B), as it prints them, read with Vlc.

   The tables, each a lesson in what's common in video:

     macroblock_address_increment   1 for "the next macroblock", the
       usual case; longer for skipping some (those left as they were)
     macroblock_type                what a macroblock is, per picture
       type: in a P picture "1", moved and corrected, is the commonest;
       intra (coded alone) is 5 bits long -- the rare case
     coded_block_pattern            which of the 6 blocks (4 Y, Cb,
       Cr) carry a correction: "111" all four Y, not the color, the
       commonest
     motion_code                    a vector's difference from the
       last one's: "1", no change, the commonest (things move together)
     dct_dc_size_*                  JPEG's DC sizes
     dct_coefficient                (run of zeros, level) pairs: "11s"
       for (0, 1), the commonest; then "s", the sign; EOB "10";
       escape "0000 01" for the rest, run and level written plainly *)

type mb_type = { quant : bool; forward : bool; backward : bool; pattern : bool; intra : bool }

val address_increment : (string * int) list (* 1-33, and -1 stuffing, -2 escape (+33) *)
val mb_type_i : (string * mb_type) list
val mb_type_p : (string * mb_type) list
val mb_type_b : (string * mb_type) list
val coded_block_pattern : (string * int) list (* 1-63, bit 5 the first Y block *)
val motion_code : (string * int) list (* -16 to 16, the sign in the code *)
val dc_size_luminance : (string * int) list
val dc_size_chrominance : (string * int) list

type coefficient = Coeff of int * int (* run, level; the sign bit follows *) | Eob | Escape

val dct_first : (string * coefficient) list (* a non-intra block's first: "1s" is (0, 1) *)
val dct_next : (string * coefficient) list (* the others: "11s" (0, 1), "10" EOB *)
