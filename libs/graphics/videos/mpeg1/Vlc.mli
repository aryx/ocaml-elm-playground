(* Vlc: MPEG-1's variable-length codes -- the standard's tables, as it
 * prints them.

   JPEG sends its Huffman tables in the file (Jpeg.mli), each picture
   its own; MPEG-1 fixes its codes once, in the standard (ISO/IEC
   11172-2, annex B), built from statistics of typical video: short
   codes for what's common, long ones for the rare. A code here is
   written as the standard prints it, "0000 0101 11", spaces for the
   eye; read bit by bit down a binary tree until a leaf.

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
       escape "0000 01" for the rest, run and level written plainly

   A code table must be **prefix-free** (no code the start of another,
   so the reader knows where each ends): [of_list] checks it. *)

type 'a t

(* [of_list codes]: the tree of [codes], (code, value); raises
 * Invalid_argument if one is a prefix of another *)
val of_list : (string * 'a) list -> 'a t

(* [read bits table]: the next code's value; Failure if the bits are no
 * code of the table *)
val read : Bits.t -> 'a t -> 'a

(* the Kraft sum of [codes]: sum of 2^-length, at most 1 for any
 * prefix-free code, 1 when no bit string is left out *)
val kraft : (string * 'a) list -> float

(* the tables *)

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
