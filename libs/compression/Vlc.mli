(* Vlc: variable-length codes fixed once, in a standard -- a table as
 * the standard prints it.

   JPEG sends its Huffman tables in the file (Jpeg.mli), each picture
   its own, rebuilt from their lengths (Huffman.mli: the canonical
   codes). MPEG fixes its codes once, in the standard, built from
   statistics of typical data: short codes for what's common, long ones
   for the rare -- and not canonical, so a table is the list of its
   codes. A code here is written as the standard prints it, "0000 0101
   11", spaces for the eye; read bit by bit down a binary tree until a
   leaf. MPEG-1's video tables are Mpeg1_vlc's.

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
