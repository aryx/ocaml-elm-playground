(* CRC-32: a checksum that catches a corrupted file.

   A cyclic redundancy check reads the bytes as the coefficients of a
   huge polynomial over the bits (0 and 1, where adding is xor), and
   keeps the remainder of its division by a fixed polynomial of degree
   32. Any error burst shorter than 32 bits changes the remainder, and a
   random corruption goes unnoticed once in 2^32. It is no protection
   against someone who *wants* to change the file (for that, a
   cryptographic hash), only against accidents: a bad disk, a bad
   transfer.

   The polynomial here is the one of Ethernet (1975), zip, gzip and
   PNG, whose chunks each end with the CRC-32 of their type and data
   (Png, notes_images.md section 7). Written with its bits reversed,
   0xEDB88320, because the bytes are fed least significant bit first.

   Division bit by bit costs 8 steps a byte; the usual trick, done
   here, is a table of the 256 remainders of one byte, so a byte is one
   lookup, one shift and one xor:

     crc = table.((crc xor byte) land 0xFF) xor (crc lsr 8)

   with the register starting at 0xFFFFFFFF and flipped at the end (so
   that leading zero bytes still count).

   The standard check value, the first test:

     CRC-32 ("123456789") = 0xCBF43926

   Reference: W. Wesley Peterson and D. T. Brown, "Cyclic Codes for
   Error Detection", Proceedings of the IRE 49 (1961); the table method
   as in the PNG specification, annex D "Sample CRC implementation". *)

(* The CRC is an int32 (0xCBF43926 is -873187034l, the same 32 bits):
   an int has 63 bits natively but 32 in JavaScript, Int32 is the same
   everywhere. *)

(* [update crc s ~pos ~len]: the CRC-32 of what gave [crc], followed by
 * the [len] bytes of [s] from [pos]; start from 0l *)
val update : int32 -> string -> pos:int -> len:int -> int32

(* [string s]: the CRC-32 of all of [s] *)
val string : string -> int32
