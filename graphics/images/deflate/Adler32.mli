(* Adler-32: zlib's checksum, two running sums.

   Mark Adler's checksum (1995), the trailer of every zlib stream (Zlib,
   notes_images.md section 6), checked against the bytes the decoder
   produced. Faster than a CRC -- two additions a byte, no table -- and
   a little weaker, which zlib accepts since the stream around it is
   usually protected too (by PNG's CRCs, by TCP's checksums):

     a = 1 + the sum of the bytes                (mod 65521)
     b = the sum of the successive values of a   (mod 65521)
     Adler-32 = b * 65536 + a

   [a] alone would not see bytes swapped; [b] weights each byte by how
   many bytes follow it, so it does. 65521 is the largest prime below
   2^16.

   The worked example, "hi" (bytes 104 and 105):

     a = 1 + 104 + 105 = 210
     b = (1 + 104) + (1 + 104 + 105) = 105 + 210 = 315
     Adler-32 = 315 * 65536 + 210 = 0x013B00D2

   and Wikipedia's, Adler-32 ("Wikipedia") = 0x11E60398.

   Reference: Peter Deutsch and Jean-loup Gailly, RFC 1950, "ZLIB
   Compressed Data Format Specification version 3.3" (1996), section
   8.2. *)

(* [update adler s ~pos ~len]: the Adler-32 of what gave [adler],
 * followed by the [len] bytes of [s] from [pos]; start from 1 *)
val update : int -> string -> pos:int -> len:int -> int

(* [string s]: the Adler-32 of all of [s] *)
val string : string -> int
