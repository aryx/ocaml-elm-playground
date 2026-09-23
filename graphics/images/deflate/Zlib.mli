(* zlib: a DEFLATE stream with a header and a checksum.

   Jean-loup Gailly and Mark Adler's zlib (1995) is the library nearly
   every program compresses with; its stream format (RFC 1950) is what
   PNG's IDAT chunks hold, and it is small: two bytes before the
   DEFLATE data (Inflate), four after.

     CMF  the method (8 = deflate) in the low 4 bits, the window size
          (log2, minus 8: 7 = 32 KB) in the high 4
     FLG  2 bits of "how hard the encoder tried" (information only),
          1 bit "a preset dictionary follows" (not used by PNG, not
          read here), and 5 check bits making CMF * 256 + FLG a
          multiple of 31 -- so 78 01, 78 9C, 78 DA, the usual starts
     ...  the DEFLATE blocks
     the Adler-32 (Adler32.mli) of the decompressed bytes, big-endian

   The worked example, "hi" in one stored block (notes_images.md
   section 6):

     78 01 | 01 02 00 FD FF 68 69 | 01 3B 00 D2
             ^ BFINAL 1, stored; LEN 2, NLEN = not LEN, 'h' 'i'
                                    ^ Adler-32 ("hi")

   Reference: Peter Deutsch and Jean-loup Gailly, RFC 1950, "ZLIB
   Compressed Data Format Specification version 3.3" (1996). *)

(* [decompress s]: the bytes of the zlib stream [s]. Raises Failure on a
 * bad header, a preset dictionary, a corrupt DEFLATE stream or a wrong
 * Adler-32. *)
val decompress : string -> string
