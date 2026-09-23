(* Inflate: decoding DEFLATE, LZ77 back-references written with Huffman
   codes.

   Phil Katz's DEFLATE (PKZIP 2, 1993; RFC 1951, 1996) is the
   compression of zip, gzip, zlib, PNG, HTTP and git. It makes two
   passes over the data, and the decoder undoes them in turn:

   1. LZ77 (Ziv and Lempel, 1977): a repeat of something seen in the
      last 32 KB becomes a pair (length, distance), "copy [length]
      bytes from [distance] bytes back". The copy may overlap what it
      writes, which is what makes a run cheap:

        abcabcabcabc  =  a b c (copy 9 from 3 back)

        a b c
        a b c a                     the a just copied...
        a b c a b c a b c a b c     ...is copied again: length > distance

      so the copy goes one byte at a time (a block copy, like
      Bytes.blit, gives the wrong answer here: the classic bug), and a
      distance of 1 is run-length encoding for free.

   2. Huffman codes (Huffman.mli) for what LZ77 left: one alphabet for
      literals *and* lengths -- 0-255 a byte, 256 the end of the block,
      257-285 a length from 3 to 258 -- and one for distances, 0-29, 1
      to 32768. The bigger lengths and distances share a code and are
      told apart by *extra bits* that follow it: code 265 is a length
      of 11 or 12, one extra bit saying which.

   The stream is a series of blocks, each starting with 3 bits: BFINAL
   (the last block?) and BTYPE, one of

     00 stored: the bytes as they are, for data that doesn't compress
        (up to the next byte boundary, then LEN, NLEN = not LEN, LEN
        bytes)
     01 fixed Huffman codes, given by the RFC, no table sent: literals
        0-143 in 8 bits, 144-255 in 9, 256-279 in 7, 280-287 in 8;
        distances in 5
     10 dynamic Huffman codes: the block sends its own. Their lengths
        are themselves Huffman-coded, with a third alphabet (0-15 a
        length, 16 "repeat the previous 3-6 times", 17 and 18 "3-10,
        11-138 zeros"), whose own lengths come first, 3 bits each, in
        the order 16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15 -- the
        most useful first, so the trailing ones can be left out.

   Bits are read from each byte least significant first; the Huffman
   codes are sent their most significant bit first, and everything else
   (extra bits, the header's fields) least significant first -- the
   other classic bug.

   The worked example, "abcabcabcabc" in one fixed block
   (notes_images.md section 6):

     BFINAL 1, BTYPE 01                              3 bits
     literals 'a' 'b' 'c': 0x30 + 97.. in 8 bits    24 bits
     code 263 = length 9 (257 is 3, 258 is 4, ...)   7 bits
     distance code 2 = distance 3                    5 bits
     code 256, the end of the block                  7 bits
     46 bits = 6 bytes: 4B 4C 4A 86 23 00

   (zlib itself writes 7 bytes here, 4B 4C 4A 4E 84 21 00: four
   literals, then (8, 3). Encoders choose; both decode the same.)

   Checks what a decoder must: LEN against NLEN, a distance further back
   than the output, a missing end-of-block code, data ending early.
   Doesn't check the zlib wrapper's Adler-32: Zlib's job.

   References: Peter Deutsch, RFC 1951, "DEFLATE Compressed Data Format
   Specification version 1.3" (1996); Jacob Ziv and Abraham Lempel, "A
   Universal Algorithm for Sequential Data Compression", IEEE
   Transactions on Information Theory 23 (1977); Mark Adler, puff.c,
   in zlib's contrib/puff/ (this module follows it). *)

(* the lengths of the codes 257 to 285: [length_base.(c - 257)] plus
 * the value of [length_extra.(c - 257)] extra bits; the same for the
 * distances of the codes 0 to 29 (Deflate writes them too) *)
val length_base : int array
val length_extra : int array
val dist_base : int array
val dist_extra : int array

(* [inflate s ~pos]: the bytes of the raw DEFLATE stream starting at
 * [pos] in [s], and the position just after its last byte (where
 * zlib's Adler-32 is). Raises Failure on a corrupt stream. *)
val inflate : string -> pos:int -> string * int
