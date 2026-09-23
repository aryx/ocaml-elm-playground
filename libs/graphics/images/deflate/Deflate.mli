(* Deflate: writing DEFLATE, the other way round from Inflate.mli.

   A decoder has no choice: the stream says what to do. An encoder has
   all of them -- which repeats to send as (length, distance), which
   blocks, which Huffman codes -- and its choices are the difference
   between zlib's levels 1 and 9. This one makes the simple ones:

   1. LZ77 by hash chains, zlib's method (RFC 1951, section 4): at each
      position, hash its next 3 bytes; a table gives the last position
      where the same hash was seen, and each position links to the
      previous one with that hash, a chain back through the last 32 KB:

        head.(hash "abc")  ->  9  ->  6  ->  3  ->  0  ->  (none)
                               positions where "abc" (or a collision)
                               starts, most recent first

      Walk the chain (at most [chain] links: how hard to try), keep the
      longest match of 3 to 258 bytes; take it (greedy: zlib's better
      levels first look one byte ahead, lazy matching, an exercise),
      else send the byte as a literal.

   2. One block of fixed Huffman codes (Inflate.mli's type 01): no
      table to send, and the lengths of those codes suit the pictures
      it is written for -- golden frames, mostly flat colors, where
      long repeats dominate. A dynamic block, codes built from the
      block's own counts (Huffman's algorithm, with its lengths
      limited to 15), would save more on photographs: an exercise.

   The worked example, "abcabcabcabc": 'a' 'b' 'c' as literals, then at
   position 3 the chain finds position 0, and a match of 9 at distance
   3 (running into itself) -- the 6 bytes 4B 4C 4A 86 23 00 of
   Inflate.mli's example.

   References: Peter Deutsch, RFC 1951 (1996), section 4, "Compression
   algorithm details"; Jacob Ziv and Abraham Lempel (1977), as in
   Inflate.mli. *)

(* [deflate ?chain s]: [s] as a raw DEFLATE stream, one fixed-code
 * block; [chain] (default 64) caps the matches tried at each
 * position *)
val deflate : ?chain:int -> string -> string
