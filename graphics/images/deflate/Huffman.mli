(* Huffman codes: short codes for common symbols, rebuilt from their
   lengths alone.

   David Huffman's algorithm (1952, a term paper in Robert Fano's class)
   gives each symbol a code of bits, shorter the more common the symbol,
   and no code the beginning of another (a *prefix-free* code), so the
   bits can be read one by one with no separator. For the eight symbols
   AAAABBCD (notes_images.md section 3):

         (8)
        0/ \1           A = 0      AAAABBCD = 0 0 0 0 10 10 110 111
        A  (4)          B = 10     14 bits, where 2 bits a symbol
          0/ \1         C = 110    take 16: 1.75 bits a symbol, the
          B  (2)        D = 111    entropy of the source exactly
            0/ \1
            C   D

   A decoder needs the same code as the encoder, and sending the tree is
   wasteful. DEFLATE (and JPEG, the same idea met twice) sends only each
   symbol's *code length*, and both sides rebuild the one *canonical*
   code with those lengths: shorter codes first, and within a length,
   the symbols in order, counting up. With lengths A 1, B 2, C 3, D 3:

     how many codes of each length:     1 -> 1,  2 -> 1,  3 -> 2
     the first code of each length:     1:  0
                                        2: (0 + 1) << 1 = 2  = 10
                                        3: (2 + 1) << 1 = 6  = 110
     then counting up within a length:  A 0,  B 10,  C 110,  D 111

   (RFC 1951, section 3.2.2.) Decoding needs no table of codes at all:
   read a bit at a time, and at each length check whether the code read
   so far falls within that length's range of codes -- the codes of
   length [len] are the [count.(len)] numbers from the first code of
   that length. That is Mark Adler's puff.c's decoder, the one here: at
   most 15 steps a symbol, which is slow next to zlib's lookup tables
   and plenty for the playground's pictures.

   JPEG (Jpeg.mli) sends a table differently: how many codes of each
   length, 1 to 16, then the symbols in the order of their codes -- not
   sorted by value, so [of_counts] rather than [of_lengths]; the codes
   are the same canonical ones.

   Lengths are at most 16 (JPEG's limit; DEFLATE's is 15). A set of
   lengths can be *over-subscribed* (more codes than bits allow, e.g.
   three codes of length 1): an error. It can be *incomplete* (codes
   left over, e.g. a single code of length 1, which DEFLATE uses for a
   block with one distance): allowed, and decoding one of the missing
   codes is then an error.

   References: David Huffman, "A Method for the Construction of
   Minimum-Redundancy Codes", Proceedings of the IRE 40 (1952); Peter
   Deutsch, RFC 1951 (1996), section 3.2.2; Mark Adler, puff.c, in
   zlib's contrib/puff/. *)

(* a canonical code, ready to decode *)
type t

(* the longest code *)
val max_bits : int

(* [of_lengths lengths]: the canonical code where symbol [i] has a code
 * of [lengths.(i)] bits, 0 meaning the symbol has no code. Raises
 * Failure if the lengths are over-subscribed. *)
val of_lengths : int array -> t

(* [of_counts counts symbols]: JPEG's form, [counts.(i)] codes of
 * length [i + 1] (16 counts), given to [symbols] in order. Raises
 * Failure if the counts are over-subscribed, or don't add up to the
 * symbols. *)
val of_counts : int array -> int array -> t

(* [decode next_bit code]: the next symbol, reading its code one bit at
 * a time with [next_bit] (first the code's most significant bit).
 * Raises Failure on a code that isn't there (an incomplete code). *)
val decode : (unit -> int) -> t -> int

(* [codes lengths]: each symbol's (code, length), as the encoder writes
 * them; (0, 0) for a symbol without a code. For A 1, B 2, C 3, D 3:
 * [| (0, 1); (2, 2); (6, 3); (7, 3) |]. *)
val codes : int array -> (int * int) array
