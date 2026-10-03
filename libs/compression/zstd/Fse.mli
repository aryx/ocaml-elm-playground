(* FSE, Finite State Entropy: a symbol may cost a fraction of a bit, and
   decoding one is a table lookup.

   A Huffman code (Huffman.mli) gives each symbol a whole number of
   bits, so a symbol seen 9 times in 10, which is worth 0.15 bit, still
   costs 1. Arithmetic coding (Rissanen, Pasco, 1976) pays the exact
   price by keeping the message as one number in an interval, but it
   multiplies and divides for every symbol. Jarek Duda's Asymmetric
   Numeral Systems (2009, 2013) pay the exact price with *one integer*,
   the state, and in its table form (tANS) with no arithmetic at all;
   Yann Collet's FSE (2013) is that table form made fast, the entropy
   coder of Zstandard (Zstd.mli) and, from the same idea, of Apple's
   LZFSE and JPEG XL.

   The idea. Take a table of 2^accuracy states, and give each symbol a
   share of them in proportion to how common it is -- its *count*. With
   16 states and A 8, B 4, C 3, D 1:

     state   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
     symbol  A  A  B  C  A  B  C  A  B  C  A  A  B  A  A  D
     bits    1  1  2  3  1  2  2  1  2  2  1  1  2  1  1  4
     base    0  2  0  8  4  4  0  6  8  4  8 10 12 12 14  0

   Being in a state *is* the symbol: state 6 says C. Then the state
   reads its [bits] from the stream and the next state is [base] plus
   them. A, half of the table, reads 1 bit, a Huffman code's price; D,
   a sixteenth, reads 4. C has 3 states of 16, worth log2 (16/3) = 2.42
   bits: one of its states reads 3 bits and two read 2, and that is the
   fraction -- the state carries from one symbol to the next what a
   whole number of bits can't say.

   Where bits and base come from: number a symbol's states, in the
   table's order, count, count + 1, ... 2 count - 1 (C's: 3, 4, 5).
   Each reads the bits that bring its number back into [16, 32), and
   the result, less 16, is the next state: 3 reads 3 bits (24 to 31,
   states 8 to 15), 4 reads 2 (16 to 19, states 0 to 3), 5 reads 2 (20
   to 23, states 4 to 7). A symbol's states so share out *all* the
   states among them, each next state reached from exactly one of them:
   what lets the encoder run the table the other way.

   The symbols are spread over the table by a step of 5/8 of its size
   plus 3 (13 here: A at 0, 13, 10, 7, 4, 1, 14, 11, then B at 8, ...),
   which has no factor in common with the size, so it lands on every
   state once and each symbol's states end up scattered evenly. A count
   of -1 means "less than one": the symbol gets a single state, at the
   end of the table, which reads a full [accuracy] bits (D above).

   Backwards. The encoder is a stack: the last symbol it pushed on the
   state is the first the decoder finds there. So it encodes the
   message from its last symbol to its first, and the decoder reads the
   stream *from its end*: [backward]. The bytes are one little-endian
   number; its highest bit set is a mark (the encoder's last write),
   and the fields are read from just under it downwards.

   The worked example, C D B A with the table above, in 2 bytes:

     C9 27 = 0010 0111 1100 1001
               ^ the mark
                0 011             the first state, 4 bits: 3, C
                     1 11         C's 3 bits: 8 + 7 = state 15, D
                         00 10    D's 4 bits: 0 + 2 = state 2, B
                              01  B's 2 bits: 0 + 1 = state 1, A

   A table is sent as its counts ([read_distribution]): the accuracy
   less 5 in 4 bits, then each count plus one in just the bits that
   what is *left* of the table needs -- after 12 states of 16 are given
   out, a count is at most 4 -- and one bit fewer for the smallest
   values when the bits have values to spare. A zero count is followed
   by 2 bits, how many more zeros.

   References: Jarek Duda, "Asymmetric numeral systems", arXiv:0902.0271
   (2009), and "Asymmetric numeral systems: entropy coding combining
   speed of Huffman coding with compression rate of arithmetic coding",
   arXiv:1311.2540 (2013); Yann Collet, "Finite State Entropy - A new
   breed of entropy coder", fastcompression.blogspot.com (2013); Yann
   Collet and Murray Kucherawy, RFC 8878 (2021), section 4.1; zstd's
   doc/educational_decoder/zstd_decompress.c (this module follows
   it). *)

(*****************************************************************************)
(* {1 Bits} *)
(*****************************************************************************)

(* the position of the highest bit set: 1 -> 0, 2 and 3 -> 1, 4 -> 2 *)
val log2 : int -> int

(* [field s ~bit n]: the [n] bits from bit [bit] up of the little-endian
 * number that the bytes of [s] make (bit 8 is the second byte's lowest
 * bit); zeros outside [s] *)
val field : string -> bit:int -> int -> int

(* a stream read from its end *)
type reader

(* [backward s ~pos ~len]: the stream in the [len] bytes at [pos],
 * ready under its mark. Raises Failure if the last byte is 0 (no
 * mark), or the bytes aren't in [s]. *)
val backward : string -> pos:int -> len:int -> reader

(* [bits r n]: the next [n] bits down, as a number; the bits under the
 * stream's first bit read as zeros (see [left]) *)
val bits : reader -> int -> int

(* how many bits are left to read: 0 when a stream was read exactly,
 * negative once [bits] has read under its first bit *)
val left : reader -> int

(*****************************************************************************)
(* {1 The table} *)
(*****************************************************************************)

(* a decoding table *)
type t

(* [of_distribution ~accuracy counts]: the table of 2^[accuracy] states
 * where symbol [i] has [counts.(i)] states, -1 meaning one state, at
 * the end. Raises Failure if the counts (a -1 counting for 1) don't
 * add up to the number of states. *)
val of_distribution : accuracy:int -> int array -> t

(* [read_distribution s ~pos ~max_accuracy]: the table described at
 * [pos], and the position of the byte after its description *)
val read_distribution : string -> pos:int -> max_accuracy:int -> t * int

(* the table of a single symbol, which reads no bit: zstd's "RLE" *)
val single : int -> t

(*****************************************************************************)
(* {1 Decoding} *)
(*****************************************************************************)

(* the first state: [accuracy] bits *)
val start : t -> reader -> int

(* the symbol a state says *)
val symbol : t -> int -> int

(* the state after: its base plus its bits, read from the stream *)
val next : t -> reader -> int -> int
