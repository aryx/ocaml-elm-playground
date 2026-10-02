(* XXH64: a checksum of 64 bits made for the processor, not for the
   wire.

   CRC-32 (Crc32.mli) comes from hardware: a division of polynomials, a
   shift register, in software a table lookup for each *byte*, each
   depending on the one before. Yann Collet's xxHash (2012, written to
   check LZ4's output without slowing it down) is made of what a 64-bit
   processor does in one cycle or a few -- multiply, rotate, xor -- on 8
   bytes at a time, and on four of them side by side:

     the input, in stripes of 32 bytes:

       8 bytes   8 bytes   8 bytes   8 bytes
          |         |         |         |
          v1        v2        v3        v4     acc = rotl (acc + bytes * P2, 31) * P1

   The four accumulators don't depend on each other, so a processor
   that runs several instructions at once (all of them since the
   Pentium Pro) computes the four in the time of one: several gigabytes
   a second, ten times a table-driven CRC-32, which is why LZ4 and
   Zstandard (Zstd.mli, which keeps the low 32 bits) check their frames
   with it. At the end the four are merged, the bytes left over (fewer
   than 32) are mixed in by 8, by 4 and by 1, and an *avalanche* of
   shifts and multiplications makes every bit of the result depend on
   every bit of the input. P1 to P5 are primes, chosen for their bits,
   half ones and half zeros.

   It is not a cryptographic hash: making two inputs with the same
   XXH64 is easy. It catches accidents, as a CRC does.

   Worked examples: the XXH64 of "" is EF46DB3751D8E999, of "abc"
   44BC2CF5AD770999.

   An Int64, so the same 64 bits in JavaScript (see Crc32.mli).

   Reference: Yann Collet, "xxHash fast digest algorithm",
   github.com/Cyan4973/xxHash, doc/xxhash_spec.md. *)

(* the XXH64 of a string, with the seed 0 *)
val xxh64 : string -> int64
