(* Aes: the block cipher of the world -- 16 bytes in, 16 bytes out,
   under a key of 16, 24 or 32 (Joan Daemen and Vincent Rijmen's
   Rijndael, chosen by NIST in 2001, FIPS 197). TLS 1.3 uses it in GCM
   mode (Gcm.mli), for the servers that do not offer ChaCha20.

   The block is a 4x4 matrix of bytes, and each of the 10 rounds (for a
   128-bit key; 12, 14 for longer ones) is four steps:

       SubBytes     each byte through the S-box: its inverse in the field
                    GF(2^8), then an affine map (the only non-linear step)
       ShiftRows    row i rotated left by i
       MixColumns   each column multiplied by a fixed matrix in GF(2^8)
                    (not in the last round)
       AddRoundKey  xored with this round's 16 bytes of the expanded key

   The S-box is computed here from that definition, not typed in as its
   usual table of 256 numbers: the inverses from the field's powers of
   its generator 3, then the affine map x ^ rotl1 ^ rotl2 ^ rotl3 ^
   rotl4 ^ 0x63. The tests check it against the table's first entries.

   Worked example (FIPS 197 appendix C.1, checked by the tests): the key
   00010203...0f and the block 00112233...ff give 69c4e0d8 6a7b0430
   d8cdb780 70b4c55a.

   Encryption only: GCM's counter mode never decrypts a block.

   References: FIPS 197, "Advanced Encryption Standard" (NIST, 2001);
   Joan Daemen and Vincent Rijmen, "The Design of Rijndael" (2002). *)

type key

(* the round keys of a 16-, 24- or 32-byte key *)
val expand : string -> key

val encrypt_block : key -> string -> string

(* the S-box, computed *)
val sbox : int array
