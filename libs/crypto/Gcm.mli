(* Gcm: AES made an AEAD -- counter mode for the encryption, and a
   polynomial hash in GF(2^128) for the tag (David McGrew and John
   Viega, 2004; NIST SP 800-38D): TLS 1.3's AES_128_GCM.

       H  = AES(K, 0^128)                         the hash's key
       J0 = nonce (12 bytes) || 00000001
       ciphertext = plaintext xor AES(K, J0+1), AES(K, J0+2), ...
       S  = GHASH_H(aad | pad | ciphertext | pad | len aad | len ct, in bits)
       tag = AES(K, J0) xor S

   GHASH is Chacha20_poly1305's Poly1305 in another field: each 16-byte
   block xored into an accumulator, multiplied by H -- in GF(2^128),
   where adding is xor and multiplying is shifting and xoring with the
   polynomial x^128 + x^7 + x^2 + x + 1, the bits in GCM's reflected
   order. Here the plain way, a bit at a time (128 shifts a block):
   slow, and the easiest to read.

   Worked examples (checked by the tests): the GCM paper's test case 3
   (a 128-bit key feffe992..., its 64-byte plaintext d9313225...), and
   Python's cryptography package's output on other lengths.

   Int64 (emulated in a browser; correct there too).

   References: NIST SP 800-38D (2007); David McGrew and John Viega,
   "The Galois/Counter Mode of Operation (GCM)" (2004), its test
   vectors. *)

val seal : key:string -> nonce:string -> aad:string -> string -> string
val open_ : key:string -> nonce:string -> aad:string -> string -> string option
