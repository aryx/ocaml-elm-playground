(* Sha1: a fingerprint of 160 bits for any bytes.

   A hash function turns bytes of any length into a fixed-size number,
   the same bytes always the same number, and -- the cryptographic part
   -- nobody able to find two inputs with the same number, a
   *collision*, nor an input for a number given. SHA-1 (NSA, 1995,
   FIPS 180-1) gives 160 bits, and is the construction of its era,
   Merkle and Damgard's: the message padded to a multiple of 512 bits,
   then each 512-bit block mixed into a 160-bit state by a compression
   function:

       state = 67452301 EFCDAB89 98BADCFE 10325476 C3D2E1F0   (5 words)

       message + 1 bit + 0s + its length in bits (64)  =  blocks of 64 bytes
              |
         block 1 --> compress(state) --> block 2 --> ... --> state = the hash

   The compression: the block's 16 words stretched to 80 (each the xor
   of four earlier ones, rotated by 1), then 80 rounds over five words
   a, b, c, d, e, each round

       t = rotl5(a) + f(b, c, d) + e + K + w[i]
       e = d;  d = c;  c = rotl30(b);  b = a;  a = t     (mod 2^32)

   with f and K changing every 20 rounds (choose, parity, majority,
   parity). All 32-bit arithmetic, on Int32: it wraps the same natively
   and in a browser.

   Broken: Marc Stevens' team found a collision in 2017 (SHAttered: two
   PDF files, one SHA-1) after years of warnings, and it is retired for
   signatures and certificates. Here it isn't protecting anything: the
   WebSocket handshake (Websocket.mli) uses it only to prove that the
   server understood the request, not to keep a secret -- which is why
   RFC 6455 could pick it, and why it is still there. SHA-256 (the
   next module, for TLS) is the same construction with more rounds and
   bigger words.

   Worked examples (checked by the tests, FIPS 180's own): "abc" is
   a9993e36 4706816a ba3e2571 7850c26c 9cd0d89d; "" is da39a3ee 5e6b4b0d
   3255bfef 95601890 afd80709; a million "a" 34aa973c d4c4daa4 f61eeb2b
   dbad2731 6534016f.

   References: FIPS 180-4, "Secure Hash Standard" (NIST, 2015); RFC 3174
   (2001), SHA-1 with C code; Ralph Merkle and Ivan Damgard (1989), the
   construction; Marc Stevens et al., "The first collision for full
   SHA-1" (CRYPTO 2017). *)

(* the 20 bytes of the hash *)
val digest : string -> string

(* the hash as 40 hexadecimal digits *)
val hex : string -> string
