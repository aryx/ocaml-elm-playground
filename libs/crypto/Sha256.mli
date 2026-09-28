(* Sha256: SHA-1's construction (Sha1.mli) with a bigger state and
   more rounds -- the hash of TLS 1.3's transcript and key schedule
   (NSA, 2001, FIPS 180-2).

   The message padded as SHA-1's (a 1 bit, zeros, the length in bits
   on 64), in blocks of 64 bytes, each mixed into eight 32-bit words:

       h = 6a09e667 bb67ae85 3c6ef372 a54ff53a 510e527f 9b05688c 1f83d9ab 5be0cd19
           (the fractional parts of the square roots of the first 8 primes)

   the block's 16 words stretched to 64 (w[i] = s1(w[i-2]) + w[i-7] +
   s0(w[i-15]) + w[i-16], s0 and s1 rotations and a shift xored), then
   64 rounds, each

       t1 = h + S1(e) + ch(e, f, g) + K[i] + w[i]
       t2 = S0(a) + maj(a, b, c)
       h = g; g = f; f = e; e = d + t1; d = c; c = b; b = a; a = t1 + t2

   with K the fractional parts of the cube roots of the first 64
   primes -- constants chosen in the open, "nothing up my sleeve".

   Worked examples (FIPS 180's, checked by the tests): "abc" is
   ba7816bf 8f01cfea 414140de 5dae2223 b00361a3 96177a9c b410ff61
   f20015ad; "" is e3b0c442 98fc1c14 9afbf4c8 996fb924 27ae41e4 649b934c
   a495991b 7852b855.

   In Int32, as Sha1: the same bits natively and in a browser.

   References: FIPS 180-4, "Secure Hash Standard" (NIST, 2015),
   sections 4.2.2, 5.3.3 and 6.2; RFC 6234 (2011), with C code. *)

(* the 32 bytes of the hash *)
val digest : string -> string

(* bytes as hexadecimal digits (any bytes: a digest, a key) *)
val hex : string -> string
