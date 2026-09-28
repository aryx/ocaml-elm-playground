(* Sha512: SHA-256 (Sha256.mli) on 64-bit words, 80 rounds, blocks of
   128 bytes and a length on 128 bits -- and SHA-384, the same with
   other starting words, cut to 48 bytes: what a certificate signed
   with ECDSA over P-384 is hashed with (Google's roots are).

   The rounds are SHA-256's with other rotations (28, 34, 39 for S0; 14,
   18, 41 for S1; 1, 8, >>7 and 19, 61, >>6 for the schedule) and 80
   constants, the first 64 bits of the cube roots' fractional parts of
   the first 80 primes.

   Worked examples (FIPS 180's, checked by the tests): SHA-512 of "abc"
   starts ddaf35a1 93617aba, SHA-384 of "abc" cb00753f 45a35e8b.

   In Int64, which js_of_ocaml emulates: correct in a browser too, slower.

   References: FIPS 180-4 (NIST, 2015), sections 4.2.3, 5.3.4, 5.3.5
   and 6.4. *)

val digest : string -> string (* SHA-512, 64 bytes *)
val digest384 : string -> string (* SHA-384, 48 bytes *)
