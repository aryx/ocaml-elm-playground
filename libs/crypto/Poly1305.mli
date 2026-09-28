(* Poly1305: a one-time authenticator -- a tag of 16 bytes that only
   who knows the key could have made for this message, and a key that
   must never be used twice (D. J. Bernstein, 2005; RFC 8439).

   The message, cut in blocks of 16 bytes, each read as a number
   (little-endian) with a 1 appended above its top byte, is the
   polynomial evaluated at a secret point r, modulo the prime 2^130 - 5
   (hence the name):

       acc = 0
       for each block:  acc = (acc + block) * r   mod 2^130 - 5
       tag = acc + s    mod 2^128

   r and s are the key's two halves, r "clamped" (some bits cleared) so
   that the products stay small. Here as poly1305-donna does it: 130
   bits as five limbs of 26, their products 52 bits, the reduction by
   2^130 = 5 (mod p) a multiplication by 5.

   Worked example (RFC 8439 section 2.5.2, checked by the tests): the key
   85d6be78 57556d33 7f4452fe 42d506a8 0103808a fb0db2fd 4abff6af 4149f51b
   and "Cryptographic Forum Research Group" give a8061dc1 305136c6
   c22b8baf 0c0127a9.

   Native ints, 63 bits (the products need 55).

   References: RFC 8439 (2018), section 2.5; D. J. Bernstein, "The
   Poly1305-AES message-authentication code" (FSE 2005); Andrew Moon,
   poly1305-donna (2014). *)

(* [mac ~key message]: the 16-byte tag; the key is 32 bytes *)
val mac : key:string -> string -> string
