(* X25519: Diffie-Hellman on Curve25519 -- two strangers agree on a
   secret over a line everyone listens to (D. J. Bernstein, 2006; RFC
   7748): TLS 1.3's key exchange, a new pair of keys per connection.

       client: secret a, sends A = a*G         server: secret b, sends B = b*G
       client computes a*B      =      b*A     server computes
                        = ab*G, the shared secret: nobody else has a or b

   G is the point of x = 9 on the curve y^2 = x^3 + 486662 x^2 + x over
   the integers modulo the prime 2^255 - 19, and a*G is computed on x
   alone, by the *Montgomery ladder*: two points kept a step apart, one
   doubled and the other added each bit of a, the same work whatever
   the bit (a first defence against timing: here in Bignum's arithmetic,
   itself not constant time). The scalar is "clamped": its three low
   bits cleared (a multiple of the cofactor 8) and its bit 254 set.

   Worked examples (RFC 7748, checked by the tests): section 5.2's two
   vectors, and 6.1's Alice and Bob, whose shared secret is 4a5d9d5b
   a4ce2de1 728e3bf4 80350f25 e07e21c9 47d19e33 76f09b3c 1e161742.

   References: RFC 7748, "Elliptic Curves for Security" (2016), section
   5; D. J. Bernstein, "Curve25519: new Diffie-Hellman speed records"
   (PKC 2006). *)

(* [scalar_mult k u]: k (32 bytes, clamped here) times the point of x = u
 * (32 bytes); little-endian, as the RFC *)
val scalar_mult : string -> string -> string

(* the public key of a secret: k times the base point, 9 *)
val public_key : string -> string
