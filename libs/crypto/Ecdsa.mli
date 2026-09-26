(* Ecdsa: checking a signature made with an elliptic curve (NIST's
   curves P-256 and P-384, FIPS 186-4; the algorithm, DSA's on a curve,
   Scott Vanstone 1992) -- what signs Google's certificates, and their
   servers' handshakes.

   A curve is y^2 = x^3 - 3x + b modulo a prime p, its points (and a
   point at infinity, O) a group: two points added by the chord through
   them, a point doubled by its tangent. G is a point of prime order n
   (n*G = O). A private key is a number d, the public key Q = d*G --
   going back from Q to d is the discrete logarithm, which nobody knows
   how to do on these curves.

   A signature of a hash e is two numbers (r, s); it checks if

       w = 1/s mod n,  u1 = e*w mod n,  u2 = r*w mod n
       (x, y) = u1*G + u2*Q          and x mod n = r

   The points in Jacobian coordinates (X/Z^2, Y/Z^3), so that adding
   needs no division until the end, and the field in Montgomery's form
   (Bignum.mli); a*P by double and add, a bit of a at a time.

   The curves' constants are FIPS 186-4's; the tests check each G is on
   its curve and that n*G = O, which a wrong digit would break.

   Worked examples (checked by the tests): signatures made by Python's
   cryptography package over P-256 with SHA-256 and P-384 with SHA-384
   accepted, and refused for a changed message or signature; a real
   certificate's (X509's tests).

   References: FIPS 186-4, "Digital Signature Standard" (NIST, 2013),
   6.4 and appendix D (the curves); SEC 1 v2 (Certicom, 2009), 4.1.4
   (verifying); Henri Cohen et al., "Efficient Elliptic Curve
   Exponentiation Using Mixed Coordinates" (1998), Jacobian
   coordinates; the formulas of the Explicit-Formulas Database
   (dbl-2001-b, add-2007-bl). *)

type curve

val p256 : curve
val p384 : curve

(* the curve's size in bytes: 32, 48 *)
val size : curve -> int

(* [verify curve ~public ~hash ~r ~s]: [public] the point as SEC 1 writes
 * it uncompressed (04, x, y), [hash] the message's digest *)
val verify : curve -> public:string -> hash:string -> r:Bignum.t -> s:Bignum.t -> bool

(* for the tests: is G on the curve, and n*G the point at infinity? *)
val generator_ok : curve -> bool
