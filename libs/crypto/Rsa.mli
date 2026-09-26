(* Rsa: checking a signature made with RSA (Ron Rivest, Adi Shamir and
   Leonard Adleman, 1977) -- what signs most of the web's certificates.

   A public key is a modulus n (the product of two secret primes, 2048
   bits or more) and an exponent e (almost always 65537); signing is
   raising to the secret power d, checking is raising to e:

       m = s^e mod n       and m must be the message's hash, *encoded*

   The encoding is what keeps it safe, two of them in use (RFC 8017):

   - PKCS#1 v1.5 (1993): 00 01 FF FF ... FF 00, then the hash wrapped in
     its DigestInfo (the DER that names the hash algorithm), as long as
     n -- certificates are signed so;
   - PSS (Mihir Bellare and Phillip Rogaway, 1996): the hash with a
     random salt, masked by MGF1 (a hash stretched), ending in 0xbc --
     provably safe, and what TLS 1.3 wants a server's handshake signed
     with (RSA_PSS_RSAE_SHA256).

   Worked examples (checked by the tests): signatures of both kinds made
   by Python's cryptography package with a 2048-bit key accepted, and
   refused for a changed message; a real root certificate's (X509's).

   References: RFC 8017, "PKCS #1: RSA Cryptography Specifications
   Version 2.2" (2016), sections 8.1.2, 8.2.2, 9.1.2 (EMSA-PSS-VERIFY),
   9.2 and appendix B.2.1 (MGF1). *)

type hash = Sha256 | Sha384 | Sha512

val digest : hash -> string -> string

val verify_pkcs1 : n:Bignum.t -> e:Bignum.t -> hash -> message:string -> signature:string -> bool

(* PSS, the salt as long as the hash, MGF1 with the same hash *)
val verify_pss : n:Bignum.t -> e:Bignum.t -> hash -> message:string -> signature:string -> bool
