(* Hkdf: keys from a secret, HMAC-based (Hugo Krawczyk, 2010; RFC
   5869) -- TLS 1.3's key schedule is made of nothing else (Tls13.mli).

   Two steps. Extract: a secret that may be lumpy (a Diffie-Hellman
   result, not uniformly random) made into a uniform key, PRK =
   HMAC(salt, IKM). Expand: as many bytes as wanted from it, blocks
   T(i) = HMAC(PRK, T(i-1) || info || i), info saying what they are
   for, so that the same PRK gives independent keys for independent
   uses:

       IKM --Extract(salt)--> PRK --Expand("c hs traffic")--> a key
                                  --Expand("s hs traffic")--> another

   Worked example (RFC 5869's test case 1, checked by the tests): IKM
   22 bytes 0x0b, salt 000102..0c, info f0f1..f9, 42 bytes: PRK 077709
   36 2c2e32df..., OKM 3cb25f25 faacd57a...

   References: RFC 5869 (2010); Hugo Krawczyk, "Cryptographic Extraction
   and Key Derivation: The HKDF Scheme" (CRYPTO 2010). *)

(* [extract ~hmac ~salt ikm] *)
val extract : hmac:(string -> string -> string) -> salt:string -> string -> string

(* [expand ~hmac prk ~info length] *)
val expand : hmac:(string -> string -> string) -> string -> info:string -> int -> string
