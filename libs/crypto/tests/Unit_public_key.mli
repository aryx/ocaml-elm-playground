(* Bignum, X25519, Ecdsa, Rsa: Python's integers for products and
 * remainders of random big numbers; RFC 7748's X25519 vectors and its
 * Alice and Bob; each curve's generator checked on its curve and of
 * order n; ECDSA (P-256, P-384) and RSA (PKCS#1 v1.5, PSS) signatures
 * made by Python's cryptography package accepted, and refused once a
 * byte changes *)
val tests : Testo.t list
