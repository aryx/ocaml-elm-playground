(* Chacha20, Poly1305, Chacha20_poly1305, Aes, Gcm: RFC 8439's worked
 * examples (2.3.2, 2.4.2, 2.5.2, 2.8.2), FIPS 197's (C.1, C.3), the GCM
 * paper's test case 3, and sealed messages of other lengths computed
 * by Python's cryptography package; a changed byte refused *)
val tests : Testo.t list
