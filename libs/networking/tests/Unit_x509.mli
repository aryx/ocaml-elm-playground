(* Asn1, Pem, X509: DER's worked examples; four roots of the system's
 * store (tls/roots.pem), their own signatures checked (ECDSA P-384,
 * RSA 4096); four real chains captured once with openssl (tls/*.pem:
 * Gmail's, Google's in RSA, Wikipedia's wildcard, GitHub's), verified
 * at the date they were captured, and refused for another host, years
 * later, with a byte changed, or with no roots *)
val tests : Testo.t list
