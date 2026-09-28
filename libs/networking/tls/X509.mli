(* X509: certificates -- who a key belongs to, said by someone we
   already trust (ITU-T X.509, 1988; for the web, RFC 5280).

   A certificate binds a name to a public key, and is signed by its
   *issuer*; the issuer's certificate is signed by its issuer, up to a
   *root* the system trusts because it shipped with it:

       pop.gmail.com  --signed by-->  WE2  --signed by-->  GTS Root R4
       (the server's key)             (an intermediate)    (in /etc/ssl/certs)

   The server sends its certificate and the intermediates; the client
   builds the path up to one of its roots and checks, at each step: the
   issuer's name is the next one's subject, the signature checks with
   the next one's key, the next one is allowed to sign (a CA: basic
   constraints), today is within each one's dates -- and that the first
   one names the host we wanted (subjectAltName, "*.wikipedia.org"
   covering one label). A certificate the server sends that we do not
   need (Gmail sends GTS Root R4 cross-signed by GlobalSign, for old
   clients) is ignored once a trusted root is reached.

       Certificate ::= SEQUENCE {
         tbsCertificate       -- "to be signed": version, serial,
                              -- issuer, validity, subject, key, extensions
         signatureAlgorithm   -- ecdsa-with-SHA384, sha256WithRSAEncryption...
         signatureValue       -- the issuer's signature over tbsCertificate }

   Not checked: revocation (CRLs, OCSP), name constraints, key usage,
   policies -- the parts a browser adds, and what this teaching client
   leaves out on purpose (the tutorial says so).

   Worked examples (checked by the tests, on real chains captured once
   with openssl, at a fixed date): Gmail's (ECDSA P-256, a P-384 root,
   the extra cross-signed root), Google's (RSA), Wikipedia's (a wildcard
   name, a cross-signed "Root YE"), GitHub's (Sectigo's ECC); each
   refused for another host, after its dates, or with a byte changed;
   the roots' own signatures checked.

   References: RFC 5280, "Internet X.509 Public Key Infrastructure
   Certificate and CRL Profile" (2008): 4.1 the structure, 4.2.1.9
   basic constraints, 4.2.1.6 subjectAltName, 6 path validation; RFC
   6125 (2011), host names, wildcards. *)

type public_key =
  | Rsa of Bignum.t * Bignum.t (* n, e *)
  | Ec of Ecdsa.curve * string (* the point, SEC 1 *)
  | Other of string (* an algorithm's OID we do not know *)

type t = {
  der : string;
  tbs : string; (* what the issuer signed *)
  issuer : string; (* the issuer's Name, its DER *)
  subject : string;
  common_name : string; (* the subject's CN, to show *)
  not_before : float;
  not_after : float;
  key : public_key;
  algorithm : string; (* the signature's OID *)
  signature : string;
  names : string list; (* subjectAltName's dNSNames *)
  ca : bool; (* basic constraints: may sign certificates *)
}

val parse : string -> (t, string) result

(* was [cert] signed by [issuer]'s key? *)
val signed_by : issuer:t -> t -> bool

(* does it name this host (subjectAltName, a wildcard for one label;
 * the common name when there is no subjectAltName)? *)
val names_host : t -> string -> bool

(* [verify ~trust ~now ~host chain]: the server's chain (its certificate
 * first) checked up to one of [trust]'s roots *)
val verify : trust:t list -> now:float -> host:string -> t list -> (unit, string) result

(* a signature by the certificate's key, in TLS 1.3's SignatureScheme
 * [scheme] (0x0403 ecdsa_secp256r1_sha256, 0x0503 ecdsa_secp384r1_sha384,
 * 0x0804 rsa_pss_rsae_sha256, 0x0805, 0x0806, 0x0401 rsa_pkcs1_sha256...) *)
val verify_scheme : t -> scheme:int -> message:string -> signature:string -> bool
