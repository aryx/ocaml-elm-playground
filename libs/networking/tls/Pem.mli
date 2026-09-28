(* Pem: certificates as text, the base64 of their DER between two lines
   (Privacy-Enhanced Mail, RFC 1421, 1993 -- mail again, Mail.mli; its
   format outlived it, RFC 7468):

       -----BEGIN CERTIFICATE-----
       MIICCTCCAY6gAwIBAgINAgPlwGjvYxqccpBQUjAKBggqhkjOPQQDAzBHMQswCQYD
       ...
       -----END CERTIFICATE-----

   How the system keeps its trusted roots (a bundle of them, one file:
   /etc/ssl/certs/ca-certificates.crt) and how openssl shows a chain.

   References: RFC 7468, "Textual Encodings of PKIX, PKCS, and CMS
   Structures" (2015). *)

(* the DER of each "CERTIFICATE" block, in order *)
val certificates : string -> string list
