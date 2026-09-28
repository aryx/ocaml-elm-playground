(* Chacha20_poly1305: encryption that also proves the message was not
   changed -- an AEAD, "authenticated encryption with associated data"
   (RFC 8439, section 2.8): what protects each TLS 1.3 record.

       key, nonce --Chacha20 block 0--> the first 32 bytes: Poly1305's
                                        one-time key (never reused: a
                                        new nonce per record)
       plaintext  --Chacha20 from block 1--> ciphertext
       tag = Poly1305(aad | pad | ciphertext | pad | len aad | len ct)

   The *associated data* is authenticated but not encrypted -- in TLS,
   the record's header, which the network must read. Opening checks the
   tag before giving anything back: a changed byte, and nothing.

   Worked example (RFC 8439 section 2.8.2, checked by the tests): the
   sunscreen text again, with its key, nonce and AAD 50515253 c0c1c2c3
   c4c5c6c7, gives a ciphertext starting d31a8d34 648e60db and the tag
   1ae10b59 4f09e26a 7e902ecb d0600691.

   References: RFC 8439 (2018), sections 2.6-2.8. *)

(* ciphertext followed by the 16-byte tag *)
val seal : key:string -> nonce:string -> aad:string -> string -> string

(* the plaintext, or None if the tag does not check *)
val open_ : key:string -> nonce:string -> aad:string -> string -> string option

(* two tags compared in time that does not depend on where they differ *)
val same : string -> string -> bool
