(* Tls13: a TLS 1.3 client -- the handshake that agrees on keys and
   checks who the server is, then records encrypted with them (Eric
   Rescorla, RFC 8446, 2018).

       client                                         server
       ClientHello: a random, the ciphers we know,
         our X25519 public key (key_share),
         the host (server_name)            -------->
                                           <--------  ServerHello: its X25519 key
                        (both compute the shared secret: X25519.mli;
                         from here on, everything is encrypted)
                                           <--------  EncryptedExtensions
                                                      Certificate: its chain (X509)
                                                      CertificateVerify: the
                                                        transcript signed by its key
                                                      Finished: an HMAC of the
                                                        transcript
       Finished                            -------->
       application data                    <------->  application data

   One round trip. The *transcript* -- every handshake message so far,
   hashed (SHA-256) -- is what everything is bound to: the keys are
   derived from the shared secret and the transcript (the key schedule,
   HKDF), CertificateVerify signs it, each Finished is an HMAC of it, so
   a man in the middle who changed a byte of the hello is found out.

   The key schedule:

       0 --Extract--> early --Derive("derived")--> salt
       salt, shared secret --Extract--> handshake secret
           --Derive("c hs traffic", CH..SH)--> client handshake keys
           --Derive("s hs traffic", CH..SH)--> server handshake keys
           --Derive("derived")--> salt, 0 --Extract--> master secret
               --Derive("c ap traffic", CH..server Finished)--> client keys
               --Derive("s ap traffic", CH..server Finished)--> server keys

   each Derive an HKDF-Expand-Label: HKDF-Expand with the label "tls13
   ...", its length and a transcript hash as the info.

   A record: a 5-byte header (type, 0x0303, length) and, once
   encrypted, the AEAD (ChaCha20-Poly1305 or AES-128-GCM) of the data
   and its real type, the nonce the key's IV xored with the record's
   number, the header as associated data.

   Ours offers X25519 only, and the two SHA-256 suites
   (TLS_CHACHA20_POLY1305_SHA256 first, TLS_AES_128_GCM_SHA256); it
   checks the chain with a function it is given ([verify], X509.verify
   with the trusted roots and the time), the signature in
   CertificateVerify, and the server's Finished. A server that asks for
   a client certificate (Gmail's SMTP does, optionally) gets an empty
   one. Not: resumption, 0-RTT, client certificates of our own,
   HelloRetryRequest, other groups.

   A pure machine, as Smtp's: [received] is given the bytes that came
   and answers the bytes to send; the socket is Tls_client's.

   Worked example (checked by the tests): RFC 8448's "simple 1-RTT
   handshake", its keys and messages -- the shared secret, every secret
   of the key schedule, every key and IV, the server's encrypted flight
   decrypted into its four messages, the CertificateVerify's RSA-PSS
   signature, both Finished, the application data and the alerts, byte
   for byte; and a whole handshake with a local openssl s_server
   (Tls_client's tests).

   References: RFC 8446, "The Transport Layer Security (TLS) Protocol
   Version 1.3" (2018): 4 the handshake, 5 the records, 7 the key
   schedule; RFC 8448, "Example Handshake Traces for TLS 1.3" (2019);
   Michael Driscoll, "The Illustrated TLS 1.3 Connection"
   (tls13.xargs.org), every byte of one connection explained. *)

(*****************************************************************************)
(* {1 The key schedule and the records} *)
(*****************************************************************************)

val hkdf_expand_label : string -> label:string -> context:string -> int -> string

(* [derive_secret secret label transcript]: the transcript's messages,
 * concatenated, hashed *)
val derive_secret : string -> string -> string -> string

(* the handshake secret from the shared secret, and the master secret
 * from the handshake secret *)
val handshake_secret : string -> string
val master_secret : string -> string

type cipher = Chacha20_poly1305 | Aes128_gcm

(* a direction's keys, and how many records it has sealed or opened *)
type keys = { cipher : cipher; key : string; iv : string; seq : int }

val traffic_keys : cipher -> string -> keys

(* HMAC(finished_key(secret), hash(transcript)) *)
val finished : string -> string -> string

(* a record sealed: its bytes, and the keys one further *)
val seal : keys -> int -> string -> string * keys

(* [open_record keys header body]: the real content type and the data *)
val open_record : keys -> string -> string -> ((int * string) * keys) option

(*****************************************************************************)
(* {1 The client} *)
(*****************************************************************************)

type t

type state =
  | Handshaking
  | Open (* application data may flow *)
  | Closed (* the server said close_notify *)
  | Failed of string

(* [client ~host ~random ~secret ~session_id ~verify]: the machine, and
 * the ClientHello's record to send first. [random], [secret] (our X25519
 * key) and [session_id] are 32 random bytes each, the caller's (a pure
 * machine rolls no dice) *)
val client :
  host:string -> random:string -> secret:string -> session_id:string -> verify:(X509.t list -> (unit, string) result) -> t * string

(* the bytes that came from the server: the machine, and the bytes to
 * send back *)
val received : t -> string -> t * string

val state : t -> state

(* the application data arrived since the last call *)
val read : t -> t * string

(* application data to send, as records *)
val write : t -> string -> t * string

(* close_notify *)
val close : t -> t * string

(* the server's certificates, once received; the cipher chosen *)
val certificates : t -> X509.t list
val cipher : t -> cipher option
