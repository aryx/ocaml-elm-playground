# Plan: TLS 1.3 from scratch, and curl gone

## Context

Two things here still borrow someone else's TLS: `https://` goes
through curl (graphics/images' `Download`, native_common's `Commands`),
and TinyEudora's Gmail through openssl's `s_client` run beside us
(`Tls_tunnel`). Plain HTTP, SMTP and POP3 are ours; the encryption
under them is not. `plan_dependencies_remaining.md` section 2 (option
2) and `plan_teaching_other.md` section 4b sketched the way: TLS 1.3
(RFC 8446, 2018) is the version to write -- one round trip, a handful
of algorithms, the old ones gone -- and each of its pieces is a
famous, small, well-specified algorithm with test vectors in its RFC.

The author asked for it (2026-09-26), after TinyEudora's Gmail.

## What a TLS 1.3 client needs

| piece | what for | standard |
|---|---|---|
| SHA-256, SHA-384 | the transcript, the key schedule; certificates' signatures | FIPS 180-4 |
| HMAC, HKDF | the key schedule, Finished | RFC 2104, 5869 |
| X25519 | the key exchange (ephemeral Diffie-Hellman) | RFC 7748 |
| ChaCha20-Poly1305 | the records (an AEAD) | RFC 8439 |
| AES-128-GCM | the records, for the servers without ChaCha20 | FIPS 197, SP 800-38D |
| big natural numbers, Montgomery multiplication | RSA and the curves | Knuth vol. 2; Montgomery 1985 |
| ECDSA over P-256 and P-384 | certificates and CertificateVerify (Google's are ECDSA) | FIPS 186-4, SEC 1 |
| RSA, PKCS#1 v1.5 and PSS | certificates and CertificateVerify (most of the web's) | RFC 8017 |
| ASN.1 DER, X.509 | reading certificates | X.690, RFC 5280 |
| the trust store | the roots: the system's PEM bundle | RFC 7468 |
| the handshake and the records | ClientHello to Finished, then application data | RFC 8446 |

Where: the primitives in `libs/crypto/` (library `crypto`, one idea
per module, as `Sha1`); the formats and the protocol in a new
`libs/networking/tls/` (library `networking_tls`: `Asn1`, `X509`,
`Pem`, `Tls13` -- pure, bytes in and bytes out, as `Smtp` and `Pop3`
are); the socket in `networking/unix/` (`Tls_client`).

**Portability.** `crypto` also runs in a browser (`Sha1`, for the
WebSocket handshake), where js_of_ocaml's `int` is 32 bits. The hashes
work in `Int32`/`Int64` and run everywhere; the big numbers, the
curves and Poly1305 use native 63-bit ints and say so -- TLS runs only
natively anyway (a page has no sockets; the browser does its own).

**Security, said plainly.** A teaching implementation: correct
(checked against the RFCs' vectors, RFC 8448's whole handshake, and a
second implementation, Python's `cryptography`), but not hardened --
not constant time, no side-channel care. Certificates are checked
(chain, names, dates, basic constraints, the signatures), revocation
is not.

## Phases

1. **Hashes and keys**: `Sha256`, `Sha512` (and 384), `Hmac`, `Hkdf`;
   FIPS and RFC 4231/5869 vectors.
2. **The records' ciphers**: `Chacha20`, `Poly1305`,
   `Chacha20_poly1305`; `Aes`, `Gcm`; RFC 8439, FIPS 197, the GCM
   paper's test cases.
3. **Numbers and curves**: `Bignum` (naturals, Montgomery), `X25519`,
   `Ecdsa` (P-256, P-384; each curve's G checked on it and n·G = O),
   `Rsa` (PKCS#1 v1.5, PSS); RFC 7748, and signatures made by Python's
   `cryptography`.
4. **Certificates**: `Asn1`, `X509`, `Pem`; real roots as fixtures,
   their self-signatures verified (one ECDSA P-384, one P-256, one RSA).
5. **The protocol**: `Tls13`, the client's machine; RFC 8448's
   "simple 1-RTT handshake" replayed byte for byte (its keys, its
   Finished, its records).
6. **The socket, and the world**: `Tls_client`, a Transport and a
   byte stream; tried against Gmail's POP3 and SMTP and a few web
   servers; `Transport.tunnel` ours (openssl's kept behind a flag).
7. **curl gone**: `https://` by `Http_client` and `Http_request` over
   `Tls_client`; the ocurl dependency out of the dune files and the
   opam packages -- only if 6 holds with the sites TinyChrome visits.

## Status

**Phases 1 to 7 done** (2026-09-26), in one night, the author asleep;
nothing committed, for the author's review.

- **1-3, the primitives** (`libs/crypto/`): `Sha256`, `Sha512` (384),
  `Hmac`, `Hkdf`, `Chacha20`, `Poly1305`, `Chacha20_poly1305`, `Aes`
  (its S-box computed from its definition), `Gcm`, `Bignum` (26-bit
  limbs, Montgomery's CIOS), `X25519`, `Ecdsa` (P-256, P-384; each G on
  its curve and n*G = O checked, which confirms the constants), `Rsa`
  (PKCS#1 v1.5, PSS). Tests: the RFCs' and FIPS' vectors (taken from
  the RFCs' text by scripts, not retyped), and a second implementation,
  Python's `cryptography`, for sealed messages of odd lengths and for
  signatures, each also refused once a byte changes. All passed at the
  first run, which is why the tests were then checked to have run.
- **4, certificates** (`libs/networking/tls/`): `Asn1`, `Pem`, `X509`
  (parsing; signatures; host names with wildcards; path building up to
  a trusted root, basic constraints, dates). Fixtures: four real chains
  captured with openssl (Gmail's with its cross-signed root, Google's
  RSA, Wikipedia's wildcard through a cross-signed "Root YE", GitHub's
  Sectigo) and their roots, at a fixed date.
- **5, the protocol**: `Tls13`, the client as a pure machine; checked
  step by step against RFC 8448's simple 1-RTT trace (extracted by a
  script, page breaks and all): the shared secret, every secret, key
  and IV, the server's flight opened (AES-128-GCM), its RSA-PSS
  CertificateVerify, both Finished, application data, alerts. The
  extraction had two bugs of its own (page breaks, a wrong index), each
  caught by a length check or by the RFC's own bytes.
- **6, the socket and the world**: `Tls_client` (TCP, /dev/urandom, the
  system's roots, a non-blocking loop; lines for Transport.tls and a
  blocking `exchange`), tested against a local `openssl s_server` (both
  ciphers, ECDSA and RSA certificates, a CertificateRequest, refusals);
  `tls_get`, a program, fetched Wikipedia, Google, GitHub, Hacker News,
  example.com and Debian, their chains checked. TinyEudora's Gmail over
  it (POP3 and SMTP answered, in Gmail's words, with no account); Gmail's
  SMTP asks for a client certificate, which the first version refused:
  an empty Certificate now (RFC 8446, 4.4.2), with a test.
- **7, curl gone**: `Http_client` speaks `https://` over `Tls_client`
  (`fetch`, the final URL too); `Commands`' `https://` and `Download`
  use it; `ocurl` out of `dune-project` and every dune file. Checked:
  Turtle's picture from elm-lang.org (the bytes identical to curl's
  command line's), HttpText on example.com, TinyChrome on Hacker News.

**A decision for the author to review**: `Tls_client` reads the
system's roots and /dev/urandom under `Cap.network` alone, as curl did
-- reading them is part of what connecting with TLS means -- rather than
asking `Cap.open_in` of every program that fetches an https:// picture.

**Measured**: a handshake is mostly its signature checks (ECDSA P-384
31 ms after `Bignum`'s allocation-free additions, `notes_opti_ocaml.md`
§12); chains already checked are cached per program. The arithmetic
runs in OCaml, so a fetch on a worker thread now holds the runtime lock
while computing, where curl's C did not (`notes_browser.md`).
AES-GCM's GHASH is a bit at a time (300 ms for 500 KB); every server
tried chose ChaCha20-Poly1305, which we offer first (31 ms).

Not done: revocation, resumption and 0-RTT, HelloRetryRequest (we
offer only X25519), client certificates of our own, TLS 1.2 (a server
that speaks only 1.2 is refused: none among those tried).

**Phase 8, the tutorial** (2026-09-26): `tutorials/notes_tls.md` -- the
three promises and what they do not cover, one connection end to end
(a real `tls_get` transcript), X25519 and forward secrecy, the key
schedule, the records' AEADs and the nonce rule, certificates and their
chains (Wikipedia's cross-signed Root YE, Gmail's extra root),
CertificateVerify, the signatures and Montgomery's numbers, how we know
it is right (and what the method caught), the costs measured, what is
left out and why it is not hardened, and the history from SSL 2.0.
