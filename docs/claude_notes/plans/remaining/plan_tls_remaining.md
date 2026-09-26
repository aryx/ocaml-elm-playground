# Plan: what's left for TLS

The plan is done: see [`done/plan_tls.md`](../done/plan_tls.md) -- a
TLS 1.3 client (RFC 8446, 2018) from scratch, and curl gone. The
primitives in `libs/crypto/` (`Sha256`, `Sha512`, `Hmac`, `Hkdf`,
`Chacha20`, `Poly1305`, `Chacha20_poly1305`, `Aes`, `Gcm`, `Bignum`,
`X25519`, `Ecdsa` over P-256 and P-384, `Rsa` with PKCS#1 v1.5 and
PSS); certificates and the protocol in `libs/networking/tls/` (`Asn1`,
`Pem`, `X509`, `Tls13`, a pure machine checked byte for byte against
RFC 8448's trace); the socket, `Tls_client`, under TinyEudora's Gmail,
`Http_client`'s `https://` and `tls_get`; ocurl out of the dune files;
and the tutorial, `notes_tls.md`.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. A faster GHASH

- `Gcm`'s GHASH multiplies a bit at a time: 300 ms for 500 KB, where
  ChaCha20-Poly1305 takes 31 ms. Every server tried chose ChaCha20,
  which we offer first, so it has not mattered yet.
- A table of H's multiples, precomputed per key, to multiply 4 or 8
  bits at a time; the GCM paper's test cases already check the
  answers.

## 2. HelloRetryRequest

- We offer only X25519. A server that wants another group (P-256)
  answers with a HelloRetryRequest, which `Tls13` refuses today: the
  second ClientHello, the transcript's special hash of the first, and
  an ECDHE over P-256 (`Ecdsa` has the curve already).

## 3. Resumption and 0-RTT

- The server's NewSessionTicket kept, a PSK offered at the next
  connection: one round trip without certificates to check (the
  signature checks are most of a handshake's cost).
- 0-RTT, early data sent with the first flight, and its replay
  problem, as a lesson more than a feature.

## 4. Revocation

- Certificates are checked (chain, names, dates, basic constraints,
  signatures); revocation is not: CRLs and OCSP (or OCSP stapling,
  the answer inside the handshake).

## 5. Client certificates of our own

- Gmail's SMTP asks for one, and gets an empty Certificate (RFC 8446,
  4.4.2). Sending a real one: a key and a certificate read from PEM,
  the CertificateVerify signed by us (`Ecdsa` and `Rsa` would need
  signing, not only verifying).

## 6. TLS 1.2

- A server that speaks only 1.2 is refused. None among those tried
  was, so this is last: another key exchange and record format, and
  most of the old algorithms 1.3 dropped.

## 7. Open question: a decision for the author to review

Not a piece of work: a choice made during the night the plan was done,
left for the author.

- `Tls_client` reads the system's roots and /dev/urandom under
  `Cap.network` alone, as curl did: reading them is part of what
  connecting with TLS means. The other way is to ask `Cap.open_in` of
  every program that fetches an https:// picture. Kept as it is until
  the author decides; `Tls_client.mli` says why.
