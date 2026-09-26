# TLS 1.3, from scratch: a tutorial for `crypto/`, `networking/tls/` and `Tls_client`

How a browser talks to a server that nobody else can read, nobody can
change, and that is really who it says: the handshake that agrees on
keys in public, the records those keys protect, and the certificates
that say whose key it is. All of it written here from the standards,
about 1,900 lines, and used for every `https://` this repository
fetches -- TinyChrome's pages, TinyEudora's Gmail, the pictures of
`image "https://..."`.

It goes with [`plan_tls.md`](../plans/plan_tls.md), which built it;
[`notes_mail.md`](notes_mail.md) §10, whose POP3 and SMTP run inside
it; and [`notes_networking.md`](notes_networking.md), for what is under
it (TCP).

## 0. Where the code is, and a reading order

The primitives are `crypto/` (pure OCaml, one idea per module, each
`.mli` with its worked example); the protocol is `networking/tls/`, a
pure machine as `Smtp` is; only `Tls_client` touches a socket.

| module | what | section |
|---|---|---|
| `crypto/X25519` | agreeing on a secret in public | §3 |
| `crypto/Sha256`, `Sha512`, `Hmac`, `Hkdf` | from one secret, many keys | §4 |
| `crypto/Chacha20`, `Poly1305`, `Chacha20_poly1305`, `Aes`, `Gcm` | the records: encrypted, and proved unchanged | §5 |
| `tls/Asn1`, `Pem`, `X509` | certificates, and their chains | §6 |
| `crypto/Bignum`, `Ecdsa`, `Rsa` | the signatures, and the numbers under them | §7 |
| `tls/Tls13` | the client: the handshake, the key schedule, the records | §2, §4 |
| `unix/Tls_client`, `tlsget/tls_get` | a socket, the system's roots; a page fetched, the handshake shown | §9 |

Read §1-§2 for what TLS promises and how a connection goes, §3-§5 for
the keys and what they protect, §6-§7 for who is on the other end,
§8 for how we know it is right, and §9-§11 for the real web, what is
left out, and how we got here.

## 1. Three promises, and what they do not cover

Between you and a server there are routers, Wi-Fi access points,
Internet providers -- anyone of them can read the bytes, change them,
or answer in the server's place. TLS promises three things:

- **Nobody reads**: the bytes are encrypted (*confidentiality*).
- **Nobody changes**: a changed byte is found, and the connection
  ends (*integrity*).
- **It is really them**: the server proves it holds the key a
  certificate says belongs to the name you asked for (*authentication*).

Not promised: *who* you talk to (the host's name travels in clear, in
the hello: `server_name`, so that one address can serve many sites),
how much you say and when (the records' sizes and times), and anything
about the server once the bytes arrive -- a real certificate for
`evil.example` is a real certificate.

## 2. One connection, end to end

What `tls_get` shows for Wikipedia:

```
$ dune exec networking/tlsget/tls_get.exe -- https://en.wikipedia.org/wiki/Transport_Layer_Security 4
TLS 1.3 with en.wikipedia.org:443 in 1063 ms, ChaCha20-Poly1305
  *.wikipedia.org                          ECDSA P-256
  YE2                                      ECDSA P-384
  Root YE                                  ECDSA P-384
  ISRG Root X2                             ECDSA P-384
1438693 bytes:
  HTTP/1.1 200 OK
```

and what happened, one round trip:

```
client (Tls13)                                    server
ClientHello  a random; the ciphers we know;
             our X25519 public key (key_share);
             the host (server_name)       -------->
                                          <--------  ServerHello   its X25519 public key
                  both compute the shared secret (§3), and the keys (§4);
                  from here on, every byte is encrypted (§5)
                                          <--------  EncryptedExtensions
                                                     Certificate        its chain (§6)
                                                     CertificateVerify  the transcript,
                                                                        signed with its key
                                                     Finished           an HMAC of the transcript
Finished     an HMAC of the transcript    -------->
GET /wiki/...                             <------->  HTTP/1.1 200 OK ...
```

Everything is bound to the **transcript** -- every handshake message
so far, hashed. The keys are derived from it, CertificateVerify signs
it, each Finished is an HMAC of it. A man in the middle who changes a
byte of the hello (to take out our best cipher, say) changes the
transcript, and the Finished do not check. TLS 1.2 bound its
transcript too, but needed two round trips, and its many options gave
the downgrade attacks room (§11); 1.3 is shorter because
the client guesses the key exchange -- X25519, which everyone speaks --
and sends its key share in the first message.

`Tls13` is a pure machine: `received` is given the bytes that came and
answers the bytes to send. It rolls no dice (the random, the X25519
secret and the session id are given to it) and reads no clock (it is
given a `verify` function that checks the chain at a time). So a test
can replay a whole handshake from a transcript (§8).

## 3. Agreeing on a secret in public: X25519

Whitfield Diffie and Martin Hellman's idea (1976): each side keeps a
secret number and sends a public one; each combines its secret with the
other's public number; both get the same result, which nobody listening
can compute.

```
client: secret a, sends A = a*G         server: secret b, sends B = b*G
client computes a*B     =     b*A       server computes
                        = ab*G   -- nobody else has a or b
```

On Curve25519 (Daniel J. Bernstein, 2006), G is a point, a*G is the
point added to itself a times (done in some 255 doublings and additions,
not a of them), and going back from a*G to a is the *discrete
logarithm*, which nobody knows how to do fast. `X25519.scalar_mult`
works on the x coordinate alone, by the *Montgomery ladder*: two points
kept a step apart, one doubled and one added at each bit, the same work
whatever the bit.

The keys are **ephemeral**: a new pair for every connection, thrown
away after. So a recording of today's traffic stays unreadable even if
the server's certificate key is stolen next year -- *forward secrecy*.
TLS 1.2 still allowed the other way, the client encrypting the secret
with the server's RSA key, where that theft reads everything ever
recorded; 1.3 removed it.

Worked example (RFC 7748, section 6.1, in the tests): Alice's secret
77076d0a..., Bob's 5dab087e...; each computes 4a5d9d5b a4ce2de1
728e3bf4 80350f25 e07e21c9 47d19e33 76f09b3c 1e161742.

## 4. From one secret to many keys: the key schedule

One shared secret, but many keys wanted: the client's and the server's,
for the handshake and for the application, each a key and an IV. They
come from **HKDF** (Hugo Krawczyk, 2010), made of HMAC (§5's cousin: a
hash with a key):

```
0 --Extract--> early --Derive("derived")--> salt
salt, the shared secret --Extract--> handshake secret
    --Derive("c hs traffic", ClientHello..ServerHello)--> client handshake keys
    --Derive("s hs traffic", ClientHello..ServerHello)--> server handshake keys
    --Derive("derived")--> salt, 0 --Extract--> master secret
        --Derive("c ap traffic", ClientHello..server Finished)--> client keys
        --Derive("s ap traffic", ClientHello..server Finished)--> server keys
```

*Extract* turns a secret that may be lumpy (a curve point's coordinate
is not uniformly random) into a uniform one; *Expand* draws as many
bytes as wanted, with a label saying what they are for -- "tls13 c hs
traffic" -- and the transcript's hash as context. Different labels,
independent keys; a different transcript, different keys. That is the
whole of `Tls13`'s key schedule, some 20 lines (`hkdf_expand_label`,
`derive_secret`, `handshake_secret`, `master_secret`, `traffic_keys`),
and RFC 8448's trace checks every value of it (§8).

## 5. The records: encrypted, and proved unchanged

Once the keys are there, every record is **AEAD**-sealed --
"authenticated encryption with associated data":

```
record:  17 03 03 00 35  | ciphertext of (data | real type) | tag (16 bytes)
         \____________/     the header, authenticated (not encrypted:
          type, version,    the network must read the length)
          length
nonce = the key's IV xor the record's number (0, 1, 2, ...)
```

Two AEADs are offered, and every server we tried chose the first:

- **ChaCha20-Poly1305** (Bernstein; RFC 8439). ChaCha20 makes a stream
  of bytes from the key, the nonce and a block counter, with additions,
  xors and rotations only (no table to leak through the cache), and the
  data is xored with it. Poly1305 is the tag: the message's 16-byte
  blocks as a polynomial, evaluated at a secret point modulo the prime
  2^130 - 5 -- here in five limbs of 26 bits, poly1305-donna's way.
- **AES-128-GCM** (Rijndael, 2001; GCM, McGrew and Viega, 2004). AES
  encrypts 16-byte blocks -- here its S-box computed from its
  definition, the inverse in GF(2^8) then an affine map, not typed in --
  in counter mode; GHASH, Poly1305's relative in GF(2^128), is the tag,
  here a bit at a time (slow: §9).

The rules that matter: the **nonce is never reused** under a key (the
record number sees to it: two messages under one nonce leak their xor,
and Poly1305's key); a record that does not **authenticate** is refused
whole, before anything of it is used; and the record's *real* type
(handshake, data, alert) is inside the encryption, so an observer sees
only "application data" of some length.

## 6. Who is on the other end: certificates

X25519 gives a secret shared with *whoever answered*. Who that is, the
server proves with a **certificate**: its public key and its name,
signed by an *issuer* -- whose own certificate is signed by its issuer,
up to a **root** that ships with the system (`/etc/ssl/certs`, 121 of
them here):

```
*.wikipedia.org  --signed by-->  YE2  --signed by-->  Root YE  --signed by-->  ISRG Root X2
(the server's key)               (intermediate)       (sent by the server,       (in the system's
                                                       cross-signed)              store: trusted)
```

A certificate is ASN.1, written in DER: every value a tag, a length and
its bytes (`Asn1.parse`), nested. `X509.parse` reads what matters --
the issuer and subject names, the dates, the public key (RSA or ECDSA
on P-256 or P-384), the subjectAltName's host names, the basic
constraints (may it sign certificates?) -- and keeps the signed part's
bytes, since a signature covers bytes, not values.

`X509.verify` builds the path up from the server's certificate: at each
step, the issuer's name is the next one's subject, the signature checks
with the next one's key, the next one is a CA, and today is within each
one's dates -- until a trusted root has signed. Servers send more than
needed: Gmail sends its root, GTS Root R4, *cross-signed* by an older
one (GlobalSign), for old clients; we stop at the first root we trust.
And the server's certificate must name the host we asked for:
`*.wikipedia.org` covers `fr.wikipedia.org`, not `a.b.wikipedia.org`
(a wildcard covers one label).

A certificate is public: anyone can send Wikipedia's. So the server
must also prove it holds the *private* key, now: **CertificateVerify**,
its signature over the transcript -- 64 spaces, "TLS 1.3, server
CertificateVerify", a zero, the hash -- which includes our random,
fresh for this connection; a recording from yesterday would not do.
Then **Finished**, an HMAC with a key only the two ends can have.

## 7. The signatures, and the numbers under them

Two kinds are in use, and we need both (Google's chain mixes them: an
ECDSA leaf under an RSA intermediate):

- **ECDSA** (`Ecdsa`, P-256 and P-384, FIPS 186-4). A key is a number
  d and the point Q = d*G on the curve y^2 = x^3 - 3x + b modulo a
  prime; a signature of a hash e is (r, s), checked by computing
  u1 = e/s and u2 = r/s modulo the curve's order n and then the point
  u1*G + u2*Q, whose x must be r. The points are kept in Jacobian
  coordinates, so that adding needs no division until the end.
- **RSA** (`Rsa`). A key is a modulus n (2048 to 4096 bits) and an
  exponent e (65537); checking is s^e mod n, which must be the hash in
  its *encoding*: PKCS#1 v1.5 (00 01 FF..FF 00, the hash named in DER),
  what certificates are signed with; or PSS (a random salt, masked),
  what TLS 1.3 wants a handshake signed with.

Both are arithmetic on numbers far bigger than a machine word, and
OCaml has none without a library: `Bignum`, 26-bit limbs so that the
product of two fits a native int, and **Montgomery's multiplication**
(1985) for the thousands of a*b mod m a signature needs -- a number
kept as aR mod m, so that dividing by R is a shift, and no product is
ever divided by m.

A check worth knowing: the curves' constants are long hexadecimal
numbers, easy to get one digit wrong. The tests check that G is on its
curve and that n*G is the point at infinity; a wrong digit breaks both.

## 8. How we know it is right

Cryptographic code that is wrong usually still runs, and often still
"works" against itself. So nothing here is checked only against itself:

- **The standards' own vectors**: FIPS 180 for the hashes, RFC 4231
  and 5869, RFC 8439, FIPS 197, RFC 7748. Taken from the RFCs' text
  by a script, not retyped.
- **A second implementation**: Python's `cryptography` package sealed
  messages of odd lengths and signed with ECDSA and RSA; ours opens
  and checks them, and refuses each once a byte changes.
- **A whole handshake, byte for byte**: RFC 8448 prints one TLS 1.3
  connection with every key and every byte. `Unit_tls13` checks ours
  at each step: the shared secret, the secrets and keys, the server's
  encrypted flight opened into its four messages, its RSA-PSS
  CertificateVerify, both Finished, the data and the alerts.
- **A real server**: `Unit_tls_client` runs `openssl s_server` on
  localhost with a self-signed certificate, over each cipher, with an
  ECDSA and an RSA key -- and checks the refusals (a root not trusted,
  another name).
- **Real chains at a fixed date**: `Unit_x509` has Gmail's, Google's,
  Wikipedia's and GitHub's, captured once, checked as of that day.

What the method caught on the way: the RFC's hex blocks cross page
breaks (a length check in the extraction script); an index that took
the ClientHello's record for the Finished's (the RFC's bytes did not
match -- ours did); Gmail's SMTP asking for a client certificate, which
the first version refused (RFC 8446 says: answer with an empty one).
And the tests that passed on their first run were checked to have run
at all.

## 9. The real web

`tls_get` fetched Wikipedia, Google, GitHub, Hacker News, example.com
and Debian, each chain checked (Debian's is RSA-4096 all the way, its
handshake signed with PSS); TinyChrome draws Hacker News through it,
and TinyEudora reads Gmail (`notes_mail.md` §10).

What it costs, measured (`notes_opti_ocaml.md` §12):

| operation | time |
|---|---|
| X25519 | 3 ms |
| ECDSA P-384, one signature checked | 31 ms |
| RSA-4096, one signature checked | 24 ms |
| the 121 system roots read (once a program) | 74 ms |
| ChaCha20-Poly1305, 500 KB | 31 ms |
| AES-128-GCM, 500 KB (GHASH a bit at a time) | 300 ms |

A handshake is mostly its three or so signatures; a chain already
checked in the program is not checked again (`Tls_client`'s cache), so
a page's pictures from one host cost one CertificateVerify each. One
difference with curl, which this replaced: its arithmetic ran in C,
outside OCaml's runtime lock; ours runs inside it, so a fetch on a
worker thread takes the frames' time while it computes
(`notes_browser.md`).

## 10. What is left out, and why this is not hardened

This is teaching code, correct by every test above, and **not
hardened**:

- **Not constant time.** `Bignum`'s loops, `Ecdsa`'s double-and-add,
  the AES table lookups: how long they take depends on the numbers.
  Paul Kocher (1996) recovered keys from timings alone; a client that
  only *checks* signatures, with public keys, gives away little, but a
  signer or a server must not be written this way.
- **No revocation**: a stolen key's certificate, withdrawn by its CA
  (CRLs, OCSP), is still accepted until its dates run out.
- **One of everything**: X25519 only (a server wanting another group
  would send a HelloRetryRequest, refused), two AEADs, no resumption,
  no 0-RTT, no client certificate of our own, no TLS 1.2 -- every
  server tried spoke 1.3.
- **Name constraints, key usage, policies**: the rest of RFC 5280's
  path validation, which a browser adds.

And one decision to know: `Tls_client` reads the system's roots and the
kernel's randomness (`/dev/urandom`) under the network capability
alone, as curl did -- reading them is part of what connecting with TLS
means -- rather than asking every program that shows an `https://`
picture for the right to read files.

## 11. How we got here

- **1995, SSL 2.0**, Netscape's, for Navigator: the first encrypted web.
  Flawed; **1996, SSL 3.0** (Paul Kocher, Phil Karlton, Alan Freier).
- **1999, TLS 1.0** (RFC 2246): SSL 3.0 standardized by the IETF,
  renamed. **2006, 1.1**; **2008, 1.2** (RFC 5246).
- Then a decade of attacks on the options 1.2 still allowed: BEAST
  (2011) and Lucky Thirteen (2013) on CBC mode, RC4's biases, POODLE
  (2014) on SSL 3.0's padding, downgrades to "export" ciphers (FREAK,
  Logjam, 2015) -- and Heartbleed (2014), not in the protocol but in
  OpenSSL's code, a missing length check reading a server's memory.
- **2015, Let's Encrypt**: certificates free and automatic; most of the
  web moves to `https://` (Wikipedia's and Hacker News' chains above are
  its).
- **2018, TLS 1.3** (RFC 8446, Eric Rescorla): one round trip, and
  everything those attacks used removed -- RSA key transport, CBC, RC4,
  SHA-1, compression, renegotiation, custom Diffie-Hellman groups. What
  is left is small enough to write in a night, which is this.

## References

- RFC 8446, "The Transport Layer Security (TLS) Protocol Version 1.3"
  (Eric Rescorla, 2018); RFC 8448, "Example Handshake Traces for TLS
  1.3" (Martin Thomson, 2019).
- Michael Driscoll, "The Illustrated TLS 1.3 Connection"
  (tls13.xargs.org): every byte of one connection, explained.
- Whitfield Diffie and Martin Hellman, "New Directions in Cryptography"
  (1976); D. J. Bernstein, "Curve25519" (2006); RFC 7748 (2016).
- RFC 5869, HKDF (Hugo Krawczyk, 2010); RFC 2104, HMAC.
- RFC 8439, ChaCha20 and Poly1305 (2018); FIPS 197, AES (2001); NIST SP
  800-38D, GCM (2007).
- FIPS 186-4, ECDSA and the curves (2013); RFC 8017, PKCS #1 (RSA,
  2016); Peter Montgomery, "Modular Multiplication Without Trial
  Division" (1985).
- RFC 5280, X.509 certificates for the Internet (2008); ITU-T X.690,
  DER; RFC 6125, host names in certificates (2011).
- Paul Kocher, "Timing Attacks on Implementations of Diffie-Hellman,
  RSA, DSS, and Other Systems" (CRYPTO 1996).
