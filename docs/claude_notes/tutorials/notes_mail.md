# Mail, from scratch: a tutorial for `networking/mail/`, `Smtp`, `Pop3` and TinyEudora

How a message goes from one person to another: the shape every mail
has had since 1982 and the two conversations that carry it, one to
send and one to fetch. All of it is text a person can read, and type:
by the end you will have sent mail by hand, with telnet, the way
people did in 1985 -- and seen why anybody can send mail as anybody.

It goes with [`plan_tiny_eudora.md`](../plans/done/plan_tiny_eudora.md), which built it, and
[`notes_networking.md`](notes_networking.md) §13, the other protocols
a person can read (HTTP, IRC).

## 0. Where the code is, and a reading order

Mail is **three formats and two protocols**. The formats are pure
OCaml with no network at all (a mailbox is a file); the protocols are
pure too, state machines fed the lines that arrive; only the server,
and the tunnel, touch sockets.

| module | what | section |
|---|---|---|
| `mail/Mail` | a message: header fields, folding, addresses, dates (RFC 822, 5322) | §1 |
| `mail/Mime` | parts, attachments, base64, quoted-printable, encoded words (RFC 2045-2047) | §2 |
| `mail/Mbox` | a mailbox, one file, and its `>From` lines | §3 |
| `protocols/Smtp` | sending: the dialogue, the envelope, the dot (RFC 5321) | §4 |
| `protocols/Pop3` | fetching: the maildrop, deleting at QUIT (RFC 1939) | §5 |
| `unix/Mail_server`, `maild/tiny_maild` | a server: SMTP in, POP3 out | §6 |
| `mail/Mail_thread` | conversations: Zawinski's threading | §8 |
| `apps/internet/TinyEudora` | the client, after Eudora (1988) | §7-§9 |
| `tls/Tls13`, `unix/Tls_client` | today's servers, inside TLS (Gmail): our own TLS 1.3 | §10 |

Read §1-§3 for what a message is, §4-§5 for how it travels, §6 to
watch it happen by hand, §7-§9 for the client, and §10-§11 for what
changed since 1996 and what is left out.

## 1. A message: RFC 822, 1982

A message is **header fields**, an **empty line**, and a **body**:

```
From: Alice <alice@tiny>                  a field: a name, a colon, a value
To: bob@tiny
Subject: lunch
Date: Fri, 25 Sep 2026 12:00:00 +0200
Message-ID: <1@tiny>
Received: from eudora by tiny;            a long value folded: a line that
  Fri, 25 Sep 2026 12:00:01 +0200         starts with a space goes on
                                          the empty line: the headers end
Noon at the usual place?                  the body
```

That is the whole format, and it has not changed since David
Crocker's RFC 822: what came later (MIME, §2) put everything else
*inside* this shape rather than change it. A few things to see:

- **A list, not a table.** A field can appear twice: every server a
  message crosses adds a `Received:` at the top, so read bottom up
  they are its route. `Mail.t` keeps the fields in order, and looks
  them up without regard to case.
- **Kept as written.** `Mail` stores each value exactly as it came,
  folding and all, and `get` unfolds it on the way out: a mailbox
  rewritten after marking one message read must not change the
  others by a byte. The tests read the built-in mailboxes and write
  them back identical.
- **Addresses** come in two forms, `alice@tiny` and
  `Alice <alice@tiny>` (and the old `alice@tiny (Alice)`), in lists
  separated by commas -- but not the commas inside quotes:
  `"Smith, J" <j@tiny>` is one.
- **Dates** carry their own zone, `+0200`: the time *where it was
  written*. TinyEudora shows them as written, but sorts by the instant
  -- which is why Carol's 5:40 PM (-0700) comes after Elodie's 7:05 PM
  (+0200) the same day: it was later in Greenwich. (Writing this
  tutorial's examples found that the plan's "Thu, 25 Sep 2026" was a
  Friday; the date code checks the weekday it writes.)
- **Message-ID, In-Reply-To, References** name messages: what a
  reply answers, and the whole chain back to the first. §8 builds
  conversations from them.

## 2. MIME: everything else, without changing mail (1992)

Mail was 7-bit lines of English. Nathaniel Borenstein and Ned Freed's
MIME added pictures, other languages and attachments by adding a few
**header fields that say how to read the body**, so that every server
that knew nothing of it passed it on unchanged:

```
Content-Type: multipart/mixed; boundary="xyz"      what it is
                                                   (the "preamble", for
This is a multi-part message in MIME format.        the readers of 1992)
--xyz
Content-Type: text/plain                           a part: headers, an empty
                                                   line, a body -- a message
Here are the photos.                                of its own, recursively
--xyz
Content-Type: image/png
Content-Transfer-Encoding: base64                  how its bytes became lines
Content-Disposition: attachment; filename="a.png"

iVBORw0KGgoAAAANSUhEUgAA...
--xyz--                                            the last part has ended
```

A message is a **tree** whose leaves are the text and the attachments
(`Mime.leaves`). The two encodings:

- **base64** (core's `Base64`): three bytes as four letters, for what
  is not text -- lines of 76.
- **quoted-printable**, for text that is mostly ASCII: it stays
  readable, and what is not is `=` and two hex digits. `caf=C3=A9` is
  "café" (é is two bytes in UTF-8, C3 A9), and `=` at a line's end is a
  *soft* break, joining the line to the next -- which lets a long line
  cross a 1982 server that cuts at 1000 characters. Elodie's message in
  TinyEudora's In even cuts an é in two across a soft break, `=C3=` then
  `=A0`, and gets it back.

And headers, which have no Content-Type of their own, hold other
languages in **encoded words** (RFC 2047): `=?utf-8?Q?caf=C3=A9?=`, or
the same in base64, `=?utf-8?B?Y2Fmw6k=?=`. A French subject is the
first thing a French author types.

## 3. A mailbox: one file (1975)

```
From alice@tiny Fri Sep 25 12:00:00 2026     "From ", a space: a message starts
From: Alice <alice@tiny>                     (the colon makes this one a header)
Subject: lunch

Noon at the usual place?
                                             an empty line ends each message
From carol@tiny Thu Sep 24 17:40:00 2026
...
```

The `From ` line is the **envelope's** sender (§4) and the date it
arrived -- not the `From:` header, and the forged message in
TinyEudora's In is the one where they differ. So a line of a body that
starts with `From ` would start a new message: it is written `>From `,
the "From munging" everyone has seen in a quoted email. The first
mailboxes munged only `From `, which cannot be undone -- was a `>From `
in the file written by a person, or by the mailer? `Mbox` is *mboxrd*:
any number of `>` before `From ` gets one more, and reading takes one
away, so every body comes back as it was.

## 4. Sending: SMTP (1982)

A client connects to a server, and they talk, a line each way. The
client says a command; the server answers three digits and a line, the
first digit all a client needs: **2** done, **3** go on, **4** not now,
**5** never. Here is a real session with `tiny_maild`, typed by hand
(§6):

```
S: 220 tiny ESMTP tiny_maild
C: HELO telnet
S: 250 tiny
C: MAIL FROM:<alice@tiny>                                  the envelope:
S: 250 OK                                                  who from,
C: RCPT TO:<bob@tiny>                                      who to
S: 250 OK
C: RCPT TO:<someone@elsewhere.example>
S: 550 not a relay: someone@elsewhere.example is not for tiny
C: DATA
S: 354 End data with <CR><LF>.<CR><LF>
C: From: The President <president@whitehouse.gov>         the message:
C: To: bob@tiny                                            whatever the
C: Subject: hello                                          client says
C:
C: ..a line starting with a dot                            its dot doubled
C: .                                                       the end
S: 250 OK: queued as 1
C: QUIT
S: 221 Bye
```

Two lessons are in it.

**The envelope is not the headers.** The server delivers to the
`RCPT TO`s, whatever `To:` says. That is how **Bcc:** works -- its
recipients are in the envelope and nowhere in the message
(`Smtp.envelope` takes the field out) -- and how a mailing list works.
And it is why **anybody can send mail as anybody**: nothing above
checked `MAIL FROM` or `From:` against anything, and Bob's POP3 will
hand him a message "From: The President". The only honest line is the
`Received:` the server adds, saying where it really came from (the
forged message in TinyEudora's In: press `b`). SPF, DKIM and DMARC,
thirty years later, are the patch; and SMTP AUTH (§10), which a server
of today asks before it takes mail for elsewhere.

**The dot.** The message ends at a line of a single `.`, so a line of
the message that starts with a dot is sent with a second one
(`Smtp.stuff`), and the server takes it off -- the same trick as
mbox's `>From`, for the same reason. (RFC 5321's own example session
has a body line `...etc. etc. etc.`; replaying it, our client sent
`....etc. etc. etc.` -- right, the RFC's line being a placeholder.)

`Smtp.client` is a **state machine**: `step` is given each line the
server sends and answers the lines to send, so a socket, a WebSocket,
a TLS tunnel or a test replaying the RFC can drive it. It sends a
queue of messages over one connection -- Eudora's *Send Queued
Messages*, written for a dial-up modem: write offline, connect once.

## 5. Fetching: POP3 (1984, 1996)

SMTP brings mail to a server, which keeps a **maildrop** per user; a
computer that is not always on comes and takes it:

```
S: +OK tiny_maild POP3 ready
C: USER bob
S: +OK
C: PASS anything                          in clear: why APOP (1993), then TLS
S: +OK bob's maildrop has 1 message
C: STAT
S: +OK 1 179                              how many, how big
C: RETR 1
S: +OK 179 octets
S: Received: from telnet by tiny with SMTP;
S:   Sat, 26 Sep 2026 00:15:37 +0000
S: From: The President <president@whitehouse.gov>
S: ...
S: ..a line starting with a dot          dot-stuffed on the way out too
S: .
C: DELE 1                                 marked, not deleted yet
S: +OK message 1 deleted
C: QUIT                                   now: the update state
S: +OK bye
```

**Deletions happen at QUIT**, and only then: a connection that drops
in the middle loses nothing, the next session fetches the same mail
again. `Unit_mail_server` checks it by opening a connection by hand,
sending `DELE 1` and closing the socket: the message is still there
(and, as a control, the same session with `QUIT` deletes it -- the
first version of that test sent WebSocket frames the server ignores,
and passed for the wrong reason, until the control caught it).

**Leave mail on server**, Eudora's checkbox, sends no `DELE`, and asks
`UIDL` for each message's unique id, to fetch only the ones it has not
got -- which is how TinyEudora reads Gmail (§10), and what IMAP (§11)
generalised.

## 6. A server, and doing it by hand

`tiny_maild` is `Mail_server`, 230 lines: SMTP in, POP3 out, a
maildrop per user, made by the first mail they receive (as a channel by
its first user in `tiny_ircd`), a `Received:` line added to each
message. Mail for another domain is refused, **550: not a relay**. In
the 1980s every server passed mail on for anyone, trusting the others;
the spam of the 1990s made all these *open relays* close.

It listens twice, over WebSocket (8025, 8110) for TinyEudora in a
browser -- a web page has no plain TCP -- and over plain TCP (2525,
1100) for everything else (`Server.listen ~lines:true`). So:

```
$ dune exec networking/maild/tiny_maild.exe -- spool=/tmp/spool
$ telnet localhost 2525
```

and type §4's session. Then `telnet localhost 1100` and §5's. Any mail
client of the world can talk to it; tried once each: curl's `smtp://`
and `pop3://`, Python's `smtplib` and `poplib`. `spool=dir` keeps each
maildrop as an mbox file (§3), `users=alice:x,bob:y` makes POP3 check
passwords.

## 7. The client: TinyEudora

Eudora (Steve Dorner, University of Illinois, 1988, then Qualcomm's)
was the mail client of the 1990s, and the POP client: mail
*downloaded* to your disk, your mailboxes plain files you own. It makes
the two halves of mail two commands: **Send Queued Messages** is §4,
**Check Mail** is §5.

- **Mailboxes**: In, Out, Trash and yours, each an mbox (§3) in the
  platform's store -- natively files you can open with mutt.
- **The list**: Eudora's columns (status, who, date, size, subject),
  sorted by a click; `•` unread, `R` replied, `F` forwarded, `Q`
  queued, `S` sent -- kept in the Unix mailers' `Status:` and
  `X-Status:` headers, which stay home when a message is sent.
- **Reading**: the text part of the MIME tree (§2), the attachments
  listed, a picture decoded by the repository's own PNG, GIF and JPEG
  readers, `b` for every header and the envelope.
- **Writing**: Reply quotes with `> ` under "At 12:00 PM 9/25/26, Alice
  wrote:" and sets In-Reply-To and References; a message is *queued*,
  not sent; nicknames (`team`) are expanded from vCards, the address
  book TinyPalmPilot also reads.

## 8. Conversations: Zawinski's threading (1997)

Jamie Zawinski wrote it for Netscape Mail 2.0 and published it as an
essay; IMAP took it as `THREAD=REFERENCES`. The links are in the
messages you *have*, and they name messages you may not:

```
<1> the plan                      <1> (never received)
  <2> Re: the plan       --->     +-- <2> Re: the plan
    <3> Re: the plan                +-- <3> Re: the plan
  <4> Re: the plan                +-- <4> Re: the plan
```

So `Mail_thread` makes a **container** per Message-ID, holding its
message or empty; links each message's References in a chain (no link
that would close a loop); takes the containers with no parent as the
roots; prunes the empty ones -- but keeps one at the top that holds
several replies together, a thread whose first message never came;
groups the roots by subject for the mailers that sent no references
("Re: lunch" under "lunch"); and sorts brothers by date. It is generic
over the message, so a news reader can use it as is (§11). In
TinyEudora, `t`.

**Filters** are Eudora 1.4's (1993): a header, a word it contains, a
mailbox -- records tried in order, on the mail as it arrives. The
built-in one takes the tiny-list digest out of In.

## 9. Mail as the network sees it

What TinyEudora sends is exactly what these files say: a queued
message in Out is an mbox entry; `Smtp.envelope` makes it an envelope
and a text; `Smtp.stuff` makes the text lines; the server's
`Smtp.unstuff` and `Mail.parse` read it back and add their
`Received:`; `Pop3.stuff` sends it again, and the other TinyEudora's
`Mbox` writes it in In. Every step is a function a test calls; the
round trip -- alice sends, bob checks -- is `Unit_mail_server`, in one
process.

## 10. Today: TLS, and logging in

A server of today speaks POP3 and SMTP only inside **TLS**: Gmail's
POP3 on 995, its SMTP on 465, encrypted from the first byte. And its
SMTP asks who you are before taking mail for elsewhere -- **AUTH PLAIN**
(RFC 4954, 2007), a user and a password as one base64 string after
`EHLO` (`Smtp.client ~auth`): the patch, twenty-five years late, for
§4's lesson. Base64 is no secret, so only over TLS.

The TLS is ours (`plan_tls.md`, and its tutorial `notes_tls.md`):
`Tls13`, a client machine as pure as
`Pop3`'s -- X25519 for the key exchange, ChaCha20-Poly1305 (or
AES-128-GCM) for the records, the server's certificate chain checked
up to one of the system's roots (ECDSA, RSA), all written from the
RFCs and checked against RFC 8448's handshake byte for byte -- and
`Tls_client` its socket. Our `Pop3` and `Smtp` machines talk through
it, a line each way, as they do to `tiny_maild` in plain text:

```
Pop3's machine --lines--> Tls13 (ours) --TLS--> pop.gmail.com:995
               <-lines--               <-TLS--
```

Before it was written, openssl did the encryption instead, run beside
us with a pipe each way -- as stunnel (1998) gave TLS to the plain-text
programs of the 1990s; `tls=openssl` still takes that way
(`Tls_tunnel`).

`TinyEudora account=gmail user=you@gmail.com` reads your mail, with an
app password (Google's 2-step verification, then App passwords; POP
enabled in Gmail's settings), asked once and never stored, the mail
left on Gmail and only the new ids fetched. Tried with no account:
POP3 answers `[AUTH] Username and password not accepted.`, SMTP `535
5.7.8` -- the whole path working, in Gmail's own words.

## 11. What is left out, and why the world moved on

- **IMAP** (1986, RFC 3501): the mail kept on the server, folders and
  flags there, several computers reading the same mailbox -- what
  replaced POP once people had more than one computer, and then
  **webmail** (Hotmail, 1996; Gmail, 2004), which is TinyChrome's.
- **Relaying between servers**: DNS MX records, a second `tiny_maild`
  for another domain -- the relay is SMTP again, one server the client
  of the other. An exercise.
- **SPF, DKIM, DMARC**; spam filtering -- Paul Graham's "A Plan for
  Spam" (2002), naive Bayes over the words, is a lovely exercise for
  `ai/`'s learning folder.
- **HTML mail** (over `appkits/browser`), PGP and S/MIME, uuencode
  (MIME won).
- **Revocation** (CRLs, OCSP), TLS resumption and 0-RTT, client
  certificates: what our TLS leaves out (`plan_tls.md`).
- **Usenet**: a news reader would reuse `Mail` whole (an article *is*
  an RFC 822 message) and `Mail_thread`.

## References

- RFC 822 (David Crocker, 1982), RFC 5322 (2008): the message.
- RFC 2045-2047 (Ned Freed, Nathaniel Borenstein, Keith Moore, 1996):
  MIME; RFC 2183, Content-Disposition.
- RFC 4155 (2005): the mbox; Jamie Zawinski, "mbox From_ lines".
- RFC 821 (Jonathan Postel, 1982), RFC 5321 (John Klensin, 2008):
  SMTP; RFC 4954 and 4616, AUTH PLAIN.
- RFC 918 (Joyce Reynolds, 1984), RFC 1939 (John Myers, Marshall Rose,
  1996): POP.
- Jamie Zawinski, "message threading" (1997); RFC 5256 (2008).
- The Eudora Users' Manual (Qualcomm, 1990s); the Computer History
  Museum's release of Eudora's source (2018).
