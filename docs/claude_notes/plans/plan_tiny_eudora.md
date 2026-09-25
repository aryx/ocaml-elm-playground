# Plan: TinyEudora, a mail client, and mail as bytes (`apps/internet/`)

## Context

`apps/internet/` has chat (TinyIRC, over `Irc` and `tiny_ircd`) and
the web (TinyMosaic to TinyChrome, over `libs/web/` and `tiny_httpd`);
its dune file lists what is still missing, and the first item is **a
mail client**. Mail is the oldest of the three (Ray Tomlinson's `@`,
ARPANET, 1971) and the one everybody still uses every day, and its
pieces are some of the best-documented, most readable protocols there
are: every command a line of text, every reply a number and a line,
every message headers and a body -- things a person can type into a
telnet session, and people did.

This plan adds **TinyEudora** -- after Eudora (Steve Dorner,
University of Illinois, 1988, for the Macintosh; Qualcomm's from
1991, free and then shareware, the mail client of the 1990s): a
window of mailboxes, a list of messages with Eudora's columns, a
message, a composition window, **Check Mail** and **Send Queued
Messages**. And, under it, what it needs in `libs/networking/`, each
module a protocol or a format, bytes in and bytes out; and a server
small enough to read, `tiny_maild`, as `tiny_ircd` is for TinyIRC.

**Why Eudora and not a modern client.** Eudora is the POP client:
mail *downloaded* to your disk and deleted from the server, read
offline, your mailboxes plain files you own -- the model before IMAP
and webmail kept everything on the server. It makes the two halves of
mail visible as two separate commands: *sending* is SMTP, to a server
that relays; *receiving* is POP3, from your mailbox on a server. The
separation is the lesson (why can you send mail as anybody? because
SMTP never asked), and a later client would hide it.

**The tiny rule.** Not the whole of Internet mail: a core small enough
to read in an afternoon. What is left out is listed, each item an
exercise or a "never", so the line is drawn on purpose.

## The parts, and what each keeps

| Part | What it is | Kept |
|---|---|---|
| the message | headers and a body (RFC 822, 1982; RFC 5322) | the headers, folded lines, addresses, dates |
| MIME | several parts, attachments, other charsets (1992) | multipart, base64, quoted-printable, encoded-word subjects |
| the mailbox | messages in one file | mbox, the `From ` line and its escaping |
| sending | SMTP (RFC 821, 1982; RFC 5321) | the dialogue, the envelope, the dot |
| receiving | POP3 (RFC 1939, 1996; POP, RFC 918, 1984) | USER, PASS, STAT, LIST, RETR, DELE, QUIT |
| the server | receives by SMTP, serves by POP3 | local delivery, users by name |
| the client | TinyEudora | mailboxes, list, reading, composing, the queue, replies, nicknames |
| threads | a conversation from its headers | Jamie Zawinski's algorithm (1997) |

### The message: RFC 822

A message is **headers** (`Name: value` lines, a line starting with
space or tab continuing the one before -- *folding*), an empty line,
and a **body**. The worked example of the `.mli`, checked by the
tests:

```
From: Alice <alice@tiny>
To: bob@tiny
Subject: lunch
Date: Thu, 25 Sep 2026 12:00:00 +0200
Message-ID: <1@tiny>

Noon at the usual place?
```

Kept: the header fields as a list in order (a header can appear
twice: `Received:`), looked up without regard to case; unfolding;
addresses in their two forms (`alice@tiny`, `Alice <alice@tiny>`),
lists of them separated by commas; the date read and written with
`core/time`'s `Civil` and `Clock` (the zone's offset is in the date,
which `Clock` already models); `Message-ID`, `In-Reply-To` and
`References`, for replies and threads.

### MIME

MIME (Nathaniel Borenstein and Ned Freed, RFC 1341, 1992; RFCs
2045-2049) put everything that is not a short English letter into
mail *without changing mail*: a body stays 7-bit lines, and what is
not is encoded. Kept:

- `Content-Type` with its parameters (`multipart/mixed;
  boundary="xyz"`, `text/plain; charset=utf-8`);
- **multipart**: the body split at `--boundary` lines, each part a
  message of its own (headers, blank line, body), recursively;
- `Content-Transfer-Encoding`: **base64** (core's `Base64`, already
  shared by the textures and the WebSocket handshake) and
  **quoted-printable** (`=E9` for é, `=` at a line's end to join it to
  the next) -- one written as a worked example in the `.mli`;
- **encoded words** in headers (RFC 2047: `=?utf-8?Q?caf=C3=A9?=`),
  since a subject with an accent is the first thing a French author
  types;
- `Content-Disposition: attachment; filename="..."`.

### The mailbox: mbox

A mailbox is one file, messages one after another, each starting with
a line `From alice@tiny Thu Sep 25 12:00:00 2026` (the envelope's
sender, not the `From:` header -- the space, not the colon, is the
difference). A body line that starts with `From ` would start a new
message, so it is written `>From ` (the "From munging" everyone has
seen in a quoted email). Eudora's own mailboxes were this format, one
file per mailbox (In, Out, Trash, and the user's), plus a table of
contents file for speed -- which we do not need (an exercise).

### SMTP

The dialogue, a line each way, every reply a three-digit number (the
first digit is all a client needs: 2 fine, 3 go on, 4 try later, 5
never):

```
S: 220 tiny ESMTP tiny_maild
C: HELO eudora
S: 250 tiny
C: MAIL FROM:<alice@tiny>
S: 250 OK
C: RCPT TO:<bob@tiny>
S: 250 OK
C: DATA
S: 354 End data with <CR><LF>.<CR><LF>
C: From: Alice <alice@tiny>
C: ...
C: .
S: 250 OK: queued as 7
C: QUIT
S: 221 Bye
```

The lesson in it: the **envelope** (`MAIL FROM`, `RCPT TO`) is not the
message's headers (`From:`, `To:`); `Bcc:` works *because* of that
(the recipient is in the envelope, not in the headers), and so does
forged mail (nobody checks that `From:` matches anything -- SPF, DKIM
and DMARC, thirty years later, are the patch; out of scope, and the
tutorial says why they had to exist). And the **dot**: a line of a
single `.` ends the message, so a line of the message starting with a
dot is sent with a second one (*dot stuffing*), the same trick as
mbox's `>From`.

Written as `Irc` is: pure, a module of parsed commands and replies,
and a client **state machine** stepped by the lines that arrive
(greeting, HELO, MAIL, each RCPT, DATA, the body, QUIT), giving the
lines to send -- no socket in it.

### POP3

Receiving, as simple: `USER bob`, `PASS secret` (in clear: the
tutorial's paragraph on why APOP, 1993, and then TLS came), `STAT`
(how many, how big), `LIST`, `RETR 1` (the message, dot-terminated as
in SMTP), `DELE 1`, `QUIT` (only now are the deletions done: the
*update state*, so a dropped connection loses nothing). Replies are
`+OK` and `-ERR`. Eudora's "Leave mail on server" is a checkbox that
does not send `DELE` (and `UIDL`, to know which ones it already has:
kept, it is two lines).

### The server: `tiny_maild`

`Mail_server` (in `networking/unix/`, beside `Irc_server`) and its
program `networking/maild/tiny_maild.exe`: SMTP on one port, POP3 on
another, users created by the first mail they receive (as a channel
is by its first user in `tiny_ircd`), each a mailbox in memory -- an
mbox file per user with a flag `spool=dir` (through `Cap.fs`), so the
mail survives the server. Local delivery only: mail to `bob@tiny` goes
to bob; mail to anywhere else is refused with `550` -- the server is
not a relay, and the tutorial says what an *open relay* was, and why
the 1990s' spam made them all close.

Passwords: any, accepted, by default (a teaching server on 127.0.0.1);
a flag `users=alice:x,bob:y` to make `-ERR` reachable.

Over **WebSocket first**, a line a frame (`Server.mli`), as
`tiny_ircd` is, so that a TinyEudora in a browser can connect -- a web
page has no plain TCP. Then over **plain TCP** (phase 6), because the
telnet session is the lesson: `telnet localhost 2525` and typing the
dialogue above by hand, and because then a real client (Thunderbird,
`mutt`, `swaks`) can talk to our server and TinyEudora to a real one
on a local network.

### The client: TinyEudora

The screen, after Eudora 1.x on a Macintosh (black and white, a window
per thing, drawn as the other Tiny apps draw, with the gui toolkit and
`File_menu`):

```
 File Edit Mailbox Message Transfer Special Window
+----------------------------------------------------------+
| In                                                   [x] |
|  * | Alice            | 12:00 9/25 | 2 | lunch           |
|    | Bob              | 11:02 9/25 | 4 | Re: the plan    |
|  R | Carol            | 17:40 9/24 | 9 | photos      [A] |
+----------------------------------------------------------+
| From: Alice <alice@tiny>            (the message, below, |
| Subject: lunch                       or in its own       |
|                                      window, as Eudora)  |
| Noon at the usual place?                                 |
+----------------------------------------------------------+
```

- **Mailboxes**: In, Out, Trash, and the user's own (Mailbox > New),
  each an mbox; a message moved by Transfer (Eudora's menu name);
  deleting moves to Trash, emptying Trash forgets.
- **The list**: Eudora's columns -- the status (• unread, R replied,
  F forwarded, S sent, Q queued), who, the date, the size in K, the
  subject, [A] for an attachment; sorted by a click on a column.
- **Reading**: the headers that matter (Eudora's "Blah Blah Blah"
  button showed them all: kept, the key "b"), the text part of a
  multipart, attachments listed and saved with
  `Playground_platform.export`; an attached picture (PNG, GIF, JPEG --
  the repository's own decoders) shown under the text.
- **Composing**: To, From, Subject, Cc, Bcc, Attachments (a file
  opened with the File menu's open); **Reply** quotes the message with
  `> ` and sets `In-Reply-To` and `References`; **Forward**; the
  signature; and **Queue** -- a message is not sent when written but
  put in Out, queued, and **Send Queued Messages** sends them all.
  That was Eudora on a dial-up modem: write offline, connect once.
- **Check Mail** (⌘M): POP3, the new messages appended to In.
- **Nicknames** (Eudora's address book): a nickname for an address or
  a list of them, typed in To:, expanded when sent -- stored as vCards
  through `appkits/pim`'s `Vcard`, the address book TinyPalmPilot
  already reads and writes.
- **Filters** (Eudora 1.4, 1993 -- the version to check): a header, a word, a mailbox to move
  to; run on the mail as it arrives. A short list, each a record, no
  language.
- **Threads** (optional view, key "t"): the list indented by
  conversation, by Jamie Zawinski's algorithm ("message threading",
  1997, written for Netscape Mail): by `References`, then
  `In-Reply-To`, then, for the mail with neither, by subject less its
  `Re:`s -- with the containers for the messages you never received.
  The algorithm fits in a hundred lines and its essay is a model of
  explaining one; it is `plan_gui_remaining.md`'s mail item too.

**With no server**: TinyEudora opens with a built-in mailbox,
`Our_mail` (a library of messages as files embedded by dune, as
`internet_site` is the browsers'), so it shows something with no
network and the golden frames are deterministic. The messages are
written for what they show: a plain one, a reply and its thread of
five, a multipart with a PNG attached, a quoted-printable one with
accents and an encoded-word subject, a digest, a message with a
`>From` line, a forged one (its `From:` is not its envelope's
sender, and the `Received:` lines give it away). Flags: `server=`
(host:port, `localhost` by default), `user=`, `mailbox=` (a file,
natively).

## Where it goes

```
libs/networking/
  mail/                 NEW, library networking_mail (pure, unwrapped)
    Mail                a message: headers, folding, addresses, dates
                        (RFC 822/5322), read and written
    Mime                content types, multipart, base64 and
                        quoted-printable, encoded words (RFC 2045-2047)
    Mbox                a mailbox file, read and written, >From
    Mail_thread         Zawinski's threading
  protocols/            (exists: Http, Irc, Websocket, ...)
    Smtp                NEW: commands, replies, the client's machine
    Pop3                NEW: commands, replies, the client's machine
  unix/                 (exists)
    Mail_server         NEW: SMTP and POP3 served, the spools
  maild/                NEW: tiny_maild.exe, as ircd/
  tests/                Unit_mail, Unit_mime, Unit_smtp, Unit_pop3,
                        Unit_mail_server (two clients, one server, in
                        one process)
apps/internet/
  TinyEudora.ml         the client
  Our_mail.ml(i), mail/ the built-in mailbox, embedded
  software/, web/       its golden frames, its page
```

`mail/` is a folder of its own rather than more of `protocols/`
because the formats are not protocols -- a mailbox file is read with
no network at all, and `appkits/pim`'s `Ics` and `Vcard` are the same
family (a MIME-like text format, lines and folding: RFC 5545 took
RFC 822's folding). Whether `Mail` and `Vcard` should share their
line-folding code is a question for the first phase (`Ics.mli` already
has "the layers, shared with Vcard"). Nothing is moved; everything is
new.

## Phases

1. **The formats**: `Mail`, `Mime`, `Mbox`, each `.mli` with its worked
   example checked by a test; the built-in mailbox's messages written
   and read back byte for byte.
2. **TinyEudora offline**: the mailboxes, the list, reading, the
   headers key, attachments shown and saved; with `Our_mail` only.
   Golden frames: `TinyEudora.png` (the list and a message),
   `TinyEudora_attachment.png`. Its catalogue row and web page (the
   catalogue test wants both from the first commit).
3. **Composing**: the composition window, Reply's quoting and headers,
   Forward, the queue in Out, nicknames over `Vcard`, the mailboxes
   saved (natively as files, in a browser through the platform's
   `store`, as TinyPalmPilot's). `TinyEudora_compose.png`.
4. **The protocols**: `Smtp` and `Pop3`, pure, each with the RFCs'
   own example sessions as tests (RFC 5321's appendix D, RFC 1939's
   section 10), and the dot and the envelope each as a test.
5. **The server, and the network**: `Mail_server` and `tiny_maild`
   over WebSocket; Send Queued Messages and Check Mail in TinyEudora,
   over `Transport`-like connections (natively the platform's, in a
   browser its WebSocket); `Unit_mail_server`: alice sends, bob checks,
   in one process. Two TinyEudoras exchanging mail, by hand, once.
6. **Plain TCP**: `tiny_maild` on 2525 and 1100 too (the privileged 25
   and 110 are for root), the telnet session in the tutorial,
   Thunderbird pointed at it once, by hand, and noted.
7. **Filters and threads**: filters on arriving mail; `Mail_thread`
   and the threaded view. `TinyEudora_threads.png`.
8. **Docs**: a tutorial, `notes_mail.md`: mail as three formats and two
   protocols, the envelope, the dot, why POP deletes, why SMTP trusts,
   the timeline (1971 Tomlinson, 1982 RFC 822 and SMTP, 1984 POP, 1988
   Eudora, 1992 MIME, 1996 POP3 as we know it, 1997 threading, 2000s
   spam and its patches, webmail); `networking/README.md`'s table;
   `CATALOG.md`'s row settled.

Sizes, to hold ourselves to: the formats ~500 lines, the two protocols
~250, the server ~250, TinyEudora ~900. TinyIRC is 238 and its
protocol and server 218, for scale.

## Status

**Phases 1 and 2 done** (2026-09-26): `libs/networking/mail/`
(`networking_mail`: `Mail`, `Mime`, `Mbox`, 480 lines, with their
`.mli`s' worked examples in `networking/tests/`'s `Unit_mail` and
`Unit_mime`), TinyEudora offline over the built-in mailboxes
(`Our_mail`, `apps/internet/mail/*.mbox`, checked byte for byte by
`apps/internet/tests/`), 520 lines: In, Out, Trash and Projects, the
list and its columns sorted by a click, reading, b for every header
and the envelope, Delete, Transfer, Empty Trash, attachments shown
and saved; golden frames `TinyEudora.png`, `TinyEudora_attachment.png`
(flag `message=8`). Found on the way: 25 Sep 2026 is a Friday, not
the Thursday this plan's examples said -- the `.mli`s say Fri.
Decided in phase 1: `Mail`'s folding is not shared with `Vcard`/`Ics`
(RFC 822 keeps the space after the line break, iCalendar removes it,
Mail.mli says so); `mail/` as the folder's name; the client's parts
stay in TinyEudora until a second user comes (no appkit yet); the
messages are written for the lessons, none historical (the author may
still add Thuerk's).

**Phase 3 done** (2026-09-26): the lower window a pane -- reading,
composing (the gui toolkit's fields and text area, in a black and
white theme), the store's documents to attach (a list of their names:
Eudora's File menu opened mailboxes, so no File_menu), a new
mailbox's name, the nicknames. New Message, Reply (quoted under "At
..., Alice wrote:", In-Reply-To and References, the original marked
R once queued), Forward (its attachments kept, marked F); Queue: the
message built by `Mime`'s new writers (`text_part`, quoted-printable
when not ASCII; `attachment`, base64 in lines of 76; `multipart`),
the subject as an encoded word, nicknames expanded, the signature
added, Bcc: kept until sent, into Out marked Q. Nicknames as vCards
(`Our_mail.nicknames`, `mail/nicknames.vcf`: alice, carol, dave, and
team, a card with three addresses), Special > Make Nickname. The
mailboxes kept in the store as mbox files, `eudora-<name>.mbox`, the
nicknames as `eudora-nicknames.vcf`, the built-in ones the defaults;
flags `compose=new|reply|forward`, `mailbox=`, `user=`. Checked by
script once each: a reply queued and read back from the store, team
expanded, an attachment picked and queued. Golden frame
`TinyEudora_compose.png`. TinyEudora is 870 lines, `Mime` 272.

**Phase 4 done** (2026-09-26): `Smtp` and `Pop3` in `protocols/`
(which now uses `networking_mail`), pure, their clients state machines
stepped by the lines that arrive (`step`, `finished`), 245 lines.
`Smtp`: replies of one line or several, commands parsed both ways, the
envelope made from a message (To:, Cc: and Bcc:'s addresses; the text
less its Bcc:), the dot stuffed and unstuffed; several messages in one
connection, each sent (to how many, the refusals kept) or refused
(RSET), HELO when EHLO is refused. `Pop3`: USER, PASS, STAT, LIST or,
leaving the mail on the server, UIDL and only the ids not known, RETR,
DELE, QUIT. `Unit_smtp` replays RFC 5321's D.1 and `Unit_pop3` RFC
1939's section 10 (USER and PASS for APOP, whose MD5 we lack). Found
on the way: D.1's "...etc. etc. etc." is sent "....etc. etc. etc." --
the RFC's line is a placeholder, and a real one starting with a dot
gets a second. `Mail.remove` added.

**Phase 5 done** (2026-09-26): `Mail_server` (`networking/unix/`, 230
lines) and `networking/maild/tiny_maild.exe`: SMTP on 8025 and POP3 on
8110 over WebSocket, a maildrop per user (made by its first mail), a
Received: line added, mail for another domain refused 550 (not a
relay), POP3's deletions at QUIT only (by identity: mail arriving
during a session is kept), UIDL a digest of the message, any password
unless `users=alice:x,bob:y`; `spool=dir`, each maildrop an mbox file
written by tiny_maild as it changes (the server keeps nothing on disk:
a `changed` callback). `Unit_mail_server`, in one process: alice sends
and bob checks, twice; not a relay; a connection dropped after DELE
deletes nothing (a WebSocket made by hand, closed -- with a control,
the same session with QUIT deleting, which caught the test's first
version sending text frames the server ignores); a wrong password.
TinyEudora: a File menu, Check Mail (the password asked once, drawn
as stars, never stored, forgotten when refused) and Send Queued
Messages (Status:/X-Status: kept home, Q becoming S), each an errand
over `Transport`, the machine fed each frame, given up after 10 s of
silence; flags `server=`, `smtp=`, `pop=`. Done by hand once, natively
(tiny_maild with a spool, on other ports): bob's reply sent, alice's
TinyEudora "you have new mail", her spool emptied. Not yet tried in a
browser. Next: 5b (Gmail), then 6 (plain TCP).

**Phase 5b done** (2026-09-26), not over curl after all: the OCaml
binding of libcurl has its connect-only mode but not
`curl_easy_send`/`recv`, so curl could only have spoken POP3 itself.
Instead a TLS *tunnel*, `Tls_tunnel` (`networking/unix/`): openssl's
`s_client -quiet -verify_return_error` run beside us, a pipe each way,
as stunnel gave TLS to the plain-text programs of the 1990s -- so our
own `Pop3` and `Smtp` machines talk to Gmail. `Transport.tunnel`, a
hook of its own taking `Cap.exec` (it runs a program), installed by
the two native 2D platforms (the 3D ones have no mail program; a
browser answers an Error). `Smtp.client ?auth` logs in, AUTH PLAIN
(RFC 4954, 4616's example in the tests); `Pop3.client ?limit` fetches
only the newest. TinyEudora `account=gmail user=you@gmail.com`:
POP3 to pop.gmail.com:995 and SMTP to smtp.gmail.com:465 through the
tunnel, the login the whole address, an app password asked once
(before sending too) and never stored, mail left on Gmail and only the
ids not yet fetched asked (kept in the store, `eudora-gmail-uids`),
`limit=` (20); the account's own files, `eudora-gmail-*.mbox`, no
built-in message in them; the silence timeout now counted from the
last line heard (30 s); the status line moved under the windows,
where a server's long answer fits. `Unit_tls_tunnel`: a local
`openssl s_server` with a self-signed certificate is refused (checked
by hand to fail on the verification itself). Tried against Gmail with
no account, `nobody@example.invalid`: POP3 answers "[AUTH] Username
and password not accepted.", SMTP "535 5.7.8 Username and Password not
accepted", the message staying queued -- the whole path, TLS
included, working; reading a real mailbox is for the author, with
their app password.

**Phase 5b, added (2026-09-26): your own mailbox, Gmail.** The
author would like to read their own mail. Gmail speaks POP3 and SMTP
only over TLS (`pop.gmail.com:995`, `smtp.gmail.com:465`) and takes
an *app password* (2-step verification on, POP enabled in Gmail's
settings), not the account's password; OAuth2 otherwise. Until TLS is
ours, natively only, through curl, as `https://` is
(`native_common/Commands`): `pop3s://` (LIST, then each message by
number: our `Mime` and `Mbox` read what it gives) and `smtps://` (the
envelope from `Smtp.envelope`, the text uploaded). curl then speaks
the protocols, not our machines, which keep teaching against
`tiny_maild`; once TLS is ours they can talk to Gmail themselves. The
app password in a file of the store (`eudora-password`, through the
capability), never a flag; your mail never in a test. In a browser:
not possible (no TCP, no TLS sockets; Gmail's HTTP API with OAuth is
another project).

Decisions taken before starting:

- **the author picked it** (2026-09-25), from the list of what is
  missing, over the JRPGs the plans rank low on purpose;
- Eudora 1.x, the Macintosh's, black and white: POP, not IMAP; a
  window of lists, not a web page;
- the server in the house, as for IRC and HTTP: nothing the tests or
  the golden frames need leaves the machine;
- a built-in mailbox, as the browsers' built-in site.

Open, for the author:

- the **name** of the formats' folder (`mail/`) and whether the
  client's parts become an appkit now (`appkits/mail`) or only when a
  second user comes (TinyOutlook in `apps/office/`, beside the
  calendar, as `plan_gui_remaining.md` imagines -- the rule so far is
  "the moment a second app needs it");
- whether `Our_mail`'s messages include historical ones where their
  text is published (Gary Thuerk's DEC announcement of 1978, the first
  spam, is the obvious one), or only messages written for the lessons.

## Verification

- `make test`: every `.mli`'s worked example (a message, a multipart,
  quoted-printable, an encoded word, an mbox with `>From`, an SMTP
  session, a POP3 session); read then written gives the same bytes;
  the server test (send, check, delete, the update state honoured on a
  dropped connection); threading on Zawinski's own examples, and on
  a missing parent.
- Golden frames: TinyEudora and its `_attachment`, `_compose` and
  `_threads` scenes, from the built-in mailbox; the catalogue test.
- By hand, once each, noted in the tutorial: two TinyEudoras through
  `tiny_maild`, native and in a browser; the telnet session; a real
  client against `tiny_maild`.

## Out of scope

- IMAP (RFC 3501): mail kept on the server, folders there, flags there
  -- an exercise, and the tutorial's paragraph on why the world moved
  to it (and then to webmail, which is TinyChrome's).
- TLS, STARTTLS, SMTP AUTH: until TLS is ours
  (`plan_teaching_other.md`); a local server does not need them, and
  the tutorial says where they go.
- DNS MX records and relaying between servers (a second `tiny_maild`
  for another domain is an exercise: the relay is SMTP again, one
  server a client of the other).
- SPF, DKIM, DMARC; spam filtering (Paul Graham's "A Plan for Spam",
  2002, naive Bayes over words, is a lovely exercise for `ai/`'s
  learning folder).
- HTML mail (an exercise over `appkits/browser`'s `Browser_page`, which
  already lays out a page in a rectangle), PGP, S/MIME, uuencode (MIME
  won), the table-of-contents files, Eudora's later Paige-based styled
  text.
- Usenet: the news reader (rn, NNTP), `apps/internet/dune`'s other
  missing item, is a plan of its own -- though it would reuse `Mail`
  whole (an article *is* an RFC 822 message) and `Mail_thread`, which
  is a reason to do it next.
