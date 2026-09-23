# Plan: what's left for networking

The teaching plan is done: see
[`done/plan_networking_teaching.md`](done/plan_networking_teaching.md)
-- `networking/`'s protocols (`Wire`, `Sim_net`, `Inputs`,
`Lockstep`, `Rollback`, `Checksum`, `Snapshot`, `Prediction`,
`Interpolation`, `Websocket`, `Url`, `Http`, `Irc`), `networking/unix/`'s
sockets (`Tcp`, `Udp`, `Server`, `Relay`, `Relay_client`,
`Universe_server`, `Http_client`, `Http_request`, `Irc_server`), the
playground's `Multiplayer` (four netcodes, five ways to connect) and
`Universe`, the programs `relay_server` and `tiny_ircd`, the games
TinySpacewar and TinyTronscroll, the examples HttpText and
UniverseBall, the app TinyIRC, and the tutorial
[`notes_networking.md`](../tutorials/notes_networking.md) checked
against the code, its numbers measured in
[`notes_networking_related_work.md`](../related-work/notes_networking_related_work.md)'s
postscript.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test, over
`Sim_net` when it's a protocol.

## 1. Loose ends of what was built

- **Interpolation in `Multiplayer`.** `Interpolation` is written and
  tested, but `netcode=server` shows the others predicted, not drawn
  from the past: the game has to say which parts of its model are
  "the others". A hook, `?interpolate:(me:int -> predicted:'model ->
  older:'model -> newer:'model -> float -> 'model)`, TinySpacewar
  drawing the other ship between two snapshots. Then the difference
  between predicted and interpolated can be felt on the latency keys.
- **`netcode=server` between computers.** It runs only in
  `net=simulate`, the world as Marshal's bytes. A game's codec over
  `Wire` (`?encode`, `?decode` on `Multiplayer.game`), then the host
  as the server and the joiners as clients, over UDP or the relay.
- **The browser, tried.** The web side of `net=relay`, of UniverseBall
  and of TinyIRC compiles but hasn't run here (the machine's Node, 18,
  has no WebSocket): `make serve-build`, a relay or a server, a
  browser tab against a native program, and whatever that finds.
- **Fewer packets.** `Inputs` sends one every frame, 2,400 bytes a
  second per peer where the tutorial's §2 planned 620: a packet every
  third frame, three inputs in each. Measured before and after, on the
  postscript's table.
- **TinyTronscroll's 8 players**, as the original: a `players=` flag
  read by the game's main, more panels in `net=simulate` (a grid past
  three), the relay already seating any number.
- **The global `Random`** in the eight games still using it (Snake,
  TinyTetris, TinyBlockout, TinyWorms, Asteroid, StarCollector3d,
  FloatingCity3d, TinyMinecraft), each moved to `Playground.random`
  when next touched -- the condition for any of them to go online.

## 2. The rest of client-server

- **Lag compensation** (Source, 2001): the server keeps the last
  second of worlds and judges a shot in the world the shooter saw,
  their latency ago. Needs a game with shots judged by the server:
  TinySpacewar's torpedoes.
- **Delta compression** (Quake 3, 1999): each snapshot as the changes
  since one the client acknowledged, a lost one costing only a bigger
  next delta. Measured against the full snapshot.
- **The XPilot arena**: TinyXpilot (`games/flight/`) on a server, more
  players than a keyboard holds, joining and leaving mid-game -- the
  plan's last game.
- **Time dilation**: the server's queue of a player's inputs grows when
  the network hiccups, and stays grown (that player then lags); the
  server, or the client, speeding up a little until it drains
  (Overwatch's, GDC 2017).

## 3. Transports and the Internet

- **Plain TCP for IRC**: `Irc_server` accepting plain TCP beside
  WebSocket (`Server` with a line mode), so that `irssi` or `weechat`
  talk to it, and TinyIRC talking to real networks (Libera.Chat still
  listens on 6667).
- **TLS**, then `https://` without curl and `ircs://`: SHA-256, HMAC,
  HKDF, ChaCha20-Poly1305, X25519 in `crypto/`, the handshake in
  `networking/` -- `plan_dependencies_remaining.md` section 2's option
  2, a plan of its own.
- **Hole punching** (Ford, Srisuresh and Kegel, 2005) through a
  rendezvous server, two homes playing without the relay carrying
  every packet; **WebRTC data channels** for the browser, unreliable
  and unordered like UDP.
- **A DNS query** written by hand (RFC 1035, over a non-blocking UDP
  socket): the one blocking call left in `Http_request`.

## 4. More programs on it

- **The universe's other examples**: a shared whiteboard and a
  turn-based game (the plan's), on `Universe`.
- **More of `apps/internet/`**, as its dune file lists: a web browser
  (WorldWideWeb, 1990; `Http_client` and the typesetting appkit), a
  mail client (Eudora, 1988; SMTP and POP3, two more line protocols),
  a news reader (rn; NNTP).

## Out of scope, still

Matchmaking, accounts, lobbies; anti-cheat beyond a server's checks;
more than a few dozen players.
