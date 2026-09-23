# Plan: multiplayer for the playground, from scratch, for teaching (`networking/`)

Companions, like every other area here (they started inside this
document and were split out of it on 2026-09-20, which is what its
phase 7 anticipated): [`notes_networking.md`](../tutorials/notes_networking.md),
the tutorial -- packets and latency in frames, lockstep, rollback,
client-server, and the determinism checklist all of them stand on --
and
[`notes_networking_related_work.md`](../related-work/notes_networking_related_work.md)
-- Doom, Quake, Age of Empires, GGPO, and the distributed-systems
algorithms the games rediscovered. Its prerequisites are the physics
engine's fixed time step
([`plan_physics_teaching.md`](done/plan_physics_teaching.md)) and the
games' determinism, so it comes after them.

## Context

`graphics/`, `physics/` and `audio/` teach how a computer makes
pictures, motion and sound. The last piece of a game is **other
players**: Spacewar! was a two-player game from 1962, on one computer,
one screen; the plan's goal is the same duel on **two computers**, then
an XPilot-like arena for more -- and, on the way, teaching what makes
networked games hard: the network is slow (a Paris-New York round trip
is at least 60 ms, 4 frames), loses packets, reorders them, and every
player must see the same game anyway.

The good news, and the reason this plan fits this project: **Elm's
architecture is already the right shape for it.** A game's model is a
pure function of its inputs -- the initial model, and each tick's
`computer` (keyboard, mouse, time) folded through `update`. So two
machines running the same `update` on the same inputs compute the same
model, bit for bit: they only need to exchange their *inputs*, a few
bytes per frame, never the game state. That's **lockstep**, how Doom
(1993) and Age of Empires (1997) did multiplayer, and it's almost free
here. And **rollback** netcode (GGPO, the fighting games' standard),
which needs to save old states and replay inputs, is almost free too:
old models are immutable values, kept in a list.

## Prior art in the house: tronscroll (the late 1990s)

The author's own first network game, kept at
`~/Dropbox/role-programmer/project/project-tron/tronscroll-0.1`: *tron
v0.1*, C and svgalib, GPL, a README that opens "excuse me but i am
french so i don t speak english very well" and an address at
ens.insa-rennes.fr. Up to **8 players**, each a pixel drawing a wall
behind it, on a **1600x1200 virtual map seen through a 320x200
scrolling window** -- the scroll is the idea the name is about, and it
is what makes it more than the arcade Tron. Keys on the numeric keypad
(8/5/4/6, `u` to use an item), and six power-ups: freeze the others,
speed, teleport, invulnerability, erase all pixels of your colour, and
"swap to be the tail".

Its netcode is the teaching, and every one of its choices is one this
plan has an opinion about:

- **TCP** (`SOCK_STREAM`), not UDP -- so a lost packet stalls
  everything behind it (§1 of the tutorial);
- **state, not inputs**: the payload is a fixed C struct `t_coord`
  holding *every* player's `x[]`, `y[]` and `option[]`, plus the
  current power-up and its position -- the snapshot approach (§6),
  chosen years before anyone told him there was another one;
- **the struct is written raw** (`write(sd, &coord, sizeof(t_coord))`):
  perfect between two identical PCs, broken by any difference of
  endianness or padding -- which is exactly why `Wire` exists;
- **ports as matchmaking**: a client tries to connect to 2223, then
  2224, then 2225... and the first port that accepts *is* your player
  number (`network.c`, `init_socket_client`). Charming, and impossible
  today through any NAT (§7);
- and the one that matters most, in `motor.c` lines 140-141:
  **`send_coord` then a blocking `recv_coord`, every frame.** A full
  round trip per frame, synchronously. On the school LAN it was
  invisible (a round trip under a millisecond); over the Internet it
  caps the frame rate at 1/RTT -- about **16 frames per second at 60
  ms**. That single line is the reason input delay (§4) and rollback
  (§5) were invented, and the reason this plan's `-simulate` mode puts
  latency on a key: so that the stall can be *seen* rather than
  explained.

So `games/TinyTronscroll.ml` is this plan's milestone (see "Games"),
and it keeps the 1997 behaviour as a switch, beside the two modern
ones.

## Principles (the same as the other plans)

- **Independent of the Playground.** `networking/` knows messages, bytes,
  ticks and peers; `playground/Multiplayer.ml` is the adapter.
- **One idea per module, the simple and the better version side by
  side** (lockstep, then rollback; the same game, switchable),
  explained in `.mli`s with diagrams, worked examples and references;
  `networking/tests/` checks them.
- **A simulated network first.** The teaching tool, the testing tool,
  and the debug keys all in one: an in-process network where two (or
  more) players run in the same program, each with its own model, their
  messages going through a queue with **configurable latency, jitter,
  loss and duplication**, deterministic from a seed. You can play both
  sides, side by side, and *see* what player 2 sees; with `-debug-keys`,
  keys add 100 ms of latency or 10% loss, like the graphics keys switch
  features. And tests run the whole protocol without a socket.
- **Deterministic games.** No wall clock, no unseeded `Random`, only the
  fixed step: a requirement lockstep makes visible (a desync is a bug,
  caught by comparing checksums of the models).
- **Safe defaults.** Real networking listens on 127.0.0.1 unless told
  otherwise, and never trusts what it receives (a malformed packet is
  dropped, not a crash).

## The Playground API, Evan-style

For a beginner, a two-player game on two computers should be a
two-player game on one computer plus a flag. The one new concept is the
**player**: each player's input, like `computer` is yours.

```ocaml
(* every player's input this tick: their keyboard and mouse *)
type player = { id : int; keyboard : keyboard; mouse : mouse }

val multiplayer :
  players:int ->
  (computer -> int -> 'model -> shape list) ->    (* view: for player n *)
  (time -> player list -> 'model -> 'model) ->     (* update: everyone's input *)
  'model -> ('model, msg) app
```

Spacewar!'s update then just gives each ship its player's keys:

```ocaml
let update _time players game =
  let pilot n = (List.nth players n).keyboard in
  { game with ship1 = fly (pilot 0) game.ship1; ship2 = fly (pilot 1) game.ship2 }
  |> torpedoes_and_collisions
```

and it's run with `-local` (both players on one keyboard, as in 1962),
`-host` (wait for the other player), `-join address`, or, for learning,
`-simulate` (both players in one window, side by side, through the
simulated network, with latency and loss keys). The game code doesn't
change between the four.

Open questions: whether `update` gets the `time` (lockstep makes the
tick number the only time there is); how randomness gets a shared seed
(the host picks it, it travels in the first message); what a player's
view shows while the game waits for a late input (lockstep's stall).

### The other API: a universe of worlds (HtDP), `playground/Universe.mli`

Not a new idea for this repository -- **the gap is already written
down in the code**. `playground/Bigbang.mli` brings HtDP's *world
programs* to the playground (a world, `to_draw`, `on_tick`, `on_key`;
its header calls big-bang "Elm's architecture before Elm"), and its
list of what big-bang has and the playground does not ends with:

> 4. universe, several world programs and a server exchanging
>    messages: the playground has no networking yet
>    (plan_networking_teaching.md; a Universe.ml would come with it).

This is that `Universe.ml`, and **its `.mli` should be written the way
`Bigbang.mli` is**: the Racket original beside the OCaml, the book
cited with its edition and its URL, then what the OCaml adds, what it
deliberately drops, and a straight answer to "is it worth having two
ways to do this?" -- that file is the model for this one, down to the
shape of the comment.

A world gains a mailbox; the server is a program of the same shape:

```racket
; Racket (2htdp/universe)
(big-bang 0
  [to-draw render] [on-tick move]
  [register "localhost"]
  [on-receive (lambda (w msg) (make-package w msg))])

(universe '()
  [on-new (lambda (u w)   (make-bundle (cons w u) '() '()))]
  [on-msg (lambda (u w m) (make-bundle u (list (make-mail w m)) '()))])
```

```ocaml
(* OCaml: the same, with tuples where Racket has make-package,
   make-bundle and make-mail -- as Bigbang dropped pinholes and
   "solid"/"outline" strings *)
val big_bang :
  ... ->
  ?register:string ->                                  (* the universe's host *)
  ?on_receive:('world -> msg -> 'world * msg list) ->   (* mail in, mail out *)
  ... -> ('world, _) app

val universe :
  'state ->
  ?on_new:('state -> world_id -> 'state * (world_id * msg) list) ->
  ?on_msg:('state -> world_id -> msg -> 'state * (world_id * msg) list) ->
  ?on_disconnect:('state -> world_id -> 'state * (world_id * msg) list) ->
  unit -> unit
```

**The open question to settle by writing an example with it**: what a
`msg` is. Racket sends S-expressions and checks them at runtime; OCaml
cannot, so either `msg = string` (simplest, and honest for a chat or
a board game), or a `'msg` type parameter with `~encode` and `~decode`
supplied by the program over `Wire` (§2 of the tutorial) -- which is
more OCaml and one more concept for a beginner. Probably: strings
first, `'msg` when a game needs it.

Why it earns its place beside `multiplayer`: the two teach different
things, and HtDP has twenty years of evidence that the gentler one is
how beginners get there. `multiplayer` is *one game, many players,
simulated everywhere* -- lockstep, determinism, rollback, the subject
of this plan. `universe` is *many little programs sending each other
messages*, with no determinism requirement at all: a chat, a shared
whiteboard, a turn-based game, twenty students' rockets in one sky.
It is the honest introduction, and it is where the plan's turn-based
game (Games, below) belongs.

It also costs almost nothing here: the universe server *is* the relay
server this plan already builds for the browser (Target layout), with
handlers instead of a fixed forwarding rule.

### The first API: Elm's `Http.get`, a request as a `Cmd`

*(Added 2026-09-23, after `Url`, `Http` and `Http_client` were
written to replace curl, [`plan_dependencies_remaining.md`](plan_dependencies_remaining.md)
section 2.)*

Before other players, the simplest use of a network: **ask a server
for something, and get the answer later**. Elm's own answer is the one
to copy, since the playground already has its architecture: `update`
returns a `Cmd`, the runtime performs it, and the result comes back as
a message, like a key press. In Elm (the guide's "HTTP" chapter):

```elm
getBook = Http.get { url = "https://elm-lang.org/assets/public-opinion.txt"
                   , expect = Http.expectString GotText }

update msg model = case msg of
  GotText (Ok text) -> (Success text, Cmd.none)
  GotText (Err _)   -> (Failure, Cmd.none)
```

and here, for an `app` (the Elm-architecture level of `Playground.mli`,
whose `init` and `update` already return a `'msg Cmd.t`):

```ocaml
type msg = GotText of (string, Playground.Http.error) result | ...

let init _flags =
  (Loading, Http.get ~url:"http://localhost:8001/public-opinion.txt"
              ~expect:(Http.expect_string (fun r -> GotText r)))
```

What it takes:

- **`Cmd` made real.** `core/Cmd.ml` is a stub today (`None | Msg of
  'msg`): it grows an effect the platform performs -- `Http of
  request * (response -> 'msg)`, and `batch`, Elm's `Cmd.batch` -- a
  value describing the request, never the request done: `update` stays
  pure, and a test can look at the `Cmd` it returned.
- **`Playground.Http`**, a submodule, so that `open Playground` gives
  Elm's spelling `Http.get` (the protocol module `networking/Http` is
  the platforms' business, not the programs'). Its error type is
  Elm's `Http.Error`: `Bad_url`, `Timeout`, `Network_error`,
  `Bad_status of int`, `Bad_body of string`.
- **Natively, without blocking the frame.** `Download` blocks, which a
  picture loaded once can afford and a game can't: 200 ms without a
  frame is a visible freeze. So `networking/unix/` gets the request as
  a *state machine* -- connecting, sending, receiving, done --
  advanced a little each frame over non-blocking sockets, `select`
  (4.2BSD, 1983) with a zero timeout telling which sockets are ready.
  That is the **event loop**, the idea under every server and every
  browser, taught here on one request, and exactly what lockstep's
  UDP (phase 3) needs next: a frame loop that also listens to the
  network. `Http.parse_response` doesn't change -- the bytes arrive in
  pieces, it still parses them once the server has closed.
- **On the web**, the browser does it: `fetch` or `XMLHttpRequest`, as
  `Playground_platform.fetch_web` already does for `Audio.loop_from`'s
  files. Same `Cmd`, two runtimes -- the point of the virtual module.
- **https://** is still refused natively (a `Network_error` saying
  why) until TLS is ours; the browser has it.

The game-level API (`game view update`) gets nothing here, as in Evan's
playground: a beginner's network is `multiplayer`, below, where the
other player's keys arrive like one's own and no request is ever
written. `Cmd` is for the `app` level, where Elm's programmers are.

## The modules, with their references

The ideas themselves, with their diagrams and their arithmetic, are
[`notes_networking.md`](../tutorials/notes_networking.md); this is the
map from module to idea to source. (From memory, to be checked when
each `.mli` is written.)

- **Wire** (§2): fixed sizes, network byte order, variable-length
  integers -- the same seven-bits-per-byte trick as MIDI's delta times
  ([`notes_audio_midi.md`](../tutorials/notes_audio_midi.md) §4); a
  parser that rejects garbage, because it is fed by the Internet. The
  bandwidth arithmetic that shapes the protocol (1,740 bytes/s naive,
  620 with three inputs per packet at 20 Hz) is in the tutorial.
- **Sim_net** (§3): no source to cite, and the most important module
  here -- a seeded, in-process network with latency, jitter, loss,
  duplication and reordering. Gabriel Gambetta's "Fast-Paced
  Multiplayer" demos, with their latency sliders, are the model for
  what it should feel like.
- **Lockstep** (§4): Doom (1993) over IPX; Paul Bettner and Mark
  Terrano, "1500 Archers on a 28.8: Network Programming in Age of
  Empires and Beyond" (GDC 2001), which is also the best account of
  why determinism, not bandwidth, is the hard part.
- **Checksum** (§4): a hash of the serialized model, compared every
  second -- the difference between a bug and a ghost story.
- **Rollback** (§5): GGPO (Tony Cannon, 2006; open sourced 2019); and
  its ancestor, David Jefferson's "Virtual Time" (ACM TOPLAS, 1985),
  where rolling back a speculative simulation was already called Time
  Warp.
- **Snapshot** (§6, later): Quake (1996) and QuakeWorld's client-side
  prediction (Carmack, 1996); Yahn Bernier, "Latency Compensating
  Methods in Client/Server In-game Protocol Design and Optimization"
  (2001) for interpolation, reconciliation and lag compensation; the
  Tribes model (GDC 2000) for what to send when there is not room for
  everything.
- **The transports** (§7): UDP natively; a relay plus WebSockets for
  the browser, with WebRTC data channels as the later, unreliable
  option. ENet and Valve's GameNetworkingSockets are what a real
  project would use instead, and are named in the `.mli` as such.

## Target layout

```
networking/               (networking, private, package elm_playground: pure
                          OCaml, no sockets; already Url and Http, see
                          plan_dependencies_remaining.md)
  Wire                    serialization: bytes, varints, messages; parsing
                          that rejects garbage
  Sim_net                 the simulated network: latency, jitter, loss,
                          duplication, reordering, from a seed
  Lockstep                input exchange, input delay, stalls
  Rollback                prediction, saved models, replay
  Checksum                desync detection
  Snapshot                (later) server snapshots, interpolation,
                          client prediction and reconciliation
networking/tests/         protocols over Sim_net: every peer ends with the
                          same model, whatever the loss and latency
native transport          UDP sockets (Unix), in networking/unix/, beside
                          Tcp
web transport             WebSockets to the relay (later: WebRTC)
networking/relay/         a tiny relay server, native OCaml -- the same
                          program as the universe server, with a fixed
                          forwarding rule instead of handlers
playground/Playground.ml  Playground.Http, Elm's Http.get as a Cmd (the
                          platforms perform it: native_common's loops,
                          the web's fetch)
networking/unix/          (already Tcp, Http_client) and the request as a
                          non-blocking state machine, for the Cmd
playground/Multiplayer.ml the Evan-style API above
playground/Universe.ml    HtDP's universe: a Bigbang world with a
  (and Universe.mli)      mailbox, and the server that carries the mail;
                          its .mli written like Bigbang.mli's, Racket
                          beside OCaml (see above)
```

## Games

- **Spacewar!** (the physics plan's game): the two-player duel, first
  with `-local` (one keyboard), `-simulate`, then on two machines; the
  flagship.
- **Pong**: two players, the simplest possible test of lockstep.
- **TinyTronscroll, the milestone** (`games/TinyTronscroll.ml`): the
  author's own first network game, rebuilt Tiny (see "Prior art in the
  house") -- 8 players on a map much bigger than the screen, a
  scrolling viewport (`Camera2d`), light trails (`gamekits/lightcycles`,
  which `TinyTron.ml` already uses) and the six power-ups.
  What makes it the right milestone rather than one more game: its
  netcode is a **key**, and the three settings are the plan's three
  chapters -- `netcode=1997` (send the whole state, then block for
  the answer, every frame: the original, and unplayable past a LAN),
  `netcode=lockstep` (inputs, with input delay), `netcode=rollback`
  (no delay, and a visible snap when a guess was wrong). Same game,
  same map, three eras, with the latency and loss keys of `-simulate`
  to push each one until it breaks.
- **Later, an XPilot-like arena**: more players, walls, gravity, over a
  server (section 5): XPilot's own architecture (1991, a server and X11
  clients).
- A turn-based game (a board game, or HtDP's own shared-world
  examples) on the **universe** API above, to show that turns need
  none of the rest: messages, and waiting.

## Phasing

0. *(done, 2026-09-23)* **Groundwork**, after the physics plan's fixed step: the games'
   randomness seeded from the model (not `Random.self_init`), a
   checksum of a model (a hash of its serialized form); `-local` mode
   for Spacewar! (two players, one keyboard).
   Done as: the seeded randomness of `plan_playground_other.md`
   section 1 (`random/Lehmer`, `Playground.random` and `pick`, the seed
   in the model), `networking/Checksum` (FNV-1a of the model's
   marshalled bytes), and `Tetris.ml` converted -- its pieces from the
   model's seed, its fall a fixed 1/60 s per Tick instead of the gap
   between two Ticks on the machine's clock -- with
   `tests/games/Unit_determinism.ml`: one seed and the same keys, but
   Ticks from two different clocks, give the same checksum every
   second. `TinySpacewar` needed nothing: already two players on one
   keyboard, and no randomness (its stars are a formula).
0b. *(done, 2026-09-23)* **Http as a `Cmd`** (the section "The first API" above), over
   `Url` and `Http`, already written: `Cmd` with effects and `batch`;
   `Playground.Http` (`get`, `expect_string`, the errors); the native
   request as a state machine over non-blocking sockets and `select`,
   stepped by the frame loops; the web's through `fetch_web`; an
   example after the Elm guide's, reading a text and showing it (a
   golden frame from a file served by the test itself, never the
   Internet). Tests: the state machine fed its bytes one at a time
   gives the same response as `Http.parse_response`; a slow server
   doesn't stop the frames. Independent of phases 0 to 2, and the
   first to do: it reuses what exists, and builds the event loop
   phase 3 needs.
   Done as: `Cmd.Http_get` and `Cmd.Batch` (`core/Cmd.mli`),
   `Playground.Http`, `networking/unix/Http_request` (the state
   machine, beside the blocking `Http_client` it is tested against),
   `native_common/Commands` (the loop's side), the web's
   `fetch_text` (an XMLHttpRequest), and `examples/HttpText.ml`,
   which fetches its own source from `make serve-build`'s server; its
   golden frame is the request refused (port 1), the one answer that
   doesn't depend on the machine.
1. *(done, 2026-09-23)* **Wire and Sim_net**: serialization and its tests (round trips,
   garbage rejected), the simulated network (its statistics tested).
   Done as: `Wire` (u8, u16, MIDI's varint bounded to 4 bytes, zigzag,
   strings; one value one encoding -- a varint's useless leading byte
   refused -- so 10,000 random strings never raise and whatever parses
   re-encodes to the same bytes; the input message's 7 bytes, 700
   bytes a second with the headers), `Sim_net` (packets as bytes
   between numbered peers, latency, jitter, loss and duplication drawn
   from a seed, reordering from the jitter; the laws tested: 10% of
   10,000 lost within 3 sigma, delays between latency and latency +
   jitter with the mean in the middle, no reordering without jitter).
2. *(done, 2026-09-23)* **Lockstep over Sim_net**: `Lockstep`, `Checksum`, input delay; the
   `multiplayer` API; `-simulate` (both players side by side), with
   latency and loss keys. Tests: identical models after 1000 ticks under
   any latency, loss and reordering; an injected nondeterminism caught
   as a desync.
   Done as: `networking/Lockstep` (input delay; each packet carries
   every input the other side hasn't acknowledged, and acks theirs --
   loss never stalls forever; the latest checksum, the first mismatch a
   desync), tested over `Sim_net` (two and three peers, 1,000 ticks
   under latency, jitter, 10% loss and duplication, every tick's model
   the same as the game alone; 30 ms never stalls a delay of 3, 100 ms
   runs at half speed; a disagreement at tick 500 caught at 540), and
   `playground/Multiplayer` (`Multiplayer.game ~players view update`,
   `update computer players model` with a cleaned computer -- no
   keyboard, no mouse, a fixed screen, ticks for time -- and each
   player's `keyboard` and `pressed`; the flag `net=local`, the default,
   or `net=simulate` with `latency=`, `loss=`, `jitter=`, `delay=` and
   the keys [ ] - =). The open questions answered so: update gets no
   wall time (a cleaned computer instead); a stalled peer shows its
   last model; the seed is shared by the flags. TinySpacewar converted
   (its local golden frames unchanged to the pixel), with a golden
   frame of the simulated duel.
3. **Real UDP, native**: `-host`, `-join`; Spacewar! on a LAN.
4. **Rollback**: `Rollback`, switchable with lockstep (a key), to feel
   the difference with 100 ms of simulated latency. Tests: rollback's
   final models equal lockstep's. Then **TinyTronscroll**, the
   milestone: the scrolling map, the 8 players, the power-ups, and the
   `netcode=` key with its three eras (1997, lockstep, rollback).
5. **The web**: the relay server, WebSockets; a browser against a
   native player.
5b. **The universe** (HtDP): `playground/Universe.ml` over the relay --
   `on_receive` and `register` for a `Bigbang` world, `on_new` and
   `on_msg` for the server; a chat, a shared whiteboard and a
   turn-based game as its examples. Independent of the lockstep
   phases, and the gentler door into all of this.
6. *(later)* **Client-server**: `Snapshot`, prediction, reconciliation,
   interpolation; the XPilot-like arena.
7. **Docs**: `notes_networking.md` checked against the code and its
   numbers measured (bandwidth, delay, rollback's replays per frame),
   and the related-work note's postscript filled in. (The split into
   a tutorial and a related-work note this phase anticipated already
   happened, on 2026-09-20, before the code: the author noticed
   networking was the one area without them.)

## Verification

- `make test`: the protocols over the simulated network, deterministic
  (a seed per test), with many seeds; serialization round trips.
- By hand: Spacewar! on two machines of the LAN; latency and loss
  through the keys.
- Numbers, in this document: bytes per second, input delay in frames,
  rollback's replayed ticks per frame.

## Out of scope

- Matchmaking, accounts, lobbies; encryption and authentication (a LAN
  and a local relay only; explained, not built).
- Anti-cheat beyond the server's checks of section 5.
- More than a handful of players.

## Related work

Split out into
[`notes_networking_related_work.md`](../related-work/notes_networking_related_work.md)
(2026-09-20): the games that invented each technique, the transports
and engines in use today, the distributed-systems ancestors (state
machine replication, Jefferson's Time Warp, dead reckoning from
SIMNET and DIS, Croquet's replicated computation), the teaching
lineage, and what Elm and OCaml each bring -- with this library's
ceiling stated there rather than here.
