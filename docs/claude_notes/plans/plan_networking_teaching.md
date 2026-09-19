# Plan: multiplayer for the playground, from scratch, for teaching (`network/`)

One document for networking, unlike graphics, physics and audio (a
plan, a tutorial and a related-work survey each): the plan first, then
the ideas it relies on (sections "The ideas"), then where they come
from ("Related work"). Its prerequisites are the physics engine's
fixed time step ([`plan_physics_teaching.md`](plan_physics_teaching.md))
and the games' determinism, so it comes after them.

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

## Principles (the same as the other plans)

- **Independent of the Playground.** `network/` knows messages, bytes,
  ticks and peers; `playground/Multiplayer.ml` is the adapter.
- **One idea per module, the simple and the better version side by
  side** (lockstep, then rollback; the same game, switchable),
  explained in `.mli`s with diagrams, worked examples and references;
  `network/tests/` checks them.
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

## The ideas

What the plan builds on, in the order the phases need them (the
tutorial part; each will be expanded in the modules' `.mli`s).

### 1. Packets, latency, loss

The Internet moves **packets**, a few hundred bytes each, with no
promise: one can arrive late, twice, out of order, or never. Two
protocols on top: **TCP** makes it a reliable, ordered stream
(resending what's lost) -- but a lost packet then blocks everything
behind it until it's resent (**head-of-line blocking**), a stall of a
round trip or more; **UDP** delivers packets as they come, or not at
all. Games use UDP, and handle loss themselves, because an old input or
position is worthless once a newer one exists.

**Latency** has a floor, the speed of light in fiber, about 200,000
km/s: **5 ms per 1000 km**, so Paris to New York (5,840 km) is 29 ms
one way, 60 ms for a round trip, before any router adds its own. At 60
frames per second a frame is 16.7 ms: a round trip across the Atlantic
is **4 frames**. **Jitter** is latency varying from packet to packet.
Nothing here can make them zero; the techniques below only hide them.

### 2. Encoding messages

A message is bytes. A player's input fits in one: a bit per arrow key
and button. **Serialization** writes values to bytes and reads them
back (`Wire`): fixed sizes, a byte order (network order is
big-endian), and for sizes that vary, **variable-length integers** --
the same 7-bits-per-byte trick as MIDI's delta times
([`notes_audio_midi.md`](notes_audio_midi.md) §4). Every message
starts with its type and the tick it's about; anything that doesn't
parse is dropped.

Bandwidth: with lockstep, each peer sends 60 inputs a second, 1 byte
each, but every UDP packet costs 28 bytes of headers (IP and UDP):
(1 + 28) x 60 = **1,740 bytes per second**, 94% headers. Sending the
last 3 inputs in each packet at 20 packets per second -- also a cheap
defense against loss, each input sent three times -- is (3 + 28) x 20
= **620 bytes per second**: a 1990s modem could do it.

### 3. Lockstep: exchange inputs, simulate everywhere

```
   tick:    1     2     3     4
   peer A:  a1    a2    a3    a4  ---.
                                      >  each peer applies tick n only
   peer B:  b1    b2    b3    b4  ---'   when it has a_n and b_n: both
                                          compute update (a_n, b_n)
```

Every peer runs the whole game; each tick, it sends its input and
waits for everyone else's for that tick, then steps. Nothing but inputs
travels, so it scales to any amount of state (Age of Empires' "1500
archers on a 28.8 modem"). The costs:

- **Input delay**: to not stall every frame, an input is scheduled a
  few ticks ahead: pressed at tick n, applied at tick n + 3, time for
  it to arrive (50 ms one way is 3 frames). The game feels 3 frames
  late, for everyone.
- **The slowest peer sets the pace**: a late packet stalls everyone.
- **Determinism, exactly**: the same inputs must give the same model on
  every machine, bit for bit. Same binary, same OCaml, the same float
  operations in the same order: fine (the golden tests already rely on
  it); a different compiler or CPU could differ in the last bit of a
  float, and the games drift apart forever: a **desync**. So each peer
  sends a **checksum** of its model every second, and a mismatch stops
  the game with a message instead of letting it diverge silently.

### 4. Rollback: predict, then correct

Lockstep's input delay is what fighting games can't accept. **Rollback**
(GGPO, Tony Cannon, 2006) doesn't wait: each peer applies its own input
at once, and *predicts* the others' (the simplest prediction: the same
as their last known input -- usually right, keys stay pressed). When
the real input arrives and differs, it **rolls back**: restores the
model of that tick, and replays the ticks since with the real inputs.

```
   tick:        10   11   12   13        B's input for tick 11 arrives
   A computes:  m10  m11' m12' m13'      at tick 13, and differs from
                      |                  the guess: go back to m10, redo
   A redoes:          m11  m12  m13      11, 12, 13 with it, in one frame
```

The cost is CPU: replaying up to a round trip of ticks in a single
frame (4 ticks at 60 ms), and saving every tick's state -- which, with
Elm's immutable models, is keeping the last few models in a list: no
copying, no "save state" code. The visible cost: a remote ship jumps a
little when a guess was wrong.

### 5. Client and server: for more players, and for the web

Past two to four players, and on the web, the usual shape is a
**server** that owns the game: clients send inputs, the server
simulates and sends back **snapshots** of the state (Quake, 1996).
Each client then **predicts** its own ship locally, so its controls
feel instant (QuakeWorld, John Carmack, 1996), corrects when the
server's snapshot disagrees (**reconciliation**), and shows the other
players slightly in the past, **interpolated** between two snapshots,
so they move smoothly despite jitter. The server can also rewind time
to check what a shooter saw (**lag compensation**). More bandwidth
(states, not inputs; **delta compression**: only what changed) but no
desync possible, and the server can refuse impossible moves (cheating:
lockstep gives every peer the whole state, so a hacked client can see
everything).

### 6. Connecting: addresses, NAT, the browser

Two computers at home usually can't reach each other directly: their
routers' **NAT** shares one public address and drops unknown incoming
packets. Fixes: a LAN (same network), a server both connect to (a
relay), or **hole punching** (both send first, through a server that
told them each other's address). In a browser, there are no UDP
sockets at all: **WebSockets** (TCP, to a server) or **WebRTC data
channels** (which can be unreliable and unordered, like UDP, and do
the hole punching). So the web backend needs a small server: a relay,
native OCaml, part of this plan.

## Target layout

```
network/                  (network, private, package elm_playground: pure
                          OCaml, no sockets)
  Wire                    serialization: bytes, varints, messages; parsing
                          that rejects garbage
  Sim_net                 the simulated network: latency, jitter, loss,
                          duplication, reordering, from a seed
  Lockstep                input exchange, input delay, stalls
  Rollback                prediction, saved models, replay
  Checksum                desync detection
  Snapshot                (later) server snapshots, interpolation,
                          client prediction and reconciliation
network/tests/            protocols over Sim_net: every peer ends with the
                          same model, whatever the loss and latency
native transport          UDP sockets (Unix), in native_common
web transport             WebSockets to the relay (later: WebRTC)
network/relay/            a tiny relay server, native OCaml
playground/Multiplayer.ml the Evan-style API above
```

## Games

- **Spacewar!** (the physics plan's game): the two-player duel, first
  with `-local` (one keyboard), `-simulate`, then on two machines; the
  flagship.
- **Pong**: two players, the simplest possible test of lockstep.
- **Later, an XPilot-like arena**: more players, walls, gravity, over a
  server (section 5): XPilot's own architecture (1991, a server and X11
  clients).
- Maybe a turn-based game (e.g. a board game) to show that turns need
  none of the above: messages, and waiting.

## Phasing

0. **Groundwork**, after the physics plan's fixed step: the games'
   randomness seeded from the model (not `Random.self_init`), a
   checksum of a model (a hash of its serialized form); `-local` mode
   for Spacewar! (two players, one keyboard).
1. **Wire and Sim_net**: serialization and its tests (round trips,
   garbage rejected), the simulated network (its statistics tested).
2. **Lockstep over Sim_net**: `Lockstep`, `Checksum`, input delay; the
   `multiplayer` API; `-simulate` (both players side by side), with
   latency and loss keys. Tests: identical models after 1000 ticks under
   any latency, loss and reordering; an injected nondeterminism caught
   as a desync.
3. **Real UDP, native**: `-host`, `-join`; Spacewar! on a LAN.
4. **Rollback**: `Rollback`, switchable with lockstep (a key), to feel
   the difference with 100 ms of simulated latency. Tests: rollback's
   final models equal lockstep's.
5. **The web**: the relay server, WebSockets; a browser against a
   native player.
6. *(later)* **Client-server**: `Snapshot`, prediction, reconciliation,
   interpolation; the XPilot-like arena.
7. **Docs**: this plan's "ideas" checked against the code, the numbers
   measured (bandwidth, delay, rollback's replays per frame); maybe
   split into a tutorial of its own then.

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

(From memory, to be checked before relying on it for teaching.)

**Games that invented the techniques.** *Doom* (id Software, 1993):
four players in peer-to-peer lockstep over IPX on a LAN, sending
inputs; everyone stalls with the slowest. *Quake* (1996): client-server
over UDP, the server authoritative; then *QuakeWorld* (John Carmack,
1996) added client-side prediction, for the Internet's latency.
*Age of Empires* (1997): deterministic lockstep with hundreds of units
(Paul Bettner and Mark Terrano, "1500 Archers on a 28.8: Network
Programming in Age of Empires and Beyond", GDC 2001). *Starsiege:
Tribes* (1998): the "Tribes networking model", prioritizing what each
client needs (Mark Frohnmayer and Tim Gift, GDC 2000). Valve's Source
engine: interpolation and lag compensation documented (Yahn Bernier,
"Latency Compensating Methods in Client/Server In-game Protocol Design
and Optimization", 2001). *XPilot* (Bjørn Stabell and Ken Ronny
Schouten, 1991): a multiplayer Spacewar!/Thrust over the Internet, a
server and X11 clients.

**Rollback.** GGPO (Tony Cannon, 2006; open source since 2019): the
rollback library that became the fighting games' standard; the idea is
also Elm-friendly by nature, and was used in emulators' netplay.

**Articles.** Glenn Fiedler's "Networking for Game Programmers" and
"Networked Physics" series (gafferongames.com), the clearest
introduction, from UDP to deterministic lockstep and snapshot
interpolation; Gabriel Gambetta's "Fast-Paced Multiplayer" (client-side
prediction, reconciliation, interpolation, with live demos).

**Libraries.** ENet (reliable and unreliable channels over UDP), RakNet,
Valve's GameNetworkingSockets; for the web, WebSockets and WebRTC data
channels, and servers like Colyseus; in OCaml, Lwt or Eio with sockets,
and WebSocket libraries.

**Replicated computation.** *Croquet* (David A. Smith, Alan Kay and
others, 2000s, and the Croquet company today): every participant runs
the same deterministic computation, only external events are
replicated through a reflector -- lockstep as a general platform, and
the closest idea to this plan's "Elm's model is already a pure function
of the inputs".

**Elm.** Elm has WebSocket support but no multiplayer framework; the
determinism of its architecture is exactly what lockstep needs, which
is this plan's bet.
