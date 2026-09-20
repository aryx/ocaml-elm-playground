# Plan: multiplayer for the playground, from scratch, for teaching (`network/`)

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
