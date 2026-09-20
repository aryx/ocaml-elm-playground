# network/ vs. the rest of the multiplayer world

Where a small teaching netcode sits among the architectures the games
industry actually ships -- lockstep RTSs, client-server shooters,
rollback fighting games -- and among the distributed-systems ideas
they rediscovered. What they do that this will not, and which of their
ideas fit in a few hundred readable lines. Companions:
[`notes_networking.md`](../tutorials/notes_networking.md) (how it
works) and
[`plan_networking_teaching.md`](../plans/plan_networking_teaching.md)
(what gets built, in what order).

## The one-line version

| | What it optimizes for | What you write |
|---|---|---|
| Lockstep RTS (Doom, Age of Empires, StarCraft) | Huge simulations on tiny bandwidth | Inputs on the wire, and a simulation that must be bit-for-bit deterministic |
| Client-server FPS (Quake, QuakeWorld, Source, Tribes) | Many players, hostile clients, Internet latency | A server that owns the world, snapshots out, prediction and interpolation on the client |
| Rollback (GGPO, and every modern fighting game) | Your own input feeling instant, at any ping | Save every tick, predict the opponent, replay when wrong |
| Unity Netcode, Photon, Mirror, Nakama, Colyseus | Shipping a product this quarter | Attributes and callbacks over someone else's replication engine |
| ENet, RakNet, GameNetworkingSockets, netcode.io, QUIC | Reliable-and-unreliable channels over UDP, done properly | A transport API: channels, fragmentation, congestion, encryption |
| Croquet / TeaTime, distributed simulation (DIS, HLA) | Replicated computation as a platform | A deterministic world and a reflector that orders external events |
| HtDP's `2htdp/universe` | Teaching beginners that programs can talk | A world with `on-receive`, and a server with `on-new` / `on-msg` -- no determinism asked for |
| `network/` + `playground/Multiplayer` | Seeing *why* each of those exists, on a game you already have | `multiplayer ~players:2`, and a fake network with latency and loss on a key |

## Part 1: the games that invented it

- **Doom** (id Software, 1993): four players, peer-to-peer lockstep
  over IPX on a LAN, exchanging inputs -- and everyone stalling for
  the slowest machine. Its demo files are the same idea recorded: a
  run *is* its inputs.
- **Quake** (1996): client-server over UDP, the server authoritative.
  Then **QuakeWorld** (John Carmack, 1996) added **client-side
  prediction**, because the Internet, unlike a LAN, has a round trip
  you can feel. The two together are the shape almost every shooter
  still has.
- **Age of Empires** (1997) and the talk that taught a generation:
  Paul Bettner and Mark Terrano, **"1500 Archers on a 28.8: Network
  Programming in Age of Empires and Beyond"** (GDC 2001) -- lockstep,
  the turn queue, and the discovery that the hard part is not
  bandwidth but *determinism* (a pathfinder that reads a hash table in
  memory order will desync two machines in minutes).
- **Starsiege: Tribes** (1998): the "Tribes networking model" (Mark
  Frohnmayer and Tim Gift, GDC 2000) -- prioritising *what each client
  most needs to know* when there is not enough bandwidth for
  everything, which is the question snapshot engines have asked ever
  since.
- **Valve's Source engine**: Yahn Bernier, **"Latency Compensating
  Methods in Client/Server In-game Protocol Design and Optimization"**
  (2001), the canonical write-up of interpolation, prediction,
  reconciliation and **lag compensation** -- and the reason you
  sometimes die after reaching cover.
- **XPilot** (Bjørn Stabell and Ken Ronny Schouten, 1991): a
  multiplayer Spacewar!/Thrust over the Internet, with a server and
  X11 clients, years before any of the above. The plan's later arena
  is its descendant.
- **Fighting games and rollback**: **GGPO** (Tony Cannon, 2006; open
  sourced 2019) made rollback the community standard, first in
  emulator netplay, then in shipped games (Killer Instinct, Skullgirls,
  Guilty Gear Strive, Street Fighter 6). The fighting-game community's
  own explainer -- Infil's rollback guide -- is better than most
  academic treatments. (Names and dates from memory, to check.)

## Part 2: the systems today

- **Transports**: **ENet** (reliable and unreliable channels over UDP,
  the hobbyist standard for twenty years), **RakNet**, Valve's
  **GameNetworkingSockets**, Glenn Fiedler's **netcode.io** and
  **reliable.io**, and now **QUIC** -- which is, from a game's point
  of view, UDP with congestion control, encryption and multiple
  streams, and which finally gives the browser something like UDP
  through **WebTransport**.
- **Engines and services**: Unity's Netcode for GameObjects, Mirror,
  FishNet, Photon, Nakama, **Colyseus** (room-based, JavaScript),
  Geckos.io (UDP-ish in the browser over WebRTC). All of them answer
  "replicate my objects for me", which is the opposite of what a
  teaching library should do.
- **The browser** remains the constrained case: **WebSockets** (TCP,
  head-of-line blocking) or **WebRTC data channels** (unreliable and
  unordered, plus a signalling server and a pile of specification).
  Anything cross-platform therefore needs a small server, which is why
  the plan builds a relay rather than pretending peers can meet.

## Part 3: the distributed-systems ancestors

The thing worth knowing, and the part game articles rarely say: **the
games rediscovered algorithms the simulation and distributed-systems
communities had already published.**

- **Lockstep is state machine replication**: every replica runs the
  same deterministic machine on the same ordered inputs (Lamport's
  work on ordering and replication, 1978 onwards). A game's "turn
  queue" is a total order of commands.
- **Rollback is Time Warp**: David Jefferson, **"Virtual Time"** (ACM
  TOPLAS, 1985), optimistic synchronisation for parallel discrete
  event simulation -- process events speculatively, and roll back with
  anti-messages when a straggler arrives. GGPO is Time Warp with two
  processes and a 16-millisecond deadline.
- **Dead reckoning** -- extrapolating a remote entity from its last
  known position and velocity -- comes from **SIMNET and DIS**, the
  US military's distributed simulation work of the 1980s-90s, later
  standardised as **HLA**. Every "the other ship keeps gliding" in a
  game is that.
- **Croquet / TeaTime** (David A. Smith, Alan Kay and others, 2000s,
  and the Croquet company today): replicated deterministic computation
  as a *platform* -- only external events go through a reflector that
  orders them. It is the closest thing to this plan's bet that "the
  model is already a pure function of the inputs", generalised beyond
  games.
- The cousin this project deliberately does not build:
  **collaborative editing** (operational transformation, CRDTs), where
  the state is a document rather than a simulation and convergence
  replaces determinism -- named in
  [`plan_gui_teaching.md`](../plans/plan_gui_teaching.md)'s out of
  scope.

## Part 4: the teaching lineage

- **Glenn Fiedler's gafferongames.com** -- "Networking for Game
  Programmers", "Networked Physics", "Fix Your Timestep!" -- the
  clearest introduction there is, and already this project's reference
  for the fixed step.
- **Gabriel Gambetta, "Fast-Paced Multiplayer"**: four short chapters
  with *live, interactive demos* of prediction, reconciliation and
  interpolation, each with a latency slider. The closest existing
  thing to this plan's `-simulate` mode, and the model for it.
- The **GDC talks** above (Age of Empires, Tribes) and Bernier's Valve
  paper: primary sources, all readable in an evening.
- **Overwatch's "Netcode" GDC talk** (Tim Ford, 2017) for how a modern
  AAA game stacks all of it at once. (To check.)
- **HtDP's `2htdp/universe`** (Matthias Felleisen, Robert Bruce
  Findler, Matthew Flatt, Shriram Krishnamurthi, *How to Design
  Programs*): the only one of these written for *beginners*, and the
  one with the longest teaching record. Its shape -- a world program
  with `on-receive`, and a universe server with `on-new` and `on-msg`
  returning a new state plus the letters to post -- deliberately asks
  for no determinism, no prediction and no checksums, which is why a
  fourteen-year-old can write a networked program with it in an
  afternoon. This repository already has its other half
  (`playground/Bigbang`), so the universe is a layer rather than a
  project, and the plan treats it as the gentle door into everything
  the rest of this note is about.

## Part 5: in Elm, and in OCaml

- **Elm** has WebSocket support and no multiplayer framework at all --
  but its architecture is exactly what lockstep needs, and saying so
  precisely is this plan's whole bet: `update : msg -> model -> model`
  is a state machine, and two copies of a state machine fed the same
  messages stay equal. What Elm lacks for it is not the architecture
  but the transport (no UDP, and no way to be a server).
- **OCaml** brings two things that matter here and are worth naming:
  immutable values, so rollback's "save state" is a list rather than a
  memcpy; and a type system that makes a wire format's parser honest
  (a `Result`, not a segfault). The ecosystem's pieces are **Lwt** or
  **Eio** for the sockets, `Dream` or `ocaml-websocket` for the
  browser side, and **MirageOS**, which is the extreme version of this
  project's spirit: the TCP/IP stack itself, from scratch, in OCaml.
- **In this repository**, the groundwork is already half-built, which
  is why the plan is short: the fixed time step
  ([`notes_2d_physics.md`](../tutorials/notes_2d_physics.md) §7), the
  golden frames that prove determinism at one machine's scale,
  `Input_script`'s recorded keys, and
  [`plan_inspect_teaching.md`](../plans/plan_inspect_teaching.md)'s
  recording -- which is lockstep's input log wearing a different hat.

## Prior art in the house: tronscroll

The author's first network game, kept at
`~/Dropbox/role-programmer/project/project-tron/tronscroll-0.1` and
worth reading against every architecture above: *tron v0.1*, C and
svgalib, up to 8 players, a 1600x1200 map seen through a scrolling
320x200 window, six power-ups, and a README apologising for its
English. Written at INSA/ENS Rennes in the late 1990s (the date is the
author's to confirm; the README asks for "a minimum 486DX2/66").

Read as netcode, it is a complete catalogue of the choices this field
later gave names to:

| what it did | what it is called | where it hurts |
|---|---|---|
| TCP, `SOCK_STREAM` | reliable ordered stream | head-of-line blocking (§1) |
| sent every player's x, y and option | **snapshots**, not inputs (§6) | grows with players, not with the world |
| wrote the C struct raw to the socket | no wire format | one endianness, one compiler, one machine type |
| clients probe ports 2223, 2224, ... until one accepts; the port is your player number | matchmaking | dead on arrival behind a NAT (§7) |
| `send_coord` then a blocking `recv_coord`, every frame | a synchronous round trip per frame | caps the frame rate at 1/RTT: ~16 fps at 60 ms |

The last row is the interesting one, and it is why it belongs in a
teaching document rather than in a memoir: on a school LAN, with a
round trip under a millisecond, that design is *invisible* -- it runs
at hundreds of frames per second and nothing is wrong. The Internet is
what turns it into a 16 fps slideshow. Every technique in Part 1
above, from Doom's input delay to GGPO's rollback, exists to avoid
that one line, and none of them would have looked necessary from
inside a computer room in Rennes.

`games/TinyTronscroll.ml` is the plan's milestone for exactly that
reason: the same game, with the 1997 behaviour kept on a key beside
lockstep and rollback.

## Where `network/` and `Multiplayer` actually sit

Two levels, as everywhere here:

- **`network/`, the library**, at the legible end: `Wire`, `Sim_net`,
  `Lockstep`, `Rollback`, `Checksum` -- one idea each, with the
  simulated network making every one of them testable without a
  socket, and the two architectures switchable on a key so the
  difference between three frames of input delay and a rollback snap
  can be *felt* rather than described.
- **`playground/Multiplayer`, the API**, at the simple end: one new
  concept (the `player`), and the same game running local,
  simulated, hosting or joining without a line changing -- beside
  **`playground/Universe`**, HtDP's shape for the other half of the
  subject: many worlds, one postbox, and nothing to keep in sync.

**The ceiling, stated now**: a handful of players, a LAN or a local
relay, no matchmaking, no accounts, no encryption or authentication,
no anti-cheat beyond what a server's own checks give, and no
client-server snapshot engine until the "later" phase. What it is for
is that two computers should be able to play Spacewar!, and that a
reader should understand exactly which lie each technique tells to
hide the speed of light.

## Postscript: the numbers (to come)

Once built: bytes per second on the wire for lockstep and for
rollback; the input delay in frames at 0, 50 and 150 ms of simulated
latency; rollback's replayed ticks per frame at the same latencies,
and the frame time that costs; the loss rate at which lockstep starts
stalling; and how many of this repository's games pass the
determinism check of
[`notes_networking.md`](../tutorials/notes_networking.md) §8 without
changes (the honest number, and probably small at first).

Sources: from memory unless linked, and to be checked before relying
on them for teaching -- particularly the GGPO and rollback-in-shipped-
games dates, the Tribes and Overwatch talks, and the DIS/HLA
chronology.
