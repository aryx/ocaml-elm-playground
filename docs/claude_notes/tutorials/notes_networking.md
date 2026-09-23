# Multiplayer, from scratch: a tutorial for `networking/`

How two computers run the same game: what the network actually gives
you (packets, late and lost), the three architectures built on top of
that (lockstep, rollback, client-server), and why this playground is
an unusually good place to learn them -- a model that is a pure
function of its inputs is already half of a netcode.

It was the specification of the library planned in
[`plan_networking_teaching.md`](../plans/done/plan_networking_teaching.md),
written before the code; it has since been checked against it
(2026-09-23), what was built differently said where it was, and its
numbers measured (all of them in the related-work note's postscript). Companions:
[`notes_networking_related_work.md`](../related-work/notes_networking_related_work.md)
(Doom, Quake, Age of Empires, GGPO, and where ours stops) and
[`notes_2d_physics.md`](notes_2d_physics.md), whose fixed time step is
this library's precondition.

## 0. Where the code is, and a reading order

The protocols and the netcode are in `networking/`, pure OCaml, bytes
in and bytes out, no system call -- so the tests need no network, and
the browser runs them too: `protocols/` the bytes other programs agree
on, `netcode/` the multiplayer games' machinery on top of them
(`networking/README.md`); what opens sockets is in `networking/unix/`,
native only.

| module | what | section |
|---|---|---|
| `protocols/Wire` | values to bytes and back: varints, zigzag, garbage refused | §2 |
| `netcode/Sim_net` | a network in one process: latency, jitter, loss, duplication, from a seed | §3 |
| `netcode/Inputs` | the input exchange lockstep and rollback share: unacked inputs resent, acks, checksums | §4 |
| `netcode/Lockstep` | wait for every input, with an input delay (0: 1997's way) | §4 |
| `netcode/Checksum` | catching a desync before it becomes a mystery | §4 |
| `netcode/Rollback` | predict the others, correct when wrong | §5 |
| `netcode/Snapshot`, `Prediction`, `Interpolation` | a server owns the game; clients predict and interpolate | §6 |
| `protocols/Transport`; `unix/Udp`, `unix/Tcp` | what carries packets; UDP between two computers | §7 |
| `protocols/Websocket`; `unix/Server`, `unix/Relay`, `unix/Relay_client` | the browser's socket, and the relay it needs | §7 |
| `playground/ways/Universe`, `unix/Universe_server` | HtDP's other shape: worlds with a mailbox, and a server | §9 |
| `playground/apis/Multiplayer` | the Evan-style API over all of it | §12 |
| `protocols/Url`, `Http`, `Irc`; `unix/Http_client`, `unix/Http_request`, `unix/Irc_server` | the protocols of §13, beyond games | §13 |

Read §1-§2 for what the network is, §3 for the tool that makes the
rest testable, §4-§6 for the three architectures in increasing order
of ambition, §8 for the property all of them stand on, §9 for the
gentler shape that needs none of it, and §10-§11 for how it compares
with what games ship and what is left as exercises.

## 1. The network is not a wire

It moves **packets** -- a few hundred bytes, handed to the Internet
with no promise. A packet can arrive late, arrive twice, arrive out of
order, or never arrive.

Two protocols sit on top, and games famously pick the "worse" one:

```
   TCP: a reliable, ordered stream. Lost packets are resent, and
        everything behind a lost packet WAITS for it
        (head-of-line blocking) -- a stall of one round trip or more.

   UDP: packets, as they come, or not at all. You handle loss.
```

Games use UDP, because **an old input is worthless once a newer one
exists**. Resending a position from 100 ms ago, and delaying
everything behind it to do so, is exactly the wrong trade for a game;
it is the right one for a file.

**Latency has a floor**, and it is worth knowing in frames rather than
milliseconds. Light in fibre goes about 200,000 km/s, i.e.

```
   5 ms per 1000 km, one way

   Paris -> New York = 5,840 km  ->  29 ms one way, ~60 ms round trip
   one frame at 60 fps           =   16.7 ms
   so: an Atlantic round trip    =   4 frames, before any router
```

**Jitter** is that latency varying packet to packet; **loss** is a few
percent on a bad link, and more on Wi-Fi. Nothing in this note makes
any of it smaller. Every technique below is a way of *hiding* it, and
each hides it by giving something up: input delay, or CPU, or
correctness about the other players' present.

## 2. A message is bytes

A player's whole input fits in one byte: a bit per arrow, a bit per
button. `Wire` turns values into bytes and back, with three rules
worth stating because breaking them is how protocols rot:

- **fixed sizes and a fixed byte order** (network order is big-endian);
- **variable-length integers** where sizes vary -- the same
  seven-bits-per-byte trick MIDI uses for delta times
  ([`notes_audio_midi.md`](notes_audio_midi.md) §4), which is a nice
  reminder that a wire format is a wire format;
- **every message starts with its type and the tick it is about**, and
  anything that does not parse is *dropped*, never trusted. A parser
  fed by the Internet is the most attacked code you will ever write.

The arithmetic is the surprising part, and it changes the design:

```
   lockstep, 1 byte of input per tick, 60 ticks a second
   every UDP packet costs 28 bytes of IP + UDP headers

   naive:  (1 + 28) x 60  = 1,740 bytes/s   -- 94% headers!
   better: send the last 3 inputs, 20 packets a second
           (3 + 28) x 20  =   620 bytes/s   -- and loss is now free:
                                               each input is sent 3 times
```

Redundancy instead of retransmission: by the time you could ask for a
lost input again, you have already received the next three. That one
idea is most of what "handle loss yourself" means in practice.

**As built** (`Inputs`), simpler than the above and more generous: a
packet *every frame*, carrying every input the other side hasn't
acknowledged yet, and acknowledging theirs -- so a lost packet is
covered by the next one whatever the loss, and nothing ever waits for
a resend. Measured (two peers, a byte of input a tick): 12 bytes a
packet on a LAN, 22 at 100 ms (more inputs unacknowledged in flight),
so 2,400 to 3,000 bytes a second per peer with the headers -- four
times the 620 above, the price of 60 packets a second instead of 20.
Sending a packet every third frame is the exercise that gets the 620
back.

## 3. Build the fake network first

The most useful module in the library is the one that never touches a
socket. `Sim_net` runs every peer in **one process**, passing messages
through a queue that applies **latency, jitter, loss, duplication and
reordering, deterministically from a seed**.

It is three tools at once:

- **a teaching tool**: play both sides side by side in one window, and
  *see* what player 2 sees -- `net=simulate` (§12), where `[` and `]`
  change the latency, `-` and `=` the loss, and `n` the netcode, the
  way the graphics keys toggle shading;
- **a testing tool**: the whole protocol runs in `make test`, with no
  sockets, no ports, no flakiness -- "after 1,000 ticks, every peer's
  model is identical, under any latency and loss" is a *unit test*;
- **a debugging tool**: a desync that happens once an hour on a LAN
  happens every time, on demand, with a seed you can paste into a bug
  report.

Write it first. Every real netcode bug you will meet is easier to see
here than on two machines.

## 4. Lockstep: send the inputs, simulate everywhere

The oldest architecture, and the one this playground is shaped for.

```
   tick:    1     2     3     4
   peer A:  a1    a2    a3    a4  ---.
                                      >  each peer applies tick n only
   peer B:  b1    b2    b3    b4  ---'   once it has BOTH a_n and b_n,
                                          then runs update (a_n, b_n)
```

Nobody sends the game state. Nobody *can* send the game state -- both
peers compute it, from the same starting model and the same inputs.
That is why Age of Empires could put 1,500 units on a 28.8k modem: the
bandwidth is the players' fingers, not the world.

Three costs, all real:

- **Input delay.** Waiting for the other peer's input every tick would
  stall constantly, so an input pressed at tick *n* is scheduled to
  apply at tick *n+3* -- enough time to arrive. The game is then three
  frames late *for everyone, always*, which is the trade fighting
  games refuse (§5) and strategy games never notice.
- **The slowest peer sets the pace.** One player on hotel Wi-Fi stalls
  everybody. Doom did this in 1993 and so does every RTS since.
- **Determinism, bit for bit.** The same inputs must give the same
  model on every machine. Same binary and same CPU: fine, and the
  golden frames already depend on it. A different compiler, or a
  float operation reordered, and the two games drift apart *silently*
  and forever -- a **desync**.

So a peer sends a **checksum** of its model every second, and a
mismatch stops the game with a message. A desync you detect is a bug;
a desync you do not detect is a ghost story about how "the game went
weird after ten minutes".

**A worked example, from this project's own history.** The author's
first network game (*tronscroll*, C and svgalib, late 1990s; see the
plan's "Prior art in the house") did something simpler than any
architecture in this note: each frame it sent the whole state and then
**blocked, waiting for the answer** -- `send_coord` then `recv_coord`,
over TCP. That is one full round trip per frame, so the frame rate is
capped at `1 / RTT`:

```
   school LAN, RTT under 1 ms   ->  hundreds of frames per second: fine
   Internet,   RTT 60 ms        ->  16 frames per second, and jerky
```

Everything in §4 and §5 is machinery for not doing that: input delay
pays 3 frames *once* instead of a round trip *every* frame, and
rollback pays none at all and apologises afterwards.

**Measured** (`Lockstep` over `Sim_net`, two peers, 600 ticks): with a
delay of 3, full speed up to 30 ms one way; 1.09 times slower at 50
(exactly the delay's budget); 2.05 times at 100 (600 ticks in 1,231
frames: half speed); 3.04 at 150. Loss is cheap: at 30 ms, 10% of the
packets lost costs 2.5% of the speed, 50% lost costs 39%, since every
packet repeats what the lost ones carried. And with no delay at all
-- tronscroll's way, `netcode=1997`, peer to peer here so a trip one
way rather than a round trip -- a tick costs 1.5 frames even at 0 ms
(each peer steps before the other's input of the tick has come), 3 at
30 ms, 7 at 100: the slide show, measured.

## 5. Rollback: guess, and fix it afterwards

Lockstep's three-frame delay is exactly what a fighting game cannot
have. **Rollback** (GGPO, Tony Cannon, 2006) applies your own input
*immediately* and **predicts** the others -- the prediction being
"they are still doing what they did last tick", which is right most of
the time, because keys stay pressed.

When the real input arrives and disagrees, the peer goes back:

```
   tick:        10   11   12   13      B's real input for tick 11 arrives
   A computed:  m10  m11' m12' m13'    at tick 13 and differs from the guess
                      |
   A redoes:          m11  m12  m13    restore m10, replay 11-13 with the
                                        truth -- all inside one frame
```

The costs move from *delay* to *CPU and correctness*: up to a round
trip of ticks replayed every frame (4 ticks across the Atlantic), and
the other player visibly **snapping** when a guess was wrong -- which
players tolerate far better than lag on their own hands.

Saving every tick's state is the part that is painful in C++ and free
here: **old models are immutable values**, so "save state" is keeping
the last few in a list, with all the unchanged parts shared. The
rollback library the fighting-game world standardised on is, in this
architecture, a page of bookkeeping: `Rollback.ml`'s guessing, fixing
and confirming are 63 lines, and not one of them copies a model.
(This note first said "about twenty"; three times that, honestly
counted.)

**Measured** (600 ticks, the keys changing every 20 ticks): full speed
up to 100 ms (606 frames), paid in replays -- 30 rollbacks (one a
change of the other's keys), 30 ticks replayed at 0 ms, 93 at 50, 183
at 100, never more than 7 at once; at 150 ms the 8 ticks it may guess
ahead run out, and it stalls a little (714 frames). The worst frame
costs 0.15 ms with the tests' tiny game: a real game's replays cost its
update times the depth, the budget to watch.

## 6. Client and server: more players, and the browser

Past a handful of peers, and anywhere you cannot trust the players,
the shape changes: a **server owns the game**, clients send inputs and
receive **snapshots** of the state (Quake, 1996). Four techniques
follow, each fixing what the previous one broke:

- **client-side prediction** (QuakeWorld, 1996): the client simulates
  its own player at once, so the controls feel instant;
- **reconciliation**: when the server's snapshot disagrees, the client
  snaps to it and replays its own unacknowledged inputs -- rollback
  again, one-sided;
- **entity interpolation**: other players are drawn *slightly in the
  past*, between two received snapshots, so they move smoothly
  through jitter;
- **lag compensation**: the server rewinds the world to what the
  shooter actually saw before deciding whether a shot hit -- which is
  why you sometimes die behind cover.

More bandwidth (state, not inputs -- hence **delta compression**:
send only what changed), but no desyncs possible and cheating gets
harder: in lockstep every peer holds the whole world, so a hacked
client sees through walls by construction.

**As built**: `Snapshot` (the server applies each player's inputs in
order, one a tick, and repeats the last when the next is late -- the
game never waits for a slow client; the world goes out every 3 ticks,
20 a second, with each client's last input applied), `Prediction` (the
client plays its own inputs at once, the others' guessed from their
latest, and reconciles on each snapshot), `Interpolation` (a buffer
drawn 100 ms behind). Measured over 600 ticks at 50 ms: a client alone
is never mispredicted; with another player whose keys change 30 times,
27 and 23 mispredictions, about one a change, each corrected, the
worlds agreeing at the end. In `net=simulate` the server's screen sits
between the two clients', and the clients run a few ticks *ahead* of
it (127 against 120 at 100 ms): each lives in the server's near future.
Not built: interpolation in `Multiplayer` (the game must say which
parts are "the others"), lag compensation, delta compression.

## 7. Getting connected at all

Two homes cannot usually reach each other: each router's **NAT**
shares one public address and drops unsolicited incoming packets.
The three answers: a **LAN**, a **relay** server both sides connect
out to, or **hole punching** (both send out at once, to addresses a
rendezvous server told them).

In a browser there are no UDP sockets at all: **WebSockets** (TCP, to
a server -- head-of-line blocking and all) or **WebRTC data
channels**, which can be unreliable and unordered like UDP and do the
hole punching for you, at the price of a signalling server. Either
way the web backend needs a small server, which is why the plan
includes one.

**As built**: UDP between two computers (`net=host`, `net=join`: the
host learns its player from the first datagram, no handshake), and the
relay (`net=relay`): a WebSocket server (`Websocket`, RFC 6455, its
handshake needing SHA-1, `crypto/Sha1`) that every player connects out
to, which numbers the players as they come and copies each one's
packets to the others -- the game still peer to peer, only the route
through a middleman everyone can reach. Its event loop, `unix/Server`,
also serves §9's universe and §13's IRC. Not built: hole punching,
WebRTC.

## 8. What makes a game networkable

Everything above rests on one property, and it is worth stating as a
checklist because it is what actually fails:

```
   deterministic  =  same inputs  ->  same model, bit for bit

   [ ] a fixed time step (physics/: 1/60 s, never the wall clock)
   [ ] no global Random -- a seed, in the model, shared at the start
   [ ] no reading the clock inside update
   [ ] no iteration over a hash table whose order can differ
   [ ] the same float operations, in the same order, on both machines
```

This project has been quietly paying for that property since the
physics plan's fixed step, and the golden-frame tests are its proof at
one machine's scale. For randomness, `Playground.random` and `pick`
keep a seed in the model (`random/Lehmer`, the same numbers natively
and in a browser); `Tetris.ml` was converted and is checked by a test
(one seed, the same keys, Ticks from two different clocks: the same
checksum every second). The honest count of networkable games: the two
written for it, `TinySpacewar` and `TinyTronscroll`, and Tetris; eight
games still draw from the global `Random`. Lockstep is simply the same property, checked by
a second computer -- which is also why the networking phases wait for
`plan_playground_other.md`'s seeded randomness.

## 9. The other shape: a universe of worlds

Everything above assumes one game simulated in several places, which
is the hard version. There is a gentler one, and it has been taught to
beginners for twenty years: **many small programs, each with its own
world, sending each other messages through a server.**

That is HtDP's `2htdp/universe` (Felleisen, Findler, Flatt and
Krishnamurthi, *How to Design Programs*), and this playground already
has its other half: `Bigbang` runs a *world program* -- a
world, `to_draw`, `on_tick`, `on_key`. Its `.mli` even names the
missing piece, in its list of what big-bang has and we do not:
"universe, several world programs and a server exchanging messages:
the playground has no networking yet [...]; a Universe.ml would come
with it". This section is that file's specification. The networked
version adds two things and nothing else:

```
    world A                  the universe                 world B
   +---------+   message    +-------------+   message   +---------+
   | on_tick |------------->|   on_msg    |------------>| on_tick |
   | on_key  |              |   on_new    |             | on_key  |
   |on_receive|<-------------|  (a state + |<------------|on_receive|
   +---------+   message     |   letters)  |   message   +---------+
                             +-------------+
```

- a world gains **`on_receive`** (a message arrived; here is the new
  world) and the ability to **send**;
- the server is a program of the same shape: a state, **`on_new`**
  (someone joined) and **`on_msg`**, each returning the new state and
  the letters to post.

No determinism is required, no checksums, no rollback, nothing from
§4-§6: messages arrive when they arrive, and each world decides what
to do about it. That is the right first lesson, and the right shape
for a chat, a shared whiteboard, a turn-based board game, or twenty
students' rockets flying in one sky -- and it is the *wrong* shape for
Spacewar!, which is exactly the comparison worth teaching.

The two APIs therefore both exist here: `Multiplayer` (§4-§6, one
simulation, everywhere) and `Universe` (this section, many worlds, one
postbox), with the same transport underneath.

**As built**: `Universe` (a Bigbang world whose handlers
return a package, the world and the messages to send, and
`on_receive`), `unix/Universe_server` (the universe: a state and
`on_new`, `on_msg`, `on_disconnect` returning bundles, on HtDP's port,
4567) -- the relay's event loop with the program's handlers where the
relay has its rule. Messages are strings. The example is
2htdp/universe's first: `examples/UniverseBall.ml`, a ball passed from
world to world.

## 10. Compared with GGPO, ENet, Quake 3, Source and WebRTC

Who invented what, and the whole landscape, is
[`notes_networking_related_work.md`](../related-work/notes_networking_related_work.md).

**State as a value.** GGPO, the rollback library, is C++,
and the game hands it callbacks -- `save_game_state`,
`load_game_state`, `advance_frame` -- because a C++ game's state is
mutable memory that must be copied into a buffer every tick and
copied back on a rollback. Here "save" is keeping a value and "load"
is using an old one (§5), which is where §5's page of bookkeeping
comes from (63 lines, measured). ENet is the other
reference point: reliable and unreliable sequenced channels,
fragmentation, a connection handshake, bandwidth throttling -- a
transport. We have none of that: one message type per tick and
redundancy instead of retransmission (§2), which is enough for inputs
and nothing else.

**Snapshots, counted.** A back-of-the-envelope snapshot for
`TinySpacewar.ml`: a ship's `Physics.body` needs `x`, `y`, `vx`, `vy`,
`angle`, `spin` -- 24 bytes as 32-bit floats -- and a torpedo 16. Two
ships and four torpedoes are 112 bytes; at 20 packets a second with
§2's 28 bytes of headers, (112 + 28) x 20 = 2,800 bytes/s, 4.5 times
lockstep's 620, and growing with every torpedo, where lockstep's does
not. The playground's `netcode=server` sends Marshal's bytes of the whole
model instead, bigger still, and readable only by a copy of the same
program -- a codec of the game's own over `Wire` is what a real
network needs. Quake 3's answer is to send each snapshot as a **delta** against
the last one the client acknowledged, all over plain UDP: a lost
packet costs nothing but a larger next delta, against an older base.
Valve's Source engine adds §6's other half: other players drawn 100
ms in the past by default (`cl_interp`), and the server rewinding
them by the shooter's latency to judge a hit (Bernier, 2001).

**The browser.** WebRTC data channels are SCTP over DTLS over UDP,
each channel choosing ordered or not and how many times to
retransmit: an ENet built into the browser, with ICE, STUN and TURN
doing §7's hole punching and relaying. The price is a signalling
server and a tall stack of specifications -- which is why the plan
starts with WebSockets and a relay, the dumbest thing that works, and
accepts §1's head-of-line blocking in the browser.

## 11. What's missing, and exercises

Beyond what was built (WebRTC and the rest are in
[`plan_networking_remaining.md`](../plans/plan_networking_remaining.md)),
in rough order of difficulty:

- **adaptive input delay**: measure the round trip and pick the delay
  from it (half the RTT, in ticks, plus one) instead of §4's fixed
  three, and change it smoothly when the RTT does (`Lockstep`);
- **a demo file**: write the tick-by-tick inputs lockstep already
  exchanges to disk, and replay them, Doom's way -- which is
  [`notes_inspect.md`](notes_inspect.md)'s recording by another
  route (`Lockstep`);
- **finding the first bad tick**: on a checksum mismatch, exchange
  the checksums of the last N ticks to find the first that differs,
  and print both models there (needs a printer, like `Inspect`'s
  `?show`) (`Checksum`, §4);
- **time synchronisation for rollback**: a peer that runs ahead makes
  the other roll back further every frame; measure each side's
  "frame advantage" and let the one ahead wait a frame now and then,
  as GGPO does (`Rollback`, §5);
- **a reliable channel** for the few messages that must arrive (the
  seed and the starting model, a chat line): sequence numbers, acks
  piggybacked on the input packets, resend on timeout -- a small
  ENet (`Wire`, §2);
- **congestion avoidance**: drop from 20 packets a second to 10 when
  the RTT climbs, back when it recovers (Fiedler's articles)
  (`Sim_net` can make the congestion, §3);
- **dead reckoning** in the snapshot engine: extrapolate another ship
  from its last velocity instead of interpolating in the past, and
  compare the two on the latency keys (`Snapshot`, §6);
- **delta compression against the acknowledged snapshot**, Quake 3's
  way (`Snapshot`, §6);
- **hole punching** through a rendezvous server, so two homes can play
  without the relay carrying every packet (§7);
- **cross-platform determinism**: run the lockstep test of §8 between
  the native and the web backends; if floats diverge (and whether they
  do is the experiment), make the physics fixed-point (§8);
- **a packet every third frame**, three inputs in each, to bring §2's
  measured 2,400 bytes a second down to its 620 (`Inputs`);
- **interpolation in `Multiplayer`**: a hook where the game says what
  of the others to draw from the past, so that `netcode=server` shows
  them smoothly (`Interpolation`, §6);
- **`netcode=server` between computers**, the world encoded by the
  game over `Wire` instead of Marshal (§6);
- **TinyTronscroll's 8 players**, as the original had: a `players=`
  flag, and more panels in `net=simulate` -- the relay already seats
  any number (§7);
- **plain TCP for IRC**, so that `irssi` talks to our server and
  TinyIRC to the real networks (§13).

## 12. In the playground

Evan-style, the new concept is the **player** -- everyone's input,
where `computer` is yours:

```ocaml
type player = { id : int; keyboard : keyboard; pressed : keyboard }

val Multiplayer.game :
  ?network:< Cap.network ; .. > -> ?split:bool -> players:int ->
  (computer -> int -> 'model -> shape list) ->            (* view, for player n *)
  (computer -> player list -> 'model -> 'model) ->        (* update, everyone's input *)
  'model -> ('model state game, msg) app
```

`update` gets a *cleaned* computer -- no keyboard, no mouse (they are
in the players), a fixed screen, ticks for time, the flags -- since
anything that differs between computers breaks §8; `pressed` is each
player's keys that went down this tick, from the inputs themselves.
The same game then runs without a line changing, chosen by flags:
`net=local` (one keyboard; `?split` gives each player a window),
`net=simulate` (two computers side by side through §3's fake network,
`latency=`, `loss=` and the keys), `net=host` / `net=join` (UDP),
`net=relay` (WebSocket, a browser too); and `netcode=lockstep`,
`rollback`, `1997` or `server`. `TinySpacewar` is the flagship,
`TinyTronscroll` (the author's own 1997 game) the three eras side by
side. A program reaching the network says so in its type: it passes
its `Cap.network` (plan_caps.md).

## 13. Beyond games: requests and chat

The plan was about games; three protocols came along, each teaching
one idea:

- **HTTP** (`Url`, `Http`, `unix/Http_client`): a request and its
  answer, the four ways a body ends, redirections resolved by RFC
  3986's algorithm -- written to replace curl for `http://` (`https://`
  needs TLS: `crypto/` is its start). `Playground.Http.get`, Elm's, is a
  command performed without blocking the frames (`unix/Http_request`:
  a non-blocking state machine over `select`, the event loop on one
  socket).
- **IRC** (`Irc`, `unix/Irc_server`, `apps/internet/TinyIRC`): chat as
  lines of text a person could type, a server keeping nicks and
  channels, over WebSocket so that a browser joins.
- **The universe** (§9): messages between programs, no determinism.

## Glossary

- **Packet**, **UDP**, **TCP**, **head-of-line blocking**: §1.
- **Latency**, **round trip time (RTT)**, **jitter**, **loss**.
- **Tick**: one simulation step; in lockstep, the only clock there is.
- **Lockstep**: exchange inputs, simulate everywhere; **input delay**:
  scheduling an input a few ticks ahead so it arrives in time.
- **Desync**: two peers' models diverging; **checksum**: how you find
  out before the players do.
- **Rollback**: predict the others' inputs, restore and replay when
  the guess was wrong; **prediction**, **replay**.
- **Snapshot**, **client-side prediction**, **reconciliation**,
  **entity interpolation**, **lag compensation**, **delta
  compression**: the client-server toolkit (§6).
- **NAT**, **hole punching**, **relay**, **WebRTC data channel**: how
  two machines reach each other at all.
- **World program** / **universe** (HtDP): a program with its own
  world and a mailbox, and the server that carries the mail (§9) --
  the shape that needs none of the determinism above.
- **Determinism**: the property everything here stands on (§8).

## References

- Leslie Lamport, "Time, Clocks, and the Ordering of Events in a
  Distributed System", Communications of the ACM 21(7):558-565, 1978
  (lockstep's total order of inputs).
- Jon Postel, "User Datagram Protocol", RFC 768, 1980.
- Jon Postel (ed.), "Transmission Control Protocol", RFC 793, 1981.
- David R. Jefferson, "Virtual Time", ACM Transactions on Programming
  Languages and Systems 7(3):404-425, 1985 (Time Warp, rollback's
  ancestor).
- Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Shriram
  Krishnamurthi, "How to Design Programs", MIT Press, 2001 (2nd ed.
  2018).
- Paul Bettner, Mark Terrano, "1500 Archers on a 28.8: Network
  Programming in Age of Empires and Beyond", Game Developers
  Conference, 2001.
- Yahn W. Bernier, "Latency Compensating Methods in Client/Server
  In-game Protocol Design and Optimization", Game Developers
  Conference, 2001.
- Glenn Fiedler, "Fix Your Timestep!", gafferongames.com, 2004.
- Bryan Ford, Pyda Srisuresh, Dan Kegel, "Peer-to-Peer Communication
  Across Network Address Translators", USENIX Annual Technical
  Conference, 2005.
- Tony Cannon, GGPO, 2006 (open sourced 2019).
- Glenn Fiedler, "Networking for Game Programmers" (article series),
  gafferongames.com, 2008.
- Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, Shriram
  Krishnamurthi, "A Functional I/O System, or, Fun for Freshman
  Kids", ICFP 2009 (`2htdp/universe`).
- Ian Fette, Alexey Melnikov, "The WebSocket Protocol", RFC 6455,
  2011.
- Randell Jesup, Salvatore Loreto, Michael Tüxen, "WebRTC Data
  Channels", RFC 8831, 2021.
