# Multiplayer, from scratch: a tutorial for `network/`

How two computers run the same game: what the network actually gives
you (packets, late and lost), the three architectures built on top of
that (lockstep, rollback, client-server), and why this playground is
an unusually good place to learn them -- a model that is a pure
function of its inputs is already half of a netcode.

It is the specification of the library planned in
[`plan_networking_teaching.md`](../plans/plan_networking_teaching.md):
written before the code, to be checked against it and have its numbers
measured. Companions:
[`notes_networking_related_work.md`](../related-work/notes_networking_related_work.md)
(Doom, Quake, Age of Empires, GGPO, and where ours stops) and
[`notes_2d_physics.md`](notes_2d_physics.md), whose fixed time step is
this library's precondition.

## 0. Where the code is, and a reading order

| module (`network/`) | what | section |
|---|---|---|
| `Wire` | values to bytes and back: varints, message headers, rejecting garbage | §2 |
| `Sim_net` | a network in one process: latency, jitter, loss, duplication, from a seed | §3 |
| `Lockstep` | exchange inputs, simulate everywhere | §4 |
| `Checksum` | catching a desync before it becomes a mystery | §4 |
| `Rollback` | predict the others, correct when wrong | §5 |
| `Snapshot` (later) | a server owns the game; clients predict and interpolate | §6 |
| `network/relay/` | the little server the browser needs | §7 |
| `playground/Multiplayer` | the Evan-style API over all of it | §9 |

Read §1-§2 for what the network is, §3 for the tool that makes the
rest testable, §4-§6 for the three architectures in increasing order
of ambition, and §8 for the property all of them stand on.

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

## 3. Build the fake network first

The most useful module in the library is the one that never touches a
socket. `Sim_net` runs every peer in **one process**, passing messages
through a queue that applies **latency, jitter, loss, duplication and
reordering, deterministically from a seed**.

It is three tools at once:

- **a teaching tool**: play both sides side by side in one window, and
  *see* what player 2 sees -- with `-debug-keys`, one key adds 100 ms,
  another 10% loss, the way the graphics keys toggle shading;
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
architecture, about twenty lines of bookkeeping.

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
one machine's scale. Lockstep is simply the same property, checked by
a second computer -- which is also why the networking phases wait for
`plan_playground_other.md`'s seeded randomness.

## 9. In the playground

Evan-style, the new concept is the **player** -- everyone's input,
where `computer` is yours:

```ocaml
type player = { id : int; keyboard : keyboard; mouse : mouse }

val multiplayer :
  players:int ->
  (computer -> int -> 'model -> shape list) ->   (* view, for player n *)
  (time -> player list -> 'model -> 'model) ->   (* update, everyone's input *)
  'model -> ('model, msg) app
```

and the same game runs four ways without a line changing: `-local`
(two players, one keyboard, as in 1962), `-simulate` (both sides in
one window through §3's fake network, with latency and loss on keys),
`-host` / `-join address` (two machines). Spacewar! is the flagship,
Pong the simplest test.

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
- **Determinism**: the property everything here stands on (§8).
