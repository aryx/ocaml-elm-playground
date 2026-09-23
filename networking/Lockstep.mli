(* Lockstep: send the inputs, simulate everywhere.

   The oldest way to play a game on several computers (Doom, 1993; Age
   of Empires, 1997), and the one Elm's architecture is shaped for. A
   game's model is a function of its inputs: the first model, then each
   tick's inputs folded through update. So nobody sends the game --
   every peer computes it, from the same first model and the same
   inputs, and the peers only exchange their inputs: a few bytes a
   tick, whatever the size of the world (1,500 archers on a 28.8k
   modem).

       tick:      0    1    2    3    4    5
       peer A:    -    -    -    a3   a4   a5    each peer simulates tick
       peer B:    -    -    -    b3   b4   b5    n only once it has every
                  \___________/                  player's input for n
                   delay = 3: known by all
                   in advance (no input)

   **Input delay.** Waiting for the other peer's input of *this* tick
   would stall every frame (a packet takes time), so an input read at
   tick t applies at tick t + delay: it is sent at once, and has
   [delay] ticks to arrive before anyone needs it. The first [delay]
   ticks have no input, for everybody. The game answers the keys
   [delay] frames late -- for every player, always: the price, which
   strategy games never notice and fighting games refuse (rollback,
   plan_networking_teaching.md phase 4).
   With a delay of 3 (50 ms at 60 ticks a second), a network faster
   than that one way never stalls; slower, the game waits -- a
   *stall* -- and the slowest peer sets everybody's pace.

   The inputs travel as Inputs.mli says: each packet carries every
   input not yet acknowledged, so a lost packet costs nothing, and the
   checksums of the models, so that a desync is caught.

   Worked example (checked by the tests, Sim_net as the network): two
   and three peers, 1,000 ticks each, their inputs from a seed, under
   latency, jitter, 10% loss and duplication: every peer computes the
   same model at every tick, the model of the same inputs applied on
   one machine. With a delay of 3, 30 ms of latency never stalls; 100
   ms (6 frames) stalls about every other frame: 600 ticks take some
   1,230 frames, the game at half speed -- 3 ticks of delay for 6
   frames of travel. A game whose update reads something only one peer has
   (a disagreement injected at tick 500) is caught at the first
   checksum after it.

   References: Paul Bettner and Mark Terrano, "1500 Archers on a 28.8:
   Network Programming in Age of Empires and Beyond" (GDC 2001); John
   Carmack's Doom (1993) network code, peer-to-peer lockstep over IPX;
   Glenn Fiedler, "Deterministic Lockstep" (gafferongames.com, 2014). *)

type t

(* peer [me] of [players] (numbered 0 to players - 1), inputs applied
 * [delay] ticks after they are read *)
val create : me:int -> players:int -> delay:int -> t

(* the next tick to simulate (0 at the start) *)
val tick : t -> int

(* [step peer mine]: if every player's input for the next tick is
 * known, that tick's inputs, by player (and [mine], read now, scheduled
 * for [delay] ticks later), the tick counted done; None if one is
 * missing: a stall, try again next frame *)
val step : t -> string -> string array option

(* the packet to send to every other peer, this frame *)
val packet : t -> string

(* a packet from another peer; garbage, or a packet about another
 * game, is dropped *)
val receive : t -> string -> unit

(* the checksum of my model after [tick] (the game computes it,
 * Checksum.of_model): sent in my next packets, compared with theirs *)
val checksum : t -> tick:int -> int32 -> unit

(* the first disagreement seen: the tick, and the peer whose checksum
 * differed from mine *)
val desync : t -> (int * int) option

type stats = { stalls : int; dropped : int; inputs_sent : int }

val stats : t -> stats
