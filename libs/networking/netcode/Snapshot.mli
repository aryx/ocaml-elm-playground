(* Snapshot: a server owns the game; clients send keys, get the world.

   Lockstep and rollback (Lockstep.mli, Rollback.mli) run the whole game
   on every computer, from everyone's inputs. Past a handful of players,
   and wherever players can't be trusted (in lockstep every computer
   holds the whole world: a hacked client sees through walls by
   construction), the shape changes: one *server* runs the game, the
   *clients* send it their inputs and are sent back the world, a
   *snapshot* of it, a few times a second (Quake, 1996).

     client 0 --- inputs 7, 8, 9 --->                  <--- inputs 4, 5 --- client 1
                                        server: the game
     client 0 <-- tick 212, your 8 applied, the world --    ... -- tick 212, your 5 -->

   Inputs, as in lockstep, are numbered and carried until acknowledged
   (each packet has all of mine the server hasn't applied), so a lost
   packet costs nothing. The server applies each player's inputs in
   order, one a tick, and when the next one hasn't arrived repeats the
   last (the key is probably still held): the game never waits for a
   slow client -- it stutters for that client alone. Every [rate] ticks
   (3: 20 a second) it sends each client the world, with the number of
   that client's last input applied (what Prediction.mli needs to
   reconcile) and each player's latest input (to guess the others).

   More bandwidth than lockstep's inputs -- the world, not the keys --
   which is why real games send only what changed since a snapshot the
   client acknowledged (delta compression, Quake 3's, 1999): an
   exercise. No desyncs possible, though: there is one game, the
   server's.

   The world travels as bytes the game chooses; the playground's
   Multiplayer sends Marshal's, which only works between copies of the
   same program, and which a program must never read from a stranger
   (Marshal's reader can be crashed by bytes made for it): fine through
   Sim_net, and a codec of the game's own over Wire.mli for a real
   network.

   The messages, in Wire's bytes:

     01 player first count input*        client -> server: my inputs
                                         first, first+1, ... (unacked)
     02 tick acked n input*n state       server -> client: the world
                                         after [tick], your input
                                         [acked] applied (zigzag: -1
                                         none yet), each player's latest

   Worked example (checked by the tests, Sim_net as the network, a tiny
   game): two clients, 600 ticks, 50 ms of latency and 10% loss: the
   server applies every input of each client, once, in order, whatever
   was lost on the way; the clients receive the world 20 times a
   second.

   References: John Carmack, Quake (1996) and QuakeWorld (1996); Yahn
   Bernier, "Latency Compensating Methods in Client/Server In-game
   Protocol Design and Optimization" (GDC 2001, Valve); Glenn Fiedler,
   "Snapshot Interpolation" and "Snapshot Compression"
   (gafferongames.com, 2014). *)

type state = {
  tick : int; (* the world after this server tick *)
  acked : int; (* the last of my inputs applied, -1 if none yet *)
  latest : string array; (* each player's latest input *)
  world : string; (* the world's bytes, the game's encoding *)
}

(* the server's side *)
module Server : sig
  type t

  val create : players:int -> t

  (* a client's packet of inputs; garbage dropped *)
  val receive : t -> string -> unit

  (* the inputs of the next tick, one per player: its next input not
   * yet applied, or its last one again *)
  val inputs : t -> string array

  (* the snapshot for [player], of the world after [tick] *)
  val packet : t -> tick:int -> world:string -> int -> string
end

(* a client's side *)
module Client : sig
  type t

  val create : me:int -> players:int -> t

  (* my input for this tick, and its number *)
  val record : t -> string -> int

  (* my inputs the server hasn't applied yet, as a packet *)
  val packet : t -> string

  (* a packet from the server: its snapshot, if newer than the last one
   * (an older one, overtaken on the way, is dropped); garbage dropped *)
  val receive : t -> string -> state option
end
