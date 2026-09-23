(* Prediction: my keys at once, the server's word when it comes.

   With a server owning the game (Snapshot.mli), a client that only
   drew what it was sent would feel every key a round trip late: press
   left, wait 100 ms, see the ship turn. QuakeWorld's answer (1996),
   *client-side prediction*: the client keeps a copy of the world and
   plays its own inputs on it at once, as the server will -- the same
   update, the same inputs -- guessing the other players' from their
   latest (they are probably still doing it).

   The server's snapshot then says what really happened, up to my input
   [acked]. *Reconciliation*: take the server's world, and play again,
   on it, my inputs it hasn't applied yet -- rollback (Rollback.mli)
   again, one-sided: only my inputs are replayed, and the server is
   never wrong.

     my inputs:     5    6    7    8    9          (9: this frame)
     snapshot:      the world after my 6, from the server
     replayed:                7    8    9          on the server's world

   When the guess was right -- the others did what they did, the world
   had nothing I didn't know -- the replay lands exactly where the
   prediction was, and nothing shows. When it was wrong, the world
   *snaps* to the corrected one: a misprediction, counted here, since
   the models before and after are compared (what the server says for
   my input 6, against what I had predicted after 6).

   Worked example (checked by the tests, Sim_net, a tiny game): alone,
   whatever the latency, the prediction is never wrong -- my input is the
   only one there is; with a second player whose keys change, some
   predictions are wrong, and each is corrected by the next snapshot:
   at the end, the client's world is the server's. Over 600 ticks at
   50 ms, the other's keys changing 30 times: 27 and 23 mispredictions
   of some 200 snapshots, about one a change -- the guess "still doing
   it" wrong exactly when a key changes.

   References: John Carmack's QuakeWorld (1996), and his notes of the
   time; Gabriel Gambetta, "Client-Side Prediction and Server
   Reconciliation" (2014), whose demo shows exactly this. *)

type 'model t

(* my prediction as player [me]; [update inputs model] is one tick,
 * everyone's input *)
val create : me:int -> players:int -> update:(string array -> 'model -> 'model) -> 'model -> 'model t

(* my input number [seq], played at once, the others guessed *)
val step : 'model t -> seq:int -> string -> unit

(* the server's word: its world after my input [acked], each player's
 * latest input; my inputs after [acked] played again on it *)
val correct : 'model t -> world:'model -> acked:int -> latest:string array -> unit

(* the world to show: the server's, and mine played on it *)
val model : 'model t -> 'model

(* the snapshots whose world differed from the prediction *)
val mispredictions : 'model t -> int
