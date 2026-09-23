(* Rollback: guess the others' keys, and fix it afterwards.

   Lockstep's input delay (Lockstep.mli) is what a fighting game can't
   have: 50 ms between your thumb and your fighter is a different game.
   Rollback (GGPO, Tony Cannon, 2006) applies your input *at once*, and
   *predicts* the others': they are still doing what they did last tick
   -- right most of the time, since keys stay held for many ticks. When
   a real input arrives and contradicts the guess, the peer goes back
   and plays the ticks again, with the truth:

     tick:          10    11    12    13     B's real input for 11
     A computed:    m10   m11'  m12'  m13'   arrives at tick 13, and
                           |                 differs from the guess
     A redoes:             m11   m12   m13   m10 restored, 11 to 13
                                             replayed -- in one frame

   The costs move from *delay* to *CPU and correctness*: up to a round
   trip's worth of ticks replayed every frame, and the other player
   visibly *snapping* when a guess was wrong -- which players forgive
   far more than lag on their own hands.

   Saving the models to go back to is the part that is hard in C++
   (GGPO asks the game to serialize its whole state, every frame) and
   free here: Elm's models are immutable values, so saving one is
   keeping it, in a list, with its unchanged parts shared. The whole
   technique is then the bookkeeping below:

   - [saved]: for each tick not yet *confirmed*, the model before it and
     the inputs it was played with (some guessed);
   - each frame, the earliest saved tick whose guesses now differ from
     real inputs is the one to go back to: its model restored, every
     tick since replayed (the guesses refreshed with what is known now);
   - a tick is confirmed once all its inputs are real (and it was played
     with them): only then is its model final, the same on every peer,
     and handed to [on_confirm] -- for the checksums of desync detection
     (a guessed model would differ between peers, and mean nothing);
   - at most [max_ahead] ticks guessed in a row (8, GGPO's usual cap):
     beyond, the peer stalls as lockstep would, so that a peer that has
     heard nothing for a second doesn't replay a second of game every
     frame.

   The inputs travel as Inputs.mli says, exactly as lockstep's.

   Worked example (checked by the tests, Sim_net as the network): two
   and three peers, 1,000 ticks, under latency, jitter, 10% loss and
   duplication: every confirmed model is the one of the same inputs
   applied on one machine -- the guesses never show in the end. At 100
   ms, lockstep (a delay of 3) plays 600 ticks in some 1,230 frames,
   half speed; rollback in 606, paying in replays instead: the other
   player's keys change 30 times, so 30 wrong guesses, each found a
   network trip late (6 frames), 183 ticks replayed, 7 at most at once.

   References: Tony Cannon, GGPO (2006; open-sourced, MIT, 2019), and
   his "Fight the Lag!" (Game Developer, 2012); David Jefferson, "Virtual
   Time" (ACM TOPLAS, 1985) -- Time Warp, the same idea for parallel
   simulations, twenty years before. *)

type 'model t

(* peer [me] of [players]: [update inputs model] is one tick (inputs by
 * player); [on_confirm tick model] is called once per tick, in order,
 * when its model is final. [delay] (0) is an input delay added to the
 * prediction, as some games do (1 or 2 ticks, fewer rollbacks) *)
val create :
  me:int ->
  players:int ->
  ?delay:int ->
  ?max_ahead:int ->
  update:(string array -> 'model -> 'model) ->
  on_confirm:(int -> 'model -> unit) ->
  'model ->
  'model t

(* one frame: my input read now; the rollback if a guess was wrong; the
 * next tick played (with guesses if needed) -- unless [max_ahead]
 * ticks are guessed already: a stall *)
val step : 'model t -> string -> unit

(* the rollback if a guess was wrong, and the confirmations, without
 * playing a new tick (a game that has ended, the tests) *)
val settle : 'model t -> unit

(* the model to show: the latest, maybe on guesses *)
val model : 'model t -> 'model

(* the next tick to play; the first not yet confirmed *)
val tick : 'model t -> int
val confirmed : 'model t -> int

val packet : 'model t -> string
val receive : 'model t -> string -> unit

(* my model's checksum after a confirmed tick (in on_confirm) *)
val checksum : 'model t -> tick:int -> int32 -> unit
val desync : 'model t -> (int * int) option

type stats = {
  stalls : int;
  rollbacks : int; (* times a guess was wrong *)
  replayed : int; (* ticks played again because of them *)
  deepest : int; (* the most ticks replayed at once *)
  dropped : int;
}

val stats : 'model t -> stats
