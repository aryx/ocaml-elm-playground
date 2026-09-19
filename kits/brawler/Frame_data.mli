(* Frame_data: a move, frame by frame.

   A punch isn't instant: the arm draws back (startup), the fist is out
   and can hit (active), the arm comes back (recovery), and only then
   can the fighter do something else. Counted in frames, 1/60 s: a jab
   is 3 + 2 + 6, a heavy kick 8 + 4 + 18. A hit also stops the other in
   hitstun (on the block, blockstun) for a while. Fighting games are
   played in these numbers: the faster startup wins a race, and the
   "frame advantage" says who moves first after a hit -- the one whose
   stun outlasts the other's recovery.

       frames  1 2 3 | 4 5 | 6 7 8 9 10 11 | 12
               startup active  recovery     over

   Street Fighter II (Capcom, 1991) made it a science: its players
   discovered that some moves recover fast enough to link into another
   before the other recovers -- the combos, an accident the designers
   kept. The numbers of every character's moves ("frame data") are
   published, and learned.

   Part of the brawler kit (kits/brawler/), with Hitbox.mli and
   Stickman.mli. *)

type move = {
  startup : int; (* frames before it can hit *)
  active : int; (* frames it can hit *)
  recovery : int; (* frames after *)
  damage : int;
  hitstun : int; (* frames the other can't act, hit *)
  blockstun : int; (* the same, blocking *)
  hitbox : Hitbox.box; (* where it hits, while active *)
}

type phase = Startup | Active | Recovery | Over

(* startup + active + recovery *)
val length : move -> int

(* [phase m frame]: where the move is at its [frame], from 1. E.g. the
 * jab above: frames 1 to 3 startup, 4 and 5 active, 6 to 11 recovery,
 * 12 over. *)
val phase : move -> int -> phase

(* [advantage_on_hit m]: hitting on its first active frame, how many
 * frames the attacker can act before the other: the hitstun minus what's
 * left of the move (its other active frames, and its recovery). E.g. the
 * jab, hitstun 12: 12 - (1 + 6) = +5, time for another jab (startup 3)
 * to hit before the other can block: a combo. [advantage_on_block] the
 * same with the blockstun, 8: +1. *)
val advantage_on_hit : move -> int
val advantage_on_block : move -> int
