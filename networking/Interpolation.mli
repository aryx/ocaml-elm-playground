(* Interpolation: the others, shown a little in the past, smoothly.

   Snapshots (Snapshot.mli) come 20 times a second, not 60, and not
   evenly: the network adds jitter, and loses some. Another player drawn
   at its latest snapshot moves in jumps, a frame of 50 ms, then a
   frame of 100 when one was lost. The fix, *entity interpolation*: draw
   the others where they were a little in the past -- [delay] behind the
   newest snapshot, 100 ms, two snapshots' worth -- between the two
   snapshots around that moment, a fraction of the way from one to the
   next:

      snapshots:   A          B          C          (at 0, 50, 100 ms)
      now = 125:                    ^ 125 - 100 = 25 ms: halfway A -> B

   Always a step behind, and always smooth: as long as one snapshot of
   the two arrives within the delay, the other player never stops. The
   cost is honesty: what you see of the others is 100 ms old, so aiming
   at them means aiming at the past -- the reason for *lag
   compensation*, the server rewinding the world to what the shooter
   saw before deciding a hit (Source, 2001): an exercise.

   My own player isn't interpolated but predicted (Prediction.mli):
   the present for me, the past for the others -- every online shooter's
   compromise.

   Worked example (checked by the tests): snapshots at 0, 50 and 100 ms,
   a delay of 100 ms: at 125 ms the moment shown is 25 ms, halfway from
   the one at 0 to the one at 50; before the second snapshot, the first
   alone; past the newest, the newest (never guessing ahead: that would
   be extrapolation, dead reckoning, what DIS and SIMNET did in the
   1980s, and a second exercise).

   References: Yahn Bernier (Valve, 2001), for Half-Life's
   interpolation; Gabriel Gambetta, "Entity Interpolation" (2014). *)

type 'a t

(* a buffer drawing [delay] seconds behind the newest snapshot *)
val create : delay:float -> 'a t

(* a snapshot received at [time] *)
val add : 'a t -> time:float -> 'a -> unit

(* at [now]: the two snapshots around [now - delay], and how far from
 * the first to the second (0 to 1); None before any *)
val sample : 'a t -> now:float -> ('a * 'a * float) option
