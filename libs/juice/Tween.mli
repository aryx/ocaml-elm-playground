(* A tween: a value going from one number to another over a given time,
   along a curve (Ease.mli), as a function of when it started.

   The word is the animators' "in-between": the master drew the key
   poses, and the in-betweeners drew the frames between them. Here the
   key poses are two numbers, and the in-between is computed:

      value
      100 |                  .------------   the end, held
          |              .-'
          |           .-'     out_quad, 0 to 100 over 2 s,
          |         /         started at 1 s
          |        /
        0 |-------'           the start, held before it begins
          +-------+-------+-------+-------> now (s)
          0       1       2       3

   Two ways to write one. Flash's (ActionScript, 2002, and most engines
   since): a tween is an object, registered once, that changes a
   variable a bit every frame until it is done -- state that lives
   outside the program's own, and that must be stopped or it keeps
   running. Elm's (elm-community/easing-functions): a tween is only a
   formula of the time now, and all the program keeps is *when it
   started*. The value is recomputed each frame from that, so there is
   nothing to step, nothing to stop, and a replay or a rewind shows the
   same frame at the same time. This module is Elm's way, which is the
   only one a Model-View-Update program can have without cheating: its
   model can hold a start time, not a running object.

   It is three small steps, each a function below:

     progress   now -> how far through the time, clamped to [0, 1]
     curve      that fraction through an Ease.t
     lerp       the result from one number to the other

   Worked example (the figure, checked by the tests): out_quad from 0
   to 100 over 2 s, started at 1 s. At 0.5 s it has not started: 0. At
   2 s, halfway through the time, out_quad 0.5 = 0.75: 75, already three
   quarters of the way. At 3.5 s it is over: 100, held.

   Honest about scale: a tween knows its end from the start. When the
   end moves while it plays (a camera following the player), use a
   spring, stepped each frame. *)

(* [lerp a b p]: [a] at 0, [b] at 1, a straight line in between (and
 * beyond: not clamped) *)
val lerp : float -> float -> float -> float

(* [progress ~start ~duration now]: how far through, 0 before [start],
 * 1 from [start + duration] on; 1 at once when [duration] <= 0 *)
val progress : start:float -> duration:float -> float -> float

(* [value curve a b ~start ~duration now]: from [a] to [b] along
 * [curve] *)
val value : Ease.t -> float -> float -> start:float -> duration:float -> float -> float

(* [finished ~start ~duration now]: whether it has reached its end *)
val finished : start:float -> duration:float -> float -> bool
