(* Juice: what makes a game feel alive, beyond what it does.

   Two Breakouts with the same rules, the same levels and the same
   score can feel nothing alike: in one the bricks just vanish; in the
   other they pop in when the level starts, flash when hit, and burst
   into pieces while the screen shakes. Martin Jonasson and Petri Purho
   showed exactly that, one effect at a time, in "Juice it or lose it"
   (GDC Europe 2012), and the name stuck. None of it changes the game
   -- turn it all off and every rule is the same -- which is why it
   can be switched off: run a game with the flag juice=off and every
   effect of this module does nothing.

   The first kind of juice is the simplest: a number that does not jump
   but *eases* from one value to another, the way things that weigh
   something start and stop:

     (* in the model: when the brick appeared *)
     let size = Juice.tween Juice.out_back 0. 1. 0.3 brick.born computer in
     rectangle red 60. 20. |> scale size

   and the brick grows from nothing in 0.3 s, overshoots by 10% and
   settles. The model keeps only *when* it started; the rest is a
   function of the time, like [wave] and [spin].

   Underneath is juice/ (see docs/claude_notes/tutorials/notes_juice.md):
   Ease.mli, the curves and why [out] is [in] run backwards, and
   Tween.mli, the tween as a function of its start time. *)

open Playground

(*****************************************************************************)
(* {1 Effects as functions of time} *)
(*****************************************************************************)
(* Nothing in the model but when it started; called in view, like wave. *)

(* How a number goes from one value to another: fast or slow at the
 * start, at the end, overshooting or bouncing. Robert Penner's curves
 * (2002), named as everyone names them: [in_] starts slow, [out_] ends
 * slow, [in_out_] both. *)
type ease

val linear : ease

val in_quad : ease
val out_quad : ease
val in_out_quad : ease

val in_cubic : ease
val out_cubic : ease
val in_out_cubic : ease

val in_sine : ease
val out_sine : ease
val in_out_sine : ease

(* back: goes a little the wrong way first ([in_back]) or too far at the
 * end ([out_back], 10% too far) *)
val in_back : ease
val out_back : ease
val in_out_back : ease

(* elastic: wobbles like a spring let go *)
val in_elastic : ease
val out_elastic : ease
val in_out_elastic : ease

(* bounce: lands like a dropped ball *)
val in_bounce : ease
val out_bounce : ease
val in_out_bounce : ease

(* [curve ease t]: the curve itself, how far along at [t] of the time
 * (0 at 0, 1 at 1), to draw it *)
val curve : ease -> number -> number

(* [tween ease from to seconds started computer]: [from] until
 * [started], then going to [to] along [ease] for [seconds], then [to].
 * With the flag juice=off, [to] at once. *)
val tween : ease -> number -> number -> number -> time -> computer -> number
