(* Juice: what makes a game feel alive, beyond what it does.

   Two Breakouts with the same rules, the same levels and the same
   score can feel nothing alike: in one the bricks just vanish; in the
   other they pop in when the level starts, the ball squashes against
   the paddle, and the screen shakes when a brick breaks. Martin
   Jonasson and Petri Purho showed exactly that, on a Breakout, one
   effect at a time, in "Juice it or lose it" (GDC Europe 2012), and the
   name stuck. None of it changes the game -- turn it all off and every
   rule is the same -- which is why it can be switched off: run a game
   with the flag juice=off and every effect of this module does nothing
   (games/arcade/TinyBreakout.ml, with and without).

   One value in the model holds the effects' own clock and the effects
   under way, stepped in update:

     type model = { ...; fx : Juice.t }

     let update computer m = { m with fx = Juice.step computer m.fx; ... }

   The simplest juice is then a number that does not jump but *eases*
   from one value to another, the way things that weigh something start
   and stop:

     (* in the model: when the brick appeared, Juice.now m.fx then *)
     let size = Juice.tween Juice.out_back 0. 1. 0.3 brick.born m.fx in
     rectangle red 60. 20. |> scale size

   and the brick grows from nothing in 0.3 s, overshoots by 10% and
   settles. The model keeps only *when* it started; the rest is a
   function of the effects' clock.

   Underneath is juice/ (see docs/claude_notes/tutorials/notes_juice.md):
   Ease.mli, the curves and why [out] is [in] run backwards,
   Tween.mli, the tween as a function of its start time, Squash.mli,
   squash and stretch, Trauma.mli, screen shake, Emitter.mli,
   particles, and Hash.mli, the random numbers they share. *)

open Playground

(*****************************************************************************)
(* {1 The effects' clock} *)
(*****************************************************************************)
(* The effects run on their own clock, one frame a step, not on the
 * wall clock (computer.time): the same frames give the same effects,
 * in a replay, and in a golden frame test, whose clock is frozen. *)

(* the effects' clock, and the effects under way *)
type t

(* [none ~seed]: the clock at 0, no effect yet; [seed] makes the shake's
 * noise (the same seed, the same shake) *)
val none : seed:int -> t

(* [step computer fx]: one frame later (1/60 s): the clock moves on,
 * trauma falls, the freeze and the flash count down. With the flag
 * juice=off, every effect is gone, and stays so. *)
val step : computer -> t -> t

(* [now fx]: the effects' clock, to remember when something happened *)
val now : t -> time

(* A game that had juice written by hand before this module (hitstop
 * counters, sparks as a list of its own) keeps it, as the simple
 * version, and gets this module's as the other: the flag juice=hand or
 * juice=engine chooses, juice=off neither. [mode ~default flags]: which
 * one, [default] when the flag is absent or is none of the three --
 * [Hand] for such a game, whose original look stays its default. *)
type mode = Off | Hand | Engine

val mode : default:mode -> flags -> mode

(*****************************************************************************)
(* {1 Effects as functions of time} *)
(*****************************************************************************)
(* Nothing in the model but when it started; called in view. *)

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

(* [tween ease from to seconds started fx]: [from] until [started], then
 * going to [to] along [ease] for [seconds], then [to]. With juice=off,
 * [to] at once. *)
val tween : ease -> number -> number -> number -> time -> t -> number

(* [during seconds started fx]: whether less than [seconds] have passed
 * since [started]: a flash of a few frames; never with juice=off *)
val during : number -> time -> t -> bool

(* Squash and stretch: a thing that lands flattens, and springs back,
 * its area kept -- a ball that reads as rubber, not stone:
 *
 *     ball |> Juice.stretch (Juice.squash 0.4 0.5 landed m.fx)
 *
 * [squash amount seconds landed fx]: how to stretch it (across, up),
 * [amount] flatter at [landed] (0.4: 60% of its height, and as much
 * wider), back to (1, 1) after [seconds]; (1, 1) with juice=off. *)
val squash : number -> number -> time -> t -> number * number

(* [stretch (across, up) shape]: [shape] wider by [across] and taller by
 * [up], about the point (0, 0) of its frame: build a thing that lands
 * with its bottom at (0, 0), stretch it, then move it where it is, and
 * it squashes against the ground, not in the air. A circle becomes an
 * oval; a rotated rectangle, or anything inside a rotated group, the
 * polygon it becomes (ovals by 32 points). Words and images inside a
 * rotated group, and words anywhere, are only scaled evenly. *)
val stretch : number * number -> shape -> shape

(* [whiten shape]: the same shape, all white: the hit flash, drawn for
 * a frame or two when something is hit (images are left as they are) *)
val whiten : shape -> shape

(*****************************************************************************)
(* {1 Effects that last} *)
(*****************************************************************************)
(* The game says what happened, in update, and the effect plays out by
 * itself over the next frames:
 *
 *     let fx = if hit then m.fx |> Juice.shake 0.5 |> Juice.freeze 4 else m.fx in
 *     let fx = Juice.step computer fx in
 *     if Juice.frozen fx then { m with fx } else ... the game's own update ...
 *
 *     (* view *)
 *     Juice.view m.fx world *)

(* [shake trauma fx]: add trauma, 0 to 1: 0.3 a knock, 1 an explosion.
 * The screen shakes by trauma squared, and trauma falls by 1 a second
 * (juice/Trauma.mli), so a knock is felt for a moment, and a string of
 * knocks adds up. *)
val shake : number -> t -> t

(* [freeze frames fx]: hitstop: the game stands still for [frames]
 * frames (4 to 8 for a hit), while the shake and the flash go on.
 * A punch is felt more than seen; the pause says it connected. The
 * game does the standing still: see [frozen]. It is the one effect
 * that changes when things happen, not only how they look. *)
val freeze : int -> t -> t

(* [flash color frames fx]: the whole screen tinted [color], fading out
 * over [frames] frames: an explosion, a smart bomb *)
val flash : color -> int -> t -> t

(* Particles: a burst of small squares, each flying off, spinning,
 * falling and fading by itself (William Reeves, 1983; juice/Emitter.mli).
 * The same seed, the same bursts: a replay sees the same sparks. *)
type burst

(* white, yellow and orange, fast, all around, gone in half a second: a
 * hit, a spark off metal *)
val sparks : burst

(* gray puffs, slow, rising and fading: something burning, a landing in
 * dust *)
val smoke : burst

(* pieces of [color], thrown up and falling, tumbling: a brick broken,
 * a ship blown up *)
val debris : color -> burst

(* [burst ~at b fx]: a burst at the point [at] (at most 400 particles
 * in all, the oldest dropped first); nothing with juice=off *)
val burst : at:number * number -> burst -> t -> t

(* Followers: a number going after a target that moves -- a camera
 * after the player, a health bar draining, eyes following a ball --
 * like a mass on a spring (juice/Follow.mli). Where a tween knows its
 * end from the start, a follower is told where its target is, frame
 * after frame:
 *
 *     let look = Juice.toward ball.x m.fx m.look in    (* update *)
 *     ... Juice.value m.look ...                         (* view *)
 *)
type follow

(* [follow ?frequency ?damping x]: a follower at [x], still. [frequency]
 * (2 Hz by default): how fast it answers; [damping] (1 by default): 1
 * gets there as fast as it can without going past, less goes past and
 * swings back (0.5: 14% too far), more is sluggish. *)
val follow : ?frequency:number -> ?damping:number -> number -> follow

(* [toward target fx f]: one frame later, pulled towards [target]; at
 * [target] at once with juice=off *)
val toward : number -> t -> follow -> follow

(* [value f]: where it is *)
val value : follow -> number

(* [frozen fx]: whether the game should skip its own update this frame *)
val frozen : t -> bool

(* [on fx]: whether the juice is on (not juice=off): for juice a game
 * draws itself, to leave it out of the dry game *)
val on : t -> bool

(* [view fx world]: the world and the particles, shaken, and the flash
 * over it. Draw what must not shake (the background, the score)
 * outside it. *)
val view : t -> shape list -> shape list
