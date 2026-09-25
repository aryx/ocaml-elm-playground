(* Juice3d: Juice.mli, for a game in 3D.

   The same idea: effects that change how a game feels, never what it
   does, all gone with the flag juice=off. Most of Juice works in 3D
   as it is -- a tween, a follower, a freeze, a flash -- and Juice3d
   keeps them, on the same clock ([juice]). Two things do not: a 2D
   game shakes by moving the picture, and throws particles in the
   picture's plane. In 3D, the camera shakes, and the particles fly in
   the world, falling to its ground, hidden behind its walls.

   One value in the model, stepped in update, as with Juice:

     type model = { ...; fx : Juice3d.t }

     let update computer m = { m with fx = Juice3d.step computer m.fx; ... }

     (* when something is hit *)
     let fx = m.fx |> Juice3d.shake 0.4 |> Juice3d.burst ~at:(x, 1., z) Juice3d.sparks in

     (* view *)
     (Juice3d.camera m.fx cam, Juice3d.view m.fx world)

   (games/fps/TinyCyberSled.ml, with and without.) *)

open Playground
open Playground3d

(*****************************************************************************)
(* {1 The effects' clock} *)
(*****************************************************************************)

(* the effects' clock, and the effects under way *)
type t

(* [none ~seed]: the clock at 0, no effect yet; [seed] makes the shake's
 * noise and the particles' ways (the same seed, the same effects) *)
val none : seed:int -> t

(* [step computer fx]: one frame later (1/60 s); with juice=off, every
 * effect gone, as Juice.step *)
val step : computer -> t -> t

(* [now fx]: the effects' clock, to remember when something happened *)
val now : t -> time

(* [juice fx]: the 2D effects on the same clock, for the effects that
 * need no third dimension: Juice.tween, Juice.during, Juice.squash,
 * Juice.toward (a follower) *)
val juice : t -> Juice.t

(* [on fx]: whether the juice is on (not juice=off) *)
val on : t -> bool

(*****************************************************************************)
(* {1 Effects that last} *)
(*****************************************************************************)

(* [shake trauma fx]: add trauma, 0 to 1, as Juice.shake: the camera
 * shakes by trauma squared, trauma falling by 1 a second. The camera,
 * not the world: its eye and its target moved together, across and
 * up, and a little roll. *)
val shake : number -> t -> t

(* [freeze frames fx]: hitstop, as Juice.freeze; see [frozen] *)
val freeze : int -> t -> t

(* [frozen fx]: whether the game should skip its own update this frame *)
val frozen : t -> bool

(* [flash color frames fx]: the whole view tinted [color], fading out,
 * as Juice.flash *)
val flash : color -> int -> t -> t

(* Particles in the world: small cubes flying off from a point,
 * tumbling, falling, shrinking away, bouncing once off the ground
 * (y = 0). They are part of the scene: a wall hides them. *)
type burst

(* white, yellow and orange, fast, all around, gone in half a second: a
 * hit, a spark off metal *)
val sparks : burst

(* gray puffs, slow, rising and growing: a missile's trail, a wreck
 * burning *)
val smoke : burst

(* pieces of [color], thrown up and falling, tumbling: a tank blown up *)
val debris : color -> burst

(* drops of [color], many and small, thrown up and falling back soon:
 * a fruit cut, a splash of water *)
val drops : color -> burst

(* [burst ~at b fx]: a burst at the point [at] (at most 300 particles
 * in all, the oldest dropped first); nothing with juice=off *)
val burst : at:number * number * number -> burst -> t -> t

(*****************************************************************************)
(* {1 Drawing} *)
(*****************************************************************************)

(* [camera fx cam]: [cam], shaken *)
val camera : t -> camera -> camera

(* [view fx world]: the world, the particles, and the flash over it (a
 * HUD shape, over the whole view) *)
val view : t -> shape3d list -> shape3d list
