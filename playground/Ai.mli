(* Game AI for the playground: what an enemy wants, in one line.

   This first part is steering: characters that move by wanting to go
   somewhere, as verbs on a [Physics.body] next to [fall] and [push].
   Each adds the steering force of one behaviour to what pushes the
   body, so they combine with each other and with the rest of the
   physics, and [Physics.step] comes last:

     fish |> flocking school |> avoiding rocks |> Physics.step

   A fish is one line of [update]:

     let update _ school = List.map (fun f -> f |> flocking school |> Physics.step) school

   Every verb takes the body's top speed, [speed] (200 pixels per
   second by default), and how hard it may turn, [force] (400 pixels per
   second, per second): a heavy barge and a darting fly are the same
   behaviours with different numbers. Underneath is ai/Steering.mli
   (Craig Reynolds, 1999) and ai/Flock.mli (Reynolds, 1987), where the
   forces are written out. *)

open Playground

(* [seek x y b]: [b] steered straight at (x, y), at top speed (and
 * overshooting it, like a homing missile: see [arrive]) *)
val seek : ?speed:number -> ?force:number -> number -> number -> Physics.body -> Physics.body

(* [flee x y b]: straight away from (x, y) *)
val flee : ?speed:number -> ?force:number -> number -> number -> Physics.body -> Physics.body

(* [arrive x y b]: to (x, y), slowing down within [slowing] (100
 * pixels) of it, and stopping there *)
val arrive : ?speed:number -> ?force:number -> ?slowing:number -> number -> number -> Physics.body -> Physics.body

(* [chase target b]: towards where [target] will be when [b] gets there *)
val chase : ?speed:number -> ?force:number -> Physics.body -> Physics.body -> Physics.body

(* [escaping target b]: away from where [target] will be *)
val escaping : ?speed:number -> ?force:number -> Physics.body -> Physics.body -> Physics.body

(* [wandering time b]: an idle stroll, curving one way then the other.
 * [time] drives it (computer.time, say, plus something different for
 * each body so that they don't all turn together): a point on a circle
 * ahead of [b] drifts with it, smoothly, and [b] follows the point *)
val wandering : ?speed:number -> ?force:number -> number -> Physics.body -> Physics.body

(* [avoiding rocks b]: turned away from whichever of the circles (x, y,
 * radius) is in front of it, and not otherwise *)
val avoiding : ?speed:number -> ?force:number -> (number * number * number) list -> Physics.body -> Physics.body

(* [flocking others b]: Reynolds's three rules among [others] within
 * [radius] (100 pixels) -- [others] may be the whole flock, [b]
 * included: away from the ones too close, the way the neighbours go,
 * towards where they are, weighted [separation] (1.5), [alignment] (1),
 * [cohesion] (1); 0 turns one off *)
val flocking :
  ?speed:number ->
  ?force:number ->
  ?radius:number ->
  ?separation:number ->
  ?alignment:number ->
  ?cohesion:number ->
  Physics.body list ->
  Physics.body ->
  Physics.body

(* [following path b]: along the polyline [path], kept within [width]
 * (20 pixels) of it, like a car keeping to its road *)
val following : ?speed:number -> ?force:number -> ?width:number -> (number * number) list -> Physics.body -> Physics.body

(* [facing b]: [b] pointing the way it goes (its angle, for [draw]):
 * a fish nose first *)
val facing : Physics.body -> Physics.body
