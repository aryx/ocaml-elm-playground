(* A car on a flat world, driven with a gas pedal and a wheel, sliding in
   the turns; and a track as a loop of waypoints, for laps, who leads,
   and the computer's driving.

   The world is a plane, the car a point on it with a heading, as seen
   from above -- whatever the game then draws: TinyMicroMachines
   draws it from above, TinyMarioKart from behind the car, in Mode 7.
   The same model, two pictures (Car.mli's lesson again, where one road
   is raced in pseudo-3D and in polygons).

   The car slides ([drive]): its speed follows the gas, but its velocity
   only turns towards where it points by a fraction each frame, the
   "grip"; a fast turn keeps going the old way a moment, it drifts:

         heading  ^                 velocity after one frame:
                  |   / velocity    grip of the way from the old
                  |  /              velocity to heading * speed
                  | /
                  car

   Not physics either (Physics.mli would be tires and friction forces):
   the arcade's few rules, like Car's.

   Part of the racing kit (gamekits/racing/, see Road.mli). *)

open Playground

(*****************************************************************************)
(* {1 The car} *)
(*****************************************************************************)

type t = {
  x : number;
  y : number;
  vx : number; (* the velocity, world units per second *)
  vy : number;
  heading : number; (* degrees, counterclockwise from +x *)
  speed : number; (* along the heading, what the gas controls *)
  next : int; (* the waypoint it drives to, counted from the start: laps included *)
}

(* How it feels, in world units, degrees and seconds. *)
type params = {
  accel : number; (* the speed full gas adds per second *)
  friction : number; (* the fraction of its speed lost per second *)
  grip : number; (* the fraction of the way the velocity turns to the heading, per frame *)
  steering : number; (* degrees per frame, the wheel full over *)
  steering_speed : number; (* the speed from which the wheel turns fully *)
}

(* TinyMicroMachines' toy cars: 900 per second of gas, 1.5 of friction,
 * a grip of 0.12, 3.5 degrees a frame from a speed of 250 *)
val toy : params

(* [drive params top gas steer car]: one frame (1/60 s) later, [gas]
 * from -1 (reverse) to 1, [steer] from -1 (right) to 1 (left), [top]
 * the top speed where the car is (the road's, the grass's: the game
 * knows which); reversing at a third of it. The wheel turns the car
 * only as much as it's moving, as a real car's. E.g. with [toy], full
 * gas from a stop on a top of 700: a speed of 14.625 (15 added, then
 * 1.5 / 60 of it lost), then 28.88. [next] doesn't change: see
 * [follow]. *)
val drive : params -> number -> number -> number -> t -> t

(*****************************************************************************)
(* {1 Hitting things} *)
(*****************************************************************************)
(* A track with walls (Super Sprint's, Super Off Road's fences) and cars
   that bump into each other: not rigid bodies (physics/2d would turn the
   car too, by where it was hit), but the arcade's two rules, each a few
   lines. *)

(* [bounce wall before after]: the car moved from [before] to [after]
 * (one [drive]); [wall x y]: whether (x, y) is inside a wall. The move
 * is tried across first, then up and down, from where the first try
 * left it; each one that would go into a wall is undone, and that part
 * of the velocity turned back at half its speed; the car's own speed
 * halved too (the gas pushing it back into the wall otherwise, full
 * speed at once). A car already in a wall ([push] can put it there)
 * moves freely, to get out:

         wall                 e.g. a wall from x = 100: from (95, 0)
          |                   to (105, 0) at vx = 300: back at
     --->*|     then  <-*|    (95, 0), vx = -150
          |                   *)
val bounce : (number -> number -> bool) -> t -> t -> t

(* [push radius a b]: two cars, each a disc of [radius]; overlapping,
 * they're moved apart along the line between them, each half the
 * overlap, and swap the parts of their velocities along that line (two
 * billiard balls of the same weight: the one hitting stops, the one hit
 * goes). E.g. a radius of 10, [a] at (0, 0) going 100 to the right and
 * [b] still at (10, 0): [a] at (-5, 0) and still, [b] at (15, 0) going
 * 100. *)
val push : number -> t -> t -> t * t

(*****************************************************************************)
(* {1 The track} *)
(*****************************************************************************)

(* a loop of waypoints, the track's center line; [reach]: how near one
 * counts as passed; [corner]: from how far the computer starts cutting
 * the corner to the next one *)
type track = { points : (number * number) array; reach : number; corner : number }

(* [point track i]: waypoint [i], counted from the start, laps included
 * ([i] modulo the number of waypoints) *)
val point : track -> int -> number * number

(* [start track i side]: a car still on waypoint [i], facing the next
 * one, [side] to its left (negative: to its right); driving to [i + 1] *)
val start : track -> int -> number -> t

(* [follow track car]: [next] one further once the car is within
 * [reach] of it *)
val follow : track -> t -> t

(* [lap track car]: which lap it's on, the first 0 *)
val lap : track -> t -> int

(* [distance track x y]: from (x, y) to the nearest point of the center
 * line (of any of its segments, the closing one included). A road
 * [width] wide is then where [distance < width / 2], anything further a
 * wall or the grass: the track is only its waypoints, no map to draw,
 * and a corner is round on the outside (the points at the same distance
 * from a waypoint, a circle):

         .-------------.        the center line *---*, and the road
        ( *-----------* )       around it; e.g. waypoints (0, 0),
         '--------.   |         (100, 0), (100, 100): (50, 10) is 10
                  |   |         from it, (110, -10) hypot(10, 10)
                  | * |         from (100, 0), a corner's outside *)
val distance : track -> number -> number -> number

(* [distance_from track i n x y]: the same, to the [n] segments from
 * waypoint [i] on (counted from the start, laps included, as [point]):
 * a car's walls measured only around where it is, for a track that
 * crosses itself (Super Sprint's bridges, TinySuperSprint.ml): at the
 * crossing, the other road is not its road, it goes straight on *)
val distance_from : track -> int -> int -> number -> number -> number

(* [ribbon color width track]: the road [width] wide along the center
 * line, a rectangle per segment and a disc on each waypoint, which
 * fills the gap on a corner's outside -- the points [distance] says
 * are on it. Drawn twice, the second narrower in another color, it
 * has its borders (the white lines of Gran Trak's track, the kerbs). *)
val ribbon : color -> number -> track -> shape

(* [progress track car]: how far along the track, to know who leads:
 * waypoints passed, then the distance to the next one (so waypoints
 * less than 10000 apart) *)
val progress : track -> t -> number

(*****************************************************************************)
(* {1 The computer} *)
(*****************************************************************************)

(* [computer track car]: the (gas, steer) the computer drives with:
 * steering towards the next waypoint, and, once within [corner] of it,
 * towards the one after too, to cut the corner (not before: aiming at
 * a mix of the two from the start of a long straight would take it off
 * the road); full gas, unless the turn ahead is sharp and it's faster
 * than [corner] per second (it would be on the corner within a second):
 * then it brakes. *)
val computer : track -> t -> number * number
