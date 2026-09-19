(* A car driven on a Road: how fast it goes, and where across the road.

   Not physics (compare the Physics layer, playground/Physics.mli): an
   arcade car is a few rules that feel right, the ones of Jake Gordon's
   racer, from Out Run's era:
     - up accelerates, down brakes, nothing lets it slow down by itself;
     - steering moves it across the road, faster at speed;
     - a curve pushes it outwards, more at speed: the "centrifugal force"
       you must steer against (in a curve right, the car drifts left);
     - off the road, on the grass, it slows down to a crawl.
   Its position is along the track and across it, not in space: the same
   car drives in a pseudo-3D game and a polygon one, the games drawing it
   their way.

   Part of the racing kit (kits/racing/, see Road.mli).
*)

open Playground

type t = {
  position : number; (* along the track, from the start, in world units *)
  x : number; (* across the road: -1 its left edge, 0 its center, 1 its right *)
  speed : number; (* world units per second *)
  steer : number; (* -1 steering left, 0 straight, 1 right: to draw the car *)
}

(* at the start, still, in the middle of the road *)
val start : t

(* The car's feel, in world units and seconds. *)
type params = {
  max_speed : number;
  accel : number;
  brake : number; (* negative *)
  decel : number; (* negative: slowing down by itself *)
  off_road_decel : number; (* negative *)
  off_road_limit : number; (* the speed the grass slows it down to *)
  centrifugal : number; (* how hard curves push it outwards *)
}

(* [params segment_length]: Jake Gordon's settings: a top speed of one
 * segment per frame (at 60 frames per second), full speed from a stop
 * in 5 seconds, stopped by the brakes in 1, the grass halving the top
 * speed's worth of speed every second until a quarter of it *)
val params : number -> params

(* [drive params road keyboard car]: the car one frame (1/60 s) later,
 * driven with the arrows. [position] isn't wrapped at the end of the
 * track: a lap game takes it modulo Road.length, a stage game stops. *)
val drive : params -> Road.t -> keyboard -> t -> t
