(* A Topdown car on ground that has a height: slower up a slope, faster
   down it, flying over a crest, stopped by ground too steep to climb.

   Topdown.mli's car lives on a plane. Off-road racers put it on hills
   (Super Off Road's stadium, Big Red Racing's landscapes, Ignition's
   ramps), and the car then needs one more number, its height, and
   three rules:

   - the slope ([slope]): gravity along the ground, the part of it along
     the car taken off its speed, the part across pushing it sideways (a
     car on a side slope slides down it);
   - the takeoff: the car follows the ground only as long as the ground
     doesn't fall away faster than gravity would pull the car down.
     Where the car would be one frame on if it were flying is compared
     with the ground there:

              ___         flying, one frame on,       .  <- the car, off:
          ___/   \        above the ground ahead:     .     its height, and
      ___/        \___    off it goes                        its vertical speed

     a crest taken slowly, the ground drops a little and the car stays
     on it; fast, the ground drops more than gravity can follow in a
     frame, and the car flies: no wheel, no gas, until it lands (a hard
     landing costs speed). A lower gravity (the Moon's) sends it flying
     off crests it would have stayed on. It is the physics' rule, a
     frame at a time: a crest curving by [k] (the slope changing by [k]
     per unit) throws a car at a speed [v] when v * v * k > gravity. The
     comparison keeps a margin of a hundredth of a unit, so that the
     corners between a heightmap's cells don't make the car hop (at 60
     frames a second, 36 more than gravity);
   - the walls: ground higher than the car can climb in a frame, or
     steeper than it can climb at all, uphill, bounces it back
     (Topdown.bounce), so a canyon's walls, a cliff or a house raised in
     the ground need nothing else; and the edge of the world stops it.

   The ground is a function, not a grid: a game gives its heightmap's
   [height] (gamekits/heightmap), or its road's profile where the road
   is, or both mixed. The car is still a point: its pitch and roll
   ([pose]) are only drawn.

   Part of the racing kit (gamekits/racing/, see Road.mli). Used by
   TinyIgnition. *)

open Playground

type ground = {
  (* the height at (x, y) *)
  height : number -> number -> number;
  (* world units per second squared, downwards *)
  gravity : number;
  (* the square the car stays in, from (lo, lo) to (hi, hi) *)
  lo : number;
  hi : number;
}

type t = {
  body : Topdown.t;
  (* the height, and how fast it changes, per second *)
  h : number;
  vh : number;
  air : bool;
}

(* [start ground body]: on the ground, still *)
val start : ground -> Topdown.t -> t

(* [slope ground x y]: how much the ground rises per unit, along x and
 * along y, e.g. (0.5, 0.) on a ramp rising towards +x by 1 in 2 *)
val slope : ground -> number -> number -> number * number

(* [drive ground track params top gas steer car]: one frame (1/60 s)
 * later. On the ground: Topdown.drive with [params], [top], [gas] and
 * [steer], then the slope's pull, the walls, and either still on the
 * ground or off it. In the air: carried by its velocity, falling. Then
 * Topdown.follow, for its next waypoint. E.g. on a flat ground a car
 * drives exactly as Topdown's; up a ramp rising 1 in 3 it climbs at a
 * third of its speed, and over the top onto a flat ground it flies,
 * rising at that speed (less a frame of gravity). *)
val drive : ground -> Topdown.track -> Topdown.params -> number -> number -> number -> t -> t

(* [push radius a b]: Topdown.push, for two cars at about the same
 * height (within 2): one flying over another doesn't touch it *)
val push : number -> t -> t -> t * t

(* [pose ground car]: (pitch, roll), in degrees, for drawing: on the
 * ground, the slope along the car (nose up positive) and across it
 * (the right side up positive); in the air, the nose up as it rises
 * and down as it falls, not rolled *)
val pose : ground -> t -> number * number
