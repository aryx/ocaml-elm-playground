(* Path: the curves enemies fly along.

   In Galaga (Namco, 1981) the enemies don't march, they fly: in waves
   along swirling curves, then down at you in dives. A path is typed as
   a few points the enemy passes through, made smooth by a Catmull-Rom
   spline ([catmull_rom]): the curve goes through every point, and its
   direction at each point is the direction from the point before to the
   point after -- no control points to place off the curve, as a Bézier
   curve would need. Edwin Catmull (of Pixar) and Raphael Rom, "A Class
   of Local Interpolating Splines" (1974); the splines of animation and
   camera paths since.

            p1 ------- p2        the segment from p1 to p2 leaves p1
           /             \       parallel to p0 -> p2, and arrives at
         p0               p3     p2 parallel to p1 -> p3

   But a spline's parameter isn't a distance: t from 0 to 1 covers a
   long segment as fast as a short one, and a ship following t would
   rush and dawdle. So the curve is measured once ([make]: 16 points per
   segment, and the length so far at each), and a ship moves along it
   by distance, [s] pixels from the start, found in that table ([at]):
   the same speed everywhere, and the direction there to turn the
   sprite. The arc-length parametrization, in its simplest form.

   Related: BulletML (Kenta Cho, ABA Games), a language for bullet
   patterns; the plan's Waves (a level as a timeline of paths),
   plan_games.md section 5.

   Part of the shoot 'em up kit (kits/shmup/), with Shots.mli; used by
   games/TinyGalaga (the waves' ways in, the dives). *)

open Playground

type point = number * number

(* [catmull_rom p0 p1 p2 p3 t]: the point at [t] (0 to 1) on the curve
 * from p1 to p2, p0 and p3 their neighbors:
 *   0.5 (2 p1 + (p2 - p0) t + (2 p0 - 5 p1 + 4 p2 - p3) t^2
 *        + (3 p1 - p0 - 3 p2 + p3) t^3)
 * At t = 0, p1; at t = 1, p2. E.g. with points in a line, (0, 0) (100,
 * 0) (200, 0) (300, 0), the middle of the curve from the second to the
 * third is (150, 0); around a corner, (0, 0) (100, 0) (100, 100) (0,
 * 100), the middle from (100, 0) to (100, 100) is (112.5, 50): the
 * curve bulges out, smooth, rather than turning at the corners. *)
val catmull_rom : point -> point -> point -> point -> number -> point

(* a curve, measured: its points, 16 per segment, and the length from
 * the start at each *)
type t = { pts : point array; lengths : number array }

(* [make points]: the curve through [points] (at least 2; the first and
 * the last are their own neighbors) *)
val make : point list -> t

(* its length, in pixels: e.g. through (0, 0) (100, 0) (200, 0) (300, 0),
 * 300 *)
val length : t -> number

(* [at p s]: where the path is [s] pixels from its start, and its
 * direction there (degrees): between the two measured points around
 * [s], in proportion. E.g. the 300-long line above: (150, 0) at 150,
 * direction 0 *)
val at : t -> number -> point * number

(* the points mirrored left to right (x to -x): a wave's second line *)
val mirror : point list -> point list
