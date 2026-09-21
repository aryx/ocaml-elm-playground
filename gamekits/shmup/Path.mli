(* Path: the curves enemies fly along.

   In Galaga (Namco, 1981) the enemies don't march, they fly: in waves
   along swirling curves, then down at you in dives. A path is typed as
   a few points the enemy passes through, made smooth by a Catmull-Rom
   spline: the curve goes through every point, and its direction at
   each point is the direction from the point before to the point after
   -- no control points to place off the curve, as a Bézier curve would
   need.

            p1 ------- p2        the segment from p1 to p2 leaves p1
           /             \       parallel to p0 -> p2, and arrives at
         p0               p3     p2 parallel to p1 -> p3

   But a spline's parameter isn't a distance: t from 0 to 1 covers a
   long segment as fast as a short one, and a ship following t would
   rush and dawdle. So the curve is measured once ([make]: 16 points
   per segment, and the length so far at each), and a ship moves along
   it by distance, [s] pixels from the start, found in that table
   ([at]): the same speed everywhere, and the direction there to turn
   the sprite.

   The curves themselves are Curve.mli (graphics/2d/geometry/), with
   the splines, de Casteljau's flattening and the arc-length
   parametrization explained; what is here is what a shoot 'em up adds:
   16 samples a segment and no more (an enemy sprite doesn't need a
   tenth of a pixel), directions in degrees to hand to [rotate], and a
   wave's second line as the mirror image of the first.

   Related: BulletML (Kenta Cho, ABA Games), a language for bullet
   patterns; the plan's Waves (a level as a timeline of paths),
   plan_games.md section 5.

   Part of the shoot 'em up kit (gamekits/shmup/), with Shots.mli; used by
   games/TinyGalaga (the waves' ways in, the dives) and
   games/TinyGradius (the fans and sine waves flying in from the
   right). *)

open Playground

type point = number * number

(* a curve, measured: its points, 16 per segment, and the length from
   the start at each *)
type t = Curve.t

(* [make points]: the curve through [points] (at least 2; the first and
   the last are their own neighbors) *)
val make : point list -> t

(* its length, in pixels: e.g. through (0, 0) (100, 0) (200, 0) (300, 0),
   300 *)
val length : t -> number

(* [at p s]: where the path is [s] pixels from its start, and its
   direction there (degrees, as [rotate] wants them): between the two
   measured points around [s], in proportion. E.g. the 300-long line
   above: (150, 0) at 150, direction 0 *)
val at : t -> number -> point * number

(* the points mirrored left to right (x to -x): a wave's second line *)
val mirror : point list -> point list
