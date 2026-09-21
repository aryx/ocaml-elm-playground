(* A circuit in space: a closed line through the middle of a road,
   which has a width, a height and a lean at every point, turned into a
   ribbon of quads.

   Where gamekits/racing/Road.mli describes a course as a *list of
   segments* read as the car advances -- the way the arcade's pseudo-3D
   racers did, and what games2.5d/TinyOutRun and games3d/TinyVirtuaRacing
   drive on -- this one describes it as a *shape in space*, which is
   what a polygon racer with hills and banked corners needs:

     Road.t              a table: curve 2, hill 20, straight, ...
                         the road never really turns; the drawing does

     Track3d.t           a closed line through points in space, each
                         with its width and its bank; the road turns,
                         climbs and leans, and you can stand anywhere
                         and look at it

   A course is written as a handful of control points, and the line
   through them is a Catmull-Rom spline (Edwin Catmull and Raphael Rom,
   1974), which has the property that matters here: it passes *through*
   its control points, so a course is drawn by saying where the road
   goes, not by guessing at weights. The spline is then resampled at
   even distances, so that a segment is the same length everywhere and
   "how far along" is a distance in world units, not a parameter.

   Everything a game asks is in terms of that distance [s] along the
   middle, and an [offset] across it (positive to the right of the way
   it is driven):

                    offset < 0     |     offset > 0
                                   |
        ~~~~~~~~~~~~~~~~~~~~~~~~~~~+~~~~~~~~~~~~~~~~~~~~~~~~~   the middle
             s = 0           s = 100          s = 200            [at]
        <--- width ---><--- width --->

   so that the road's surface is [across], the whole of a kart's
   position is (s, offset), an item box stands at (s, offset), the
   traffic's lane is a constant offset, "am I on the road" is
   |offset| < width, and the lap is s / [length]. [locate] is the way
   back: a point in the world to (s, offset).

   The lean ([bank], degrees) tilts the ribbon around its own middle,
   so the outside of a banked corner is *higher* than the inside, and a
   kart drawn at its offset stands on the slope. It is what lets a toy
   have Suzuka's 130R or Wario Stadium's walls without any physics:
   nothing here pushes a car into the corner, it only puts the road
   where the eye expects it (an exercise: let the bank pull the car,
   which is the whole of a banked corner in a real game).

   Part of the racing kit (gamekits/racing/, see Road.mli), in its own
   library because it draws: the 3D playground is a virtual library, so
   a 2D game linking the racing kit would have to link a 3D backend
   too.

   References: Jake Gordon, "How to build a racing game" (2012), for the
   segments and their kerbs; Catmull and Rom, "A class of local
   interpolating splines" (1974); and, for what the numbers are for,
   the tracks of Mario Kart 64 (Nintendo, 1996), which are exactly this
   -- a closed ribbon that climbs, drops and leans. *)

open Playground
open Playground3d

(* {1 Writing a course} *)

(* A control point: where the middle of the road passes, how wide it is
 * there ([width] is *half* the road, since everything across the
 * ribbon is measured from the middle), and how much it leans, in
 * degrees (positive: the right-hand side lifted, for a left-hand
 * corner). Between two control points all three are eased from one to
 * the other, so a course need only say where each thing changes. *)
type control = { x : number; y : number; z : number; width : number; bank : number }

(* [control ~y ~width ~bank x z]: a control point at (x, z), by default
 * flat ([y] = 0), 10 wide and not leaning *)
val control : ?y:number -> ?width:number -> ?bank:number -> number -> number -> control

type t

(* [build ~step controls]: the closed circuit through [controls], cut
 * into segments of [step] (default 3.) -- the quads a game draws, and
 * the grain of every answer below. At least four control points are
 * needed for a spline; fewer raises [Invalid_argument]. *)
val build : ?step:number -> control list -> t

(* [of_road ~width ~degrees_per_curve ~bank_per_curve road]: the ribbon
 * a Road.t describes (gamekits/racing/Road.mli: a course as a table of
 * segments, the way the arcade's pseudo-3D racers wrote one). Its
 * centre line is walked into space by [Road.centerline], which is what
 * [degrees_per_curve] is for; this adds the width and, from each
 * segment's own curve, the lean:
 *
 *     bank = -curve * bank_per_curve
 *
 * so a road turning right lifts its left-hand side, eased in and out
 * with the curve because Road eases the curve. Real roads are built
 * that way (superelevation).
 *
 * The result is a *stage*, not a circuit: [Road.coast] ends somewhere
 * else than it started, so distances do not wrap and [at] stops at the
 * end. games3d/TinyVirtuaRacing drives this, on the same Road.t that
 * games2.5d/TinyOutRun reads segment by segment -- which is the whole
 * point of having both games. *)
val of_road : ?width:number -> ?degrees_per_curve:number -> ?bank_per_curve:number -> Road.t -> t

(* the course's length, in world units: [segments] times [step] *)
val length : t -> number

val segments : t -> int
val step : t -> number

(* {1 Places on it} *)

(* the middle of the road at some distance along: where it is, which
 * way it goes (degrees, 0 towards -z and 90 towards +x, Camera3d's
 * headings), and the width and lean there *)
type place = {
  px : number;
  py : number;
  pz : number;
  heading : number;
  width : number;
  bank : number;
}

(* [at t s]: the place at the distance [s] along the middle, the two
 * nearest samples mixed. On a circuit the distance wraps round the lap
 * (so [at t (length t +. 1.)] is [at t 1.]); on a stage it stops at
 * each end. *)
val at : t -> number -> place

(* [across t s offset]: the point on the road's surface [offset] to the
 * right of the middle at [s] (negative: to the left), lifted and
 * tilted by the lean there. This is the ribbon: every quad, every kart
 * and every item box goes through it. *)
val across : t -> number -> number -> number * number * number

(* [locate ?near t x z]: the other way -- (how far along, how far to
 * the right) for a point in the world, the ground under it forgotten
 * (a circuit that climbs over itself would need more).
 *
 * A circuit comes back near itself, so the answer is not unique: pass
 * [near], the distance the thing was at last frame, and the search
 * stays within a few segments of it, which is both right and quick.
 * Without it, every segment is tried. *)
val locate : ?near:number -> t -> number -> number -> number * number

(* [forward t s] : the direction of travel at [s], as (x, z): what
 * Camera3d.forward gives for [at t s]'s heading *)
val forward : t -> number -> number * number

(* {1 Drawing it} *)

(* [strip t color i a b]: the quad of segment [i] (0 to [segments] - 1)
 * from [a] to [b] across, its face up. The road, its kerbs, its verges
 * and the line down its middle are all strips at different offsets --
 * the game chooses the colors and where each one stops, since that is
 * all a course looks like:
 *
 *     grass | kerb |        road        | kerb | grass
 *        -6w  -1.15w  -w     0      w   1.15w    6w
 *)
val strip : t -> color -> int -> number -> number -> shape3d

(* [wall t color i offset height]: the quad standing along segment
 * [i] at [offset], [height] tall: a rail, a barrier, the side of a
 * tunnel. Drawn on both sides, since which side a game sees it from
 * depends on where the car is. *)
val wall : t -> color -> int -> number -> number -> shape3d
