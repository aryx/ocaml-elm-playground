(* A race track, as data: a list of segments, short slices of road, each
   with its curve and its height.

   The racing games of the arcade described their courses the same way:
   a table of segments, curving and climbing, read as the car advances
   (Pole Position, Namco, 1982; Out Run, Sega, 1986). A course is written
   as sections, "a long right curve up a hill", each easing in and out so
   that curves and hills come and go smoothly:

     let track = Road.build 200. [ Road.straight 40; Road.curve 80 2.; Road.hill 80 20. ]

         enter      hold       leave       a section of 80 segments: its
     0 ......../~~~~~~~~~~\........ 0      curve (or height) eases in
                                           over a quarter, holds half,
                                           eases out over a quarter

   What a segment's [curve] means is up to the game:
     - drawn in pseudo-3D (games2.5d/TinyOutRun.ml), it's how much the road
       bends *on the screen*: each segment is shifted sideways a bit more
       than the one before; the road never really turns;
     - drawn with polygons (games3d/TinyVirtuaRacing.ml), it's how much
       the road really turns, in degrees per segment: [centerline] walks
       the track and gives each segment's point in space.
   The same track, raced in both games: the difference is the lesson.

   This module is part of the racing kit (kits/racing/), a layer on top
   of the playground for racing games, like Camera2d and Tilemap are for
   all games (see docs/claude_notes/plan_games.md, "kits").

   References: Jake Gordon, "How to build a racing game" (2012, a
   JavaScript Out Run in four parts: its track is built from exactly such
   eased sections); Lou Gorenfeld, "Lou's Pseudo 3d Page" (how the arcade
   games did it).
*)

open Playground

(* {1 Sections} *)

(* [enter] segments easing into the curve and the height, [hold]
 * segments of them, [leave] easing out; [curve] is the curve while
 * held; [hill] how much the road climbs (or, negative, descends) over
 * the whole section, in segment lengths *)
type section = { enter : int; hold : int; leave : int; curve : number; hill : number }

(* sections of n segments: a quarter to enter, a half held, a quarter to
 * leave *)
val straight : int -> section
val curve : int -> number -> section
val hill : int -> number -> section
val curve_hill : int -> number -> number -> section

(* the course of games2.5d/TinyOutRun and games3d/TinyVirtuaRacing: 1060
 * segments of curves and hills, ending at the height it started from *)
val coast : section list

(* {1 The track} *)

(* a segment: its index from the start, the height of its near and far
 * edges, its curve *)
type segment = { index : int; y1 : number; y2 : number; curve : number }

type t = { segment_length : number; segments : segment array }

(* [build segment_length sections]: the track, section after section.
 * Curves ease in quadratically and out smoothly (a cosine), heights
 * with a cosine all along the section, as in Jake Gordon's tutorial.
 * E.g. [build 200. [ hill 4 1. ]]: 4 segments climbing 200 in all,
 * their heights 0 -> 29.3 -> 100 -> 170.7 -> 200 (the cosine's ease). *)
val build : number -> section list -> t

(* the track's length, in world units: segments times their length *)
val length : t -> number

(* [segment_at track z]: the segment at the distance [z] from the start,
 * wrapping around after the end (a loop, for laps) *)
val segment_at : t -> number -> segment

(* {1 In space} *)

(* a point of the road's center line: where, and which way the road
 * goes, in degrees (0 towards -z, 90 towards +x) *)
type point = { x : number; y : number; z : number; heading : number }

(* [centerline degrees_per_curve track]: the road in space, one point
 * per segment edge (so one more than segments): starting at (0, 0, 0)
 * heading 0, each segment turns the heading by its curve times
 * [degrees_per_curve], then goes [segment_length] that way, at its far
 * edge's height. E.g. with a length of 2 and 90 degrees per curve unit,
 * one segment of curve 1 from the start ends at (2, 0, 0), heading 90:
 * turned right, then gone right. *)
val centerline : number -> t -> point array
