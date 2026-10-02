(* Code_road: a use drawn on the code map as a road, from the user to
   the used: a smooth curve through a few points, and its direction
   without an arrowhead (Holten and van Wijk, "A user study on
   visualizing directed edges in graphs", CHI 2009): a colour gradient,
   green at the user to red at the used, and a taper, wide at the user.

       user ====------......  used
       green     ->      red

   The roads of Map_v2 (a unit's ties, the skeletons' joints) and of
   Code_street (a use to its definition).

   Worked example (the tests'): the spline through (0, 0), (10, 10),
   (20, 0) starts at (0, 0), ends at (20, 0), and is pulled towards
   (10, 10) without reaching it. *)

(* a uniform cubic B-spline through the control points, its ends
   clamped (each end point three times), [per] points a span (8); two
   points or less, themselves *)
val bspline : ?per:int -> (float * float) array -> (float * float) list

(* a road on the screen through [pts] (the map's pixels), [w] pixels
   wide at the user's end, a third of it at the used's, faded by
   [alpha]; [colours] the user's end's and the used's (green, red); the
   pieces off the map left out *)
val road : ?colours:(int * int * int) * (int * int * int) -> Code_map_base.area -> (float * float) list -> float -> float -> Playground.shape list
