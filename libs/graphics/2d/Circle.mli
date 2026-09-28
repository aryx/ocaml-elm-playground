(* Circles and ellipses.
 *
 * Circles: Bresenham's midpoint circle algorithm, which finds the
 * pixels of a circle with integer additions only, like Line.bresenham
 * for lines. It uses the circle's 8-fold symmetry: compute one eighth,
 * the "octant" from the top going right until the diagonal, and mirror
 * each pixel (x, y) found there into the 7 others:
 *
 *                  (-x, y) | (x, y)          here y goes up, as in
 *          (-y, x)    \    |    /   (y, x)   math: (x, y) is x right
 *                   \  \   |   /  /          and y above the center
 *         -------------- center --------------
 *                   /  /   |   \  \
 *          (-y, -x)   /    |    \   (y, -x)
 *                 (-x, -y) | (x, -y)
 *
 * In the octant the circle is flatter than 45 degrees, so, like a
 * mostly-horizontal line, each step goes right by 1 and the question
 * is whether to also go down by 1. The answer: look at the point half
 * way between the two candidate pixels (the "midpoint"); if it's inside
 * the circle, stay, else go down.
 *
 *     x   x+1
 *     #---o      <- stay: (x+1, y)
 *         |
 *         *      <- midpoint (x+1, y-1/2): inside the circle? then stay
 *         |
 *         o      <- go down: (x+1, y-1)
 *
 * "Inside" is x^2 + y^2 < r^2, and like Bresenham's error for lines,
 * that quantity changes by a simple amount from one step to the next,
 * so it can be updated with additions instead of recomputed.
 *
 * Ellipses (ovals, and circles once stretched by a non-uniform scale)
 * are drawn as polygons with enough sides to look round: that's also
 * what Cairo, PostScript, and GPUs do with curves. Midpoint algorithms
 * for ellipses exist too, but only for ellipses aligned with the x and
 * y axes -- and Playground ovals can be rotated.
 *
 * References:
 * - Jack E. Bresenham, "A linear algorithm for incremental digital
 *   display of circular arcs", Communications of the ACM 20(2):100-106,
 *   1977.
 * - M. L. V. Pitteway, "Algorithm for drawing ellipses or hyperbolae
 *   with a digital plotter", The Computer Journal 10(3):282-289, 1967
 *   (the midpoint idea, for any conic).
 *)

(* [octant r] is the octant's pixels for a circle of radius r centered
 * on (0, 0), from (0, r) at the top going right, as (x, y) offsets with
 * y up. For r = 5: [(0, 5); (1, 5); (2, 5); (3, 4)] -- then (4, 3)
 * would be past the diagonal, and is the mirror image of (3, 4). *)
val octant : int -> (int * int) list

(* [fill fb ~cx ~cy ~r ~rgb ~alpha] fills the circle of radius r
 * centered on pixel (cx, cy): each row once, between the two pixels of
 * the circle on that row. *)
val fill : Framebuffer.t -> cx:int -> cy:int -> r:int -> rgb:int -> alpha:float -> unit

(* Only the pixels of the circle itself: the octant's, mirrored 8 ways *)
val outline : Framebuffer.t -> cx:int -> cy:int -> r:int -> rgb:int -> alpha:float -> unit

(* [ellipse_points ~rx ~ry ~segments]: [segments] points on the ellipse
 * of radii rx (horizontally) and ry (vertically) centered on (0, 0),
 * evenly spaced in angle, to be drawn as a polygon *)
val ellipse_points : rx:float -> ry:float -> segments:int -> (float * float) list

(* How many polygon sides a circle of [radius] pixels needs so that the
 * polygon is never more than [tolerance] pixels (default 0.25) inside
 * the true circle. The worst place is the middle of a side, where the
 * gap (the "sagitta") is radius * (1 - cos (pi / segments)); e.g. a
 * radius of 10 pixels needs 15 sides, 100 pixels 45, 400 pixels 89. *)
val segments_for_radius : ?tolerance:float -> float -> int
