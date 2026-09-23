(* Collision detection, the narrow phase: do two placed hitboxes
 * overlap, and if so, how (see notes_2d_physics.md section 8)? From the
 * cheapest test to the most general, each its own function, then
 * [touching] and [contact] choosing the right one for any two hitboxes.
 *
 * Circles: overlapping when the distance between their centers is less
 * than the sum of their radii. Example: centers (0, 0) and (30, 40), 50
 * apart; radii 20 and 20 (a sum of 40): apart; 30 and 25 (55): an
 * overlap of 5, the normal (0.6, 0.8).
 *
 * Bounding boxes: overlapping when their x ranges overlap *and* their y
 * ranges do. Too coarse to decide a collision, cheap enough to rule most
 * of them out first.
 *
 * Point in polygon: cast a ray from the point, to the right, and count
 * the edges it crosses: odd, inside (the Jordan curve theorem; the
 * even-odd rule of graphics/2d/Fill). Any polygon, convex or not:
 *
 *        _____
 *       /     \___
 *      /   p ------|------>   one crossing: inside
 *      \          /
 *       \________/
 *
 * Segments: two segments cross when each one's ends are on opposite
 * sides of the other's line (the sign of a cross product, the same side
 * test as a triangle's edge functions in graphics/3d/Triangle). Two
 * polygons touch when an edge of one crosses an edge of the other, or
 * one is inside the other (a corner of it in the other).
 *
 * The separating axis theorem (SAT), for convex polygons: they don't
 * overlap exactly when some line separates them, and it's enough to try
 * the directions perpendicular to their edges. Project both on each
 * such axis: if their shadows don't overlap on one, they're apart;
 * otherwise the axis where they overlap the least gives the contact's
 * normal and depth. Example: the boxes [0, 2] x [0, 2] and [1, 3] x
 * [3, 4] overlap on the x axis ([0, 2] and [1, 3]) but not on the y
 * axis ([0, 2] and [3, 4]): apart.
 *
 *      +----+                 on the y axis, their shadows
 *      | A  |   +----+        [0, 2] and [3, 4] don't overlap:
 *      +----+   | B  |        a separating axis
 *               +----+
 *
 * (GJK -- Gilbert, Johnson, Keerthi, 1988 -- does it for any convex
 * shapes, curves included; Box2D and Bullet use it. Not here: circles
 * against polygons have their own test below.)
 *
 * References: Christer Ericson, Real-Time Collision Detection, 2005
 * (all of it); Metanet Software's N tutorials, 2004 (SAT, for game
 * programmers); Gottschalk, Lin, Manocha, "OBBTree", SIGGRAPH 1996 (the
 * separating axis theorem in graphics). *)

(*****************************************************************************)
(* {1 The tests} *)
(*****************************************************************************)

(* the tests, each on its own *)

val circles : Vec2.t * float -> Vec2.t * float -> Contact.t option
val bounds_overlap : Vec2.t * Vec2.t -> Vec2.t * Vec2.t -> bool
val point_in_polygon : Vec2.t -> Vec2.t list -> bool
val segments_cross : Vec2.t * Vec2.t -> Vec2.t * Vec2.t -> bool

(* any two polygons, convex or not: an edge crossing, or one inside *)
val polygons_touch : Vec2.t list -> Vec2.t list -> bool

(* two convex polygons: the separating axis theorem, with the contact.
 * Its point is the middle of the overlap region (the average of its
 * corners: the corners of each polygon inside the other, and where
 * their edges cross), where the rotation's lever arms start (Resolve):
 * a box landing on a corner is pushed at that corner, and tips; landing
 * flat, at the middle of its bottom edge, and doesn't.
 *
 *      +--------+
 *      |  box   |
 *   ===x===*====x=====   floor: the overlap's corners (x) and
 *      +--------+           their middle (star), the contact point
 *
 * (Box2D keeps two points for an edge, clipping one polygon's edge
 * against the other's: steadier stacks, the plan's phase 8.) *)
val sat : Vec2.t list -> Vec2.t list -> Contact.t option

(* a circle and a polygon (convex or not): the center inside, or an
 * edge nearer than the radius *)
val circle_polygon : Vec2.t * float -> Vec2.t list -> bool

(* the same, with the contact, for a convex polygon: the normal from
 * the circle to the polygon *)
val circle_convex : Vec2.t * float -> Vec2.t list -> Contact.t option

(* [nearest_on_outline p corners]: the point of the polygon's outline
 * nearest to [p]: where to push a point out of it (Particles.keep_out) *)
val nearest_on_outline : Vec2.t -> Vec2.t list -> Vec2.t

(*****************************************************************************)
(* {1 Swept tests} *)
(*****************************************************************************)

(* The swept tests. A bullet at 1500 pixels per second moves 25 pixels
 * per step: a wall thinner than that can be jumped over between two
 * steps, never seen overlapping -- tunneling (notes_2d_physics.md
 * section 12):
 *
 *          step n        step n+1
 *            o   |wall|    o          touching at neither step,
 *            *---|----|--->*          but the path crossed the wall
 *
 * The fix, continuous collision detection's simplest form: test the
 * path, the segment from the old position to the new one, not the
 * position. Example: the square [0, 2] x [0, 2] and the segment from
 * (-1, 1) to (3, 1): it enters at (0, 1); from (-1, 5) to (3, 5), it
 * misses. *)

(* [segment_polygon (a, b) corners]: where segment ab first enters the
 * polygon ([a] if it starts inside), if it does *)
val segment_polygon : Vec2.t * Vec2.t -> Vec2.t list -> Vec2.t option

(* [segment_circle (a, b) (c, r)]: the segment's point nearest to the
 * circle's center, if within it *)
val segment_circle : Vec2.t * Vec2.t -> Vec2.t * float -> Vec2.t option

(*****************************************************************************)
(* {1 Hitboxes} *)
(*****************************************************************************)

(* for any two placed hitboxes *)

(* do they overlap? (the bounding boxes first, then the exact test) *)
val touching : Shape.placed -> Shape.placed -> bool

(* how, the normal from the first to the second: for points, circles and
 * convex polygons; None when they don't overlap, and for concave
 * polygons (split them in convex pieces for a contact) *)
val contact : Shape.placed -> Shape.placed -> Contact.t option

(* [manifold a b]: the contact as up to two points, for stacking
 * (Solver.mli): a box lying on another touches it along an edge, and a
 * single point in the middle would balance it on a knife's edge. For
 * two convex polygons, the corners of each inside the other, each with
 * its own depth along the normal, and when there are more than two
 * (two boxes of the same width), the two farthest apart:
 *
 *        +--------+
 *        |  box   |
 *     ===x========x===    the box's two bottom corners, inside the
 *        +--------+          floor: two points, 2 pixels deep each
 *
 * Otherwise (the corners all outside: two thin bars crossed like a +;
 * or circles) the one point of [contact]. Box2D finds its two points
 * by clipping one polygon's edge against the other's sides (Catto,
 * "Contact Manifolds", GDC 2007): sturdier for deep overlaps; the
 * corners inside are the idea, in fewer lines. *)
val manifold : Shape.placed -> Shape.placed -> Contact.t list
