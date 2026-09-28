(* A binary space partitioning tree: the faces in an order that is
 * right from wherever you look (Henry Fuchs, Zvi Kedem, Bruce Naylor,
 * "On Visible Surface Generation by A Priori Tree Structures",
 * SIGGRAPH 1980).
 *
 * The painter's algorithm (Painter.mli) sorts the faces by one number
 * each, their distance, and is wrong whenever that number lies: a
 * window's recess, its sides deep in the wall, is nearer than the
 * wall's middle and yet hidden by it. The BSP tree asks a question
 * that cannot lie instead. Take one face's plane: every other face is
 * in front of it, behind it, or cut in two by it (and then each half
 * is on one side). An eye in front of the plane sees the faces behind
 * it first overdrawn by the plane's own, then by those in front -- so
 * draw back, plane, front; an eye behind, the other way round. Do the
 * same inside each side, recursively:
 *
 *              A's plane
 *                  |
 *      behind A    |    in front of A           eye in front:
 *        (B)       A       (C) (D)              B, A, then C and D
 *                  |                            in their own order
 *
 * The tree depends only on the faces, not on the eye: built once, it
 * is walked in a different order from each point of view. That is why
 * Doom (1993) built its levels' trees in advance, and why a model that
 * changes as you push it is rebuilt each time (a few hundred faces:
 * nothing).
 *
 * Cutting a face in two is Sutherland and Hodgman's clipping of a
 * polygon by a plane ("Reentrant Polygon Clipping", 1974), which the
 * camera uses too, to cut what is behind the eye (Skp_view). A corner
 * carries whether the side leaving it is to be drawn as an edge: the
 * sides a cut creates are not, so a face cut by the tree is still
 * outlined only where the model has edges.
 *
 * Worked example: a square at z = 0 and another at z = 1, both facing
 * up: the tree is the first, the second in front of it. From above
 * (the eye at z = 5) the order is z = 0 then z = 1; from below, the
 * other way round. *)

(* a polygon's corners, each with whether the side from it to the next
   is an edge to draw; [data], what it is a piece of *)
type 'a poly = { corners : (Vec3.t * bool) list; data : 'a }

(* a plane: its normal n and d, the points p with n . p = d *)
type plane = Vec3.t * float

(* the plane a polygon lies in (Newell's normal); None if it has no area *)
val plane_of : (Vec3.t * bool) list -> plane option

(* [split plane corners]: the polygon's part in front of the plane and
   its part behind, [] for none; a corner on the plane goes to both *)
val split : plane -> (Vec3.t * bool) list -> (Vec3.t * bool) list * (Vec3.t * bool) list

type 'a t

(* the tree, the largest polygons taken first as the planes that split
   the rest (a large one is seldom cut by a small one's plane, the other
   way round often: fewer pieces) *)
val build : 'a poly list -> 'a t

(* the pieces of the polygons, the farthest from the eye first *)
val back_to_front : eye:Vec3.t -> 'a t -> 'a poly list

(* how many pieces the tree holds (the polygons, and the halves of
   those it cut) *)
val size : 'a t -> int
