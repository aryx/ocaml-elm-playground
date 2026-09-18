(* Hitboxes: the shape a body has for collisions (see
 * notes_2d_physics.md section 8). Often simpler than what's drawn --
 * a circle for a ball, a box for a crate -- because the tests between
 * simple shapes are cheap, and exact.
 *
 *     Point      Circle r      Box (w, h)       Polygon [corners]
 *       .         .---.        +--------+           /\
 *                (  o  )       |   o    |          /  \___
 *                 '---'        +--------+         /   o   |
 *                                                 \_______|
 *
 * A hitbox is described around its body's center (o); [place] puts it
 * in the world, at a position and an angle, where the tests happen
 * (Collide). Once placed, a box is just a polygon of 4 corners: the
 * tests only know points, circles, and polygons.
 *
 * Polygons can be convex or not (an asteroid, a U): the general tests
 * handle both; the separating axis theorem, faster and giving a
 * contact, needs convex ones (see [convex] and Collide.mli). *)

type t =
  | Point
  | Circle of float (* radius *)
  | Box of float * float (* width, height, centered *)
  | Polygon of Vec2.t list (* the corners, around the center, in order *)

(* a hitbox placed in the world *)
type placed =
  | Point_at of Vec2.t
  | Circle_at of Vec2.t * float (* center, radius *)
  | Polygon_at of Vec2.t list (* corners, in world coordinates *)

(* [place ~angle ~scale pos t]: [t] turned by [angle] (radians,
 * counterclockwise), scaled, and moved to [pos] (in that order) *)
val place : ?angle:float -> ?scale:float -> Vec2.t -> t -> placed

(* the corners of a [w] x [h] box around (0, 0), counterclockwise from
 * the bottom left *)
val box_corners : float -> float -> Vec2.t list

(* a polygon's edges: each corner with the next, the last with the
 * first *)
val edges : Vec2.t list -> (Vec2.t * Vec2.t) list

(* the area: pi r^2, w h, and for a polygon the shoelace formula (half
 * the sum of the cross products of consecutive corners). Example: the
 * box (4, 2) and the polygon [(0,0); (4,0); (4,2); (0,2)] both have
 * area 8. *)
val area : t -> float

(* the axis-aligned bounding box, (min corner, max corner): the cheap
 * test before the exact ones *)
val bounds : placed -> Vec2.t * Vec2.t

(* whether a polygon is convex: all its turns the same way. Example: a
 * square is; an arrowhead [(0,0); (4,2); (0,4); (1,2)] isn't. *)
val convex : Vec2.t list -> bool
