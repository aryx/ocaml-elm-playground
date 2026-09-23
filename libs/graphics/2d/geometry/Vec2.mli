(* 2D vectors and points, as plain float pairs (x, y) -- the 2D twin of
 * graphics/3d/geometry/Vec3. *)

type t = float * float

val add : t -> t -> t
val sub : t -> t -> t

(* [scale s v]: v stretched s times *)
val scale : float -> t -> t

(* ax*bx + ay*by: how much two directions agree; 0 when perpendicular *)
val dot : t -> t -> float

(* ax*by - ay*bx, the 2D "cross product" (the z of the 3D one): twice
 * the signed area of the triangle (0, a, b), positive when b is
 * counterclockwise from a (with y up; clockwise on a y-down screen).
 * E.g. cross (1, 0) (0, 1) = 1. *)
val cross : t -> t -> float

(* [perp v]: v turned a quarter turn, (-y, x) *)
val perp : t -> t

val length : t -> float

(* [v] scaled to length 1; the zero vector stays (0, 0) *)
val normalize : t -> t
