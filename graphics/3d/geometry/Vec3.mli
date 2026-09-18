(* 3D vectors and points, as plain float triples (x, y, z), and the
 * handful of operations every 3D renderer is built from. See
 * docs/claude_notes/notes_3d.md, section 2, for what dot and cross
 * products are for.
 *
 * Used by the 3D Playground (playground3d/Playground3d.ml), its
 * software rasterizer and its OpenGL backend (Gpu_scene). *)

type t = float * float * float

val add : t -> t -> t
val sub : t -> t -> t

(* [scale s v]: v stretched s times *)
val scale : float -> t -> t

(* ax*bx + ay*by + az*bz: how much two directions agree; 0 when they're
 * perpendicular *)
val dot : t -> t -> float

(* a vector perpendicular to both, by the right-hand rule *)
val cross : t -> t -> t

val length : t -> float

(* [v] scaled to length 1; the zero vector stays (0, 0, 0) (see
 * [face_normal] for why that deserves attention) *)
val normalize : t -> t

(* the average of the points *)
val centroid : t list -> t

(* The unit normal of a polygon (perpendicular to its plane, pointing
 * out of the side from which its points turn counterclockwise), by
 * Newell's method: robust to repeated points, and the best fit for a
 * polygon that isn't quite flat. See Vec3.ml for why the obvious
 * formula isn't enough. *)
val face_normal : t list -> t
