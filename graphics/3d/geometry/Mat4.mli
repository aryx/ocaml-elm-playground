(* 4x4 matrices, for the camera: what a GPU vertex shader needs to
 * transform every point of a scene into the screen's space, in one
 * matrix product per point. Row-major, as a 16-element float array. *)

type t = float array

(* A view matrix: the scene re-described relative to a camera at [eye]
 * looking at [target] (x to the camera's right, y up, z forward, like
 * the software rasterizer's view_space, with (0, 1, 0) as "up") *)
val look_at : eye:Vec3.t -> target:Vec3.t -> t

(* A projection matrix: things twice as far drawn half as big, with a
 * vertical field of view of [fov_degrees], the same framing as the
 * software rasterizer's project_vertex, and a z row mapping the depths
 * [near]..[far] to -1..+1 (what a GPU's depth test expects) *)
val perspective : fov_degrees:float -> aspect:float -> near:float -> far:float -> t

(* [mul a b] applied to a point means "apply b first, then a", so
 * [mul projection view] is "view, then project" *)
val mul : t -> t -> t
