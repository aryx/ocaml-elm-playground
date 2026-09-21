(* 4x4 matrices, for the camera: what a GPU vertex shader needs to
 * transform every point of a scene into the screen's space, in one
 * matrix product per point. Row-major, as a 16-element float array. *)

type t = float array

(* A view matrix: the scene re-described relative to a camera at [eye]
 * looking at [target] (x to the camera's right, y up, z forward, like
 * the software rasterizer's view_space, with [up], default (0, 1, 0),
 * as "up", see Camera.basis) *)
val look_at : ?up:Vec3.t -> eye:Vec3.t -> target:Vec3.t -> unit -> t

(* A projection matrix: things twice as far drawn half as big, with a
 * vertical field of view of [fov_degrees], the same framing as the
 * software rasterizer's project_vertex, and a z row mapping the depths
 * [near]..[far] to -1..+1 (what a GPU's depth test expects) *)
val perspective : fov_degrees:float -> aspect:float -> near:float -> far:float -> t

(* [orthographic ~height ~aspect ~near ~far]: the projection with no
 * perspective in it -- a box, not a pyramid, so nothing shrinks with
 * distance. [height] is how much of the world fits up the screen, the
 * same at every depth. *)
val orthographic : height:float -> aspect:float -> near:float -> far:float -> t

(* [mul a b] applied to a point means "apply b first, then a", so
 * [mul projection view] is "view, then project" *)
val mul : t -> t -> t

(* Rows become columns: the same matrix in column-major order, the one
 * a GPU stores. OpenGL can transpose while uploading a matrix
 * (uniform_matrix4fv's [transpose] argument), but WebGL 1 must be
 * given column-major already, so its backend calls this first. *)
val transpose : t -> t
