(* Portals: one transform, and a honest drawing (notes_3d_physics.md
 * section 15; plan_physics3d_teaching.md phase 12).
 *
 * A portal is a rectangle on a surface: its middle, the way out of the
 * surface ([normal], into the room), which way is up on it, and its
 * size. A *pair* is one rigid motion, the one that takes the room in
 * front of the first portal to the room in front of the second, turned
 * half round about the portal's up so that going *in* one is coming
 * *out* of the other:
 *
 *        in at A, going -nA                out at B, going +nB
 *
 *              nA                                  nB
 *           <--|  wall                     wall  |-->
 *          o-> |                                 |  o->
 *
 *     in A's frame (right, up, normal) a point is (x, y, z); it comes
 *     out at (-x, y, -z) in B's -- half a turn about up
 *
 * and that motion is applied to everything about a body that crosses:
 * where it is, which way it goes, which way it points, how it spins --
 * the velocity *turned and not scaled*, which is Portal's whole physics
 * ("speedy thing goes in, speedy thing comes out"): fall 20 m into a
 * floor portal, come out of a wall one going 20 m/s sideways.
 *
 * The drawing is the expensive half. With no stencil buffer and no
 * render-to-texture in any backend, what is seen through a portal is
 * drawn by taking the other room's polygons through the pair's motion
 * (so that what is in front of B lands behind A), and cutting them to
 * what the eye can see through A: the four planes through the eye and
 * A's four edges, and A's own plane -- Sutherland-Hodgman, as
 * TinyDescent.ml cuts its rooms to its portals' windows. The
 * cut polygons are then ordinary polygons behind a hole in the wall,
 * and the z-buffer does the rest. *)

open Playground3d

type vec = number * number * number

type t = {
  centre : vec;
  normal : vec; (* out of the surface, into the room: unit *)
  up : vec; (* along the surface: unit, square to [normal] *)
  width : number;
  height : number;
}

(* the portal's third axis, up x normal: its right, seen from the room *)
val right : t -> vec

(*****************************************************************************)
(* {1 Going through} *)
(*****************************************************************************)

(* [point ~from ~into p]: where [p], in front of [from], is in front of
 * [into]; [direction] the same for a direction (turned, not moved) *)
val point : from:t -> into:t -> vec -> vec
val direction : from:t -> into:t -> vec -> vec

(* the same motion for an orientation *)
val orientation : from:t -> into:t -> Quat.t -> Quat.t

(* [carry ~from ~into b]: the body [b] come out of [into]: its place,
 * velocity, orientation and spin all taken through *)
val carry : from:t -> into:t -> Physics3d.body -> Physics3d.body

(* [crossed p ~before ~after]: whether a point going from [before] to
 * [after] went through the portal -- from in front of its surface to
 * behind it, inside its rectangle *)
val crossed : t -> before:vec -> after:vec -> bool

(*****************************************************************************)
(* {1 Seeing through} *)
(*****************************************************************************)

(* [clip ~eye p polygon]: the part of [polygon] the eye can see through
 * [p] and behind it: cut by the planes through the eye and each of the
 * portal's edges, and by the portal's own plane ([] when none of it) *)
val clip : eye:vec -> t -> vec list -> vec list

(*****************************************************************************)
(* {1 A rotation from its frame} *)
(*****************************************************************************)

(* the quaternion of the rotation taking the world's x, y, z axes to
 * the three given unit, square, right-handed ones (Shepperd, 1978) *)
val of_frame : vec -> vec -> vec -> Quat.t
