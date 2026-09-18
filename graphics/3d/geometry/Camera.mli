(* A camera: where it is, where it looks, how wide it sees -- and the
 * first two steps of every 3D renderer, putting the scene in front of
 * the camera and then flattening it:
 *
 *   world coordinates        view coordinates           normalized device
 *   (the scene's)       ->   (the camera's: x right, -> coordinates (x, y
 *                            y up, z forward)           in -1..1 on screen)
 *        [view]                     [ndc]
 *
 * The view step is a change of basis: the camera's own three directions,
 *
 *                 up                     forward = from eye to target
 *                 ^   ^ forward          right   = forward x (0, 1, 0)
 *                 |  /                   up      = right x forward
 *                 | /
 *          eye    +----> right
 *
 * and a point's view coordinates are how far it is along each of them
 * from the eye: three dot products (see notes_3d.md, section 4).
 *
 * The ndc step is the perspective: things twice as far look half as
 * big, i.e. divide by the depth z. The field of view [fov] (vertical, in
 * degrees) sets the scale: f = 1 / tan(fov / 2), so a point at the top
 * edge of the view (y / z = tan(fov / 2)) lands at ndc y = 1. Example:
 * with fov = 90 degrees, f = 1, and a point 10 in front of the camera
 * and 5 up is at ndc y = 5 / 10 = 0.5, half way to the top.
 *
 * Reference: Lawrence G. Roberts, "Machine Perception of
 * Three-Dimensional Solids", MIT PhD thesis, 1963 (perspective
 * projection, and homogeneous coordinates, for computer graphics). *)

type t = {
  eye : Vec3.t;
  target : Vec3.t;
  (* vertical field of view, in degrees *)
  fov : float;
  (* only what's between these two depths is drawn *)
  near : float;
  far : float;
}

(* The camera's (right, up, forward) unit vectors, "up" being as close
 * to the world's (0, 1, 0) as possible (a camera looking straight up
 * or down has no right: out of scope) *)
val basis : eye:Vec3.t -> target:Vec3.t -> Vec3.t * Vec3.t * Vec3.t

(* [view camera point]: [point] in view coordinates, (along right,
 * along up, along forward = depth) *)
val view : t -> Vec3.t -> Vec3.t

(* f = 1 / tan(fov / 2), the scale of the perspective *)
val focal : t -> float

(* [ndc camera ~aspect (x, y, z)]: a point already in view coordinates,
 * perspective-divided to normalized device coordinates, x and y in
 * -1..1 for what's in view; [aspect] is the screen's width / height (x
 * is squeezed by it, so a square stays square). None when its depth z
 * is not strictly between [near] and [far]. *)
val ndc : t -> aspect:float -> Vec3.t -> (float * float) option
