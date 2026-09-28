(* A 3D modeller's scene (Blender, Ton Roosendaal, NeoGeo, 1994; open
 * source since 2002): objects, each a shape placed by three vectors,
 * and what the modeller does to them.
 *
 * An object is not its geometry but a *recipe*: a primitive (the cube
 * from -1 to 1, the sphere of radius 1, ...) and its transform --
 * location, rotation, scale, the three rows of Blender's N panel. The
 * shape is never changed; the numbers are. Moving an object is adding
 * to its location, and "a squashed sphere" is a sphere with a scale of
 * (1, 1, 0.5). A point p of the primitive lands in the world at
 *
 *     world = location + Rz (Ry (Rx (scale * p)))
 *
 * scaled first, then turned about x, y and z in that order (Blender's
 * XYZ Euler), then moved. The order matters: scaled after the turn,
 * a stretched box would shear.
 *
 * Blender's world has z up (an architect's: the floor is x, y); a
 * ray tracer's usually y (a screen's). [to_world] is the change between
 * the two, (x, y, z) -> (x, z, -y), a quarter turn about x, so that
 * both stay right-handed.
 *
 * Two objects can be one solid: a Boolean modifier on an object names
 * another, the *cutter*, and the solid is their difference (or union,
 * or intersection) -- constructive solid geometry, which the ray
 * tracer does exactly (Csg.mli). The cutter itself is not drawn.
 *
 * Worked example: a cube scaled (2, 1, 1), turned 90 degrees about z
 * and moved to (0, 0, 3): its corner (1, 1, 1) is first (2, 1, 1),
 * then (-1, 2, 1), then (-1, 2, 4). *)

type v3 = float * float * float

type shape =
  | Cube
  | Sphere
  | Cylinder
  | Cone
  | Torus of float (* the tube's radius, the ring's being 1 *)
  | Ground (* the plane z = 0, for ever *)

type look = Plain | Checker | Marble | Wood

(* a colour, 0xRRGGBB; a mirror from 0 to 1; glass or not *)
type material = { color : int; look : look; mirror : float; glass : bool }

type kind = Mesh of shape * material | Point_light of int | Sun_light of int | Camera

type boolean = Difference | Union | Intersect

type obj = {
  name : string;
  kind : kind;
  location : v3;
  rotation : v3; (* degrees, about x, then y, then z *)
  scale : v3;
  hidden : bool;
  modifier : (boolean * string) option; (* the cutter, by name *)
}

type t = obj list

val grey : material

(* Blender's startup: the cube, the light, the camera *)
val default : t

(* "Cube", then "Cube.001", "Cube.002"... as Blender names *)
val fresh_name : string -> t -> string

(* [add kind base t]: a new object at the origin, named from [base] *)
val add : kind -> string -> t -> t * string

val find : t -> string -> obj option
val update : string -> (obj -> obj) -> t -> t

(* the objects gone, and the modifiers that named them *)
val remove : string list -> t -> t

(* copies, named afresh; the new names *)
val duplicate : string list -> t -> t * string list

(* is it some Boolean's cutter? (not drawn in the render) *)
val is_cutter : t -> string -> bool

(*****************************************************************************)
(* {1 Transforms} *)
(*****************************************************************************)

val transform : obj -> v3 -> v3

(* Blender's z up to the ray tracer's y up *)
val to_world : v3 -> v3

(* [rotate_point axis degrees p]: p turned about the axis (0, 1, 2: x,
   y, z) through the origin, counterclockwise looking down the axis *)
val rotate_point : int -> float -> v3 -> v3

(* [translate d o], [turn axis degrees o], [resize (kx, ky, kz) o]:
   what G, R and S do. [turn] adds to that axis' Euler angle, which is
   a turn about the world's axis exactly for z (applied last) and for
   an object not turned about the others yet; the exercise is to keep
   a rotation matrix instead *)
val translate : v3 -> obj -> obj

val turn : int -> float -> obj -> obj
val resize : v3 -> obj -> obj

(*****************************************************************************)
(* {1 Wireframes} *)
(*****************************************************************************)

(* the object's edges, in the world (z up): a cube's twelve, a sphere's
   rings and meridians, a light's star, the camera's pyramid looking
   at [target] *)
val wires : ?target:v3 -> obj -> (v3 * v3) list
