(* The camera of a modeller you walk around: it looks at a point, the
 * target, from a distance, an azimuth (turning round the vertical) and
 * an elevation (up and down) -- orbit changes the two angles, pan moves
 * the target across the screen, zoom the distance. SketchUp's middle
 * button, and every 3D program's since.
 *
 * The world is z up, SketchUp's (and the architect's: z is the
 * height); Camera takes its "up" as a hint, so nothing is converted.
 * What is behind the eye must be cut off before the perspective
 * divides by the depth (a point behind the eye would come out the
 * other side of the screen, upside down): the ground, which goes on
 * behind you, is always cut, by Bsp.split with the plane just in front
 * of the eye.
 *
 * Worked example: target (0, 0, 0), distance 10, azimuth 0, elevation
 * 0: the eye is at (10, 0, 0), looking along -x; the point (0, 0, 1) is
 * straight above the middle of the screen. *)

type t = { target : Vec3.t; distance : float; azimuth : float; elevation : float; fov : float }

(* where the view is drawn: its middle and size, in screen units *)
type area = { cx : float; cy : float; w : float; h : float }

val start : t
val eye : t -> Vec3.t
val camera : t -> Camera.t

(* a world point on the screen, None if behind the eye *)
val project : t -> area -> Vec3.t -> (float * float) option

(* the ray from the eye through a point of the screen: its origin and
   its direction (unit) *)
val ray : t -> area -> float * float -> Vec3.t * Vec3.t

(* a polygon (corners with their drawn flags, as Bsp's) on the screen,
   cut where it goes behind the eye; [] if none of it is in front *)
val polygon : t -> area -> (Vec3.t * bool) list -> ((float * float) * bool) list

(* a segment on the screen, cut the same way *)
val segment : t -> area -> Vec3.t -> Vec3.t -> ((float * float) * (float * float)) option

(* [orbit t dx dy]: turned by the mouse's move, in screen units;
   [pan t area dx dy]: the target moved so that what was under the
   mouse stays under it; [zoom t notches]: nearer (positive) or
   farther *)
val orbit : t -> float -> float -> t
val pan : t -> area -> float -> float -> t
val zoom : t -> float -> t

(* seen whole: the points in the middle, near enough to fill it *)
val extents : t -> Vec3.t list -> t
