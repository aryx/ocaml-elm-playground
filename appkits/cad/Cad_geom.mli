(* The geometry a 2D CAD program is made of: points, and three curves
 * -- a segment, a circle, an arc -- met, measured and cut (AutoCAD,
 * Autodesk, 1982; plan_cad.md).
 *
 * Everything AutoCAD's editing commands do comes down to one question,
 * asked of two curves: where do they cross? TRIM cuts a curve at its
 * crossings with the cutting edges, EXTEND runs a line to its first
 * crossing with a boundary, FILLET rounds the crossing of two lines,
 * and the INTersection snap is the crossing nearest the cursor. Three
 * pairs cover it:
 *
 * - two lines: one linear system, a x = b, by Cramer's rule, none when
 *   they are parallel (the cross product of their directions is 0);
 * - a line and a circle: the point a + t d at distance r from the
 *   center, a quadratic in t, two, one or no roots;
 * - two circles: along the line of their centers, at the distance
 *   a = (d^2 + r1^2 - r2^2) / 2d from the first (the two triangles
 *   share their height h, h^2 = r1^2 - a^2), and h across it.
 *
 * Each is first solved on the curves' *carriers* -- the infinite line
 * through a segment, the whole circle of an arc -- and then kept if it
 * is on both pieces: EXTEND needs the carriers, TRIM the pieces.
 *
 * Angles are in degrees, counterclockwise from the x axis, as in DXF;
 * an arc goes counterclockwise from its start angle to its end angle.
 *
 * Worked example: the segment (0,0)-(10,0) and the circle of center
 * (5,0) and radius 3 cross at t = 0.2 and t = 0.8, (2,0) and (8,0). *)

type pt = float * float

val add : pt -> pt -> pt
val sub : pt -> pt -> pt
val scale : float -> pt -> pt
val dot : pt -> pt -> float
val cross : pt -> pt -> float
val dist : pt -> pt -> float

(* the vector of length 1 the same way; (0, 0) stays *)
val unit : pt -> pt

(* [polar p d a]: the point at distance d from p, at angle a *)
val polar : pt -> float -> float -> pt

(* the angle of the direction from a to b, in [0, 360) *)
val angle : pt -> pt -> float

(* an angle in [0, 360) *)
val norm_angle : float -> float

(* [within a0 a1 a]: is a on the arc going counterclockwise from a0 to
   a1? *)
val within : float -> float -> float -> bool

type curve =
  | Segment of pt * pt
  | Circle of pt * float
  | Arc of pt * float * float * float (* center, radius, start, end *)

(* an arc's two ends *)
val arc_ends : pt -> float -> float -> float -> pt * pt

(* [param a b p]: where p is along the segment from a to b, 0 at a and 1
   at b (p's foot, if it is off the segment) *)
val param : pt -> pt -> pt -> float

(* the crossings of the carriers: the infinite lines, the whole circles *)
val carrier_intersections : curve -> curve -> pt list

(* is the point (on the carrier) on the piece itself? *)
val on_piece : curve -> pt -> bool

(* the crossings of the pieces *)
val intersections : curve -> curve -> pt list

(* the nearest point of the piece *)
val nearest : curve -> pt -> pt
val distance : curve -> pt -> float

(* the foot of the perpendicular from a point to the carrier *)
val foot : curve -> pt -> pt
