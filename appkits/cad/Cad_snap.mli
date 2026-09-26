(* Object snaps: the cursor caught by the drawing's own points
 * (AutoCAD's OSNAP, Release 2, 1984 -- Sketchpad's pen aiming,
 * appkits/sketch, grown a menu of what to aim at).
 *
 * A draftsman's line does not end "about there" but *at* the end of
 * that other line, at the center of that hole: the snap finds, among
 * the pieces near the cursor (within the aperture), the points of each
 * kind and returns the nearest. The kinds, each with its marker:
 *
 * - Endpoint (a square): a line's ends, an arc's;
 * - Midpoint (a triangle): the middle of a line or an arc;
 * - Center (a circle): a circle's or an arc's;
 * - Quadrant (a diamond): a circle's points at 0, 90, 180, 270;
 * - Intersection (an X): two pieces near the cursor crossing
 *   (Cad_geom.intersections);
 * - Perpendicular (a right angle): from the last point, the foot on
 *   the piece under the cursor -- which may be far from the cursor, so
 *   it is tried only when nothing else is near.
 *
 * AutoCAD Release 12 showed only the aperture box and jumped; the
 * markers are Release 14's AutoSnap (1997), shown because they say
 * which rule caught the cursor. *)

type kind = Endpoint | Midpoint | Center | Quadrant | Intersection | Perpendicular

val name : kind -> string

(* [find t ~aperture ?from p]: the snap nearest p within the aperture,
   of the pieces on layers switched on; [from], the last point, for
   Perpendicular *)
val find : Cad_drawing.t -> aperture:float -> ?from:Cad_geom.pt -> Cad_geom.pt -> (kind * Cad_geom.pt) option
