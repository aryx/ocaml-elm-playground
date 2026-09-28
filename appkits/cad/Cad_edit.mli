(* AutoCAD's editing commands that make new geometry from old: OFFSET,
 * TRIM, EXTEND, FILLET. Each works on lines, circles and arcs
 * (Cad_drawing's entities), and each is Cad_geom's crossings put to
 * work. They return the entities that replace the one edited, or an
 * error to show as AutoCAD did.
 *
 * - **TRIM** cuts at the crossings with the cutting edges and removes
 *   the piece that was clicked, between the two cuts around the click.
 *   Along a line, "between" is a parameter from 0 to 1; around an arc,
 *   an angle from its start; a whole circle needs two cuts, and what
 *   is left of it is an arc.
 *
 *     edges:     |        |              |        |
 *     a line: ---+---x----+---   ->   ---+        +---
 *                    ^ clicked
 *
 * - **EXTEND** runs the end of a line nearer the click along the line
 *   until it meets a boundary: the first crossing past the end, of the
 *   line's carrier with the boundaries' pieces.
 *
 * - **OFFSET** draws a copy at a distance, on the side clicked: a line
 *   moved along its normal, a circle or an arc with its radius grown
 *   or shrunk (a parallel curve of a circle is a circle).
 *
 * - **FILLET** rounds the corner of two lines with an arc of a given
 *   radius, tangent to both. At the corner P, with u1 and u2 the
 *   directions along the lines towards the parts clicked and theta
 *   the angle between them, the arc touches each line at d = r /
 *   tan (theta/2) from P, and its center is on the bisector at r /
 *   sin (theta/2):
 *
 *       line 1  ------T1..
 *                          .  arc, center C inside the corner
 *                           T2
 *                           |  line 2
 *
 *   Radius 0 makes a sharp corner: both lines trimmed or extended to P.
 *
 * Worked example: FILLET radius 10 of (0,0)-(100,0) and (100,0)-(100,
 * 100): theta = 90 degrees, d = 10, the lines end at (90,0) and
 * (100,10), and the arc has its center at (90,10), from 270 to 0. *)

type pt = Cad_geom.pt

val offset : float -> Cad_drawing.entity -> side:pt -> (Cad_drawing.entity, string) result

(* [trim edges e ~at]: e cut by the curves [edges], the piece at [at]
   removed *)
val trim : Cad_geom.curve list -> Cad_drawing.entity -> at:pt -> (Cad_drawing.entity list, string) result

val extend : Cad_geom.curve list -> Cad_drawing.entity -> at:pt -> (Cad_drawing.entity, string) result

(* [fillet r (l1, q1) (l2, q2)]: the two lines, each clicked at q;
   their new selves, and the arc (none for radius 0) *)
val fillet :
  float ->
  Cad_drawing.entity * pt ->
  Cad_drawing.entity * pt ->
  (Cad_drawing.entity * Cad_drawing.entity * Cad_drawing.entity option, string) result

(* the curve of a line, circle or arc *)
val curve : Cad_drawing.entity -> Cad_geom.curve option
