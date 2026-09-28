(* Thick lines ("stroking", in PostScript's vocabulary): Line draws
 * lines 1 pixel wide; a line [width] pixels wide is instead an *area*,
 * all the points within width/2 of the line, so it's drawn by turning
 * it into polygons and filling them. For each segment, the rectangle
 * around it; for each point, a disk of diameter [width], which rounds
 * the ends and fills the notches where two segments meet at an angle:
 *
 *        +--------------------+         +-----+      the corner
 *      (      segment p0-p1     )   ... | ... |      of 2 rectangles
 *        +--------------------+         +-----+      leaves a notch
 *       ^                      ^
 *     disk                    disk     (round "joins" and "caps")
 *
 * All the rectangles and disks are filled in one Fill.polygons call,
 * after turning them all the same way, so with the Nonzero rule the
 * result is their union: each pixel painted once, even where they
 * overlap (see Fill.polygons).
 *
 * Reference: Adobe Systems, "PostScript Language Reference Manual",
 * Addison-Wesley, 1985 (stroke, line width, round joins and caps). *)

(* The rectangles and disks of [lines] (each a list of points, in pixel
 * coordinates, joined by segments), all turned the same way: to fill
 * with the Nonzero rule, e.g. with Fill.polygons_aa for antialiased
 * strokes *)
val contours : (float * float) list list -> width:float -> (float * float) list list

(* [polylines fb lines ~width ~rgb ~alpha]: fill the [contours] of
 * [lines] with Fill.polygons *)
val polylines :
  Framebuffer.t -> (float * float) list list -> width:float -> rgb:int -> alpha:float -> unit
