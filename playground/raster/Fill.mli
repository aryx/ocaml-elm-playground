(* Filling polygons: which pixels are inside a polygon given by its
 * corners? The classic scanline algorithm, processing the pixels row
 * by row ("scanline" = one row, from CRT screens drawing the image one
 * line at a time):
 *
 *   for each row, look at the horizontal line through the centers of
 *   its pixels, find where it crosses the polygon's edges, sort those
 *   crossings from left to right, and fill the pixels between them
 *   that are inside.
 *
 * For example, for this concave "U" shape, row y crosses 4 edges, at
 * x = 1, 3, 7, and 9, so it gets 2 spans, [1, 3) and [7, 9):
 *
 *     1  3   7  9
 *     +--+   +--+
 *     |  |   |  |
 *   --|##|---|##|--  row y
 *     |  +---+  |
 *     +---------+
 *
 * Works for any polygon: convex, concave, or even self-intersecting,
 * for which "inside" needs a definition, the fill rule:
 *
 * - Even_odd: a point is inside if a ray from it to infinity crosses
 *   the polygon's edges an odd number of times.
 * - Nonzero (the default in SVG, PostScript, Cairo, and so in the other
 *   Playground backends): count +1 for each edge crossed going down,
 *   -1 going up (the "winding number": how many times the polygon's
 *   outline winds around the point); inside if not 0.
 *
 * They differ only for self-intersecting polygons. For a 5-pointed star
 * drawn in one stroke (a pentagram: each corner connected to the one
 * two corners further), the outline goes around each of the 5 tips once
 * but around the center pentagon twice: winding number 2, so Nonzero
 * fills the whole star, while Even_odd leaves a pentagon-shaped hole in
 * the middle (see the "star" test in tests/Unit_fill.ml).
 *
 * References:
 * - C. Wylie, G. W. Romney, D. C. Evans, A. Erdahl, "Half-tone
 *   perspective drawings by computer", AFIPS Fall Joint Computer
 *   Conference, 1967 (scanline rendering, with the edge-coherence idea
 *   used here: an edge's crossing moves by a constant amount from one
 *   row to the next).
 * - Paul S. Heckbert, "Concave Polygon Scan Conversion", in Graphics
 *   Gems, Academic Press, 1990 (the version with an active edge list,
 *   close to this one).
 * - Foley, van Dam, Feiner, Hughes, "Computer Graphics: Principles and
 *   Practice", 2nd ed., 1990, section 3.6 (filling polygons).
 *)

type fill_rule = Nonzero | Even_odd

(* [polygon fb points ~rgb ~alpha] fills the polygon whose corners are
 * [points], in pixel coordinates (x right, y down; the last point is
 * connected back to the first). A pixel is filled if its center is
 * inside, e.g. the square with corners (1, 1) and (4, 3) fills the
 * pixels x = 1, 2, 3 of rows y = 1, 2 (centers 1.5, 2.5, 3.5 and 1.5,
 * 2.5); with the same rule for any polygon, two polygons sharing an
 * edge never both fill a pixel, nor leave one unfilled between them. *)
val polygon :
  ?rule:fill_rule -> Framebuffer.t -> (float * float) list -> rgb:int -> alpha:float -> unit
