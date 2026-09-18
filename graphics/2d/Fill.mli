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

(* Several polygons ("contours") filled as one shape: their edges all go
 * into the same edge table, so on each row the winding number counts
 * the crossings of all of them. With Nonzero, overlapping contours
 * turned the same way (all clockwise, or all counterclockwise) add up,
 * 1 + 1 = 2, still inside: filling them together paints their *union*,
 * each pixel once, even where they overlap -- no darker overlaps when
 * drawn half-transparent, unlike filling them one by one. (Contours
 * turned opposite ways subtract instead, 1 - 1 = 0: that's how a
 * letter "O" is an outer contour minus an inner one.) Stroke uses it to
 * draw thick lines. *)
val polygons :
  ?rule:fill_rule -> Framebuffer.t -> (float * float) list list -> rgb:int -> alpha:float -> unit

(* The scanline algorithm alone: [scan ~height contours ~on_span] calls
 * [on_span ~y xa xb] for each span of each row y in [0, height), with
 * its exact ends xa and xb (not rounded to pixels). [polygons] turns
 * the spans into pixels by their centers; [polygons_aa] by coverage. *)
val scan :
  ?rule:fill_rule ->
  height:int ->
  (float * float) list list ->
  on_span:(y:int -> float -> float -> unit) ->
  unit

(* Like [polygons], with antialiasing: instead of "is the pixel's
 * center inside?", yes or no, compute *how much* of the pixel is
 * inside, from 0 to 1, and paint it that opaque. Edges then get
 * intermediate shades instead of "jaggies", the staircase effect of
 * all-or-nothing pixels (see the magnifier, "z", with "n" on and off).
 *
 * How much of a pixel is covered is estimated by scanning [subrows]
 * (default 4) rows per pixel row, and adding up, for each pixel, the
 * exact horizontal overlap of each sub-row's spans, divided by 4. E.g.
 * for 3 pixels with these spans on their 4 sub-rows:
 *
 *      pixel 0   pixel 1   pixel 2
 *     +--------+---------+---------+
 *     |    ====|=========|====     |   [0.5, 2.5)
 *     |     ===|=========|=====    |   [0.6, 2.6)
 *     |       =|=========|=======  |   [0.8, 2.8)
 *     |        |=========|=========|   [1.0, 3.0)
 *     +--------+---------+---------+
 *       0.275     1.0       0.725      coverage
 *
 * pixel 0: (0.5 + 0.4 + 0.2 + 0) / 4 = 0.275; pixel 2:
 * (0.5 + 0.6 + 0.8 + 1) / 4 = 0.725. Horizontally the coverage is
 * exact, vertically it's sampled 4 times: a middle ground between
 * sampling (as many samples as you can afford, e.g. 4x4 per pixel)
 * and computing exact areas.
 *
 * References:
 * - Franklin C. Crow, "The aliasing problem in computer-generated
 *   shaded images", Communications of the ACM 20(11):799-805, 1977
 *   (what jaggies are, and why: sampling a signal with sharper details
 *   than the sampling rate).
 * - Loren Carpenter, "The A-buffer, an antialiased hidden surface
 *   method", SIGGRAPH '84 (per-pixel coverage from subpixel samples).
 * - Tom Duff, "Polygon scan conversion by exact convolution", Raster
 *   Imaging and Digital Typography, 1989 (exact area coverage). *)
val polygons_aa :
  ?rule:fill_rule ->
  ?subrows:int ->
  Framebuffer.t ->
  (float * float) list list ->
  rgb:int ->
  alpha:float ->
  unit
