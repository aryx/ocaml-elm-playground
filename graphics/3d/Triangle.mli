(* Drawing a triangle: which pixels it covers, and, for each, its color
 * and whether it's hidden by something nearer.
 *
 * Which pixels: the "edge function" of an edge from a to b, at a point
 * p, is the cross product (b - a) x (p - a), positive on one side of
 * the edge's line, negative on the other, zero on it. A point is inside
 * the triangle when it's on the inner side of all 3 edges: the
 * triangle is the intersection of 3 half-planes.
 *
 *                 v2
 *                 /\          w0 = edge v1 v2 p   (>= 0 left of v1->v2)
 *       w1 < 0   /  \  w0 < 0 w1 = edge v2 v0 p
 *               / p  \        w2 = edge v0 v1 p
 *              /  .   \
 *          v0 +--------+ v1   inside: w0, w1, w2 all >= 0
 *                w2 < 0       (all <= 0 if wound the other way)
 *
 * So: for every pixel of the triangle's bounding box, compute the 3
 * edge functions at the pixel's center (x + 0.5, y + 0.5), and draw it
 * if they agree. No sorting of vertices, no special cases for flat tops
 * or bottoms (unlike scanline filling, see graphics/2d/Fill.mli), and
 * every pixel is independent of the others: what GPUs do, in parallel.
 *
 * A bonus: the 3 values, divided by the triangle's area (the edge
 * function of the third vertex, twice the area really), are the
 * pixel's barycentric weights l0, l1, l2 (how much of each vertex the
 * pixel is made of, summing to 1), what Interpolate and Shading need.
 *
 * Example: the triangle (0, 0), (4, 0), (0, 4), on a 4x4 screen. Its
 * area is edge v0 v1 v2 = 4 * 4 - 0 * 0 = 16. At pixel (1, 1), center
 * (1.5, 1.5): w0 = 4, w1 = 6, w2 = 6, all positive, inside, with
 * weights 0.25, 0.375, 0.375. At pixel (3, 3), center (3.5, 3.5):
 * w0 = -12, outside. It covers the 10 pixels with x + y <= 3, including
 * the 4 whose centers are exactly on the long edge (w0 = 0):
 *
 *     # # # #
 *     # # # .
 *     # # . .
 *     # . . .
 *
 * Which triangle gets a pixel exactly on an edge shared by two
 * triangles (w = 0 for both)? A rectangle is drawn as 2 triangles
 * sharing a diagonal, a mesh as thousands sharing edges: a pixel
 * drawn by both is drawn twice (wrong with transparency, and wasted
 * work), a pixel drawn by neither is a hole, a "crack". Two answers:
 *
 *  - [Epsilon]: count w >= -epsilon as inside, so an edge pixel is
 *    drawn by both triangles, even when floating-point rounding makes
 *    both compute a tiny negative w instead of 0 (the crack). Simple,
 *    invisible when opaque, but pixels drawn twice.
 *
 *  - [Top_left], what GPUs do: an edge pixel belongs to the triangle
 *    for which that edge is a "top" edge (horizontal, the triangle
 *    below it) or a "left" edge (the triangle to its right), so to
 *    exactly one of the two, whose edges are the same line seen from
 *    opposite sides:
 *
 *        +-------+       the shared diagonal is a left edge of A (A is
 *        |\   A  |       on its right), a right edge of B: its pixels
 *        |  \    |       are A's; the rectangle's top edge is A's top,
 *        | B  \  |       its left edge B's left: drawn; its bottom and
 *        |      \|       right edges aren't (they're the top and left
 *        +-------+       of the rectangles below and to the right)
 *
 *    For the rule to work, w must be *exactly* 0 on a shared edge,
 *    for both triangles: computed from the same 2 vertices, in the
 *    opposite order, their values must be exact opposites. Floating
 *    point doesn't guarantee that (it rounds), unless the numbers are
 *    simple enough to need no rounding: so the vertices are first
 *    snapped to a grid of 1/256th of a pixel ("sub-pixel precision",
 *    fixed-point coordinates in hardware), after which every edge
 *    function value is an exact multiple of 1/65536, computed without
 *    any rounding.
 *
 * References:
 * - Juan Pineda, "A Parallel Algorithm for Polygon Rasterization",
 *   SIGGRAPH 1988.
 * - Fabian Giesen, "The barycentric conspiracy" and "Optimizing the
 *   basic rasterizer", blog posts, 2013 (the top-left rule, sub-pixel
 *   precision, incremental edge functions). *)

(* How pixels exactly on an edge are decided, see above *)
type fill_rule = Epsilon | Top_left

(* [fill fb ~zbuffer ~fill_rule ~interpolation ~shading ~color v0 v1 v2]:
 * the pixels of the triangle, each colored [color ~u ~v ~brightness] (a
 * 0xRRGGBB color; u, v from [interpolation], brightness from
 * [shading]). With a [zbuffer], a pixel is drawn only if nearer than
 * what's there (see Zbuffer); without, always: the painter's
 * algorithm, see Painter. *)
val fill :
  Framebuffer.t ->
  fill_rule:fill_rule ->
  zbuffer:Zbuffer.t option ->
  interpolation:Interpolate.mode ->
  shading:Shading.mode ->
  color:(u:float -> v:float -> brightness:float -> int) ->
  Project.vertex ->
  Project.vertex ->
  Project.vertex ->
  unit

(* [outline fb ~rgb v0 v1 v2]: only the triangle's 3 edges, as lines
 * (graphics/2d/Line.draw: clipped to the framebuffer, then Bresenham's
 * algorithm): wireframe mode, deliberately much simpler than [fill]
 * (no edge functions, no interpolation, no z-buffer: all the edges are
 * drawn, even hidden ones) *)
val outline : Framebuffer.t -> rgb:int -> Project.vertex -> Project.vertex -> Project.vertex -> unit
