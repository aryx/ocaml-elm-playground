(* Ps_graphics: the geometry under PostScript's drawing operators.
 *
 * Three ideas, and they are the ones every 2D graphics system since
 * has kept (PDF, Quartz, Cairo, the HTML canvas, SVG):
 *
 * - **the current transformation matrix** (CTM). A program draws in
 *   its own coordinates, "user space"; the CTM takes them to the
 *   device's. translate, scale and rotate change the CTM, not the
 *   drawing, so a procedure that draws a star at the origin draws it
 *   anywhere, any size, at any angle. Six numbers, [a b c d tx ty]:
 *
 *       x' = a x + c y + tx
 *       y' = b x + d y + ty
 *
 *   and translate, scale, rotate are each such a matrix, composed with
 *   the CTM ([concat]): the new one is applied first, then the old;
 *
 * - **the path**: moveto, lineto, curveto build an outline, and
 *   nothing is drawn until fill or stroke paints it. Its points are
 *   kept already transformed (device space), so a path can be built
 *   across several changes of the CTM;
 *
 * - **Bezier curves** (Pierre Bezier at Renault, Paul de Casteljau at
 *   Citroen, around 1960): curveto's four points, the curve starting
 *   toward the second and arriving from the third. Transforming the
 *   four points transforms the curve -- an affine map commutes with
 *   the curve's weighted averages -- which is why the path can hold
 *   them in device space. A circle is not a Bezier, but a quarter of
 *   one is very nearly: [arc] makes each quarter from four points,
 *   the middle two at k = 4/3 (sqrt 2 - 1) = 0.5523 of the radius
 *   along the tangents -- chosen so that the curve's midpoint is on
 *   the circle, and it strays at most 0.027% of the radius on either
 *   side of it (checked by the tests). And a device draws straight
 *   lines, so [flatten] cuts a curve in two at its middle (de
 *   Casteljau's construction, averages of averages) until each piece
 *   is flat enough to be one line. *)

(* [a b c d tx ty] *)
type matrix = { a : float; b : float; c : float; d : float; tx : float; ty : float }

val identity : matrix
val translation : float -> float -> matrix
val scaling : float -> float -> matrix

(* degrees, counterclockwise *)
val rotation : float -> matrix

(* [concat m ctm]: [m] then [ctm], what translate, scale and rotate do
 * to the CTM *)
val concat : matrix -> matrix -> matrix

val transform : matrix -> float * float -> float * float

(* a distance, not a point: the matrix without its translation *)
val dtransform : matrix -> float * float -> float * float

(* the matrix undone (currentpoint answers in user space); the
 * identity for a matrix with no inverse *)
val invert : matrix -> matrix

(* how much a length grows, on average: the square root of the area's
 * growth, for a line's width *)
val scale_of : matrix -> float

type point = float * float
type segment = Move of point | Line of point | Curve of point * point * point | Close

(* [arc (cx, cy) r a1 a2 ~clockwise]: Bezier curves of at most 90
 * degrees each from angle [a1] to [a2] (degrees), counterclockwise as
 * arc draws or clockwise as arcn; the first element is the arc's
 * starting point *)
val arc : point -> float -> float -> float -> clockwise:bool -> point * (point * point * point) list

(* the path as polylines, each with whether it was closed; curves cut
 * until no piece strays more than [tolerance] from its chord *)
val flatten : ?tolerance:float -> segment list -> (point list * bool) list
