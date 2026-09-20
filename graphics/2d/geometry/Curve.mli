(* Curves: the cubic they all are, flattened into a polyline, and
 * walked at a constant speed.
 *
 * Nothing draws a curve. A rasterizer -- ours, Cairo, PostScript, a
 * GPU -- knows points and edges between them, so a curve is always
 * *flattened* first, cut into short straight pieces, exactly as
 * Circle's ellipses become polygons with enough sides. A curve
 * therefore lives twice here: as a formula giving the point at a
 * parameter t, and as the polyline it becomes.
 *
 * {1 The cubic, named two ways}
 *
 * A cubic curve is 4 points and a rule for mixing them. Two rules are
 * worth knowing, and they differ only in how their 4 points relate to
 * the curve they describe:
 *
 *   Bézier: the curve runs from p0 to p3, *pulled* towards p1 and p2
 *   without ever reaching them. The rule of fonts and drawing formats:
 *   PostScript, Type 1, TrueType (quadratic), SVG, Cairo.
 *
 *       p1 ......... p2        it leaves p0 heading for p1,
 *      .               .       and arrives at p3 coming from p2
 *     p0               p3
 *
 *   Catmull-Rom: the curve runs from p1 to p2, *through* them, and p0
 *   and p3 are merely their neighbors, there to give a direction at
 *   each: the tangent at p1 is p0 -> p2. The rule of animation and
 *   camera paths: you type the points you want to pass through, with
 *   no control points to place off the curve.
 *
 *       p1 --------- p2
 *      /               \
 *     p0               p3
 *
 * The two are the same family. The Catmull-Rom segment p1 -> p2 is the
 * Bézier curve with control points p1, p1 + (p2 - p0)/6, p2 - (p3 -
 * p1)/6, p2 ([cubic_of_catmull_rom]): a sixth of the neighbors' span
 * is exactly the pull that makes the tangents match. So one evaluator
 * would do for both; both are written out here because each is clearer
 * read in its own terms.
 *
 * {1 Flattening}
 *
 * Sampling a curve at n equal steps of t is the simple way, and what
 * [through] does; the trouble is choosing n: too few and a tight turn
 * shows its corners, too many and a gentle curve costs a hundred
 * segments for nothing.
 *
 * De Casteljau's answer ([flatten]) is to ask the curve. Cut it in two
 * halves (which his algorithm does with midpoints only, no powers of
 * t), and keep cutting until a half is so close to its own chord that
 * the chord will do:
 *
 *     p1 ------- p2        the control points p1, p2 within [tolerance]
 *      \         /         of the chord p0 -> p3? then draw the chord,
 *       p0 ~~~ p3          else split and ask the two halves
 *
 * A curve's flatness is thus measured, not guessed, and a straight
 * "curve" costs a single segment.
 *
 * {1 Walking a curve}
 *
 * The parameter t is not a distance: t from 0 to 1 crosses a long
 * segment as fast as a short one, so anything moving by t rushes and
 * dawdles. The fix is to flatten once, remember the length from the
 * start at each point ([measure]), and then move in *pixels*, looking
 * the position up in that table ([at]) -- the arc-length
 * parametrization, in its simplest form. It is a table because a
 * curve's length has no closed form worth having.
 *
 * References:
 * - Paul de Casteljau, at Citroën, 1959 (the subdivision algorithm),
 *   and Pierre Bézier, at Renault, 1962 (the curves, and the name):
 *   car bodies, both of them.
 * - Edwin Catmull and Raphael Rom, "A Class of Local Interpolating
 *   Splines", Computer Aided Geometric Design, 1974.
 * - Adobe Systems, "PostScript Language Reference Manual",
 *   Addison-Wesley, 1985 (curveto, and flattening with a tolerance).
 *
 * Users: kits/shmup/Path.ml (the curves shmup enemies fly along).
 * Depends on nothing, like its neighbors Vec2 and Affine.
 *)

type point = float * float

(* {1 Evaluating} *)

(* [cubic p0 p1 p2 p3 t]: the point at [t] (0 to 1) on the Bézier curve
 * from p0 to p3 pulled by p1 and p2:
 *   (1-t)^3 p0 + 3(1-t)^2 t p1 + 3(1-t) t^2 p2 + t^3 p3
 * At t = 0, p0; at t = 1, p3. E.g. with the two control points at the
 * same height, (0, 0) (0, 100) (100, 100) (100, 0), the middle of the
 * curve is (50, 75): three quarters of the way up, not all the way --
 * the curve is pulled, it doesn't reach. *)
val cubic : point -> point -> point -> point -> float -> point

(* [quadratic p0 p1 p2 t]: the same with one control point,
 *   (1-t)^2 p0 + 2(1-t) t p1 + t^2 p2
 * TrueType's curves (and so most of the text on a screen) are these.
 * E.g. (0, 0) (50, 100) (100, 0) is at (50, 50) in its middle: half
 * way up to its control point. *)
val quadratic : point -> point -> point -> float -> point

(* [catmull_rom p0 p1 p2 p3 t]: the point at [t] (0 to 1) on the curve
 * from p1 to p2, p0 and p3 their neighbors:
 *   0.5 (2 p1 + (p2 - p0) t + (2 p0 - 5 p1 + 4 p2 - p3) t^2
 *        + (3 p1 - p0 - 3 p2 + p3) t^3)
 * At t = 0, p1; at t = 1, p2. E.g. with points in a line, (0, 0) (100,
 * 0) (200, 0) (300, 0), the middle of the curve from the second to the
 * third is (150, 0); around a corner, (0, 0) (100, 0) (100, 100) (0,
 * 100), the middle from (100, 0) to (100, 100) is (112.5, 50): the
 * curve bulges out, smooth, rather than turning at the corner. *)
val catmull_rom : point -> point -> point -> point -> float -> point

(* [cubic_of_catmull_rom p0 p1 p2 p3] is the same curve as
 * [catmull_rom p0 p1 p2 p3], written as the control points of a
 * [cubic]: p1, p1 + (p2 - p0)/6, p2 - (p3 - p1)/6, p2 -- how to hand a
 * hand-typed path to something that speaks Bézier (a font, an SVG,
 * Cairo), or to [flatten] it. *)
val cubic_of_catmull_rom : point -> point -> point -> point -> point * point * point * point

(* {1 Flattening} *)

(* [flatten p0 p1 p2 p3]: the polyline of the Bézier curve, from p0 to
 * p3 included, by de Casteljau subdivision: split until each piece is
 * within [tolerance] (default 0.1, a tenth of a pixel) of its chord.
 * A curve whose control points sit on its chord is already straight,
 * and comes back as its two ends. *)
val flatten : ?tolerance:float -> point -> point -> point -> point -> point list

(* [through points]: the polyline of the smooth curve through
 * [points] (at least 2; the first and the last are their own
 * neighbors), Catmull-Rom sampled at [steps] points per segment
 * (default 16). Fixed steps rather than [flatten]'s measured ones:
 * cheap, and enough when the points are a screen apart. E.g. through
 * (0, 0) (100, 0) (200, 0), the 33 points of the straight line
 * between the ends. *)
val through : ?steps:int -> point list -> point list

(* {1 Walking} *)

(* a flattened curve and, at each of its points, the length from the
 * start: the table a walk reads *)
type t = { pts : point array; lengths : float array }

(* [measure polyline]: its points and the lengths along it (0 at the
 * first point) *)
val measure : point list -> t

(* its length: the last of [lengths], e.g. 300 for the polyline of
 * (0, 0) (100, 0) (200, 0) (300, 0) *)
val length : t -> float

(* [at c s]: where the curve is [s] pixels from its start, and the
 * direction it heads in there (radians, as Float.atan2 gives them):
 * between the two points around [s], in proportion. E.g. the 300-long
 * line above: (150, 0) at 150, heading 0. Before the start, or past
 * the end, the first or last point. *)
val at : t -> float -> point * float
