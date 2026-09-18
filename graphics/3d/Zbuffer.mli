(* The z-buffer (depth buffer): for every pixel, the depth of the
 * nearest thing drawn there so far. A new pixel is drawn only if it is
 * nearer; then its depth replaces the old one. So the triangles can be
 * drawn in any order, and even intersect, and each pixel still ends
 * up showing the nearest surface. See notes_3d.md section 6.
 *
 * Example: two overlapping triangles, A at depth 5 and B at depth 3
 * (nearer); whichever is drawn first, the overlap shows B:
 *
 *    A then B:               B then A:
 *    . . . . . . .           . . . . . . .
 *    . A A A . . .           . A A A . . .
 *    . A B B B B .           . A B B B B .    in the overlap:
 *    . A B B B B .           . A B B B B .    A first: 5 < inf, drawn;
 *    . . B B B B .           . . B B B B .      then B: 3 < 5, drawn
 *                                             B first: 3 < inf, drawn;
 *                                               then A: 5 > 3, not
 *
 * The cost: a number per pixel, and a comparison per pixel drawn. The
 * alternative, sorting the faces (the painter's algorithm, see
 * PaintersAlgorithmFail3d), costs less memory but can't be right for
 * intersecting faces; memory got cheap, and the z-buffer won (GPUs
 * have one in hardware).
 *
 * References:
 * - Edwin Catmull, "A Subdivision Algorithm for Computer Display of
 *   Curved Surfaces", PhD thesis, University of Utah, 1974.
 * - Wolfgang Straßer, "Schnelle Kurven- und Flächendarstellung auf
 *   grafischen Sichtgeräten", PhD thesis, TU Berlin, 1974 (the same
 *   idea, independently). *)

type t

(* all pixels at an infinite depth: anything drawn is nearer *)
val create : width:int -> height:int -> t

(* back to all infinite, before drawing a new frame *)
val clear : t -> unit

(* [test_and_set zbuffer ~x ~y z]: is [z] nearer than what's at pixel
 * (x, y)? If so, it becomes the pixel's depth. (x, y) must be on the
 * screen: not checked, it's called for every pixel drawn. *)
val test_and_set : t -> x:int -> y:int -> float -> bool
