(* 2D affine transformations: moves (translations), rotations, scalings,
 * and any combination of them.
 *
 * Every transformation here maps a point (x, y) to
 *
 *   x' = a*x + c*y + tx
 *   y' = b*x + d*y + ty
 *
 * i.e. a 2x2 matrix [a c; b d] (which rotates/scales/flips) followed by
 * a translation (tx, ty). Written as a 3x3 matrix acting on (x, y, 1),
 * a trick called "homogeneous coordinates", the translation becomes
 * part of the matrix too:
 *
 *   | x' |   | a  c  tx |   | x |
 *   | y' | = | b  d  ty | * | y |
 *   | 1  |   | 0  0  1  |   | 1 |
 *
 * so that *every* transformation, translations included, is a matrix,
 * and doing one transformation after another is just multiplying their
 * matrices ([compose]). That's what makes a Playground [group] cheap:
 * moving a group of 100 shapes multiplies one matrix, not 100 shapes.
 *
 * References:
 * - Lawrence G. Roberts, "Homogeneous Matrix Representation and
 *   Manipulation of N-Dimensional Constructs", MIT Lincoln Laboratory
 *   MS-1405, 1965 (homogeneous coordinates for computer graphics).
 * - Ivan E. Sutherland, "Sketchpad: A Man-Machine Graphical
 *   Communication System", MIT PhD thesis, 1963 (drawings made of
 *   transformed instances of other drawings -- the ancestor of
 *   Playground's [group]).
 * - Foley, van Dam, Feiner, Hughes, "Computer Graphics: Principles and
 *   Practice", 2nd ed., 1990, chapter 5 (the textbook treatment).
 *)

type t = { a : float; b : float; c : float; d : float; tx : float; ty : float }

(* (x, y) -> (x, y) *)
val identity : t

(* [translate dx dy]: (x, y) -> (x + dx, y + dy) *)
val translate : float -> float -> t

(* [rotate radians]: counterclockwise around (0, 0), in a y-up world
 * like Elm's; e.g. rotate (pi/2) maps (1, 0) to (0, 1) *)
val rotate : float -> t

(* [scale sx sy]: (x, y) -> (sx * x, sy * y); e.g. scale 1. (-1.)
 * flips upside down *)
val scale : float -> float -> t

(* [compose m n] is "n, then m", like function composition (m o n):
 *   apply (compose m n) p = apply m (apply n p)
 * The order matters: rotating then moving is not moving then rotating.
 * For example, for the point (1, 0):
 *   compose (translate 10. 0.) (rotate (pi/2)):  (1, 0) -> (0, 1) -> (10, 1)
 *   compose (rotate (pi/2)) (translate 10. 0.):  (1, 0) -> (11, 0) -> (0, 11)
 *)
val compose : t -> t -> t

val apply : t -> float * float -> float * float
