(* Sutherland's relaxation: making a sheet's constraints hold (Sketch;
 * Sketchpad, chapter V, 1963).
 *
 * Each constraint has an *error*, a distance that is zero when it
 * holds: a horizontal line's error is how far one end is above the
 * other, a point on a circle's how far it is from the rim. The sheet
 * is right when every error is zero, and the unknowns are the points'
 * coordinates.
 *
 * Solving all of them together is a nonlinear system. Sutherland's
 * method takes one point at a time instead, the others held still:
 *
 * 1. for each constraint on the point, its error e, and how the error
 *    changes when the point moves a little in x, then in y -- the
 *    derivatives a and b, found *numerically*, by trying: move it by
 *    h, look at the error again, (e' - e) / h. So a new kind of
 *    constraint is only an error function: nothing to derive by hand;
 *
 * 2. near where it is, each error is linear, e + a dx + b dy; the
 *    move (dx, dy) that makes the sum of their squares least solves
 *    two equations (the normal equations of least squares):
 *
 *      | sum a a   sum a b | | dx |      | sum a e |
 *      | sum a b   sum b b | | dy |  = - | sum b e |
 *
 * 3. move the point there, and go on to the next.
 *
 * One sweep over the points is not enough -- fixing one breaks its
 * neighbours a little -- but sweep again and again and the whole sheet
 * settles: this is Gauss-Seidel iteration, the "relaxation" of the
 * numerical analysts of the 1950s, the same as the physics' Solver
 * and Particles use for contacts and sticks. A constraint that cannot
 * hold with the others (overconstrained) does not stop it: the squares
 * are shared out, and the error stays above zero, which the program
 * can show.
 *
 * One constraint alone on a point does not fix where it goes (a point
 * on a line can slide along it): the equations are then singular, and
 * a tiny damping on the diagonal (Levenberg's, 1944) picks the
 * shortest move, straight to the line. So a constrained drawing moves
 * as little as it can. Two constraints nearly alike (a point on two
 * circles nearly tangent) make the equations nearly singular and the
 * move enormous: a move is kept no longer than the worst of the
 * point's errors, which is as far as it can need to go.
 *
 * Worked example: a horizontal line from (0, 0) to (100, 10), its
 * first end fixed. Its error is y2 - y1 = 10 and the derivative for
 * the free end is b = 1 (a = 0): dy = -(b e) / (b b) = -10, and the
 * end goes to (100, 0) in one step, its x unchanged. *)

(* the constraint's error on the sheet: 0 when it holds; lengths in the
   sheet's units (an angle as the offset it makes over the lines'
   mean length) *)
val error_of : Sketch.sheet -> Sketch.constr -> float

(* the sum of the constraints' |error| *)
val error : Sketch.sheet -> float

(* one sweep over the points not fixed, in order; the points [held]
   are not moved either (the one the pen drags) *)
val sweep : ?held:int list -> Sketch.sheet -> Sketch.sheet

(* [solve ~sweeps s]: up to [sweeps] sweeps, fewer once the error is
   under a thousandth *)
val solve : ?held:int list -> sweeps:int -> Sketch.sheet -> Sketch.sheet
