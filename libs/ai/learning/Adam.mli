(* Walking downhill with a memory: Adam (notes_ai_learning.md
 * section 6).
 *
 * Plain gradient descent ([Backprop.step]) moves every weight by its
 * slope times one rate:
 *
 *     w  <-  w - rate * g
 *
 * and one rate is wrong for most of them. In a long narrow valley the
 * slope across is a hundred times the slope along it: a rate small
 * enough not to bounce off the walls crawls along the floor, and the
 * floor is where the minimum is.
 *
 * Adam keeps two running averages per weight and divides one by the
 * other:
 *
 *     m  <-  b1 m + (1 - b1) g          where it has been going
 *     v  <-  b2 v + (1 - b2) g^2        how big its slopes are
 *
 *                       m / (1 - b1^t)
 *     w  <-  w - rate ------------------------
 *                     sqrt (v / (1 - b2^t)) + epsilon
 *
 * [m] is momentum: slopes that keep their sign add up, slopes that
 * flip from step to step (the bouncing across the valley) cancel. [v]
 * is each weight's own scale: dividing by its root makes the step
 * about [rate] whatever the size of the slope, so the steep direction
 * and the flat one advance at the same pace. The divisions by
 * [1 - b^t] undo the averages' start at zero, which would otherwise
 * make the first steps too small ([t] is the step's number).
 *
 * Example: the first step is always [rate] long, towards where the
 * slope points down, whatever the slope's size -- m is (1 - b1) g
 * and v is (1 - b2) g^2, the corrections remove the two factors, and
 * g / sqrt (g^2) is its sign:
 *
 *     slopes   [| 300.;  -0.002 |]      rate 0.1
 *     weights  [| 1.;     1.    |]  ->  [| 0.9;  1.1 |]
 *
 * Measured on Rosenbrock's valley, (1 - x)^2 + 100 (y - x^2)^2, from
 * (-1.2, 1), the minimum 0 at (1, 1), 2000 steps each at the largest
 * rate that works (Unit_adam):
 *
 *     plain descent, rate 0.001     ends at a loss of   0.078
 *     Adam, rate 0.02                                   0.00078
 *
 * and plain descent at Adam's rate leaves the valley altogether.
 *
 * It works on arrays of numbers and knows nothing of networks: the
 * weights of a [Net], the values of a [Grad] graph, anything with a
 * slope.
 *
 * References: Diederik Kingma, Jimmy Ba, "Adam: A Method for
 * Stochastic Optimization", 2014; Boris Polyak, "Some methods of
 * speeding up the convergence of iteration methods", 1964 (momentum);
 * Tijmen Tieleman, Geoffrey Hinton, RMSProp, 2012 (the division, from
 * a lecture's slide). *)

(* the two averages per weight, and the steps taken *)
type t

(* [make n]: for [n] weights, before any step. The defaults are the
 * paper's: rate 0.001, b1 0.9, b2 0.999, epsilon 1e-8. *)
val make : ?rate:float -> ?b1:float -> ?b2:float -> ?epsilon:float -> int -> t

(* [step ?rate a weights slopes]: one step, the averages and the
 * weights after it. Nothing is changed in place. [rate] replaces
 * [make]'s for this step: a rate that shrinks as training goes is
 * the caller's schedule. *)
val step : ?rate:float -> t -> float array -> float array -> t * float array

(* the steps taken so far *)
val steps : t -> int
