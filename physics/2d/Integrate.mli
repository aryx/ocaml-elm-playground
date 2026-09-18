(* One time step: from a body's state at time t, and the acceleration
 * the forces give it, its state at t + dt. Newton's laws say how
 * position and velocity *change*, continuously; a computer takes small
 * steps instead, and each way of stepping makes its own small error --
 * which, over thousands of steps, is the whole difference between a
 * planet staying on its orbit and a planet flying away. Four methods,
 * each a few lines, to compare (notes_2d_physics.md sections 4-5).
 *
 * Worked example: a ball thrown up at 10 m/s, g = 10 m/s^2, dt = 0.1 s.
 * After 0.5 s the exact height is 10 * 0.5 - 5 * 0.5^2 = 3.75 m; after 5
 * steps:
 *
 *   step   explicit Euler    semi-implicit Euler
 *          v     y           v     y
 *     1    9     1.0         9     0.9
 *     2    8     1.9         8     1.7
 *     3    7     2.7         7     2.4
 *     4    6     3.4         6     3.0
 *     5    5     4.0         5     3.5        Verlet, RK4: 3.75
 *
 * explicit Euler 0.25 m too high, semi-implicit 0.25 m too low (both
 * "first order": halve dt, halve the error), Verlet and RK4 exact (a
 * constant acceleration is their easy case).
 *
 * Over a long run, what matters is energy. On a circular orbit (gm =
 * 1,000,000, r = 100, v = 100, dt = 1/60 s: 377 steps per orbit),
 * explicit Euler gains energy at every step: after one orbit the
 * radius is 120, after ten 190 -- the planet spirals out. Semi-implicit
 * Euler and Verlet are "symplectic": their energy error oscillates
 * instead of accumulating, the radius stays within 0.01% of 100 after
 * ten orbits. RK4 is much more accurate per step (four accelerations
 * per step), but not symplectic: over very long runs it drifts too.
 * Games (Box2D, Chipmunk) use semi-implicit Euler: as cheap as Euler,
 * and stable.
 *
 * References: Euler, Institutionum calculi integralis, 1768; Verlet,
 * "Computer 'Experiments' on Classical Fluids", Physical Review, 1967
 * (Stormer used it before him, 1907); Runge 1895 and Kutta 1901 (RK4);
 * Hairer, Lubich, Wanner, Geometric Numerical Integration, 2002 (why
 * symplectic methods keep orbits closed); Feynman, Lectures on
 * Physics, vol. 1, chapter 9 (an orbit stepped by hand). *)

(* the four methods, to switch between them *)
type method_ = Explicit_euler | Semi_implicit_euler | Verlet | Rk4

(* in that order, the simplest first *)
val methods : method_ list

(* e.g. "semi-implicit Euler" *)
val name : method_ -> string

(* [step m ~force ~dt body]: [body] dt seconds later, under [force],
 * with the method [m] *)
val step : method_ -> force:Force.t -> dt:float -> Body.t -> Body.t

(* The methods, each on its own. With a the acceleration at (pos, vel): *)

(* pos += vel dt, then vel += a dt: the obvious one; gains energy *)
val explicit_euler : force:Force.t -> dt:float -> Body.t -> Body.t

(* vel += a dt, then pos += vel dt (the *new* vel): the one games use *)
val semi_implicit_euler : force:Force.t -> dt:float -> Body.t -> Body.t

(* velocity Verlet: pos += vel dt + a dt^2 / 2, then vel += the average
 * of the old and the new accelerations times dt; second order, and
 * symplectic. (With a force depending on the velocity, like drag, the
 * new acceleration uses a predicted velocity, vel + a dt: then it's
 * only an approximation of the method.) *)
val verlet : force:Force.t -> dt:float -> Body.t -> Body.t

(* the classic fourth-order Runge-Kutta: the derivative (vel, a)
 * sampled at the start, twice in the middle, and at the end of the
 * step, averaged with weights 1, 2, 2, 1 *)
val rk4 : force:Force.t -> dt:float -> Body.t -> Body.t
