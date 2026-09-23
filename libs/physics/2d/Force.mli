(* What pushes and pulls: forces, as the accelerations they give (see
 * notes_2d_physics.md section 6).
 *
 * Newton's second law, F = m a, turned around: an integrator needs the
 * acceleration a = F / m, a function of the body's position (and
 * velocity, for drag), the same at every step. For gravity the mass
 * cancels out (Galileo: a heavy and a light ball fall together); for a
 * spring it doesn't, hence the k / m below.
 *
 *     uniform              gravitation              spring
 *        |  |  |               \  |  /               anchor
 *        v  v  v             ---  *  ---               |
 *     the same             towards the star,           /\/\/\ o
 *     everywhere           1 / r^2                     pulled back,
 *                                                      proportional to x
 *
 * References: Newton, Philosophiae Naturalis Principia Mathematica,
 * 1687 (the laws of motion, universal gravitation); Hooke, De Potentia
 * Restitutiva, 1678 ("ut tensio, sic vis": as the extension, so the
 * force). *)

(* an acceleration as a function of position and velocity *)
type t = Vec2.t -> Vec2.t -> Vec2.t

(* none: a = (0, 0), a body keeps its velocity (the first law) *)
val none : t

(* [uniform g]: the same acceleration everywhere, e.g. gravity near the
 * ground, (0, -g) *)
val uniform : Vec2.t -> t

(* [gravitation ~gm ~center]: Newton's gravitation towards a mass M
 * fixed at [center], with gm = G M: an acceleration of gm / r^2 towards
 * it, r the distance. A body at distance r needs the speed
 * sqrt (gm / r) to stay on a circular orbit. Example: gm = 1,000,000,
 * r = 100 pixels: a pull of 100 px/s^2, a circular speed of 100 px/s,
 * one orbit in 2 pi r / v = 6.28 s. *)
val gravitation : gm:float -> center:Vec2.t -> t

(* [spring ~k_over_m ~anchor]: Hooke's law, a pull back towards
 * [anchor], proportional to the distance from it: -(k / m) (pos -
 * anchor). A mass on such a spring oscillates with the period
 * 2 pi / sqrt (k / m) (with k / m = 1, 6.28 s). *)
val spring : k_over_m:float -> anchor:Vec2.t -> t

(* [drag ~c]: linear drag, -c vel: a pull against the motion,
 * proportional to the speed (a body moving slowly through a thick
 * fluid; air at speed is closer to quadratic, -c |vel| vel). Under a
 * constant push a, a body then reaches a top speed, a / c, where the two
 * balance. Example: c = 0.5 and gravity 10: a terminal speed of 20. *)
val drag : c:float -> t

(* [sum forces]: forces add up (the force accumulator of every engine) *)
val sum : t list -> t
