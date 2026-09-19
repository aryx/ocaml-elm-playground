(* Springs between bodies: chains, soft things (see notes_2d_physics.md
 * section 6).
 *
 * Force.spring pulls one body towards a fixed point; here a spring ties
 * two bodies, with a rest length:
 *
 *      a o/\/\/\/\/\o b      stretched by x = length - rest: each end
 *        --> f     f <--     pulled towards the other by k x (Hooke,
 *                            1678), equal and opposite (Newton's third
 *                            law: momentum kept)
 *
 * plus damping, a force against the ends' relative speed along the
 * spring, so it settles instead of bouncing forever: c (v_b - v_a) . u,
 * u the unit vector from a to b. A body of infinite mass is pinned (a
 * ceiling): the spring pulls on it, and it doesn't move.
 *
 * Example: a spring of stiffness k = 4 and rest length 1 between two
 * bodies of mass 1, 2 apart, still: stretched by 1, each pulled
 * towards the other by 4, an acceleration of 4.
 *
 * The catch: a stiff spring oscillates fast, and a time step too long
 * for its oscillation makes the simulation explode. For semi-implicit
 * Euler, a mass on a spring stays bounded only if dt < 2 / sqrt (k / m)
 * (its stability limit): at 60 steps per second, k / m < 14,400 (and
 * near it, bounded but overshooting: the swings grow to 1 / sqrt (1 -
 * (w dt / 2)^2) of the first one, w = sqrt (k / m); 1.81 at k / m =
 * 10,000). Past it, the energy grows at every step, and the chain
 * flies apart (examples/Elastic.ml, key x). Stiff things need smaller
 * steps, implicit integrators, or constraints instead of springs
 * (Particles.mli).
 *
 * References: Hooke, De Potentia Restitutiva, 1678; Andrew Witkin and
 * David Baraff, "Physically Based Modeling", SIGGRAPH course notes, 1997
 * and 2001 (particle systems, springs, damping, stiffness). *)

type spring = {
  (* the bodies at its ends, their indices in the array *)
  a : int;
  b : int;
  rest : float;
  (* its stiffness k: the force per unit of stretch *)
  k : float;
  (* its damping c: the force per unit of relative speed *)
  damping : float;
}

(* [accelerations bodies springs]: each body's acceleration from all the
 * springs (0 for a pinned one) *)
val accelerations : Body.t array -> spring list -> Vec2.t array

(* [step ~gravity ~dt bodies springs]: one step of semi-implicit Euler
 * for them all, the springs and gravity (an acceleration, e.g. (0,
 * -800)) pushing; the pinned bodies don't move *)
val step : gravity:Vec2.t -> dt:float -> Body.t array -> spring list -> Body.t array

(* [chain ~from ~towards n ~k ~damping]: [n] bodies of mass 1 in a
 * line from [from] to [towards], the first pinned, each tied to the
 * next by a spring at rest *)
val chain : from:Vec2.t -> towards:Vec2.t -> int -> k:float -> damping:float -> Body.t array * spring list
