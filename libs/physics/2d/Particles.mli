(* Particles and sticks: ropes, cloth, ragdolls, the Hitman way
 * (Thomas Jakobsen, "Advanced Character Physics", GDC 2001; see
 * notes_2d_physics.md sections 4 and 12).
 *
 * Two ideas, each a few lines, and together the most stable soft-body
 * trick there is:
 *
 * - Position Verlet: a particle keeps its position and its previous
 *   one, and its velocity is just their difference: the next position
 *   is pos + (pos - old) + a dt^2 (Verlet, 1967, section 4). No
 *   velocity to keep in sync: move a particle, and its velocity follows.
 *
 * - Sticks instead of springs: a stick says "these two particles are
 *   [length] apart", and after each step every stick moves its two ends
 *   halfway each towards its length (all of it on the free end if the
 *   other is pinned). Fixing one stick breaks its neighbours a bit, so
 *   go over all of them a few times (relaxation, the same Gauss-Seidel
 *   as Solver's): the rope converges to its length.
 *
 *     too long:    a o-------------o b     each end moved by half the
 *     fixed:          a o--------o b       excess, towards the other
 *
 * The fix travels one stick per iteration, so a long rope converges
 * slowly: hanging under gravity, a rope of 20 sticks relaxed 20 times a
 * step stretches by a few percent, less with more iterations (the
 * stiffness is the iterations).
 *
 * Why better than stiff springs: a spring's force is multiplied by dt
 * into a velocity, and a stiff one overshoots and explodes (Springs.mli);
 * a stick sets positions directly, never overshoots, and is as stiff
 * as its iterations make it. The velocities come out right by
 * themselves, from the positions (Verlet).
 *
 * Example: a pendulum, one stick of length 100 from a pinned particle,
 * g = 800: a small swing takes 2 pi sqrt (L / g) = 2.22 seconds, like
 * a real pendulum's (Galileo, then Huygens, 1673).
 *
 * References: Thomas Jakobsen, "Advanced Character Physics", GDC 2001
 * (the Hitman: Codename 47 ragdolls); Loup Verlet, 1967. *)

type particle = {
  pos : Vec2.t;
  (* its position at the previous step: its velocity is pos - old *)
  old : Vec2.t;
  (* held in place: a nail, the hand holding the rope *)
  pinned : bool;
}

(* two particles, [length] apart *)
type stick = { a : int; b : int; length : float }

(* [particle ?pinned pos]: still (old = pos) *)
val particle : ?pinned:bool -> Vec2.t -> particle

(* [velocity ~dt p]: (pos - old) / dt *)
val velocity : dt:float -> particle -> Vec2.t

(* [step ?drag ~accel ~dt particles]: position Verlet, each free
 * particle to pos + (pos - old) (1 - drag) + accel dt^2 (drag, 0 by
 * default, a bit of air: 0.01 loses 1% of the speed per step) *)
val step : ?drag:float -> accel:Vec2.t -> dt:float -> particle array -> particle array

(* [relax ~iterations sticks particles]: every stick pulled back to its
 * length, [iterations] times over *)
val relax : iterations:int -> stick list -> particle array -> particle array

(* [keep_out ?friction polygons particles]: each free particle inside
 * one of the (convex or not) [polygons] pushed back to its outline's
 * nearest point: a ragdoll lying on the ground. Friction (0.3 by
 * default) takes that much of its velocity away, by moving its old
 * position towards the new one: a body landing slides, then stops. *)
val keep_out : ?friction:float -> Vec2.t list list -> particle array -> particle array

(* [rope ~from ~towards n]: [n] particles in a line, the first pinned,
 * each stuck to the next *)
val rope : from:Vec2.t -> towards:Vec2.t -> int -> particle array * stick list
