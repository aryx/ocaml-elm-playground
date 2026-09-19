(* Collision response: two bodies touch (a Contact, from Collide), now
 * what? (see notes_2d_physics.md section 10). A force would take many
 * steps to push them apart; a collision takes no time at all. So the
 * velocities change *instantly*, by an impulse j along the contact's
 * normal n, equal and opposite (Newton's third law):
 *
 *        a            b
 *      (   )  n --> (   )      a's velocity  -= j n / m_a
 *    <-- j        j -->        b's velocity  += j n / m_b
 *
 * so momentum is conserved exactly, by construction, whatever j.
 *
 * How big? Just enough to turn the speed at which they approach along
 * n, v_rel . n (v_rel = b's velocity - a's, negative when closing),
 * into -e times that: e is the restitution (Newton's experimental law
 * of impact, the "bounciness"), 1 bouncing back as fast (a superball),
 * 0 not at all (clay). Solving for j:
 *
 *      j = -(1 + e) (v_rel . n) / (1/m_a + 1/m_b)
 *
 * Example: two balls of mass 1, the first at 2 m/s into the second, at
 * rest, n = (1, 0), v_rel . n = -2:
 *
 *      e = 1:    j = 2    velocities after: 0 and 2    (Newton's cradle)
 *      e = 0:    j = 1                      1 and 1    (stuck together)
 *      e = 0.5:  j = 1.5                    0.5 and 1.5
 *
 * The momentum is 2 before and after, every time; the kinetic energy,
 * 2 before, is 2, 1 and 1.25 after: only e = 1 keeps it.
 *
 * An immovable body (a wall, the floor, a paddle the player moves) has
 * an infinite mass: 1/m = 0, it gets none of the impulse, and the
 * other body bounces off it with all of it -- no special case needed.
 *
 * Rotation (section 11): the impulse pushes at the contact point, r
 * away from each body's center, so it also spins the body, by
 * (r x j n) / I -- the torque's lever arm: the same push spins a door
 * more at the handle. The speeds that count are those of the touching
 * points (Body.point_velocity), and each body resists the impulse by
 * its mass *and* by its inertia, so the denominator gains the terms
 * (r x n)^2 / I:
 *
 *      j = -(1 + e) (v_rel . n) / (1/m_a + 1/m_b + (r_a x n)^2 / I_a + (r_b x n)^2 / I_b)
 *
 * Example: a ball of mass 1 (not turning) moving up at 1 into the end
 * of a stick at rest, of mass 1, length 2 (I = m L^2 / 12 = 1/3), e = 1:
 *
 *                  ^ the stick goes up at 0.4, spinning at 1.2
 *     ===========*=         (counterclockwise: its right end up)
 *                ^ ball, 1 -> 0.6
 *
 *   r_b = (1, 0), n = (0, 1): r_b x n = 1, j = 2 / (1 + 1 + 3) = 0.4;
 *   the ball keeps 0.6, the stick moves at 0.4 and spins at 1 * 0.4 *
 *   3 = 1.2 radians per second. The energy, 0.5 before, is 0.18 + 0.08
 *   + 0.24 after (the last in the spin): 0.5, kept. Hit at its middle
 *   (r_b x n = 0), the stick would take all the speed, like a ball.
 *
 * Two more pieces make it look right:
 *
 * - Friction (Coulomb's law): a second impulse, along the contact's
 *   tangent, against the sliding, but at most mu times the normal
 *   impulse j: a ball hitting a moving paddle is dragged along with it
 *   (Pong's "english"), up to a point; a ball sliding on the floor
 *   starts rolling (friction at its bottom point spins it).
 *
 * - Positional correction: when a collision is found, the bodies
 *   already overlap (by the contact's depth). The impulse stops them
 *   approaching but doesn't separate them, so resting bodies would
 *   slowly sink into each other: push them apart too, the lighter one
 *   more.
 *
 * Not here yet: a velocity under which restitution is ignored (a ball
 * resting on the floor keeps making tiny bounces, invisible), and
 * iterating over many contacts at once (sequential impulses, for
 * stacks: phase 8).
 *
 * References: Chris Hecker, "Physics, Part 3: Collision Response",
 * Game Developer, 1997; David Baraff, "Physically Based Modeling:
 * Rigid Body Simulation", SIGGRAPH course notes, 2001; Erin Catto, Box2D
 * Lite, 2006 (the friction clamp, the positional correction). *)

(* 1 / mass: 0 for an immovable body (an infinite mass) *)
val inverse_mass : Body.t -> float

(* 1 / inertia: 0 for a body that never turns *)
val inverse_inertia : Body.t -> float

(* [impulse ~restitution a b contact]: j above, the size of the
 * impulse along the contact's normal (from a to b), at its point; 0
 * when they're already moving apart there (nothing to do: they
 * touched, but are separating), or both are immovable *)
val impulse : restitution:float -> Body.t -> Body.t -> Contact.t -> float

(* [apply j dir point (a, b)]: the impulse j along [dir], at [point],
 * given to b, and -j to a: each one's velocity changed by it divided
 * by its mass, its spin by its torque divided by its inertia *)
val apply : float -> Vec2.t -> Vec2.t -> Body.t * Body.t -> Body.t * Body.t

(* [bounce ~restitution ~friction (a, b) contact]: the new velocities:
 * the normal impulse, then friction's, at most [friction] times it *)
val bounce : restitution:float -> friction:float -> Body.t * Body.t -> Contact.t -> Body.t * Body.t

(* [separate ?percent (a, b) contact]: the new positions, moved apart
 * along the normal by [percent] (1. by default) of the depth, shared
 * in proportion to their inverse masses (an immovable body doesn't
 * move, the other one takes all of it) *)
val separate : ?percent:float -> Body.t * Body.t -> Contact.t -> Body.t * Body.t

(* both: [bounce], then [separate] *)
val resolve : restitution:float -> friction:float -> Body.t * Body.t -> Contact.t -> Body.t * Body.t
