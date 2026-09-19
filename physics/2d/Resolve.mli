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
 * Two more pieces make it look right:
 *
 * - Friction (Coulomb's law): a second impulse, along the contact's
 *   tangent, against the sliding, but at most mu times the normal
 *   impulse j: a ball hitting a moving paddle is dragged along with it
 *   (Pong's "english"), up to a point. (Without rotation, the ball
 *   doesn't spin: phase 7 of the plan.)
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

(* [impulse ~restitution a b normal]: j above, the size of the impulse
 * along [normal] (from a to b); 0 when they're already moving apart
 * (nothing to do: they touched, but are separating), or both are
 * immovable *)
val impulse : restitution:float -> Body.t -> Body.t -> Vec2.t -> float

(* [apply j dir (a, b)]: the impulse j along [dir] given to b, and -j
 * to a, each divided by its mass *)
val apply : float -> Vec2.t -> Body.t * Body.t -> Body.t * Body.t

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
