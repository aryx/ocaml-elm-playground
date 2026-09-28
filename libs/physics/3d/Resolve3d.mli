(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Two bodies touch (a Contact3d, from Collide3d), now what? (see
 * notes_3d_physics.md section 9). The answer is physics/2d/Resolve's,
 * and that .mli is where the *idea* is explained: a collision takes no
 * time, so the velocities change instantly, by an impulse j along the
 * contact's normal, equal and opposite, which conserves momentum
 * whatever j is; and j is whatever it takes to turn the closing speed
 * into -e times itself.
 *
 *      j = -(1 + e) (v_rel . n) / (1/m_a + 1/m_b + the lever arms)
 *
 * What a dimension changes is the lever arms. In 2D a body resists a
 * turn by one number and the arm's term is (r x n)^2 / I. In 3D the
 * inertia is a matrix that turns with the body (Mat3.mli), and the
 * same term becomes
 *
 *      n . ( (I^-1 (r x n)) x r )
 *
 * which is the tensor sandwiched between two cross products, and
 * reduces to the 2D one when everything is flat. [resistance] is that
 * whole denominator; 1 / it is the "effective mass" the impulse sees.
 *
 * Both conservation laws hold *by construction*, not by luck: the two
 * bodies get the same impulse with opposite signs, at the same point
 * in the world, so the linear momentum cancels, and so does the
 * angular momentum about any point at all (the change to each body is
 * p x J with p the contact point). The tests check both over a
 * thousand random collisions, and the energy, which only e = 1 keeps.
 *
 * {2 Friction is a pyramid, not a cone}
 *
 * In 2D the tangent is a single direction and friction is one impulse
 * clamped to mu j. In 3D the tangent plane is two-dimensional, and the
 * honest limit is a *cone*: the friction impulse may point any way
 * across the normal, with magnitude at most mu j. Picking two
 * perpendicular tangents and clamping each to mu j is easier, and it
 * is what every engine ships -- but it is a square where the truth is
 * a circle:
 *
 *        t2                     the clamp lets friction reach
 *        |   . - .              mu j sqrt 2 along the diagonals,
 *      +-|-+' . . `+--          about 41% too much, and the square's
 *      | | |  cone |            corners point in whichever directions
 *      | +-|-------|-- t1       [tangents] happened to pick
 *      +---+       |
 *        pyramid   '
 *
 * So a box sliding diagonally across a floor grips a little harder
 * than one sliding along a tangent, by up to 41%, and which way those
 * tangents lie is an implementation detail of [tangents]. Nobody
 * notices in a game; it is worth knowing when a measurement disagrees
 * with a textbook.
 *
 * {2 What is not here}
 *
 * One contact at a time, resolved once. A pile of boxes needs the same
 * contact solved again and again while its neighbours move (sequential
 * impulses, warm starting), a manifold of up to four points per pair
 * rather than one, and a velocity under which restitution is ignored,
 * or a resting body bounces forever in miniature. That is phase 8
 * ([Solver3d]); this module is one collision, done properly.
 *
 * References: Chris Hecker, "Physics, Part 3: Collision Response"
 * (Game Developer, 1997); David Baraff and Andrew Witkin, "Physically
 * Based Modeling" (SIGGRAPH course notes), where the tensor form of
 * the denominator is derived; Erin Catto, Box2D Lite (2006), for the
 * friction clamp and the positional correction; Coulomb (1785) for the
 * friction law itself. *)

(* 1 / mass: 0 for an immovable body (an infinite mass), so it takes
 * none of the impulse and no special case is needed *)
val inverse_mass : Body3d.t -> float

(* I^-1 in the *world* frame, R I^-1 R^T: the zero matrix for a body
 * that never turns *)
val inverse_inertia : Body3d.t -> Mat3.t

(* [relative_velocity a b point]: how fast b's material point at
 * [point] moves away from a's -- the velocities of the touching
 * points, spin included, not of the centres *)
val relative_velocity : Body3d.t -> Body3d.t -> Vec3.t -> Vec3.t

(* the denominator above: how the pair resists an impulse along [dir]
 * at [point] *)
val resistance : Body3d.t -> Body3d.t -> Vec3.t -> Vec3.t -> float

(* [impulse ~restitution a b contact]: j, along the contact's normal
 * (which points from a to b). 0 when they are already separating
 * there, or when neither body can move. *)
val impulse : restitution:float -> Body3d.t -> Body3d.t -> Contact3d.t -> float

(* [apply j dir point (a, b)]: j along [dir] given to b at [point], and
 * -j to a -- each velocity changed by it over the mass, each spin by
 * the tensor's inverse times r x the impulse *)
val apply : float -> Vec3.t -> Vec3.t -> Body3d.t * Body3d.t -> Body3d.t * Body3d.t

(* two perpendicular directions across a normal: the tangent plane's
 * axes, and the pyramid's -- see above *)
val tangents : Vec3.t -> Vec3.t * Vec3.t

(* [bounce ~restitution ~friction (a, b) contact]: the new velocities
 * and spins -- the normal impulse, then friction along each tangent,
 * each clamped to [friction] times it *)
val bounce : restitution:float -> friction:float -> Body3d.t * Body3d.t -> Contact3d.t -> Body3d.t * Body3d.t

(* [separate ?percent (a, b) contact]: the new positions, pushed apart
 * along the normal by [percent] (1 by default) of the depth, shared in
 * proportion to their inverse masses. Without it, bodies found
 * overlapping stay overlapping: an impulse stops them approaching, it
 * does not take them back out. *)
val separate : ?percent:float -> Body3d.t * Body3d.t -> Contact3d.t -> Body3d.t * Body3d.t

(* both, in that order *)
val resolve : restitution:float -> friction:float -> Body3d.t * Body3d.t -> Contact3d.t -> Body3d.t * Body3d.t
