(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Stacking: every contact of a step, solved together (see
 * notes_3d_physics.md section 10). physics/2d/Solver's twin, and that
 * .mli is where the idea is laid out; the summary is that
 * [Resolve3d.bounce] fixes one contact at a time, and in a pile each
 * fix undoes a bit of another -- push the top crate up and the one
 * below goes down, and sinks into the floor. Solved once, a pile
 * jitters and sinks into itself.
 *
 * The fix is Erin Catto's sequential impulses (Box2D Lite, 2006): go
 * over all the contact points again and again ([iterations]),
 * correcting each one's impulse a little each time, and the whole pile
 * converges. It is Gauss-Seidel, the old method for linear systems,
 * applied to contacts. Two details make it work, and they are the same
 * two in any dimension:
 *
 *   - the impulses *accumulate* per contact point, and it is the sum
 *     that may never pull (>= 0): a later iteration can take back what
 *     an earlier one gave too much;
 *   - **warm starting**: a pile is still from one step to the next, so
 *     each point starts from its own impulses of the previous step,
 *     found again by where it is ([matching]). The iterations then
 *     only fix the difference.
 *
 *       step 1:  0 -> 0.5 -> 0.8 -> 0.95 ...    (iterations)
 *       step 2:  0.95 -> 0.99 -> ...            (warm started)
 *
 * Overlaps are not pushed apart any more ([Resolve3d.separate]): a
 * contact deeper than [slop] asks for a small separating speed, a
 * fraction [baumgarte] of its depth per step (Baumgarte, 1972), so a
 * pile rises out of the floor smoothly instead of popping. And
 * restitution only counts for contacts closing faster than
 * [bounce_threshold], or a crate resting on the floor bounces for ever
 * in miniature.
 *
 * {2 What 3D adds}
 *
 * Two things, both of them already met:
 *
 *   - **two tangents, not one** ([Resolve3d.tangents]): friction is
 *     solved twice per point per iteration, each within mu times the
 *     normal impulse, which is the pyramid Resolve3d.mli draws;
 *   - **up to four points per pair, not two** ([Collide3d.manifold]):
 *     a box resting on a box touches along a face, and holding it
 *     still needs every corner. That is what the face clipping is for,
 *     and without it a crate tips about its single contact point for
 *     ever.
 *
 * {2 What is left out}
 *
 * Sleeping is not here but in the layer above ([Physics3d.simulate]),
 * because it is about bodies rather than contacts: a body that has
 * been still for a while stops being stepped at all until something
 * touches it. Joints are phase 11.
 *
 * References: Erin Catto, "Iterative Dynamics with Temporal Coherence"
 * (GDC 2005), "Contact Manifolds" (GDC 2007) and Box2D Lite (2006,
 * 1000 lines, worth reading whole); J. Baumgarte, "Stabilization of
 * constraints and integrals of motion in dynamical systems" (1972). *)

type options = {
  (* how many times to go over the contacts per step *)
  iterations : int;
  (* start from the previous step's impulses *)
  warm_starting : bool;
  (* the fraction of an overlap corrected per step, and the overlap
   * tolerated (so that resting bodies stay in touch) *)
  baumgarte : float;
  slop : float;
  (* the closing speed under which bodies do not bounce *)
  bounce_threshold : float;
  (* how near a contact point must be to one of the previous step to
   * count as the same point *)
  matching : float;
}

(* 10 iterations, warm starting, baumgarte 0.2, and in metres: 5 mm of
 * slop, a metre a second of bounce threshold, 3 cm of matching *)
val default : options

(* two bodies touching, by their indices in the bodies' array, a < b,
 * with the contact points [Collide3d.manifold] found *)
type pair = { a : int; b : int; contacts : Contact3d.t list; restitution : float; friction : float }

(* the impulses of the last step's contact points, by pair *)
type memory

val nothing : memory

(* [solve options ~dt bodies pairs memory]: the bodies with their
 * velocities and spins changed by the contacts -- not their positions,
 * which the caller integrates afterwards with the new velocities --
 * and the memory for the next step's warm start *)
val solve : options -> dt:float -> Body3d.t array -> pair list -> memory -> Body3d.t array * memory

(* [impulses memory (a, b)]: the normal impulses of that pair's contact
 * points at the last step, for tests and for drawing *)
val impulses : memory -> int * int -> float list
