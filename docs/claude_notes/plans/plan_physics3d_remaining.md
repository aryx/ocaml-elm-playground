# Plan: what's left for the 3D physics engine

The engine and its games are done: see
[`done/plan_physics3d_teaching.md`](done/plan_physics3d_teaching.md)
(`physics/3d/`, phases 0-13: quaternions and tensors, integrators,
forces and buoyancy, collisions with the 15 axes, the broad phase,
impulses, stacking with sleeping by islands, rolling, the character
controller, continuous collision, joints, portals; the
`playground3d/Physics3d` API and its layers `Character3d`,
`Ragdoll3d`, `Portal3d`; PhysicsSpin3d, Float3d, Hitbox3d, Bounce3d,
Marbles3d, Roll3d, Stack3d, Walk3d, Ragdoll3d; TinyPinball3d,
TinyHalfLife2, TinyPortal, TinyTeardown, and the `physics=engine`
ports of StarCollector3d, TinyMarbleMadness, TinyMinecraft and
TinyMario64), the tutorial,
[`notes_3d_physics.md`](../tutorials/notes_3d_physics.md), checked
against the code, and the related-work note's measured postscript,
[`notes_physics3d_related_work.md`](../related-work/notes_physics3d_related_work.md).
What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example, its test, and where it helps a
switch to see its difference.

## 1. The engine's known gaps

- **Joints that stretch at a hard landing.** A ragdoll's elbow opens
  by about 4 cm for a few frames when the arm is caught between a step
  and the body falling on it: contact and joint each correcting by
  Baumgarte's slow speed. Measured not to be the solver's patience
  (4.5 cm at 10 iterations, 5.4 at 20, 3.4 at 40) nor a cold start
  (joint warm starting tried, 4.5 against 4.6, taken out). The cure
  named: correct the positions themselves after the velocities (Erin
  Catto's non-linear Gauss-Seidel, as Box2D does), for joints and
  contacts both. The test is there: `Unit_joint3d`'s ragdoll, whose
  bound (6 cm) would tighten.
- **Compound bodies.** A body is one hitbox, so a loose piece of
  TinyTeardown collides as its bounding box, and an L-shaped piece
  lands on its box's corners. Several boxes per body, with the tensor
  of their sum (`Body3d.shifted`, the parallel-axis theorem, is
  already there).
- **Sweeping more than a sphere.** `Sweep3d` sweeps spheres; a fast
  box is not swept, and `Character3d` traces its capsule by stepping
  and bisecting instead (enough for walking, not for a bullet-fast
  character). Conservative advancement only needs a distance, so a
  capsule is the next shape, and then Character3d's stand-in goes.
- **The sweep's cost.** A fast sphere is swept against every other
  body, every step: fine on a pinball table, not for a thousand
  marbles. The broad phase with the swept bounds.
- **Buoyancy's torque.** `Force3d.buoyancy` pushes at the centre, so
  nothing floating ever rights itself; the push at the submerged
  part's centre, and a barrel pushed under turns back up.
- **A ragdoll's twist.** Its ball joints limit a cone but not the turn
  about the limb (a forearm can spin on its axis); the twist limit of
  Bullet's cone-twist joint.
- **GJK and EPA** (Gilbert, Johnson, Keerthi, 1988; van den Bergen,
  2001): convex hulls, named in `Collide3d.mli`; and a **static
  triangle mesh** for levels, from `sphere_triangle` and
  `ray_triangle`, which exist. No game has needed either yet -- every
  level is boxes.
- **`box_capsule` is approximate** (the middle of the line when a
  capsule lies flat on a face): exact segment-box closest points.
- **`went_through` is a bullet's question**, worked out from the
  body's present velocity and so wrong after a bounce; keeping the
  previous position in the body would make it right for anything.
- **An energy-conserving orientation step** (Moser-Veselov, or Jacobi's
  closed form), named in `Integrate3d.mli`: the one integrator the
  tutorial says exists and we do not have.

## 2. Speed

Measured (`notes_physics3d_related_work.md`'s postscript): about 0.17
ms per awake box per step, a hundred awake boxes filling a 60 Hz
frame, and the broad phase making no difference at that size -- the
cost is the contact points and the solver's iterations over them.
Worth trying, each measured on the same piles: solving islands
separately (and skipping the ones asleep before building their
contacts), the manifolds cached from one step to the next while two
bodies barely move, lists turned into arrays in `Solver3d`.

## 3. Seeing the physics

- **The debug keys** of the finished plan's "Debug drawing and keys":
  "v" velocities, "c" contacts, "g" the broad phase's grid, "j" joints
  -- today `Physics3d.debug` (hitbox and velocity) and `debug_joints`,
  called by each game or example on its own key.
- **Golden frames for the flags**: the golden runner cannot pass flags
  (`seed=1` only), so the `physics=engine` modes of StarCollector3d,
  TinyMarbleMadness, TinyMinecraft and TinyMario64 have no golden
  frame; their tests cover them. The same missing field as in
  [`plan_physics_remaining.md`](plan_physics_remaining.md)'s section 2.
- **StarCollector3d reads its flag at load time**, which a test
  linking it would trip over (the playground's flag parser rejects
  Testo's `--worker`); the other ports read `computer.flags`.

## 4. The games' next steps

- **TinyPinball3d**: the flippers as hinged bodies driven by a motor
  (phase 11 made it possible) rather than turned by the game; a ramp,
  which the 2D table cannot have; multiball.
- **TinyHalfLife2**: the header's exercises -- saw blades (a thin fast
  box: the sweep for boxes of section 1), a zombie getting up from its
  ragdoll, a crate lid as a shield.
- **TinyPortal**: portals seen through portals, one level more (the cut
  twice, a second copy of the chamber); the portal gun's shot going
  through a portal; portals on inner walls, which would need the cut
  to hide the real room behind the hole; portals smaller than a panel.
- **TinyTeardown**: its pieces as compound bodies (section 1); and a
  report to look into, the water tower's tank left floating after its
  legs were cut, not reproduced so far (bodies did not tumble in a
  world until phase 10's fix, which may have been it).
- **TinyMarbleMadness**: the engine's marble flies off crests where the
  hand-written one sticks to them, which makes the course harder;
  whether that is a feature is a design question, and the tests' robot
  brakes earlier for it.

## 5. Measuring against the real engines

The postscript gives cannon.js's and Jolt's sizes from memory; their
demos run on the same machine, on the same piles, would give the rest
-- as for the 2D engine's Chipmunk and Box2D, not done either.
