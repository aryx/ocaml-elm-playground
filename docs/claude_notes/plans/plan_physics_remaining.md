# Plan: what's left for the physics engine

The engine and its games are done: see
[`done/plan_physics_teaching.md`](done/plan_physics_teaching.md)
(`physics/2d/`, phases 0-9: integrators, forces, collisions,
rotation, the broad phase, stacking, springs and particles; the
`playground/Physics` API; Orbit, Bounce, Marbles, Boxes, Pyramid,
Elastic; TinyWorms, TinySpacewar, TinyPong, TinyCameltry,
TinySlingshot, TinySoldat, and Asteroid's `physics=engine`) and the
tutorial, [`notes_2d_physics.md`](notes_2d_physics.md). What's left,
roughly from most to least worth doing. Like the rest, each piece with
its worked example, its test, and where it helps a switch to see its
difference (`solver=off`, `rotation=off`).

## 1. The engine's known gaps

- **Contact manifolds by clipping** (Catto, "Contact Manifolds", GDC
  2007): `Collide.manifold` takes the corners of each polygon inside
  the other, simpler but shakier for deep overlaps; Box2D's clipping
  of the incident edge against the reference face's sides, as a
  switch, compared on `examples/PhysicsPyramid.ml`.
- **Sleeping**: bodies still for half a second skipped until touched;
  a pyramid then costs nothing at rest. Measured on Pyramid (1.3 ms a
  step for 28 boxes today).
- **Joints between bodies**: a revolute joint (a pin: a seesaw, a
  wheel, Box2D Lite's "A Teeter" and "Suspension Bridge") and a
  distance joint, solved in `Solver`'s loop like the contacts.
  `Particles`' sticks are the same idea for particles only.
- **The center of mass**: a body is taken to turn around its (x, y);
  a group whose shapes are off center turns around the wrong point.
  Compute it from `Shape.moments`' areas, and turn around it.
- **A restitution threshold for `bounce`**: the solver has one, the
  one-pair `Resolve` doesn't (a resting ball makes tiny, invisible
  bounces).
- **Quadratic drag** (`-c |v| v`, air at speed) next to `Force.drag`,
  and **N-body gravitation** (every body pulling every other, both
  moving: the momentum test the plan's phase 2 named; `attracted_by`
  pulls one way only).
- **Sort and sweep's temporal coherence**: keep the previous order,
  insertion-sort it (nearly sorted: nearly linear), measured on
  Marbles.
- **GJK** (Gilbert, Johnson, Keerthi, 1988): the general convex test,
  mentioned in `Collide.mli`, and the way to rounded shapes.

## 2. Seeing the physics

- **The debug keys** of the plan's "Debug drawing and keys", never
  done -- and now the physics panel of
  [`plan_inspect_teaching.md`](plan_inspect_teaching.md) (phase 1),
  which gives them one mechanism, one key and golden frames instead of
  a per-game flag: with `-debug-keys`, "e" the integrator, "v" velocity and force
  arrows, "c" contact points and normals (the manifold's two points,
  the solver's impulses as their lengths: `Solver.impulses`), "g" the
  broad phase's grid. Today, `Physics.debug` (hitboxes and velocity)
  behind each game's `hitboxes` flag.
- **Golden frames for the flags**: the golden runner can't pass flags
  (`seed=1` only), so the `physics=engine` modes of Mario, TinyMario
  and Asteroid have no golden frame (the games with a key for it,
  Pyramid and TinySlingshot, do). A flags field in
  `tests/2d/Golden_frames.ml`'s scenes.
- **The keys at launch for every game**: TinyCameltry, Pyramid,
  Elastic, TinySlingshot and TinySoldat print theirs; an optional
  `~help` to `run_app` would make it one mechanism.

## 3. The games' next steps

- **TinySoldat's kit** (`plan_games.md` section 17): a map bigger than
  the screen with `Camera2d`, a weapons table (rate, speed, damage,
  spread), the bots' waypoints and pathfinding, a map editor, two
  players over the network (`plan_networking_teaching.md`).
- **TinyCameltry and TinySlingshot levels** from strings, and
  TinySlingshot's materials (wood, stone, glass: mass, friction, how
  hard to break).
- **A cave level** à la Thrust or Gravitar (the plan's "later"):
  gravity, fuel, landing pads, polygons to land on.
- **Asteroid's swept bullets**: small fast rocks can be missed between
  two ticks, like TinySoldat's bullets would without
  `Physics.went_through`.

## 4. Measuring against the real engines

The plan's phase 9 asked for `notes_physics_related_work.md`'s
postscript: our numbers against Chipmunk's and Box2D's on the same
scenes (a pyramid, a pile of balls), if meaningful. Not done: it needs
their demos run on the same machine, the same scene sizes.

## 5. 3D (the plan's "later")

Started, so it is a plan of its own:
[`plan_physics3d_teaching.md`](plan_physics3d_teaching.md)
(`physics/3d/` and a `Physics3d` API; the tutorial
[`notes_3d_physics.md`](../tutorials/notes_3d_physics.md), the
related-work note
[`notes_physics3d_related_work.md`](../related-work/notes_physics3d_related_work.md)).
The items this section used to list -- spheres, boxes, gravity,
`Vec3`, 3x3 inertia tensors, SAT with edge cross products, and the
users StarCollector3d, TinyMinecraft and TinyMario64 -- are its
phases 0-9.
