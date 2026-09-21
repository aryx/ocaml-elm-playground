# Plan: 3D physics for the playground, from scratch, for teaching (`physics/3d/`)

## Context

`physics/2d/` teaches how a computer makes things move: integrators,
forces, collisions, impulses, stacking, one idea per module, each
`.mli` with its diagram, its worked example and the paper behind it,
and the `playground/Physics` API over it, so a game says what happens
rather than how
([`done/plan_physics_teaching.md`](done/plan_physics_teaching.md),
[`notes_2d_physics.md`](../tutorials/notes_2d_physics.md)). It ends
with a "later": *3D*. This is that plan.

A dimension up is not the same code with a `z` added, and saying where
it really differs is most of the teaching here:

- **Orientation stops being a number.** In 2D an angle is one float
  and spin is another; in 3D an orientation is a quaternion (or a
  3x3 matrix), angular velocity is a vector that need not point along
  the spin axis, and the two are related by a differential equation.
- **Inertia stops being a number too.** `m r^2 / 2` becomes a 3x3
  tensor that *turns with the body*: `I_world = R I R^T`, recomputed
  every step. That single fact is why a spinning body in 3D can flip
  over by itself, with no force at all (the tennis-racket theorem),
  and no 2D body ever can.
- **Collisions gain a family of axes.** Two convex polygons in 2D
  separate along one of their edge normals; two boxes in 3D need their
  6 face normals *and* the 9 cross products of their edges -- 15 axes,
  and the edge-edge case is the one you cannot skip.
- **Contacts gain points.** A resting edge in 2D is 2 points; a box
  lying on the floor in 3D is a polygon clipped against a polygon, up
  to 4 points kept.
- **The player is not a rigid body.** Quake, Half-Life, Minecraft and
  Mario 64 all move the player as a *capsule or box that slides along
  planes*, never as a body with a torque -- a rigid-body player falls
  over. This is a real lesson, not a shortcut, and it has its own
  phase below.

Today every 3D game here does its own motion by hand, and each does it
differently -- which is the same starting point the 2D plan had:

- `games3d/TinyMinecraft.ml`: the player as a box against blocks,
  gravity, jumping, one axis at a time;
- `games3d/TinyMario64.ml`: box against box, feet, sides and head
  resolved separately, plus coyote time and jump buffering ("no
  physics engine (see plan_physics_teaching.md)", says its header);
- `games3d/TinyMarbleMadness.ml`: a ball on a height map, `5/7 g sin(a)`
  derived by hand, elastic ball-ball collisions, "not Physics: it's
  2D, and a ball on a height map is ten lines";
- `games3d/TinyQuake.ml`, `TinyTombRaider.ml`, `TinyDescent3d.ml`,
  `StarCollector3d.ml`: movement and pickups by distance or by box;
- `examples3d/PhysicsSolarSystem3d.ml`: Kepler in closed form (no
  integration at all, deliberately -- see its header).

**The flagship goals, the user's picks, each the excuse for one hard
part of the engine:**

1. **TinyPinball3d** -- a small very fast ball, the case that breaks a
   naive engine: continuous collision detection, kinematic flippers,
   bumpers with restitution, and sleeping when the ball is in the
   drain. The `3d` suffix because it is a **pair**, like
   TinyDoom/TinyDoom3d: `games/TinyPinball.ml`, a table seen from
   above on the *2D* engine, is being written as this plan is
   (2026-09-20), and the comparison is worth having -- the 2D one
   caps the ball's speed and thickens its walls, the 3D one has to
   sweep.
2. **TinyHalfLife2** -- Havok's greatest hits as a level: the gravity
   gun (a ray pick, a held-point constraint, an impulse launch), a
   seesaw and a ramp (hinge joints), floating barrels (buoyancy),
   ragdolls (joints with limits) -- the game that made physics a *toy*
   rather than a decoration.
3. **TinyPortal** -- the same body, moved through a transform: a
   portal rotates a position, a velocity *and* an orientation, and
   "speedy thing goes in, speedy thing comes out" is one matrix
   multiply plus the honesty to say what the rendering costs.

Companions: [`notes_3d_physics.md`](../tutorials/notes_3d_physics.md),
the tutorial (written ahead of the code, as its specification, like
`notes_audio.md` and `notes_ai.md` were), and
[`notes_physics3d_related_work.md`](../related-work/notes_physics3d_related_work.md)
(Havok, Bullet, Jolt, elm-physics, the games that made 3D physics a
feature, and the ceiling here), beside the 2D note's own
[`notes_physics_related_work.md`](../related-work/notes_physics_related_work.md).

## Principles

The eight of [`../README.md`](../README.md) unchanged, plus what this
area needs of its own:

- **Independent of the Playground**, like `physics/2d/`: `physics/3d/`
  knows vectors, quaternions, bodies, hitboxes and time steps, and
  nothing of `shape3d`, `camera` or `computer`;
  `playground3d/Physics3d.ml` is the adapter, as `Camera3d` is for
  the camera.
- **Say where 3D actually differs**, and only there. A module that is
  the 2D one with a `z` added (`Force3d`, `Energy3d`) stays short and
  says so in one line; the modules that are genuinely new
  (orientation, the inertia tensor, 15-axis SAT, manifold clipping,
  the character controller) are where the diagrams and the worked
  examples go.
- **The existing games keep their hand-written physics**, and gain a
  `physics=engine` flag, exactly as `Asteroid.ml`, `Mario.ml` and
  `TinyMario.ml` did in 2D: two engines on the same objects in one
  file is the best comparison there is, and rewriting TinyMarbleMadness's
  ten lines into an engine call would *lose* the lesson its header
  makes.
- **Deterministic**: a fixed step, no wall clock, no global `Random`,
  so `tests/3d/` golden frames work for physics examples too.
- **Test against the laws**: linear *and* angular momentum, energy,
  and the closed forms 3D has of its own (a rolling sphere's
  `5/7 g sin a`, a box's inertia tensor, a free body's flip).

## The Playground API, Evan-style

Tentative (`playground3d/Physics3d.mli`), **to be settled by writing
the games with it** -- the 2D API changed in three ways the moment a
real game was written on it (`step` not `move`, accumulating forces,
mass 1 by default), and this one will too.

The 2D API's shape is kept on purpose: someone who has written
`games/TinySlingshot.ml` should recognize every verb. Names stay
unsuffixed where nothing clashes (`Playground3d` suffixes only what
collides with `Playground`: `move3d`, `rotate3d`, but plain `box`,
`sphere`, `camera`); a 3D game opens `Playground`, `Playground3d` and
`Physics3d`, and none of these names is taken.

```ocaml
(* a shape that moves, in 3D *)
type body = {
  shape : shape3d;                 (* what it looks like, and its hitbox *)
  x : number; y : number; z : number;
  vx : number; vy : number; vz : number;    (* units per second *)
  orientation : Quat.t;            (* which way it points *)
  spin : Vec3.t;                   (* angular velocity: axis * radians/s *)
  mass : number;                   (* 1 by default, like the 2D API *)
  bounciness : number;             (* 0 (clay) by default *)
  friction : number;
  upright : bool;                  (* a player: collisions never turn it *)
  ax : number; ay : number; az : number;    (* the force accumulator *)
  torque : Vec3.t;
}

val body : shape3d -> body
val at : number -> number -> number -> body -> body
val moving : number -> number -> number -> body -> body
val heavy : number -> body -> body
val bouncy : number -> body -> body
val rough : number -> body -> body
val immovable : body -> body          (* a floor, a wall: 1/m = 0 *)
val upright : body -> body            (* a player, a pinball flipper *)

(* in update, once per tick *)
val fall : number -> body -> body                 (* uniform gravity *)
val push : number -> number -> number -> body -> body
val thrust : number -> body -> body               (* along its facing *)
val slow : number -> body -> body                 (* drag *)
val attracted_by : body -> body -> body           (* gravitation *)
val spin_by : number -> number -> number -> body -> body
val step : body -> body                           (* one fixed tick *)
val tick : number                                 (* 1/60 s *)

val touching : body -> body -> bool
val bounce : body -> body -> body * body
val bounce_off : body -> body -> body             (* against an immovable *)

(* the whole step, for a pile *)
type world
val world : body list -> world
val simulate : ?gravity:number -> ?iterations:int -> world -> world

(* what 3D adds to the 2D API *)
val ray : number * number * number ->             (* from *)
          number * number * number ->             (* direction *)
          body list -> (body * number) option     (* what it hits, how far *)
val walk : ... -> body -> body        (* the character controller, phase 9 *)
val held_by : number * number * number -> body -> body   (* the gravity gun *)
val through : portal -> body -> body  (* phase 12 *)

(* in view *)
val draw : body -> shape3d            (* the shape, moved and turned *)
val debug : body -> shape3d           (* hitbox, velocity, contacts *)
```

A gravity-gun frame then reads like its rule:

```ocaml
let update computer game =
  let held =
    match game.held with
    | Some b when computer.mouse.down -> Some (b |> held_by (aim_point game))
    | Some b -> None  (* released: it keeps the velocity it had *)
    | None -> if fired computer then ray (eye game) (look game) game.props
              else None
  in
  { game with world = simulate ~gravity:9.8 game.world; held }
```

Open questions, to settle by writing the three games: whether `spin`
is a vector or an axis plus an angle for beginners; whether `draw`
converts the quaternion to `Playground3d.rotate3d`'s XYZ Euler angles
(see Groundwork) or the playground gains a matrix rotation; whether
the character controller is a `body` verb (`walk`) or its own layer
(`playground3d/Character3d`, which is what plan_games3d's
`Fps_controller` wish wants).

## Games

- **TinyPinball3d, new** (the first flagship; after *3D Pinball: Space
  Cadet*, Cinematronics/Maxis, 1995, and the Pinball Fantasies
  lineage -- names and dates from memory, to check): a table tilted
  6 degrees, a plunger, two flippers, bumpers, drop targets, a drain.
  Why it is the right first game: a pinball is ~27 mm across and goes
  up to ~10 m/s, so at 60 Hz it moves *six times its own diameter
  between two frames* -- every naive engine loses it through the
  table. It needs, in order: swept tests (phase 10), kinematic bodies
  (a flipper is driven by the player, not by forces), a restitution
  threshold and sleeping (a ball resting in the drain must stop
  buzzing). Nudging the table (Alt keys) is a one-line impulse and a
  tilt counter.
- **TinyHalfLife2, new** (the second flagship; Valve, 2004, on Havok):
  one room of the Ravenholm kind, and every Havok party trick as a
  toy: a **gravity gun** (a ray pick, then the body pulled to a point
  a metre in front of the eye by a stiff constraint, then launched
  with an impulse -- the whole gun is three engine calls), a
  **seesaw** and a plank ramp (hinge joints), a stack of crates and
  cinder blocks (phase 8's solver), **floating barrels** (buoyancy:
  a body's submerged volume as an upward force), and **ragdolls**
  (bodies joined by ball-and-socket joints with limits) that go limp
  when hit. The level is one of HL2's actual lessons: physics you can
  *use*, not decoration.
- **TinyPortal, new** (the third flagship; Valve, 2007): a room, two
  portals, a weighted cube and a button. The physics is small and
  exact -- a body crossing a portal has its position, velocity *and*
  orientation multiplied by the portal-to-portal transform, which is
  why "speedy thing goes in, speedy thing comes out" and why falling
  through a floor portal into a wall portal turns fall into flight.
  The *rendering* is the expensive half, and this plan says so up
  front: the destination room's polygons, transformed into the source
  room and clipped against the portal's four side planes plus its own
  plane (the same Sutherland-Hodgman clipping `games2.5d/TinyDescent.ml`
  already does for its portals), one level of recursion. No
  render-to-texture, no stencil buffer: those exist in none of the
  four backends.
- **The ports, as a choice rather than a rewrite** (`physics=engine`,
  the 2D plan's pattern): `games3d/StarCollector3d.ml` (movement and
  pickups: the smallest one, and the first user), `TinyMario64.ml`
  (box collisions against the engine's; its game feel stays
  hand-written, which is the point -- coyote time is not physics),
  `TinyMinecraft.ml` (the player capsule against blocks, phase 9),
  `TinyMarbleMadness.ml` (the height-map ball against a real rolling sphere:
  its `5/7` is the engine's own test, phase 7).
- **A game written on it from the start**: `games3d/TinyTeardown.ml`
  (DONE), where every piece of a building knocked loose is a body of a
  simulated `world` -- tumbling (the quaternion and the tensor),
  landing at any angle (box/box separating axes), piling up and going
  to sleep (phase 8's solver). The standing level is immovable boxes,
  rebuilt after each blow. Found on the way: a body starting *inside*
  another's box is thrown out of it hard enough to tunnel through a
  thin floor (a crate inside a tank's bounding box went through a 1 m
  ground; the ground is now 10 m thick and the tank flat).
- **Later**: a raycast vehicle for `TinyVirtuaRacing.ml`
  (elm-physics's RaycastCar is the model: four springs with rays for
  wheels, no wheel bodies), a Jenga/domino toy, a Monkey Ball tilt
  maze on top of phase 7.

## Target layout

```
graphics/3d/geometry/     Vec3 (exists, gains neg/lerp/min/max/distance),
                          Mat4, Camera, Lighting -- package elm_playground_3d,
                          which every 3D backend already depends on, so
                          physics/3d/ can use it from any backend (the 2D
                          plan's phase-0 groundwork is already satisfied here)
physics/3d/               (physics_3d, private, package elm_playground_3d:
                          pure OCaml, so the web backend gets it too)
  Quat                    orientation: multiply, from axis-angle, to matrix,
                          the derivative q' = 1/2 w q, renormalization
  Mat3                    the inertia tensor: multiply, transpose, inverse,
                          R I R^T
  Body3d                  position, velocity, orientation, spin, mass,
                          inertia tensor (body frame and world frame)
  Integrate3d             one step: semi-implicit Euler (the default),
                          Verlet, RK4; and the orientation's own update
  Force3d                 gravity, gravitation, springs, drag, buoyancy
  Energy3d                kinetic (linear + w . I w / 2), potential,
                          momentum, angular momentum: the diagnostics
  Hitbox3d                sphere, AABB, box (OBB), capsule, convex hull,
                          plane, triangle mesh (static); volume, inertia
                          tensor, bounds
  Collide3d               narrow phase: sphere/sphere, sphere/box,
                          AABB/AABB, SAT with 15 axes, sphere/triangle,
                          capsule/capsule, rays (Moller-Trumbore)
  Contact3d               a contact's normal, depth, point(s)
  Broadphase3d            all pairs, a uniform grid, sweep and prune,
                          (later) a dynamic AABB tree
  Resolve3d               impulses with the tensor, two friction tangents,
                          positional correction
  Solver3d                stacking: manifolds by face clipping, sequential
                          impulses, warm starting, sleeping
  Joint3d                 distance, hinge, ball-and-socket, motors, limits
  Sweep3d                 continuous collision: swept sphere, speculative
                          contacts, sub-stepping
physics/tests/            the same test binary, gaining Unit_*3d.ml
playground3d/Physics3d.ml the Evan-style API above
playground3d/Character3d.ml the capsule controller (phase 9; subsumes
                          plan_games3d.md's Fps_controller wish)
```

Why `Hitbox3d` and not `Shape3d`: `Playground3d.shape3d` is the
drawing type, and a physics module of the same name next to it in
every game's `open` list would be a permanent stumble. The 2D
`Shape`/`Collide`/`Contact` names are kept, suffixed, everywhere they
do not create that confusion -- and suffixed at all because both
libraries are `(wrapped false)`, as `graphics/2d` and `graphics/3d`
are, so `Body` and `Body3d` must differ.

## Groundwork decisions

### Orientation: quaternions, and what `draw` does with them

A quaternion (4 floats) rather than a 3x3 matrix (9) or Euler angles
(3). Euler angles are out for the *state*: accumulating them gimbal-
locks and drifts, and `Playground3d.rotate3d`'s fixed X-then-Y-then-Z
order makes "add a bit of spin" ambiguous. Matrices are fine but drift
out of orthogonality and cost more to renormalize (Gram-Schmidt on 9
numbers against one `normalize` on 4).

`draw` must still hand the playground something it understands, and
`rotate3d` takes XYZ Euler degrees. Decision: **convert the quaternion
to XYZ Euler angles in the adapter**, which is exact away from the
singularity at pitch = +/-90 degrees (where any equivalent triple will
do, and the picture is right either way). The alternative -- adding
`Playground3d.orient3d : Mat3.t -> shape3d -> shape3d` -- touches all
four backends (software, OpenGL, WebGL, web/vdom) and is the fallback
if the conversion ever shows in a golden frame; it would be the better
end state, and the plan's phase 3 measures whether it is needed.

### Where the new math lives

`Quat` and `Mat3` start in `physics/3d/`, not in
`graphics/3d/geometry/`, because nothing else needs them yet
(`Camera3d`'s `?up` and `Mat4.look_at` do their own thing). If a
backend or `Camera3d` ever wants them, they move to `geometry/` and
this line becomes the note saying why -- the same move `Vec2` did not
have to make in 2D.

### Units: metres, not pixels

The 2D engine works in pixels and seconds, with gravity around
500-1000 px/s^2. `Playground3d`'s world has no natural pixel: a
`cube white 1.` is one unit, cameras sit a few units away. Decision:
**one unit is one metre, gravity is 9.8**, so every real number from
physics (a 27 mm pinball, a 10 m/s shot, a barrel's density of 0.6)
can be written down as itself and checked against reality. The
examples say their scale in their headers.

### Time: the same fixed step

1/60 s per tick, no accumulator, no clock inside the engine -- the 2D
plan's reasoning (Fiedler, "Fix Your Timestep!", 2004) unchanged, and
what `tests/3d/`'s golden frames depend on. The one addition: phase
10's sub-stepping, which subdivides *inside* a tick, deterministically
(a fixed number of sub-steps for a fast body, not "until it stops
moving").

### Debug drawing and keys

`Physics3d.debug : body -> shape3d` draws the hitbox as a wireframe,
the velocity as a line, the contact points as small cubes and the
contact normals as lines -- ordinary `shape3d`s, so every backend
shows them. Behind `-debug-keys` and per-game flags, as in 2D:
"v" velocities, "c" contacts, "g" the broad phase's grid, "j" joints.
Seeing a 3D contact normal is worth more than seeing a 2D one: most
wrong 3D collisions are a normal pointing the wrong way.

## The modules, with their references

(To check against the sources when each `.mli` is written; names and
dates from memory unless stated.)

- **Quat**: Hamilton (1843); Ken Shoemake, "Animating Rotation with
  Quaternion Curves" (SIGGRAPH 1985), the paper that brought them into
  graphics. The `q' = 1/2 * omega * q` derivative, and why
  renormalizing every step is enough.
- **Mat3, Body3d, Resolve3d**: David Baraff and Andrew Witkin,
  "Physically Based Modeling" (SIGGRAPH course notes, 1997-2001) --
  the rigid-body derivation everyone else's is downstream of:
  `I_world = R I_body R^T`, `L = I w`, the impulse denominator with
  `(r x n)^T I^-1 (r x n)`.
- **Integrate3d**: as 2D (`Integrate.mli`), plus the orientation's
  update; Hairer, Lubich, Wanner, *Geometric Numerical Integration*
  (2002) for why the symplectic one keeps orbits and free rotations
  honest.
- **Energy3d**: the referee -- `w . I w / 2` for the spin, `L = I w`
  conserved by a free body (the flip's test).
- **Hitbox3d, Collide3d, Contact3d**: Christer Ericson, *Real-Time
  Collision Detection* (2005) -- chapter 4 (bounding volumes), 5
  (closest points and primitive tests: the capsule's segment-segment
  is 5.1.9), 9 (convexity and SAT); Gottschalk, Lin, Manocha,
  "OBBTree" (SIGGRAPH 1996) for the 15-axis box test; Moller and
  Trumbore, "Fast, Minimum Storage Ray/Triangle Intersection"
  (Journal of Graphics Tools, 1997); Gilbert, Johnson, Keerthi (1988)
  and van den Bergen's EPA (2001) as the general convex route, named
  in the `.mli` and left as the next step.
- **Broadphase3d**: uniform grids; sweep and prune (Baraff 1992;
  I-COLLIDE, Cohen et al., 1995); the dynamic AABB tree (Box2D's and
  Bullet's `btDbvt`) as the modern default.
- **Solver3d**: Erin Catto, "Iterative Dynamics with Temporal
  Coherence" (GDC 2005) and "Contact Manifolds" (GDC 2007) -- the
  face-clipping manifold this phase needs; Brian Mirtich and John
  Canny, "Impulse-based Simulation of Rigid Bodies" (1995).
- **Joint3d**: Catto, "Soft Constraints" (GDC 2011); the ragdoll's
  cone and twist limits.
- **Sweep3d**: conservative advancement (Mirtich); speculative
  contacts (Erwin Coumans / Paul Firth, around 2010-2013); the
  sub-stepping fallback.
- **Character3d**: Quake's `SV_FlyMove` (id Software, 1996; the
  source released in 1999) -- move, clip the velocity to each plane
  hit, repeat up to 4 times; step offset and slope limit as every
  engine's `CharacterController` has them.
- **Throughout**: Ian Millington, *Game Physics Engine Development*
  (2007), the 3D counterpart of the 2D plan's Bourg; David H. Eberly,
  *Game Physics* (2003).

## New examples

Each small, each one idea, each deterministic (golden frames in
`tests/3d/`), named like `examples3d/PhysicsSolarSystem3d.ml`:

- `PhysicsSpin3d.ml`: a wing-nut tumbling in free fall. No forces at
  all, and it flips over, again and again, on a period you can count
  -- the intermediate-axis (tennis-racket) theorem, the one lesson
  that has no 2D version. Keys: the three principal axes to start it
  about, and `L` and the energy on screen, both constant while it
  flips.
- `PhysicsBounce3d.ml`: spheres of bounciness 0 to 1 on a floor, and
  a box dropped on a corner (the 2D `PhysicsBounce.ml`'s twin).
- `PhysicsRoll3d.ml`: a sphere, a cylinder and a box down the same
  ramp. The sphere arrives at `5/7 g sin a`, the box at `g sin a`
  minus friction, and `games3d/TinyMarbleMadness.ml` derived that same 5/7 by
  hand: the example is the engine's own cross-check against a game
  that predates it.
- `PhysicsStack3d.ml`: a brick wall, a domino run and a Jenga tower;
  `s` for the solver off (phase 7's pairwise bounce: it slumps), `i`
  for the iteration count, `w` for warm starting -- the 2D
  `PhysicsPyramid.ml`'s keys, one dimension up.
- `PhysicsRagdoll3d.ml`: 9 bodies, 8 joints, dropped down a staircase.
- `PhysicsWalk3d.ml`: the capsule controller on a course of steps,
  slopes and a low ceiling; keys for the step offset and the slope
  limit, to see a 0.5 m step become a wall at 0.4.

## Phasing

Each phase builds, tests and ships on its own.

0. **Groundwork**: `physics/3d/` (library `physics_3d`, package
   `elm_playground_3d`) and its dune; `physics/tests/` gains the 3D
   libraries; `Vec3` gains `neg`, `lerp`, `distance`, `min`/`max`;
   `playground3d/Physics3d.mli` stub.
1. **Orientation and integration**: `Quat`, `Mat3`, `Body3d`,
   `Integrate3d`, `Energy3d`; `PhysicsSpin3d.ml`. Tests: quaternion
   algebra against matrices, `I_world = R I R^T` against a rotated
   box's analytic tensor, angular momentum conserved by a free body,
   the flip's period, the convergence order of each integrator (as
   2D's `Unit_integrate` measures them).
2. **Forces**: `Force3d` (gravity, gravitation, spring, drag,
   buoyancy). Tests: momentum conserved by N-body gravitation, a
   floating body's rest depth (Archimedes, checked against the
   analytic waterline).
3. **The API, v1, no collisions**: `Physics3d.body`, `at`, `moving`,
   `fall`, `push`, `thrust`, `slow`, `attracted_by`, `spin_by`,
   `step`, `draw`, `debug`. Port `StarCollector3d.ml` behind
   `physics=engine`, and settle the open questions above by writing
   it. Measure whether the quaternion-to-Euler `draw` is visible.
4. **Collision detection**: `Hitbox3d`, `Collide3d`, `Contact3d`;
   `touching`, `ray`. Tests: each `.mli`'s worked example; SAT against
   brute force on random convex hulls; rays against an analytic
   sphere; the capsule's segment-segment distance against sampling.
   Users: StarCollector3d's pickups exact instead of by distance.
5. **Collision response**: `Resolve3d`; `bounce`, `bounce_off`;
   `PhysicsBounce3d.ml`. Tests: momentum and angular momentum
   conserved over 1000 random collisions, energy never created, kept
   exactly at `e = 1`.
6. **Broad phase**: `Broadphase3d`, all pairs vs grid vs sweep and
   prune, switchable, each with its count of box tests; a stress scene
   (500 spheres) measured, the numbers into the notes.
7. **Rotation and rolling**: inertia tensors from every hitbox,
   off-centre impulses, rolling friction; `PhysicsRoll3d.ml`, and
   `TinyMarbleMadness.ml`'s `physics=engine`.
8. **Stacking**: `Solver3d` -- manifolds by clipping the incident face
   against the reference face (up to 4 points), sequential impulses,
   warm starting by matching points, a bounce threshold, sleeping;
   `world`, `simulate`; `PhysicsStack3d.ml`.
9. **The character controller**: `playground3d/Character3d` -- a
   capsule, Quake's clip-and-retry loop, step offset, slope limit,
   ground detection; `PhysicsWalk3d.ml`; `TinyMinecraft.ml` and
   `TinyMario64.ml` behind `physics=engine`, their game feel untouched.
10. **Continuous collision**: `Sweep3d` -- swept sphere against planes
    and boxes, speculative contacts, deterministic sub-stepping;
    `Physics3d.went_through`. Then **TinyPinball3d**: the tilted table,
    the plunger, kinematic flippers, bumpers, sleeping in the drain.
    The measured proof: the ball at 10 m/s kept on the table, and the
    same ball lost without the phase (a key to switch it off).
11. **Joints**: `Joint3d` -- distance, hinge, ball-and-socket, motors,
    limits, solved in `Solver3d`'s loop; `PhysicsRagdoll3d.ml`. Then
    **TinyHalfLife2**: the gravity gun, the seesaw, the barrels, the
    ragdolls.
12. **Portals**: the portal transform applied to position, velocity
    and orientation; the clipped double-draw of the destination room,
    one level deep. Then **TinyPortal**.
13. **Docs**: `notes_3d_physics.md` checked against the code and its
    numbers filled in; `notes_physics3d_related_work.md`'s postscript
    measured (lines of code against cannon.js and Jolt, bodies at
    60 Hz, the pinball kept and lost).

## Status

**Phases 0 to 8 done** (2026-09-20), bar one port named below; the
rest not started. Written as the specification, with
[`notes_3d_physics.md`](../tutorials/notes_3d_physics.md) beside it:
the tutorial is the design review, the plan is the order. Decisions
already taken and the reasons, so they are not re-argued later:

- the flagship games are the user's picks (2026-09-20): a 3D pinball,
  a Half-Life 2 physics room, and Portal -- chosen because each one
  *forces* a hard part of the engine (continuous collision, joints and
  constraints, transforms through a portal) rather than decorating a
  finished one;
- quaternions for the state, Euler angles only at the drawing edge
  (Groundwork);
- one unit is one metre and gravity is 9.8, unlike the 2D engine's
  pixels (Groundwork);
- the existing 3D games keep their hand-written physics and gain a
  `physics=engine` flag, as `Asteroid.ml` did in 2D;
- `Hitbox3d`, not `Shape3d`, next to `Playground3d.shape3d`.

Each phase appends its own DONE entry here, with its numbers and its
wrong turns, as `done/plan_physics_teaching.md` does.

- **Phase 0, DONE**: `physics/3d/` (library `physics_3d`, package
  `elm_playground_3d`, `wrapped false`, depending only on
  `graphics_3d_geometry` for `Vec3`); `physics/tests/` gains it.
  `Vec3` needed nothing added after all (`neg`, `lerp`, `min`/`max`
  were listed here speculatively; `sub`, `scale` and `dot` did the
  work), and there is no `playground3d/Physics3d.mli` stub either: an
  empty module is not groundwork, and phase 3 will write the real one
  by porting a game onto it, as the 2D API was.
- **Phase 1, DONE**: `Quat`, `Mat3`, `Body3d`, `Integrate3d`,
  `Energy3d`, 19 tests in `physics/tests/` (`Unit_quat3d`,
  `Unit_body3d`, `Unit_integrate3d`), and
  `examples3d/PhysicsSpin3d.ml` on all four backends with three golden
  frames. The numbers, and the one place the sketch above was wrong:
  - **the step follows L, not w.** The plan and the tutorial both
    wrote the rotational half as `w += I^-1 (torque - w x (I w)) dt`,
    the gyroscopic form. Writing the tests showed the simpler one:
    `dL/dt = torque` has no gyroscopic term at all, and reading `w`
    back as `I_world^-1 L` produces the same wobble because the tensor
    has turned. Measured on the T-handle (tensor
    `diag(0.00149, 0.00233, 0.00370)` kg m^2, 10 rad/s, 30 s,
    `dt = 1/600`): `|L|` drifts 1e-11 stepping L against 2e-2 stepping
    w. `Integrate3d.spin_law` keeps all three (`Momentum`, the
    default; `Spin_gyroscopic`, what Bullet does; `Spin_naive`,
    without the term) because the comparison is the lesson.
  - **the energy is not conserved by either**, and saying so is part of
    the teaching: the orientation step is first order and the
    middle-axis motion multiplies any error by `e^(4.6 t)`. 57% in 10 s
    at `dt = 1/60`, 15% at 1/600, 0.9% at 1/6000, with `|L|` at 1e-12
    throughout. `PhysicsSpin3d` runs ten sub-steps a frame and prints
    both drifts; a symplectic Lie-group integrator (Moser-Veselov) is
    named in the `.mli`, not built.
  - **the trap, on a key**: `Spin_naive` never flips *and* conserves
    both `|L|` and the energy to 1e-16. The quiet diagnostics are the
    warning sign, which is the argument for keeping the demo.
  - the orientation's exact turn (`Quat.turned_by`) is the default
    rather than the first-order step engines ship: measured lag over
    one second, 0.0013 degrees at 1 rad/s, 0.17 at 5, 10.4 at 20, 110
    at 60, for one sin and one cos more.
  - `Body3d` holds the tensors of a box and a sphere and the 3D
    parallel-axis theorem (`shifted`) until `Hitbox3d` (phase 4) takes
    them over; the T-handle's tensor is two boxes shifted to their
    common centre of mass, which is also what makes its three moments
    differ.
  - the quaternion-to-Euler conversion at the drawing edge
    (`Quat.to_euler_xyz` into `Playground3d.rotate3d`) is exact and
    invisible in the golden frames, so the fallback of adding
    `orient3d` to all four backends (Groundwork) stays unneeded. Phase
    3 measures it again on a moving body.
- **Phase 2, DONE**: `Force3d` (`none`, `uniform`, `gravitation`,
  `spring`, `drag`, `sum` -- the 2D formulas with a `Vec3`, and the
  `.mli` says so rather than repeating their explanations -- plus
  `buoyancy` and the `submerged` fraction it is built on). Six tests:
  a circular orbit closing on itself, two bodies keeping their
  momentum while they pull on each other, drag's terminal speed,
  Hooke's `cos t`, the submerged fraction by hand, and Archimedes'
  waterline -- a block of relative density 0.6 settles with 0.6000 of
  itself under water, 0.2 with 0.2000, and 2.7 keeps sinking. Left
  out, and said so in the `.mli`: the force is applied at the body's
  centre, so nothing rights itself (a torque at the centre of the
  submerged part, phase 11), and the submerged fraction comes from a
  height rather than a shape (`Hitbox3d`, phase 4).
  `examples3d/PhysicsFloat3d.ml` is phase 2's demo and its own check:
  five blocks, each with a stripe painted at its density, and nothing
  lines those stripes up with the water but the simulation.
- **Phase 3, DONE**: `playground3d/Physics3d`, the Evan-style API, with
  the verbs the engine can honestly support today (`body`, `at`,
  `moving`, `pointing`, `turning`, `heavy`, `bouncy`, `rough`,
  `immovable`, `upright`, `solid_as`, `fall`, `push`, `thrust`,
  `slow`, `attracted_by`, `pulled_to`, `floating`, `spin_by`, `step`,
  `draw`, `debug`, `position`, `forward`, `distance`, `speed`,
  `bounds`); 9 tests in `Unit_physics3d_api`. The open questions,
  settled by writing it:
  - **spin is a vector of degrees per second**, not radians and not an
    axis-plus-angle: the playground is degree-land (`rotate3d`,
    `Camera3d`'s headings), the conversion happens in `step`, and
    `turning axis degrees_per_second` is how a game says it. The
    engine underneath stays in radians.
  - **`draw` converts the quaternion to Euler angles**, as Groundwork
    guessed, and `games3d/StarCollector3d.ml` with `physics=engine`
    is the measurement asked for: a cube tumbling about a tilted axis
    while it moves, drawn through `rotate3d`, with nothing visible to
    fix. `orient3d` in four backends stays unneeded.
  - **every body gets the inertia tensor of its own bounding box**
    (`Physics3d.bounds`, a fold over the shape's points), so a torque
    does something without the game having to know what a tensor is,
    and `upright` is how a game takes it away. The 2D API's default is
    the same shape-derived one; what is new is that in 3D it makes
    bodies *wobble*.
  - `floating` is in the API, because buoyancy with no collisions is
    already a game-worthy force and `PhysicsFloat3d` is one line of it
    per block.
  - `touching`, `bounce`, `ray`, `world`/`simulate` are **not** there
    yet, and the `.mli` names the gap rather than stubbing it: they
    arrive with phases 4, 5 and 8.
  - `games3d/StarCollector3d.ml` ported behind `physics=engine`, as
    `games/Asteroid.ml` was in 2D: the arrows push a 1 kg body against
    a drag of 6, which balances at the same 4.8 m/s the hand-written
    version moved at, and the fence stops it across while it keeps
    sliding along. The default path is untouched, and its two golden
    frames are byte-identical, which is what makes the flag a
    comparison rather than a rewrite.
- **Phase 4, DONE**: `Hitbox3d` (sphere, box/OBB, capsule, plane, with
  their volumes, tensors, bounds, corners, support and extent),
  `Contact3d`, `Collide3d` (the closest points, every pair, and the
  rays), `Physics3d`'s `touching`, `contact`, `ray`, `ball`, `pill`,
  `hitbox`, `hitbox_of` and a `debug` that now draws the *hitbox*;
  14 tests in `Unit_collide3d` and 3 more in `Unit_physics3d_api`;
  `examples3d/PhysicsHitbox3d.ml`, and `StarCollector3d`'s pickups
  exact with `physics=engine`.
  - **The nine axes, measured rather than described.** `Collide3d.boxes`
    takes `~edge_axes:false`, and its test hunts for a pair the 6-axis
    version gets wrong: 20,000 random pairs of rods, 3 found, and the
    first is now in the test with a *certificate* -- a separating axis
    computed straight off the 16 corners, which the test checks is an
    edge cross and not a face. The 15-axis test says apart, the 6-axis
    one says collision. That is the classic 3D bug, on record.
  - **A thousand random pairs** against the same certificate, with a
    point-sampling cross-check when they are called apart: no
    disagreement.
  - **"Just touching" counts as apart**, everywhere, and the `.mli`
    says so: a contact always has depth > 0. Two of the tests were
    written the other way round first, which is how the inconsistency
    turned up (the plane test already assumed it).
  - **What is not there**, in the `.mli` rather than in a stub: GJK and
    EPA (SAT on boxes plus the primitives is what our games need and
    can be read in an afternoon); a convex hull, which needs its faces
    and not just its points; a static triangle mesh, which waits for a
    level, with `ray_triangle` and `sphere_triangle` already there to
    build it from; and a manifold, since a box resting on a box needs
    four points and phase 8 is where faces get clipped.
  - **One approximation, named**: `box_capsule` walks back and forth
    between the two shapes four times, which is exact when the closest
    feature is a point and reports the middle of the line when a
    capsule lies flat on a face. Good enough for a character against a
    wall (phase 9), not for stacking.
  - `Physics3d.debug` draws the hitbox as a wireframe of thin rods (a
    flat polygon vanishes edge-on, and the software backend has no
    alpha for a translucent solid): a box's twelve edges, a sphere's
    three rings, a capsule's outline, a plane's grid.
- **Phase 5, DONE**: `Resolve3d` (the impulse with the tensor in its
  denominator, friction along two tangents, the positional
  correction), `Physics3d`'s `bounce`, `bounce_off` and `bounce_all`,
  6 tests in `Unit_resolve3d` and one more in the API's, and
  `examples3d/PhysicsBounce3d.ml`.
  - **The two 2D worked examples had to come out the same**, and they
    do: two balls head on (0 and 2 at e = 1, 1 and 1 at e = 0, the
    energy kept only by e = 1), and a ball into the end of a rod (the
    rod takes 0.4 of the speed and spins at 1.2 rad/s, the ball keeps
    0.6). That second one is the whole phase in one number: the 2D
    formula's `(r x n)^2 / I` became `n . ((I^-1 (r x n)) x r)`, and
    the answer did not move.
  - **Both conservation laws hold by construction**, and the test says
    so with numbers: over a thousand random collisions -- random
    masses, tensors, spins, orientations, restitutions and frictions
    -- the pair's momentum and its angular momentum about the origin
    are unchanged to better than 1e-12, energy is never created, and
    with `e = 1` and no friction it is not lost either.
  - **Friction is a pyramid, not a cone**, and `Resolve3d.mli` draws
    the picture: two perpendicular tangents each clamped to `mu j`
    lets the diagonal reach `mu j sqrt 2`, 41% too much. Everyone
    ships the pyramid.
  - `PhysicsBounce3d` checks itself the way `PhysicsFloat3d` does: the
    bar beside each ball is drawn at `e^2` of its fall, from `e` alone,
    and the ball comes back and touches it -- about 2% under, which is
    the discrete step's cost and is printed rather than hidden. The
    crate dropped on a corner is the part with no 2D version: the
    impulse lands far from its centre, so most of what it gets is
    spin.
  - Still one contact per pair, solved once a frame. `bounce_all` is
    every pair once, quadratic, and says so: the broad phase is phase
    6 and the solver phase 8.
- **Phase 6, DONE**: `Broadphase3d` (all pairs, a hashed uniform grid,
  sweep and prune), `Physics3d.bounce_all ?broad_phase`,
  `Physics3d.broad_phase` and `world_bounds`, 4 tests, and
  `examples3d/PhysicsMarbles3d.ml` -- 200 marbles in a wireframe cage,
  space switching the method, the count of compared boxes on screen.
  Measured on 500 marbles piled in a box:

  ```
                     box tests        ms      pairs found
     all pairs         124,750       0.75         174
     grid                1,340       0.76         174
     sweep and prune     7,472       0.22         174
  ```

  the same shape of answer as the 2D engine's: the grid makes far the
  fewest comparisons and spends the winnings on its hash table, while
  sweep and prune, a sort and a list, is fastest.
  - **What the third dimension actually changes** is two things, and
    both are in the `.mli`. The grid's memory: a dense grid at 100 a
    side is a million cells, nearly all empty, so ours hashes them and
    pays only for the ones something is in -- which
    `PhysicsMarbles3d`'s "g" key draws, since that is a picture worth
    having. And the sweep's *axis*, which 2D never had to choose: on
    a pile that is wide in x and z and thin in y, sweeping y costs
    84,750 box tests against 6,644 for x -- nearly as bad as testing
    everything. `sweep_and_prune` therefore picks the axis by the
    variance of the centres, I-COLLIDE's heuristic, and the `.mli`
    admits it is a heuristic: here it picks z (7,472) where x (6,644)
    was marginally better.
  - Not built, and named: the dynamic AABB tree every modern engine
    defaults to (it also answers ray queries, which is why it wins in
    3D where in 2D it is a tie), and keeping the sorted order between
    frames, which is what makes sweep and prune a pair *manager*
    rather than a function.
- **Phase 7, DONE** except the game port: `Physics3d.spin_slow`
  (rolling friction), 3 tests in `Unit_rolling3d`, and
  `examples3d/PhysicsRoll3d.ml`. The tensors from every hitbox and the
  off-centre impulses were already there (phases 4 and 5), so what
  this phase really was is the *measurement*:
  - **the engine arrives at 5/7 g sin a by itself.** A sphere let go
    on a 15 degree slope rolls at 1.805 m/s^2 against the textbook's
    1.812, and at 30 degrees 3.488 against 3.500 -- within 0.4%,
    having been told only about a tensor, a contact point and a
    friction impulse. `games3d/TinyMarbleMadness.ml` derives that same
    5/7 by hand in its header, from a textbook, so the two halves of
    the repository now check each other. With `mu = 0` the same sphere
    slides at exactly `g sin a` with no spin at all, which is the
    other half of the lesson.
  - **it is really rolling, not slipping**: the test checks `v = w r`
    at the contact to 2%, which is the condition that makes 5/7 true
    in the first place.
  - **the constant generalises**: `a = g sin a / (1 + k)` with
    `k = I / m r^2` read straight off the body's own tensor, so
    `PhysicsRoll3d` is a race between a frictionless ball (k = 0, and
    it wins), a solid sphere (k = 0.40) and a capsule lying across the
    slope (k = 0.47), each labelled with what its tensor predicts and
    what the run measured: 3.03 against 3.03, 2.16 against 2.16, 2.06
    against 2.05.
  - **rolling friction is the other one**, and easy to confuse with
    the first: `rough` is the grip at a contact that makes a ball roll,
    `spin_slow` is the loss that afterwards stops it (a torque against
    the angular momentum, so the spin decays at the same rate whatever
    the shape). On the "r" key in the example.
  - **What is left**: `TinyMarbleMadness` behind `physics=engine`. Its
    marble runs on a height map, which is not a hitbox, so the port
    needs the course's cells turned into box hitboxes -- and then the
    game's own rules (slopes between cells, marbles that break,
    the steelie that pushes) all have to meet the engine at once. That
    is a bigger piece than the rest of phase 7 put together, and it
    belongs after phase 8's solver, when a body resting on a surface
    is something the engine can hold still.
- **Phase 8, DONE**: `Collide3d.manifold` (face clipping),
  `Solver3d` (sequential impulses, warm starting, Baumgarte, a bounce
  threshold), `Physics3d.world` / `simulate` with sleeping, 7 new
  tests, and `examples3d/PhysicsStack3d.ml`. Measured, a crate dropped
  on the floor and left for 300 steps:

  ```
     solved            y = 0.24500 (0.25 less the 5 mm slop), speed 0, asleep
     one pass per pair y = 0.25120, speed 0.0487 -- shivering, for ever
  ```

  and a tower of five, after 600 steps: every crate within 2 cm of
  where it started, sideways creep 3 mm, the whole tower asleep.
  - **Three bugs, each worth the finding.** The single-point box/box
    contact put its point at the midpoint of the two support points,
    which for a small box on a big floor is *a corner of the floor* --
    metres away, with a lever arm so long the impulse vanished and the
    crate fell through. It now uses the incident body's deepest point,
    and `Physics3d.bounce` uses the whole manifold anyway.
  - The manifold's clipping kept the *outside* of each side plane: the
    winding of a box's six faces is not all the same way round, so the
    plane is now oriented from the face's own middle, which cannot be
    got wrong.
  - Warm starting remembered friction as *two numbers* in the tangent
    basis, and `Resolve3d.tangents` jumps by a quarter turn when the
    normal wobbles across a tie -- replaying last step's friction
    along this step's axes. It now remembers a *vector*, which has no
    opinion about the basis.
  - **Sleeping had to be by islands.** Per-body sleeping put the
    bottom crate to sleep while the ones above were still settling,
    and something woke it a moment later with a jolt -- measured, once
    every 61 steps, which is the threshold plus one. Bodies that touch
    are now unioned into groups and a group sleeps only when every
    body in it is ready, which is what Box2D does. The tower's
    sideways creep fell from 2 cm to 3 mm with it.
  - `PhysicsStack3d` is the phase's demo and its argument: a brick
    wall and a row of dominoes, "s" to turn the solver off (the wall
    comes apart into a heap that never quite stops moving), "i" for
    the iteration count, "w" for warm starting, space to throw a ball
    through it, and a marker over every body still awake -- they go
    out one by one as it settles, and at seven seconds the scene
    solves *no* contacts at all.

## Verification

- `dune build` and `make test` at every phase: each `.mli`'s worked
  example, and the laws (momentum, angular momentum, energy,
  convergence orders, the analytic tensors and waterlines).
- Golden frames in `tests/3d/` for every new example and for the first
  frames of the three games (fixed step, so frame *n* is
  deterministic); `-script` runs for the ones that need input (the
  plunger, a gravity-gun shot, a portal crossing).
- Each phase's switch measured, not asserted: the pyramid without the
  solver, the pinball without sweeping, the broad phase's pair counts,
  the walk with and without the step offset.
- The ported games still play the same with `physics=engine` absent
  (byte-identical golden frames), which is what makes the flag a
  comparison rather than a rewrite.

## Out of scope

- Soft bodies, cloth, fluids (SPH), destruction: named in the notes,
  not built.
- Articulated-body dynamics (Featherstone, 1987) and proper vehicle
  dynamics: the raycast car is the ceiling, and it is a "later".
- GJK/EPA as the general convex test: the `.mli` names it, SAT on
  hulls and the primitive tests do the work.
- Convex decomposition of concave meshes: static geometry is a
  triangle soup, dynamic bodies are convex. A concave *moving* body is
  not supported, and the `.mli` says so.
- Rotational CCD (a long thin body sweeping while it turns), SIMD,
  threading, and more than a few hundred bodies at 60 Hz.
