# physics/3d/ vs. the rest of the 3D physics world

Where a small teaching 3D engine sits among Havok, PhysX, Bullet and
Jolt -- what those do that this will not, and which of their ideas are
small enough to be written out and read. The 2D half of the story is
[`notes_physics_related_work.md`](notes_physics_related_work.md) (the
engines, the teaching lineage, Elm and OCaml), and is not repeated:
this note is about what the third dimension changes, in the field as
much as in the code. Companions:
[`notes_3d_physics.md`](../tutorials/notes_3d_physics.md) (how it
works) and
[`plan_physics3d_teaching.md`](../plans/plan_physics3d_teaching.md)
(what gets built, in what order).

## The one-line version

| | What it optimizes for | What you see / write |
|---|---|---|
| Havok, PhysX, Jolt | AAA games: thousands of bodies, ragdolls, vehicles, destruction, multi-threading, console SIMD | A world, actors, shapes, filters, callbacks; hundreds of thousands of lines, most of it not about physics |
| Bullet, ODE | The same, open source, also robotics and film | A big C++ API and a dispatcher of collision algorithms; PyBullet made it the robotics default |
| MuJoCo, Drake, Isaac | Contact-rich control and robotics research | Models in XML, generalized coordinates, differentiable or fast-resettable; accuracy and reproducibility over feel |
| Rapier 3D, cannon.js, ammo.js, Jolt's JS build | Physics in a browser or in Rust/Wasm | A world stepped per frame, bodies as handles; cannon.js is the readable one, ammo.js is Bullet compiled |
| elm-physics | Functional 3D physics: bodies and the world as immutable values | `World.simulate (1/60) world` -- the closest thing to this project's design, in Elm |
| Millington's Cyclone, Bullet's and Jolt's sample code | Teaching how a 3D engine is built | One book, or a large sample browser |
| each game by hand (`TinyMinecraft`, `TinyMario64`, `TinyMarble` today) | Just enough for one game | A box moved one axis at a time; `5/7 g sin a` derived in a header |
| `physics/3d/` + `Physics3d` | Every number that turns a body readable, and the same 3-line API as 2D | `cube white 1. \|> body \|> fall 9.8 \|> step`, over one module per idea |

## Part 1: the games that made 3D physics a feature

3D physics arrived in games as a *simulation* first and a *toy* second,
and the order matters for what is worth teaching.

- **Trespasser** (DreamWorks Interactive, 1998), the Jurassic Park
  game: famously built around a full rigid-body simulation -- a whole
  arm physically simulated as the player's hand, crates and doors
  everywhere -- and famously undone by it. It is the field's cautionary
  tale: simulation everywhere, with 1998's solvers and no design
  around the failure cases.
- **Half-Life 2** (Valve, 2004, on Havok): the opposite lesson. The
  physics is *bounded* -- a ramp puzzle here, a seesaw there, barrels
  that float, corpses that go limp -- and one object, the gravity gun,
  turns the whole system into a verb the player holds. Then **Garry's
  Mod** (2004) made the sandbox itself the game.
- **Portal** (Valve, 2007): almost no new physics at all, and a new
  genre. The portal is a rigid transform applied to a body's position,
  velocity and orientation; "speedy thing goes in, speedy thing comes
  out" is a design rule stated as an equation. What it costs is in the
  *renderer*, not the solver.
- **Pinball simulations** are the fidelity end of the same field: a
  27 mm ball at 10 m/s among thin flippers, rubbers and targets breaks
  naive engines outright, and the enthusiast simulators (Visual
  Pinball and its physics revisions) are largely arguments about
  contact models and time steps. A good reminder that "3D physics" and
  "fast small sphere" are nearly different problems.
- **Marble Madness** (1984) and **Super Monkey Ball** (Amusement
  Vision, 2001): rolling as the whole game -- and mostly done by
  *cheating* (a ball stuck to a height field), which `TinyMarble.ml`
  does here too, and which §11 of the tutorial cross-checks against a
  real rolling sphere.
- **Kerbal Space Program** (2011), **BeamNG.drive** (soft-body cars),
  **Red Faction: Guerrilla** (2009, destruction): the three directions
  past rigid bodies -- orbital mechanics, soft bodies, fracture -- all
  out of scope here, and worth naming so the ceiling is visible.

(Names and dates from memory, to check.)

## Part 2: the engines

- **Havok** (Havok, Dublin, from around 1998-2000): the commercial
  standard of the 2000s, Half-Life 2's engine and hundreds of others;
  closed, later bought by Intel and then Microsoft.
- **PhysX**: NovodeX (2001), then Ageia -- which sold an actual
  *physics card* in 2006 -- then NVIDIA, who made it run on GPUs and
  eventually open-sourced it. The card is a nice historical marker:
  for a couple of years physics was believed to need its own silicon.
- **ODE**, Open Dynamics Engine (Russell Smith, 2001): the open one
  everybody used first; a maximal-coordinates solver with joints, and
  the reason a generation of hobby 3D games all felt the same.
- **Bullet** (Erwin Coumans, mid-2000s on): the open-source default
  for a decade, in films (it has Academy credits) and, through
  PyBullet, in robotics and reinforcement-learning papers. Its
  `btDbvt` dynamic AABB tree and its speculative-contact work are
  directly behind §8 and §12 of the tutorial.
- **Jolt** (Jorrit Rouwe, Guerrilla Games; used in Horizon Forbidden
  West, open-sourced around 2021): the modern one -- deterministic,
  multi-threaded, with a documented architecture that is unusually
  readable for its size. Where Bullet was the thing to port from, Jolt
  is the thing to read.
- **Rapier 3D** (Dimforge, Rust): cross-platform determinism as a
  headline feature, which is exactly this project's fixed-step,
  golden-frame discipline taken seriously at scale.
- **cannon.js** (Stefan Hedman) and **ammo.js** (Bullet through
  Emscripten): the browser's two, and the reason elm-physics exists.
- **MuJoCo** (Emo Todorov, 2012; open-sourced by DeepMind, 2021) and
  the robotics simulators: generalized coordinates instead of
  maximal ones, soft contacts chosen for solvability rather than
  realism, and resets that must be bit-exact. Same equations, an
  entirely different set of priorities -- worth knowing so that "my
  engine is not like MuJoCo" is not read as a defect.

What all of them have that this will not: multi-threading and SIMD,
convex decomposition of arbitrary meshes, GPU paths, articulated-body
solvers (Featherstone, 1987), vehicles with real tire models,
destruction, soft bodies and cloth, and scene formats. A rough count,
from memory: Jolt and Bullet are each in the hundreds of thousands of
lines; `physics/2d/` is under 2,000 for the whole engine, and
`physics/3d/` is planned at the same order of magnitude.

## Part 3: the teaching lineage, in 3D

The 2D note's lineage (Feynman, Shiffman, Fiedler, Hecker, Catto,
Ericson, Bourg, Millington, Gaul) all still applies -- integration and
impulses are dimension-agnostic. What is specifically 3D:

- **David Baraff and Andrew Witkin, "Physically Based Modeling"**
  (SIGGRAPH course notes, 1997-2001): the rigid-body derivation in
  full -- quaternions, the inertia tensor, `I_world = R I R^T`, the
  impulse with its tensor terms. Every engine's core is a restatement
  of these notes.
- **Ian Millington, *Game Physics Engine Development* (2007)**: builds
  a 3D engine (Cyclone) from particles up; the book-shaped counterpart
  of this plan's phases.
- **Christer Ericson, *Real-Time Collision Detection* (2005)**: chapter
  5 (closest points, segment-segment for capsules), chapter 9 (SAT and
  the 15 axes). The single most useful book for §7.
- **David H. Eberly, *Game Physics* (2003)** and the Wild Magic /
  Geometric Tools code: the heavier mathematical treatment, with every
  intersection case written out.
- **Erin Catto's GDC talks** (2005-2014): sequential impulses, contact
  manifolds by clipping (2007), soft constraints (2011) -- 2D talks
  whose content is what the 3D solvers do, one dimension up.
- **Ken Shoemake, "Animating Rotation with Quaternion Curves"**
  (SIGGRAPH 1985): how quaternions entered graphics, and still the
  clearest derivation of the half-angle.
- **Gino van den Bergen, *Collision Detection in Interactive 3D
  Environments* (2003)** and the SOLID library: GJK and EPA from the
  person who made them practical.
- **Quake's source** (id Software, 1996; released 1999): `SV_FlyMove`,
  40 lines, is still the character controller everyone writes
  (tutorial §14). Reading it is worth more than reading a modern
  engine's `CharacterController` class.

## Part 4: in Elm and OCaml

- **elm-physics** (Andrey Kuzmin) is the closest relative this plan
  has: 3D rigid bodies in Elm, ported from cannon.js, the world and
  the bodies immutable values, stepped functionally
  (`World.simulate`), drawn with elm-3d-scene. Its `RaycastCar`
  example is the model for a future vehicle here, and its API is
  evidence that the functional shape works at this size. The
  difference is purpose: it is a port of a real engine, meant to be
  *used*; `physics/3d/` is meant to be *read*, one idea per module,
  with the simple version kept beside the better one.
- **elm-playground-3d** (lucamug), which this project's `Playground3d`
  follows for its shapes, has no physics.
- In OCaml: no 3D physics engine in common use, as far as I know --
  bindings to C engines where it comes up. As in 2D, that makes this
  a gap worth filling for teaching, and not one worth claiming as a
  product.

## Where `physics/3d/` and `Physics3d` will sit

Two levels, as everywhere else in this project:

- **`physics/3d/`, the engine**, at the legible end: `Quat` and `Mat3`
  so orientation and inertia are readable rather than assumed;
  `Collide3d` with the 15 axes written out and named; `Solver3d` with
  sequential impulses and warm starting, each switchable so the pile
  can be watched slumping without them; `Sweep3d` with a key to turn
  continuous collision off and lose the pinball through the table.
  Every `.mli` with its diagram, its worked example and its reference.
- **`Physics3d`, the API**, at the simple end: the *same verbs as the
  2D API*, which is the point -- a reader who wrote a 2D game with
  `fall`, `step`, `touching` and `bounce` writes a 3D one with them
  too, and meets quaternions and tensors only if they open the engine.

And the ceiling, stated now rather than discovered later: convex
moving bodies only (concave means a static mesh or several pieces),
SAT and primitives rather than GJK/EPA, no articulated solver, no
vehicles beyond a raycast car, no soft bodies, cloth, fluids or
destruction, no threading or SIMD, and a budget of a few hundred
bodies at 60 Hz on the software backend rather than thousands. A
pinball table, a room of crates and barrels, a ragdoll and a portal --
which is exactly the set of games the plan commits to, and nothing
larger.

## Postscript: the numbers (to come)

Once built, and in the same spirit as the graphics notes' comparisons:
lines of code of `physics/3d/` against cannon.js and Jolt; bodies at
60 frames per second in `PhysicsStack3d.ml`, with and without the
broad phase, the solver and sleeping; the pinball kept on the table at
10 m/s with `Sweep3d` and lost without it; and the ported games'
`physics=engine` frames against their hand-written ones.

Sources: from memory, to be checked before relying on them for
teaching -- the books and talks named above, the documentation of
Bullet, Jolt, PhysX, Rapier, cannon.js and elm-physics, and general
knowledge of the games' history (the dates of Havok, Novodex and
Trespasser especially).
