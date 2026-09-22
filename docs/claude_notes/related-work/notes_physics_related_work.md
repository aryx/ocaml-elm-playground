# physics/ vs. the rest of the physics engine world

The physics twin of [`notes_playground_related_work.md`](notes_playground_related_work.md)
(2D graphics) and [`notes_playground3d_related_work.md`](notes_playground3d_related_work.md)
(3D): where the planned `physics/` engine and its Evan-style
`Physics` API (see [`plan_physics_teaching.md`](plan_physics_teaching.md))
come from, and where they sit among the real engines -- Chipmunk, Box2D
and the others. The same through-line as the other two notes: **the
real engines are designed to be as fast, as stable and as capable as
possible; `physics/` is designed to be as *legible* as possible**, and,
one level up, `Physics` to be as *simple to use* as possible. Box2D and
Chipmunk are excellent at what they're for; neither is meant to be read
by a student in an afternoon, nor used by a beginner in three lines.

## The one-line version

| | What it optimizes for | What you see / write |
|---|---|---|
| Box2D, Chipmunk (and Matter.js, Planck.js, Rapier 2D) | Robust, fast rigid-body games: stacking, joints, many bodies | A world object, body and shape definitions, handles, callbacks; the solver inside is tens of thousands of lines |
| Bullet, PhysX, Havok, Jolt, ODE | The same in 3D, for AAA games and robotics | Even more API surface; SIMD, multithreading, GPU paths inside |
| MuJoCo, N-body and molecular dynamics codes | Scientific accuracy (robots, galaxies, molecules) | Equations and parameters; the numerics are the point, but not written for beginners |
| Box2D Lite, Millington's Cyclone, Randy Gaul's ImpulseEngine | Teaching how a game physics engine works | Small readable engines, one book or article each |
| *The Nature of Code*, Fiedler's and Hecker's articles, SICM | Teaching the ideas: vectors, forces, integration, collisions | Prose and small programs, no engine |
| each game by hand (Mario, Pong, Asteroid today) | Just enough for one game | `vy - dt / 8.`, a `near` function, a vector type per game |
| `physics/` + `Physics` | Every number that moves a body readable, and a 3-line API | `circle white 10 \|> body \|> fall 800 \|> move` over one module per idea |

## Part 1: games, where game physics came from

The physics in games started as hand-written special cases, and the
games the playground now wants are the classics:

- **Tennis for Two** (William Higinbotham, 1958, on an oscilloscope):
  a ball with gravity bouncing over a net, computed by an analog
  computer -- arguably the first game with physics.
- **Spacewar!** (Steve Russell and friends, MIT, 1962, on the PDP-1):
  two ships with inertia, thrust and torpedoes, and a star in the
  middle whose gravity (added by Dan Edwards) pulls everything in; the
  whole simulation stepped in fixed-point arithmetic, in a few
  kilobytes. The game `games/Spacewar.ml` will recreate.
- **Lunar Lander** (text version 1969, Atari's vector arcade game
  1979), **Asteroids** (Atari, Lyle Rains and Ed Logg, 1979: inertia,
  thrust, a wrap-around screen -- `Asteroid.ml`'s original),
  **Gravitar** (Atari, 1982: planets with gravity), **Thrust** (Jeremy
  Smith, 1986: a pod on a rope, a swinging pendulum under gravity), and
  **XPilot** (Bjørn Stabell and Ken Ronny Schouten, 1991: multiplayer
  over the network, ships, gravity, walls) -- the "cave with gravity"
  game the plan leaves for later.
- **Angry Birds** (Rovio, 2009) and **Limbo** (Playdead, 2010): both on
  Box2D; the moment a general physics engine became *the* game. The
  plan's third game, `games/Slingshot.ml`, is an Angry Birds-like: the
  test of whether `physics/` can stack boxes as steadily as Box2D.

## Part 2: the engines

### Before Chipmunk and Box2D

2D physics existed in games long before general engines, each game
writing its own:

- **The Incredible Machine** (Kevin Ryan, Dynamix/Sierra, 1993): a
  puzzle game *about* physics -- balls, ropes, conveyor belts, gravity
  -- with its own simulation; Crayon Physics Deluxe (Petri Purho, 2009)
  and Phun/Algodoo (Emil Ernerfeldt, 2008) are its descendants.
- **Metanet Software's N** (Raigan Burns and Mare Sheppard, 2004-2005):
  a platformer with tight physics, and its famous online tutorials,
  "Collision Detection and Response" and "Broad-Phase Collision" --
  the separating axis theorem and grids explained for game programmers,
  with interactive Flash demos; still one of the best introductions
  (Collide and Broadphase in the plan).
- **Flash engines**, the web games' physics before Box2D was ported:
  **APE** (Alec Cove, ActionScript Physics Engine, 2006: particles and
  Verlet constraints, after Jakobsen's Hitman article), **Motor2**
  (Michael Baczynski, 2006: impulses, close to Box2D), **Fisix**
  (2007); then Box2DFlash made Box2D itself the standard.
- **3D engines already existed** (ODE, 2001; MathEngine's Karma, around
  1998; Havok, 2000; Newton Game Dynamics, 2003), used in 2D by
  ignoring a dimension; Box2D Lite (2006) was a 2D engine written from
  scratch, and small enough to read, which is what made it the model.

### Chipmunk (2007) -- 2D, C, simple

Scott Lembcke's Chipmunk (now Chipmunk2D): a 2D rigid-body engine in
C, small and fast, MIT-licensed, used by cocos2d and many mobile games;
its Python binding, Pymunk, is a common way physics is taught with
code. Its API is the standard shape of an engine: a *space* (the
world) with a gravity; *bodies* (mass, moment of inertia, position,
velocity) and *shapes* attached to them (circles, segments, polygons,
with friction and elasticity); *constraints* (pins, springs, motors);
*collision handlers* (callbacks); and `cpSpaceStep(space, dt)`. Roughly:

```c
cpSpace *space = cpSpaceNew();
cpSpaceSetGravity(space, cpv(0, -100));
cpBody *ball = cpSpaceAddBody(space, cpBodyNew(mass, cpMomentForCircle(mass, 0, r, cpvzero)));
cpSpaceAddShape(space, cpCircleShapeNew(ball, r, cpvzero));
cpBodySetPosition(ball, cpv(0, 15));
for (;;) cpSpaceStep(space, 1.0/60.0);
```

Every call a handle and a side effect: fine for C, the opposite of
Evan's style.

### Box2D (2006-) -- the standard

Erin Catto's Box2D: it started as **Box2D Lite**, the small demo
engine of his GDC 2006 talk (about a thousand lines: boxes, contacts,
sequential impulses), and grew into the most used 2D engine (Angry
Birds, Limbo, Unity's 2D physics, ports to every language: Planck.js
in JavaScript, JBox2D in Java). What makes it good is the solver:
sequential impulses with warm starting (Catto, "Iterative Dynamics
with Temporal Coherence", GDC 2005), continuous collision for fast
bodies, sleeping bodies, a dynamic AABB tree for the broad phase.
Version 3 (2024) is a rewrite in C, faster, multithreaded. Its API is
Chipmunk's shape again, with *definitions* passed to factories:

```cpp
b2World world(b2Vec2(0.0f, -10.0f));
b2BodyDef def; def.type = b2_dynamicBody; def.position.Set(0.0f, 4.0f);
b2Body* body = world.CreateBody(&def);
b2PolygonShape box; box.SetAsBox(1.0f, 1.0f);
body->CreateFixture(&box, 1.0f);           // density
world.Step(1.0f / 60.0f, 6, 2);            // velocity and position iterations
```

Box2D Lite is the closest precedent to `physics/`: an engine written to
be read. The differences: `physics/` starts from integration (the
computational physics Box2D Lite takes for granted), is one idea per
module with a worked example and a test each, keeps the simple
versions next to the better ones behind keys, and has tests of the
conservation laws.

### The others

- **Matter.js** (Liam Brummitt, 2014), **p2.js** (Stefan Hedman):
  JavaScript 2D engines, the web's Box2D alternatives.
- **Rapier** (Dimforge, Rust, 2020): 2D and 3D, with cross-platform
  determinism as a feature (the same bits on every machine: what
  lockstep networking and replays need -- this project's fixed time
  step for golden frames, at industrial scale).
- **3D**: **ODE** (Russell Smith, 2001), **Bullet** (Erwin Coumans,
  open source since the mid-2000s, used in films and robotics),
  **Havok** (1998-, Half-Life 2 and hundreds of games), **PhysX**
  (NovodeX, then Ageia's physics card, then NVIDIA's GPUs), **Jolt**
  (Jorrit Rouwe, Guerrilla Games, open source, Horizon Forbidden West),
  and **cannon.js** (Stefan Hedman) in JavaScript.
- **Science**: **MuJoCo** (Emo Todorov, 2012, open-sourced by DeepMind:
  robots, contact-rich control), N-body codes for galaxies (Barnes-Hut
  trees), molecular dynamics (where Verlet's integrator comes from,
  1967). Same equations, different priorities: accuracy over speed,
  and error bars.

### In Elm and OCaml

- **elm-physics** (Andrey Kuzmin): 3D rigid-body physics for Elm,
  ported from cannon.js, pairing with elm-3d-scene for drawing. The
  same author wrote elm-flatris, which `Tetris.ml` ports. Its
  API is functional (bodies are values, the world is stepped as a
  value), the natural reference for `Physics`'s design; its engine is a
  port of a real one rather than a teaching one.
- Evan's elm-playground has no physics at all: its games (Mario) do it
  by hand, the model this project's games copied.
- In OCaml, as far as I know, no physics engine in common use: bindings
  to C engines, and games' own code. A gap `physics/` fills for this
  project's purpose.

## Part 3: the teaching lineage

Where the ideas are taught best, and what `physics/` borrows from each:

- **Richard Feynman, *The Feynman Lectures on Physics*, vol. 1,
  chapter 9 (1963)**: Newton's laws, then an orbit computed by hand in
  small steps -- computational physics' founding page, and the model
  for `notes_2d_physics.md`'s first sections.
- **Gerald Jay Sussman and Jack Wisdom, *Structure and Interpretation
  of Classical Mechanics* (2001)**: mechanics taught with Scheme, every
  equation a program you run -- the functional-programming ancestor of
  this project's approach (Sussman and Wisdom also showed, with a
  special-purpose computer, that Pluto's orbit is chaotic, 1988).
- **Daniel Shiffman, *The Nature of Code* (2012; 2nd edition with
  p5.js, 2024)**: vectors, forces, oscillation, particles, then physics
  libraries (Box2D, Matter.js), for artists, in Processing -- the
  closest in spirit to the Playground's audience.
- **Glenn Fiedler's articles** (gafferongames.com: "Integration
  Basics", "Fix Your Timestep!", 2004-2006): integrators and the fixed
  time step, the clearest short treatment.
- **Chris Hecker's "Physics" columns** (Game Developer, 1996-97):
  rigid bodies and collision response derived for game programmers;
  **David Baraff and Andrew Witkin's "Physically Based Modeling"**
  (SIGGRAPH course notes, 1997-2001): the same with the full math.
- **Christer Ericson, *Real-Time Collision Detection* (2005)**: the
  reference for §8-9, every test with its code.
- **David M. Bourg, *Physics for Game Developers* (O'Reilly, 2002; 2nd
  edition with Bryan Bywalec, 2013)**: the physics itself -- kinematics,
  forces, projectiles (with drag and wind: the artillery game), rigid
  bodies, collisions -- explained for programmers, with code; the book
  between the physics textbooks and the engine code.
- **Ian Millington, *Game Physics Engine Development* (2007)**: builds
  a whole engine (Cyclone) chapter by chapter, particles first, then
  springs, then rigid bodies -- the progression `physics/`'s phases
  follow; **Randy Gaul's "How to Create a Custom 2D Physics Engine"**
  (2013) and his ImpulseEngine: the same in a few articles.
- **Numerical analysis**: Hairer, Lubich and Wanner, *Geometric
  Numerical Integration* (why symplectic integrators keep orbits
  closed); Press et al., *Numerical Recipes* (RK4 and friends);
  Giordano and Nakanishi, *Computational Physics* (the undergraduate
  course).

## Where `physics/` and `Physics` actually sit

Two levels, like graphics:

- **`physics/2d/`, the engine**, at the legible end: one idea per
  module (integration, forces, collision tests, broad phase, impulses),
  each `.mli` with its diagram, worked example and reference, the
  simple and the better version side by side (four integrators, three
  broad phases), keys to switch them live, a debug overlay to see
  velocities, contacts and the grid, and tests of both the worked
  examples and the laws (momentum, energy, convergence orders). Box2D
  Lite's goal -- an engine to read -- pushed further, and starting one
  step earlier, at the numerics.
- **`Physics`, the API**, at the simple end: one concept, a body made
  from a shape, and verbs in `update` (`move`, `fall`, `attracted_by`,
  `thrust`, `touching`, `bounce`), values all the way, no world object,
  no handles, no callbacks. Evan's style applied to what Chipmunk and
  Box2D do with a space, factories and a step function: a beginner
  writes

  ```ocaml
  let ball = circle red 20 |> body |> at 0 200
  ...
  update: ball |> fall 800 |> move |> keep_in computer.screen
  view:   [ draw ball ]
  ```

The ceiling is real, and deliberate: no stacking of a thousand boxes, no
joints beyond springs, no continuous collision beyond the basics, no
3D until later. What's there is enough for Asteroid, Spacewar!,
Slingshot, Mario, Pong, and a cave game, and small enough to read.

## Postscript: the numbers (to come)

As for graphics (Cairo vs ours, the GPU vs ours), once built: lines of
code of `physics/2d/` against Box2D Lite and Box2D, bodies at 60 frames
per second in the stress scene, and the ported games' line counts before
and after.

Sources: from memory, to be checked before relying on them for
teaching -- the books and articles named above, the documentation of
Chipmunk2D, Box2D, Matter.js, Rapier, elm-physics, and general
knowledge of the games' and the Flash engines' history (names and dates
of the pre-Box2D engines especially).
