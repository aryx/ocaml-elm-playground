# Plan: physics for the playground, from scratch, for teaching (`physics/`)

## Context

`graphics/` teaches how pictures are computed: small modules, each
explaining one classic algorithm, with ASCII diagrams, worked examples
checked by tests, and the papers that introduced them
(`done/plan_software_2d.md`, `done/plan_code_reorg_teaching_3d.md`).
This plan does the same for *motion*: a small 2D physics engine (3D
later), under `physics/`, that teaches **computational physics** --
how a computer turns Newton's laws into positions, one time step at a
time, and what goes wrong when it does it naively -- and, on top of
it, the collision detection and response games need.

And, like `Playground` for pictures, a small **Evan-style API** over
it, so that a game says what happens, not how: a ship is a body with a
velocity, gravity makes it fall, a bullet that touches an asteroid
destroys it. Today every game does its own physics by hand:

- `examples/Mario.ml`: gravity and jumping, `vy - dt / 8.`, with a
  hard-coded `dt = 1.666`;
- `games/Pong.ml`: velocities, bouncing off walls, a `near`/`within`
  test for paddle hits (its TODO: "physics engine (friction,
  bounciness)");
- `games/Asteroid.ml`: thrust, velocities, wrap-around, circle-circle
  collisions (its `resolved_shape` has a `(* TODO | Poly *)`), a
  `vector` type of its own;
- `games3d/StarCollector3d.ml`: movement, distance-based pickups.

**The flagship goals: Asteroid rewritten on the physics API**, shorter
and more correct (exact polygon hits instead of circles), **a new game
with gravity, Spacewar!** (two ships around a star), **and a new
Angry Birds-like game, Slingshot** (towers of boxes to knock down: the
real test of an engine, see "Games"); Mario and Pong after them.

Companions: [`notes_2d_physics.md`](notes_2d_physics.md), the tutorial
(written ahead of the code, as its specification), and
[`notes_physics_related_work.md`](notes_physics_related_work.md)
(Chipmunk, Box2D and the rest, and the teaching lineage).

## Principles (the same as `graphics/`)

- **Independent of the Playground.** `physics/2d/` knows vectors,
  bodies, shapes, time steps: no `Playground.shape`, no `computer`.
  `playground/Physics.ml` is the adapter, like
  `Shape_render_software` for graphics.
- **One idea per module, one feature per function**, the simple and
  the better version side by side, switchable (e.g. the four
  integrators), so their difference can be *seen*.
- **Every `.mli` explains its idea** with an ASCII diagram, a worked
  example with numbers, and the paper or book that introduced it;
  `physics/tests/` checks the examples.
- **Physics has laws to test against.** Beyond worked examples, the
  tests check what physics guarantees: a projectile's parabola against
  the closed form, a spring's period, momentum conserved by every
  collision, energy conserved by an elastic one, and how far each
  integrator drifts from it (the numbers in the notes).
- **Deterministic.** A fixed time step, no wall clock inside the
  engine: the same inputs give the same frames, so physics examples
  can have golden frames (`tests/2d/`) like the others.
- **Optimizations keep their simple version** (`Opti`, "o"), e.g. the
  broad phase.
- **Comments describe the code as it is**; long explanations in the
  `.mli`s and the notes.

## The Playground API, Evan-style

The goal is an API as small and as elegant as Evan's for pictures:
few concepts, all values, words a beginner already knows (`circle`,
`move`, `rotate`, `computer`, `game`). Physics adds **one concept, the
body -- a shape that moves** -- and verbs on it, applied in `update`,
the same way `move` and `rotate` apply to shapes in `view`. Everything
Box2D or Chipmunk have (velocity, mass, inertia, forces, gravity,
collisions, bounciness, friction, rotation) is there, but as values and
verbs, not a world object, callbacks and handles.

A body is made *from a shape*, so a beginner never writes a hitbox or a
mass: `circle` gives a circular hitbox, `rectangle` a box, `polygon`
and `ngon` a polygon, `words` and `image` their box; the mass comes
from the area (heavier when bigger), the moment of inertia from the
shape too. `draw` gives the shape back, moved and rotated to where the
body is. Tentative (`playground/Physics.mli`), to be refined by writing
the games with it:

```ocaml
(* a shape that moves: a record, like computer *)
type body = {
  shape : shape;                   (* what it looks like, and its hitbox *)
  x : number; y : number;          (* where it is *)
  vx : number; vy : number;        (* its velocity, pixels per second *)
  angle : number; spin : number;   (* degrees, degrees per second *)
  mass : number;                   (* from the shape's area, by default *)
  bounciness : number;             (* 0. a lump of clay .. 1. a superball *)
}

val body : shape -> body                     (* at (0, 0), still *)
val at : number -> number -> body -> body    (* like move, for bodies *)
val moving : number -> number -> body -> body
val heavy : number -> body -> body           (* its mass *)
val bouncy : number -> body -> body

(* in update, once per tick (one fixed time step, see the notes) *)
val move : body -> body                      (* velocity -> position *)
val fall : number -> body -> body            (* uniform gravity *)
val attracted_by : body -> body -> body      (* Newton's gravitation *)
val push : number -> number -> body -> body  (* a force: thrust, wind *)
val thrust : number -> body -> body          (* a push forward, along angle *)
val turn : number -> body -> body            (* spin *)
val slow : number -> body -> body            (* drag, friction *)
val wrap : screen -> body -> body            (* Asteroids' screen *)
val keep_in : screen -> body -> body         (* Pong's walls, bouncing *)

val touching : body -> body -> bool          (* collision detection *)
val bounce : body -> body -> body * body     (* collision response *)
val step : body list -> body list            (* all of it, for many bodies *)

(* in view *)
val draw : body -> shape                     (* the shape, where the body is *)
```

A gravity game's update then reads like its rules (Spacewar!, below):

```ocaml
let update computer game =
  let ship =
    game.ship
    |> turn (-4 * to_x computer.keyboard)
    |> thrust (if computer.keyboard.kup then 300 else 0)
    |> attracted_by game.star
    |> move
    |> wrap computer.screen
  in
  if touching ship game.star then explode game else { game with ship }

let view computer game = draw game.star :: draw game.ship :: stars
```

and Asteroid's collisions:

```ocaml
let hit asteroid = List.exists (touching asteroid) bullets in
let asteroids = List.filter (fun a -> not (hit a)) asteroids in
```

Open questions, to settle in phase 3 by writing the games with it: a
record (Evan's style for `computer`, and lets games read `ship.x`) vs an
abstract type; whether `move` takes the `computer` or always one fixed
step; a `world` for `step` (bodies plus options) vs a plain list; how a
game keeps its own data next to a body (a record containing bodies, as
above, is the Elm way).

## Games

- **Asteroid, ported** (the first flagship): movement (`thrust`,
  `turn`, `slow`, `wrap`), exact hits (bullets are points in the
  asteroids' polygons, the ship's polygon against theirs), instead of
  its own vectors and circles. Shorter, and more correct.
- **Spacewar!, new** (`games/Spacewar.ml`, the second flagship): the
  1962 PDP-1 game (Steve Russell et al., MIT), the first video game with
  physics, and Asteroids' own ancestor (see `games/Asteroid.ml`'s
  header): two ships, one keyboard (arrows and w/a/s/d), duelling
  around a star whose gravity pulls ships and torpedoes; inertia,
  thrust, orbits, slingshots. Everything the engine teaches, in one
  game: gravitation, integration (a bad integrator makes the orbits
  wrong -- try it with the integrator key), collisions.
- **Slingshot, new** (`games/Slingshot.ml`, the third flagship, an
  Angry Birds-like -- the game that made Box2D famous): pull a
  projectile back with the mouse and let go (a spring's force, or
  directly a launch velocity proportional to the pull, with the
  predicted arc drawn as dots: the parabola of section 4 of the
  notes), towers of boxes and planks that stand still until hit, then
  tumble, slide and topple, and targets that break when hit hard
  enough (the impulse of the hit, section 10). It needs everything:
  gravity, rotation, friction, and above all **stable stacking** --
  a tower that doesn't jitter or sink while waiting -- which is why
  stacking (sequential impulses, warm starting) is a real phase below,
  not a "later".
- **Later, a cave level à la XPilot or Thrust**: gravity plus static
  walls to land on and bounce off (polygons against a terrain),
  fuel, landing pads -- Gravitar/Lunar Lander territory.
- **Mario and Pong, ported**: `fall` and jumping; `keep_in` and
  `bounce` for the ball.

## Target layout

```
graphics/2d/geometry/     Vec2 (exists) -- moves to package elm_playground,
                          see "Groundwork" below
physics/2d/               (physics_2d, private, package elm_playground: pure
                          OCaml, so it runs on the web backend too)
  Body                    position, velocity, mass, angle, spin; the state
  Integrate               one time step: explicit Euler, semi-implicit
                          (symplectic) Euler, Verlet, RK4
  Force                   uniform gravity, Newton's gravitation (N bodies),
                          springs (Hooke) and damping, drag
  Energy                  kinetic, potential, momentum: the diagnostics
  Shape                   hitboxes: circle, box, convex/any polygon; area,
                          moment of inertia, bounding box
  Collide                 narrow phase: circle/circle, box/box, point in
                          polygon, segment/segment, SAT, (later) GJK
  Contact                 a collision's normal, depth and points
  Broadphase              which pairs to test: all pairs, uniform grid,
                          sort and sweep
  Resolve                 impulses: restitution, friction, positional
                          correction
  World                   the step: forces -> integrate -> detect ->
                          resolve, with an options record (like Render)
physics/3d/               (later) the same ideas in 3D, for games3d/
physics/tests/            the worked examples and the conservation laws
playground/Physics.ml     the Evan-style API above, over physics/2d
```

(The empty `physics/collision/`, `gravity/`, `mechanics/` placeholders
go: those three themes are the modules above.)

## Groundwork decisions

### Vector math shared by graphics and physics

`Vec2` is in `graphics/2d/geometry/` (library `graphics_2d_geometry`,
package `elm_playground_software`), but physics must be usable by any
backend, the web one included, so it can't depend on the software
backend's package. Proposal: move `graphics_2d_geometry` (pure, no
dependency) to package `elm_playground`; `elm_playground_software`
already depends on it. (Or a new top-level `geometry/`; the move is
smaller.) `Vec2` gains what physics needs (`neg`, `perp` exists,
`cross` of a scalar and a vector for angular velocity).

### Time: a fixed step per tick

`computer.time` is the wall clock; Evan's games (Mario) assume one tick
per animation frame, at 60 Hz. The engine takes a `dt` and never reads
a clock; the Playground API uses a fixed `dt = 1/60` s per tick. Why
fixed and not "the real time since the last frame": a variable step
makes the simulation depend on the frame rate (a jump height that
varies with the machine), can explode on a slow frame, and breaks
determinism (Glenn Fiedler, "Fix Your Timestep!", 2004). The price: a
game runs slower when frames are slow (Mario's behavior today). The
accumulator that decouples the two is explained in the notes, and
possible later.

### Debug drawing and keys

Seeing the physics matters as much as seeing the pixels: hitboxes,
velocity and force arrows, contact points and normals, the broad
phase's grid. `Physics.debug : body list -> shape list` draws them as
ordinary shapes (so on every backend); with `-debug-keys`, keys switch
the integrator, the broad phase, and the debug drawing (e.g. "e"
integrator, "v" vectors, "g" grid; not the graphics keys). A physics
options record, a `ref` like `Opti.enabled`, is the one global.

## The modules, with their references

(To double-check against the sources when writing each `.mli`.)

- **Integrate**: explicit Euler (Euler, *Institutionum calculi
  integralis*, 1768); semi-implicit/symplectic Euler; Verlet (Loup
  Verlet, "Computer 'Experiments' on Classical Fluids", Physical
  Review, 1967; Störmer before him); RK4 (Runge 1895, Kutta 1901).
  Worked example: a ball thrown up at 10 m/s, g = 10 m/s^2, dt = 0.1 s:
  after 5 steps, explicit Euler says 4.0 m, semi-implicit Euler 3.5 m,
  Verlet and RK4 the exact 3.75 m (see `notes_2d_physics.md` section 4). Feynman's
  *Lectures on Physics*, vol. 1, chapter 9 (planetary motion stepped
  by hand); Hairer, Lubich, Wanner, *Geometric Numerical Integration*
  (why symplectic methods keep orbits closed).
- **Force**: Newton's *Principia* (1687): F = ma, gravitation;
  Hooke's law (1678); damping. The N-body direct sum (O(n^2)), and a
  pointer to Barnes-Hut (1986) as the next step.
- **Energy**: the conserved quantities as the integrators' referee.
- **Shape**: areas and moments of inertia (disk, box, polygon via
  triangles).
- **Collide**: Christer Ericson, *Real-Time Collision Detection*
  (2005), the reference for all of it; point in polygon by crossing
  number (Jordan curve theorem); the separating axis theorem (Gottschalk,
  Lin, Manocha, "OBBTree", SIGGRAPH 1996, for its use in graphics);
  GJK (Gilbert, Johnson, Keerthi, 1988).
- **Broadphase**: uniform grids; sort and sweep (Baraff, PhD thesis,
  1992; I-COLLIDE, Cohen et al., 1995).
- **Resolve**: impulses (Chris Hecker, "Physics" columns, Game
  Developer, 1996-97; David Baraff, "Physically Based Modeling",
  SIGGRAPH course notes); restitution (Newton's experimental law of
  impact); Coulomb friction; sequential impulses (Erin Catto,
  "Iterative Dynamics with Temporal Coherence", GDC 2005, and Box2D
  Lite).

## New examples

Each small, each showing one idea, each deterministic (golden frames):

- `examples/Orbit.ml`: a planet around a star; switch the integrator
  and watch explicit Euler spiral out, Verlet stay on its ellipse --
  computational physics' first lesson, visible.
- `examples/Springs.ml`: a mass on a spring, a chain, a rope (Verlet
  with distance constraints: Jakobsen, "Advanced Character Physics",
  GDC 2001).
- `examples/Bounce.ml`: balls falling, bouncing on the floor and off
  each other, with bounciness from clay to superball.
- `examples/Boxes.ml` (with rotation): boxes tumbling, a small pile,
  then a pyramid that must stand (the stacking test, before Slingshot).

## Phasing

0. **Groundwork**: `graphics_2d_geometry` to package `elm_playground`;
   `physics/2d/` and `physics/tests/` skeletons; `playground/Physics.mli`
   stub.
1. **Integration, the computational physics core**: `Body`,
   `Integrate` (the four methods, switchable), `Energy`; `Orbit.ml`.
   Tests: projectile vs closed form; a harmonic oscillator's period and
   energy after 1000 steps per method (the numbers in the notes); the
   order of each method (halve dt, measure the error).
2. **Forces**: `Force` (uniform gravity, gravitation, springs, drag);
   `Springs.ml`. Tests: the N-body momentum conserved; a damped spring's
   decay.
3. **The Playground API, v1** (no collisions yet): `Physics.body`,
   `move`, `fall`, `attracted_by`, `push`, `thrust`, `turn`, `slow`,
   `wrap`, `keep_in`, `draw`. Port Mario's jump and Asteroid's movement
   (thrust, drag, wrap) to it, write Spacewar!'s flight (a ship around a
   star), and settle the open questions above by writing them.
4. **Collision detection**: `Shape`, `Collide`, `Contact`; `touching`,
   `Physics.debug`. Asteroid's hits exact: bullets are points in the
   asteroids' polygons, the ship's polygon against theirs (segments),
   replacing its circles; Spacewar!'s torpedoes and star. Tests: each
   test's worked example, and SAT against brute force on random convex
   polygons.
5. **Collision response**: `Resolve` (restitution, friction, positional
   correction); `bounce`, `step`; `Bounce.ml`; Pong on it. Tests:
   momentum conserved exactly, kinetic energy with bounciness 1, the
   worked impulse example.
6. **Broad phase**: `Broadphase`, all pairs vs grid vs sort and sweep,
   switchable, counters of pairs tested; a stress scene (hundreds of
   balls) measured.
7. **Rotation**: moments of inertia, torque, angular impulses (the
   r x n terms); `Boxes.ml`.
8. **Stacking**: sequential impulses (several iterations over all the
   contacts per step) with warm starting (each contact's impulse kept
   from the previous step), contacts matched from step to step,
   sleeping bodies; `Boxes.ml`'s pyramid standing still -- Box2D Lite's
   level. Each piece switchable, to see the pile jitter and sink
   without it. Then **Slingshot**: the mouse launch, the predicted arc,
   breakable targets.
9. **Docs**: `notes_2d_physics.md` pointers checked against the code,
   numbers filled in; `notes_physics_related_work.md`'s postscript
   (measured against Chipmunk/Box2D numbers, if meaningful).
10. *(later)* **3D**: `physics/3d/` (spheres, boxes, gravity) and a
   `Physics3d` API, for StarCollector3d and Minecraft3d (walking on
   blocks).

## Verification

- `dune build`, `make test` at every phase: the `.mli`s' worked
  examples, and the laws (closed forms, conservation, convergence
  orders).
- Golden frames for each new example (fixed step, so the frame after
  n ticks is deterministic), and for the ported games where possible
  (Asteroid seeds its randomness: a `-seed` flag, see
  `plan_2d_remaining.md`).
- The ported games play the same (by hand), and are shorter (line
  counts before and after, in the notes).

## Out of scope

- Soft bodies beyond springs and ropes, fluids (SPH), cloth: mentioned
  in the notes, not built.
- Joints other than springs and distance constraints (a Slingshot
  variant with pendulums or ropes would want them).
- Continuous collision detection beyond explaining tunneling (and
  maybe sub-stepping fast bullets).
- 3D until phase 9.
