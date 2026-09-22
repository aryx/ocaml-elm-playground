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

Companions: [`notes_2d_physics.md`](../../tutorials/notes_2d_physics.md), the tutorial
(written ahead of the code, as its specification), and
[`notes_physics_related_work.md`](../../related-work/notes_physics_related_work.md)
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
- **Spacewar!, new** (DONE: `games/TinySpacewar.ml`, the second flagship): the
  1962 PDP-1 game (Steve Russell et al., MIT), the first video game with
  physics, and Asteroids' own ancestor (see `games/Asteroid.ml`'s
  header): two ships, one keyboard (arrows and w/a/s/d), duelling
  around a star whose gravity pulls ships and torpedoes; inertia,
  thrust, orbits, slingshots. Everything the engine teaches, in one
  game: gravitation, integration (a bad integrator makes the orbits
  wrong -- try it with the integrator key), collisions.
- **Slingshot, new** (DONE, as `games/TinySlingshot.ml`, the toys'
  naming; the third flagship, an
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
- **The capstone, TinySoldat** (after Soldat, Michał Marcinkowski,
  2002; see `plan_games.md` section 17): soldiers with jets (thrust
  against gravity, fuel), a polygon map to run on and collide with
  (phases 4-5), fast bullets (tunneling and swept tests), bouncing
  grenades (restitution), ragdolls on death (Verlet particles and
  distance constraints, from `Springs.ml`'s rope), then bots and the
  network -- everything this plan builds, in one game.

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
- **Throughout**: David M. Bourg, *Physics for Game Developers*
  (O'Reilly, 2002; 2nd ed. with Bryan Bywalec, 2013), the physics for
  programmers (projectiles with drag and wind, rigid bodies, collisions).
- **Force**: Newton's *Principia* (1687): F = ma, gravitation;
  Hooke's law (1678); damping. The N-body direct sum (O(n^2)), and a
  pointer to Barnes-Hut (1986) as the next step.
- **Energy**: the conserved quantities as the integrators' referee.
- **Shape**: areas and moments of inertia (disk, box, polygon via
  triangles).
- **Collide**: Christer Ericson, *Real-Time Collision Detection*
  (2005), the reference for all of it; Metanet Software's N tutorials
  (2004-5), SAT and grids for game programmers, the gentlest; point in polygon by crossing
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
- `examples/Elastic.ml` (DONE; planned as Springs.ml): a mass on a spring, a chain, a rope (Verlet
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

## Status

**DONE** (2026-09-19): phases 0-8, the docs pass (phase 9, the notes'
pointers checked against the code), and every game; what's left is in
[`../plan_physics_remaining.md`](../plan_physics_remaining.md).

- **Phase 0, DONE**: `graphics/2d/geometry/dune`'s package is
  `elm_playground` (the library itself unchanged, not moved);
  `physics/2d/` (`physics_2d`) and `physics/tests/` exist. The empty
  `physics/collision/`, `gravity/`, `mechanics/` placeholders are left
  as they were.
- **Phase 1, DONE except the Orbit example**: `Body`, `Integrate` (the
  four methods, each its own function, and `step` to switch), `Energy`,
  and, needed by the tests, the start of `Force` (`none`, `uniform`,
  `gravitation`, `spring`, `drag`, `sum`). Tests (`Unit_integrate`,
  `Unit_energy`): the projectile table (4.0, 3.5, 3.75, 3.75); the
  convergence orders measured (error ratios for dt halved: 2.1 and 2.0
  for the Eulers, 4.0 for Verlet, 15.5 for RK4); the spring's energy
  (explicit Euler exactly 1.01^100 = 2.70 after 100 steps,
  semi-implicit within 0.95-1.05 over 1000); the orbit (gm 1,000,000,
  r 100, dt 1/60: explicit Euler's radius 119.94 after one orbit,
  190.18 after ten; the other three at 100 within 0.01); drag's
  terminal speed. `examples/Orbit.ml`: DONE, a planet on an ellipse
  (G M = 10,000,000, from 250 px at 160 px/s: down to 118 px, 4.95 s
  per orbit, 2 steps per frame), space switching the integrator and
  restarting, the energy ratio E/E0 on screen: explicit Euler at 0.544
  and 501 px away after 8 seconds, semi-implicit Euler at 1.004 on a
  closed ellipse (golden frames of both).
- **Phase 2, DONE** (after phase 8, on the way to TinySoldat): `Force.drag`
  (linear); `physics/2d/Springs` (springs between bodies, Hooke plus
  damping along the spring, pinned ends as infinite masses; `step`,
  `chain`) and `physics/2d/Particles` (Jakobsen's Hitman technique:
  position Verlet, sticks relaxed Gauss-Seidel style, pinned
  particles; `rope`). Tests (`Unit_springs`): the spring's worked
  example, momentum kept by internal springs (the N-body test),
  damping's e^(-c t / 2), the stability limit (k / m = 10,000 bounded,
  its swings 1.81 times the first as the theory says; 20,000
  exploding), a rope's pinned end and its stretch (under 5% with 20
  iterations, less with 100), a pendulum's period 2 pi sqrt (L / g).
  `Physics.pulled_to x y k` (a spring to a point: bungee, grappling
  hook). The example is `examples/Elastic.ml`, not `Springs.ml` (an
  executable named like the engine's module would shadow it): a mass
  on a spring (d: damping), a chain of springs (x: 10 times stiffer,
  exploding in 8 steps), a rope of sticks dragged by the mouse; golden
  frames of it at rest, kicked, and exploding. Left: quadratic drag,
  N-body gravitation between bodies (Force's is towards a fixed
  center), cloth.
- **TinySoldat, DONE** (`games/TinySoldat.ml`, `plan_games.md`
  section 17), the capstone: a one-screen map of convex polygons
  (hills, walls, platforms, a bunker) as immovable bodies, with 3
  soldiers (you and two bots) as `upright` boxes in a `Physics.world`,
  run by setting their speed and flown on jets (`push` against
  gravity, fuel refilled on the ground). The engine got the swept
  tests: `Collide.segment_polygon`, `segment_circle` (and
  `nearest_on_outline` exposed), `Physics.went_through` (a body's path
  over its last tick), tested on the tunneling example (a bullet at
  1500 px/s crossing a 10-pixel wall between two ticks, touching it at
  neither); `Particles.keep_out` (particles pushed out of polygons,
  with friction). The bullets fly 25 px a tick, tested by
  `went_through`; the grenades are bouncy bodies in the world, their
  blast pushing soldiers and ragdolls; the dead become ragdolls of 9
  particles and 9 sticks, lying on the map, and respawn after 2 s at
  the spawn point farthest from the living. The bots: line of sight by
  the same swept test against the map, an aim wobbling by a sine (no
  Random), run, strafe, jump, jets, grenades at the hidden. Golden
  frames: the title, the bots' fight (a ragdoll), the player's jets
  and a grenade. The kit's parts (camera, weapons table, waypoints,
  editor, network) left as exercises in its header.
- **Phase 3, v1 DONE, differing from the sketch above** (by writing a
  game with it): `playground/Physics.mli`, a layer on top of the
  playground like `Camera2d` (in the `elm_playground` library, which
  now depends on `physics_2d`). Changes: the verb is **`step`**, not
  `move` (which moves shapes: a game opens both `Playground` and
  `Physics`); `fall`, `push`, `thrust`, `slow` **accumulate** (the
  record's `ax`, `ay`: the force accumulator of every engine) and
  `step` uses them up, so their order doesn't matter; the **mass is 1
  by default** (`heavy` to change it), not from the shape's area:
  `push` then behaves the same on every body until one is made heavy,
  F = m a as a lesson to opt into (the area can still give the mass
  with the collisions, phase 4-5); no `bounciness` field yet
  (`bounce_in screen bounciness` takes it as an argument). Also
  `launched`, `pointing`, `turn`, `wrap`, `bounce_in`, `distance`,
  `speed`, `outside`, `tick`. Tests: `Unit_physics_api` (the thrown
  ball of `Physics.mli`: x 200, y 300 - 406.67, 6.67 below the exact
  parabola; the accumulator; the top speed; directions; edges).
  Then `attracted_by` (Newton's gravitation towards another body,
  other.mass / r^2: G = 1 in the playground's units, so a star `heavy
  1000000.` keeps a body at 100 px on a circle at 100 px/s, tested over
  one orbit) and `shot_from` (a projectile from a moving shooter's nose,
  with the shooter's velocity).
- **The ports, DONE, as a choice rather than a rewrite**: each game
  keeps its own hand-written physics, "the dumb engine", by default, and
  the flag `physics=engine` (`?physics=engine` on the web) switches to
  the playground's, so the two can be compared in the same file:
  - `examples/Mario.ml`: `update_dumb` (Evan's code, velocities per
    1/100 s) or `update_physics` (a body: 100 px/s, a jump at 500 px/s,
    `fall 1250.`, the same numbers converted);
  - `games/TinyMario.ml` (the platformer): only the falling speed
    (0.8 px/frame^2, or `fall 2880.` and `step`), the moving through the
    tiles staying `move_by`'s, until collisions; the two engines give
    byte-identical frames (150 frames with two jumps, compared), since
    the dumb engine is semi-implicit Euler too;
  - `games/Asteroid.ml`: one game, two engines on the same objects: the
    dumb one (every 30 ms of wall-clock time, velocities added, a
    `failwith "Todo"` past v_max) or `Physics` bodies at every frame,
    with the numbers converted (bullets 1000 px/s, thrust 1111 px/s^2,
    turning 573 degrees/s) and two improvements: drag (`slow 1.67`)
    giving the top speed v_max by itself, and bullets `shot_from` the
    ship, keeping its velocity. The positions and velocities became
    floats (the file's own wish), so the dumb engine's golden frames
    moved by less than a pixel (approved). Rendered with the clock
    frozen, the dumb engine doesn't move at all (it waits for real
    time) and the physics engine does: a fixed step's determinism.
- **Spacewar!, DONE** as `games/TinySpacewar.ml` (the toys' naming):
  two ships on one keyboard (arrows, w/a/s/d) around a star of mass
  2,000,000, `turn |> thrust |> attracted_by star |> step |> wrap`,
  torpedoes `shot_from` the ships and falling around the star too (the
  original's flew straight), 4 each at most, 3 s each; hits by distance
  until phase 4; rounds and scores; no randomness. Golden frames of its
  title and a scripted duel.
- **The artillery game, DONE, before the plan's own games**:
  `games/TinyWorms.ml` (`plan_games.md`'s artillery toy), two players
  taking turns, a shell `launched` then `fall |> push wind |> step`, a
  height-map terrain (Scorched Earth's, no caves) carved by explosions,
  seeded hills and winds (`seed=n`). Checked with rendered frames
  (`-script "space:2-3,space:10-11"`: the arc, the crater); golden
  frames of its title and a scripted shot.
- **Phase 4, collision detection, DONE (the narrow phase)**: in
  `physics/2d/`, `Shape` (the hitboxes: point, circle, box, polygon,
  convex or not; `place`, `bounds`, `area` by the shoelace formula,
  `convex`), `Contact` (normal, depth, point) and `Collide`, one test
  per function: circles, bounding boxes, point in polygon (crossing
  number), segments (orientation tests, collinear overlaps), any two
  polygons (an edge crossing or one inside), SAT for convex ones with
  the contact, circle against polygon; `touching` (the bounding boxes
  first) and `contact` (None for concave polygons) for any two. Tests
  (`Unit_collide`): the `.mli`s' examples, a concave U, and properties
  on 2000 random convex pairs (SAT agrees with the general test, and
  so does the circle's contact version). In the API,
  `Physics.touching`, the hitboxes read from the body's own shape (a
  circle stays one, rectangles and images are boxes, an oval a
  16-gon, an ngon its corners, groups recursively, scaled, rotated
  and moved like the drawing), and `Physics.debug` drawing them
  translucent, with a velocity arrow. Users: Asteroid's physics engine
  hits the asteroids' real polygons (the dumb engine keeps its 10 px
  circles), TinySpacewar's hits and the star; both take a `hitboxes`
  flag drawing `debug`. The golden frames didn't change. Left: the
  broad phase (a grid, sort and sweep), for many bodies (phase 5+).
- **Phase 5, collision response, DONE (without rotation)**:
  `physics/2d/Resolve`: the impulse along the normal (restitution),
  Coulomb friction along the tangent (clamped at mu j), positional
  correction (the depth shared by inverse masses); an immovable body
  is an infinite mass (1/m = 0, no special case). Tests
  (`Unit_resolve`): the notes' worked example (e = 1, 0, 0.5: the
  velocities, momentum 2, energies 2, 1, 1.25), a wall, friction and a
  moving paddle, and 1000 random collisions (momentum conserved to
  1e-9, energy never created, kept with e = 1). In the API, the body
  record got `bounciness` (0 by default, like Box2D; the pair uses the
  bouncier) and `friction` (0; the pair's is the geometric mean), set
  by `bouncy`, `rough`, `immovable`; `bounce a b` (the deepest contact
  among their hitboxes, None for concave polygons: no bounce),
  `bounce_off wall b` for pipelines, `bounce_all` (all pairs; the
  broad phase is phase 6). `examples/Bounce.ml`: five balls from clay
  to superball, and a pile of balls of all sizes (mass = area)
  bouncing off each other. `games/TinyPong.ml`, new, beside the
  hand-written `games/Pong.ml` (untouched): paddles as immovable,
  rough, bouncy 1.05 bodies (bumpers: rallies speed up), so a moving
  paddle drags the ball (friction) and corners deflect it (the
  contact normal) -- two of Pong.ml's TODOs, from the physics; a
  computer opponent (`players=2` for two), the ball's speed capped
  below tunneling. Golden frames: Bounce, TinyPong, TinyPong_rally.
  Left: a restitution threshold (resting balls make invisible tiny
  bounces), friction on rotation (phase 7), iterations for stacks
  (phase 8).
- **Phase 6, the broad phase, DONE**: `physics/2d/Broadphase`, all
  pairs, a uniform grid (cells as big as the biggest box) and sort and
  sweep, each returning the same pairs (i < j, sorted) and its count of
  box tests. Tests (`Unit_broadphase`): the `.mli`'s four boxes (6, 1
  and 3 tests), the same pairs on 200 random scenes, 300 spread boxes
  (44,850 tests for all pairs, under 2,000 for the others).
  `Physics.bounce_all ?broad_phase` (sort and sweep by default) bounces
  only the candidate pairs, the bounding boxes computed once per
  pass (so a pair pushed together by an earlier bounce of the same
  pass waits for the next frame: Bounce's golden frame moved, as
  plausibly), and `Physics.broad_phase` gives the counts.
  `examples/Marbles.ml`: 300 marbles, space switching the method, the
  counts on screen, the grid drawn; golden frames with all pairs and
  with the grid, identical marbles. The measured times are in
  `notes_2d_physics.md` section 9: the box test before the exact one
  is the win (10.2 -> 1.6 ms for 300, 107.6 -> 8.7 for 1000). Left:
  the insertion sort of the previous order (sort and sweep's temporal
  coherence), trees.
- **Phase 7, rotation, DONE**: `Body` got `spin` (radians per second)
  and `inertia` (infinite by default: the earlier phases' bodies and
  tests unchanged), `Body.point_velocity`; `Energy.kinetic` counts the
  spin's energy, `Energy.angular_momentum`; `Shape.moments` (area and
  polar second moment around the center: polygons by triangles,
  circles with the parallel axis theorem); `Collide.sat`'s contact
  point became the middle of the overlap region (its corners: each
  polygon's corners inside the other, and the edges' crossings), where
  it was the second polygon's deepest corner -- a floor's far corner
  for a box on the floor, a 500-pixel lever arm; `Resolve` pushes at
  the contact point: the relative velocity of the touching points, the
  (r x n)^2 / I terms, the torques, for friction too. Tests: the
  `.mli`'s ball into the end of a stick (j = 0.4, the stick at 0.4 and
  1.2 rad/s, the energy kept), a sliding disk starting to roll at 2/3
  of its speed, 1000 random collisions with spins and off-center
  points (momentum and angular momentum conserved, energy never
  created, kept with e = 1), the moments (disk, box, a moved disk), the
  overlap's middle. In the API, bodies turn by default, their inertia
  from their shape's hitboxes and mass (not stored: computed at each
  bounce), `upright` to never turn (TinyPong's ball keeps its gameplay
  with it). `examples/Boxes.ml`: boxes landing on corners and tipping
  over, tumbling down a rough ramp, balls rolling down it; `u` makes
  everything upright (the boxes balanced on corners, all stuck on the
  ramp: static friction, tan 25 = 0.47 < 0.69). Golden frames: Boxes,
  Boxes_upright; Bounce and Marbles moved (the frictionless balls
  don't spin, but a center-line lever arm is 1e-16, not 0, and a pile
  amplifies the last bits). Then **TinyCameltry** (`plan_games.md`
  section 18), the phase's game, the user's pick after Rolling-Moon: a
  maze turned around a rolling moon. Left: one contact point for a
  resting edge (two, clipped, with phase 8), the body's center taken
  as its center of mass (a group off-center turns around the wrong
  point).
- **Phase 8, stacking, DONE**: `Collide.manifold`, up to two contact
  points: the corners of each convex polygon inside the other, each
  with its own depth along the normal, the two farthest apart kept
  (not Box2D's clipping: the same idea in fewer lines, cited);
  `physics/2d/Solver`: sequential impulses (the accumulated normal
  impulse clamped >= 0, friction's within mu of it), warm starting
  (each point's impulses of the step before, found again within 3
  pixels: position matching, like Bullet's persistent manifolds, no
  feature ids), Baumgarte's velocity bias for the overlaps (0.2, slop
  0.5 px) instead of pushing apart, a bounce threshold (50 px/s: no
  micro-bounces at rest); `Resolve` exposes `relative_velocity` and
  `resistance` for it. Tests (`Unit_solver`): the manifold (two points
  lying flat, one on a corner), the `.mli`'s box on the floor (0.5 at
  each corner, still, not turning), warm starting (one iteration per
  step enough warm, not cold), momentum and angular momentum kept. In
  the API, a `world` (bodies + the solver's memory, the bodies known
  by their place in the list) and `simulate ?gravity ?iterations
  ?warm_starting`, the whole step (pushes, broad phase, manifolds,
  solver, moves). `examples/Pyramid.ml`: 28 boxes standing still; keys
  `s` (the solver off: phase 7's `step` + `bounce_all`, the pyramid a
  heap after 5 s), `i` (1, 4, 10, 20 iterations: 1 cold, a heap), `w`
  (warm starting: 10 cold, standing but sagging, gaps), space (a ball
  knocking the top off), `r`; flag `solver=off`; keys printed at
  launch. Golden frames: Pyramid, Pyramid_no_solver, Pyramid_ball. The
  examples now credit the classic demos they follow, with URLs
  (checked): Box2D Lite's "Pyramid Stacking" and "Varying Friction
  Coefficients", Box2D's testbed "Pyramid", "Restitution", "Friction",
  Chipmunk's "PyramidStack", "Plink". Measured: a 28-box pyramid step
  is 1.3 ms of physics; the golden runs are slow (30-45 ms a frame)
  because of the offscreen rendering, for every example. Left:
  sleeping, joints (springs and ropes: phase 2's Springs), clipping.
- **Slingshot, DONE** as `games/TinySlingshot.ml`: the mouse pulls
  the ball back from the sling (or keys: the angle, the power, space),
  the launch speed proportional to the pull; the arc drawn before the
  shot is the engine's own steps (`fall |> step`, 60 of them), so the
  ball follows the dots exactly; a two-floor tower of pillars and
  planks in a `Physics.world`, standing still until hit; 3 green
  targets that break when their velocity jumps by more than 250 px/s
  in a step (J = m dv, gravity's own 13 px/s taken out: a hard hit or
  a hard fall, no new engine API); 3 balls, won or lost. The bodies
  kept in the order walls, blocks, targets, balls, the balls added at
  the end (the solver's memory knows bodies by their place). The key
  `s` / flag `solver=off` plays it with phase 7's `bounce_all`: the
  tower slumps by itself before any shot. Golden frames: the title, a
  shot (the tower tumbling), no_solver. Exercises in its header:
  levels from strings, materials, powers, a camera, stars.

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
