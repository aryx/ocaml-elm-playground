# 2D physics, from scratch: a tutorial for `physics/`

How a computer makes things move: the few ideas every physics engine,
from Spacewar! on a 1962 PDP-1 to Box2D in Angry Birds, is built from,
where they came from, and what goes wrong when they're done naively.
It's also the specification of `physics/2d/` (see
[`plan_physics_teaching.md`](plan_physics_teaching.md)): written before
the code, its pointers name the planned modules, to be checked against
the code as it lands. Companions: [`notes_2d.md`](notes_2d.md) and
[`notes_3d.md`](notes_3d.md) (the graphics side), and
[`notes_physics_related_work.md`](notes_physics_related_work.md)
(Chipmunk, Box2D and the rest).

Two halves, the second built on the first:

- **Computational physics** (sections 1-7): Newton's laws turned into
  small time steps, and the numerical analysis of doing that -- why
  the obvious method gains energy and spirals planets out of their
  orbits, and the one-line fix. The part that's the same in a video
  game and in a simulation of the solar system.
- **Game physics** (sections 8-12): collisions -- finding them, and
  making things bounce, slide and tumble -- the part Box2D and
  Chipmunk are mostly about.

## 0. Where the code is, and a reading order

| module (`physics/2d/`, planned) | what | section |
|---|---|---|
| `graphics/2d/geometry/Vec2` | vectors | §2 |
| `Body` | a body's state: position, velocity, mass, angle, spin | §3 |
| `Integrate` | one time step: explicit Euler, semi-implicit Euler, Verlet, RK4 | §4, §5 |
| `Energy` | kinetic and potential energy, momentum: the checks | §5 |
| `Force` | gravity, gravitation, springs, drag | §6 |
| `World` | the whole step, with a fixed time step | §7 |
| `Shape`, `Collide`, `Contact` | collision detection | §8 |
| `Broadphase` | which pairs to test | §9 |
| `Resolve` | collision response: impulses | §10, §11 |
| `playground/Physics` | the Evan-style API over all of it | §13 |

## 1. The big picture: a simulation is a loop

A physics simulation is a **state** -- where everything is, how fast it
goes -- and a **step** that turns the state at time t into the state at
t + dt, a small time later. Run the step 60 times a second, draw the
state each time, and things move. Each step is the same three stages:

```
      state at t
          |
          |  forces: what pushes and pulls (gravity, thrust, springs)
          |  integrate: forces -> new velocities -> new positions
          |  collisions: find the overlaps, push things apart, bounce
          v
      state at t + dt
```

Newton's laws are *differential* equations: they say how position and
velocity *change*, continuously. A computer can't do "continuously"; it
replaces the calculus by many small steps, and **computational
physics** is the art of doing that without the small errors of each
step adding up to nonsense. The idea is old: Richard Feynman's
*Lectures on Physics* (vol. 1, chapter 9, 1963) computes a planet's
orbit by hand, in steps of a tenth of a unit of time, to show that
F = ma is all you need -- that page is this whole note in miniature.

## 2. Vectors: position, velocity, acceleration

A position is a 2D vector `(x, y)`; a velocity is how the position
changes per second, a vector too; an acceleration is how the velocity
changes per second. In one step of `dt` seconds:

```
   new position ~= position + velocity * dt
   new velocity ~= velocity + acceleration * dt
```

("~=": only approximately, which is the whole story of sections 4-5.)
Units matter as soon as numbers are compared: the playground's are
pixels and seconds, so a velocity of 100 crosses a 1000-pixel screen
in 10 seconds, and gravity is in pixels per second squared (a
convincing "falling" is around 500-1000 px/s^2 for a 1000-pixel
screen; Earth's 9.81 m/s^2 is only right if 1 pixel is 1 meter).

## 3. Newton's laws in code

- **Inertia** (first law): with no force, velocity doesn't change. A
  ship in space keeps drifting when you stop thrusting -- the whole
  feel of Asteroids and Spacewar!, and what a beginner's game usually
  gets wrong by moving things directly instead of through a velocity.
- **F = ma** (second law): a force changes the velocity, less so for
  a heavier body: acceleration = force / mass. Forces add up: each
  step, sum all the forces on a body (gravity + thrust + drag + ...),
  then divide by its mass once -- the "force accumulator" of every
  engine.
- **Action and reaction** (third law): when two bodies push each other,
  the forces are equal and opposite -- which is why momentum,
  mass * velocity summed over everything, never changes, whatever the
  bodies do to each other. The first law to check in the tests (§10).

For rotation, the same three, with angle for position, angular velocity
("spin") for velocity, torque for force, and moment of inertia for mass
(§11).

## 4. The time step: integration

Turning "velocity is the rate of change of position" into steps is
**numerical integration**. Four methods, each a few lines
(`Integrate`), each worth seeing.

**Explicit Euler** (Leonhard Euler, 1768), the obvious one: move with
the *old* velocity, then update the velocity.

```
   position += velocity * dt
   velocity += acceleration * dt
```

**Semi-implicit Euler** (also "symplectic Euler"): the same two lines,
the other way around: update the velocity first, then move with the
*new* one. It looks like a detail; it isn't (§5).

**Verlet** (Loup Verlet, 1967, for simulating molecules; Carl Størmer
used it for charged particles in the aurora, 1907): position from the
two previous positions, `x(t+dt) = 2 x(t) - x(t-dt) + a dt^2`, or its
"velocity Verlet" form with an explicit velocity. Exact when the
acceleration is constant.

**RK4** (Runge 1895, Kutta 1901): sample the acceleration at four
points inside the step and average them, like a careful surveyor.
Very accurate for smooth forces, four times the work.

**Worked example.** A ball thrown straight up at 10 m/s, g = 10 m/s^2,
steps of dt = 0.1 s. After 0.5 s the exact height is
10 * 0.5 - 5 * 0.5^2 = 3.75 m. After 5 steps:

```
   step   explicit Euler      semi-implicit Euler
          v      y            v      y
     0    10     0            10     0
     1    9      1.0          9      0.9
     2    8      1.9          8      1.7
     3    7      2.7          7      2.4
     4    6      3.4          6      3.0
     5    5      4.0          5      3.5      exact: 3.75
```

Explicit Euler is 0.25 m too high (it uses each step's *starting*
velocity, the fastest), semi-implicit 0.25 m too low (the slowest);
Verlet and RK4 give 3.75 exactly (a constant acceleration is their
easy case). Halve dt and both Euler errors halve: they're **first
order** methods, error proportional to dt; Verlet is second order
(error ~ dt^2), RK4 fourth (~ dt^4). A test checks those orders by
measuring the error at dt and dt/2.

## 5. Stability and energy: why games use semi-implicit Euler

Accuracy on one throw isn't what matters most; what the errors do over
*thousands* of steps is. The test case is the **harmonic oscillator**,
a mass on a spring: acceleration = -x (in units where the spring's
frequency is 1). Its energy, x^2 + v^2 (twice the sum of kinetic and
spring energy), should stay constant forever.

- **Explicit Euler** multiplies it by exactly (1 + dt^2) every step
  (expand (x + v dt)^2 + (v - x dt)^2). With dt = 0.1: 1% more energy
  per step, **2.70 times more after 100 steps** (about 1.6
  oscillations). The spring swings wider and wider: the simulation
  explodes. On an orbit, the same energy gain makes the planet
  **spiral out** into space.
- **Semi-implicit Euler** doesn't conserve it exactly either, but its
  error *oscillates* instead of accumulating: with dt = 0.1 the energy
  stays **between 0.95 and 1.05, forever** (checked over 1000 steps).
  The spring keeps its amplitude; the planet stays on a (slightly
  wobbly) closed orbit.

The reason has a name: semi-implicit Euler and Verlet are
**symplectic** -- they exactly conserve a slightly different energy, so
the real one can't drift (Hairer, Lubich and Wanner, *Geometric
Numerical Integration*, 2002). RK4 is far more accurate per step, but
not symplectic: over a very long run, its tiny energy error does drift.

That's why Box2D and Chipmunk use semi-implicit Euler: as cheap as
Euler, and stable. And why `examples/Orbit.ml` has an integrator key:
switch to explicit Euler and watch the planet leave.

```
   explicit Euler           semi-implicit Euler / Verlet
      .--.                        .----.
    .'  .-'-.                   .'      '.
   /  .' .-. '.                /    *     \      * = the star
   | /  / * \  \               \          /
    ... spiraling out           '.______.'   a closed orbit
```

## 6. Forces

- **Uniform gravity** (`fall`): the same acceleration downward for
  everything, g, whatever the mass (Galileo's point: the mass cancels
  out of F = ma). Mario's jump: an upward velocity, then g pulls it
  back.
- **Gravitation** (`attracted_by`), Newton's law (*Principia*, 1687):
  a force G m1 m2 / r^2 along the line between two bodies. The
  acceleration of a small body towards a big one of mass M is G M /
  r^2: doubling the distance quarters the pull. A circular orbit at
  radius r needs the speed v = sqrt(G M / r). Worked example, in game
  units: with G M = 1,000,000 px^3/s^2 and r = 100 px, v = 100 px/s,
  and one orbit takes 2 pi r / v = 6.3 s -- Spacewar!'s star, tuned so
  that orbits take a few seconds. Between n bodies, every pair: n^2/2
  forces per step (Barnes and Hut's tree, 1986, makes it n log n, for
  galaxies).
- **Springs** (Hooke, 1678): a force proportional to how far the
  spring is stretched, -k (length - rest length), plus **damping**, a
  force against the velocity, so it settles instead of bouncing
  forever. Too stiff a spring for the time step and even
  semi-implicit Euler explodes: the step must be small against the
  spring's period (dt < 2 / sqrt(k / m) for this method).
- **Drag** (`slow`): a force against the velocity, linear (-c v, slow
  things in a thick fluid) or quadratic (-c |v| v, air at speed). Gives
  a maximum speed where it balances the thrust: Asteroid's "max
  velocity", for free.
- **Thrust** (`thrust`): a force along the body's angle; the ship
  accelerates the way it points, not the way it moves -- which is what
  makes Asteroids and Spacewar! hard, and fun.

## 7. The fixed time step

Which dt? The tempting answer is "the real time since the last frame".
Glenn Fiedler's "Fix Your Timestep!" (2004) explains why not: a
variable dt makes the simulation depend on the frame rate (a jump
reaches a different height on a slow machine), one slow frame gives one
huge step that can go unstable (§5) or pass through walls (§12), and
the same inputs no longer give the same result (no replays, no
golden-frame tests, no network games in lockstep). So: a **fixed dt**,
here 1/60 s per tick, like Evan's own games assume.

The price is that the game slows down when the frames do. Fiedler's
fix: an *accumulator* of real time, consumed in fixed steps (maybe 0,
maybe 2 per frame), with the drawing interpolated between the last two
states. Not needed while the playground runs at 60 frames per second;
explained here because every serious engine does it.

## 8. Collision detection

Two questions per pair of bodies: do they overlap, and if so, the
**contact**: along which direction to push them apart (the *normal*),
and by how much (the *depth*). Christer Ericson's *Real-Time Collision
Detection* (2005) is the book; here, the cases the playground needs
(`Collide`), from cheapest to most general.

- **Circle against circle**: overlapping when the distance between the
  centers is less than the sum of the radii. Worked example: centers
  (0, 0) and (30, 40), distance 50; radii 20 and 20, sum 40: apart;
  radii 30 and 25, sum 55: overlapping by 5, normal (0.6, 0.8), the
  direction from the first to the second. Compare squared distances to
  avoid the square root.
- **Box against box** (axis-aligned, AABB): overlapping when their
  x ranges overlap *and* their y ranges do. Also the cheap test every
  broad phase uses (§9).
- **Point in polygon** (a bullet in an asteroid): cast a ray from the
  point, count how many edges it crosses: odd, inside. Works for any
  polygon, convex or not (it's the Jordan curve theorem; the same
  even-odd rule as `graphics/2d/Fill`).

```
        _____
       /     \___          one crossing: inside
      /   p ------|---->
      \          /
       \________/
```

- **Segment against segment**: two segments cross when each one's
  endpoints are on opposite sides of the other's line -- the same side
  test as a triangle's edge functions (`notes_3d.md` §7). Two polygons
  touch when an edge of one crosses an edge of the other, or one is
  inside the other.
- **The separating axis theorem** (SAT), for convex polygons: two
  convex shapes don't overlap exactly when some line separates them,
  and it's enough to try the directions perpendicular to their edges.
  Project both polygons on each such axis: if on one axis their
  shadows don't overlap, they're apart; if they overlap on every axis,
  they touch, and the axis with the smallest overlap gives the contact
  normal and depth.

```
      +----+                   on the y axis, their shadows
      | A  |   +----+          [0, 2] and [3, 4] don't
      +----+   | B  |          overlap: a separating axis,
               +----+          so no collision
```

- **GJK** (Gilbert, Johnson, Keerthi, 1988): the general algorithm for
  any two convex shapes (circles, polygons, their rotations), working
  on their Minkowski difference; what Box2D and Bullet use. Mentioned,
  maybe later.

A concave shape (an Asteroids rock often is) is either tested with the
exact but slower general tests above (points and segments), or split
into convex pieces for SAT.

## 9. The broad phase: not testing every pair

n bodies make n (n - 1) / 2 pairs: 4,950 pairs for 100 balls, most of
them far apart. The **broad phase** finds the pairs *worth* testing,
cheaply, before the exact tests (the **narrow phase**, §8):

- **All pairs**: the simple version, fine for a dozen bodies.
- **A uniform grid**: put each body in the cells its bounding box
  covers; only bodies sharing a cell are paired. Great when bodies are
  about the same size.
- **Sort and sweep** (sweep and prune): sort the bodies' bounding boxes
  along x, sweep, and pair only those whose x ranges overlap; from one
  step to the next the order barely changes, so re-sorting is nearly
  free (Baraff, 1992; I-COLLIDE, 1995).
- Trees (quadtrees, bounding volume hierarchies): for big worlds with
  bodies of very different sizes.

A counter of pairs tested, in the debug overlay, makes the difference
visible.

## 10. Collision response: impulses

Two balls touch; now what? A force would take many steps to push them
apart; a collision takes no time at all. So engines change the
velocities *instantly*, with an **impulse**, j, along the contact
normal n: the first body gets -j/m1, the second +j/m2 -- equal and
opposite (third law), so **momentum is conserved exactly**, by
construction.

How big? Just enough to change the velocity at which they approach
each other along n, v_rel . n (negative when approaching), into -e
times that, where e is the **coefficient of restitution** (Newton's
experimental law of impact): 1 bounces back as fast (a superball), 0
not at all (clay). Solving gives

```
   j = -(1 + e) (v_rel . n) / (1/m1 + 1/m2)
```

**Worked example.** Two balls of mass 1 on a line, the first moving at
2 m/s into the second, at rest; n = (1, 0), v_rel = 0 - 2 = -2.

```
   e = 1:    j = 2    ->  velocities 0 and 2      (Newton's cradle)
   e = 0:    j = 1    ->  1 and 1                 (they move together)
   e = 0.5:  j = 1.5  ->  0.5 and 1.5
```

Momentum is 2 before and after, every time; kinetic energy, 2 before,
is 2, 1 and 1.25 after: only e = 1 keeps it (tests check both).

Two more pieces make it look right:

- **Positional correction**: at the time the collision is found, the
  bodies already overlap (by the depth); the impulse stops them
  approaching but doesn't separate them, so resting bodies slowly sink
  into each other. Push them apart by (a fraction of) the depth too.
- **Friction** (Coulomb): a second impulse, along the contact's
  tangent, against the sliding, at most mu times the normal impulse --
  a box on a slope slides or stays.

## 11. Rotation

A body also has an **angle** and a **spin** (angular velocity), and
the rotational twins of mass and force: the **moment of inertia**, I
(how hard it is to spin: a disk of mass m and radius r has m r^2 / 2,
a w x h box m (w^2 + h^2) / 12), and the **torque**, a force times its
lever arm (r x F, the 2D cross product: the same force spins a door
more at the handle than near the hinges). An impulse at a contact point
off the center changes both the velocity and the spin, which is what
makes a box hitting the floor on a corner tumble; the impulse formula
gains the terms (r x n)^2 / I in its denominator (Chris Hecker's
"Physics, Part 3: Collision Response", Game Developer, 1997, derives it
step by step).

## 12. The hard parts

- **Resting contact and stacking.** A box on the floor collides with
  it every step; a pile of boxes, each with its neighbors, all at once.
  Resolving the contacts one at a time, each fix undoes a bit of
  another: the pile jitters and sinks. Engines iterate over all the
  contacts several times per step (**sequential impulses**), carrying
  the impulses over from one step to the next (**warm starting**)
  (Erin Catto, "Iterative Dynamics with Temporal Coherence", GDC 2005 --
  the heart of Box2D, and of Box2D Lite, its 1000-line teaching
  version). What an Angry Birds tower needs to stand still until it's
  hit: the plan's stacking phase, before the Slingshot game.
- **Tunneling.** A bullet at 600 px/s moves 10 pixels per step: a wall
  thinner than that can be jumped over between two steps, never seen
  overlapping. Fixes: smaller steps for fast things (sub-stepping),
  testing the swept path (the segment from the old position to the
  new) instead of the position -- **continuous collision detection** --
  or making walls thick. Asteroid's bullets against small rocks need
  the swept test.
- **Stiffness.** Very stiff springs, or very heavy things on very light
  ones, need tiny steps or implicit integrators (§5-6).

## 13. In the playground

The API (`playground/Physics.mli`) hides all of the above behind one
concept, a **body** -- a shape that moves -- and verbs in `update`:
`fall`, `push`, `thrust`, `slow` (§6) add up what pushes the body,
and `step` (§4: one tick of semi-implicit Euler) moves it; planned:
`attracted_by` (§6), `touching` (§8), `bounce` (§10), and a step for
many bodies at once (§9). `games/TinyWorms.ml`, an artillery game, is
its first user: a shell `launched` at an angle and a speed, then
`fall`, `push` (the wind) and `step` at every tick. Three more games
will show it off: **Asteroid**, ported (inertia, thrust, drag, wrap-around, exact
polygon hits), and **Spacewar!**, new (two ships and their torpedoes
around a star: gravitation, orbits, slingshots -- and, with the
integrator key, what explicit Euler does to an orbit), and
**Slingshot**, an Angry Birds-like, new (a projectile's parabola into
towers of boxes that tumble: rotation, friction, and stable stacking,
§11-12).

## Glossary

- **State**: all positions, velocities, angles and spins at one time.
- **Time step (dt)**: the time between two states; fixed here, 1/60 s.
- **Integration**: computing the next state from the forces; explicit
  Euler, **semi-implicit (symplectic) Euler**, **Verlet**, **RK4**.
- **Order** of a method: how its error shrinks with dt (1: halves with
  dt; 2: quarters; 4: 1/16).
- **Symplectic**: an integrator whose energy error oscillates instead
  of drifting; keeps orbits closed.
- **Momentum**: mass times velocity, summed: conserved by any
  interaction between bodies.
- **Kinetic energy**: m v^2 / 2; conserved by elastic collisions only.
- **Impulse**: an instant change of momentum, j; a force over no time.
- **Restitution (e)**: how bouncy a collision is, 0 to 1.
- **Contact**: the normal, depth and point(s) of a collision.
- **Narrow phase / broad phase**: the exact overlap test on a pair /
  choosing which pairs to test.
- **AABB**: axis-aligned bounding box.
- **SAT**: separating axis theorem; **GJK**: Gilbert-Johnson-Keerthi.
- **Moment of inertia (I)**, **torque**: rotation's mass and force.
- **Tunneling**: a fast body passing through a thin one between two
  steps; **CCD**: continuous collision detection, its fix.
