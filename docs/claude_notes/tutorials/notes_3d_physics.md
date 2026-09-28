# 3D physics, from scratch: a tutorial for `physics/3d/`

What actually changes when a physics engine gains a third dimension --
which is much more than an added `z`, and much less than a rewrite. It
was the specification of `physics/3d/` (see
[`plan_physics3d_teaching.md`](../plans/done/plan_physics3d_teaching.md)),
written before the code; it has since been checked against the code
(the plan's phase 13), and where the two disagreed it now says what
was built, and its numbers are measured ones. Companions:
[`notes_2d_physics.md`](notes_2d_physics.md), which this one assumes
(Newton's laws, integrators, impulses, the broad phase, stacking -- all
of that carries over unchanged and is not repeated here),
[`notes_3d.md`](notes_3d.md) (how the pictures are made), and
[`notes_physics3d_related_work.md`](../related-work/notes_physics3d_related_work.md)
(Bullet, PhysX, Havok, Jolt, and where ours stops).

The short version, and the plan of the note: **five things are genuinely
different in 3D**, and everything else is the 2D engine with vectors of
three numbers.

| | 2D | 3D | section |
|---|---|---|---|
| orientation | one angle | a quaternion, and a differential equation | §3 |
| inertia | one number, `m r^2 / 2` | a 3x3 tensor that turns with the body | §4 |
| separating axes | the edge normals | face normals *and* edge cross products (15 for two boxes) | §7 |
| a resting contact | 2 points | a clipped polygon, up to 4 points kept | §10 |
| the player | a box with gravity | a capsule that slides along planes -- not a rigid body at all | §14 |

## 0. Where the code is, and a reading order

| module (`physics/3d/`) | what | section | see it in |
|---|---|---|---|
| `graphics/3d/geometry/Vec3` | vectors (exists: the renderers use it) | §2 | |
| `Quat`, `Mat3` | orientation, and the inertia tensor | §3, §4 | `PhysicsSpin3d.ml` |
| `Body3d` | a body's state: position, velocity, orientation, spin, tensor | §3, §5 | |
| `Integrate3d` | one time step, and the orientation's own update | §5 | `PhysicsSpin3d.ml` |
| `Force3d` | gravity, gravitation, springs, drag, buoyancy | §6 | `TinyHalfLife2.ml` |
| `Energy3d` | energy, momentum, angular momentum: the referee | §4, §5 | `PhysicsSpin3d.ml` |
| `Hitbox3d` | sphere, box (an OBB), capsule, plane | §7 | `PhysicsHitbox3d.ml` |
| `Collide3d`, `Contact3d` | the narrow phase, and rays | §7 | `StarCollector3d.ml` |
| `Broadphase3d` | which pairs to test | §8 | `PhysicsStack3d.ml` |
| `Resolve3d` | impulses with the tensor, friction | §9 | `PhysicsBounce3d.ml` |
| `Solver3d` | manifolds, sequential impulses, warm starting, sleeping | §10 | `PhysicsStack3d.ml` |
| `Sweep3d` | continuous collision: the fast small ball | §12 | `TinyPinball3d.ml` |
| `Joint3d` | distance, hinge, ball-and-socket, motors, limits | §13 | `PhysicsRagdoll3d.ml`, `TinyHalfLife2.ml` |
| `Character3d` | the capsule controller: a player is not a body | §14 | `PhysicsWalk3d.ml`, `TinyMinecraft.ml` |
| `Ragdoll3d` | ten boxes and nine joints | §13 | `PhysicsRagdoll3d.ml`, `TinyHalfLife2.ml` |
| `Portal3d` | a portal pair's motion, crossing, seeing through | §15 | `TinyPortal.ml` |
| `Physics3d` | the Evan-style API over all of it | §18 | every game above |

Read §1-§6 for the mechanics (the part a simulation of the solar
system shares with a video game), §7-§12 for the collisions (the part
Bullet and Jolt are mostly about), §13-§15 for what the three flagship
games each needed.

## 1. The loop is the same

```
      state at t
          |
          |  forces: gravity, thrust, springs, buoyancy       (§6)
          |  integrate: forces -> velocities -> positions     (§5)
          |             torques -> spin -> orientation        (§3)
          |  collisions: find the overlaps, solve them        (§7-§10)
          v
      state at t + dt
```

Same three stages, same fixed `dt` of 1/60 s, same reasons
(`notes_2d_physics.md` §7). What the third dimension adds sits inside
the second and third boxes: the orientation is no longer a number to
add a spin to, and "find the overlaps" has more cases.

One change of habit: **the units are metres and seconds**, where the
2D engine uses pixels. `Playground3d` has no natural pixel, and once a
pinball is 27 mm across and travels at 10 m/s, writing the real
numbers down is what lets the simulation be checked against reality
(§12 is entirely that arithmetic).

## 2. Vectors, and two frames

`Vec3` is the renderers' own vector type (`graphics/3d/geometry/`),
reused here rather than redefined: `add`, `sub`, `scale`, `dot`,
`cross`, `length`, `normalize`, `face_normal`. The playground's world
is right-handed with **y up**, as the 3D examples and the camera
assume.

`cross` is the one that has no real 2D counterpart. In 2D, "the cross
product" is a scalar (`ax*by - ay*bx`) and torque and spin are
scalars too. In 3D it is a vector, perpendicular to both, and that is
exactly why spin becomes a vector: it points along the axis you turn
about, its length is how fast.

From here on, two frames matter and confusing them is the classic 3D
physics bug:

```
   body frame                     world frame
   (the box's own axes,           (the level's axes,
    where its tensor is           where gravity points
    a fixed diagonal)              down and contacts live)

        y                                Y
        |   +-----+                      |
        |  /     /|          R           |      the body, turned
        | +-----+ |     ----------->     |      by R = orientation
        | |     | +                      |
        +-|-----|/-- x                   +------------- X
```

`R` is the body's orientation as a matrix; a direction in the body
frame becomes `R v` in the world, and back with `R^T v` (a rotation
matrix's transpose is its inverse -- the one fact that makes §4 cheap).

## 3. Orientation: why not three angles

The obvious 3D generalization of "an angle" is three angles: pitch,
yaw, roll -- which is what `Playground3d.rotate3d dx dy dz` takes, and
what every 3D example in this repo draws with. As *state for a body
that spins*, it fails, in two ways:

- **Gimbal lock**: with the X-then-Y-then-Z convention, a pitch of 90
  degrees makes the X and Z rotations do the same thing -- a degree of
  freedom disappears, and the body cannot be turned out of it by
  changing any single angle.
- **Composition**: "turn the body a little about the axis it is
  spinning around" is not "add something to each angle". Angles do not
  add; rotations compose, and they do not commute (turn a book 90
  degrees about x then y, and about y then x: two different books).

Two representations survive. A **3x3 matrix** (9 numbers, drifts out
of orthogonality as floating point accumulates, re-orthogonalized with
Gram-Schmidt), or a **quaternion** (4 numbers, drifts off the unit
sphere, fixed by one `normalize`). Engines use the quaternion for the
state and the matrix where the maths wants one (§4). Ours does too
(`Quat`, `Mat3`).

A quaternion is a scalar and a vector, `q = (w, v)`, and a rotation of
`theta` about the unit axis `a` is

```
   q = (cos (theta/2), sin (theta/2) * a)

   e.g. a quarter turn about y:  theta = 90 deg
        w = cos 45 = 0.7071,  v = (0, 0.7071, 0)
```

The half-angle is not a decoration: rotating a vector is `q v q^-1`,
so the rotation is applied twice, and the halves make it come out
right. Two facts are all a physics engine uses:

- composing rotations is multiplying quaternions (`q_total = q2 * q1`,
  applying `q1` first), which is 16 multiplies against a matrix
  product's 27;
- **the spin's differential equation**: if `w` is the angular velocity
  (a vector, world frame), then

```
   q' = 1/2 * (0, w) * q          and one step of it:

   q_next = normalize (q + dt * 1/2 * (0, w) * q)
```

That is the whole orientation update: a quaternion multiply, an add,
and a renormalize. First order and slightly wrong for large spins (the
normalize hides most of it); the exact alternative is to turn `|w| dt`
about `w/|w|` and multiply, and `Integrate3d` has both. Measured (a
sphere spun for one second, how far the first-order step falls behind
the exact one): 0.0013 degrees at 1 rad/s, 0.17 at 5, 10.4 at 20, 110
at 60 -- fine at a frame's worth of turn, useless past about 20
degrees a step, which is why the exact one is the default here and the
cheap one is what engines ship.

**Drawing it.** `Playground3d.rotate3d` wants XYZ Euler degrees, so
`Physics3d.draw` converts the quaternion back to three angles. That
conversion is exact except at pitch = +/-90 degrees, where many
triples describe the same orientation and any of them draws the same
picture -- gimbal lock hurts *accumulation*, not a one-shot
conversion. (The alternative, a matrix rotation in the playground,
touches all four backends; see the plan's Groundwork.)

## 4. Inertia is a matrix, and bodies flip on their own

In 2D, the moment of inertia is one number: `m r^2 / 2` for a disk,
`m (w^2 + h^2) / 12` for a box. In 3D it is a **tensor**, a 3x3 matrix
`I` such that the angular momentum is `L = I w`. For a box of mass `m`
and sides `w, h, d`, about its centre and along its own axes:

```
          m  | h^2+d^2     0        0    |
   I =   ---  |   0     w^2+d^2     0    |        (the body frame)
          12  |   0        0     w^2+h^2 |

   a solid sphere:  I = 2/5 m r^2 * identity     (the same about
                                                  every axis)
```

Two consequences, both absent from 2D:

- **The tensor turns with the body.** The numbers above are in the
  body frame; the solver needs them in the world frame, so every step
  computes `I_world = R I R^T` (and its inverse
  `I_world^-1 = R I^-1 R^T`, which is what the impulse formula
  actually uses). This is the 3D engine's one unavoidable per-step
  matrix job.
- **`L = I w` means the spin and the momentum point different ways.**
  Only along a principal axis (a diagonal entry) are they parallel.
  A free body conserves `L` exactly, so if `w` is not parallel to it,
  `w` has to move -- the body wobbles, and, for the *intermediate*
  axis, it flips:

```
   a wing nut, sides so that  I_1 < I_2 < I_3

   spin about axis 1 (smallest I)  -> stable, wobbles a little
   spin about axis 3 (largest I)   -> stable
   spin about axis 2 (in between)  -> flips over, again and again,
                                      with no force acting on it
```

This is the **intermediate-axis (tennis-racket) theorem**, famously
filmed by the cosmonaut Vladimir Dzhanibekov with a wing nut in orbit
in 1985 (names and dates from memory, to check). It is the best demo
3D physics has: no gravity, no contact, nothing but `I_world = R I R^T`
done right, and a body that turns itself over. `PhysicsSpin3d.ml` is
it, with `|L|` and the energy printed on screen -- both constant
through every flip, which is also its test.

If your engine does *not* flip the wing nut, your tensor is not being
rotated. It is the sharpest single test of a 3D rigid-body core, and
it costs nothing to run.

## 5. The step, and what the integrators do here

Semi-implicit (symplectic) Euler for the linear part, as in 2D and for
the same reason (`notes_2d_physics.md` §4-§5): velocity first, then
position with the *new* velocity.

```
   v += (F / m) * dt
   p += v * dt

   w += I_world^-1 * (torque - w x (I_world w)) * dt     (§4)
   q  = normalize (q + dt/2 * (0, w) * q)                (§3)
```

The `w x (I_world w)` term is the **gyroscopic** one -- the part of
the equation that makes the wing nut flip. It is also stiff: explicit
stepping of it can gain energy and blow up for fast spins, which is
why Box2D has no such term at all (it is 2D: there is none) and Bullet
makes it an opt-in flag.

Writing it (phase 1) turned up a better third line, and `Integrate3d`
took it as the default. The conserved quantity is not `w` but
`L = I_world w`, and its own equation has no gyroscopic term in it at
all:

```
   L += torque * dt                     with no torque, L does not move
   q  = turn q by w for dt                             (§3)
   w  = I_world^-1 * L                  read back, with the *new* q
```

The wobble and the flip now come out of that last line, because
`I_world` has turned. Measured over 30 s on the T-handle of
`PhysicsSpin3d` (tensor `diag(0.00149, 0.00233, 0.00370)`, spun at 10
rad/s, `dt = 1/600`): `|L|` drifts by 1e-11 stepping `L` against 2e-2
stepping `w` with the gyroscopic term, and the flips are there either
way. Energy is the other way round -- neither conserves it, because
the orientation step is first order and the middle-axis motion
multiplies any error by `e^(4.6 t)`: 57% in 10 s at `dt = 1/60`, 15% at
1/600, 0.9% at 1/6000. An integrator that keeps both exists (a
symplectic Lie-group step, Moser-Veselov; or the closed form in Jacobi
elliptic functions) and is named, not built.

All three are switchable, because the difference *is* the lesson, and
the third one is the trap: drop the gyroscopic term and the handle
spins about a fixed axis for ever while `|L|` and the energy are
conserved to the last bit. Every diagnostic says the simulation is
perfect. It is just not this universe -- which is why
`PhysicsSpin3d.ml` puts that switch on a key.

Verlet and RK4 exist here too, for the same comparisons as in 2D, but
the orientation is where the methods differ most and it is worth
saying plainly: **a symplectic integrator plus renormalization is what
production engines use**, and the accuracy argument for RK4 loses to
the cost of four tensor rotations per step.

## 6. Forces: the 2D list plus one

`Force3d` is the shortest module in the engine -- gravity, Newton's
gravitation, Hooke springs and drag, all of them the 2D formulas with
a `Vec3` instead of a `Vec2`. Only **buoyancy** is new enough to
explain, and it is in because Half-Life 2's floating barrels are one
of its set pieces:

```
   Archimedes (c. 250 BC): the upward force equals the weight of the
   fluid displaced.

        water line   ~~~~~+--------+~~~~~
                          |        |   submerged height s of h
                          +--------+

   F_up = rho_water * g * (submerged volume)    up
   plus a drag proportional to the submerged area (else it bobs
   forever)
```

A barrel of density 0.6 floats with 60% of its height under water, and
the rest depth is exactly what the tests check against the analytic
waterline (0.6000, 0.2000, 0.9500 in `Unit_force3d`).
`PhysicsFloat3d.ml` makes the same check visible: each block wears a
stripe painted at its density, nothing lines those stripes up with the
surface, and the simulation puts them there.

Torque would come for free -- apply the force at the *centre of the
submerged part* rather than the body's centre and a barrel pushed
under rights itself -- but that needs a torque channel and a shape to
find the submerged centre of, so `Force3d.buoyancy` pushes at the
centre and the example holds its blocks upright instead of pretending
otherwise. It is still so: phase 11 brought joints, not that torque,
and `TinyHalfLife2`'s barrels float upright only because nothing tips
them. The submerged centre remains the exercise.

## 7. Collision detection: more cases, one new idea

The primitives (`Hitbox3d`): **sphere**, **box** (an OBB: an AABB is
only ever a bounding box here, for the broad phase), **capsule** and
**plane**. The specification also had a convex hull and a static
triangle mesh for levels; neither was built, and no game needed them
-- every level in the games is boxes. What exists of the mesh is its
pieces, `Collide3d.sphere_triangle` and `ray_triangle`. Ericson's
*Real-Time Collision Detection* (2005) is the reference for every test
below; chapter 5 is the one to have open.

The easy ones carry over from 2D unchanged in spirit:

- sphere/sphere: distance against the sum of radii (2D circles);
- AABB/AABB: overlap on three axes instead of two;
- sphere/box: clamp the centre into the box, measure to the result;
- **capsule/capsule**: the distance between two *segments*, against
  the sum of radii. The capsule is 3D's most useful shape and has no
  2D equivalent worth the name: it is cheap, it has no corners to
  catch on a staircase, and it is what nearly every game character in
  the world is.

The new idea is in the box/box case. In 2D, two convex polygons are
separated by one of their edge normals, and the separating axis
theorem tests a handful of axes. In 3D, faces are not enough:

```
   two boxes crossed like a +, neither's face normal separates them,
   yet they may not touch -- the gap lies across the direction
   perpendicular to one edge of each:

        box A edge  ---->  a
        box B edge  ---->  b          test also  a x b

   axes to test for two boxes:  3 (A's faces) + 3 (B's faces)
                              + 9 (each A edge x each B edge) = 15
```

Skipping the 9 edge-edge axes is the classic 3D collision bug: boxes
interpenetrate at the corners and look glued. (Gottschalk, Lin,
Manocha, "OBBTree", SIGGRAPH 1996, is where the 15-axis test is
usually cited from.)

`Collide3d.boxes` takes `~edge_axes:false` so that the bug can be
watched instead of imagined, and its test went looking for a pair it
gets wrong: of 20,000 random pairs of rods, 3 are separated by an
edge cross and by nothing else, and the first of them is in
`Unit_collide3d` with a certificate -- the separating axis worked out
straight from the sixteen corners, checked to be a cross and not a
face. Six axes call that pair a collision. Fifteen do not.

Beyond boxes, two routes: **SAT on convex hulls** (face normals of
both plus all edge-pair crosses: correct, and quadratic in the edge
count) or **GJK** (Gilbert, Johnson, Keerthi, 1988) with **EPA** for
the depth -- the general, elegant, harder one. We do SAT and the
primitives, and `Collide3d.mli` names GJK as the next step, honestly
(the plan's Out of scope).

**Rays** deserve their own line, because 3D needs them where 2D did
not: picking a body with the mouse, the gravity gun's aim, a bullet,
a raycast car's wheels, a character's ground check. Ray/sphere and
ray/box are quadratics and slab tests; **ray/triangle** is
Möller-Trumbore (1997), the one everyone uses, and it is 20 lines.

The honest limit: a *moving* body must be convex. Concave shapes are
either static triangle meshes (the level) or several convex pieces
glued together by the game. Real engines do convex decomposition;
we say so and stop.

Two more limits worth stating where they are easy to find.
`Collide3d.box_capsule` is the module's one approximation: it goes
back and forth between the box and the segment until the closest pair
settles, which is exact when that closest feature is a point and
reports the middle of the line when a capsule lies flat along a face.
And every test here answers with *one* contact point, which is what a
bounce needs; a box resting on a box touches along a whole face, and
the four points that keeps a stack still are phase 8's job (§10).

`PhysicsHitbox3d.ml` is where all of this is visible: four shapes
wearing their hitboxes as wireframes, one more that you push through
them, and the contact drawn as an arrow along the way out, as long as
the overlap is deep.

## 8. The broad phase: the same three, one dimension up

All pairs (`n(n-1)/2`), a **uniform grid** (cells the size of the
biggest body, now in three dimensions -- which is why the memory
argument against grids gets sharper: a 100x100x100 grid is a million
cells), and **sweep and prune** on one axis (Baraff 1992; I-COLLIDE,
Cohen et al., 1995). Modern engines default to a **dynamic AABB tree**
(Box2D's `b2DynamicTree`, Bullet's `btDbvt`): a bounding-volume
hierarchy that is cheap to update as bodies move, and which also
answers the ray queries of §7 -- which is why it wins in 3D and
roughly tied in 2D.

Measured, 500 marbles piled in a box (`Broadphase3d.mli` has the
100-marble row too):

```
                     box tests        ms      pairs found
   all pairs           124,750       0.75         174
   grid                  1,340       0.76         174
   sweep and prune       7,472       0.22         174
```

the same shape of answer as the 2D engine's (`notes_2d_physics.md`
§9): the cheap box test before the exact one is most of the win
whichever way the pairs are found, the grid makes the fewest
comparisons and spends the winnings on its hash table, and sweep and
prune -- a sort and a list -- is the fastest of the three.

Two things are genuinely different here. The grid's memory, as above:
ours hashes its cells, and `PhysicsMarbles3d.ml` draws the ones that
exist. And the sweep's *axis*, which in 2D one never has to
think about: on that same pile, wide in x and z and thin in y,

```
   sweeping x        6,644 box tests
   sweeping y       84,750            (the pile is 0.6 m tall)
   sweeping z        7,472
```

so `sweep_and_prune` picks the axis with the most spread of centres,
which is I-COLLIDE's heuristic and Bullet's -- and is a heuristic: it
picks z here where x was marginally better, the two being within noise
of each other.

## 9. Response: the impulse, with a tensor in it

The 2D impulse (`notes_2d_physics.md` §10) becomes, for a contact with
normal `n` at a point `r_a`, `r_b` from each centre:

```
                 -(1 + e) * (relative velocity at the contact . n)
   j = ---------------------------------------------------------------
        1/m_a + 1/m_b + n . ( (I_a^-1 (r_a x n)) x r_a )
                           + n . ( (I_b^-1 (r_b x n)) x r_b )
```

Which is exactly the 2D formula with `(r x n)^2 / I` replaced by its
tensor version -- Baraff and Witkin's course notes (SIGGRAPH,
1997-2001) derive it line by line. Apply `j n` to the velocities and
`r x (j n)` to the spins.

Both conservation laws then hold *by construction* rather than by
luck: the two bodies get the same impulse with opposite signs at the
same point in the world, so the linear momentum cancels, and the
angular momentum does too about any point at all (each body's change
is `p x J`, with `p` the contact point). Measured over a thousand
random collisions in `Unit_resolve3d`: both within 1e-12, energy never
created, and never lost either when `e = 1` and there is no friction.

The check that mattered most while writing it was the 2D engine's own
worked example, one dimension up: a ball into the *end* of a rod gives
the rod 0.4 of the speed and 1.2 rad/s of spin, and the ball keeps
0.6. The tensor version has to produce that same pair of numbers, and
it does.

**Friction** gains a dimension too, and this is a small, real
difference: in 2D there is one tangent direction, so friction is one
impulse clamped to `mu j`. In 3D the tangent plane is two-dimensional.
Engines pick two perpendicular tangents and clamp each to `mu j`,
which is a friction *pyramid* -- slightly too strong on the diagonals
-- instead of the true *cone*. Everyone ships the pyramid; we do too,
and `Resolve3d.mli` draws the picture of what it gets wrong (the
diagonals grip `sqrt 2` times too hard, and which way the diagonals
lie is an implementation detail of `Resolve3d.tangents`).

`PhysicsBounce3d.ml` is the phase's demo and checks itself: the bar
beside each ball is drawn at `e^2` of its fall from `e` alone, and the
simulation comes back and touches it, about 2% under -- the discrete
step's cost, printed rather than hidden. The crate dropped on
a corner is the part with no 2D version: the impulse lands far from
its centre, so most of what it gets is spin.

## 10. Contacts, and stacking

A sphere on a plane touches at one point. A box lying on the floor
touches over a whole face, and one point cannot hold it up -- it would
pivot. The manifold is built by **clipping the incident face against
the reference face's side planes** (Sutherland-Hodgman, 1974, the same
clipping `TinyDescent.ml` already does for its portals),
then keeping 4 of the resulting points -- the deepest, and then each
time the one farthest from those already kept, so that the four span
the face rather than huddle in a corner (`Collide3d.spread_out`):

```
   reference face (the floor)        clip the box's bottom face
   +------------------+              against the floor's 4 side
   |    +--------+    |              planes, keep up to 4 points
   |    |  box   |    |
   |    +--------+    |              2D kept 2 points; 3D keeps 4,
   +------------------+              and 4 is enough for any convex
                                     resting contact
```

Everything after that is the 2D engine's phase 8, unchanged in idea:
**sequential impulses** over all contacts, several iterations per step;
**warm starting** (each point's impulse kept from the last step, points
matched by position); a Baumgarte bias for the overlap; a restitution
threshold so a resting body stops making invisible micro-bounces
(Erin Catto, GDC 2005 and GDC 2007). And **sleeping**: a body still
for a second is skipped until something touches it -- optional in
2D, close to mandatory in 3D, where a pinball table full of settled
targets should cost nothing.

Sleeping has to be by **islands**, and that is worth knowing before
writing it: bodies that touch sleep as a group. Put a crate to sleep
on its own while the ones above it are still settling and something
wakes it a moment later with a jolt -- measured here, once every
sixty-one steps, which is the sleep threshold plus one. Box2D unions
touching bodies and sleeps the group; so does `Physics3d.simulate`,
and the tower's sideways creep fell from 2 cm to 3 mm when it started
doing so.

Measured, a crate dropped on the floor and left for 300 steps:

```
   solved together        y = 0.24500 (0.25 less the 5 mm slop),
                          speed 0, asleep, no contacts solved at all
   one pair at a time     y = 0.25120, speed 0.0487 -- shivering,
                          and it will shiver for ever
```

and a tower of five crates, after 600 steps: each resting within the
slop of the contacts under it (5 mm each, so the top crate 2.1 cm low
of 2.5 allowed), 3 mm of sideways creep, the whole tower asleep.
`PhysicsStack3d.ml` is the same argument with bricks: "s" turns the
solver off, and the wall comes apart into a heap.

Three things went wrong while writing it, all of them the kind that
look like physics bugs and are not. The single-point box/box contact
put its point at the *midpoint of the two support points*, which for a
crate on a wide floor is a corner of the floor, metres away, with a
lever arm so long that the impulse vanished and the crate fell
through. The face clipping kept the outside of each side plane,
because a box's six faces do not all wind the same way round (orient
the plane from the face's own middle and the question does not arise).
And warm starting remembered friction as two numbers in a tangent
basis that jumps by a quarter turn when the normal wobbles across a
tie -- so it replayed last step's friction along this step's axes, and
kicked the pile. Remember the friction impulse as a *vector* and the
basis stops mattering.

And one was found only at phase 10, the worst of them: `simulate`
moved each body by its velocity and never *turned* it by its spin --
only the single-body `step` did. The solver computed spins that went
nowhere, so in every world a domino could slide but not topple, and
nothing tumbled. Every test of the stacking passed, because a pile
that cannot turn stands very well. Two of those tests had thresholds
measured on the world that could not turn, and one of them measured
the wrong thing: "one iteration is not enough" was checked by how far
the tower sank, and a tower still bouncing at 0.6 m/s sinks less than
a settled one. It is checked now by what it is, the tower still
moving (0.63 m/s against 0.0001 at ten iterations).

What it costs, measured on one core with crates in towers of four:
about 0.17 ms per awake crate per step, so a hundred awake crates fill
a 60 Hz frame; asleep, the same pile is twenty times cheaper (400
crates: 72 ms a step while falling, 3.3 ms once asleep). The broad
phase makes no difference here -- all pairs or sweep and prune, the
same times: what costs is the contact points and the solver's ten
iterations over them, not finding the pairs.

## 11. Rolling, and the 5/7 that is already in this repo

A sphere rolling down a slope of angle `a` accelerates at
`5/7 g sin a`, not `g sin a`: two sevenths of the energy goes into the
spin, because `I = 2/5 m r^2`. Galileo timed balls down inclined
planes (*Two New Sciences*, 1638) precisely to slow falling down
enough to measure it, and missed the 5/7.

The nice part for us is that `TinyMarbleMadness.ml` *already*
computes that number by hand -- its header derives it, because a ball
on a height map was ten lines and an engine was not needed. So
`PhysicsRoll3d.ml` is a cross-check in both directions: the engine,
which knows only tensors and friction impulses, must reproduce the
figure a game wrote down from a textbook, and vice versa. When the two
agree, a whole chain (tensor, contact point, friction impulse, the
integrator) is right at once.

Rolling also shows where friction is doing the work: with `mu = 0` the
sphere *slides* and arrives at `g sin a` with no spin at all; between
the two, it slips first and starts rolling when the contact point's
velocity reaches zero (the classic result: a struck billiard ball
rolls at 5/7 of the speed it was struck with).

Measured, and the agreement is the point: 1.805 m/s^2 on a 15 degree
slope against the textbook's 1.812, 3.488 on 30 degrees against 3.500,
with `v = w r` at the contact to within 2% -- the engine having been
told only about a tensor, a contact point and a friction impulse. With
`mu = 0`, exactly `g sin a` and no spin.

The formula generalises to any shape as `a = g sin a / (1 + k)` with
`k = I / m r^2` read off the body's own tensor, which is what
`PhysicsRoll3d.ml` races: a frictionless ball (k = 0, and it wins), a
solid sphere (k = 0.40) and a capsule lying across the slope
(k = 0.47), each labelled with its prediction and its measurement.

One more distinction the example puts on a key, because the two are
easy to confuse: the friction *at the contact* (`rough`) is the grip
that makes a ball roll at all, and **rolling friction**
(`Physics3d.spin_slow`, a torque against the angular momentum) is the
loss that afterwards stops it.

## 12. The fast small ball: continuous collision

A body moving faster than its own size per step can pass through a
wall, seen overlapping on no frame at all (`notes_2d_physics.md` §12).
In 3D it is the same idea with worse arithmetic, and the pinball makes
it concrete:

```
   a pinball:   diameter 27 mm,  up to 10 m/s,  dt = 1/60 s

   distance travelled in one step:  10 / 60 = 167 mm
                                    = 6 ball diameters

   a flipper is ~10 mm thick  ->  it is jumped over completely
```

Three fixes, in increasing order of honesty and cost:

- **make everything thick**: the table's walls are solid blocks, not
  sheets. Free, and it is what many games do;
- **sub-stepping**: run 4 or 8 physics steps per frame for everything
  (deterministic if the count is fixed). Simple, and 8x the cost;
- **swept tests / speculative contacts**: test the *path*, the segment
  or the swept sphere from the old position to the new
  (`Sweep3d`), and answer the first hit -- or let the solver see the
  contact *before* it happens (a speculative contact) and brake the
  body over the step. The 2D engine already has the segment version
  (`Physics.went_through`, `TinySoldat.ml`'s bullets), and so
  does this one; both are a bullet's question, worked out from where
  the body is going now, and wrong for a body that has just bounced
  (§18).

`TinyPinball3d` takes the third, and gets a key to switch it off, so
that the ball can be watched going through the table -- the switch is
the teaching (principle 3). Measured on the pinball against a 1 cm
wall, head on (`physics/tests/Unit_sweep3d.ml`):

```
   plain steps          lost from 1.5 m/s
   4 substeps           lost from 6 m/s
   the sweep            never (tried up to 10 m/s)
```

`Sweep3d` is Mirtich's *conservative advancement*: advance the sphere
by the gap divided by the fastest the gap can close, again and again,
never past anything; it needs nothing of the obstacle but a distance,
and the obstacle may move and turn during the step (the closing speed
is then bounded by its turning speed times its reach). At the first
touch the ball is bounced off there and then, relative to the
surface's own speed at the point, and goes on for the rest of the step
-- not handed to the solver as a speculative contact, which would stop
it gently at the wall and lose the bounce's energy, which a pinball
cannot afford.

And the obstacle can be the fast one: a flipper turning at 1400
degrees a second moves its tip 4.5 cm a frame, more than the ball's
width and its own together, so a ball a centimetre above it is behind
it at the next frame without ever having overlapped it. The sweep is
the ball's, but it measures the gap against the flipper *turning*:
with it the flipper throws the ball at 2.98 m/s, without it at 0.12 --
it went through. A ball resting *on* the flipper is thrown either way,
being in contact at the start of the step: the solver's business.

## 13. Joints: the gravity gun is three calls

A **joint** (constraint) removes degrees of freedom between two
bodies, and is solved in the same loop as the contacts -- a contact is
just an inequality constraint, which is why they cost the same code:

```
   distance        keep |p_a - p_b| = L          a rope, a chain
   ball-and-socket keep p_a = p_b                a shoulder, a hip
   hinge           ball-and-socket + one axis    a door, a seesaw,
                                                 a pinball flipper
   motor / limit   drive or clamp a hinge angle  a flipper's kick,
                                                 an elbow that does
                                                 not bend backwards
```

A **ragdoll** is then ten bodies and nine joints (`Ragdoll3d`):
ball-and-sockets with cones at the neck, the shoulders and the hips,
hinges with limits at the elbows and the knees -- and the moment it
exists, a corpse falls down a staircase convincingly for
free, which is the observation that made Half-Life 2 (Valve, 2004, on
Havok) feel different from everything before it.

Each joint is a few *rows* for the solver, as a contact point is: a
direction, a speed to reach along it, a clamp on the running impulse
(three rows for a ball-and-socket, five for a hinge, one for a rod, one
more for a limit that is past or a motor). Unlike the contacts they
are not warm started: it was tried, on the three rows along the
world's axes, and bought nothing measurable; on a hinge's two rows
across its axis it was worse than useless, for the reason friction's
warm start was in §10 -- their directions jump when the axis wobbles.

The **gravity gun**, HL2's toy and the best advertisement a physics
engine ever had, is smaller than it looks:

```
   fire    ->  ray (§7) from the eye: the first body hit
   hold    ->  each step, the body pulled to a point in front of the
               eye: a direct "kinematic" carry, Physics3d.held_by --
               the velocity that gets there in a tenth of a second,
               15 m/s at most, and its spin mostly taken away (a
               spring would overshoot)
   launch  ->  release, then one impulse along the look direction
```

Three engine calls, and the whole game's vocabulary changes. The
seesaw beside it is one hinge; the barrels are §6's buoyancy; the
crate stack is §10. All four are `TinyHalfLife2.ml`, and its
zombies are the ragdoll's best argument: each walks as one upright
body, and the moment something knocks it faster than it walks it is
swapped for a ragdoll going on at that speed -- no animation of a fall
anywhere in the file.

Measured (`physics/tests/Unit_joint3d.ml`): a 1 m pendulum on a rod
swings with a period of 2.000 s (the textbook's 2 pi sqrt(L/g) is
2.007), the rod 0.2 mm off its length at most. A door set turning
about its middle turns about its hinge at a quarter of the speed,
because the hinge keeps its angular momentum *about the hinge* and a
door is four times harder to turn about its edge. And a ragdoll's
joints are not rigid: at a hard landing an elbow opens by about 4 cm
for a few frames, which more iterations do not cure -- the contact and
the joint are fighting, each at Baumgarte's slow speed, and the cure
is to correct positions directly (Catto's non-linear Gauss-Seidel).

## 14. A player is not a rigid body

This is the section that saves the most time, and every 3D game in
this repo already knows it: `TinyMinecraft`'s player is a box moved
one axis at a time, `TinyMario64`'s is a box whose feet, sides and
head are resolved separately, `TinyQuake`'s follows the original's
own code. None of them is a rigid body, and that is *correct*.

Drop a rigid-body capsule into a level with gravity and friction and
you get: a player that tips over on a ramp, that spins when clipped by
a corner, that slides down slopes, that cannot climb a 20 cm step, and
whose top speed depends on friction instead of on what the player
asked for. A game wants none of those.

So every engine ships a **character controller** as a separate thing:
a capsule with no torque at all, moved by a loop that Quake wrote down
first (`SV_FlyMove`, id Software, 1996; source released 1999):

```
   remaining = velocity * dt
   repeat up to 4 times:
     sweep the capsule along remaining
     if nothing hit: move, done
     else: move to just before the hit, then
           remaining = remaining - n * (remaining . n)   <- slide
                                                            along
                                                            the plane
```

`Character3d`'s "sweep" is a trace instead, since `Sweep3d` sweeps a
sphere and not a capsule: it steps along the move in pieces shorter
than half the radius, and bisects the piece where an overlap starts.
A walking character moves a few centimetres a step, so nothing is
stepped over.

Plus three parameters every engine has: a **step offset** (walk up
anything below ~0.4 m without jumping), a **slope limit** (walk up to
~45 degrees, slide above it), and a **ground check** (a short ray or
sweep down: are we standing, and on what). `Character3d` is that, and
`PhysicsWalk3d.ml` puts the parameters on keys so that a 0.5 m step
can be watched turning into a wall when the offset drops to 0.4.

A round foot is not Quake's box, and that cost three bugs, each now a
test in `Unit_character3d`. Coming down on the edge of a step, the
capsule touches its corner, whose normal is 60 degrees from level on a
perfectly walkable step -- so only a *face*'s slope is a slope, an edge
is not. Sliding along an edge's slanted normal, the round foot rolled
up over any edge lower than its radius, on top of the offset -- so,
walking, an edge is a wall, and climbing is the step's job alone (as in
PhysX's controller). And a step set down could leave the foot perched
on a corner higher than the offset -- so a step is judged by the
height of the *contact*, not the feet's.

The game feel on top -- coyote time, jump buffering, variable jump
height (`TinyMario64.ml`'s header) -- stays in the game. It is not
physics; it is deliberately *un*physical, and an engine that imposed
it would be wrong.

## 15. Portals: one transform, and an honest rendering cost

Portal (Valve, 2007) needs almost nothing from the engine, and that is
what makes it a good final lesson. A portal pair defines a rigid
transform `T` from one mouth to the other. When a body crosses:

```
   position     p  ->  T p
   velocity     v  ->  R_T v        (rotation part only: no scale)
   orientation  q  ->  q_T * q

   so: fall 20 m into a floor portal, come out of a wall portal
       moving 20 m/s sideways -- "speedy thing goes in, speedy
       thing comes out" is literally this line
```

The subtleties are in the crossing, not the transform: a body must be
allowed to be *half through* -- the portal's own wall not blocking it,
which `TinyPortal` does by taking the portal's panel out of the solids
while the pair is open -- and the teleport must happen when its centre
crosses the plane, not when it touches (`Portal3d.crossed`).

The *rendering* is the expensive half, and this note says so up front
rather than discovering it in phase 12: with no stencil buffer and no
render-to-texture in any of the four backends, a portal view is drawn
by transforming the destination room's polygons into the source room
and clipping them against the portal's four side planes plus its own
plane -- Sutherland-Hodgman again, and exactly the technique
`TinyDescent.ml` uses to draw through its mine's portals.
One level of recursion (a portal seen through a portal), and no more:
the cost is a second copy of the room's geometry per level.

`TinyPortal.ml` does exactly this, one level deep (a portal
seen through a portal shows its colour, not a view), with
`Portal3d.mli`'s motion, crossing and cut. The one
surprise was the eye rather than the body: walking forward into a
floor portal, looking ahead, is looking along that portal's up, which
comes out of a wall portal as straight up -- the view has to be turned
back upright, which Portal does over a moment and this game at once.

## 16. Compared with Bullet, PhysX and Jolt

The landscape is in
[`notes_physics3d_related_work.md`](../related-work/notes_physics3d_related_work.md).

**The same solver.** Bullet's default solver is sequential impulses
too (`btSequentialImpulseConstraintSolver`), and so is Jolt's: 10
velocity iterations, as here, then 2 position iterations that move
the bodies directly. That position pass is the one thing ours lacks
at the core, and it is the cure §13 names for the ragdoll's opening
elbows. PhysX offers a second solver, TGS, which sub-steps instead of
iterating.

**Where they are bigger.** Shapes, first: any convex hull through GJK
and EPA, triangle meshes and height fields for the level, compound
shapes, and a margin (Bullet) or convex radius (Jolt) around each
shape so that contacts are found just before the overlap. Ours is four
primitives and SAT (§7). Scale, second: islands solved in parallel on
every core (Jolt, PhysX), or on the GPU (PhysX), where ours runs on
one core, with a hundred awake bodies to a frame (§10). And the
features the ceiling of the related-work note leaves out: vehicles,
soft bodies, cloth, destruction.

## 17. What's missing, and exercises

In rough order of difficulty:

- collision filtering: a category and a mask per `Physics3d.body`,
  checked before a pair is tested;
- the friction cone instead of the pyramid (§9): clamp the length of
  the tangent impulse, a 2D vector, in `Resolve3d` and `Solver3d`, and
  watch the diagonals stop gripping `sqrt 2` too hard;
- buoyancy's torque (§6): push at the centre of the submerged part in
  `Force3d.buoyancy`, and let `PhysicsFloat3d.ml`'s blocks go free, to
  right themselves;
- sweep and prune keeping its sorted order between steps, with an
  insertion sort (`Broadphase3d.mli`'s "What is not here"; §8);
- a capsule sweep in `Sweep3d`, so that `Character3d` sweeps instead
  of tracing in small pieces (§14);
- speculative contacts in `Solver3d` (§12), measured against `Sweep3d`
  on `TinyPinball3d`'s wall: the ball kept, the bounce's energy lost;
- a position pass after the velocity iterations in `Solver3d`, for
  the joints and the contacts (non-linear Gauss-Seidel, §13), measured
  on the ragdoll's elbow;
- a dynamic AABB tree in `Broadphase3d`, answering `Physics3d.ray`
  too (§8);
- a static triangle mesh in `Hitbox3d`, from `Collide3d.sphere_triangle`
  and `ray_triangle`, with a tree over its triangles (§7);
- a convex hull in `Hitbox3d`, with SAT on hulls (§7);
- GJK and EPA in `Collide3d` (its `.mli` names them as the next
  step), one test for every pair of convex shapes;
- the Moser-Veselov step in `Integrate3d`, conserving both `|L|` and
  the energy through `PhysicsSpin3d.ml`'s flips (§5);
- a reduced-coordinates ragdoll (Featherstone's articulated-body
  algorithm) instead of `Ragdoll3d`'s ten bodies and nine joints: the
  elbow can then not open at all (§13).

## 18. In the playground

The API (`Physics3d.mli`) hides all of the above behind
the same one concept the 2D API has -- a **body**, a `shape3d` that
moves -- and the same verbs, so that someone who wrote
`TinySlingshot.ml` recognizes every one: `fall`, `push`,
`thrust`, `slow`, `attracted_by` accumulate what pushes it (§6),
`step` moves it one tick (§5), `touching` and `bounce` handle a pair
(§7, §9), and a `world` stepped by `simulate` solves a pile together
(§10). `heavy`, `bouncy`, `rough`, `immovable` and `upright` set what
a body is; `draw` gives the shape back where the body is, turned (§3);
`debug` draws its hitbox and its velocity.

What 3D adds to that vocabulary is small and specific: `ray` (§7:
picking, aiming, ground checks), `simulate ~continuous` and
`went_through` (§12), the joints -- `ball_joint`, `hinge`, `rod`,
`set_motor` -- and `held_by` (§13: the gravity gun); and three layers
beside the API rather than verbs in it, since none of them is a body:
`Character3d.walk` (§14), `Ragdoll3d` (§13) and `Portal3d.carry` (§15).
One question the API answers wrongly on purpose: `went_through` works
out where a body was from where it is going now, which is right for a
bullet and wrong for anything that has just bounced -- `TinyPinball3d`
counts its tunnellings from the ball's two positions instead.

The games arrived in the order the engine could support them
([`plan_physics3d_teaching.md`](../plans/done/plan_physics3d_teaching.md)):
`StarCollector3d.ml` ported behind a `physics=engine` flag first (the
2D plan's pattern: the hand-written physics stays, beside the
engine's, in the same file), then `TinyMarbleMadness.ml`'s rolling against
§11's, `TinyMinecraft.ml`'s and `TinyMario64.ml`'s players against
§14's, and then the three the plan was really for -- `TinyPinball3d`
(§12), `TinyHalfLife2` (§13, §6), `TinyPortal` (§15). Without the
flag, the ported games draw their golden frames byte for byte as
before.

## Glossary

Everything `notes_2d_physics.md`'s glossary defines still holds
(state, time step, integration, symplectic, momentum, impulse,
restitution, contact, broad/narrow phase, AABB, SAT, tunneling, CCD).
What this note added:

- **Quaternion**: four numbers, `(cos(theta/2), sin(theta/2) * axis)`,
  representing a rotation; multiplied to compose, normalized to stay a
  rotation.
- **Gimbal lock**: the degree of freedom Euler angles lose at a
  90-degree pitch.
- **Body frame / world frame**: the body's own axes, where its tensor
  is constant / the level's axes, where contacts and gravity live.
- **Inertia tensor**: the 3x3 matrix `I` with `L = I w`; `I_world =
  R I R^T`.
- **Principal axes**: the three directions in which `L` and `w` are
  parallel; **intermediate axis**: the middle one, about which free
  rotation is unstable (the tennis-racket theorem).
- **Gyroscopic term**: `w x (I w)`, the part of the rotational
  equation that makes the flip happen; stiff, and optional in most
  engines.
- **OBB**: an oriented bounding box (an AABB that turned);
  **capsule**: a segment with a radius -- the character's shape.
- **Edge-edge axis**: the cross product of one edge from each body;
  the 9 axes 3D adds to SAT, and the ones it is fatal to skip.
- **Manifold**: the set of contact points of one touching pair, up to
  4 here, obtained by clipping face against face.
- **Friction pyramid / cone**: two clamped tangent impulses / the
  correct circular limit they approximate.
- **Speculative contact**: a contact reported before the bodies touch,
  so the solver can brake them within the step; a cheap CCD, and not
  the one used here, since it loses a bounce's energy (§12).
- **Conservative advancement**: a swept test that advances a body by
  the gap divided by the fastest the gap can close, again and again,
  never past anything (§12, `Sweep3d`).
- **Kinematic body**: one that nothing can push, moved by the game
  through a velocity or a spin the engine carries out (a flipper).
- **Row**: one direction of a constraint for the solver -- a contact's
  normal, a joint's axis -- with a speed to reach and a clamp (§13).
- **Character controller**: a capsule moved by sweeping and sliding
  along planes, with a step offset and a slope limit -- not a rigid
  body (§14).
- **Portal transform**: the rigid transform between two portal mouths,
  applied to position, velocity and orientation (§15).

## References

- Archimedes, "On Floating Bodies", c. 250 BC.
- Galileo Galilei, "Discorsi e dimostrazioni matematiche intorno a due
  nuove scienze" (Two New Sciences), Leiden, 1638.
- J. Baumgarte, "Stabilization of constraints and integrals of motion
  in dynamical systems", Computer Methods in Applied Mechanics and
  Engineering 1(1):1-16, 1972.
- Ivan E. Sutherland, Gary W. Hodgman, "Reentrant polygon clipping",
  Communications of the ACM 17(1):32-42, 1974.
- Roy Featherstone, "Robot Dynamics Algorithms", Kluwer, 1987.
- E. G. Gilbert, D. W. Johnson, S. S. Keerthi, "A fast procedure for
  computing the distance between complex objects in three-dimensional
  space", IEEE Journal of Robotics and Automation 4(2):193-203, 1988.
- J. Moser, A. P. Veselov, "Discrete versions of some classical
  integrable systems and factorization of matrix polynomials",
  Communications in Mathematical Physics 139, 1991.
- David Baraff, "Dynamic Simulation of Non-Penetrating Rigid Bodies",
  PhD thesis, Cornell University, 1992.
- Jonathan D. Cohen, Ming C. Lin, Dinesh Manocha, Madhav K. Ponamgi,
  "I-COLLIDE: An Interactive and Exact Collision Detection System for
  Large-Scale Environments", Symposium on Interactive 3D Graphics,
  1995.
- S. Gottschalk, M. C. Lin, D. Manocha, "OBBTree: A Hierarchical
  Structure for Rapid Interference Detection", SIGGRAPH '96.
- Brian Mirtich, "Impulse-based Dynamic Simulation of Rigid Body
  Systems", PhD thesis, UC Berkeley, 1996 (conservative advancement).
- Tomas Möller, Ben Trumbore, "Fast, Minimum Storage Ray-Triangle
  Intersection", Journal of Graphics Tools 2(1):21-28, 1997.
- David Baraff, Andrew Witkin, "Physically Based Modeling", SIGGRAPH
  course notes, 1997-2001.
- id Software, Quake's source code (`SV_FlyMove`, in `sv_phys.c`),
  released under the GPL, 1999.
- Erin Catto, "Iterative Dynamics with Temporal Coherence", Game
  Developers Conference, 2005.
- Christer Ericson, "Real-Time Collision Detection", Morgan Kaufmann,
  2005.
- Erin Catto, "Fast and Simple Physics using Sequential Impulses",
  Game Developers Conference, 2006 (Box2D Lite).
