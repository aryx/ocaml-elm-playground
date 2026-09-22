(* Physics in 3D: things that move by themselves.

   The 3D twin of playground/Physics, and deliberately the same API: a
   *body* is a shape3d that moves -- where it is, how fast it goes,
   which way it points -- and everything else is verbs on bodies.
   Someone who wrote games/TinySlingshot.ml already knows this module.

     let ball = body (sphere red 0.2) |> at 0. 3. 0. |> moving 2. 0. 0.

     (* in update, at every tick *)
     let ball = ball |> fall 9.8 |> step

     (* in view *)
     [ draw ball ]

   Three things to know, the same three as in 2D:

   - [step] is one tick, 1/60 s. What is new is the unit of length:
     the 2D engine counts in pixels, and this one counts in **metres**,
     because Playground3d's world has no pixel (a [cube white 1.] is
     one unit). So gravity is 9.8, not 800, and a body [moving 2. 0. 0.]
     crosses two units a second. Writing the real numbers down is what
     lets a simulation be checked against the world.
   - [fall], [push], [slow] and friends do not move the body: they add
     up what pushes it, and [step] uses it all at once. Their order
     does not matter, and [step] comes last.
   - Everything is a value; [step] gives back a new body.

   What the third dimension adds to the vocabulary is small:

   - which way a body points is no longer an angle but an orientation
     ([pointing], [turning], and see physics/3d/Quat.mli for why three
     angles will not do as state). [draw] turns the quaternion back
     into the three angles Playground3d.rotate3d takes;
   - a body's resistance to being spun is a matrix that turns with it,
     so a body left to itself can wobble, and about its middle axis
     turn itself over (PhysicsSpin3d.ml). [body] gives every
     body the tensor of its own bounding box, so this happens by
     itself; [upright] takes it away, for a player or a flipper that
     must never topple;
   - [floating], because Archimedes needs a third dimension to be
     interesting and a barrel bobbing in water is the cheapest good
     demonstration of a force (physics/3d/Force3d.mli).

   Underneath is physics/3d/, written to be read (see
   docs/claude_notes/tutorials/notes_3d_physics.md): [step] is one
   Integrate3d.semi_implicit_euler for the position and one
   Integrate3d.spin_step for the orientation.

   Collisions are found ([touching], [contact], [ray]) and answered
   ([bounce], [bounce_off], [bounce_all]) -- one pair at a time. A
   *pile* of bodies that must stay still needs more than that: every
   contact solved again and again while its neighbours move, and four
   contact points per pair rather than one. That is a [world] to
   [simulate], phase 8 of
   docs/claude_notes/plans/plan_physics3d_teaching.md, and the
   character controller and the joints come with 9 and 11. A game that
   needs a stack today does its own contacts, as games3d/TinyMario64.ml
   does.
*)

open Playground3d

(* A shape3d that moves. A record, like [computer], so a game can read
 * [ship.x] or [ship.vy]; make one with [body], then change it with the
 * functions below rather than by hand. *)
type body = {
  shape : shape3d; (* what it looks like, drawn by [draw] *)
  x : number; (* where it is, in metres *)
  y : number;
  z : number;
  vx : number; (* its velocity, in metres per second *)
  vy : number;
  vz : number;
  orientation : Quat.t; (* which way it points *)
  spin : number * number * number; (* how that changes: degrees per second about each axis *)
  mass : number; (* in kg, 1 by default *)
  bounciness : number; (* how it bounces, 0 (clay) by default *)
  friction : number; (* how it grips what it slides on, 0 by default *)
  hitbox : Hitbox3d.t; (* what it *is* to a collision: its bounding box, by default *)
  inertia : Mat3.t; (* how hard it is to spin: its hitbox's, by default *)
  ax : number; (* what pushes it until the next [step]: *)
  ay : number; (*   accelerations, set by fall, push, ... *)
  az : number;
  torque : number * number * number; (* and what turns it, set by spin_by *)
}

(* {1 Making bodies} *)

(* [body shape]: a body looking like [shape], at the origin, not
 * moving, not turned, of mass 1, whose hitbox is the box of its own
 * bounding box, and whose inertia tensor is that box's -- so it can be
 * spun, and will wobble like the solid it looks like. [shape] should
 * be centred on the origin (as {!Playground3d.box},
 * {!Playground3d.sphere} and friends are): [draw] turns it about that
 * point. *)
val body : shape3d -> body

(* {1 What it is to a collision}

   A body's hitbox is what {!touching}, {!contact} and {!ray} work on,
   and it is not its drawing: a tree is a cylinder, a character is a
   capsule, a spaceship is a sphere. The default is the box of the
   shape's own bounds, which is right often enough to start with. Each
   of these also gives the body that hitbox's inertia tensor (a ball
   rolls differently from a crate), unless it is [upright], which stays
   upright. *)

(* [ball b]: a sphere, as wide as the narrowest side of its bounds --
 * for anything round *)
val ball : body -> body

(* [pill b]: a capsule standing up the y axis, as wide as the narrowest
 * of its two horizontal sides: what a character wants, since it has no
 * corners to catch on a staircase *)
val pill : body -> body

(* [hitbox h b]: any hitbox, spelled out (physics/3d/Hitbox3d.mli) *)
val hitbox : Hitbox3d.t -> body -> body

(* where the hitbox is and how it is turned, right now: what the engine
 * actually tests, and what [debug] draws *)
val hitbox_of : body -> Hitbox3d.placed

(* [at x y z b] *)
val at : number -> number -> number -> body -> body

(* [moving vx vy vz b]: metres per second *)
val moving : number -> number -> number -> body -> body

(* [pointing axis degrees b]: [b] turned [degrees] about [axis] *)
val pointing : number * number * number -> number -> body -> body

(* [turning axis degrees_per_second b]: and turning, from now on *)
val turning : number * number * number -> number -> body -> body

(* [heavy mass b]: in kg (1 by default). Its tensor scales with it. *)
val heavy : number -> body -> body

(* [bouncy e b]: 0 clay, 1 a superball. Kept for the collisions of a
 * later phase; nothing here reads it yet. *)
val bouncy : number -> body -> body

(* [rough mu b]: friction, same. *)
val rough : number -> body -> body

(* [immovable b]: an infinite mass, so nothing can push it (a floor).
 * Given a velocity ([moving]) or a spin ([turning]), it is *kinematic*:
 * [simulate] moves and turns it by them, nothing can change them, and a
 * body it meets is hit by its surface's speed at the contact -- a
 * pinball flipper is exactly that, driven by the game from one tick to
 * the next rather than by forces (games3d/TinyPinball3d.ml). *)
val immovable : body -> body

(* [upright b]: nothing can turn it. A player, a pinball flipper: a
 * rigid body that can topple is exactly what a character must not be
 * (notes_3d_physics.md section 14). *)
val upright : body -> body

(* [solid_as sides b]: the tensor of a box of [sides] instead of the
 * bounding box's -- for a shape whose mass is not spread like its
 * box (a T-handle, a hammer). physics/3d/Body3d has the tensors
 * themselves, and the parallel-axis theorem for putting two together. *)
val solid_as : number * number * number -> body -> body

(* {1 What pushes it (until the next step)} *)

(* [fall g b]: gravity, pulling down g metres per second, per second
 * (9.8 on Earth) *)
val fall : number -> body -> body

(* [push fx fy fz b]: a force, in newtons: the wind, a jet, a spring *)
val push : number -> number -> number -> body -> body

(* [thrust f b]: a push of [f] the way the body points -- its own -z,
 * the direction Camera3d's headings and every character in games3d/
 * face when they are not turned *)
val thrust : number -> body -> body

(* [slow c b]: drag, a push against the motion, c times the velocity.
 * Under [fall g] it gives a terminal speed of g / c. *)
val slow : number -> body -> body

(* [attracted_by other b]: gravitation towards [other], with G M taken
 * as its mass (so a "sun" of mass 1e6 pulls a body 100 m away at
 * 100 m/s^2, as in 2D) *)
val attracted_by : body -> body -> body

(* [pulled_to x y z k b]: a spring from [b] to that point *)
val pulled_to : number -> number -> number -> number -> body -> body

(* [floating ~water ~density b]: Archimedes. [water] is the height of
 * the surface, [density] the body's relative to the water (0.6 wood,
 * 1.0 neutral, 2.7 aluminium); the body settles with that fraction of
 * its height under the surface, and [~damping] (1.5) is the water's
 * drag on the part that is in it. Includes gravity: use it instead of
 * [fall], not with it. *)
val floating : ?damping:number -> water:number -> density:number -> body -> body

(* [spin_slow c b]: rolling friction -- a torque against the way it is
 * turning, c times its angular momentum, so the spin dies away at the
 * same rate whatever the body's shape. Not the same thing as [rough],
 * which is the grip *at a contact* that makes a ball roll in the first
 * place: this is the loss that stops it afterwards. *)
val spin_slow : number -> body -> body

(* [spin_by tx ty tz b]: a torque about each axis, turned into an
 * angular acceleration by the body's tensor -- so it does nothing to
 * an [upright] body, by design *)
val spin_by : number -> number -> number -> body -> body

(* {1 Moving} *)

(* [step b]: [b] one tick later: its velocity changed by everything
 * that pushed it, its position by its velocity, its orientation by its
 * spin, and the pushes forgotten *)
val step : body -> body

(* 1/60 s *)
val tick : number

(* {1 Looking at bodies} *)

(* [draw b]: its shape, turned the way it points and moved where it is *)
val draw : body -> shape3d

(* [debug b]: its *hitbox* -- the box's twelve edges, a ball's or a
 * pill's outline -- and its velocity as a line from its centre;
 * ordinary shape3ds, so every backend draws them. Worth looking at the
 * first time a collision behaves oddly: most of the time the hitbox is
 * not where the drawing is. *)
val debug : body -> shape3d

(* {1 Collisions}

   Found, not yet resolved: see the header. *)

(* do their hitboxes overlap, exactly -- the turned box, the sphere,
 * the capsule, not the bounding boxes *)
val touching : body -> body -> bool

(* and by how much, and which way to push them apart
 * (physics/3d/Contact3d.mli); [None] when they miss *)
val contact : body -> body -> Contact3d.t option

(* [bounce a b]: if they touch, the two of them bouncing off each other
 * -- new velocities and spins from the impulse
 * (physics/3d/Resolve3d.mli), and pushed apart by the overlap. The
 * pair's bounciness is the bouncier one's, its friction the geometric
 * mean of theirs, as in 2D and in Box2D. *)
val bounce : body -> body -> body * body

(* [bounce_off wall b]: [b] bouncing off [wall], which does not move
 * however heavy [b] is (a floor, a bat, a pinball flipper) *)
val bounce_off : body -> body -> body

(* [bounce_all ?broad_phase bodies]: every pair whose bounding boxes
 * overlap, once each. The pairs come from
 * physics/3d/Broadphase3d.mli -- sweep and prune by default, which on
 * 500 marbles compares 7,472 boxes where testing every pair compares
 * 124,750 -- and all three methods find the same pairs, so only the
 * work differs. A pile that has to *stay* still needs more than one
 * pass: that is phase 8. *)
val bounce_all : ?broad_phase:Broadphase3d.method_ -> body list -> body list

(* [broad_phase m bodies]: the pairs [bounce_all] would test, and how
 * many boxes [m] compared to find them -- for drawing the count on
 * screen, as PhysicsMarbles3d.ml does *)
val broad_phase : Broadphase3d.method_ -> body list -> Broadphase3d.result

(* the body's hitbox's axis-aligned box in the world *)
val world_bounds : body -> Broadphase3d.box

(* [ray ~from ~direction bodies]: the first body the ray meets and how
 * far away it is, in metres. What picking with the mouse, aiming, a
 * bullet and a ground check are all made of. *)
val ray :
  from:number * number * number ->
  direction:number * number * number ->
  body list ->
  (body * number) option

(* where it is, and which way it faces (its own -z, turned) *)
val position : body -> number * number * number
val forward : body -> number * number * number

(* between their centres, in metres *)
val distance : body -> body -> number

(* how fast, whatever the direction *)
val speed : body -> number

(* the bounding box of a shape3d, as (min, max) corners: what [body]
 * measures a shape with. A shape with no points at all (a lone
 * {!Playground3d.hud}) gives ((0,0,0), (0,0,0)). *)
val bounds : shape3d -> (number * number * number) * (number * number * number)

(* {1 A pile: every body at once}

   [bounce] answers one pair, once. In a pile each answer undoes a bit
   of another -- push the top crate up and the one under it goes down,
   and into the floor -- so a stack solved that way jitters and sinks.
   A [world] solves all of a step's contacts together, over and over
   until they agree (physics/3d/Solver3d.mli), which is what lets
   things be *stacked* rather than merely bounced.

   It is the same loop a game writes by hand, so it reads the same:

   {[
     let world = Physics3d.world (floor :: crates)

     (* in update *)
     let world = Physics3d.simulate ~gravity:9.8 world

     (* in view *)
     List.map Physics3d.draw world.bodies
   ]} *)

type world = {
  bodies : body list;
  (* the previous step's impulses, for warm starting *)
  memory : Solver3d.memory;
  (* how many steps each body has been slow *)
  still : int list;
  (* which of them are asleep, in order: for drawing them differently,
   * which is worth doing once to watch a pile settle *)
  asleep : bool list;
  (* how many contact points the last step solved, for a HUD *)
  solved : int;
  (* how many bodies the last tick's sweep stopped short (see [simulate]
   * ~continuous): each one would have been through something *)
  swept : int;
  (* what holds its bodies together (see Joints below), naming them by
   * their place in [bodies] *)
  joints : Joint3d.t list;
}

(* the walls and floors among them [immovable] *)
val world : body list -> world

(* [simulate ?gravity ?iterations ?warm_starting ?sleeping ?broad_phase
 * w]: one tick of the whole world -- gravity, then every contact
 * solved together, then the moves.
 *
 * [sleeping] (on by default) stops stepping bodies that have been slow
 * for a second, until something moving touches them: a finished pile
 * then costs nothing and, more to the point, stops shivering. They
 * sleep in *groups* -- everything that touches, together -- because a
 * crate sent to sleep alone while the ones above it are still settling
 * gets woken a moment later with a jolt, which is measurable (it
 * happened every sixty-one steps: the threshold, plus one). *)
(* [continuous] (off by default): continuous collision, for small fast
 * spheres (physics/3d/Sweep3d.mli). A sphere going farther in a step
 * than a quarter of its radius is swept along its path, and stopped
 * where it first touches something -- its velocity kept, so that the
 * next step's contact bounces it. Without it, a 27 mm pinball at 3 m/s
 * goes 5 cm a step and passes through a 1 cm wall having overlapped it
 * on no step at all.
 *
 * [substeps] (1): the tick cut into that many steps, the other answer
 * to the same problem -- simpler, as costly as the count, and only as
 * good as a step is small. The game's pushes last the whole tick. *)
val simulate :
  ?gravity:number ->
  ?iterations:int ->
  ?warm_starting:bool ->
  ?sleeping:bool ->
  ?broad_phase:Broadphase3d.method_ ->
  ?continuous:bool ->
  ?substeps:int ->
  world ->
  world

(* [went_through fast b]: whether [fast], during its last tick (from
 * where it was a tick ago to where it is), passed through [b] -- swept
 * as a sphere of its narrowest size. The question, where [simulate]
 * ~continuous is the answer: for counting what tunnelled, or for a
 * bullet that only needs to know what it hit. Where it was is worked
 * out from where it is going now, so the answer is for a body that did
 * not bounce during the tick: one that did has its velocity turned
 * round, and its path is drawn through the very wall it bounced off
 * (games3d/TinyPinball3d.ml compares its two positions instead). *)
val went_through : body -> body -> bool


(* {1 Joints}

   A joint takes away some of the ways two bodies of a world can move
   against each other (physics/3d/Joint3d.mli), solved in [simulate]'s
   loop with the contacts. The bodies are named by their place in the
   world's [bodies], and the joint is made from where they are now:

   {[
     let world =
       Physics3d.world [ frame; door ]
       |> Physics3d.hinge 0 1 ~at:(0.5, 1., 0.) ~axis:(0., 1., 0.) ~limits:(0., 110.)
   ]}

   Two bodies joined never collide with each other, and sleep together.
   Angles are in degrees, as everywhere in this module. *)

(* [ball_joint ?cone a b ~at w]: a ball-and-socket at [at] (a
 * shoulder); [cone] (an axis, degrees): the most the two bodies may
 * turn that axis apart *)
val ball_joint : ?cone:(number * number * number) * number -> int -> int -> at:number * number * number -> world -> world

(* [hinge ?limits ?motor a b ~at ~axis w]: a hinge through [at] about
 * [axis] (a door); [limits] (least, most) in degrees from where it is
 * now; [motor] (degrees a second, the most torque in N m) *)
val hinge :
  ?limits:number * number ->
  ?motor:number * number ->
  int ->
  int ->
  at:number * number * number ->
  axis:number * number * number ->
  world ->
  world

(* [rod a b ~at_a ~at_b w]: those two points kept as far apart as they
 * are now (a rope that never slackens, a chain's link) *)
val rod : int -> int -> at_a:number * number * number -> at_b:number * number * number -> world -> world

(* [set_motor i (speed, torque) w]: the [i]-th joint's motor, if it is a
 * hinge (a pinball flipper's kick, a car's wheel) *)
val set_motor : int -> number * number -> world -> world

(* the [i]-th joint's angle, degrees, if it is a hinge *)
val joint_angle : int -> world -> number

(* [held_by point b]: [b] pulled to [point] -- to reach it in a tenth of
 * a second, no faster than 15 m/s -- and its spin mostly taken away:
 * the gravity gun's hold, to set on the held body before each
 * [simulate] *)
val held_by : number * number * number -> body -> body

(* the joints' anchors, one small cube for each body's: when a joint
 * holds, the two are in the same place *)
val debug_joints : world -> shape3d list
