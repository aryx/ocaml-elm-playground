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
     turn itself over (examples3d/PhysicsSpin3d.ml). [body] gives every
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

   Not here yet, and named so that the gap is visible: collisions
   ([touching], [bounce]), rays, the character controller, joints. They
   arrive with the phases of
   docs/claude_notes/plans/plan_physics3d_teaching.md; until then a game
   does its own contacts, as games3d/TinyMario64.ml does.
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
  inertia : Mat3.t; (* how hard it is to spin: its bounding box's, by default *)
  ax : number; (* what pushes it until the next [step]: *)
  ay : number; (*   accelerations, set by fall, push, ... *)
  az : number;
  torque : number * number * number; (* and what turns it, set by spin_by *)
}

(* {1 Making bodies} *)

(* [body shape]: a body looking like [shape], at the origin, not
 * moving, not turned, of mass 1, and with the inertia tensor of its
 * own bounding box -- so it can be spun, and will wobble like the
 * solid it looks like. [shape] should be centred on the origin (as
 * {!Playground3d.box}, {!Playground3d.sphere} and friends are): [draw]
 * turns it about that point. *)
val body : shape3d -> body

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

(* [immovable b]: an infinite mass, so nothing can push it (a floor) *)
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

(* [debug b]: its bounding box as twelve edges, and its velocity as a
 * line from its centre -- ordinary shape3ds, so every backend draws
 * them *)
val debug : body -> shape3d

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
