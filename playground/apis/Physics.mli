(* Physics: things that move by themselves.

   With the playground alone, a game moves its things by hand: add 3 to
   x at every tick, subtract a bit from the jump speed so Mario comes
   back down (examples/Mario.ml). That works, but it's the game doing
   the physics, and each game does it differently. With this module, a
   game says *what* happens -- the ship thrusts, the ball falls, the
   wind pushes -- and the thing moves the way things move in the real
   world:

     let ball = body (circle red 20) |> at 0 300 |> moving 200 0

     (* in update, at every tick *)
     let ball = ball |> fall 800 |> step

     (* in view *)
     [ draw ball ]

   and the ball flies off sideways, curving down in a parabola, like a
   thrown ball does. One new idea only: a *body* is a shape that moves --
   where it is, how fast it goes, which way it points. Everything else
   is verbs on bodies, used like move and rotate on shapes.

   Three things to know:

   - [step] is one tick of time, 1/60 of a second: speeds are in pixels
     per second ([moving 100 0] crosses a 1000-pixel screen in 10
     seconds), and gravity in pixels per second, per second.
   - Verbs like [fall], [push] and [slow] don't move the body: they add
     up what pushes it, and [step] then moves it, all at once. So their
     order doesn't matter, and [step] comes last:
       ship |> thrust 300 |> fall 50 |> slow 0.5 |> step
   - Everything is a value: [step] gives back a new body, like [update]
     gives back a new model.

   Underneath is a small physics engine written to be read,
   physics/2d/ (see docs/claude_notes/notes_2d_physics.md): [step] is
   one step of Integrate.semi_implicit_euler, the method real game
   physics engines use, and Integrate.mli shows what the simpler one
   would do to an orbit.
*)

open Playground

(* A shape that moves. A record, like [computer], so a game can read
 * [ship.x] or [ship.vy]; make one with [body], then change it with the
 * functions below rather than by hand. *)
type body = {
  shape : shape;          (* what it looks like, drawn by [draw] *)
  x : number;             (* where it is *)
  y : number;
  vx : number;            (* its velocity, in pixels per second *)
  vy : number;
  angle : number;         (* which way it points, in degrees, like rotate *)
  spin : number;          (* how its angle changes, degrees per second *)
  mass : number;          (* how hard it is to push, 1 by default *)
  bounciness : number;    (* how it bounces, 0 (clay) by default *)
  friction : number;      (* how it grips what it slides on, 0 by default *)
  upright : bool;         (* true: collisions never turn it (false by default) *)
  group : int;            (* bodies of the same group never collide (0, none: see [grouped]) *)
  ax : number;            (* what pushes it until the next [step]: *)
  ay : number;            (*   accelerations, set by fall, push, ... *)
}

(*****************************************************************************)
(* {1 Making bodies} *)
(*****************************************************************************)

(* [body shape]: a body looking like [shape], at (0, 0), not moving,
 * pointing right (angle 0) *)
val body : shape -> body

(* [at x y b]: [b] placed at (x, y) *)
val at : number -> number -> body -> body

(* [moving vx vy b]: [b] moving vx pixels per second to the right and
 * vy up *)
val moving : number -> number -> body -> body

(* [launched speed angle b]: [b] moving at [speed] in the direction
 * [angle] (degrees: 0 right, 90 up), like a cannonball or a bullet *)
val launched : number -> number -> body -> body

(* [shot_from speed distance shooter b]: [b] fired by [shooter]: placed
 * [distance] pixels ahead of it, the way it points, and moving at
 * [speed] that way plus [shooter]'s own velocity -- a bullet leaving a
 * moving ship keeps the ship's motion (Galileo's ship, again) *)
val shot_from : number -> number -> body -> body -> body

(* [pointing angle b]: [b] turned to [angle] degrees *)
val pointing : number -> body -> body

(* [heavy mass b]: [b] with this mass (1 by default); a heavier body
 * moves less when pushed (F = m a), but falls just as fast *)
val heavy : number -> body -> body

(* [bouncy e b]: how [b] bounces off things (the restitution): 0. not
 * at all, like clay (the default), 0.5 losing half its speed, 1. a
 * superball bouncing back as fast, and above 1 a pinball bumper,
 * *adding* speed. When two bodies collide, the bouncier one decides. *)
val bouncy : number -> body -> body

(* [rough mu b]: friction, how much [b] grips what it slides against,
 * from 0. (ice, the default) to 1. (rubber): a ball hitting a rough
 * moving paddle is dragged along with it. Both bodies must be rough
 * for friction (their rough-nesses are multiplied, then the square root
 * taken, like Box2D) *)
val rough : number -> body -> body

(* [immovable b]: nothing it collides with moves it (an infinite mass):
 * walls, the floor, a paddle the game moves itself *)
val immovable : body -> body

(* [upright b]: collisions never make it turn (an infinite moment of
 * inertia): a platformer's hero, who shouldn't tip over on a ledge.
 * Otherwise a body hit off its center spins, like a real one: a box
 * landing on a corner tips over, a ball sliding on a rough floor
 * starts rolling. How hard it is to spin (its moment of inertia) comes
 * from its shape and mass: a ring of mass at the edge spins harder
 * than the same mass at the center. [turn] still turns it.
 *
 * An upright body is exactly the engine as it was before rotation (the
 * plan's phase 6), not an approximation: its inertia is infinite, so
 * every rotation term of the collisions (physics/2d/Resolve.mli) is
 * multiplied by 1 / inertia = 0 -- the lever arms (r x n)^2 / I, the
 * torques -- and its spin stays 0, so its touching point moves at its
 * velocity: what's left are the formulas without rotation, bit for
 * bit. (The contact point, only used through the lever arms, then
 * doesn't matter either.) Drawn, its angle never changes. *)
val upright : body -> body

(* [grouped n b]: [b] in the group [n] (a number of the game's choosing,
 * not 0: 0 is "no group", what [body] gives). Two bodies of the same
 * group never collide in [simulate]: they pass through each other, as
 * if the other were not there. Everything else still collides with
 * both.
 *
 * What it is for: things that overlap *by design*. A soldier seen
 * from the side has its arms over its chest and one leg over the
 * other, all the time; a car's wheels sit inside its wheel arches;
 * the links of a chain cross at their pins. Made of bodies and
 * joints, such a thing would tear itself apart, each part pushed out
 * of the others at every step. Give all its parts the same group:
 *
 *   let limb shape = body shape |> grouped 7
 *
 * and they collide with the floor, the walls and other soldiers
 * (another group, or none), but not with each other.
 *
 * Before this there was one way to say "these two do not collide": a
 * joint between them (a seesaw sits on its pivot, see Joints below).
 * mini-soldat's ragdoll, ten limbs, had nine real joints and 36 ropes
 * ten thousand pixels long, never taut, between every other pair,
 * only to say so. That is what this replaces.
 *
 * It is the simplest of the ways engines have for it: Box2D has this
 * one (a negative "group index": never collide) and also sixteen
 * categories with a mask each ("I am a bullet, I hit walls and
 * soldiers but not bullets"), which says more and takes two more
 * numbers a body; a category for each kind of thing is the next step
 * if a game needs "bullets go through bullets". Not asked by [bounce],
 * [bounce_all] or [touching]: there the game names the two bodies
 * itself, and can leave a pair out. *)
val grouped : int -> body -> body

(*****************************************************************************)
(* {1 What pushes it (until the next step)} *)
(*****************************************************************************)

(* [fall g b]: gravity, pulling down g pixels per second, per second,
 * whatever the mass (500 to 1000 feels right on a 1000-pixel screen) *)
val fall : number -> body -> body

(* [push fx fy b]: a force: the wind, a spring, a jet; divided by the
 * mass *)
val push : number -> number -> body -> body

(* [thrust f b]: a push of [f] forward, the way the body points: a
 * rocket, Asteroids' ship *)
val thrust : number -> body -> body

(* [slow c b]: drag, a push against the motion, c times the velocity:
 * air, water, friction. Under a steady push, a body then stops speeding
 * up (at the push divided by c): a top speed, for free *)
val slow : number -> body -> body

(* [attracted_by other b]: gravitation, [b] pulled towards [other] --
 * harder when [other] is heavier and nearer: other.mass / r^2 pixels
 * per second, per second, r the distance between them (Newton's law of
 * gravitation, with the constant G = 1 in the playground's units). A
 * star made [heavy 1000000.] keeps a body 100 pixels away going around
 * it at 100 pixels per second: on a circle, the speed is
 * sqrt (mass / r). Spacewar!'s star, a planet's moon. *)
val attracted_by : body -> body -> body

(* [pulled_to x y k b]: a spring from [b] to the point (x, y): pulled
 * towards it k times the distance, per second per second (Hooke's
 * law): a bungee, a grappling hook (Worms' ninja rope, Soldat's). Add
 * [slow] to calm it down; too stiff for the time step (k over 14,400),
 * it explodes (physics/2d/Springs.mli) *)
val pulled_to : number -> number -> number -> body -> body

(* [turn speed b]: [b] turning at [speed] degrees per second (positive
 * counterclockwise, like rotate); 0 to stop *)
val turn : number -> body -> body

(*****************************************************************************)
(* {1 Moving} *)
(*****************************************************************************)

(* [step b]: [b] one tick (1/60 s) later: its velocity changed by what
 * pushed it, its position by its velocity, its angle by its spin; the
 * pushes are then used up. (Not called move, which moves shapes: a game
 * opens both.) *)
val step : body -> body

(* the length of a tick, 1/60 s *)
val tick : number

(* [wrap screen b]: [b] brought back on the other side when it goes off
 * the screen, like Asteroids' ship *)
val wrap : screen -> body -> body

(* [bounce_in screen bounciness b]: [b] bouncing off the screen's edges
 * instead of leaving: its velocity reversed, times [bounciness] (1. a
 * superball, 0.5 loses half its speed, 0. stops dead), like Pong's
 * ball *)
val bounce_in : screen -> number -> body -> body

(*****************************************************************************)
(* {1 Collisions} *)
(*****************************************************************************)

(* [touching a b]: whether they overlap, exactly: the bodies' real
 * shapes, turned the way they point -- a bullet (a small circle) inside
 * an asteroid (a polygon, even a concave one), the corner of a rotated
 * box. The hitboxes come from the shapes themselves: a [circle] is a
 * circle, a [rectangle], [square], [image], [triangle], [hexagon] (the
 * regular polygons) or [polygon] a polygon, an [oval] a 16-sided polygon, [words] their box; a [group]'s
 * shapes each count, where they were moved. (See physics/2d/Collide.mli
 * for the tests, from circles to the separating axis theorem.) *)
val touching : body -> body -> bool

(* [bounce a b]: if they touch, [a] and [b] bouncing off each other --
 * their velocities changed at once, like billiard balls, heavier
 * bodies moving less, and spinning when hit off their center (unless
 * [upright]), and pushed apart so they don't overlap anymore;
 * otherwise [a] and [b] unchanged. After [step]:
 *   let (ball1, ball2) = bounce (step ball1) (step ball2)
 * The total momentum (mass times velocity) is the same after; the
 * speeds too if the bounciness is 1. For circles and convex polygons
 * (a concave one doesn't bounce: split it in convex pieces, a group).
 * (See physics/2d/Resolve.mli.) *)
val bounce : body -> body -> body * body

(* [bounce_off wall b]: [b] bouncing off [wall], which doesn't move
 * (treated as [immovable]), for pipelines:
 *   ball |> fall 800. |> step |> bounce_off floor |> bounce_off paddle *)
val bounce_off : body -> body -> body

(* [bounce_all bodies]: every two of them bouncing off each other, a
 * box of marbles. Only the pairs whose bounding boxes overlap are
 * tested exactly, found by [broad_phase] (sort and sweep by default,
 * see physics/2d/Broadphase.mli), so hundreds of bodies are fine. *)
val bounce_all : ?broad_phase:Broadphase.method_ -> body list -> body list

(* [broad_phase m bodies]: the pairs [bounce_all] would test exactly,
 * and how many box tests it took [m] to find them: to compare the
 * three methods (examples/PhysicsMarbles.ml) *)
val broad_phase : Broadphase.method_ -> body list -> Broadphase.result

(* [went_through fast b]: whether [fast], during its last [step] (from
 * where it was a tick ago to where it is), went through [b] -- for
 * things too fast for [touching]: a bullet at 1500 pixels per second
 * moves 25 pixels a tick, and jumps over a 10-pixel wall without ever
 * touching it (tunneling). Its path is tested instead of its position
 * (physics/2d/Collide.mli's swept tests). *)
val went_through : body -> body -> bool

(* [debug b]: [b]'s hitboxes as translucent green shapes, and its
 * velocity as an arrow (a quarter of a second of motion): draw it over
 * the game to see what the physics sees *)
val debug : body -> shape

(*****************************************************************************)
(* {1 Piles: many bodies at once} *)
(*****************************************************************************)

(* [bounce_all] fixes one pair at a time, once: fine for balls in a box,
 * not for a pile of boxes, which jitters and sinks (each fix undoes a
 * bit of another). A [world] solves all the contacts together, again
 * and again, remembering them from one step to the next (see
 * physics/2d/Solver.mli): a pyramid of boxes stands still. *)
type world = {
  bodies : body list;
  (* the contacts' impulses of the last step, for the next one: the
   * bodies are known by their place in the list, so add new ones at
   * the end *)
  memory : Solver.memory;
  (* what holds its bodies together (see Joints below), naming them by
   * their place in [bodies] *)
  joints : Joint2d.t list;
}

(* [world bodies]: the walls and floors among them [immovable] *)
val world : body list -> world

(* [simulate ~gravity w]: one tick of the whole world: every body
 * pushed (by gravity, pixels per second per second, 0 by default, and
 * by what was added to it with fall, push...), then all the contacts
 * solved together, then every body moved -- [step] and [bounce_all]
 * in one, for piles:
 *   let w = w |> simulate ~gravity:800.
 * [iterations] (10 by default: more is stiffer, slower) and
 * [warm_starting] (true) are there to see what they do: with 1
 * iteration, or without warm starting, a pyramid sags and slides.
 *
 * {b [steps]: several small steps in a tick} (1 by default).
 *
 * A step moves every body by its speed times the step's length, and
 * only then looks at what overlaps. Nothing is looked at *on the way*.
 * So a body that goes farther in one step than it is thick can be,
 * when the step ends, deeper inside what it hit than it is wide --
 * or right through it (a bullet through a wall: the classic,
 * "tunnelling"; [went_through] above is for that one, a ray along its
 * way).
 *
 * The first case is the nastier, and what [steps] is for. Two shapes
 * that overlap are pushed apart along the direction in which they
 * overlap *least* (physics/2d/Collide.mli: the separating axis test),
 * which is right when the overlap is shallow: a box resting on the
 * floor overlaps it by a hair, downwards, and is pushed up. But take
 * a stick 2 pixels thick, standing on its end, falling at 200 pixels a
 * second: at 60 steps a second it moves 3.3 pixels a step, and the
 * step in which it reaches the floor leaves it 3.2 deep in it.
 *
 *       before the step      after: 3.2 deep, 2 wide
 *
 *            |                     |
 *            |                     |
 *     -------+------         ------|------    the least overlap is now
 *                                  |          *across* the stick (2),
 *                                             not up (3.2)
 *
 * The least overlap is now sideways, so the stick is pushed out
 * sideways -- along a floor that has no side to come out of. The next
 * step finds it still inside and pushes again, harder; in a few
 * steps it leaves at 10,000 pixels a second. Nothing is wrong with
 * the solver: it was asked the wrong question, about where the stick
 * is rather than how it got there. (Found in mini-soldat, where a
 * rifle let go is exactly that stick; a box 4 thick is not thrown, 2
 * is.)
 *
 * The cure is not to let a body go farther in a step than its
 * thinnest part: [~steps:4] cuts the tick into four steps of 1/240 of
 * a second, each with its own contacts found and solved, and the
 * stick, moving 0.8 a step, is caught while its overlap with the
 * floor is still the shallow one:
 *
 *   let w = w |> simulate ~gravity:800. ~steps:4
 *
 * Still one tick: speeds are per second as always, what was added
 * with fall, push... pushes during all four, and the world a tick
 * later comes back. What changes, besides that nothing is thrown:
 *
 * - it costs [steps] times the work: use it where something is thin
 *   or fast, not everywhere;
 * - a fall is a little shorter. Each step adds its part of gravity to
 *   the speed and then moves, so in one tick from rest a body falls
 *   g/3600 with one step, and g/3600 x (1+2+3+4)/16 = 0.625 of that
 *   with four (the exact answer is half: Euler's error, which
 *   smaller steps shrink; Integrate.mli has the picture). A game that
 *   compared positions to the pixel after a step will see it;
 * - joints and piles are stiffer, for the same reason more
 *   [iterations] are: an error is corrected a part at each step.
 *
 * Rule of thumb: steps >= the fastest speed, in pixels a tick, over
 * the thinnest body's thickness. Box2D calls these sub-steps and now
 * prefers them to iterations ("Solver2D", Erin Catto, 2024: many small
 * steps of one iteration beat one step of many); the other cure,
 * looking along the way of every fast body (continuous collision
 * detection), costs less but is far more code.
 *
 * Two bodies of the same group (see [grouped]) do not collide, nor two
 * that a joint holds together. *)
val simulate : ?gravity:number -> ?iterations:int -> ?warm_starting:bool -> ?steps:int -> world -> world

(*****************************************************************************)
(* {1 Joints} *)
(*****************************************************************************)
(* A joint takes away some of the ways two bodies of a world can move
   against each other (physics/2d/Joint2d.mli), solved in [simulate]'s
   loop with the contacts; two bodies joined don't collide. The bodies
   are named by their place in the world's [bodies], and the joint is
   made from where they are now; a joint to the world is one to an
   [immovable] body:

     let w = world [ pivot; plank ] |> pin 0 1 ~at:(0., 0.)
*)

(* [pin ?motor a b ~at w]: [a] and [b] held together at the point [at],
   free to turn about it (a seesaw, a wheel); [motor]: the spin of b
   against a asked (degrees a second, counterclockwise) and the most
   torque (a conveyor's roller) *)
val pin : ?motor:number * number -> int -> int -> at:number * number -> world -> world

(* [rod a b ~at_a ~at_b w]: those two points kept as far apart as they
   are now *)
val rod : int -> int -> at_a:number * number -> at_b:number * number -> world -> world

(* [rope ?length a b ~at_a ~at_b w]: a rope between the two points,
   [length] long (as long as they are apart by default): it pulls when
   taut, and is slack when shorter *)
val rope : ?length:number -> int -> int -> at_a:number * number -> at_b:number * number -> world -> world

(* [pulley a b ~at_a ~at_b ~ground_a ~ground_b w]: a rope from [at_a] up
   over the fixed point [ground_a], across to [ground_b], down to
   [at_b]: what one side gains, the other gives up *)
val pulley :
  int -> int -> at_a:number * number -> at_b:number * number -> ground_a:number * number -> ground_b:number * number -> world -> world

(* [set_motor i (speed, torque) w]: the [i]-th joint's motor, if it is a
   pin (a switch turning a machine on) *)
val set_motor : int -> number * number -> world -> world

(* the [i]-th joint's length now (a rope's, a rod's, a pulley's two
   sides together) *)
val joint_length : int -> world -> number

(* the joints, drawn: a dot for a pin, a line for a rod or a rope, the
   three lines of a pulley *)
val debug_joints : world -> shape list

(*****************************************************************************)
(* {1 Looking at bodies} *)
(*****************************************************************************)

(* [draw b]: its shape, where it is, turned the way it points *)
val draw : body -> shape

(* [distance a b]: between their centers, in pixels *)
val distance : body -> body -> number

(* [speed b]: how fast, in pixels per second, whatever the direction *)
val speed : body -> number

(* [outside screen b]: [b]'s center is off the screen *)
val outside : screen -> body -> bool
