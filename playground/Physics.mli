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
  ax : number;            (* what pushes it until the next [step]: *)
  ay : number;            (*   accelerations, set by fall, push, ... *)
}

(* {1 Making bodies} *)

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

(* {1 What pushes it (until the next step)} *)

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

(* [turn speed b]: [b] turning at [speed] degrees per second (positive
 * counterclockwise, like rotate); 0 to stop *)
val turn : number -> body -> body

(* {1 Moving} *)

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

(* {1 Collisions} *)

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
 * bodies moving less, and pushed apart so they don't overlap anymore;
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
 * box of marbles (every pair tested: fine for tens of bodies, slow for
 * thousands) *)
val bounce_all : body list -> body list

(* [debug b]: [b]'s hitboxes as translucent green shapes, and its
 * velocity as an arrow (a quarter of a second of motion): draw it over
 * the game to see what the physics sees *)
val debug : body -> shape

(* {1 Looking at bodies} *)

(* [draw b]: its shape, where it is, turned the way it points *)
val draw : body -> shape

(* [distance a b]: between their centers, in pixels *)
val distance : body -> body -> number

(* [speed b]: how fast, in pixels per second, whatever the direction *)
val speed : body -> number

(* [outside screen b]: [b]'s center is off the screen *)
val outside : screen -> body -> bool
