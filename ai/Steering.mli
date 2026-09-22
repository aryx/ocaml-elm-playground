(* Steering behaviours: how a character moves, as a small force added
 * every frame, rather than along a path planned ahead.
 *
 * Craig Reynolds's idea (1999): a character is a vehicle -- a position,
 * a velocity, a top speed and a limit on how hard it can turn -- and
 * each behaviour says only where it *wants* to go, as a velocity:
 *
 *   desired = (where I want to go) - (where I am), scaled to top speed
 *   steer   = desired - (my velocity),  clamped to the vehicle's force
 *
 *             desired                 the steering force is what turns
 *            ^                        the velocity into the desired one,
 *           /                         a little every frame: a vehicle
 *          /   steer                  going up that wants to go up and
 *         / ----->                    right curves over, it doesn't
 *        o-------> velocity           snap round
 *
 * So every behaviour here returns a *desired velocity*, and [steer]
 * turns it into the force; forces add ([blend]), so behaviours combine
 * -- flee the wolf, stay with the flock, avoid the rocks -- by adding
 * them with weights. Because the force is an acceleration, it goes
 * straight into a physics engine's body (playground/Ai.mli does that
 * for Physics.body), or through [move] for a vehicle on its own.
 *
 * And a second form, for characters with no velocity to steer: a
 * character that walks at a fixed speed and dashes on commitment
 * (games3d/TinyBoomerangFu.ml's) has nothing a force could act on, so
 * it takes the [direction] of the desired velocity and walks that way.
 * The same behaviours, a different body.
 *
 * Worked example ([seek]): a vehicle at (0, 0) going up at 100, top
 * speed 200, force 50, seeking (300, 400). The target is 500 away in
 * the direction (0.6, 0.8): desired (120, 160); steer = desired -
 * velocity = (120, 60), 134 long, clamped to 50: (44.7, 22.4). The
 * vehicle keeps going up, and starts curving right.
 *
 * Why [arrive] and [wander] matter more than they look: [seek] alone
 * gives the unmistakable homing missile, which overshoots and orbits;
 * [arrive] is what makes a thing look as if it meant to stop there, and
 * [wander] what makes an idle creature look alive rather than frozen or
 * twitching. The believability is in the derivative.
 *
 * References: Craig Reynolds, "Steering Behaviors For Autonomous
 * Characters" (GDC 1999) -- seek, flee, pursuit, evasion, arrival,
 * wander, obstacle avoidance, path following, all here; Mat Buckland,
 * "Programming Game AI by Example" (2005), chapter 3, for the forms
 * written out. *)

type vec = float * float

type vehicle = {
  position : vec;
  velocity : vec;
  max_speed : float; (* units per second *)
  max_force : float; (* units per second, per second: how hard it turns *)
}

(* {1 Behaviours: desired velocities} *)

(* straight at the target, at top speed *)
val seek : vec -> vehicle -> vec

(* straight away from it *)
val flee : vec -> vehicle -> vec

(* [arrive ~slowing target v]: seek, but slower and slower within
 * [slowing] of the target (100 by default), stopping on it: at half
 * that distance, half the top speed *)
val arrive : ?slowing:float -> vec -> vehicle -> vec

(* [pursue target v]: seek where [target] will be, guessing that
 * catching it takes its distance over [v]'s top speed. E.g. from
 * (0, 0), top speed 200, a target at (400, 0) going up at 100: 2
 * seconds, so seek (400, 200) *)
val pursue : vehicle -> vehicle -> vec

(* [evade target v]: flee where [target] will be *)
val evade : vehicle -> vehicle -> vec

(* [wander ~distance ~radius ~angle v]: seek a point on a circle of
 * [radius] (40) held [distance] (80) ahead of [v]; [angle] (radians,
 * 0 straight ahead) is where on the circle, and the caller moves it a
 * little each frame -- by a small random step, or smooth noise -- so the
 * point drifts round the circle and the path curves one way, then the
 * other, instead of twitching:
 *
 *        ,-- . --,     the circle ahead; the target point drifts on
 *       /    x    \    it, and the vehicle seeks it
 *      |           |
 *       \ _     _ /
 *            |
 *            o  -> heading
 *)
val wander : ?distance:float -> ?radius:float -> angle:float -> vehicle -> vec

(* [avoid ~ahead ~size obstacles v]: [v]'s velocity as it is, unless one
 * of the circles (centre, radius) is in the corridor [ahead] (100)
 * long and [size] (10) wide on each side in front of it: then turned
 * away from the nearest such circle, sideways, the more urgently the
 * nearer it is *)
val avoid : ?ahead:float -> ?size:float -> (vec * float) list -> vehicle -> vec

(* [follow ~ahead ~width path v]: [v]'s velocity as it is while where it
 * will be in [ahead] (50) is within [width] of the path (a polyline);
 * otherwise, seek the point of the path nearest to that, moved [ahead]
 * along the path -- Reynolds's path following, the road kept to
 * without a rail *)
val follow : ?ahead:float -> width:float -> vec list -> vehicle -> vec

(* {1 Forces} *)

(* [steer v desired]: the force turning [v]'s velocity towards
 * [desired], clamped to [v.max_force] *)
val steer : vehicle -> vec -> vec

(* the weighted sum of forces *)
val blend : (float * vec) list -> vec

(* [move ~dt force v]: [v] after [dt] seconds pushed by [force] (an
 * acceleration): its velocity changed then clamped to its top speed,
 * then its position moved (semi-implicit Euler) *)
val move : dt:float -> vec -> vehicle -> vehicle

(* the unit vector a desired velocity points along, (0, 0) for none: the
 * way a fixed-speed character walks *)
val direction : vec -> vec

(* the way [v] goes, a unit vector (to the right when it is still) *)
val heading : vehicle -> vec
