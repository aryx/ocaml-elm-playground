(* Joints: what keeps two bodies together (physics/3d/Joint3d.mli is
 * the same idea in 3D, where a hinge needs five rows where a pin here
 * needs two).
 *
 * A joint takes away some of the ways two bodies can move against each
 * other. It is solved in the very loop that solves the contacts
 * (Solver): a contact is a constraint too, one that may only push.
 * Each joint becomes one or two *rows*, each a direction, and a speed
 * to reach along it:
 *
 *   pin       the two anchors kept on the same point: 2 rows, x and y
 *             (a seesaw on its pivot, a wheel on its axle) -- and with a
 *             motor, a third on the two bodies' turning, at the speed
 *             asked, with a torque it cannot exceed (a conveyor's roller)
 *   rod       1 row along the line between the anchors: always that far
 *   rope      the same row, but it may only pull: slack when shorter
 *   pulley    two ropes over two fixed points, one row on the *sum* of
 *             their lengths: what one side gains, the other gives up
 *
 *            pin                 rope                 pulley
 *                                                  ga *    * gb
 *        a ===*=== a'          a *                    |    |
 *             ^ b               \  (slack: nothing)   |    |
 *        the same point          \                    a    b
 *        in both bodies           * b              la + lb = constant
 *
 * One row, whatever the joint, is the same few numbers: how each body
 * moves along it (a direction for its center, a lever for its turning),
 * the speed wanted, and the running total of its impulse, which may be
 * clamped (a rope's never pushes, a motor's torque is limited). Solved
 * by sequential impulses exactly as a contact is: each row's impulse
 * corrected towards its speed, again and again with the others. What
 * has drifted apart is pulled back by Baumgarte's term, a speed towards
 * closing the gap, as an overlap is.
 *
 * Example: a body of mass 1 hanging from a fixed pin by a rod of 10,
 * falling at 10 a second: the rod's row takes away all its speed along
 * the rod, and leaves the speed across it -- a pendulum.
 *
 * The anchors are in each body's own frame, so that they turn with it:
 * the solver is given the bodies' angles (radians) with the bodies, as
 * Body.t has none of its own. Joints name their bodies by their place
 * in the array the solver is given, as the contact pairs do; a joint
 * to the world is a joint to an immovable body.
 *
 * References: Erin Catto, "Iterative Dynamics with Temporal Coherence"
 * (GDC 2005), "Modeling and Solving Constraints" (GDC 2009), and Box2D
 * Lite's "A Teeter" and "Suspension Bridge" demos. *)

type kind =
  | Pin of { motor : (float * float) option } (* the relative spin asked, radians a second, and the most torque *)
  | Rod of { length : float }
  | Rope of { length : float } (* the most length: shorter, slack *)
  | Pulley of { ground_a : Vec2.t; ground_b : Vec2.t; length : float } (* la + lb, the ropes' total *)

type t = {
  a : int;
  b : int;
  anchor_a : Vec2.t; (* in a's own frame *)
  anchor_b : Vec2.t; (* in b's *)
  kind : kind;
}

(* {1 Making joints, from where the bodies are now} *)

(* [pin bodies angles a b ~at ?motor ()]: a pin at the world point
 * [at]; [motor]: the spin of b relative to a asked (radians a second,
 * counterclockwise) and the most torque *)
val pin : Body.t array -> float array -> int -> int -> at:Vec2.t -> ?motor:float * float -> unit -> t

(* [rod bodies angles a b ~at_a ~at_b ()]: a rod between those two world
 * points, as long as they are apart now *)
val rod : Body.t array -> float array -> int -> int -> at_a:Vec2.t -> at_b:Vec2.t -> unit -> t

(* [rope ... ?length ()]: a rope between them, [length] long (by
 * default as long as they are apart now) *)
val rope : Body.t array -> float array -> int -> int -> at_a:Vec2.t -> at_b:Vec2.t -> ?length:float -> unit -> t

(* [pulley bodies angles a b ~at_a ~at_b ~ground_a ~ground_b ()]: a rope
 * from [at_a] up over the fixed point [ground_a], across to [ground_b]
 * and down to [at_b], as long as it is now *)
val pulley :
  Body.t array -> float array -> int -> int -> at_a:Vec2.t -> at_b:Vec2.t -> ground_a:Vec2.t -> ground_b:Vec2.t -> unit -> t

(* {1 What the solver does with them} *)

(* one row: how each body moves along it, the speed asked, the running
 * total of its impulse, clamped to [lo, hi] *)
type row

(* [rows ~beta ~dt bodies angles j]: [j]'s rows for this step, [beta]
 * the fraction of the drift corrected per step; none for a slack rope *)
val rows : beta:float -> dt:float -> Body.t array -> float array -> t -> row list

(* [solve_row bodies r]: one correction of the row's impulse, applied to
 * the bodies' velocities *)
val solve_row : Body.t array -> row -> unit

(* {1 Measuring} *)

(* where the joint's two anchors are in the world: the same point when a
 * pin holds *)
val anchors : Body.t array -> float array -> t -> Vec2.t * Vec2.t

(* the rope, rod or pulley's length now (a pulley's, both sides'); 0
 * for a pin *)
val length_now : Body.t array -> float array -> t -> float
