(* Joints: what keeps two bodies together (see notes_3d_physics.md
 * section 13).
 *
 * A joint takes away some of the ways two bodies can move against each
 * other, and it is solved in the very loop that solves the contacts
 * (Solver3d): a contact is a constraint too, one that may only push.
 * Each joint becomes a few *rows*, each a direction and a speed to
 * reach along it:
 *
 *   ball-and-socket   the two anchor points kept together: 3 rows, one
 *                     per axis, on the points' relative velocity
 *                     (a shoulder, a hip)
 *   hinge             the same 3, and 2 more on the *turning*: the two
 *                     bodies' hinge axes kept lined up, so the only
 *                     turn left is about the axis (a door, a seesaw)
 *   distance          1 row along the line between the anchors: a rod
 *
 * and a hinge may have a motor (a row along its axis at the speed asked,
 * with a torque it cannot exceed) and limits (a row that only pushes,
 * once the angle is past them), and a ball-and-socket a cone (the same,
 * on the angle between two axes: a shoulder that does not go all the
 * way round). Solved by sequential impulses, exactly as a contact is:
 * each row's impulse corrected towards its speed, over and over with
 * the others, its running total clamped when the row may only push.
 * What has drifted apart is brought back by Baumgarte's term, a speed
 * towards closing the gap, as an overlap is.
 *
 *              anchor         the two anchors, one in each body's own
 *         a ----*    *---- b  frame, are the same point of the world
 *                             when the joint holds, and pulled back
 *                             together when it does not
 *
 * Joints name their bodies by their place in the array the solver is
 * given, as the contact pairs do.
 *
 * References: Erin Catto, "Iterative Dynamics with Temporal Coherence"
 * (GDC 2005) and "Modeling and Solving Constraints" (GDC 2009) for the
 * rows and their effective masses; the ragdoll's cone, as in
 * Box2D's and Bullet's cone twist. *)

type kind =
  | Ball of { cone : (Vec3.t * Vec3.t * float) option }
      (* an axis in each body's frame, and the most angle between them, in radians *)
  | Hinge of {
      axis_a : Vec3.t; (* the hinge's axis, in a's frame *)
      axis_b : Vec3.t; (* and in b's *)
      ref_a : Vec3.t; (* a direction across the axis, in a's frame, from which the angle is measured *)
      ref_b : Vec3.t; (* the same direction, in b's frame, when the angle is 0 *)
      limits : (float * float) option; (* the least and the most angle, radians *)
      motor : (float * float) option; (* the speed asked, radians a second, and the most torque *)
    }
  | Distance of { length : float }

type t = {
  a : int;
  b : int;
  anchor_a : Vec3.t; (* in a's own frame *)
  anchor_b : Vec3.t; (* in b's *)
  kind : kind;
}

(*****************************************************************************)
(* {1 Making joints, from where the bodies are now} *)
(*****************************************************************************)

(* [ball bodies a b ~at ?cone ()]: a ball-and-socket at the world point
 * [at]; [cone], an axis in the world and the most angle (radians) the
 * two bodies may turn it apart *)
val ball : Body3d.t array -> int -> int -> at:Vec3.t -> ?cone:Vec3.t * float -> unit -> t

(* [hinge bodies a b ~at ~axis ?limits ?motor ()]: a hinge through [at]
 * about the world [axis]; its angle is 0 as the bodies are now *)
val hinge : Body3d.t array -> int -> int -> at:Vec3.t -> axis:Vec3.t -> ?limits:float * float -> ?motor:float * float -> unit -> t

(* [distance bodies a b ~at_a ~at_b ()]: a rod between those two world
 * points, as long as they are apart now *)
val distance : Body3d.t array -> int -> int -> at_a:Vec3.t -> at_b:Vec3.t -> unit -> t

(*****************************************************************************)
(* {1 What the solver does with them} *)
(*****************************************************************************)

(* one row: a direction, the speed asked along it, and the running
 * total of its impulse, clamped to [lo, hi] *)
type row

(* [rows ~beta ~dt bodies j]: [j]'s rows for this step, [beta] the
 * fraction of the drift corrected per step *)
val rows : beta:float -> dt:float -> Body3d.t array -> t -> row list

(* [solve_row bodies r]: one correction of the row's impulse, applied *)
val solve_row : Body3d.t array -> row -> unit

(*****************************************************************************)
(* {1 Measuring} *)
(*****************************************************************************)

(* where the joint's two anchors are in the world: the same point when
 * it holds *)
val anchors : Body3d.t array -> t -> Vec3.t * Vec3.t

(* a hinge's angle, radians (0 for the other joints) *)
val angle : Body3d.t array -> t -> float
