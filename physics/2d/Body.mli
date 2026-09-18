(* A body's state: where it is, how fast it goes, how heavy it is --
 * everything a step of the simulation needs to know about it, and
 * everything it changes (see notes_2d_physics.md sections 2-3).
 *
 *              vel (a velocity: how the position changes per second)
 *             ^
 *            /
 *       pos o      mass: how hard it is to change vel (F = m a)
 *
 * Units are the caller's; the playground's are pixels, seconds, and
 * an arbitrary mass unit (only ratios of masses matter).
 *
 * (Rotation, an angle and a spin, comes with the rigid bodies, later
 * in the plan.) *)

type t = {
  (* position *)
  pos : Vec2.t;
  (* velocity, per second *)
  vel : Vec2.t;
  (* > 0 *)
  mass : float;
}

(* [make ?vel ?mass pos]: a body at [pos], still ((0, 0)) and of mass 1
 * by default *)
val make : ?vel:Vec2.t -> ?mass:float -> Vec2.t -> t
