(* A contact: how two overlapping hitboxes touch, what collision
 * response needs (see notes_2d_physics.md sections 8 and 10):
 *
 *            normal
 *      .----->
 *     (  a  (/)  b  )       the direction to push b away from a (a
 *      '---'  depth         unit vector), how far they overlap along
 *                           it, and a point where they touch
 *
 * Example: circles of radius 30 at (0, 0) and 25 at (30, 40): their
 * centers are 50 apart, 5 less than 30 + 25, so depth 5, and normal
 * (0.6, 0.8), from the first center to the second. *)

type t = {
  normal : Vec2.t; (* unit, from the first hitbox to the second *)
  depth : float; (* > 0 *)
  point : Vec2.t; (* where they touch (roughly, for polygons) *)
}
