(* The player of games3d/Minecraft3d: where they are, where they look,
 * and how they move -- walking, jumping, falling, flying, and bumping
 * into blocks. A port of the original's Window methods
 * (~/software-src/game/tiny-minecraft/main.py: get_sight_vector,
 * get_motion_vector, _update, collide), same constants, same
 * behavior. Independent of the Playground, like Minecraft_model: the
 * inputs are plain values, so it can be tested standalone
 * (Test_minecraft_model.exe).
 *
 * Conventions, the original's (OpenGL's): y is up; yaw 0 looks along
 * -z, and a positive yaw turns right (towards +x); pitch is in
 * -90..90, positive looking up.
 *
 *              -z (yaw 0)
 *               ^
 *               |
 *   -x  <-------+------->  +x (yaw 90)
 *               |
 *              +z (yaw 180)
 *
 * [position] is the player's eyes; their body is [height] = 2 blocks
 * tall, from the eyes down (see [collide]). *)

type t = {
  position : float * float * float;
  (* in degrees, see above *)
  yaw : float;
  pitch : float;
  (* vertical speed, blocks per second (0 when standing on something) *)
  dy : float;
  flying : bool;
}

(* at (0, 0, 0), looking along -z, standing on the ground (which is at
 * y = -2, so the feet are just above it), not flying *)
val initial : t

(* What the player wants to do this frame, from the keys:
 * [forward] 1 forward (W), -1 back (S), 0 neither; [right] 1 right (D),
 * -1 left (A); [jump] (space) *)
type input = { forward : int; right : int; jump : bool }

(* the unit vector the player looks along (for the camera, and for
 * Minecraft_model.hit_test) *)
val sight_vector : t -> float * float * float

(* [step world ~dt input player]: the player [dt] seconds later: moved
 * according to [input] (walking in the horizontal plane; or, flying,
 * along the sight vector), pulled down by gravity unless flying, and
 * pushed out of the blocks of [world] it would otherwise enter. Like
 * the original, done in 8 small substeps, so that a fast fall can't
 * jump over a block in one step. *)
val step : Minecraft_model.t -> dt:float -> input -> t -> t

(* [collide world player]: [player], pushed out of the blocks of
 * [world] its body overlaps (by more than a quarter of a block), with
 * [dy] set to 0 when hitting the ground or a ceiling. Exposed for the
 * tests. *)
val collide : Minecraft_model.t -> t -> t
