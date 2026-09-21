(* A ragdoll: a body of boxes held together by joints
 * (physics/3d/Joint3d.mli), which falls like a body because it is
 * built like one -- the observation that made Half-Life 2 (Valve, 2004,
 * on Havok) feel like nothing before it (notes_3d_physics.md section
 * 13).
 *
 *            [head]            neck: ball, a 40 degree cone
 *     [ua][ torso  ][ua]       shoulders: ball, 80 degrees
 *     [fa]            [fa]     elbows: hinges, bending 0 to 140 forward
 *         [th]  [th]           hips: ball, 70 degrees
 *         [sh]  [sh]           knees: hinges, bending 0 to 140 back
 *
 * Ten bodies and nine joints, in metres and kilograms (a person of
 * about 70 kg, 1.7 m tall), standing on its feet at the point given,
 * facing -z. The limits are what make it a body rather than a string of
 * sausages: without them a knee bends backwards and an arm goes round
 * the shoulder. *)

open Playground3d

(* how many bodies a ragdoll is *)
val count : int

(* [bodies ?color (x, y, z)]: the ten, standing with their feet at
 * (x, y, z), in the order [join] expects *)
val bodies : ?color:Playground.color -> number * number * number -> Physics3d.body list

(* [join ?limits first w]: the nine joints, for a ragdoll whose bodies
 * are the world's [first] to [first + count - 1], standing as [bodies]
 * made them; [limits] (true): the cones and the hinges' limits *)
val join : ?limits:bool -> int -> Physics3d.world -> Physics3d.world
