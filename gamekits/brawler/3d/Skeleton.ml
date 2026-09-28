(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground
open Playground3d

(* See Skeleton.mli *)

type limb = { pitch : number; yaw : number; bend : number }

type pose = {
  lean : number;
  turn : number;
  front_arm : limb;
  back_arm : limb;
  front_leg : limb;
  back_leg : limb;
}

let limb ?(yaw = 0.) ?(bend = 0.) (pitch : number) : limb = { pitch; yaw; bend }

let stand : pose =
  { lean = 0.;
    turn = 0.;
    front_arm = limb ~yaw:8. 8.;
    back_arm = limb ~yaw:(-8.) 4.;
    front_leg = limb ~bend:6. 4.;
    back_leg = limb ~bend:6. (-4.) }

(*****************************************************************************)
(* Animating *)
(*****************************************************************************)

let mix (a : number) (b : number) (t : number) : number = a +. ((b -. a) *. t)

let lerp_limb (a : limb) (b : limb) (t : number) : limb =
  { pitch = mix a.pitch b.pitch t; yaw = mix a.yaw b.yaw t; bend = mix a.bend b.bend t }

let lerp (a : pose) (b : pose) (t : number) : pose =
  { lean = mix a.lean b.lean t;
    turn = mix a.turn b.turn t;
    front_arm = lerp_limb a.front_arm b.front_arm t;
    back_arm = lerp_limb a.back_arm b.back_arm t;
    front_leg = lerp_limb a.front_leg b.front_leg t;
    back_leg = lerp_limb a.back_leg b.back_leg t }

let at (keys : (int * pose) list) (frame : int) : pose =
  let rec go = function
    | [] -> stand
    | [ (_, p) ] -> p
    | (f1, p1) :: ((f2, p2) :: _ as rest) ->
        if frame <= f1 then p1
        else if frame >= f2 then go rest
        else lerp p1 p2 (float_of_int (frame - f1) /. float_of_int (f2 - f1))
  in
  go keys

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* Stickman's proportions, so that the two figures are the same
 * fighter: the legs half the height, the torso 0.3 of it, an arm 0.34,
 * and the head what is left. *)
let leg_of (h : number) = h *. 0.5
let torso_of (h : number) = h *. 0.3
let arm_of (h : number) = h *. 0.34
let head_of (h : number) = h *. 0.2
let thick_of (h : number) = h *. 0.085

(* A limb, hanging from the origin: the upper part reaches down
 * [upper], the lower part carries on from its end, bent by [bend].
 *
 * The lower part is rotated by the bend and *then* moved to the elbow,
 * which is what puts it in the upper part's frame; the pair is then
 * turned by the shoulder's own angles, and the lower part goes along
 * without knowing it. That is the whole of hierarchical transforms,
 * and the only interesting thing in this file. *)
let jointed ?held (color : color) (thick : number) (upper : number) (lower : number) (l : limb) : shape3d =
  let part (len : number) = box color thick len thick |> move_y3d (-.len /. 2.) in
  (* what the hand holds is one more level down: moved to the end of the
   * lower part, and from there carried by the elbow and the shoulder *)
  let held = match held with Some s -> [ s |> move_y3d (-.lower) ] | None -> [] in
  group3d [ part upper; group3d (part lower :: held) |> rotate3d l.bend 0. 0. |> move_y3d (-.upper) ]
  |> rotate3d l.pitch l.yaw 0.

(* Where the end of such a limb lands, the same two rotations done with
 * arithmetic instead of boxes. [rotate3d] turns about X first, which
 * swings a hanging limb towards -z, and then about Y, which is what
 * the yaw does -- so a pitch of 90 points the limb the way the figure
 * faces, and the two agree by construction. *)
let jointed_end (upper : number) (lower : number) (l : limb) : number * number * number =
  let radians (d : number) = d *. Float.pi /. 180. in
  let p = radians l.pitch and b = radians (l.pitch +. l.bend) and y = radians l.yaw in
  let forward = (upper *. sin p) +. (lower *. sin b) in
  let down = (upper *. cos p) +. (lower *. cos b) in
  (-.forward *. sin y, -.down, -.forward *. cos y)

(* the figure, built facing -z (the playground's heading 0), its feet
 * on y = 0 *)
let figure ?front_hand ?back_hand ~(body : color) ~(back : color) ~(skin : color) (h : number) (p : pose) : shape3d =
  let leg = leg_of h and torso = torso_of h and arm = arm_of h and head = head_of h in
  let thick = thick_of h in
  let side ?held (l : limb) (color : color) (upper : number) (lower : number) (at_y : number) (across : number) =
    jointed ?held color (thick *. 0.95) upper lower l |> move3d (across *. thick *. 0.9) at_y 0.
  in
  let trunk =
    group3d
      [ box body (thick *. 2.4) torso (thick *. 1.6) |> move_y3d (torso /. 2.);
        box skin (head *. 0.8) head (head *. 0.8) |> move_y3d (torso +. (head /. 2.));
        (* the nose says which way it faces, which boxes otherwise do not *)
        box skin (head *. 0.3) (head *. 0.25) (head *. 0.3)
        |> move3d 0. (torso +. (head /. 2.)) (-.head *. 0.5);
        side ?held:back_hand p.back_arm back (arm *. 0.5) (arm *. 0.5) (torso -. (thick *. 0.3)) (-1.);
        side ?held:front_hand p.front_arm body (arm *. 0.5) (arm *. 0.5) (torso -. (thick *. 0.3)) 1. ]
    (* the lean and the twist carry the arms and the head with them,
     * because they are inside the group before it turns *)
    |> rotate3d p.lean p.turn 0.
    |> move_y3d leg
  in
  group3d
    [ trunk;
      side p.back_leg back (leg *. 0.5) (leg *. 0.5) leg (-1.);
      side p.front_leg body (leg *. 0.5) (leg *. 0.5) leg 1. ]

let draw ?front_hand ?back_hand ~(body : color) ~(back : color) ~(skin : color) (h : number) (heading : number)
    (p : pose) : shape3d =
  figure ?front_hand ?back_hand ~body ~back ~skin h p |> rotate3d 0. (-.heading) 0.

(* the same rotation, by hand: [rotate3d 0 (-heading) 0] takes (x, z)
 * to (x cos h + z sin h, -x sin h + z cos h) *)
let turned (heading : number) ((x, y, z) : number * number * number) : number * number * number =
  let a = -.heading *. Float.pi /. 180. in
  ((x *. cos a) +. (z *. sin a), y, (-.x *. sin a) +. (z *. cos a))

let hand (h : number) (heading : number) (p : pose) : number * number * number =
  let arm = arm_of h in
  let x, y, z = jointed_end (arm *. 0.5) (arm *. 0.5) p.front_arm in
  let shoulder = leg_of h +. torso_of h -. (thick_of h *. 0.3) in
  turned heading (x +. (thick_of h *. 0.9), shoulder +. y, z)

let foot (h : number) (heading : number) (p : pose) : number * number * number =
  let leg = leg_of h in
  let x, y, z = jointed_end (leg *. 0.5) (leg *. 0.5) p.front_leg in
  turned heading (x +. (thick_of h *. 0.9), leg +. y, z)
