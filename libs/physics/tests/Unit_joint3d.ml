(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Joint3d, through Physics3d's world: each kind of joint
 * against what it should do, measured *)

open Playground
open Playground3d

let t = Testo.create

let run ?(gravity = 9.8) (steps : int) (f : int -> Physics3d.world -> unit) (w : Physics3d.world) : Physics3d.world =
  let w = ref w in
  for i = 1 to steps do
    w := Physics3d.simulate ~gravity ~sleeping:false !w;
    f i !w
  done;
  !w

let anchor_gap (w : Physics3d.world) (i : int) : number =
  let s = Array.of_list (List.map (fun (b : Physics3d.body) ->
    Body3d.make ~vel:(b.vx, b.vy, b.vz) ~orientation:b.orientation ~mass:b.mass (b.x, b.y, b.z)) w.bodies) in
  let pa, pb = Joint3d.anchors s (List.nth w.joints i) in
  Vec3.length (Vec3.sub pa pb)

let pivot = Physics3d.body (cube white 0.1) |> Physics3d.at 0. 2. 0. |> Physics3d.immovable

(* A pendulum, a 1 m rod from a fixed pivot, let go at 10 degrees: the
 * rod stays a metre long, and it swings with the period of the
 * textbook, 2 pi sqrt(L / g) = 2.007 s (0.2% more at 10 degrees) *)
let pendulum () =
  let a = 10. *. Float.pi /. 180. in
  let bx = sin a and by = 2. -. cos a in
  let bob = Physics3d.body (sphere white 0.05) |> Physics3d.ball |> Physics3d.at bx by 0. in
  let w = Physics3d.world [ pivot; bob ] |> Physics3d.rod 0 1 ~at_a:(0., 2., 0.) ~at_b:(bx, by, 0.) in
  let worst = ref 0. and crossings = ref [] and last_x = ref bx in
  ignore
    (run 360
       (fun i w ->
         let b = List.nth w.Physics3d.bodies 1 in
         worst := Float.max !worst (Float.abs (Float.hypot b.x (b.y -. 2.) -. 1.));
         if !last_x > 0. && b.x <= 0. then crossings := float_of_int i /. 60. :: !crossings;
         last_x := b.x)
       w);
  Alcotest.(check bool) (Printf.sprintf "the rod holds its length (%.4f m off at most)" !worst) true (!worst < 0.005);
  match !crossings with
  | later :: earlier :: _ ->
      let period = later -. earlier in
      Alcotest.(check bool) (Printf.sprintf "the period is the textbook's (%.3f s)" period) true (Float.abs (period -. 2.007) < 0.04)
  | _ -> Alcotest.fail "it swings"

(* a door, 1 m wide and 2 m tall, hung on a frame at its left edge *)
let door ?limits () =
  let frame = Physics3d.body (box white 0.1 2. 0.1) |> Physics3d.at (-0.05) 1. 0. |> Physics3d.immovable in
  let door = Physics3d.body (box white 1. 2. 0.05) |> Physics3d.at 0.5 1. 0. |> Physics3d.heavy 20. in
  Physics3d.world [ frame; door ] |> Physics3d.hinge ?limits 0 1 ~at:(0., 1., 0.) ~axis:(0., 1., 0.)

let upright (w : Physics3d.world) : number =
  let _, uy, _ = Quat.rotate (List.nth w.Physics3d.bodies 1).orientation (0., 1., 0.) in
  uy

(* The door, set turning about its own middle at 60 degrees a second,
 * turns about its hinge and nothing else: the hinge's two anchors stay
 * together, and the door stays upright under its own weight. And it
 * turns at a quarter of that speed: the hinge keeps its angular
 * momentum *about the hinge*, and a door is four times harder to turn
 * about its edge than about its middle (m w^2 (1/12 + 1/4) against
 * m w^2 / 12) -- 15 degrees a second, 30 in two seconds *)
let a_door () =
  let w = door () in
  let w = { w with bodies = [ List.hd w.bodies; List.nth w.bodies 1 |> Physics3d.turning (0., 1., 0.) 60. ] } in
  let worst = ref 0. in
  let w = run 120 (fun _ w -> worst := Float.max !worst (anchor_gap w 0)) w in
  Alcotest.(check bool) (Printf.sprintf "the hinge holds (%.4f m apart at most)" !worst) true (!worst < 0.01);
  Alcotest.(check bool) (Printf.sprintf "the door stays upright (%.4f)" (upright w)) true (upright w > 0.999);
  let angle = Physics3d.joint_angle 0 w in
  Alcotest.(check bool) (Printf.sprintf "a quarter as fast: 30 degrees in 2 s (%.1f)" angle) true (Float.abs (Float.abs angle -. 30.) < 1.5)

(* With limits of 0 to 90 degrees, pushed at 180 degrees a second, it
 * stops at 90 and does not go through *)
let limits () =
  let w = door ~limits:(-90., 0.) () in
  let w = { w with bodies = [ List.hd w.bodies; List.nth w.bodies 1 |> Physics3d.turning (0., 1., 0.) (-180.) ] } in
  let most = ref 0. in
  ignore (run ~gravity:0. 120 (fun _ w -> most := Float.min !most (Physics3d.joint_angle 0 w)) w);
  Alcotest.(check bool) (Printf.sprintf "stopped at the limit (%.1f degrees)" !most) true (!most > -93. && !most < -85.)

(* A wheel on an axle, driven by a motor at 360 degrees a second: after
 * a second it turns at that speed *)
let motor () =
  let wheel = Physics3d.body (box white 0.1 0.6 0.6) |> Physics3d.at 0. 2. 0. |> Physics3d.heavy 5. in
  let w = Physics3d.world [ pivot; wheel ] |> Physics3d.hinge ~motor:(360., 50.) 0 1 ~at:(0., 2., 0.) ~axis:(1., 0., 0.) in
  let w = run ~gravity:0. 60 (fun _ _ -> ()) w in
  let sx, _, _ = (List.nth w.Physics3d.bodies 1).spin in
  Alcotest.(check bool) (Printf.sprintf "at the motor's speed (%.1f degrees a second)" sx) true (Float.abs (sx -. 360.) < 10.)

(* A stick hanging from a ball-and-socket, allowed 30 degrees from
 * straight down, spun hard: it swings out to the cone and no further *)
let cone () =
  let stick = Physics3d.body (box white 0.05 0.5 0.05) |> Physics3d.at 0. 1.75 0. |> Physics3d.turning (0., 0., 1.) 400. in
  let w = Physics3d.world [ pivot; stick ] |> Physics3d.ball_joint ~cone:((0., -1., 0.), 30.) 0 1 ~at:(0., 2., 0.) in
  let most = ref 0. in
  ignore
    (run ~gravity:0. 90
       (fun _ w ->
         let _, uy, _ = Quat.rotate (List.nth w.Physics3d.bodies 1).orientation (0., 1., 0.) in
         most := Float.max !most (Float.acos (Float.min 1. uy) *. 180. /. Float.pi))
       w);
  Alcotest.(check bool) (Printf.sprintf "within the cone (%.1f degrees at most)" !most) true (!most < 33. && !most > 25.)

(* A ragdoll (Ragdoll3d, ten bodies, nine joints) shoved
 * down a flight of stairs: every joint holds all the way down, and at
 * the bottom it comes to rest, its joints together again. Not rigidly:
 * at the hardest landing an elbow opens by about 5 cm for a few frames,
 * the arm caught between a step and the body falling on it, contact and
 * joint each correcting by Baumgarte's slow speed. More iterations do
 * not help (measured: 4.5 cm at 10, 5.4 at 20, 3.4 at 40), nor warm
 * starting the joints (4.5 against 4.6); correcting the positions
 * themselves would (Catto's non-linear Gauss-Seidel, as Box2D) -- the
 * exercise *)
let a_ragdoll () =
  let stairs =
    List.init 6 (fun i ->
        let top = 1.2 -. (0.2 *. float_of_int i) in
        Physics3d.body (box white 0.35 top 2.) |> Physics3d.at ((0.35 *. float_of_int i) +. 0.175) (top /. 2.) 0.
        |> Physics3d.immovable |> Physics3d.rough 0.7)
  in
  let ground = Physics3d.body (box white 12. 0.2 6.) |> Physics3d.at 2. (-0.1) 0. |> Physics3d.immovable |> Physics3d.rough 0.7 in
  let landing = Physics3d.body (box white 1.2 1.2 2.) |> Physics3d.at (-0.6) 0.6 0. |> Physics3d.immovable |> Physics3d.rough 0.7 in
  let first = 8 in
  let w = Physics3d.world ((ground :: landing :: stairs) @ Ragdoll3d.bodies (-0.3, 1.2, 0.)) |> Ragdoll3d.join first in
  let w = { w with bodies = List.mapi (fun i (b : Physics3d.body) -> if i = first then Physics3d.moving 2. 0. 0.3 b else b) w.bodies } in
  let worst = ref 0. in
  let w = run 300 (fun _ w -> List.iteri (fun i _ -> worst := Float.max !worst (anchor_gap w i)) w.joints) w in
  let fastest = List.fold_left (fun m b -> Float.max m (Physics3d.speed b)) 0. w.bodies in
  let resting = List.fold_left Float.max 0. (List.mapi (fun i _ -> anchor_gap w i) w.joints) in
  Alcotest.(check bool) (Printf.sprintf "every joint holds (%.3f m apart at worst)" !worst) true (!worst < 0.06);
  Alcotest.(check bool) (Printf.sprintf "at the bottom it rests (%.3f m/s)" fastest) true (fastest < 0.2);
  Alcotest.(check bool) (Printf.sprintf "its joints together again (%.4f m)" resting) true (resting < 0.005)

let tests =
  [ t "Joint3d, a pendulum on a rod" pendulum;
    t "Joint3d, a door on a hinge" a_door;
    t "Joint3d, a hinge's limits" limits;
    t "Joint3d, a hinge's motor" motor;
    t "Joint3d, a ball-and-socket's cone" cone;
    t "Joint3d, a ragdoll down the stairs" a_ragdoll ]
