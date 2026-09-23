(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Sweep3d, continuous collision, and Physics3d.simulate's
 * ~continuous: a pinball kept on the table, and lost without it *)

open Playground
open Playground3d

let t = Testo.create

(* a pinball: 27 mm across *)
let r = 0.0135

(* a 1 cm wall across x, its faces at 0.295 and 0.305 *)
let wall_box = Hitbox3d.place (0.3, 0., 0.) (Hitbox3d.Box (0.005, 0.2, 0.2))

(* The worked example: a sphere of radius 0.5 dropped 2 m onto the
 * ground from 1 m up touches it when its middle is 0.5 up, a quarter of
 * the way down -- and the sweep stops a millimetre into it *)
let onto_a_plane () =
  let ground = Hitbox3d.place (0., 0., 0.) (Hitbox3d.Plane ((0., 1., 0.), 0.)) in
  match Sweep3d.sphere ~radius:0.5 ~from:(0., 1., 0.) ~motion:(0., -2., 0.) ground with
  | Some t -> Alcotest.(check (float 1e-6)) "a quarter of the way, and a millimetre" (0.25 +. (0.001 /. 2.)) t
  | None -> Alcotest.fail "it lands"

(* A pinball at 10 m/s, 16.7 cm in a step, meets the 1 cm wall halfway
 * through the step -- and at the end of the step it is past it,
 * touching nothing: a test of where it is misses the wall entirely *)
let a_thin_wall () =
  let motion = (10. /. 60., 0., 0.) in
  (match Sweep3d.sphere ~radius:r ~from:(0.2, 0., 0.) ~motion wall_box with
  | Some t -> Alcotest.(check (float 1e-3)) "where it touches" ((0.295 -. r -. 0.2 +. 0.001) /. (10. /. 60.)) t
  | None -> Alcotest.fail "the path meets the wall");
  let after = Hitbox3d.place (0.2 +. (10. /. 60.), 0., 0.) (Hitbox3d.Sphere r) in
  Alcotest.(check bool) "at the end of the step, touching nothing" false (Collide3d.touching wall_box after);
  let resting = Hitbox3d.place (0.295 -. r, 0., 0.) (Hitbox3d.Sphere r) in
  ignore resting;
  Alcotest.(check bool) "touching at the start: the solver's, not the sweep's" true
    (Sweep3d.sphere ~radius:r ~from:(0.295 -. r +. 0.00001, 0., 0.) ~motion wall_box = None)

(* The obstacle can be the fast one: a paddle 60 cm long turning at
 * 10 radians a second swings its point 20 cm out by 3.3 cm a step, and
 * meets a ball at rest 6.5 mm in front of it; standing still, it never
 * does *)
let a_turning_paddle () =
  let paddle = Hitbox3d.place (0., 0., 0.) (Hitbox3d.Box (0.3, 0.01, 0.01)) in
  let ball = (0.2, 0., -.(0.01 +. 0.0065 +. r)) in
  let turn = (0., 10. /. 60., 0.) in
  (match Sweep3d.sphere ~radius:r ~from:ball ~motion:(0., 0., 0.) ~moving:((0., 0., 0.), turn) paddle with
  | Some t -> Alcotest.(check bool) (Printf.sprintf "met, a fraction into the step (%.2f)" t) true (t > 0. && t < 1.)
  | None -> Alcotest.fail "the paddle meets the ball");
  Alcotest.(check bool) "not turning, it never does" true
    (Sweep3d.sphere ~radius:r ~from:ball ~motion:(0., 0., 0.) paddle = None)

(* the ball at [speed] at the wall, 30 ticks, no gravity: where it ends *)
let shot ?(continuous = false) ?(substeps = 1) (speed : number) : Physics3d.body =
  let wall = Physics3d.body (box white 0.01 0.4 0.4) |> Physics3d.at 0.3 0. 0. |> Physics3d.immovable |> Physics3d.bouncy 0.5 in
  let ball = Physics3d.body (sphere white r) |> Physics3d.ball |> Physics3d.heavy 0.08 |> Physics3d.moving speed 0. 0. in
  let w = ref (Physics3d.world [ wall; ball ]) in
  for _ = 1 to 30 do w := Physics3d.simulate ~continuous ~substeps !w done;
  List.nth !w.Physics3d.bodies 1

(* The plan's measured switch: at 10 m/s the pinball is kept by the
 * sweep, bounced back off the wall, and without it goes through *)
let kept_or_lost () =
  let lost speed ?continuous ?substeps () = (shot ?continuous ?substeps speed).x > 0.3 in
  let first ?continuous ?substeps () =
    List.find_opt (fun s -> lost s ?continuous ?substeps ()) (List.init 40 (fun i -> 0.25 *. float_of_int (i + 1)))
  in
  (* measured: lost from 1.5 m/s in plain steps, from 6 m/s in four
   * substeps, and never with the sweep (tried up to 10) *)
  Alcotest.(check (option (float 1e-9))) "plain steps: lost from 1.5 m/s" (Some 1.5) (first ());
  Alcotest.(check (option (float 1e-9))) "four substeps: from 6 m/s" (Some 6.) (first ~substeps:4 ());
  Alcotest.(check (option (float 1e-9))) "the sweep: never" None (first ~continuous:true ());
  Alcotest.(check bool) "without the sweep, through the wall at 10 m/s" true (lost 10. ());
  let b = shot ~continuous:true 10. in
  Alcotest.(check bool) "with it, kept" true (b.x < 0.3);
  Alcotest.(check bool) "and bounced back" true (b.vx < 0.)

(* A flipper is a wall that moves: the paddle, turning at 10 radians a
 * second and immovable (kinematic), meets the ball at rest 20 cm from
 * its pivot and throws it at its surface's speed there, 2 m/s, in the
 * step it meets it. (Left to go on turning, it catches the ball again
 * farther out and throws it harder: 3.4 m/s after six steps -- a
 * flipper carries the ball rather than batting it, as
 * TinyPinball.ml's header says.) Without the sweep, the paddle's
 * face goes past the ball between two steps and throws nothing *)
let a_kinematic_paddle () =
  let throw continuous steps =
    let paddle =
      Physics3d.body (box white 0.6 0.02 0.02) |> Physics3d.immovable |> Physics3d.turning (0., 1., 0.) (10. *. 180. /. Float.pi)
    in
    let ball = Physics3d.body (sphere white r) |> Physics3d.ball |> Physics3d.heavy 0.08 |> Physics3d.at 0.2 0. (-.(0.01 +. 0.0065 +. r)) in
    let w = ref (Physics3d.world [ paddle; ball ]) in
    for _ = 1 to steps do w := Physics3d.simulate ~continuous !w done;
    Physics3d.speed (List.nth !w.Physics3d.bodies 1)
  in
  let fast = throw true 1 and none = throw false 6 in
  Alcotest.(check bool) (Printf.sprintf "thrown at the surface's speed (%.2f m/s)" fast) true (fast > 1.8 && fast < 2.4);
  Alcotest.(check bool) (Printf.sprintf "without the sweep, not thrown (%.2f m/s)" none) true (none < 0.5)

let tests =
  [ t "Sweep3d, onto a plane" onto_a_plane;
    t "Sweep3d, a thin wall met halfway through a step" a_thin_wall;
    t "Sweep3d, a turning paddle meets a ball at rest" a_turning_paddle;
    t "Physics3d, continuous: the pinball kept, and lost without" kept_or_lost;
    t "Physics3d, a kinematic paddle throws the ball" a_kinematic_paddle ]
