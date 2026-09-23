(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Quat and physics/3d/Mat3: the worked examples of their
 * .mlis, and the algebra a rotation has to obey *)

let t = Testo.create
let close = Alcotest.(check (float 1e-9))
let deg d = d *. Float.pi /. 180.

let check_vec name (ax, ay, az) (bx, by, bz) =
  Alcotest.(check (float 1e-9)) (name ^ " x") ax bx;
  Alcotest.(check (float 1e-9)) (name ^ " y") ay by;
  Alcotest.(check (float 1e-9)) (name ^ " z") az bz

(* a handful of orientations to try everything on *)
let samples =
  [ Quat.identity;
    Quat.of_axis_angle (0., 1., 0.) (deg 90.);
    Quat.of_axis_angle (1., 0., 0.) (deg 37.);
    Quat.of_axis_angle (1., 2., 3.) (deg 200.);
    Quat.of_axis_angle (-2., 0.5, 1.) (deg 89.9) ]

let vectors = [ (1., 0., 0.); (0., 1., 0.); (0., 0., 1.); (0.3, -0.7, 2.); (-1.5, 0.25, 0.75) ]

(* Quat.mli's example: a quarter turn about y, and what it does to the
 * x axis (right-handed, y up: x goes to -z) *)
let quarter_turn () =
  let q = Quat.of_axis_angle (0., 1., 0.) (deg 90.) in
  close "w = cos 45" (cos (deg 45.)) q.Quat.w;
  check_vec "v = sin 45 about y" (0., sin (deg 45.), 0.) q.Quat.v;
  check_vec "x turns to -z" (0., 0., -1.) (Quat.rotate q (1., 0., 0.));
  check_vec "y is the axis, it stays" (0., 1., 0.) (Quat.rotate q (0., 1., 0.));
  check_vec "z turns to x" (1., 0., 0.) (Quat.rotate q (0., 0., 1.))

(* the matrix and the sandwich are the same rotation *)
let matrix_agrees () =
  List.iter
    (fun q ->
      let m = Quat.to_mat3 q in
      List.iter (fun v -> check_vec "matrix = q v q^-1" (Quat.rotate q v) (Mat3.mul_vec m v)) vectors)
    samples

(* rotations compose, and do not commute: [mul a b] is b first *)
let composition () =
  let a = Quat.of_axis_angle (1., 0., 0.) (deg 90.) and b = Quat.of_axis_angle (0., 1., 0.) (deg 90.) in
  List.iter (fun v -> check_vec "mul a b is b then a" (Quat.rotate a (Quat.rotate b v)) (Quat.rotate (Quat.mul a b) v)) vectors;
  let turned_one_way = Quat.rotate (Quat.mul a b) (0., 0., 1.) in
  let turned_the_other = Quat.rotate (Quat.mul b a) (0., 0., 1.) in
  Alcotest.(check bool) "and the order matters" false (turned_one_way = turned_the_other)

(* a rotation matrix is orthogonal: R R^T = identity, det 1 *)
let rotation_matrices () =
  List.iter
    (fun q ->
      let m = Quat.to_mat3 q in
      let i = Mat3.mul m (Mat3.transpose m) in
      close "R R^T is the identity (00)" 1. i.Mat3.m00;
      close "R R^T is the identity (11)" 1. i.Mat3.m11;
      close "R R^T is the identity (22)" 1. i.Mat3.m22;
      close "R R^T is the identity (01)" 0. i.Mat3.m01;
      close "R R^T is the identity (20)" 0. i.Mat3.m20;
      close "det 1: no reflection, no stretch" 1. (Mat3.det m))
    samples

(* the axis and angle survive the round trip *)
let axis_angle_round_trip () =
  List.iter
    (fun q ->
      let axis, angle = Quat.to_axis_angle q in
      List.iter (fun v -> check_vec "of_axis_angle (to_axis_angle q) = q" (Quat.rotate q v) (Quat.rotate (Quat.of_axis_angle axis angle) v)) vectors)
    samples

(* to_euler_xyz feeds Playground3d.rotate3d, which turns about x, then
 * y, then z: rebuilding the rotation that way must give the same one *)
let euler_for_drawing () =
  List.iter
    (fun q ->
      let dx, dy, dz = Quat.to_euler_xyz q in
      let rebuilt =
        Quat.mul
          (Quat.of_axis_angle (0., 0., 1.) (deg dz))
          (Quat.mul (Quat.of_axis_angle (0., 1., 0.) (deg dy)) (Quat.of_axis_angle (1., 0., 0.) (deg dx)))
      in
      List.iter (fun v -> check_vec "x then y then z draws the same body" (Quat.rotate q v) (Quat.rotate rebuilt v)) vectors)
    samples

(* Mat3.mli's inverse, and the tensor's own trip: R I R^T *)
let matrix_inverse () =
  let m = Mat3.of_rows (1., 2., 3.) (0., 1., 4.) (5., 6., 0.) in
  (match Mat3.inverse m with
  | None -> Alcotest.fail "this one is invertible"
  | Some inv ->
      let i = Mat3.mul m inv in
      close "m m^-1 = identity (00)" 1. i.Mat3.m00;
      close "m m^-1 = identity (12)" 0. i.Mat3.m12);
  Alcotest.(check bool) "a singular matrix has no inverse" true (Mat3.inverse (Mat3.diagonal 1. 0. 1.) = None);
  (* a box on its side: turning it a quarter turn about z swaps the
   * tensor's x and y entries, which is the whole of I_world = R I R^T *)
  let i = Mat3.diagonal 1. 2. 3. in
  let r = Quat.to_mat3 (Quat.of_axis_angle (0., 0., 1.) (deg 90.)) in
  let w = Mat3.conjugate r i in
  close "the x moment is now the old y" 2. w.Mat3.m00;
  close "and the y moment the old x" 1. w.Mat3.m11;
  close "the z axis was the turn's own, so it keeps its moment" 3. w.Mat3.m22;
  close "still diagonal" 0. w.Mat3.m01

let tests =
  [ t "Quat, a quarter turn about y" quarter_turn;
    t "Quat, the matrix agrees with the sandwich" matrix_agrees;
    t "Quat, rotations compose and do not commute" composition;
    t "Quat, the matrices are rotations" rotation_matrices;
    t "Quat, axis and angle round trip" axis_angle_round_trip;
    t "Quat, the Euler angles the playground draws with" euler_for_drawing;
    t "Mat3, the inverse, and R I R^T" matrix_inverse ]
