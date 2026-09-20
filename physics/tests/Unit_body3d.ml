(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/3d/Body3d and physics/3d/Energy3d: the worked examples of
 * their .mlis -- the tensors of the usual shapes, what happens to one
 * when the body turns, and the quantities that referee a simulation *)

let t = Testo.create
let close = Alcotest.(check (float 1e-9))

let check_vec name (ax, ay, az) (bx, by, bz) =
  Alcotest.(check (float 1e-9)) (name ^ " x") ax bx;
  Alcotest.(check (float 1e-9)) (name ^ " y") ay by;
  Alcotest.(check (float 1e-9)) (name ^ " z") az bz

(* Body3d.mli: m/12 diag (h^2+d^2, w^2+d^2, w^2+h^2), and 2/5 m r^2 *)
let tensors () =
  let i = Body3d.box ~mass:12. (1., 1., 1.) in
  close "a cube of mass 12, side 1: 2 about every axis" 2. i.Mat3.m00;
  close "and the same about y" 2. i.Mat3.m11;
  close "no products of inertia: its axes are its principal ones" 0. i.Mat3.m01;
  (* a long thin bar is easy to spin about its length, hard across it *)
  let bar = Body3d.box ~mass:1. (2., 0.1, 0.1) in
  Alcotest.(check bool) "a bar spins easily about its own length" true (bar.Mat3.m00 < bar.Mat3.m11 /. 100.);
  close "and the same across either way" bar.Mat3.m11 bar.Mat3.m22;
  let s = Body3d.solid_sphere ~mass:1. ~radius:1. in
  close "a unit sphere: 2/5" 0.4 s.Mat3.m00;
  close "the same about every axis: a sphere does not care" 0.4 s.Mat3.m22

(* the parallel-axis theorem: a part away from the centre is harder to
 * turn, by m r^2, and not at all about the axis it sits on *)
let parallel_axis () =
  let i = Body3d.box ~mass:2. (0.2, 0.2, 0.2) in
  let shifted = Body3d.shifted ~mass:2. (0., 3., 0.) i in
  close "about y, the axis it was moved along: unchanged" i.Mat3.m11 shifted.Mat3.m11;
  close "about x: heavier by m r^2 = 18" (i.Mat3.m00 +. 18.) shifted.Mat3.m00;
  close "about z: the same" (i.Mat3.m22 +. 18.) shifted.Mat3.m22;
  close "still diagonal, the offset being along an axis" 0. shifted.Mat3.m02

(* the tensor in the world frame, R I R^T, and the two infinities *)
let world_tensor () =
  let b = Body3d.make ~inertia:(Mat3.diagonal 1. 2. 3.) (0., 0., 0.) in
  let turned = { b with Body3d.orientation = Quat.of_axis_angle (0., 0., 1.) (Float.pi /. 2.) } in
  let w = Body3d.inertia_world turned in
  close "a quarter turn about z swaps the x and y moments" 2. w.Mat3.m00;
  close "and back" 1. w.Mat3.m11;
  close "z was the axis of the turn: unchanged" 3. w.Mat3.m22;
  let inv = Body3d.inv_inertia_world turned in
  close "and the inverse follows" 0.5 inv.Mat3.m00;
  let never = Body3d.make (0., 0., 0.) in
  Alcotest.(check bool) "by default a body never turns: an infinite tensor" true (never.Body3d.inertia = Body3d.never_turns);
  Alcotest.(check bool) "which is read as a zero inverse, not an infinity" true (never.Body3d.inv_inertia = Mat3.zero)

(* Body3d.mli's example: a wheel spinning at 2 rad/s about +y *)
let point_velocity () =
  let b = Body3d.make ~spin:(0., 2., 0.) ~inertia:(Body3d.solid_sphere ~mass:1. ~radius:1.) (0., 0., 0.) in
  check_vec "the point at +x moves towards -z" (0., 0., -2.) (Body3d.point_velocity b (1., 0., 0.));
  check_vec "the point on the axis does not move" (0., 0., 0.) (Body3d.point_velocity b (0., 1., 0.));
  let carried = { b with Body3d.vel = (5., 0., 0.) } in
  check_vec "and a moving wheel carries its points along" (5., 0., -2.) (Body3d.point_velocity carried (1., 0., 0.))

(* Energy3d.mli's examples *)
let energies () =
  let b = Body3d.make ~vel:(3., 0., 0.) ~mass:2. (0., 0., 0.) in
  close "a mass of 2 at speed 3: 9" 9. (Energy3d.kinetic b);
  check_vec "and a momentum of 6" (6., 0., 0.) (Energy3d.momentum b);
  let ball = Body3d.make ~spin:(0., 10., 0.) ~inertia:(Body3d.solid_sphere ~mass:1. ~radius:1.) (0., 0., 0.) in
  close "a unit sphere at 10 rad/s: 0.4 * 100 / 2" 20. (Energy3d.kinetic ball);
  close "|L| = 0.4 * 10" 4. (Vec3.length (Energy3d.angular ball));
  (* a body flying past a point has angular momentum about it *)
  let flying = Body3d.make ~vel:(3., 0., 0.) ~mass:1. (0., 2., 0.) in
  check_vec "r x m v, about the origin" (0., 0., -6.) (Energy3d.angular_momentum ~around:(0., 0., 0.) flying);
  close "and m g y" 19.6 (Energy3d.gravity ~g:9.8 (Body3d.make ~mass:2. (0., 1., 0.)));
  (* the infinities must not turn into nans *)
  let floor = Body3d.make ~mass:infinity (0., -1., 0.) in
  close "an immovable floor has no kinetic energy, not a nan" 0. (Energy3d.kinetic floor);
  check_vec "nor a momentum" (0., 0., 0.) (Energy3d.momentum floor)

let tests =
  [ t "Body3d, the tensors of a box and a sphere" tensors;
    t "Body3d, the parallel-axis theorem" parallel_axis;
    t "Body3d, the tensor in the world frame, and the two infinities" world_tensor;
    t "Body3d, the velocity of a point on a spinning body" point_velocity;
    t "Energy3d, the worked examples, and no nan from an infinity" energies ]
