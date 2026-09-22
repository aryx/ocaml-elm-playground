(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Logo3d: the turtle's frame, turned, pitched, rolled *)

open Logo3d

let t = Testo.create

(* the turtle, the last shape: a pyramid, its tip 14 ahead of its
 * position, its base's center 5 behind; its position and heading *)
let turtle (shapes : Playground3d.shape3d list) : (float * float * float) * (float * float * float) =
  let s : Playground3d.shape3d = List.nth shapes (List.length shapes - 1) in
  match s.form with
  | Group3d ({ form = Polygon3d (_, [ b0; b1; tip ]); _ } :: _ :: _ :: _ :: [ { form = Polygon3d (_, [ b3; b2; _; _ ]); _ } ]) ->
      let avg4 (x0, y0, z0) (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) =
        ((x0 +. x1 +. x2 +. x3) /. 4., (y0 +. y1 +. y2 +. y3) /. 4., (z0 +. z1 +. z2 +. z3) /. 4.)
      in
      let (bx, by, bz) = avg4 b0 b1 b2 b3 and (tx, ty, tz) = tip in
      ( ((5. *. tx +. 14. *. bx) /. 19., (5. *. ty +. 14. *. by) /. 19., (5. *. tz +. 14. *. bz) /. 19.),
        ((tx -. bx) /. 19., (ty -. by) /. 19., (tz -. bz) /. 19.) )
  | _ -> Alcotest.fail "the turtle: not a pyramid"

let check_turtle msg ((x, y, z), (hx, hy, hz)) shapes =
  let ((x', y', z'), (hx', hy', hz')) = turtle shapes in
  let close a b = Float.abs (a -. b) < 1e-6 in
  if not (close x x' && close y y' && close z z' && close hx hx' && close hy hy' && close hz hz') then
    Alcotest.failf "%s: the turtle at (%g, %g, %g) heading (%g, %g, %g)" msg x' y' z' hx' hy' hz'

let home_up = ((0., 0., 0.), (0., 1., 0.))

let test_square () =
  let square = [ repeat 4 [ forward 100.; right 90. ] ] in
  let shapes = draw square in
  Alcotest.(check int) "4 tubes, and the turtle" 5 (List.length shapes);
  check_turtle "back home, heading up" home_up shapes;
  Alcotest.(check (float 1e-9)) "the same work as in 2D" 490. (work square)

(* up is towards the viewer (+z), left towards -x, as in Logo3d.mli *)
let test_frame () =
  check_turtle "right 90: heading +x" ((100., 0., 0.), (1., 0., 0.)) (draw [ right 90.; forward 100. ]);
  check_turtle "up 90: climbing, heading +z" ((0., 0., 100.), (0., 0., 1.)) (draw [ up 90.; forward 100. ]);
  check_turtle "down 90: heading -z" ((0., 0., -100.), (0., 0., -1.)) (draw [ down 90.; forward 100. ]);
  (* banked right, its left is up: turning left climbs *)
  check_turtle "roll_right 90, then left 90: climbing" ((0., 0., 100.), (0., 0., 1.))
    (draw [ roll_right 90.; left 90.; forward 100. ])

(* a square pitched instead of turned: a square in the (y, z) plane,
 * back home; and the tree's trick, 3 rolls of 120 *)
let test_closed () =
  check_turtle "repeat 4 [fd; up 90]: back home" home_up (draw [ repeat 4 [ forward 100.; up 90. ] ]);
  check_turtle "3 x roll 120, a branch each: back home" home_up
    (draw [ repeat 3 [ down 35.; forward 50.; back 50.; up 35.; roll_right 120. ] ]);
  (* a staircase around a square tower: 4 flights, each rising, the
   * frame back to start after the whole loop *)
  let ((_, _, z), _) = turtle (draw [ repeat 4 [ up 30.; forward 100.; down 30.; right 90. ] ]) in
  Alcotest.(check (float 1e-6)) "a staircase: each flight 100 sin 30 = 50 higher" 200. z

let test_upto () =
  let shapes = draw_upto 150. [ up 90.; forward 200. ] in
  (* 90 degrees: 22.5; then 127.5 along +z *)
  check_turtle "half way up" ((0., 0., 127.5), (0., 0., 1.)) shapes;
  Alcotest.(check int) "a tube so far, and the turtle" 2 (List.length shapes)

(* the camera sees the drawing's center in the middle of the screen *)
let test_camera () =
  let cam = camera_around [ repeat 4 [ forward 100.; right 90. ] ] 0. in
  let screen = Playground.to_screen 1000. 1000. in
  match Playground3d.project cam screen (50., 50., 0.) with
  | Some (x, y) ->
      if Float.abs x > 1e-6 || Float.abs y > 1e-6 then Alcotest.failf "the center at (%g, %g)" x y
  | None -> Alcotest.fail "the center not seen"

let tests =
  Testo.categorize "Logo3d"
    [
      t "the square, as in 2D" test_square;
      t "the frame: yaw, pitch, roll" test_frame;
      t "closed paths" test_closed;
      t "draw_upto" test_upto;
      t "camera_around" test_camera;
    ]
