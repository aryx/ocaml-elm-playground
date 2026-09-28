(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Joint2d, through Physics' world: each kind of joint
 * against what it should do, measured -- the laws of the textbook
 * where there is one (a pendulum's period, Atwood's machine) *)

open Playground

let t = Testo.create
let g = 800.

let run (steps : int) (f : int -> Physics.world -> unit) (w : Physics.world) : Physics.world =
  let w = ref w in
  for i = 1 to steps do
    w := Physics.simulate ~gravity:g !w;
    f i !w
  done;
  !w

let body i (w : Physics.world) : Physics.body = List.nth w.bodies i
let pivot = Physics.body (circle white 2.) |> Physics.at 0. 0. |> Physics.immovable

(* A pendulum, a rod of 100 from a fixed pivot, let go at 10 degrees:
 * the rod keeps its length, and it swings with the period of the
 * textbook, 2 pi sqrt(L / g) = 2.221 s (0.2% more at 10 degrees) *)
let pendulum () =
  let a = 10. *. Float.pi /. 180. in
  let bx = 100. *. sin a and by = -100. *. cos a in
  let bob = Physics.body (circle white 5.) |> Physics.at bx by in
  let w = Physics.world [ pivot; bob ] |> Physics.rod 0 1 ~at_a:(0., 0.) ~at_b:(bx, by) in
  let worst = ref 0. and crossings = ref [] and last_x = ref bx in
  ignore
    (run 400
       (fun i w ->
         let b = body 1 w in
         worst := Float.max !worst (Float.abs (Float.hypot b.x b.y -. 100.));
         if !last_x > 0. && b.x <= 0. then crossings := float_of_int i /. 60. :: !crossings;
         last_x := b.x)
       w);
  Alcotest.(check bool) (Printf.sprintf "the rod holds its length (%.2f off at most)" !worst) true (!worst < 1.);
  match !crossings with
  | later :: earlier :: _ ->
      let period = later -. earlier in
      Alcotest.(check bool) (Printf.sprintf "the period is the textbook's (%.3f s)" period) true (Float.abs (period -. 2.221) < 0.05)
  | _ -> Alcotest.fail "it swings"

(* A seesaw: a plank pinned at its middle to a fixed pivot, a box
 * dropped on its left end: the left end goes down, the plank turning,
 * and the pin holds it at its middle *)
let seesaw () =
  let plank = Physics.body (rectangle white 200. 10.) |> Physics.at 0. 0. |> Physics.heavy 2. in
  let box = Physics.body (square white 20.) |> Physics.at (-90.) 60. |> Physics.heavy 3. in
  let w = Physics.world [ pivot; plank; box ] |> Physics.pin 0 1 ~at:(0., 0.) in
  let w = run 45 (fun _ _ -> ()) w in
  let p = body 1 w in
  Alcotest.(check bool) (Printf.sprintf "turned, the left end down (%.1f degrees)" p.angle) true (p.angle > 5.);
  Alcotest.(check bool) (Printf.sprintf "still on its pivot (%.2f, %.2f)" p.x p.y) true (Float.hypot p.x p.y < 1.)

(* A rope, 100 long, the ball starting 40 below the pivot: slack, it
 * falls freely; taut, it is caught at 100 and hangs there *)
let rope () =
  let ball = Physics.body (circle white 5.) |> Physics.at 0. (-40.) in
  let w = Physics.world [ pivot; ball ] |> Physics.rope ~length:100. 0 1 ~at_a:(0., 0.) ~at_b:(0., -40.) in
  let w10 = run 10 (fun _ _ -> ()) w in
  let fallen = -40. -. (body 1 w10).y in
  (* free fall, semi-implicit Euler: g dt^2 n (n + 1) / 2 *)
  let free = g /. 3600. *. 55. in
  Alcotest.(check (float 0.01)) "slack: free fall" free fallen;
  let worst = ref 0. in
  let w = run 120 (fun _ w -> worst := Float.max !worst (Float.hypot (body 1 w).x (body 1 w).y)) w10 in
  Alcotest.(check bool) (Printf.sprintf "taut: caught at 100 (%.1f at most)" !worst) true (!worst < 103.);
  Alcotest.(check bool) "and hanging still" true (Float.abs (body 1 w).vy < 5.)

(* Atwood's machine: 1 and 2 hung over a pulley. The rope keeps its
 * length, and the heavy side goes down with the acceleration of the
 * textbook, g (m2 - m1) / (m1 + m2) = g / 3 *)
let atwood () =
  let light = Physics.body (circle white 5.) |> Physics.at (-50.) (-100.) |> Physics.heavy 1. in
  let heavy = Physics.body (circle white 5.) |> Physics.at 50. (-100.) |> Physics.heavy 2. in
  let w =
    Physics.world [ pivot; light; heavy ]
    |> Physics.pulley 1 2 ~at_a:(-50., -100.) ~at_b:(50., -100.) ~ground_a:(-50., 0.) ~ground_b:(50., 0.)
  in
  let length0 = Physics.joint_length 0 w in
  let worst = ref 0. in
  let w = run 30 (fun _ w -> worst := Float.max !worst (Float.abs (Physics.joint_length 0 w -. length0))) w in
  let t = 30. /. 60. in
  let dropped = -100. -. (body 2 w).y in
  let a = 2. *. dropped /. (t *. t) in
  Alcotest.(check bool) (Printf.sprintf "the rope keeps its length (%.2f off)" !worst) true (!worst < 1.);
  Alcotest.(check bool) "the light side up" true ((body 1 w).y > -100.);
  Alcotest.(check bool) (Printf.sprintf "down at g / 3 (%.0f for %.0f)" a (g /. 3.)) true (Float.abs (a -. (g /. 3.)) < 0.08 *. g /. 3.)

(* A motor: a wheel pinned to the world, its motor asking 180 degrees a
 * second: it gets there, and stays *)
let motor () =
  let wheel = Physics.body (circle white 20.) |> Physics.at 0. 0. in
  let w = Physics.world [ pivot; wheel ] |> Physics.pin ~motor:(180., 1e7) 0 1 ~at:(0., 0.) in
  let w = run 30 (fun _ _ -> ()) w in
  Alcotest.(check (float 1.)) "180 degrees a second" 180. (body 1 w).spin;
  let w = Physics.set_motor 0 (-90., 1e7) w in
  let w = run 30 (fun _ _ -> ()) w in
  Alcotest.(check (float 1.)) "set to -90" (-90.) (body 1 w).spin

let tests =
  Testo.categorize "joint2d"
    [ t "a pendulum on a rod" pendulum; t "a seesaw on a pin" seesaw; t "a rope, slack then taut" rope;
      t "Atwood's machine on a pulley" atwood; t "a motor" motor ]
