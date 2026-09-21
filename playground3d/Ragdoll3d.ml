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

let count = 10

(* each body: its box (width, height, depth), where its middle is above
 * the feet and across (x), and its mass *)
let parts =
  [ ((0.34, 0.55, 0.2), (0., 1.2), 25.) (* 0 torso, 0.925 to 1.475 *);
    ((0.22, 0.22, 0.22), (0., 1.6), 5.) (* 1 head *);
    ((0.1, 0.3, 0.1), (-0.23, 1.3), 2.5) (* 2 left upper arm, 1.15 to 1.45 *);
    ((0.1, 0.3, 0.1), (0.23, 1.3), 2.5) (* 3 right upper arm *);
    ((0.09, 0.3, 0.09), (-0.23, 0.99), 2.) (* 4 left forearm, 0.84 to 1.14 *);
    ((0.09, 0.3, 0.09), (0.23, 0.99), 2.) (* 5 right forearm *);
    ((0.14, 0.44, 0.14), (-0.1, 0.69), 8.) (* 6 left thigh, 0.47 to 0.91 *);
    ((0.14, 0.44, 0.14), (0.1, 0.69), 8.) (* 7 right thigh *);
    ((0.11, 0.44, 0.11), (-0.1, 0.235), 4.) (* 8 left shin, 0.015 to 0.455 *);
    ((0.11, 0.44, 0.11), (0.1, 0.235), 4.) (* 9 right shin *) ]

let bodies ?(color = rgb 200 170 140) ((x, y, z) : number * number * number) : Physics3d.body list =
  List.map
    (fun ((w, h, d), (dx, dy), mass) ->
      Physics3d.body (box color w h d) |> Physics3d.at (x +. dx) (y +. dy) z |> Physics3d.heavy mass |> Physics3d.rough 0.6)
    parts

let join ?(limits = true) (first : int) (w : Physics3d.world) : Physics3d.world =
  (* where the ragdoll stands: its torso's feet *)
  let torso = List.nth w.bodies first in
  let x = torso.x and y = torso.y -. 1.2 and z = torso.z in
  let at (dx, dy) = (x +. dx, y +. dy, z) in
  let i k = first + k in
  let cone axis deg = if limits then Some (axis, deg) else None in
  let down = (0., -1., 0.) and side = (1., 0., 0.) in
  let ball ?cone a b p w = Physics3d.ball_joint ?cone (i a) (i b) ~at:(at p) w in
  let hinge lim a b p w = Physics3d.hinge ?limits:(if limits then Some lim else None) (i a) (i b) ~at:(at p) ~axis:side w in
  w
  |> ball ?cone:(cone (0., 1., 0.) 40.) 0 1 (0., 1.49)
  |> ball ?cone:(cone down 80.) 0 2 (-0.2, 1.44)
  |> ball ?cone:(cone down 80.) 0 3 (0.2, 1.44)
  (* the forearm turning about x from hanging down goes forwards (-z)
   * for a positive angle: an elbow *)
  |> hinge (0., 140.) 2 4 (-0.23, 1.145)
  |> hinge (0., 140.) 3 5 (0.23, 1.145)
  |> ball ?cone:(cone down 70.) 0 6 (-0.1, 0.92)
  |> ball ?cone:(cone down 70.) 0 7 (0.1, 0.92)
  (* and a shin backwards (+z) for a negative one: a knee *)
  |> hinge (-140., 0.) 6 8 (-0.1, 0.46)
  |> hinge (-140., 0.) 7 9 (0.1, 0.46)
