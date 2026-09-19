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

(* See Camera3d.mli *)

type pose = { x : number; y : number; z : number; heading : number }

let radians (d : number) : number = d *. Float.pi /. 180.
let forward (heading : number) : number * number = (sin (radians heading), -.cos (radians heading))
let far = 2000.

(*****************************************************************************)
(* Following a pose *)
(*****************************************************************************)

let behind ?(fov = 60.) ~back ~height ~ahead ~look (p : pose) : camera =
  let fx, fz = forward p.heading in
  camera
    ~eye:(p.x -. (back *. fx), p.y +. height, p.z -. (back *. fz))
    ~target:(p.x +. (ahead *. fx), p.y +. look, p.z +. (ahead *. fz))
    ~fov ~far ()

let chase (p : pose) : camera = behind ~back:7. ~height:3.5 ~ahead:6. ~look:0.5 p
let cockpit (p : pose) : camera = behind ~back:(-0.3) ~height:0.9 ~ahead:10. ~look:0.7 p

(*****************************************************************************)
(* Looking at a place *)
(*****************************************************************************)

let looking_down ?(fov = 60.) ~height ((x, y, z) : number * number * number) : camera =
  camera ~eye:(x, y +. height, z +. 0.01) ~target:(x, y, z) ~fov ~far ()

let from_far ?(fov = 60.) ~offset:((dx, dy, dz) : number * number * number) ((x, y, z) : number * number * number) :
    camera =
  camera ~eye:(x +. dx, y +. dy, z +. dz) ~target:(x, y, z) ~fov ~far ()

let orbit ?(fov = 60.) ~distance ~height ~look (angle : number) ((x, y, z) : number * number * number) : camera =
  let a = radians angle in
  camera ~eye:(x +. (distance *. sin a), y +. height, z +. (distance *. cos a)) ~target:(x, y +. look, z) ~fov ~far ()

(*****************************************************************************)
(* Smoothing *)
(*****************************************************************************)

let follow (fraction : number) (wanted : camera) (cam : camera) : camera =
  let toward (x1, y1, z1) (x2, y2, z2) =
    (x1 +. (fraction *. (x2 -. x1)), y1 +. (fraction *. (y2 -. y1)), z1 +. (fraction *. (z2 -. z1)))
  in
  { wanted with eye = toward cam.eye wanted.eye; target = toward cam.target wanted.target }

(*****************************************************************************)
(* The world around *)
(*****************************************************************************)

let floor ?(color = rgb 8 10 20) ?(ground = -0.02) (cam : camera) : shape3d =
  let ex, _, ez = cam.eye in
  plane color 1600. 1600. |> move3d ex ground ez

let sky ?(sky = rgb 12 14 30) ?(horizon = rgb 12 14 30) ?(ground = -0.02) (cam : camera) : shape3d list =
  let ex, ey, ez = cam.eye and tx, _, tz = cam.target in
  let dx = tx -. ex and dz = tz -. ez in
  let d = Float.max 1e-9 (Float.hypot dx dz) in
  let fx = dx /. d and fz = dz /. d in
  let rx = -.fz and rz = fx in
  (* the horizon: a skirt ahead, from the ground 700 away up to just
   * above the eye 1400 away -- nearly flat, facing up, so lit like the
   * floor, the land (or sea) going on to the horizon; 2400 wide, its
   * corners within the far plane (2000) *)
  let at ahead k y = (ex +. (ahead *. fx) +. (k *. 1200. *. rx), y, ez +. (ahead *. fz) +. (k *. 1200. *. rz)) in
  [ plane sky 2400. 2400. |> move3d ex (ey +. 10.) ez;
    polygon3d horizon [ at 700. (-1.) ground; at 700. 1. ground; at 1400. 1. (ey +. 12.); at 1400. (-1.) (ey +. 12.) ] ]
