(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* See Isometric.mli *)
open Playground
open Basics (* float arithmetics *)

type t = {
  (* where one unit of world x goes on the screen *)
  ax : number;
  ay : number;
  (* and one unit of z *)
  bx : number;
  by : number;
  (* and one unit of height, straight up *)
  up : number;
  (* the screen point the world's origin is drawn at *)
  ox : number;
  oy : number;
  (* the world point that sits at that origin *)
  fx : number;
  fz : number;
}

let make ~across:(ax, ay) ~along:(bx, by) ~up = { ax; ay; bx; by; up; ox = 0.; oy = 0.; fx = 0.; fz = 0. }
let origin (ox : number) (oy : number) (v : t) : t = { v with ox; oy }
let follow (fx : number) (fz : number) (v : t) : t = { v with fx; fz }

(* the two lines the whole kit is *)
let project (v : t) ((x, y, z) : number * number * number) : number * number =
  let x = x - v.fx and z = z - v.fz in
  (v.ox + (v.ax * x) + (v.bx * z), v.oy + (v.ay * x) + (v.by * z) + (v.up * y))

let at (v : t) (p : number * number * number) (s : shape) : shape =
  let sx, sy = project v p in
  s |> move sx sy

let shadow (v : t) ((x, _y, z) : number * number * number) (s : shape) : shape = at v (x, 0., z) s

(* the two lines run backwards, on the floor: a 2x2 system, whose
 * determinant is not zero as long as the two screen axes are not
 * parallel (a view worth looking at) *)
let ground (v : t) ((sx, sy) : number * number) : number * number =
  let px = sx - v.ox and py = sy - v.oy in
  let det = (v.ax * v.by) - (v.ay * v.bx) in
  (v.fx + (((px * v.by) - (py * v.bx)) / det), v.fz + (((v.ax * py) - (v.ay * px)) / det))

(* The direction this projection flattens to nothing: the one that
 * solves both lines at once, which is what "no perspective" buys --
 * one vector for the whole world instead of a ray per point.
 *
 *   ax dx + bx dz = 0         ->  dx = -(bx / ax) dz
 *   ay dx + by dz + up dy = 0 ->  dy = -(ay dx + by dz) / up
 *
 * taken with dz = -1, so that it points towards the eye (nearer is a
 * smaller z in every view a game builds with [along] pointing away). *)
let toward_eye (v : t) : number * number * number =
  let dz = -1. in
  let dx = -.(v.bx / v.ax) * dz in
  let dy = -.((v.ay * dx) + (v.by * dz)) / v.up in
  (dx, dy, dz)

(* how far from the eye, along that direction: the dot product with it,
 * negated so that bigger is farther *)
let depth (v : t) ((x, y, z) : number * number * number) : number =
  let ex, ey, ez = toward_eye v in
  -.((x * ex) + (y * ey) + (z * ez))

let sorted (l : (number * shape) list) : shape list =
  List.map snd (List.sort (fun (a, _) (b, _) -> compare b a) l)

let sight (v : t) ((x, y, z) : number * number * number) (plane : number) : (number * number) option =
  let ex, ey, ez = toward_eye v in
  (* how far along the line of sight the plane is; ez is -1, so this is
   * z - plane, positive when the plane is between the point and the eye *)
  let t = (plane - z) / ez in
  if t <= 0. then None else Some (x + (ex * t), y + (ey * t))
