(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Skp_view.mli *)

type t = { target : Vec3.t; distance : float; azimuth : float; elevation : float; fov : float }
type area = { cx : float; cy : float; w : float; h : float }

let start = { target = (0., 0., 2.); distance = 22.; azimuth = -60.; elevation = 14.; fov = 35. }
let radians d = d *. Float.pi /. 180.

let eye t =
  let az = radians t.azimuth and el = radians t.elevation in
  Vec3.add t.target (Vec3.scale t.distance (cos el *. cos az, cos el *. sin az, sin el))

(* just in front of the eye: a centimetre *)
let near = 0.01

let camera t : Camera.t = { eye = eye t; target = t.target; up = (0., 0., 1.); fov = t.fov; ortho = 0.; near; far = infinity }

let to_screen t area v =
  match Camera.ndc (camera t) ~aspect:(area.w /. area.h) v with
  | Some (x, y) -> Some (area.cx +. (x *. area.w /. 2.), area.cy +. (y *. area.h /. 2.))
  | None -> None

let project t area p = to_screen t area (Camera.view (camera t) p)

let ray t area (sx, sy) =
  let c = camera t in
  let right, up, forward = Camera.basis ~up:c.up ~eye:c.eye ~target:c.target () in
  let f = Camera.focal c in
  let x = (sx -. area.cx) /. (area.w /. 2.) *. (area.w /. area.h) /. f and y = (sy -. area.cy) /. (area.h /. 2.) /. f in
  (c.eye, Vec3.normalize (Vec3.add forward (Vec3.add (Vec3.scale x right) (Vec3.scale y up))))

(* in view coordinates, z is the depth: the plane where polygons are
   cut, its front the side away from the eye -- a little beyond [near],
   so that the corners the cut makes are not refused by Camera.ndc for
   being a rounding error nearer *)
let cut_at = 2. *. near
let in_front : Bsp.plane = ((0., 0., 1.), cut_at)

let polygon t area corners =
  let c = camera t in
  let front, _ = Bsp.split in_front (List.map (fun (p, drawn) -> (Camera.view c p, drawn)) corners) in
  List.filter_map (fun (v, drawn) -> Option.map (fun s -> (s, drawn)) (to_screen t area v)) front

let segment t area p q =
  let c = camera t in
  let (_, _, zp) as vp = Camera.view c p and ((_, _, zq) as vq) = Camera.view c q in
  let cut a za b zb = Vec3.add a (Vec3.scale ((cut_at -. za) /. (zb -. za)) (Vec3.sub b a)) in
  let ends =
    if zp >= cut_at && zq >= cut_at then Some (vp, vq)
    else if zp >= cut_at then Some (vp, cut vp zp vq zq)
    else if zq >= cut_at then Some (cut vq zq vp zp, vq)
    else None
  in
  match ends with
  | Some (a, b) -> ( match (to_screen t area a, to_screen t area b) with Some a, Some b -> Some (a, b) | _ -> None)
  | None -> None

let orbit t dx dy = { t with azimuth = t.azimuth -. (dx *. 0.4); elevation = Float.max (-89.) (Float.min 89. (t.elevation -. (dy *. 0.4))) }

let pan t area dx dy =
  let c = camera t in
  let right, up, _ = Camera.basis ~up:c.up ~eye:c.eye ~target:c.target () in
  (* the world's size of a screen unit, at the target's depth *)
  let unit = 2. *. t.distance /. Camera.focal c /. area.h in
  { t with target = Vec3.sub t.target (Vec3.add (Vec3.scale (dx *. unit) right) (Vec3.scale (dy *. unit) up)) }

let zoom t notches = { t with distance = Float.max 0.5 (Float.min 2000. (t.distance *. (0.85 ** notches))) }

let extents t points =
  match points with
  | [] -> start
  | p :: _ ->
      let lo = List.fold_left (fun (a, b, c) (x, y, z) -> (Float.min a x, Float.min b y, Float.min c z)) p points in
      let hi = List.fold_left (fun (a, b, c) (x, y, z) -> (Float.max a x, Float.max b y, Float.max c z)) p points in
      let radius = Float.max 1. (Vec3.length (Vec3.sub hi lo) /. 2.) in
      { t with target = Vec3.scale 0.5 (Vec3.add lo hi); distance = radius /. sin (radians t.fov /. 2.) *. 1.15 }
