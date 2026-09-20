(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Hitbox3d.mli *)

type t = Sphere of float | Box of Vec3.t | Capsule of float * float | Plane of Vec3.t * float
type placed = { shape : t; pos : Vec3.t; orientation : Quat.t }

let place ?(orientation = Quat.identity) pos shape = { shape; pos; orientation }

(* the local axes, turned into the world *)
let axes (p : placed) : Vec3.t * Vec3.t * Vec3.t =
  ( Quat.rotate p.orientation (1., 0., 0.),
    Quat.rotate p.orientation (0., 1., 0.),
    Quat.rotate p.orientation (0., 0., 1.) )

let corners (p : placed) : Vec3.t list =
  match p.shape with
  | Box (hx, hy, hz) ->
      let ax, ay, az = axes p in
      List.concat_map
        (fun sx ->
          List.concat_map
            (fun sy ->
              List.map
                (fun sz ->
                  Vec3.add p.pos
                    (Vec3.add (Vec3.scale (sx *. hx) ax) (Vec3.add (Vec3.scale (sy *. hy) ay) (Vec3.scale (sz *. hz) az))))
                [ -1.; 1. ])
            [ -1.; 1. ])
        [ -1.; 1. ]
  | _ -> []

let face_axes (p : placed) : Vec3.t list =
  match p.shape with
  | Box _ ->
      let ax, ay, az = axes p in
      [ ax; ay; az ]
  | Plane (n, _) -> [ n ]
  | _ -> []

let segment (p : placed) : Vec3.t * Vec3.t =
  match p.shape with
  | Capsule (half, _) ->
      let _, ay, _ = axes p in
      (Vec3.add p.pos (Vec3.scale (-.half) ay), Vec3.add p.pos (Vec3.scale half ay))
  | _ -> (p.pos, p.pos)

let support (p : placed) (d : Vec3.t) : Vec3.t =
  let u = Vec3.normalize d in
  match p.shape with
  | Sphere r -> Vec3.add p.pos (Vec3.scale r u)
  | Box _ -> (
      match corners p with
      | [] -> p.pos
      | c :: cs -> List.fold_left (fun best q -> if Vec3.dot q u > Vec3.dot best u then q else best) c cs)
  | Capsule (_, r) ->
      let a, b = segment p in
      let tip = if Vec3.dot a u > Vec3.dot b u then a else b in
      Vec3.add tip (Vec3.scale r u)
  | Plane _ -> p.pos

let extent (p : placed) (axis : Vec3.t) : float * float =
  match p.shape with
  | Sphere r ->
      let c = Vec3.dot p.pos axis in
      (c -. r, c +. r)
  | Box _ ->
      let ds = List.map (fun c -> Vec3.dot c axis) (corners p) in
      (List.fold_left Float.min infinity ds, List.fold_left Float.max neg_infinity ds)
  | Capsule (_, r) ->
      let a, b = segment p in
      let da = Vec3.dot a axis and db = Vec3.dot b axis in
      (Float.min da db -. r, Float.max da db +. r)
  | Plane (n, d) ->
      (* a half-space: everything below the plane along its normal *)
      if Vec3.dot n axis > 0.999999 then (neg_infinity, d) else (neg_infinity, infinity)

let bounds (p : placed) : Vec3.t * Vec3.t =
  match p.shape with
  | Plane _ -> ((neg_infinity, neg_infinity, neg_infinity), (infinity, infinity, infinity))
  | _ ->
      let along a =
        let lo, hi = extent p a in
        (lo, hi)
      in
      let lx, hx = along (1., 0., 0.) and ly, hy = along (0., 1., 0.) and lz, hz = along (0., 0., 1.) in
      ((lx, ly, lz), (hx, hy, hz))

let volume : t -> float = function
  | Sphere r -> 4. /. 3. *. Float.pi *. r *. r *. r
  | Box (hx, hy, hz) -> 8. *. hx *. hy *. hz
  | Capsule (half, r) -> (Float.pi *. r *. r *. (2. *. half)) +. (4. /. 3. *. Float.pi *. r *. r *. r)
  | Plane _ -> 0.

let inertia ~mass : t -> Mat3.t = function
  | Sphere r -> Body3d.solid_sphere ~mass ~radius:r
  | Box (hx, hy, hz) -> Body3d.box ~mass (2. *. hx, 2. *. hy, 2. *. hz)
  | Plane _ -> Body3d.never_turns
  | Capsule (half, r) ->
      (* a cylinder of length 2 half and two hemispheres, each moved
       * onto the axis by the parallel-axis theorem: the 3 h r / 8 term
       * is a hemisphere's own centroid, 3r/8 from its flat face *)
      let l = 2. *. half in
      let v_cyl = Float.pi *. r *. r *. l and v_hemi = 2. /. 3. *. Float.pi *. r *. r *. r in
      let total = v_cyl +. (2. *. v_hemi) in
      let mc = mass *. v_cyl /. total and mh = mass *. v_hemi /. total in
      let along = (mc *. r *. r /. 2.) +. (2. *. mh *. 2. /. 5. *. r *. r) in
      let across =
        (mc *. ((l *. l /. 12.) +. (r *. r /. 4.)))
        +. (2. *. mh *. ((2. /. 5. *. r *. r) +. (l *. l /. 4.) +. (3. *. l *. r /. 8.)))
      in
      Mat3.diagonal across along across
