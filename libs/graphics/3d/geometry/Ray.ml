(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ray.mli *)

type t = { origin : Vec3.t; direction : Vec3.t }

(* below this, a denominator is taken for zero: the ray runs parallel *)
let eps = 1e-9

let make (origin : Vec3.t) (direction : Vec3.t) : t =
  if Vec3.length direction < eps then invalid_arg "Ray.make: a zero direction";
  { origin; direction = Vec3.normalize direction }

let at (ray : t) (t : float) : Vec3.t = Vec3.add ray.origin (Vec3.scale t ray.direction)

(*****************************************************************************)
(* Intersections *)
(*****************************************************************************)

let sphere (ray : t) ((c, r) : Vec3.t * float) : (float * float) option =
  let m = Vec3.sub ray.origin c in
  let b = Vec3.dot m ray.direction and k = Vec3.dot m m -. (r *. r) in
  let disc = (b *. b) -. k in
  if disc < 0. then None
  else
    let s = sqrt disc in
    Some (-.b -. s, -.b +. s)

let plane (ray : t) ((n, d) : Vec3.t * float) : float option =
  let denom = Vec3.dot n ray.direction in
  if Float.abs denom < eps then None else Some ((d -. Vec3.dot n ray.origin) /. denom)

(* Moller and Trumbore (1997): no plane equation, no precomputation --
 * the barycentric coordinates fall out of one cross product each *)
let triangle (ray : t) ((a, b, c) : Vec3.t * Vec3.t * Vec3.t) : (float * float * float) option =
  let e1 = Vec3.sub b a and e2 = Vec3.sub c a in
  let p = Vec3.cross ray.direction e2 in
  let det = Vec3.dot e1 p in
  if Float.abs det < eps then None
  else
    let inv = 1. /. det in
    let s = Vec3.sub ray.origin a in
    let u = Vec3.dot s p *. inv in
    if u < 0. || u > 1. then None
    else
      let q = Vec3.cross s e1 in
      let v = Vec3.dot ray.direction q *. inv in
      if v < 0. || u +. v > 1. then None else Some (Vec3.dot e2 q *. inv, u, v)

let box (ray : t) (((lx, ly, lz), (hx, hy, hz)) : Vec3.t * Vec3.t) : (float * float) option =
  let ox, oy, oz = ray.origin and dx, dy, dz = ray.direction in
  (* one slab: the interval so far, narrowed to where the line is
   * between lo and hi on this axis *)
  let slab o d lo hi (t_in, t_out) =
    if Float.abs d < eps then
      (* parallel to the slab: all of the line, or none of it *)
      if o < lo || o > hi then (infinity, neg_infinity) else (t_in, t_out)
    else
      let t1 = (lo -. o) /. d and t2 = (hi -. o) /. d in
      (Float.max t_in (Float.min t1 t2), Float.min t_out (Float.max t1 t2))
  in
  let t_in, t_out = slab oz dz lz hz (slab oy dy ly hy (slab ox dx lx hx (neg_infinity, infinity))) in
  if t_in > t_out then None else Some (t_in, t_out)
