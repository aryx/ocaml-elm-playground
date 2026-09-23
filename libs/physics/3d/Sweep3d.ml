(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let gap (p : Vec3.t) (h : Hitbox3d.placed) : float =
  match h.shape with
  | Hitbox3d.Sphere r -> Float.max 0. (Vec3.length (Vec3.sub p h.pos) -. r)
  | Hitbox3d.Box _ -> Vec3.length (Vec3.sub p (Collide3d.closest_on_box h p))
  | Hitbox3d.Capsule (_, r) -> Float.max 0. (Vec3.length (Vec3.sub p (Collide3d.closest_on_segment (Hitbox3d.segment h) p)) -. r)
  | Hitbox3d.Plane (n, d) -> Float.max 0. (Vec3.dot n p -. d)

let reach (h : Hitbox3d.placed) : float =
  match h.shape with
  | Hitbox3d.Sphere r -> r
  | Hitbox3d.Box half -> Vec3.length half
  | Hitbox3d.Capsule (half, r) -> half +. r
  | Hitbox3d.Plane _ -> infinity

(* touching, for the start of a step: within this of the surface *)
let touching = 1e-4

(* the depth the sweep stops at: a millimetre into the obstacle, so that
 * the next step's contact finds it (a sphere stopped exactly at the
 * surface would be touching nothing, and go on) -- well within the
 * solver's slop, so the solver does not push it out *)
let overlap = 1e-3

let sphere ~(radius : float) ~(from : Vec3.t) ~(motion : Vec3.t) ?(moving = ((0., 0., 0.), (0., 0., 0.)))
    (h : Hitbox3d.placed) : float option =
  let shift, turn = moving in
  let at (t : float) : Hitbox3d.placed =
    { h with pos = Vec3.add h.pos (Vec3.scale t shift); orientation = Quat.turned_by ~spin:turn ~dt:t h.orientation }
  in
  let clearance t = gap (Vec3.add from (Vec3.scale t motion)) (at t) -. radius in
  (* the fastest the clearance can shrink, per unit of the step: the
   * relative motion, and the obstacle's farthest point swinging round *)
  let closing =
    Vec3.length (Vec3.sub motion shift) +. if Vec3.length turn = 0. then 0. else Vec3.length turn *. reach h
  in
  if clearance 0. <= touching || closing = 0. || Float.is_nan closing || closing = infinity then None
  else
    let rec advance t n =
      let c = clearance t in
      if c <= -.overlap +. 1e-9 then Some t
      else if n = 0 then Some t (* nearly there, in 64 steps: close enough *)
      else
        let t = t +. ((c +. overlap) /. closing) in
        if t > 1. then None else advance t (n - 1)
    in
    advance 0. 64
