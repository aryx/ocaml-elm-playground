(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Resolve3d.mli *)

let inverse_mass (b : Body3d.t) : float = if Float.is_finite b.Body3d.mass && b.Body3d.mass > 0. then 1. /. b.Body3d.mass else 0.
let inverse_inertia (b : Body3d.t) : Mat3.t = Body3d.inv_inertia_world b

let relative_velocity (a : Body3d.t) (b : Body3d.t) (point : Vec3.t) : Vec3.t =
  Vec3.sub
    (Body3d.point_velocity b (Vec3.sub point b.Body3d.pos))
    (Body3d.point_velocity a (Vec3.sub point a.Body3d.pos))

(* 1/m_a + 1/m_b + n . ((I_a^-1 (r_a x n)) x r_a) + the same for b:
 * how hard the pair is to push along [dir] at [point] *)
let resistance (a : Body3d.t) (b : Body3d.t) (point : Vec3.t) (dir : Vec3.t) : float =
  let arm (body : Body3d.t) =
    let r = Vec3.sub point body.Body3d.pos in
    Vec3.dot dir (Vec3.cross (Mat3.mul_vec (inverse_inertia body) (Vec3.cross r dir)) r)
  in
  inverse_mass a +. inverse_mass b +. arm a +. arm b

let impulse ~(restitution : float) (a : Body3d.t) (b : Body3d.t) (c : Contact3d.t) : float =
  let n = c.Contact3d.normal in
  let closing = Vec3.dot (relative_velocity a b c.Contact3d.point) n in
  (* already moving apart: they touched, and there is nothing to do *)
  if closing > 0. then 0.
  else
    let k = resistance a b c.Contact3d.point n in
    if k < 1e-12 then 0. else -.(1. +. restitution) *. closing /. k

let apply (j : float) (dir : Vec3.t) (point : Vec3.t) ((a, b) : Body3d.t * Body3d.t) : Body3d.t * Body3d.t =
  let push = Vec3.scale j dir in
  let changed (body : Body3d.t) (sign : float) =
    let r = Vec3.sub point body.Body3d.pos in
    let p = Vec3.scale sign push in
    { body with
      Body3d.vel = Vec3.add body.Body3d.vel (Vec3.scale (inverse_mass body) p);
      spin = Vec3.add body.Body3d.spin (Mat3.mul_vec (inverse_inertia body) (Vec3.cross r p)) }
  in
  (changed a (-1.), changed b 1.)

(* two perpendicular directions across the normal. Any pair will do --
 * what matters is that they span the tangent plane -- and picking the
 * axis the normal leans on least keeps the cross product well
 * conditioned. *)
let tangents (n : Vec3.t) : Vec3.t * Vec3.t =
  let nx, ny, nz = n in
  let away = if Float.abs nx <= Float.abs ny && Float.abs nx <= Float.abs nz then (1., 0., 0.) else if Float.abs ny <= Float.abs nz then (0., 1., 0.) else (0., 0., 1.) in
  let t1 = Vec3.normalize (Vec3.cross n away) in
  (t1, Vec3.normalize (Vec3.cross n t1))

let bounce ~(restitution : float) ~(friction : float) ((a, b) : Body3d.t * Body3d.t) (c : Contact3d.t) :
    Body3d.t * Body3d.t =
  let n = c.Contact3d.normal and point = c.Contact3d.point in
  let j = impulse ~restitution a b c in
  if j <= 0. then (a, b)
  else
    let a, b = apply j n point (a, b) in
    if friction <= 0. then (a, b)
    else
      (* Coulomb, along each of the two tangents, clamped to mu j: the
       * friction *pyramid* -- see the .mli for what it gets wrong *)
      let t1, t2 = tangents n in
      List.fold_left
        (fun (a, b) t ->
          let sliding = Vec3.dot (relative_velocity a b point) t in
          let k = resistance a b point t in
          if k < 1e-12 then (a, b)
          else
            let jt = -.sliding /. k in
            let limit = friction *. j in
            let jt = Float.max (-.limit) (Float.min limit jt) in
            apply jt t point (a, b))
        (a, b) [ t1; t2 ]

let separate ?(percent = 1.) ((a, b) : Body3d.t * Body3d.t) (c : Contact3d.t) : Body3d.t * Body3d.t =
  let ia = inverse_mass a and ib = inverse_mass b in
  let total = ia +. ib in
  if total < 1e-12 then (a, b)
  else
    let push = Vec3.scale (percent *. c.Contact3d.depth /. total) c.Contact3d.normal in
    ( { a with Body3d.pos = Vec3.sub a.Body3d.pos (Vec3.scale ia push) },
      { b with Body3d.pos = Vec3.add b.Body3d.pos (Vec3.scale ib push) } )

let resolve ~restitution ~friction (pair : Body3d.t * Body3d.t) (c : Contact3d.t) : Body3d.t * Body3d.t =
  separate (bounce ~restitution ~friction pair c) c
