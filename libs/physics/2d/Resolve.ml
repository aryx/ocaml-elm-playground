(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Resolve.mli *)

let inverse_mass (b : Body.t) : float = 1. /. b.mass
let inverse_inertia (b : Body.t) : float = 1. /. b.inertia

(* how a and b resist an impulse along [dir] at [point]: by their
 * masses, and by their inertias through the lever arms r *)
let resistance (a : Body.t) (b : Body.t) (point : Vec2.t) (dir : Vec2.t) : float =
  let ra = Vec2.cross (Vec2.sub point a.pos) dir and rb = Vec2.cross (Vec2.sub point b.pos) dir in
  inverse_mass a +. inverse_mass b +. (ra *. ra *. inverse_inertia a) +. (rb *. rb *. inverse_inertia b)

(* how fast b's point moves away from a's, at [point] *)
let relative_velocity (a : Body.t) (b : Body.t) (point : Vec2.t) : Vec2.t =
  Vec2.sub (Body.point_velocity b (Vec2.sub point b.pos)) (Body.point_velocity a (Vec2.sub point a.pos))

let impulse ~restitution (a : Body.t) (b : Body.t) (c : Contact.t) : float =
  let closing = Vec2.dot (relative_velocity a b c.point) c.normal in
  let k = resistance a b c.point c.normal in
  if closing >= 0. || k = 0. then 0. else -.(1. +. restitution) *. closing /. k

let apply (j : float) (dir : Vec2.t) (point : Vec2.t) ((a, b) : Body.t * Body.t) : Body.t * Body.t =
  let push = Vec2.scale j dir in
  (* the torque's lever arm: r x push *)
  let torque (body : Body.t) = Vec2.cross (Vec2.sub point body.pos) push *. inverse_inertia body in
  ( { a with vel = Vec2.sub a.vel (Vec2.scale (inverse_mass a) push); spin = a.spin -. torque a },
    { b with vel = Vec2.add b.vel (Vec2.scale (inverse_mass b) push); spin = b.spin +. torque b } )

let bounce ~restitution ~friction ((a, b) : Body.t * Body.t) (c : Contact.t) : Body.t * Body.t =
  let j = impulse ~restitution a b c in
  let (a, b) = apply j c.normal c.point (a, b) in
  (* the sliding: what's left of the touching points' relative
   * velocity once its part along the normal is taken out *)
  let rel = relative_velocity a b c.point in
  let sliding = Vec2.sub rel (Vec2.scale (Vec2.dot rel c.normal) c.normal) in
  let speed = Vec2.length sliding in
  if j = 0. || friction = 0. || speed = 0. then (a, b)
  else
    let tangent = Vec2.scale (1. /. speed) sliding in
    (* the impulse that would stop the sliding, but no more than
     * friction * j (Coulomb): past that, they slide *)
    let stop = speed /. resistance a b c.point tangent in
    apply (-.Float.min stop (friction *. j)) tangent c.point (a, b)

let separate ?(percent = 1.) ((a, b) : Body.t * Body.t) (c : Contact.t) : Body.t * Body.t =
  let inv = inverse_mass a +. inverse_mass b in
  if inv = 0. then (a, b)
  else
    let d = percent *. c.depth /. inv in
    ( { a with pos = Vec2.sub a.pos (Vec2.scale (d *. inverse_mass a) c.normal) },
      { b with pos = Vec2.add b.pos (Vec2.scale (d *. inverse_mass b) c.normal) } )

let resolve ~restitution ~friction (ab : Body.t * Body.t) (c : Contact.t) : Body.t * Body.t =
  separate (bounce ~restitution ~friction ab c) c
