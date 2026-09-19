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

let impulse ~restitution (a : Body.t) (b : Body.t) (normal : Vec2.t) : float =
  let closing = Vec2.dot (Vec2.sub b.vel a.vel) normal in
  let inv = inverse_mass a +. inverse_mass b in
  if closing >= 0. || inv = 0. then 0. else -.(1. +. restitution) *. closing /. inv

let apply (j : float) (dir : Vec2.t) ((a, b) : Body.t * Body.t) : Body.t * Body.t =
  ( { a with vel = Vec2.sub a.vel (Vec2.scale (j *. inverse_mass a) dir) },
    { b with vel = Vec2.add b.vel (Vec2.scale (j *. inverse_mass b) dir) } )

let bounce ~restitution ~friction ((a, b) : Body.t * Body.t) (c : Contact.t) : Body.t * Body.t =
  let j = impulse ~restitution a b c.normal in
  let (a, b) = apply j c.normal (a, b) in
  (* the sliding: what's left of the relative velocity once its part
   * along the normal is taken out *)
  let rel = Vec2.sub b.vel a.vel in
  let sliding = Vec2.sub rel (Vec2.scale (Vec2.dot rel c.normal) c.normal) in
  let speed = Vec2.length sliding in
  if j = 0. || friction = 0. || speed = 0. then (a, b)
  else
    let tangent = Vec2.scale (1. /. speed) sliding in
    (* the impulse that would stop the sliding, but no more than
     * friction * j (Coulomb): past that, they slide *)
    let stop = speed /. (inverse_mass a +. inverse_mass b) in
    apply (-.Float.min stop (friction *. j)) tangent (a, b)

let separate ?(percent = 1.) ((a, b) : Body.t * Body.t) (c : Contact.t) : Body.t * Body.t =
  let inv = inverse_mass a +. inverse_mass b in
  if inv = 0. then (a, b)
  else
    let d = percent *. c.depth /. inv in
    ( { a with pos = Vec2.sub a.pos (Vec2.scale (d *. inverse_mass a) c.normal) },
      { b with pos = Vec2.add b.pos (Vec2.scale (d *. inverse_mass b) c.normal) } )

let resolve ~restitution ~friction (ab : Body.t * Body.t) (c : Contact.t) : Body.t * Body.t =
  separate (bounce ~restitution ~friction ab c) c
