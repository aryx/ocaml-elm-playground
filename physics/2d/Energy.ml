(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Energy.mli *)

(* (a body that never turns has an infinite inertia and no spin:
 * no energy, not infinity * 0) *)
let rotational (b : Body.t) : float = if b.spin = 0. then 0. else b.inertia *. b.spin *. b.spin /. 2.
let kinetic (b : Body.t) : float = (b.mass *. Vec2.dot b.vel b.vel /. 2.) +. rotational b
let momentum (b : Body.t) : Vec2.t = Vec2.scale b.mass b.vel

let angular_momentum ~(around : Vec2.t) (b : Body.t) : float =
  (b.mass *. Vec2.cross (Vec2.sub b.pos around) b.vel) +. if b.spin = 0. then 0. else b.inertia *. b.spin
let gravity ~(g : float) (b : Body.t) : float = b.mass *. g *. snd b.pos

let gravitation ~(gm : float) ~(center : Vec2.t) (b : Body.t) : float =
  -.gm *. b.mass /. Vec2.length (Vec2.sub b.pos center)

let spring ~(k_over_m : float) ~(anchor : Vec2.t) (b : Body.t) : float =
  let d = Vec2.sub b.pos anchor in
  k_over_m *. b.mass *. Vec2.dot d d /. 2.
