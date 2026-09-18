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

let kinetic (b : Body.t) : float = b.mass *. Vec2.dot b.vel b.vel /. 2.
let momentum (b : Body.t) : Vec2.t = Vec2.scale b.mass b.vel
let gravity ~(g : float) (b : Body.t) : float = b.mass *. g *. snd b.pos

let gravitation ~(gm : float) ~(center : Vec2.t) (b : Body.t) : float =
  -.gm *. b.mass /. Vec2.length (Vec2.sub b.pos center)

let spring ~(k_over_m : float) ~(anchor : Vec2.t) (b : Body.t) : float =
  let d = Vec2.sub b.pos anchor in
  k_over_m *. b.mass *. Vec2.dot d d /. 2.
