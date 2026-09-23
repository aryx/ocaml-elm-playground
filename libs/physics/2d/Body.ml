(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Body.mli *)

type t = { pos : Vec2.t; vel : Vec2.t; mass : float; spin : float; inertia : float }

let make ?(vel = (0., 0.)) ?(mass = 1.) ?(spin = 0.) ?(inertia = infinity) (pos : Vec2.t) : t =
  { pos; vel; mass; spin; inertia }

let point_velocity (b : t) (r : Vec2.t) : Vec2.t = Vec2.add b.vel (Vec2.scale b.spin (Vec2.perp r))
