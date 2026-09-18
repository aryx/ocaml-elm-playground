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

type t = { pos : Vec2.t; vel : Vec2.t; mass : float }

let make ?(vel = (0., 0.)) ?(mass = 1.) (pos : Vec2.t) : t = { pos; vel; mass }
