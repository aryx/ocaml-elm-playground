(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Contact3d.mli *)

type t = { normal : Vec3.t; depth : float; point : Vec3.t }

let make ~normal ~depth ~point = { normal = Vec3.normalize normal; depth; point }
let flip (c : t) : t = { c with normal = Vec3.scale (-1.) c.normal }
