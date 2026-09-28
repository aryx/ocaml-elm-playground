(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Golden_scene.mli *)

type scene = string * string * int
type scripted = string * string * int * string
type flagged = string * string * int * string list
type scripted_flagged = string * string * int * string * string list
