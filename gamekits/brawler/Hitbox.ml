(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

(* See Hitbox.mli *)

type box = { x : number; y : number; w : number; h : number }

let place (facing : number) ((fx, fy) : number * number) (b : box) : box = { b with x = fx +. (facing *. b.x); y = fy +. b.y }
let overlap (a : box) (b : box) : bool = Float.abs (a.x -. b.x) < (a.w +. b.w) /. 2. && Float.abs (a.y -. b.y) < (a.h +. b.h) /. 2.
let draw (color : color) (b : box) : shape = rectangle color b.w b.h |> fade 0.4 |> move b.x b.y
