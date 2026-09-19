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

(* See Shots.mli *)

type t = { x : number; y : number; vx : number; vy : number }

let straight (x : number) (y : number) (vx : number) (vy : number) : t = { x; y; vx; vy }

let aimed (speed : number) ((x, y) : number * number) ((tx, ty) : number * number) : t =
  let dx = tx -. x and dy = ty -. y in
  let d = Float.max 1e-9 (Float.hypot dx dy) in
  { x; y; vx = speed *. dx /. d; vy = speed *. dy /. d }

let advance (s : t) : t = { s with x = s.x +. s.vx; y = s.y +. s.vy }

let on_screen (margin : number) (screen : screen) (s : t) : bool =
  s.x > screen.left -. margin && s.x < screen.right +. margin && s.y > screen.bottom -. margin && s.y < screen.top +. margin

let near (r : number) ((x, y) : number * number) (s : t) : bool = Float.hypot (s.x -. x) (s.y -. y) < r

let angle (s : t) : number = atan2 s.vy s.vx *. 180. /. Float.pi
