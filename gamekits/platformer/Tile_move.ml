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
open Basics (* float arithmetics *)

(* See Tile_move.mli *)

let hits (solid : char -> bool) (map : Tilemap.t) ((w, h) : number * number) (x : number) (y : number) : bool = Tilemap.hits solid map x y w h

let move_by (solid : char -> bool) (map : Tilemap.t) (size : number * number) ((x, y) : number * number) ((dx, dy) : number * number) :
    (number * number) * bool =
  let n = int_of_float (ceil (Float.abs dx + Float.abs dy)) in
  let rec go i (x, y) =
    if i >= n then ((x, y), false)
    else
      let x' = x + (dx / float_of_int n) and y' = y + (dy / float_of_int n) in
      if hits solid map size x' y' then ((x, y), true) else go (succ i) (x', y')
  in
  go 0 (x, y)

let on_ground (solid : char -> bool) (map : Tilemap.t) (size : number * number) (x : number) (y : number) : bool = hits solid map size x (y - 1.)
