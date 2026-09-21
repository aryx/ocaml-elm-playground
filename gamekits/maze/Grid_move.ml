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

(* See Grid_move.mli *)

type dir = Up | Down | Left | Right | Stop

let delta (d : dir) : int * int =
  match d with Up -> (0, -1) | Down -> (0, 1) | Left -> (-1, 0) | Right -> (1, 0) | Stop -> (0, 0)

let opposite (d : dir) : dir = match d with Up -> Down | Down -> Up | Left -> Right | Right -> Left | Stop -> Stop

type grid = { tile : int; cols : int; rows : int }
type mover = { gx : int; gy : int; dir : dir; wanted : dir }

let mover_at (g : grid) ((col, row) : int * int) : mover = { gx = col * g.tile; gy = row * g.tile; dir = Stop; wanted = Stop }
let at_center (g : grid) (m : mover) : bool = m.gx mod g.tile = 0 && m.gy mod g.tile = 0
let tile_of (g : grid) (m : mover) : int * int = ((m.gx + (g.tile / 2)) / g.tile mod g.cols, (m.gy + (g.tile / 2)) / g.tile)

let next_tile (g : grid) (m : mover) (d : dir) : int * int =
  let col, row = tile_of g m in
  let dc, dr = delta d in
  ((col + dc + g.cols) mod g.cols, row + dr)

(* one pixel in [m.dir], wrapping around horizontally *)
let advance (g : grid) (m : mover) : mover =
  let dx, dy = delta m.dir in
  let w = g.cols * g.tile in
  { m with gx = (m.gx + dx + w) mod w; gy = m.gy + dy }

let slide (g : grid) ~(choose : mover -> mover) (speed : int) (m : mover) : mover =
  let rec go n m =
    if n = 0 then m
    else
      let m = if at_center g m then choose m else m in
      if m.dir = Stop then m else go (n - 1) (advance g m)
  in
  go speed m

let steer (g : grid) ~(open_ : int * int -> bool) (m : mover) : mover =
  if m.wanted <> Stop && open_ (next_tile g m m.wanted) then { m with dir = m.wanted }
  else if m.dir <> Stop && open_ (next_tile g m m.dir) then m
  else { m with dir = Stop }

let move_player (g : grid) ~(open_ : int * int -> bool) (speed : int) (m : mover) : mover =
  let m = if m.wanted = opposite m.dir && m.dir <> Stop then { m with dir = m.wanted } else m in
  slide g ~choose:(steer g ~open_) speed m

let to_world (g : grid) (bounds : Camera2d.rect) (m : mover) : number * number =
  let half = float_of_int g.tile /. 2. in
  (bounds.left +. float_of_int m.gx +. half, bounds.top -. float_of_int m.gy -. half)
