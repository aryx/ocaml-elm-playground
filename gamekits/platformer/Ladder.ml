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

(* See Ladder.mli *)

(* the column center of a ladder tile at (x, y), if there's one *)
let ladder_at (is_ladder : char -> bool) (map : Tilemap.t) (x : number) (y : number) : number option =
  match Tilemap.tile_at map x y with
  | Some c when is_ladder c ->
      let col, row = Tilemap.cell map x y in
      Some (fst (Tilemap.center map col row))
  | _ -> None

let reach (is_ladder : char -> bool) (map : Tilemap.t) ((_, h) : number * number) (x : number) (y : number) : number option =
  match ladder_at is_ladder map x y with Some cx -> Some cx | None -> ladder_at is_ladder map x (y - (h / 2.) - 0.5)

let standing (solid : char -> bool) (is_ladder : char -> bool) (map : Tilemap.t) (size : number * number) (x : number) (y : number) : bool =
  Tile_move.on_ground solid map size x y || reach is_ladder map size x y <> None

let on_top (is_ladder : char -> bool) (map : Tilemap.t) ((_, h) : number * number) (x : number) (y : number) : bool =
  ladder_at is_ladder map x y = None && ladder_at is_ladder map x (y - (h / 2.) - 0.5) <> None

let climb (solid : char -> bool) (is_ladder : char -> bool) (map : Tilemap.t) (size : number * number) ((x, y) : number * number) (dy : number) :
    number * number =
  match reach is_ladder map size x y with
  | None -> (x, y)
  | Some cx ->
      let n = int_of_float (ceil (Float.abs dy)) in
      let rec go i y =
        if i >= n then y
        else
          let y' = y + (dy / float_of_int n) in
          if reach is_ladder map size cx y' = None || Tile_move.hits solid map size cx y' then y else go (succ i) y'
      in
      (cx, go 0 y)
