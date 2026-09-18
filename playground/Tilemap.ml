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

(* See Tilemap.mli *)

(* [cells.(row)] is a row, of [cols] characters (padded with ' ') *)
type t = { size : number; cols : int; cells : string array }

let of_strings (size : number) (rows : string list) : t =
  let cols = List.fold_left (fun acc row -> max acc (String.length row)) 0 rows in
  let pad row = row ^ String.make (cols - String.length row) ' ' in
  { size; cols; cells = Array.of_list (List.map pad rows) }

let size (map : t) : number = map.size
let cols (map : t) : int = map.cols
let rows (map : t) : int = Array.length map.cells
let width (map : t) : number = float_of_int map.cols *. map.size
let height (map : t) : number = float_of_int (rows map) *. map.size

let bounds (map : t) : Camera2d.rect =
  let half_w = width map /. 2. and half_h = height map /. 2. in
  { left = -.half_w; right = half_w; bottom = -.half_h; top = half_h }

(*****************************************************************************)
(* Cells *)
(*****************************************************************************)

let inside (map : t) (col : int) (row : int) : bool =
  col >= 0 && col < map.cols && row >= 0 && row < rows map

let get (map : t) (col : int) (row : int) : char option =
  if inside map col row then Some map.cells.(row).[col] else None

let set (map : t) (col : int) (row : int) (c : char) : t =
  if not (inside map col row) then map
  else
    let cells = Array.copy map.cells in
    cells.(row) <- String.mapi (fun i old -> if i = col then c else old) cells.(row);
    { map with cells }

let to_strings (map : t) : string list = Array.to_list map.cells

let find (map : t) (c : char) : (int * int) list =
  map.cells |> Array.to_list
  |> List.mapi (fun row line ->
         List.init map.cols (fun col -> col) |> List.filter (fun col -> line.[col] = c)
         |> List.map (fun col -> (col, row)))
  |> List.concat

let center (map : t) (col : int) (row : int) : number * number =
  let b = bounds map in
  ( b.left +. ((float_of_int col +. 0.5) *. map.size),
    b.top -. ((float_of_int row +. 0.5) *. map.size) )

let cell (map : t) (x : number) (y : number) : int * int =
  let b = bounds map in
  (int_of_float (floor ((x -. b.left) /. map.size)), int_of_float (floor ((b.top -. y) /. map.size)))

let tile_at (map : t) (x : number) (y : number) : char option =
  let col, row = cell map x y in
  get map col row

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

(* the cells from (col1, row1) to (col2, row2) included, clipped to the map *)
let view_cells (tile : char -> shape) (map : t) (col1, row1) (col2, row2) : shape =
  let shapes = ref [] in
  for row = max 0 row1 to min (rows map - 1) row2 do
    for col = max 0 col1 to min (map.cols - 1) col2 do
      let c = map.cells.(row).[col] in
      if c <> ' ' then
        let x, y = center map col row in
        shapes := (tile c |> move x y) :: !shapes
    done
  done;
  group (List.rev !shapes)

let view (tile : char -> shape) (map : t) : shape =
  view_cells tile map (0, 0) (map.cols - 1, rows map - 1)

let view_visible (rect : Camera2d.rect) (tile : char -> shape) (map : t) : shape =
  view_cells tile map (cell map rect.left rect.top) (cell map rect.right rect.bottom)

(*****************************************************************************)
(* Collisions *)
(*****************************************************************************)

(* The cells a box enters, e.g. along x: from the one its left side is
 * in, to the one just before its right side. Its right side being on a
 * border, at a whole number of tiles, the cell on the right of the
 * border is not entered: hence the ceil - 1, not a floor.
 *
 *       |  col 1  |  col 2  |  col 3  |
 *            [=======box=====]          left in col 1, right on the
 *                                       border: cols 1 and 2 only
 *)
let hits (solid : char -> bool) (map : t) (x : number) (y : number) (w : number) (h : number) : bool =
  let b = bounds map in
  let first a = int_of_float (floor (a /. map.size)) in
  let last a = int_of_float (ceil (a /. map.size)) - 1 in
  let col1 = first (x -. (w /. 2.) -. b.left) and col2 = last (x +. (w /. 2.) -. b.left) in
  let row1 = first (b.top -. (y +. (h /. 2.))) and row2 = last (b.top -. (y -. (h /. 2.))) in
  let hit = ref false in
  for row = row1 to row2 do
    for col = col1 to col2 do
      match get map col row with
      | Some c when solid c -> hit := true
      | _ -> ()
    done
  done;
  !hit
