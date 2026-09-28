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

let steps = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

let problem ~(walkable : int * int -> bool) (target : int * int) : (int * int) Pathfind.problem =
  {
    neighbors =
      (fun (x, y) -> List.filter_map (fun (dx, dy) -> let c = (x +.. dx, y +.. dy) in if walkable c then Some (c, 1.) else None) steps);
    goal = (fun c -> c = target);
    estimate = (fun c -> Pathfind.manhattan c target);
  }

let path ~(walkable : int * int -> bool) ~(from : int * int) (target : int * int) : (int * int) list =
  (Pathfind.astar (problem ~walkable target) from).path

let nearest ~(walkable : int * int -> bool) ~(from : int * int) (wanted : int * int -> bool) : (int * int) list =
  (* no estimate: nothing to aim at until the search finds it *)
  let p = { (problem ~walkable from) with goal = wanted; estimate = (fun _ -> 0.) } in
  (Pathfind.astar p from).path

let field ~(walkable : int * int -> bool) (target : int * int) : ((int * int) * float) list =
  Pathfind.field (problem ~walkable target) target

let downhill ~(walkable : int * int -> bool) (field : ((int * int) * float) list) (cell : int * int) : (int * int) option =
  Pathfind.downhill (problem ~walkable cell) field cell

let cell_of ((x, y) : number * number) : int * int = (int_of_float (Float.round x), int_of_float (Float.round y))

let toward ~(speed : number) ((x, y) : number * number) ((cx, cy) : int * int) : number * number =
  let tx = float_of_int cx and ty = float_of_int cy in
  let dx = tx - x and dy = ty - y in
  let d = Float.hypot dx dy in
  if d <= speed then (tx, ty) else (x + (speed * dx / d), y + (speed * dy / d))

let advance ~(speed : number) ((x, y) : number * number) (path : (int * int) list) : (number * number) * (int * int) list =
  (* the tiles already under the unit are dropped, then it walks at the
   * next one *)
  let rec drop = function c :: rest when c = cell_of (x, y) -> drop rest | p -> p in
  match drop path with
  | [] -> ((x, y), [])
  | next :: rest ->
      let pos = toward ~speed (x, y) next in
      if cell_of pos = next then (pos, rest) else (pos, next :: rest)
