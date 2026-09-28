(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Grid_move

(* See Chase.mli *)

let ways (g : grid) ~(open_ : int * int -> bool) (m : mover) : dir list =
  let ways = List.filter (fun d -> d <> opposite m.dir && open_ (next_tile g m d)) [ Up; Left; Down; Right ] in
  if ways = [] then [ opposite m.dir ] else ways

let toward (g : grid) ~(open_ : int * int -> bool) ~(goal : int * int) (m : mover) : mover =
  let ways = ways g ~open_ m in
  let dist d =
    let c, r = next_tile g m d and gc, gr = goal in
    ((c - gc) * (c - gc)) + ((r - gr) * (r - gr))
  in
  { m with dir = List.fold_left (fun best d -> if dist d < dist best then d else best) (List.hd ways) ways }

let at_random (g : grid) ~(open_ : int * int -> bool) (n : int) (m : mover) : mover =
  let ways = ways g ~open_ m in
  { m with dir = List.nth ways (n mod List.length ways) }

let next_random (r : int) : int = ((r * 5) + 1) land 8191
