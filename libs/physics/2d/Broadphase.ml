(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Broadphase.mli *)

type box = Vec2.t * Vec2.t
type method_ = All_pairs | Grid | Sort_and_sweep

let methods = [ All_pairs; Grid; Sort_and_sweep ]
let name = function All_pairs -> "all pairs" | Grid -> "grid" | Sort_and_sweep -> "sort and sweep"

type result = { pairs : (int * int) list; tests : int }

(* the pair (i, j), i < j *)
let ordered i j = if i < j then (i, j) else (j, i)

let all_pairs (boxes : box array) : result =
  let n = Array.length boxes in
  let pairs = ref [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if Collide.bounds_overlap boxes.(i) boxes.(j) then pairs := (i, j) :: !pairs
    done
  done;
  { pairs = List.rev !pairs; tests = n * (n - 1) / 2 }

let cell_size (boxes : box array) : float =
  Array.fold_left (fun m ((x0, y0), (x1, y1)) -> Float.max m (Float.max (x1 -. x0) (y1 -. y0))) 0. boxes

let grid ?cell (boxes : box array) : result =
  let cell = match cell with Some c -> c | None -> cell_size boxes in
  (* a box of size 0 (points only): any cell size will do *)
  let cell = if cell > 0. then cell else 1. in
  let index v = int_of_float (Float.floor (v /. cell)) in
  (* cell -> the boxes in it, in the order they were added *)
  let cells : (int * int, int list) Hashtbl.t = Hashtbl.create 64 in
  boxes
  |> Array.iteri (fun i ((x0, y0), (x1, y1)) ->
         for cx = index x0 to index x1 do
           for cy = index y0 to index y1 do
             Hashtbl.replace cells (cx, cy) (i :: Option.value ~default:[] (Hashtbl.find_opt cells (cx, cy)))
           done
         done);
  (* two boxes sharing several cells are tested once *)
  let tested = Hashtbl.create 64 and pairs = ref [] in
  cells
  |> Hashtbl.iter (fun _ inside ->
         List.iter
           (fun i ->
             List.iter
               (fun j ->
                 if i < j && not (Hashtbl.mem tested (i, j)) then (
                   Hashtbl.add tested (i, j) ();
                   if Collide.bounds_overlap boxes.(i) boxes.(j) then pairs := (i, j) :: !pairs))
               inside)
           inside);
  { pairs = List.sort compare !pairs; tests = Hashtbl.length tested }

let sort_and_sweep (boxes : box array) : result =
  let left i = fst (fst boxes.(i)) and right i = fst (snd boxes.(i)) in
  let order = List.sort (fun i j -> compare (left i) (left j)) (List.init (Array.length boxes) Fun.id) in
  let tests = ref 0 and pairs = ref [] in
  let (_ : int list) =
    List.fold_left
      (fun active i ->
        (* the boxes ending before i starts can't touch it, nor any box
         * after it: they leave *)
        let active = List.filter (fun a -> right a >= left i) active in
        List.iter
          (fun a ->
            incr tests;
            (* their x ranges overlap: the y ranges decide *)
            if Collide.bounds_overlap boxes.(a) boxes.(i) then pairs := ordered a i :: !pairs)
          active;
        i :: active)
      [] order
  in
  { pairs = List.sort compare !pairs; tests = !tests }

let pairs (m : method_) (boxes : box array) : result =
  match m with All_pairs -> all_pairs boxes | Grid -> grid boxes | Sort_and_sweep -> sort_and_sweep boxes
