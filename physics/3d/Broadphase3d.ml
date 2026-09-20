(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Broadphase3d.mli *)

type box = Vec3.t * Vec3.t
type method_ = All_pairs | Grid | Sweep_and_prune

let methods = [ All_pairs; Grid; Sweep_and_prune ]
let name = function All_pairs -> "all pairs" | Grid -> "grid" | Sweep_and_prune -> "sweep and prune"

type result = { pairs : (int * int) list; tests : int }

let ordered i j = if i < j then (i, j) else (j, i)

let all_pairs (boxes : box array) : result =
  let n = Array.length boxes in
  let pairs = ref [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if Collide3d.bounds_overlap boxes.(i) boxes.(j) then pairs := (i, j) :: !pairs
    done
  done;
  { pairs = List.rev !pairs; tests = n * (n - 1) / 2 }

let cell_size (boxes : box array) : float =
  Array.fold_left
    (fun m ((x0, y0, z0), (x1, y1, z1)) -> Float.max m (Float.max (x1 -. x0) (Float.max (y1 -. y0) (z1 -. z0))))
    0. boxes

let grid ?cell (boxes : box array) : result =
  let cell = match cell with Some c -> c | None -> cell_size boxes in
  let cell = if cell > 0. then cell else 1. in
  let index v = int_of_float (Float.floor (v /. cell)) in
  (* claude: a *hash* of the cells, not an array of them: in 3D a dense
   * grid is the memory argument against grids (see the .mli), and only
   * the cells something is in cost anything here *)
  let cells : (int * int * int, int list) Hashtbl.t = Hashtbl.create 64 in
  boxes
  |> Array.iteri (fun i ((x0, y0, z0), (x1, y1, z1)) ->
         for cx = index x0 to index x1 do
           for cy = index y0 to index y1 do
             for cz = index z0 to index z1 do
               Hashtbl.replace cells (cx, cy, cz) (i :: Option.value ~default:[] (Hashtbl.find_opt cells (cx, cy, cz)))
             done
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
                   if Collide3d.bounds_overlap boxes.(i) boxes.(j) then pairs := (i, j) :: !pairs))
               inside)
           inside);
  { pairs = List.sort compare !pairs; tests = Hashtbl.length tested }

(* how spread out the boxes' centres are along each axis: the axis to
 * sweep is the one they differ most along (Bullet and I-COLLIDE pick
 * it the same way, by variance) *)
let spread (boxes : box array) : float * float * float =
  let n = Array.length boxes in
  if n = 0 then (0., 0., 0.)
  else
    let sum = ref (0., 0., 0.) and sum2 = ref (0., 0., 0.) in
    Array.iter
      (fun ((x0, y0, z0), (x1, y1, z1)) ->
        let c = ((x0 +. x1) /. 2., (y0 +. y1) /. 2., (z0 +. z1) /. 2.) in
        sum := Vec3.add !sum c;
        let cx, cy, cz = c in
        sum2 := Vec3.add !sum2 (cx *. cx, cy *. cy, cz *. cz))
      boxes;
    let f = float_of_int n in
    let mx, my, mz = Vec3.scale (1. /. f) !sum and sx, sy, sz = Vec3.scale (1. /. f) !sum2 in
    (sx -. (mx *. mx), sy -. (my *. my), sz -. (mz *. mz))

let widest_axis (boxes : box array) : int =
  let vx, vy, vz = spread boxes in
  if vx >= vy && vx >= vz then 0 else if vy >= vz then 1 else 2

let along (axis : int) ((lo, hi) : box) : float * float =
  let x0, y0, z0 = lo and x1, y1, z1 = hi in
  match axis with 0 -> (x0, x1) | 1 -> (y0, y1) | _ -> (z0, z1)

let sweep_and_prune ?axis (boxes : box array) : result =
  let axis = match axis with Some a -> a | None -> widest_axis boxes in
  let low i = fst (along axis boxes.(i)) and high i = snd (along axis boxes.(i)) in
  let order = List.sort (fun i j -> compare (low i) (low j)) (List.init (Array.length boxes) Fun.id) in
  let tests = ref 0 and pairs = ref [] in
  let (_ : int list) =
    List.fold_left
      (fun active i ->
        (* the boxes that end before i starts cannot touch it, nor
         * anything after it: they leave the active set *)
        let active = List.filter (fun a -> high a >= low i) active in
        List.iter
          (fun a ->
            incr tests;
            if Collide3d.bounds_overlap boxes.(a) boxes.(i) then pairs := ordered a i :: !pairs)
          active;
        i :: active)
      [] order
  in
  { pairs = List.sort compare !pairs; tests = !tests }

let pairs (m : method_) (boxes : box array) : result =
  match m with All_pairs -> all_pairs boxes | Grid -> grid boxes | Sweep_and_prune -> sweep_and_prune boxes
