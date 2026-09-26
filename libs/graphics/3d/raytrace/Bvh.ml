(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Bvh.mli *)

type split = Median | Sah
type box = Vec3.t * Vec3.t

(* each solid with its place in the scene's list: two solids met at the
 * same t (the edge two triangles share) are told apart as brute force
 * does, the first in the list winning, so that the pictures are the
 * same bytes *)
type item = int * Solid.t

type node =
  | Leaf of box * item array
  | Node of box * node * node

type stats = { mutable boxes : int; mutable tests : int }
type t = { root : node option; unbounded : item list; stats : stats }

(*****************************************************************************)
(* Boxes *)
(*****************************************************************************)

let union (((ax, ay, az), (bx, by, bz)) : box) (((cx, cy, cz), (dx, dy, dz)) : box) : box =
  ((Float.min ax cx, Float.min ay cy, Float.min az cz), (Float.max bx dx, Float.max by dy, Float.max bz dz))

let area (((ax, ay, az), (bx, by, bz)) : box) : float =
  let x = bx -. ax and y = by -. ay and z = bz -. az in
  2. *. ((x *. y) +. (y *. z) +. (z *. x))

let centre (((ax, ay, az), (bx, by, bz)) : box) : Vec3.t = ((ax +. bx) /. 2., (ay +. by) /. 2., (az +. bz) /. 2.)
let axis (i : int) ((x, y, z) : Vec3.t) : float = match i with 0 -> x | 1 -> y | _ -> z

(*****************************************************************************)
(* Building *)
(*****************************************************************************)

(* a leaf of at most this many solids, whatever the cut *)
let leaf_size = 4

(* the cost of entering a box, in solid tests: a box test is cheaper
 * than most solids' *)
let traversal = 1.

(* the surface area heuristic's candidate cuts: at the bins' edges,
 * this many bins per axis (binned SAH: Wald 2007), rather than at every
 * solid, which would sort the solids at every level *)
let bins = 12

(* a solid to place: its box, the box's centre (computed once), and
 * the solid with its place in the scene *)
type entry = { b : box; c : Vec3.t; item : item }

let rec build_node (split : split) (entries : entry array) : node =
  let n = Array.length entries in
  let whole = Array.fold_left (fun b e -> union b e.b) entries.(0).b entries in
  let leaf () = Leaf (whole, Array.map (fun e -> e.item) entries) in
  if n <= 1 then leaf ()
  else
    (* the box of the centres: where they spread *)
    let (lx, ly, lz), (hx, hy, hz) = Array.fold_left (fun b e -> union b (e.c, e.c)) (entries.(0).c, entries.(0).c) entries in
    let lo = [| lx; ly; lz |] and extent = [| hx -. lx; hy -. ly; hz -. lz |] in
    let longest = if extent.(0) >= extent.(1) && extent.(0) >= extent.(2) then 0 else if extent.(1) >= extent.(2) then 1 else 2 in
    let cut (left : entry array) (right : entry array) : node =
      Node (whole, build_node split left, build_node split right)
    in
    (* half the solids on each side along the longest axis, by their
     * centres: a sort *)
    let median () =
      let sorted = Array.copy entries in
      Array.stable_sort (fun e1 e2 -> Float.compare (axis longest e1.c) (axis longest e2.c)) sorted;
      cut (Array.sub sorted 0 (n / 2)) (Array.sub sorted (n / 2) (n - (n / 2)))
    in
    if extent.(longest) <= 0. then (* all the centres at one point: no cut separates them *)
      if n <= leaf_size then leaf () else median ()
    else
      match split with
      | Median -> if n <= leaf_size then leaf () else median ()
      | Sah ->
          let bin_of a (e : entry) =
            if extent.(a) <= 0. then 0
            else Int.min (bins - 1) (int_of_float (float_of_int bins *. (axis a e.c -. lo.(a)) /. extent.(a)))
          in
          (* the best cut: an axis, and how many bins go left *)
          let best = ref (float_of_int n, -1, 0) in
          for a = 0 to 2 do
            let count = Array.make bins 0 and box = Array.make bins None in
            Array.iter
              (fun e ->
                let i = bin_of a e in
                count.(i) <- count.(i) + 1;
                box.(i) <- Some (match box.(i) with None -> e.b | Some b -> union b e.b))
              entries;
            let merge acc i = match (acc, box.(i)) with None, b | b, None -> b | Some x, Some y -> Some (union x y) in
            (* the right sides, swept from the right *)
            let right_box = Array.make bins None and right_count = Array.make bins 0 in
            let acc = ref None and k = ref 0 in
            for i = bins - 1 downto 1 do
              acc := merge !acc i;
              k := !k + count.(i);
              right_box.(i) <- !acc;
              right_count.(i) <- !k
            done;
            let acc = ref None and k = ref 0 in
            for i = 0 to bins - 2 do
              acc := merge !acc i;
              k := !k + count.(i);
              (* left: bins 0..i, right: bins i+1.. *)
              match (!acc, right_box.(i + 1)) with
              | Some l, Some r ->
                  let cost =
                    traversal
                    +. (((area l *. float_of_int !k) +. (area r *. float_of_int right_count.(i + 1)))
                       /. Float.max (area whole) 1e-12)
                  in
                  let c, _, _ = !best in
                  if cost < c then best := (cost, a, i + 1)
              | _ -> ()
            done
          done;
          (match !best with
          | _, -1, _ ->
              (* no cut beats testing them all: a leaf, if it is small *)
              if n <= leaf_size then leaf () else median ()
          | _, a, k ->
              (* arrays, not lists: a scene of 100,000 triangles would
               * overflow a browser's stack in List.filter *)
              let is_left = Array.map (fun e -> bin_of a e < k) entries in
              let pick side = Array.of_seq (Seq.filter_map (fun i -> if is_left.(i) = side then Some entries.(i) else None) (Seq.init n Fun.id)) in
              cut (pick true) (pick false))

(* a box a hair larger than its solid (see Bvh.mli, "a box must never
 * say no"): a millionth of a millionth of its size, and of its
 * distance from the origin, where the rounding happens *)
let padded (((ax, ay, az), (bx, by, bz)) : box) : box =
  let size = List.fold_left (fun m v -> Float.max m (Float.abs v)) 1. [ ax; ay; az; bx; by; bz ] in
  let e = 1e-9 *. size in
  ((ax -. e, ay -. e, az -. e), (bx +. e, by +. e, bz +. e))

let build ~(split : split) (solids : Solid.t list) : t =
  (* claude: arrays and Seq, no List.map: the same stack overflow *)
  let items = Array.mapi (fun i s -> (i, s)) (Array.of_list solids) in
  let bounded =
    Array.of_seq
      (Seq.filter_map
         (fun ((_, s) as item) -> Option.map (fun b -> let b = padded b in { b; c = centre b; item }) (Solid.bounds s))
         (Array.to_seq items))
  in
  let unbounded = List.of_seq (Seq.filter (fun (_, s) -> Solid.bounds s = None) (Array.to_seq items)) in
  let root = if Array.length bounded = 0 then None else Some (build_node split bounded) in
  { root; unbounded; stats = { boxes = 0; tests = 0 } }

let stats (t : t) : stats = t.stats

let depth (t : t) : int =
  let rec go = function Leaf _ -> 1 | Node (_, l, r) -> 1 + Int.max (go l) (go r) in
  match t.root with None -> 0 | Some n -> go n

(*****************************************************************************)
(* Searching *)
(*****************************************************************************)

let box_of = function Leaf (b, _) | Node (b, _, _) -> b

let nearest (t : t) ~(min_t : float) ~(max_t : float) (ray : Ray.t) : (float * Solid.t) option =
  (* the best so far, and its t: nothing beyond it matters any more *)
  let best = ref None and limit = ref max_t and best_index = ref max_int in
  let test ((i, solid) : item) =
    t.stats.tests <- t.stats.tests + 1;
    match Solid.hit ~min_t ray solid with
    (* a tie only with a hit: at max_t itself, brute force takes none *)
    | Some tt when tt < !limit || (tt = !limit && !best <> None && i < !best_index) ->
        best := Some (tt, solid);
        limit := tt;
        best_index := i
    | _ -> ()
  in
  List.iter test t.unbounded;
  (* where the ray enters a node's box, if it does before [limit] *)
  let entry node =
    t.stats.boxes <- t.stats.boxes + 1;
    match Ray.box ray (box_of node) with
    (* at [limit] itself, not beyond: a tie there may be an earlier solid *)
    | Some (t_in, t_out) when t_out >= min_t && t_in <= !limit -> Some t_in
    | _ -> None
  in
  let rec visit node =
    match node with
    | Leaf (_, solids) -> Array.iter test solids
    | Node (_, l, r) -> (
        match (entry l, entry r) with
        | None, None -> ()
        | Some _, None -> visit l
        | None, Some _ -> visit r
        | Some tl, Some tr ->
            (* the nearer first: its hit may make the other's box too far *)
            let first, second, t_second = if tl <= tr then (l, r, tr) else (r, l, tl) in
            visit first;
            if t_second <= !limit then visit second)
  in
  (match t.root with Some root when entry root <> None -> visit root | _ -> ());
  !best

let any (t : t) ~(min_t : float) ~(max_t : float) (ray : Ray.t) : bool =
  let test ((_, solid) : item) =
    t.stats.tests <- t.stats.tests + 1;
    match Solid.hit ~min_t ray solid with Some tt -> tt < max_t | None -> false
  in
  let enters node =
    t.stats.boxes <- t.stats.boxes + 1;
    match Ray.box ray (box_of node) with Some (t_in, t_out) -> t_out >= min_t && t_in < max_t | None -> false
  in
  let rec visit = function
    | Leaf (_, solids) -> Array.exists test solids
    | Node (_, l, r) -> (enters l && visit l) || (enters r && visit r)
  in
  List.exists test t.unbounded || match t.root with Some root -> enters root && visit root | None -> false
