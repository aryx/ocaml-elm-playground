(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type vec = float * float * float
type box = { x0 : float; y0 : float; z0 : float; x1 : float; y1 : float; z1 : float }
type segment = { box : box; rgb : int * int * int; light : float }
type side = { axis : int; positive : bool }
type quad = { corners : vec list; normal : vec; side : side }
type opening = { into : int; quad : quad }

type level = {
  segments : segment array;
  walls : quad list array;
  openings : opening list array;
  start : vec;
  exit : int;
}

(*****************************************************************************)
(* Boxes, axis by axis *)
(*****************************************************************************)

(* the two other axes, in the order that makes (u, v, axis) turn the
 * same way as (x, y, z): u = axis + 1, v = axis + 2 *)
let others (axis : int) : int * int = ((axis + 1) mod 3, (axis + 2) mod 3)

let low (b : box) (axis : int) : float = match axis with 0 -> b.x0 | 1 -> b.y0 | _ -> b.z0
let high (b : box) (axis : int) : float = match axis with 0 -> b.x1 | 1 -> b.y1 | _ -> b.z1
let coord ((x, y, z) : vec) (axis : int) : float = match axis with 0 -> x | 1 -> y | _ -> z

(* the point whose [axis] coordinate is [c], [u] and [v] the other two
 * (in [others]' order) *)
let point (axis : int) (c : float) (u : float) (v : float) : vec =
  let a, _ = others axis in
  let get i = if i = axis then c else if i = a then u else v in
  (get 0, get 1, get 2)

let face (b : box) (axis : int) (positive : bool) : float = if positive then high b axis else low b axis

(* the side's rectangle, in the two other axes *)
let rect (b : box) (axis : int) : float * float * float * float =
  let u, v = others axis in
  (low b u, high b u, low b v, high b v)

let contains (b : box) (p : vec) : bool =
  let (x, y, z) = p in
  x >= b.x0 && x <= b.x1 && y >= b.y0 && y <= b.y1 && z >= b.z0 && z <= b.z1

(* A side's rectangle as a quad, its corners counterclockwise seen from
 * inside the box: (u0, v0), (u1, v0), (u1, v1), (u0, v1) turns that way
 * seen from the axis' positive end, so the positive side (whose inside
 * is the other way) takes it backwards. *)
let quad_of (side : side) (c : float) ((u0, u1, v0, v1) : float * float * float * float) : quad =
  let p u v = point side.axis c u v in
  let corners = [ p u0 v0; p u1 v0; p u1 v1; p u0 v1 ] in
  let one = if side.positive then -1. else 1. in
  { corners = (if side.positive then List.rev corners else corners);
    normal = point side.axis one 0. 0.;
    side }

(*****************************************************************************)
(* Building *)
(*****************************************************************************)

(* the part of two rectangles that overlaps, if it has an area *)
let overlap (a0, a1, b0, b1) (c0, c1, d0, d1) : (float * float * float * float) option =
  let u0 = Float.max a0 c0 and u1 = Float.min a1 c1 and v0 = Float.max b0 d0 and v1 = Float.min b1 d1 in
  if u0 < u1 && v0 < v1 then Some (u0, u1, v0, v1) else None

(* The rock of a side: its rectangle with the openings cut out. The
 * openings' edges cut it into a grid, and a cell of that grid is rock
 * unless it falls inside an opening:
 *
 *      +----+-------+---+     the side, with one opening (##); the
 *      |    |       |   |     lines through its edges make 9 cells,
 *      +----+-------+---+     8 of them rock, drawn as they are (a
 *      |    |#######|   |     rectangle each, never cut further)
 *      +----+-------+---+
 *      |    |       |   |
 *      +----+-------+---+
 *)
let rock (side : side) (c : float) (r : float * float * float * float) (holes : (float * float * float * float) list) : quad list =
  let u0, u1, v0, v1 = r in
  let cuts lo hi ends = List.sort_uniq compare ((lo :: hi :: ends) |> List.filter (fun x -> x >= lo && x <= hi)) in
  let us = cuts u0 u1 (List.concat_map (fun (a0, a1, _, _) -> [ a0; a1 ]) holes) in
  let vs = cuts v0 v1 (List.concat_map (fun (_, _, b0, b1) -> [ b0; b1 ]) holes) in
  let rec pairs = function a :: (b :: _ as rest) -> (a, b) :: pairs rest | _ -> [] in
  List.concat_map
    (fun (ua, ub) ->
      List.filter_map
        (fun (va, vb) ->
          let mu = (ua +. ub) /. 2. and mv = (va +. vb) /. 2. in
          if List.exists (fun (a0, a1, b0, b1) -> mu > a0 && mu < a1 && mv > b0 && mv < b1) holes then None
          else Some (quad_of side c (ua, ub, va, vb)))
        (pairs vs))
    (pairs us)

let make (segments : segment list) ~(start : vec) ~(exit : int) : level =
  let segments = Array.of_list segments in
  let n = Array.length segments in
  let openings = Array.make n [] in
  (* the holes of each (segment, side), to cut the rock around *)
  let holes = Hashtbl.create 64 in
  let add_hole i side r = Hashtbl.replace holes (i, side) (r :: Option.value (Hashtbl.find_opt holes (i, side)) ~default:[]) in
  (* i's positive side against j's negative one: the same rectangle
   * becomes an opening on both sides of it *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if i <> j then
        for axis = 0 to 2 do
          let a = segments.(i).box and b = segments.(j).box in
          if high a axis = low b axis then
            match overlap (rect a axis) (rect b axis) with
            | None -> ()
            | Some r ->
                let at = high a axis in
                let mine_ = { axis; positive = true } and theirs = { axis; positive = false } in
                add_hole i mine_ r;
                add_hole j theirs r;
                openings.(i) <- { into = j; quad = quad_of mine_ at r } :: openings.(i);
                openings.(j) <- { into = i; quad = quad_of theirs at r } :: openings.(j)
        done
    done
  done;
  let walls =
    Array.init n (fun i ->
        List.concat_map
          (fun axis ->
            List.concat_map
              (fun positive ->
                let side = { axis; positive } in
                let b = segments.(i).box in
                rock side (face b axis positive) (rect b axis) (Option.value (Hashtbl.find_opt holes (i, side)) ~default:[]))
              [ false; true ])
          [ 0; 1; 2 ])
  in
  { segments; walls; openings; start; exit }

(*****************************************************************************)
(* Asking *)
(*****************************************************************************)

let segment_at (lv : level) (p : vec) : int option =
  let rec go i = if i >= Array.length lv.segments then None else if contains lv.segments.(i).box p then Some i else go (i + 1) in
  go 0

let inside (lv : level) ~(radius : float) (p : vec) : bool =
  match segment_at lv p with
  | None -> false
  | Some i ->
      let b = lv.segments.(i).box in
      (* every side the ball reaches must be an opening, with room
       * around the ball *)
      List.for_all
        (fun (axis, positive) ->
          let c = face b axis positive in
          if Float.abs (coord p axis -. c) >= radius then true
          else
            let u, v = others axis in
            let pu = coord p u and pv = coord p v in
            List.exists
              (fun (o : opening) ->
                o.quad.side.axis = axis && o.quad.side.positive = positive
                &&
                let corners = o.quad.corners in
                let us = List.map (fun q -> coord q u) corners and vs = List.map (fun q -> coord q v) corners in
                let mn l = List.fold_left Float.min infinity l and mx l = List.fold_left Float.max neg_infinity l in
                pu -. radius >= mn us && pu +. radius <= mx us && pv -. radius >= mn vs && pv +. radius <= mx vs)
              lv.openings.(i))
        [ (0, false); (0, true); (1, false); (1, true); (2, false); (2, true) ]

let move (lv : level) ~(radius : float) (p : vec) (delta : vec) : vec =
  let step p axis =
    let d = coord delta axis in
    let (x, y, z) = p in
    let q = match axis with 0 -> (x +. d, y, z) | 1 -> (x, y +. d, z) | _ -> (x, y, z +. d) in
    if inside lv ~radius q then q else p
  in
  List.fold_left step p [ 0; 1; 2 ]

let clear (lv : level) ((x1, y1, z1) : vec) ((x2, y2, z2) : vec) : bool =
  let n = Float.to_int (sqrt (((x2 -. x1) ** 2.) +. ((y2 -. y1) ** 2.) +. ((z2 -. z1) ** 2.))) + 1 in
  List.for_all
    (fun k ->
      let f = float_of_int k /. float_of_int n in
      segment_at lv (x1 +. ((x2 -. x1) *. f), y1 +. ((y2 -. y1) *. f), z1 +. ((z2 -. z1) *. f)) <> None)
    (List.init (n + 1) Fun.id)

(*****************************************************************************)
(* The mine *)
(*****************************************************************************)

let box x0 y0 z0 x1 y1 z1 = { x0; y0; z0; x1; y1; z1 }

let seg ?(rgb = (96, 88, 80)) ?(light = 0.7) b : segment = { box = b; rgb; light }

(* Twelve boxes: the start room, then two ways round to the reactor
 * room -- east through a corridor, a shaft up, the upper room and a
 * corridor north; or north, another room, a shaft up and the west room
 * -- and the exit beyond the reactor:
 *
 *      seen from above (x to the right, z up the page)
 *
 *          +------+ 10       +----------+
 *          |      |----------|          |
 *          | 8/9  |    ^     |    6     |--+ 11 (exit)
 *          +------+    |     +----------+
 *             |        5          |
 *             7                   |
 *          +------+  1  +------+  |
 *          |  0   |-----| 2/3  |--+ 4 above 2
 *          +------+     +------+
 *)
let mine : level =
  make
    ~start:(20., 12., 20.) ~exit:11
    [ seg ~rgb:(104, 96, 84) (box 0. 0. 0. 40. 24. 40.);
      (* the corridor east *)
      seg ~light:0.5 (box 40. 4. 12. 80. 20. 28.);
      seg ~rgb:(96, 92, 100) (box 80. 0. 0. 120. 24. 40.);
      (* the shaft up, out of the junction room's ceiling *)
      seg ~light:0.45 (box 92. 24. 12. 108. 64. 28.);
      seg ~rgb:(96, 92, 100) (box 80. 64. 0. 120. 88. 40.);
      (* the corridor north, to the reactor room *)
      seg ~light:0.5 (box 92. 68. 40. 108. 84. 80.);
      (* the reactor room *)
      seg ~rgb:(120, 80, 72) ~light:0.85 (box 60. 56. 80. 140. 96. 140.);
      (* the corridor north, out of the start room *)
      seg ~light:0.5 (box 12. 4. 40. 28. 20. 80.);
      seg ~rgb:(104, 96, 84) (box 0. 0. 80. 40. 24. 120.);
      (* its shaft up *)
      seg ~light:0.45 (box 12. 24. 92. 28. 60. 108.);
      seg ~rgb:(96, 92, 100) (box 0. 60. 80. 60. 96. 120.);
      (* the exit, east of the reactor room *)
      seg ~rgb:(72, 120, 80) ~light:1. (box 140. 64. 100. 160. 80. 120.) ]
