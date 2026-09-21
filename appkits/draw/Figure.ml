(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type point = float * float
type box = { x0 : float; y0 : float; x1 : float; y1 : float }

let box (ax, ay) (bx, by) = { x0 = Float.min ax bx; y0 = Float.min ay by; x1 = Float.max ax bx; y1 = Float.max ay by }

type style = { fill : float option; pen : float }

type t =
  | Line of point * point * style
  | Rect of box * style
  | Oval of box * style
  | Text of box * string * float
  | Group of t list

let union a b = { x0 = Float.min a.x0 b.x0; y0 = Float.min a.y0 b.y0; x1 = Float.max a.x1 b.x1; y1 = Float.max a.y1 b.y1 }

let rec bounds = function
  | Line (a, b, _) -> box a b
  | Rect (b, _) | Oval (b, _) | Text (b, _, _) -> b
  | Group [] -> { x0 = 0.; y0 = 0.; x1 = 0.; y1 = 0. }
  | Group (f :: rest) -> List.fold_left (fun acc f -> union acc (bounds f)) (bounds f) rest

(*****************************************************************************)
(* Hit testing *)
(*****************************************************************************)

(* how far a point is from the segment a-b: from its nearest point *)
let distance_to_segment (px, py) (ax, ay) (bx, by) =
  let dx = bx -. ax and dy = by -. ay in
  let len2 = (dx *. dx) +. (dy *. dy) in
  let t = if len2 = 0. then 0. else Float.max 0. (Float.min 1. ((((px -. ax) *. dx) +. ((py -. ay) *. dy)) /. len2)) in
  let nx = ax +. (t *. dx) and ny = ay +. (t *. dy) in
  Float.sqrt (((px -. nx) ** 2.) +. ((py -. ny) ** 2.))

let inside b (x, y) = x >= b.x0 && x <= b.x1 && y >= b.y0 && y <= b.y1

let rec hit ~tolerance f ((x, y) as p) =
  match f with
  | Line (a, b, s) -> distance_to_segment p a b <= tolerance +. (s.pen /. 2.)
  | Text (b, _, _) -> inside b p
  | Rect (b, s) -> (
      match s.fill with
      | Some _ -> inside b p
      | None ->
          (* near one of the four sides *)
          let corners = [ (b.x0, b.y0); (b.x1, b.y0); (b.x1, b.y1); (b.x0, b.y1) ] in
          let sides = List.combine corners (List.tl corners @ [ List.hd corners ]) in
          List.exists (fun (a, c) -> distance_to_segment p a c <= tolerance +. (s.pen /. 2.)) sides)
  | Oval (b, s) -> (
      let a = (b.x1 -. b.x0) /. 2. and r = (b.y1 -. b.y0) /. 2. in
      if a <= 0. || r <= 0. then false
      else
        (* how far out the point is, in radii: 1 on the ellipse *)
        let u = (x -. (b.x0 +. a)) /. a and v = (y -. (b.y0 +. r)) /. r in
        let k = Float.sqrt ((u *. u) +. (v *. v)) in
        match s.fill with
        | Some _ -> k <= 1.
        | None -> Float.abs (k -. 1.) *. Float.min a r <= tolerance +. (s.pen /. 2.))
  | Group fs -> List.exists (fun f -> hit ~tolerance f p) fs

(*****************************************************************************)
(* Moving and resizing: maps of the points *)
(*****************************************************************************)

let rec map_points m = function
  | Line (a, b, s) -> Line (m a, m b, s)
  | Rect (b, s) -> Rect (box (m (b.x0, b.y0)) (m (b.x1, b.y1)), s)
  | Oval (b, s) -> Oval (box (m (b.x0, b.y0)) (m (b.x1, b.y1)), s)
  | Text (b, text, size) ->
      (* text keeps its size: only its place moves *)
      let x, y = m (b.x0, b.y1) in
      Text ({ x0 = x; y1 = y; x1 = x +. (b.x1 -. b.x0); y0 = y -. (b.y1 -. b.y0) }, text, size)
  | Group fs -> Group (List.map (map_points m) fs)

let translate dx dy = map_points (fun (x, y) -> (x +. dx, y +. dy))

let fit nb f =
  let b = bounds f in
  let scale lo hi nlo nhi v = if hi = lo then nlo else nlo +. ((v -. lo) *. (nhi -. nlo) /. (hi -. lo)) in
  map_points (fun (x, y) -> (scale b.x0 b.x1 nb.x0 nb.x1 x, scale b.y0 b.y1 nb.y0 nb.y1 y)) f

(*****************************************************************************)
(* Handles *)
(*****************************************************************************)

let handles = function
  | Line (a, b, _) -> [ a; b ]
  | f ->
      let b = bounds f in
      let mx = (b.x0 +. b.x1) /. 2. and my = (b.y0 +. b.y1) /. 2. in
      [ (b.x0, b.y1); (b.x1, b.y1); (b.x1, b.y0); (b.x0, b.y0); (mx, b.y1); (b.x1, my); (mx, b.y0); (b.x0, my) ]

let drag_handle f i (x, y) =
  match f with
  | Line (a, b, s) -> if i = 0 then Line ((x, y), b, s) else Line (a, (x, y), s)
  | f ->
      let b = bounds f in
      (* which sides the handle carries: left/right, top/bottom *)
      let l, r, t, bo =
        match i with
        | 0 -> (Some x, None, Some y, None)
        | 1 -> (None, Some x, Some y, None)
        | 2 -> (None, Some x, None, Some y)
        | 3 -> (Some x, None, None, Some y)
        | 4 -> (None, None, Some y, None)
        | 5 -> (None, Some x, None, None)
        | 6 -> (None, None, None, Some y)
        | _ -> (Some x, None, None, None)
      in
      let v o d = Option.value o ~default:d in
      (* the corners it ends with, in whichever order: [box] turns a
         box dragged past its opposite side over *)
      fit (box (v l b.x0, v bo b.y0) (v r b.x1, v t b.y1)) f

let rec restyle g = function
  | Line (a, b, s) -> Line (a, b, g s)
  | Rect (b, s) -> Rect (b, g s)
  | Oval (b, s) -> Oval (b, g s)
  | Text _ as t -> t
  | Group fs -> Group (List.map (restyle g) fs)
