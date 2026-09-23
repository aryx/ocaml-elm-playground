(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Shape.mli *)

type t = Point | Circle of float | Box of float * float | Polygon of Vec2.t list
type placed = Point_at of Vec2.t | Circle_at of Vec2.t * float | Polygon_at of Vec2.t list

let box_corners (w : float) (h : float) : Vec2.t list =
  let x = w /. 2. and y = h /. 2. in
  [ (-.x, -.y); (x, -.y); (x, y); (-.x, y) ]

let place ?(angle = 0.) ?(scale = 1.) (pos : Vec2.t) (t : t) : placed =
  let c = cos angle and s = sin angle in
  let world (x, y) = Vec2.add pos (Vec2.scale scale ((c *. x) -. (s *. y), (s *. x) +. (c *. y))) in
  match t with
  | Point -> Point_at pos
  | Circle r -> Circle_at (pos, r *. scale)
  | Box (w, h) -> Polygon_at (List.map world (box_corners w h))
  | Polygon corners -> Polygon_at (List.map world corners)

(* each corner with the next one, the last with the first *)
let edges (corners : Vec2.t list) : (Vec2.t * Vec2.t) list =
  match corners with
  | [] -> []
  | first :: _ ->
      let rec go = function a :: (b :: _ as rest) -> (a, b) :: go rest | [ last ] -> [ (last, first) ] | [] -> [] in
      go corners

let area (t : t) : float =
  match t with
  | Point -> 0.
  | Circle r -> Float.pi *. r *. r
  | Box (w, h) -> w *. h
  | Polygon corners -> Float.abs (List.fold_left (fun acc (a, b) -> acc +. Vec2.cross a b) 0. (edges corners)) /. 2.

let moments (p : placed) : float * float =
  match p with
  | Point_at _ -> (0., 0.)
  | Circle_at (c, r) ->
      let a = Float.pi *. r *. r in
      (a, (a *. r *. r /. 2.) +. (a *. Vec2.dot c c))
  | Polygon_at corners ->
      (* each edge (p, q) and (0, 0) make a triangle, of signed area
       * (p x q) / 2 and second moment (p x q) (p.p + p.q + q.q) / 12 *)
      let (a, j) =
        List.fold_left
          (fun (a, j) (p, q) ->
            let c = Vec2.cross p q in
            (a +. (c /. 2.), j +. (c *. (Vec2.dot p p +. Vec2.dot p q +. Vec2.dot q q) /. 12.)))
          (0., 0.) (edges corners)
      in
      (Float.abs a, Float.abs j)

let bounds (p : placed) : Vec2.t * Vec2.t =
  match p with
  | Point_at v -> (v, v)
  | Circle_at ((x, y), r) -> ((x -. r, y -. r), (x +. r, y +. r))
  | Polygon_at corners ->
      List.fold_left
        (fun ((x0, y0), (x1, y1)) (x, y) -> ((Float.min x0 x, Float.min y0 y), (Float.max x1 x, Float.max y1 y)))
        ((infinity, infinity), (neg_infinity, neg_infinity))
        corners

let convex (corners : Vec2.t list) : bool =
  (* the turn at each corner: the cross product of its two edges *)
  let turns =
    match corners with
    | a :: b :: _ ->
        let rec go = function
          | p :: (q :: r :: _ as rest) -> Vec2.cross (Vec2.sub q p) (Vec2.sub r q) :: go rest
          | _ -> []
        in
        go (corners @ [ a; b ])
    | _ -> []
  in
  List.for_all (fun t -> t >= 0.) turns || List.for_all (fun t -> t <= 0.) turns
