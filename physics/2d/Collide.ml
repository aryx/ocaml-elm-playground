(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Collide.mli *)

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let circles ((c1, r1) : Vec2.t * float) ((c2, r2) : Vec2.t * float) : Contact.t option =
  let d = Vec2.sub c2 c1 in
  let dist = Vec2.length d in
  if dist >= r1 +. r2 then None
  else
    (* the same center: any direction will do *)
    let normal = if dist = 0. then (1., 0.) else Vec2.scale (1. /. dist) d in
    Some { normal; depth = r1 +. r2 -. dist; point = Vec2.add c1 (Vec2.scale r1 normal) }

let bounds_overlap (((ax0, ay0), (ax1, ay1)) : Vec2.t * Vec2.t) (((bx0, by0), (bx1, by1)) : Vec2.t * Vec2.t) : bool =
  ax0 <= bx1 && bx0 <= ax1 && ay0 <= by1 && by0 <= ay1

let point_in_polygon ((px, py) : Vec2.t) (corners : Vec2.t list) : bool =
  (* the edges crossing the horizontal ray from p to the right: an edge
   * whose ends are on both sides of the ray's line, crossing it right of
   * p. (A corner exactly on the line counts for one of its two edges
   * only: one end strictly above, the other not.) *)
  Shape.edges corners
  |> List.fold_left
       (fun inside ((x1, y1), (x2, y2)) ->
         if y1 > py <> (y2 > py) && px < x1 +. ((py -. y1) *. (x2 -. x1) /. (y2 -. y1)) then not inside else inside)
       false

(* on which side of the line through a and b is p: > 0 left, < 0 right *)
let side (a : Vec2.t) (b : Vec2.t) (p : Vec2.t) : float = Vec2.cross (Vec2.sub b a) (Vec2.sub p a)

(* do the intervals [a1, a2] and [b1, b2] (in any order) overlap *)
let overlap_1d a1 a2 b1 b2 = Float.max (Float.min a1 a2) (Float.min b1 b2) <= Float.min (Float.max a1 a2) (Float.max b1 b2)

let segments_cross ((a, b) : Vec2.t * Vec2.t) ((c, d) : Vec2.t * Vec2.t) : bool =
  (* <= 0.: touching counts, an end on the other segment *)
  side a b c *. side a b d <= 0. && side c d a *. side c d b <= 0.
  (* ... but two segments on the same line only if they overlap *)
  && (side a b c <> 0. || side a b d <> 0.
     || (overlap_1d (fst a) (fst b) (fst c) (fst d) && overlap_1d (snd a) (snd b) (snd c) (snd d)))

let polygons_touch (p : Vec2.t list) (q : Vec2.t list) : bool =
  List.exists (fun e -> List.exists (fun f -> segments_cross e f) (Shape.edges q)) (Shape.edges p)
  || (match p with v :: _ -> point_in_polygon v q | [] -> false)
  || match q with v :: _ -> point_in_polygon v p | [] -> false

(* the shadow of a polygon on an axis: the min and max of its corners'
 * projections *)
let shadow (axis : Vec2.t) (corners : Vec2.t list) : float * float =
  List.fold_left (fun (lo, hi) v -> let d = Vec2.dot axis v in (Float.min lo d, Float.max hi d)) (infinity, neg_infinity) corners

let centroid (corners : Vec2.t list) : Vec2.t =
  Vec2.scale (1. /. float_of_int (List.length corners)) (List.fold_left Vec2.add (0., 0.) corners)

(* where segments ab and cd cross, if they do (not when parallel) *)
let crossing ((a, b) : Vec2.t * Vec2.t) ((c, d) : Vec2.t * Vec2.t) : Vec2.t option =
  let ab = Vec2.sub b a and cd = Vec2.sub d c and ac = Vec2.sub c a in
  let denom = Vec2.cross ab cd in
  if denom = 0. then None
  else
    (* a + t ab = c + u cd *)
    let t = Vec2.cross ac cd /. denom and u = Vec2.cross ac ab /. denom in
    if t >= 0. && t <= 1. && u >= 0. && u <= 1. then Some (Vec2.add a (Vec2.scale t ab)) else None

let overlap_middle (p : Vec2.t list) (q : Vec2.t list) : Vec2.t option =
  (* the corners of the overlap region: the corners of each inside the
   * other, and where their edges cross *)
  let inside = List.filter (fun v -> point_in_polygon v q) p @ List.filter (fun v -> point_in_polygon v p) q in
  let crossings = List.concat_map (fun e -> List.filter_map (crossing e) (Shape.edges q)) (Shape.edges p) in
  match inside @ crossings with [] -> None | points -> Some (centroid points)

let sat (p : Vec2.t list) (q : Vec2.t list) : Contact.t option =
  (* the axes: perpendicular to every edge of both *)
  let axes = List.map (fun (a, b) -> Vec2.normalize (Vec2.perp (Vec2.sub b a))) (Shape.edges p @ Shape.edges q) in
  (* the least overlap, and its axis; None as soon as one separates *)
  let rec go best = function
    | [] -> best
    | axis :: rest -> (
        let (p0, p1) = shadow axis p and (q0, q1) = shadow axis q in
        let overlap = Float.min p1 q1 -. Float.max p0 q0 in
        if overlap <= 0. then None
        else
          match best with
          | Some (_, o) when o <= overlap -> go best rest
          | _ -> go (Some (axis, overlap)) rest)
  in
  match go None axes with
  | None -> None
  | Some (axis, depth) ->
      (* the normal from p to q *)
      let normal = if Vec2.dot axis (Vec2.sub (centroid q) (centroid p)) < 0. then Vec2.scale (-1.) axis else axis in
      (* the middle of the overlap region; failing that (they only
       * touch along an edge, no area), q's corner deepest in p *)
      let deepest () = List.fold_left (fun best v -> if Vec2.dot normal v < Vec2.dot normal best then v else best) (List.hd q) q in
      let point = match overlap_middle p q with Some m -> m | None -> deepest () in
      Some { normal; depth; point }

(* the point of segment ab nearest to p *)
let nearest_on_segment (p : Vec2.t) ((a, b) : Vec2.t * Vec2.t) : Vec2.t =
  let ab = Vec2.sub b a in
  let len2 = Vec2.dot ab ab in
  if len2 = 0. then a
  else
    let t = Float.max 0. (Float.min 1. (Vec2.dot (Vec2.sub p a) ab /. len2)) in
    Vec2.add a (Vec2.scale t ab)

(* the point of the polygon's outline nearest to p *)
let nearest_on_outline (p : Vec2.t) (corners : Vec2.t list) : Vec2.t =
  let points = List.map (nearest_on_segment p) (Shape.edges corners) in
  List.fold_left
    (fun best q -> if Vec2.length (Vec2.sub q p) < Vec2.length (Vec2.sub best p) then q else best)
    (List.hd points) points

let circle_polygon ((c, r) : Vec2.t * float) (corners : Vec2.t list) : bool =
  point_in_polygon c corners || Vec2.length (Vec2.sub (nearest_on_outline c corners) c) <= r

let circle_convex ((c, r) : Vec2.t * float) (corners : Vec2.t list) : Contact.t option =
  let q = nearest_on_outline c corners in
  let d = Vec2.length (Vec2.sub q c) in
  if point_in_polygon c corners then
    (* the center inside: the polygon must go past the center, away from
     * the nearest edge *)
    let normal = if d = 0. then (1., 0.) else Vec2.scale (1. /. d) (Vec2.sub c q) in
    Some { normal; depth = r +. d; point = q }
  else if d < r then
    let normal = if d = 0. then (1., 0.) else Vec2.scale (1. /. d) (Vec2.sub q c) in
    Some { normal; depth = r -. d; point = q }
  else None

(*****************************************************************************)
(* Any two hitboxes *)
(*****************************************************************************)

let touching (a : Shape.placed) (b : Shape.placed) : bool =
  bounds_overlap (Shape.bounds a) (Shape.bounds b)
  &&
  match (a, b) with
  | Point_at p, Point_at q -> p = q
  | Point_at p, Circle_at (c, r) | Circle_at (c, r), Point_at p -> Vec2.length (Vec2.sub p c) <= r
  | Point_at p, Polygon_at q | Polygon_at q, Point_at p -> point_in_polygon p q
  | Circle_at (c1, r1), Circle_at (c2, r2) -> Vec2.length (Vec2.sub c2 c1) <= r1 +. r2
  | Circle_at (c, r), Polygon_at q | Polygon_at q, Circle_at (c, r) -> circle_polygon (c, r) q
  | Polygon_at p, Polygon_at q -> polygons_touch p q

let flip (c : Contact.t option) : Contact.t option =
  Option.map (fun (c : Contact.t) -> { c with normal = Vec2.scale (-1.) c.normal }) c

let contact (a : Shape.placed) (b : Shape.placed) : Contact.t option =
  (* a point is a circle of radius 0 *)
  let circle = function Shape.Point_at p -> Some (p, 0.) | Circle_at (c, r) -> Some (c, r) | Polygon_at _ -> None in
  match (a, b) with
  | Polygon_at p, Polygon_at q -> if Shape.convex p && Shape.convex q then sat p q else None
  | Polygon_at p, other -> (
      match circle other with Some c when Shape.convex p -> flip (circle_convex c p) | _ -> None)
  | other, Polygon_at q -> (
      match circle other with Some c when Shape.convex q -> circle_convex c q | _ -> None)
  | _ -> (
      match (circle a, circle b) with Some c1, Some c2 -> circles c1 c2 | _ -> None)

(* the corners of each polygon inside the other, each with its own
 * depth along the normal (from p to q), the two farthest apart kept *)
let polygon_manifold (p : Vec2.t list) (q : Vec2.t list) : Contact.t list =
  match sat p q with
  | None -> []
  | Some c ->
      let n = c.normal in
      let along v = Vec2.dot n v in
      (* how far p reaches towards q, and q back towards p *)
      let p_front = List.fold_left (fun m v -> Float.max m (along v)) neg_infinity p in
      let q_front = List.fold_left (fun m v -> Float.min m (along v)) infinity q in
      let points =
        List.filter_map (fun v -> if point_in_polygon v p then Some (v, p_front -. along v) else None) q
        @ List.filter_map (fun v -> if point_in_polygon v q then Some (v, along v -. q_front) else None) p
        |> List.filter (fun (_, depth) -> depth > 0.)
      in
      let contact (point, depth) : Contact.t = { normal = n; depth; point } in
      (match points with
      | [] -> [ c ]
      | [ _ ] | [ _; _ ] -> List.map contact points
      | first :: _ ->
          (* the two extremes along the contact's surface: the widest
           * support *)
          let t v = Vec2.cross n v in
          let pick better = List.fold_left (fun m x -> if better (t (fst x)) (t (fst m)) then x else m) first points in
          [ contact (pick ( < )); contact (pick ( > )) ])

let manifold (a : Shape.placed) (b : Shape.placed) : Contact.t list =
  match (a, b) with
  | Polygon_at p, Polygon_at q when Shape.convex p && Shape.convex q -> polygon_manifold p q
  | _ -> Option.to_list (contact a b)
