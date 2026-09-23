(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Stroke.mli for the idea, with a picture *)

(* The rectangle around the segment from p0 to p1: p0 and p1 moved by
 * width/2 on each side, perpendicularly to the segment (along the
 * segment's direction turned a quarter turn, Vec2.perp) *)
let segment_rectangle (p0 : Vec2.t) (p1 : Vec2.t) ~width : Vec2.t list option =
  let along = Vec2.sub p1 p0 in
  if Vec2.length along = 0. then None
  else
    let side = Vec2.scale (width /. 2.) (Vec2.normalize (Vec2.perp along)) in
    Some [ Vec2.add p0 side; Vec2.add p1 side; Vec2.sub p1 side; Vec2.sub p0 side ]

let disk (center : Vec2.t) ~width : Vec2.t list =
  let r = width /. 2. in
  Circle.ellipse_points ~rx:r ~ry:r ~segments:(Circle.segments_for_radius r)
  |> List.map (Vec2.add center)

(* Twice the signed area of a polygon (the "shoelace formula"): the sum
 * of the 2D cross products of consecutive points, each twice the signed
 * area of the triangle they make with the origin; its sign says which
 * way the polygon turns *)
let signed_area (points : Vec2.t list) : float =
  match points with
  | [] -> 0.
  | first :: rest ->
      (* each point with the next one, the last with the first *)
      List.combine points (rest @ [ first ])
      |> List.fold_left (fun acc (p, q) -> acc +. Vec2.cross p q) 0.

(* All turned the same way, so that Nonzero adds them up *)
let same_way (points : Vec2.t list) : Vec2.t list =
  if signed_area points < 0. then List.rev points else points

let contours (lines : (float * float) list list) ~width : (float * float) list list =
  let rec segments = function
    | p :: (q :: _ as rest) -> Option.to_list (segment_rectangle p q ~width) @ segments rest
    | [ _ ] | [] -> []
  in
  List.concat (List.map (fun line -> segments line @ List.map (disk ~width) line) lines)
  |> List.map same_way

let polylines (fb : Framebuffer.t) (lines : (float * float) list list) ~width ~rgb ~alpha =
  Fill.polygons ~rule:Nonzero fb (contours lines ~width) ~rgb ~alpha
