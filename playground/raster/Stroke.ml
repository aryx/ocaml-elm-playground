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
 * width/2 on each side, perpendicularly to the segment. (dx, dy) along
 * the segment, (-dy, dx) is perpendicular to it. *)
let segment_rectangle (x0, y0) (x1, y1) ~width : (float * float) list option =
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let length = Float.hypot dx dy in
  if length = 0. then None
  else
    let nx = -.dy /. length *. width /. 2. and ny = dx /. length *. width /. 2. in
    Some [ (x0 +. nx, y0 +. ny); (x1 +. nx, y1 +. ny); (x1 -. nx, y1 -. ny); (x0 -. nx, y0 -. ny) ]

let disk (cx, cy) ~width : (float * float) list =
  let r = width /. 2. in
  Circle.ellipse_points ~rx:r ~ry:r ~segments:(Circle.segments_for_radius r)
  |> List.map (fun (x, y) -> (cx +. x, cy +. y))

(* Twice the signed area of a polygon (the "shoelace formula"): its
 * sign says which way the polygon turns *)
let signed_area (points : (float * float) list) : float =
  match points with
  | [] -> 0.
  | first :: _ ->
      let rec loop acc = function
        | (x0, y0) :: ((x1, y1) :: _ as rest) -> loop (acc +. ((x0 *. y1) -. (x1 *. y0))) rest
        | [ (x0, y0) ] ->
            let x1, y1 = first in
            acc +. ((x0 *. y1) -. (x1 *. y0))
        | [] -> acc
      in
      loop 0. points

(* All turned the same way, so that Nonzero adds them up *)
let same_way (points : (float * float) list) : (float * float) list =
  if signed_area points < 0. then List.rev points else points

let polylines (fb : Framebuffer.t) (lines : (float * float) list list) ~width ~rgb ~alpha =
  let rec segments = function
    | p :: (q :: _ as rest) -> Option.to_list (segment_rectangle p q ~width) @ segments rest
    | [ _ ] | [] -> []
  in
  let contours =
    List.concat (List.map (fun line -> segments line @ List.map (disk ~width) line) lines)
  in
  Fill.polygons ~rule:Nonzero fb (List.map same_way contours) ~rgb ~alpha
