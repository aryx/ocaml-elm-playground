(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

open Steering

let distance ((ax, ay) : vec) ((bx, by) : vec) = Float.hypot (ax -. bx) (ay -. by)

let neighbours ~(radius : float) (others : vehicle list) (v : vehicle) : vehicle list =
  List.filter (fun o -> o != v && distance o.position v.position < radius) others

(* the average of some vectors *)
let mean (vs : vec list) : vec =
  let n = float_of_int (List.length vs) in
  let sx, sy = List.fold_left (fun (sx, sy) (x, y) -> (sx +. x, sy +. y)) (0., 0.) vs in
  (sx /. n, sy /. n)

let separation (near : vehicle list) (v : vehicle) : vec =
  (* away from each, the more the closer: 1/d along the line, so the sum
   * of (me - it) / d^2 *)
  let x, y = v.position in
  let push =
    List.fold_left
      (fun (px, py) o ->
        let ox, oy = o.position in
        let dx = x -. ox and dy = y -. oy in
        let d2 = Float.max 1e-6 ((dx *. dx) +. (dy *. dy)) in
        (px +. (dx /. d2), py +. (dy /. d2)))
      (0., 0.) near
  in
  if near = [] || push = (0., 0.) then v.velocity
  else
    let dx, dy = direction push in
    (v.max_speed *. dx, v.max_speed *. dy)

let alignment (near : vehicle list) (v : vehicle) : vec =
  if near = [] then v.velocity
  else
    let dx, dy = direction (mean (List.map (fun o -> o.velocity) near)) in
    (v.max_speed *. dx, v.max_speed *. dy)

let cohesion (near : vehicle list) (v : vehicle) : vec =
  if near = [] then v.velocity else seek (mean (List.map (fun o -> o.position) near)) v

(* the rules, under other names: [flock]'s weights are named after them *)
let separation_of = separation
let alignment_of = alignment
let cohesion_of = cohesion

let flock ?(separation = 1.5) ?(alignment = 1.) ?(cohesion = 1.) ~(radius : float) (others : vehicle list) (v : vehicle) : vec =
  let near = neighbours ~radius others v in
  let too_near = neighbours ~radius:(radius /. 2.) others v in
  blend
    [ (separation, steer v (separation_of too_near v));
      (alignment, steer v (alignment_of near v));
      (cohesion, steer v (cohesion_of near v)) ]
