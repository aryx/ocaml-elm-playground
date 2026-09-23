(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type vec = float * float
type vehicle = { position : vec; velocity : vec; max_speed : float; max_force : float }

(*****************************************************************************)
(* Vectors *)
(*****************************************************************************)

let add (ax, ay) (bx, by) = (ax +. bx, ay +. by)
let sub (ax, ay) (bx, by) = (ax -. bx, ay -. by)
let scale k (x, y) = (k *. x, k *. y)
let dot (ax, ay) (bx, by) = (ax *. bx) +. (ay *. by)
let length (x, y) = Float.sqrt ((x *. x) +. (y *. y))

let direction (v : vec) : vec =
  let l = length v in
  if l = 0. then (0., 0.) else scale (1. /. l) v

(* [v] no longer than [max] *)
let clamp (max : float) (v : vec) : vec = if length v > max then scale max (direction v) else v

let heading (v : vehicle) : vec = if length v.velocity = 0. then (1., 0.) else direction v.velocity

(*****************************************************************************)
(* Behaviours *)
(*****************************************************************************)

let seek (target : vec) (v : vehicle) : vec = scale v.max_speed (direction (sub target v.position))
let flee (target : vec) (v : vehicle) : vec = scale (-.v.max_speed) (direction (sub target v.position))

let arrive ?(slowing = 100.) (target : vec) (v : vehicle) : vec =
  let offset = sub target v.position in
  let d = length offset in
  let speed = if d < slowing then v.max_speed *. d /. slowing else v.max_speed in
  scale speed (direction offset)

(* where [target] will be when [v] gets there: its distance over [v]'s
 * top speed, the time to catch it *)
let predicted (target : vehicle) (v : vehicle) : vec =
  let time = length (sub target.position v.position) /. v.max_speed in
  add target.position (scale time target.velocity)

let pursue (target : vehicle) (v : vehicle) : vec = seek (predicted target v) v
let evade (target : vehicle) (v : vehicle) : vec = flee (predicted target v) v

let wander ?(distance = 80.) ?(radius = 40.) ~(angle : float) (v : vehicle) : vec =
  let hx, hy = heading v in
  let centre = add v.position (scale distance (hx, hy)) in
  (* the angle is from the heading: turned by it *)
  let c = Float.cos angle and s = Float.sin angle in
  let point = add centre (scale radius ((c *. hx) -. (s *. hy), (s *. hx) +. (c *. hy))) in
  seek point v

let avoid ?(ahead = 100.) ?(size = 10.) (obstacles : (vec * float) list) (v : vehicle) : vec =
  let h = heading v in
  let left = (-.snd h, fst h) in
  (* each obstacle in [v]'s frame: how far ahead, how far to the left *)
  let in_the_way =
    List.filter_map
      (fun (centre, r) ->
        let offset = sub centre v.position in
        let along = dot offset h and side = dot offset left in
        if along > 0. && along < ahead +. r && Float.abs side < r +. size then Some (along, side, r) else None)
      obstacles
  in
  match List.sort compare in_the_way with
  | [] -> v.velocity
  | (along, side, r) :: _ ->
      (* away from its side (to the right when dead ahead), the harder
       * the nearer: 1 touching it, 0 at the corridor's end *)
      let away = if side >= 0. then scale (-1.) left else left in
      let urgency = 1. -. (along /. (ahead +. r)) in
      add (scale v.max_speed h) (scale (2. *. urgency *. v.max_speed) away)

(* the point of segment [a]-[b] nearest to [p] *)
let nearest_on (a : vec) (b : vec) (p : vec) : vec =
  let ab = sub b a in
  let l2 = dot ab ab in
  if l2 = 0. then a else add a (scale (Float.max 0. (Float.min 1. (dot (sub p a) ab /. l2))) ab)

let follow ?(ahead = 50.) ~(width : float) (path : vec list) (v : vehicle) : vec =
  let future = add v.position (scale ahead (heading v)) in
  let rec segments = function a :: (b :: _ as rest) -> (a, b) :: segments rest | _ -> [] in
  match segments path with
  | [] -> ( match path with [ p ] -> arrive p v | _ -> v.velocity)
  | segs ->
      let (point, (a, b)) =
        List.fold_left
          (fun (best, seg) (a, b) ->
            let q = nearest_on a b future in
            if length (sub q future) < length (sub best future) then (q, (a, b)) else (best, seg))
          (nearest_on (fst (List.hd segs)) (snd (List.hd segs)) future, List.hd segs)
          segs
      in
      if length (sub point future) <= width then v.velocity else seek (add point (scale ahead (direction (sub b a)))) v

(*****************************************************************************)
(* Forces *)
(*****************************************************************************)

let steer (v : vehicle) (desired : vec) : vec = clamp v.max_force (sub desired v.velocity)
let blend (forces : (float * vec) list) : vec = List.fold_left (fun acc (w, f) -> add acc (scale w f)) (0., 0.) forces

let move ~(dt : float) (force : vec) (v : vehicle) : vehicle =
  let velocity = clamp v.max_speed (add v.velocity (scale dt force)) in
  { v with velocity; position = add v.position (scale dt velocity) }
