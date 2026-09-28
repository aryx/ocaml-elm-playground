(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cad_geom.mli *)

type pt = float * float

let add (ax, ay) (bx, by) = (ax +. bx, ay +. by)
let sub (ax, ay) (bx, by) = (ax -. bx, ay -. by)
let scale k (x, y) = (k *. x, k *. y)
let dot (ax, ay) (bx, by) = (ax *. bx) +. (ay *. by)
let cross (ax, ay) (bx, by) = (ax *. by) -. (ay *. bx)
let dist a b = Float.hypot (fst b -. fst a) (snd b -. snd a)

let unit ((x, y) as v) =
  let l = Float.hypot x y in
  if l = 0. then v else (x /. l, y /. l)

let rad a = a *. Float.pi /. 180.
let deg a = a *. 180. /. Float.pi
let polar (x, y) d a = (x +. (d *. Float.cos (rad a)), y +. (d *. Float.sin (rad a)))

let norm_angle a =
  let a = Float.rem a 360. in
  if a < 0. then a +. 360. else a

let angle (ax, ay) (bx, by) = norm_angle (deg (Float.atan2 (by -. ay) (bx -. ax)))

(* a little slack, in degrees and in units: a crossing computed on an
   arc's end is on it *)
let eps = 1e-7

let within a0 a1 a =
  let span = norm_angle (a1 -. a0) and d = norm_angle (a -. a0) in
  let span = if span = 0. then 360. else span in
  d <= span +. eps || d >= 360. -. eps

type curve = Segment of pt * pt | Circle of pt * float | Arc of pt * float * float * float

let arc_ends c r a0 a1 = (polar c r a0, polar c r a1)

(*****************************************************************************)
(* Crossings *)
(*****************************************************************************)

let lines a b c d =
  let r = sub b a and s = sub d c in
  let den = cross r s in
  if Float.abs den < 1e-12 then []
  else
    let t = cross (sub c a) s /. den in
    [ add a (scale t r) ]

let line_circle a b center r =
  let d = sub b a and f = sub a center in
  let qa = dot d d and qb = 2. *. dot f d and qc = dot f f -. (r *. r) in
  let disc = (qb *. qb) -. (4. *. qa *. qc) in
  if qa = 0. || disc < -1e-9 then []
  else if disc <= 1e-9 then [ add a (scale (-.qb /. (2. *. qa)) d) ]
  else
    let s = Float.sqrt disc in
    [ add a (scale ((-.qb -. s) /. (2. *. qa)) d); add a (scale ((-.qb +. s) /. (2. *. qa)) d) ]

let circles c1 r1 c2 r2 =
  let d = dist c1 c2 in
  if d = 0. || d > r1 +. r2 +. 1e-9 || d < Float.abs (r1 -. r2) -. 1e-9 then []
  else
    let a = ((d *. d) +. (r1 *. r1) -. (r2 *. r2)) /. (2. *. d) in
    let h = Float.sqrt (Float.max 0. ((r1 *. r1) -. (a *. a))) in
    let u = unit (sub c2 c1) in
    let m = add c1 (scale a u) and n = (-.snd u, fst u) in
    if h < 1e-9 then [ m ] else [ add m (scale h n); sub m (scale h n) ]

let round_of = function Circle (c, r) | Arc (c, r, _, _) -> Some (c, r) | Segment _ -> None

let carrier_intersections x y =
  match (x, y) with
  | Segment (a, b), Segment (c, d) -> lines a b c d
  | Segment (a, b), other | other, Segment (a, b) -> (
      match round_of other with Some (c, r) -> line_circle a b c r | None -> [])
  | _ -> (
      match (round_of x, round_of y) with Some (c1, r1), Some (c2, r2) -> circles c1 r1 c2 r2 | _ -> [])

(* where p is along a segment: 0 at its start, 1 at its end *)
let param a b p =
  let d = sub b a in
  let l2 = dot d d in
  if l2 = 0. then 0. else dot (sub p a) d /. l2

let on_piece curve p =
  match curve with
  | Segment (a, b) ->
      let t = param a b p in
      t >= -.eps && t <= 1. +. eps
  | Circle _ -> true
  | Arc (c, _, a0, a1) -> within a0 a1 (angle c p)

let intersections x y = List.filter (fun p -> on_piece x p && on_piece y p) (carrier_intersections x y)

(*****************************************************************************)
(* Distances *)
(*****************************************************************************)

let foot curve p =
  match curve with
  | Segment (a, b) -> add a (scale (param a b p) (sub b a))
  | Circle (c, r) | Arc (c, r, _, _) -> if dist c p = 0. then polar c r 0. else polar c r (angle c p)

let nearest curve p =
  match curve with
  | Segment (a, b) -> add a (scale (Float.max 0. (Float.min 1. (param a b p))) (sub b a))
  | Circle _ -> foot curve p
  | Arc (c, r, a0, a1) ->
      let q = foot curve p in
      if within a0 a1 (angle c q) then q
      else
        let s, e = arc_ends c r a0 a1 in
        if dist p s <= dist p e then s else e

let distance curve p = dist p (nearest curve p)
