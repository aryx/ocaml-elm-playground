(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ps_graphics.mli *)

type matrix = { a : float; b : float; c : float; d : float; tx : float; ty : float }

let identity = { a = 1.; b = 0.; c = 0.; d = 1.; tx = 0.; ty = 0. }
let translation tx ty = { identity with tx; ty }
let scaling sx sy = { identity with a = sx; d = sy }

let rotation deg =
  let r = deg *. Float.pi /. 180. in
  { identity with a = Float.cos r; b = Float.sin r; c = -.Float.sin r; d = Float.cos r }

let transform m (x, y) = ((m.a *. x) +. (m.c *. y) +. m.tx, (m.b *. x) +. (m.d *. y) +. m.ty)
let dtransform m (x, y) = ((m.a *. x) +. (m.c *. y), (m.b *. x) +. (m.d *. y))

(* the matrix of "m, then ctm": ctm's linear part times m's *)
let concat m ctm =
  let tx, ty = transform ctm (m.tx, m.ty) in
  {
    a = (ctm.a *. m.a) +. (ctm.c *. m.b);
    b = (ctm.b *. m.a) +. (ctm.d *. m.b);
    c = (ctm.a *. m.c) +. (ctm.c *. m.d);
    d = (ctm.b *. m.c) +. (ctm.d *. m.d);
    tx;
    ty;
  }

let invert m =
  let det = (m.a *. m.d) -. (m.b *. m.c) in
  if det = 0. then identity
  else
    let a = m.d /. det and b = -.m.b /. det and c = -.m.c /. det and d = m.a /. det in
    { a; b; c; d; tx = -.((a *. m.tx) +. (c *. m.ty)); ty = -.((b *. m.tx) +. (d *. m.ty)) }

let scale_of m = Float.sqrt (Float.abs ((m.a *. m.d) -. (m.b *. m.c)))

type point = float * float
type segment = Move of point | Line of point | Curve of point * point * point | Close

let arc (cx, cy) r a1 a2 ~clockwise =
  let rad d = d *. Float.pi /. 180. in
  (* the sweep, in the arc's direction, at most a turn *)
  let sweep =
    if clockwise then -.Float.rem (Float.rem (a1 -. a2) 360. +. 360.) 360. else Float.rem (Float.rem (a2 -. a1) 360. +. 360.) 360.
  in
  let sweep = if sweep = 0. && a1 <> a2 then if clockwise then -360. else 360. else sweep in
  let pieces = max 1 (int_of_float (Float.ceil (Float.abs sweep /. 90.))) in
  let step = sweep /. float_of_int pieces in
  let at deg = (cx +. (r *. Float.cos (rad deg)), cy +. (r *. Float.sin (rad deg))) in
  (* the tangent's length for a piece of [step] degrees: 4/3 tan(step/4) *)
  let k = 4. /. 3. *. Float.tan (rad step /. 4.) *. r in
  let piece i =
    let t0 = a1 +. (step *. float_of_int i) and t1 = a1 +. (step *. float_of_int (i + 1)) in
    let (x0, y0), (x3, y3) = (at t0, at t1) in
    let c1 = (x0 -. (k *. Float.sin (rad t0)), y0 +. (k *. Float.cos (rad t0))) in
    let c2 = (x3 +. (k *. Float.sin (rad t1)), y3 -. (k *. Float.cos (rad t1))) in
    (c1, c2, (x3, y3))
  in
  (at a1, List.init pieces piece)

let mid (x0, y0) (x1, y1) = ((x0 +. x1) /. 2., (y0 +. y1) /. 2.)

(* how far a point is from the line through two others *)
let distance (px, py) (x0, y0) (x1, y1) =
  let dx = x1 -. x0 and dy = y1 -. y0 in
  let len = Float.sqrt ((dx *. dx) +. (dy *. dy)) in
  if len = 0. then Float.sqrt (((px -. x0) ** 2.) +. ((py -. y0) ** 2.)) else Float.abs ((dx *. (y0 -. py)) -. ((x0 -. px) *. dy)) /. len

(* de Casteljau: the curve's two halves are Beziers too, their points
   averages of averages; a piece whose control points hug its chord is
   a line *)
let rec bezier tolerance depth p0 p1 p2 p3 : point list =
  if depth > 12 || (distance p1 p0 p3 <= tolerance && distance p2 p0 p3 <= tolerance) then [ p3 ]
  else
    let p01 = mid p0 p1 and p12 = mid p1 p2 and p23 = mid p2 p3 in
    let p012 = mid p01 p12 and p123 = mid p12 p23 in
    let m = mid p012 p123 in
    bezier tolerance (depth + 1) p0 p01 p012 m @ bezier tolerance (depth + 1) m p123 p23 p3

let flatten ?(tolerance = 0.25) (segments : segment list) : (point list * bool) list =
  (* the polylines finished, and the one being built (reversed) *)
  let finish acc current closed = match current with [] | [ _ ] -> acc | pts -> (List.rev pts, closed) :: acc in
  let rec go acc current segments =
    match segments with
    | [] -> List.rev (finish acc current false)
    | Move p :: rest -> go (finish acc current false) [ p ] rest
    | Line p :: rest -> go acc (p :: current) rest
    | Curve (p1, p2, p3) :: rest ->
        let p0 = match current with p :: _ -> p | [] -> p1 in
        go acc (List.rev_append (bezier tolerance 0 p0 p1 p2 p3) current) rest
    | Close :: rest ->
        let start = match List.rev current with p :: _ -> [ p ] | [] -> [] in
        go (finish acc current true) start rest
  in
  go [] [] segments
