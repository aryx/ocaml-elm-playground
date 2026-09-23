(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Juice.mli *)

open Playground

(* the flag juice=off: every effect does nothing *)
let off (computer : computer) : bool = List.assoc_opt "juice" computer.flags = Some "off"

(*****************************************************************************)
(* Effects as functions of time *)
(*****************************************************************************)

type ease = Ease.t

let linear = Ease.linear

let in_quad = Ease.quad
let out_quad = Ease.out Ease.quad
let in_out_quad = Ease.in_out Ease.quad

let in_cubic = Ease.cubic
let out_cubic = Ease.out Ease.cubic
let in_out_cubic = Ease.in_out Ease.cubic

let in_sine = Ease.sine
let out_sine = Ease.out Ease.sine
let in_out_sine = Ease.in_out Ease.sine

let in_back = Ease.back
let out_back = Ease.out Ease.back
let in_out_back = Ease.in_out Ease.back

let in_elastic = Ease.elastic
let out_elastic = Ease.out Ease.elastic
let in_out_elastic = Ease.in_out Ease.elastic

let in_bounce = Ease.bounce
let out_bounce = Ease.out Ease.bounce
let in_out_bounce = Ease.in_out Ease.bounce

let curve (ease : ease) (t : number) : number = ease t

let tween (ease : ease) (from : number) (to_ : number) (seconds : number) (Time started : time) (computer : computer) :
    number =
  if off computer then to_
  else
    let (Time now) = computer.time in
    Tween.value ease from to_ ~start:started ~duration:seconds now

let squash (amount : number) (seconds : number) (Time landed : time) (computer : computer) : number * number =
  if off computer then (1., 1.)
  else
    let (Time now) = computer.time in
    Squash.keep_area (Squash.landing ~amount (Tween.progress ~start:landed ~duration:seconds now))

(* Stretching a shape tree. A shape is scaled, then rotated, then moved
 * inside its parent, all of which a 2x2 matrix and a translation can
 * say; a stretch is one more matrix, and it is pushed down the tree:
 * the shape's position goes through it, and what is left -- the
 * stretch times the shape's own rotation and scale -- is given to its
 * form. As long as that matrix is diagonal (no rotation in the way), a
 * circle becomes an oval and a rectangle a longer one, exactly; when
 * it is not, the form becomes the polygon it is. *)

(* (x, y) -> (a x + b y, c x + d y) *)
type linear = { a : number; b : number; c : number; d : number }

let apply (m : linear) ((x, y) : number * number) : number * number = ((m.a *. x) +. (m.b *. y), (m.c *. x) +. (m.d *. y))

(* [m] after [n] *)
let compose (m : linear) (n : linear) : linear =
  { a = (m.a *. n.a) +. (m.b *. n.c); b = (m.a *. n.b) +. (m.b *. n.d); c = (m.c *. n.a) +. (m.d *. n.c); d = (m.c *. n.b) +. (m.d *. n.d) }

(* the shape's own rotation and scale, as a matrix *)
let own (s : shape) : linear =
  let t = s.angle *. Float.pi /. 180. in
  let cs = s.scale *. cos t and sn = s.scale *. sin t in
  { a = cs; b = -.sn; c = sn; d = cs }

let diagonal (m : linear) : bool = Float.abs m.b < 1e-9 && Float.abs m.c < 1e-9

let ellipse (rx : number) (ry : number) : (number * number) list =
  List.init 32 (fun i ->
      let t = 2. *. Float.pi *. float_of_int i /. 32. in
      (rx *. cos t, ry *. sin t))

(* as Playground draws them: the first corner at the top, then clockwise *)
let ngon (n : int) (r : number) : (number * number) list =
  List.init n (fun i ->
      let t = (90. -. (360. *. float_of_int i /. float_of_int n)) *. Float.pi /. 180. in
      (r *. cos t, r *. sin t))

let rec transform (m : linear) (s : shape) : shape =
  let x, y = apply m (s.x, s.y) in
  let l = compose m (own s) in
  let baked form = { x; y; angle = 0.; scale = 1.; alpha = s.alpha; form } in
  let polygon color points = baked (Polygon (color, List.map (apply l) points)) in
  (* the rotation and even scale nearest to [l]: for what can only be
   * scaled evenly *)
  let even () = { s with x; y; angle = atan2 l.c l.a *. 180. /. Float.pi; scale = sqrt (Float.abs ((l.a *. l.d) -. (l.b *. l.c))) } in
  match s.form with
  | Group shapes -> baked (Group (List.map (transform l) shapes))
  | Words _ -> even ()
  | Image (w, h, url) when diagonal l -> baked (Image (w *. Float.abs l.a, h *. Float.abs l.d, url))
  | Image _ -> even ()
  | Circle (color, r) when diagonal l -> baked (Oval (color, 2. *. r *. Float.abs l.a, 2. *. r *. Float.abs l.d))
  | Oval (color, w, h) when diagonal l -> baked (Oval (color, w *. Float.abs l.a, h *. Float.abs l.d))
  | Rectangle (color, w, h) when diagonal l -> baked (Rectangle (color, w *. Float.abs l.a, h *. Float.abs l.d))
  | Circle (color, r) -> polygon color (ellipse r r)
  | Oval (color, w, h) -> polygon color (ellipse (w /. 2.) (h /. 2.))
  | Rectangle (color, w, h) -> polygon color [ (-.w /. 2., h /. 2.); (w /. 2., h /. 2.); (w /. 2., -.h /. 2.); (-.w /. 2., -.h /. 2.) ]
  | Ngon (color, n, r) -> polygon color (ngon n r)
  | Polygon (color, points) -> polygon color points

let stretch ((across, up) : number * number) (s : shape) : shape = transform { a = across; b = 0.; c = 0.; d = up } s

let rec whiten (s : shape) : shape =
  let form =
    match s.form with
    | Circle (_, r) -> Circle (white, r)
    | Oval (_, w, h) -> Oval (white, w, h)
    | Rectangle (_, w, h) -> Rectangle (white, w, h)
    | Ngon (_, n, r) -> Ngon (white, n, r)
    | Polygon (_, points) -> Polygon (white, points)
    | Words (_, text) -> Words (white, text)
    | Image _ as image -> image
    | Group shapes -> Group (List.map whiten shapes)
  in
  { s with form }
