(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Curve.mli *)

type point = float * float

(* {1 Evaluating} *)

let cubic ((x0, y0) : point) ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) (t : float) : point =
  let u = 1. -. t in
  let f a b c d = (u *. u *. u *. a) +. (3. *. u *. u *. t *. b) +. (3. *. u *. t *. t *. c) +. (t *. t *. t *. d) in
  (f x0 x1 x2 x3, f y0 y1 y2 y3)

let quadratic ((x0, y0) : point) ((x1, y1) : point) ((x2, y2) : point) (t : float) : point =
  let u = 1. -. t in
  let f a b c = (u *. u *. a) +. (2. *. u *. t *. b) +. (t *. t *. c) in
  (f x0 x1 x2, f y0 y1 y2)

let catmull_rom ((x0, y0) : point) ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) (t : float) : point =
  let f a b c d = 0.5 *. ((2. *. b) +. ((c -. a) *. t) +. (((2. *. a) -. (5. *. b) +. (4. *. c) -. d) *. t *. t) +. (((3. *. b) -. a -. (3. *. c) +. d) *. t *. t *. t)) in
  (f x0 x1 x2 x3, f y0 y1 y2 y3)

(* a sixth of the span between the neighbors is the pull that makes the
 * Bézier tangents match the Catmull-Rom ones *)
let cubic_of_catmull_rom (p0 : point) (p1 : point) (p2 : point) (p3 : point) : point * point * point * point =
  (p1, Vec2.add p1 (Vec2.scale (1. /. 6.) (Vec2.sub p2 p0)), Vec2.sub p2 (Vec2.scale (1. /. 6.) (Vec2.sub p3 p1)), p2)

(* {1 Flattening} *)

(* how far [p] is from the chord [a] -> [b] (from [a] itself, for a
 * chord of no length: a curve that comes back to where it started) *)
let distance_to_chord (a : point) (b : point) (p : point) : float =
  let chord = Vec2.sub b a in
  let n = Vec2.length chord in
  if n = 0. then Vec2.length (Vec2.sub p a) else Float.abs (Vec2.cross chord (Vec2.sub p a)) /. n

let flatten ?(tolerance = 0.1) (p0 : point) (p1 : point) (p2 : point) (p3 : point) : point list =
  let flat_enough a b c d = distance_to_chord a d b <= tolerance && distance_to_chord a d c <= tolerance in
  (* de Casteljau: the halves of a curve, found with midpoints only *)
  let rec go depth a b c d acc =
    if depth = 0 || flat_enough a b c d then d :: acc
    else
      let mid = Vec2.scale 0.5 (Vec2.add b c) in
      let lb = Vec2.scale 0.5 (Vec2.add a b) in
      let rc = Vec2.scale 0.5 (Vec2.add c d) in
      let lc = Vec2.scale 0.5 (Vec2.add lb mid) in
      let rb = Vec2.scale 0.5 (Vec2.add mid rc) in
      let m = Vec2.scale 0.5 (Vec2.add lc rb) in
      go (depth - 1) a lb lc m (go (depth - 1) m rb rc d acc)
  in
  (* claude: a depth of 16 would be 65536 segments, far past any
   * tolerance worth asking for: it is there to end, not to be reached *)
  p0 :: go 16 p0 p1 p2 p3 []

let through ?(steps = 16) (points : point list) : point list =
  let a = Array.of_list points in
  let n = Array.length a in
  let get i = a.(max 0 (min (n - 1) i)) in
  List.concat (List.init (n - 1) (fun i -> List.init steps (fun k -> catmull_rom (get (i - 1)) (get i) (get (i + 1)) (get (i + 2)) (float_of_int k /. float_of_int steps))))
  @ [ a.(n - 1) ]

(* {1 Walking} *)

type t = { pts : point array; lengths : float array }

let measure (polyline : point list) : t =
  let pts = Array.of_list polyline in
  let lengths = Array.make (Array.length pts) 0. in
  for i = 1 to Array.length pts - 1 do
    let (x0, y0), (x1, y1) = (pts.(i - 1), pts.(i)) in
    lengths.(i) <- lengths.(i - 1) +. Float.hypot (x1 -. x0) (y1 -. y0)
  done;
  { pts; lengths }

let length (c : t) : float = c.lengths.(Array.length c.lengths - 1)

let at (c : t) (s : float) : point * float =
  let n = Array.length c.pts in
  if n < 2 then (c.pts.(0), 0.)
  else
    let rec find i = if i < n - 1 && c.lengths.(i) < s then find (i + 1) else i in
    let i = max 1 (find 1) in
    let (x0, y0), (x1, y1) = (c.pts.(i - 1), c.pts.(i)) in
    let seg = c.lengths.(i) -. c.lengths.(i - 1) in
    let f = if seg > 0. then Float.max 0. (Float.min 1. ((s -. c.lengths.(i - 1)) /. seg)) else 1. in
    ((x0 +. (f *. (x1 -. x0)), y0 +. (f *. (y1 -. y0))), Float.atan2 (y1 -. y0) (x1 -. x0))
