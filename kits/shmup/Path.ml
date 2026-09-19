(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground
open Basics (* float arithmetics *)

(* See Path.mli *)

type point = number * number

let catmull_rom ((x0, y0) : point) ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) (t : number) : point =
  let f a b c d = 0.5 * ((2. * b) + ((c - a) * t) + (((2. * a) - (5. * b) + (4. * c) - d) * t * t) + (((3. * b) - a - (3. * c) + d) * t * t * t)) in
  (f x0 x1 x2 x3, f y0 y1 y2 y3)

type t = { pts : point array; lengths : number array }

let make (points : point list) : t =
  let a = Array.of_list points in
  let n = Array.length a in
  let get i = a.(max 0 (min (n -.. 1) i)) in
  let pts =
    Array.of_list
      (List.concat (List.init (n -.. 1) (fun i -> List.init 16 (fun k -> catmull_rom (get (i -.. 1)) (get i) (get (i +.. 1)) (get (i +.. 2)) (float_of_int k / 16.))))
      @ [ a.(n -.. 1) ])
  in
  let lengths = Array.make (Array.length pts) 0. in
  for i = 1 to Array.length pts -.. 1 do
    let (x0, y0), (x1, y1) = (pts.(i -.. 1), pts.(i)) in
    lengths.(i) <- lengths.(i -.. 1) + Float.hypot (x1 - x0) (y1 - y0)
  done;
  { pts; lengths }

let length (p : t) : number = p.lengths.(Array.length p.lengths -.. 1)

let at (p : t) (s : number) : point * number =
  let n = Array.length p.pts in
  let rec find i = if i < n -.. 1 && p.lengths.(i) < s then find (i +.. 1) else i in
  let i = max 1 (find 1) in
  let (x0, y0), (x1, y1) = (p.pts.(i -.. 1), p.pts.(i)) in
  let seg = p.lengths.(i) - p.lengths.(i -.. 1) in
  let f = if seg > 0. then clamp 0. 1. ((s - p.lengths.(i -.. 1)) / seg) else 1. in
  ((x0 + (f * (x1 - x0)), y0 + (f * (y1 - y0))), atan2 (y1 - y0) (x1 - x0) * 180. / pi)

let mirror (points : point list) : point list = List.map (fun (x, y) -> (-.x, y)) points
