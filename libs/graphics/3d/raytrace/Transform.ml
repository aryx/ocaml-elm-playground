(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Transform.mli *)

(* a 3 x 3 matrix, row by row *)
type m3 = float array

(* p -> m p + o, and its inverse p -> inv_m p + inv_o *)
type t = { m : m3; o : Vec3.t; inv_m : m3; inv_o : Vec3.t }

let id3 : m3 = [| 1.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 1. |]

let apply (m : m3) ((x, y, z) : Vec3.t) : Vec3.t =
  ((m.(0) *. x) +. (m.(1) *. y) +. (m.(2) *. z), (m.(3) *. x) +. (m.(4) *. y) +. (m.(5) *. z),
   (m.(6) *. x) +. (m.(7) *. y) +. (m.(8) *. z))

(* the transpose applied: m^T v *)
let apply_transposed (m : m3) ((x, y, z) : Vec3.t) : Vec3.t =
  ((m.(0) *. x) +. (m.(3) *. y) +. (m.(6) *. z), (m.(1) *. x) +. (m.(4) *. y) +. (m.(7) *. z),
   (m.(2) *. x) +. (m.(5) *. y) +. (m.(8) *. z))

let mul (a : m3) (b : m3) : m3 =
  Array.init 9 (fun k ->
      let i = k / 3 and j = k mod 3 in
      (a.((3 * i) + 0) *. b.(j)) +. (a.((3 * i) + 1) *. b.(3 + j)) +. (a.((3 * i) + 2) *. b.(6 + j)))

let identity = { m = id3; o = (0., 0., 0.); inv_m = id3; inv_o = (0., 0., 0.) }
let translate (v : Vec3.t) : t = { identity with o = v; inv_o = Vec3.scale (-1.) v }

let scale ((x, y, z) : Vec3.t) : t =
  if x = 0. || y = 0. || z = 0. then invalid_arg "Transform.scale: a zero flattens the solid";
  { identity with m = [| x; 0.; 0.; 0.; y; 0.; 0.; 0.; z |]; inv_m = [| 1. /. x; 0.; 0.; 0.; 1. /. y; 0.; 0.; 0.; 1. /. z |] }

let rotate (axis : int) (degrees : float) : t =
  let r a =
    let c = cos a and s = sin a in
    match axis with
    | 0 -> [| 1.; 0.; 0.; 0.; c; -.s; 0.; s; c |]
    | 1 -> [| c; 0.; s; 0.; 1.; 0.; -.s; 0.; c |]
    | _ -> [| c; -.s; 0.; s; c; 0.; 0.; 0.; 1. |]
  in
  let a = degrees *. Float.pi /. 180. in
  { identity with m = r a; inv_m = r (-.a) }

(* b first, then a: p -> ma (mb p + ob) + oa; undone by b's inverse
 * after a's *)
let compose (a : t) (b : t) : t =
  { m = mul a.m b.m; o = Vec3.add (apply a.m b.o) a.o; inv_m = mul b.inv_m a.inv_m;
    inv_o = Vec3.add (apply b.inv_m a.inv_o) b.inv_o }

let point (t : t) (p : Vec3.t) : Vec3.t = Vec3.add (apply t.m p) t.o
let direction (t : t) (d : Vec3.t) : Vec3.t = apply t.m d
let inverse_point (t : t) (p : Vec3.t) : Vec3.t = Vec3.add (apply t.inv_m p) t.inv_o
let inverse_direction (t : t) (d : Vec3.t) : Vec3.t = apply t.inv_m d
let normal (t : t) (n : Vec3.t) : Vec3.t = Vec3.normalize (apply_transposed t.inv_m n)
let is_translation (t : t) : bool = t.m = id3
