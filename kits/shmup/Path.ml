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
type t = Curve.t

let make (points : point list) : t = Curve.measure (Curve.through ~steps:16 points)
let length (p : t) : number = Curve.length p

let at (p : t) (s : number) : point * number =
  let pt, radians = Curve.at p s in
  (pt, radians * 180. / pi)

let mirror (points : point list) : point list = List.map (fun (x, y) -> (-.x, y)) points
