(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Force.mli *)

type t = Vec2.t -> Vec2.t -> Vec2.t

let none : t = fun _pos _vel -> (0., 0.)
let uniform (g : Vec2.t) : t = fun _pos _vel -> g

let gravitation ~(gm : float) ~(center : Vec2.t) : t =
 fun pos _vel ->
  let d = Vec2.sub center pos in
  let r = Vec2.length d in
  (* gm / r^2 along the unit vector d / r *)
  Vec2.scale (gm /. (r *. r *. r)) d

let spring ~(k_over_m : float) ~(anchor : Vec2.t) : t =
 fun pos _vel -> Vec2.scale (-.k_over_m) (Vec2.sub pos anchor)

let drag ~(c : float) : t = fun _pos vel -> Vec2.scale (-.c) vel

let sum (forces : t list) : t =
 fun pos vel -> List.fold_left (fun acc (f : t) -> Vec2.add acc (f pos vel)) (0., 0.) forces
