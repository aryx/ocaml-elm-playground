(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Force3d.mli *)

type t = Vec3.t -> Vec3.t -> Vec3.t

let none _ _ = (0., 0., 0.)
let uniform a _ _ = a

let gravitation ~gm ~center pos _ =
  let d = Vec3.sub center pos in
  let r = Vec3.length d in
  if r < 1e-9 then (0., 0., 0.) else Vec3.scale (gm /. (r *. r *. r)) d

let spring ~k_over_m ~anchor pos _ = Vec3.scale k_over_m (Vec3.sub anchor pos)
let drag ~c _ vel = Vec3.scale (-.c) vel

let submerged ~water ~half_height y =
  if half_height <= 0. then if y < water then 1. else 0.
  else Float.max 0. (Float.min 1. ((water -. (y -. half_height)) /. (2. *. half_height)))

let buoyancy ?(damping = 1.5) ~g ~water ~half_height ~density () (_, y, _) vel =
  let s = submerged ~water ~half_height y in
  (* up by Archimedes, down by gravity: g (s/d - 1), and a drag on the
   * part that is in the water *)
  let up = (g *. s /. density) -. g in
  Vec3.add (0., up, 0.) (Vec3.scale (-.damping *. s) vel)

let sum fs pos vel = List.fold_left (fun acc f -> Vec3.add acc (f pos vel)) (0., 0., 0.) fs
