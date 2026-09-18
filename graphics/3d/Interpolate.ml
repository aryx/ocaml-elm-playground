(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Interpolate.mli *)

type mode = Perspective_correct | Linear

let make (mode : mode) (v0 : Project.vertex) (v1 : Project.vertex) (v2 : Project.vertex) :
    l0:float -> l1:float -> l2:float -> float * float * float =
  match mode with
  | Linear ->
      fun ~l0 ~l1 ~l2 ->
        let z = (l0 *. v0.z) +. (l1 *. v1.z) +. (l2 *. v2.z) in
        let u = (l0 *. v0.u) +. (l1 *. v1.u) +. (l2 *. v2.u) in
        let v = (l0 *. v0.v) +. (l1 *. v1.v) +. (l2 *. v2.v) in
        (z, u, v)
  | Perspective_correct ->
      fun ~l0 ~l1 ~l2 ->
        let inv_z = (l0 *. v0.inv_z) +. (l1 *. v1.inv_z) +. (l2 *. v2.inv_z) in
        let u_over_z = (l0 *. v0.u_over_z) +. (l1 *. v1.u_over_z) +. (l2 *. v2.u_over_z) in
        let v_over_z = (l0 *. v0.v_over_z) +. (l1 *. v1.v_over_z) +. (l2 *. v2.v_over_z) in
        (* the "perspective divide": undo the *. inv_z we multiplied by
         * back in Project.vertex, now that interpolation is done *)
        (1. /. inv_z, u_over_z /. inv_z, v_over_z /. inv_z)
