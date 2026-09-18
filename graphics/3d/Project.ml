(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Project.mli *)

type vertex = {
  vx : float;
  vy : float;
  z : float;
  u : float;
  v : float;
  inv_z : float;
  u_over_z : float;
  v_over_z : float;
  normal : Vec3.t;
}

let vertex (camera : Camera.t) ~(width : int) ~(height : int)
    ((point, (u, v), normal) : Vec3.t * (float * float) * Vec3.t) : vertex option =
  let ((_px, _py, pz) as view_point) = Camera.view camera point in
  let fsx = float_of_int width and fsy = float_of_int height in
  match Camera.ndc camera ~aspect:(fsx /. fsy) view_point with
  | None -> None
  | Some (ndc_x, ndc_y) ->
    let inv_z = 1. /. pz in
    Some
      { vx = (fsx /. 2.) +. (ndc_x *. (fsx /. 2.));
        vy = (fsy /. 2.) -. (ndc_y *. (fsy /. 2.));
        z = pz;
        u;
        v;
        inv_z;
        u_over_z = u *. inv_z;
        v_over_z = v *. inv_z;
        normal;
      }
