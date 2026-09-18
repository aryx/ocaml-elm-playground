(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Shading *)

let t = Testo.create

let vertex normal : Project.vertex =
  { vx = 0.; vy = 0.; z = 1.; u = 0.; v = 0.; inv_z = 1.; u_over_z = 0.; v_over_z = 0.; normal }

(* Shading.mli's example: v0 facing the light, v1 and v2 away from it,
 * at the triangle's center *)
let test_center () =
  let light = Lighting.light_dir in
  let away = Vec3.scale (-1.) light in
  let at mode = Shading.make mode (vertex light) (vertex away) (vertex away) ~l0:(1. /. 3.) ~l1:(1. /. 3.) ~l2:(1. /. 3.) in
  Alcotest.(check (float 1e-9)) "no lighting" 1. (at Shading.Flat_color);
  Alcotest.(check (float 1e-9)) "flat: v0's" 1. (at Shading.Flat_shading);
  Alcotest.(check (float 1e-9)) "Gouraud: the brightnesses blended" 0.5 (at Shading.Gouraud);
  Alcotest.(check (float 1e-9)) "Phong: the normals blended" 0.25 (at Shading.Phong)

let tests = Testo.categorize "Shading" [ t "Gouraud vs Phong, the worked example" test_center ]
