(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Interpolate *)

let t = Testo.create

(* a vertex at depth z with texture coordinate u, as Project.vertex
 * builds it *)
let vertex ~z ~u : Project.vertex =
  { vx = 0.; vy = 0.; z; u; v = 0.; inv_z = 1. /. z; u_over_z = u /. z; v_over_z = 0.; normal = (0., 0., 1.) }

(* Interpolate.mli's example: half way on the screen between z = 1,
 * u = 0 and z = 3, u = 1 *)
let test_half_way () =
  let v0 = vertex ~z:1. ~u:0. and v1 = vertex ~z:3. ~u:1. in
  let at mode = Interpolate.make mode v0 v1 v1 ~l0:0.5 ~l1:0.5 ~l2:0. in
  let z, u, _ = at Interpolate.Linear in
  Alcotest.(check (float 1e-9)) "linear z" 2. z;
  Alcotest.(check (float 1e-9)) "linear u" 0.5 u;
  let z, u, _ = at Interpolate.Perspective_correct in
  Alcotest.(check (float 1e-9)) "perspective-correct z" 1.5 z;
  Alcotest.(check (float 1e-9)) "perspective-correct u" 0.25 u

(* both exact at the corners *)
let test_corners () =
  let v0 = vertex ~z:1. ~u:0. and v1 = vertex ~z:3. ~u:1. in
  [ Interpolate.Linear; Perspective_correct ]
  |> List.iter (fun mode ->
         let z, u, _ = Interpolate.make mode v0 v1 v1 ~l0:0. ~l1:1. ~l2:0. in
         Alcotest.(check (float 1e-9)) "z at v1" 3. z;
         Alcotest.(check (float 1e-9)) "u at v1" 1. u)

let tests =
  Testo.categorize "Interpolate"
    [ t "half way: linear vs perspective-correct" test_half_way; t "exact at the corners" test_corners ]
