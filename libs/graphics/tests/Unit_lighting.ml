(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Lighting *)

let t = Testo.create

(* Lighting.mli's worked example, with ambient = 0.25 *)
let test_lambert () =
  let check msg expected normal =
    Alcotest.(check (float 1e-9)) msg expected (Lighting.brightness_of_normal normal)
  in
  Alcotest.(check (float 1e-9)) "light_dir is a unit vector" 1. (Vec3.length Lighting.light_dir);
  check "facing the light" 1. Lighting.light_dir;
  check "facing away: the ambient floor" 0.25 (Vec3.scale (-1.) Lighting.light_dir);
  (* a unit normal at 60 degrees from light_dir: cos 60 = 0.5 *)
  let side = Vec3.normalize (Vec3.cross Lighting.light_dir (0., 1., 0.)) in
  let at_60 = Vec3.add (Vec3.scale 0.5 Lighting.light_dir) (Vec3.scale (sqrt 3. /. 2.) side) in
  check "60 degrees: 0.25 + 0.75 * 0.5" 0.625 at_60;
  check "90 degrees: the ambient floor" 0.25 side

let tests = Testo.categorize "Lighting" [ t "Lambert's cosine law, the worked example" test_lambert ]
