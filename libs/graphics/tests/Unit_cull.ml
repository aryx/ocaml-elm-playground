(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Cull *)

let t = Testo.create

(* Cull.mli's example: a cube's +z face, counterclockwise seen from +z *)
let front = [ (-1., -1., 1.); (1., -1., 1.); (1., 1., 1.); (-1., 1., 1.) ]

let test_cube_face () =
  Alcotest.(check bool) "seen from +z" true (Cull.faces_camera ~eye:(0., 0., 5.) front);
  Alcotest.(check bool) "from the other side" false (Cull.faces_camera ~eye:(0., 0., -5.) front)

let tests = Testo.categorize "Cull" [ t "a cube's face, from both sides" test_cube_face ]
