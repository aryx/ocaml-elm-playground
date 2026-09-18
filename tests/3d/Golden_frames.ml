(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Golden_frames.mli *)

(* the keys: f wireframe, z painter's algorithm, b culling off, m the
 * next shading mode, p linear interpolation, i nearest texture
 * filtering *)
let scenes : Testutil_golden.scene list =
  [
    ("examples3d/Cube3d", "", 3);
    ("examples3d/Cubes3d", "", 3);
    ("examples3d/Cubes3d", "f", 3);
    ("examples3d/Cubes3d", "z", 3);
    ("examples3d/Cubes3d", "b", 3);
    ("examples3d/Cubes3d", "bf", 3);
    ("examples3d/Spheres3d", "", 3);
    ("examples3d/Spheres3d", "m", 3);
    ("examples3d/Spheres3d", "mm", 3);
    ("examples3d/Spheres3d", "mmm", 3);
    ("examples3d/TexturedCube3d", "", 3);
    ("examples3d/TexturedCube3d", "p", 3);
    ("examples3d/TexturedCube3d", "i", 3);
    ("examples3d/InteractiveCube3d", "", 3);
    ("examples3d/PaintersAlgorithmFail3d", "", 3);
    ("examples3d/PaintersAlgorithmFail3d", "z", 3);
    ("examples3d/FloatingCity3d", "", 3);
  ]

let tests = Testutil_golden.tests ~dir:"tests/3d" ~approve:"approve-golden3d" scenes
