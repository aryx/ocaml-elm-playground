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
 * filtering, t the top-left fill rule, c clipping off, o the simple code instead of the
 * optimized one (Opti), h the help.
 * Not x (the magnifier): it follows the mouse. *)
let scenes : Testutil_golden.scene list =
  [
    ("examples3d/Cube3d", "", 3);
    ("examples3d/Cubes3d", "", 3);
    ("examples3d/Cubes3d", "f", 3);
    ("examples3d/Cubes3d", "z", 3);
    ("examples3d/Cubes3d", "b", 3);
    ("examples3d/Cubes3d", "bf", 3);
    ("examples3d/Cubes3d", "t", 3);
    ("examples3d/Cubes3d", "h", 3);
    (* the same golden frame as without "o", on purpose: an optimization
     * must not change a single pixel *)
    ("examples3d/Cubes3d", "o", 3);
    ("examples3d/Spheres3d", "", 3);
    ("examples3d/Spheres3d", "m", 3);
    ("examples3d/Spheres3d", "mm", 3);
    ("examples3d/Spheres3d", "mmm", 3);
    ("examples3d/Spheres3d", "t", 3);
    ("examples3d/TexturedCube3d", "", 3);
    ("examples3d/TexturedCube3d", "p", 3);
    ("examples3d/TexturedCube3d", "i", 3);
    ("examples3d/InteractiveCube3d", "", 3);
    ("examples3d/PaintersAlgorithmFail3d", "", 3);
    ("examples3d/PaintersAlgorithmFail3d", "z", 3);
    ("examples3d/FloatingCity3d", "", 3);
    ("examples3d/Corridor3d", "", 3);
    ("examples3d/Corridor3d", "c", 3);
    ("examples3d/Corridor3d", "f", 3);
    ("examples3d/CachedGrid3d", "", 3);
    (* claude: random stars, but with the runner's seed=1 flag the same
     * every run (see Testutil_golden.render) *)
    ("games3d/StarCollector3d", "", 3);
    (* claude: the title's tank, turned by 40 degrees *)
    ("games3d/TinyBattlezone", "", 40);
    (* claude: the same view as games/TinyWolf's golden frame, in 3D *)
    ("games3d/TinyWolf3d", "", 5);
  ]

(* claude: played with keys (-script, see Input_script) *)
let scripted : Testutil_golden.scripted list =
  [ ("games3d/StarCollector3d", "move", 40, "up:1-40,right:10-25");
    (* turned, driving towards a pyramid (cut by the near plane), a shell
     * flying at the enemy tank *)
    ("games3d/TinyBattlezone", "play", 150, "space:1,right:5-20,up:30-140,space:100");
    (* the same walk as games/TinyWolf's *)
    ("games3d/TinyWolf3d", "treasure", 60, "right:1-16,up:20-60") ]

let tests = Testutil_golden.tests ~dir:"tests/3d" ~approve:"approve-golden3d" ~scripted scenes
