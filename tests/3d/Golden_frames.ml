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
    (* claude: the planets' night sides turned away from the Sun *)
    ("examples3d/SolarSystem3d", "", 3);
    (* claude: random stars, but with the runner's seed=1 flag the same
     * every run (see Testutil_golden.render) *)
    ("games3d/StarCollector3d", "", 3);
    (* claude: the title's tank, turned by 40 degrees *)
    ("games3d/TinyBattlezone", "", 40);
    (* claude: the same view as games/TinyWolf's golden frame, in 3D *)
    ("games3d/TinyWolf3d", "", 5);
    ("games3d/TinyVirtuaRacing", "", 5);
    ("games3d/TinyTron3d", "", 5);
    ("games3d/TinyMario64", "", 5);
    ("games3d/TinyMarble", "", 5);
    (* claude: the same view as games/TinyDoom's golden frame, in 3D *)
    ("games3d/TinyDoom3d", "", 5);
  ]

(* claude: played with keys (-script, see Input_script) *)
let scripted : Testutil_golden.scripted list =
  [ ("games3d/StarCollector3d", "move", 40, "up:1-40,right:10-25");
    (* the camera turned, the time sped up to 80 days a second *)
    ("examples3d/SolarSystem3d", "turned", 90, "w:2,w:4,left:10-60");
    (* turned, driving towards a pyramid (cut by the near plane), a shell
     * flying at the enemy tank *)
    ("games3d/TinyBattlezone", "play", 150, "space:1,right:5-20,up:30-140,space:100");
    (* the same walk as games/TinyWolf's *)
    ("games3d/TinyWolf3d", "treasure", 60, "right:1-16,up:20-60");
    (* the same drive as games/TinyOutRun's golden frame, in polygons *)
    ("games3d/TinyVirtuaRacing", "curve", 230, "space:1,up:2-230");
    (* v three times: the view from above *)
    ("games3d/TinyVirtuaRacing", "above", 300, "space:1,up:2-300,v:100,v:150,v:200");
    (* games/TinyTron's "computer" game, seen from behind the blue cycle,
     * then from above *)
    ("games3d/TinyTron3d", "behind", 150, "1:1,up:40,right:80,down:120,right:150");
    ("games3d/TinyTron3d", "above", 150, "1:1,up:40,right:80,down:120,right:150,v:3,v:6");
    (* a jump onto the first platform: Mario in the air, his shadow on
     * it; then the camera turned with d *)
    ("games3d/TinyMario64", "jump", 160, "space:1,left:2-63,up:64-175,space:145-165");
    ("games3d/TinyMario64", "camera", 200, "space:1,left:2-63,up:64-175,space:145-165,d:170-193");
    (* rolling south (down and left: the screen's diagonals) down the
     * first ramp, its band turned; then on over the cliff, broken *)
    ("games3d/TinyMarble", "ramp", 45, "space:1,down:2-200,left:2-200");
    ("games3d/TinyMarble", "broken", 125, "space:1,down:2-200,left:2-200");
    (* the same walks as games/TinyDoom's *)
    ("games3d/TinyDoom3d", "stairs", 80, "left:1-10,up:11-80");
    ("games3d/TinyDoom3d", "window", 60, "right:1-5,up:6-55");
    (* the 3D turtle's drawings, all at once (the clock frozen): the
     * tree, its leaves; Hilbert's curve in 3D, level 2 *)
    ("examples3d/LogoFractals3d", "tree", 5, "a:2");
    ("examples3d/LogoFractals3d", "hilbert", 8, "right:2,a:4") ]

let tests = Testutil_golden.tests ~dir:"tests/3d" ~approve:"approve-golden3d" ~scripted scenes
