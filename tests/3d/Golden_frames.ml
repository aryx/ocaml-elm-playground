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
    ("examples3d/Triangle3d", "", 3);
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
    ("examples3d/PhysicsSolarSystem3d", "", 3);
    (* claude: the T-handle at rest in its own frame, L and w still
     * pointing the same way *)
    ("examples3d/PhysicsSpin3d", "", 3);
    (* claude: the five blocks in the air, a moment before the splash *)
    ("examples3d/PhysicsFloat3d", "", 3);
    (* claude: random stars, but with the runner's seed=1 flag the same
     * every run (see Testutil_golden.render) *)
    ("games3d/StarCollector3d", "", 3);
    (* claude: the title's tank, turned by 40 degrees *)
    ("games3d/TinyBattlezone", "", 40);
    (* claude: the same view as games2.5d/TinyWolf's golden frame, in 3D *)
    ("games3d/TinyWolf3d", "", 5);
    ("games3d/TinyVirtuaRacing", "", 5);
    ("games3d/TinyTron3d", "", 5);
    ("games3d/TinyMario64", "", 5);
    ("games3d/TinyMarble", "", 5);
    (* claude: the same view as games2.5d/TinyDoom's golden frame, in 3D *)
    ("games3d/TinyDoom3d", "", 5);
    (* claude: "r" twice: a third of the resolution, 3x3 pixels (Pixelate) *)
    ("games3d/TinyDoom3d", "rr", 5);
    (* claude: the same view as games2.5d/TinyComanche's golden frame *)
    ("games3d/TinyComanche3d", "", 5);
    (* claude: the same view as games2.5d/TinyDescent's golden frame *)
    ("games3d/TinyDescent3d", "", 5);
    (* claude: the start room, its pillar's shadow, the lit doorway *)
    ("games3d/TinyQuake", "", 5);
    ("games3d/TinyBlockout", "", 5);
    ("games3d/TinyTombRaider", "", 5);
    (* claude: the title's four foods on their turning arena *)
    ("games3d/TinyBoomerangFu3d", "", 5);
  ]

(* claude: played with keys (-script, see Input_script) *)
let scripted : Testutil_golden.scripted list =
  [ ("games3d/StarCollector3d", "move", 40, "up:1-40,right:10-25");
    (* the camera turned, the time sped up to 80 days a second *)
    ("examples3d/PhysicsSolarSystem3d", "turned", 90, "w:2,w:4,left:10-60");
    (* turned, driving towards a pyramid (cut by the near plane), a shell
     * flying at the enemy tank *)
    ("games3d/TinyBattlezone", "play", 150, "space:1,right:5-20,up:30-140,space:100");
    (* the same walk as games2.5d/TinyWolf's *)
    ("games3d/TinyWolf3d", "treasure", 60, "right:1-16,up:20-60");
    (* the same drive as games2.5d/TinyOutRun's golden frame, in polygons *)
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
    (* the same walks as games2.5d/TinyDoom's *)
    ("games3d/TinyDoom3d", "stairs", 80, "left:1-10,up:11-80");
    ("games3d/TinyDoom3d", "window", 60, "right:1-5,up:6-55");
    (* the same flight as games2.5d/TinyComanche's *)
    ("games3d/TinyComanche3d", "island", 70, "up:1-70,w:1-20");
    (* the same flight as games2.5d/TinyDescent's *)
    ("games3d/TinyDescent3d", "corridor", 30, "w:1-30");
    (* through the doorway into the corridor: the visibility set drops
     * to a few leaves, and "v" (the second one) draws the whole level *)
    ("games3d/TinyQuake", "doorway", 60, "w:1-60");
    ("games3d/TinyQuake", "everything", 60, "v:2,w:1-60");
    (* four seconds in: thrown about the middle axis the handle has
     * turned itself over once, with nothing acting on it; thrown about
     * the largest axis it has not, and will not. The purple arrow (L)
     * is in the same place in both, which is the point. *)
    ("examples3d/PhysicsSpin3d", "flip", 240, "2:1");
    ("examples3d/PhysicsSpin3d", "stable", 240, "3:1");
    (* settled, and then the water raised by 60 cm: every block rises
     * with it, each keeping exactly its own density under the surface,
     * and the stone stays on the bottom *)
    ("examples3d/PhysicsFloat3d", "risen", 600, "up:300-360");
    (* the 3D turtle's drawings, all at once (the clock frozen): the
     * tree, its leaves; Hilbert's curve in 3D, level 2 *)
    ("examples3d/LogoFractals3d", "tree", 5, "a:2");
    ("examples3d/LogoFractals3d", "hilbert", 8, "right:2,a:4");
    (* four pieces dropped around the pit, a fifth on its way down: the
     * settled cubes darker the deeper they lie, and the lit ring of the
     * well marking the level this one will land on *)
    ( "games3d/TinyBlockout",
      "pit",
      95,
      "space:1,left:5,left:10,space:15,right:20,right:25,right:30,space:35,up:40,up:45,space:50,down:55,down:60,down:65,space:70,x:75" );
    (* down the entrance corridor: the texture page on the walls, one
     * square of it per square of wall, hieroglyphs along the north side *)
    ("games3d/TinyTombRaider", "corridor", 90, "space:1,up:10-88");
    (* a boomerang in the air with its shadow under it (the one depth
     * cue this fixed, nearly isometric camera gets), all four still
     * standing; then, further in, the avocado in two halves, its cut
     * faces pale, and the flight that did it *)
    ("games3d/TinyBoomerangFu3d", "flight", 45, "space:1,right:5-25,space:26,right:30-60");
    ("games3d/TinyBoomerangFu3d", "cut", 95, "space:1,up:5-40,right:41-70,space:71,right:75-140") ]

let tests = Testutil_golden.tests ~dir:"tests/3d" ~approve:"approve-golden3d" ~scripted scenes
