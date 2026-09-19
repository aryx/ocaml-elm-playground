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

(* the keys (see playground/software/Playground_platform.ml): n
 * antialiasing off, f wireframe, b bounding boxes, o the simple code
 * instead of the optimized one (Opti), h the help. Not t (alpha blending off): only
 * examples/Mouse fades a shape, while the mouse button is down. *)
let scenes : Testutil_golden.scene list =
  [
    ("examples/software/Picture", "", 5);
    ("examples/software/Picture", "n", 5);
    ("examples/software/Picture", "f", 5);
    ("examples/software/Picture", "b", 5);
    (* the same golden frame as without "o", on purpose: an optimization
     * must not change a single pixel *)
    ("examples/software/Picture", "o", 5);
    ("examples/software/Picture", "h", 5);
    ("examples/software/Smiley", "", 5);
    ("examples/software/Words", "", 5);
    ("examples/software/Words", "n", 5);
    ("examples/software/Misc", "", 5);
    ("examples/software/Animation", "", 5);
    ("examples/software/Mouse", "", 5);
    ("examples/software/Keyboard", "", 5);
    ("games/software/Pong", "", 5);
    ("games/software/Asteroid", "", 5);
    ("games/software/Asteroid", "f", 5);
    (* claude: random, but with the runner's seed=1 flag the same every
     * run (see Testutil_golden.render) *)
    ("games/software/Snake", "", 5);
    ("games/software/Tetris", "", 5);
    ("games/software/TinyMario", "", 5);
    ("games/software/TinyInvaders", "", 5);
    ("games/software/TinySokoban", "", 5);
    ("games/software/TinyPacman", "", 5);
    ("games/software/TinyWolf", "", 5);
    ("games/software/TinyOutRun", "", 5);
    ("games/software/TinyBomberman", "", 5);
    ("games/software/TinyTron", "", 5);
    ("games/software/TinyMicroMachines", "", 5);
    (* the physics plan's: explicit Euler's orbit, spiraling out after 8
     * seconds; the artillery and Spacewar! titles *)
    ("examples/software/Orbit", "", 480);
    ("games/software/TinyWorms", "", 5);
    ("games/software/TinySpacewar", "", 5);
    (* the bounces after 5 seconds: clay flat, the superball back up;
     * Pong's title *)
    ("examples/software/Bounce", "", 300);
    ("games/software/TinyPong", "", 5);
    (* 300 marbles after 2 seconds, all pairs: 44850 box tests *)
    ("examples/software/Marbles", "", 120);
    (* rotation: boxes tipping over and tumbling, a ball rolling down *)
    ("examples/software/Boxes", "", 120);
    ("games/software/TinyCameltry", "", 5);
    (* stacking: the pyramid standing still after 5 seconds *)
    ("examples/software/Pyramid", "", 300);
    ("games/software/TinySlingshot", "", 5);
    (* a mass on a spring, a chain of springs, a rope of sticks *)
    ("examples/software/Elastic", "", 60);
    ("games/software/TinySoldat", "", 5);
    (* the planets on 2000-01-02, Jupiter and Saturn near their May 2000
     * conjunction *)
    ("examples/software/SolarSystem", "", 2);
    (* the audio plan's first examples (their sounds: -dump-audio) *)
    ("examples/software/Theremin", "", 2);
    ("examples/software/Piano", "", 2);
    (* the sound, seen (the "v" debug key, Audio_debug): TinyMario's
     * music at 1 s, as an oscilloscope, then a spectrum *)
    ("games/software/TinyMario", "v", 60);
    ("games/software/TinyMario", "vv", 60);
  ]

(* claude: games played with keys (-script, see Input_script): what the
 * start of a game can't show -- the camera scrolled, a coin taken; the
 * formation shot at, stepped down, a bunker bitten *)
let scripted : Testutil_golden.scripted list =
  [
    ("games/software/TinyMario", "run", 150, "right:1-150,up:30-34,up:95-99");
    ( "games/software/TinyInvaders",
      "play",
      300,
      "space:1,space:10,space:50,space:90,right:100-116,space:130,space:170,left:180-212,space:220,space:260,space:280" );
    (* the first level's shortest solution, uldurrd, a key every 5 frames *)
    ( "games/software/TinySokoban",
      "solve1",
      40,
      "space:1,up:5,left:10,down:15,up:20,right:25,right:30,down:35" );
    (* after READY!, left, up, right along the top: dots eaten, the
     * ghosts out of the house, scattering *)
    ("games/software/TinyPacman", "play", 300, "space:1,left:120-170,up:160-230,right:220-300");
    (* turned towards a treasure, walking to it: the billboard in front
     * of the far wall, a near wall on the right *)
    ("games/software/TinyWolf", "treasure", 60, "right:1-16,up:20-60");
    (* flat out into the first curve, which bends right *)
    ("games/software/TinyOutRun", "curve", 230, "space:1,up:2-230");
    (* a bomb dropped in the corner, the bomber walking away, the fire *)
    ("games/software/TinyBomberman", "bomb", 165, "space:1,space:5,right:8-30,down:31-45");
    (* two bombs, the first one's fire setting off the second: a chain *)
    ( "games/software/TinyBomberman",
      "chain",
      178,
      "space:1,right:3-22,space:25,left:27-46,space:49,down:51-90,right:91-110" );
    (* against the computer, blue turning around into its trail *)
    ("games/software/TinyTron", "computer", 200, "1:1,up:40,right:80,down:120,right:150");
    (* two players, both turning *)
    ("games/software/TinyTron", "duel", 150, "2:1,up:30,w:40,right:70,s:90,a:120");
    (* against the computer, north up; then with the camera turning *)
    ("games/software/TinyMicroMachines", "race", 200, "1:1,up:62-200");
    ("games/software/TinyMicroMachines", "turning", 200, "1:1,up:62-200,v:2");
    (* semi-implicit Euler: the same orbit, closed *)
    ("examples/software/Orbit", "semi", 480, "space:1-2");
    (* a shot, pushed back by the wind, digging its crater *)
    ("games/software/TinyWorms", "shot", 150, "space:2-3,space:10-11");
    (* both ships thrusting, turning and firing around the star *)
    ("games/software/TinySpacewar", "duel", 120, "space:2-3,up:10-60,left:30-45,down:50,down:70,w:10-40,s:55,s:75");
    (* a serve, returned by the computer, missed by the player *)
    ("games/software/TinyPong", "rally", 150, "space:2-3,w:100-160");
    (* the same marbles, the same frame (the three methods find the same
     * pairs), with the grid and its count *)
    ("examples/software/Marbles", "grid", 120, "space:60-61");
    (* rotation off: boxes balanced on their corners, stuck on the ramp *)
    ("examples/software/Boxes", "upright", 300, "u:2-3");
    (* the maze turned right then left, the moon rolling, a target taken *)
    ("games/software/TinyCameltry", "turns", 200, "space:2-3,right:30-75,left:150-200");
    (* the maze turned 30 degrees: the moon rolls away; upright, it
     * slides, and friction holds it (up to 39 degrees): still there *)
    ("games/software/TinyCameltry", "tilt", 150, "space:2-3,right:60-74");
    ("games/software/TinyCameltry", "tilt_upright", 150, "space:2-3,u:5,right:60-74");
    (* the same pyramid without the solver (phase 7's engine): a heap;
     * and hit by the ball, its top knocked off *)
    ("examples/software/Pyramid", "no_solver", 300, "s:2");
    ("examples/software/Pyramid", "ball", 200, "space:120");
    (* a shot along the dotted arc, the tower tumbling; and without the
     * solver, the tower slumping by itself, no shot *)
    ("games/software/TinySlingshot", "shot", 130, "space:2,space:60");
    ("games/software/TinySlingshot", "no_solver", 120, "space:2,s:5");
    (* all three kicked; and the chain too stiff for the time step,
     * exploding in 8 steps *)
    ("examples/software/Elastic", "kick", 60, "space:30");
    ("examples/software/Elastic", "stiff", 8, "x:2");
    (* the bots' fight, GREEN's ragdoll tumbling; the player running,
     * jumping, flying on the jets, shooting, a grenade, the blasts *)
    ("games/software/TinySoldat", "bots", 260, "space:2");
    ("games/software/TinySoldat", "jets", 130, "space:2,d:10-70,w:30,w:40-90,space:100-160,q:120");
    (* the true distances, the inner planets crowded; and 80 days a
     * second for 2 seconds: mid-2000 *)
    ("examples/software/SolarSystem", "true_distances", 3, "d:2");
    ("examples/software/SolarSystem", "later", 120, "up:2,up:4");
    (* keys held, lit; the square wave *)
    ("examples/software/Piano", "keys", 30, "space:2,a:10-30,g:10-30,u:10-30");
  ]

let tests = Testutil_golden.tests ~dir:"tests/2d" ~approve:"approve-golden2d" ~scripted scenes
