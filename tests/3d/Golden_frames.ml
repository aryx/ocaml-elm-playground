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
    (* claude: the four hitboxes as the engine sees them -- a sphere's
     * rings, a box's edges, a turned box's, a capsule's *)
    ("examples3d/PhysicsHitbox3d", "", 3);
    (* claude: the five balls in the air, and the bar each one has to
     * come back to, drawn from its bounciness alone *)
    ("examples3d/PhysicsBounce3d", "", 3);
    (* claude: two hundred marbles above the floor of their cage, and
     * the count of bounding boxes the broad phase compared *)
    ("examples3d/PhysicsMarbles3d", "", 3);
    (* claude: three bodies at the top of the ramp, each labelled with
     * the acceleration its own tensor predicts *)
    ("examples3d/PhysicsRoll3d", "", 3);
    (* claude: the wall and the dominoes as they are built, every body
     * awake (the yellow markers) *)
    ("examples3d/PhysicsStack3d", "", 3);
    (* claude: random stars, but with the runner's seed=1 flag the same
     * every run (see Testutil_golden.render) *)
    ("games3d/StarCollector3d", "", 3);
    (* the title's tank, turned by 40 degrees, solid (the same frame as
     * games2.5d/TinyBattlezone's, in lines) *)
    ("games3d/TinyBattlezone3d", "", 40);
    (* the station turning, solid (games2.5d/TinyElite's title frame) *)
    ("games3d/TinyElite3d", "", 5);
    (* the heist from above: the house, the vault, the water tower and
     * its crate *)
    ("games3d/TinyTeardown", "", 5);
    (* claude: the same view as games2.5d/TinyWolfenstein's golden frame, in 3D *)
    ("games3d/TinyWolfenstein3d", "", 5);
    ("games3d/TinyVirtuaRacing", "", 5);
    (* claude: the grid on the start line, from the camera turning
     * round it: the karts are drawings, the lorries and the item
     * boxes polygons -- the mix the game is about *)
    ("games3d/TinyMarioKart64", "", 5);
    (* claude: two skeletons of boxes on the ring, in their guard: the
     * pose is the angles of their joints, and nothing else *)
    ("games3d/TinyVirtuaFighter", "", 5);
    (* claude: the canyon from the title's camera, turning round the
     * start: the ribbon of the racing kit, flown over *)
    ("games3d/TinyStarFox", "", 5);
    (* claude: the hall, from its corner, behind the title *)
    ("games3d/TinyAloneInTheDark", "", 5);
    ("games3d/TinyTron3d", "", 5);
    ("games3d/TinyMario64", "", 5);
    ("games3d/TinyMarbleMadness", "", 5);
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
    (* claude: the first chamber, its three shades, and the hero the
     * arcade angle looks down on *)
    ("games3d/TinyHades", "", 5);
    (* claude: the first monument, which is two structures three
     * blocks apart drawn as one beam: the whole game in one frame *)
    ("games3d/TinyMonumentValley", "", 5);
    (* claude: the band on its stage: the four highways, the fret pads
     * of the guitar and the bass, the drum pads and the pedal, the
     * keyboard of the keys, and the difficulty to pick *)
    ("games3d/TinyRockBand", "", 5);
  ]

(* claude: played with keys (-script, see Input_script) *)
let scripted : Testutil_golden.scripted list =
  [ ("games3d/StarCollector3d", "move", 40, "up:1-40,right:10-25");
    (* the camera turned, the time sped up to 80 days a second *)
    ("examples3d/PhysicsSolarSystem3d", "turned", 90, "w:2,w:4,left:10-60");
    (* the same battle as games2.5d/TinyBattlezone's golden frame: the
     * pyramid now hides the enemy tank *)
    ("games3d/TinyBattlezone3d", "play", 150, "space:1,right:5-20,up:30-140,space:100");
    (* the same launch as games2.5d/TinyElite's: the station ahead, the
     * slot turning, Lave a sphere behind it *)
    ("games3d/TinyElite3d", "flight", 200, "space:1");
    (* three blows at the house's south wall: the hole, and the greedy
     * mesh cut round it *)
    ("games3d/TinyTeardown", "hole", 130, "space:1,w:2-99,x:101,down:102-109,x:121,left:122-124,x:141");
    (* under the water tower, its four legs knocked out one by one, then
     * back out to look: the tank came down onto the stumps *)
    ( "games3d/TinyTeardown",
      "tower",
      600,
      "space:1,right:2-37,w:38-105,left:106-141,w:142-224,down:225-235,left:236-253,x:255,left:256-291,x:293,left:294-329,x:331,left:332-367,x:369,right:370-423,w:424-513,right:514-585,up:586-593"
    );
    (* the same walk as games2.5d/TinyWolfenstein's *)
    ("games3d/TinyWolfenstein3d", "treasure", 60, "right:1-16,up:20-60");
    (* the same drive as games2.5d/TinyOutRun's golden frame, in polygons *)
    ("games3d/TinyVirtuaRacing", "curve", 230, "space:1,up:2-230");
    (* the grid, on the last second of the countdown: eight karts drawn
     * four abreast, the chequered line under them, and the road
     * climbing away to the crest *)
    ("games3d/TinyMarioKart64", "grid", 90, "space:1");
    (* a kick landing, held still by the hitstop the hit itself caused:
     * the leg is out exactly while the move is active, because the
     * keyframes and the frame data are the same numbers *)
    ("games3d/TinyVirtuaFighter", "kick", 150, "space:1,right:20-80,g:110-200");
    (* down the canyon on rails, the arwing slid to the left, its bolts
     * streaming ahead and a wave crossing below *)
    ("games3d/TinyStarFox", "canyon", 200, "space:1,space:150-200,left:120-150");
    (* through the hall's south door: the cut to the corridor, Carnby
     * small at the far end and the thing in the foreground walking at
     * him *)
    ("games3d/TinyAloneInTheDark", "corridor", 240, "space:1,right:20-47,up:48-128,right:129-156,up:157-240");
    (* the ramp at the top of the climb: the field in the air, each
     * kart's shadow left on the boards below it *)
    ("games3d/TinyMarioKart64", "jump", 395, "space:1,up:2-450");
    (* a powerslide into the banked right-hander, taken downhill: the
     * kart shows its side (the drawing is chosen by the angle it is
     * seen from), and the sparks say the mini-turbo is charged *)
    ("games3d/TinyMarioKart64", "slide", 560, "space:1,up:2-620,right:470-545,Shift:470-580");
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
    ("games3d/TinyMarbleMadness", "ramp", 45, "space:1,down:2-200,left:2-200");
    ("games3d/TinyMarbleMadness", "broken", 125, "space:1,down:2-200,left:2-200");
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
    (* pushed into the crate: the contact's point and the way out,
     * drawn as long as the overlap is deep *)
    ("examples3d/PhysicsHitbox3d", "inside", 70, "left:1-25,up:1-120");
    (* after the first bounce: each ball at the top of its return,
     * beside the bar at e^2 of its fall, about 2% under it -- the cost
     * of a discrete step, and on screen rather than hidden *)
    ("examples3d/PhysicsBounce3d", "returned", 260, "x:1");
    (* the grid's cells, drawn where they exist: a hashed grid holds
     * only the cells something is in, and in 3D a dense one would be a
     * million of them. Two spaces would show sweep and prune instead;
     * all three find the same pairs, which is the point of the
     * counter. *)
    ("examples3d/PhysicsMarbles3d", "grid", 45, "space:1,g:10");
    (* two seconds down the slope: the ice ball ahead, the sphere next,
     * the capsule last -- the order the formula gives -- and each
     * measured acceleration on its prediction, which is the engine
     * arriving at 5/7 g sin a on its own *)
    ("examples3d/PhysicsRoll3d", "race", 110, "x:1");
    (* seven seconds later, the same wall: standing, every body asleep,
     * no contact points solved at all. And the same seven seconds with
     * the solver turned off at the start ("s"), which is a heap. *)
    ("examples3d/PhysicsStack3d", "asleep", 400, "x:1");
    ("examples3d/PhysicsStack3d", "no_solver", 200, "s:2");
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
    (* claude: the song under way on the drums, on Easy (a pad a beat,
       no pedal): the count-in's hi-hat, the crash, snare, hi-hat,
       snare, each struck on its beat, the band playing its own parts
       beside you; the sound card fed 735 samples a frame, so the
       song's clock is exact *)
    ("games3d/TinyRockBand", "gig", 340,
     "3:1,up:2,space:3,d:123,d:153,d:183,d:213,g:243,s:273,d:303,s:333");
    (* claude: the figure part way along the impossible beam, having
       stepped from the ground path onto a terrace three blocks up and
       three away without anything in between *)
    ("games3d/TinyMonumentValley", "walked", 120, "space:2,at(120;-40):20-40,click:30");
    (* claude: a run in progress: one death already paid for, so the
       gauge says run 2 and the life kept from the first *)
    ("games3d/TinyHades", "run", 300,
     "space:1,down:10-80,right:10-80,space:90,space:110,up:120-200,left:150-200,space:210,x:220,space:240,space:270,space:290");
    ("games3d/TinyBoomerangFu3d", "cut", 95, "space:1,up:5-40,right:41-70,space:71,right:75-140") ]

let tests = Testutil_golden.tests ~dir:"tests/3d" ~approve:"approve-golden3d" ~scripted scenes
