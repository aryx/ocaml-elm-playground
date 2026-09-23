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
    ("examples/software/Triangle3d", "", 3);
    ("examples/software/Cube3d", "", 3);
    ("examples/software/Cubes3d", "", 3);
    ("examples/software/Cubes3d", "f", 3);
    ("examples/software/Cubes3d", "z", 3);
    ("examples/software/Cubes3d", "b", 3);
    ("examples/software/Cubes3d", "bf", 3);
    ("examples/software/Cubes3d", "t", 3);
    ("examples/software/Cubes3d", "h", 3);
    (* the same golden frame as without "o", on purpose: an optimization
     * must not change a single pixel *)
    ("examples/software/Cubes3d", "o", 3);
    ("examples/software/Spheres3d", "", 3);
    ("examples/software/Spheres3d", "m", 3);
    ("examples/software/Spheres3d", "mm", 3);
    ("examples/software/Spheres3d", "mmm", 3);
    ("examples/software/Spheres3d", "t", 3);
    ("examples/software/TexturedCube3d", "", 3);
    ("examples/software/TexturedCube3d", "p", 3);
    ("examples/software/TexturedCube3d", "i", 3);
    ("examples/software/InteractiveCube3d", "", 3);
    ("examples/software/PaintersAlgorithmFail3d", "", 3);
    ("examples/software/PaintersAlgorithmFail3d", "z", 3);
    ("examples/software/FloatingCity3d", "", 3);
    ("examples/software/Corridor3d", "", 3);
    ("examples/software/Corridor3d", "c", 3);
    ("examples/software/Corridor3d", "f", 3);
    ("examples/software/CachedGrid3d", "", 3);
    (* claude: the planets' night sides turned away from the Sun *)
    ("examples/software/PhysicsSolarSystem3d", "", 3);
    (* claude: the T-handle at rest in its own frame, L and w still
     * pointing the same way *)
    ("examples/software/PhysicsSpin3d", "", 3);
    (* claude: the five blocks in the air, a moment before the splash *)
    ("examples/software/PhysicsFloat3d", "", 3);
    (* claude: the four hitboxes as the engine sees them -- a sphere's
     * rings, a box's edges, a turned box's, a capsule's *)
    ("examples/software/PhysicsHitbox3d", "", 3);
    (* claude: the five balls in the air, and the bar each one has to
     * come back to, drawn from its bounciness alone *)
    ("examples/software/PhysicsBounce3d", "", 3);
    (* claude: two hundred marbles above the floor of their cage, and
     * the count of bounding boxes the broad phase compared *)
    ("examples/software/PhysicsMarbles3d", "", 3);
    (* claude: three bodies at the top of the ramp, each labelled with
     * the acceleration its own tensor predicts *)
    ("examples/software/PhysicsRoll3d", "", 3);
    (* claude: the wall and the dominoes as they are built, every body
     * awake (the yellow markers) *)
    ("examples/software/PhysicsStack3d", "", 3);
    ("examples/software/PhysicsWalk3d", "", 5);
    ("examples/software/PhysicsRagdoll3d", "", 3);
    (* claude: random stars, but with the runner's seed=1 flag the same
     * every run (see Testutil_golden.render) *)
    ("examples/software/StarCollector3d", "", 3);
    (* the title's tank, turned by 40 degrees, solid (the same frame as
     * TinyBattlezone's, in lines) *)
    ("games/fps/software/TinyBattlezone3d", "", 40);
    (* claude: the arena from the title's camera, the two sleds face
     * to face beside the pillar in its middle *)
    ("games/fps/software/TinyCyberSled", "", 5);
    (* the station turning, solid (TinyElite's title frame) *)
    ("games/flight/software/TinyElite3d", "", 5);
    (* the heist from above: the house, the vault, the water tower and
     * its crate *)
    ("games/fps/software/TinyTeardown", "", 5);
    ("games/sports/software/TinyPinball3d", "", 5);
    ("games/fps/software/TinyHalfLife2", "", 5);
    ("games/puzzle/software/TinyPortal", "", 5);
    (* claude: the same view as TinyWolfenstein's golden frame, in 3D *)
    ("games/fps/software/TinyWolfenstein3d", "", 5);
    ("games/racing/software/TinyVirtuaRacing", "", 5);
    (* claude: the grid on the start line, from the camera turning
     * round it: the karts are drawings, the lorries and the item
     * boxes polygons -- the mix the game is about *)
    ("games/racing/software/TinyMarioKart64", "", 5);
    (* claude: two skeletons of boxes on the ring, in their guard: the
     * pose is the angles of their joints, and nothing else *)
    ("games/fighting/software/TinyVirtuaFighter", "", 5);
    (* claude: the canyon from the title's camera, turning round the
     * start: the ribbon of the racing kit, flown over *)
    ("games/shmup/software/TinyStarFox", "", 5);
    (* claude: the hall, from its corner, behind the title *)
    ("games/adventure/software/TinyAloneInTheDark", "", 5);
    (* claude: Hyrule Field turning round the title, by day: the trees,
     * the hills all round, and Link small in the middle *)
    ("games/adventure/software/TinyZeldaOcarina", "", 5);
    ("games/arcade/software/TinyTron3d", "", 5);
    ("games/platform/software/TinyMario64", "", 5);
    ("games/arcade/software/TinyMarbleMadness", "", 5);
    (* claude: the same view as TinyDoom's golden frame, in 3D *)
    ("games/fps/software/TinyDoom3d", "", 5);
    (* claude: "r" twice: a third of the resolution, 3x3 pixels (Pixelate) *)
    ("games/fps/software/TinyDoom3d", "rr", 5);
    (* claude: the same view as TinyComanche's golden frame *)
    ("games/flight/software/TinyComanche3d", "", 5);
    (* claude: the same view as TinyDescent's golden frame *)
    ("games/flight/software/TinyDescent3d", "", 5);
    (* claude: the start room, its pillar's shadow, the lit doorway *)
    ("games/fps/software/TinyQuake", "", 5);
    ("games/puzzle/software/TinyBlockout", "", 5);
    ("games/adventure/software/TinyTombRaider", "", 5);
    (* claude: frame 60, not 5: the world's chunks are built a few per
     * frame, and at 5 most of it is not there yet *)
    ("games/fps/software/TinyMinecraft", "", 60);
    (* claude: the title's four foods on their turning arena *)
    ("games/arcade/software/TinyBoomerangFu", "", 5);
    (* claude: the first chamber, its three shades, and the hero the
     * arcade angle looks down on *)
    ("games/rpg/software/TinyHades", "", 5);
    (* claude: the first monument, which is two structures three
     * blocks apart drawn as one beam: the whole game in one frame *)
    ("games/puzzle/software/TinyMonumentValley", "", 5);
    ("games/puzzle/software/TinyCrush3d", "", 5);
    ("games/puzzle/software/TinyPerspective", "", 5);
    (* claude: the band on its stage: the four highways, the fret pads
     * of the guitar and the bass, the drum pads and the pedal, the
     * keyboard of the keys, and the difficulty to pick *)
    ("games/rhythm/software/TinyRockBand", "", 5);
  ]

(* claude: played with keys (-script, see Input_script) *)
let scripted : Testutil_golden.scripted list =
  [ ("examples/software/StarCollector3d", "move", 40, "up:1-40,right:10-25");
    (* the camera turned, the time sped up to 80 days a second *)
    ("examples/software/PhysicsSolarSystem3d", "turned", 90, "w:2,w:4,left:10-60");
    (* the same battle as TinyBattlezone's golden frame: the
     * pyramid now hides the enemy tank *)
    ("games/fps/software/TinyBattlezone3d", "play", 150, "space:1,right:5-20,up:30-140,space:100");
    (* claude: the view from behind (v); the right tread alone turns
     * the sled left, both ahead, then both sticks sideways: strafing
     * right, firing, the computer's sled beyond the pillar *)
    ( "games/fps/software/TinyCyberSled",
      "fight",
      150,
      "space:1,v:3,up:5-30,w:40-90,up:40-90,space:100-150,d:100-150,right:100-150" );
    (* claude: strafing against the wall and firing, hit: the juice's
     * sparks round the sled (Juice3d) *)
    ("games/fps/software/TinyCyberSled", "hit", 1100, "space:1,v:3,space:5-1500,d:5-1500,right:5-1500");
    (* the same launch as TinyElite's: the station ahead, the
     * slot turning, Lave a sphere behind it *)
    ("games/flight/software/TinyElite3d", "flight", 200, "space:1");
    (* three blows at the house's south wall: the hole, and the greedy
     * mesh cut round it *)
    (* the flippers raised, turned by the game (kinematic bodies); the
     * ball, launched, round the dome, the sweep catching its touches *)
    (* the gravity gun: up to the pile, a crate grabbed out of it and
     * held up, the others tumbling down *)
    (* a portal in the floor, one on the wall over the ledge (seen
     * through: the ceiling, from the floor portal); walked into the
     * floor, flung out of the wall onto the ledge, looking out *)
    ("games/puzzle/software/TinyPortal", "portals", 62, "space:1,right:2-6,down:7-25,z:27,up:30-58,x:61,down:64-73,w:75-110");
    ("games/puzzle/software/TinyPortal", "fling", 150, "space:1,right:2-6,down:7-25,z:27,up:30-58,x:61,down:64-73,w:75-110");
    ("games/fps/software/TinyHalfLife2", "grab", 240, "space:1,w:2-166,down:170-185,z:190,up:195-215");
    ("games/sports/software/TinyPinball3d", "flippers", 15, "space:1,left:2-15,right:2-15");
    ("games/sports/software/TinyPinball3d", "launch", 95, "space:1,space:3-62");
    ("games/fps/software/TinyTeardown", "hole", 130, "space:1,w:2-99,x:101,down:102-109,x:121,left:122-124,x:141");
    (* under the water tower, its four legs knocked out one by one, then
     * back out to look: the tank came down onto the stumps *)
    ( "games/fps/software/TinyTeardown",
      "tower",
      600,
      "space:1,right:2-37,w:38-105,left:106-141,w:142-224,down:225-235,left:236-253,x:255,left:256-291,x:293,left:294-329,x:331,left:332-367,x:369,right:370-423,w:424-513,right:514-585,up:586-593"
    );
    (* the same walk as TinyWolfenstein's *)
    ("games/fps/software/TinyWolfenstein3d", "treasure", 60, "right:1-16,up:20-60");
    (* the same drive as TinyOutRun's golden frame, in polygons *)
    ("games/racing/software/TinyVirtuaRacing", "curve", 230, "space:1,up:2-230");
    (* the grid, on the last second of the countdown: eight karts drawn
     * four abreast, the chequered line under them, and the road
     * climbing away to the crest *)
    ("games/racing/software/TinyMarioKart64", "grid", 90, "space:1");
    (* a kick landing, held still by the hitstop the hit itself caused:
     * the leg is out exactly while the move is active, because the
     * keyframes and the frame data are the same numbers *)
    ("games/fighting/software/TinyVirtuaFighter", "kick", 150, "space:1,right:20-80,g:110-200");
    (* down the canyon on rails, the arwing slid to the left, its bolts
     * streaming ahead and a wave crossing below *)
    ("games/shmup/software/TinyStarFox", "canyon", 200, "space:1,space:150-200,left:120-150");
    (* through the hall's south door: the cut to the corridor, Carnby
     * small at the far end and the thing in the foreground walking at
     * him *)
    ("games/adventure/software/TinyAloneInTheDark", "corridor", 240, "space:1,right:20-47,up:48-128,right:129-156,up:157-240");
    (* walking across the field towards the temple set in the hills,
     * the camera trailing *)
    ("games/adventure/software/TinyZeldaOcarina", "field", 60, "space:1,up:10-60");
    (* through the temple's door, a loading zone, into the room, and
     * locked on: the black bars, the fairy over the Stalfos winding up
     * its chop, the camera over Link's shoulder on the line through
     * the two *)
    ("games/adventure/software/TinyZeldaOcarina", "locked", 480, "space:1,up:10-420,z:380-480,right:425-480");
    (* the ramp at the top of the climb: the field in the air, each
     * kart's shadow left on the boards below it *)
    ("games/racing/software/TinyMarioKart64", "jump", 395, "space:1,up:2-450");
    (* a powerslide into the banked right-hander, taken downhill: the
     * kart shows its side (the drawing is chosen by the angle it is
     * seen from), and the sparks say the mini-turbo is charged *)
    ("games/racing/software/TinyMarioKart64", "slide", 560, "space:1,up:2-620,right:470-545,Shift:470-580");
    (* claude: four players, the screen in quadrants as on the N64: the
       race without the computer's karts, each view its own camera and
       HUD *)
    ("games/racing/software/TinyMarioKart64", "four", 300, "4:1,space:3");
    (* claude: the battle, four players on Block Fort: the forts, the
       bridges, three balloons each, an item in every hand *)
    ("games/racing/software/TinyMarioKart64", "battle", 230, "4:1,b:3");
    (* v three times: the view from above *)
    ("games/racing/software/TinyVirtuaRacing", "above", 300, "space:1,up:2-300,v:100,v:150,v:200");
    (* TinyTron's "computer" game, seen from behind the blue cycle,
     * then from above *)
    ("games/arcade/software/TinyTron3d", "behind", 150, "1:1,up:40,right:80,down:120,right:150");
    ("games/arcade/software/TinyTron3d", "above", 150, "1:1,up:40,right:80,down:120,right:150,v:3,v:6");
    (* a jump onto the first platform: Mario in the air, his shadow on
     * it; then the camera turned with d *)
    ("games/platform/software/TinyMario64", "jump", 160, "space:1,left:2-63,up:64-175,space:145-165");
    ("games/platform/software/TinyMario64", "camera", 200, "space:1,left:2-63,up:64-175,space:145-165,d:170-193");
    (* rolling south (down and left: the screen's diagonals) down the
     * first ramp, its band turned; then on over the cliff, broken *)
    ("games/arcade/software/TinyMarbleMadness", "ramp", 45, "space:1,down:2-200,left:2-200");
    ("games/arcade/software/TinyMarbleMadness", "broken", 125, "space:1,down:2-200,left:2-200");
    (* the same walks as TinyDoom's *)
    ("games/fps/software/TinyDoom3d", "stairs", 80, "left:1-10,up:11-80");
    ("games/fps/software/TinyDoom3d", "window", 60, "right:1-5,up:6-55");
    (* the same flight as TinyComanche's *)
    ("games/flight/software/TinyComanche3d", "island", 70, "up:1-70,w:1-20");
    (* the same flight as TinyDescent's *)
    ("games/flight/software/TinyDescent3d", "corridor", 30, "w:1-30");
    (* through the doorway into the corridor: the visibility set drops
     * to a few leaves, and "v" (the second one) draws the whole level *)
    ("games/fps/software/TinyQuake", "doorway", 60, "w:1-60");
    ("games/fps/software/TinyQuake", "everything", 60, "v:2,w:1-60");
    (* four seconds in: thrown about the middle axis the handle has
     * turned itself over once, with nothing acting on it; thrown about
     * the largest axis it has not, and will not. The purple arrow (L)
     * is in the same place in both, which is the point. *)
    ("examples/software/PhysicsSpin3d", "flip", 240, "2:1");
    ("examples/software/PhysicsSpin3d", "stable", 240, "3:1");
    (* settled, and then the water raised by 60 cm: every block rises
     * with it, each keeping exactly its own density under the surface,
     * and the stone stays on the bottom *)
    ("examples/software/PhysicsFloat3d", "risen", 600, "up:300-360");
    (* pushed into the crate: the contact's point and the way out,
     * drawn as long as the overlap is deep *)
    ("examples/software/PhysicsHitbox3d", "inside", 70, "left:1-25,up:1-120");
    (* after the first bounce: each ball at the top of its return,
     * beside the bar at e^2 of its fall, about 2% under it -- the cost
     * of a discrete step, and on screen rather than hidden *)
    ("examples/software/PhysicsBounce3d", "returned", 260, "x:1");
    (* the grid's cells, drawn where they exist: a hashed grid holds
     * only the cells something is in, and in 3D a dense one would be a
     * million of them. Two spaces would show sweep and prune instead;
     * all three find the same pairs, which is the point of the
     * counter. *)
    ("examples/software/PhysicsMarbles3d", "grid", 45, "space:1,g:10");
    (* two seconds down the slope: the ice ball ahead, the sphere next,
     * the capsule last -- the order the formula gives -- and each
     * measured acceleration on its prediction, which is the engine
     * arriving at 5/7 g sin a on its own *)
    ("examples/software/PhysicsRoll3d", "race", 110, "x:1");
    (* seven seconds later, the same wall: standing, every body asleep,
     * no contact points solved at all. And the same seven seconds with
     * the solver turned off at the start ("s"), which is a heap. *)
    ("examples/software/PhysicsStack3d", "asleep", 400, "x:1");
    ("examples/software/PhysicsStack3d", "no_solver", 200, "s:2");
    (* the step offset: the 0.5 m step a wall at 0.4, a stair at 0.6
     * ("o" once); the slope limit: the 50 degree ramp a wall at 45, and
     * walked up at 60 ("l" once) *)
    (* the ragdoll at the foot of the stairs; and without the joints'
     * limits, an arm wound over the shoulder on the way down *)
    ("examples/software/PhysicsRagdoll3d", "fallen", 200, "x:1");
    ("examples/software/PhysicsRagdoll3d", "no_limits", 60, "l:1");
    ("examples/software/PhysicsWalk3d", "wall", 200, "up:2-200");
    ("examples/software/PhysicsWalk3d", "stair", 200, "o:1,up:2-200");
    ("examples/software/PhysicsWalk3d", "steep", 260, "left:2-161,up:162-260");
    ("examples/software/PhysicsWalk3d", "steep_60", 260, "l:1,left:2-161,up:162-260");
    (* the 3D turtle's drawings, all at once (the clock frozen): the
     * tree, its leaves; Hilbert's curve in 3D, level 2 *)
    ("examples/software/LogoFractals3d", "tree", 5, "a:2");
    ("examples/software/LogoFractals3d", "hilbert", 8, "right:2,a:4");
    (* four pieces dropped around the pit, a fifth on its way down: the
     * settled cubes darker the deeper they lie, and the lit ring of the
     * well marking the level this one will land on *)
    ( "games/puzzle/software/TinyBlockout",
      "pit",
      95,
      "space:1,left:5,left:10,space:15,right:20,right:25,right:30,space:35,up:40,up:45,space:50,down:55,down:60,down:65,space:70,x:75" );
    (* down the entrance corridor: the texture page on the walls, one
     * square of it per square of wall, hieroglyphs along the north side *)
    ("games/adventure/software/TinyTombRaider", "corridor", 90, "space:1,up:10-88");
    (* a boomerang in the air with its shadow under it (the one depth
     * cue this fixed, nearly isometric camera gets), all four still
     * standing; then, further in, the avocado in two halves, its cut
     * faces pale, and the flight that did it *)
    ("games/arcade/software/TinyBoomerangFu", "flight", 45, "space:1,right:5-25,space:26,right:30-60");
    (* claude: the song under way on the drums, on Easy (a pad a beat,
       no pedal): the count-in's hi-hat, the crash, snare, hi-hat,
       snare, each struck on its beat, the band playing its own parts
       beside you; the sound card fed 735 samples a frame, so the
       song's clock is exact *)
    ("games/rhythm/software/TinyRockBand", "gig", 340,
     "3:1,up:2,space:3,d:123,d:153,d:183,d:213,g:243,s:273,d:303,s:333");
    (* claude: the figure part way along the impossible beam, having
       stepped from the ground path onto a terrace three blocks up and
       three away without anything in between *)
    ("games/puzzle/software/TinyMonumentValley", "walked", 120, "space:2,at(120;-40):20-40,click:30");
    (* the first level, three-quarters on: Danny's slice in orange, the
       bridge and the wall behind in grey, at their depths *)
    ("games/puzzle/software/TinyCrush3d", "uncrushed", 20, "space:1");
    (* half way through the crush: the slices sliding in depth onto
       Danny's, the camera turning straight on *)
    ("games/puzzle/software/TinyCrush3d", "crushing", 30, "space:1,c:22");
    (* crushed, and a few steps: the crushed plane, the gap filled *)
    ("games/puzzle/software/TinyCrush3d", "crushed", 60, "space:1,c:22,right:45-60");
    (* the first picture: the runner on his ledge, the other out of
       reach, the far bridge high above them *)
    ("games/puzzle/software/TinyPerspective", "first", 10, "space:1");
    (* the camera lowered to the ledges' height: every top on the
       horizon, the far bridge between the ledges, the runner across *)
    ("games/puzzle/software/TinyPerspective", "horizon", 40, "space:1,Tab:5,down:6-17,Tab:25,right:28-40");
    (* claude: a run in progress: one death already paid for, so the
       gauge says run 2 and the life kept from the first *)
    ("games/rpg/software/TinyHades", "run", 300,
     "space:1,down:10-80,right:10-80,space:90,space:110,up:120-200,left:150-200,space:210,x:220,space:240,space:270,space:290");
    ("games/arcade/software/TinyBoomerangFu", "cut", 95, "space:1,up:5-40,right:41-70,space:71,right:75-140") ]

(* claude: played and flagged: TinyCyberSled's variants, each its own
 * section of the game *)
let scripted_flagged : Testutil_golden.scripted_flagged list =
  [ (* two missiles away, the second's smoke trailing towards the
     * computer's sled, beyond the pillar *)
    ("games/fps/software/TinyCyberSled", "missiles", 120, "space:1,v:2,Enter:5,Enter:60", [ "missiles=on" ]);
    (* the screen split, both players driving ahead and firing *)
    ( "games/fps/software/TinyCyberSled",
      "players2",
      100,
      "space:1,w:2-50,up:2-50,i:2-50,ArrowUp:2-50,q:40-100,Enter:40-100",
      [ "players=2" ] );
    (* across the arena, up the west ramp and off its high end: in the
     * air *)
    ( "games/fps/software/TinyCyberSled",
      "jump",
      520,
      "space:1,v:2,s:3-37,up:3-37,w:38-285,up:38-285,w:286-321,down:286-321,w:322-560,up:322-560",
      [ "ramps=on" ] ) ]

let tests = Testutil_golden.tests ~dir:"tests/3d" ~approve:"approve-golden3d" ~scripted ~scripted_flagged scenes
