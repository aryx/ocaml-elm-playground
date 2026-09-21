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
    ("examples/software/Typing", "", 5);
    (* the widgets at rest: nothing under the mouse, which waits at
     * (0, 0) -- pressing one is a sequence of frames, which the golden
     * frames cannot script (a script holds keys, not the mouse), so
     * that is gui/tests' business instead *)
    ("examples/software/GuiWidgets", "", 5);
    (* the first four 7GUIs tasks (Eugen Kiss, 2014), in immediate
     * mode: a counter, two fields that convert into each other, a
     * booking form whose rules turn its widgets off, and a timer --
     * which at frame 90 is a second and a half into its ten (under
     * the 100 frames past which 'make test' skips a scene, so that
     * the everyday run checks it) *)
    ("examples/software/Gui7Counter", "", 5);
    ("examples/software/Gui7Temperature", "", 5);
    ("examples/software/Gui7Flight", "", 5);
    ("examples/software/Gui7Timer", "", 90);
    (* the same counter written four ways, running four times at once:
     * the frame is the proof that they draw the same thing, and
     * gui/tests/Unit_architectures.ml is the proof that they keep
     * doing so as they are clicked *)
    ("examples/software/GuiFourWays", "", 5);
    (* the piece table, at rest: one piece, no versions behind it.
     * What typing into it does is gui/tests/Unit_text_edit.ml's
     * business -- a script holds keys, and a key is not a character
     * (see examples/Typing.ml) *)
    ("examples/software/GuiEditor", "", 5);
    ("examples/software/Gui7Circles", "", 5);
    (* a spreadsheet: the formulas computed, the numbers against the
     * right edge as VisiCalc put them, and the cursor on the cell the
     * bar is showing *)
    ("examples/software/Gui7Cells", "", 5);
    (* 1979, on a character display: green on black, the three status
     * lines, the block cursor, and the formulas in VisiCalc's own
     * spelling (+B3*2 rather than =B3*2) *)
    ("apps/software/TinyVisiCalc", "", 5);
    (* and 1985: the same engine with a menu bar, a formula bar and a
     * mouse *)
    ("apps/software/TinyExcel", "", 5);
    (* a page set by Knuth and Plass's breaker: justified, each line's
     * ratio in the margin, and the one loose line it could not avoid
     * without hyphenation marked *)
    ("examples/software/TypesetParagraph", "", 5);
    (* 1974: the page as it would print, its looks drawn by the pen from
     * Hershey's own strokes -- a bold title at 26, a bold word, an
     * italic one *)
    ("apps/software/TinyBravo", "", 5);
    (* 1985: the same page with a menu bar and a toolbar, its icons drawn
     * by the same pen as the text *)
    ("apps/software/TinyWord", "", 5);
    (* 1984: a picture as dots, drawn as rectangles -- a run of black
     * dots merged with the runs under it *)
    ("apps/software/TinyMacPaint", "", 5);
    (* 1994: a document of parts -- a text, a sheet and a picture side
     * by side, and a part of a kind nobody here can read, kept *)
    ("apps/software/TinyOpenDoc", "", 5);
    (* 1987: a slide made from the outline, in the master's look *)
    ("apps/software/TinyPowerPoint", "", 5);
    (* 1987: a card, its background's fields and buttons, and the page
     * number the stack's script wrote on it when it opened *)
    ("apps/software/TinyHyperCard", "", 5);
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
    ("games2.5d/software/TinyWolfenstein", "", 5);
    ("games2.5d/software/TinyOutRun", "", 5);
    ("games/software/TinyBomberman", "", 5);
    ("games/software/TinyTron", "", 5);
    ("games/software/TinyMicroMachines", "", 5);
    (* the physics plan's: explicit Euler's orbit, spiraling out after 8
     * seconds; the artillery and Spacewar! titles *)
    ("examples/software/PhysicsOrbit", "", 480);
    ("games/software/TinyWorms", "", 5);
    ("games/software/TinySpacewar", "", 5);
    (* the bounces after 5 seconds: clay flat, the superball back up;
     * Pong's title *)
    ("examples/software/PhysicsBounce", "", 300);
    ("games/software/TinyPong", "", 5);
    (* 300 marbles after 2 seconds, all pairs: 44850 box tests *)
    ("examples/software/PhysicsMarbles", "", 120);
    (* rotation: boxes tipping over and tumbling, a ball rolling down *)
    ("examples/software/PhysicsBoxes", "", 120);
    ("games/software/TinyCameltry", "", 5);
    (* stacking: the pyramid standing still after 5 seconds *)
    ("examples/software/PhysicsPyramid", "", 300);
    ("games/software/TinySlingshot", "", 5);
    (* a mass on a spring, a chain of springs, a rope of sticks *)
    ("examples/software/PhysicsElastic", "", 60);
    ("games/software/TinySoldat", "", 5);
    (* the planets on 2000-01-02, Jupiter and Saturn near their May 2000
     * conjunction *)
    ("examples/software/PhysicsSolarSystem", "", 2);
    (* the audio plan's first examples (their sounds: -dump-audio) *)
    ("examples/software/AudioTheremin", "", 2);
    ("examples/software/AudioPiano", "", 2);
    ("examples/software/AiTictactoe", "", 3);
    ("examples/software/AiPathfinding", "", 60);
    ("games/software/AiOthello", "", 3);
    ("games/software/TinyTowerDefense", "", 5);
    ("games/software/TinyDune2", "", 5);
    ("games/software/TinyWarcraft2", "", 5);
    ("games/software/TinySonic", "", 5);
    ("games/software/TinyRobotron", "", 5);
    ("games/software/TinyPinball", "", 5);
    ("games/software/TinyPortal2D", "", 5);
    ("games/software/TinyGauntlet2", "", 5);
    ("games/software/TinyKickOff2", "", 5);
    ("games/software/TinySpeedball2", "", 5);
    ("games/software/TinySensibleSoccer", "", 5);
    ("games/software/TinyJoust", "", 5);
    ("games/software/TinyDefender", "", 5);
    ("games/software/TinyCeleste", "", 5);
    ("games/software/TinyDDR", "", 5);
    ("games2.5d/software/TinyGuitarHero", "", 5);
    (* the sound, seen (the "v" debug key, Audio_debug): TinyMario's
     * music at 1 s, as an oscilloscope, then a spectrum *)
    ("games/software/TinyMario", "v", 60);
    ("games/software/TinyMario", "vv", 60);
    ("games/software/TinyFlappyBird", "", 5);
    ("games/software/TinyBreakout", "", 5);
    ("games/software/TinyXpilot", "", 5);
    ("games/software/TinyGalaga", "", 5);
    ("games/software/TinyDonkeyKong", "", 5);
    ("games/software/TinyLodeRunner", "", 5);
    ("games/software/TinyRick", "", 5);
    ("games/software/TinyGradius", "", 5);
    ("games/software/TinyZelda", "", 5);
    ("games/software/TinyRogue", "", 5);
    ("games/software/TinyStreetFighter", "", 5);
    ("games/software/TinyFinalFight", "", 5);
    (* the world programs of How to Design Programs (Bigbang.mli): the
     * rocket coming down; the worm dead against the border, its
     * epitaph (last_picture) *)
    ("examples/software/BigBangRocket", "", 60);
    ("games/software/TinyBabaIsYou", "", 5);
    ("examples/software/BigBangWorm", "", 400);
    ("games2.5d/software/TinyKart", "", 5);
    ("games2.5d/software/TinyDoom", "", 5);
    (* "r" twice: a third of the resolution, 3x3 pixels (Pixelate) *)
    ("games2.5d/software/TinyDoom", "rr", 5);
    ("games2.5d/software/TinyComanche", "", 5);
    ("games2.5d/software/TinyDescent", "", 5);
    ("games2.5d/software/TinyElite", "", 5);
    (* the title's tank, turned by 40 degrees *)
    ("games2.5d/software/TinyBattlezone", "", 40);
    ("games/software/TinyMissileCommand", "", 5);
    ("games/software/TinyLemmings", "", 5);
    ("games/software/TinyPuzzleBobble", "", 5);
    ("games2.5d/software/TinyDungeonMaster", "", 5);
    ("games2.5d/software/TinyZaxxon", "", 5);
    ("games2.5d/software/TinyDiablo", "", 5);
    (* a whole game as a map and one rule (playground/Puzzlescript) *)
    ("examples/software/PuzzleScriptSokoban", "", 5);
    ("examples/software/PuzzleScriptBoulders", "", 5);
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
    ("games2.5d/software/TinyWolfenstein", "treasure", 60, "right:1-16,up:20-60");
    (* flat out into the first curve, which bends right *)
    ("games2.5d/software/TinyOutRun", "curve", 230, "space:1,up:2-230");
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
    ("examples/software/PhysicsOrbit", "semi", 480, "space:1-2");
    (* a shot, pushed back by the wind, digging its crater *)
    ("games/software/TinyWorms", "shot", 150, "space:2-3,space:10-11");
    (* both ships thrusting, turning and firing around the star *)
    ("games/software/TinySpacewar", "duel", 120, "space:2-3,up:10-60,left:30-45,down:50,down:70,w:10-40,s:55,s:75");
    (* a serve, returned by the computer, missed by the player *)
    ("games/software/TinyPong", "rally", 150, "space:2-3,w:100-160");
    (* the same marbles, the same frame (the three methods find the same
     * pairs), with the grid and its count *)
    ("examples/software/PhysicsMarbles", "grid", 120, "space:60-61");
    (* rotation off: boxes balanced on their corners, stuck on the ramp *)
    ("examples/software/PhysicsBoxes", "upright", 300, "u:2-3");
    (* the maze turned right then left, the moon rolling, a target taken *)
    ("games/software/TinyCameltry", "turns", 200, "space:2-3,right:30-75,left:150-200");
    (* the maze turned 30 degrees: the moon rolls away; upright, it
     * slides, and friction holds it (up to 39 degrees): still there *)
    ("games/software/TinyCameltry", "tilt", 150, "space:2-3,right:60-74");
    ("games/software/TinyCameltry", "tilt_upright", 150, "space:2-3,u:5,right:60-74");
    (* the same pyramid without the solver (phase 7's engine): a heap;
     * and hit by the ball, its top knocked off *)
    ("examples/software/PhysicsPyramid", "no_solver", 300, "s:2");
    ("examples/software/PhysicsPyramid", "ball", 200, "space:120");
    (* a shot along the dotted arc, the tower tumbling; and without the
     * solver, the tower slumping by itself, no shot *)
    ("games/software/TinySlingshot", "shot", 130, "space:2,space:60");
    ("games/software/TinySlingshot", "no_solver", 120, "space:2,s:5");
    (* all three kicked; and the chain too stiff for the time step,
     * exploding in 8 steps *)
    ("examples/software/PhysicsElastic", "kick", 60, "space:30");
    ("examples/software/PhysicsElastic", "stiff", 8, "x:2");
    (* the bots' fight, GREEN's ragdoll tumbling; the player running,
     * jumping, flying on the jets, shooting, a grenade, the blasts *)
    ("games/software/TinySoldat", "bots", 260, "space:2");
    ("games/software/TinySoldat", "jets", 130, "space:2,d:10-70,w:30,w:40-90,space:100-160,q:120");
    (* the true distances, the inner planets crowded; and 80 days a
     * second for 2 seconds: mid-2000 *)
    ("examples/software/PhysicsSolarSystem", "true_distances", 3, "d:2");
    ("examples/software/PhysicsSolarSystem", "later", 120, "up:2,up:4");
    (* keys held, lit; the square wave *)
    ("examples/software/AudioPiano", "keys", 30, "space:2,a:10-30,g:10-30,u:10-30");
    ("examples/software/AiTictactoe", "played", 40, "space:2");
    ("examples/software/AiPathfinding", "breadth_first", 120, "b:2");
    ("examples/software/AiPathfinding", "dijkstra", 120, "d:2");
    ("games/software/AiOthello", "values", 3, "v:2");
    ("games/software/TinyDune2", "harvesting", 900, "space:1,b:30,f:100-900");
    ("games/software/TinySonic", "loop", 330, "space:1,right:5-330");
    (* the two sticks: running right, shooting left, the grunts closing
       in and two of them shot down *)
    ("games/software/TinyRobotron", "twin_stick", 95, "space:1,a:20-95,right:20-60,up:62-95");
    (* the plunger pulled and let go: the ball up the lane, under the
       dome and into the table, past the bumpers *)
    ("games/software/TinyPinball", "launch", 90, "space:1-40");
    ("games/software/TinyGauntlet2", "crowd", 400, "space:1,down:30-90,right:100-200,space:220-400");
    ("games/software/TinyKickOff2", "shot", 260, "space:1,up:70-200,space:150-170,right:171-260");
    ("games/software/TinySpeedball2", "match", 700, "space:1,up:60-200,space:120-140,left:210-400,space:260-280");
    ("games/software/TinySensibleSoccer", "loft", 300, "space:1,up:40-150,space:160-200,right:201-300");
    (* the planet, the scanner reading it, and a lander on its way
       down to a human *)
    ("games/software/TinyDefender", "patrol", 95,
     "space:1,right:5-200,space:60,space:90,space:120,down:130-170,space:150,space:200,right:210-320,space:240,space:280,space:310");
    (* the dungeon, the dark around it, and a click being walked to:
       the first golden frames here that are played with a mouse *)
    ("games2.5d/software/TinyDiablo", "dungeon", 95,
     "click:2,at(150;20):6-60,click:8,at(-120;-40):70-140,click:72,at(60;120):150-260,click:152,rclick:200,click:230");
    (* a bolt cast (the mana orb half down), an imp at arm's length *)
    ("games2.5d/software/TinyDiablo", "fight", 260,
     "click:2,at(150;20):6-60,click:8,at(-120;-40):70-140,click:72,at(60;120):150-260,click:152,rclick:200,click:230");
    (* the fortress, a wall flown through and the next one coming, and
       the fighter over its own shadow: the gap between the two is the
       altitude, which is the whole game *)
    ("games2.5d/software/TinyZaxxon", "fortress", 95,
     "space:1,space:60,right:120-150,up:180-210,space:200,left:260-300,space:300,down:330-360,space:380,right:400-430,up:430-470,space:470");
    (* deeper in: three walls, a tower, and a shot on its way *)
    ("games2.5d/software/TinyZaxxon", "deep", 260,
     "space:1,space:60,right:120-150,up:180-210,space:200,left:260-300,space:300,down:330-360,space:380,right:400-430,up:430-470,space:470");
    (* the first four steps danced, judged by the music's clock: the
       card is fed 735 samples a frame here, so the song's time is
       exact, and a press on a frame lands about 35 ms early by it --
       GREAT rather than PERFECT, the offset the calibration is for *)
    ("games/software/TinyDDR", "steps", 318, "space:1,left:228,up:256,up:284,down:312");
    (* the riff's first four notes on Medium, every fret held and each
       strummed on its beat (132 a minute, an eighth 0.227 s) *)
    ("games2.5d/software/TinyGuitarHero", "riff", 305,
     "space:1,a:200-305,s:200-305,d:200-305,f:200-305,g:200-305,space:217,space:245,space:272,space:299");
    (* the first ledge reached with a held jump, the four lies all on *)
    ("games/software/TinyCeleste", "climb", 60, "space:1,right:10-40,space:22-34");
    (* a jump, then a dash spent straight up: the hair gone blue, which
       is the only interface Celeste needs *)
    ("games/software/TinyCeleste", "dash", 48, "space:1,right:10-60,space:22-40,up:36-50,x:38");
    (* off the bank on six flaps, the buzzards already coming *)
    ("games/software/TinyJoust", "flaps", 95, "space:1,space:20,space:35,space:50,space:65,space:80,right:10-95");
    (* the scanner is the game: three abductions are under way in the
       strip while the screen shows one lander and a laser *)
    ("games/software/TinyDefender", "hunt", 320,
     "space:1,right:5-200,space:60,space:90,space:120,down:130-170,space:150,space:200,right:210-320,space:240,space:280,space:310");
    (* a flap every quarter of a second is a climb: he ends up under
       the eyries with a buzzard coming up at him, which is the whole
       game -- be the higher one when you meet *)
    ("games/software/TinyJoust", "flight", 200,
     "space:1,space:20,space:35,space:50,space:65,space:80,space:95,space:110,space:125,space:140,space:155,space:170,space:185,right:10-105,left:125-200");
    (* the first chamber: the goo between him and the way out (the
       portals themselves want a mouse, which a script has none of, so
       they are in tests/games/ instead) *)
    ("games/software/TinyPortal2D", "chamber1", 60, "space:1,right:12-55");
    (* the dungeon, and later the crowd the generators have poured
       into it (chase=field is a flag, which a golden cannot pass: the
       two chases are compared in tests/games/ instead) *)
    ("games/software/TinyGauntlet2", "dungeon", 95, "space:1,down:30-95");
    (* the kick off, and (heavy) a shot bent in the air by the
       aftertouch, which is the game's other idea *)
    ("games/software/TinyKickOff2", "kickoff", 95, "space:1,up:20-95");
    (* the metal, its furniture, and (heavy) a match well under way:
       the score is mostly what the arena paid *)
    ("games/software/TinySpeedball2", "arena", 95, "space:1,up:20-95");
    (* the pitch pulled back, and (heavy) a lofted ball bent in the
       air, which is the game everybody remembers *)
    ("games/software/TinySensibleSoccer", "pitch", 95, "space:1,up:20-95");
    ("games/software/TinyWarcraft2", "crowd", 300, "space:1,a:10,p:14,right:20-44,space:50");
    ("games/software/TinyTowerDefense", "maze", 400, "space:1,right:20-40,space:45,up:50-56,space:60,up:64-70,space:74,left:80-84,space:90");
    ("games/software/AiOthello", "reply", 60, "space:2");
    (* flaps timed to thread 5 pipes (the pipes from the LFSR's seed=1);
     * and no flap after the first: the bird on the ground, game over *)
    ( "games/software/TinyFlappyBird",
      "fly",
      600,
      "space:1,space:5,space:51,space:91,space:131,space:172,space:212,space:258,space:299,space:346,space:386,space:426,space:447,space:472,space:512,space:555,space:596" );
    ("games/software/TinyFlappyBird", "crash", 200, "space:1,space:5");
    (* a take-off from the base, a turn, two shots; by the fuel station,
     * its beam refueling the ship, the shield up *)
    ("games/software/TinyXpilot", "refuel", 150, "space:1,up:5-40,right:22-28,up:60-80,space:100,space:115,down:140-150");
    (* the first waves flying in along their curves; the formation, all
     * in, a boss diving, the fighter having fired *)
    ("games/software/TinyGalaga", "waves", 200, "space:1");
    ("games/software/TinyGalaga", "formation", 1100, "space:1,space:900,space:930,left:950-980,space:990");
    (* the first waves, a turret firing; later, a red one shot down *)
    ("games/software/TinyGradius", "waves", 200, "space:1,right:5-30,space:40,space:60,space:80,space:100,space:130,space:150,space:170,space:190,up:120-150");
    ("games/software/TinyGradius", "later", 600, "space:1,right:5-30,space:40,space:60,space:80,space:100,space:130,space:150,space:170,space:190,up:120-150,space:220,space:240,space:260,space:300,space:330");
    (* the sword taken, and swung; walking into the next room, the
     * screen sliding to it *)
    ("games/software/TinyZelda", "sword", 130, "space:1,right:3-42,up:43-110,space:120");
    ("games/software/TinyZelda", "slide", 370, "space:1,right:3-42,up:43-110,down:130-200,right:210-400");
    (* the first room, walked around, a bat fought *)
    ("games/software/TinyRogue", "level", 120, "space:2,right:10,right:14,right:18,right:22,right:26,right:30,up:40,up:44,up:48,left:60,left:64,down:80,down:84,down:88,down:92");
    (* a fireball thrown (down, down-forward, forward, punch), the
     * computer jumping it; later, trading blows *)
    ("games/software/TinyStreetFighter", "fireball", 40, "space:1,s:3-8,d:6-12,f:11");
    ("games/software/TinyStreetFighter", "fight", 150, "space:1,d:95-120,f:122,g:135");
    (* the first wave on the street; the spin *)
    ("games/software/TinyFinalFight", "wave", 160, "space:1,right:3-60,space:100,space:108,space:116,space:124,space:132");
    ("games/software/TinyFinalFight", "spin", 206, "space:1,right:3-60,space:100,space:108,space:116,space:124,space:132,z:200");
    (* level 1, Baba pushing the rocks; ten moves right: won *)
    ("games/software/TinyBabaIsYou", "pushing", 30, "space:1,right:5,right:10,right:15,right:20,right:25,right:30,right:35,right:40,right:45,right:50");
    ("games/software/TinyBabaIsYou", "won", 60, "space:1,right:5,right:10,right:15,right:20,right:25,right:30,right:35,right:40,right:45,right:50");
    (* running from the boulder; through the hole, the boulder stopped
     * over it (too big to fall in) *)
    (* the grid, GO!; the first corner, the karts passed on the way
     * coming up behind *)
    ("games2.5d/software/TinyKart", "grid", 200, "space:1");
    ("games2.5d/software/TinyKart", "corner", 430, "space:1,up:2-430,right:370-405");
    (* on the stairs, upstairs ahead; at the window onto the dark room
     * (games3d/TinyDoom3d's golden frames are the same walks) *)
    ("games2.5d/software/TinyDoom", "stairs", 80, "left:1-10,up:11-80");
    ("games2.5d/software/TinyDoom", "window", 60, "right:1-5,up:6-55");
    (* over the island, climbing a little (games3d/TinyComanche3d's
     * golden frame is the same flight) *)
    ("games2.5d/software/TinyComanche", "island", 70, "up:1-70,w:1-20");
    (* down the corridor, the robot of the next cell ahead
     * (games3d/TinyDescent3d's golden frames are the same flight) *)
    ("games2.5d/software/TinyDescent", "corridor", 30, "w:1-30");
    (* launched from the station: Coriolis ahead, its slot turning, in
     * front of Lave; the three Sidewinders coming *)
    ("games2.5d/software/TinyElite", "flight", 200, "space:1");
    (* turned, driving towards a pyramid (cut by the near plane), a shell
     * flying at the enemy tank, at the height of its hull *)
    ("games2.5d/software/TinyBattlezone", "play", 150, "space:1,right:5-20,up:30-140,space:100");
    ("games/software/TinyRick", "boulder", 70, "space:1,right:2-160");
    ("games/software/TinyRick", "hole", 160, "space:1,right:2-160");
    (* right to the ladder, up it, a hole dug on the right; the guards
     * coming *)
    ("games/software/TinyLodeRunner", "dig", 110, "space:1,right:2-41,up:42-81,x:86");
    (* Jumpman walks to the first ladder and climbs it, the barrels
     * rolling down *)
    ("games/software/TinyDonkeyKong", "climb", 400, "space:1,right:5-230,up:231-300");
    (* two players: both take off, turn, fire; the camera zoomed out to
     * frame them both, blue's shield up *)
    ("games/software/TinyXpilot", "duel", 200, "2:1,w:5-60,d:20-26,up:5-60,left:20-26,w:100-110,up:100-110,space:120,space:140,return:130,s:190-200");
    (* the same, the screen split: a camera for each, the walls cut at
     * each view's edge *)
    ("games/software/TinyXpilot", "split", 200, "3:1,w:5-60,d:20-26,up:5-60,left:20-26,w:100-110,up:100-110,space:120,space:140,return:130,s:190-200");
    (* a serve, the paddle moved to where the ball comes down each time
     * (aiming off-center, so the ball goes to the side): 15 seconds,
     * 11 points, the ball sped up *)
    ( "games/software/TinyBreakout",
      "play",
      900,
      "space:1,space:5,right:6-11,right:210-229,left:396-403,right:570-570,right:740-740" );
    (* the turtle's drawings, all at once (the clock frozen, it wouldn't
     * move): Koch's filled snowflake; the dragon, 4096 lines *)
    ("examples/software/LogoFractals", "snowflake", 5, "a:2");
    ("examples/software/LogoFractals", "dragon", 10, "right:2,right:4,right:6,a:8");
    (* the crosshair raised, a counter-missile from each base: flying,
     * then exploding there, the three explosions one *)
    ("games/software/TinyMissileCommand", "fire", 45, "space:1,up:2-30,a:32,s:33,d:34");
    ("games/software/TinyMissileCommand", "explosions", 95, "space:1,up:2-30,a:32,s:33,d:34");
    (* the lemmings out of the hatch, walking, "4" picking the diggers *)
    ("games/software/TinyLemmings", "walking", 500, "space:1,4:10");
    ("games/software/TinyPuzzleBobble", "pop", 52, "space:1,left:3-12,space:20");
    (* turned east and walked down the corridor: the torch ahead in
     * its slot, a wall on the right, the dark past the light's reach *)
    ("games2.5d/software/TinyDungeonMaster", "corridor", 80, "space:1,right:5-6,up:15-60");
    (* the one push that solves the first level, and the banner *)
    ("examples/software/PuzzleScriptSokoban", "solved", 40, "left:2-3");
    (* claude: the first scenes here that use the mouse (-script's
     * at(x;y), click and rclick, see Input_script.mli), which is what
     * an application is made of and what no game golden needed.
     *
     * Three circles put down by clicking, the last one under the
     * pointer and so drawn lit; then the 7GUIs task's real question:
     * a right click opens the dialog, the slider is dragged over ten
     * frames, and "done" records it -- as ONE edit ("back 3": two
     * circles and one adjustment), which is the bug the task exists
     * to catch *)
    ( "examples/software/Gui7Circles",
      "drawn",
      40,
      "at(-150;100):1-8,click:5,at(60;40):9-16,click:12,at(-20;20):17-24,click:20,at(60;40):25-40" );
    ( "examples/software/Gui7Circles",
      "adjusted",
      42,
      "at(-150;100):1-8,click:5,at(60;40):9-24,click:12,rclick:20,at(-60;-258):25-27,click:26-34,at(-30;-258):28-34,at(0;-307):36-42,click:38" );
    (* a slider being dragged: the knob under the mouse and held, the
     * disc as big as the drag has made it. No golden frame could show
     * that before the script could hold a button down *)
    ("examples/software/GuiWidgets", "dragging", 20, "at(-330;10):1-20,click:5-20,at(-280;10):8-20");
    (* clicking a cell of the spreadsheet: the cursor moves there and
     * the bar shows what was typed into it (the typing itself a
     * script cannot do -- a key is not a character, examples/Typing) *)
    ("examples/software/Gui7Cells", "picked", 20, "at(-180;100):1-20,click:8");
    (* the arrows are the whole interface: the cursor walked to B3,
     * and the line at the top showing what is in it -- @SUM(B4...B6),
     * as 1979 spelled it. (The slash commands take characters, which
     * a script cannot send: a key is not a character.) *)
    ("apps/software/TinyVisiCalc", "cursor", 16, "right:3,down:6,down:10");
    (* what 1985 bought, in one scripted run: a range dragged out with
     * the mouse (D2 to D5), Edit > Fill Down copying the formula into
     * it -- =B2*C2 becoming =B3*C3, =B4*C4, =B5*C5, which is what
     * relative references are for -- the total following, and
     * Chart > Show drawing the bars *)
    (* editing a cell through the bar, which is scriptable because
     * backspace is a *key* and not a character: C2 selected, the bar
     * clicked, one backspace turning 120 into 12, Enter -- and D2,
     * which reads C2, following to 54. It is here because it did not
     * work: the bar was refreshed from the cell on every frame, so a
     * keystroke was undone before it could be seen *)
    (* the same page, switched to greedy through the dropdown: three
     * rivers marked where Knuth-Plass had one, "shrunk until the line
     * fills the" stretched to 2.80 -- the lesson of the program, in
     * one frame *)
    ( "examples/software/TypesetParagraph",
      "greedy",
      20,
      "at(-146;455):1-5,click:3,at(-146;419):6-12,click:8,at(300;-300):13-20" );
    (* Bravo's modes, which a script can now reach because it can type:
     * "edit" typed in command mode -- e selects everything, d deletes
     * it, i starts inserting, and a t is all that is left -- then
     * Escape and two undos, one per command, bringing it all back *)
    ("apps/software/TinyBravo", "edit", 12, "type(edit):10");
    ("apps/software/TinyBravo", "undone", 22, "type(edit):10,escape:14,type(uu):18");
    (* a selection dragged with the mouse, then l u and l s from the
     * keyboard: underlined and struck, the pen's two rules *)
    ( "apps/software/TinyBravo",
      "looks",
      20,
      "at(-222;285):1-6,click:5-10,at(-118;285):8-10,type(lu):14,type(ls):16,at(300;-400):17-20" );
    (* Tesler's answer: no modes, so "edit" typed anywhere is the word
     * edit, and the four letters are one "Undo Typing" *)
    ("apps/software/TinyWord", "typed", 12, "type( edit):10");
    (* a selection dragged with the mouse, the B icon, then the centring
     * one: the looks are the selection's, the alignment the page's *)
    ( "apps/software/TinyWord",
      "looks",
      24,
      "at(-222;285):1-6,click:5-10,at(-118;285):8-12,at(-300;428):14-16,click:15,at(-82;428):18-20,click:19,at(300;-400):21-24"
    );
    (* the filled oval and the diagonal pattern, rubber-banded from one
     * corner to the other; then the bucket and grey, poured into the
     * house's window *)
    ( "apps/software/TinyMacPaint",
      "shapes",
      30,
      "at(-398;202):1-3,click:2,at(88;-160):4-6,click:5,at(-260;350):7-10,click:9-16,at(-200;300):12,at(-120;250):13-17,at(-398;370):18-20,click:19,at(-104;-160):21-23,click:22,at(-4;112):24-27,click:25,at(400;-400):28-30"
    );
    (* the sun selected, then dragged left: lifted, white left behind,
     * put down opaque over the roof, the ants round it *)
    ( "apps/software/TinyMacPaint",
      "move",
      26,
      "at(-440;370):1-3,click:2,at(190;350):4-7,click:6-12,at(310;230):9-14,at(250;290):15-17,click:17-24,at(100;290):20,at(0;290):21-26"
    );
    (* the sheet clicked once (selected), twice (active: the hatched
     * border, and its menu in the document's bar), then B1 clicked and
     * =B2*2 typed into it -- the total follows *)
    ( "apps/software/TinyOpenDoc",
      "sheet",
      18,
      "at(-195;245):1-18,click:2,click:5,click:8,type(=B2*2):10,return:13" );
    (* the picture activated, the fill tool and grey chosen from its own
     * menu, poured into the sky -- the sun's grey joins it seamlessly,
     * the pattern being laid from the picture's corner -- and put down
     * with Escape *)
    ( "apps/software/TinyOpenDoc",
      "picture",
      26,
      "at(200;250):1-6,click:2,click:5,at(-125;470):7-9,click:8,at(-125;293):10-12,click:11,at(-125;470):13-15,click:14,at(-125;221):16-18,click:17,at(200;255):19-21,click:20,escape:23,at(400;-400):22-26"
    );
    (* File > Save, then File > Revert: the document read back through
     * the registry, the unknown part included, the same *)
    ( "apps/software/TinyOpenDoc",
      "reverted",
      16,
      "at(-410;470):1-3,click:2,at(-410;401):4-6,click:5,at(-410;470):7-9,click:8,at(-410;365):10-12,click:11,at(400;-400):13-16"
    );
    (* the master changed twice -- a black band, titles centred -- and
     * every slide changes with it *)
    ( "apps/software/TinyPowerPoint",
      "master",
      16,
      "at(-30;470):1-3,click:2,at(-30;293):4-6,click:5,at(-30;470):7-9,click:8,at(-30;257):10-12,click:11,at(400;-480):13-16"
    );
    (* the sorter: every slide, the same drawing scaled *)
    ( "apps/software/TinyPowerPoint",
      "sorter",
      10,
      "at(-220;470):1-3,click:2,at(-220;329):4-6,click:5,at(400;-480):7-10" );
    (* the outline, and a line typed against its edge: a sixth slide,
     * shown beside it as it is typed *)
    ( "apps/software/TinyPowerPoint",
      "outline",
      18,
      "at(-220;470):1-3,click:2,at(-220;365):4-6,click:5,at(80;-66):7-9,click:8,return:10,type(Questions?):12,at(400;-480):13-18"
    );
    (* the show, caught halfway through pushing slide 1 away for slide
     * 2: the two drawings, moved *)
    ( "apps/software/TinyPowerPoint",
      "show",
      14,
      "at(-220;470):1-3,click:2,at(-220;293):4-6,click:5,right:9,at(0;0):12-14" );
    (* slide 4's sheet clicked (activated: its menu in the bar), B1
     * clicked and 3 typed into it -- B3, =B2/B1, follows *)
    ( "apps/software/TinyPowerPoint",
      "part",
      24,
      "right:2,right:4,right:6,at(175;122):8-24,click:9,click:12,type(3):15,return:17" );
    (* typing on the slide itself: a point clicked at its end, a word
     * added, Enter for a new point, Tab to push it a level down -- all
     * of it edits of the outline's lines *)
    ( "apps/software/TinyPowerPoint",
      "typed",
      16,
      "at(300;66):1-3,click:2,type( Plus):5,return:7,type(Two years after the first Mac):9,tab:11,at(400;-480):12-16"
    );
    (* Next, then the button clicked three times: its script counts *)
    ( "apps/software/TinyHyperCard",
      "clicks",
      16,
      "at(299;-204):1-4,click:2,at(0;79):5-16,click:7,click:10,click:13" );
    (* the message path: "Pass it on" answers and passes, the card's
     * script answers next -- twice *)
    ( "apps/software/TinyHyperCard",
      "path",
      16,
      "at(299;-204):1-7,click:2,click:5,at(-236;150):8-16,click:9,click:12" );
    (* ten clicks, and the script's "answer" *)
    ( "apps/software/TinyHyperCard",
      "answer",
      30,
      "at(299;-204):1-4,click:2,at(0;79):5-40,click:6,click:8,click:10,click:12,click:14,click:16,click:18,click:20,click:22,click:24"
    );
    (* the button tool, the button selected, Objects > Script...: what it
     * does, to read and change *)
    ( "apps/software/TinyHyperCard",
      "script",
      20,
      "at(299;-204):1-4,click:2,at(455;202):5-7,click:6,at(0;79):8-10,click:9,at(-190;470):11-13,click:12,at(-190;401):14-16,click:15,at(600;-600):17-20"
    );
    (* Objects > New Button, dragged where it goes *)
    ( "apps/software/TinyHyperCard",
      "new",
      20,
      "at(-190;470):1-3,click:2,at(-190;257):4-6,click:5,at(0;33):7-9,click:9-14,at(-100;33):11,at(-200;-80):12-16,at(600;-600):17-20"
    );
    ( "apps/software/TinyExcel",
      "edited",
      20,
      "at(19;84):1-5,click:3,at(100;181):6-10,click:8,backspace:11,return:14,at(19;84):16-20" );
    ( "apps/software/TinyExcel",
      "filled",
      40,
      "at(122;84):1-6,click:5-13,at(122;40):8-10,at(122;0):11-13,at(-45;227):15-18,click:16,at(-45;120):19-24,click:21,at(70;227):26-29,click:27,at(70;156):30-36,click:32,at(122;0):37-40" );
  ]

let tests = Testutil_golden.tests ~dir:"tests/2d" ~approve:"approve-golden2d" ~scripted scenes
