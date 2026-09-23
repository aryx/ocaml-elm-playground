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
    (* 7GUIs 5: a list, a filter, and a selection kept as a person *)
    ("examples/software/Gui7Crud", "", 5);
    (* 1979, on a character display: green on black, the three status
     * lines, the block cursor, and the formulas in VisiCalc's own
     * spelling (+B3*2 rather than =B3*2) *)
    ("apps/office/software/TinyVisiCalc", "", 5);
    (* and 1985: the same engine with a menu bar, a formula bar and a
     * mouse *)
    ("apps/office/software/TinyExcel", "", 5);
    (* a page set by Knuth and Plass's breaker: justified, each line's
     * ratio in the margin, and the one loose line it could not avoid
     * without hyphenation marked *)
    ("examples/software/TypesetParagraph", "", 5);
    (* 1974: the page as it would print, its looks drawn by the pen from
     * Hershey's own strokes -- a bold title at 26, a bold word, an
     * italic one *)
    ("apps/office/software/TinyBravo", "", 5);
    (* 1985: the same page with a menu bar and a toolbar, its icons drawn
     * by the same pen as the text *)
    ("apps/office/software/TinyWord", "", 5);
    (* 1984: a picture as dots, drawn as rectangles -- a run of black
     * dots merged with the runs under it *)
    ("apps/office/software/TinyMacPaint", "", 5);
    ("apps/gamedev/software/TinyAseprite", "", 5);
    ("apps/gamedev/software/TinyTiled", "", 5);
    (* 1970: the Model D's panel, black between wooden cheeks, left to
     * right; the bass preset *)
    ("apps/music/software/TinyMinimoog", "", 5);
    (* 1987: the tracker, our song's first pattern, four channels, the
     * current row in the middle *)
    ("apps/music/software/TinySoundtracker", "", 5);
    (* 1991-2001: the media player, the first item of our playlist, an
     * ABC round, as a piano roll (the note sounding lit, mid-note: its
     * scope and spectrum full) *)
    ("apps/media/software/TinyMediaPlayer", "", 80);
    (* 1994: a document of parts -- a text, a sheet and a picture side
     * by side, and a part of a kind nobody here can read, kept *)
    ("apps/office/software/TinyOpenDoc", "", 5);
    (* 1987: a slide made from the outline, in the master's look *)
    ("apps/office/software/TinyPowerPoint", "", 5);
    (* 1987: a card, its background's fields and buttons, and the page
     * number the stack's script wrote on it when it opened *)
    ("apps/office/software/TinyHyperCard", "", 5);
    (* 1984: a picture made of objects, not of dots *)
    ("apps/office/software/TinyMacDraw", "", 5);
    (* 1986: one text flowing over pages from a master page, a sheet
     * anchored in it *)
    ("apps/office/software/TinyFrameMaker", "", 5);
    (* the office suite today: first the kind of document *)
    ("apps/office/software/TinyOffice", "", 5);
    ("examples/software/Smiley", "", 5);
    ("examples/software/Words", "", 5);
    ("examples/software/Words", "n", 5);
    ("examples/software/Misc", "", 5);
    ("examples/software/Animation", "", 5);
    ("examples/software/Mouse", "", 5);
    ("examples/software/Keyboard", "", 5);
    ("games/arcade/software/Pong", "", 5);
    ("games/shmup/software/Asteroid", "", 5);
    ("games/shmup/software/Asteroid", "f", 5);
    (* claude: random, but with the runner's seed=1 flag the same every
     * run (see Testutil_golden.render) *)
    ("games/arcade/software/Snake", "", 5);
    ("games/puzzle/software/Tetris", "", 5);
    ("games/platform/software/TinyMario", "", 5);
    ("games/shmup/software/TinyInvaders", "", 5);
    ("games/puzzle/software/TinySokoban", "", 5);
    ("games/puzzle/software/TinySokobanEd", "", 5);
    ("games/arcade/software/TinyPacman", "", 5);
    ("games/fps/software/TinyWolfenstein", "", 5);
    ("games/racing/software/TinyOutRun", "", 5);
    ("games/arcade/software/TinyBomberman", "", 5);
    ("games/arcade/software/TinyTron", "", 5);
    ("games/racing/software/TinyMicroMachines", "", 5);
    ("games/racing/software/TinyGranTrak10", "", 5);
    ("games/racing/software/TinySuperSprint", "", 5);
    ("games/racing/software/TinySupercars", "", 5);
    ("games/racing/software/TinySuperOffRoad", "", 5);
    (* the physics plan's: explicit Euler's orbit, spiraling out after 8
     * seconds; the artillery and Spacewar! titles *)
    ("examples/software/PhysicsOrbit", "", 480);
    ("games/strategy/software/TinyWorms", "", 5);
    ("games/arcade/software/TinySpacewar", "", 5);
    (* the bounces after 5 seconds: clay flat, the superball back up;
     * Pong's title *)
    ("examples/software/PhysicsBounce", "", 300);
    ("games/arcade/software/TinyPong", "", 5);
    (* 300 marbles after 2 seconds, all pairs: 44850 box tests *)
    ("examples/software/PhysicsMarbles", "", 120);
    (* rotation: boxes tipping over and tumbling, a ball rolling down *)
    ("examples/software/PhysicsBoxes", "", 120);
    ("games/arcade/software/TinyCameltry", "", 5);
    (* stacking: the pyramid standing still after 5 seconds *)
    ("examples/software/PhysicsPyramid", "", 300);
    ("games/puzzle/software/TinySlingshot", "", 5);
    (* a mass on a spring, a chain of springs, a rope of sticks *)
    ("examples/software/PhysicsElastic", "", 60);
    ("games/arcade/software/TinySoldat", "", 5);
    (* the planets on 2000-01-02, Jupiter and Saturn near their May 2000
     * conjunction *)
    ("examples/software/PhysicsSolarSystem", "", 2);
    (* the audio plan's first examples (their sounds: -dump-audio) *)
    ("examples/software/AudioTheremin", "", 2);
    ("examples/software/AudioPiano", "", 2);
    (* a naive 1250 Hz square: its aliases in red, all over the spectrum *)
    ("examples/software/AudioAliasing", "", 2);
    (* the explosion's numbers, and its shape: a burst, then a rumble *)
    ("examples/software/AudioSfx", "", 2);
    (* the car coming from the left: its pan, gain and Doppler shown *)
    ("examples/software/AudioSpace", "", 2);
    (* one recording, every note: each key's ratio and length *)
    ("examples/software/AudioSampler", "", 2);
    ("examples/software/AiTictactoe", "", 3);
    ("examples/software/AiPathfinding", "", 60);
    (* claude: seek, the mouse at the centre: a curve into it *)
    ("examples/software/AiSteering", "", 90);
    (* claude: the school forming, one fish's neighbours drawn *)
    ("examples/software/AiFlock", "", 90);
    (* claude: the first wave, scatter: each ghost's target its corner *)
    ("examples/software/AiGhosts", "", 60);
    (* the bot on its rounds, having seen nobody yet *)
    ("examples/software/AiBots", "", 2);
    ("examples/software/AiDebug", "", 3);
    ("examples/software/AiPerceptron", "", 3);
    ("examples/software/AiNeuralNet", "", 3);
    ("examples/software/AiDigits", "", 3);
    ("examples/software/AiQlearn", "", 3);
    (* claude: the image formats taken apart: JPEG from 6 coefficients
     * a block, PNG's filtered bytes, GIF's LZW 40 codes in *)
    ("examples/software/ImageJpeg", "", 3);
    ("examples/software/ImagePng", "", 3);
    ("examples/software/ImageLzw", "", 3);
    (* claude: every easing curve, the balls halfway through the time
     * (frame 60: 1 s on the effects' clock, into a 2 s tween) *)
    ("examples/software/JuiceCurves", "", 60);
    (* claude: the moment of landing (frame 75: 1.25 s on the effects'
     * clock, a landing): squashed flattest, the face white *)
    ("examples/software/JuiceSquash", "", 75);
    ("games/puzzle/software/AiConnect4", "", 3);
    ("games/puzzle/software/AiGo", "", 3);
    ("games/puzzle/software/AiOthello", "", 3);
    ("games/puzzle/software/AiChess", "", 3);
    ("games/strategy/software/TinyTowerDefense", "", 5);
    ("games/strategy/software/TinyDune2", "", 5);
    ("games/strategy/software/TinyWarcraft2", "", 5);
    ("games/platform/software/TinySonic", "", 5);
    ("games/shmup/software/TinyRobotron", "", 5);
    ("games/sports/software/TinyPinball", "", 5);
    ("games/puzzle/software/TinyPortal2D", "", 5);
    ("games/rpg/software/TinyGauntlet2", "", 5);
    ("games/sports/software/TinyKickOff2", "", 5);
    ("games/sports/software/TinySpeedball2", "", 5);
    ("games/sports/software/TinySensibleSoccer", "", 5);
    ("games/platform/software/TinyJoust", "", 5);
    ("games/shmup/software/TinyDefender", "", 5);
    ("games/platform/software/TinyCeleste", "", 5);
    ("games/platform/software/TinyBraid", "", 5);
    ("games/platform/software/TinyVVVVVV", "", 5);
    ("games/platform/software/TinySuperMeatBoy", "", 5);
    ("games/adventure/software/TinyMetalGearSolid", "", 5);
    ("games/adventure/software/TinyGTA", "", 5);
    ("games/adventure/software/TinyZork", "", 5);
    ("games/adventure/software/TinyManiacMansion", "", 5);
    ("games/strategy/software/TinyHamurabi", "", 5);
    ("games/sports/software/TinyTennisForTwo", "", 5);
    ("games/sports/software/TinyTonyHawk", "", 5);
    ("games/fps/software/TinyMazeWar", "", 5);
    ("games/puzzle/software/TinyCrush", "", 5);
    ("games/puzzle/software/TinyFez", "", 5);
    ("games/rhythm/software/TinyDDR", "", 5);
    ("games/strategy/software/TinySimCity", "", 5);
    ("games/strategy/software/TinyCivilization", "", 5);
    ("games/platform/software/TinyMarioWorld", "", 5);
    ("games/shmup/software/TinyRType", "", 5);
    ("games/puzzle/software/TinyIncredibleMachine", "", 5);
    ("games/strategy/software/TinyXCOM", "", 5);
    ("games/platform/software/TinyMetroid", "", 5);
    ("games/rhythm/software/TinyGuitarHero", "", 5);
    (* claude: Microsoft's deal 1, FreeCell's and Klondike's ways *)
    ("games/cards/software/TinyFreeCell", "", 3);
    ("games/cards/software/TinySolitaire", "", 3);
    (* claude: the Dwarf and the Imp loaded, the core empty between *)
    ("games/programming/software/TinyCoreWar", "", 3);
    (* the sound, seen (the "v" debug key, Audio_debug): TinyMario's
     * music at 1 s, as an oscilloscope, then a spectrum *)
    ("games/platform/software/TinyMario", "v", 60);
    ("games/platform/software/TinyMario", "vv", 60);
    ("games/arcade/software/TinyFlappyBird", "", 5);
    ("games/arcade/software/TinyFrogger", "", 5);
    ("games/arcade/software/TinyLunarLander", "", 5);
    ("games/arcade/software/TinyBreakout", "", 5);
    ("games/flight/software/TinyXpilot", "", 5);
    ("games/shmup/software/TinyGalaga", "", 5);
    ("games/platform/software/TinyDonkeyKong", "", 5);
    ("games/platform/software/TinyLodeRunner", "", 5);
    ("games/platform/software/TinyRick", "", 5);
    ("games/platform/software/TinyPrinceOfPersia", "", 5);
    ("games/shmup/software/TinyGradius", "", 5);
    ("games/adventure/software/TinyZelda", "", 5);
    ("games/adventure/software/TinyZeldaLinkPast", "", 5);
    ("games/rpg/software/TinyRogue", "", 5);
    ("games/fighting/software/TinyStreetFighter", "", 5);
    ("games/fighting/software/TinyFinalFight", "", 5);
    (* the world programs of How to Design Programs (Bigbang.mli): the
     * rocket coming down; the worm dead against the border, its
     * epitaph (last_picture) *)
    ("examples/software/BigBangRocket", "", 60);
    ("games/puzzle/software/TinyBabaIsYou", "", 5);
    ("examples/software/BigBangWorm", "", 400);
    ("games/racing/software/TinyMarioKart", "", 5);
    ("games/fps/software/TinyDoom", "", 5);
    (* "r" twice: a third of the resolution, 3x3 pixels (Pixelate) *)
    ("games/fps/software/TinyDoom", "rr", 5);
    ("games/flight/software/TinyComanche", "", 5);
    ("games/flight/software/TinyDescent", "", 5);
    ("games/flight/software/TinyElite", "", 5);
    (* the title's tank, turned by 40 degrees *)
    ("games/fps/software/TinyBattlezone", "", 40);
    ("games/sports/software/TinyShufflePuck", "", 5);
    ("games/shmup/software/TinyMissileCommand", "", 5);
    ("games/puzzle/software/TinyLemmings", "", 5);
    ("games/puzzle/software/TinyPuzzleBobble", "", 5);
    ("games/rpg/software/TinyDungeonMaster", "", 5);
    ("games/shmup/software/TinyZaxxon", "", 5);
    ("games/rpg/software/TinyDiablo", "", 5);
    (* a whole game as a map and one rule (playground/Puzzlescript) *)
    ("examples/software/PuzzleScriptSokoban", "", 5);
    ("examples/software/PuzzleScriptBoulders", "", 5);
  ]

(* claude: TinySimCity's starter town, built with the keys: a road, a
 * row of homes above it, factories and shops below, a plant at the end
 * and a power line around the road to the homes; then fast. *)
let simcity_town =
  "space:1,right:5,right:7,right:9,right:11,right:13,right:15,right:17,"
  ^ "right:19,right:21,right:23,right:25,space:3-27,3:29,up:31,left:35,"
  ^ "left:37,left:39,left:41,left:43,left:45,left:47,left:49,left:51,"
  ^ "left:53,left:55,space:33-57,down:59,down:61,5:63,right:67,right:69,"
  ^ "right:71,right:73,space:65-75,4:77,right:79,right:83,right:85,"
  ^ "right:87,right:89,right:91,right:93,space:81-95,6:97,down:99,"
  ^ "space:101,2:103,up:105,left:107,left:109,left:111,left:113,left:115,"
  ^ "left:117,left:119,left:121,left:123,left:125,left:127,left:129,up:133,"
  ^ "up:135,space:131-137,f:139"

(* claude: TinyCivilization's first thirty turns: Rome founded where the
 * settlers start, its warriors walking east into the fog one tile a
 * turn, then Enter, turn after turn *)
let civ_thirty_turns =
  "space:1,b:4,right:6,return:8,right:10,return:12,right:14,return:16,"
  ^ "right:18,return:20,right:22,return:24,right:26,return:28,right:30,"
  ^ "return:32,right:34,return:36,return:38,return:40,return:42,return:44,"
  ^ "return:46,return:48,return:50,return:52,return:54,return:56,return:58,"
  ^ "return:60,return:62,return:64,return:66,return:68,return:70,return:72,"
  ^ "return:74,return:76,return:78,return:80"

(* claude: TinyLunarLander's first moon (seed=1), from the start down to
 * the x2 pad: turned left to kill the drift, a long burn to brake, then
 * short ones holding the fall at 10 units a second; touchdown at frame
 * 1170 *)
let lunar_lander_x2 =
  "space:1,left:2-21,left:89-98,left:117-126,right:533-543,right:560-570,"
  ^ "up:365-590,right:595-604,up:605-619,right:645-653,left:682-690,"
  ^ "up:658-691,up:710-734,up:754-777,left:778-783,up:799-819,up:841-860,"
  ^ "up:882-899,left:893-899,up:922-940,up:964-980,up:1006-1022,"
  ^ "up:1048-1064,up:1091-1107,up:1133-1149"

(* claude: TinyMarioWorld's Donut Hills played to its secret exit: the
 * keys tests/games' pilot pressed (mw_keyhole prints them), from the
 * map's first course, entered at frame 5 *)
let mario_world_keyhole =
  "space:1,right:3,space:5,right:6-393,right:484-696,left:697-789,"
  ^ "down:394-483,x:484-696,space:90-103,space:328-341,space:531-544,"
  ^ "space:635-696"

(* claude: games played with keys (-script, see Input_script): what the
 * start of a game can't show -- the camera scrolled, a coin taken; the
 * formation shot at, stepped down, a bunker bitten *)
let scripted : Testutil_golden.scripted list =
  [
    ("games/platform/software/TinyMario", "run", 150, "right:1-150,up:30-34,up:95-99");
    ( "games/shmup/software/TinyInvaders",
      "play",
      300,
      "space:1,space:10,space:50,space:90,right:100-116,space:130,space:170,left:180-212,space:220,space:260,space:280" );
    (* the first level's shortest solution, uldurrd, a key every 5 frames *)
    ( "games/puzzle/software/TinySokoban",
      "solve1",
      40,
      "space:1,up:5,left:10,down:15,up:20,right:25,right:30,down:35" );
    (* its editor: a wall and a box typed at the cursor (3 boxes for 2
     * goals, which the status line says); the second level solved by
     * the kit's solver, 37 moves; and the first one tested, one box
     * pushed onto its goal *)
    ( "games/puzzle/software/TinySokobanEd",
      "typed",
      25,
      "right:3,right:6,type($):9,down:15,type(#):18" );
    ("games/puzzle/software/TinySokobanEd", "solve2", 10, "Tab:3,s:6");
    ("games/puzzle/software/TinySokobanEd", "test", 15, "Enter:3,up:6,left:9,down:12");
    (* after READY!, left, up, right along the top: dots eaten, the
     * ghosts out of the house, scattering *)
    ("games/arcade/software/TinyPacman", "play", 300, "space:1,left:120-170,up:160-230,right:220-300");
    (* turned towards a treasure, walking to it: the billboard in front
     * of the far wall, a near wall on the right *)
    ("games/fps/software/TinyWolfenstein", "treasure", 60, "right:1-16,up:20-60");
    (* flat out into the first curve, which bends right *)
    ("games/racing/software/TinyOutRun", "curve", 230, "space:1,up:2-230");
    (* a bomb dropped in the corner, the bomber walking away, the fire *)
    ("games/arcade/software/TinyBomberman", "bomb", 165, "space:1,space:5,right:8-30,down:31-45");
    (* two bombs, the first one's fire setting off the second: a chain *)
    ( "games/arcade/software/TinyBomberman",
      "chain",
      178,
      "space:1,right:3-22,space:25,left:27-46,space:49,down:51-90,right:91-110" );
    (* against the computer, blue turning around into its trail *)
    ("games/arcade/software/TinyTron", "computer", 200, "1:1,up:40,right:80,down:120,right:150");
    (* two players, both turning *)
    ("games/arcade/software/TinyTron", "duel", 150, "2:1,up:30,w:40,right:70,s:90,a:120");
    (* against the computer, north up; then with the camera turning *)
    ("games/racing/software/TinyMicroMachines", "race", 200, "1:1,up:62-200");
    ("games/racing/software/TinyMicroMachines", "turning", 200, "1:1,up:62-200,v:2");
    (* shifted up to third, the engine's pitch dropping at each shift *)
    ("games/racing/software/TinyGranTrak10", "gears", 260, "space:1,up:3-260,2:70,3:130,right:200-230");
    (* the drones round the figure eight, over and under the bridge *)
    ("games/racing/software/TinySuperSprint", "bridge", 330, "1:1,up:95-330");
    (* two missiles fired, the camera ahead of the car, the minimap *)
    ("games/racing/software/TinySupercars", "race", 300, "space:1,up:95-300,space:200,space:230");
    (* the computer's trucks through the mud hole, their shadows *)
    ("games/racing/software/TinySuperOffRoad", "race", 700, "1:1,up:95-700");
    (* semi-implicit Euler: the same orbit, closed *)
    ("examples/software/PhysicsOrbit", "semi", 480, "space:1-2");
    (* a shot, pushed back by the wind, digging its crater *)
    (* the bazooka: aimed up, charged, fired -- through a girder, the
       terrain's steel as destructible as its earth -- and the turn
       passed, with a new wind *)
    ("games/strategy/software/TinyWorms", "shot", 190, "space:1,up:6-25,space:30-75");
    (* the ninja rope hooked on a girder, the worm swinging from it *)
    ("games/strategy/software/TinyWorms", "rope", 150, "space:1,3:4,up:6-35,space:40,up:45-80,right:85-105,left:110-130,right:135-150");
    (* a grenade in flight, its fuse counting *)
    ("games/strategy/software/TinyWorms", "grenade", 140, "space:1,2:4,up:6-20,space:30-60");
    (* both ships thrusting, turning and firing around the star *)
    ("games/arcade/software/TinySpacewar", "duel", 120, "space:2-3,up:10-60,left:30-45,down:50,down:70,w:10-40,s:55,s:75");
    (* a serve, returned by the computer, missed by the player *)
    ("games/arcade/software/TinyPong", "rally", 150, "space:2-3,w:100-160");
    (* the same marbles, the same frame (the three methods find the same
     * pairs), with the grid and its count *)
    ("examples/software/PhysicsMarbles", "grid", 120, "space:60-61");
    (* rotation off: boxes balanced on their corners, stuck on the ramp *)
    ("examples/software/PhysicsBoxes", "upright", 300, "u:2-3");
    (* the maze turned right then left, the moon rolling, a target taken *)
    ("games/arcade/software/TinyCameltry", "turns", 200, "space:2-3,right:30-75,left:150-200");
    (* the maze turned 30 degrees: the moon rolls away; upright, it
     * slides, and friction holds it (up to 39 degrees): still there *)
    ("games/arcade/software/TinyCameltry", "tilt", 150, "space:2-3,right:60-74");
    ("games/arcade/software/TinyCameltry", "tilt_upright", 150, "space:2-3,u:5,right:60-74");
    (* the same pyramid without the solver (phase 7's engine): a heap;
     * and hit by the ball, its top knocked off *)
    ("examples/software/PhysicsPyramid", "no_solver", 300, "s:2");
    ("examples/software/PhysicsPyramid", "ball", 200, "space:120");
    (* a shot along the dotted arc, the tower tumbling; and without the
     * solver, the tower slumping by itself, no shot *)
    ("games/puzzle/software/TinySlingshot", "shot", 130, "space:2,space:60");
    ("games/puzzle/software/TinySlingshot", "no_solver", 120, "space:2,s:5");
    (* all three kicked; and the chain too stiff for the time step,
     * exploding in 8 steps *)
    ("examples/software/PhysicsElastic", "kick", 60, "space:30");
    ("examples/software/PhysicsElastic", "stiff", 8, "x:2");
    (* the bots' fight, GREEN's ragdoll tumbling; the player running,
     * jumping, flying on the jets, shooting, a grenade, the blasts *)
    ("games/arcade/software/TinySoldat", "bots", 260, "space:2");
    ("games/arcade/software/TinySoldat", "jets", 130, "space:2,d:10-70,w:30,w:40-90,space:100-160,q:120");
    (* the true distances, the inner planets crowded; and 80 days a
     * second for 2 seconds: mid-2000 *)
    ("examples/software/PhysicsSolarSystem", "true_distances", 3, "d:2");
    ("examples/software/PhysicsSolarSystem", "later", 120, "up:2,up:4");
    (* keys held, lit; the square wave *)
    ("examples/software/AudioPiano", "keys", 30, "space:2,a:10-30,g:10-30,u:10-30");
    (* band-limited, then an octave up (12 semitones): 2500 Hz, the low
     * aliases gone, those near Nyquist left *)
    ("examples/software/AudioAliasing", "band_limited", 20, "space:2,right:5-16");
    (* the laser, echoed: its copies, each 0.4 of the last *)
    ("examples/software/AudioSfx", "laser_echo", 10, "4:2,e:5");
    (* panning and Doppler off, the car nearly in front of you *)
    ("examples/software/AudioSpace", "effects_off", 100, "1:2,3:4");
    (* cubic, and C5 played: half as long *)
    ("examples/software/AudioSampler", "cubic", 10, "space:2,k:6");
    ("examples/software/AiTictactoe", "played", 40, "space:2");
    ("examples/software/AiPathfinding", "breadth_first", 120, "b:2");
    ("examples/software/AiPathfinding", "dijkstra", 120, "d:2");
    (* claude: arrive: slowed within its circle round the mouse *)
    ("examples/software/AiSteering", "arrive", 90, "3:2,at(-80;-120):1-90");
    (* claude: pursue: the prey, and where it will be *)
    ("examples/software/AiSteering", "pursue", 90, "4:2");
    (* claude: avoid: round the rock in its corridor *)
    ("examples/software/AiSteering", "avoid", 90, "6:2");
    (* claude: follow: along the road *)
    ("examples/software/AiSteering", "follow", 90, "7:2");
    (* claude: cohesion off: the school spreading out *)
    ("examples/software/AiFlock", "no_cohesion", 90, "c:2");
    (* claude: chase, Pac-Man walked up and right: Pinky ahead of him,
     * Inky's doubled vector from Blinky, Clyde's circle *)
    ("examples/software/AiGhosts", "chase", 500, "left:1-60,up:61-140,right:141-500");
    (* its rounds bring it round the corner: it sees you, aims (a little
     * off: the error has not settled yet) and fires *)
    ("examples/software/AiBots", "seen", 330, "right:5-60");
    (* claude: the four pictures at once -- the walker half way to the
     * flag with its way and the field drawn, its mind resting, and the
     * opponent's opinion of a Nim position it has already won *)
    ("examples/software/AiDebug", "thinking", 95, "2:5");
    (* claude: the perceptron settles on AND -- nought of four wrong,
     * the line drawn where its weighted sum is zero -- and never
     * settles on XOR, where it ends up worse than the best line *)
    ("examples/software/AiPerceptron", "and", 95, "a:3");
    ("examples/software/AiPerceptron", "xor", 95, "x:3");
    (* claude: the network half way through the two spirals, and the
     * same run with no hidden layer at all ("0"), where the boundary
     * is a straight line and stays one: capacity, seen *)
    ("examples/software/AiNeuralNet", "spirals", 95, "");
    ("examples/software/AiNeuralNet", "flat", 95, "0:2");
    (* claude: a training digit put in the square with "n", read by a
     * network that has seen six thousand of them: the bars say what
     * it was torn between *)
    ("examples/software/AiDigits", "digit", 95, "n:3");
    (* claude: two hundred episodes of falling off the cliff, the
     * values seeped back across the grid, and the greedy way it has
     * learned drawn over them: thirteen steps along the edge *)
    ("examples/software/AiQlearn", "learned", 95, "f:2,g:90");
    (* claude: JPEG from one coefficient a block, each its average: the
     * mosaic; PNG with no filter, its bytes as they are, bright; GIF's
     * LZW a second into playing, the codes 8 bits wide *)
    ("examples/software/ImageJpeg", "one", 12, "down:1,down:3,down:5,down:7,down:9");
    (* claude: the picture encoded again by our own writer at quality 20
     * (Jpeg_encode.mli), every coefficient kept: the quantization's
     * blocks and ringing *)
    ("examples/software/ImageJpeg", "quality20", 24, "2:1,right:3,right:5,right:7,right:9,right:11,right:13,right:15,right:17");
    ("examples/software/ImagePng", "none", 4, "0:2");
    ("examples/software/ImageLzw", "played", 60, "space:1");
    ("games/puzzle/software/AiOthello", "values", 3, "v:2");
    ("games/strategy/software/TinyDune2", "harvesting", 900, "space:1,b:30,f:100-900");
    ("games/platform/software/TinySonic", "loop", 330, "space:1,right:5-330");
    (* the two sticks: running right, shooting left, the grunts closing
       in and two of them shot down *)
    ("games/shmup/software/TinyRobotron", "twin_stick", 95, "space:1,a:20-95,right:20-60,up:62-95");
    (* the plunger pulled and let go: the ball up the lane, under the
       dome and into the table, past the bumpers *)
    ("games/sports/software/TinyPinball", "launch", 90, "space:1-40");
    ("games/rpg/software/TinyGauntlet2", "crowd", 400, "space:1,down:30-90,right:100-200,space:220-400");
    ("games/sports/software/TinyKickOff2", "shot", 260, "space:1,up:70-200,space:150-170,right:171-260");
    ("games/sports/software/TinySpeedball2", "match", 700, "space:1,up:60-200,space:120-140,left:210-400,space:260-280");
    ("games/sports/software/TinySensibleSoccer", "loft", 300, "space:1,up:40-150,space:160-200,right:201-300");
    (* the planet, the scanner reading it, and a lander on its way
       down to a human *)
    ("games/shmup/software/TinyDefender", "patrol", 95,
     "space:1,right:5-200,space:60,space:90,space:120,down:130-170,space:150,space:200,right:210-320,space:240,space:280,space:310");
    (* the dungeon, the dark around it, and a click being walked to:
       the first golden frames here that are played with a mouse *)
    ("games/rpg/software/TinyDiablo", "dungeon", 95,
     "click:2,at(150;20):6-60,click:8,at(-120;-40):70-140,click:72,at(60;120):150-260,click:152,rclick:200,click:230");
    (* a bolt cast (the mana orb half down), an imp at arm's length *)
    ("games/rpg/software/TinyDiablo", "fight", 260,
     "click:2,at(150;20):6-60,click:8,at(-120;-40):70-140,click:72,at(60;120):150-260,click:152,rclick:200,click:230");
    (* the fortress, a wall flown through and the next one coming, and
       the fighter over its own shadow: the gap between the two is the
       altitude, which is the whole game *)
    ("games/shmup/software/TinyZaxxon", "fortress", 95,
     "space:1,space:60,right:120-150,up:180-210,space:200,left:260-300,space:300,down:330-360,space:380,right:400-430,up:430-470,space:470");
    (* deeper in: three walls, a tower, and a shot on its way *)
    ("games/shmup/software/TinyZaxxon", "deep", 260,
     "space:1,space:60,right:120-150,up:180-210,space:200,left:260-300,space:300,down:330-360,space:380,right:400-430,up:430-470,space:470");
    (* the first four steps danced, judged by the music's clock: the
       card is fed 735 samples a frame here, so the song's time is
       exact, and a press on a frame lands about 35 ms early by it --
       GREAT rather than PERFECT, the offset the calibration is for *)
    ("games/rhythm/software/TinyDDR", "steps", 318, "space:1,left:228,up:256,up:284,down:312");
    (* the town five years on: grown, and the homes across the road
       from the factories empty again -- the smog *)
    ("games/strategy/software/TinySimCity", "town", 600, simcity_town);
    (* and why, in the pollution view: the air around the factories and
       the plant *)
    ("games/strategy/software/TinySimCity", "smog", 610, simcity_town ^ ",v:600,v:602");
    (* 3400 BC: Rome, the strip its warriors uncovered, the rival's
       first advance in the news *)
    ("games/strategy/software/TinyCivilization", "rome", 90, civ_thirty_turns);
    (* and the tree of advances at that point: Alphabet known, Bronze
       Working under way, what is open and what is not *)
    ("games/strategy/software/TinyCivilization", "tree", 96, civ_thirty_turns ^ ",t:94");
    (* the world map, one path open, Mario at home *)
    ("games/platform/software/TinyMarioWorld", "map", 10, "space:1");
    (* crouched on the long slope of Donut Hills: the slide *)
    ("games/platform/software/TinyMarioWorld", "slide", 435, mario_world_keyhole);
    (* the cape: taken off at the end of the runway, rising to the
       island in the sky and its keyhole *)
    ("games/platform/software/TinyMarioWorld", "flight", 690, mario_world_keyhole);
    (* back on the map: the secret exit found, the Star Road open *)
    ("games/platform/software/TinyMarioWorld", "secret", 800, mario_world_keyhole);
    (* the Force sent out ahead, hanging there at the ship's height, the
       first wave coming in *)
    ("games/shmup/software/TinyRType", "force", 150, "space:1,f:60,up:70-85");
    (* the beam: space held two seconds, let go *)
    ("games/shmup/software/TinyRType", "beam", 176, "space:1,space:100-170");
    (* the pulley puzzle, built with the mouse: the ramp picked from the
       bin, tilted, put under the bowling ball -- then run, the bowling
       ball in the bucket and the tray on its way up *)
    ("games/puzzle/software/TinyIncredibleMachine", "built", 14, "space:1,n:3,n:5,at(-330;-410):7-9,click:8,r:10,at(-380;240):11-14,click:13");
    ("games/puzzle/software/TinyIncredibleMachine", "pulley", 200, "space:1,n:3,n:5,at(-330;-410):7-9,click:8,r:10,at(-380;240):11-14,click:13,space:16");
    (* the fan puzzle: the ball on the switch, the fan on, the balloon
       blown out from under the ledge *)
    ("games/puzzle/software/TinyIncredibleMachine", "fan", 260, "space:1,n:3,n:5,n:7,at(-330;-410):9-11,click:10,r:12,r:14,r:16,r:18,at(400;50):20-23,click:22,space:25");
    (* two soldiers walked out, the second shot at by reaction fire,
       and the chances shown over the alien who fired; then the aliens'
       turn, closing in *)
    ("games/strategy/software/TinyXCOM", "aim", 130, "space:1,at(-119;136):3-40,click:5,at(-85;102):41-110,n:60,at(-119;68):62-80,click:65,at(85;-34):111-130");
    ("games/strategy/software/TinyXCOM", "aliens", 400, "space:1,at(-119;136):3-40,click:5,at(-85;102):41-200,n:60,at(-119;68):62-80,click:65,return:120");
    (* the morph ball taken; rolled up through the tunnel out of the
       start; the map, two areas been to *)
    ("games/platform/software/TinyMetroid", "morph", 85, "space:1,left:5-80");
    ("games/platform/software/TinyMetroid", "ball", 230, "space:1,left:5-80,right:90-235,down:200");
    ("games/platform/software/TinyMetroid", "map", 330, "space:1,left:5-80,right:90-235,down:200,right:237-300,up:302,return:320");
    (* claude: the 6S parked in a free cell, and the 9C picked up *)
    ("games/cards/software/TinyFreeCell", "cell", 28,
     "at(-420;0):1-10,click:3,at(-420;380):11-20,click:13,at(-300;0):21-30,click:23");
    (* claude: the ace of hearts home, the card under it turned over, and
     * two cards turned from the stock onto the waste *)
    ("games/cards/software/TinySolitaire", "home", 33,
     "at(120;150):1-10,click:3,at(240;380):11-20,click:13,at(-360;380):21-33,click:23,click:28");
    (* claude: cycle 360: the Imp has walked through the Dwarf and made it
     * an Imp, two walking the core, which neither can kill *)
    ("games/programming/software/TinyCoreWar", "imp", 90, "");
    (* claude: the Mice chosen in warrior 2's menu, and Fight: they have
     * multiplied, and the Dwarf is dead *)
    ("games/programming/software/TinyCoreWar", "mice", 90,
     "at(360;100):1-6,click:2,at(360;10):7-20,click:8,at(-420;-385):21-90,click:22");
    (* claude: a mistake typed into the Imp -- a word that is no opcode,
     * so a label with no instruction -- shown with its line, and Fight
     * greyed out *)
    ("games/programming/software/TinyCoreWar", "mistake", 12, "at(250;-250):1-12,click:3,type(HALT):6");
    (* the riff's first four notes on Medium, every fret held and each
       strummed on its beat (132 a minute, an eighth 0.227 s) *)
    ("games/rhythm/software/TinyGuitarHero", "riff", 305,
     "space:1,a:200-305,s:200-305,d:200-305,f:200-305,g:200-305,space:217,space:245,space:272,space:299");
    (* the first ledge reached with a held jump, the four lies all on *)
    ("games/platform/software/TinyCeleste", "climb", 60, "space:1,right:10-40,space:22-34");
    (* a jump, then a dash spent straight up: the hair gone blue, which
       is the only interface Celeste needs *)
    ("games/platform/software/TinyCeleste", "dash", 48, "space:1,right:10-60,space:22-40,up:36-50,x:38");
    (* run into the first pit, die on its spikes, and hold shift: the
       picture gone sepia, Tim going back up out of the pit *)
    ("games/platform/software/TinyBraid", "rewind", 90, "space:1,right:5-70,Shift:75-90");
    (* flipped up to the ceiling, walking along it over the wall, the
       spikes below: upside down, and still smiling *)
    ("games/platform/software/TinyVVVVVV", "ceiling", 90, "space:1,space:12,right:30-100");
    (* straight into the pit's saw: the splat, and the smear along the
       floor the run left *)
    ("games/platform/software/TinySuperMeatBoy", "splat", 45, "space:1,right:2-37");
    (* that death, then a try that makes it, and every try at once: the
       first dead in the pit again, the second in the air over it *)
    ("games/platform/software/TinySuperMeatBoy", "replay", 191,
     "space:1,right:2-37,right:53-140,space:76-88,space:112-124");
    (* a knock on his room's wall: the guard next door has heard it
       ('?'), and comes round through the door by A*; the radar in the
       corner is the only place the cones are drawn *)
    ("games/adventure/software/TinyMetalGearSolid", "knock", 200, "space:1,right:5-115,x:118");
    (* he came in, saw Snake ('!'), the radar jammed, and caught him *)
    ("games/adventure/software/TinyMetalGearSolid", "caught", 380, "space:1,right:5-115,x:118");
    (* into the parked car and full gas: the camera higher the faster,
       the buildings leaning out from the middle of the screen *)
    ("games/adventure/software/TinyGTA", "drive", 160, "space:1,right:5-50,up:5-45,space:55,up:60-160");
    (* three commands typed, and a fourth being typed: the terminal, a
       character per column *)
    ("games/adventure/software/TinyZork", "typed", 80,
     "type(open mailbox):5,Enter:20,type(take leaflet):30,Enter:45,type(read leaflet):55,Enter:70,type(nor):75");
    (* Push, the doormat: Dave walks to it, and finds the key; the mouse
       over the key, the sentence line says what a click would do *)
    ("games/adventure/software/TinyManiacMansion", "mat", 100,
     "at(0;0):1-3,click:2,at(-330;-140):5-10,click:8,at(0;135):12-20,click:15,at(30;135):21-100");
    (* the first year's three answers typed, the second year's report,
       and the next answer being typed, on the teletype's paper *)
    ("games/strategy/software/TinyHamurabi", "year", 60,
     "type(0):5,Enter:10,type(2000):15,Enter:25,type(1000):30,Enter:40,type(5):50");
    (* the serve, over the net, its trail on the phosphor *)
    ("games/sports/software/TinyTennisForTwo", "rally", 70, "d:3");
    (* pumped five times across, the fifth air: a kickflip, in the
       chain, and an indy grab held *)
    ("games/sports/software/TinyTonyHawk", "air", 615, "down:1-571,x:575,z:596-640");
    (* five steps down the first corridor, to its side opening, and a
       quarter turn into it: the frames of a new corridor *)
    ("games/fps/software/TinyMazeWar", "walk", 50, "up:3,up:10,up:17,up:24,up:31,right:38");
    (* the first level, uncrushed: Danny's slice in colour, the bridge
       and the wall behind in grey, each depth leaning up and right *)
    ("games/puzzle/software/TinyCrush", "uncrushed", 20, "space:1");
    (* crushed, and a few steps: the bridge fills the gap, the wall
       from the back slice stands in the way *)
    ("games/puzzle/software/TinyCrush", "crushed", 60, "space:1,c:22,right:45-60");
    (* the front: the tower, the ledges of its east and west sides, the
       ones in front and behind hidden in its column *)
    ("games/puzzle/software/TinyFez", "front", 20, "space:1");
    (* half a quarter turn: each cube two faces, the pixel art squashed
       to the cosine and the sine *)
    ("games/puzzle/software/TinyFez", "turning", 32, "space:1,e:24");
    (* turned: the south and north ledges, and their bits, where the
       east and west ones were *)
    ("games/puzzle/software/TinyFez", "turned", 60, "space:1,e:24");
    (* off the bank on six flaps, the buzzards already coming *)
    ("games/platform/software/TinyJoust", "flaps", 95, "space:1,space:20,space:35,space:50,space:65,space:80,right:10-95");
    (* the scanner is the game: three abductions are under way in the
       strip while the screen shows one lander and a laser *)
    ("games/shmup/software/TinyDefender", "hunt", 320,
     "space:1,right:5-200,space:60,space:90,space:120,down:130-170,space:150,space:200,right:210-320,space:240,space:280,space:310");
    (* a flap every quarter of a second is a climb: he ends up under
       the eyries with a buzzard coming up at him, which is the whole
       game -- be the higher one when you meet *)
    ("games/platform/software/TinyJoust", "flight", 200,
     "space:1,space:20,space:35,space:50,space:65,space:80,space:95,space:110,space:125,space:140,space:155,space:170,space:185,right:10-105,left:125-200");
    (* the first chamber: the goo between him and the way out (the
       portals themselves want a mouse, which a script has none of, so
       they are in tests/games/ instead) *)
    ("games/puzzle/software/TinyPortal2D", "chamber1", 60, "space:1,right:12-55");
    (* the dungeon, and later the crowd the generators have poured
       into it (chase=field is a flag, which a golden cannot pass: the
       two chases are compared in tests/games/ instead) *)
    ("games/rpg/software/TinyGauntlet2", "dungeon", 95, "space:1,down:30-95");
    (* the kick off, and (heavy) a shot bent in the air by the
       aftertouch, which is the game's other idea *)
    ("games/sports/software/TinyKickOff2", "kickoff", 95, "space:1,up:20-95");
    (* the metal, its furniture, and (heavy) a match well under way:
       the score is mostly what the arena paid *)
    ("games/sports/software/TinySpeedball2", "arena", 95, "space:1,up:20-95");
    (* the pitch pulled back, and (heavy) a lofted ball bent in the
       air, which is the game everybody remembers *)
    ("games/sports/software/TinySensibleSoccer", "pitch", 95, "space:1,up:20-95");
    ("games/strategy/software/TinyWarcraft2", "crowd", 300, "space:1,a:10,p:14,right:20-44,space:50");
    ("games/strategy/software/TinyTowerDefense", "maze", 400, "space:1,right:20-40,space:45,up:50-56,space:60,up:64-70,space:74,left:80-84,space:90");
    ("games/puzzle/software/AiOthello", "reply", 60, "space:2");
    (* claude: you drop in the middle, it answers in the middle too, and
     * says what the tricks saved it *)
    ("games/puzzle/software/AiConnect4", "reply", 90, "space:2");
    (* claude: you put a stone on the middle point, and it answers after
     * a thousand random games -- played out over the frames, not in one
     * of them (Mcts.mli: anytime) *)
    ("games/puzzle/software/AiGo", "reply", 92, "space:2");
    (* claude: the knight on g1 clicked, its two squares shown *)
    ("games/puzzle/software/AiChess", "selected", 10, "at(250;-350):1-10,click:3");
    (* claude: e2-e4 in two clicks, and the computer's answer *)
    ("games/puzzle/software/AiChess", "reply", 60, "at(50;-250):1-6,click:3,at(50;-50):7-60,click:9");
    (* flaps timed to thread 5 pipes (the pipes from the LFSR's seed=1);
     * and no flap after the first: the bird on the ground, game over *)
    ( "games/arcade/software/TinyFlappyBird",
      "fly",
      600,
      "space:1,space:5,space:51,space:91,space:131,space:172,space:212,space:258,space:299,space:346,space:386,space:426,space:447,space:472,space:512,space:555,space:596" );
    ("games/arcade/software/TinyFlappyBird", "crash", 200, "space:1,space:5");
    (* hops timed between the cars (the traffic is the same every game,
     * a formula of time), then onto a log, then the long log, which
     * carries the frog right *)
    ( "games/arcade/software/TinyFrogger",
      "log",
      280,
      "space:1,up:10,up:20,up:30,up:40,up:109,up:119,up:135,up:145,up:250" );
    (* the first moon of seed=1, flown by keys an autopilot pressed (a
     * throwaway one, in the style of the test pilots of tests/games):
     * turned and braking high up, the whole moon on the screen; then
     * down on the x2 pad, close up, a good landing; and no key at all,
     * a crash on a slope, the module in pieces *)
    ("games/arcade/software/TinyLunarLander", "descent", 450, lunar_lander_x2);
    ("games/arcade/software/TinyLunarLander", "landed", 1200, lunar_lander_x2);
    ("games/arcade/software/TinyLunarLander", "crash", 520, "space:1");
    (* a take-off from the base, a turn, two shots; by the fuel station,
     * its beam refueling the ship, the shield up *)
    ("games/flight/software/TinyXpilot", "refuel", 150, "space:1,up:5-40,right:22-28,up:60-80,space:100,space:115,down:140-150");
    (* the first waves flying in along their curves; the formation, all
     * in, a boss diving, the fighter having fired *)
    ("games/shmup/software/TinyGalaga", "waves", 200, "space:1");
    ("games/shmup/software/TinyGalaga", "formation", 1100, "space:1,space:900,space:930,left:950-980,space:990");
    (* the first waves, a turret firing; later, a red one shot down *)
    ("games/shmup/software/TinyGradius", "waves", 200, "space:1,right:5-30,space:40,space:60,space:80,space:100,space:130,space:150,space:170,space:190,up:120-150");
    ("games/shmup/software/TinyGradius", "later", 600, "space:1,right:5-30,space:40,space:60,space:80,space:100,space:130,space:150,space:170,space:190,up:120-150,space:220,space:240,space:260,space:300,space:330");
    (* the sword taken, and swung; walking into the next room, the
     * screen sliding to it *)
    ("games/adventure/software/TinyZelda", "sword", 130, "space:1,right:3-42,up:43-110,space:120");
    ("games/adventure/software/TinyZelda", "slide", 370, "space:1,right:3-42,up:43-110,down:130-200,right:210-400");
    (* the three-quarter view: Link walked into the woods, right above a
     * trunk, hidden by its canopy but for his cap; and a swing at the
     * soldier come down to the path *)
    ("games/adventure/software/TinyZeldaLinkPast", "woods", 310, "space:1,up:2-250,left:250-305");
    ("games/adventure/software/TinyZeldaLinkPast", "sword", 95, "space:1,up:2-80,left:84,space:90");
    (* the first room, walked around, a bat fought *)
    ("games/rpg/software/TinyRogue", "level", 120, "space:2,right:10,right:14,right:18,right:22,right:26,right:30,up:40,up:44,up:48,left:60,left:64,down:80,down:84,down:88,down:92");
    (* a fireball thrown (down, down-forward, forward, punch), the
     * computer jumping it; later, trading blows *)
    ("games/fighting/software/TinyStreetFighter", "fireball", 40, "space:1,s:3-8,d:6-12,f:11");
    ("games/fighting/software/TinyStreetFighter", "fight", 150, "space:1,d:95-120,f:122,g:135");
    (* the first wave on the street; the spin *)
    ("games/fighting/software/TinyFinalFight", "wave", 160, "space:1,right:3-60,space:100,space:108,space:116,space:124,space:132");
    ("games/fighting/software/TinyFinalFight", "spin", 206, "space:1,right:3-60,space:100,space:108,space:116,space:124,space:132,z:200");
    (* level 1, Baba pushing the rocks; ten moves right: won *)
    ("games/puzzle/software/TinyBabaIsYou", "pushing", 30, "space:1,right:5,right:10,right:15,right:20,right:25,right:30,right:35,right:40,right:45,right:50");
    ("games/puzzle/software/TinyBabaIsYou", "won", 60, "space:1,right:5,right:10,right:15,right:20,right:25,right:30,right:35,right:40,right:45,right:50");
    (* running from the boulder; through the hole, the boulder stopped
     * over it (too big to fall in) *)
    (* the grid, GO!; the first corner, the karts passed on the way
     * coming up behind *)
    ("games/racing/software/TinyMarioKart", "grid", 200, "space:1");
    ("games/racing/software/TinyMarioKart", "corner", 430, "space:1,up:2-430,right:370-405");
    (* two players, the screen split as on the SNES: the first on top
       (the arrows), the second below (w a s d), both on the gas *)
    ("games/racing/software/TinyMarioKart", "split", 260, "2:1,up:2-260,w:2-260,d:200-215");
    (* on the stairs, upstairs ahead; at the window onto the dark room
     * (TinyDoom3d's golden frames are the same walks) *)
    ("games/fps/software/TinyDoom", "stairs", 80, "left:1-10,up:11-80");
    ("games/fps/software/TinyDoom", "window", 60, "right:1-5,up:6-55");
    (* over the island, climbing a little (TinyComanche3d's
     * golden frame is the same flight) *)
    ("games/flight/software/TinyComanche", "island", 70, "up:1-70,w:1-20");
    (* down the corridor, the robot of the next cell ahead
     * (TinyDescent3d's golden frames are the same flight) *)
    ("games/flight/software/TinyDescent", "corridor", 30, "w:1-30");
    (* launched from the station: Coriolis ahead, its slot turning, in
     * front of Lave; the three Sidewinders coming *)
    ("games/flight/software/TinyElite", "flight", 200, "space:1");
    (* turned, driving towards a pyramid (cut by the near plane), a shell
     * flying at the enemy tank, at the height of its hull *)
    ("games/fps/software/TinyBattlezone", "play", 150, "space:1,right:5-20,up:30-140,space:100");
    (* the served puck struck up the table, Ned's paddle coming to meet it *)
    ("games/sports/software/TinyShufflePuck", "rally", 45, "space:1,up:3-14");
    ("games/platform/software/TinyRick", "boulder", 70, "space:1,right:2-160");
    ("games/platform/software/TinyRick", "hole", 160, "space:1,right:2-160");
    (* claude: a standing jump over the first gap, in the air; then,
     * the plate stepped on and the gate run through, hanging from the
     * shaft's edge (the loose floor fallen, the gate closing); and
     * climbing onto the ledge below the door (tests/games' robot plays
     * the whole way) *)
    ("games/platform/software/TinyPrinceOfPersia", "jump", 72, "space:1,right:10-40,up:54-58,right:54-58");
    ("games/platform/software/TinyPrinceOfPersia", "hang", 670,
     "space:1,right:10-40,up:54-58,right:54-58,right:100-290,up:193-197,left:330-550,left:600-601,Shift:600-601,down:630-634");
    ("games/platform/software/TinyPrinceOfPersia", "climb", 1122,
     "space:1,right:10-40,up:54-58,right:54-58,right:100-290,up:193-197,left:330-550,left:600-601,Shift:600-601,down:630-634,down:690-694,right:745-768,up:790-794,right:790-794,right:840-1060,up:1070-1074,up:1110-1114");
    (* right to the ladder, up it, a hole dug on the right; the guards
     * coming *)
    ("games/platform/software/TinyLodeRunner", "dig", 110, "space:1,right:2-41,up:42-81,x:86");
    (* Jumpman walks to the first ladder and climbs it, the barrels
     * rolling down *)
    ("games/platform/software/TinyDonkeyKong", "climb", 400, "space:1,right:5-230,up:231-300");
    (* two players: both take off, turn, fire; the camera zoomed out to
     * frame them both, blue's shield up *)
    ("games/flight/software/TinyXpilot", "duel", 200, "2:1,w:5-60,d:20-26,up:5-60,left:20-26,w:100-110,up:100-110,space:120,space:140,return:130,s:190-200");
    (* the same, the screen split: a camera for each, the walls cut at
     * each view's edge *)
    ("games/flight/software/TinyXpilot", "split", 200, "3:1,w:5-60,d:20-26,up:5-60,left:20-26,w:100-110,up:100-110,space:120,space:140,return:130,s:190-200");
    (* a serve, the paddle moved to where the ball comes down each time
     * (aiming off-center, so the ball goes to the side): 15 seconds,
     * 11 points, the ball sped up *)
    ( "games/arcade/software/TinyBreakout",
      "play",
      900,
      "space:1,space:5,right:6-11,right:210-229,left:396-403,right:570-570,right:740-740" );
    (* claude: the juice (Juice.mli): the wall popping in, 11 frames
     * after the game started, the yellow rows overshooting, the green
     * ones growing, the orange and red ones not there yet *)
    ("games/arcade/software/TinyBreakout", "pop", 12, "space:1");
    (* claude: and the first brick broken (at frame 108), its pieces
     * thrown up and falling, 8 frames later *)
    ("games/arcade/software/TinyBreakout", "debris", 116, "space:1,space:5,right:6-11");
    (* the turtle's drawings, all at once (the clock frozen, it wouldn't
     * move): Koch's filled snowflake; the dragon, 4096 lines *)
    ("examples/software/LogoFractals", "snowflake", 5, "a:2");
    ("examples/software/LogoFractals", "dragon", 10, "right:2,right:4,right:6,a:8");
    (* the crosshair raised, a counter-missile from each base: flying,
     * then exploding there, the three explosions one *)
    ("games/shmup/software/TinyMissileCommand", "fire", 45, "space:1,up:2-30,a:32,s:33,d:34");
    ("games/shmup/software/TinyMissileCommand", "explosions", 95, "space:1,up:2-30,a:32,s:33,d:34");
    (* the lemmings out of the hatch, walking, "4" picking the diggers *)
    ("games/puzzle/software/TinyLemmings", "walking", 500, "space:1,4:10");
    ("games/puzzle/software/TinyPuzzleBobble", "pop", 52, "space:1,left:3-12,space:20");
    (* turned east and walked down the corridor: the torch ahead in
     * its slot, a wall on the right, the dark past the light's reach *)
    ("games/rpg/software/TinyDungeonMaster", "corridor", 80, "space:1,right:5-6,up:15-60");
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
    (* Mustermann selected, his name changed, Update *)
    ( "examples/software/Gui7Crud",
      "update",
      24,
      "at(-163;36):1-4,click:2,at(163;23):5-8,click:6,end:9,backspace:11,backspace:13,backspace:15,type(Erika):17,at(0;-121):19-24,click:20"
    );
    (* Emil selected, then filtered out by "T": Update and Delete off *)
    ("examples/software/Gui7Crud", "filter", 14, "at(-163;72):1-4,click:2,at(71;122):5-8,click:6,type(T):9,at(300;-300):11-14");
    (* the four ways side by side, Flight Booker chosen from the menu *)
    ("examples/software/GuiFourWays", "flight", 10, "at(0;300):1-3,click:2,at(0;210):4-6,click:5,at(0;600):7-10");
    (* and the Timer, a second and a half in: four clocks, one reading *)
    ("examples/software/GuiFourWays", "timer", 96, "at(0;300):1-3,click:2,at(0;158):4-6,click:5,at(0;600):7-96");
    (* Circle Drawer, in the immediate column: two circles, the first
     * right-clicked, "Adjust diameter...", the slider dragged and the
     * dialog closed -- Undo on, for the whole drag as one step *)
    ( "examples/software/GuiFourWays",
      "circles",
      38,
      "at(0;300):1-3,click:2,at(0;122):4-6,click:5,at(-420;50):7-9,click:8,at(-330;-20):10-12,click:11,at(-420;50):13-16,rclick:14,at(-320;30):17-20,click:19,at(-432;-142):21-23,click:22-30,at(-400;-142):24,at(-360;-142):25,at(-330;-142):26-31,at(-375;-188):32-34,click:33,at(-375;-300):35-38"
    );
    (* the arrows are the whole interface: the cursor walked to B3,
     * and the line at the top showing what is in it -- @SUM(B4...B6),
     * as 1979 spelled it. (The slash commands take characters, which
     * a script cannot send: a key is not a character.) *)
    ("apps/office/software/TinyVisiCalc", "cursor", 16, "right:3,down:6,down:10");
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
    ("apps/office/software/TinyBravo", "edit", 12, "type(edit):10");
    ("apps/office/software/TinyBravo", "undone", 22, "type(edit):10,escape:14,type(uu):18");
    (* a selection dragged with the mouse, then l u and l s from the
     * keyboard: underlined and struck, the pen's two rules *)
    ( "apps/office/software/TinyBravo",
      "looks",
      20,
      "at(-222;285):1-6,click:5-10,at(-118;285):8-10,type(lu):14,type(ls):16,at(300;-400):17-20" );
    (* Tesler's answer: no modes, so "edit" typed anywhere is the word
     * edit, and the four letters are one "Undo Typing" *)
    ("apps/office/software/TinyWord", "typed", 12, "type( edit):10");
    (* a selection dragged with the mouse, the B icon, then the centring
     * one: the looks are the selection's, the alignment the page's *)
    ( "apps/office/software/TinyWord",
      "looks",
      24,
      "at(-222;285):1-6,click:5-10,at(-118;285):8-12,at(-300;428):14-16,click:15,at(-82;428):18-20,click:19,at(300;-400):21-24"
    );
    (* the filled oval and the diagonal pattern, rubber-banded from one
     * corner to the other; then the bucket and grey, poured into the
     * house's window *)
    ( "apps/office/software/TinyMacPaint",
      "shapes",
      30,
      "at(-398;202):1-3,click:2,at(88;-160):4-6,click:5,at(-260;350):7-10,click:9-16,at(-200;300):12,at(-120;250):13-17,at(-398;370):18-20,click:19,at(-104;-160):21-23,click:22,at(-4;112):24-27,click:25,at(400;-400):28-30"
    );
    (* the sun selected, then dragged left: lifted, white left behind,
     * put down opaque over the roof, the ants round it *)
    ( "apps/office/software/TinyMacPaint",
      "move",
      26,
      "at(-440;370):1-3,click:2,at(190;350):4-7,click:6-12,at(310;230):9-14,at(250;290):15-17,click:17-24,at(100;290):20,at(0;290):21-26"
    );
    (* the sprite editor: on the first walking frame, three red pixels
     * typed at the cursor, the frame copied (n) and flipped (f), then a
     * red pixel painted with the mouse in its top-left corner, over the
     * onion skin *)
    ( "apps/gamedev/software/TinyAseprite",
      "edited",
      22,
      "Tab:3,right:5,down:7,type(RRR):9,type(n):12,type(f):15,at(-310;240):17-20,click:18-19" );
    (* a color clicked with the transparent brush, which adds a
     * character for it ('G'), a second one, which recolors it, and the
     * canvas grown by a column and a row *)
    ( "apps/gamedev/software/TinyAseprite",
      "palette",
      16,
      "at(359;-120):3-5,click:4,at(129;-172):8-10,click:9,type(+):13" );
    (* the map editor: the cursor run to the right, the camera following
     * it along the level (the minimap's window moves with it), three
     * bricks typed and a column added *)
    ( "apps/gamedev/software/TinyTiled",
      "edited",
      55,
      "right:3-40,up:6,type(BBB):45,type(+):50" );
    (* the synthesizer played: the cutoff knob pressed and dragged up 40
     * pixels (from 0.32 to 0.52: the Gui's knob turns by the drag), C3
     * held and E3 over it -- the keys lit, the scope ringing at the
     * resonance, the spectrum brighter *)
    ( "apps/music/software/TinyMinimoog",
      "playing",
      40,
      "at(115;360):1-10,at(115;380):11-14,at(115;400):15-40,click:5-30,a:5-40,d:20-40" );
    (* the tracker edited: C-2 and D-2 typed on the piano's lower row
     * (the cursor going down after each), then C20 typed in hex on the
     * next row's effect *)
    ("apps/music/software/TinySoundtracker", "editing", 26, "down:3,z:6,x:9,right:12,c:15,2:18,0:21");
    (* and played: 57 frames in, row 8 (120 ms a row), the grid following
     * the playhead, the channels' volumes lit *)
    ("apps/music/software/TinySoundtracker", "playing", 60, "space:3");
    (* the media player, n pressed to the next items: the module as a
     * tracker shows it, the recording's whole wave, our animated GIF *)
    ("apps/media/software/TinyMediaPlayer", "module", 60, "n:3,n:6,n:9");
    ("apps/media/software/TinyMediaPlayer", "recording", 60, "n:3,n:6,n:9,n:12");
    ("apps/media/software/TinyMediaPlayer", "animation", 70, "n:3,n:6,n:9,n:12,n:15,n:18,n:21");
    (* claude: and our first video, raw Y4M filmed by the 2D rasterizer,
     * 0.8 s in: 4:2:0's fringes at the square's and the ball's edges *)
    ("apps/media/software/TinyMediaPlayer", "video", 80, "n:3,n:6,n:9,n:12,n:15,n:18,n:21,n:24,n:27,n:30");
    (* claude: the same clip as FLC, d pressed: what each delta frame
     * stores, the ball's two crescents and the square's corners, the
     * rest dimmed *)
    ("apps/media/software/TinyMediaPlayer", "changes", 80, "n:3,n:6,n:9,n:12,n:15,n:18,n:21,n:24,n:27,n:30,n:33,d:40");
    (* claude: and as AVI, Motion JPEG and a sound, 1.02 s in: the frame
     * the sound's position says (the audio clock), the second landing's
     * blip in the scope; the playlist scrolled to show it *)
    ("apps/media/software/TinyMediaPlayer", "avi", 97, "n:3,n:6,n:9,n:12,n:15,n:18,n:21,n:24,n:27,n:30,n:33,n:36");
    (* claude: and as MPEG-1, a pressed: the analyzer on a B frame --
     * the still sky skipped, the ball predicted both ways (its vectors,
     * white to the past, cyan to the future), the strip of I, P and B *)
    ("apps/media/software/TinyMediaPlayer", "mpeg1", 92, "n:3,n:6,n:9,n:12,n:15,n:18,n:21,n:24,n:27,n:30,n:33,n:36,n:39,a:45");
    (* the sheet clicked once (selected), twice (active: the hatched
     * border, and its menu in the document's bar), then B1 clicked and
     * =B2*2 typed into it -- the total follows *)
    ( "apps/office/software/TinyOpenDoc",
      "sheet",
      18,
      "at(-195;245):1-18,click:2,click:5,click:8,type(=B2*2):10,return:13" );
    (* the picture activated, the fill tool and grey chosen from its own
     * menu, poured into the sky -- the sun's grey joins it seamlessly,
     * the pattern being laid from the picture's corner -- and put down
     * with Escape *)
    ( "apps/office/software/TinyOpenDoc",
      "picture",
      26,
      "at(200;250):1-6,click:2,click:5,at(-125;470):7-9,click:8,at(-125;293):10-12,click:11,at(-125;470):13-15,click:14,at(-125;221):16-18,click:17,at(200;255):19-21,click:20,escape:23,at(400;-400):22-26"
    );
    (* File > Save As..., then File > Open...: the document written to
     * the store and read back through the registry, the unknown part
     * included, the same *)
    ( "apps/office/software/TinyOpenDoc",
      "reopened",
      26,
      "at(-410;470):1-3,click:2,at(-410;291):4-6,click:5,type(parts):8,return:10,at(-410;470):11-13,click:12,at(-410;363):14-16,click:15,at(0;160):17-19,click:18,at(90;-75):20-22,click:21,at(400;-400):23-26"
    );
    (* a fourth kind of part, TinyMacDraw's: inserted, activated, an
     * oval added from its menu, dragged and made grey; then saved and
     * opened again -- the drawing written by Marshal and read back
     * through the registry *)
    ( "apps/office/software/TinyOpenDoc",
      "drawing",
      62,
      "at(0;17):1-3,click:2,at(-220;470):4-6,click:5,at(-220;293):7-9,click:8,at(-200;-60):10-13,click:11,at(-125;470):14-16,click:15,at(-125;365):17-19,click:18,at(-195;-122):20-22,click:21-27,at(-150;-130):24,at(-95;-150):25-28,at(-125;470):29-31,click:30,at(-125;293):32-34,click:33,escape:36,at(-410;470):38-40,click:39,at(-410;291):41-43,click:42,type(drawing):44,return:46,at(-410;470):47-49,click:48,at(-410;363):50-52,click:51,at(0;160):53-55,click:54,at(90;-75):56-58,click:57,at(600;-600):59-62"
    );
    (* the first text selected, its bottom handle dragged down: it is
     * given more room, and the row, the text and the placeholder below
     * it reflow as the mouse moves *)
    ( "apps/office/software/TinyOpenDoc",
      "height",
      18,
      "at(0;350):1-3,click:2,at(0;293):4-7,click:5-12,at(0;250):8,at(0;213):9-14,at(600;-600):15-18" );
    (* the gap between the sheet and the picture dragged right: the row's
     * width shared out anew, one Resize to undo *)
    ("apps/office/software/TinyOpenDoc", "split", 14, "at(0;207):1-3,click:2-8,at(20;207):4,at(40;207):5-10,at(600;-600):11-14");
    (* the picture and the sheet each made "Scale to Fit" from the Edit
     * menu, then the gap between them dragged left: the picture scaled
     * up with its share, the sheet down with its -- OLE's way, where the
     * others negotiate *)
    ( "apps/office/software/TinyOpenDoc",
      "scaled",
      32,
      "at(200;200):1-3,click:2,at(-315;470):4-6,click:5,at(-315;293):7-9,click:8,at(-250;230):10-12,click:11,at(-315;470):13-15,click:14,at(-315;293):16-18,click:17,at(0;195):19-21,click:20-26,at(-60;195):23,at(-120;195):24-28,at(600;-600):29-32"
    );
    (* a document, its text running round the sheet floating on it *)
    ("apps/office/software/TinyOffice", "document", 8, "at(-360;30):1-2,click:1,at(600;-600):4-8");
    (* the sheet dragged left and its corner dragged out: scaled up, and
     * the text reflowing round it as it moves *)
    ( "apps/office/software/TinyOffice",
      "drag",
      26,
      "at(-360;30):1-2,click:1,at(160;227):3-5,click:4-11,at(60;190):7,at(-60;150):8-12,at(50;97):13-15,click:14-20,at(90;70):17,at(130;40):18-22,at(600;-600):23-26"
    );
    (* the sheet clicked twice: edited in place, the menu bar File and
     * the sheet's own -- OLE 2's menu merging *)
    ("apps/office/software/TinyOffice", "active", 12, "at(-360;30):1-2,click:1,at(160;227):3-9,click:4,click:7,at(600;-600):10-12");
    (* the four other kinds, each holding another *)
    ("apps/office/software/TinyOffice", "spreadsheet", 6, "at(-180;30):1-2,click:1,at(600;-600):3-6");
    ("apps/office/software/TinyOffice", "presentation", 6, "at(0;30):1-2,click:1,at(600;-600):3-6");
    ("apps/office/software/TinyOffice", "picture", 6, "at(180;30):1-2,click:1,at(600;-600):3-6");
    ("apps/office/software/TinyOffice", "drawing", 6, "at(360;30):1-2,click:1,at(600;-600):3-6");
    (* the sheet dragged to the middle of the text, then Arrange > Wrap
     * Both Sides: a line filling the stretches on both of its sides *)
    ( "apps/office/software/TinyOffice",
      "both",
      24,
      "at(-360;30):1-2,click:1,at(160;227):3-5,click:4-11,at(60;227):7,at(-40;227):8-12,at(-104;470):13-15,click:14,at(-104;149):16-18,click:17,at(600;-600):19-24"
    );
    (* the sheet tied to its paragraph (Arrange > Move with Text), then
     * twelve new lines typed above that paragraph: the sheet moves down
     * with it, and the last paragraph onto a second page *)
    ( "apps/office/software/TinyOffice",
      "push",
      50,
      "at(-360;30):1-2,click:1,at(160;227):3-5,click:4,at(-104;470):6-8,click:7,at(-104;257):9-11,click:10,at(-272;345):12-14,click:13,return:20,return:22,return:24,return:26,return:28,return:30,return:32,return:34,return:36,return:38,return:40,return:42,at(600;-600):45-50"
    );
    (* and PageDown: the second page *)
    ( "apps/office/software/TinyOffice",
      "scroll",
      52,
      "at(-360;30):1-2,click:1,at(160;227):3-5,click:4,at(-104;470):6-8,click:7,at(-104;257):9-11,click:10,at(-272;345):12-14,click:13,return:20,return:22,return:24,return:26,return:28,return:30,return:32,return:34,return:36,return:38,return:40,return:42,at(600;-600):45-52,PageDown:47"
    );
    (* the sheet made "Top and Bottom": no text beside it, only above
     * and below -- each object its own way of wrapping *)
    ( "apps/office/software/TinyOffice",
      "wrap",
      14,
      "at(-360;30):1-2,click:1,at(160;227):3-5,click:4,at(-104;470):6-8,click:7,at(-104;113):9-11,click:10,at(600;-600):12-14"
    );
    (* a click in the top margin, and the header edited: the body
     * dimmed, the fields shown as codes in the footer's place *)
    ( "apps/office/software/TinyOffice",
      "header",
      14,
      "at(-360;30):1-2,click:1,at(-100;415):3-5,click:4,type( -- draft):7,at(600;-600):9-14"
    );
    (* saving, and opening again, in one run (each scene has a store
     * of its own, empty): the sheet made "Top and Bottom", saved as
     * "letter", File > New back to the start screen, and its Open...
     * -- the document as it was, its wrap included *)
    ( "apps/office/software/TinyOffice",
      "reopened",
      40,
      "at(-360;30):1-2,click:1,at(160;227):3-5,click:4,at(-104;470):6-8,click:7,at(-104;113):9-11,click:10,at(-410;472):12-14,click:13,at(-410;293):15-17,click:16,type(letter):18,return:20,at(-410;472):21-23,click:22,at(-410;401):24-26,click:25,at(0;-220):27-29,click:28,at(0;160):30-32,click:31,at(90;-75):33-35,click:34,at(600;-600):36-40"
    );
    (* B2 cleared, saved as "budget", File > New, File > Open... *)
    ( "apps/office/software/TinyExcel",
      "saved",
      40,
      "at(-85;85):1-3,click:2,at(24;229):4-6,click:5,at(24;158):7-9,click:8,at(-116;229):10-12,click:11,at(-116;50):13-15,click:14,type(budget):16,return:18,at(-116;229):19-21,click:20,at(-116;158):22-24,click:23,at(-116;229):25-27,click:26,at(-116;122):28-30,click:29,at(0;160):31-33,click:32,at(90;-75):34-36,click:35,at(600;-600):37-40"
    );
    (* 42 in A1, /S S demo, /C, /S L demo: VisiCalc's storage command,
     * the same file TinyExcel opens *)
    ( "apps/office/software/TinyVisiCalc",
      "storage",
      32,
      "type(42):2,return:4,type(/):6,type(s):8,type(s):10,type(demo):12,return:14,type(/):16,type(c):18,type(/):20,type(s):22,type(l):24,type(demo):26,return:28"
    );
    (* the presentation's show, a click on to its second slide *)
    ( "apps/office/software/TinyOffice",
      "show",
      16,
      "at(0;30):1-2,click:1,at(100;470):3-5,click:4,at(100;293):6-8,click:7,at(0;0):9-16,click:12"
    );
    (* the sheet selected, Insert > Chart, then the sheet edited in
     * place, Ink made 90: the chart, linked to it, follows *)
    ( "apps/office/software/TinyOffice",
      "chart",
      30,
      "at(-360;30):1-2,click:1,at(160;227):3-5,click:4,at(-206;470):6-8,click:7,at(-206;257):9-11,click:10,at(170;237):12-21,click:13,click:16,click:19,type(90):22,return:24,at(600;-600):26-30"
    );
    (* the master changed to two columns: everything lays itself out
     * again, the drawing shrinking to its column, the sheet spilling
     * out of it -- widths are not negotiated *)
    ("apps/office/software/TinyFrameMaker", "columns", 10, "at(-110;470):1-3,click:2,at(-110;365):4-6,click:5,at(600;-600):7-10");
    (* eight new lines typed above the sheet: it moves down with the
     * text, onto the next page, and the document grows a page *)
    ( "apps/office/software/TinyFrameMaker",
      "push",
      24,
      "at(-406;42):1-3,click:2,return:5,return:7,return:9,return:11,return:13,return:15,return:17,return:19,at(600;-600):20-24"
    );
    (* the master changed twice -- a black band, titles centred -- and
     * every slide changes with it *)
    ( "apps/office/software/TinyPowerPoint",
      "master",
      16,
      "at(-30;470):1-3,click:2,at(-30;293):4-6,click:5,at(-30;470):7-9,click:8,at(-30;257):10-12,click:11,at(400;-480):13-16"
    );
    (* the sorter: every slide, the same drawing scaled *)
    ( "apps/office/software/TinyPowerPoint",
      "sorter",
      10,
      "at(-220;470):1-3,click:2,at(-220;329):4-6,click:5,at(400;-480):7-10" );
    (* the outline, and a line typed against its edge: a sixth slide,
     * shown beside it as it is typed *)
    ( "apps/office/software/TinyPowerPoint",
      "outline",
      18,
      "at(-220;470):1-3,click:2,at(-220;365):4-6,click:5,at(80;-66):7-9,click:8,return:10,type(Questions?):12,at(400;-480):13-18"
    );
    (* the show, caught halfway through pushing slide 1 away for slide
     * 2: the two drawings, moved *)
    ( "apps/office/software/TinyPowerPoint",
      "show",
      14,
      "at(-220;470):1-3,click:2,at(-220;293):4-6,click:5,right:9,at(0;0):12-14" );
    (* slide 4's sheet clicked (activated: its menu in the bar), B1
     * clicked and 3 typed into it -- B3, =B2/B1, follows *)
    ( "apps/office/software/TinyPowerPoint",
      "part",
      24,
      "right:2,right:4,right:6,at(175;122):8-24,click:9,click:12,type(3):15,return:17" );
    (* typing on the slide itself: a point clicked at its end, a word
     * added, Enter for a new point, Tab to push it a level down -- all
     * of it edits of the outline's lines *)
    ( "apps/office/software/TinyPowerPoint",
      "typed",
      16,
      "at(300;66):1-3,click:2,type( Plus):5,return:7,type(Two years after the first Mac):9,tab:11,at(400;-480):12-16"
    );
    (* Next, then the button clicked three times: its script counts *)
    ( "apps/office/software/TinyHyperCard",
      "clicks",
      16,
      "at(299;-204):1-4,click:2,at(0;79):5-16,click:7,click:10,click:13" );
    (* the message path: "Pass it on" answers and passes, the card's
     * script answers next -- twice *)
    ( "apps/office/software/TinyHyperCard",
      "path",
      16,
      "at(299;-204):1-7,click:2,click:5,at(-236;150):8-16,click:9,click:12" );
    (* ten clicks, and the script's "answer" *)
    ( "apps/office/software/TinyHyperCard",
      "answer",
      30,
      "at(299;-204):1-4,click:2,at(0;79):5-40,click:6,click:8,click:10,click:12,click:14,click:16,click:18,click:20,click:22,click:24"
    );
    (* the button tool, the button selected, Objects > Script...: what it
     * does, to read and change *)
    ( "apps/office/software/TinyHyperCard",
      "script",
      20,
      "at(299;-204):1-4,click:2,at(455;202):5-7,click:6,at(0;79):8-10,click:9,at(-80;470):11-13,click:12,at(-80;401):14-16,click:15,at(600;-600):17-20"
    );
    (* Objects > New Button, dragged where it goes *)
    ( "apps/office/software/TinyHyperCard",
      "new",
      20,
      "at(-80;470):1-3,click:2,at(-80;257):4-6,click:5,at(0;33):7-9,click:9-14,at(-100;33):11,at(-200;-80):12-16,at(600;-600):17-20"
    );
    (* a click in the middle of the hollow rectangle goes through it,
     * and selects the grey oval behind *)
    ("apps/office/software/TinyMacDraw", "through", 8, "at(-140;135):1-4,click:2,at(600;-600):5-8");
    (* the "Objects" group dragged down, then its corner handle: the box
     * scales, its label goes with it -- the resize a map of the points *)
    ( "apps/office/software/TinyMacDraw",
      "group",
      24,
      "at(100;280):1-3,click:2-8,at(100;200):5,at(100;60):6-10,at(210;10):12-14,click:13-19,at(260;-30):16,at(320;-60):17-20,at(600;-600):21-24"
    );
    (* a rectangle drawn across the thick line, filled dark grey from the
     * Fill menu, and sent to the back: the line is in front of it *)
    ( "apps/office/software/TinyMacDraw",
      "draw",
      28,
      "at(-440;262):1-3,click:2,at(0;-150):4-6,click:5-11,at(120;-200):8,at(250;-280):9-12,at(-95;472):13-15,click:14,at(-95;259):16-18,click:17,at(-200;472):19-21,click:20,at(-200;367):22-24,click:23,at(600;-600):25-28"
    );
    ( "apps/office/software/TinyExcel",
      "edited",
      20,
      "at(19;84):1-5,click:3,at(100;181):6-10,click:8,backspace:11,return:14,at(19;84):16-20" );
    ( "apps/office/software/TinyExcel",
      "filled",
      40,
      "at(122;84):1-6,click:5-13,at(122;40):8-10,at(122;0):11-13,at(-45;227):15-18,click:16,at(-45;120):19-24,click:21,at(70;227):26-29,click:27,at(70;156):30-36,click:32,at(122;0):37-40" );
  ]

(* claude: the other look of the games that have two (Sprite.mli, the
 * artwork flag): each draws in its original's own medium by default,
 * and artwork=shapes asks for the playground's plain shapes -- the
 * teaching version, where nothing hides behind a picture. *)
let flagged : Testutil_golden.flagged list =
  [
    ("games/platform/software/TinyMario", "shapes", 5, [ "artwork=shapes" ]);
    ("games/platform/software/TinyCeleste", "shapes", 5, [ "artwork=shapes" ]);
    ("games/platform/software/TinyVVVVVV", "shapes", 5, [ "artwork=shapes" ]);
    (* claude: and juice=off (Juice.mli): every tween at its end at once *)
    ("examples/software/JuiceCurves", "off", 60, [ "juice=off" ]);
    ("examples/software/JuiceSquash", "off", 75, [ "juice=off" ]);
  ]

(* claude: played and flagged: the games whose juice was written by
 * hand, with juice=engine (Juice.mode) at the moment of a hit *)
let scripted_flagged : Testutil_golden.scripted_flagged list =
  [
    (* the computer's kick landing: sparks bursting, the screen knocked
     * (the same hit as "fight"'s, the same 6 frames of hitstop) *)
    ("games/fighting/software/TinyStreetFighter", "engine", 136, "space:1,d:95-120,f:122,g:135", [ "juice=engine" ]);
    (* a smart bomb, 2 frames after: the flash, and the world knocked
     * under the still scanner *)
    ("games/shmup/software/TinyDefender", "engine", 42, "space:1,right:5-60,b:40", [ "juice=engine" ]);
  ]

let tests = Testutil_golden.tests ~dir:"tests/2d" ~approve:"approve-golden2d" ~scripted ~flagged ~scripted_flagged scenes
