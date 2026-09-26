# Catalogue of the games and apps

Every program in this repository that you can play or use, sorted by
genre: the video games of `games/`, one directory per section below
(`games/shmup/`, ..., `games/rhythm/`), and the applications of
`apps/`, one directory per category (`apps/office/` so far). The **Dir** column says how a game is drawn: 2D, 2.5D (a 3D
look drawn by the game itself on the 2D playground) or 3D (drawn by
playground3d). The demos of one feature (`examples/`) and the unit
programs are not listed here.

Most programs are a toy version of a famous one; the **After** column
names it, with its authors and year (mostly as the program's header
gives them), or names the program here it is a twin of. **In one line**
says what the program is, short enough to become a tooltip. **What it
brought** is what the original introduced, or the technique the toy
had to write out to be that game -- each program's header comment
says it at length, with what it uses and what it leaves as exercises.

Each entry follows the same conventions, so a web page can be built from
this list without more information:

- **Source**: the link on the name, `<dir>/<Name>.ml`.
- **Screenshot**: the golden frame `tests/2d/golden/<Name>.png` (for
  2D, 2.5D and apps) or `tests/3d/golden/<Name>.png` (for 3D); other
  frames of the same program are next to it, as `<Name>_<scene>.png`.
- **Run online**: `<dir>/web/<Name>.html` (on WebGL for a 3D game),
  once built with `make js`.
- **Run natively**: `dune exec <dir>/<Name>.exe`.

A game that has two looks draws, by default, in the medium its
original really used: Pac-Man's ghosts and Mario were sprites, while
Asteroids and Battlezone were vector displays and Pong was
rectangles. The flag `artwork` changes it -- `artwork=shapes` for the
playground's plain shapes, where the whole game is in the code and
nothing hides behind a picture, `artwork=sprites` for the pixel art
(see `Sprite.mli`):

    dune exec games/platform/TinyCeleste.exe -- artwork=shapes

A **2.5D** or **3D** game with a twin says so: TinyDoom and TinyDoom3d
are the same level, drawn once by the game's own trick and once by
playground3d, side by side in `games/fps/` (see `games/README-2.5d.md`).

# Games

Each section, and its directory under `games/`, starts with its
definition. A game goes where its design puts it -- what the player
does, what the game is about -- and not where the original ran (half
of these were arcade cabinets) nor how it is drawn (that is the Dir
column).

## Shoot 'em up

`games/shmup/`: a ship, or a gun, against waves of enemies, the screen
scrolling past or the enemies coming down at you; you shoot and dodge,
and a level is a timeline of waves. Not a deathmatch against other
fighters in one arena (arcade and party).

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyInvaders](games/shmup/TinyInvaders.ml) | 2D | Space Invaders (Tomohiro Nishikado, Taito, 1978) | A formation of aliens marching down, and a cannon under four bunkers. | Difficulty that rises by itself: one alien moved per frame, so the fewer left, the faster they march. Bunkers as tile maps eroded by shots. |
| [Asteroid](games/shmup/Asteroid.ml) | 2D | Asteroids (Atari, 1979), via Haskell in Space | A ship with inertia, rocks breaking into smaller rocks. | Inertia and a wrap-around screen; two physics engines side by side (`physics=engine`: drag, exact polygon hits). |
| [TinyMissileCommand](games/shmup/TinyMissileCommand.ml) | 2D | Missile Command (Dave Theurer, Atari, 1980) | Missiles rain on six cities; aim counter-missiles where they will be. | Aiming at a point, not a target; explosions as growing circles, and chain reactions; warheads that split. |
| [TinyGalaga](games/shmup/TinyGalaga.ml) | 2D | Galaga (Namco, 1981) | A fixed shooter whose enemies fly in on paths and dive at you. | Enemies that fly: paths as Catmull-Rom splines, flown at constant speed by arc length. |
| [TinyDefender](games/shmup/TinyDefender.ml) | 2D | Defender (Eugene Jarvis and Larry DeMar, Williams, 1981) | A planet six screens wide, humanoids to save from the landers. | A world bigger than the screen that goes on without you: a cylinder world, and the scanner (a minimap is a second camera). |
| [TinyRobotron](games/shmup/TinyRobotron.ml) | 2D | Robotron: 2084 (Eugene Jarvis and Larry DeMar, Williams, 1982) | Two sticks, one to run and one to shoot, and a crowd of robots. | The twin-stick shooter: running and shooting in two independent directions; a swarm made of one-line robots. |
| [TinyZaxxon](games/shmup/TinyZaxxon.ml) | 2.5D | Zaxxon (Sega, 1982) | A fighter along an isometric fortress, over walls and between towers. | The first game seen from an angle: the isometric projection, the back-to-front sort, and the shadow that gives back the lost height. |
| [TinyGradius](games/shmup/TinyGradius.ml) | 2D | Gradius (Konami, 1985) | A cave scrolling by itself, waves of enemies, and the power-up bar. | The power-up bar; the level as a timeline of waves; options trailing where the ship was. |
| [TinyRType](games/shmup/TinyRType.ml) | 2D | R-Type (Irem, 1987) | The Force at your ship's nose, a beam to charge, a battleship at the end. | The Force, a pod you place (a five-state machine); the charged beam; the level itself as the boss. |
| [TinyStarFox](games/shmup/TinyStarFox.ml) | 3D | Star Fox (Nintendo and Argonaut, 1993) | An arwing down a canyon on rails: dodge and shoot. | Polygons on a SNES by staying on rails: the ship is two coordinates on a canyon ribbon, and Galaga's 2D paths fly across it. |

## Beat 'em up and fighting

`games/fighting/`: the fighters' own mechanics are the game -- moves
timed in frames, hitboxes against hurtboxes, blocks, combos, a body
as a skeleton of joints; one against one, or one against waves of
thugs down a street.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyFinalFight](games/fighting/TinyFinalFight.ml) | 2D | Final Fight (Capcom, 1989) | Walk down a street beating up wave after wave of thugs, then their boss. | The belt: depth along a street, and fighters drawn sorted by it; combos by chaining; the screen that locks until the wave is down. |
| [TinyStreetFighter](games/fighting/TinyStreetFighter.ml) | 2D | Street Fighter II (Capcom, 1991) | Two fighters, one screen, best of three rounds, and a fireball. | Moves measured in frames (startup, active, recovery); hitboxes against hurtboxes; high and low blocks; special moves read from the input history; hitstop. |
| [TinyVirtuaFighter](games/fighting/TinyVirtuaFighter.ml) | 3D | Virtua Fighter (Yu Suzuki, Sega AM2, 1993) | Two fighters of flat-shaded boxes on a ring you can be knocked out of. | The fighter as a skeleton: hierarchical transforms and keyframed poses; the ring-out; a camera framing two subjects. |

## Platform

`games/platform/`: crossing a level by running, jumping and climbing,
gravity and the ground being the challenge; the level bigger than the
screen, or cut into rooms to go through.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyDonkeyKong](games/platform/TinyDonkeyKong.ml) | 2D | Donkey Kong (Shigeru Miyamoto, Nintendo, 1981) | Slanted girders, ladders, and barrels rolling down at you. | Jumping as the point of a game; the hero as a state machine (walk, jump, climb, fall); slanted girders as segments. |
| [TinyJoust](games/platform/TinyJoust.ml) | 2D | Joust (John Newcomer, Williams, 1982) | Flap an ostrich over lava; the higher lance wins. | A flap that adds lift; a whole arcade game in about 200 lines, the physics layer doing the flight and every collision. |
| [TinyLodeRunner](games/platform/TinyLodeRunner.ml) | 2D | Lode Runner (Doug Smith, Broderbund, 1983) | Take all the gold, no jumping and no fighting: dig holes instead. | A map that changes: holes dug that grow back; guards' greedy pursuit; one of the first level editors. |
| [TinyMario](games/platform/TinyMario.ml) | 2D | Super Mario Bros. (Shigeru Miyamoto, Nintendo, 1985) | Run, jump, stomp, and scroll to the flag. | The side-scroller: a level bigger than the screen, typed as strings; three camera modes; parallax; sounds and music. |
| [TinyMetroid](games/platform/TinyMetroid.ml) | 2D | Metroid (Yoshio Sakamoto, Makoto Kano, Gunpei Yokoi, Nintendo, 1986) | One closed world of caves: the morph ball, the missiles, the boots, the bombs, and Kraid. | The world as locks and keys, and a checker (a search over her poses) proving it can be finished, in one order only; the map of the areas been to. |
| [TinyRick](games/platform/TinyRick.ml) | 2D | Rick Dangerous (Simon Phipps, Core Design, 1989) | A temple full of traps, starting with a boulder rolling after you. | Traps as tiles in the level's data; flip-screen rooms instead of scrolling. |
| [TinyPrinceOfPersia](games/platform/TinyPrinceOfPersia.ml) | 2D | Prince of Persia (Jordan Mechner, Broderbund, 1989) | Run, jump, hang and climb through a dungeon of ledges, spikes and a gate. | Movement driven by the animation: each move a table of rotoscoped frames, played to its end, the keys read only where it allows. |
| [TinyTurrican](games/platform/TinyTurrican.ml) | 2D | Turrican (Manfred Trenz, Rainbow Arts, 1990) | Run and gun through a world bigger than the screen: spread and laser, the lightning beam, the gyroscope wheel, and a secret cave. | A weapon aimed by an angle you steer: a ray cast through the tile map, enemies hurt by their distance to the segment; a body that changes its box, and opens only where there is room. |
| [TinyMarioWorld](games/platform/TinyMarioWorld.ml) | 2D | Super Mario World (Takashi Tezuka and Shigeru Miyamoto, Nintendo, 1990) | Cape Mario on hills, in the sky, and on a map with a secret exit. | Slopes felt by sensors, and the slide; the cape's flight as speed traded for height; the world map as a graph with secret exits. |
| [TinySonic](games/platform/TinySonic.ml) | 2D | Sonic the Hedgehog (Yuji Naka, Hirokazu Yasuhara, Naoto Ohshima, Sega, 1991) | Keep your speed, take the hill, go round the loop. | Speed along a surface: shaped tiles, one ground speed, gravity as a slope factor, the loop as ordinary tiles; the spindash. |
| [TinyVikings](games/platform/TinyVikings.ml) | 2D | The Lost Vikings (Silicon & Synapse, Interplay, 1992) | Three vikings, one keyboard: Erik jumps and headbutts walls, Baleog fights and shoots, Olaf's shield is a glider and a step; all three to the exit. | The party as the puzzle: heroes each with one ability, switched with Tab, the waiting ones part of the level; a hero as a moving one-way platform. |
| [TinyMario64](games/platform/TinyMario64.ml) | 3D | Super Mario 64 (Shigeru Miyamoto, Nintendo, 1996) | Floating platforms and five stars, running and jumping in 3D. | The platformer in 3D: controls relative to the camera, a camera you steer, a shadow to judge landings. |
| [TinyMarioGalaxy2D](games/platform/TinyMarioGalaxy2D.ml) | 2D | Super Mario Galaxy (Yoshiaki Koizumi, Nintendo, 2007) | Run round planetoids, stand underneath them, jump from one into the pull of the next, to the Power Star. | Gravity as level design: zones with a down each and a priority, not Newton; up as local, the velocity split along the ground and up; the camera turned with it, and the arrow kept while held. |
| [TinyMarioGalaxy](games/platform/TinyMarioGalaxy.ml) | 3D | Super Mario Galaxy (Yoshiaki Koizumi, Nintendo, 2007) | Round a planet, over a cube's edges, down onto a platform from under the planet, and on to the tiny planet of the Power Star. | The 2D twin's gravity zones in 3D, a box's giving cube gravity for free; directions carried along the ground as it turns; a camera that rolls after Mario's up; the world turned so the sun stays over his head. |
| [TinyBraid](games/platform/TinyBraid.ml) | 2D | Braid (Jonathan Blow, 2008) | Run, jump, die, and hold shift: time runs backwards, and each room bends it another way. | Rewind as a list of past models (the puzzle kit's Undo, sixty a second); Braid's worlds as rules over that list: green things out of time, time as position, a shadow replaying what was rewound. |
| [TinySuperMeatBoy](games/platform/TinySuperMeatBoy.ml) | 2D | Super Meat Boy (Edmund McMillen and Tommy Refenes, Team Meat, 2010) | Run, jump and wall-jump past saws to Bandage Girl, die a lot, and watch every try at once. | Death that costs nothing; the room smeared by every try; the replay of all tries, each kept as its inputs and played again through a pure step. |
| [TinyVVVVVV](games/platform/TinyVVVVVV.ml) | 2D | VVVVVV (Terry Cavanagh, 2010) | You cannot jump: flip gravity and walk on the ceiling, past spikes on both sides. | The jump replaced by one bit: a flip goes all the way and only from a surface; gravity as a sign; gravity lines, and a room that wraps top to bottom. |
| [TinyCeleste](games/platform/TinyCeleste.ml) | 2D | Celeste (Maddy Thorson and Noel Berry, 2018) | Climb a mountain a screen at a time, with a jump, a dash, and the walls. | Game feel as small named lies, each switchable: coyote time, jump buffering, variable jump, corner correction; the dash and the wall jump. |

## Arcade and party games

`games/arcade/`: one screen, or one arena (or a screen wrapping round),
with nothing to explore and no level to cross; one rule learned in
seconds; short rounds, played again at once, for a score, the last
one standing or the first to five; nothing carried from one round to
the next; often several players in the same arena.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinySpacewar](games/arcade/TinySpacewar.ml) | 2D | Spacewar! (MIT, 1962) | Two ships duelling around a star whose gravity pulls them in. | The first game with physics: inertia and gravitation, orbits and slingshots; exact hits on the ships' outlines. |
| [Pong](games/arcade/Pong.ml) | 2D | Pong (Atari, 1972), via Elm's "Making Pong" | The first port: two paddles and a ball, by the rules. | The project's first game: Model-View-Update on the simplest game there is. |
| [TinyPong](games/arcade/TinyPong.ml) | 2D | Pong (Allan Alcorn, Atari, 1972) | Pong again, the ball, walls and paddles as bodies of the physics engine. | Rules replaced by physics: a moving paddle drags the ball (friction), corners deflect it; a speed cap against tunnelling. |
| [TinyBreakout](games/arcade/TinyBreakout.ml) | 2D | Breakout (Atari, 1976) | Pong on its side, played alone against a wall of bricks. | The paddle aims by where the ball lands (a rule, not physics); the wall as a Tilemap; the original's speed-ups. |
| [Snake](games/arcade/Snake.ml) | 2D | Snake (Blockade, Gremlin, 1976), via elm snek | Eat, grow longer, don't bite yourself. | Movement on a grid and a body that grows; a port from Elm. |
| [TinyLunarLander](games/arcade/TinyLunarLander.ml) | 2D | Lunar Lander (Howard Delman and Rich Moore, Atari, 1979) | Turn, fire the engine, and set a module down on a pad of the moon. | A landing judged, not a collision: two speeds and an angle within tolerances, narrower pads worth more; one tank of fuel as the game's only clock; the camera zooming in near the ground. |
| [TinyPacman](games/arcade/TinyPacman.ml) | 2D | Pac-Man (Toru Iwatani, Namco, 1980) | Eat the dots, avoid four ghosts, eat a pellet and turn the tables. | Ghosts with personalities, each a choice of target tile; moving in corridors, the turn asked for remembered. |
| [TinyFrogger](games/arcade/TinyFrogger.ml) | 2D | Frogger (Konami, 1981) | Hop a frog across a road of cars and a river of logs, into five bays. | Obstacles that need no state: each lane a pattern repeating at a constant speed, where everything is a formula of time; riding a log is being carried by the lane's speed. |
| [TinyTron](games/arcade/TinyTron.ml) | 2D | Tron's light cycles (Bally Midway, 1982) | Two to four cycles leaving walls of light, on arenas with obstacles: the last one riding wins. | A computer at three levels, up to a search: alpha-beta scored by the Voronoi partition, as the 2010 Google AI Challenge's winners did; the rules in a kit, shared with a 3D view. |
| [TinyTronscroll](games/arcade/TinyTronscroll.ml) | 2D | tron v0.1 (Yoann Padioleau, INSA Rennes, 1997) | Light cycles on a map bigger than the screen, each player's window scrolling with them, options to take. | The author's first network game, its three netcodes side by side: 1997's wait for every answer, lockstep, rollback. |
| [TinyTron3d](games/arcade/TinyTron3d.ml) | 3D | TinyTron | The same light cycles, the trails as walls you can look at from behind. | The Elm architecture's promise: the same model, another view, with four cameras. |
| [TinyBomberman](games/arcade/TinyBomberman.ml) | 2D | Bomberman (Hudson Soft, 1983) | The NES's stage, and the battle: you against three computer bombers, the last one standing wins. | Fire spreading tile by tile, and chain reactions; computer bombers that know where the fire will be. |
| [TinyBoomerangFu](games/arcade/TinyBoomerangFu.ml) | 3D | Boomerang Fu (Cranky Watermelon, 2020) | Four foods on floating islands, one boomerang each, one hit kills. | Your only weapon leaves your hand: a return arc homing on its owner; two arenas written as text, with holes, water, a terrace and bridges, and a jump; a camera zooming to frame everybody, and shadows to read height. |
| [TinySoldat](games/arcade/TinySoldat.ml) | 2D | Soldat (Michał Marcinkowski, 2002) | A side-view deathmatch on jet boots against two bots, and ragdolls. | The 2D physics engine's capstone: a stacking world, swept bullets against tunnelling, grenade blasts, particle ragdolls. |
| [TinyMarbleMadness](games/arcade/TinyMarbleMadness.ml) | 3D | Marble Madness (Mark Cerny, Atari Games, 1984) | Roll a marble down a course floating in space, against the clock. | A ball rolling on a height map (5/7 of g sin a), falls that break it, collisions between balls; a far, nearly isometric camera. |
| [TinyCameltry](games/arcade/TinyCameltry.ml) | 2D | Cameltry (Taito, 1989) | You don't move the ball, you turn the maze. | Turning the maze is turning gravity; rotation, so the ball rolls instead of sliding (`rotation=off` to compare). |
| [TinyPang](games/arcade/TinyPang.ml) | 2D | Pang (Mitchell, 1989) | Shoot a harpoon up at bouncing balloons: each bursts into two smaller ones, round the world's landmarks. | A bounce that is a rule, not physics: each size bounces to its own height forever; splitting as recursion, one big balloon fifteen hits. |
| [TinyFlappyBird](games/arcade/TinyFlappyBird.ml) | 2D | Flappy Bird (Dong Nguyen, .GEARS, 2013) | One button: flap through the gaps between pipes. | A flap that sets the velocity; an endless world made and dropped as you fly; randomness from an LFSR kept in the model. |

## Puzzle and board games

`games/puzzle/`: thinking over reflexes -- a set of rules to master, a
level to solve or a board to win; time pressure, when there is some
(Tetris), only adds to the thinking.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [AiChess](games/puzzle/AiChess.ml) | 2D | Chess, as Claude Shannon's "Programming a Computer for Playing Chess" (1950) set it for computers | Chess against a computer thinking 3 moves ahead with alpha-beta. | Rules checked by perft; move ordering (most valuable victim first); quiescence, the captures played out at the leaves against the horizon effect. |
| [AiConnect4](games/puzzle/AiConnect4.ml) | 2D | Connect Four (Milton Bradley, 1974; solved by Allen and by Allis, 1988) | Connect 4 against a computer that searches deeper by iterative deepening, move ordering and a transposition table, and says what each saved. | Iterative deepening, ordering (the middle columns first) and Zobrist keys, with the node counts they save. |
| [AiGo](games/puzzle/AiGo.ml) | 2D | Go on 9x9, and the Monte Carlo programs of 2006 that first played it decently | Go against a computer with no evaluation function at all: it judges a position by playing it out at random, hundreds of times. | Monte Carlo tree search (UCT), the playouts and the tree, anytime: it thinks while the game draws. |
| [AiOthello](games/puzzle/AiOthello.ml) | 2D | Othello (Goro Hasegawa, 1971) | Othello against a computer thinking 4 moves ahead with alpha-beta. | Game-tree search: alpha-beta, an evaluation table, and its cuts counted against plain minimax. |
| [TinySokoban](games/puzzle/TinySokoban.ml) | 2D | Sokoban (Hiroyuki Imabayashi, Thinking Rabbit, 1982) | Push every box onto a goal, one at a time, never pulling. | Deep puzzles from a few rules (PSPACE-complete); undo for free, since the model is a value; levels checked by breadth-first search. |
| [TinySokobanEd](games/puzzle/TinySokobanEd.ml) | 2D | the level editors games shipped with (Lode Runner's, Doug Smith, Broderbund, 1983) | TinySokoban's levels: type them, check them, play them, export the file the game is built with. | A game's tool: the rules, format and solver in the kit, the level a text file embedded at build time; a text editor on a grid, with the checks a text editor cannot do. |
| [Tetris](games/puzzle/Tetris.ml) | 2D | Tetris (Alexey Pajitnov, 1984), via elm-flatris | Falling pieces, full lines cleared. | Falling pieces and cleared lines on a grid; a port from Elm. |
| [TinyTetris](games/puzzle/TinyTetris.ml) | 2D | Tetris (Alexey Pajitnov, 1984), with the Nintendo version's scores (1989) and the later bag of seven | Falling pieces, full lines cleared, juiced: lines burst into their colors, drops knock the well. | The seven-piece bag, the ghost, the lock delay; juice from the start, in a section of its own watching the rules (juice=off: the dry game). |
| [TinyBlockout](games/puzzle/TinyBlockout.ml) | 3D | BlockOut (P.Z.Karen Co., California Dreams, 1989) | Tetris down a well, seen from above, with pieces turning in 3D. | Tetris with one more index; depth cues (shading, the landing ring); quarter turns of polycubes in integers. |
| [TinyLemmings](games/puzzle/TinyLemmings.ml) | 2D | Lemmings (DMA Design, Psygnosis, 1991) | Creatures walk mindlessly; give them jobs to save them. | Indirect control; the terrain as a bitmap in the model, dug and built; creatures as tiny state machines reading the pixels. |
| [TinyIncredibleMachine](games/puzzle/TinyIncredibleMachine.ml) | 2D | The Incredible Machine (Kevin Ryan, Jeff Tunnell, Dynamix / Sierra, 1993) | Build a Rube Goldberg machine from a bin of parts, then watch it put a ball in a basket. | Building, then watching: physics that must be deterministic; joints (a seesaw's pin, a pulley's ropes); parts acting on each other, a switch turning on a fan. |
| [TinyStoneAge](games/puzzle/TinyStoneAge.ml) | 2D | Stone Age (Stonehenge Soft Art, Eclipse Software, 1992) | Take a dinosaur across stones over the void to the cave: cracked ones fall behind you, arrow blocks carry you, keys open locks. | A puzzle of moves that can't be undone, so of their order; a level as a graph of states, and a breadth-first search proving each level can be done in time. |
| [TinyPuzzleBobble](games/puzzle/TinyPuzzleBobble.ml) | 2D | Puzzle Bobble (Taito, 1994) | Shoot a bubble off the walls; three of a colour pop. | A hexagonal grid stored as offset rows; snapping to it; two flood fills (the pop, then the fall); the aiming guide as the shot flown ahead. |
| [TinyPortal2D](games/puzzle/TinyPortal2D.ml) | 2D | Portal: The Flash Version (We Create Stuff, 2007) | A platformer where the way through is a pair of holes you shoot. | A portal is a rigid transform: position and velocity rotated, never scaled -- the fling -- on the physics layer. |
| [TinyPortal](games/puzzle/TinyPortal.ml) | 3D | Portal (Valve, 2007) | One test chamber, a cube on a ledge, and a portal gun: fling yourself. | The portal in 3D: a rigid motion carrying orientation and spin; seeing through by taking the chamber through it and clipping, no stencil. |
| [TinyCrush](games/puzzle/TinyCrush.ml) | 2.5D | Crush (Zoë Mode, Sega, 2007) | Crush a level flat along your line of sight, and walk across what was far apart in depth. | The projection as the puzzle: a stack of slices, crushed into their union; the same platformer on either plane; the crush drawn as a cabinet projection losing its depth. |
| [TinyCrush3d](games/puzzle/TinyCrush3d.ml) | 3D | Crush3D (Zoë Mode, Sega, 2012) | TinyCrush with real cubes: crush, and watch the level fold onto your plane as the camera turns straight on. | The same rules drawn twice (`gamekits/crush`): the crush as the slices sliding in depth and the camera swinging from three-quarters to straight on; why the crushed level is drawn as its plane (z-fighting). |
| [TinySlingshot](games/puzzle/TinySlingshot.ml) | 2D | Angry Birds (Rovio, 2009) | Pull back the slingshot and knock down a tower of physics bodies. | The whole 2D engine at once: a solver for stacking, rotation, friction; targets broken by impulse; the arc predicted by the engine's own steps. |
| [TinyFez](games/puzzle/TinyFez.ml) | 2.5D | Fez (Phil Fish, Polytron, 2012) | A flat world you can turn a quarter at a time: what lines up on the screen is next to each other. | Depth thrown away, four ways: any top a floor, only what is at your depth a wall; the turn drawn as each cube's two faces squashed to the cosine and the sine; pixel art from XPM files. |
| [TinyPerspective](games/puzzle/TinyPerspective.ml) | 3D | Perspective (DigiPen, a student game, 2012) | Place a camera in a 3D world, then run a 2D platformer on the picture it takes. | The projection with perspective: far is small, near is big, so the camera builds the level; the runner's world the boxes' silhouettes filled into a grid of the screen; the camera turning round the point under his feet, found by undoing the projection. |
| [TinyMonumentValley](games/puzzle/TinyMonumentValley.ml) | 3D | Monument Valley (ustwo, 2014) | Walk over architecture that cannot exist, trusting the picture. | An orthographic camera's ambiguity as the rule: blocks that look adjacent are walkable; a search over the picture's graph. |
| [TinyWitness](games/puzzle/TinyWitness.ml) | 2D | The Witness (Jonathan Blow, Thekla, 2016) | Draw one line across each panel of an island: mazes, dots, and black and white squares to keep apart, taught without a word. | Rules about regions: the line cuts the grid, a flood fill finds the regions, a rule is a check on each; a panel typed as text; a rule taught by a row of panels. |
| [TinyBabaIsYou](games/puzzle/TinyBabaIsYou.ml) | 2D | Baba Is You (Arvi Teikari, 2019) | The rules are words on the board, and you can push them. | The rules as data in the world they rule, rewritten by the player: a game that is its own level editor. |

## Card games

`games/cards/`: a deck of cards and the rules of a game played with
it -- patience alone, or a table of players; what shows and what is
hidden, and the luck of the deal, are the game. Every deal has a
number (Microsoft's, the cards kit's), so a game can be played again.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinySolitaire](games/cards/TinySolitaire.ml) | 2D | Solitaire, Klondike (Wes Cherry, Microsoft, 1990) | Seven columns, the stock, and four foundations to build, ace to king. | The patience that taught a generation the mouse; face-down cards turned over as they are uncovered, and the luck of what is hidden. |
| [TinyFreeCell](games/cards/TinyFreeCell.ml) | 2D | FreeCell (Paul Alfille, 1978; Windows, 1995) | All 52 cards face up on eight columns, and four free cells to park them. | Numbered deals, the same on every computer (Microsoft's random generator, written out); the supermove, a run moved at once as the single moves it stands for. |

## Action-adventure and horror

`games/adventure/`: a world explored -- in words and turns at first
(the text adventure), then in pictures, then in real time -- whose
rooms, items and keys open the way on; the hero does not grow, the player's
knowledge of the world does. In horror, the camera and the dark are
part of the game.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyZork](games/adventure/TinyZork.ml) | 2D | Zork (Tim Anderson, Marc Blank, Bruce Daniels and Dave Lebling, MIT, 1977; Infocom, 1980) | A white house, a trap door, a troll, and a grue in the dark, in words only. | The text adventure: the world as data and a rule per thing to try (the adventure kit), a parser of whole sentences, the dark; the game nearly all model, a terminal for its view. |
| [TinyWumpus](games/adventure/TinyWumpus.ml) | 2D | Hunt the Wumpus (Gregory Yob, 1973) | Twenty rooms in the dark, a smell, a draft, a rustle of bats, and five crooked arrows. | The cave a dodecahedron, the first game off the grid, a table of tunnels mapped in the player's head; an arrow told the rooms it flies through, around corners; the game a conversation (Teletype, Tty_wumpus), the same value a command of TinyTerminal's shell. |
| [TinyZelda](games/adventure/TinyZelda.ml) | 2D | The Legend of Zelda (Shigeru Miyamoto and Takashi Tezuka, Nintendo, 1986) | An overworld of screens, a sword, a key, and a dungeon. | A land to explore in any order: rooms that slide in, an inventory as locks and keys, monsters wandering on a generator in the model. |
| [TinyManiacMansion](games/adventure/TinyManiacMansion.ml) | 2D | Maniac Mansion (Ron Gilbert and Gary Winnick, Lucasfilm Games, 1987) | Past the purple tentacle and into Dr. Fred's lab, a verb and a click at a time. | The parser become a menu: SCUMM's verbs and sentence line, on the same world model as TinyZork; walkboxes, the floor as boxes and a path through them; a script per object. |
| [TinyZeldaLinkPast](games/adventure/TinyZeldaLinkPast.ml) | 2.5D | The Legend of Zelda: A Link to the Past (Nintendo, 1991), after TinyZelda | Hyrule scrolling past, soldiers and bushes to cut, three pendants, and the Master Sword in the Lost Woods. | The three-quarter view: the ground from above, what stands on it from the front, drawn farthest first by its base line -- walking behind a tree's canopy with no height anywhere in the model; pixel art in XPM files, a sprite sheet cut into frames, each picture painted colour over colour in few rectangles. |
| [TinyAloneInTheDark](games/adventure/TinyAloneInTheDark.ml) | 3D | Alone in the Dark (Frédérick Raynal, Infogrames, 1992) | A house at night seen by fixed cameras, a locked study, and something in the corridor. | Fixed cameras that cut between rooms (cinematography); tank controls, the only ones that survive a cut; a doorway's hysteresis. |
| [TinyTombRaider](games/adventure/TinyTombRaider.ml) | 3D | Tomb Raider (Toby Gard, Core Design, 1996) | A raider in a stone tomb, an idol on a pedestal, and a boulder. | Committed moves of fixed length, and a level measured in them (no collision code); textured stone from one atlas page. |
| [TinyGTA](games/adventure/TinyGTA.ml) | 2.5D | Grand Theft Auto (DMA Design, 1997) | A city from above: any car is yours, phones ring with work, and the police count your crimes in stars. | The open world: a city that runs without you (traffic and people on a road graph), any car taken, the wanted level; the camera rising with speed; buildings leaning out of the screen. |
| [TinyMetalGearSolid](games/adventure/TinyMetalGearSolid.ml) | 2D | Metal Gear Solid (Hideo Kojima, Konami, 1998), after Metal Gear (1987) | Across an enemy base to the elevator, unseen, with a radar, a knock and a cardboard box. | Stealth: cones of vision stopped by walls, shown only on the radar, which jams in an alert; guards as a state machine written as data (patrol, "?", "!", evasion), walking by A*. |
| [TinyZeldaOcarina](games/adventure/TinyZeldaOcarina.ml) | 3D | The Legend of Zelda: Ocarina of Time (Nintendo EAD, 1998) | Across Hyrule Field by day and by night to a temple, and a Stalfos whose shield must be got round. | Z-targeting: the lock-on, the controls re-read around the enemy held (sideways is an orbit) and the camera over the shoulder on the line through the two; an enemy that blocks and opens after its telegraphed chop. The field as a hub: loading zones to places with their own coordinates, a clock that runs only there, a night with its own monsters, the light baked in. |
| [TinyIco](games/adventure/TinyIco.ml) | 2D | Ico (Fumito Ueda, Team Ico, Sony, 2001) | Lead Yorda by the hand out of a castle: pull her up ledges, call her over gaps, beat off the shadows that come for her. | The companion you look after: an AI with limits of her own, a hand as a kept distance, obstacles that are hers not yours; enemies after her, not you. |
| [TinyJourney](games/adventure/TinyJourney.ml) | 2D | Journey (Jenova Chen, thatgamecompany, 2012) | Walk, slide and fly across a desert to the light on the mountain, a stranger in white at your side. | Cooperation without words: one verb, the chirp, and nearness; the scarf as the only resource; the ground a smooth function, its slope the sliding. |

## Role-playing and dungeons

`games/rpg/`: a character who grows -- hit points, levels, items --
through a dungeon of many rooms or levels, often generated, fought in
turns or in real time.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyRogue](games/rpg/TinyRogue.ml) | 2D | Rogue (Michael Toy, Glenn Wichman, Ken Arnold, 1980) | A dungeon made anew each game, down to the Amulet of Yendor. | Time in turns; a dungeon generated from a seed; what you see, and what you have seen. |
| [TinyGauntlet2](games/rpg/TinyGauntlet2.ml) | 2D | Gauntlet II (Ed Logg, Atari Games, 1986) | Four heroes, a dungeon seen from above, and generators spawning crowds. | Generators (monsters as a flow, not a set); health as the clock; greedy chasers or one flow field (`chase=field`). |
| [TinyDungeonMaster](games/rpg/TinyDungeonMaster.ml) | 2.5D | Dungeon Master (FTL Games, 1987) | One hero underground, a step at a time, with a torch and no map. | Real time on a grid; a view with no projection at all: nested frames at fixed places, drawn farthest first. |
| [TinyDiablo](games/rpg/TinyDiablo.ml) | 2.5D | Diablo (Blizzard North, 1996) | Click to walk and kill in a dungeon drawn from one fixed angle. | Rogue in real time: click to walk (the isometric projection inverted, then A*); a dungeon from an LFSR; the dark. |
| [TinyHades](games/rpg/TinyHades.ml) | 3D | Hades (Supergiant, 2020) | Out of the underworld a chamber at a time, a boon from a god after each. | The roguelite: death pays for the next run; boons as numbers changed for a run; the invulnerable dash. |

## First-person

`games/fps/`: the world seen through the eyes of whoever you play, on
foot -- shooting, building or breaking; and each game here is also
about its view, from a raycaster to a real 3D renderer.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyMazeWar](games/fps/TinyMazeWar.ml) | 2.5D | Maze War (Steve Colley, Greg Thompson and Howard Palmer, NASA Ames, 1973-74) | Eyeballs hunting each other down the corridors of a maze, in lines. | The first first-person game and shooter: nested frames, a cell at a time, in lines only; every player a command per tick, the shape of lockstep networking; robots. |
| [TinyBattlezone](games/fps/TinyBattlezone.ml) | 2.5D | Battlezone (Ed Rotberg, Atari, 1980) | A tank's periscope over a plain of wireframe pyramids and blocks. | The first 3D arcade hit: wireframes taken into the eye's coordinates, clipped at the near plane, divided by the depth. |
| [TinyBattlezone3d](games/fps/TinyBattlezone3d.ml) | 3D | TinyBattlezone | The same plain and the same enemy, with solid faces. | A z-buffer turns obstacles into cover. |
| [TinyCyberSled](games/fps/TinyCyberSled.ml) | 3D | Cyber Sled (Namco, 1993) | Two hover-tanks duel in a closed arena of flat-shaded polygons. | Twin-stick tank controls, one stick per tread, plus strafing: two sticks read as a speed, a turn and a slide; homing missiles, ramps, a split screen, 3D juice (Juice3d) and play over the network (Multiplayer3d) as flags. |
| [TinyWolfenstein](games/fps/TinyWolfenstein.ml) | 2.5D | Wolfenstein 3D (id Software, 1992) | A maze walked in first person. | The raycaster: one ray per screen column through a grid (DDA); billboards hidden by a one-dimensional z-buffer. |
| [TinyWolfenstein3d](games/fps/TinyWolfenstein3d.ml) | 3D | TinyWolfenstein | The same maze, each wall cell a box. | What 3D costs and buys: every box projected and filled, nothing assumed about the walls. |
| [TinyDoom](games/fps/TinyDoom.ml) | 2.5D | Doom (id Software, 1993) | A level of sectors: stairs, a pillar, a window onto a dark room. | The BSP tree: walls nearest first, per-column clip arrays, floors and ceilings at any height, and no z-buffer. |
| [TinyDoom3d](games/fps/TinyDoom3d.ml) | 3D | TinyDoom | The same level, drawn as any 3D scene. | What a z-buffer makes unnecessary: no tree, no order, every polygon every frame. |
| [TinyQuake](games/fps/TinyQuake.ml) | 3D | Quake (id Software, 1996) | Three rooms, three runes, and the exit, in true 3D. | The level prepared before the game: qbsp (CSG and a BSP of solid space), vis (potentially visible sets), light (lightmaps). |
| [TinyHalfLife2](games/fps/TinyHalfLife2.ml) | 3D | Half-Life 2 (Valve, 2004) | A yard of Ravenholm, two zombies, and the gravity gun. | Physics as the game: the gravity gun, a hinged seesaw, floating barrels, zombies that go limp as ragdolls. |
| [TinyMinecraft](games/fps/TinyMinecraft.ml) | 3D | Minecraft (Markus Persson, 2009) | Walk, jump, fly, and remove and place blocks. | A voxel world in a hash table: chunks cached on the GPU, hidden faces culled, blocks picked by a ray, one texture atlas. |
| [TinyTeardown](games/fps/TinyTeardown.ml) | 3D | Teardown (Dennis Gustafsson, Tuxedo Labs, 2020) | A heist in a level of voxels, every one of which can be knocked out. | Destructible voxels: greedy meshing, a flood fill finding what is loose, and loose pieces turned into rigid bodies of the 3D engine. |

## Flight and space

`games/flight/`: flying a craft in three dimensions (or its top-down
shadow), over terrain, through space or down a mine; the controls of
a vehicle that is not on the ground.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyElite](games/flight/TinyElite.ml) | 2.5D | Elite (David Braben and Ian Bell, Acornsoft, 1984) | A Cobra in orbit round Lave, pirates, and docking in the Coriolis station. | Full 3D in 32 KB: the universe turning round a ship that never moves, small-angle rotations, hidden lines on convex hulls, a generated galaxy. |
| [TinyElite3d](games/flight/TinyElite3d.ml) | 3D | TinyElite | The same flight, with solid ships. | Backface culling and a z-buffer take over the hidden lines, and hide ships behind ships. |
| [TinyXpilot](games/flight/TinyXpilot.ml) | 2D | XPilot (Bjørn Stabell and Ken Ronny Schouten, 1991) | Ships with inertia and gravity in a cave, cannons and fuel stations. | Newton's third law by hand (a rope between ship and ball); a crash judged by the jolt; cannons that lead their target; the split screen by clipping. |
| [TinyComanche](games/flight/TinyComanche.ml) | 2.5D | Comanche: Maximum Overkill (NovaLogic, 1992) | A helicopter over a voxel island, popping balloons. | Voxel Space: a height map drawn column by column, nearest first, with a y-buffer. |
| [TinyComanche3d](games/flight/TinyComanche3d.ml) | 3D | TinyComanche | The same island as triangles and a z-buffer. | The flight simulators' way, then the GPUs': the terrain as a mesh, the camera free to roll. |
| [TinyDescent](games/flight/TinyDescent.ml) | 2.5D | Descent (Parallax Software, 1995) | A ship flying any way up through a mine, to the robots and the exit. | Six degrees of freedom; a mine drawn through the portals between its convex cells, farthest first, with no z-buffer. |
| [TinyDescent3d](games/flight/TinyDescent3d.ml) | 3D | TinyDescent | The same mine, drawn by playground3d. | The trade the GPUs won: a depth test per pixel instead of a portal walk. |

## Racing

`games/racing/`: a course driven round against the clock or against
other drivers; the road, the track or the map, and the car on it.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyGranTrak10](games/racing/TinyGranTrak10.ml) | 2D | Gran Trak 10 (Atari, 1974) | One white car on a black track, alone against the clock, a gear lever and oil slicks. | The first racer from above: a four-speed gearbox heard in the engine's pitch; the track as its center line, walls where it's too far. |
| [TinyOutRun](games/racing/TinyOutRun.ml) | 2.5D | Out Run (Yu Suzuki, Sega, 1986) | A red convertible on a road of curves and hills, palm trees going by. | The pseudo-3D road: segments projected as trapezoids, curves as shifts that add up, hills that hide, fog. |
| [TinySuperSprint](games/racing/TinySuperSprint.ml) | 2D | Super Sprint (Atari Games, 1986) | Four little cars on a figure eight with a bridge, golden wrenches buying upgrades. | Cars that bump and bounce off walls; a track crossing itself, each car's walls measured around where it is, the bridge drawn between the two levels. |
| [TinySuperOffRoad](games/racing/TinySuperOffRoad.ml) | 2.5D | Ironman Ivan Stewart's Super Off Road (Leland, 1989) | Four trucks in a dirt stadium seen from the stands, over a jump, the whoops, a hill and a mud hole. | Ground with a height: slower up, faster down, flying off a crest when the ground falls away faster than gravity; the stadium as lifted quads, sorted with the trucks. |
| [TinySupercars](games/racing/TinySupercars.ml) | 2D | Supercars (Magnetic Fields, Gremlin, 1990) | Three laps against three cars on a scrolling track, missiles, and a shop between races. | A career: prize money, damage, repairs and upgrades; the camera following ahead, and the whole track again as a minimap. |
| [TinyMicroMachines](games/racing/TinyMicroMachines.ml) | 2D | Micro Machines (Codemasters, 1991) | Tiny cars racing seen from above on a breakfast table. | Cars that drift (the velocity turning towards the heading by a grip); a camera looking ahead of the leader; the screen's edge as the finish line. |
| [TinyMarioKart](games/racing/TinyMarioKart.ml) | 2.5D | Super Mario Kart (Nintendo, 1992) | A kart race on a Mode 7 floor, and the balloon battle on Battle Course 1, alone or two in a split screen. | Mode 7: the floor sampled a screen row at a time; karts, shells and item boxes as billboards; the split screen. |
| [TinyVirtuaRacing](games/racing/TinyVirtuaRacing.ml) | 3D | Virtua Racing (Yu Suzuki, Sega AM2, 1992) | TinyOutRun's course in flat-shaded polygons, to the GOAL arch. | The first great polygon racer: the road as a banked ribbon in space, four views, hills hidden by the z-buffer. |
| [TinyMarioKart64](games/racing/TinyMarioKart64.ml) | 3D | Mario Kart 64 (Nintendo, 1996) | Three laps against seven karts, item boxes, and a four-player battle. | Polygons for the world and sprites for the karts; four players on one screen; a battle arena with two heights. |
| [TinyBigRedRacing](games/racing/TinyBigRedRacing.ml) | 3D | Big Red Racing (Big Red Software, Domark, 1996) | Three laps against three drivers over the Highlands, down a red canyon and on the Moon, a vehicle per course. | The course in the terrain, not on it: the road painted quads, the canyon's walls the ground raised; a wall is ground too steep to climb; the Moon's sixth of gravity, every crater a ramp. |
| [TinyIgnition](games/racing/TinyIgnition.ml) | 3D | Ignition (UDS, Virgin, 1997) | A sports car, a police car and a school bus racing a country road seen from above, over two ramps; two players side by side. | Micro Machines' view and driving on ground with a height: the racing kit's Offroad (the slope's pull, the takeoff when the ground falls away faster than the car falls, the landing), the ground a function made from the road (Track3d.locate), the screen split down the middle. |
| [TinySSX](games/racing/TinySSX.ml) | 3D | SSX (EA Canada, 2000) | A snowboard race down a mountain against three riders; off the kickers, spins, flips and grabs, landed straight or wiped out, filling the boost meter. | Tricks as the race's fuel: in the air the keys turn the rider, and on landing the angles are checked, named and scored; the rider a car without an engine (the racing kit's Offroad, gravity along the slope), the mountain the racing kit's Road going down, walked into space by Track3d.of_road, its profile smoothed so the riders don't hop off every sample. |

## Sports and tables

`games/sports/`: a sport or a table game played on the screen as it is
played off it -- a ball or a puck, a pitch or a table, its physics and
its rules (or, like Speedball's, rules it could have), and a team or
an opponent to beat. The table games count: pinball and air hockey
(Shufflepuck) are simulated tables. Pong is not here: it only borrowed
tennis's idea, two paddles and a ball, and is an arcade game.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyTennisForTwo](games/sports/TinyTennisForTwo.ml) | 2D | Tennis for Two (William Higinbotham and Robert Dvorak, Brookhaven National Laboratory, 1958) | Tennis from the side on an oscilloscope, a knob and a button each. | The first video game made to entertain: a ballistic trajectory, its bounces, a net; the volley; the phosphor's trail. |
| [TinyShufflePuck](games/sports/TinyShufflePuck.ml) | 2.5D | Shufflepuck Café (Christopher Gross, Brøderbund, 1988) | Air hockey down a table in perspective against a bar's regulars. | The table seen from your end: one division of perspective, and its inverse for the mouse; opponents as a few knobs; substeps against tunnelling. |
| [TinyKickOff2](games/sports/TinyKickOff2.ml) | 2D | Kick Off 2 (Dino Dini, Anco, 1990) | Football from above, where the ball is not glued to your feet. | The free ball (`ball=glued` to compare); aftertouch; a team as a formation pulled towards the ball. |
| [TinySpeedball2](games/sports/TinySpeedball2.ml) | 2D | Speedball 2: Brutal Deluxe (The Bitmap Brothers, 1990) | Handball, ice hockey and a fist fight on a sheet of metal. | The ball carried; an arena that scores like a pinball table; walls instead of touchlines; the tackle as a move. |
| [TinySensibleSoccer](games/sports/TinySensibleSoccer.ml) | 2D | Sensible Soccer (Jon Hare and Chris Yates, Sensible Software, 1992) | Football with aftertouch, the third answer to the ball question. | Close control, between glued and free; the ball's height and its shadow; a pulled-back view; lofted aftertouch. |
| [TinyPinball](games/sports/TinyPinball.ml) | 2D | Pinball Dreams (Digital Illusions, 1992) | A table from above: plunger, flippers, bumpers, targets, and tilt. | The flipper carries the ball (its surface speed, w r); the table as data; substeps against tunnelling; two engines. |
| [TinyTonyHawk](games/sports/TinyTonyHawk.ml) | 2D | Tony Hawk's Pro Skater (Neversoft, Activision, 1999) | Two minutes on a half-pipe seen from the side: pump, fly, flip, grab, and chain it all. | The combo: a chain's tricks summed and multiplied by its length, a repeat worth half; the revert and the manual as its links; the landing judged by the board's angle; the skater as a point on a curve. |
| [TinyPinball3d](games/sports/TinyPinball3d.ml) | 3D | TinyPinball | The same table in 3D, on the physics engine. | Continuous collision: the ball swept by conservative advancement, even against flippers moving and turning. |

## Strategy and simulation

`games/strategy/`: planning over reflexes, at a scale above one
character -- an army, a squad in turns, a city, a civilization -- or a
system to run and keep alive.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyHamurabi](games/strategy/TinyHamurabi.ml) | 2D | Hamurabi (Doug Dyment, 1968, after The Sumer Game, Mabel Addis and William McKay, IBM, 1964) | Ten years of ancient Sumer, a harvest at a time, typed on a teletype. | The first simulation game, and the first designed by a woman: a city of five numbers, a year one function of three answers and four dice; the rules as David Ahl's BASIC printed them. |
| [TinySimCity](games/strategy/TinySimCity.ml) | 2D | SimCity (Will Wright, Maxis, 1989) | Lay out roads, power and zones, and the city builds itself. | The game is the simulation: zones that need each other (the RCI bars), power as a flood fill, pollution as a blur. |
| [TinyCivilization](games/strategy/TinyCivilization.ml) | 2D | Civilization (Sid Meier and Bruce Shelley, MicroProse, 1991) | Found cities from 4000 BC and climb a tree of technologies. | The tech tree as a DAG; a city as an economy of food, shields and trade; a world made from noise; a rival playing by the same rules. |
| [TinyXCOM](games/strategy/TinyXCOM.ml) | 2D | X-COM: UFO Defense (Julian Gollop, Mythos Games / MicroProse, 1994) | Four soldiers, a crashed UFO, and a fight in turns. | Time units spent on every step and shot; the chance to hit shown before shooting; a vision cone per soldier; reaction fire in the enemy's turn. |
| [TinyDune2](games/strategy/TinyDune2.ml) | 2D | Dune II (Westwood Studios, 1992) | Harvest, build, and order units by pointing: real-time strategy. | Real-time strategy's loop (harvest, build, fight); orders as A* paths; a search whose goal is a question. |
| [TinyWarcraft2](games/strategy/TinyWarcraft2.ml) | 2D | Warcraft II (Blizzard, 1995) | Peasants mine gold and chop wood; footmen fight orcs. | One flow field for a whole crowd; the fog of war; two resources. |
| [TinyWorms](games/strategy/TinyWorms.ml) | 2D | Worms (Andy Davidson, Team17, 1995) | Artillery for two: angle, power, the wind, and craters. | Projectiles under gravity and wind; a height-map terrain carved by craters. |
| [TinyTowerDefense](games/strategy/TinyTowerDefense.ml) | 2D | Desktop Tower Defense (Paul Preece, 2007) | Build the maze the monsters must walk, out of towers. | The player builds the maze: A* redone at every tower, and the search as a referee that forbids closing the way. |

## Rhythm

`games/rhythm/`: pressing in time with music, judged by the music's
clock rather than the screen's.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyDDR](games/rhythm/TinyDDR.ml) | 2D | Dance Dance Revolution (Konami, 1998) | Press each arrow as it reaches the top, in time with the music. | What time is it? Steps judged by the music's clock, the calibration measured from the player; charts computed from the melody. |
| [TinyGuitarHero](games/rhythm/TinyGuitarHero.ml) | 2.5D | Guitar Hero (Harmonix, 2005) | Hold the fret and strum as each note reaches the line. | The instrument: fret and strum, long notes, a difficulty as the same song reduced; the highway as Out Run's road straightened. |
| [TinyRockBand](games/rhythm/TinyRockBand.ml) | 3D | Rock Band (Harmonix, 2007) | A band on four highways: play guitar, bass, drums or keys. | The band: four parts from a tune's four voices, four ways of pressing, drums from a percussion voice, one crowd meter. |

## Programming games

`games/programming/`: you win by writing the program that plays --
the player is a programmer, the game a machine and its rules, and a
match is programs run against the machine or against each other.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyKarel](games/programming/TinyKarel.ml) | 2D | Karel the Robot (Richard E. Pattis, Stanford, 1981) | Type a program in Pattis's language and watch the robot run it: the newspaper, the harvest, the stairs, the maze. | Teaching a robot new words before any variable; a recursive-descent parser with its mistakes on their lines; the right-hand rule, a program for a world it has never seen. |
| [TinyCoreWar](games/programming/TinyCoreWar.ml) | 2D | Core War (A. K. Dewdney and D. G. Jones, 1984) | Write two programs in its editor, then watch them fight in one circular memory, each trying to make the other execute a DAT. | A virtual machine and its assembler, a page each (Redcode, MARS), the assembler's mistakes shown on their lines; the classic warriors to start from -- the Imp, the Dwarf, the Mice -- and why each beats the next. |

# Apps

In `apps/office/`, the sections below up to Graphics (excluded) being
its kinds of program, and in `apps/gamedev/`, the tools making what any
game can use (a genre's level editor is with its games instead, as
TinySokobanEd is: see `games/README-tools.md`). The other categories
each have a section, which names their directory.

## Word processing and publishing

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyBravo](apps/office/TinyBravo.ml) | app | Bravo (Butler Lampson and Charles Simonyi, Xerox PARC, 1974) | The first editor where the screen looked like the page. | WYSIWYG, and the piece table; modal commands, with their famous "edit" trap. |
| [TinyWord](apps/office/TinyWord.ml) | app | Microsoft Word (1983 on DOS, 1985 on the Macintosh) | The same text as TinyBravo, with no modes. | No modes: a caret wherever you click, cut, copy and paste, menus you can read, undo by name. |
| [TinyFrameMaker](apps/office/TinyFrameMaker.ml) | app | FrameMaker (Charles Corfield, Frame Technology, 1986) | A long document that lays itself out in columns and pages. | Text flowing through a chain of frames over pages; master pages; anchored frames carrying parts along with the text. |

## Spreadsheets

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyVisiCalc](apps/office/TinyVisiCalc.ml) | app | VisiCalc (Dan Bricklin and Bob Frankston, 1979) | The program that sold the Apple II: cells and formulas. | The spreadsheet on a 40-column screen with no mouse: the cursor as the interface, slash commands, recalculation in row or column order. |
| [TinyExcel](apps/office/TinyExcel.ml) | app | Excel (Microsoft, 1985, on the Macintosh) | The same spreadsheet six years later, with the mouse. | The same engine with 1985's answers: ranges, menus, a formula bar, Fill Down's relative references, a dependency graph. |

## Presentations

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyPowerPoint](apps/office/TinyPowerPoint.ml) | app | PowerPoint (Robert Gaskins and Dennis Austin, Forethought, 1987) | Slides, an outline, a master, a sorter, and the show. | A talk written before it is drawn: four views of one outline, and a master slide, a style sheet for pages. |

## Authoring and compound documents

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyHyperCard](apps/office/TinyHyperCard.ml) | app | HyperCard (Bill Atkinson, Apple, 1987) | Cards, buttons and scripts: programming for non-programmers. | Using and building as one: backgrounds as a database's columns, the message path, HyperTalk. |
| [TinyOpenDoc](apps/office/TinyOpenDoc.ml) | app | OpenDoc (Apple, IBM and CI Labs, 1994-97) | A document with no application: text, sheet, picture and drawing parts. | Parts edited in place, the menu bar becoming theirs; unknown parts kept byte for byte; sizes negotiated or scaled. |
| [TinyOffice](apps/office/TinyOffice.ml) | app | today's office suites (Microsoft 365, iWork, LibreOffice) | Choose a document, sheet, presentation, picture or drawing; each holds the others. | A start screen; every kind a host; objects floating anywhere, dragged and resized; text running round them, each object its own way; pages with headers, footers and page numbers; objects that move with the text; a chart linked to a sheet; a slide show; OLE's menu merging. |

## Graphics

`apps/graphics/`: the programs pictures are made with -- the two ways
of keeping one, as dots (TinyMacPaint) and as objects (TinyMacDraw),
both Apple's of 1984; the Amiga's dots, each a colour's number in a
palette that can turn (TinyDeluxePaint); and what the dots became with
24 bits each (TinyPhotoshop, over `libs/graphics/imaging`:
`plan_photoshop.md`).

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyMacPaint](apps/graphics/TinyMacPaint.ml) | app | MacPaint (Bill Atkinson, Apple, 1984) | Paint with dots: pencil, brush, shapes, bucket, and a selection to move. | The picture as bits: palettes of tools and patterns, the seed fill, marching ants, one undo per stroke. |
| [TinyMacDraw](apps/graphics/TinyMacDraw.ml) | app | MacDraw (Apple, 1984) | A picture made of objects you can select, move and group. | The picture as a list of objects: the order is the depth, handles, grouping, hollow shapes clicked through. |
| [TinyDeluxePaint](apps/graphics/TinyDeluxePaint.ml) | app | Deluxe Paint (Dan Silva, Electronic Arts, 1985, on the Amiga) | A picture of 32 colours whose waterfall and fire move by the palette alone: Tab, and the colours cycle. | Indexed colour, a dot the number of a colour in a palette of the Amiga's 12 bits; colour cycling, a range of the palette turning at each frame, the picture untouched; the brush cut from the picture, its transparent colour the background's; symmetry; its file IFF ILBM, chunks (RIFF's ancestor), bitplanes, ByteRun1 (MacPaint's PackBits) and the cycling ranges. |
| [TinyPhotoshop](apps/graphics/TinyPhotoshop.ml) | app | Photoshop 1.0 (Thomas and John Knoll, Adobe, 1990) | NASA's Blue Marble to retouch: select with the magic wand, adjust Levels or Hue/Saturation, filter, paint. | A photograph's 24 bits a dot and image processing as menus: point operations as tables of 256 (Levels with its histogram, Curves, Hue/Saturation in HSL), convolutions (blur, sharpen, emboss; the Gaussian separable; unsharp mask), Sobel's Find Edges, the median, interpolation (nearest, bilinear, bicubic); the selection as a mask a byte a dot, every operation applied through it, the wand a flood fill with a tolerance, feathering a blur of the mask; brushes as dabs with a hardness, a stroke's opacity uniform, the airbrush, rubber stamp and smudge; dialogs with a live preview; the picture drawn as tiles, only the changed ones sent again; and Photoshop 3.0's layers, each with its opacity and blend mode (Multiply, Screen, Overlay, Color...), flattened by Porter and Duff's over, a Layers palette, a photograph placed as a layer. |

## PIM

`apps/pim/`: the personal information managers -- the time, the
calendar, and the Palm Pilot's address book and to-do list to come
(`plan_pim.md`), over core's `Civil` and `Clock`.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyClock](apps/pim/TinyClock.ml) | app | the Alarm Clock (Apple, Macintosh, 1984), xclock (X Window System, mid-1980s) | The time here and in six cities, on hands or in a strip, and an alarm. | The wall clock's time, seconds since 1970 and the offset only the platform knows; hands keeping every fraction where the strip truncates; cities with no daylight saving, so one offset all year. |
| [TinyCalendar](apps/pim/TinyCalendar.ml) | app | cal (Unix, 1971), iCal (Apple, 2002) | A month or a week of events, dragged, stretched and repeated; September 1752 as England lived it. | The calendar computed, not looked up (a date a day number); the Julian calendar before the switch, cal's eleven missing days; a repetition as a rule (RRULE), its occurrences computed for the days shown; iCalendar files, read and written. |
| [TinyPalmPilot](apps/pim/TinyPalmPilot.ml) | app | the Pilot (Jeff Hawkins, Donna Dubinsky and Ed Colligan, Palm Computing, 1996) | A Date Book, an Address book, a To Do list and a Memo Pad behind four buttons, on a 160 x 160 screen. | Four things done and nothing else; no Save and no waiting, every change kept at once; written straight on the screen (an event on its hour, a name in the Look Up line); the data in the world's formats, iCalendar and vCard, for HotSync. |

## Game making

`apps/gamedev/`: the tools making what any game can use -- sprites,
maps, sounds -- written in the format of the playground layer that
reads them, in files the games embed at build time.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyAseprite](apps/gamedev/TinyAseprite.ml) | app | Aseprite (David Capello, 2001) | TinyMario's hero, four frames of pixel art: paint them, play them, export them. | The sprite as text: XPM files (a palette and rows of characters) that GIMP opens and a game embeds; the onion skin; the keys as the file's own characters. |
| [TinyTiled](apps/gamedev/TinyTiled.ml) | app | Tiled (Thorbjorn Lindeijer, 2008) | TinyMario's level, a cell at a time, the camera following the cursor along it. | The same file as a sprite, a character per cell (Tilemap.of_xpm); an editor that knows the level and not the game, so a cell is its palette color and its character; a minimap, and a keyboard's repeat written out. |

## Music

`apps/music/`: instruments played live, their voices built from
`audio/`'s blocks (`plan_synth_teaching.md`), and the tracker.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinySoundtracker](apps/music/TinySoundtracker.ml) | app | Ultimate Soundtracker (Karsten Obarski, Amiga, 1987) | A song as a grid of notes in four channels, typed on a piano of letters, played as it's edited. | The tracker: time going down the screen, a cell a note, an instrument and an effect in hex; a song that carries its own instruments (the MOD file, read and written by audio/formats/mod), played on the Amiga's four channels. |
| [TinyHammond](apps/music/TinyHammond.ml) | app | Hammond B-3 (1955) and its Leslie 122 (1965) | Nine drawbars pulled into a registration, chords on the computer's keys, the Leslie's horn and drum turning slow or fast. | Additive synthesis: sines added, from 91 tonewheels on gears (their harmonics tempered, a cent off); a voice per key, freed once silent; the percussion single-triggered; the Leslie modelled from its geometry, the Doppler made by a moving delay. |
| [TinyTB303](apps/music/TinyTB303.ml) | app | Roland TB-303 Bass Line (1981) | A 16-step bass line playing itself, its pattern a grid to click, its knobs turned while it plays: acid. | The sequencer in the audio clock, its steps at the sample, not the frame; the diode ladder, its poles coupled (the "18 dB"); the accent sweep, a capacitor's charge climbing over accents in a row; slides; parameter locks, Elektron's and the OP-XY's. |
| [TinyTR808](apps/music/TinyTR808.ml) | app | Roland TR-808 Rhythm Composer (1980) | The drum machine of electro, hip hop and house: eleven drums with their knobs, the 16 coloured step buttons, the whole pattern shown as a grid. | Drums synthesized, not sampled: a ringing filter struck (the kick's punch and sigh), six square waves through band-passes for the metal, the closed hat choking the open; the sequencer's hits at their samples. |
| [TinyOp1](apps/music/TinyOp1.ml) | app | OP-1 (Teenage Engineering, 2011) | The synthesizer and four-track studio the size of a keyboard: six engines, an envelope, an effect and an LFO, each on the same four coloured encoders, eight sounds, and a tape recording what you play over what it plays back. | Four encoders for everything, the screen saying what they turn; FM, supersaw, waveguide, pulse, phase distortion and bit crushing behind one interface; overdubbing on a tape. |
| [TinyOpxy](apps/music/TinyOpxy.ml) | app | OP-XY (Teenage Engineering, 2024) | The groovebox: eight tracks stepping together -- an 808 kit sampled, a bass, sampled electric piano chords, a lead -- their 16 steps, parameter locks, scenes switched at the bar, and the brain moving the song into another key and scale. | A step holds a chord and its knobs' locks, points the value glides through; the sampler, a recording played as an instrument; scenes on the bar; transposing by scale degree, not by interval. |
| [TinyReface](apps/music/TinyReface.ml) | app | Reface YC, CP, DX and CS (Yamaha, 2015) | Four small keyboards in one case: a switch for the combo organ, the electric piano, FM and virtual analog, each our original behind it -- the Hammond, the Rhodes, the DX7, the CS-80 -- on 37 mini keys. | A hub made thin by an interface: each voice gives its knobs by name, so a face is a list of labels and names, its controls drawn from the knobs' kinds; one voice playing at a time. |
| [TinyReBirth](apps/music/TinyReBirth.ml) | app | ReBirth RB-338 (Propellerhead, 1997) | The techno studio in a computer: two TB-303s, a TR-808 and a TR-909 playing together, each machine's pattern chosen, mixed, through distortion, delay, compressor and the pattern controlled filter. | One clock: four sample-exact sequencers started on the same sample stay together for ever; a hub over the machines' own voices; a filter whose cutoff is a pattern. |
| [TinyRhodes](apps/music/TinyRhodes.ml) | app | Fender Rhodes Mark I (1970) and its Suitcase; the Wurlitzer 200A (1974) and the Clavinet D6 (1971) | The electric pianos: a Rhodes to play softly or hard, its pickup's curve drawn with the tine's swing across it, the Suitcase's tremolo moving between its speakers. | Physical modelling: the tine as struck resonators (its modes at 1, 6.27, 17.55), read by a pickup whose curve makes the bark as the velocity grows; the Wurlitzer's reed and its capacitor; the Clavinet's string. |
| [TinyCS80](apps/music/TinyCS80.ml) | app | Yamaha CS-80 (1977) | Vangelis's synthesizer: two synthesizers per key, a chord held and one of its notes pressed harder to open it alone, a ribbon to bend them. | Polyphonic aftertouch, each key's pressure its own, into its filters and its level; two layers per voice; a filter envelope with an initial and an attack level; the ring modulator. |
| [TinyJuno](apps/music/TinyJuno.ml) | app | Roland Juno-106 (1984) | The polyphonic synthesizer everyone could afford: its sliders in their sections, six voices, and the chorus that makes one oscillator sound like an orchestra. | A DCO, in tune by a digital clock; one envelope for filter and amplifier (measured curves); one high-pass for all the voices; the chorus's two bucket-brigade lines, the right one's modulation inverted, at its measured rates. |
| [TinyDX7](apps/music/TinyDX7.ml) | app | Yamaha DX7 (1983) | The FM synthesizer of the 1980s: 145 parameters edited one at a time on its LCD, as the DX7 made you, beside what it hid -- the algorithm drawn as a graph lit as it plays, the six envelopes; its cartridges read. | FM with six operators: 32 algorithms, feedback from a sine to a sawtooth to noise; the envelopes' rates and levels in decibels; 16 voices, one stolen when they're all taken; velocity changing the timbre. |
| [TinyMinimoog](apps/music/TinyMinimoog.ml) | app | Minimoog Model D (Moog Music, 1970) | Three oscillators, the ladder filter and two contours, played from the computer's keys. | Subtractive synthesis in a fixed signal path read left to right; the ladder filter's resonance, overdriven by the mixer, played by the keyboard; low-note priority and legato; a patch as the panel's positions. |

## Media

`apps/media/`: programs for files of every kind -- sounds, tunes,
pictures, and videos as `graphics/videos/` comes
(`plan_video_teaching.md`).

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyMediaPlayer](apps/media/TinyMediaPlayer.ml) | app | Media Player (Microsoft, 1991), VLC (VideoLAN, 2001) | One player for every file this repository reads: recordings (WAV, MP2, MP3), tunes, modules, pictures, an animation, a video, and an .mpg with its sound. | A file's kind found from its bytes (magic numbers), not its name; each kind shown as what it is: a piano roll, a wave, a tracker's rows, a picture; a playlist, a scope and a spectrum. |

## Internet

`apps/internet/`: programs that talk to other computers, over
`networking/`, the servers they talk to small enough to run beside
them.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyIRC](apps/internet/TinyIRC.ml) | app | ircII (Michael Sandrof, 1989), for IRC (Jarkko Oikarinen, 1988) | Channels and nicks: join #tiny, type a line, everyone in it reads it. | A protocol a person can read, a line of text a message (Irc); its own server (networking/ircd), over WebSocket so that a browser joins too; /raw to type the protocol itself. |
| [TinyMosaic](apps/internet/TinyMosaic.ml) | app | NCSA Mosaic (Marc Andreessen and Eric Bina, 1993) | A web browser: a page fetched, read and drawn, its links followed. | Built stage by stage as every browser's pipeline (plan_browser_teaching.md); all of them now, in a first form: a page's bytes fetched (a built-in about: page, or http://) and decoded, its encoding decided (web's Charset), cut into tokens (Html_lexer, the WHATWG's state machine), built into a tree, its mistakes repaired (Html_tree, over MMM's DTD-as-data), given Mosaic's looks (Looks), laid out in blocks and lines (Html_layout; lines broken greedily or by Knuth and Plass, w) and drawn by Hershey's pen (Stroke_text); pictures in the text, fetched one after the other as Mosaic did and decoded by our own GIF, PNG and JPEG readers; fill-out forms in Motif's look, sent with GET or POST, answered by the browser (about:echo) or by tiny_httpd's CGI; and back, a click to its link (Hit), a history of two stacks, visited links purple; http:// by our own client, https:// by our own TLS 1.3 (networking/tls, plan_tls.md), and its own web server, networking/httpd's tiny_httpd; a view per stage, one the 1991 Line Mode Browser's, links followed by their number. |
| [TinyNetscape](apps/internet/TinyNetscape.ml) | app | Netscape Navigator 1.0 (Marc Andreessen and the Mosaic Communications team, 1994) | Mosaic's successor: a toolbar, a Location field to type in, pictures arriving four at a time. | The same engine as TinyMosaic (appkits/browser), in Netscape's window: the Location field typed into, the toolbar's buttons, the status bar's progress and the "N" whose meteors fall while a page loads; pictures fetched over four connections at once instead of one after the other, the page readable before they arrive, Stop and Images (images=off) to not wait; and threads for what still blocks, a host's name resolved and the https:// fetches on a pool of four (Worker), the window going on meanwhile (threads=off: Mosaic's frozen window); and Netscape's extensions to HTML, marked as such in the tokens and the tree (not HTML 2.0's) and honoured here, not by TinyMosaic: colours, fonts, centring, rules, pictures the text flows around (floats), tables, their columns as wide as their cells need (each cell laid out twice to measure it); and CSS1's style sheets, the cascade by specificity over the browser's own looks, c to see a page without them (plan_browser_teaching.md). |
| [TinyFirefox](apps/internet/TinyFirefox.ml) | app | Firefox 1.0 (the Mozilla Foundation, 2004), and Firebug (Joe Hewitt, 2006) | A browser that runs the page's own program: click, type, and watch the console and the page's tree change. | The page's JavaScript run by an engine written from scratch (libs/languages/javascript: a lexer, a Pratt parser -- and why not yacc --, a tree walker with closures and JavaScript's coercions) over the page's tree (Browser_script: the DOM, a click bubbling to the document, timers on the frame clock, one reflow per task), all of TinyNetscape's looks (extensions, tables, CSS), and a panel after Firebug: the console, with a command line into the page's world, and the tree as the scripts leave it; its home page leads to a counter, a to-do list, a stopwatch and tic-tac-toe, each a small program (plan_tiny_firefox.md). |
| [TinyChrome](apps/internet/TinyChrome.ml) | app | Google Chrome (2008) | A small browser for the real web: Hacker News, Wikipedia (its search from the omnibox), Google's home page, GitHub, drawn mostly as their authors meant, in tabs, with developer tools; Hacker News' own script run, a thread folded; TinyTube, a video site of our own. | CSS's own engine, from scratch (libs/web): a cascade with @media, custom properties and var(), calc(), the attributes' hints, quirks mode, a record of computed values per element, and CSS 2.1's box model -- margins, borders and paddings, auto margins centring, collapsing margins, floats, inline-blocks shrunk to fit, relative and absolute positioning, tables and lists, and flexbox -- and SVG drawn by our own rasterizer (files, inline <svg>, backgrounds, masks) -- an ES5 engine (prototypes, new, call and apply, var hoisted, ==, regular expressions by a backtracking matcher of its own) running a few sites' scripts, Hacker News' comment folding with its own hn.js; <video> and <audio> played by our own readers (MPEG-1 and MP2, AVI, FLC, Y4M, GIF, MP3), in about:tube; and developer tools after Chrome's: an element inspected, each of its winning declarations with the rule and the sheet it came from, the network -- over the browser's own style sheet written in CSS (ua.css), the pages' linked sheets and their @imports fetched with their pictures; its home page, about:chrome, a page in today's CSS (plan_tiny_chrome.md). |
| [TinyEudora](apps/internet/TinyEudora.ml) | app | Eudora (Steve Dorner, University of Illinois, 1988) | Mailboxes and a message: the list with Eudora's columns, a message read under it, its headers all shown with b, its attachment saved; a reply written, queued in Out, and sent; Check Mail; your own Gmail. | Mail as bytes (networking/mail): a message as RFC 822's header fields, folded lines, addresses and dates (Mail); MIME's parts, base64, quoted-printable and encoded words, read and written (Mime); a mailbox as one mbox file, its ">From" lines quoted the reversible way (Mbox); a built-in mailbox of messages written for what they show -- a thread, a forged sender given away by the envelope, a digest, a picture attached; composing as Eudora did on a modem, a message queued rather than sent, a reply quoted and threaded by In-Reply-To and References, nicknames as vCards, the mailboxes kept as mbox files in the store; SMTP and POP3 as pure state machines fed the server's lines (Smtp, Pop3), and its own server, networking/maild's tiny_maild (Mail_server: not a relay, deletions at QUIT only), over WebSocket so that a browser checks mail too and over plain TCP so that telnet and any mail client do; and Gmail's servers, natively, the same machines through a TLS tunnel (openssl's s_client, until TLS is ours), SMTP logging in (AUTH PLAIN) (plan_tiny_eudora.md). |

## System

`apps/system/`: the programs a computer is used through before any
application -- so far the terminal and its shell, over the
playground's Teletype way and `libs/terminal/` (`plan_terminal.md`).

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyTerminal](apps/system/TinyTerminal.ml) | app | the VT100 (DEC, 1978), the Unix shell (Ken Thompson, 1971; Stephen Bourne, 1979) | A screen of 80 by 24 and a prompt: type a command, run Hangman, stop it with Control-C; basic, then RUN MANDEL. | A terminal as three machines: the screen reading bytes and escape sequences (Vt), the tty editing the line (Line_discipline), the shell reading it and running programs written as conversations (Teletype), a spawn being Unix's fork, exec and wait, and Control-C interrupting only the program the shell waits for. |

## Programming tools

`apps/devtools/`: the programs programs are written with -- so far a
BASIC as a home computer had it, over `libs/languages/basic`
(`plan_terminal.md`, section 5), and the two text editors, full-screen
programs of the terminal over `appkits/editor` (section 6), Emacs's
Lisp over `libs/languages/lisp`, and Turbo Pascal's IDE over
`libs/languages/pascal`.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyBasic](apps/devtools/TinyBasic.ml) | app | Tiny BASIC (Dennis Allison, 1975; Li-Chen Wang, Palo Alto, 1976) and Applesoft (Microsoft, 1978), on the Apple II's screen (Steve Wozniak's Integer BASIC, 1977) | Type a program a numbered line at a time, LIST it, RUN it; FP for floating point; Guess the Number is typed in already. | The prompt as the whole environment, editor, calculator and shell; a recursive-descent parser reading characters, finding keywords inside names as Microsoft's cruncher did (SCORE is SC OR E); two arithmetics, 16-bit integers wrapping and Applesoft's floating point; strings, arrays, FOR loops, DATA; the interpreter a Teletype conversation, INPUT's continuation the rest of the program, a step per statement so that 10 GOTO 10 runs and Control-C breaks it; a disk of our own listings, Apple DOS's CATALOG and RUN name: Guess checked against Tty_guess, Bagels, 23 Matches, Animal (a program that learns), Lunar, the Mandelbrot set checked against the same loop in OCaml, Sierpinski's triangle from Pascal's, each crediting its sources in REMs. |
| [TinyVi](apps/devtools/TinyVi.ml) | app | vi (Bill Joy, Berkeley, 1976), on ex and ed (Ken Thompson, 1969) | A screen editor with modes: keys are commands until i, text until Escape; README says them. | Modes; commands as sentences, an operator and a motion with counts that multiply (d2w, 3dd, cw), parsed by a grammar; "." repeating the last change, its keys typed again; vi's one level of undo, u again redoing; ex under the colon (:w, :e, :%s/old/new/g, :12); the lines an array of strings, ed's view of a file; the same program in a real terminal, curses' bytes counted. |
| [TinyEmacs](apps/devtools/TinyEmacs.ml) | app | GNU Emacs (Richard Stallman, 1985), after EMACS on TECO (1976) | An editor that is a Lisp machine: type (+ 1 2) then C-j in *scratch*, or C-h k and a key to see what it runs. | Every key a Lisp function looked up in a keymap that is a Lisp variable, most commands written in that Lisp, .emacs loaded at start and changeable while it runs; a small Emacs Lisp of our own, dynamically scoped with its specpdl, macros, condition-case; the command loop reading (interactive) specs through the minibuffer, TAB completing; the text in a gap buffer, persistent; the undo list, undo undoable; the kill ring, C-k's appended; incremental search; the same program in a real terminal. |
| [TinyTurboPascal](apps/devtools/TinyTurboPascal.ml) | app | Turbo Pascal (Anders Hejlsberg, Borland, 1983), in the look of Turbo Pascal 7 (1992) | Wirth's eight queens in a blue window: F9 compiles, Ctrl-F9 runs, the error in a red bar with the cursor on it. | The editor, the compiler and the program one keystroke apart; the PC's text screen, its sixteen CGA colours and its box-drawing characters, menus and dialogs with shadows drawn cell by cell; Pascal compiled in one pass to P-code, Wirth's Pascal-P scheme (libs/languages/pascal), run by the P-machine on the user screen, a run-time error bringing the cursor to its line; the P-code of the cursor's line shown, static links and array bounds checks readable in it; and Turbo's debugger: F7 and F8 a line at a time, the execution bar, breakpoints, watches, the call stack with each frame's static and dynamic links, over the compiler's debug information and a P-machine that pauses. |
