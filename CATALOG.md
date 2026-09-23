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
| [TinyMarioWorld](games/platform/TinyMarioWorld.ml) | 2D | Super Mario World (Takashi Tezuka and Shigeru Miyamoto, Nintendo, 1990) | Cape Mario on hills, in the sky, and on a map with a secret exit. | Slopes felt by sensors, and the slide; the cape's flight as speed traded for height; the world map as a graph with secret exits. |
| [TinySonic](games/platform/TinySonic.ml) | 2D | Sonic the Hedgehog (Yuji Naka, Hirokazu Yasuhara, Naoto Ohshima, Sega, 1991) | Keep your speed, take the hill, go round the loop. | Speed along a surface: shaped tiles, one ground speed, gravity as a slope factor, the loop as ordinary tiles; the spindash. |
| [TinyMario64](games/platform/TinyMario64.ml) | 3D | Super Mario 64 (Shigeru Miyamoto, Nintendo, 1996) | Floating platforms and five stars, running and jumping in 3D. | The platformer in 3D: controls relative to the camera, a camera you steer, a shadow to judge landings. |
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
| [TinyTron](games/arcade/TinyTron.ml) | 2D | Tron's light cycles (Bally Midway, 1982) | Two cycles leaving walls of light: the first to crash loses. | A computer that goes where there is most room (a flood fill); the rules in a kit, shared with a 3D view. |
| [TinyTron3d](games/arcade/TinyTron3d.ml) | 3D | TinyTron | The same light cycles, the trails as walls you can look at from behind. | The Elm architecture's promise: the same model, another view, with four cameras. |
| [TinyBomberman](games/arcade/TinyBomberman.ml) | 2D | Bomberman (Hudson Soft, 1983) | Pillars, soft blocks, bombs exploding in a cross, and balloons. | Fire spreading tile by tile, and chain reactions; random moves that replay. |
| [TinyBoomerangFu](games/arcade/TinyBoomerangFu.ml) | 3D | Boomerang Fu (Cranky Watermelon, 2020) | Four foods in an arena, one boomerang each, one hit kills. | Your only weapon leaves your hand: a return arc homing on its owner; a fixed party-game camera, and shadows to read height. |
| [TinySoldat](games/arcade/TinySoldat.ml) | 2D | Soldat (Michał Marcinkowski, 2002) | A side-view deathmatch on jet boots against two bots, and ragdolls. | The 2D physics engine's capstone: a stacking world, swept bullets against tunnelling, grenade blasts, particle ragdolls. |
| [TinyMarbleMadness](games/arcade/TinyMarbleMadness.ml) | 3D | Marble Madness (Mark Cerny, Atari Games, 1984) | Roll a marble down a course floating in space, against the clock. | A ball rolling on a height map (5/7 of g sin a), falls that break it, collisions between balls; a far, nearly isometric camera. |
| [TinyCameltry](games/arcade/TinyCameltry.ml) | 2D | Cameltry (Taito, 1989) | You don't move the ball, you turn the maze. | Turning the maze is turning gravity; rotation, so the ball rolls instead of sliding (`rotation=off` to compare). |
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
| [TinyBlockout](games/puzzle/TinyBlockout.ml) | 3D | BlockOut (P.Z.Karen Co., California Dreams, 1989) | Tetris down a well, seen from above, with pieces turning in 3D. | Tetris with one more index; depth cues (shading, the landing ring); quarter turns of polycubes in integers. |
| [TinyLemmings](games/puzzle/TinyLemmings.ml) | 2D | Lemmings (DMA Design, Psygnosis, 1991) | Creatures walk mindlessly; give them jobs to save them. | Indirect control; the terrain as a bitmap in the model, dug and built; creatures as tiny state machines reading the pixels. |
| [TinyIncredibleMachine](games/puzzle/TinyIncredibleMachine.ml) | 2D | The Incredible Machine (Kevin Ryan, Jeff Tunnell, Dynamix / Sierra, 1993) | Build a Rube Goldberg machine from a bin of parts, then watch it put a ball in a basket. | Building, then watching: physics that must be deterministic; joints (a seesaw's pin, a pulley's ropes); parts acting on each other, a switch turning on a fan. |
| [TinyPuzzleBobble](games/puzzle/TinyPuzzleBobble.ml) | 2D | Puzzle Bobble (Taito, 1994) | Shoot a bubble off the walls; three of a colour pop. | A hexagonal grid stored as offset rows; snapping to it; two flood fills (the pop, then the fall); the aiming guide as the shot flown ahead. |
| [TinyPortal2D](games/puzzle/TinyPortal2D.ml) | 2D | Portal: The Flash Version (We Create Stuff, 2007) | A platformer where the way through is a pair of holes you shoot. | A portal is a rigid transform: position and velocity rotated, never scaled -- the fling -- on the physics layer. |
| [TinyPortal](games/puzzle/TinyPortal.ml) | 3D | Portal (Valve, 2007) | One test chamber, a cube on a ledge, and a portal gun: fling yourself. | The portal in 3D: a rigid motion carrying orientation and spin; seeing through by taking the chamber through it and clipping, no stencil. |
| [TinyCrush](games/puzzle/TinyCrush.ml) | 2.5D | Crush (Zoë Mode, Sega, 2007) | Crush a level flat along your line of sight, and walk across what was far apart in depth. | The projection as the puzzle: a stack of slices, crushed into their union; the same platformer on either plane; the crush drawn as a cabinet projection losing its depth. |
| [TinyCrush3d](games/puzzle/TinyCrush3d.ml) | 3D | Crush3D (Zoë Mode, Sega, 2012) | TinyCrush with real cubes: crush, and watch the level fold onto your plane as the camera turns straight on. | The same rules drawn twice (`gamekits/crush`): the crush as the slices sliding in depth and the camera swinging from three-quarters to straight on; why the crushed level is drawn as its plane (z-fighting). |
| [TinySlingshot](games/puzzle/TinySlingshot.ml) | 2D | Angry Birds (Rovio, 2009) | Pull back the slingshot and knock down a tower of physics bodies. | The whole 2D engine at once: a solver for stacking, rotation, friction; targets broken by impulse; the arc predicted by the engine's own steps. |
| [TinyFez](games/puzzle/TinyFez.ml) | 2.5D | Fez (Phil Fish, Polytron, 2012) | A flat world you can turn a quarter at a time: what lines up on the screen is next to each other. | Depth thrown away, four ways: any top a floor, only what is at your depth a wall; the turn drawn as each cube's two faces squashed to the cosine and the sine; pixel art from XPM files. |
| [TinyPerspective](games/puzzle/TinyPerspective.ml) | 3D | Perspective (DigiPen, a student game, 2012) | Place a camera in a 3D world, then run a 2D platformer on the picture it takes. | The projection with perspective: far is small, near is big, so the camera builds the level; the runner's world the boxes' silhouettes filled into a grid of the screen; the camera turning round the point under his feet, found by undoing the projection. |
| [TinyMonumentValley](games/puzzle/TinyMonumentValley.ml) | 3D | Monument Valley (ustwo, 2014) | Walk over architecture that cannot exist, trusting the picture. | An orthographic camera's ambiguity as the rule: blocks that look adjacent are walkable; a search over the picture's graph. |
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
| [TinyZelda](games/adventure/TinyZelda.ml) | 2D | The Legend of Zelda (Shigeru Miyamoto and Takashi Tezuka, Nintendo, 1986) | An overworld of screens, a sword, a key, and a dungeon. | A land to explore in any order: rooms that slide in, an inventory as locks and keys, monsters wandering on a generator in the model. |
| [TinyManiacMansion](games/adventure/TinyManiacMansion.ml) | 2D | Maniac Mansion (Ron Gilbert and Gary Winnick, Lucasfilm Games, 1987) | Past the purple tentacle and into Dr. Fred's lab, a verb and a click at a time. | The parser become a menu: SCUMM's verbs and sentence line, on the same world model as TinyZork; walkboxes, the floor as boxes and a path through them; a script per object. |
| [TinyAloneInTheDark](games/adventure/TinyAloneInTheDark.ml) | 3D | Alone in the Dark (Frédérick Raynal, Infogrames, 1992) | A house at night seen by fixed cameras, a locked study, and something in the corridor. | Fixed cameras that cut between rooms (cinematography); tank controls, the only ones that survive a cut; a doorway's hysteresis. |
| [TinyTombRaider](games/adventure/TinyTombRaider.ml) | 3D | Tomb Raider (Toby Gard, Core Design, 1996) | A raider in a stone tomb, an idol on a pedestal, and a boulder. | Committed moves of fixed length, and a level measured in them (no collision code); textured stone from one atlas page. |
| [TinyGTA](games/adventure/TinyGTA.ml) | 2.5D | Grand Theft Auto (DMA Design, 1997) | A city from above: any car is yours, phones ring with work, and the police count your crimes in stars. | The open world: a city that runs without you (traffic and people on a road graph), any car taken, the wanted level; the camera rising with speed; buildings leaning out of the screen. |
| [TinyMetalGearSolid](games/adventure/TinyMetalGearSolid.ml) | 2D | Metal Gear Solid (Hideo Kojima, Konami, 1998), after Metal Gear (1987) | Across an enemy base to the elevator, unseen, with a radar, a knock and a cardboard box. | Stealth: cones of vision stopped by walls, shown only on the radar, which jams in an alert; guards as a state machine written as data (patrol, "?", "!", evasion), walking by A*. |
| [TinyZeldaOcarina](games/adventure/TinyZeldaOcarina.ml) | 3D | The Legend of Zelda: Ocarina of Time (Nintendo EAD, 1998) | Across Hyrule Field by day and by night to a temple, and a Stalfos whose shield must be got round. | Z-targeting: the lock-on, the controls re-read around the enemy held (sideways is an orbit) and the camera over the shoulder on the line through the two; an enemy that blocks and opens after its telegraphed chop. The field as a hub: loading zones to places with their own coordinates, a clock that runs only there, a night with its own monsters, the light baked in. |

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
| [TinyMarioKart](games/racing/TinyMarioKart.ml) | 2.5D | Super Mario Kart (Nintendo, 1992) | A kart race on a Mode 7 floor, alone or two in a split screen. | Mode 7: the floor sampled a screen row at a time; karts as billboards seen from four angles; the split screen. |
| [TinyVirtuaRacing](games/racing/TinyVirtuaRacing.ml) | 3D | Virtua Racing (Yu Suzuki, Sega AM2, 1992) | TinyOutRun's course in flat-shaded polygons, to the GOAL arch. | The first great polygon racer: the road as a banked ribbon in space, four views, hills hidden by the z-buffer. |
| [TinyMarioKart64](games/racing/TinyMarioKart64.ml) | 3D | Mario Kart 64 (Nintendo, 1996) | Three laps against seven karts, item boxes, and a four-player battle. | Polygons for the world and sprites for the karts; four players on one screen; a battle arena with two heights. |

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
| [TinyCoreWar](games/programming/TinyCoreWar.ml) | 2D | Core War (A. K. Dewdney and D. G. Jones, 1984) | Write two programs in its editor, then watch them fight in one circular memory, each trying to make the other execute a DAT. | A virtual machine and its assembler, a page each (Redcode, MARS), the assembler's mistakes shown on their lines; the classic warriors to start from -- the Imp, the Dwarf, the Mice -- and why each beats the next. |

# Apps

In `apps/office/`, the sections below up to PIM being its kinds of
program, and in `apps/gamedev/`, the tools making what any game can
use (a genre's level editor is with its games instead, as
TinySokobanEd is: see `games/README-tools.md`). The other categories
are waiting for their first app, each with a dune file saying what it
might hold: `apps/music/`, `apps/internet/`, `apps/devtools/`,
`apps/graphics/`, `apps/system/`.

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

## Graphics

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyMacPaint](apps/office/TinyMacPaint.ml) | app | MacPaint (Bill Atkinson, Apple, 1984) | Paint with dots: pencil, brush, shapes, bucket, and a selection to move. | The picture as bits: palettes of tools and patterns, the seed fill, marching ants, one undo per stroke. |
| [TinyMacDraw](apps/office/TinyMacDraw.ml) | app | MacDraw (Apple, 1984) | A picture made of objects you can select, move and group. | The picture as a list of objects: the order is the depth, handles, grouping, hollow shapes clicked through. |

## Authoring and compound documents

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyHyperCard](apps/office/TinyHyperCard.ml) | app | HyperCard (Bill Atkinson, Apple, 1987) | Cards, buttons and scripts: programming for non-programmers. | Using and building as one: backgrounds as a database's columns, the message path, HyperTalk. |
| [TinyOpenDoc](apps/office/TinyOpenDoc.ml) | app | OpenDoc (Apple, IBM and CI Labs, 1994-97) | A document with no application: text, sheet, picture and drawing parts. | Parts edited in place, the menu bar becoming theirs; unknown parts kept byte for byte; sizes negotiated or scaled. |
| [TinyOffice](apps/office/TinyOffice.ml) | app | today's office suites (Microsoft 365, iWork, LibreOffice) | Choose a document, sheet, presentation, picture or drawing; each holds the others. | A start screen; every kind a host; objects floating anywhere, dragged and resized; text running round them, each object its own way; pages with headers, footers and page numbers; objects that move with the text; a chart linked to a sheet; a slide show; OLE's menu merging. |

## PIM

None yet (a calendar, an address book, a to-do list would go here).

## Game making

`apps/gamedev/`: the tools making what any game can use -- sprites,
maps, sounds -- written in the format of the playground layer that
reads them, in files the games embed at build time.

| Program | Dir | After | In one line | What it brought |
|---|---|---|---|---|
| [TinyAseprite](apps/gamedev/TinyAseprite.ml) | app | Aseprite (David Capello, 2001) | TinyMario's hero, four frames of pixel art: paint them, play them, export them. | The sprite as text: XPM files (a palette and rows of characters) that GIMP opens and a game embeds; the onion skin; the keys as the file's own characters. |
| [TinyTiled](apps/gamedev/TinyTiled.ml) | app | Tiled (Thorbjorn Lindeijer, 2008) | TinyMario's level, a cell at a time, the camera following the cursor along it. | The same file as a sprite, a character per cell (Tilemap.of_xpm); an editor that knows the level and not the game, so a cell is its palette color and its character; a minimap, and a keyboard's repeat written out. |
