# Plan: the classic games, genre by genre, and the kits they need

## Context

`games/` started with five games (Pong, Snake, Tetris, Asteroid,
TinyMario), `games3d/` with two (StarCollector3d,
TinyMinecraft). Each was written from scratch on top of `Playground`
alone. But games come in **genres**, and the games of a genre share
most of their machinery: every shoot 'em up has bullets, enemy waves
and paths; every racing game a track, laps and opponents. Writing the
second game of a genre should reuse what the first one needed.

This plan goes through video game history genre by genre: for each,
the games that invented it, a **toy version** of the classic to write
(the way `plan_tiny_minecraft.md` turned Minecraft into a few hundred
lines), and the **layers** on top of the playground that a series of
such games needs. (`plan_games3d.md`, later, will do the same for 3D:
first-person shooters, flight simulators, 3D platformers, kart racing.)

The first two layers exist: `playground/Camera2d.mli` and
`playground/Tilemap.mli`, both used by `games/TinyMario.ml`. They
are the model for the others: not in `Playground.mli` (which stays
Evan's API), built only from its shapes (so every backend gets them for
free), with the history of the games that needed them and related work
in their `.mli`, and unit tests of their worked examples.

## Principles

- **Toys, not clones**: a toy captures what made the classic matter
  (its one mechanic, its feel) in a few hundred lines, with shapes
  rather than sprites where that's enough. No copyrighted assets or
  names in the files (TinyInvaders, not Space Invaders), but the
  history in the comments says which game it pays homage to.
- **A layer when a second game needs it**: the first game of a genre
  keeps its helpers in its own file; they move to a layer when the
  second game wants them (the rule of three, relaxed to two). That's
  how `TinyMario.ml`'s one-pixel-at-a-time `move_by` will become part
  of a platformer layer: when TinyDonkeyKong needs it too.
- **Teaching**: the same rules as `graphics/` and `physics/` -- one idea
  per function, ASCII diagrams, references (papers, talks, and here
  also the games themselves and their designers), worked examples
  checked by tests, golden frames for every game (with `seed=1`).
- **Game culture**: every genre section, every game's header, says
  where the idea comes from, who made it, and why it mattered. The
  playground is also a small museum.

## What to call them: kits

Generic layers (`Camera2d`, `Tilemap`, a future `Sprite`, `Scene`)
serve all genres; genre layers serve one. For the latter, video game
history has a name: the **construction kits** of the 1980s, each a
genre's engine plus an editor -- Bill Budge's Pinball Construction Set
(1983), the Adventure Construction Set (Stuart Smith, 1984), Sensible
Software's Shoot-'Em-Up Construction Kit (1987), later RPG Maker
(1992). Next to them, the game programming languages of the home
computers: STOS Basic (François Lionet, Constantin Sotiropoulos, 1988)
on the Atari ST and AMOS (1990) on the Amiga, BASICs with sprite, map
and music editors, and commands for sprites, collisions, scrolling --
the closest ancestors of this playground. The industry's own word is **engine** (the SCUMM engine, made
for Maniac Mansion (1987), ran all of LucasArts' point-and-click
adventures; Doom's engine was licensed for a dozen shooters). "Kit"
says the smaller, teaching-sized thing better:

```
  games       TinyMario  TinyDK   TinyGradius TinyTouhou  TinyOutRun  TinyPacman
                  |        |           |          |           |           |
  genre kits  platformer kit      shmup kit (Bullets,     racing kit  maze kit
              (Actor)             Waves, Paths)           (Road)      (Grid_move,
                  |                    |                      |        Ghosts)
  layers      Camera2d  Tilemap  Sprite  Scene  Hitbox  (physics/, audio/,
                                                          network/ plans)
                  |        |        |       |       |
  base        Playground: shapes, computer, game
```

Where: generic layers in `playground/` (in the `elm_playground`
library, like `Camera2d`); each kit in `gamekits/<genre>/`, its own
library (`elm_kit_shmup`, ...), depending on `elm_playground` only, so
a kit is visibly optional -- and the playground's own `.mli` never
grows. (The physics plan's `physics/` is a layer of the same kind, and
its genre games -- Spacewar!, Slingshot -- are in the physics plan.)

## References: which games are "classic"

- Wikipedia, [List of video game genres](https://en.wikipedia.org/wiki/List_of_video_game_genres)
  and [Video game genre](https://en.wikipedia.org/wiki/Video_game_genre):
  the genres below, and their sub-genres;
- Wikipedia, [List of video games considered the best](https://en.wikipedia.org/wiki/List_of_video_games_considered_the_best),
  [Golden age of arcade video games](https://en.wikipedia.org/wiki/Golden_age_of_arcade_video_games),
  [History of video games](https://en.wikipedia.org/wiki/History_of_video_games);
- The Strong museum's World Video Game Hall of Fame (since 2015): a
  few inductees a year, each with why it matters;
- books: Steven L. Kent, *The Ultimate History of Video Games* (2001);
  Tristan Donovan, *Replay: The History of Video Games* (2010); Nick
  Montfort and Ian Bogost, *Racing the Beam* (2009, the Atari 2600:
  how hardware shaped the games); Steve Swink, *Game Feel* (2008);
  Robert Nystrom, *Game Programming Patterns* (2014).

## Sources to adapt: games in Elm, Haskell, OCaml

Before writing a toy from scratch, look for an existing version in a
functional language: its model/update/view split is usually already
there, and porting it is mostly syntax (how `games/Tetris.ml` came from
elm-flatris).
Check each one's license, and credit it in the game's header, like
`examples/Mario.ml` credits elm-lang.org.

Catalogs:
- [rofrol/elm-games](https://github.com/rofrol/elm-games), "all Elm
  games (hopefully)", including the Elm Game Jam entries;
- the HaskellWiki's [Games](https://wiki.haskell.org/Applications_and_libraries/Games)
  page;
- [Open Source Game Clones](https://osgameclones.com), one page per
  classic (e.g. its Pac-Man page) listing clones with their language;
- Wikipedia, [List of open-source video games](https://en.wikipedia.org/wiki/List_of_open-source_video_games).

Known ones, by genre (found, not yet read):

| Genre | Game | Language | Notes |
|---|---|---|---|
| ball and paddle | [haskanoid](https://github.com/ivanperez-keera/haskanoid) | Haskell (Yampa, SDL) | Breakout, Ivan Perez |
| fixed shooter | [SpaceInvaders](https://github.com/ivanperez-keera/SpaceInvaders) | Haskell (Yampa) | from the paper below |
| fixed shooter | Antony Courtney, Henrik Nilsson, John Peterson, "The Yampa Arcade" (Haskell Workshop, 2003) | Haskell | a Space Invaders explained, to teach functional reactive programming |
| shmup | Monadius | Haskell | Gradius, with its power-up bar |
| shmup | elm-shooter | Elm | a side-scrolling shooter (in rofrol/elm-games) |
| puzzle | [elm-flatris](https://github.com/w0rm/elm-flatris) | Elm | Tetris, Andrey Kuzmin (w0rm); already ported: `games/Tetris.ml` |
| platformer | [MariOCaml](https://github.com/mahsu/MariOCaml) | OCaml (js_of_ocaml) | Super Mario Bros., procedurally generated levels; a BuckleScript port by Hongbo Zhang |
| platformer | Nikki and the Robots, Raincat | Haskell | puzzle-platformers |
| maze / pathing | [elm-street-404](https://github.com/w0rm/elm-street-404) | Elm (WebGL) | a delivery game, made at Zalando in five days (2016) |
| arcade | Down the River | Elm | Frogger (in rofrol/elm-games) |
| tower defense | Safe Tea | Elm | Elm Game Jam 2018 (in rofrol/elm-games) |
| roguelike | LambdaHack | Haskell | a roguelike engine (and its game, Allure of the Stars) |

Genres with nothing found yet in these languages (racing, beat 'em
ups, fighting) are the ones where our toys would be new.

Not functional, but the closest thing to this project's goal in book
form: **Code the Classics**, volumes 1 and 2 (Raspberry Pi Press,
2019 and later; free PDFs): complete remakes of classic games in
Python with Pygame Zero, each with its full source, explained, and
the history of the original -- e.g. volume 1's Boing! (Pong), Cavern
(Bubble Bobble), Infinite Bunner (Frogger), Myriapod (Centipede) and
Soccer (Sensible Soccer). A model for the toys' headers (the history)
and for the course (`plan_teaching_other.md`); the games are small
enough to port. (From memory: the titles and contents to check.)

## The genres

Ordered roughly by history. For each: the inventors, the toys, the
kit, and what it teaches.

### 1. Ball and paddle

Tennis for Two (William Higinbotham, 1958, on an oscilloscope), Pong
(Atari, 1972, done: `games/Pong.ml`, and `games/TinyPong.ml` on the
physics plan's `bounce`), Breakout (Atari, 1976, Nolan
Bushnell and Steve Wozniak), Arkanoid (Taito, 1986: power-ups).

- **Toy**: TinyBreakout (DONE: `games/TinyBreakout.ml`) -- the best
  first game to write after Pong. Bricks are a `Tilemap` (a brick two
  tiles, "Rr"), the original's rules: speed-ups, the paddle halved
  after breaking through, two walls; the paddle's angle a rule, not
  physics (TinyPong shows the physics way).
- **Kit**: none.

### 1b. One-button games and endless runners

Helicopter games (SFCave, Helicopter Game, 2000s), Canabalt (Adam
Saltsman, 2009), Flappy Bird (Dong Nguyen, 2013).

- **Toy**: TinyFlappyBird (DONE: `games/TinyFlappyBird.ml`): a flap
  *sets* the velocity; an endless world, pipes made at the screen's
  right edge and dropped at its left; the pipes' heights from an LFSR
  whose state is in the model (Pitfall!'s and River Raid's trick), so
  the whole run replays from its seed.
- **Kit**: none; `Camera2d` (look_at, parallax).

### 2. Snake and the grid

Blockade (Gremlin, 1976), Tron's light cycles (Bally Midway, 1982),
Snake on Nokia phones (1997; done: `games/Snake.ml`).

- **Toy**: TinyTron (DONE: `games/TinyTron.ml`, two players on one
  keyboard, or against the computer choosing the way with the most
  room, a flood fill; and `games3d/TinyTron3d.ml`, the same
  model in 3D, four views: both on the light cycles kit,
  `gamekits/lightcycles/`, only their views differing).

### 3. Fixed shooters

Space Invaders (Tomohiro Nishikado, Taito, 1978: the game that caused
a coin shortage in Japan, legend says), Galaxian (Namco, 1979:
colors, diving aliens), Galaga (Namco, 1981), Centipede (Atari, 1981:
the trackball).

- **Toys**: TinyInvaders (DONE: `games/TinyInvaders.ml`, with
  `Sprite`, `Scene2d`, and bunkers as eroding `Tilemap`s; the formation marches, speeds up as it
  shrinks -- an accident of the hardware, which drew faster with fewer
  aliens, kept as the design), TinyGalaga (DONE: `games/TinyGalaga.ml`,
  the waves flying in and the dives along Catmull-Rom splines, moved
  along by arc length; the formation breathing; bosses taking two hits;
  a robot in `tests/games/` clears stage 1), TinyMissileCommand (DONE:
  `games/TinyMissileCommand.ml`, Missile Command, Dave Theurer, Atari,
  1980: aiming at a point, the counter-missile exploding there; the
  explosions growing and shrinking, the chain reaction; MIRVs; its
  missiles `Shots`; a robot in the tests aims at the intercept and
  survives two waves).
- **Kit** (the start of the shmup kit, DONE: `gamekits/shmup/`): `Shots`
  (the player's and the enemies' bullets, created, moved, removed when
  off-screen or on a hit), and `Path` (TinyGalaga's Catmull-Rom curves,
  flown by arc length, for section 5's `Paths` too). Not `Formation`:
  TinyInvaders' marches one alien per frame, TinyGalaga's breathes,
  little in common yet.

### 4. Multi-directional shooters

Asteroids (Atari, 1979; done: `games/Asteroid.ml`), Robotron: 2084
(Eugene Jarvis, 1982: two joysticks, the twin-stick), Geometry Wars
(2003).

- **Toy**: TinyRobotron (DONE: `games/TinyRobotron.ml`, arrows to move,
  w/a/s/d to shoot -- `to_xy` and `to_x2`/`to_y2`'s first user, and the
  point of the game: running one way while shooting the other. One
  screen, seven kinds of robot each with one line of AI (a step towards
  the man, or towards the nearest human), the family to carry out at
  1000 a head and up, the grunts speeding up while the wave lasts, and
  the hulk nothing kills. No pathfinding on purpose: a grunt walks into
  an electrode and dies there, as in the arcade. A robot in the tests
  clears the first wave).
- **Kit**: the shmup kit's `Shots` (its third user: the man's shots and
  the enforcers' aimed sparks). Not the physics plan: nothing here has
  inertia, the man stops the frame you let the arrows go.

### 5. Scrolling shoot 'em ups (shmups)

Scramble (Konami, 1981: the first forced horizontal scroll with
several stages), Xevious (Namco, 1983: vertical, air and ground
targets), Gradius (Konami, 1985: the power-up bar), R-Type (Irem,
1987), then "bullet hell" (danmaku): DonPachi (Cave, 1995), Touhou
Project (ZUN, 1996-).

- **Toys**: TinyGradius (DONE: `games/TinyGradius.ml`, the cave as two
  strings of digits, the waves a timeline of the kit's Paths, the
  power-up bar, the options on the ship's trail, the Big Core; a robot
  clears it), TinyTouhou (a boss's
  bullet patterns -- spirals, rings, aimed volleys -- and a tiny
  hitbox).
- **Kit**, the shmup kit:
  - `Bullets`: patterns as functions of time (a ring of n bullets, a
    spiral rotating by d degrees per shot, aimed at the player);
    related work: BulletML (Kenta Cho, ABA Games), an XML language
    for bullet patterns;
  - `Paths`: enemies following curves (lines, arcs, Bézier curves,
    Catmull-Rom splines through points);
  - `Waves`: a level as a timeline, "at t = 12 s, 5 of these, along
    that path" -- a level is data, like a `Tilemap`'s strings;
  - `Camera2d` with an automatic scroll (the camera moves by itself;
    the player is kept inside the screen).

### 6. Maze games

Pac-Man (Toru Iwatani, Namco, 1980: the first game character, and
ghosts with personalities), Bomberman (Hudson, 1983), Boulder Dash
(First Star, 1984: rocks fall, diamonds roll).

- **Toys**: TinyPacman (DONE: `games/TinyPacman.ml`, our own 19x21
  maze), TinyBomberman (DONE: `games/TinyBomberman.ml`, bombs, fire in
  a cross, chain reactions); both on the maze kit, `gamekits/maze/`
  (`Grid_move`, `Chase`; the ghosts' personalities stay in TinyPacman).
- **Kit**, the maze kit:
  - `Grid_move`: moving along a `Tilemap`'s corridors, with the turn
    you ask for early remembered until possible ("pre-turning", what
    makes Pac-Man feel good), also for Snake and Bomberman;
  - `Ghosts`: Pac-Man's four chase targets (Blinky the player's tile,
    Pinky 4 tiles ahead, ...), and the chase/scatter/frightened
    modes, a state machine (Jamey Pittman, "The Pac-Man Dossier",
    2009; also `plan_teaching_other.md`'s game AI section).

### 7. Puzzle games on grids

Sokoban (Hiroyuki Imabayashi, Thinking Rabbit, 1982), Tetris (Alexey
Pajitnov, 1984; done: `games/Tetris.ml`), Lemmings (DMA Design,
1991), Puzzle Bobble (Taito, 1994; its free clone Frozen Bubble,
2002), Baba Is You (2019: the rules are tiles you push).

- **Toys**: TinySokoban (DONE: `games/TinySokoban.ml`, three levels of
  our own checked by a breadth-first solver; undo in Elm style, the
  list of past boards), TinyBabaIsYou (DONE: `games/TinyBabaIsYou.ml`,
  the rules as words on the board, read as sentences after each move,
  YOU, WIN, STOP, PUSH, DEFEAT, SINK and NOUN IS NOUN; four levels, each
  solved by a breadth-first search in the tests). Both on the puzzle
  kit, `gamekits/puzzle/`: `Push` (a chain pushed, TinySokoban's limited to
  one box) and `Undo`. TinyLemmings (DONE: `games/TinyLemmings.ml`, the
  terrain a bitmap of 4-pixel cells in the model, copied once per tick
  and drawn as row runs; walkers as tiny state machines reading the
  cells around them; Blocker, Builder, Basher, Digger; three levels as
  ASCII, each solved by a one-job plan in the tests). Not on the
  puzzle kit: no grid, no pushing; its bitmap is section 14's kit, the
  day TinyWorms wants caves. TinyPuzzleBobble (DONE:
  `games/TinyPuzzleBobble.ml`, a hexagonal grid in offset coordinates,
  the shot snapped to its nearest empty cell, then two flood fills: its
  color's group pops, what no longer hangs from the ceiling falls; the
  aiming guide is the shot flown ahead; three rounds, cleared by a
  robot aiming with that guide in the tests). Not on the puzzle kit
  either: hexagons, not squares.
- **Kit**: DONE, and it turned out not to be a kit at all:
  `playground/Puzzlescript.mli`, beside `Logo.mli` and `Bigbang.mli`.
  A `Rules` layer that a game *calls* from its own update would have
  belonged in `gamekits/`; this one takes the game over -- you give it
  things on layers, a map, the rules and what winning means, and there
  is no update and no view left to write -- and that is what
  `playground/` is for. A game as a map plus a dozen rules, as in
  PuzzleScript (Stephen Lavelle, 2013) and TileCode (Thomas Ball,
  Stefania Druga, et al., 2020, see `Tilemap.mli`): Sokoban comes out
  as *one* rule (`examples/PuzzleScriptSokoban.ml`) and a Boulder Dash
  as four (`examples/PuzzleScriptBoulders.ml`), the same engine twice.
  Its .mli carries the line this idea comes down: Papert's Logo, then
  KidSim / Cocoa / Stagecast Creator (Smith, Cypher and Schmucker,
  1994), Repenning's AgentSheets (1991 on), PuzzleScript, TileCode --
  people teaching programming by having you draw the board before and
  after. The engine's worked examples are in
  `playground/tests/Unit_puzzlescript.ml`; `tests/games` checks a
  breadth-first search solves all three Sokoban levels.

### 8. Platformers

Donkey Kong (Shigeru Miyamoto, Nintendo, 1981: jumping, and Mario),
Pitfall! (David Crane, Activision, 1982), Super Mario Bros. (Nintendo,
1985), Sonic the Hedgehog (Sega, 1991: speed, slopes, loops), Celeste
(2018: precise controls, and assist mode).

- **Toys**: `games/TinyMario.ml` (DONE, without enemies yet),
  TinyDonkeyKong (DONE: `games/TinyDonkeyKong.ml`, the first stage:
  slanted girders as segments, ladders, barrels zigzagging down; the
  hero and the barrels as state machines; a robot rescues Pauline),
  TinyCeleste (DONE: `games/TinyCeleste.ml`, Maddy Thorson and Noel
  Berry, 2018: three rooms -- the climb, the gap, the shaft -- for the
  jump, the dash and the wall jump; and **game feel** as four small,
  named lies told in the player's favour, each a function and each
  switched off in the game with 1-4: coyote time, the jump buffer,
  variable jump height, corner correction. Measured in the tests: a
  held jump rises 141 px and a tap 65; with corner correction a head
  clipping a ceiling by 2 px gets to 58, above it, and without, stops
  at 3. The first game written on `Tile_move` after it, whose header
  already cited Celeste's way of moving), TinyLodeRunner (DONE:
  `games/TinyLodeRunner.ml`, Doug Smith, 1983: digging holes that grow
  back, guards trapped in them, the escape ladder), TinyRick (DONE:
  `games/TinyRick.ml`, Rick Dangerous, Core Design, 1989: traps as
  tiles, the boulder, a pistol, dynamite, flip-screen rooms),
  TinySonic (DONE: `games/TinySonic.ml`, Sega, 1991: not losing speed
  rather than jumping exactly -- the ground felt as a *surface* with an
  angle rather than as solid tiles, so the hero runs up the hill and
  round the loop; the spindash; rings as life).
- **Kit**, the platformer kit (`gamekits/platformer/`): `Tile_move`,
  TinyMario's `move_by`, one pixel at a time; `Ladder`, climbing
  ladder tiles, for TinyLodeRunner and TinyRick; and `Slope`, the
  ground as tiles with a shape and an angle, found by sensors under
  the feet, for TinySonic. TinyDonkeyKong's slanted girders are
  segments, its own. And **game feel**, which is in TinyCeleste and
  not yet in the kit -- it moves there when a second game wants it,
  TinyMario the obvious one: coyote time (jumping a few
  frames after leaving a ledge), jump buffering (a jump pressed just
  before landing), variable jump height (releasing the button early),
  one-way platforms, slopes; stomping enemies. References: Steve
  Swink's *Game Feel*; Maddy Thorson, "Celeste and TowerFall Physics"
  (2017); the Sonic Physics Guide (the Sonic Retro community's
  reverse-engineered notes).

### 9. Beat 'em ups

Kung-Fu Master (Irem, 1984), Renegade (Technōs, 1986), Double Dragon
(Technōs, 1987: two players together), Final Fight (Capcom, 1989),
Streets of Rage (Sega, 1991).

- **Toy**: TinyFinalFight (DONE: `games/TinyFinalFight.ml`, one street
  on the belt, drawn back to front; the jab-jab-hook combo chained in
  the recoveries; waves locking the screen; a barrel's roast chicken;
  the boss; a robot clears it).
- **Kit**, the brawler kit (shared with fighting games):
  - the "belt": walking in depth as well as sideways, coordinates
    (x, depth, height), shapes sorted by depth before drawing -- the
    painter's algorithm again (see `graphics/3d`'s `Painter`);
  - `Hitbox`: attack boxes (hitboxes) vs. vulnerable boxes (hurtboxes),
    different on each frame of an animation;
  - `Anim`: a character as a state machine (idle, walk, punch, hurt,
    knocked down), each state a few frames long; needs sprite sheets
    (`plan_playground_other.md` section 4) for real characters, or
    stick figures made of shapes (which are fine, and funny).

### 10. Fighting games

Karate Champ (Technōs, 1984), Street Fighter II (Capcom, 1991: the
genre as we know it, combos by accident), Mortal Kombat (Midway,
1992).

- **Toy**: TinyStreetFighter (DONE: `games/TinyStreetFighter.ml`, vs
  the computer or two players on one keyboard; frame data, hitboxes,
  blocking high and low, the fireball's quarter circle read from the
  input history, hitstop; the brawler kit, `gamekits/brawler/`: `Hitbox`,
  `Frame_data`, `Stickman`, shared with TinyFinalFight).
- **Kit**: the brawler kit plus frame data (each move's startup,
  active and recovery frames: what fighting game players study), an
  input buffer recognizing motions (down, down-forward, forward +
  punch), and rollback for playing over the network (GGPO, Tony
  Cannon, 2006; see `plan_networking_teaching.md`).

### 11. Racing

Gran Trak 10 (Atari, 1974: top-down), Speed Race (Taito, 1974:
scrolling), Pole Position (Namco, 1982: pseudo-3D behind the car), Out
Run (Yu Suzuki, Sega, 1986: hills, forks, the radio), Micro Machines
(Codemasters, 1991: top-down again), Super Mario Kart (Nintendo, 1992:
Mode 7, see `plan_games3d.md`).

- **Toys**: TinyOutRun (DONE: `games2.5d/TinyOutRun.ml`, on the racing
  kit `gamekits/racing/` (`Road`, `Car`), shared with
  `games3d/TinyVirtuaRacing.ml`;
  pseudo-3D: all 2D shapes, so a perfect fit
  for the 2D playground -- the road is trapezoids, the scenery scaled
  sprites), TinyMicroMachines (DONE: `games/TinyMicroMachines.ml`, the
  head-to-head mode, drifting cars, the computer on the waypoints; a
  `Camera2d` looking ahead of the leader, or turning with it: the
  `angle` added to `Camera2d`), TinyKart (DONE: `games2.5d/TinyKart.ml`,
  Mode 7, see `plan_games3d.md`, on TinyMicroMachines' model).
- **Kit**, the racing kit:
  - `Road`: a track as segments (length, curve, hill), projected one
    segment at a time, from far to near; references: Lou Gorenfeld,
    "Lou's Pseudo 3d Page" (the classic on how the arcade games did
    it), Jake Gordon, "How to build a racing game" (2012, a JavaScript
    Out Run in four parts);
  - `Car`: speed, steering, off-road slowdown, and top-down drifting
    (the bicycle model, with the physics plan);
  - laps and checkpoints, opponents following a racing line (DONE:
    `Topdown`, TinyMicroMachines' drifting car and waypoints, out of it
    for TinyKart: laps, places, the computer's driving).

### 12. Action-adventure

Adventure (Warren Robinett, Atari 2600, 1980: the first action-
adventure, and the first Easter egg), The Legend of Zelda (Miyamoto
and Takashi Tezuka, Nintendo, 1986).

- **Toy**: TinyZelda (DONE: `games/TinyZelda.ml`, 3 x 2 rooms of 16 x
  11 tiles sliding in, a sword, a key, a locked door, a dungeon, the
  Triforce; octoroks and keese; `Camera2d.room` and `flip`, with
  TinyRick).
- **Kit**: a `Camera2d` flipping screen by screen (Zelda's rooms)
  rather than scrolling, a `Scene` for the title and game-over screens
  (`plan_playground_other.md` section 5), dialogs (its section 8),
  an inventory.

### 13. RPGs and roguelikes

Rogue (Michael Toy, Glenn Wichman, 1980), NetHack (1987), Dragon
Quest (Enix, 1986), Pokémon (Game Freak, 1996).

- **Toys**: TinyRogue (DONE: `games/TinyRogue.ml`, Rogue's own
  generator -- 3 x 3 cells, a room or a crossing in each, corridors --
  its lit rooms as the field of view, turns, three levels to the
  Amulet; a robot gets it), TinyDragonQuest (walk, then turn-based battles).
- **Kit**:
  - a turn-based loop: the world moves only when the player does
    (an `update` doing nothing without a key);
  - field of view: recursive shadowcasting (Björn Bergström, 2001,
    on RogueBasin), which also gives an RTS's fog of war;
  - dungeon generation (`plan_teaching_other.md` section 4: rooms and
    corridors, BSP, cellular automata caves);
  - menus, and battle scenes.

### 14. Artillery

Artillery (on mainframes and home computers, late 1970s), Gorillas
(QBasic, 1991), Scorched Earth (1991), Worms (Team17, 1995), Angry
Birds (Rovio, 2009: the physics plan's Slingshot).

- **Toy**: TinyWorms, with destructible terrain. (DONE:
  `games/TinyWorms.ml`, on the physics plan's `Physics` layer; its
  terrain a height map, as in Scorched Earth, so no caves yet: the
  bitmap kit below would give them.)
- **Kit**: terrain as a bitmap, explosions carving circles out of it
  (a `graphics/core` `Framebuffer`, collisions by pixel), plus the
  physics plan's projectiles and wind.

### 15. Strategy and tower defense

Dune II (Westwood, 1992: the RTS), Warcraft (Blizzard, 1994),
Command & Conquer (Westwood, 1995); Rampart (Atari, 1990), Desktop
Tower Defense (2007: tower defense in a browser).

- **Toys**: TinyTowerDefense (DONE: `games/TinyTowerDefense.ml`, the
  *maze* tower defense of Desktop Tower Defense rather than a fixed
  road: there is no road, the towers are the road, and every tower
  placed makes the monsters' way longer -- so the game is the
  pathfinding, `ai/Pathfind`, recomputed as you build, and you may
  never close the way completely), TinyDune2 (DONE:
  `games/TinyDune2.ml`, Westwood, 1992: harvest, build, and orders as
  paths -- a click on the ground is an A* for the selected unit), and
  TinyWarcraft2 (DONE: `games/TinyWarcraft2.ml`, Blizzard, 1995: what
  Dune II lacks -- a box dragged round several units, one search for a
  whole crowd as a flow field that each unit walks downhill
  (`Pathfind.field` and `downhill`), and the fog of war as two bitmaps,
  what has been seen and what is seen now).
- **Kit** (DONE): `gamekits/rts/`'s `Orders`, the layer between
  `ai/Pathfind`'s searches and a game -- the grid as a search problem,
  and the walking -- used by TinyDune2, TinyWarcraft2 and later
  TinyGauntlet2. Not done: a minimap (a second, zoomed-out camera,
  which is `games/TinyDefender.ml`'s scanner), and fog as roguelike
  field of view rather than a radius.

### 16. Rhythm games

PaRappa the Rapper (1996), Dance Dance Revolution (Konami, 1998),
Guitar Hero (2005).

- **Toy**: TinyDDR (DONE: `games/TinyDDR.ml`, Konami, 1998): arrows
  rising in time with an original tune, pressed as each reaches its
  outline. What the genre is about underneath is *what time it is*:
  every other game here runs on the frame clock, but the player is
  listening to the music, which runs on the sound card's clock and
  never waits for a late frame. So the steps are judged, and the
  arrows placed, by the music's clock (`Audio.position`, below); the
  flag clock=frame judges by the frame clock for comparison. And the
  music's clock is itself early by the machine's latency, which is why
  there is a calibration (- and =) and why the results report the
  average error -- a steady player is off by exactly the latency, so
  the average *is* the calibration to set. The chart is not typed in:
  a step on each note of the melody at the note's start (from
  `audio/Abc`), the arrow following the tune's shape (up, down, a leap
  sideways, a repeat the same arrow).
- **Second toy**: TinyGuitarHero (DONE: `games2.5d/TinyGuitarHero.ml`,
  Harmonix, 2005): **the instrument**. One highway, five frets, and a
  note is two hands -- the fret held, then the strum (the frets down at
  the strum are the ones played). Plus the long notes (a sustain held
  goes on scoring), chords (power chords: two frets at once), and the
  difficulty as the *same song reduced* rather than another song:
  Easy three frets and one note a chord, Medium four, Hard five and a
  chord's outer two, Expert everything (`Rhythm.reduce`, the table in
  `Rhythm.mli`). And you hear what you play: the song loops with the
  guitar muted (`Rhythm.muted`), a note hit sounds when strummed
  (`Rhythm.struck`, `Audio.of_tune`), a note missed is silence. The
  song has a drummer (an ABC `clef=perc` voice, below). In games2.5d
  because the highway is Out Run's road
  straightened: one division by the depth per point, the trick of the
  game, 23 lines.
- **Third toy**: TinyRockBand (DONE: `games3d/TinyRockBand.ml`,
  Harmonix, 2007, with Rock Band 3's keyboard): **the band**. Four
  highways -- guitar, bass, drums, keys -- in real 3D because a
  highway is a road into the distance and a camera draws four for
  free. You play one part at your own difficulty and the band plays
  the others as written; the parts are the four voices of one ABC tune
  (melody, bass line, chords, a beat), each charted from its own
  voice. What it adds is that **an instrument is a way of pressing**:
  fret-then-strum for guitar and bass, the key *is* the note on the
  keyboard (the same five keys), and hits for the drums (four pads and
  the kick pedal on space). The drums are charted from their GM drum
  keys (`pad_of_key`) and reduced their own way -- no pedal below
  Hard, one pad at a time below Expert, Easy only on the beats -- since
  folding frets means nothing on a kit. One crowd meter for the band,
  and the band heard without you: your part muted, your notes sounding
  only when hit, as in TinyGuitarHero.
  It needed the native 3D loop to feed the sound card at all (see
  `plan_audio_teaching.md`, phase 4).
- **Kit** (DONE): `gamekits/rhythm/`, TinyDDR's machinery moved out when
  TinyRockBand wanted it -- the grades and their windows (drawn in
  `Rhythm.mli`), the clock less the calibration, a chart played
  through, and `sounding`, the notes of a tune's voice to chart from --
  then grown by TinyGuitarHero with what an instrument needs:
  `on_frets` (a voice's pitches on five frets), `reduce` (the
  difficulty), `strummed`, `sustaining`.
  TinyDDR was rewritten onto it: its four tests and two golden frames
  did not move. And under it, in `audio/` and the playground, the
  music's own clock -- `Mixer.played`, the samples
  of a loop sent to the card counting every time round (its read
  position wraps, and a game timing steps by that would lose a whole
  song each pass), and `Audio.position`, the same in seconds. Tested
  in `audio/tests` and in the game's own tests, where two seconds of
  pulls are two seconds of song.

### 17. Side-view deathmatch: Soldat

Liero (Joosa Riekkinen, 1998: two worms on one keyboard, destructible
dirt, ninja ropes), then **Soldat** (Michał Marcinkowski, "MM", 2002,
written in Delphi, freeware, later open source): fast 2D deathmatch
seen from the side, soldiers with jet boots, dozens of weapons,
polygon maps, ragdoll deaths, bots, and online play -- the genre's
peak, and a favorite of this project's author.

- **Toy**: TinySoldat (DONE: `games/TinySoldat.ml`, one screen, you
  against two bots, first to 5 kills; Opensoldat, the open-sourced
  Soldat, is MIT: https://github.com/Soldat/soldat), the capstone of
  the physics plan: soldiers as
  bodies running, jumping and flying on jets (`thrust` against `fall`,
  with fuel), colliding with a polygon map (the physics plan's phases
  4-5), bullets fast enough to need its anti-tunneling (a swept test,
  notes_2d_physics.md section 12), grenades bouncing, ragdolls when
  a soldier dies (Verlet particles and distance constraints, Jakobsen's
  Hitman technique, the physics plan's Springs example grown up); then
  bots (`plan_teaching_other.md`'s game AI: pathfinding on the map's
  waypoints, aiming) and two players over the network
  (`plan_networking_teaching.md`).
- **Kit**: the physics plan's polygon collisions and ragdolls, a weapon
  table, spawn points, the camera following the player
  (`Camera2d`), a map as polygons (a map editor later).

(Names and dates from memory, to check.)

### 18. Turn the world: Cameltry

Cameltry (Taito, arcade, 1989; "On the Ball" on the SNES): you don't
move the ball, you turn the maze, and the ball rolls wherever down now
is, against the clock. Florent Monnier's Rolling-Moon (2008, OCaml on
his bindings of the Chipmunk engine, GPL 3; its sources and Inkscape
levels in `~/software-src/game/OCAML-games/rolling-moon`) is a
version of it: the ball a moon, touch every target. LocoRoco (Sony,
2006) tilts its world the same way.

- **Toy**: TinyCameltry (DONE: `games/TinyCameltry.ml`, written from
  scratch, not from Rolling-Moon's GPL code or levels): turning the
  maze is turning gravity (`push`), the moon rolls (the physics plan's
  rotation, phase 7: friction at its bottom point spins it), space
  jumps, the maze an ASCII map whose rows' runs of walls are immovable
  bodies. Exercises: levels, the exit and the timer, bumpers.
- **Kit**: none; `Physics`.

### 19. Gravity and caves: XPilot

Gravitar (Atari, 1982: a ship, gravity, caves), Thrust (Jeremy Smith,
1986: a pod hauled on a rod), then XPilot (Bjørn Stabell and Ken Ronny
Schouten, University of Tromsø, 1991: the same, multiplayer over the
Internet, on X terminals, with ASCII maps, cannons, fuel, teams and the
ball game). (Names and dates from memory, to check.)

- **Toy**: TinyXpilot (DONE: `games/TinyXpilot.ml`): the ball game
  alone against the cannons; the connector a rope that only pulls,
  the same force on the ship and the ball, opposite (Newton's third
  law, by hand with `Physics.push`); crashing when a bounce changes
  the velocity by too much; cannons aiming ahead (the intercept
  quadratic); a radar. A robot pilot in `tests/games/` wins it. Two
  players on one keyboard, on a mirrored map, each stealing the other's
  ball, one camera framing both ships, or the screen split (the walls,
  ropes and beams clipped to each view, Sutherland-Hodgman; a strip
  down the middle hiding the small things' overlap).
- **Kit**: none; `Physics`, `Tilemap`, `Camera2d`.

### 20. Pinball

Humpty Dumpty (Gottlieb, 1947: the first flippers, six of them, facing
outwards), Bill Budge's Pinball Construction Set (Apple II, 1983: a
table you drew, and the first argument that a table is data), Pinball
Dreams and Pinball Fantasies (Digital Illusions, 1992-93), 3D Pinball:
Space Cadet (Cinematronics/Maxis, 1995). (Names and dates from memory,
to check.)

- **Toy**: TinyPinball (DONE: `games/TinyPinball.ml`, one screen, a
  plunger, two flippers, three pop bumpers, two slingshots, two banks
  of drop targets, a drain, and the tilt; the table is a list of
  segments and circles, each with its restitution, its kick and its
  score, so the table can be changed without touching the physics).
  Two engines, `physics=engine` as in Asteroid and TinyMario: ours (30
  lines: the nearest point of a segment, reflect about the normal) and
  the playground's `Physics` layer (`bounce_off`: Collide and Resolve,
  with friction and the flipper's surface speed for free). They agree
  to 9 pixels on a wall bounce, which is the point of having both.
- **What it taught**, and what `plan_physics_remaining.md` should hear:
  a pinball is the case that breaks a fixed time step. A flipper
  throws the ball at ~3000 pixels a second, 50 a frame, four times its
  radius, so the game integrates in 4 substeps rather than with
  `Physics.step` (a fixed 1/60 s); `substeps=1` loses the ball through
  the table within a second, and a test checks exactly that. The
  subtler half: a *wall* can be the fast one -- a flipper tip travels
  45 pixels a frame, so the flippers substep too, or they sweep past a
  ball resting on them and throw nothing.
- **Kit**: none. There is one pinball, and what it would share with a
  second one is already in `physics/2d`.
- **Later**: `games3d/TinyPinball3d.ml`, the same game with 3D
  graphics and the 3D physics of `plan_physics3d_teaching.md` -- a
  bigger game, not a port of this one.

### 21. Puzzles with a mechanic: Portal

Narbacular Drop (DigiPen, 2005: the student game whose team Valve
hired), Portal (Valve, 2007), and in two dimensions Portal: The Flash
Version (We Create Stuff, 2007), which showed the idea survives losing
a dimension. (Names and dates from memory, to check.)

- **Toy**: TinyPortal2D (DONE: `games/TinyPortal2D.ml`, three test
  chambers: a goo pit crossed through the side walls, the fling, and a
  cube on a button behind a wall you can only shoot over. Left click
  the blue portal, right click the orange, on the white walls only;
  the physics is the `Physics` layer directly -- tiles as `immovable`
  bodies, the player an `upright` box, the cube one that tumbles).
- **The one idea**: a portal is a *transform*. In 2D it is a rotation
  R -- the one that turns the way you went into A into the way you
  come out of B -- applied to the position and, unscaled, to the
  velocity: `p' = B + R (p - A)`, `v' = R v`. Unscaled is why a fall
  becomes height somewhere else, which is the fling, and is the whole
  of "speedy thing goes in, speedy thing comes out".
- **What it taught**, beyond the transform, each found by playing it:
  - the gun needs a real grid walk (Amanatides and Woo, 1987), not a
    ray stepped a few pixels at a time: at a corner the stepped ray
    crosses both boundaries at once and puts the portal on the side of
    the floor instead of its top;
  - a teleport must test the body's *leading edge*, not its middle: a
    player walking over a hole at 260 pixels a second crosses its
    50-pixel mouth in 11 frames and falls 22 in that time -- exactly
    the distance from his middle to his feet -- so he skims across it;
  - a portal leaves its tile with *no* collision at all here, where a
    real engine carves the hole and keeps the rest solid;
  - a body must come out with a least speed, or two holes in the floor
    trap it: it pops out of one with nothing, falls back in, and a
    half turn mirrors left and right, so walking does not get it out;
  - and a level whose exit can be reached by a portal is not a puzzle:
    the floor behind the door had to stop being white.
- **Kit**: none yet. A second game with portals (a portal in a
  platformer, light through a portal) would want the transform and the
  crossing test out of this file.
- **Later**: the 3D one of `plan_physics3d_teaching.md`, where the
  transform carries an orientation too, and which is a bigger game
  than this, not a port of it.

### 22. The arcade dungeon crawl: Gauntlet

Dandy (Atari 800, 1983), Gauntlet (Atari Games, 1985, Ed Logg) and
Gauntlet II (1986: four of the same hero, the deflecting walls, and
the voice), then every crawl with a spawner in it. (Names and dates
from memory, to check.)

- **Toy**: TinyGauntlet2 (DONE: `games/TinyGauntlet2.ml`, two dungeons
  read as strings, four heroes that differ only by a row of numbers,
  the generators, keys and doors, food, potions, treasure, and the
  voice saying what just happened).
- **The ideas**, in the order they matter:
  - **the generator**: the monsters are a *flow*, not a set -- a tile
    that makes another one for ever until it is shot, so killing what
    comes at you is losing slowly, and the tap matters more than the
    water. Every crowd game since borrows it;
  - **health is the clock**: it drains by itself (10 a second here, of
    700), food is the only way to buy more, and every choice in the
    game -- fight? go round for the treasure? -- is the same question.
    In the arcade that clock was also the coin slot;
  - **stupid monsters on purpose**, with `chase=field` to see the
    difference. Measured, on a pen whose way out faces away from the
    hero: the greedy walk leaves 0 of 3 arriving (231 pixels away
    after 15 seconds), one Dijkstra flow field from the hero brings 3
    of 3 (1 pixel). On open floor or round a pillar the greedy walk
    arrives too -- trying the other axis when one is blocked is
    already a wall-follower -- so a dungeon of rooms and corridors is
    exactly where the cheap rule holds up;
  - and the meanest rule in arcade history: **your shot destroys the
    food**, the one thing keeping you alive.
- **The scroll**: Gauntlet's own selling point in 1986 was smooth
  scrolling in all eight directions at once, which the arcade hardware
  did in its display chip and a home computer of the time could not do
  at all. Here it is `Camera2d` and one number, the camera's zoom of 2:
  the screen shows ten tiles of a level twenty-four across, `follow`
  eases after the hero and `clamp` stops at the level's walls. What
  the zoom costs is the map, so the corner has one --
  `Sprite.pixels` over `Tilemap.to_strings`, since the dungeon is
  already a list of strings and so is a sprite (the trick
  `games3d/TinyComanche3d` draws its terrain with).
- **Kit**: none of its own. The flow field is `ai/Pathfind` through
  `gamekits/rts`' `Orders` (its third user, after TinyDune2 and
  TinyWarcraft2: a crowd walking to one place is the same problem
  whether it is peasants or grunts). Not `gamekits/maze`: `Grid_move`
  locks a mover to the middle of a tile, which is Pac-Man's movement,
  not Gauntlet's eight directions with sliding.
- **Exercises**: the second player (the arcade's real subject, and the
  shared food that makes friends fight), the thief, "It's a trap!",
  walls that deflect shots.

### 23. Sports: the ball that is not yours

Pong is here already (section 1); this is the other kind, the team
game seen from above. Kick Off (Dino Dini, Anco, 1989) and Kick Off 2
(1990), Sensible Soccer (1992), and beside them Speedball 2: Brutal
Deluxe (The Bitmap Brothers, 1990), which is a sport the way Rollerball
is. (Names and dates from memory, to check.)

- **Toy**: TinyKickOff2 (DONE: `games/TinyKickOff2.ml`, a pitch taller
  than the screen, five a side, a two-minute half, throw-ins and
  goals).
- **The one idea**: the ball is **not glued to your feet**. Every other
  football game of the era carried the ball with whoever was nearest;
  Dini's ball is touched ahead of you as you run into it and rolls on
  a little faster than you can run. The flag `ball=glued` plays it the
  other way, and the difference is a measurement: dribbling straight
  for three seconds, glued keeps the ball 22 pixels away (his feet --
  the two radii exactly), free lets it get 48 ahead and takes 5
  touches to keep it. A dribble becomes a chase you are only just
  winning.
- **The second**: the **aftertouch** -- while the ball is in the air
  from your kick, the arrows bend it. The same shot, held right for
  its 40 frames in the air, lands 141 pixels to the side of the one
  left alone.
- **The third**: a team is a **shape**, not ten brains. Each player
  has his place in a formation, pulled a third of the way towards the
  ball; only the nearest one chases. Ten lines, and it looks like
  football -- the same trick as a flock.
- **Second toy**: TinySpeedball2 (DONE: `games/TinySpeedball2.ml`,
  Speedball 2: Brutal Deluxe, The Bitmap Brothers, 1990). Its ball is
  the *opposite* of Kick Off's, which is the first thing to say about
  it: this is handball, so **the ball is carried** -- run near it and
  you simply have it, with no button and no chase, and you keep it
  until you throw it or somebody knocks it out of you. A carried ball
  does not roll, does not bounce off the walls and does not score off
  the furniture, so using the arena means letting go of it. The two
  games are the two answers to the same question, which is why they
  share a kit and not a mechanic. What else it adds:
  - **the arena scores**. A goal is 10 and so are two hits on a bounce
    dome; the stars are 5 and light the ball, the x2 plates double
    everything a side scores for ten seconds, flattening an opponent
    is 10. The thing on the screen is a *table*, not a pitch, and a
    match is usually won by whoever used the furniture -- it is
    `games/TinyPinball.ml`'s table with players on it;
  - **the loose ball never stops**: no touchlines, no referee, walls
    that give it back keeping four fifths of its speed
    (`Free_ball.bounce_in`), and a mouth at each end which is a *gap*
    in the wall;
  - **the view is nailed to the ball**, and close in: 625 pixels of a
    760 by 1320 arena, scrolling both ways, where Kick Off shows you
    the width of the pitch and follows the ball up and down it;
  - **violence is a move**, not a foul: space with no ball is a
    tackle, two seconds on the floor and ten points.
- **Third toy**: TinySensibleSoccer (DONE:
  `games/TinySensibleSoccer.ml`, Sensible Software -- Jon Hare and
  Chris Yates -- 1992). It is worth writing after the other two
  because it is the third answer to their one question, and the three
  differ by a single number, how fast a touch sends the ball against
  how fast a man runs:

  | game | touch / run | the ball, dribbling straight |
  |---|---|---|
  | `ball=glued` (TinyKickOff2's flag) | -- | 22 px: it is his feet |
  | TinySensibleSoccer | 4.0 / 3.6 | 28 px: close control |
  | TinyKickOff2 | 4.8 / 3.4 | 48 px: a chase you are just winning |
  | TinySpeedball2 | -- | in his hands: he carries it |

  What is its own: **the ball has a height** (a z, a shadow on the
  grass, a bounce when it lands, and above head height nobody can
  touch it -- so the lofted through-ball and the header), **the view
  is pulled back** (nearly the whole pitch, the smallest men of the
  three, against Speedball's close-in ball-centred view), and
  **aftertouch is the game**: over the same 45 frames a lofted shot
  held right ends 321 pixels to the side of the one left alone, a tap
  along the grass 163.
- **Where it goes**: `games/`, not `games2.5d/`. That directory is for
  games that fake a 3D *view* on the 2D playground -- a raycaster,
  Mode 7, voxels -- and say so ("the trick of this game"). A ball with
  a height and a shadow is a coordinate, not a trick of rendering.
- **Kit** (DONE, and this is what the second game was for):
  `gamekits/sports/` -- `Free_ball` (the ball pushed ahead of a player
  rather than carried, with the glued alternative, the grass or metal
  friction, the aftertouch push and the walls) and `Formation` (a spot
  per player, pulled part of the way towards the ball; the nearest one
  chases), now with three users. The ball's *height* is deliberately
  not in it: one game has it, and a second one wanting it is what
  would move it there. TinyKickOff2 was rewritten onto it, which is the only way
  to know a kit is real -- its five
  tests did not change.
- **What the second game taught**, all of it found by tests rather
  than by playing: a chaser must judge "I have the ball" *inside* his
  touch reach, or he turns for goal before he can ever touch it and
  walks away from the ball for ever (in Speedball the ball then never
  moved at all); only one player may touch the ball per frame, or ten
  of them round it cancel each other out; a touch must not push the
  ball through a wall, or the chaser pins it and the two travel down
  the arena together; and a goal mouth with no depth is one the ball
  slides across without going in.

### 24. The flap: Joust

Joust (John Newcomer, Williams Electronics, 1982) is the platform
game with the ground taken away: you ride a flying ostrich over a pit
of lava, the knights ride buzzards, and of two riders who meet the
higher lance wins. The loser leaves an egg, which falls, bounces along
the ledges and hatches into a faster rider unless it is collected
first. It was also the first arcade game two people could play at the
same time on the same screen, and co-operation was optional: flying
into your friend jousted him just as well.

- **Toy**: TinyJoust (DONE: `games/TinyJoust.ml`, left/right to push,
  space or up to flap -- once per press, so the key is hammered).
- **Kit**: none, and that is the point of writing it here. A whole
  arcade game -- waves, lives, a score, an enemy with a mind of its
  own, eggs that hatch -- in about 200 lines of code (the size of
  TinyBreakout, which has none of those), because the physics layer is
  already most of it:

  | what the game needs | who does it |
  |---|---|
  | the flight model | `Physics.fall`, `slow`, `push`, `step` -- five lines |
  | momentum, top speed, the long turn | nothing: what those four verbs do together |
  | the ledges, bird against bird, the eggs bouncing | one `Physics.bounce_all` |
  | the game itself | one rule: of two riders who touch, the higher wins |

  So it is the answer to "what does the `physics/` plan buy a game?",
  told by a game that would otherwise have had to write all of it.
- **What it says against its neighbours**: TinyFlappyBird's flap
  *sets* the rise (every flap identical, learnable by rhythm, and no
  physics under it at all: two numbers and a line of gravity); Joust's
  flap *adds* to it, the more physical and much harder choice, and the
  reason flapping has a rhythm of its own. TinyMario walks a tilemap
  one pixel at a time (`gamekits/platformer`'s `Tile_move`); Joust has no
  tiles and no collision code -- seven ledges are seven immovable
  bodies.
- **Left undone** (in the game's header as exercises): the second
  player on the same keyboard, which is the real Joust and needs only
  a second flyer with the `Player` role; the pterodactyl; the lava
  troll's hand; the egg collected in the air, worth more the higher it
  is taken; the survival and egg waves.

### 25. The planet, and the scanner: Defender

Defender (Eugene Jarvis and Larry DeMar, Williams Electronics, 1981),
the hardest game of the arcade and the highest-grossing of its year,
and those are the same fact. Jarvis went on to Robotron: 2084 (section
4) and to the flap of Joust's team-mates down the hall (section 24).

- **Toy**: TinyDefender (DONE: `games/TinyDefender.ml`, left/right
  thrust and flip, up/down, space the laser, b a smart bomb).
- **Kit**: none. Two layers and one kit do the work: `Camera2d` (the
  scroll that *leads* the ship, and the scanner), the shmup kit's
  `Shots` (its fourth game), and `Physics` for the ship alone -- its
  famous inertia is `push`, `slow`, `step` and nothing else.
- **What it is here for**, and why it is worth writing after a dozen
  one-screen games:
  - **the world does not fit on the screen, and it is a cylinder**.
    One function, `near`, slides a thing to whichever of its copies is
    nearest the camera; drawing, aiming and the distance between two
    things all go through it and the seam never comes up again.
    Asteroids wraps a world the size of the screen, where it is
    invisible; here it is six screens and you can chase a lander all
    the way round to where you started;
  - **the scanner is a camera, not a picture**: the whole planet
    squashed into the strip at the top is a second view of the same
    model at another scale, which is what `Camera2d.mli`'s history
    section already says Defender invented. Playing is reading it --
    the screen is where you are only now arriving;
  - **the mountains are a function**, three sines whose wavelengths
    divide the world exactly, so the range meets itself at the seam. A
    height map (TinyWorms) would be 6000 numbers and a join to get
    right.
- **And the rule that makes it a game and not a shooting gallery**:
  the ten humans are the state of the world, not the score. A lander
  that gets one to the top *becomes* a mutant, so every abduction you
  miss is an enemy you will have to fight; lose all ten and the planet
  goes -- no ground, empty space, every lander turning at once, and
  you keep playing having lost what you were playing for.
- **Left undone** (exercises in the header): the rest of the zoo
  (baiters, bombers, pods and swarmers), hyperspace, mountains that
  kill you, and a second player taking turns.

### 26. The city that builds itself: SimCity

SimCity (Will Wright, Maxis, 1989), out of the level editor of his
Raid on Bungeling Bay; its source GPL'd in 2008 as Micropolis.

- **Toy**: TinySimCity (DONE: `games/TinySimCity.ml`, tools 1 to 7 or
  the toolbar, the mouse or the arrows and space, v the view, - = the
  tax, f fast).
- **Kit**: none; `Scene2d`, and a `tile array` as TinyTowerDefense has.
- **What it is here for**: the one game of the list where the player is
  not the subject. No enemy, no goal -- a "software toy" -- and the
  code worth reading is the month (`month`), which the player never
  calls:
  - **the valves**: the R C I bars, what the city lacks of each zone,
    from a census (homes follow the jobs, shops and factories follow
    the workers, the tax pushes on all three). It feeds on itself,
    which is why a town grows at all;
  - **the maps behind the tiles**: power as a flood fill from the plants
    through what conducts (lines and zones, *not* roads), pollution as
    a diffusion -- the image blur, a tenth lost a month -- that
    empties the homes beside the factories. "v" shows both: the
    simulation's real state is maps, the tiles are only one of them;
  - **the dice as a hash** of place and month, so a neighbourhood grows
    in patches and the same city replays the same (tests, golden
    frames).
- **Left undone** (exercises in the header): traffic (the cars' random
  walk, and what it crowds and fouls), crime and police as another
  spread map, land value, a plant's capacity, bridges, 3x3 zones,
  disasters.

### 27. One more turn: Civilization

Civilization (Sid Meier and Bruce Shelley, MicroProse, 1991), after
Walter Bright's Empire and Francis Tresham's board game; the "4X" genre
(explore, expand, exploit, exterminate) was named after it.

- **Toy**: TinyCivilization (DONE: `games/TinyCivilization.ml`, arrows
  move the blinking unit, b builds a city, p production, r research,
  t the tree, Enter ends the turn).
- **Kit**: none; `Scene2d`. Turns are TinyRogue's and the black map
  TinyWarcraft2's fog ("ever seen"), both borrowed rather than new.
- **What it is here for**:
  - **the tree of advances**, a directed acyclic graph: an advance is
    open once all its parents are known, and "t" draws it in columns
    by depth (one more than the deepest parent). Research is choosing
    a path, and the military branches lead nowhere near Philosophy
    (the science victory standing in for the spaceship);
  - **a city as a small economy**: its tile and one more per citizen,
    the best first, each giving food, shields and trade -- food grows
    the city, shields build units (settlers take a citizen), trade is
    research;
  - **a made world**: a random number per tile, blurred three times,
    cut at a sea level and at levels for the kinds of land; the same
    seed, the same world;
  - **a rival on the same rules** (expanding, researching, garrisoning,
    then marching once it has an army), and combat as a weighted coin
    in which the losing defender's whole stack dies (Civilization I).
- **Left undone** (exercises in the header): the 21-tile fat cross,
  buildings, roads and terraforming, boats, taxes, the rival's own fog,
  diplomacy and more rivals.

### Later, or never

Point-and-click adventures (Maniac Mansion and SCUMM: verbs,
walkboxes), text adventures (Colossal Cave Adventure, 1976; Zork) --
different enough from the playground's model to be projects of their
own.

## Infrastructure all the games need

- **Scripted inputs for golden frames**: DONE, `-script
  "right:1-60,up:30"` (`playground/native_common/Input_script.mli`),
  game keys held over given frames, in both native loops; the golden
  runner's scripted scenes use it (`TinyMario_run`,
  `TinyInvaders_play`, `StarCollector3d_move`).
- **Sprites** (`plan_playground_other.md` section 4) for the genres
  whose characters animate (beat 'em ups, fighting); shapes are enough
  for the others.
- **Scenes** (its section 5): every arcade game has an attract mode, a
  title, a game over.
- **Seeded randomness**, pure (its section 1), for the games with
  random enemies, once replays matter.

## Ordering

1. The easy toys first, for the course (`plan_teaching_other.md`):
   TinyBreakout, TinyTron, TinySokoban -- no kit, a beginner can read
   them.
2. The maze kit with TinyPacman: `Tilemap` exists, and ghost AI is
   the best-known game AI lesson.
3. The shmup kit: TinyInvaders, then TinyGradius, then TinyTouhou
   (the bullet patterns are beautiful to look at, and mathematics).
4. The racing kit with TinyOutRun: spectacular, and pure 2D.
5. The platformer kit, when TinyDonkeyKong gives `move_by` its second
   user.
6. The brawler kit (beat 'em up, then fighting), once sprites exist.
7. The rest as they come; artillery with the physics plan, rhythm
   with the audio plan, fighting's rollback with the networking plan.
