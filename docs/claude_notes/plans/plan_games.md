# Plan: the classic games, genre by genre, and the kits they need

## Context

`games/` started with five games (Pong, Snake, Tetris, Asteroid,
TinyMario), `games3d/` with two (StarCollector3d,
Minecraft3d). Each was written from scratch on top of `Playground`
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
library, like `Camera2d`); each kit in `kits/<genre>/`, its own
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
  `kits/lightcycles/`, only their views differing).

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
  a robot in `tests/games/` clears stage 1).
- **Kit** (the start of the shmup kit): `Shots` (the player's and the
  enemies' bullets, created, moved, removed when off-screen or on a
  hit), `Formation`.

### 4. Multi-directional shooters

Asteroids (Atari, 1979; done: `games/Asteroid.ml`), Robotron: 2084
(Eugene Jarvis, 1982: two joysticks, the twin-stick), Geometry Wars
(2003).

- **Toy**: TinyRobotron (arrows to move, w/a/s/d to shoot).
- **Kit**: the shmup kit's `Shots`; the physics plan for the ship.

### 5. Scrolling shoot 'em ups (shmups)

Scramble (Konami, 1981: the first forced horizontal scroll with
several stages), Xevious (Namco, 1983: vertical, air and ground
targets), Gradius (Konami, 1985: the power-up bar), R-Type (Irem,
1987), then "bullet hell" (danmaku): DonPachi (Cave, 1995), Touhou
Project (ZUN, 1996-).

- **Toys**: TinyGradius (horizontal, power-ups), TinyTouhou (a boss's
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
  a cross, chain reactions); both on the maze kit, `kits/maze/`
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
1991), Baba Is You (2019: the rules are tiles you push).

- **Toys**: TinySokoban (DONE: `games/TinySokoban.ml`, three levels of
  our own checked by a breadth-first solver; undo in Elm style, the
  list of past boards), TinyBabaIsYou (later: rules as tiles).
- **Kit**: a `Rules` layer rewriting a `Tilemap` with patterns, as in
  PuzzleScript (Stephen Lavelle, 2013) and TileCode (Thomas Ball,
  Stefania Druga, et al., 2020, see `Tilemap.mli`): a game as a map
  plus a dozen rules -- maybe the most beginner-friendly kit of all.

### 8. Platformers

Donkey Kong (Shigeru Miyamoto, Nintendo, 1981: jumping, and Mario),
Pitfall! (David Crane, Activision, 1982), Super Mario Bros. (Nintendo,
1985), Sonic the Hedgehog (Sega, 1991: speed, slopes, loops), Celeste
(2018: precise controls, and assist mode).

- **Toys**: `games/TinyMario.ml` (DONE, without enemies yet),
  TinyDonkeyKong (one screen, ladders, rolling barrels),
  TinyCeleste (a dash, wall jumps).
- **Kit**, the platformer kit: `Actor` (TinyMario's `move_by`, one
  pixel at a time), and **game feel**: coyote time (jumping a few
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

- **Toy**: TinyFinalFight, one street, three kinds of thugs.
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

- **Toy**: TinyStreetFighter, two players on one keyboard.
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

- **Toys**: TinyOutRun (DONE: `games/TinyOutRun.ml`, on the racing
  kit `kits/racing/` (`Road`, `Car`), shared with
  `games3d/TinyVirtuaRacing.ml`;
  pseudo-3D: all 2D shapes, so a perfect fit
  for the 2D playground -- the road is trapezoids, the scenery scaled
  sprites), TinyMicroMachines (DONE: `games/TinyMicroMachines.ml`, the
  head-to-head mode, drifting cars, the computer on the waypoints; a
  `Camera2d` looking ahead of the leader, or turning with it: the
  `angle` added to `Camera2d`).
- **Kit**, the racing kit:
  - `Road`: a track as segments (length, curve, hill), projected one
    segment at a time, from far to near; references: Lou Gorenfeld,
    "Lou's Pseudo 3d Page" (the classic on how the arcade games did
    it), Jake Gordon, "How to build a racing game" (2012, a JavaScript
    Out Run in four parts);
  - `Car`: speed, steering, off-road slowdown, and top-down drifting
    (the bicycle model, with the physics plan);
  - laps and checkpoints, opponents following a racing line.

### 12. Action-adventure

Adventure (Warren Robinett, Atari 2600, 1980: the first action-
adventure, and the first Easter egg), The Legend of Zelda (Miyamoto
and Takashi Tezuka, Nintendo, 1986).

- **Toy**: TinyZelda: a few screens, a sword, a key, a door.
- **Kit**: a `Camera2d` flipping screen by screen (Zelda's rooms)
  rather than scrolling, a `Scene` for the title and game-over screens
  (`plan_playground_other.md` section 5), dialogs (its section 8),
  an inventory.

### 13. RPGs and roguelikes

Rogue (Michael Toy, Glenn Wichman, 1980), NetHack (1987), Dragon
Quest (Enix, 1986), Pokémon (Game Freak, 1996).

- **Toys**: TinyRogue (the whole genre fits in the characters of a
  `Tilemap`), TinyDragonQuest (walk, then turn-based battles).
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

- **Toys**: TinyTowerDefense (enemies along a `Paths` path in
  `Waves`: the shmup kit reused!), TinyRTS (a few units).
- **Kit**: selection with a mouse rectangle (`Camera2d.to_world`),
  orders, A* on a `Tilemap` (`plan_teaching_other.md`'s game AI),
  fog of war (roguelike FOV), a minimap (a second, zoomed-out camera).

### 16. Rhythm games

PaRappa the Rapper (1996), Dance Dance Revolution (Konami, 1998),
Guitar Hero (2005).

- **Toy**: TinyDDR: arrows scrolling up, pressed on the beat.
- **Kit**: in `plan_audio_teaching.md` (timing against the music,
  not the frame).

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
  ball, one camera framing both ships (a split screen would need
  clipping, which the playground lacks).
- **Kit**: none; `Physics`, `Tilemap`, `Camera2d`.

### Later, or never

Point-and-click adventures (Maniac Mansion and SCUMM: verbs,
walkboxes), city builders (SimCity, 1989), text adventures (Colossal
Cave Adventure, 1976; Zork) -- different enough from the playground's
model to be projects of their own.

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
