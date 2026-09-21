# Plan: the classic 3D games, genre by genre, and the kits they need

## Context

The 3D counterpart of [`plan_games.md`](plan_games.md) (read it first:
the principles, the "kits" and the "layers", the references are the
same). `games3d/` has two games: StarCollector3d (a third-person toy)
and TinyMinecraft (a voxel sandbox, the port of the Python tiny-minecraft,
see `done/plan_tiny_minecraft.md`), plus the scenes of `examples3d/`
(Corridor3d walks down a corridor first-person, CachedGrid3d draws a big
static world, FloatingCity3d a composition).

3D games came in genres too, each one born from a rendering trick
before it was a design: Battlezone is what vector hardware could draw,
Wolfenstein 3D what a raycaster could, Doom what a BSP tree could,
Super Mario 64 what a GPU and an analog stick could. So this plan is
also a history of 3D rendering, told through the games -- which fits
`graphics/3d`'s teaching (`notes_3d.md`) and
[`notes_vs_doom_quake.md`](notes_vs_doom_quake.md), which it
references rather than repeats.

What `playground3d` already gives a game (see `Playground3d.mli`):
shapes (`box`, `sphere`, `polygon3d`, textures), `group3d`/`move3d`/
`rotate3d`, a `camera` (an eye and a target), `hud` for 2D shapes on
top, `cached3d` for big static scenes, `project` (a 3D point to the
screen), flat/smooth shading, and, since TinyMinecraft, relative mouse
motion (`mdx`/`mdy`) and `?capture_mouse`. Four backends: software,
OpenGL, WebGL, and the web SVG one.

## Principles

The same as `plan_games.md`'s -- toys not clones, a layer when a second
game needs it, teaching, game culture -- plus two for 3D:

- **Performance decides the toy**: the software rasterizer is the
  teaching backend, and it's slow (TinyMinecraft's frame rate,
  `plan_minecraft_remaining.md` section 5). A toy's scene should stay
  small enough to be playable there: flat-shaded low-poly (Virtua
  Racing's look) rather than Quake's. The GPU backends
  (`plan_3d_remaining.md` section 5: fog, frustum culling) are for the
  big scenes.
- **Pseudo-3D is 2D**: several classics aren't really 3D (a raycaster,
  Mode 7, voxel terrain, Out Run's road); they fit the 2D playground,
  or `graphics/core`'s `Framebuffer`, as well as `playground3d`. Each
  section says which; the pseudo-3D toys are the best lesson on *why*
  real 3D (a z-buffer, clipping, perspective-correct textures) was
  needed.

## Kits

```
  games       TinyBattlezone TinyWolfenstein  TinyDoom  TinyMario64  TinyVirtuaRacing  TinyMinecraft
                    |           |         |           |              |               |
  3D kits     Vector   Raycaster (2D  Sectors    Camera3d       Track3d        Voxels
              (lines)  grid, DDA)     (portals)  (chase, orbit)  (+ racing kit)  (its block grid)
                    |           |         |           |              |               |
  3D layers   Fps_controller (from TinyMinecraft)  Heightmap  Collide3d (box vs. world)
                    |                                     |
  base        Playground3d: shapes, camera, hud, cached3d        (+ 2D: Camera2d, Tilemap)
```

Where: generic 3D layers in `playground3d/` (next to `Gpu_scene`), 3D
kits in `kits/<genre>/` like the 2D ones. Two layers exist in all but
name, both inside `games3d/TinyMinecraft.ml`: its player section (a
first-person controller: walking, jumping, gravity, looking with
yaw/pitch, colliding with blocks) and its world section (a voxel grid,
with a ray walk to find the targeted block). They become layers when a
second game wants them.

## References

The Wikipedia and book references of `plan_games.md`, plus, for 3D:

- Wikipedia, [First-person shooter](https://en.wikipedia.org/wiki/First-person_shooter)
  (its history section),
  [Vector monitor](https://en.wikipedia.org/wiki/Vector_monitor),
  [Mode 7](https://en.wikipedia.org/wiki/Mode_7),
  [2.5D](https://en.wikipedia.org/wiki/2.5D);
- Fabien Sanglard, *Game Engine Black Book: Wolfenstein 3D* (2017) and
  *Game Engine Black Book: Doom* (2018): the engines and their
  hardware, read line by line;
- Michael Abrash, *Graphics Programming Black Book* (1997): its last
  chapters are Quake's renderer, by one of its authors;
- Lode Vandevenne, [Raycasting](https://lodev.org/cgtutor/raycasting.html)
  (and its parts II-IV: floors, sprites, doors): the classic tutorial
  on Wolfenstein's technique, the DDA grid walk;
- John Amanatides and Andrew Woo, "A Fast Voxel Traversal Algorithm
  for Ray Tracing" (Eurographics, 1987): the same grid walk, in 3D --
  a raycaster's and Minecraft's block picking.

## Sources to adapt: 3D games in Elm, Haskell, OCaml

As in `plan_games.md`: look for a functional version first. Fewer
exist in 3D (found, not yet read):

| Genre | Game | Language | Notes |
|---|---|---|---|
| first-person | [first-person-elm](https://github.com/evancz/first-person-elm) | Elm (WebGL) | Evan Czaplicki's first-person walk in a small world, ~300 lines |
| FPS | [Frag](https://github.com/rainbyte/frag) (also on Hackage) | Haskell (Yampa, OpenGL) | Mun Hon Cheong's 2005 undergraduate thesis, "Functional Programming and 3D Games": an FPS loading Quake III BSP levels |
| racing / physics | [elm-physics](https://github.com/w0rm/elm-physics)'s RaycastCar example | Elm | a car on raycast wheels, Andrey Kuzmin (w0rm) |
| various | [elm-3d-scene examples](https://github.com/ianmackenzie/elm-3d-scene/tree/main/examples) | Elm | Ian Mackenzie's engine: lighting, shadows |
| toys | [lucamug/elm-playground-3d](https://github.com/lucamug/elm-playground-3d), nateabele/elm-3d-playground | Elm | the playground3d lineage (see `notes_playground3d_related_work.md`); StarCollector3d's idea comes from the latter |

No OCaml 3D game was found (searching for OCaml raycasters found
only C, Go and JavaScript ones): our toys would be the first.

## The genres

In historical order, which here is also the order of rendering
techniques.

### 1. Vector 3D: wireframes

Battlezone (Ed Rotberg, Atari, 1980: tanks on a vector monitor,
seen through a periscope), Tempest (Dave Theurer, Atari, 1981), Star
Wars (Atari, 1983), and Elite (David Braben and Ian Bell, 1984: a whole
galaxy of trading and dogfights, generated from a few numbers, on a
32 KB BBC Micro).

- **Toy**: TinyBattlezone (DONE: `games2.5d/TinyBattlezone.ml`, on
  the 2D playground, its segments taken into the eye's coordinates,
  near-clipped and divided by the depth in the game itself; its twin
  `games3d/TinyBattlezone3d.ml` is the same battle in solid faces, a
  z-buffer hiding the tank behind the pyramid) -- tanks
  and pyramids as wireframes, a radar. The first 3D game to write: no hidden surfaces, no shading,
  just `project`; the software backend's wireframe mode ("f") is
  already its look. TinyElite (DONE: `games2.5d/TinyElite.ml`, a
  ship, a space station to dock in, spinning; its whole engine written
  in the game, on the 2D playground: the universe turning round you,
  the 6502's small-angle turns and TIDY, hidden lines on convex hulls,
  and the galaxy out of its seed).
- **Kit**: `Vector`: shapes as lists of 3D line segments, drawn with
  `project` into 2D lines (so it also fits the 2D playground), with
  near-plane clipping for lines (the 2D version of `graphics/3d`'s
  `Clip`).

### 2. First-person mazes and dungeon crawlers

Maze War (Steve Colley, Greg Thompson, Howard Palmer, NASA Ames,
1973-74: the first first-person shooter, soon on the ARPANET), 3D
Monster Maze (Malcolm Evans, ZX81, 1981), Wizardry (Sir-Tech, 1981:
step by step, turn by turn), Dungeon Master (FTL, 1987: in real time).

- **Toy**: TinyDungeonMaster (DONE, and in the *2D* playground:
  `games2.5d/TinyDungeonMaster.ml`, since the trick needs no 3D at
  all). A `Tilemap` of walls, the hero moving one cell and turning 90
  degrees at a time, so each cell in view has one fixed place on the
  screen: at depth d the 2d + 1 cells across, each a square (the face
  it shows) and a trapezoid (the face along the corridor), drawn
  farthest first. The step between a 2D map and real 3D, and the
  shortest renderer of `games2.5d/` -- 24 lines for the slots. Around
  it, what actually made Dungeon Master: the iron key and the door,
  the lever and the portcullis, monsters on their own clock (the
  "dance": one that has just moved cannot strike yet), the torch
  burning down, and a panel you act through. The A* of `ai/Pathfind`
  walks the monsters.
- **Kit**: none needed in the end -- `Tilemap` and `ai/Pathfind` were
  enough. Not `Grid_move` of the 2D maze kit: that one slides a mover
  between tiles, and here a step is a whole cell, at once.
- **No 3D twin**, on purpose (unlike TinyWolfenstein, TinyDoom, TinyComanche,
  TinyDescent): those pairs share a world and differ only in the
  renderer, but the grid view is not a renderer, it is a rule about
  where the hero may stand and which way it may look, and the whole
  game is built on it -- cell steps, quarter turns, the dance. Give the
  camera its freedom and nothing of the game is left, only TinyWolfenstein3d
  with a smaller map. See `games2.5d/README.md`.

### 3. Raycasting: Wolfenstein 3D

Hovertank 3D and Catacomb 3-D (id Software, 1991), Wolfenstein 3D (id,
1992: John Carmack's raycaster, one ray per screen column, walls all
the same height on a grid).

- **Toys** (DONE: `games2.5d/TinyWolfenstein.ml` and `games3d/TinyWolfenstein3d.ml`,
  the same map, walk and golden frames; the 2D one with billboards
  hidden per column and a minimap of the rays): TinyWolfenstein in the **2D playground** (one rectangle per
  column, its height 1 / distance: a raycaster needs no 3D at all),
  and TinyWolfenstein3d in `playground3d` (the same map as boxes, the same
  controls), to compare the two -- the lesson of the section.
- **Kit**: `Raycaster`: the DDA walk through a `Tilemap` grid
  (Lode's tutorial; Amanatides and Woo), the fisheye correction,
  wall shading by side, then textures and sprites (Lode's parts II
  and III). In `graphics/` style, one idea per function.

### 4. Doom: sectors, and 2.5D

Doom (id, 1993), Duke Nukem 3D (1996, Ken Silverman's Build engine:
sectors joined by portals). Levels are a 2D floor plan of sectors,
each with a floor and a ceiling height (`notes_vs_doom_quake.md`,
"Doom: not actually 3D").

- **Toys** (DONE: `games2.5d/TinyDoom.ml`, `games3d/TinyDoom3d.ml`), a
  pair, like TinyWolfenstein and TinyWolfenstein3d:
  - TinyDoom, in the *2D* playground, Doom's own renderer in small:
    the level's segs split into a BSP tree by a node builder at
    startup (id's was a separate tool, `idbsp`), walked front to back
    from the player (`R_RenderBSPNode`), each seg drawn column by
    column, one-sided walls closing columns, two-sided ones drawing
    their upper and lower steps and narrowing the per-column clip
    arrays (`R_StoreWallRange`'s ceilingclip and floorclip), floors
    and ceilings filled between them, a subtree skipped when its box
    is behind closed columns (`R_CheckBBox`). No z-buffer, no 3D:
    the lesson is Doom's. Sector light, darker with the distance, and
    the "fake contrast"; the sector you're in found by walking the
    BSP (`R_PointInSubsector`).
  - TinyDoom3d, the same level in `playground3d`: floors, ceilings and
    walls extruded as `polygon3d`s once (`cached3d`), a z-buffer, no
    BSP. The comparison is the point.
- **Kit**: `Sectors` (`kits/sectors/`, shared by the pair): a level
  as sectors, polygons with a floor, a ceiling and a light, from which
  the linedefs are found (the shared edges are two-sided); the sector
  at a point by its polygons; moving a circle against the lines (steps
  up to 24 units, 56 of headroom); and the level of both games. The
  node builder and the renderer stay in TinyDoom.

### 5. True 3D shooters

Ultima Underworld (Blue Sky, 1992), Descent (Parallax, 1995: the first
6-degrees-of-freedom shooter, its mine a graph of convex cells drawn
through portals), Quake (id, 1996: real 3D levels, lightmaps, and
online play), then Half-Life (1998).

- **Toys** (DONE: `games2.5d/TinyDescent.ml`, `games3d/TinyDescent3d.ml`),
  a pair again:
  - TinyDescent, in the *2D* playground, with its own projection,
    near-plane clipping and Sutherland-Hodgman window clipping: the
    mine walked through its portals from the cell holding the eye
    (Descent's `render_mine` and its window rectangles), the cells
    drawn farthest first, painter's, no z-buffer. The status bar
    counts the cells drawn (3 of 12 down a corridor).
  - TinyDescent3d, the same mine as `cached3d` polygons in
    `playground3d`, all of it every frame, the z-buffer sorting it
    out.
  - It needed `Playground3d.camera` to take an `?up` (a ship that
    rolls), honored by the four 3D backends.
- **Toy** (DONE: `games3d/TinyQuake.ml`): Quake's three offline tools
  in small, all run at startup -- `qbsp` (the map's solid boxes, their
  hidden faces removed as CSGFaces does, then a BSP tree whose leaves
  are rock or air), `vis` (each leaf's potentially visible set; ours
  samples sight lines where Quake clipped through portal chains) and
  `light` (each face cut into patches, each patch asking every lamp
  whether it can see it: real shadows, a lightmap of one texel per
  patch) -- then, per frame, the eye's leaf's set drawn with the
  z-buffer, and "v" to turn the set off and see the same picture cost
  6 times more patches. No pair: the lesson is the level pipeline, not
  the rasterizer.
- **Kit**: `Fps_controller`, extracted from `TinyMinecraft`'s player (move,
  jump, gravity, mouse look), with collisions against boxes instead of
  blocks (`Collide3d`); shared with TinyWolfenstein3d and TinyDoom. Both are
  phases 4 and 9 of
  [`plan_physics3d_teaching.md`](plan_physics3d_teaching.md), which
  builds `Collide3d` and the capsule controller
  (`playground3d/Character3d`) that this kit wants -- and whose own
  games are TinyPinball, TinyHalfLife2 and TinyPortal.

### 6. Flight and space

Flight Simulator (Bruce Artwick, subLOGIC, 1979-80; Microsoft's from
1982), Elite (above), Star Fox (Nintendo and Argonaut, 1993: the Super
FX chip, polygons on a SNES), Comanche (NovaLogic, 1992: voxel
terrain).

- **Toys**: TinyStarFox (DONE: `games3d/TinyStarFox.ml`), on rails: the
  ship flies forward by itself and the player dodges and shoots. On
  rails is two numbers instead of six -- how far down the canyon, and
  where across it -- which are exactly what `Track3d` hands out, so the
  canyon is the racing kit's ribbon with walls, flown over: the kit's
  third user, in a genre it was not designed for, and its bank rolls
  the whole canyon through a turn. The enemies are the 2D shmup kit's
  unaltered: `Path` (Galaga's curves) is where an enemy flies across the
  canyon's cross-section, `Shots` carries the bolts across it, so a 2D
  pattern becomes a 3D flight. TinyComanche (DONE:
  `games2.5d/TinyComanche.ml`, voxel terrain in the *2D* playground: a
  height map drawn column by column, front to back, a y-buffer; after
  Sebastian Macke's VoxelSpace explainer on GitHub; and
  `games3d/TinyComanche3d.ml`, the same island as `cached3d` triangles
  with a z-buffer, the teaching comparison, like the TinyDoom pair).
- **Kit**: `Heightmap` (DONE: `kits/heightmap/`, shared by the
  TinyComanche pair): a grid of heights, an island made up by
  diamond-square (not noise), the ground under a point, a line of
  sight, colors by height and slope; the triangles are the game's. Not
  yet: a chase camera (`Camera3d`) for a helicopter seen from behind.

### 7. Voxels and sandboxes

Infiniminer (Zachtronics, 2009), Minecraft (Markus Persson, 2009-).
Done: `games3d/TinyMinecraft.ml` (see `plan_minecraft_remaining.md` for
what's left).

- **Kit**: `Voxels`, from `TinyMinecraft`'s world: the block grid, exposed
  faces, the ray walk for picking -- when a second voxel toy (a
  TinyTeardown with destructible blocks?) needs it.

### 8. 3D platformers and the camera problem

Alpha Waves (Christophe de Dinechin, Infogrames, 1990, often called
the first 3D platformer), Super Mario 64 (Nintendo, 1996: the analog
stick, and a camera operated by a character, Lakitu, filming Mario),
Crash Bandicoot (Naughty Dog, 1996: a corridor, the camera behind).

- **Toys**: TinyMario64 (DONE: `games3d/TinyMario64.ml`, controls
  relative to the camera, a/d turning it, a drop shadow, coyote time,
  jump buffering, variable jump height; a few platforms, jumping,
  stars: a StarCollector3d with a real jump), TinyMarbleMadness (DONE:
  `games3d/TinyMarbleMadness.ml`, after Marble Madness (Mark Cerny, Atari,
  1984): a course of heights in an ASCII map, the nearly isometric
  `Camera3d.from_far`, the controls on the screen's diagonals (or the
  mouse as a trackball), a rolling ball's 5/7 g sin(a), marbles
  breaking when they fall too far, and a steelie pushing; Super Monkey
  Ball's tilting board left as an exercise).
- **Kit**: `Camera3d` (started: `playground3d/Camera3d.mli`, `behind`,
  `chase`, `cockpit`, `looking_down`, `from_far`, `orbit`, the smoothing
  `follow`, and `floor`/`sky`; used by TinyVirtuaRacing and TinyTron3d),
  the 3D `Camera2d`: `look_at` exists (the
  camera record), plus `chase` (behind the player, at a distance and
  height, smoothed like `Camera2d.follow`), `orbit` (the player turns
  the camera around the character, the mouse's `mdx`), and the
  camera's own collision (not going through walls: the hardest part,
  and why Super Mario 64 made the camera a character you can blame).
  Keren's GDC talk has no 3D counterpart this famous; John Nesky's
  "50 Game Camera Mistakes" (GDC 2014, the camera of Journey) is the
  closest.

### 9. Racing, from Mode 7 to polygons

F-Zero (Nintendo, 1990) and Super Mario Kart (1992): Mode 7, a SNES
mode rotating and scaling a flat tile map differently on each scan
line, which makes a flat plane look like a floor. Then polygons: Hard
Drivin' (Atari, 1989), Virtua Racing (Yu Suzuki, Sega AM2, 1992: flat-
shaded, 60 frames per second), Ridge Racer (Namco, 1993: textured).
And the console's own answer, Mario Kart 64 (Nintendo, 1996): a
polygon circuit whose karts are still drawings, one per viewing
angle, standing in it.

- **Toys**: TinyKart in Mode 7 (DONE: `games2.5d/TinyKart.ml`, in the 2D
  playground: a `Tilemap` track sampled row by row with a per-row scale
  into a 200x130 picture of characters drawn by `Sprite.pixels`, TinyWolfenstein
  turned sideways; the karts billboards, four drawings by the viewing
  angle; three laps against three computer karts, on the racing kit's
  `Topdown`, TinyMicroMachines' model), and TinyVirtuaRacing (DONE: `games3d/TinyVirtuaRacing.ml`, a stage on
  TinyOutRun's course, with the racing kit `kits/racing/`) --
  flat-shaded polygons are exactly `playground3d`'s look, so this may
  be the most satisfying 3D toy. Then TinyMarioKart64 (DONE:
  `games3d/TinyMarioKart64.ml`), the same `Topdown` model as TinyKart
  and TinyMicroMachines drawn a third way, and the one toy that mixes
  the two pictures: polygons for the circuit, the rails, the item
  boxes and the traffic, sprites (`billboard`, pixel art as quads on a
  plane facing the eye) for the karts, the trees, the bananas and the
  shells -- which is the question the N64 era actually answered, what
  to model and what to draw. With it the three things the picture
  cannot give: the powerslide and its mini-turbo, items handed out by
  place, and the rubber band (`plan_ai_teaching.md`'s example) -- and,
  on the ribbon, a circuit that climbs, leans into its one banked
  corner and throws you off a ramp near the crest, where the shadow
  staying on the boards is what says how high you are.
- **Kit**, the racing kit of `plan_games.md` in 3D: `Track3d` (DONE:
  `kits/racing/3d/Track3d.ml`, its own library beside `kit_racing`
  because it draws, and the 3D playground is virtual: a 2D game
  linking the racing kit would otherwise have to link a 3D backend
  too). A course is a handful of control points with a width, a height
  and a bank; a Catmull-Rom spline through them, resampled at even
  distances, gives segments of the same length, so "how far along" is
  a distance in world units. A game then says everything in (`s`,
  `offset`): `at`/`across` place a thing on the ribbon, `locate` is the
  way back from a point in the world, `strip`/`wall` draw it.
  TinyMarioKart64 drives on it; TinyVirtuaRacing is the obvious second
  user (its course is a `Road.t` and its ribbon is written out in the
  game). Still to come when a game needs them: the chase `Camera3d`
  (exists), a car (elm-physics's RaycastCar for the real thing; the 2D
  kit's bicycle model is enough for a toy), checkpoints, and a bank
  that *pulls* the car rather than only tilting the road.

### 10. Third-person action and fixed cameras

Alone in the Dark (Frédérick Raynal, Infogrames, 1992: 3D characters
over pre-drawn backgrounds, fixed camera angles, the start of survival
horror), Tomb Raider (Core Design, 1996).

- **Toys**: TinyTombRaider (DONE: `games3d/TinyTombRaider.ml`, a tomb,
  an idol, a chasm, a block to push, and a boulder). Its lesson is not
  the renderer but the *movement*, and it is the one character in this
  repository who has no velocity: every other one integrates an
  acceleration you steer each frame, and she has a vocabulary of moves,
  each a fixed number of frames over a fixed distance, committed the
  moment it starts (so its outcome is settled then too, which is why
  there is no collision detection in the file). That pays for the
  second half: because a running jump is always two squares, a room is
  a puzzle with a countable answer, and `tests/games` writes the whole
  route down, move by move, as the level's answer. The textures are the
  era's technique as well -- one texture *page* with a sub-rectangle per
  face, one textured square per square of wall, and the page crushed
  into a single 16-colour dithered palette, which is where the famous
  "random stone" speckle came from. Also the first 3D game to use a 2D
  kit: `kits/puzzle`'s `Push`, the one TinySokoban uses, on the tomb's
  floor grid.
- **Toy**: TinyAloneInTheDark (DONE: `games3d/TinyAloneInTheDark.ml`), a
  house of four rooms on a `Tilemap`, each with its camera bolted in a
  corner: when you cross a doorway the shot *cuts*, with no smoothing at
  all. The first camera here that follows nobody. The cut is placed past
  the doorway, never in it -- a doorway belongs to no room, so standing
  on the threshold keeps the shot you came in with (a hysteresis, or it
  flickers). And the cuts are why the game has *tank controls*: up
  walks the way Carnby faces, not up the screen, because a control tied
  to the screen reverses at every cut. Carnby and the creature are the
  brawler kit's `Skeleton` (TinyVirtuaFighter's): a walk is two key
  poses, a zombie a pose with its arms out. Two things the first
  version got wrong and the frames showed: a camera above the walls of
  a roofless house sees every room at once, and a corner camera needs a
  wide lens to see its own room -- 60 degrees leaves a third of it
  out.

### 11. 3D fighting

Virtua Fighter (Yu Suzuki, Sega AM2, 1993: the first 3D fighting game,
flat-shaded, like Virtua Racing), Tekken (Namco, 1994).

- **Toy**: TinyVirtuaFighter (DONE: `games3d/TinyVirtuaFighter.ml`),
  two box-figures on a raised ring, best of three rounds, and a round
  won by knocking the other *off* it -- Virtua Fighter's own rule, and
  the one thing a wall-bounded 2D fighter cannot have. The rules are
  games/TinyStreetFighter's, unaltered and out of the same kit
  (`Frame_data`, `Hitbox`): what changed is what a character *is*.
- **Kit**: `Skeleton` (DONE: `kits/brawler/3d/Skeleton.ml`, its own
  library beside `kit_brawler` because it draws): a figure as a tree of
  boxes with joint angles, poses interpolated between keyframes, and
  the hierarchical transforms three dimensions need -- a forearm is
  built, bent, and only then turned by its upper arm, so a shoulder
  moves the hand without the hand knowing. Its proportions are
  Stickman's, so the two are the same fighter drawn twice. A move's
  keyframes are its own frame data, which is what keeps the fist out
  exactly while the move can hit. Not yet: a sidestep (VF2's, which
  needs hitboxes with a width), throws, a replay camera.

### 12. 3D puzzles

Blockout (1989: Tetris in a 3D pit), Monument Valley (ustwo, 2014:
impossible architecture, an orthographic camera making far and near
paths connect).

- **Toys**: TinyBlockout (DONE: `games3d/TinyBlockout.ml`, Tetris down
  a well -- the pit an array of cols x levels x rows, a layer full when
  its cols x rows cells are, and a quarter turn nothing but the piece's
  bounding box turned: integers, no trigonometry, and four turns about
  any axis the identity, which `tests/games` checks for every piece.
  Not `games/Tetris.ml`'s logic in the end: that came from elm-flatris
  and is written around a 2D grid, so the three rules here were shorter
  re-derived. The lesson is that the rules are the easy half -- most of
  the file is *depth cues*, since from up there you cannot tell how
  deep anything is: the camera just above the mouth so the near walls
  are wide, cubes drawn darker the deeper they lie, and the ring of the
  well lit at the level the piece will land on. It has to be the ring,
  and that is the nice part: a game seen from the side can draw a drop
  shadow under the falling piece and one seen from straight above
  cannot, because the shadow is always exactly behind the thing casting
  it), TinyMonumentValley (DONE:
  `games3d/TinyMonumentValley.ml`, and it brought the orthographic
  camera with it: `Playground3d.camera` gained an `ortho` field --
  the height of the view in world units instead of a `fov`, and no
  divide by the depth at all -- with `Camera3d.orthographic` to set
  it, `Mat4.orthographic` for the GPU backends, and a diagram of the
  two kinds of camera in `graphics/3d/geometry/Camera.mli`.

  The game is one rule and it is the camera's: with the view direction
  (1, 1, 1) and no perspective, the points (0, 0, 0) and (3, 3, 3) are
  drawn on the same pixel, so two blocks that far apart *look*
  adjacent -- and `connected` lets the figure step between any two
  blocks that look adjacent, which is Escher's staircase. A piece that
  turns decides which of those lies is currently being told. It is the
  opposite lesson to `games2.5d/TinyZaxxon.ml`, which has the same
  ambiguity and spends a shadow on removing it; here it is the
  material. Four tests, two golden frames.

  It also cost the 3D native loop a gap it had had all along: it never
  produced `mclick` (only `mdown`), so no 3D game could be played by
  clicking, and `-script`'s `at(x;y)`/`click` reached the 2D loop only.
  Both are fixed in `Native_loop_3d`.

### 13. The local arena: one screen, one hit, four players

Bomberman (Hudson Soft, 1983) set the shape and the last decade filled
it in: Samurai Gunn (Beau Blyth, 2013), TowerFall (Matt Thorson, 2013),
Duck Game (2014), Boomerang Fu (Cranky Watermelon, 2020). One screen,
one hit, a round over in twenty seconds, three friends on the sofa.
They are in *this* plan and not `plan_games.md`'s because the modern
ones are drawn in 3D -- low-poly, flat-shaded, a fixed high camera --
while being, underneath, exactly the 2D arena game Bomberman was.

- **Toy**: TinyBoomerangFu3d (DONE: `games3d/TinyBoomerangFu3d.ml`),
  four foods, a boomerang each. Its lesson is a design one, and it is
  the cleanest example in `games3d/` of a whole game falling out of a
  single rule: **your only weapon leaves your hand**. Throw and you are
  unarmed for the second it takes to come back, with only the dash;
  hold it and the dash becomes a slash that kills, but at arm's length
  only. Everything else is a consequence -- the arena is small enough
  that someone unarmed can always be reached, the return leg steers at
  a *moving* owner (so you may throw and run), the way out cuts
  everyone and the way back is the owner's catch, and a throw turns on
  the one who threw it once it has got away and come off the fence.
  Two things the 3D costs and pays for: the camera cannot follow
  anybody (a party game shows one screen to four people, which is *why*
  these arenas are one screen big), and at that fixed, nearly
  isometric angle nothing that leaves the ground can be placed, so
  every flying thing drags a shadow, as TinyMario64's does.
- **Kit**: none. A second arena party game (a TinyTowerFall with arrows
  to catch, a TinyDuckGame) would want what `kits/` has not got yet: a
  round-and-score layer (`Rounds`?), which half of `games/` writes out
  by hand.
- **The computer**: written in the game, not from `ai/` -- see
  [`plan_ai_teaching.md`](plan_ai_teaching.md), where it is now listed
  as a waiting user of the unwritten `Steering` and `Fsm`. Two numbers
  there are worth keeping in mind for whoever writes those: computer
  players that throw the frame they have a line kill a human in a
  second and a half (so they must hesitate), and a "pick the first
  clear way" walker wedged between two obstacles oscillates for ever
  (so it must keep last frame's way while that stays clear).

### Isometric: 3D worlds drawn in 2D

Zaxxon (Sega, 1982), Q*bert (Gottlieb, 1982), Marble Madness (1984),
Knight Lore (Ultimate Play the Game, 1984: the "Filmation" engine).
Not 3D rendering at all: a projection of a 3D grid to 2D with shapes
sorted back to front, so they belong in the 2D playground; listed here
because players see them as 3D.

- **Toy**: TinyZaxxon (DONE: `games2.5d/TinyZaxxon.ml`), and it went
  to `games2.5d/` rather than `games/` because that directory's rule
  is "a 3D look on the 2D playground, with the trick written out in
  the game" -- which an isometric projection is. It is the oldest and
  the smallest trick there (two lines and a sort, 33 of its 404), and
  the fourth family of `games2.5d/README.md` next to cell by cell, row
  by row and column by column: **object by object**.
- **What it alone has to deal with**: its world keeps the same
  restriction as the rest of `games2.5d/`, one height per point (a
  wall is blocks on the floor, and nothing is ever above you), but the
  *player* has a height where the others fix the eye at one level --
  and a projection that keeps no depth cannot say what that height is.
  Hence the shadow: every flying thing drawn twice, and the gap
  between the two *is* the altitude. Every isometric game since
  inherits both the problem and the answer.
- **Second toy**: TinyDiablo (DONE: `games2.5d/TinyDiablo.ml`,
  Blizzard North, 1996), which is what paid for the kit. Its own
  subject is not the view but what the view makes possible: **the
  mouse**. A click is not a direction but a place, and turning the
  pixel into a place is the projection run backwards
  (`Isometric.ground`, a 2x2 determinant that exists only because
  there is no perspective); then A* from `ai/Pathfind` walks there.
  Next to `games/TinyRogue.ml` it is the same dungeon with the turns
  taken out -- monsters on their own clock, a health orb rather than a
  number, loot on the floor -- which is the whole difference between
  the roguelike and the action RPG.
- **Kit** (DONE): `kits/isometric/` -- the two lines of the
  projection, the shadow that gives back the height they throw away,
  the back-to-front sort, the line of sight (what stands between a
  thing and the eye) and the inverse under the mouse. TinyZaxxon was
  rewritten onto it, which is the only way to know a kit is real: its
  six tests and its three golden frames did not move.

### 14. The roguelite run: Hades

Rogue (1980) made the dungeon, Diablo (1996) took the turns out of it,
and Hades (Supergiant, 2020) changed what death is worth. The three
are in this repository as one line, and the difference between them is
one line of the model:

| game | a death is |
|---|---|
| `games/TinyRogue` | final; the dungeon is forgotten |
| `games2.5d/TinyDiablo` | the end of a life; the character keeps what it carried |
| `games3d/TinyHades` | the end of a *run*, and it pays for the next one |

- **Toy**: TinyHades (DONE: `games3d/TinyHades.ml`, arrows to move,
  space to strike, x to dash, 1/2/3 for the boon between chambers).
- **Why `games3d/` and not `games2.5d/`**: Hades is drawn in 2D from a
  fixed angle, like Diablo, so the *honest* twin of TinyDiablo would
  be that same dungeon with a camera over it. This is not that -- it
  shares no world with it -- and it sits here to be the same genre
  with the engine doing the drawing: a camera, triangles and a
  z-buffer, where TinyDiablo has two lines of arithmetic and a sort.
  Read side by side, `chamber_camera` is exactly what
  `kits/isometric`'s two lines do by hand.
- **What it teaches**, and neither of the other two does:
  - **the run loop**: the chamber is the unit, the boon is the choice
    between chambers, and `kept` is what a death is worth -- the only
    game here where losing is progress;
  - **a boon is a number**: `take_boon` changes `damage`, `reach`,
    `dash_wait` or `max_hp` for the rest of the run, so a run is a
    little machine built out of whatever the gods offered. Boons that
    *combine* (fire that spreads, dashes that strike) are the
    exercise, and `strike` is where they would go;
  - **the dash with invulnerability**: eight frames in which nothing
    lands, which turns every fight from a question of position into a
    question of timing. That is Dark Souls' roll (2011) and everything
    after it; compare `games3d/TinyBoomerangFu3d.ml`, whose dash is
    only speed.
- **Kit**: none. An arena is a floor and four walls, and there is no
  second game to share one with yet.

## Infrastructure all the games need

- The 2D plan's list (scripted inputs for golden frames, sprites,
  scenes, pure randomness) applies here too; `tests/3d/` already has
  golden frames for the 3D examples and StarCollector3d.
- **Performance on the software backend**: the toys' budgets, measured
  with `-debug`'s stats line; fog and frustum culling
  (`plan_3d_remaining.md` section 5) for the bigger ones.
- **The web**: WebGL's `Mesh_cache` (`plan_webgl_remaining.md` section
  1) before any big `cached3d` world runs in the browser.
- **An orthographic camera** (for isometric-looking 3D and puzzles).
- **3D physics** ([`plan_physics3d_teaching.md`](plan_physics3d_teaching.md)):
  today each game does its own (a box one axis at a time in
  TinyMinecraft and TinyMario64, a ball on a height map in TinyMarbleMadness),
  and keeps it -- the engine arrives beside them behind a
  `physics=engine` flag, as in 2D.

## Ordering

1. TinyBattlezone: the easiest 3D game (wireframes, `project`), and
   the start of the history.
2. The raycaster pair, TinyWolfenstein (2D) and TinyWolfenstein3d: the best lesson
   on what 3D rendering adds, and the `Fps_controller` layer out of
   `TinyMinecraft`'s player with its second user.
3. `Camera3d` with TinyMario64 and TinyMarbleMadness: the camera problem
   (both DONE).
4. The racing kit in 3D with TinyVirtuaRacing (and TinyKart in Mode 7
   next to `plan_games.md`'s TinyOutRun) (both DONE).
5. `Heightmap` with TinyComanche and TinyComanche3d (DONE), TinyStarFox;
   `Sectors` with TinyDoom and TinyDoom3d (DONE).
6. Descent with TinyDescent and TinyDescent3d, on the segments kit
   (DONE); TinyQuake after it.
7. The rest as they come: fighting with the 2D brawler kit, puzzles,
   TinyElite (DONE, in `games2.5d/`).
