Pseudo-3D games ("2.5D")
========================

The games of this directory look 3D, but they run on the *2D*
playground: no camera, no z-buffer, no triangles, only rectangles and
polygons on a flat screen. Each one draws its world with the trick a
real game of the late 1980s or early 1990s used to fake 3D on hardware
that couldn't do it. The trick is written out in the game itself, as
the original did it, not hidden in an engine: its header explains it
with a diagram, and its code is short enough to read in one sitting.

"2.5D" because each of these worlds is really a flat map with one
height per point, or no height at all: a grid of cells you stand in the
middle of (Dungeon Master), a floor plan (Doom), a grid of walls
(Wolfenstein), a map of tiles (Mode 7), a height map (Comanche), a road
given as a list of curves (Out Run). Nothing can be above something
else, and that restriction is what makes each trick possible.

The `Lines` column counts the section of the game that *is* the trick
(each one is marked in its file: `grep "the trick of this game"
games2.5d/*.ml`) against the whole game.

| Game | After | Trick | Drawn by | Lines | What the trick can't do |
| --- | --- | --- | --- | --- | --- |
| `TinyDungeonMaster` | Dungeon Master (FTL, 1987) | the hero stands in the middle of a cell facing one of four ways, so every cell in view has one fixed place on the screen: nested frames, filled in farthest first | cell (a square and a trapezoid) | 24 / 624 | anything off the grid: a diagonal, a step, standing between two cells, looking up |
| `TinyOutRun` | Out Run (Sega, 1986) | the road as a list of segments, their edges projected, each slice a trapezoid; a curve, each segment shifted sideways a bit more; a hill hides what's behind (a segment drawn only if it rises above the nearer ones) | slice of road | 23 / 250 | a world beyond the road: a track that crosses itself, a free camera |
| `TinyWolfenstein` | Wolfenstein 3D (id, 1992) | a ray cast per column through a grid of walls (DDA); the distance gives the wall's height | column | 61 / 306 | walls at an angle, heights, floors |
| `TinyKart` | Super Mario Kart (Nintendo, 1992), the SNES's Mode 7 | a flat map, turned, sampled row by row, each row at its distance: height * focal / rows below the horizon | row | 84 / 441 | walls, hills: only a flat floor |
| `TinyDoom` | Doom (id, 1993) | a BSP tree of the level's walls, walked nearest first; each column's clip arrays say what's left to draw | column | 300 / 499 | rooms above rooms, looking up or down, walls that aren't vertical |
| `TinyComanche` | Comanche (NovaLogic, 1992), "Voxel Space" | a height map, each column a line across the map, near to far; a y-buffer, what's drawn of each column | column | 126 / 302 | overhangs, caves, a roll |
| `TinyDescent` | Descent (Parallax, 1995) | the level a graph of convex cells; from the eye's cell, each portal shows the next cell inside its own window on screen; drawn farthest first | cell (polygons) | 216 / 440 | nothing, really: it is full 3D, and pays for it with a projection per corner |

`TinyDescent` is the odd one out, and on purpose: it is not 2.5D at
all. Its ship flies in every direction and rolls, its world is a real
3D graph of cells, and the projection, the near-plane clipping and the
polygon clipping are all written in the game. What it still doesn't
have is a depth per pixel: the order the cells come out in does that
work. It sits here because it is the end of the line this directory
draws -- how far a game can get on the 2D playground, drawing polygons
in the right order -- and because it is Doom's "nearest first" idea,
grown up.

Three families, then:

- **Cell by cell**, the grid: Dungeon Master. If the eye is always at a
  cell's center and looks along an axis, there is a small, fixed set of
  cells it can see, and each one has one place on the screen -- so the
  view is a set of pictures, worked out (or, in 1987, painted) once and
  for all. The oldest trick here, and the one that needs no arithmetic
  per frame at all; it is also the most restrictive, because the moment
  the hero stands between two cells, or turns by anything but a quarter
  turn, none of the pictures fit any more.
- **Row by row**, the floor: Mode 7. A screen row below the horizon
  sees the ground at one distance, so everything on that row is scaled
  the same. The SNES did it in hardware, a register changed between two
  rows. Out Run is its cousin, by slices of road instead of rows: a
  slice is at one distance too, so it's a trapezoid.
- **Column by column**, the walls: Wolfenstein, Doom, Comanche. A
  screen column is one direction from the eye, so drawing the nearest
  thing first and remembering how much of the column is still empty
  (nothing in Wolfenstein, where one wall fills the column; the clip
  arrays in Doom; the y-buffer in Comanche) hides what's behind without
  a depth per pixel. Out Run's hills are the same idea for the whole
  screen: one number, the highest row drawn so far.

They read best in order: TinyDungeonMaster, where the view is a fixed
picture and there is nothing to compute; then TinyWolfenstein, which gives up
the fixed pictures for a ray per column and can then stand anywhere and
look anywhere; then TinyDoom, which does TinyWolfenstein's columns with walls
at any angle and heights; TinyKart, TinyWolfenstein turned sideways (a line per
row across the floor, where TinyWolfenstein has a ray per column); TinyComanche,
Doom's "nearest first" with the order given by the distance, no tree;
TinyOutRun, Mode 7 for a road that isn't a map.

The `Lines` column has a lesson of its own, and it is not the one you
would guess: the simplest trick belongs to the longest game. The other
games here are locomotion -- you drive, you walk, you fly, and nearly
every line of them draws -- while Dungeon Master's renderer is a page
and the rest of the file is the *game*: a key, a door, a lever, a
portcullis, monsters on their own clock, a torch burning down, and a
panel telling you about all of it. That is the right shape for it.
Dungeon Master's innovation was never in the pixels; it was real time
and a world you reach into with the mouse. A trick being cheap to draw
is not the same as a game being small.

Their twins
-----------

Five of them have a twin in `games3d/`, the same game with a real 3D
engine (playground3d: triangles, a camera, a z-buffer), for comparison:

| Pseudo-3D | Real 3D | Shared |
| --- | --- | --- |
| `TinyWolfenstein` | `games3d/TinyWolfenstein3d` | the map (a copy, a `Tilemap`) |
| `TinyOutRun` | `games3d/TinyVirtuaRacing` | the course and the car (`kits/racing`: `Road`, `Car`) |
| `TinyDoom` | `games3d/TinyDoom3d` | the level (`kits/sectors`: `Sectors`) |
| `TinyComanche` | `games3d/TinyComanche3d` | the island (`kits/heightmap`: `Heightmap`) |
| `TinyDescent` | `games3d/TinyDescent3d` | the mine and the ship (`kits/segments`: `Segments`, `Sixdof`) |

The 3D twin is shorter: the engine does the work, and the camera can do
anything. The pseudo-3D one shows what the engine does, and why games
could run in 1992 without one. TinyKart's model is shared the other way,
with the top-down `games/TinyMicroMachines` (`kits/racing`: `Topdown`):
the same race, seen from above.

`TinyDungeonMaster` is the one that gets no twin, and for a reason
worth knowing: the pairs above share a *world* and differ only in how
it is drawn, so putting the same level in front of a real camera is a
fair comparison. The grid view is not like that. It is not a way of
drawing a dungeon that an engine would do better -- it is a rule about
where the hero may stand and which way it may look, and the game is
built on that rule: cell steps, quarter turns, and fights that are
footwork on a chessboard. Take the rule away and give the camera the
freedom it wants, and nothing of the game survives; what is left is
TinyWolfenstein3d with a smaller map.

Running them
------------

```bash
dune exec games2.5d/TinyDoom.exe            # native, Cairo
dune exec games2.5d/software/TinyDoom.exe   # the from-scratch 2D rasterizer
```

and in a browser, `games2.5d/js/` (see the main README). On the software
backend, run with `-debug-keys` and press `r` to draw at a half, a third
or a quarter of the resolution (`graphics/core/Pixelate.mli`): the look
of 320 x 200 screens, and a lot faster.

See also `docs/claude_notes/plans/plan_games3d.md` (sections 1 to 6)
for the history and references of each genre.
