Pseudo-3D games ("2.5D")
========================

The 2.5D games look 3D, but they run on the *2D*
playground: no camera, no z-buffer, no triangles, only rectangles and
polygons on a flat screen. Each one draws its world with the trick a
real game of the 1980s or early 1990s used to fake 3D on hardware
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
games/*/*.ml`) against the whole game.

| Game | After | Trick | Drawn by | Lines | What the trick can't do |
| --- | --- | --- | --- | --- | --- |
| `TinyZaxxon` | Zaxxon (Sega, 1982) | two lines of projection (`x` across the fortress, `z` along it, `y` up), then everything sorted back to front; the altitude of a thing is the gap on screen between it and its shadow | object (a shape, and its shadow at y = 0) | 54 / 478 | tell you how high anything is without drawing it twice; turn, tilt or look from anywhere else (one fixed angle, for ever); two things whose order the sort cannot settle |
| `TinyDiablo` | Diablo (Blizzard North, 1996) | the same two lines as `TinyZaxxon`, from the kit now (`gamekits/isometric`), on a grid of diamond tiles; and the two lines run *backwards*, which is what turns a mouse click into a place in the dungeon | tile, and one cube per wall | 0 / 567 (the trick is the kit's) | tell you what is under a pixel without the floor being flat; a room above a room; anything the sort cannot order (a long wall against a body inside it) |
| `TinyDungeonMaster` | Dungeon Master (FTL, 1987) | the hero stands in the middle of a cell facing one of four ways, so every cell in view has one fixed place on the screen: nested frames, filled in farthest first | cell (a square and a trapezoid) | 24 / 624 | anything off the grid: a diagonal, a step, standing between two cells, looking up |
| `TinyOutRun` | Out Run (Sega, 1986) | the road as a list of segments, their edges projected, each slice a trapezoid; a curve, each segment shifted sideways a bit more; a hill hides what's behind (a segment drawn only if it rises above the nearer ones) | slice of road | 23 / 250 | a world beyond the road: a track that crosses itself, a free camera |
| `TinyGuitarHero` | Guitar Hero (Harmonix, 2005) | Out Run's road straightened: one division by the depth for every point of the highway, the gems shrinking and the frets closing in towards the horizon; the lines across it, one a beat, are all that makes it move | point (a lane edge, a gem, a beat line) | 23 / 351 | anything off the highway: it has no world, only a road that the music scrolls |
| `TinyWolfenstein` | Wolfenstein 3D (id, 1992) | a ray cast per column through a grid of walls (DDA); the distance gives the wall's height | column | 61 / 306 | walls at an angle, heights, floors |
| `TinyMarioKart` | Super Mario Kart (Nintendo, 1992), the SNES's Mode 7 | a flat map, turned, sampled row by row, each row at its distance: height * focal / rows below the horizon | row | 99 / 511 | walls, hills: only a flat floor |
| `TinyDoom` | Doom (id, 1993) | a BSP tree of the level's walls, walked nearest first; each column's clip arrays say what's left to draw | column | 300 / 499 | rooms above rooms, looking up or down, walls that aren't vertical |
| `TinyComanche` | Comanche (NovaLogic, 1992), "Voxel Space" | a height map, each column a line across the map, near to far; a y-buffer, what's drawn of each column | column | 126 / 302 | overhangs, caves, a roll |
| `TinyShufflePuck` | Shufflepuck Café (Christopher Gross, Brøderbund, 1988) | one plane, one eye that never moves: a point of the table drawn at `f x / depth` across and `horizon - f h / depth` up, its size `f / depth`; and the same division backwards for the mouse | object (a disc on the table, or a person behind it) | 22 / 445 | anything not on the table's plane; the eye moving -- then it is Mode 7 |
| `TinyBattlezone` | Battlezone (Ed Rotberg, Atari, 1980) | every object a list of segments; each end taken into the eye's coordinates (three dot products), cut at a plane just ahead of the eye, and divided by its depth | segment | 62 / 420 | hide anything: every edge is drawn, and you see through tanks, pyramids and mountains alike |
| `TinyElite` | Elite (Braben and Bell, 1984) | you never move: the universe turns round you, each ship's orientation three vectors turned by `sin a ~ a`, `cos a ~ 1 - a^2/2` and straightened every 16 frames; a convex hull, an edge drawn when either of its faces is turned towards you | edge | 144 / 606 | hide one ship behind another (they show through each other); a ship that isn't convex; anything filled |
| `TinyDescent` | Descent (Parallax, 1995) | the level a graph of convex cells; from the eye's cell, each portal shows the next cell inside its own window on screen; drawn farthest first | cell (polygons) | 216 / 440 | nothing, really: it is full 3D, and pays for it with a projection per corner |

`TinyDescent` is the odd one out, and on purpose: it is not 2.5D at
all. Its ship flies in every direction and rolls, its world is a real
3D graph of cells, and the projection, the near-plane clipping and the
polygon clipping are all written in the game. What it still doesn't
have is a depth per pixel: the order the cells come out in does that
work. It sits here because it is the end of the line these games
draw -- how far a game can get on the 2D playground, drawing polygons
in the right order -- and because it is Doom's "nearest first" idea,
grown up.

`TinyBattlezone` and `TinyElite` are the other two, older and with no
order at all. Battlezone is where 3D games start: the whole pipeline
in its smallest form -- into the eye's coordinates, clip, divide --
and no hiding of any kind, which a vector monitor, drawing lines and
no surfaces, never needed. Elite, four years later, is the same
pipeline and one step more. Its engine is as full 3D as Descent's --
six degrees of freedom, a divide by the depth, a near plane -- but it
draws only lines, and each ship hides its own back edges by itself: a
face turned away is one dot product, and a convex hull cannot hide
anything else from itself. Between two ships nothing is hidden, and on a vector look
nobody minds.

Four families, then:

- **Object by object**, the projection: Zaxxon, and every isometric
  game after it. The oldest trick here and the smallest -- two lines
  that turn three world axes into two screen ones, and a sort. Its
  world keeps the same restriction as the others, one height per point
  (a wall is blocks on the floor; there is nothing to fly under), but
  it is the one where the *player* has a height and must judge it,
  where the rest put the eye at a fixed level and give the world all
  the geometry. That one number is exactly what the projection throws
  away -- so every flying thing is drawn twice, once where it is and
  once as a shadow on the ground, and the gap between them is the
  altitude. Q*bert, Knight Lore, Populous and Diablo are all this
  trick. `TinyZaxxon` and `TinyDiablo` are the two of them here, and
  between them they use the projection both ways round: Zaxxon draws
  with it, Diablo also runs it backwards, because a click is a pixel
  and the game needs a place. That inverse exists only because there
  is no perspective to divide by, and it is why every game that looks
  like this is played with a mouse.
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
  slice is at one distance too, so it's a trapezoid. And Guitar Hero's
  highway is Out Run's road with the curves and the hills taken out:
  straight, it needs no slices at all, only the one division per point.
- **Column by column**, the walls: Wolfenstein, Doom, Comanche. A
  screen column is one direction from the eye, so drawing the nearest
  thing first and remembering how much of the column is still empty
  (nothing in Wolfenstein, where one wall fills the column; the clip
  arrays in Doom; the y-buffer in Comanche) hides what's behind without
  a depth per pixel. Out Run's hills are the same idea for the whole
  screen: one number, the highest row drawn so far.

They read best in order: TinyZaxxon, two lines and a sort, where
nothing is hidden and the only hard part is knowing where you are;
TinyDiablo, the same two lines with a dungeon on them and a mouse
putting things back through them; then TinyDungeonMaster, where the view is a fixed
picture and there is nothing to compute; then TinyWolfenstein, which gives up
the fixed pictures for a ray per column and can then stand anywhere and
look anywhere; then TinyDoom, which does TinyWolfenstein's columns with walls
at any angle and heights; TinyMarioKart, TinyWolfenstein turned sideways (a line per
row across the floor, where TinyWolfenstein has a ray per column); TinyComanche,
Doom's "nearest first" with the order given by the distance, no tree;
TinyOutRun, Mode 7 for a road that isn't a map; TinyGuitarHero, the
same road straightened, where what moves down it is the music.

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

Seven of them have a 3D twin, the same game with a real 3D
engine (playground3d: triangles, a camera, a z-buffer), for comparison:

| Pseudo-3D | Real 3D | Shared |
| --- | --- | --- |
| `TinyWolfenstein` | `TinyWolfenstein3d` | the map (a copy, a `Tilemap`) |
| `TinyOutRun` | `TinyVirtuaRacing` | the course and the car (`gamekits/racing`: `Road`, `Car`) |
| `TinyDoom` | `TinyDoom3d` | the level (`gamekits/sectors`: `Sectors`) |
| `TinyComanche` | `TinyComanche3d` | the island (`gamekits/heightmap`: `Heightmap`) |
| `TinyDescent` | `TinyDescent3d` | the mine and the ship (`gamekits/segments`: `Segments`, `Sixdof`) |
| `TinyBattlezone` | `TinyBattlezone3d` | the battle: obstacles, enemy, shells (a copy) |
| `TinyElite` | `TinyElite3d` | the ships, their small turns and the flight (a copy); the hidden lines become the engine's backface culling |

The 3D twin is shorter: the engine does the work, and the camera can do
anything. The pseudo-3D one shows what the engine does, and why games
could run in 1992 without one. TinyMarioKart's model is shared the other way,
with the top-down `TinyMicroMachines` (`gamekits/racing`: `Topdown`):
the same race, seen from above. And TinyGuitarHero's highway, drawn by
hand here, is drawn by a camera in `TinyRockBand`, four of them
side by side for a whole band (`gamekits/rhythm`: the clock, the charts,
the difficulty): not a twin, the next game.

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
dune exec games/fps/TinyDoom.exe            # native, Cairo
dune exec games/fps/software/TinyDoom.exe   # the from-scratch 2D rasterizer
```

and in a browser, `games/<genre>/web/` (see the main README). On the software
backend, run with `-debug-keys` and press `r` to draw at a half, a third
or a quarter of the resolution (`graphics/core/Pixelate.mli`): the look
of 320 x 200 screens, and a lot faster.

See also `docs/claude_notes/plans/plan_games3d.md` (sections 1 to 6)
for the history and references of each genre.
