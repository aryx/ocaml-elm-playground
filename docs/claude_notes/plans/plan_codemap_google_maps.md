# Plan: the code map as a Google Map for code

## Context

The author (2026-09-28): "What I want is a 'Google map' for code. I tried
already a bit with my ~/codemap/, where at the high level we would not
render the code content but just the column hint (so you appreciate the
amount of code), and the filename, and as you zoom in show in higher font
the most important functions (based on #uses), most important types,
fields, etc, semantic code highlighting. A big criteria like for Google
maps is to not clutter the display ... find the right balance of showing
useful info without cluttering." And: "it might be useful to sketch some
possible rendering of codemap on some imaginary codebase to better feel
the appropriate design".

Where we are: tinybox's code map (`launcher/codemap/`) lays a repository
or a directory out as an ordered treemap under a camera; from afar each
file is SeeSoft's picture (a pixel per character, its category's colour),
directories' names big and faint, files' names on tabs, and definitions
written over their files, bigger for a function than for a local
(`Highlight_code.emphasis`, the category alone), placed greedily, the most
important first, none over another (`Code_map.labels`, `place`); up close
the code itself, readable, with its names lit and clickable
(`plan_codemap_naming.md`, levels 1 to 3).

The target (the author): "something useful for codebases like
~/principia, ~/xix, ~/ix, and this playground, not necessarily really big
codebases like the Linux kernel". So from a few thousand lines to about a
million, a few thousand files at most:

| codebase | files | lines | languages |
|---|---|---|---|
| ~/ix | 1,462 | 162k | OCaml, 80 C files |
| ~/xix | 1,295 | 128k | OCaml, 384 C files (a kernel, OCaml's runtime) |
| this playground | 2,230 | 294k | OCaml |
| ~/principia | 3,972 | 1,083k | C (Plan 9), some OCaml |

(OCaml and C files, measured 2026-09-28; principia's map shows 3,654 of
them, its .codemapignore and links to directories left out.)

At that size everything can be computed exactly and once, when the map is
made: every name resolved, every definition's uses counted, every label's
minzoom decided -- no sampling, no server, no precomputed tile store.
What a street map needs its tiles for (a planet does not fit in memory),
we do not; what it does with them (a feature's minzoom, placement with
collision, stability across zoom) we keep. Principia is the one whose
opening must be made faster (15 to 20 s today, all its files parsed
before the first picture: see "Speed" below).

What is missing is the map's *judgement*: which few things to show at each
zoom so that the screen says the most with the least. The author's rule,
first of all: "the most important is useful semantic info and no
cluttering".

And the reader it is for (the author): "with the teaching context of the
playground and tinybox! make it easier for the user to explore the code of
the games and apps". The map is how a learner, having played TinyMario in
tinybox, finds how it is written: its own file first, the kits it stands
on around it, the one trick worth reading marked like a landmark. See
"For the learner" below.

The author's answers (2026-09-28): five zoom levels; the zooming smooth,
the display changing continuously as you zoom (as a street map does);
from his codemap, keep the files' colour by role (archi_code) and the
directories' and files' names shown.

An interactive mock-up of this plan on an imaginary codebase (`harbor`,
84,000 lines), for feeling the design before building it:
https://claude.ai/artifact/C3cAvjjJXEwQPVy51wtBZZ (the source is in
this session's scratchpad; the page computes the ordered treemap, the
levels, the column hints, the labels' minzoom placement and van Wijk's
smooth flights live). That is what a
street map is good at, and this plan borrows its methods.

## How a street map decides what to draw

What is published about vector map renderers (Mapbox GL and MapLibre
are open; Google's vector maps are described as working the same way in
their talks and patents, though their code is not public):

1. **Zoom levels, and a minimum zoom per feature.** The world is cut
   into tiles at discrete zoom levels (0 the whole earth, about 20 a
   house). Every feature carries a `minzoom`: a country's name from zoom
   2, a city from 4 to 10 by population, a street from 14, a house
   number from 17. The renderer draws a feature only above its minzoom:
   which features exist at a zoom is decided *once*, offline, not every
   frame.

2. **Cartographic generalization.** What is shown at a zoom is not the
   detail shrunk but a different drawing: selection (only big roads),
   simplification (a coastline with fewer points), aggregation (a town is
   a dot, not its streets), exaggeration (a motorway drawn wider than its
   true width, so it is seen). Each zoom answers its own question: which
   countries, which cities, which streets.

3. **Label placement with collision.** Labels are placed most important
   first (a rank: a capital before a village, a motorway before a lane);
   each takes a box in a collision index (a grid of cells), and a label
   whose box overlaps a placed one is not drawn. A label has a halo (a
   dark outline) so it reads over anything.

4. **Stability across zoom.** A label seen at zoom z is still there at
   z+1 unless it leaves the screen: the placement at a zoom starts from
   the labels of the zoom before, which keep their places. Nothing
   flickers while zooming, and a label appearing or leaving fades (about
   a quarter of a second) rather than popping.

5. **Constant density.** Whatever the zoom, the screen holds about as
   many labels (a few dozen): zooming in does not add clutter, it swaps
   big things for smaller ones. The limit is a budget per screen area,
   not per feature.

6. **Importance is data.** A city's rank is its population; a road's
   its class; a shop's its reviews. The rank is computed once, globally,
   and drives both the minzoom and the size of the label.

7. **The rest of the interface**: a search box that flies to a place and
   drops a pin; a card when a place is clicked (its name, its kind, its
   address, its photos); layers turned on and off (traffic, transit,
   terrain, satellite); Street View, the ground seen as it is; a scale
   bar; the URL of the view, shareable; and tiles made at each zoom and
   cached, so a zoom is instant.

## The same for code

| a map | the code |
|---|---|
| continents, countries | top directories, then subdirectories (regions of colour, `archi`) |
| a city, and its population | a definition, and its **uses** (fan-in: how many places name it) |
| a capital | a module's most used definition; `main` |
| streets | the top-level definitions of a file, in their order |
| districts | a file's sections (`(* Model *)`, codemap's CommentSection levels) |
| roads between cities | a file's uses of another (`Code_names`), a directory's of another |
| the land, from above | the column hint: a file's lines as bars, how much code, not what |
| satellite view | SeeSoft's picture: every character a coloured pixel |
| Street View | the code, readable (the map up close, or the file view) |
| a place card | a definition's card: kind, signature, doc comment, uses, callers |
| traffic | git churn (codemap's layer_vcs); later coverage, owners |

### Importance: a definition's population

The rank that drives everything is how much a definition is used, as a
city's is its population:

- **fan-in**: the number of names, in the map's files, that resolve to it
  (`Code_names` for other files, the resolvers' `binds` in its own); the
  other files' counting more than its own file's (a helper used 30 times
  in its own file is a local habit, a function used by 30 files is a
  capital);
- **its kind** as codemap weighed it (`Style.size_font_multiplier_of_categ`):
  a module and a type 5, a function 3.5, a global 3, a constant or a macro
  2, a field 1.7, a constructor 1.2 -- the same buckets of use as codemap's
  (`NoUse` 0.9 to `HugeUse` 3.3, on a log scale of the count);
- **a file's** rank: its definitions' ranks, and how many files use it;
  **a directory's**: its files'; so the three most important things of a
  directory are known without looking inside it.

Counting exactly needs every name resolved, across the map: level 3's
search for each reference. Cheap first: a global index of references by
name and namespace (and module for `M.x`), counted without the nearness
ranking; exact later, from the same search, done once in the background.

## The zoom levels

Five levels, by how big a line of code is on the screen (a map's zoom is
its scale); each has its question, and draws only what answers it.

| level | a line on screen | the question | drawn |
|---|---|---|---|
| Z0 the world | < 0.05 px | what are the parts, how big | top directories as regions of colour, their names and sizes (lines), the 3 capitals of the whole map |
| Z1 countries | 0.05 - 0.3 px | what is in each part | subdirectories' names; files as column hints (grey bars, a line's length each); each directory's 2-3 most used definitions, as cities |
| Z2 regions | 0.3 - 2 px | which files, what they are for | files' names on tabs; sections as districts; each file's most used definitions, sized by rank; types' names |
| Z3 streets | 2 - 6 px | where things are in a file | SeeSoft's colours arrive; every top-level definition, in its place, fields of the big types; the names bound across files (level 3) as faint roads when a file is hovered |
| Z4 the ground | >= 6 px | what the code says | the code, readable (today's), names lit on hover, clicked to their definitions |

Between levels, a feature fades in over its first quarter of a level, so
zooming is continuous but what exists is decided by the level.

### The budget

- about **40 labels** on the screen at any zoom (a map's density), in a
  collision grid of cells of 120 by 40 pixels, a label taking the cells
  its box covers;
- a directory's big faint name fades out as it grows past half the
  screen (you are in it: a country's name is not written over its own
  streets); its path stays as a tab (today's);
- the code's colours (SeeSoft) not before Z3: from afar they are noise,
  the column hint says the amount of code without it (the author's
  codemap);
- every label on a dark halo (today's tabs), in its kind's colour, its
  size its rank; the name's kind shown by its colour, not by a word.

### Stability

Each feature gets its **minzoom** once, when the map is made (and again
when files come in): the labels are placed level by level, from Z0 to Z4,
each level starting with the labels placed at the level before (kept, in
their places), then the new ones by rank, into the free cells. A label
then shows from its minzoom on, and zooming only ever adds labels in the
room the zoom makes; panning shows what was always there. No flicker,
nothing computed per frame but the drawing.

## Sketches: an imaginary codebase

`harbor/`, a small game engine and its games, 84,000 lines:

```
harbor/                         84,210 lines
  engine/      31,400   render/ physics/ audio/ input/ world.ml core.ml
  games/       22,800   racer/ puzzle/ shooter/
  libs/        18,100   json/ png/ zlib/ math/
  tools/        7,900   editor/ profiler/
  tests/        4,010
```

The most used definitions (fan-in): `Vec.add` 412, `World.t` 380,
`Render.draw` 260, `Json.parse` 95, `Physics.step` 88, `Racer.update` 12.

### Z0 the world (a line: 0.02 px)

```
+-------------------------------------------------------------------+
|                                    |                              |
|                                    |                              |
|        engine                      |       games                  |
|        31,400                      |       22,800                 |
|                                    |                              |
|    * World.t                        |                              |
|                    * Render.draw   |                              |
|                                    +--------------+---------------+
+--------------------------+---------+              |               |
|                          |         |   tools      |  tests        |
|        libs              |         |   7,900      |  4,010        |
|        18,100            |         |              |               |
|    * Vec.add             |         |              |               |
+--------------------------+---------+--------------+---------------+
  harbor: 5 parts, 84,210 lines                  1 cm = 10,000 lines
```

Five regions of colour (archi), five names, their sizes, three capitals
(the map's three most used definitions, a star each). No file, no code.
The eye learns the continents.

### Z1 countries (a line: 0.15 px), zoomed on engine/

```
+-------------------------------------------------------------------+
| render/                    | physics/               | audio/      |
|  ||||| |||| ||||| |||      |  |||| ||||| ||         |  ||| ||||    |
|  ||||| |||| ||||| |||      |  |||| ||||| ||         |  ||| ||||    |
|  |||   ||   ||||           |  ||   |||              |  |           |
|        * Render.draw       |      * Physics.step    |             |
|        * Mesh.t            |                        +-------------+
+----------------------------+------------------------+ input/      |
| world.ml   core.ml         |                        |  || |||     |
|  ||||||||  |||||           |                        |  || |       |
|  ||||||||  |||             |                        |             |
|  * World.t   * Vec.add     |                        |             |
+----------------------------+------------------------+-------------+
  engine/  31,400 lines   4 dirs, 2 files                harbor > engine
```

Files are only their **column hints**: each column of a file a grey bar
whose lines are as long as the code's, tinted by the region's colour (how
much code, and its shape: a long flat file, a file of short lines). The
subdirectories' names; each one's two most used definitions. The
breadcrumb says where you are.

### Z2 regions (a line: 1 px), zoomed on engine/render/

```
+-------------------------------------------------------------------+
| mesh.ml            | draw.ml                    | shader.ml        |
| ░░░░░░ ░░░░░ ░░░   | ░░░░░░░ ░░░░░░ ░░░░░░      | ░░░░ ░░░         |
|  MESH              |  DRAWING                   |   compile        |
|  type t            |  draw        (260 uses)    |   uniform        |
|  make              |  sprite                    |                  |
| ░░░░░░ ░░░░░       |  (* Batching *)            |                  |
|                    |  flush                     +------------------+
+--------------------+ ░░░░░░░ ░░░░░               | camera.ml        |
| texture.ml         |                            |  type t          |
|  load              |                            |  project         |
+--------------------+----------------------------+------------------+
  engine/render/  6,200 lines   draw.ml: 1,900 lines      1 cm = 300 lines
```

Files' names on tabs; the sections (`(* Batching *)`, codemap's levels:
a banner bigger than a subsection) as districts; each file's definitions
by rank, `draw` biggest (260 uses), a type's name (`type t`) in the
types' colour. Still no code colours: the column hints (the `░`) are grey.
A file's own helpers, used only inside it, not yet shown.

### Z3 streets (a line: 4 px), in draw.ml

```
+-------------------------------------------------------------------+
| draw.ml                                                    render/|
| ▓▓▒▒░░▓▓▒▒░▒▓▓  ▓▒▒░░▓▓▒░▒▓▓▓▒   ▓▓▒▒░▓▒░░▒▓▓▓▒▒░               |
|  (* Drawing *)   let draw ▓▒░▒▓    let sprite ▓▒░▒▓              |
| ▓▓▒░▒▓▓▒░░▒▓▓▒  ▓▓▒▒░▓▓▒░░▒▓▓▓   ▓▓▒▒░▓▒░░▒▓▓▓▒▒░               |
|  let batch ▓▒░   (* Batching *)    let flush ▓▒░▒▓               |
| ▓▓▒▒░░▓▓▒▒░▒▓▓  ▓▒▒░░▓▓▒░▒▓▓▓▒   ▓▓▒▒░▓▒░░▒▓▓▓▒▒░               |
|  type state =    let push_quad     let clip_rect                  |
|    { quads; tex; count }                                          |
|                                                                   |
|        hovered draw.ml:  -> mesh.ml  -> shader.ml  -> Vec (libs)  |
+-------------------------------------------------------------------+
```

SeeSoft's colours arrive (`▓▒░`, a character a pixel of its category's
colour); every top-level definition in its place, a type's fields; the
file hovered, its roads: faint lines to the files whose names it uses.

### Z4 the ground (a line: 16 px): today's

The code, readable; a name lit on hover, its definition pulsing; a click
to it, in this file or another; `b` back.

### A place card (hover or click at Z2 to Z4)

```
+------------------------------------------+
|  draw                    function        |
|  engine/render/draw.ml:212               |
|  val draw : Camera.t -> Mesh.t -> unit   |
|  "Queues a mesh for the frame; flushed   |
|   when the batch fills or the frame ends"|
|  260 uses in 41 files                    |
|  most from: racer/track.ml (32),         |
|    shooter/ship.ml (28), puzzle/board.ml |
+------------------------------------------+
```

Its kind, where, its signature (the .mli's `val`, or the C prototype),
its doc comment, its population, and where the uses come from (a click on
one flies there).

### Search (/)

```
  / dra_
    draw            function   engine/render/draw.ml     260 uses
    draw_text       function   engine/ui/text.ml          44 uses
    Drawing         section    engine/render/draw.ml
```

The definitions by name, ranked by population; Enter flies there and
drops a pin that stays until Escape.

## For the learner (tinybox)

tinybox's map opens on one program's code (`code=TinyMario`): its file,
its folder's modules and the kits it uses (Codemap's Own scope). What a
street map does for a visitor, the map does for a learner:

- **You are here**: the program's main file marked (today's yellow tab)
  and, at Z0 and Z1, its `main` and its `update` and `view` as the
  capitals -- a Playground program's three entry points, whatever their
  uses count;
- **The sights**: the "trick of this game" marks (Code_file.marks) as
  landmarks, shown from Z1, a star each, never left out by the budget;
- **A guided walk**: the tour (n, p: its headers, sections and tricks, in
  reading order) kept, the reading order's numbers on the tabs kept;
  flying from stop to stop by the smooth zoom instead of opening the file
  view;
- **Neighbourhoods**: the kits (`gamekits/`, `appkits/`) and the
  Playground as regions of their own colour round the program, their
  names at Z0, so a learner sees at once what is the game's own and what
  it borrows;
- **The card** says, for a definition of the Playground or a kit, where
  else it is used among the programs (TinyMario's `Tile_move` is also
  TinyLodeRunner's): the way from one game to the next.

## The rest of a street map, for later

- **Layers** (codemap's `layer_archi`, `layer_vcs`): git churn as heat
  (traffic), age, a golden test's coverage; each a colour over the
  column hints, one at a time, with its legend.
- **Roads** at Z1-Z2: the directories' dependencies as lines between
  regions, thicker the more names cross them; a highway is a module used
  by everyone (libs/math).
- **The URL**: `tinybox.html?code=harbor&at=engine/render/draw.ml:212`
  (the web's `code=` already opens a program's map).
- **A scale bar** ("1 cm = 300 lines") and **a minimap** of the whole
  map with the view's rectangle (codemap's view_minimap).

## Styles: the street map beside today's map

The author (2026-09-28, after the mock-up): "I like it; we should find a
way though to keep the option to also run the current way it's done so
that we have multiple possible google-maps way to do it". The
repository's habit: the Povray way keeps each of the ray tracer's
algorithms and shows them side by side (the flag `evolution`); a game's
switchable layer lives in its own bannered section, hooks elsewhere.

So the map's drawing becomes **styles**, one chosen at a time, switched
live:

- `classic`: today's map, kept as it is: SeeSoft's picture at every
  zoom, directories' names big and faint, files' tabs, definitions
  written by their kind (Highlight_code.emphasis), placed greedily each
  frame;
- `streets`: this plan's: the five levels, the column hints before the
  colours, the files by role, the labels by their uses, each one's
  minzoom decided once, fading;
- later, as they are wanted: codemap's own look (its draw_macrolevel,
  its fonts by kind and uses, its summary mode); a `learner` style, the
  program's own code bright and the kits dimmed, the tour's stops as
  landmarks.

A style is a record of the map's drawing decisions (no functors, no
objects: the repository's ocaml-light rule), each a function of the
camera and the map:

    type style = {
      name : string;
      file : ...;    (* how a file is painted at this zoom: hints, SeeSoft, text *)
      dirs : ...;    (* the directories' names *)
      labels : ...;  (* which names, where, how big *)
    }

What does not change with the style stays shared: the treemap, the
camera and its smooth moves, the names lit and clicked (naming, levels 1
to 3), the glass, the tour, the card, search. Chosen by a key (`m`, the
map's style, as `t` chooses its layout) and by a flag
(`style=classic|streets`, also for tinybox's web page, `?style=`); the
status line says which. Each style its own module (`Map_classic`,
`Map_streets`) in its bannered section's spirit: a style is read whole
in one file.

The golden frames: each style's Z0 to Z4 frames, side by side, so a
change to one style is seen not to touch the other.

## The code

- `Code_rank` (new, pure): the populations -- a definition's uses (the
  reference index, then Code_names), a file's, a directory's; codemap's
  buckets. Tested on a small tree of files.
- `Code_labels` (new, pure): the features (directory, file, section,
  definition), their rank, their minzoom by the level-by-level placement
  in a collision grid, their fading; what `Code_map.labels` draws. Tested:
  no two labels overlap at any level, a label placed at a level is placed
  at the next (stability), at most the budget on a screen.
- `Code_map`: what the styles share (the camera, the painting's cache,
  the names lit and clicked, the card, search), and the style chosen;
  `Map_classic` (today's drawing, moved) and `Map_streets` (the levels by
  a line's size on screen, the column hints at Z0-Z2 and SeeSoft's
  pixels from Z3, the labels from `Code_labels`).
- Speed, a street map's way: the column hints need only the lines'
  lengths, not a parse, so a big directory (Principia, 3,600 files) opens
  at once, drawn from the hints; the parses (colours, definitions,
  populations) come in the background, a few files a frame, the labels
  appearing as their files are read, fading in -- as a map's tiles load.

## Steps

0. **Styles**: today's drawing moved, unchanged, into `Map_classic`
   behind the style record, `m` and `style=` switching (one style for
   now); its golden frames, before and after, the same.
1. **Column hints** from afar, SeeSoft's pixels from Z3, in the new
   `Map_streets`: the least cluttered far view, and the fastest (no parse
   needed to draw it). Golden frames of the repository's map at Z0 to Z4
   (tinybox's `code=` and `-dump-frame`) in both styles, looked at
   together before anything else.
2. **Populations** (`Code_rank`), and the definitions' labels sized and
   chosen by them rather than by their kind alone.
3. **The placement by level** (`Code_labels`): minzoom, the budget, the
   fading; its tests (no overlap, stability, the budget).
4. **The card** and **search**.
5. **Background parsing**: Principia's map at once, its labels arriving.
6. Later: layers, roads, the URL of a view, the scale bar and minimap.

Each step's frames are compared with the sketches above, and the
sketches changed when the frames teach something the sketches did not.

## Open questions for the author

- The column hints: grey bars tinted by the file's colour (as in the
  mock-up), or your codemap's exact look (its `draw_macrolevel`)?
- The capitals at Z0: the three most used definitions of the whole map,
  or none (only the parts)?
- Uses counted how: all references alike, or the other files' more than
  the definition's own (as proposed)?
