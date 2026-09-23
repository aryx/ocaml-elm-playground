OCaml Elm Playground
=======================

Create pictures, animations, and video games with OCaml, in 2D and 3D!

This is a port of the excellent Elm playground package
https://github.com/evancz/elm-playground to OCaml.

> This is the package I wanted when I was learning programming. Start by
> putting shapes on screen and work up to making games. I hope this
> package will be fun for a broad range of ages and backgrounds!
> *- Evan Czaplicki*

A place to learn, by reading
----------------------------

Evan's playground is for learning to *write* programs. This repository
is also for learning by *reading* them. Everything a game or an
application needs is written here from scratch, in plain OCaml: the
pixels, the sound, the physics, the AI, the widgets. There is no game
engine, no GPU requirement, and no C library doing the interesting part.
Each module holds one idea, and its `.mli` explains that idea, often
with an ASCII diagram, the paper or machine it comes from, and a worked
example checked by a test. So when you wonder how a triangle becomes
pixels, how a PNG is decompressed, how a Moog filter gets its sound, or
how a chess program picks its move, the answer is a few hundred lines
you can open and read.

All of it fits in **about 50,000 lines of OCaml**. That counts only the
library, `.mli` files and their explanations included, and leaves out
the tests, the games and the applications:

| area | lines | what you can read there |
| ---- | ----: | ----------------------- |
| **the playground** (`playground/`, `core/`) | 16,500 | the 2D and 3D APIs and their seven backends (Cairo/SDL, vdom/SVG, OpenGL, WebGL, software), cameras, sprites, tilemaps, 3D characters, ragdolls and portals, and other ways to program: Logo's turtle, HtDP's big-bang, PuzzleScript |
| **graphics** (`graphics/`) | 7,100 | a 2D rasterizer (lines, circles, polygon fill, strokes), a 3D one (projection, clipping, culling, z-buffer and painter's algorithm, flat/Gouraud/Phong shading, texture mapping), Hershey fonts, and image decoders: PNG with its own inflate, Huffman and CRC, GIF (LZW), baseline JPEG (DCT), and XPM |
| **physics** (`physics/`) | 5,200 | 2D and 3D rigid bodies: integrators, broadphase, collision detection, contact resolution, an iterative solver, joints, springs, quaternions, continuous collision, particles, and gravity (Kepler's orbits) |
| **audio** (`audio/`) | 4,700 | oscillators, noise, envelopes, LFOs, filters (state-variable and Moog ladder), FM synthesis, Karplus-Strong strings, a Fourier spectrum, stereo space, effects, a mixer, sound effects and music, and WAV, MIDI and ABC files |
| **AI** (`ai/`) | 3,500 | pathfinding, state machines, behavior trees, utility AI, steering and flocking, minimax with iterative deepening and Zobrist hashing, Monte Carlo tree search, Q-learning, and neural networks trained by backpropagation and by our own automatic differentiation |
| **GUI** (`gui/`) | 3,400 | a toolkit: widgets, layout, focus, text editing, grids and themes, wired four ways (callbacks, MVC, MVU, immediate mode) |
| **application engines** (`appkits/`) | 4,800 | a spreadsheet (formula language, dependency graph, recalculation), rich text poured into pages with Knuth-Plass line breaking, bitmap painting (seed fill, PackBits), structured drawing, compound documents, slides, HyperTalk, undo and the clipboard |
| **game kits** (`gamekits/`) | 5,700 | how each genre works: Doom's sectors, heightmap terrains, Descent's mines, isometric projection, racing roads and 3D tracks, platformer slopes and ladders, hitboxes and frame data for fighting games, animated skeletons, rhythm charts, a Sokoban solver, playing cards, ... |

With the library doing the heavy lifting, a program built on it stays
short enough to read in one sitting. It is not a toy sketch either, but
a working version of a famous original:

- [TinyMario](games/platform/TinyMario.ml): **350 lines**;
- [TinyStreetFighter](games/fighting/TinyStreetFighter.ml): 450 lines;
- [TinyZelda](games/adventure/TinyZelda.ml): 380 lines;
- [TinyDoom](games/fps/TinyDoom.ml): 500 lines, and its 3D twin
  [TinyDoom3d](games/fps/TinyDoom3d.ml), over the same level: 180;
- [TinyQuake](games/fps/TinyQuake.ml), whose level is compiled by its
  own qbsp, vis and light at startup: 600 lines;
- [TinySimCity](games/strategy/TinySimCity.ml): 530 lines;
- [TinyExcel](apps/office/TinyExcel.ml), a spreadsheet with a menu bar,
  a formula bar and range selection: **300 lines**. It uses the same
  engine as [TinyVisiCalc](apps/office/TinyVisiCalc.ml), and the header
  explains what changed between 1979 and 1985;
- [TinyWord](apps/office/TinyWord.ml), a word processor: 450 lines;
- [TinyMacPaint](apps/office/TinyMacPaint.ml), with its patterns and
  flood fill: 540 lines;
- [TinyMinimoog](apps/music/TinyMinimoog.ml), the Model D synthesizer
  with its panel of knobs: 400 lines.

There are 126 games and 14 applications like these, listed in
[CATALOG.md](CATALOG.md). Each one starts with a header about its
original and what it borrows from the library, and the games that fake
3D point out the trick they use. The
tutorials in [docs/claude_notes/tutorials/](docs/claude_notes/tutorials/)
(3D, 2D, physics, audio, synthesizers, AI, images, fonts, GUI, ...) walk
through each subject module by module.


Documentation
---------------------------------------------------

* [Getting started](https://github.com/aryx/ocaml-elm-playground?tab=readme-ov-file#ocaml-elm-playground) (this file)
* [Tutorial](https://aryx.github.io/ocaml-elm-playground/elm_playground/)
* [Basic examples](https://aryx.github.io/ocaml-elm-playground/examples/)
* [Basic games](https://aryx.github.io/ocaml-elm-playground/games/)
* [API reference](https://aryx.github.io/ocaml-elm-playground/elm_playground/Playground/)
* [Catalogue of the games and applications](CATALOG.md)
* [3D, from scratch: a tutorial](docs/claude_notes/tutorials/notes_3d.md),
  and the [other tutorials](docs/claude_notes/tutorials/) (2D, physics,
  audio, AI, images, GUI, ...)
* [Index](https://aryx.github.io/ocaml-elm-playground)
* [Changelog](changes.txt)

Features
--------------

The OCaml `elm_playground` package allows you to easily create
*pictures*, *animations*, and even *video games* in a portable way using an API that
really simplifies how to view the computer and its devices (the screen,
keyboard, and mouse).

The goal is similar to the old [`graphics` package](https://github.com/ocaml/graphics)
but goes even further in terms of simplification.

The main API is defined in a single
[Playground.mli](https://github.com/aryx/ocaml-elm-playground/blob/master/playground/Playground.mli) module and is implemented by two backends:
 - a *native* (SDL-based) backend to run your game on your desktop from a terminal
 - a *web* (vdom-based) backend to run your game in a browser

Here is for example a simple [Snake game](https://aryx.github.io/ocaml-elm-playground/games/Snake.html) you can run from your browser (use the arrow keys to change the direction of the snake and eat the ball to grow your length). You can run the same game
on your desktop *without changing a line of code*.

The same idea one dimension up is `Playground3d`
([Playground3d.mli](https://github.com/aryx/ocaml-elm-playground/blob/master/playground/Playground3d.mli)):
shapes in space, a camera, and the same `game` loop. Its main backend
wraps no OpenGL, Vulkan or WebGL -- the projection, the camera math and
the whole triangle rasterizer are hand-written OCaml, on purpose, so
that you can read every line that turns a 3D shape into pixels (two
other backends then hand the same scenes to a real GPU, for comparison
and for speed). See [In 3D](#in-3d) below.

Credit where due: `Playground3d`'s API (world-space shapes, an
eye/target camera, and the trick of compiling a 3D scene down to plain
2D shapes for the browser) is adapted from Luca Mugnaini's
[elm-playground-3d](https://github.com/lucamug/elm-playground-3d),
itself built on Evan Czaplicki's original
[elm-playground](https://github.com/evancz/elm-playground); the
`StarCollector3d.ml` example's mechanics are adapted from Nate Abele's
[elm-3d-playground](https://github.com/nateabele/elm-3d-playground).
The fuller survey -- VRML, OpenGL, WebGL, Vulkan, Unity -- is in
[notes_playground3d_related_work.md](docs/claude_notes/related-work/notes_playground3d_related_work.md).

Install
--------------

To install the playground, run `opam install elm_playground` and then
install one or both backends with `opam install elm_playground_native`
and/or `opam install elm_playground_web`. The 3D packages
(`elm_playground_3d` and its backends `elm_playground_3d_opengl`,
`elm_playground_3d_software`, `elm_playground_3d_webgl`,
`elm_playground_3d_web`) live in this repository and are released the
same way.

Simple native application
--------------------------

Here is a very simple application using the playground:
```ocaml
open Playground

(* the (x, y) position of the blue square  *)
type model = (float * float)

let initial_state : model = (0., 0.)

let view _computer (x, y) = [ 
  square blue 40.
   |> move x y
 ]

let update computer (x, y) =
  (x +. to_x computer.keyboard, y +. to_y computer.keyboard)

let app = 
  game view update initial_state

let main = Playground_platform.run_app app
```
<!-- coupling: docs/toy-native-example/toy.ml and examples/Keyboard.ml -->

It is a very simple `game` defining a `model` type, a `view` function, and an `update` function to specify how the game behaves. It is using the
"Model-View-Update" architecture to organize the code of a graphical
interactive application
(see https://guide.elm-lang.org/architecture/ for more information).

To compile this application, simply do:
```bash
$ cd docs/toy-native-example
$ opam install --deps-only --yes .
$ dune exec --root . ./Toy.exe
```
You should then see on your desktop:

<img src="docs/screenshots/keyboard-game-start-native.png" alt="Toy app screenshot"
 width="60%">

If you type on the arrow keys on your keyboard the blue square should move in the
corresponding direction. If you type `q` it will exit the game.

Note that with the Playground API the center of the screen is at `(0, 0)`.

Simple web application
--------------------------

To compile this same application for the web, simply do:

```bash
$ cd docs/toy-web-example
$ opam install --deps-only --yes .
$ dune build --root .
$ cp _build/default/Toy.bc.js static/
```
You should then be able to use the web app by going to:
https://aryx.github.io/ocaml-elm-playground/toy-web-example/static/Toy.html

By default the generated javascript file can be big so to get a smaller one
you can do instead:
```bash
$ dune build --root . --profile=release
$ cp _build/default/Toy.bc.js static/
```

Parameters (flags)
------------------

A program can be given parameters, the same way natively and on the
web: `name=value` (or just `name`) arguments on the command line, or
the URL's `?name=value&name`:

```bash
$ dune exec examples/Flags.exe -- color=red speed=3
```
or `Flags.html?color=red&speed=3` in a browser.

Like Elm's flags, they reach the application purely: `main` reads them
with `Playground_platform.flags ()` and gives them to `run_app`, and
every `view` and `update` then finds them in `computer.flags`, as
`(name, value)` pairs:

```ocaml
let update computer (x, y) =
  let speed =
    match List.assoc_opt "speed" computer.flags with
    | Some s -> float_of_string s
    | None -> 1.
  in
  (x +. speed *. to_x computer.keyboard, y +. speed *. to_y computer.keyboard)

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
```

(see [examples/Flags.ml](examples/Flags.ml); a program that doesn't pass
`~flags` gets none). The arguments starting with a dash (`-debug`,
`-fixed-time 1000`, ...) are the playground's own. The same works for
3D programs, with `Playground3d_platform.run_app3d ~flags`.

In 3D
-----

The same three functions, with shapes in space and a camera:

```ocaml
open Playground
open Playground3d

let view (computer : Playground.computer) () =
  let angle = spin 6. computer.time in
  let scene = cube purple 1.5 |> rotate3d 0. angle 0. in
  let cam = camera ~eye:(3., 2., 5.) ~target:(0., 0., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
```

`view` returns a camera and the shapes it looks at. The camera keeps
(0, 1, 0) as up unless told otherwise; a game whose ship rolls or looks
straight up gives its own (`camera ~eye ~target ~up ()`, see
`TinyDescent3d.ml`). `cube`/`box`/`plane` are procedurally generated,
single-flat-color shapes -- no assets needed, the same philosophy as
2D's `circle`/`square` -- and `textured_cube`/`textured_quad` take a
file path or an http(s) URL and sample the image for real, per pixel. A
texture can also travel inside the program, with no file to find at run
time: a dune rule turns the image into base64
(`scripts/build/file_to_base64_ml.ml`), and
`Playground3d.embedded_texture ~name ~base64` gives it a name to use as
a `src` (see `TinyMinecraft.ml` and its dune file).

Some to run, from this repository:

```bash
dune exec examples/Cubes3d.exe         # a grid of overlapping cubes, orbited by the camera
dune exec examples/TexturedCube3d.exe  # a cube wrapped with a test texture
dune exec examples/software/Corridor3d.exe  # walk down a corridor (up/down arrows)
dune exec examples/StarCollector3d.exe # move a box, collect randomly-spawning stars
dune exec games/flight/TinyDescent3d.exe    # fly a ship through a mine (arrows, a/d, w/s)
dune exec games/fps/TinyQuake.exe      # a Quake level: qbsp, vis and light at startup
```

In a browser: `make serve-build`, then e.g.
http://localhost:8001/examples/web/TexturedCube3d.html (the pages must
be served over HTTP, not opened as files, for the WebGL textures to
load; the Makefile's comment above `serve-build` says why).

An application chooses how it is drawn, portably, with
`Playground3d.rendering`:

```ocaml
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = Flat } app
```

- `shading`: `No_lighting`, `Flat` (crisp facets), or `Smooth` (curved
  shapes look round; the default);
- `backface_culling`: `false` to also draw the faces turned away from
  the camera, e.g. for a lone `plane` seen from below;
- `smooth_textures`: `false` for sharp texels (pixel-art textures).

Each backend does what it can (see below). The 2D playground has the
same idea, `Playground.rendering`.

Backends
--------

The same application code runs on several backends, which is the point
of the two `.mli` files above: a program names `Playground_platform`
(or `Playground3d_platform`) and the dune file chooses who implements
it.

|                  | 2D                                  | 3D                                   |
| ---------------- | ----------------------------------- | ------------------------------------ |
| native, a library | **native**: Cairo, SDL for the window | **opengl**: OpenGL 3.3 and two small shaders |
| native, from scratch | **software**: our own 2D rasterizer  | **software**: our own 3D rasterizer   |
| browser          | **web**: vdom, SVG                  | **webgl**: WebGL 1, with the 2D backend's SVG for the HUD |
|                  |                                     | **svg**: the scene compiled to 2D shapes, drawn by the web backend |

The `software` backends compute every pixel themselves, from
`graphics/` -- projection, clipping, culling, a z-buffer, shading,
texture mapping, one module per idea, each `.mli` explaining its
algorithm with a diagram and a worked example. They use SDL only for
the window, the input and showing the finished image. That is where the
rasterizers of this repository live, and `make test` compares their
output with golden frames, pixel by pixel.

The `svg` 3D backend is the cheapest of the four: it turns a 3D scene
into ordinary 2D `Playground.shape` values every frame (backface-culled
and depth-sorted) and hands them to the unmodified web backend, getting
SVG rendering, the event loop and browser timing for free -- at the
cost of some fidelity (see the limitations below).

Debug keys
----------

A program run with `-debug-keys` (e.g. `dune exec examples/Cubes3d.exe
-- -debug-keys`) turns a dozen keys into live toggles of how it is
drawn: `m` cycles the shading (no lighting, flat, Gouraud, Phong), `f`
draws the wireframe, `z` swaps the z-buffer for the painter's
algorithm, `c` turns near-plane clipping off, `x` is a pixel
magnifier, `r` drops the resolution, `o` runs the unoptimized code, and
`h` shows the lot with their current state. Each one is there to make
one idea visible: what every key demonstrates, and why, is section 11
of [notes_3d.md](docs/claude_notes/tutorials/notes_3d.md). Without the
flag they are off, so a game can use any key it likes. The WebGL pages
take the same toggles as URL parameters (`?debug-keys`, `?keys=mb`,
`?fixed-time=1000`), since a page has no command line.

Two more flags make a run reproducible, which is how the golden frames
are made: `-fixed-time t` freezes the clock, and `-script
"right:1-60,space:30"` plays the keys and the mouse frame by frame (see
`Input_script.mli`).

Current limitations of the 3D playground
----------------------------------------

- One fixed directional light (flat, Gouraud, or Phong shading) -- no
  shadows, no multiple or colored lights, no specular highlights.
- One curved primitive, `sphere` (no `cylinder`/`cone` yet) --
  everything else is built from flat polygons.
- The SVG backend can't warp a texture onto an arbitrary projected
  quad, so a textured face renders as a flat gray placeholder there
  (the other backends sample the real texture per pixel).
- No near-plane clipping on the SVG backend either (a triangle with a
  vertex behind the camera is dropped whole, not clipped into visible
  sub-triangles; the software backend clips, see the `c` key, and the
  GPU ones in hardware).
- No transparency on the software and GPU backends (`fade3d` is
  ignored there): blending needs the faces drawn back to front, which
  the z-buffer doesn't give.
- The WebGL backend's textures need the page served over HTTP, and its
  wireframe (lines, WebGL having no polygon mode) is never culled.

Next steps
------------

Read the tutorial at:
https://aryx.github.io/ocaml-elm-playground/elm_playground/

Look at the code under [examples/](examples/) and [games/](games/), a
directory per genre; every game and application is listed, with the
original it is a toy version of, in [CATALOG.md](CATALOG.md).

Here is a screenshot of the [Tetris](games/puzzle/Tetris.ml) Playgound game running:
<img src="docs/screenshots/game-tetris.png" alt="Toy app screenshot"
 width="50%">

You can even try it online [here](https://aryx.github.io/ocaml-elm-playground/games/Tetris.html)

You can see a few more screenshots [here](docs/screenshots/).

What the project has become
---------------------------

The playground is still the small library above, but around it this
repository has grown into a place to learn how the things it draws are
made (see [A place to learn, by reading](#a-place-to-learn-by-reading)
for the numbers), each subject written from scratch, one idea per
module, with the idea explained in its `.mli` and checked by tests and
golden frames:

- **pictures**: `graphics/`, the 2D and 3D software rasterizers behind
  the `software` backends, and `Playground3d` for 3D programs;
- **games**: `games/`, a directory per genre (see
  [CATALOG.md](CATALOG.md)), 2D, 2.5D (each pseudo-3D trick written out
  in its game) and 3D side by side, over the genre kits of `gamekits/`;
- **motion, sound, decisions, networks**: `physics/`, `audio/`, `ai/`,
  `networking/`;
- **applications**: `gui/`, a small toolkit with the same widgets
  wired four ways (callbacks, MVC, MVU, immediate mode), the engines of
  `appkits/`, and the Tiny applications of `apps/` -- TinyVisiCalc and
  TinyExcel over one spreadsheet engine, TinyBravo and TinyWord over
  one text engine, TinyFrameMaker (the text poured over pages),
  TinyMacPaint and TinyMacDraw (dots, and objects),
  TinyOpenDoc (a document made of
  parts), TinyPowerPoint and TinyHyperCard -- and TinyOffice, the suite
  as it is today, where every kind of document holds the others.

The plans and tutorial notes for each are in
[docs/claude_notes/](docs/claude_notes/); for the applications, start
with [notes_gui.md](docs/claude_notes/tutorials/notes_gui.md).

AI disclaimer
------------

The 2D playground API (`Playground.mli`) and the ideas behind it are the
work of Evan Czaplicki for Elm at https://github.com/evancz/elm-playground
I (Pad) mostly ported the API and ideas, as well as some of the web backend code,
to OCaml. I then added also a native backend (using tsdl), which was not
in the Elm version (Elm is a web language).
The other APIs, written in the same spirit as Evan's (a few records and
functions, called from the same `view` and `update`), are Claude Code's: `Physics`,
`Audio`, `Ai`, `Gui`, `Sprite`, `Tilemap`, `Logo`, and the rest of
`playground/`. The exception is `Playground3d`, which is adapted from
Luca Mugnaini's elm-playground-3d (see the credits above).
The playground itself is still very small: the 2D API with its Cairo
and vdom backends is about 5,000 lines, and the 3D one about 1,300 over
a 900-line rasterizer.

I recently (Sep 2026) used Claude Code to fix many small bugs, and then
to grow everything around that library: the repository is now about
150,000 lines of OCaml, and most of it is Claude Code's. And yet it
stays small for what it holds -- 126 games and 14 applications, the 2D
and 3D rasterizers, a physics engine, a synthesizer, game AI, a GUI
toolkit, all written from scratch, with no engine, no asset pipeline
and no dependency doing the work underneath.
