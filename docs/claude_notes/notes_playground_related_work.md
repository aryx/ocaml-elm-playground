# playground/ vs. the rest of the 2D graphics world

The 2D twin of [`notes_playground3d_related_work.md`](notes_playground3d_related_work.md):
where `elm_playground` comes from -- a long line of "draw pictures and
make games in a few lines" teaching libraries -- and where its three
backends sit among the real 2D graphics APIs: PostScript, SVG, Cairo,
Canvas, Skia. The same through-line as for 3D: **the industry APIs are
designed to be as fast and as capable as possible; the playground's
software backend is designed to be as *legible* as possible.** The
difference with 3D: here the playground has a backend at each end,
with the same API and the same games, so the trade can be measured, not
just described.

## The one-line version

| | What it optimizes for | What you can see happening |
|---|---|---|
| PostScript / PDF | Device-independent pages: the same file on any printer or screen | Nothing -- a page description, rendered by whoever reads it |
| SVG | Declarative vector pictures in the browser, like HTML for drawings | Nothing below the DOM -- the browser's own renderer does it all |
| Cairo, Skia, Canvas 2D, Quartz, Direct2D | Fast, high-quality, anti-aliased vector drawing, on CPU or GPU | Nothing below the API call -- decades of tuned C/C++, SIMD, GPU paths |
| SDL | A window, input, and a pixel buffer, on every platform | Only what you draw into the buffer yourself |
| Logo, Racket's `2htdp`, Gloss, CodeWorld, Processing | Making first programs visual and fun | The API, not the rendering (it's Tk, OpenGL, a canvas, Java2D underneath) |
| elm-playground (Evan Czaplicki) | The smallest API for pictures, animations and games | The API and the model/view/update loop; rendering is SVG |
| `elm_playground`'s software backend | Every number that produces a pixel being readable | Everything: `graphics/2d/`, one algorithm per module |

## Part 1: the teaching lineage

### Logo and turtle graphics (1967) -- the ancestor of them all

Seymour Papert, Wally Feurzeig and Cynthia Solomon's Logo (BBN, 1967),
and its turtle: a cursor with a position and a heading, moved by
`forward 100` and `right 90`, drawing as it goes. Papert's *Mindstorms*
(1980) made the case that children learn geometry best by programming
it, from the turtle's point of view. Every library below is, in a way,
an answer to Logo's question -- what's the smallest thing that lets a
beginner make a picture? -- and elm-playground's `move`/`rotate` on
whole shapes is the answer that won: describe the picture, don't steer
a pen. (examples/Turtle.ml is a turtle *image*, not a turtle.)

### Racket's `2htdp/image` and `universe` -- pictures as values, games as worlds

*How to Design Programs* (Felleisen, Findler, Flatt, Krishnamurthi;
2nd edition 2018) teaches programming with images as ordinary values
(`(circle 20 "solid" "red")`, combined with `overlay`, `beside`,
`above`), and interactive programs with `big-bang`: a world state, a
function to draw it, functions to update it on each tick, key or mouse
event. That is the model/view/update architecture, a decade before Elm
named it (Felleisen et al., "A Functional I/O System, or, Fun for
Freshman Kids", ICFP 2009). The Bootstrap curriculum (Emmanuel
Schanzer) teaches algebra with the same idea, in Racket and then Pyret.

### Gloss and CodeWorld -- the same idea in Haskell

Ben Lippmeier's Gloss (Haskell) has almost exactly elm-playground's
shape: `display` a picture, `animate` a function of time, `play` a game
with a world, a draw function, an event handler and a step function --
`picture`, `animation`, `game`. Chris Smith's CodeWorld, a Haskell
environment for teaching middle-school students, has `drawingOf`,
`animationOf` and `activityOf`, the same three again. Both draw with
OpenGL (Gloss) or a browser canvas (CodeWorld): the rendering is not
something the student sees.

### Processing and p5.js -- sketches for artists

Casey Reas and Ben Fry's Processing (MIT Media Lab, 2001), and Lauren
McCarthy's p5.js (2013), its JavaScript version: a `setup` function and
a `draw` function called every frame, drawing imperatively (`fill`,
`ellipse`, `rect`) on a canvas, aimed at artists and designers rather
than programmers. The opposite style to the functional libraries above
-- a mutable canvas and global state, not pictures as values -- and
enormously successful. Scratch (MIT, 2007) goes further from text,
with blocks instead of code, and sprites instead of shapes.

### Elm: `Graphics.Collage`, then elm-playground

Elm itself started as a language for this (Evan Czaplicki's thesis,
"Elm: Concurrent FRP for Functional GUIs", 2012): its core library had
`Graphics.Collage`, shapes as values moved and rotated, rendered to an
HTML canvas. It left the core library as Elm moved to The Elm
Architecture and HTML (around Elm 0.17, 2016), and came back in 2019 as
[elm-playground](https://github.com/evancz/elm-playground): `picture`,
`animation`, `game`, a `computer` record for time, mouse, keyboard and
screen, shapes drawn with SVG. This project is its OCaml port, and
Luca Mugnaini's elm-playground-3d, its 3D extension, is the ancestor of
`playground3d/` (see the 3D note).

### OCaml's `Graphics` module

OCaml's own teaching graphics: the `Graphics` module (from the Caml
Light days, in the standard distribution until OCaml 4.09, a separate
`graphics` package since), a window and immediate drawing commands
(`moveto`, `lineto`, `fill_circle`, `set_color`), X11 or Windows
underneath. Imperative like Processing, with no animation loop or
events beyond polling: the gap an elm-playground for OCaml fills.

## Part 2: the industry 2D APIs

### PostScript (1984) and PDF (1993) -- the imaging model

John Warnock and Charles Geschke's PostScript (Adobe, 1984, from
Warnock and Martin Newell's JaM and Xerox's Interpress before it)
defined the **imaging model** nearly every 2D API since has kept: a
*path* of lines and Bézier curves (`moveto`, `lineto`, `curveto`),
*filled* (by the nonzero winding or even-odd rule) or *stroked* (with a
line width, joins and caps), in a *current transformation matrix*,
with *fonts* as outlines filled like any path. PDF (1993) is its
page-description subset. `graphics/2d/` is a small version of exactly
this model: `Fill` (paths filled with nonzero winding), `Stroke`,
`Affine` (the CTM), and text as filled strokes (Hershey's, not
outlines).

### SVG (2001) -- the declarative one

The W3C's Scalable Vector Graphics: PostScript's imaging model as XML
elements in the browser's DOM (`<circle>`, `<path d="...">`,
`transform="rotate(30)"`), styled with CSS, retained by the browser and
redrawn by it. The web backend (`playground/web/`) builds exactly this,
with ocaml-vdom's virtual DOM: the "reuse an existing renderer"
choice, like lucamug's 3D trick -- zero rendering code, zero control
over the pixels, and the browser's quality (antialiasing, text) for
free.

### Cairo (2003), Canvas 2D (2004), Skia (2005), and the platform APIs

The immediate-mode libraries implementing the same model in code:

- **Cairo** (Keith Packard and Carl Worth, 2003): C, with output to
  images, X11, PDF, PostScript, SVG, over **pixman**, its pixel
  compositing library (SIMD, span-based). What the native backend
  (`playground/native/`) uses, and what GTK, Firefox (for years) and
  many others drew with.
- **Canvas 2D** (Apple, 2004, then HTML5): the same model as a browser
  API, `ctx.arc`, `ctx.fill`, a mutable canvas -- p5.js's layer.
- **Skia** (bought by Google in 2005): C++, CPU and GPU backends,
  under Chrome, Android and Flutter.
- **Quartz 2D** (Apple, PDF's model exactly) and **Direct2D**
  (Microsoft, 2009, on the GPU).

Behind each: tens of thousands of lines (Cairo, pixman, FreeType and
fontconfig, for Cairo alone) of edge cases, SIMD paths, font formats and
hinting, color management, output devices. The current research
direction is doing all of it on the GPU (Loop and Blinn's curve
rendering, 2005; compute-shader renderers like Raph Levien's Vello),
since a GPU only knows triangles (see `notes_2d.md` section 14).

### SDL (1998) -- a window and a pixel buffer

Sam Lantinga's Simple DirectMedia Layer: a window, keyboard, mouse,
sound, and a surface of pixels, on every platform; no drawing at all
(beyond copying rectangles). That's the point: it's the platform layer
under both native backends, and the software backend asks nothing else
of it. pygame (2000) and LÖVE (2008) are 2D game frameworks built the
same way, on SDL.

## Where `elm_playground` actually sits

Three backends, the same API and the same examples and games
(`Playground_platform` is a virtual module, see the project's
`CLAUDE.md`), at three points of the spectrum:

- **web** (`elm_playground_web`): SVG in the browser, via ocaml-vdom
  -- Evan's original design, the browser does all the drawing.
- **native** (`elm_playground_native`): Cairo on an SDL window -- a
  real, capable 2D library does all the drawing.
- **software** (`elm_playground_software`): every pixel computed by
  `graphics/2d/`, from scratch, in OCaml, one classic algorithm per
  module (scanline polygon filling, Bresenham and Wu lines, midpoint
  circles, Porter-Duff blending, inverse-mapped images, coverage
  antialiasing, Hershey text), each `.mli` with a diagram, a worked
  example checked by a test, and its paper; `notes_2d.md` is the
  tutorial.

The software backend is what the Processing/Gloss/CodeWorld lineage
never offered: not just a small API to *make* pictures, but a small
renderer to *read*, with debug keys (`-debug-keys`) to turn each
feature off and see what it does, and a magnifier to look at the
pixels.

## Postscript: the actual numbers

The same question as the 3D note's postscript -- how much code, and
how much speed, does the from-scratch version cost? -- answered with
Cairo in place of the GPU (`notes_2d.md` section 14, `notes_opti.md`):

**Code size**: the whole software renderer is about 775 lines of code
(its part of `graphics/`: 540, `playground/software/`: 235, when
`notes_2d.md` was written), plus about twice as
many lines of comments, and 540 lines of tests. The Cairo backend is
235 lines of OCaml too -- on top of Cairo, pixman, FreeType and
fontconfig.

**Frame rate** (uncapped, frames per second, `scripts/bench_playground.sh`):

| scene    | Cairo | ours, optimized | ours, no antialiasing |
|----------|------:|----------------:|----------------------:|
| Picture  |   465 |             267 |                   380 |
| Smiley   |   400 |             187 |                   353 |
| Turtle   |   339 |             102 |                   125 |
| Mario    |   262 |             176 |                   277 |
| Pong     |   435 |             166 |                   293 |
| Snake    |   443 |             164 |                   279 |
| Tetris   |   423 |              61 |                   184 |
| Asteroid |   457 |             272 |                   379 |

Cairo is 1.5 to 7 times faster, antialiased too: pixman is decades of
tuned C with SIMD, compositing whole spans, caching glyphs; ours is
plain OCaml, per span or per pixel. But every example and game runs at
well over 60 fps from scratch, and the gap is where the known hot
spots are (thin antialiased text: Tetris; see `plan_2d_remaining.md`).
A much smaller gap than the 3D one (the GPU was 6 to 33 times faster
than the software rasterizer): 2D on a CPU is a solved,
well-understood problem, and a few hundred lines of classic algorithms
get most of the way there.

Sources: the papers and books named above (also cited in `notes_2d.md`
and the `graphics/2d/` `.mli`s), the READMEs and documentation of
elm-playground, Gloss, CodeWorld, Processing, p5.js, Cairo, Skia and
SDL, and general, widely-documented knowledge of PostScript, SVG and
Canvas.
