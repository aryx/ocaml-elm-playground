# Plan: `elm_playground_software`, a from-scratch 2D rasterizer backend

## Context

`playground3d/` ended up with a nice teaching pair:

|         | "do it yourself"                          | "let a library/the hardware do it" |
|---------|-------------------------------------------|------------------------------------|
| **3D**  | `playground3d/software/` (software rasterizer: z-buffer, edge functions, ...) | `playground3d/opengl/` (the GPU does it) |
| **2D**  | **missing**                               | `playground/native/` (Cairo does it) |

The 2D side has no "do it yourself" half. `playground/native/` hands
every shape to Cairo (`Cairo.arc`, `Cairo.line_to`, `Cairo.fill`,
`Cairo.show_text`, `Cairo.set_source_surface` + `Cairo.paint`), so the
actual pixel-level algorithms -- how a polygon becomes pixels, how a
circle is traced on a grid, how transparency is blended, how an image
is rotated and scaled, how a letter is drawn -- are all hidden inside
libcairo/pixman/FreeType.

This plan adds a 2D backend that does all of that itself, on a plain
`int32` pixel array blitted to an SDL window, with **zero Cairo**. Each
algorithm gets a comment pointing at the classic paper that introduced
it (Bresenham 1965, Wylie et al. 1967, Porter & Duff 1984, ...), and a
new tutorial, `notes_2d.md`, plays the role `notes_3d.md` plays for 3D.

Same API, unmodified: every `examples/` and `games/` file only uses
`Playground`'s public combinators, so it should link against the new
backend exactly as it links against `native/` today.

## Naming

Recommendation: **keep `playground/native/` as is, and call the new
one `playground/software/`, library/package `elm_playground_software`.**

- "Software rendering" / "software rasterizer" is the standard term of
  art for exactly this (as opposed to hardware-accelerated, or here
  library-accelerated, rendering) -- a reader who greps the web for it
  finds the right literature.
- `elm_playground_native` is **published on opam (0.1.7)**, is what the
  README's "Simple native application" walkthrough and
  `docs/toy-native-example/` use, and is what `playground3d/software/`
  links for its HUD. Renaming it (say to `cairo/`) would break those,
  and silently re-pointing the name `native` at a different, slower,
  aliased-by-default renderer would be worse.
- Rejected: `soft_rendered/` (awkward), `raster/` (Cairo rasterizes
  too), `pixels/`/`framebuffer/` (describe the output, not the idea),
  `from_scratch/` (cute, but not a term anyone searches for).

The one wart was that in 3D the software rasterizer used to be called
`native/`. **DONE** (before phase 0): renamed `playground3d/native/` ->
`playground3d/software/` (`elm_playground_3d_native` ->
`elm_playground_3d_software`), cheap because no 3D package was on opam
yet. Archived plans in `done/` keep the old name, as history. Now both
halves read the same way:

```
playground/     native/ (Cairo)     software/   web/ (SVG)
playground3d/   opengl/ (GPU)       software/   web/ (SVG)
```

## Code style: one feature, one function

This code is for teaching, where features get introduced one at a
time. So each feature is new code in its own function(s), which a
debug key can turn on and off (`Shape_render_software.options`), and
the simple path stays short and readable on its own: e.g. `Line.draw`
is just `Line.clip` then `Line.bresenham`, each understandable alone;
`render_form` picks `fill_polygon` or `outline_polygon` but neither
knows about the other. Each algorithm's `.mli` has an ASCII diagram
and a worked example, and `raster/tests/` checks those examples.

Optimizations follow the same rule: the original, simple code stays,
runnable, next to the optimized one, and `raster/Opti.ml`'s
`Opti.enabled` (the "o" key) switches between them -- so the simple
version still explains the idea, and "o" shows on the fps counter what
the optimization buys (numbers in `notes_opti.md`).

## Layout

The algorithms live in a **plain library** separate from the thin
`Playground_platform` implementation, for one important reason: a
library that `(implements elm_playground)` can't be linked together
with another implementation (dune rejects two implementations of one
virtual library). Keeping the rasterizer in a plain library lets a test
executable link it *next to* `elm_playground_native` and diff our
pixels against Cairo's (see Verification), and lets
`playground3d/software/` later drop Cairo for its HUD too.

**Constraint found during phase 0**: that plain library must **not
depend on `elm_playground` either**. Dune also rejects an implementation
of a virtual library that reaches the same virtual library a second
time, through one of its dependencies ("Library elm_playground was
pulled in"). So `raster/` knows nothing about `Playground.shape`: it
works on points, colors, and pixel buffers only. The code that walks a
`Playground.shape` tree and calls it, `Shape_render_software`, lives in
`elm_playground_software` itself, just as `Shape_render_native` lives
in `elm_playground_native`. That's also the better teaching layout: the
algorithms stand on their own, with no Elm in sight.

```
playground/raster/          library elm_playground_raster (wrapped false; no Cairo, no SDL, no Playground)
  Framebuffer.ml            w x h int32 Bigarray (0xAARRGGBB), clear, put, blend
  Affine.ml                 2x3 affine matrices: translate/rotate/scale/compose/apply/invert
  Line.ml                   Bresenham (+ Wu antialiased lines, + Cohen-Sutherland clipping)
  Fill.ml                   scanline polygon fill, active edge table, nonzero winding (+ AA coverage)
  Circle.ml                 midpoint circle (Bresenham 1977) + ellipse flattening
  Blit.ml                   image drawing by inverse mapping, nearest (+ bilinear)
  Hershey.ml + font data    stroked vector font for Words
playground/software/        library elm_playground_software (implements elm_playground)
  Shape_render_software.ml  Playground.shape list -> Framebuffer, the analogue of Shape_render_native
  Playground_platform.ml    Native_loop_2d + Framebuffer = the window surface's pixels
examples/software/, games/software/   (copy_files ../Foo.ml) + dune, like examples3d/opengl/
```

`dune-project` gets `elm_playground_raster` and `elm_playground_software`
package stanzas (regenerate the `.opam` files with `make`); `Makefile`'s
`OPAMS` list gets them too.

### Two things that were trapped in `elm_playground_native` (phase 0, DONE)

`elm_playground_software` can't depend on `elm_playground_native` (two
implementations again), but it needs two things that live there:

1. **Image decoding** (`Image_native.ml`: URL download via curl, decode
   via stb_image, animated GIF frames, the cache). Only its last step,
   `cairo_surface_of_stb_image`, is Cairo-specific.
2. **The 2D SDL event loop** (`run_app`: event draining, 60 fps cap,
   `-v`/`-debug` CLI parsing, "Loading..." screen).

Recommended: do what 3D already did with `native_common/` -- extract a
plain library `playground/native_common/` (`elm_playground_native_common`)
holding a Cairo-free `Image_decode` (returns RGBA pixel arrays + GIF
frame timings) and a `Native_loop_2d` parameterized over a
`draw : Playground.shape list -> unit`-style callback. `Image_native`
becomes a thin Cairo wrapper over `Image_decode`.

Cost: `elm_playground_native` gains a new dependency that must also be
published on opam next time. The alternative -- copy ~150 lines of
event loop and the decode half of `Image_native` into `software/` --
is simpler to ship but duplicates the GIF/cache logic.

**Done that way.** What actually came out:

- `Image_decode` stops at stb_image's straight-alpha RGBA8 buffers;
  its `'a animation` is polymorphic in the frame type, so
  `Image_native` converts every GIF frame to a Cairo surface once
  (`map_animation`) and still picks frames with the shared `frame_at`.
- `Native_loop_2d.run` takes the app's `init`/`update`/`subscriptions`/
  `view` one by one rather than a `Playground.app` record, for the
  "pulled in" reason above; `view`'s result type is a type variable.
- The mouse mapping from window pixels to Elm coordinates used to go
  through `Cairo.device_to_user`; it's now plain arithmetic (verified
  exact with `scripts/xdrive.py`, see
  `notes_debugging_techniques.md` section 8).
- The "Loading..." text and the FPS text stay in each backend, since
  drawing text is backend-specific (Cairo here, Hershey in `software/`).

## The algorithms, shape by shape

What each `Playground.form` needs, what Cairo did for it, what we do
instead, and the paper to cite in the code comment.

| Form | Cairo did | We do | Classic reference |
|---|---|---|---|
| all | CTM + `save`/`restore` | `Affine.t` passed down the recursion; `Group` = compose parent * child (a scene graph of instances, exactly Sketchpad's idea) | Roberts 1965 (homogeneous coordinates); Sutherland 1963 (Sketchpad instancing) |
| `Polygon`, `Rectangle`, `Ngon` | `move_to`/`line_to`/`fill` | transform vertices, then **scanline fill** with an edge table / active edge list, **nonzero winding** rule (Cairo's and SVG's default, so self-intersecting polygons match the other backends) | Wylie, Romney, Evans & Erdahl 1967; Heckbert 1990 ("Concave polygon scan conversion", Graphics Gems) |
| `Circle` | `arc` + fill | under a similarity transform a circle stays a circle: **midpoint circle** algorithm, filling horizontal spans between symmetric octant points | Bresenham 1977 |
| `Oval` (and circles under non-uniform `Group` scale) | `scale` + `arc` | an ellipse under an arbitrary affine is a rotated ellipse; **flatten** to a polygon (segment count from radius, tolerance-driven) and reuse `Fill` | Pitteway 1967 (midpoint conics, cited as the road not taken) |
| `Image` | `set_source_surface` + `scale` + `paint` | **inverse mapping**: for each destination pixel in the transformed bounding box, apply the inverse affine to find the source texel; nearest-neighbor first, bilinear as a toggle | Heckbert 1989 (texture mapping / image warping); Catmull 1974 |
| `Words` | `select_font_face` + `show_text` (FreeType underneath) | **Hershey** simplex Roman vector font: each glyph is a list of strokes, drawn with our own line code, scaled to `words_font_size`, centered like the other backends | Hershey 1967 |
| `alpha`/`fade` | `set_source_rgba` | per-pixel **"over"** compositing, straight (non-premultiplied) alpha in v1 | Porter & Duff 1984; Smith 1995 ("Alpha and the history of digital compositing") |
| (debug) outlines | -- | **Bresenham** lines, clipped to the window | Bresenham 1965; Cohen-Sutherland (Newman & Sproull 1973) |

Note what's *not* on the list: nothing in the `Playground` API draws a
stroked line or outline -- every form is filled. Bresenham still earns
its place three ways: the wireframe debug toggle below (the analogue of
3D's "f"), the Hershey font (which is nothing but line segments), and
the FPS/"Loading..." text.

A nice teaching contrast worth writing up in `notes_2d.md`:
`playground3d/software/` fills triangles with **edge functions** (Pineda
1988), a test-every-pixel-in-the-bounding-box approach that suits
triangles and GPUs; here we fill arbitrary concave polygons with the
older **scanline/active-edge** approach, which walks only the pixels
inside. Same problem, two classic answers.

## Debug toggles (same spirit as 3D's "m"/"b"/"f"/"z"/"p")

Handled entirely in `software/Playground_platform.ml` (via
`Native_loop_2d.run`'s `on_key_press`, one call per physical press),
never part of the public API; each feature that can be turned off is a
field of `Shape_render_software.options`. The window title shows every
key and its state. Plain letters, as in 3D, but **not** the keys games
use: arrows, space, and w/a/s/d (`Playground.to_x2`/`to_y2`) -- which
is why antialiasing is "n", not "a".

Done (with phase 1):

- **"t" -- transparency on/off:** without Porter-Duff blending a faded
  shape is either fully opaque or invisible (`examples/Mouse.exe`
  fades its circle while the button is down).
- **"z" -- pixel magnifier** (`raster/Magnifier.ml`): an 8x inset of
  the 32x32 pixels under the mouse, with a grid between pixels -- to
  *see* the pixel-center rule, gaps/overlaps between shapes, jaggies,
  and later what antialiasing does to an edge.

Settable by the app too: `Playground.rendering` (`antialiasing`,
`smooth_images`), passed as `Playground_platform.run_app ~rendering`,
is portable -- the software backend uses it as the starting value of
"n"/"i" (the keys can still flip them), Cairo as its antialias mode and
image filter, the web as SVG `shape-rendering` and CSS
`image-rendering`. `examples/Mario.ml` asks for sharp pixel-art
sprites (`smooth_images = false`). The other keys are debugging
features of the software rasterizer only, so they stay keys. (The 3D
backends have no such hints yet: their modes are keys only.)

Planned, each with the phase that makes it meaningful:

- **"b" -- bounding boxes** (phase 2): draw every shape as the phase-1
  box around it, to compare with the real rasterization.
- **"n" -- antialiasing on/off** (phase 6). v1 ships aliased (you *see* the
  jaggies, which is the point); then add coverage-based AA: for
  polygons, accumulate exact per-pixel area coverage along each span
  edge (Duff 1989 "Polygon scan conversion by exact convolution";
  Carpenter 1984 A-buffer; the approach libart/font-rs/stb_truetype
  use), for lines Wu 1991. Crow 1977 is the paper that named the
  problem.
- **"f" -- wireframe** (phase 3): draw every flattened polygon's outline with
  Bresenham instead of filling it -- shows how circles/ovals became
  polygons.
- **"i" -- image filtering** (phase 4): nearest vs bilinear.
- **"c" -- compare with Cairo:** *not* doable in-process (two
  implementations), so instead a separate offscreen test, see below.

## Scope for v1 (stated up front)

- No gamma-correct blending (blend in sRGB like most naive renderers;
  mention in `notes_2d.md` as a known inaccuracy).
- Hershey text will not look like the web/Cairo sans-serif font.
  Positioning (centering, size) should match; glyph shapes won't. A
  real outline-font rasterizer (parse a TrueType file's quadratic
  Béziers, flatten them via de Casteljau subdivision, fill with our own
  nonzero-winding + AA filler -- i.e. stb_truetype in OCaml) is a
  great **stretch phase**, since it reuses `Fill` wholesale, but it's
  a TTF parser worth of code on top.
- `Group` alpha: still a TODO, same as in `Shape_render_native`
  (would need an offscreen layer + composite; Porter-Duff again).
- Hershey font data: needs a vendored copy of the simplex Roman subset
  (~95 glyphs, a few KB). The Hershey fonts are freely usable with
  attribution; check the exact notice of the copy we vendor and keep it
  in the file header.

## Phasing

0. **DONE.** **Extract `native_common/`** (image decoding without Cairo, 2D SDL
   loop) out of `playground/native/`; verify every `examples/`/`games/`
   native demo behaves the same.
1. **DONE.** **Skeleton backend**: `Framebuffer` + SDL blit + event loop;
   `Shape_render_software` renders every form as its (transformed)
   bounding box in its color. Wire `examples/software/` for `Picture`
   and `Misc`. Proves the pipeline end to end, zero Cairo
   in `dune` `libraries`. In the end: `Affine` came in this phase
   already (the boxes need the real transforms, groups included), all
   examples and games are wired (`examples/software/`,
   `games/software/`, `copy_files ../*.ml`), and
   `playground/raster/tests/` has Testo tests checking the worked
   examples given in the `.mli` comments. Every box matched the
   position/size/rotation of the Cairo shape in side-by-side
   screenshots; 60 fps everywhere.
2. **DONE.** **`Fill`**: polygons, rectangles, ngons, groups/rotate/
   scale/move, alpha blending. Most games (`Snake`, `Tetris`, `Pong`,
   `Asteroid`) should now look right. In the end: scanline fill with an
   edge table + active edge list and incremental x (edge coherence),
   both Nonzero and Even_odd; the phase-1 boxes became the "b" key and
   are drawn with `Fill.polygon` too. Tests: worked examples (square,
   U, pentagram nonzero vs even-odd), "two triangles sharing an edge =
   the whole quad, pixel for pixel" (no gap, no double-painted pixel),
   and a comparison with Cairo without antialiasing: identical except
   one pixel per polygon, each within 0.005 pixel of an edge, which
   Cairo's 1/256-pixel fixed-point coordinates put on the other side
   (checked by hand: we're the exact ones). The test allows exactly
   that (< 0.01 pixel from an edge) and nothing else.
3. **DONE.** **`Circle`**: midpoint circle fast path + ellipse flattening for
   `Oval` and non-uniformly scaled circles. `Line` (Bresenham +
   clipping) and the "f" wireframe toggle. In the end: `Line.bresenham`
   and `Line.clip` (Cohen-Sutherland) are separate functions composed
   by `Line.draw`; `Circle.octant` is the midpoint walk alone, used by
   `Circle.fill` (one span per row, from the octant's half widths) and
   `Circle.outline`; a circle uses it when its transform is conformal
   (`circle_in_pixels`), else, like ovals, becomes a polygon with
   `segments_for_radius` sides (sagitta <= 1/4 pixel). The renderer is
   now one small function per way of drawing (`fill_polygon`,
   `outline_polygon`, `fill_circle`, `outline_circle`), `render_form`
   only choosing between them. Tests: the `.mli` examples (the
   (0,0)->(8,3) line with its error table, the r = 5 octant and its
   fill/outline pictures, segment counts), clipping (incl. the corner
   case needing two clips), a line from x = -1e9, each circle pixel
   painted once, and circles within 0.6 pixel of Cairo's.
4. **DONE.** **`Blit`**: images (nearest, then bilinear + "i"), animated GIFs via
   the shared decoder. `examples/Mario.ml` is the test. In the end:
   `Affine.invert`; `Blit.draw` by inverse mapping (the .mli's
   forward-leaves-holes picture), with the filter passed in
   (`sample_nearest`, `sample_bilinear`, separate functions);
   `Blit.image` has stb_image's layout, shared without copy. Bilinear
   is the default (like Cairo), "i" switches to nearest. Known
   simplification: bilinear mixes straight (not premultiplied) alpha,
   so sprite edges can darken slightly. Tests: invert, the 2x
   enlargement with no holes, a quarter turn, transparent pixels, the
   bilinear 70/30 example. Turtle and Mario look like Cairo's; the
   magnifier shows the nearest/bilinear difference; 57-60 fps.
5. **DONE.** **`Hershey`**: `Words`, FPS counter, "Loading...".
   `examples/Words.ml` is the test. In the end: `fonts/futural.jhf`
   (Hershey's Roman simplex, from github.com/kamalmostafa/hershey-fonts,
   with the required acknowledgements in `fonts/README.md`) embedded by
   a dune rule; `Hershey.decode_glyph`/`layout` (one glyph per char,
   em = 30 units); thin text as 1-pixel `Line`s, thick text (pen =
   1/12 em >= 1.5 pixels, e.g. Pong's score) with the new `Stroke`
   (rectangles + disks), filled with the new `Fill.polygons` in one
   go so the nonzero rule makes their union (each pixel painted once).
   FPS counter bottom-left on the Cairo backend's baseline, "Loading..."
   while images download. Tests: the "A" of `Hershey.mli`, layout, the
   union, a thick "V" painting each pixel once. Tutorial:
   `notes_font.md`.
6. **DONE.** **Antialiasing** ("n"): coverage AA for `Fill`, Wu lines. In the
   end: `Fill.scan` (the scanline walk alone, spans with exact ends)
   feeding either `Fill.polygons` (pixel centers) or `Fill.polygons_aa`
   (4 sub-rows per pixel row, exact horizontal overlap; the .mli's
   0.275/1.0/0.725 example); `Line.wu` + `Line.draw_aa`; antialiased
   circles are polygons; thick text via `Stroke.contours` +
   `polygons_aa`. On by default (like Cairo). Too slow at first (Snake
   5 fps), hence the optimizations, kept switchable with their simple
   originals (`raster/Opti`, "o" key), and a new
   `-uncapped` flag + `scripts/bench_playground.sh`; all measured in
   `notes_opti.md`. Tests: the coverage example, pixel-aligned = aliased,
   total coverage = area, the Wu example, optimized = simple.
7. **`notes_2d.md`** finalized (drafted incrementally from phase 1 --
   see below), plus a short perf/LOC write-up comparing against Cairo
   (the 2D twin of the `notes_playground3d_related_work.md`
   postscript: how many lines to replace Cairo, how many fps we lose).
8. *(stretch)* TrueType outline rasterizer replacing Hershey.
9. *(optional)* `playground3d/software/` HUD via `elm_playground_raster`
   instead of Cairo (making the 3D software renderer Cairo-free too).

## `notes_2d.md`: the companion tutorial

Same tone and structure as `notes_3d.md` ("from the ground up, with
pointers into the actual code"), written alongside the phases so each
section is backed by real code. Planned outline:

1. The big picture: a framebuffer is just an array of ints; vector
   graphics (shapes described by math) vs raster graphics (pixels);
   "rendering 2D" = turning the former into the latter.
2. Coordinates: Elm's centered y-up vs the framebuffer's top-left
   y-down; pixel centers at `+0.5` (Smith 1995, "A pixel is not a
   little square") and why off-by-half errors cause visible seams.
3. Affine transforms and homogeneous coordinates; `Group` as a scene
   graph; composing vs. Cairo's mutable CTM + save/restore.
4. Lines: DDA vs Bresenham's integer-only error term; why integer
   arithmetic mattered on 1960s plotters and still makes a tight loop.
5. Polygon filling: scanline + active edge table; even-odd vs nonzero
   winding (with an ASCII star showing the difference); the top-left
   fill convention that stops shared edges from being drawn twice.
6. Circles and ellipses: 8-way symmetry, the midpoint idea, and why
   general transformed ellipses are just flattened to polygons in
   practice (what Cairo, PostScript, and GPUs all do).
7. Transparency: alpha, Porter-Duff "over", premultiplied vs straight
   alpha, and the gamma caveat.
8. Images: forward vs inverse mapping (why forward mapping leaves
   holes), nearest vs bilinear, aliasing when minifying (mipmaps as a
   pointer to the 3D notes).
9. Text: vector (Hershey) vs bitmap vs outline (TrueType/Bézier) fonts;
   what FreeType/Cairo were doing for us.
10. Aliasing and antialiasing: Crow 1977, supersampling vs exact
    coverage, Wu lines.
11. Clipping: Cohen-Sutherland for lines, Sutherland-Hodgman for
    polygons, and why a scanline filler mostly gets clipping for free
    by clamping its y and x ranges.
12. How this compares to the 3D rasterizer (scanline vs edge functions)
    and to Cairo/Skia/GPUs; a references section with every paper
    cited in the code.

## References (to cite in code comments and `notes_2d.md`)

Double-check volume/page numbers against the actual papers when
writing each comment -- these are from memory.

- Sutherland, I. E. 1963. *Sketchpad: A man-machine graphical
  communication system.* AFIPS Spring Joint Computer Conference / MIT
  PhD thesis.
- Roberts, L. G. 1965. *Homogeneous matrix representation and
  manipulation of n-dimensional constructs.* MIT Lincoln Lab MS-1405.
- Bresenham, J. E. 1965. *Algorithm for computer control of a digital
  plotter.* IBM Systems Journal 4(1):25-30.
- Wylie, C., Romney, G. W., Evans, D. C., Erdahl, A. 1967. *Half-tone
  perspective drawings by computer.* AFIPS Fall Joint Computer
  Conference 31:49-58. (scanline polygon filling)
- Pitteway, M. L. V. 1967. *Algorithm for drawing ellipses or
  hyperbolae with a digital plotter.* The Computer Journal
  10(3):282-289.
- Hershey, A. V. 1967. *Calligraphy for computers.* NWL Report 2101,
  U.S. Naval Weapons Laboratory, Dahlgren.
- Newman, W. M., Sproull, R. F. 1973. *Principles of Interactive
  Computer Graphics.* McGraw-Hill. (Cohen-Sutherland line clipping)
- Sutherland, I. E., Hodgman, G. W. 1974. *Reentrant polygon
  clipping.* CACM 17(1):32-42.
- Catmull, E. 1974. *A subdivision algorithm for computer display of
  curved surfaces.* PhD thesis, University of Utah.
- Bresenham, J. E. 1977. *A linear algorithm for incremental digital
  display of circular arcs.* CACM 20(2):100-106.
- Crow, F. C. 1977. *The aliasing problem in computer-generated shaded
  images.* CACM 20(11):799-805.
- Porter, T., Duff, T. 1984. *Compositing digital images.* SIGGRAPH '84,
  Computer Graphics 18(3):253-259.
- Carpenter, L. 1984. *The A-buffer, an antialiased hidden surface
  method.* SIGGRAPH '84, Computer Graphics 18(3):103-108.
- Pineda, J. 1988. *A parallel algorithm for polygon rasterization.*
  SIGGRAPH '88. (the 3D side's edge functions, for the contrast)
- Heckbert, P. S. 1989. *Fundamentals of texture mapping and image
  warping.* Master's thesis, UC Berkeley.
- Duff, T. 1989. *Polygon scan conversion by exact convolution.* Raster
  Imaging and Digital Typography, Cambridge University Press.
- Heckbert, P. S. 1990. *Concave polygon scan conversion.* Graphics
  Gems, Academic Press.
- Foley, J. D., van Dam, A., Feiner, S. K., Hughes, J. F. 1990.
  *Computer Graphics: Principles and Practice*, 2nd ed., ch. 3 (the
  textbook treatment of almost everything above).
- Wu, X. 1991. *An efficient antialiasing technique.* SIGGRAPH '91,
  Computer Graphics 25(4):143-152.
- Smith, A. R. 1995. *A pixel is not a little square* (Tech Memo 6) and
  *Alpha and the history of digital compositing* (Tech Memo 7),
  Microsoft.

## Verification

- `dune build` after each phase; `dune ls`/`grep cairo
  playground/raster/dune playground/software/dune` stays empty (the
  "zero Cairo" invariant).
- **Pixel diff against Cairo**, at the primitive level: a Testo test
  (`tests/`) links `cairo2` and `elm_playground_raster` (no Playground
  implementation needed at all), draws the same primitives (polygons
  convex/concave/self-intersecting, circles, ellipses, rotated images)
  offscreen with both -- Cairo into a `Cairo.Image` surface, ours into a
  `Framebuffer` -- and asserts the fraction of differing pixels stays
  under a threshold (aliased vs antialiased edges will always differ by
  a 1-pixel fringe; interiors and positions must match). This is the 2D
  analogue of the 3D "pixel-identical TexturedCube3d screenshot" check,
  and catches off-by-one/half-pixel mistakes early. Whole-shape-tree
  differences (y-flip, `Group` transform order) can't be tested in one
  executable -- `Shape_render_native` and `Shape_render_software` live
  in two different implementations -- so those are covered by
  side-by-side screenshots of the same example (below).
- Side-by-side screenshots of `examples/software/` vs `examples/`
  (adapt `scripts/screenshot_playground3d.sh`) for `Picture`,
  `Animation`, `Mario`, `Words`, and each game.
- FPS at the default 1000x1000 window vs Cairo, per phase, logged the
  way `notes_3d_opti.md` logs 3D rasterizer optimizations.
