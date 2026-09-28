# The software 2D rasterizer: what each feature costs, and the optimizations

Purpose: track how fast `playground/platforms/software/` (our from-scratch 2D
rasterizer, see [`done/plan_software_2d.md`](../plans/done/plan_software_2d.md)) draws the
examples and games, what each rendering feature costs, and each
optimization: what it changed, why the simpler code before it was slow,
and measured before/after numbers. The 2D counterpart of
[`notes_3d_opti.md`](notes_3d_opti.md).

The point, besides making the games playable: every feature below is
something a GPU (or Cairo, with hand-tuned C and SIMD) gives you
"for free". Measuring what it costs when you do it yourself, in plain
OCaml on the CPU, is the best way to appreciate hardware rendering.

## How the numbers are measured

- `scripts/perf/bench_playground.sh <exe> [keys]`: runs the app with
  `-uncapped` (no 60 fps cap, see `Native_loop_2d`) and `-debug` (the
  loop logs its fps every half second), presses the software backend's
  debug keys with `scripts/input/xdrive.py` (checking the window title shows
  the change), waits for things to settle, and prints the median fps.
  `REPEAT=3` runs it 3 times: median (min-max).
- Default window, 1000x1000. Machine: ARM Neoverse-N1, 64 cores (only
  one used: everything is single-threaded), OCaml 4.14.2 native code.
- Configurations, all on the same binary thanks to the debug keys:
  - **optimized**: everything on (the default: antialiasing, bilinear
    image filtering, blending) with the optimizations (`Opti.enabled`);
  - **"o"**: the same, with the original, simple code instead of the
    optimized one (`Opti`, see below);
  - **"n"**: antialiasing off.
- Noise: runs vary (other programs, the window manager), so single
  runs can be off by 2x; trust medians of several runs. Two pitfalls
  found the hard way: a key press not reaching the app (the script now
  checks the title and retries), and images loaded from the network
  failing to download (then they're not drawn, and the scene is
  artificially fast; the script warns "(image failed)").

## Cairo vs ours

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

(Cairo: single runs; ours: medians of 3.) Cairo is antialiased too, and
still 1.5 to 7 times faster: pixman, the pixel library under it, is
decades of tuned C with SIMD, compositing whole spans at a time,
caching glyphs, and so on. Ours computes everything per span or per
pixel, in plain OCaml, with allocations in the inner loops.

## What each feature costs

**Antialiasing** ("n"): 1.1x to 3x slower. Computing *how much* of each
edge pixel is covered (4 sub-rows per pixel row, then blending the
pixel) costs much more than "is its center inside?". Tetris pays the
most (61 vs 184): many small texts, drawn with Wu's antialiased lines,
pixel by pixel. On a GPU, antialiasing (MSAA) is a hardware feature:
several coverage samples per pixel, resolved at the end.

**Images** (Turtle, Mario): before optimization 3 below, Turtle's one
192x192 turtle took most of its frame time, even without antialiasing
(55 fps).

## The optimizations

Each optimization keeps the original, simple code next to it, runnable:
`graphics/core/Opti.ml`'s `Opti.enabled` switches between them, the
"o" key flips it while a game runs. For teaching, the simple version
explains the idea, the optimized one shows the craft, and "o" shows
what it buys. A test (`Unit_antialiasing`, "optimized = simple") checks
they paint the same pixels.

### 1. Antialiasing: coverage by differences, painted by runs

- **The simple version** (`Fill.polygons_aa_simple`): a coverage array
  for the current pixel row; each sub-row's span adds its overlap to
  *every* pixel it touches, one by one; then each covered pixel is
  plotted with its coverage. A span across Pong's 1000-pixel-wide
  background: 1000 additions per sub-row, 4 sub-rows, and 1000 single
  pixel plots per row, a million per frame for the background alone.
- **First fix** (an intermediate version, never committed, described
  here only): a *difference array*. A span adds its partial coverage to its two end
  pixels only, and "+1/4 from here, -1/4 from there" for the fully
  covered pixels between them; a running sum at the end of the row
  rebuilds every pixel's coverage. Consecutive pixels of equal coverage
  are painted as one span. Adding a span costs O(1); but the end of each
  row still walks every pixel of the row.
- **Second fix, kept** (`Fill.polygons_aa_sparse`): the same differences,
  but as a short list of *cells* (pixels where something changes),
  sorted and walked at the end of the row: between two cells the
  coverage is constant, one span. A row costs a few cells per span, not
  one visit per pixel. The idea of the cell lists in libart and
  Anti-Grain Geometry.

Pong (uncapped fps): simple 18 -> difference array 126 (single run) ->
cells (+ fix 2 below) 166. Snake: 4.6 -> 76 -> 164. Tetris: 19 -> 52
-> 61. Before any of this, with the 60 fps cap on, Snake ran at 5 fps:
unplayable.

### 2. `Framebuffer.plot`: write the pixel directly

- **The simple version** (`Framebuffer.plot_simple`): a pixel is a span
  of length 1, `fill_span fb ~y ~x0:x ~x1:(x + 1)`. Obviously right,
  one code path for everything.
- **The problem**: `fill_span`'s fast path takes two Bigarray *views*
  of the row (`Array2.slice_left`, then `Array1.sub`), each a small
  allocation, then fills them. Worth it for a span of 800 pixels; for
  one pixel, the views cost far more than the write. And images
  (inverse mapping, one pixel at a time), Wu's lines, and antialiased
  edge pixels all plot single pixels.
- **The fix** (`Framebuffer.plot`): bounds check, then write the pixel
  (blending if needed) directly into the Bigarray.

Turtle: 4.3 -> 51 fps; Mario: 4.3 -> 128: 12x and 30x, for 5 lines of
code. (The "o" numbers are the simple versions of *both* optimizations;
Turtle and Mario draw mostly images, which only use `plot`.)

**Lesson**: in a pixel loop, what matters is not the arithmetic but what
happens per pixel that isn't arithmetic: allocations, bounds checks,
function calls, boxing of floats. Both optimizations remove per-pixel
overhead, not math.

### 3. Images: forward differencing, samplers inlined

- **The simple version** (`Blit.draw_simple`): for each covered screen
  pixel, a matrix product (`Affine.apply inverse`) to find where its
  center comes from in the image, then the filter, a function returning
  a `color` record (`sample_nearest`, `sample_bilinear`). Clear, one
  idea per line.
- **The problem**: per pixel, the (u, v) pair from `Affine.apply`, a
  `color` record per texel read (4 for bilinear) and one per `lerp` (3
  more): about 10 small allocations, plus boxed floats, for a few dozen
  arithmetic operations. About half a microsecond per pixel.
- **The fix** (`Blit.draw_fast`):
  - *forward differencing*: one pixel to the right on the screen is
    always the same step in the image, the inverse matrix's first
    column `(inverse.a, inverse.b)`, so compute (u, v) once per row and
    then add the step: two additions instead of a matrix product. The
    same idea as the edge coherence in `Fill` (and as how scanline
    texture mappers of the 1990s worked);
  - the filter inlined, on local numbers (which OCaml keeps unboxed,
    in registers), producing the final 0xRRGGBB directly: no records.
  `Blit.draw` takes a `~filter:Nearest|Bilinear` instead of a sampling
  function for that (the fast path must know which filter to inline).

Turtle: 51 -> 102 fps; Mario: 128 -> 176 (medians of 3). The test
"optimized = simple" allows 2 per color channel of difference: the
simple bilinear rounds to integers after each of its 3 mixes, the fast
one only at the end.

## Next

- **Antialiased thin text** (Tetris, 61 fps vs 184 without
  antialiasing): Wu's lines plot 2 pixels per column, each with a
  blend; many small texts add up.
- **The background**: with antialiasing, even a plain full-window
  rectangle goes through coverage cells for 1000 rows (Turtle: 102 fps
  with antialiasing, 125 without). A shape whose edges are exactly on
  pixel boundaries needs no antialiasing at all: a special case worth
  detecting.
