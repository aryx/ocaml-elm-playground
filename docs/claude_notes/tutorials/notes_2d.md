# 2D rendering, from scratch: a tutorial for `playground/software/`

How does a list of Playground shapes -- `circle red 50 |> move 100 0`,
`words black "Score"`, `image 70 70 "mario.gif"` -- become the pixels on
your screen? This note explains it from the ground up: what a pixel is,
which pixels a shape covers, how to draw lines, circles, polygons,
images and text, what transparency and antialiasing really are, and
where each idea came from -- with pointers into the actual code of this
repository's from-scratch 2D rasterizer, `graphics/` (the algorithms,
in plain OCaml, knowing nothing of the Playground: `graphics/core/`
the framebuffer and images, `graphics/2d/geometry/` transforms,
`graphics/2d/` rasterization, `graphics/font/` text) and
`playground/software/` (the Playground backend using them).

The 2D counterpart of [`notes_3d.md`](notes_3d.md). Companions:
[`done/plan_software_2d.md`](done/plan_software_2d.md) (how the backend was built,
phase by phase), [`notes_font.md`](notes_font.md) (text, in depth) and
[`notes_opti.md`](notes_opti.md) (what each feature costs, and the
optimizations); and for where all this comes from, and how it compares
with PostScript, SVG, Cairo, Skia, Processing, Gloss and the rest,
[`notes_playground_related_work.md`](notes_playground_related_work.md).

The same Playground programs also run on two other backends: Cairo
(`playground/native/`), a C library that does all of this for us, and
the browser (`playground/web/`), which gets SVG and does it too. Here
we do it ourselves, which is the only way to *see* how it works.

## 1. The big picture

A screen, or an image file, is a grid of **pixels** ("picture
elements"), each one color. In memory that's just a big array of
numbers: a **framebuffer** (`graphics/core/Framebuffer.ml`), here one 32-bit
integer per pixel, `0xAARRGGBB` (8 bits each of alpha, red, green,
blue). A 1000x1000 window is a million integers; drawing means writing
some of them.

A Playground shape, on the other hand, is *math*: a circle is a center
and a radius, a polygon a list of corners. That's **vector graphics**
(shapes described by geometry, drawable at any size) as opposed to
**raster graphics** (grids of pixels). Rendering 2D is converting the
first into the second, "rasterization" or "scan conversion":

```
  shape, in its own local coordinates
    (e.g. rectangle red 100 50 is the box from (-50, -25) to (50, 25))
      |
      | shape transform: its scale, rotate, move (and its groups')
      v
  world coordinates (Elm's: origin at the window's center, y up)
      |
      | screen transform: flip y, move the origin to the top-left
      v
  pixel coordinates (origin at the top-left corner, y down)
      |
      | rasterization: which pixels does the shape cover, how much?
      v
  pixels in the framebuffer
```

`playground/software/Shape_render_software.ml` is exactly this
pipeline: `shape_transform`, `screen_transform`, then `render_form`,
which hands the shape, now in pixel coordinates, to one of the
algorithms of `graphics/`.

The trick that makes the software backend short: SDL gives us the
window's pixels as a plain array (`Native_loop_2d.create_window`), and
`Framebuffer.of_pixels` makes a framebuffer *of that same memory*, so
drawing into the framebuffer is drawing into the window. SDL is only
asked to open the window, report the keyboard and mouse, and show the
pixels.

## 2. Pixels, and which ones a shape covers

Pixel (x, y) is column x, row y, counted from the top-left, y going
*down* -- the convention of screens, image files and SDL. Elm's
Playground uses math's convention instead: (0, 0) at the center of the
window, y going *up*. Section 3 converts between the two.

A pixel is not a point: think of pixel (x, y) as the unit square from
(x, y) to (x+1, y+1), whose **center** is (x + 0.5, y + 0.5). Then the
question "does this shape cover this pixel?" gets the rule used almost
everywhere (OpenGL, Direct3D, most 2D libraries): **a pixel belongs to a
shape if its center is inside the shape.** For example, a rectangle
from x = 10.2 to x = 12.7 covers the pixels whose centers 10.5, 11.5,
12.5 are inside it: x = 10, 11, 12.

```
  x = 10.2                    12.7
     |                          |
  +--|-----+--------+--------+--|-----+
  |  |  .  |   .    |   .    |  |.    |   . = pixel centers
  +--|-----+--------+--------+--|-----+       10.5, 11.5, 12.5 are in,
    10        11       12        13           13.5 is out
```

The first covered pixel is `ceil(xmin - 0.5)`, the first uncovered one
`ceil(xmax - 0.5)` (`Fill.first_pixel`). The +0.5 matters more than it
looks: with it, two shapes sharing an edge never both cover a pixel on
that edge, and never both miss one -- no gap, no pixel painted twice.
Getting it wrong by half a pixel shows up as thin seams between shapes,
or darker lines where transparent shapes meet (the test
"shared edge: no gap, no overlap" in `graphics/tests/Unit_fill.ml` checks
it).

Reference: Alvy Ray Smith, "A Pixel Is Not A Little Square" (Microsoft
Tech Memo 6, 1995), for why a pixel is really a *sample* at a point --
which is exactly what the center rule does.

## 3. Transforms: move, rotate, scale, and groups

Every transformation Playground needs -- move, rotate, scale, and the
flip from Elm's coordinates to pixels -- is an **affine transform**
(`graphics/2d/geometry/Affine.ml`): (x, y) goes to (a*x + c*y + tx, b*x + d*y + ty).
Written as a 3x3 matrix acting on (x, y, 1), a trick called
**homogeneous coordinates** (Roberts, 1965), a translation becomes a
matrix too, so *any* sequence of transforms is one matrix, their
product (`Affine.compose`).

Order matters, and it's the classic beginner's trap. For the point
(1, 0):

```
  rotate a quarter turn, then move right by 10:   (1, 0) -> (0, 1) -> (10, 1)
  move right by 10, then rotate a quarter turn:   (1, 0) -> (11, 0) -> (0, 11)
```

A Playground shape is scaled, then rotated, then moved
(`shape_transform`), like SVG's `translate(x,y) rotate(a) scale(s)`
(which also applies right to left): rotating *after* moving would swing
the shape around the window's center.

A `group` is a list of shapes with a transform of its own, applied to
all of them: `render_shape` composes the group's matrix with its
children's, recursively. Moving a group of 100 shapes is one matrix
product, not 100 moves. This "drawings made of transformed instances of
other drawings" idea goes back to Ivan Sutherland's Sketchpad (1963).

The **screen transform** is just one more affine transform, the one
from Elm's coordinates to pixels (`screen_transform`): flip y
(`scale 1 (-1)`), then move the origin to the window's center. For a
1000x1000 window, Elm's (0, 0) is pixel (500, 500), Elm's (0, 100) is
pixel (500, 400), 100 pixels *above*.

And its **inverse** (`Affine.invert`) goes back from the screen to the
shape's own coordinates, which is how images are drawn (section 8).

## 4. Filling polygons: the scanline algorithm

Rectangles, `ngon`s and `polygon`s are polygons: once their corners are
transformed to pixel coordinates, `Fill.polygon` fills them. The
classic **scanline** algorithm (Wylie, Romney, Evans, Erdahl, 1967):
process the pixels row by row, and on each row, look at the horizontal
line through the pixel centers, find where it crosses the polygon's
edges, sort those crossings from left to right, and fill between them:

```
     1  3   7  9            row y crosses 4 edges, at x = 1, 3, 7, 9:
     +--+   +--+            two "spans", [1, 3) and [7, 9)
     |  |   |  |
   --|##|---|##|--  row y
     |  +---+  |
     +---------+
```

Every filled shape in this renderer ends up as **spans**, horizontal
runs of pixels (`Framebuffer.fill_span`); the shapes only differ in how
each row's spans are computed.

Two ideas make it efficient (`Fill.scan`):

- an **edge table**: the edges sorted by the row where they start, and
  an **active edge list**, those crossing the current row, so each row
  only looks at the edges that matter;
- **edge coherence**: going down one row moves an edge's crossing by a
  constant amount, its slope dx/dy, so it's one addition per edge per
  row, not a line intersection.

For self-intersecting polygons "inside" needs a definition, the **fill
rule**. Even-odd: inside if a ray from the point crosses the edges an
odd number of times. Nonzero (SVG's, PostScript's, Cairo's default, and
ours): count +1 for each edge going down, -1 going up, the **winding
number**; inside if not 0. They only differ for shapes like a
pentagram, whose center is wound around twice: Nonzero fills it,
Even_odd leaves a hole (test "star: nonzero vs even-odd").

Nonzero has another use: several contours filled *together* make
their union if they turn the same way (1 + 1 = 2, still inside), and a
hole if they turn opposite ways (1 - 1 = 0) -- how a letter "O" is
drawn, and how thick lines are filled without painting a pixel twice
(section 9). `Fill.polygons` takes several contours for that.

Compared with Cairo without antialiasing, our pixels are identical,
except for a pixel here and there whose center is within a few
thousandths of a pixel of an edge: Cairo rounds coordinates to 1/256
pixel first (its fixed-point numbers), we compute in floating point
(test "same pixels as Cairo without antialiasing", which explains one
such pixel by hand).

## 5. Lines: Bresenham, and clipping

Nothing in Playground draws a line, but the debug views do (wireframe,
"f"), and so does thin text. A line 1 pixel wide: which pixels best
approximate it, when only whole pixels can be lit?

**Bresenham's algorithm** (1965, for a pen plotter whose motors could
only step to neighboring grid points -- the same problem): for a line
going mostly right, step one column at a time, and decide each time
whether to also step to the next row. Keep the "error", how far the
exact line is from the current row's center; when it exceeds half a
pixel, move to the next row. Multiplying everything by 2*dx makes it
integers only (`Line.bresenham`, whose `.mli` walks through the line
from (0, 0) to (8, 3), with the error at each step):

```
       x: 0 1 2 3 4 5 6 7 8
  y = 0   # #
  y = 1       # # #
  y = 2             # #
  y = 3                 # #
```

The older alternative, the DDA (add the slope to a floating-point y and
round), gives similar pixels; Bresenham's point was avoiding floating
point, and division, on 1960s hardware. It generalizes: the midpoint
circle (next section) is the same idea, and Bresenham-style
"incremental error" loops are everywhere in graphics.

**Clipping**: a line from x = -1000000 to the screen would visit a
million invisible pixels. **Cohen-Sutherland** (1967) first cuts the
segment to the window (`Line.clip`): each end gets a 4-bit code (left,
right, above, below of the window); both 0: keep it; a common 1 bit:
both ends are beyond the same side, drop it; else cut at a window side
and try again. The `.mli` has the 9 regions and their codes. `Line.draw`
is just `clip`, then `bresenham`: two ideas, two functions.

## 6. Circles and ellipses

**The midpoint circle algorithm** (Bresenham, 1977; Pitteway, 1967, for
any conic): compute one eighth of the circle, the "octant" from the top
going right until the diagonal, and mirror each pixel into the 7 others
by symmetry (`Circle.octant`). In the octant, each step goes right by
1, and the question is only whether to also go down by 1: look at the
point half way between the two candidates (the "midpoint"); inside the
circle, stay; outside, go down. "Inside" is x^2 + y^2 < r^2, a quantity
that changes by a simple integer amount from one step to the next, so
again: additions only. `Circle.ml` derives those amounts by expanding
the squares.

Filling uses it too (`Circle.fill`): the octant gives, for each row,
how far the circle extends left and right, then one span per row --
each pixel painted exactly once, which matters with transparency.

But the midpoint algorithm works on the *pixel grid*: its center is a
pixel and its radius a whole number of pixels, so a circle can be half
a pixel off, and it can only say "in" or "out" of each pixel. That's
why modern renderers (Cairo, PostScript, GPUs, and ours as soon as
antialiasing or a non-uniform scale is involved) draw curves as
**polygons with many sides** instead (`Circle.ellipse_points`), with
just enough sides that the polygon is never more than 1/4 pixel inside
the true curve. The gap in the middle of a side, the *sagitta*, is
r * (1 - cos(pi/n)), so a 10-pixel radius needs 15 sides and a
400-pixel one 89 (`Circle.segments_for_radius`). Wireframe ("f") shows
them. Ovals are always polygons: there's no midpoint algorithm for a
*rotated* ellipse.

When does a circle stay a circle? When its transform only moves,
rotates, flips, and scales equally in every direction: the matrix's two
columns are perpendicular and of the same length
(`circle_in_pixels`).

## 7. Transparency: Porter-Duff "over"

`fade 0.5` makes a shape half transparent. Painting a color `src` with
opacity `alpha` over what's there, `dst`, is, channel by channel, a
weighted average (`Framebuffer.blend`):

```
  result = src * alpha + dst * (1 - alpha)
  e.g. half-transparent red over white: 0xff0000 * 0.5 + 0xffffff * 0.5 = 0xff8080, pink
```

This is Porter and Duff's "over" (1984), one of the 12 compositing
operators they defined, the one everybody means by "transparency". The
"t" key turns it off (a faded shape then is fully opaque or invisible).

Two subtleties, both simplified here:

- **premultiplied alpha**: storing colors already multiplied by their
  alpha makes compositing and filtering (section 8) correct and
  cheaper; Cairo does, we don't (so bilinear filtering can slightly
  darken a sprite's edges);
- **gamma**: pixel values are not proportional to light (sRGB is
  roughly the square root of it), so averaging them, as `blend` does,
  isn't physically right; 50% gray looks darker than it should.
  Correct renderers convert to linear light, blend, and convert back.

And a TODO: fading a `group` should fade it as a whole (render it
alone, then blend the result), not each child separately, which lets
overlapping children show through each other. Neither this backend nor
the Cairo one does it yet.

## 8. Images: inverse mapping, nearest and bilinear

`image 70 70 "mario.gif"` draws a 35x35-pixel sprite into a 70x70 box,
possibly rotated. The obvious way, going through the *image's* pixels
and computing where each lands on screen ("forward mapping"), leaves
holes as soon as the image is enlarged: two neighboring image pixels
land two screen pixels apart, and nobody paints the one in between. So
we go the other way (`Blit.draw`): through the *screen's* pixels that
the image may cover, and for each, find where its center comes from in
the image, with the inverse transform ("inverse mapping", Heckbert,
1989). Every screen pixel gets exactly one color.

That point in the image generally falls between image pixels; which
color to take is **filtering**:

- **nearest**: the image pixel containing the point; blocky when
  enlarged, the look of pixel art (Mario asks for it, with
  `Playground.rendering`'s `smooth_images = false`);
- **bilinear**: mix the 4 image pixels whose centers surround the point,
  each weighted by how close it is, along x then along y; smooth
  (`Blit.sample_bilinear`, with a picture in `Blit.mli`).

The "i" key switches between them; with the magnifier ("z") on
`examples/software/Turtle.exe`, the difference is obvious. Shrinking an
image is harder: many image pixels fall into one screen pixel, and
taking just one or four of them flickers and shimmers (aliasing,
section 10); the real fix is precomputed smaller versions of the image,
"mipmaps" (Williams, 1983), which neither this backend nor the 3D
software rasterizer has yet (GPUs do them in hardware).

Loading images: `graphics/images/Image_decode.ml` (shared with the
Cairo backend) downloads and decodes them, animated GIFs included: it
decodes all their frames once, and each frame of the game picks the
one to show from the clock, like browsers.

## 9. Text, and thick lines

Text is covered in depth in [`notes_font.md`](notes_font.md). In short:
the software backend uses a **stroke font**, Hershey's "Roman simplex"
(1967), where each letter is a few pen strokes (`graphics/font/Hershey.ml`), so
drawing text only needs drawing lines. Small text is 1-pixel lines
(Bresenham or Wu). Big text needs **thick lines**, which are *areas*,
not lines: each segment becomes a rectangle, each point a disk (round
joins and ends), all filled together with the nonzero rule so their
union is painted once (`graphics/2d/Stroke.ml`, PostScript's "stroking").

## 10. Aliasing and antialiasing

Look at a slanted edge with the magnifier ("z"): a staircase of whole
pixels, the "jaggies". That's **aliasing**: sampling each pixel at one
point (its center) can't represent details finer than a pixel, the same
phenomenon as a spinning wheel looking like it turns backwards in a
film. Crow (1977) explained it in these terms, and the cure: instead of
"is the center inside?", yes or no, compute *how much* of the pixel is
inside, from 0 to 1, and paint the pixel that opaque (**coverage**).
Edges get intermediate shades and look smooth. The "n" key turns it on
and off.

Our coverage (`Fill.polygons_aa`) scans 4 sub-rows per pixel row, and
adds up, for each pixel, the exact horizontal overlap of each sub-row's
spans. The `Fill.mli` example: 3 pixels, 4 sub-rows,

```
      pixel 0   pixel 1   pixel 2
     +--------+---------+---------+
     |    ====|=========|====     |   [0.5, 2.5)
     |     ===|=========|=====    |   [0.6, 2.6)
     |       =|=========|=======  |   [0.8, 2.8)
     |        |=========|=========|   [1.0, 3.0)
     +--------+---------+---------+
       0.275     1.0       0.725      coverage
```

Exact horizontally, sampled 4 times vertically: a middle ground between
plain supersampling (many samples per pixel, e.g. the A-buffer's
subpixel masks, Carpenter 1984) and computing exact areas (Duff, 1989).
Circles are polygons then (section 6), and thin lines use **Xiaolin
Wu's algorithm** (1991, `Line.wu`): like Bresenham, but lighting the
*two* pixels the exact line passes between, sharing the intensity by
distance.

Antialiasing costs: 1.1 to 3 times the frame time here, and it's also
where the biggest optimization was (section 13).

## 11. Where clipping happens

A shape can be partly or entirely outside the window. Each algorithm
clips where it's cheapest:

- `Framebuffer.fill_span` clips every span to the framebuffer's width,
  and ignores rows outside it: everything that fills is safe;
- `Fill.scan` clips edges to the rows of the framebuffer when building
  the edge table, so a shape far below the window costs nothing;
- `Line.clip` (Cohen-Sutherland) clips lines before walking them;
- `Blit.draw` only visits the box around the transformed image,
  clipped to the framebuffer.

The polygon version of Cohen-Sutherland, Sutherland-Hodgman (1974),
clips a polygon to a window edge by edge; a scanline filler mostly
doesn't need it, as the list above shows.

## 12. Seeing it: the debug keys and the magnifier

Every feature of the software backend can be turned off while any
example or game runs, to see what it does, when it's run with
`-debug-keys` (e.g. `dune exec examples/software/Picture.exe --
-debug-keys`; without it, all keys go to the app, so a game can use
any key); the window title shows the state of each key
(`software/Playground_platform.ml`):

| key | what it switches | try it on |
|-----|------------------|-----------|
| `t` | transparency (Porter-Duff blending) | `examples/software/Mouse.exe`, button held |
| `b` | bounding boxes instead of the real shapes | any |
| `f` | wireframe: outlines only | `Smiley.exe` (the mouth is two ovals) |
| `i` | image filtering: bilinear or nearest | `Turtle.exe`, with `z` |
| `n` | antialiasing | anything, with `z` on an edge |
| `o` | optimizations: the original, simple code | watch the fps counter |
| `z` | the pixel magnifier, following the mouse | everything |
| `h` | help: all the keys and their state, over the frame | everything |

The magnifier (`graphics/2d/Magnifier.ml`) shows the 32x32 pixels under the
mouse enlarged 8 times, with a grid between pixels: the tool to *see*
the pixel-center rule, gaps between shapes, jaggies, and what
antialiasing does to an edge. A game can also choose the starting
values of "n" and "i" (portably, for all backends) with
`Playground_platform.run_app ~rendering`.

Each feature is its own function (e.g. `Line.draw` is `Line.clip` then
`Line.bresenham`; `render_form` picks `fill_polygon` or
`outline_polygon`), so the code can be read one feature at a time, in
the order of this note. And `graphics/tests/` checks the worked examples
of the `.mli` files, so they can't silently become wrong.

## 13. Performance

Measured in [`notes_opti.md`](notes_opti.md): Cairo (tuned C, SIMD) is
1.5 to 7 times faster than our plain OCaml, and each feature has its
price (antialiasing 1.1x to 3x). Three optimizations made the software
backend playable, each keeping its original, simple version runnable
next to it (`graphics/core/Opti.ml`, the "o" key):

1. antialiasing coverage as sparse "cells" (a few per span) instead of
   updating every pixel of every span;
2. `Framebuffer.plot` writing a pixel directly instead of through a
   1-pixel span (which allocated two array views per pixel);
3. images with forward differencing (one addition per pixel instead of
   a matrix product) and inlined filters.

The common lesson: in a pixel loop, what costs is not the arithmetic
but what happens per pixel *besides* the arithmetic -- allocations,
function calls, boxed floats. And a GPU does all of sections 4 to 10 in
fixed-function hardware, for millions of pixels, in parallel: after
writing it by hand, you appreciate what "free" means.

## 14. Compared with Cairo, the GPU, and the 3D rasterizer

**Size.** The whole from-scratch renderer: about 775 lines of code
(`graphics/`: 540, `playground/software/`: 235), plus about twice as many lines of
comments, and 540 lines of tests. The Cairo backend is 235 lines of
OCaml too -- but it delegates the actual work to Cairo, pixman,
FreeType and fontconfig, C libraries of tens of thousands of lines
each. The difference is what's in them: decades of edge cases, SIMD
code paths, font formats, hinting, output devices.

**The GPU.** A GPU rasterizes *triangles* only, with **edge functions**
(Pineda, 1988): for every pixel in a triangle's bounding box, evaluate
3 linear functions, one per edge, and the pixel is inside if all three
are positive. That tests pixels the triangle doesn't cover, which a
scanline filler never does, but every pixel is independent: perfect for
thousands of parallel units. 2D on a GPU then means triangulating every
shape first (or tricks like stencil-then-cover). Our 3D software
rasterizer (`playground3d/software/`, see `notes_3d.md`) uses edge
functions like a GPU; this 2D one uses scanlines, like the classic 2D
libraries: same problem, two classic answers.

## 15. What's missing, and exercises

Things real 2D libraries have that this one doesn't, each a good
exercise, in rough order of difficulty:

- an even-odd toggle key (the fill rule is already a parameter of
  `Fill.polygons`);
- gamma-correct blending (section 7): convert to linear light, blend,
  convert back, and compare with the magnifier;
- premultiplied alpha, for images and blending;
- group alpha (section 7), with an offscreen framebuffer;
- stroked outlines for any shape (a `Stroke` of the shape's own
  outline, with miter or bevel joins, not just round ones);
- gradients (a color computed from each pixel's position, in
  `fill_span`'s place);
- exact-area antialiasing instead of 4 sub-rows (Duff 1989; the
  "signed area accumulation" of font-rs, see `notes_font.md`);
- flood fill (painting a region of same-colored pixels, Smith 1979),
  the paint program's bucket tool;
- a TrueType outline font instead of Hershey (see `notes_font.md`,
  section 7).

## References

- Ivan E. Sutherland, "Sketchpad: A Man-Machine Graphical Communication
  System", MIT PhD thesis, 1963.
- Lawrence G. Roberts, "Homogeneous Matrix Representation and
  Manipulation of N-Dimensional Constructs", MIT Lincoln Laboratory
  MS-1405, 1965.
- Jack E. Bresenham, "Algorithm for computer control of a digital
  plotter", IBM Systems Journal 4(1):25-30, 1965.
- C. Wylie, G. W. Romney, D. C. Evans, A. Erdahl, "Half-tone
  perspective drawings by computer", AFIPS Fall Joint Computer
  Conference, 1967.
- M. L. V. Pitteway, "Algorithm for drawing ellipses or hyperbolae with
  a digital plotter", The Computer Journal 10(3):282-289, 1967.
- A. V. Hershey, "Calligraphy for Computers", NWL Report 2101, U.S.
  Naval Weapons Laboratory, 1967.
- William M. Newman, Robert F. Sproull, "Principles of Interactive
  Computer Graphics", McGraw-Hill, 1973 (Cohen-Sutherland clipping).
- Ivan E. Sutherland, Gary W. Hodgman, "Reentrant polygon clipping",
  Communications of the ACM 17(1):32-42, 1974.
- Edwin Catmull, "A Subdivision Algorithm for Computer Display of
  Curved Surfaces", PhD thesis, University of Utah, 1974.
- Jack E. Bresenham, "A linear algorithm for incremental digital
  display of circular arcs", Communications of the ACM 20(2):100-106,
  1977.
- Franklin C. Crow, "The aliasing problem in computer-generated shaded
  images", Communications of the ACM 20(11):799-805, 1977.
- Alvy Ray Smith, "Tint Fill", SIGGRAPH '79.
- Lance Williams, "Pyramidal Parametrics", SIGGRAPH '83 (mipmaps).
- Thomas Porter, Tom Duff, "Compositing Digital Images", SIGGRAPH '84.
- Loren Carpenter, "The A-buffer, an antialiased hidden surface
  method", SIGGRAPH '84.
- Juan Pineda, "A Parallel Algorithm for Polygon Rasterization",
  SIGGRAPH '88.
- Paul S. Heckbert, "Fundamentals of Texture Mapping and Image
  Warping", Master's thesis, UC Berkeley, 1989.
- Tom Duff, "Polygon scan conversion by exact convolution", Raster
  Imaging and Digital Typography, 1989.
- Paul S. Heckbert, "Concave Polygon Scan Conversion", Graphics Gems,
  Academic Press, 1990.
- Foley, van Dam, Feiner, Hughes, "Computer Graphics: Principles and
  Practice", 2nd ed., Addison-Wesley, 1990 (chapter 3: almost
  everything above).
- Xiaolin Wu, "An efficient antialiasing technique", SIGGRAPH '91.
- Alvy Ray Smith, "A Pixel Is Not A Little Square", Microsoft Tech
  Memo 6, 1995.
