# Fonts and text rendering, from scratch: a tutorial

How does a computer draw the letter "A"? This note explains the
concepts behind text rendering (glyphs, the em, baselines, the three
families of fonts, hinting, antialiasing, shaping), where they came
from historically, which libraries do it for real today, and exactly
what this repository's from-scratch software rasterizer does -- with
pointers into the code (`graphics/font/Hershey.ml`,
`graphics/2d/Stroke.ml`, `playground/software/Shape_render_software.ml`).

Companion to [`done/plan_software_2d.md`](../plans/done/plan_software_2d.md) (the software
2D backend's plan, phase 5 being text) and to [`notes_2d.md`](notes_2d.md)
(2D rasterization in general).

## 1. The big picture

Text rendering turns a *string* ("Score: 42") into *pixels*, in three
steps that are worth keeping apart, because different libraries do
different steps:

```
  "Score: 42"                        the characters (Unicode code points)
      |
      | 1. shaping: which glyphs, in which order, where?
      v                                (ligatures, kerning, right-to-left,
  [S][c][o][r][e][:][ ][4][2]           Arabic joining, Indic reordering...)
  + an x position for each
      |
      | 2. glyph lookup: each glyph's shape, from the font file
      v                                (a bitmap, strokes, or outlines)
  shapes at positions
      |
      | 3. rasterization: which pixels, how dark?
      v                                (scan conversion, hinting,
  pixels                                antialiasing)
```

A *character* is an abstract letter ("A", "é", "字"); a *glyph* is a
drawing of it in a particular font. They're not one-to-one: "fi" is
often a single glyph (a *ligature*), and "é" can be two glyphs ("e" and
an accent) placed on top of each other.

## 2. The vocabulary of type

These words come from 500 years of metal type, where each glyph was a
small metal block ("sort"), and they still name the numbers inside
every font file:

```
                 advance width (where the next glyph starts)
          |<------------------------------------------->|
          |                                             |
   ascent |            *****             *              |  - - ascender line
      ^   |           *     *            *              |
      |   |           *     *            *              |  - - cap height
      |   |           *******    ****    * ***          |
      |   |           *     *   *    *   **   *         |  - - x-height
      |   |           *     *   *    *   *    *         |
  ----+---o-----------*-----*----****----*****----------+--- baseline
      |   origin                                 *      |
      v                                          *      |  - - descender line
  descent

                 A        o        b   (and p would go below the baseline)
```

- **Baseline**: the line letters sit on. Descenders ("p", "g", "y") go
  below it.
- **Origin**: where the glyph is drawn from, on the baseline.
- **Advance width**: how far to move right after drawing the glyph --
  not the same as the ink's width: "i" has a narrow advance, and some
  glyphs' ink sticks out of their advance (an italic "f").
- **Em**: the font's size unit. Historically the height of the metal
  block (and the width of an "M" in many fonts); in a font file, a
  square of e.g. 1000 (PostScript) or 2048 (TrueType) *font units* in
  which glyphs are designed. A 12-point font is one whose em is 12
  points tall. Capital letters are usually about 0.7 em tall.
- **Kerning**: per-pair adjustments of the advance: in "AV" the V tucks
  under the A.

## 3. Three families of fonts

### Bitmap fonts: a grid of pixels per glyph

```
  . . # # . .
  . # . . # .      the simplest possible font: store the pixels.
  # . . . . #      Fast (copy the bits), perfect at its one size, and
  # # # # # #      ugly at any other: scaled up, you see the blocks;
  # . . . . #      scaled down, strokes vanish. One set of bitmaps per
  # . . . . #      size, and none for rotations.
```

Terminals, the IBM PC's text mode (an 8x16 font in ROM), early Macs and
X11 (the BDF format) all used bitmap fonts. They're still used where
their weakness doesn't matter: fixed-size pixel art, tiny embedded
displays.

### Stroke (vector) fonts: lines drawn with a pen

A glyph is a few *paths for a pen*: polylines, with pen-up moves in
between. This is what **Hershey's fonts** are (1967), designed by Allen
V. Hershey for plotters and vector displays -- devices that draw lines,
not pixels. They scale and rotate perfectly (it's just transforming
points), they're tiny (an "A" is 5 points), and drawing them needs
nothing but a line-drawing algorithm. Their limit: the pen has one
width, so there's no thick-and-thin contrast like in printed text.

```
  Hershey's "A": 3 strokes          (0,-12)
                                       /\
   RF to J[    left leg               /  \
   RF to Z[    right leg      (-5,2) /----\ (5,2)
   MT to WT    bar                  /      \
                             (-8,9)          (8,9)
```

They live on wherever a machine draws with a tool along lines: CNC
engraving, plotter software, some CAD programs -- and in this
repository, see section 7.

### Outline fonts: the shape's boundary, filled

A glyph is the *outline* of its ink, closed curves made of straight
lines and **Bézier curves**, and drawing it means *filling* the
inside. Scalable like stroke fonts, but with any shape the designer
wants: thick and thin strokes, serifs, calligraphy.

```
       ______                 the "O" is two contours: the outer one
     /  ____  \               turning one way, the inner one the other
    |  /    \  |              way; with the nonzero rule, the inside
    | |      | |              of the inner contour has winding number
    | |      | |              1 + (-1) = 0, so it's a hole
    |  \____/  |              (see graphics/2d/Fill.mli)
     \ ______ /
```

A Bézier curve is defined by control points; a *quadratic* one by 3
(start, a control point pulling the curve, end), a *cubic* one by 4. To
fill it, rasterizers *flatten* it into short line segments, usually by
de Casteljau's subdivision (cut the curve in two halves until each is
flat enough) -- the same idea as `Circle.segments_for_radius` for our
ellipses.

## 4. Getting pixels right: hinting, antialiasing, subpixel rendering

At 12 pixels per em, a stem is about 1 pixel wide, and "1 pixel wide"
can land on 1 pixel or straddle 2 at half darkness each: the same
letter looks sharp or blurry depending on where it falls. Three tricks,
from most to least radical:

- **Hinting** (*grid fitting*): distort the outline slightly so that
  stems and heights land on whole pixels at this size. TrueType fonts
  contain actual *programs* (bytecode for a small virtual machine) that
  move the outline's points at each size; FreeType's "autohinter" does
  it without them. Less and less used on high-resolution screens.
- **Antialiasing**: instead of "is this pixel's center inside?" (what
  our `Fill` does), compute *how much* of the pixel is covered, and use
  that as its opacity: edges get intermediate shades, and look smooth.
  Phase 6 of `done/plan_software_2d.md` is exactly this.
- **Subpixel rendering** (e.g. Microsoft's ClearType, around 2000): an
  LCD pixel is three thin red, green, blue bars side by side, so
  antialias at 3 times the horizontal resolution, one channel per bar.

## 5. Shaping: from characters to positioned glyphs

For English with a simple font, shaping is "one glyph per character,
advance by its width, apply kerning". For most of the world's writing
systems it's far harder: Arabic letters change shape depending on their
neighbors and are written right to left, Indic scripts reorder and
combine characters, fonts substitute ligatures. A *shaping engine* reads
the font's rules (OpenType's GSUB/GPOS tables) and does all of that.
This is why "just draw each character's glyph" (what we do) only works
for simple scripts.

## 6. A short history

- **1967**: Hershey, at the U.S. Naval Weapons Laboratory, digitizes
  more than 2000 glyphs (Latin, Greek, Cyrillic, Japanese, symbols) as
  strokes for plotters ("Calligraphy for Computers", NWL Report 2101).
  They're later distributed through the National Bureau of Standards.
- **1970s**: bitmap fonts on raster terminals and workstations; at
  Xerox PARC, proportional bitmap fonts on the Alto's bitmapped screen.
- **1977-1979**: Donald Knuth's **Metafont**, where glyphs are drawn by
  programs that move a pen along curves (a stroke font taken to its
  limit), for TeX's Computer Modern.
- **1984-1985**: Adobe's **PostScript**, with **Type 1** outline fonts
  (cubic Bézier curves, hints), a printer language that made scalable
  type the norm for desktop publishing.
- **1991**: Apple's **TrueType** (quadratic curves, hinting programs),
  in System 7, then licensed to Microsoft for Windows 3.1.
- **1996**: Microsoft and Adobe's **OpenType**, a TrueType-based
  container that can hold either kind of outline, plus shaping tables.
- **1996 on**: **FreeType**, the open-source font rasterizer (David
  Turner, Robert Wilhelm, Werner Lemberg), now in Linux, Android, iOS
  and most browsers.
- **2000s**: **HarfBuzz** (Behdad Esfahbod), the open-source shaping
  engine, now in Chrome, Firefox, Android, LibreOffice, ...
- **2007**: Chris Green (Valve), "Improved Alpha-Tested Magnification
  for Vector Textures and Special Effects": store glyphs as **signed
  distance fields** in a texture, so a GPU can draw crisp text at any
  scale -- the standard trick in games today.
- **2016**: OpenType **variable fonts**: one file, a continuous range of
  weights and widths.

## 7. What this repository does

### The Cairo backend (`playground/native/`)

`Shape_render_native.render_words` calls Cairo's "toy" text API:
`Cairo.select_font_face "sans-serif"`, `Cairo.set_font_size`,
`Cairo.show_text`. Behind it: fontconfig finds a sans-serif font file
on the system, FreeType loads its outlines and rasterizes them
(hinted, antialiased), and Cairo composites the result. Shaping is
minimal (the toy API does no complex scripts; real apps use Pango +
HarfBuzz on top of Cairo). The web backend asks the browser, which does
all of section 1 with HarfBuzz, FreeType-or-the-OS's-rasterizer, and
the GPU.

### The software backend (`playground/software/`), from scratch

Every step, in ~200 lines, with a stroke font:

1. **The font**: `graphics/font/fonts/futural.jhf`, Hershey's "Roman
   simplex", embedded in the library as a string by a dune rule
   (`graphics/font/dune`). Its free use conditions and the required
   acknowledgements are in `graphics/font/fonts/README.md`.
2. **Decoding** (`Hershey.decode_glyph`): the JHF format stores each
   coordinate as one character, its distance from 'R'; " R" lifts the
   pen. `Hershey.mli` walks through the "A".
3. **Layout** (`Hershey.layout`): the simplest possible shaping: one
   glyph per character, each placed where the previous one ends
   (`right - left`). No kerning, ASCII only.
4. **Placement** (`Shape_render_software.text_to_local`): center the
   text on the shape's position, like the web backend's
   `text-anchor="middle"` and `dominant-baseline="central"` (Hershey's
   y = 0 is already the middle of the em); scale the em
   (`Hershey.units_per_em` = 30, so that capitals, 21 units tall, are
   0.7 em like in usual fonts) to `Playground.words_font_size`.
5. **Rasterization** (`Shape_render_software.draw_words`): the pen is
   1/12 em wide. When that's less than 1.5 pixels (normal-size text),
   each stroke is a 1-pixel line (`Line`, Bresenham); when it's thicker
   (Pong's score, drawn `scale 10.`), each stroke becomes polygons --
   a rectangle per segment and a disk per point, all filled in one
   `Fill.polygons` call so the nonzero rule makes their union and no
   pixel is painted twice (`Stroke`). That's "stroking", what Cairo and
   PostScript do for thick lines. With antialiasing on (the default,
   the "n" key), thin strokes are Wu lines (`Line.draw_aa`) and thick
   ones the same polygons (`Stroke.contours`) filled with coverage
   (`Fill.polygons_aa`), see `notes_2d.md` section 10.

Try it: `dune exec examples/software/Words.exe` and
`dune exec games/arcade/software/Pong.exe`, with the debug keys: "f"
(wireframe) shows the pen's centerlines, "z" magnifies the pixels,
"b" shows each text's box.

### What we don't do (yet), and exercises

No hinting, no kerning, no shaping beyond one glyph per character,
ASCII only. Each missing piece is an exercise, in rough order of
difficulty:

- **Line breaks**: `Hershey.layout` draws a "\n" as the '?' glyph;
  make it start a new line, one em lower, and center the block.
- **UTF-8**: `layout` walks bytes, so "é" is two '?'s; decode code
  points, and draw "é" as section 1 says fonts do, two glyphs: the
  "e", and an accent (a stroke or two of your own) above it.
- **Kerning**: a table of pairs ("AV", "To", "LT") and their
  adjustment, applied in `layout`'s fold, where `x` advances.
- **More faces**: Hershey's other fonts ("Roman duplex", "Roman
  triplex", script, Greek) are the same JHF format; embed one more next
  to `futural.jhf` (`graphics/font/dune`) and choose by name.
- **A glyph cache**: every frame, `draw_words` decodes nothing (the
  font is decoded once, lazily) but re-strokes every glyph; rasterize
  each (glyph, size) once into a small coverage bitmap and blit it, the
  way FreeType's cache and every game's glyph atlas do. Measure it with
  `notes_opti.md`'s method on a text-heavy program
  (`examples/TypesetParagraph.ml`, a `words` per character).
- **Exact coverage**: thick text's edges go through `Fill.polygons_aa`'s
  4 sub-rows per pixel; font-rs's signed-area accumulation (section 8)
  computes the exact covered area instead, and compare them with the
  magnifier ("z").
- **No outlines**: Hershey's single-width strokes don't look like the
  other backends' sans-serif. A TrueType rasterizer is the natural next
  step (the plan's stretch phase 8): parse a `.ttf` file's `cmap` (which
  glyph for which character), `loca`/`glyf` (the outlines, quadratic
  curves) and `hmtx` (advance widths) tables, flatten the curves (de
  Casteljau), and fill each glyph with `Fill.polygons` and the nonzero
  rule -- contours turned opposite ways make the holes of "O", "A",
  "e", as in section 3. About what `stb_truetype.h` does, in a few
  hundred lines. Then, the hardest: **hinting**, at least the
  "autohinter" kind (section 4), snapping stems' edges to whole pixels
  at small sizes.

## 8. Related libraries

- **FreeType** (C): loading and rasterizing TrueType/OpenType/Type 1
  fonts, with hinting and antialiasing. The reference.
- **HarfBuzz** (C++): shaping.
- **fontconfig**: finding font files on a Linux system ("sans-serif"
  -> `/usr/share/fonts/.../DejaVuSans.ttf`).
- **Pango** (C): paragraphs of text on top of HarfBuzz and Cairo, used
  by GTK.
- **stb_truetype.h** (C, Sean Barrett, public domain): a whole
  TrueType parser and antialiased rasterizer in one header file;
  readable, and the model for a from-scratch version.
- **font-rs** (Rust, Raph Levien, 2016): a very fast glyph rasterizer
  based on accumulating signed area coverage, explained in his blog post
  "Inside the fastest font renderer in the world".
- Platform rasterizers: Core Text (macOS/iOS), DirectWrite (Windows),
  Skia (Chrome, Android; on top of FreeType or the platform).
- OCaml: **otfm** (Daniel Bünzli) decodes OpenType files (glyph
  outlines, metrics) in pure OCaml; **vg** (Bünzli) is a declarative 2D
  vector graphics library with text support; `cairo2`'s text functions
  are the ones our Cairo backend uses.

## 9. Compared with FreeType and stb_truetype

**What they do that we don't.** `stb_truetype.h` parses TrueType and
CFF outlines, flattens their curves and fills them with an
antialiasing rasterizer (even into signed distance fields); FreeType
adds the bytecode interpreter of section 4, a dozen font formats, and,
with HarfBuzz, the shaping of section 5 for every script of Unicode.
That is sections 4 and 5: outlines with thick and thin, and hinting
for sharp small text, where our strokes have one width and no grid
fitting.

**Where the work goes.** FreeType rasterizes a glyph once per size
into a bitmap, and a program caches and blits it (Cairo does); games
put the bitmaps, or distance fields (Green 2007), into one texture, a
*glyph atlas*, and draw each letter as a textured quad. We re-stroke
every glyph every frame, with no cache (an exercise of section 7): it
costs little because a Hershey glyph is a few segments, where an
outline glyph is dozens of curves. Even the OpenGL 3D backend's HUD
text is Hershey, drawn on the CPU by `Shape_render_software` into an
image that becomes a texture, redrawn only when the HUD changes.

## 10. In the playground

The API has one text function, `words color str` (`Playground.mli`),
centered on its position, sized by `scale` (not a font size), and
nothing to measure text, since each backend draws a different font:
Cairo and the web a system sans-serif (`words_font_family`, at
`words_font_size`, 10 units), the software backends Hershey (section 7),
10 units per em too, so that a string takes roughly the same room
everywhere. On the software backend, `words` is `draw_words`, and
`Hershey.layout` also measures the text where the backend needs a
width: its fps counter, left-aligned at the bottom, and the help
overlay of the "h" key (`Help_overlay.ml`). `examples/Words.ml` rotates
and scales words, `examples/Typing.ml` edits a line of them,
`games/arcade/Pong.ml`'s score is thick strokes (`scale 10.`), and
`examples/TypesetParagraph.ml` sets a justified paragraph with a
`words` per character. The office apps go one step further:
`apps/office/Stroke_text.ml` reads Hershey's glyphs itself
(`Hershey.glyph`) and draws their strokes as `rectangle`s, so a
program gets the same letters on every backend, with widths it can
measure (the caret of `TinyWord.ml` and `TinyBravo.ml`), and bold (a
thicker pen), italic (the points sheared), underline and strike out.

## References

- Jack E. Bresenham, "Algorithm for computer control of a digital
  plotter", IBM Systems Journal 4(1):25-30, 1965.
- A. V. Hershey, "Calligraphy for Computers", NWL Report No. 2101, U.S.
  Naval Weapons Laboratory, Dahlgren, Virginia, 1967.
- Donald E. Knuth, "The METAFONTbook", Addison-Wesley, 1986.
- Adobe Systems, "PostScript Language Reference Manual",
  Addison-Wesley, 1985; "Adobe Type 1 Font Format", 1990.
- Xiaolin Wu, "An efficient antialiasing technique", SIGGRAPH '91.
- Apple Computer, "TrueType Reference Manual" (online).
- Microsoft and Adobe, "OpenType specification" (online).
- Chris Green, "Improved Alpha-Tested Magnification for Vector Textures
  and Special Effects", SIGGRAPH 2007 Advanced Real-Time Rendering course.
- Raph Levien, "Inside the fastest font renderer in the world", 2016
  (blog post, about font-rs).
