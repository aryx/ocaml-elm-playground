# TinyPhotoshop: a photo editor, after Photoshop 1.0

Photoshop 1.0 (Thomas and John Knoll, Adobe, February 1990, on the
Macintosh II) turned MacPaint's dots into a photograph's: 24 bits a
pixel instead of 1, and with them the operations of image processing
(Rafael Gonzalez and Richard Woods's book, "Digital Image Processing",
is their catalogue) made into menus and tools a photographer could use.

The author's decisions (2026-09-25): the program in `apps/graphics/`,
with TinyMacPaint and TinyMacDraw moved there from `apps/office/`
(the category's section of CATALOG.md: dots, objects, then photos);
NASA's photographs, public domain, to open (`apps/graphics/photos/`,
made by `make_photos.sh`: the Blue Marble, Aldrin on the Moon); and
Photoshop 1.0 first, layers (Photoshop 3.0, 1994) as a later step.

Done (2026-09-25): sections 1 and 2 below -- the library and its
tests (`Unit_imaging`), and the program with five golden frames (the
window, Hue/Saturation's preview, the wand and Invert, a stroke, Levels
on Aldrin). Section 3 is left.

## 1. `libs/graphics/imaging/`: the algorithms

A library (`graphics_imaging`, package `elm_playground`, pure OCaml,
no Playground) over `Rgba_image`, one idea per module, each `.mli`
with its worked example checked by a test (`libs/graphics/tests/`).
Every operation returns a new image: the playground's `bitmap` caches
by image, and an editor's undo keeps the old one.

- **Point operations**: a pixel's new value a function of its old
  value alone, so a table of 256 entries per channel (`Lut`): invert,
  brightness and contrast, levels (black, white, gamma), curves (a
  spline through points), posterize, threshold; `Histogram`, which
  Levels shows; `Hsb`, hue and saturation turned in HSB, the colour
  wheel.
- **Neighbourhood operations**: a pixel's new value a function of
  those around it. `Convolve` (a kernel, the edges clamped: sharpen,
  emboss), `Gaussian` (the blur, separable: two passes of a line
  instead of a square; unsharp mask), `Edges` (Sobel's gradient, Find
  Edges), `Median` (not a convolution: noise removed, edges kept),
  `Noise` (seeded).
- **Geometry**: `Resample` (nearest, bilinear, bicubic: Image Size),
  flips and quarter turns.
- **Selections**: a `Mask`, a byte a pixel, 255 inside: a rectangle,
  an ellipse (its edge anti-aliased), a lasso's polygon (scanline
  fill), the magic wand (a flood fill within a tolerance), feathering
  (the mask blurred), and the marching ants (its edges); every
  operation applied *through* a mask, the old pixels and the new mixed
  by it (`Composite`).
- **Painting**: `Brush` -- a dab (a disc, its hardness the falloff of
  its edge), dabs spaced along a stroke, the paintbrush, airbrush,
  eraser, rubber stamp (the clone: dabs of the picture itself, at an
  offset), smudge; the bucket and the gradient.

## 2. `apps/graphics/TinyPhotoshop.ml`: the program

In TinyMacPaint's Mac look: Photoshop 1.0's tool palette in two
columns, the foreground and background colours, the picture's window
with its zoom, marching ants; Image > Adjust (Levels with its
histogram, Curves, Brightness/Contrast, Hue/Saturation, Invert,
Posterize, Threshold, Desaturate) as dialogs with a live preview;
Filter (Blur, Gaussian Blur, Sharpen, Unsharp Mask, Find Edges,
Emboss, Median, Add Noise); Image Size; one level of undo, as 1.0
had; File: open and save, PNG and JPEG by our own codecs, and the
photographs.

The picture is drawn as tiles, each its own `bitmap`, and a change
gives new images only to the tiles it touched: on the web every new
image is encoded as a PNG, and a brush stroke must not re-encode the
whole photograph at each frame -- the dirty rectangles of every
window system.

## 3. Later

- Layers (Photoshop 3.0, 1994): a stack of images, each with its
  opacity and blend mode (multiply, screen, overlay: the formulas of
  compositing, Porter and Duff 1984 and after), a layers palette.
- History (Photoshop 5.0, 1998): the undo list as a palette.
- Channels shown one at a time, CMYK, Lab; the pen tool's paths;
  adjustment layers; content-aware fill (2010), an exercise at most.
