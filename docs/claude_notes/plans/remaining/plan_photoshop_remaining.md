# Plan: what's left for TinyPhotoshop

The plan is done: see [`done/plan_photoshop.md`](../done/plan_photoshop.md)
-- `libs/graphics/imaging/` (`graphics_imaging`: `Lut`, `Histogram`,
`Hsl`, `Convolve`, `Gaussian`, `Sobel`, `Median`, `Add_noise`,
`Scale`, `Mask`, `Composite`, `Brush`, `Gradient`, each a new image
from the old), and TinyPhotoshop (Photoshop 1.0, Thomas and John
Knoll, 1990) in `apps/graphics/`, over NASA's photographs
(`apps/graphics/photos/`): the tools, Image > Adjust as dialogs with a
live preview, Filter, Image Size, the picture drawn as 64 tiles; then
Photoshop 3.0's layers (1994), `Blend` (the modes) and `Layers` (the
stack flattened by Porter and Duff's over), the Layers palette, Place.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. The rest of the layers

- **Moving a layer**: the move tool, a layer dragged over the others
  (an offset kept per layer, which `Layers`' flattening honours).
- **Layer masks** (Photoshop 4.0, 1996): a `Mask` per layer, painted
  with the brushes in grey, hiding without erasing; the flattening
  multiplies the layer's alpha by it.
- **Adjustment layers** (Photoshop 4.0 too): a Levels or a
  Hue/Saturation kept as a layer, applied to what is under it at each
  flattening, so it can be changed or hidden later -- a `Lut` as a
  layer instead of an image.
- **The non-separable modes**: `Blend` has Color; Hue, Saturation and
  Luminosity are its three siblings (the W3C's SetLum and SetSat,
  over `Hsl`). Soft Light, separable, a gentler Overlay (the W3C's
  formula), beside it.

## 2. History (Photoshop 5.0, 1998)

- The undo list as a palette: each step named by what it did (Levels,
  Gaussian Blur, Paintbrush), a click on one going back to it. The
  steps are already there (`Undo` keeps them all, where 1.0 had one);
  what's missing is their names and the palette.

## 3. The program's exercises (TinyPhotoshop.ml's header)

- **Curves** as a dialog where the points are dragged (`Lut.curves`
  is there; the dialog has only sliders today).
- **The zoom tool** and the hand, the tiles drawn larger.
- **The blur and sharpen tools**: a brush that applies `Convolve`
  through its dabs.
- **Crop**.
- **The airbrush's flow** as a slider.
- **A Histogram palette**, always open.
- Text, which the header lists as not done.

## 4. Later versions' ideas

- **Channels** shown one at a time, red, green, blue, each a grey
  picture.
- **CMYK**, for print, and **Lab**: a picture in
  another colour space, the conversions and what is lost going there
  and back.
- **The pen tool's paths**: Bezier curves drawn, turned into
  a selection (the lasso's scanline fill over the flattened curve).
- **Content-aware fill** (2010): a hole filled from the picture's own
  patches -- an exercise at most.
