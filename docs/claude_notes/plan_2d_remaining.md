# Plan: what's left for the 2D software rasterizer

The software 2D backend is done: see
[`done/plan_software_2d.md`](done/plan_software_2d.md) (the backend and
`graphics/2d/`, phases 0-7 and 9) and its tutorial,
[`notes_2d.md`](notes_2d.md). Since then: golden frame tests
(`tests/2d/`), the "h" help panel, and `-debug-keys`. What's left,
roughly from most to least worth doing (the 3D side has its own list,
[`plan_3d_remaining.md`](plan_3d_remaining.md)). Anything that changes
pixels ends with new golden frames, approved after looking at them
(`make approve-golden2d`).

## 1. `Blit`'s simple and optimized bilinear paths differ by 1

`Blit.sample_bilinear` (the simple path, "o" off) rounds after each of
its 3 mixes (`Blit.lerp`), `Blit.draw`'s optimized path rounds once, at
the end: so the "o" key can change a bilinear image's pixels by 1,
against the rule that an optimization keeps the pixels. Fix: `lerp` on
floats, rounding once. Then `graphics/3d/Texture.sample_bilinear`,
which rounds once too, could share Blit's sampler (see `Texture.mli`).
`graphics/tests/Unit_blit.ml` is the check (no golden has an image, see
4).

## 2. Performance: the two known hot spots

From `notes_opti.md`'s "Next" (we're 1.5 to 7 times slower than Cairo):

- **Antialiased thin text** (Tetris: 61 fps, 184 without antialiasing):
  Wu's lines plot 2 pixels per column, each with a blend.
- **The background**: with antialiasing, even a full-window rectangle
  goes through coverage cells for 1000 rows (Turtle: 102 fps, 125
  without). A shape whose edges are on pixel boundaries needs no
  antialiasing: a special case worth detecting.

Each as an `Opti`, the simple version kept, measured with
`scripts/bench_playground.sh` (now with `-debug-keys`).

## 3. Text: Hershey at small sizes, and a real outline font

- **Small text**: at the 3D HUD's size (~13 pixels), Hershey's colon,
  two tiny stroked diamonds, is a smudge close to the previous letter
  ("Mouse:"); the help panel draws its text 1.3 times bigger, where it
  reads fine. A minimum dot size, or a bit more spacing, in
  `graphics/font` or the stroke renderer.
- **A TrueType outline font** instead of Hershey (the plan's stretch
  phase 8): parse a TTF's quadratic Béziers, flatten them (de
  Casteljau), fill them with our own nonzero-winding + antialiasing
  filler -- stb_truetype in OCaml; it reuses `Fill` wholesale. See
  `notes_font.md`, section 7.

## 4. Golden frames for what isn't covered yet

- **Images**: Turtle and Mario download theirs, so they're left out
  (tests shouldn't need the network), and so are `Blit`'s paths, "i"
  and the image optimization. An example with a local image (like
  `examples3d/checker.png` for 3D) would let a golden cover them.
- **Random games**: Snake and Tetris call `Random.self_init`; a
  `-seed n` flag (in `Native_loop_2d`) would make them testable.
- **Transparency**: only examples/Mouse fades a shape, while the mouse
  button is down, which dump mode can't do; so no golden with "t". A
  scene with a faded shape would.
- **The Cairo backend**: not covered, its pixels depend on the
  installed Cairo; and it has no debug keys, so no "h" help either.

## 5. Features, the exercises of `notes_2d.md` section 15

Each a good exercise, in rough order of difficulty:

- an even-odd toggle key (the fill rule is already a parameter of
  `Fill.polygons`);
- gamma-correct blending: convert to linear light, blend, convert
  back, and compare with the magnifier;
- premultiplied alpha, for images and blending;
- `Group` alpha, with an offscreen framebuffer (a TODO in
  `Shape_render_software` and `Shape_render_native` alike);
- stroked outlines for any shape, with miter or bevel joins;
- gradients (a color computed from each pixel's position, in
  `fill_span`'s place);
- exact-area antialiasing instead of 4 sub-rows (Duff 1989; font-rs's
  signed area accumulation, see `notes_font.md`);
- flood fill (Smith 1979), the paint program's bucket tool.
