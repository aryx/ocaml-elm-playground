# Plan: what's left for Minecraft3d

The port itself is done: see
[`done/plan_tiny_minecraft.md`](done/plan_tiny_minecraft.md) (the world
model, the player's physics, walking/jumping/flying, removing and
placing blocks, a crosshair and a status line, on every 3D backend) and
[`done/plan_opengl_perf.md`](done/plan_opengl_perf.md) (why it's fast on
the GPU: the world built once as `cached3d` chunks, hidden-face
culling). What's left, roughly from most to least worth doing. The
original is `~/software-src/game/tiny-minecraft/main.py`.

## 1. The outline of the targeted block

**DONE**: Minecraft3d.ml's [outline], 12 thin boxes.

The original's `draw_focused_block`: black edges around the block under
the crosshair, so you see which one a click will remove (a cube a bit
bigger than the block, 0.51 instead of 0.5, drawn in wireframe mode).
Without it, aiming at a far or small target is guesswork. Here: in
`view`, when `target` (`Minecraft_model.hit_test`) finds a block, add
12 thin boxes along its edges (a `box` has faces from every side, see
`Playground3d.box`'s doc comment), uncached, since it changes whenever
the view moves. A per-shape wireframe would be simpler but doesn't
exist in the API (wireframe is a whole-scene debug key).

## 2. The HUD's hitch on OpenGL

Every change of the HUD costs ~20ms on OpenGL (`graphics/core/Matting`'s
pass over all 1000x1000 pixels, see its comments), and the status
line's position changes at each block crossed: a skipped frame, several
times a second when walking. Two fixes, either enough:

- **matte only where the shapes are**: the rows and columns the HUD
  touches (the crosshair and one line of text: a few thousand pixels,
  not a million); the renders already only touch those, the matting
  pass and the texture upload (`tex_sub_image2d`) can too;
- or show the position less precisely, or not at all (the original
  shows it with fps and the counts of shown and total blocks, but its
  text is drawn by the GPU, for free).

## 3. Mouse look without limits

**DONE**: `Playground.mouse`'s `mdx`/`mdy` (relative motion, every
backend), and `run_app3d ?capture_mouse` (SDL's relative mouse mode on
the software and OpenGL backends, Escape to release, a click to capture
again; the Pointer Lock API on WebGL; ignored by the SVG web backend).
Minecraft3d turns by 0.15 degree per pixel, the original's.

Looking around with the mouse stops at the window's edges (90 degrees
left or right, 60 up or down; the arrow keys turn further), because
`Playground.mouse` is an absolute position in the window, and the
original captures the mouse instead (an invisible cursor that can move
forever, reporting only how much it moved). The fix is in the
Playground API, for every backend: relative motion (e.g. `mdx`, `mdy`)
and a way to capture the cursor (SDL's `Sdl.set_relative_mouse_mode`;
the browser's Pointer Lock API, `requestPointerLock`, which needs a
click on the page first). Then Minecraft3d's `look` becomes the
original's `on_mouse_motion`.

## 4. Clicks shorter than a frame

A click is noticed by comparing `mdown` with the previous frame's
(`was_down`): a press and release between two frames is lost. At 60 fps
a real click is long enough, but on the software backend (2-3 fps)
most clicks are lost. `Playground.mouse.mclick` was meant for this, but
no backend sets it: set it on each press, and clear it once `update`
has seen it (the `game_update` TODO in `Playground.ml` says as much).

## 5. The software backend's frame rate

2-3 fps (from 0.4, thanks to hidden-face culling alone, see
`done/plan_tiny_minecraft.md`). Its time goes into per-face work on the
whole world every frame (converting, culling, projecting ~70k faces),
most of them behind the camera or off screen. What would help, each
worth a measurement first:

- **frustum culling of whole chunks** before looking at their faces
  (also useful to the GPU backends, see `plan_3d_remaining.md`, section
  5): at the start, the camera sees maybe a quarter of the chunks;
- **a draw distance** (the original's: sectors within ~4-5 of the
  player, the far plane at 60 blocks, fog hiding the cut);
- **caching the converted faces of a `cached3d`** in the software
  backend too (`Shape3d_render_software.faces`), with `Mesh_cache`.

## 6. WebGL, checked in a real browser

`games3d/webgl/Minecraft3d` draws the same picture in headless Chrome,
but was never played: the keys and the right click (the web backend's
`button` = 2, and the context menu it suppresses) are untested there,
and its fps unmeasured (headless Chrome renders WebGL in software). Also
to check there: the faint lines along block edges in the headless
screenshot (texture-atlas bleeding? see `plan_3d_remaining.md`,
section 5).

## Smaller things

- **The original's label**: fps, position, and "shown / total" block
  counts (`draw_label`); here only the position, and the selected block.
- **Time-sliced mesh building**: the original builds the meshes of newly
  shown sectors a little each frame (`process_queue`, at most 1/60 s
  per tick), so that walking into new sectors never freezes the game.
  Not needed while the whole world is built at startup (0.4s) and edits
  rebuild 1-2 chunks; needed with a draw distance (section 5).
- **A golden frame**: Minecraft3d isn't in `tests/3d/` (a 1.5 MB
  frame, and slow on the software backend); `Test_minecraft_model.exe`
  checks the model and the physics instead. The world is deterministic
  (the generator isn't seeded), so a frame could be added.
- **The hills**: the plan wondered about matching the original's
  generator more closely; it's already a line-for-line port (same
  sizes and counts), only the random numbers differ (OCaml's Random,
  not Python's).
