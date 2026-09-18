# Plan: porting tiny-minecraft to games3d/

## Context

`~/software-src/game/tiny-minecraft/main.py` (the classic
`fogleman/Minecraft` Pyglet demo, ~750 lines) is the original
motivating target for `playground3d/` (see `plan_playground3d.md`'s
Phase 5, never detailed there beyond a sketch). This plan replaces that
sketch with a concrete, phased port to `games3d/`, now that the core
library, all 5 debug rendering toggles, texture support, and a `box`
primitive exist and are verified working. Read `notes_3d_opti.md`
before starting Phase 1 specifically -- performance is the single
biggest open risk here (see below).

## What the original actually does (for reference while porting)

- `Model`: a `world` dict (block position -> texture/type) and a
  parallel `shown` dict (only the *exposed* blocks -- ones with at least
  one face-adjacent position not in `world`, via `exposed`/`FACES`),
  plus a `sectors` dict (`SECTOR_SIZE = 16`) partitioning positions
  spatially so only nearby sectors need considering for updates. `hit_
  test` walks a ray in small steps from the player outward to find the
  first solid block (and the empty cell just before it) for block
  placement/removal. `add_block`/`remove_block`/`check_neighbors` keep
  `shown` in sync when the world changes (a removed block may newly
  expose its neighbors; an added block may newly hide them).
- World generation (`_initialize`): a flat grass-over-stone base layer,
  solid stone outer walls, and a scattering of randomly-placed,
  randomly-colored/textured "hills" (small stacked blocks).
- Textures: one atlas image (`texture.png`, a 4x4 grid), `tex_coord`/
  `tex_coords` mapping a block type to 3 UV sub-rectangles (top,
  bottom, side) -- `GRASS`/`SAND`/`BRICK`/`STONE`.
- `Window` (not fully read in earlier exploration, but the standard
  shape for this class of demo): first-person WASD movement relative to
  mouse-look yaw, gravity/jumping/a fly-mode toggle, left/right mouse
  click to remove/place a block (via `hit_test`), number keys to choose
  the block type to place, and an on-screen crosshair + position/FPS
  readout.

## Design decisions for the port

### Blocks and the atlas: no new library primitive needed

`Playground3d.form3d`'s `TexturedPolygon3d of string * ((number*number*
number) * (number*number)) list` already accepts an explicit UV pair
per point -- exactly what atlas sub-rectangle mapping needs (this was
called out as the intended escape hatch when texturing was first added:
see `Playground3d.mli`'s "Textures" section). A `textured_box_faces`
helper local to the Minecraft port (not a new library primitive) can
build a block's 6 faces directly via `TexturedPolygon3d`, computing each
face's 4 UV corners from a `(row, col)` cell in the atlas -- the same
`tex_coord`/`tex_coords` idea as the original, just producing UV pairs
instead of a display-list vertex buffer. Grass/dirt/stone/sand/brick
become `(top, bottom, side)` triples of atlas cells, same as the
original.

### World/model data structure

A `Hashtbl.t` from `(int * int * int)` position to block type for
`world`, another for `shown`, and a sector `Hashtbl.t` from `(int * int
* int)` sector coordinates to a position list -- direct translations of
the Python dicts, no reason to deviate. `exposed`/`add_block`/
`remove_block`/`check_neighbors`/`hit_test` port close to line-for-line
(they're already simple, imperative, data-structure code, not
Python-specific).

### Rendering: rebuild vs. cache

The original explicitly separates "which blocks exist" (`world`) from
"which blocks currently have a visible mesh" (`shown`/`_shown`), and
only rebuilds a block's geometry when its exposure actually changes
(via `show_block`/`hide_block`), specifically to avoid redoing mesh work
for a large, mostly-static world every frame. Our `game3d` model
recomputes the whole `shape3d list` from scratch in `view3d` every
frame (see `Playground3d.mli`'s design note on this) -- fine for a
handful of shapes, unproven at "a few thousand exposed block faces".
**This needs a real measurement before going further** -- see
Verification below; if it's too slow, the fix is to build each visible
block's `shape3d` once when it becomes shown (cached in the `shown`
table itself, alongside its texture info) and only rebuild the small
set of blocks that actually changed each frame, rather than every block
every frame -- more bookkeeping, deliberately deferred unless the naive
version proves too slow.

### First-person camera and controls

`camera3d`'s eye/target model already supports a moving camera (see
`Playground3d.mli`'s `game3d` doc comment) -- the model stores
`(x, y, z, yaw, pitch)`, and `view3d` computes `target = eye + (a unit
vector from yaw/pitch)` each frame, exactly the `first_person_step`-style
helper sketched (but never built) earlier in this project's design
discussions.

Two real gaps, not yet solved anywhere in this codebase, to resolve
before this can feel like an actual FPS-style game:
1. **Mouse-look needs relative/delta mouse motion**, not the absolute,
   window-bounded position `Playground.mouse` currently exposes -- and
   ideally a captured/hidden cursor that can move infinitely (SDL's
   relative mouse mode, `Sdl.set_relative_mouse_mode`, not currently
   used anywhere in `playground3d/software/`). Without it, looking around
   is bounded by how far the mouse can physically move inside the
   window before hitting an edge. Likely needs a small, targeted
   addition to the native backend (e.g. exposing relative deltas
   somehow) -- worth its own short investigation before Phase 2 below,
   not assumed away.
2. ~~No 2D HUD/overlay channel~~ **RESOLVED**: `docs/claude_notes/done/plan_hud.md`
   added `Playground3d.hud`, so a crosshair/selected-block indicator/
   position readout can be drawn as an ordinary 2D `Playground.shape`
   dropped into `view3d`'s returned list (see `notes_3d.md` section
   12). No longer a blocker for Phase 6 below.

### Physics and collision

Gravity/jump/terminal-velocity constants port directly (they're just
numbers). Collision is simpler to get right than it might look: since
the world is a voxel grid, checking whether a candidate new position
overlaps a solid block is a small number of `Hashtbl.mem` lookups at the
player's bounding box's corners, not real geometric collision detection
-- close to what the original does.

## Phasing

1. **DONE.** World data model only, no rendering: `games3d/Minecraft_model.ml`
   (`sectorize`/`exposed`/`hit_test`/`add_block`/`remove_block`/
   `check_neighbors` and world generation, independent of `playground3d/`
   entirely -- dropped the pyglet-specific incremental show/hide queue,
   see that file's own header comment for why) plus
   `games3d/Test_minecraft_model.exe`, a standalone invariant checker
   (not a Testo suite -- this project has no existing Testo usage, and
   the plan itself only asked for "a throwaway print-based check").
   Verified: world/shown consistency, exposure correctness and
   completeness, a constructed 3x3x3-cube scenario proving
   `check_neighbors` updates exposure correctly on add/remove, and
   `hit_test` hitting/missing correctly. Default world (`n=80`, matching
   the original): 84421 blocks, 54450 shown (exposed).
2. **DONE -- and the number says "cache built meshes" is needed before
   Phase 3.** `games3d/Minecraft3d.ml` renders every `shown` block
   (textured via a local `block_shape`/atlas-UV helper, per this plan's
   own "no new library primitive" design decision) with a fixed
   overview camera, no controls -- naive rebuild-every-frame, as
   planned. Screenshotted correctly (grass/brick/sand/stone atlas
   mapping right on the first try -- see the atlas UV section above).
   **Measured** (native software rasterizer, this machine): building
   the 54450-shape list alone takes ~1-1.3s *every frame*, and a full
   frame (build + rasterize + present) takes ~2.5s, i.e. **~0.4 fps** --
   confirmed by running headlessly for 15s and counting how many
   `view3d` calls completed. This settles the "rebuild vs. cache"
   question the plan flagged as the single biggest risk: naive
   rebuild-every-frame does not scale to a world this size, so the
   "build each shown block's shape3d once, cache it in the `shown`
   table itself (or alongside it), and only rebuild the small set of
   blocks that actually changed" rework described in that section is
   now a real prerequisite for Phase 3, not a hypothetical -- **not yet
   built**, next up before first-person controls land.

   **"What if we just use the OpenGL backend instead?"** -- tested via
   `games3d/opengl/Minecraft3d.exe` (same source, `copy_files`, per
   `plan_opengl.md`'s pattern). Answer: no, it doesn't help, and the
   *why* matters more than the number -- `view3d` (the pure OCaml
   `Hashtbl.fold` building 54450 nested `shape3d` records) is
   **shared, backend-agnostic code**, called identically by both
   backends' `Native_loop.run`, and it alone was already ~40-50% of
   native's whole frame time. Measured on OpenGL: `view3d` itself
   actually got *slower* frame over frame within the same run (1.1s,
   1.9s, 2.2s, 2.5s, 2.6s -- 5 completed frames in 25s, i.e. ~0.2 fps,
   worse than native's ~0.4), for code that isn't touching the GPU at
   all -- almost certainly GC pressure from the sheer allocation
   volume (54450 short-lived nested records, discarded every frame),
   not anything OpenGL-specific. The lesson generalizes:
   **switching rendering backends can't fix a bottleneck that lives
   above the rendering step** -- the "cache built shape3d values
   instead of rebuilding them every frame" fix is needed regardless of
   which backend eventually renders them, and should probably happen
   *before* deciding whether native or OpenGL is the better fit for
   Minecraft3d specifically.

   **DONE since, differently**: not a per-block cache in `shown`, but
   the world built once as 121 `cached3d` chunks (one per sector) with
   hidden-face culling, which the GPU backends keep in GPU memory
   (`Mesh_cache`). OpenGL: ~6.5s per frame -> ~1ms. See
   `plan_opengl_perf.md`'s Results. Edits (Phase 5 below) will rebuild
   only the touched chunks.
3. **DONE.** **First-person camera + WASD + mouse-look**. The
   relative-mouse-motion gap was not closed with an API change: the
   mouse's offset from the window's center adds up to 90 degrees left
   or right (60 up or down) to the view direction, and the arrow keys
   turn further (Minecraft3d.ml's [look]). Works on every backend as
   is; the price: no unlimited turning with the mouse alone.
4. **DONE.** **Physics**: gravity, jumping, fly-mode toggle (Tab),
   collision, in a new module, `Minecraft_player` (a port of the
   original's `get_sight_vector`/`get_motion_vector`/`_update`/
   `collide`, same constants, independent of the Playground), checked
   by `Test_minecraft_model.exe` in a small hand-made world (standing,
   falling, a 1-block jump, stopping at a wall, flying). Movement uses
   the real time between frames (`computer.time`), so it's the same
   speed at any frame rate.
5. **DONE.** **Interaction**: left click removes the block under the
   crosshair (not stone), right click places one (a new
   `Playground.mouse.mrdown`, the right button, added to the API and
   every backend for this), 1/2/3 choose brick/grass/sand. Checked on
   OpenGL with `scripts/xdrive.py`: each edit rebuilds 1-2 chunks and
   frees as many meshes, the live count stays at 121. After an edit at `pos`, rebuild the `cached3d` of
   the chunks of `pos` and of its 6 neighbors (a neighbor can be in
   the adjacent sector, and its exposed faces changed); the GPU
   backends free the old meshes by themselves (`Mesh_cache`'s sweep).
   Check with `-debug`'s stats line: no visible hitch per edit, and
   the live mesh count stays at the number of chunks after many edits
   (see `done/plan_opengl_perf.md`, Phase 5).
6. **Polish**: HUD (crosshair/selected-block indicator, now
   unblocked -- see above). **Partly done**: a crosshair and a status
   line (block, position, flying), shown by the software and WebGL
   backends; **not on OpenGL**, which has no HUD yet (see
   `plan_3d_remaining.md`): the missing piece for OpenGL, the backend
   where the game is actually playable. Also: matching the original's world-generation
   "hills" more closely if the flat/simple version from Phase 1 feels
   too bare.

## Verification

- Phase 1: run the world-generation + block-management code standalone
  and sanity-check invariants (every position in `shown` is actually in
  `world` and actually exposed; removing a block updates its
  neighbors' exposure correctly), rather than relying on visual
  inspection.
- Phase 2: the FPS number *is* the verification -- record it plainly
  (screenshot the on-screen counter, same as every other perf
  measurement in `notes_3d_opti.md`), and decide the caching question
  from real data, not a guess.
- Phases 3-5: manual play-testing (movement feels right, collision
  doesn't let you clip through blocks, placing/removing blocks updates
  the world and its rendering correctly).
