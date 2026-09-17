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
   used anywhere in `playground3d/native/`). Without it, looking around
   is bounded by how far the mouse can physically move inside the
   window before hitting an edge. Likely needs a small, targeted
   addition to the native backend (e.g. exposing relative deltas
   somehow) -- worth its own short investigation before Phase 2 below,
   not assumed away.
2. **No 2D HUD/overlay channel** (already flagged repeatedly in this
   project): no way to draw a crosshair, selected-block indicator, or
   position/FPS readout on top of the 3D scene. `game3d`'s `view3d`
   only ever returns a 3D scene. A first version can simply skip the
   HUD (crosshair included) rather than block on this.

### Physics and collision

Gravity/jump/terminal-velocity constants port directly (they're just
numbers). Collision is simpler to get right than it might look: since
the world is a voxel grid, checking whether a candidate new position
overlaps a solid block is a small number of `Hashtbl.mem` lookups at the
player's bounding box's corners, not real geometric collision detection
-- close to what the original does.

## Phasing

1. **World data model only, no rendering.** Port `sectorize`/`exposed`/
   `hit_test`/`add_block`/`remove_block`/`check_neighbors` and world
   generation as plain OCaml data-structure code, independent of
   `playground3d/` entirely. Testable on its own (e.g. via `dune utop`
   or a throwaway print-based check) before any graphics are involved.
2. **Static rendering + performance checkpoint.** Render the generated
   world's exposed blocks (naive: rebuild every frame first, per the
   "rebuild vs. cache" discussion above) with a fixed camera, no
   controls yet. Measure FPS the same way `notes_3d_opti.md` did for
   `Cubes3d.ml`, at a world size comparable to the original's default.
   **Do not proceed to Phase 3 until this number is known** -- it
   determines whether the "cache built meshes" rework is needed now or
   can wait.
3. **First-person camera + WASD + mouse-look**, including resolving the
   relative-mouse-motion gap above.
4. **Physics**: gravity, jumping, fly-mode toggle, collision.
5. **Interaction**: `hit_test`-based block add/remove on click, block
   type selection.
6. **Polish**: HUD (blocked on the 2D-overlay-channel gap -- may need
   that feature built first, or ship without a HUD initially), matching
   the original's world-generation "hills" more closely if the flat/
   simple version from Phase 1 feels too bare.

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
