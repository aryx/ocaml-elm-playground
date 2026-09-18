# Plan: making the OpenGL backend fast on big static scenes (Minecraft3d)

## Context

`plan_tiny_minecraft.md`'s Phase 2 measured the naive port at ~0.4 fps
on the software backend and ~0.2 fps on OpenGL, for a 84421-block
world with 54450 shown blocks. The OpenGL number is the surprising
one: the GPU is idle almost the whole frame. This plan explains why,
compares with what the original Python/Pyglet program does (it runs
at a comfortable frame rate on the same world), and proposes a
small, measurable path to get `games3d/opengl/Minecraft3d.exe` to
60 fps without giving up the pure `view : computer -> model -> camera
* shape3d list` API.

Scope: the OpenGL backend (and, for free, the planned WebGL one, see
`plan_webgl.md`, since the reusable part lives in `Gpu_scene`). The
software backend keeps its semantics but gets no speedup from this:
it has to rasterize every triangle on the CPU each frame anyway.

## Diagnosis: where the time goes today

54450 blocks x 6 faces x 2 triangles = ~650k triangles, ~2M vertices.
A GPU draws that in a few ms *if the data is already on the GPU*. What
we do instead, every frame, all on the CPU:

1. `view` (`games3d/Minecraft3d.ml`, `world_to_shapes`): builds 54k
   `group3d` trees of `TexturedPolygon3d` faces, then `move3d` ->
   `map_points` copies every point tuple once more.
2. `Gpu_scene.collect_batches`: fan-triangulates every face into ~2M
   boxed `(vec3, vec3, color, uv)` tuples, in lists.
3. `Gpu_scene.group_by_material`: more `List.concat`/filtering over
   those lists.
4. `Gpu_scene.vertex_floats_of_group`: copies into a ~21M-element
   `float array`, calling `rgb_of_color` per vertex.
5. `Bigarray.Array1.of_array`: copies it again.
6. `Gl.buffer_data ... dynamic_draw`: uploads ~86MB to the GPU.

Tens of millions of small allocations per frame, and, worse for
OCaml's generational GC, none of them short-lived: everything is
accumulated into lists that are still live at minor-GC time, so it
all gets promoted to the major heap. This is the likely explanation
for `view` getting *slower frame after frame* on OpenGL (1.1s ->
2.6s, see `plan_tiny_minecraft.md`): an ever-bigger major heap to
mark and sweep.

The general lesson: an immediate-mode API ("describe the whole scene
every frame") costs O(scene) CPU work per frame *by construction*.
That is fine for StarCollector3d and fatal for 50k blocks. The GPU
backend only replaced the last step (pixels); steps 1-6 are the
bottleneck.

## Why the Python original is fast (and what it does *not* do)

`~/software-src/game/tiny-minecraft/main.py` is 902 lines. The part
that matters for performance is about ten of them:

```python
def _show_block(self, position, texture):          # once per block
    vertex_data = cube_vertices(x, y, z, 0.5)
    self._shown[position] = self.batch.add(24, GL_QUADS, self.group,
        ('v3f/static', vertex_data), ('t2f/static', texture_data))

def _hide_block(self, position):
    self._shown.pop(position).delete()

def on_draw(self):                                  # every frame
    ...
    self.model.batch.draw()
```

What it does:
- **Retained GPU geometry** (the whole trick). A `pyglet.graphics.Batch`
  is a set of VBOs that pyglet manages. `batch.add` copies a block's
  24 vertices into it once, when the block becomes shown;
  `vertex_list.delete()` frees that slot when it's hidden. `on_draw`
  is a single `batch.draw()`, which issues a handful of GL draw calls
  over buffers that are already on the GPU. Per frame, Python executes
  essentially nothing proportional to the world size. Being written
  in a slow language doesn't matter because the slow language is not
  in the per-frame loop.
- **Incremental updates.** Adding/removing a block touches only that
  block and its 6 neighbors (`check_neighbors`), i.e. a few
  `batch.add`/`delete` calls, never a rebuild.
- **A coarse draw distance by sector.** `sectorize` maps positions to
  full-height 16x16 columns; `change_sectors` shows only the sectors
  within a disk of radius ~4-5 sectors around the player (77 of the
  world's 121 sector columns when standing at the origin) and hides the
  others as you walk. Fog (`setup_fog`, 20..60 units) hides the edge.
- **Time-sliced mesh building.** Show/hide calls go into a queue that
  `process_queue` drains for at most 1/60s per tick, so walking into
  new sectors doesn't cause a frame hitch. The initial load is done
  in one blocking `process_entire_queue()`, so the startup cost is
  paid once, before the first frame.
- Hardware backface culling (`glEnable(GL_CULL_FACE)`), no lighting,
  no normals, one texture atlas (one material).

What it does *not* do, contrary to what one might assume and to what
real Minecraft does:
- **No hidden-face culling.** Every exposed block gets all 6 faces
  (24 vertices), including faces pressed against a neighbor. The GPU
  draws ~2/3 x 54k x 12 = ~430k triangles every frame and doesn't
  care.
- No frustum culling, no occlusion culling, no greedy meshing, no
  per-chunk meshes (it's per-block slots in one shared batch).

So the Python version doesn't win by being clever about *what* to
draw. It wins by never recomputing *how* to draw anything that hasn't
changed. That is exactly the one thing our pipeline does not do.

## How real engines handle this (for reference)

- **Chunked, retained meshes.** Minecraft splits the world into
  16x16x16 chunks, builds each chunk's mesh once into a GPU buffer,
  and rebuilds a chunk only when a block inside it changes. A frame is
  roughly "one draw call per visible chunk".
- **Hidden-face culling** while meshing (only faces touching air);
  **greedy meshing** merges coplanar neighbor faces into big quads.
- **Frustum culling** of whole chunks against the camera, and
  **occlusion culling** (Minecraft's "cave culling").
- **Texture atlases / array textures**, so one material covers all
  blocks (we already have this).
- **Immediate mode only for small dynamic things.** Dear-ImGui-style
  per-frame rebuilding is used for UIs, particles, a few moving
  characters; static geometry is retained. Unity/Unreal keep a
  persistent scene graph with dirty flags and static batching.

## Design: `cached3d`, an Elm-`Html.lazy`-style escape hatch

The pure-view API can stay, as long as the backend has a way to know
"this subtree is the same as last frame, don't reprocess it". Elm's
virtual DOM has the exact same problem and solves it with `Html.lazy`
(memoization by reference identity). The playground3d equivalent:

```ocaml
(* Playground3d.mli *)
val cached3d : shape3d list -> shape3d
```

- `cached3d shapes` wraps its argument in a new form3d case,
  `Cached3d of cached`, where `cached = { id : int; content : shape3d;
  huds : Playground.shape list }`. `id` comes from a global counter
  incremented on each call: a **fresh identity per call**, like a
  physical address. (This is the one impure bit, and a deliberate one:
  it is exactly the reference identity `Html.lazy` relies on.)
- **Contract for the caller**: build it once (in `init`/`update`, or
  a global) and return the *same value* from `view` frame after frame.
  Calling `cached3d` inside `view` every frame is correct but gives no
  speedup (a new id every frame).
- **Semantics are unchanged**: every backend may treat a `Cached3d`
  exactly like a `Group3d` of its content. The software and web
  backends do exactly that (one extra match case each).
- `move3d`/`rotate3d`/`scale3d`/`fade3d` on a `Cached3d` recurse into
  its content and return a plain, **uncached** result. Correct but
  slow: transform first, then cache. (A later extension could store a
  per-node model matrix so moving a cached object is a uniform update,
  not a re-upload -- that's what would make e.g. StarCollector3d's
  player cacheable. Out of scope here.)
- `huds` is `collect_hud_shapes content`, computed once at
  construction, so that `collect_hud_shapes` doesn't have to walk a
  50k-face cached subtree every frame just to find (usually no) HUD
  shapes.

Rejected alternative: automatic caching with no API change, by
recognizing top-level shapes that are physically equal (`==`) to last
frame's. OCaml can't hash on physical identity (the GC moves values),
and hashing on content is O(size), which is the cost we're trying to
avoid. Positional `==` matching against last frame's list is fragile.
An explicit id is simpler and predictable.

### OpenGL backend side

A table `id -> gpu_mesh`, where `gpu_mesh` is one VAO+VBO per material
group of the content (built with the existing
`Gpu_scene.group_by_material`/`vertex_floats_of_group`, uploaded once
with `Gl.static_draw`) plus its vertex counts. Per frame:

1. Walk the top-level shape list. A `Cached3d` node: look up its id,
   build and upload its mesh on a miss, and mark it live this frame. Do
   *not* recurse into it. Everything else goes through today's dynamic
   path, unchanged.
2. Draw the dynamic groups as today, then each live cached mesh (bind
   its VAO, bind its texture, `draw_arrays`).
3. **Sweep**: delete (`delete_buffers`/`delete_vertex_arrays`) every
   mesh not marked live this frame. This makes GPU memory management
   automatic: when a game replaces a chunk's `cached3d` value after an
   edit, the old id simply stops appearing and its buffers are freed
   at the end of that frame.

The "which ids are live / build on miss / sweep" bookkeeping is
GPU-API-independent. Put it in `Gpu_scene`, parameterized by
`upload : (material * vertex_data list) list -> 'mesh` and
`free : 'mesh -> unit`, so `webgl/` gets it for free.

`Cached3d` nodes nested inside non-cached `Group3d`s: handle them the
same way (the walk looks for them at any depth outside a cached node).
A `Cached3d` nested *inside* another `Cached3d` is simply part of the
outer mesh.

## Minecraft3d side

- **Chunks = the model's existing sectors** (full-height 16x16
  columns, `Minecraft_model.sectors`, ~121 of them for the default
  world). One `cached3d` per sector, built once at startup and kept in
  a `(sector, shape3d) Hashtbl.t` next to the model. `view` returns
  ~121 already-built values, i.e. O(chunks) work instead of O(blocks),
  and the backend does ~121 draw calls, one material.
- **Hidden-face culling** in `block_shape`: emit a face only if the
  neighbor in that direction is not in `world`. The original doesn't
  do this, but it's nearly free (6 `Hashtbl.mem`s per block, once at
  build time), it cuts GPU upload size and startup time, and it helps
  the software backend too. Estimated reduction is several-fold (in a
  flat-ish world most exposed blocks show 1-2 faces), to be measured
  rather than assumed.
- Edits (needed by `plan_tiny_minecraft.md`'s Phase 5): after
  `add_block`/`remove_block` at `pos`, rebuild the chunks of `pos` and
  of its 6 neighbors (a neighbor can sit in an adjacent sector, and
  its exposed faces changed). Rebuilding means a new `cached3d` value
  (a new id); the backend's sweep frees the old one.

## Render options: a formal, shared list of toggles

The Python original's `setup()` is a short list of rendering choices:
`glEnable(GL_CULL_FACE)`, `GL_NEAREST` texture filtering, linear fog,
plus the sector draw distance in `change_sectors`. We already have
most of these, but scattered and implicit:

- software backend: global refs + hotkeys in `run_app3d`'s
  `on_key_press` -- "m" shading mode, "b" backface culling, "f"
  wireframe, "z" painter's/z-buffer, "p" perspective-correct/linear
  interpolation;
- OpenGL backend: only "f" wireframe; backface culling and the depth
  test are hard-enabled at startup, `Gl.nearest` is hard-coded in
  `upload_texture`;
- no way for a *game* to pick its defaults (Minecraft3d wants fog and
  a draw distance; a debug scene may want culling off), and no way to
  set them from the command line, which matters for reproducible
  benchmarking since nobody presses keys in a headless run.

Proposal: one public, backend-independent record in `Playground3d`,
listing every option, split into two kinds:

```ocaml
type render_options = {
  (* visual/debug: change what you see *)
  wireframe : bool;
  shading : Flat_color | Flat_shading | Gouraud | Phong;
  visibility : Zbuffer | Painters;
  interpolation : Perspective_correct | Linear_interp;
  texture_filter : Nearest | Bilinear;
  fog : (number * number * Playground.color) option; (* start, end, color *)
  (* performance: should NOT change what you see (modulo the draw distance) *)
  backface_culling : bool;
  use_cache : bool;              (* false = treat every cached3d as a group3d *)
  draw_distance : number option; (* skip cached nodes farther than this *)
}
val default_render_options : render_options
```

(the exact variant names are open; the point is one shared list.)

- **A game sets its defaults** with an optional argument,
  `game3d ?options view update init`, so existing callers don't change.
- **Each backend documents which options it honors** and ignores the
  rest, via a support table in `Playground3d.mli` (e.g. `interpolation`
  and `Painters` are software-only concepts, `Bilinear` may be
  OpenGL-only at first, `fog` is a few shader lines on the GPU and
  per-pixel work on the software backend). This is the same kind of
  "accepted asymmetry" the web backend already has with textures.
- **One shared key table** in `Native_loop` (key -> option -> cycle
  function) instead of each backend's own `on_key_press`, so the same
  key does the same thing on both native backends. Each toggle is
  logged (`Logs.info "backface_culling: off"`), and the current
  settings could go in the window title next to the fps.
- **Command-line overrides** in `parse_cli_and_setup_logging`, e.g.
  `-set use_cache=false -set backface_culling=false`, so the Phase 0
  benchmarks can A/B an option without editing code or pressing keys.
- **Per-frame stats with `-debug`**, next to the existing view/draw
  split: draw calls, triangles submitted, live cached meshes, bytes
  uploaded this frame. This is what makes the "performance" options
  measurable instead of a matter of feel.

`use_cache` in particular makes the `cached3d` work testable at
runtime: toggle it and the fps should drop back to the Phase 0
baseline, while the image must stay pixel-identical.

## Phasing

0. **Baseline numbers.** Run `games3d/opengl/Minecraft3d.exe -debug`
   and `StarCollector3d.exe -debug` with `Native_loop`'s `target_fps`
   temporarily uncapped (see `notes_debugging_techniques.md` section
   7), and record the view/draw split per frame. This is the reference
   for every later phase.
1. **`cached3d` in the library + the GPU cache in `Gpu_scene` + the
   OpenGL backend using it.** Add the `Cached3d` case to every existing
   match: `Playground3d.ml`'s `map_points`/`map_points_and_normals`/
   `fade3d`/`collect_hud_shapes`/`flatten_faces`,
   `software/`'s `flatten_faces`, and `Gpu_scene.collect_batches`.
   Verify with a small `examples3d/` scene: e.g. a grid of 10k cubes
   wrapped in one `cached3d`, fps before/after, and a screenshot
   identical to the uncached version. Also check that the software and
   web backends render it identically.
2. **Minecraft3d: per-sector `cached3d` + hidden-face culling.** Measure
   fps (uncapped, then at the normal 60 cap), face count before/after
   culling, and startup time. **Target: 60 fps capped on OpenGL**,
   with `view` well under 1ms.
2b. **`render_options`** (see above), independent of Phases 1-2 and
   doable before them. Doing it first would give Phases 1-2 their
   `use_cache` A/B switch and per-frame stats for free, so it's worth
   considering as Phase 1 instead. Start by moving the existing
   software toggles and OpenGL's hard-coded culling/`Gl.nearest` into
   the record, with no behavior change, then add `fog`/`draw_distance`.
3. **Chunk invalidation on edit**, only once Phase 5 of
   `plan_tiny_minecraft.md` (block add/remove) exists to exercise it.
   Check that the rebuild of ~7 chunks per edit causes no visible hitch,
   and that GPU buffers don't leak (e.g. the live mesh count stays ~121
   after many edits; log it with `-debug`).
4. **Only if numbers say so** (each is independent and optional):
   - a sector draw distance + fog, like the original (fewer draw
     calls, bounded GPU memory for a bigger world);
   - frustum culling of cached nodes via a bounding box computed at
     `cached3d` construction;
   - a cheaper dynamic (uncached) path: write floats directly into a
     growable float32 `Bigarray` instead of building lists of 4-tuples,
     copying to a `float array`, then to a `Bigarray`. That would help
     every scene that isn't cached, e.g. StarCollector3d;
   - a per-node model matrix for cached shapes, so moving objects can
     be cached too.

## Verification

- Each phase is verified by its fps/frame-time numbers (Phase 0
  method), recorded in this file as each phase completes, same
  convention as `notes_3d_opti.md` and `plan_tiny_minecraft.md`.
- Pixel-level: screenshot (`scripts/screenshot_playground3d.sh`) before
  and after wrapping a scene in `cached3d`. Output should be identical
  (caching must not change what's drawn), and hidden-face culling must
  not create holes (compare against the Phase 2 screenshot in
  `plan_tiny_minecraft.md`).
- `scripts/smoke_test_playground3d.sh` over all examples3d/games3d, on
  every backend, after Phase 1 (new form3d case = every match
  touched).
