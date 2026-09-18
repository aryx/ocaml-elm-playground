# Plan: making the GPU backends fast on big static scenes (Minecraft3d)

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

Scope: **both GPU backends**, `playground3d/opengl/` and the WebGL one
in progress (`playground3d/webgl/`, see `plan_webgl.md`, at its "hello
triangle" phase when this was written). Everything that isn't a GL call
goes in shared, GPU-API-independent code, so each backend only
supplies "upload a float array", "draw it", "free it". The software
and SVG backends keep the same semantics (same pixels) and get no big
speedup: the software rasterizer has to fill every triangle on the CPU
each frame anyway.

For the GPU concepts used below (VBOs, uniforms, the MVP matrix,
rebuild vs. render, what's recomputed when the camera moves), see
`notes_opengl.md`, sections 5-7.

## What changed since the first version of this plan

The 3D code was reorganized in the meantime (see
`done/plan_code_reorg_teaching_3d.md` and `plan_3d_remaining.md`):

| Then | Now |
|---|---|
| `Gpu_scene` had the matrices, lighting and the flattening | the matrices, lighting, vectors and camera are in `graphics/3d/geometry/` (`Mat4`, `Lighting`, `Vec3`, `Camera`), shared by all 3D backends; `Gpu_scene` is only the shape3d -> vertex flattening |
| the software backend was one big file | `graphics/3d/` is a from-scratch rasterizer, one idea per module (`Project`, `Cull`, `Clip`, `Triangle`, `Zbuffer`, `Interpolate`, `Shading`, `Texture`, `Painter`, `Render`), independent of the Playground; `playground3d/software/Shape3d_render_software` turns shapes into `Render.face`s |
| options were hotkeys scattered in each backend | a portable `Playground3d.rendering` (`shading`, `backface_culling`, `smooth_textures`) passed to `run_app3d ?rendering`; the software backend's full list is `Render.options`; debug keys only with `-debug-keys`, "h" for help (software only so far) |
| benchmarking meant editing `target_fps` | `-uncapped`, `-fixed-time t`, `-keys k`, `-dump-frame n file` (in `Native_loop`); `-debug` logs view vs. draw time per frame |
| no pixel tests | golden frames of the software backend in `make test` (`tests/3d/`) |

What did *not* change: the OpenGL backend's per-frame path is still
the naive one, and Minecraft3d still rebuilds the whole world every
frame. The diagnosis below still holds.

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
5. The backend copies that again: `Bigarray.Array1.of_array` (OpenGL),
   `Typed_array.float32Array_fromArray` (WebGL).
6. `Gl.buffer_data ... dynamic_draw` / `gl##bufferData`: uploads ~86MB.

Tens of millions of small allocations per frame, and, worse for
OCaml's generational GC, none of them short-lived: everything is
accumulated into lists that are still live at minor-GC time, so it
all gets promoted to the major heap. This is the likely explanation
for `view` getting *slower frame after frame* on OpenGL (1.1s ->
2.6s, see `plan_tiny_minecraft.md`). On WebGL it's worse:
js_of_ocaml-compiled OCaml is several times slower at this
allocation-heavy list code.

The general lesson: an immediate-mode API ("describe the whole scene
every frame") costs O(scene) CPU work per frame *by construction*.
Fine for StarCollector3d, fatal for 50k blocks. A GPU backend only
replaces the rendering (steps after 6); steps 1-6 are the bottleneck.

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
  is a set of VBOs managed by pyglet. `batch.add` copies a block's 24
  vertices into it once, when the block becomes shown;
  `vertex_list.delete()` frees that slot when it's hidden. `on_draw`
  is a single `batch.draw()` over buffers already on the GPU. Per
  frame, Python executes nothing proportional to the world size, so
  being a slow language doesn't matter.
- **Incremental updates**: adding/removing a block touches that block
  and its 6 neighbors (`check_neighbors`), never a rebuild.
- **A coarse draw distance by sector**: `sectorize` maps positions to
  full-height 16x16 columns; `change_sectors` shows only the sectors
  within a disk of radius ~4-5 around the player (77 of the world's 121
  when standing at the origin). Also `gluPerspective(65.0, ..., 0.1,
  60.0)`: the far plane at 60 clips the rest, and linear fog ending at
  60 hides the edge.
- **Time-sliced mesh building**: show/hide calls go into a queue that
  `process_queue` drains for at most 1/60s per tick.
- `GL_CULL_FACE`, `GL_NEAREST`, no lighting, one texture atlas.

What it does *not* do: **no hidden-face culling** (every exposed block
gets all 6 faces, including those pressed against a neighbor), no
frustum or occlusion culling, no greedy meshing, no per-chunk meshes.

So it doesn't win by being clever about *what* to draw. It wins by
never recomputing anything that hasn't changed, the one thing our
pipeline doesn't do. (How real engines go further: chunked meshes,
hidden-face culling, frustum/occlusion culling, LOD; see
`notes_opengl.md` section 7.)

## Design

### 1. `cached3d`, an Elm-`Html.lazy`-style escape hatch (`Playground3d`)

The pure-view API can stay, as long as a backend can tell "this
subtree is the same as last frame". Elm's virtual DOM has the same
problem and solves it with `Html.lazy` (memoization by reference
identity). The playground3d equivalent:

```ocaml
(* Playground3d.mli *)
val cached3d : shape3d list -> shape3d
```

- A new `form3d` case, `Cached3d of cached`, with
  `cached = { id : int; content : shape3d; huds : Playground.shape list }`.
  `id` comes from a global counter incremented on each call: a **fresh
  identity per call**, like a physical address. The one impure bit,
  deliberately: exactly the reference identity `Html.lazy` relies on.
- **Contract**: build it once (in `init`/`update`, or a global), and
  return the *same value* from `view` frame after frame. Calling
  `cached3d` inside `view` every frame is correct but gives no speedup.
- **Semantics unchanged**: any backend may treat a `Cached3d` as a
  `Group3d` of its content. Same pixels either way (tested, see
  Verification).
- `move3d`/`rotate3d`/`scale3d`/`fade3d` on a `Cached3d` recurse into
  its content and return a plain, **uncached** result: correct but
  slow. Transform first, then cache. (A per-node model matrix would
  make moving cached objects cheap; later, see Phase 6.)
- `huds` = `collect_hud_shapes content`, computed once at
  construction, so finding the (usually zero) HUD shapes doesn't walk
  a 50k-face subtree every frame.

Rejected alternative: automatic caching with no API change, by
recognizing top-level shapes physically equal (`==`) to last frame's.
OCaml can't hash on physical identity (the GC moves values), hashing
on content is O(size), and positional `==` matching is fragile. An
explicit id is simpler and predictable.

### 2. Where the code goes, layer by layer

```
 Playground3d        cached3d, Cached3d, every existing match extended
      |
 Gpu_scene           split a scene: dynamic shapes vs. Cached3d nodes;
      |              flatten either into per-material float arrays
 Mesh_cache          generic: id -> 'mesh, build on miss, mark live,
      |              sweep the rest (no GL, no Playground)
      +-------------------+
 opengl/            webgl/       each: upload (static), draw, free
```

- **`Mesh_cache`** (new, `playground3d/Mesh_cache.ml` + `.mli`, in
  `elm_playground_3d` next to `Gpu_scene`, so both GPU backends and
  js_of_ocaml can use it): a table `int -> 'mesh` with
  `find_or_build : t -> int -> (unit -> 'mesh) -> 'mesh` (marks the id
  live this frame), `sweep : t -> free:('mesh -> unit) -> unit` (frees
  every mesh not marked since the last sweep, then clears the marks),
  and counters for the stats line (live meshes, built this frame,
  freed this frame). One idea, one module, with the immediate- vs.
  retained-mode explanation in its `.mli`, in the style of
  `graphics/3d/`. Pure OCaml; it knows nothing of GL or shapes.
- **`Gpu_scene`**: a function that walks the shape list and returns
  the dynamic (uncached) shapes plus the `Cached3d` nodes found at any
  depth outside a cached node (a `Cached3d` inside another is simply
  part of the outer mesh). Both halves are flattened by the existing
  per-material code.
- **Each GPU backend** supplies its `'mesh` (the per-material buffers
  and vertex counts) and three functions:
  - `opengl/`: `upload` = one VAO + VBO per material, `Gl.static_draw`;
    `draw` = bind VAO, bind texture, `draw_arrays`; `free` =
    `delete_buffers` + `delete_vertex_arrays`.
  - `webgl/`: `upload` = `createBuffer` + `bufferData ... STATIC_DRAW`;
    `draw` = bind buffer, set the attribute pointers, `drawArrays`
    (WebGL 1 has no VAOs without the `OES_vertex_array_object`
    extension, so the pointers are set per draw; cheap);
    `free` = `deleteBuffer`.
- Per frame, in both: draw the dynamic part as today, then each live
  cached mesh (built on a miss), then `Mesh_cache.sweep`. The sweep
  makes GPU memory management automatic: when a game replaces a
  chunk's `cached3d` value after an edit, the old id stops appearing
  and its buffers are freed at the end of that frame.
- **Software backend**: `Shape3d_render_software.faces` gets a
  `Cached3d` case that recurses (same pixels). Optionally, the same
  `Mesh_cache` could cache the `Render.face list` per id, skipping the
  shape -> face conversion and texture lookups; `Render` still
  projects and fills every face each frame, so the gain is limited.
  Not needed for this plan's goal.
- **SVG web backend**: `Playground3d.flatten_faces` recurses. Nothing
  else.

The existing matches on `form3d` to extend (grep `Group3d`):
`Playground3d.ml` (`map_points`, `map_points_and_normals`, `fade3d`,
`collect_hud_shapes`, `flatten_faces`), `Gpu_scene.collect_batches`,
`Shape3d_render_software.faces`.

### 3. A cheaper dynamic path (both GPU backends)

Everything not cached still goes through steps 2-5 above every frame:
lists of boxed 4-tuples, then a `float array`, then a
`Bigarray`/`Float32Array` copy. With `cached3d` that's only the small
dynamic part of a scene, but it's what every existing example and
StarCollector3d pay, and it hurts most on WebGL. Fix: `Gpu_scene`
writes floats straight into one growable flat float buffer per
material (a `Float.Array` doubled when full), no intermediate
lists or tuples, and the backend converts that once. This also makes
building a cached mesh cheaper (Minecraft3d's startup).

### 4. Debug keys and stats: what's left of the "render options" idea

The first version of this plan proposed a formal record of rendering
options, CLI overrides and per-frame stats. Most of it now exists in
another shape: portable hints in `Playground3d.rendering`, the software
backend's full list in `Render.options`, debug keys behind
`-debug-keys`, `-keys k` to press them from the command line
(reproducible A/B runs), "h" for help. What this plan adds:

- **A "c" debug key** on both GPU backends: caching on/off (off = treat
  every `Cached3d` as a group, today's behavior). It's a debug key, not
  a `rendering` hint: it must never change a pixel, only the speed.
  `-keys c` gives the uncached baseline from the command line.
- **A stats line with `-debug`**, next to `Native_loop`'s view/draw
  times: draw calls, vertices uploaded this frame, live/built/freed
  cached meshes (from `Mesh_cache`'s counters). This is what makes the
  gains measurable, and would catch a leak (live meshes growing after
  edits).
- The OpenGL backend's missing "h" help and window-title key state
  (`plan_3d_remaining.md` notes it has neither) would help here too.

Fog and a draw distance (the Python original's other two tricks)
would be `rendering` hints, since fog changes pixels: on the GPU a few
fragment-shader lines, on the software side a new one-idea
`graphics/3d/Fog` module. Only if Minecraft3d needs them (Phase 6).

## Minecraft3d side

- **Chunks = the model's existing sectors** (full-height 16x16
  columns, `Minecraft_model.sectors`, ~121 for the default world). One
  `cached3d` per sector, built once at startup and kept in a
  `(sector, shape3d) Hashtbl.t` next to the model. `view` returns ~121
  already-built values: O(chunks) instead of O(blocks), and ~121 draw
  calls with one material.
- **Hidden-face culling** in `block_shape`: emit a face only if the
  neighbor in that direction is not in `world`. The original doesn't,
  but it's nearly free (6 `Hashtbl.mem` per block, at build time),
  shrinks uploads and startup, and helps the software backend too.
  Estimated several-fold fewer faces (most exposed blocks show 1-2
  faces in a flat-ish world), to be measured.
- **`~rendering:{ default_rendering with smooth_textures = false }`**:
  the original's `GL_NEAREST`. Today Minecraft3d gets the default
  (bilinear), which blurs the pixel-art blocks and blends each atlas
  cell with its neighbors along its borders.
- **Edits** (`plan_tiny_minecraft.md`'s Phase 5): after `add_block`/
  `remove_block` at `pos`, rebuild the chunks of `pos` and of its 6
  neighbors (a neighbor can be in the adjacent sector). A new
  `cached3d` value = a new id; the sweep frees the old mesh.

## Phasing

0. **Measure.** Give the OpenGL backend a `dump_frame` (`Gl.read_pixels`
   into a PNG, like the software backend's), so `-dump-frame` works on
   the GPU too: then `-uncapped -fixed-time 0 -dump-frame n file` both
   times n frames and saves the image to compare against later (same
   machine: GPU pixels aren't portable across drivers). For fps,
   `scripts/bench_playground.sh` (2D today: `-uncapped`, keys pressed,
   median of the logged fps) should work on the 3D executables as is,
   or with small changes. Record the
   baseline for `games3d/opengl/Minecraft3d.exe` (the target) and a
   big `Cubes3d`-like scene (for general benchmarks, which leave
   Minecraft3d out).
1. **`cached3d` + `Mesh_cache` + OpenGL.** The new `form3d` case in
   every match (list above), `Mesh_cache`, the `Gpu_scene` split, the
   OpenGL upload/draw/free, the "c" key and the stats line. A new
   `examples3d/` scene (e.g. thousands of cubes in one `cached3d`) to
   show and measure it; a golden frame of it on the software backend,
   identical to its uncached twin.
2. **The cheaper dynamic path** (Design 3), in `Gpu_scene`, used by
   OpenGL.
3. **WebGL**: its upload/draw/free for `Mesh_cache`, whenever
   `plan_webgl.md` reaches real scenes (its Phase 3). Whichever plan
   lands second follows the other's `Gpu_scene` interface.
4. **Minecraft3d**: per-sector `cached3d`, hidden-face culling,
   nearest textures. **Target: 60 fps capped on OpenGL**, `view` well
   under 1ms; then `games3d/webgl/Minecraft3d` (`plan_webgl.md`'s
   Phase 6), with its fps next to OpenGL's and software's.
5. **Chunk invalidation on edit**, once `plan_tiny_minecraft.md`'s
   Phase 5 exists to exercise it: no visible hitch per edit, live mesh
   count stable after many edits.
6. **Only if numbers say so** (each independent):
   - fog + draw distance (`rendering` hints, see Design 4);
   - frustum culling of cached nodes, with a bounding box computed at
     `cached3d` construction;
   - indexed drawing (`draw_elements`: 4 vertices per quad instead of
     6, see `notes_opengl.md` section 7);
   - a per-node model matrix, so moving objects can be cached too.

## Verification

- **Same pixels**: caching must never change the image. On the
  software backend, a golden frame (`tests/3d/`) of the new example,
  and a unit test that `Shape3d_render_software.faces (cached3d l)` =
  `faces (group3d l)`. On OpenGL, `-dump-frame` with and without
  `-keys c`, compared on the same machine.
- **Speed**: each phase's numbers (Phase 0 method, plus the stats
  line) recorded in this file as it completes, like
  `notes_3d_opti.md`.
- **No leaks**: the stats line's live mesh count stays constant across
  frames, and after edits (Phase 5).
- `make test` (all goldens unchanged: `Cached3d` is a new case, no
  existing scene uses it) and `scripts/smoke_test_playground3d.sh` on
  every backend after Phase 1, since every match on `form3d` changes.
