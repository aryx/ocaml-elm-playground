# OpenGL for `playground3d/`: a tutorial, compared with the software rasterizer

`notes_3d.md` explains 3D rendering from scratch, using the software
rasterizer (`playground3d/software/Playground3d_platform.ml`) as the
running example: every step is OCaml you can read. This note is the
companion for the GPU backend (`playground3d/opengl/Playground3d_platform.ml`
plus the shared `playground3d/Gpu_scene.ml`): what a GPU actually is,
how the same pipeline maps onto OpenGL, stage by stage against the
software version, and, the part that matters most in practice, *where
the time goes*, since that's where intuition from CPU programming is
most misleading (see the Minecraft3d story in section 6).

See also:
- `notes_3d.md`: the concepts (camera, projection, culling, z-buffer,
  rasterization, shading, UVs). Assumed known here.
- `notes_playground3d_related_work.md`: the history (fixed-function vs
  programmable OpenGL, WebGL, Vulkan, engines) and the first
  software-vs-OpenGL fps/LOC numbers.
- `done/plan_opengl.md`: how the backend was built, and its
  one-for-one comparison table (expanded in section 3 below).
- `plan_opengl_perf.md`: the plan to make big static scenes fast.

## 1. What a GPU is, from a programmer's point of view

Three facts explain almost everything else in this note:

1. **It's a separate computer.** A GPU has its own memory (VRAM) and
   its own processors, and the CPU talks to it through a driver, over
   a bus (PCIe for a discrete card). OpenGL was designed explicitly as
   a *client/server* protocol (the "client" is your program, the
   "server" is the GPU and driver; under X11 they could even be on
   different machines). Most OpenGL calls don't *do* anything
   immediately: they append a command to a queue that the GPU executes
   later, asynchronously. That's why `Gl.draw_arrays` returns
   instantly, and why measuring GPU time with `Unix.gettimeofday` around
   a draw call is meaningless (the real wait shows up later, usually in
   `Sdl.gl_swap_window`).
2. **It's massively parallel, but only for one kind of work.** Thousands
   of simple cores run *the same small program* on many inputs at once:
   the vertex shader on every vertex, the fragment shader on every
   pixel. It is excellent at "apply this formula to 2 million
   vertices", and bad at anything branchy, sequential, or
   pointer-chasing (walking an OCaml list, a hash table, a tree).
3. **Moving data to it is the expensive part.** Uploading geometry costs
   bus bandwidth and driver work; data that's *already* in VRAM is
   nearly free to reuse, frame after frame. The golden rule of GPU
   programming follows from this: **upload once, draw many times**.

A useful mental picture:

```
   CPU (OCaml)                         GPU
 +------------------+   commands   +---------------------------+
 | game logic       | -----------> | VRAM: vertex buffers,     |
 | build geometry   |   (queue)    |       textures, programs  |
 | issue draw calls |              | thousands of shader cores |
 +------------------+   uploads    | fixed-function rasterizer,|
          |           ===========> | depth test, blending      |
          |           (slow bus)   +---------------------------+
          v                                     |
   Sdl.gl_swap_window  <------------------------+  (finished frame)
```

The software backend has no such split: the "GPU" is the same CPU, the
"VRAM" is ordinary OCaml arrays (`pixels`, `zbuffer`), and nothing is
ever uploaded (except the final image, blitted by
`Sdl.update_window_surface`).

## 2. The pipeline, as OpenGL sees it

This is the same pipeline as `notes_3d.md` section 1, with the GPU names
and with what is *programmable* (you write it) versus *fixed-function*
(hardware does it, you only configure it):

```
 vertex buffer (VBO, described by a VAO)            <- you upload
      |
      v
 [vertex shader]      once per vertex, PROGRAMMABLE  <- you write (GLSL)
      |  gl_Position (clip space) + outputs (normal, uv, color)
      v
 primitive assembly   group vertices into triangles     fixed
 clipping             cut triangles at the view frustum fixed
 perspective divide   x/w, y/w, z/w -> NDC              fixed
 viewport transform   NDC -> pixel coordinates          fixed
 face culling         drop back-facing triangles        fixed (toggle)
 rasterization        triangle -> fragments (pixels),   fixed
                      interpolating shader outputs,
                      perspective-correct
      |
      v
 early depth test     skip hidden fragments             fixed (automatic)
 [fragment shader]    once per pixel, PROGRAMMABLE      <- you write (GLSL)
 depth test           z-buffer compare/write            fixed (toggle)
 blending             combine with framebuffer          fixed (toggle)
      |
      v
 framebuffer  -> Sdl.gl_swap_window
```

Our two shaders are tiny: `vertex_shader_source` multiplies each
position by one matrix (`gl_Position = uMVP * vec4(aPos, 1.0)`) and
passes normal/color/uv through; `fragment_shader_source` does
`brightness = ambient + (1 - ambient) * max(dot(n, light), 0)` and
either samples the texture or uses the vertex color. Everything else in
the diagram is hardware.

## 3. Stage by stage: software rasterizer vs. OpenGL

| Stage | Software backend (`software/Playground3d_platform.ml`) | OpenGL backend (`opengl/` + `Gpu_scene.ml`) |
|---|---|---|
| Scene -> triangles | `flatten_faces`, `fan_triangles`, per frame | `Gpu_scene.collect_batches`/`group_by_material`, per frame (today), then uploaded to a VBO |
| Camera | `view_space` (right/up/forward dot products per point) | `Gpu_scene.look_at`, one 4x4 matrix per frame |
| Projection | `project_vertex` (divide by view-space z, scale to pixels) | `Gpu_scene.perspective` matrix + the hardware's divide by `w` |
| Clipping | none: a vertex behind `near` just drops its triangle | hardware clips triangles against the frustum properly |
| Backface culling | `backface_culling_enabled` + a dot product per face ("b") | `Gl.enable Gl.cull_face_enum` (winding order in screen space) |
| Rasterization | `rasterize_triangle`: bounding box, edge functions, barycentric weights | fixed-function hardware, no code |
| Interpolation | `make_interpolator`, perspective-correct or not ("p") | always perspective-correct, no code, no choice |
| Hidden surfaces | `zbuffer` float array, or painter's sort ("z") | `Gl.enable Gl.depth_test` |
| Lighting | `make_shader` + `brightness_of_normal`, 4 modes ("m") | the fragment shader: per-pixel Phong only |
| Textures | `sample_texture`, nearest-neighbor by hand | `upload_texture` once, `texture(uTexture, vUv)` in GLSL, `Gl.nearest` |
| Wireframe | `draw_triangle_wireframe`, a hand-written line drawer ("f") | `Gl.polygon_mode ... Gl.line` ("f") |
| Presenting | `Sdl.update_window_surface` (copies our pixel array) | `Sdl.gl_swap_window` (the image is already in VRAM) |

Two things the table hides:

- **The GPU deletes code but also demands new code**: shader compile/
  link error checking (`compile_shader`/`link_program`: OpenGL never
  raises, a broken shader just draws nothing), buffer layouts
  (`vertex_attrib_pointer`, stride/offsets), a matrix module, grouping
  faces by material because a draw call binds one texture. The
  measured net result is in `notes_playground3d_related_work.md`'s
  postscript: about 22% fewer lines of actual code, not 80%.
- **Some choices disappear.** The software backend can show you the
  *wrong* way (linear interpolation's texture swimming, painter's
  algorithm failures, `PaintersAlgorithmFail3d`), which is great for
  learning. The GPU only does the right way; there's nothing to toggle.

## 4. Coordinate spaces and the 4x4 matrix

The software backend does projection with plain scalar math:
`view_space` gives `(x, y, z)` relative to the camera, and
`project_vertex` divides `x` and `y` by `z` (farther = smaller). A GPU
wants something uniform it can apply to every vertex with the same
instructions: one 4x4 matrix multiply. Two tricks make that possible:

- **Homogeneous coordinates.** A point `(x, y, z)` becomes
  `(x, y, z, 1)`. With a 4th component, *translation* becomes a matrix
  multiply too (the last column of `look_at`'s matrix holds
  `-dot right eye` etc.), so "move to camera space" and "rotate to
  camera orientation" fold into one matrix.
- **Deferring the divide.** A matrix can't divide by `z`. So the
  projection matrix instead *copies* view-space `z` into the 4th
  component `w` (the `0; 0; 1; 0` last row in `Gpu_scene.perspective`),
  and the hardware divides `x`, `y`, `z` by `w` after the vertex shader.
  Same math as `project_vertex`, just split in two.

The chain is usually written `projection * view * model`:

```
 model space --model--> world space --view--> camera space --projection--> clip space
   (the shape's own)     (the scene)          (look_at)                   (perspective)
                                  then: / w  -> NDC (-1..1) -> viewport -> pixels
```

We have no model matrix: `move3d`/`rotate3d` bake transforms directly
into world-space points on the CPU (see `Playground3d.ml`'s
`map_points`). Real engines keep each object's vertices in model space,
uploaded once, and move the object by changing its (16-float) model
matrix. That's why moving a character in a game costs nothing, while
`move3d` on a big shape re-computes every point. See
`plan_opengl_perf.md`'s "per-node model matrix" future item.

Depth has one GPU-specific subtlety: the projection maps view `z` in
`near..far` to NDC `-1..1` *non-linearly* (roughly like `1/z`), so most
depth precision is near the camera. Too small a `near` (or too large a
`far/near` ratio) makes distant surfaces that are close together
flicker through each other ("z-fighting"). The software z-buffer
compares raw view-space `z` in floats and doesn't have this problem in
the same form.

## 5. OpenGL objects and the "bind, then act" state machine

OpenGL is a big global state machine. You don't pass a buffer to a
function; you *bind* it (make it current) and then call functions that
act on "whatever is currently bound". The objects our backend uses:

| Object | What it is | In our code |
|---|---|---|
| Program | compiled + linked vertex and fragment shader | `link_program`, `Gl.use_program` |
| Uniform | a per-draw-call constant for the shaders | `uMVP`, `uLightDir`, `uUseTexture`, `uTexture` |
| VBO (vertex buffer object) | raw bytes in VRAM | `vbo`, filled by `Gl.buffer_data` |
| VAO (vertex array object) | *how* to read a VBO: which bytes are position/normal/color/uv | `vao`, set up once with `vertex_attrib_pointer` |
| Texture | an image in VRAM + sampling settings | `upload_texture`, `gl_texture_cache` |
| Framebuffer | where pixels go (the window's, by default) | implicit |

A draw call in our backend, reduced to its essence (`draw_group`):

```ocaml
Gl.buffer_data Gl.array_buffer size (Some vertex_data) Gl.dynamic_draw; (* upload *)
Gl.bind_texture Gl.texture_2d tex;                                      (* bind  *)
Gl.draw_arrays Gl.triangles 0 vertex_count                              (* act   *)
```

The `Gl.dynamic_draw`/`Gl.static_draw` argument is a *hint* to the
driver about how often the data will change, i.e. where to put it. We
re-upload the whole scene every frame (hence `dynamic_draw`); a cached
chunk mesh uploaded once would use `static_draw`.

State leaks are the classic bug class: forget to re-bind, and the next
call silently acts on whatever was bound before. Errors are silent too
(`glGetError` has to be polled). That's why debuggers like
**RenderDoc** or **apitrace**, which record every GL call of a frame
and show the state and buffers at each one, are the standard tools
here, in the role `-debug` logging plays for the software backend.

## 6. Where the time goes: a cost model

For the software backend the cost model is simple: every stage runs on
one CPU core, so cost ~ number of triangles + number of pixels filled.
For a GPU there are several separate budgets, and a frame is as slow as
the worst one:

| Cost | Scales with | Typical capacity | Our situation |
|---|---|---|---|
| **CPU scene building** | whatever your code does per frame | one core; OCaml allocation is cheap when short-lived, much less so when the GC has to promote it | **the bottleneck**: we rebuild everything, every frame |
| **Upload (bus)** | bytes sent per frame | GB/s, but with driver overhead | ~86MB/frame for Minecraft3d today |
| **Draw calls** | number of draw calls + state changes | a few thousand per frame in OpenGL | 1-2 per frame today, fine |
| **Vertex work** | vertices per frame | hundreds of millions to billions/s | ~2M/frame for Minecraft3d, fine |
| **Fragment work (fill rate)** | pixels shaded (x overdraw) | billions/s | window-sized, fine |

The Minecraft3d measurement (`plan_tiny_minecraft.md`, Phase 2) is the
concrete lesson: ~0.2 fps on OpenGL, with the GPU nearly idle, because
the first two rows cost over a second per frame while the last three
would take a few milliseconds. **Switching to a GPU speeds up the
stages the GPU does; it can't speed up CPU work you do before handing
it the data.**

### Rebuild vs. render

It helps to name the two kinds of per-frame work separately:

- **Rebuilding**: deciding what triangles exist and where, in world
  coordinates, and getting them into VRAM. CPU + bus.
- **Rendering**: transforming them for the current camera and turning
  them into pixels. GPU.

Rendering *has* to be redone every frame when the camera moves:
every vertex lands at a new screen position and almost every pixel
changes. That's normal, and it's exactly what the GPU is built for. But
the camera is one 16-float uniform (`uMVP`); moving the player doesn't
change a single byte of world-space geometry. Rebuilding is only needed
for what actually changed. A static world should be rebuilt once.

### Walkthrough: the player takes one step

Say the player presses "W" and moves forward by 0.1 units, in a world
whose chunk meshes are already cached in VRAM (the state
`plan_opengl_perf.md` aims for). Here is everything involved in the
next frame, and whether it changes:

| Data | Lives in | Changes when the player moves? | Per-frame cost |
|---|---|---|---|
| block vertices (positions, normals, uvs), world space | VRAM (VBOs), uploaded once | **no**: a block doesn't move because you did | zero |
| texture atlas | VRAM, uploaded once | no | zero |
| shader program | VRAM, compiled once | no | zero |
| player position, yaw/pitch | the OCaml model | yes | a few float updates in `update` |
| camera -> `uMVP` matrix | computed on the CPU, uploaded as a uniform | yes | `look_at` + `perspective` + `mat4_mul`, 64 bytes uploaded |
| list of draw calls | CPU -> command queue | no (same chunks) | ~one `draw_arrays` per chunk |
| each vertex's screen position | computed on the GPU by the vertex shader | **yes, all of them** | ~millions of 4x4 multiplies, in parallel |
| which pixels each triangle covers, depth, color | computed on the GPU (rasterizer, fragment shader) | **yes, nearly all pixels** | ~one fragment shader run per screen pixel (x overdraw, reduced by early-Z) |
| the framebuffer | VRAM | yes | cleared and redrawn, then swapped |

So "everything must be recomputed" is true, but only for the bottom
three rows, and those are exactly the work a GPU does in parallel
hardware at billions of operations per second. The top rows, the
*description* of the world, are what's expensive to produce (CPU,
OCaml allocation, bus upload), and none of them depends on where the
camera is. The camera's position enters the pipeline in exactly one
place: the `uMVP` uniform, applied to every vertex by
`gl_Position = uMVP * vec4(aPos, 1.0)`. That split, world-space data
kept in VRAM plus one small per-frame matrix, is why a game can move
the camera through millions of triangles at 60 fps.

The same step in our *current* OpenGL backend, for contrast: the top
row is rebuilt in OCaml and re-uploaded every frame whether or not
anything moved (~86MB for Minecraft3d), so moving costs exactly as much
as standing still, and both are slow. And in the software backend, the
bottom three rows run on one CPU core too (`view_space`,
`project_vertex`, `rasterize_triangle`, per vertex and per pixel), so
even with perfect caching of the top rows it can't be as fast: caching
avoids *rebuilding*, but only a GPU makes the per-frame *rendering*
cheap.

What *does* force rebuilding some top rows:
- a block added or removed: that chunk's mesh (and possibly its
  neighbors') is rebuilt and re-uploaded, once;
- walking far enough to change the set of visible chunks (with a draw
  distance): new chunks are built/uploaded, old ones freed;
- an object that moves on its own (a character, a projectile): with a
  per-object model matrix, only its 16 floats change, like the camera;
  without one (our `move3d`, which bakes positions into world space),
  its vertices are rebuilt every frame, which is fine only for small
  objects.

The original Python/Pyglet tiny-minecraft is the textbook example of
this: `_show_block` copies a block's 24 vertices into a
`pyglet.graphics.Batch` (VBOs) once when the block becomes visible, and
`on_draw` is just `self.model.batch.draw()`. It does no hidden-face
culling and no frustum culling, and it is written in a slow language,
but it runs well because nothing proportional to the world size
happens in Python per frame. See `plan_opengl_perf.md` for the full
analysis and the `cached3d` design that brings the same property to
playground3d without giving up the pure `view` function.

### Immediate mode vs. retained mode, three times over

The same tension shows up at three levels, which is confusing until
you notice it's one idea:

1. **Old OpenGL itself**: `glBegin`/`glVertex3f`/`glEnd` sent every
   vertex through the driver every frame (immediate mode); display
   lists and then VBOs (retained mode) were added precisely because
   that doesn't scale. Core-profile OpenGL (which we use) removed
   `glBegin` entirely.
2. **The playground API**: `view` returns the whole scene every frame
   (immediate-mode API), which is lovely to program against and costs
   O(scene) per frame. `cached3d` is the retained-mode escape hatch.
3. **UI libraries**: Dear ImGui (immediate) vs. retained widget trees;
   Elm/React's virtual DOM is immediate-mode on the surface with
   diffing and `Html.lazy` memoization underneath. `cached3d` is the
   same trick as `Html.lazy`.

## 7. What the GPU does to avoid wasted work anyway

Even though rendering is redone every frame, there's plenty of
optimization, at different levels:

- **In hardware, automatically**:
  - clipping drops off-screen triangles before rasterization (their
    vertices are still transformed);
  - face culling drops ~half the triangles of closed objects before
    rasterization;
  - **early-Z**: fragments that fail the depth test are skipped
    *before* running the fragment shader. Drawing roughly
    front-to-back makes this pay off; the software backend always runs
    its shader and then compares z;
  - a **post-transform vertex cache**: with *indexed* drawing
    (`glDrawElements` + an index buffer), a vertex shared by several
    triangles is transformed once. We use `draw_arrays`, 6 vertices per
    quad instead of 4, so a third of our vertex work is redundant
    (harmless at our scale).
- **In the engine, on the CPU, before submitting anything**:
  - frustum culling of whole chunks/objects (skip what's outside the
    view);
  - occlusion culling (skip what's hidden behind other things;
    Minecraft's "cave culling"; Quake's precomputed PVS, see
    `notes_vs_doom_quake.md`);
  - level of detail (simpler meshes far away), draw distance + fog
    to hide the cutoff. The Python original does this crudely: its
    `gluPerspective(65.0, ..., 0.1, 60.0)` far plane at 60 units
    clips everything farther, and linear fog ending at 60 hides the
    edge;
  - hidden-face culling and greedy meshing when *building* voxel
    meshes (a build-time cost, paid once per chunk).
- **Across frames**: temporal anti-aliasing (TAA) and upscalers
  (DLSS/FSR) render fewer pixels and reconstruct the rest from previous
  frames; VR "timewarp" re-projects the last frame to a new head
  position when a frame is late; shadow maps of static lights are
  cached. This is the closest a GPU pipeline gets to "recompute only
  what changed" at the pixel level.

Contrast with 2D GUI toolkits, which redraw only "damaged" rectangles:
that works because in 2D most pixels don't change between frames. In
3D with a moving camera, nearly all of them do, so whole-frame redraw
is the norm and the effort goes into making each frame cheap instead.

## 8. Legacy OpenGL vs. what we use (reading the Python original)

The Python original uses *compatibility-profile* (fixed-function era)
OpenGL; our backend uses the *3.3 core profile*
(`Sdl.Gl.context_profile_core` in `run_app3d`). When porting from
older code or tutorials, these are the things that don't exist anymore
in core, and what replaces them:

| Legacy (Python original) | Core 3.3 (our backend) |
|---|---|
| `glMatrixMode`, `glRotatef`, `glTranslatef`, `gluPerspective` (a built-in matrix stack) | compute matrices yourself (`Gpu_scene.look_at`/`perspective`), upload as a uniform |
| `GL_QUADS` | gone: triangles only (`fan_triangles` splits each quad into 2) |
| `glEnable(GL_FOG)`, `glFogf(...)` | gone: a few lines in the fragment shader (mix the color toward the fog color by distance) |
| built-in lighting (`glLight*`) | gone: write it in the fragment shader (ours) |
| `glColor3d`, `glBegin`/`glEnd` | vertex attributes in a VBO |
| pyglet `Batch` (a helper library managing VBOs for you) | our own VBO/VAO code, and `cached3d` (planned) |

Things that did carry over as-is: `glEnable(GL_CULL_FACE)`,
`GL_NEAREST` texture filtering (the pixelated Minecraft look; `GL_LINEAR`
would blur the 16x16-ish atlas cells), the depth test, a texture atlas
to keep everything in one material.

## 9. Small gotchas we actually hit

- **Silent shader failures**: a GLSL typo produces no exception and a
  blank window unless you check `compile_status`/`link_status` yourself
  (`compile_shader`/`link_program` do).
- **"Textures are upside down"**: true in some pipelines, not in ours;
  see the comment above `fragment_shader_source` for why `v=0` is
  already the top row with stb_image.
- **Row alignment**: `glTexImage2D` assumes 4-byte-aligned rows by
  default; `Gl.pixel_storei Gl.unpack_alignment 1` in `upload_texture`.
- **FFI + GC**: an intermittent shader-compile failure turned out to be
  a GC-timing bug in tgls' `glShaderSource` binding, mitigated by a
  `Gc.full_major ()` before compiling. See
  `notes_debugging_techniques.md` section 7.
- **GC pressure as a performance bug**: big, long-lived per-frame
  allocations (lists of millions of boxed vertex tuples) get promoted
  to the major heap, and frame time *grows* over time. See section 6.

## 10. Glossary

- **VBO**: vertex buffer object, raw vertex bytes in GPU memory.
- **VAO**: vertex array object, the recipe for reading a VBO's bytes as
  attributes (position, normal, ...).
- **Shader**: a small GPU program (GLSL); *vertex* shaders run per
  vertex, *fragment* shaders per pixel.
- **Uniform**: a constant passed to shaders for a whole draw call
  (e.g. the MVP matrix).
- **Attribute / varying**: per-vertex input to the vertex shader /
  per-vertex output interpolated across the triangle for the fragment
  shader (`out`/`in` in GLSL 3.3).
- **Fragment**: a candidate pixel produced by rasterization, before the
  depth test decides whether it survives.
- **MVP**: model-view-projection matrix, the one matrix the vertex
  shader applies.
- **Clip space / NDC**: coordinates after the projection matrix / after
  the divide by `w` (-1..1 on each axis).
- **Draw call**: one `glDrawArrays`/`glDrawElements`; each has fixed
  CPU/driver overhead.
- **Immediate vs. retained mode**: re-describe everything every frame
  vs. describe once and reference it afterward.
- **Early-Z**: depth test before the fragment shader, to skip shading
  hidden pixels.
- **Z-fighting**: flicker between nearly coplanar surfaces from limited
  depth precision.
- **Core vs. compatibility profile**: modern OpenGL without vs. with
  the legacy fixed-function API.
