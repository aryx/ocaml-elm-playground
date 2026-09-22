# OpenGL for `Playground3d`: a tutorial, compared with the software rasterizer

`notes_3d.md` explains 3D rendering from scratch, using the software
rasterizer (the software backend's `Playground3d_platform.ml`) as the
running example: every step is OCaml you can read. This note is the
companion for the GPU backend (the OpenGL backend's `Playground3d_platform.ml`
plus the shared `Gpu_scene.ml`): what a GPU actually is,
how the same pipeline maps onto OpenGL, stage by stage against the
software version, and, the part that matters most in practice, *where
the time goes*, since that's where intuition from CPU programming is
most misleading (see the TinyMinecraft story in section 6).

See also:
- `notes_3d.md`: the concepts (camera, projection, culling, z-buffer,
  rasterization, shading, UVs). Assumed known here.
- `notes_playground3d_related_work.md`: the history (fixed-function vs
  programmable OpenGL, WebGL, Vulkan, engines) and the first
  software-vs-OpenGL fps/LOC numbers.
- `done/plan_opengl.md`: how the backend was built, and its
  one-for-one comparison table (expanded in section 3 below).
- `plan_opengl_perf.md`: how big static scenes were made fast
  (`cached3d`, `Mesh_cache`), with the measurements.

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

| Stage | Software backend (`software/Playground3d_platform.ml`) | OpenGL backend (`native/Playground3d_platform.ml` + `Gpu_scene.ml`) |
|---|---|---|
| Scene -> triangles | `flatten_faces`, `fan_triangles`, per frame | `Gpu_scene.collect_batches`/`group_by_material`, per frame, then uploaded to a VBO; a `cached3d`'s only once (see section 6) |
| Camera | `view_space` (right/up/forward dot products per point) | `Mat4.look_at`, one 4x4 matrix per frame |
| Projection | `project_vertex` (divide by view-space z, scale to pixels) | `Mat4.perspective` matrix + the hardware's divide by `w` |
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
  component `w` (the `0; 0; 1; 0` last row in `Mat4.perspective`),
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
`plan_3d_remaining.md`'s "per-node model matrix" item.

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
driver about how often the data will change, i.e. where to put it. The
scene's uncached part is re-uploaded every frame (hence
`dynamic_draw`); a `cached3d`'s mesh is uploaded once, with
`static_draw` (see section 6).

### VBO and VAO: the bytes, and how to read them

A VBO is just bytes in GPU memory; nothing in it says what they mean.
The vertex shader, on the other hand, has named inputs (`aPos`,
`aNormal`, `aColor`, `aUv`), each at a numbered *location* (the
`layout (location = N)` in the shader). Something has to connect the
two: for each input, which buffer it's read from, where it starts in a
vertex (the offset), how many floats it has (the size), and how far
apart two consecutive vertices are (the stride). Our layout, written by
`Gpu_scene.vertex_floats_of_group`, is 11 interleaved floats, 44 bytes,
per vertex:

```
 one vertex = 11 floats = 44 bytes (the stride)
 +----------+----------+----------+--------+----------+----
 | x  y  z  | nx ny nz | r  g  b  | u  v   | x  y  z  | ...  next vertex
 +----------+----------+----------+--------+----------+----
 ^ offset 0 ^ 12       ^ 24       ^ 36     ^ 44
 aPos (0)   aNormal (1) aColor (2) aUv (3)     <- the shader's inputs (locations)
```

Each input is described by one call, e.g. for the normals, "location
1: 3 floats, 44 bytes apart, starting at byte 12":

```ocaml
Gl.vertex_attrib_pointer 1 3 Gl.float false 44 (`Offset 12);
Gl.enable_vertex_attrib_array 1
```

A **VAO** (vertex array object) is the object that *remembers* these
calls: after the four of them, a single `Gl.bind_vertex_array vao`
brings the whole description back. An analogy: the VBO is a file of
numbers, the VAO its format description.

The subtle point: `vertex_attrib_pointer` records the VBO bound *when
it's called*, so a VAO is tied to its buffer(s), not just to a layout.
That's why the OpenGL backend creates a VAO/VBO pair per mesh
(`create_vao_vbo`: one pair for the scene's uncached part, one per
material of each `cached3d`), and switching from one chunk of the
Minecraft world to the next is one `bind_vertex_array`, then
`draw_arrays`.

WebGL 1 has no VAOs (only through the `OES_vertex_array_object`
extension; WebGL 2 has them built in), so the WebGL backend makes the
calls again each time it switches buffers
(`set_attribute_pointers`): the same result, just not remembered.
Four small calls per buffer, cheap at the 121 chunks of TinyMinecraft.

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

| Cost | Scales with | Typical capacity | TinyMinecraft, rebuilding every frame | TinyMinecraft, cached chunks |
|---|---|---|---|---|
| **CPU scene building** | whatever your code does per frame | one core; OCaml allocation is cheap when short-lived, much less so when the GC has to promote it | **the bottleneck**: the whole world, every frame, seconds | none (built once, at startup) |
| **Upload (bus)** | bytes sent per frame | GB/s, but with driver overhead | ~86MB per frame | none (~18MB once) |
| **Draw calls** | number of draw calls + state changes | a few thousand per frame in OpenGL | 1 | 121, one per chunk, fine |
| **Vertex work** | vertices per frame | hundreds of millions to billions/s | ~2M, fine | ~400k (hidden faces skipped), fine |
| **Fragment work (fill rate)** | pixels shaded (x overdraw) | billions/s | window-sized, fine | same |

The TinyMinecraft measurement (`plan_tiny_minecraft.md`, Phase 2) is the
concrete lesson: ~0.15 fps on OpenGL, with the GPU nearly idle, because
the first two rows cost seconds per frame while the last three take a
few milliseconds. **Switching to a GPU speeds up the stages the GPU
does; it can't speed up CPU work you do before handing it the data.**
Removing that CPU work, by building the world once and keeping it in
GPU memory (the last column, see "Rebuild vs. render" below), took the
CPU time per frame from ~6.5s to ~1ms (`plan_opengl_perf.md`, Results).

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
whose chunk meshes are already cached in VRAM (TinyMinecraft's
`cached3d` chunks; its camera doesn't move yet, but nothing below
depends on that). Here is everything involved in the next frame, and
whether it changes:

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

The same step without the cache, for contrast (a scene of plain
`group3d`s, or the "o" key of the OpenGL backend with `-debug-keys`):
the top row is rebuilt in OCaml and re-uploaded every frame whether or
not anything moved (~86MB for TinyMinecraft before hidden-face culling),
so moving costs exactly as much as standing still, and both are slow.
And in the software backend, the
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
| `glMatrixMode`, `glRotatef`, `glTranslatef`, `gluPerspective` (a built-in matrix stack) | compute matrices yourself (`Mat4.look_at`/`perspective`), upload as a uniform |
| `GL_QUADS` | gone: triangles only (`fan_triangles` splits each quad into 2) |
| `glEnable(GL_FOG)`, `glFogf(...)` | gone: a few lines in the fragment shader (mix the color toward the fog color by distance) |
| built-in lighting (`glLight*`) | gone: write it in the fragment shader (ours) |
| `glColor3d`, `glBegin`/`glEnd` | vertex attributes in a VBO |
| pyglet `Batch` (a helper library managing VBOs for you) | our own VBO/VAO code, `cached3d` and `Mesh_cache` |

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

## 10. Compared with Vulkan, Metal, WebGPU, and engines

Section 3 compares us with our own software rasterizer, and
`notes_playground3d_related_work.md` tells the history (OpenGL, WebGL,
Vulkan, Unity). What's left is what our code would become on the newer
APIs, and what an engine adds on top.

**The newer APIs.** Vulkan (2016), Metal (2014) and WebGPU (2023)
keep the pipeline of section 2 and its two shaders; they drop the
"bind, then act" state machine of section 5. The shaders, the vertex
layout (our four `vertex_attrib_pointer` calls) and most fixed-function
settings are baked together, once, into an immutable *pipeline*
object; in Vulkan and WebGPU face culling is part of it, so our "b" key
would switch between two pipelines instead of calling
`Gl.enable`/`Gl.disable`, and WebGPU has no wireframe mode at all ("f").
Uniforms and textures go in explicit descriptor sets (bind groups in
WebGPU). And the command queue of section 1 becomes visible: you record
command buffers and submit them yourself, allocate GPU memory yourself,
and synchronize with fences. Nothing in our frame is expensive enough
to need what that buys (recording from several threads, no hidden
driver validation), and shader source goes through an offline compiler
first (`notes_opengl_shaders.md` section 2).

**Engines.** three.js, Unity or Godot sit where `Playground3d` sits,
between a scene description and the GPU, but do per frame what sections
4 and 7 say we don't: a model matrix per object (moving one costs 16
floats, not its vertices), frustum culling of each object against its
bounding volume, indexed meshes, transparent objects sorted back to
front and blended, shadow maps from extra render passes, mipmapped
textures. Each is a few dozen lines on its own; section 11 lists them
as exercises.

## 11. What's missing, and exercises

Things real OpenGL programs do that our backends don't, in rough order
of difficulty:

- **Error checking**: call `glGetError` after each frame (or install a
  `KHR_debug` message callback, core since OpenGL 4.3, so not in our
  3.3 context without the extension) and log what it says; see how many
  silent bugs of section 5 it would have caught.
- **Mipmaps**: `Gl.generate_mipmap` after the upload in
  `upload_texture`, and a `Gl.linear_mipmap_linear` minification
  filter, so a far face reads a smaller copy of its texture instead
  of skipping texels (the shimmer of distant faces when the camera
  moves; Williams 1983).
- **Indexed drawing** (section 7): 4 vertices and 6 indices per quad in
  `Gpu_scene.vertex_floats_of_group`, an index buffer next to each VBO,
  `glDrawElements` instead of `draw_arrays`; a third fewer vertices to
  upload and transform.
- **Antialiasing (MSAA)**: ask SDL for a multisampled framebuffer
  (`Sdl.Gl.multisamplebuffers`, `Sdl.Gl.multisamplesamples`, before
  the window is created, next to the `context_profile_mask` attribute),
  and compare edges with the software backend's.
- **Frustum culling of cached meshes** (section 7): keep a bounding
  box with each `Mesh_cache` mesh, test it against the six planes of
  the `uMVP` frustum, and skip its `draw_arrays`; count the draw calls
  saved in TinyMinecraft.
- **Transparency**: `fade3d`'s alpha is ignored by both GPU backends
  (`Gpu_scene` never reads it). Pass it as a fourth color component,
  draw the opaque batches first, then the translucent ones sorted back
  to front with `Gl.blend` on and depth writes off.
- **A model matrix** (section 4): a `uModel` uniform, so that a
  `move3d`/`rotate3d` of a `cached3d` keeps the cache (see
  `plan_3d_remaining.md`'s "per-node model matrix").
- **Shadow maps**: render the scene's depth from the light into a
  texture (a framebuffer object), then compare with it in the fragment
  shader; the first render pass that isn't to the window.
- **WebGL 2**: VAOs and GLSL ES 3.00 built in, so `web/`'s
  `set_attribute_pointers` and most of `notes_opengl_shaders.md`
  section 9 disappear; needs bindings js_of_ocaml doesn't ship.

## 12. In the playground

A program sees none of this: it builds `shape3d`s and calls
`Playground3d_platform.run_app3d` (`Playground3d_platform.mli`), and the
library it links picks the backend: `elm_playground_3d_opengl`
(`playground/native/Playground3d_platform.ml`, SDL + tgls),
`elm_playground_3d_webgl` (`playground/web/`, the same pipeline over
js_of_ocaml's WebGL 1 bindings), or the software one of section 3. Both
GPU backends share `Gpu_scene.ml` (shapes to batches, one per
material, and the 11-float layout of section 5) and
`graphics/gpu/Mesh_cache` (the id -> mesh table behind `cached3d`,
section 6). From `Playground3d.mli`, three things reach the GPU:
`cached3d` (section 6: upload once, draw many times), the `rendering`
record (`shading`, `backface_culling`, `smooth_textures`, turned into
a uniform, `Gl.enable`/`disable` and the texture filter), and `hud`
(the 2D overlay, drawn by the 2D software renderer into a texture and
blended over the scene, the only blending we do). With `-debug-keys`,
the OpenGL backend's "f", "m", "b", "i", "o" and "u" toggle wireframe,
shading, culling, texture filtering, the cache and the HUD; `-debug`
prints the vertices uploaded per frame.

The examples, in the order of this note: `Triangle3d` (the smallest
scene; one pixel of it is computed by hand in `notes_opengl_shaders.md`),
`Cubes3d` (depth test and culling) and `Spheres3d` (the shading
modes), section 3,
`TexturedCube3d` (a texture, `upload_texture`), and `CachedGrid3d`
(1600 static cubes as one `cached3d`, orbited by the camera: section 6
in miniature, "o" to see the cost of rebuilding). Then the games:
TinyMinecraft, where section 6's numbers come from (121 cached chunks,
a texture atlas), and the other 3D games made of static levels, which
all wrap them in `cached3d`: TinyQuake, TinyDoom3d, TinyWolfenstein3d,
TinyTombRaider, TinyMarioKart64, TinyDescent3d, among others.

## Glossary

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

## References

- Lawrence G. Roberts, "Homogeneous Matrix Representation and
  Manipulation of N-Dimensional Constructs", MIT Lincoln Laboratory
  MS-1405, 1965 (section 4).
- Edwin Catmull, "A Subdivision Algorithm for Computer Display of
  Curved Surfaces", PhD thesis, University of Utah, 1974 (the
  z-buffer).
- James H. Clark, "The Geometry Engine: A VLSI Geometry System for
  Graphics", SIGGRAPH '82 (the transform stage in hardware).
- Lance Williams, "Pyramidal Parametrics", SIGGRAPH '83 (mipmaps).
- Juan Pineda, "A Parallel Algorithm for Polygon Rasterization",
  SIGGRAPH '88 (edge functions, what the hardware rasterizer does).
- Mark Segal, Kurt Akeley, "The OpenGL Graphics System: A
  Specification, Version 1.0", Silicon Graphics, 1992.
- Jackie Neider, Tom Davis, Mason Woo, "OpenGL Programming Guide" (the
  red book), Addison-Wesley, 1993.
- Kurt Akeley, "RealityEngine Graphics", SIGGRAPH '93.
- Ned Greene, Michael Kass, Gavin Miller, "Hierarchical Z-Buffer
  Visibility", SIGGRAPH '93 (rejecting hidden fragments early).
- Hugues Hoppe, "Optimization of mesh locality for transparent vertex
  caching", SIGGRAPH '99 (the post-transform vertex cache).
- Mark Segal, Kurt Akeley, "The OpenGL Graphics System: A
  Specification, Version 3.3 (Core Profile)", Khronos Group, 2010.
- Khronos Group, "WebGL Specification, Version 1.0", 2011.
- Fabian Giesen, "A trip through the Graphics Pipeline 2011", blog
  series, 2011 (what the driver and the hardware do with each call).
- Tomas Akenine-Möller, Eric Haines, Naty Hoffman et al., "Real-Time
  Rendering", 4th ed., CRC Press, 2018.
