# Shaders in `Playground3d`: what they are, where they come from, and how ours work

Both GPU backends, the OpenGL one and the WebGL one,
contain two small programs written in another language, stored in
OCaml strings:

```ocaml
let vertex_shader_source =
  "attribute vec3 aPos;\n\
   attribute vec3 aNormal;\n\
   ...
   void main() {\n\
  \  gl_Position = uMVP * vec4(aPos, 1.0);\n\
   ...
```

This note explains that code: what a shader is and why a GPU wants
one, where the idea and the languages come from, what the
alternatives are, and then a line-by-line tutorial of our two shaders,
in both of the dialects we use (OpenGL's GLSL 3.30 and WebGL's GLSL ES
1.00), down to computing one pixel by hand.

See also:
- `notes_opengl.md`: the GPU pipeline around the shaders (section 2),
  the objects (buffers, programs, uniforms, section 5) and where the
  time goes. Assumed known here, at least section 2.
- `notes_3d.md` and `notes_3d_shading.md`: the lighting math itself
  (normals, Lambert, flat/Gouraud/Phong), done by hand in the software
  rasterizer; `graphics/3d/geometry/Lighting.mli` for the formula.
- `done/plan_webgl.md`: the WebGL backend, and its table of OpenGL -> WebGL
  differences.

## 1. What a shader is, and why

In the software rasterizer, "what color is this pixel?" is an OCaml
function called in the inner loop of `graphics/3d/Triangle.ml`, once
per covered pixel. A GPU has the same loop, but in hardware, run on
thousands of small cores at once (`notes_opengl.md` section 1). To let
you decide what happens inside that loop, it takes a small program
from you and runs it there:

- a **vertex shader**, run once per vertex: it must say where the
  vertex lands on the screen (`gl_Position`), and can compute other
  values for later;
- a **fragment shader**, run once per pixel covered by a triangle (a
  "fragment": a pixel candidate, it can still lose the depth test):
  it must say the pixel's color (`gl_FragColor`, or an `out` variable).

Between the two, fixed hardware does the rest: assembling triangles,
clipping, culling, the divide by `w`, finding covered pixels, and
**interpolating** whatever the vertex shader output, across the
triangle, for each pixel. Those two small functions are the only code
of ours that runs on the GPU.

Why a separate language rather than, say, OCaml? The GPU is a
different machine, with a different instruction set, varying from one
vendor and generation to the next, and none of it public: no one ships
an OCaml (or C) compiler for it. Instead, the *driver* contains a
compiler for a small C-like language, and your program hands it the
source text **at run time**: that's why the shaders are strings, and
`compile_shader` calls `gl##compileShader` on one while the app starts.
It's also why a typo is only found then, and only if you ask
(`getShaderParameter ... COMPILE_STATUS`, see section 7).

## 2. History: from fixed functions to programs

- **The name (1984-1990).** Rob Cook's "Shade Trees" (SIGGRAPH 1984)
  described a surface's appearance as a small expression tree,
  instead of one hardcoded lighting formula for all; Ken Perlin's
  "An Image Synthesizer" (SIGGRAPH 1985) did it as a small language
  run per pixel, and introduced his noise function. Pixar's RenderMan
  (1988; Hanrahan and Lawson, "A Language for Shading and Lighting
  Calculations", SIGGRAPH 1990) made a *shading language* the
  standard way films describe materials. These ran on CPUs, offline,
  minutes per frame.
- **Fixed-function GPUs (1992-2000).** OpenGL 1.0 (1992) and the first
  consumer 3D cards had one lighting model built in, configured with
  calls: `glLight(GL_LIGHT0, GL_POSITION, ...)`, `glMaterial`,
  `glEnable(GL_FOG)`. Fast, but you got that formula and a few knobs,
  nothing else (`notes_opengl.md` section 8: that's the API the Python
  tiny-minecraft uses).
- **Programmable, in assembly (2001-2002).** The GeForce 3 (2001) and
  DirectX 8 let you replace the vertex stage (and, more limited, the
  pixel stage) with short programs in an assembly-like language, with a
  hard limit on instruction count; OpenGL followed with the
  `ARB_vertex_program`/`ARB_fragment_program` extensions (2002).
- **High-level languages (2002-2004).** NVIDIA's Cg and Microsoft's
  HLSL (DirectX 9) in 2002; the OpenGL Shading Language, **GLSL**,
  as extensions and then in the core of OpenGL 2.0 (2004). C-like,
  with vector and matrix types built in.
- **Programmable only (2007-2010).** OpenGL ES 2.0 (2007, phones)
  dropped the fixed functions entirely: no shader, no picture. Its
  dialect is **GLSL ES 1.00**. Desktop OpenGL deprecated them in 3.0
  (2008) and removed them from the *core profile* in 3.2 (2009);
  OpenGL 3.3 (2010) aligned its version numbers with GLSL's: **GLSL
  3.30**, what `opengl/` asks for with `#version 330 core`.
- **The web (2011, 2017).** WebGL 1 (2011) is OpenGL ES 2.0 for
  JavaScript, hence GLSL ES 1.00 in `webgl/`; WebGL 2 (2017) is ES
  3.0, with GLSL ES 3.00, very close to GLSL 3.30 (js_of_ocaml only
  binds WebGL 1, see `done/plan_webgl.md`). Browsers don't necessarily hand
  your GLSL to an OpenGL driver: Chrome's and Firefox's ANGLE
  translates it to Direct3D's HLSL on Windows, or to Metal on macOS.
- **Precompiled (2014-).** Metal (2014) has its own language (MSL);
  Vulkan (2016) takes **SPIR-V**, a binary intermediate code, compiled
  *before* shipping (from GLSL or HLSL), so the driver no longer
  parses source text; WebGPU (2023 in Chrome) has **WGSL**. Same two
  stages, same ideas; mostly different spellings.

## 3. Alternatives

For the programs themselves, there is no real alternative on today's
APIs: fixed-function lighting only exists in legacy OpenGL
(compatibility profile), not in the core 3.3 profile `opengl/` uses,
nor in WebGL. The choices are about *how the shader source reaches the
driver*:

- **Strings in the `.ml`, what we do.** One file per backend, nothing
  to install or load at run time, the GLSL right next to the OCaml
  that feeds it (the attribute names and the buffer layout must match,
  section 5). The cost: the OCaml compiler sees only a string, so a
  GLSL error, or a name mismatch between the two languages, shows up
  only when the app starts.
- **Separate `.glsl` files**, read at start (or embedded by a dune
  rule). Editors can highlight and check them (`glslangValidator` can
  compile them offline), but a web page would have to fetch them, and
  the two halves of a contract are in two files.
- **Checked at compile time.** Elm's own `elm-explorations/webgl`
  (used by `ianmackenzie/elm-3d-scene`, see
  `notes_playground3d_related_work.md`) writes shaders in
  `[glsl| ... |]` blocks that the Elm compiler parses, typing the
  attributes, uniforms and varyings as Elm records: a mismatch is a
  compile error. The OCaml equivalent would be a ppx; none is widely
  used, and it would be a big dependency for two tiny programs.
- **Generated from a host-language DSL** (shader combinators, or
  compiling a subset of the host language to GLSL: Haskell, Rust and
  F# have such projects). Real type safety, at the price of learning
  one more layer; overkill here.
- **The string syntax.** Ours uses `"...\n\` line continuations,
  where the next line's leading blanks are skipped, and `\ ` to keep
  the indentation inside `main`. OCaml's quoted strings,
  `{|...|}`, would take the GLSL verbatim, with no escapes at all;
  either is fine for the GPU.

## 4. The contract between the OCaml side and the shaders

Each shader has three kinds of inputs and outputs, and the OCaml code
must provide exactly what the GLSL declares:

```
  OCaml (CPU)                        GPU
  -----------                        ---

  float array, 11 floats/vertex      attribute vec3 aPos      per vertex
  (Gpu_scene.vertex_floats_of_group)   aNormal, aColor, aUv   (in: GLSL 3.30)
    |  bufferData + vertexAttribPointer       |
    +---------------------------------> [vertex shader]
                                              |
  uniform3f, uniformMatrix4fv, ...  uniform mat4 uMVP    same for the
    |                               uniform vec3 uLightDir  whole draw call
    +---------------------------------------->|
                                              v
                                     varying vec3 vNormal  per vertex out,
                                       vColor, vUv, vPos   interpolated,
                                              |            per pixel in
                                     (rasterizer interpolates) (out/in: 3.30)
                                              v
                                     [fragment shader] -> gl_FragColor
```

- **Attributes**: per-vertex inputs, read from a buffer. The OCaml side
  describes the buffer's layout, once per attribute: "`aNormal` is 3
  floats, starting 3 floats into each vertex, and vertices are 11
  floats apart" (`vertexAttribPointer loc 3 FLOAT false stride (3 * 4)`,
  in bytes):

  ```
  one vertex, 11 floats = 44 bytes (the stride)
  +----+----+----+----+----+----+----+----+----+----+----+
  | px | py | pz | nx | ny | nz | r  | g  | b  | u  | v  |
  +----+----+----+----+----+----+----+----+----+----+----+
   aPos (offset 0)  aNormal (12)   aColor (24)    aUv (36)
  ```

  To connect the name in the GLSL to that description, GLSL 3.30 lets
  the shader fix a number, `layout (location = 1) in vec3 aNormal;`,
  and the OCaml side uses the same number. GLSL ES 1.00 has no
  `layout`: the linker picks, and `getAttribLocation program "aNormal"`
  asks it (-1 if the shader never uses that attribute, in which case
  the linker removed it).
- **Uniforms**: inputs constant for a whole draw call, set from OCaml
  by name: the camera matrix, the light, which shading mode, whether
  to sample the texture. Cheap to change between draw calls, and the
  way a backend's rendering hints reach the shader (`uShading`).
- **Varyings**: the vertex shader's outputs, which become the fragment
  shader's inputs. The vertex shader writes one value per vertex; the
  rasterizer gives each pixel a blend of the triangle's three values,
  weighted by how close the pixel is to each corner (barycentric
  weights, perspective-correct: `notes_3d.md`, and `Interpolate` in
  `graphics/3d/`, which does by hand what the hardware does here). The
  names must match exactly between the two shaders, or linking fails.

## 5. Tutorial: our vertex shader

The WebGL version (GLSL ES 1.00):

```glsl
attribute vec3 aPos;      // per vertex, from the buffer
attribute vec3 aNormal;
attribute vec3 aColor;
attribute vec2 aUv;
varying vec3 vNormal;     // outputs, interpolated for the fragment shader
varying vec3 vColor;
varying vec2 vUv;
varying vec3 vPos;
uniform mat4 uMVP;        // the camera, for the whole draw call
void main() {
  gl_Position = uMVP * vec4(aPos, 1.0);
  vPos = aPos;
  vNormal = aNormal;
  vColor = aColor;
  vUv = aUv;
}
```

Line by line:

- **Types.** `vec2`/`vec3`/`vec4` are 2/3/4 floats, `mat4` a 4x4
  matrix: GLSL has vectors built in, with `+`, `*` (component-wise for
  vectors, a real matrix product for `mat * vec`), and functions like
  `dot`, `cross`, `normalize`, `max`. Integers (`int`) and booleans
  (`bool`) exist too, but GPUs are float machines.
- **`void main()`**: the entry point, called once per vertex, with the
  attributes already filled with *this* vertex's values. No loop over
  vertices anywhere: the GPU runs many `main`s in parallel.
- **`vec4(aPos, 1.0)`**: a point in homogeneous coordinates, `w = 1`
  (`notes_opengl.md` section 4: `w = 1` makes the matrix's translation
  column apply; a direction would have `w = 0`).
- **`gl_Position = uMVP * ...`**: the one required output, in *clip
  space*. `uMVP` is `Mat4.mul projection view` from the OCaml side,
  so this is the camera (look_at) then the perspective, the same two
  steps the software rasterizer does with `Camera.view` and
  `Project`. The hardware then divides by `w` and maps to pixels.
- **The four copies to varyings**: this shader does no lighting
  itself; it passes everything along to be interpolated. (Doing the
  lighting here, per vertex, and interpolating the resulting color,
  would be Gouraud shading; doing it per pixel from interpolated
  normals, in the fragment shader, is Phong: `notes_3d_shading.md`.)

The OpenGL version differs only in spelling: `#version 330 core` first
(which dialect this is), `layout (location = N) in` for `attribute`,
`out` for `varying`.

## 6. Tutorial: our fragment shader

The WebGL version, with the `OES_standard_derivatives` extension
present (`fragment_shader_source ~derivatives:true`):

```glsl
#extension GL_OES_standard_derivatives : enable
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif
varying vec3 vNormal;
varying vec3 vColor;
varying vec2 vUv;
varying vec3 vPos;
uniform vec3 uLightDir;
uniform float uAmbient;
uniform bool uUseTexture;
uniform sampler2D uTexture;
uniform int uShading;
void main() {
  vec3 n = uShading == 1 ? normalize(cross(dFdx(vPos), dFdy(vPos))) : normalize(vNormal);
  float lit = max(dot(n, uLightDir), 0.0);
  float brightness = uShading == 0 ? 1.0 : uAmbient + (1.0 - uAmbient) * lit;
  vec3 baseColor = uUseTexture ? texture2D(uTexture, vUv).rgb : vColor;
  gl_FragColor = vec4(baseColor * brightness, 1.0);
}
```

- **`#extension ... : enable`**: WebGL 1's fragment shaders don't have
  `dFdx`/`dFdy` unless this extension is turned on, both in the shader
  (this line) and from the OCaml side (`gl##getExtension`, which also
  says whether the GPU has it). GLSL 3.30 has them built in. Nearly
  every GPU has the extension; when one doesn't, we build the shader
  without this line and without `dFdx` (section 8).
- **`precision`**: GLSL ES makes you choose how precise floats are in
  the fragment shader (phones had cheap 16-bit units): `mediump`
  guarantees only about 10 bits of mantissa, 3 decimal digits,
  enough for colors but not for positions; `highp` is 32-bit float,
  optional in WebGL 1 but nearly universal. `#ifdef
  GL_FRAGMENT_PRECISION_HIGH` is the standard way to take it when
  available. Desktop GLSL has no such choice: floats are 32 bits.
- **`uniform sampler2D uTexture`**: not a texture itself but a number,
  the *texture unit* to read from (the OCaml side sets it to 0 and
  binds each material's texture to unit 0 before its draw call).
  `texture2D(uTexture, vUv)` reads it at the interpolated `(u, v)`
  (GLSL 3.30 calls it `texture`), with the filtering the OCaml side
  chose (`smooth_textures`: `LINEAR` or `NEAREST`). `.rgb` is a
  **swizzle**: the first three components as a `vec3`; any
  combination works (`.bgr`, `.xy`, `.rrr`).
- **`n`**: the normal. Interpolated normals aren't unit length anymore
  (a blend of two unit vectors is shorter), hence `normalize`. For
  `Flat`, a trick (section 8).
- **`lit`, `brightness`**: exactly `Lighting.brightness_of_normal`,
  Lambert plus an ambient floor (`graphics/3d/geometry/Lighting.mli`
  explains the formula). `uLightDir` and `uAmbient` come from
  `Lighting.light_dir` and `Lighting.ambient`, so the constants are
  written once, in OCaml, for all backends.
- **`gl_FragColor`**: the output, RGBA in 0..1 (alpha 1: opaque). In
  GLSL 3.30 it's any `out vec4` variable we declare (`FragColor`).
- **The `?:` on uniforms**: `uShading` and `uUseTexture` are the same
  for every pixel of a draw call, so every core takes the same branch;
  branching is only expensive on a GPU when neighboring pixels go
  different ways (they run in lock step).

### One pixel, by hand

`examples3d/Triangle3d` draws one `orange` triangle, `Hex
"#f57900"`, in the `z = 0` plane, facing the camera. Take any pixel
inside it, with the default `Smooth` shading:

```
vColor    = (245, 121, 0) / 255      = (0.961, 0.475, 0.0)
vNormal   = (0, 0, 1)                  (Vec3.face_normal of the triangle,
                                        the same at the 3 corners)
uLightDir = normalize(1, 1.3, 0.6)   = (0.573, 0.744, 0.344)
n         = normalize(vNormal)       = (0, 0, 1)
lit       = max(dot(n, uLightDir), 0) = 0.344
brightness = 0.25 + 0.75 * 0.344     = 0.508
gl_FragColor = vColor * 0.508        = (0.488, 0.241, 0.0)
           * 255                     = (124, 61, 0)
```

and the pixel in a headless Chrome screenshot of the page is exactly
`(124, 61, 0)`: a dark orange, since this face gets only a third of
the "sun", which sits up and to the right.

## 7. Debugging shaders

There is no `printf` on a GPU, and a broken shader doesn't raise:

- **Compile and link errors** are only visible if asked for:
  `compile_shader`/`link_program` check `COMPILE_STATUS`/`LINK_STATUS`
  and `failwith` the driver's log, which names the line, with a
  message whose wording depends on the driver (for instance, using
  GLSL 3.30's `texture` in a GLSL ES 1.00 shader gets something like
  "no matching overloaded function found" on line N). In a
  browser the exception ends up in the console
  (`--enable-logging=stderr` in headless Chrome, see
  `note_headless.md`).
- **Name mismatches** between OCaml and GLSL fail silently:
  `getUniformLocation` of a misspelled name returns null and setting it
  does nothing; `getAttribLocation` returns -1. A uniform declared but
  unused is removed by the compiler and looks the same.
- **Print with colors**: to see a value, output it as the color, e.g.
  `gl_FragColor = vec4(n * 0.5 + 0.5, 1.0);` shows normals (x, y, z in
  -1..1 mapped to red, green, blue), a classic.
- **Compare with the software rasterizer**: same camera, same lighting
  constants, so a static scene should match its golden frame
  (`tests/3d/golden/`); `done/plan_webgl.md` did that with `Corridor3d`.

## 8. The flat-shading trick: `dFdx` and `dFdy`

`Flat` shading wants one normal per *face*, but our vertices carry
per-vertex normals, and a sphere's are smooth (each corner has its
own). The fragment shader could get face normals only if we built
different vertex data for `Flat`. Instead:

```glsl
normalize(cross(dFdx(vPos), dFdy(vPos)))
```

The GPU never runs a fragment shader on one pixel alone: it runs them
in 2x2 blocks, in lock step, so each one can look at its neighbors'
values. `dFdx(v)` is "how much `v` changes from this pixel to the one
on its right", `dFdy(v)` the same upwards:

```
      +-------+-------+
      | vPos  | vPos  |      dFdx(vPos) = vPos(right) - vPos(this)
      |  (up) |       |      dFdy(vPos) = vPos(up)    - vPos(this)
      +-------+-------+
      | vPos  | vPos  |      both vectors lie in the triangle's plane
      | (this)|(right)|      (vPos is the world position, interpolated
      +-------+-------+       linearly across the flat triangle)
```

Two vectors in the plane of the triangle: their cross product is
perpendicular to it, the face normal, pointing towards the camera (x
right, y up, their cross product comes out of the screen), which is
what a visible face's outward normal does. The result is the same for
every pixel of the face, so the face is uniformly lit: flat shading,
with no change to the vertex data. The price: it needs the
derivatives, core in GLSL 3.30 but an extension in WebGL 1; without
it, `webgl/` uses the vertex normals for `Flat` too, i.e. `Smooth`,
which only looks different on curved shapes.

## 9. OpenGL (GLSL 3.30) vs. WebGL (GLSL ES 1.00), side by side

| | `opengl/` | `webgl/` |
|---|---|---|
| first line | `#version 330 core` | none (1.00 is the default) |
| per-vertex input | `layout (location = 0) in vec3 aPos;` | `attribute vec3 aPos;` + `getAttribLocation` |
| vertex -> fragment | `out` / `in` | `varying` (both sides) |
| fragment output | `out vec4 FragColor;` | built-in `gl_FragColor` |
| texture read | `texture(s, uv)` | `texture2D(s, uv)` |
| float precision | always 32-bit | `precision highp/mediump float;` required in the fragment shader |
| `dFdx`/`dFdy` | built in | `OES_standard_derivatives` extension |
| matrix upload | `uniform_matrix4fv ... true` (GL transposes) | must be column-major already: `Mat4.transpose`, `false` |

Everything else, the math, the types, the built-in functions, `main`,
uniforms, is the same language.

## 10. Things to try

Each is a few lines in the fragment shader (and a uniform or two from
OCaml), and a good way to learn the language:

- **Show the normals** as colors (section 7).
- **Fog**: blend towards white with the distance to the camera, e.g.
  `mix(color, vec3(1.0), clamp(length(vPos - uEye) / 30.0, 0.0, 1.0))`
  with a new `uniform vec3 uEye`: what `glEnable(GL_FOG)` did in the
  fixed-function days.
- **A second light**, or a colored one (`notes_3d_shading.md`'s
  roadmap, items 1 and 2): one more `dot`, summed.
- **Specular highlights** (Phong's full model, the shiny spot): needs
  the direction to the camera, `normalize(uEye - vPos)`, and a
  `pow(max(dot(reflect(-uLightDir, n), toEye), 0.0), 32.0)` term.
