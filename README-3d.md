OCaml Elm Playground 3D
=======================

*Experimental, work in progress -- not yet linked from the main
[README.md](README.md) or released as opam packages.*

Create simple 3D pictures, animations, and games with OCaml, in the
same "no assets, no boilerplate, just shapes and combinators" spirit as
[`ocaml-elm-playground`](README.md) itself.

`playground3d/` is a 3D counterpart to this project's 2D
`elm_playground`, built on top of it. Its main backend is not a wrapper
around OpenGL, Vulkan, or WebGL: the 3D-to-2D projection, the camera
math, and the entire triangle rasterizer are hand-written OCaml, on
purpose -- you can read every line involved in turning a 3D shape into
pixels on screen. (Two other backends then hand the same scenes to a
real GPU, with OpenGL and WebGL, for comparison and for speed.) See
[`docs/claude_notes/notes_3d.md`](docs/claude_notes/notes_3d.md) for a
from-scratch tutorial on the 3D concepts involved, and
[`docs/claude_notes/plan_playground3d.md`](docs/claude_notes/plan_playground3d.md)
for the design rationale.

Credit where due: the core API design (world-space shapes, an
eye/target camera, and the trick of compiling a 3D scene down to plain
2D shapes for the web backend) is adapted from Luca Mugnaini's
[elm-playground-3d](https://github.com/lucamug/elm-playground-3d),
itself built on Evan Czaplicki's original
[elm-playground](https://github.com/evancz/elm-playground) (the same
library [`elm_playground`](README.md) ports to OCaml). The
`games3d/StarCollector3d.ml` game's mechanics are adapted from Nate
Abele's [elm-3d-playground](https://github.com/nateabele/elm-3d-playground)
(a different lineage, wrapping `elm-3d-scene`'s real WebGL rendering in
an elm-playground-style API). See
[`docs/claude_notes/notes_playground3d_related_work.md`](docs/claude_notes/notes_playground3d_related_work.md)
for the fuller related-work survey, including VRML, OpenGL, WebGL,
Vulkan, and Unity.

Four backends, one API
-----------------------

Like the 2D playground, the same application code runs on several
backends, two native and two in the browser, each time one computing
the pixels itself and one handing the scene to the GPU:

|         | CPU                         | GPU                                  |
| ------- | --------------------------- | ------------------------------------ |
| native  | **software** (`examples3d/`) | **opengl** (`examples3d/opengl/`)   |
| browser | **web**, SVG (`examples3d/js/`) | **webgl** (`examples3d/webgl/`)  |

- **software** (`elm_playground_3d_software`): a real, from-scratch
  software rasterizer -- perspective projection, backface culling,
  near-plane clipping, a z-buffer depth test, Gouraud/Phong shading,
  texture mapping, and (see below) switchable alternatives to each, all
  hand-written (`graphics/3d/`, one module per idea), using raw SDL only
  for the window, input, and presenting the final image.
- **web** (`elm_playground_3d_web`): compiles the 3D scene down to
  ordinary 2D `Playground.shape` values every frame (backface-culled
  and depth-sorted) and hands them to the existing, unmodified
  `elm_playground_web` -- so it gets SVG rendering, the event loop, and
  browser timing for free, at the cost of some fidelity (see
  Limitations below).
- **opengl** (`elm_playground_3d_opengl`): the same scenes on a real
  GPU, through OpenGL 3.3 and two small shaders: the z-buffer,
  culling, clipping, and the rasterization itself are the hardware's
  (see `docs/claude_notes/notes_opengl.md` and
  `notes_opengl_shaders.md`).
- **webgl** (`elm_playground_3d_webgl`): the OpenGL backend's drawing,
  in the browser, through WebGL 1, reusing `elm_playground_web`'s event
  loop, and its SVG for the HUD, drawn over the WebGL canvas (see
  `docs/claude_notes/done/plan_webgl.md`).

Try it
------

```bash
dune exec examples3d/Cube3d.exe          # a single spinning cube
dune exec examples3d/Cubes3d.exe         # a grid of overlapping cubes, orbited by the camera
dune exec examples3d/TexturedCube3d.exe  # a cube wrapped with a test texture
dune exec examples3d/InteractiveCube3d.exe  # arrow keys/mouse move a cube around a small scene
dune exec examples3d/PaintersAlgorithmFail3d.exe  # two intersecting boxes; see the "z" toggle below
dune exec examples3d/Corridor3d.exe      # walk down a corridor (up/down arrows); see the "c" toggle below
dune exec games3d/StarCollector3d.exe    # move a box, collect randomly-spawning stars for points
```

The same, on the GPU: `dune exec examples3d/opengl/Cubes3d.exe`. In a
browser: `make serve-build`, then e.g.
http://localhost:8001/examples3d/webgl/TexturedCube3d.html (or
`js/` instead of `webgl/` for the SVG backend). The pages must be
served over HTTP, not opened as files, for the WebGL ones' textures to
load (the Makefile's comment above `serve-build` says why).

Run any native example/game with `-debug-keys` (e.g.
`dune exec examples3d/Cubes3d.exe -- -debug-keys`), and a few keys are
wired up as live debug toggles for comparing rendering strategies side
by side (see `notes_3d.md` section 11 for what each one demonstrates
and why). Without the flag they're off, so a game can use any key:

| Key | Toggles |
| --- | --- |
| `m` | Shading: cycles flat color (no lighting), flat shading (one brightness per face), Gouraud (per vertex), Phong (per pixel) -- they differ on curved shapes; try `Spheres3d.exe` |
| `b` | Backface culling on/off -- a solid's far side faces always point away from the camera, so they can never actually be visible; culling skips drawing them at all (a free performance win). No visible difference in filled mode (the z-buffer already hides them anyway); try `f` (wireframe) first to actually see it do something |
| `f` | Wireframe vs. filled |
| `z` | Painter's algorithm vs. z-buffer -- try this on `PaintersAlgorithmFail3d.exe`, not `Cubes3d.exe` (that one's grid of cubes turns out not to stress it enough to visibly break) |
| `p` | Perspective-correct vs. linear interpolation -- try this on `TexturedCube3d.exe`; `Linear` makes the texture visibly swim/drift as the cube rotates |
| `i` | Texture filtering: bilinear (smooth) vs. nearest (sharp texels) -- `TexturedCube3d.exe` again |
| `t` | Fill rule: which triangle gets the pixels on an edge shared by two -- both (an epsilon tolerance), or exactly one (the top-left rule, with sub-pixel precision); see it with the magnifier |
| `c` | Near-plane clipping on/off -- try `Corridor3d.exe`: off, the floor and walls going behind the camera vanish, leaving holes |
| `o` | Optimizations on/off: the original simple code instead of the optimized one (see `graphics/core/Opti.mli`); watch the fps |
| `x` | Pixel magnifier, following the mouse |
| `h` | Help: all these keys and their current state, over the frame |
| `Q` | Quit |

The window title shows every toggle's current state.

The OpenGL backend (`examples3d/opengl/`, `games3d/opengl/`) has `m`
(no lighting, flat, smooth), `b`, `f`, and `i` too, with `-debug-keys`
as well, and `o`: its optimization, keeping the GPU buffers of
`Playground3d.cached3d` shapes from frame to frame (try
`games3d/opengl/Minecraft3d.exe` or `examples3d/opengl/CachedGrid3d.exe`
with `-debug`, which logs the draw calls and vertices uploaded).

The WebGL pages take the same flags as URL parameters, since a page has
no command line: `?debug-keys` for `m`, `b`, `i`, `f` and `o` (their
state in the page's title), `?keys=mb` to press some before the first
frame, and `?fixed-time=1000` to freeze the clock, e.g.
http://localhost:8001/examples3d/webgl/Spheres3d.html?fixed-time=1000&keys=m
(with `make serve-build`). In a 1000x1000 window, such a frame can be
compared with the software rasterizer's golden frame
(`tests/3d/golden/`), made the same way.

Choosing how an app is drawn
----------------------------

The keys change things while an app runs; the app itself chooses the
starting values, portably, with `Playground3d.rendering`:

```ocaml
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = Flat } app
```

- `shading`: `No_lighting`, `Flat` (crisp facets), or `Smooth` (curved
  shapes look round; the default);
- `backface_culling`: `false` to also draw the faces turned away from
  the camera, e.g. for a lone `plane` seen from below;
- `smooth_textures`: `false` for sharp texels (pixel-art textures).

Each backend does what it can: the software rasterizer, OpenGL and
WebGL honor all three (WebGL's `Flat` needs a WebGL extension almost
every browser has; without it, it looks like `Smooth`); the SVG web
backend can only light each face with one color (`Smooth` looks like
`Flat` there) and has no textures. The 2D
playground has the same idea, `Playground.rendering`.

A minimal example
------------------

```ocaml
open Playground
open Playground3d

let view (computer : Playground.computer) () =
  let angle = spin 6. computer.time in
  let scene = cube purple 1.5 |> rotate3d 0. angle 0. in
  let cam = camera ~eye:(3., 2., 5.) ~target:(0., 0., 0.) () in
  (cam, [ scene ])

let update _computer () = ()

let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
```

`cube`/`box`/`plane` are procedurally generated, single-flat-color
shapes -- no assets needed, same philosophy as 2D's `circle`/`square`.
`textured_cube`/`textured_quad` accept a local file path or an http(s)
URL and, on native, sample the image for real, per pixel.

Current limitations
--------------------

This is genuinely experimental and quite young:

- One fixed directional light (flat, Gouraud, or Phong shading) -- no
  shadows, no multiple or colored lights, no specular highlights.
- One curved primitive, `sphere` (no `cylinder`/`cone` yet) --
  everything else is built from flat polygons.
- The SVG web backend can't warp a texture onto an arbitrary projected
  quad, so a textured face renders as a flat gray placeholder there
  (the other backends sample the real texture per pixel).
- No near-plane clipping on the SVG web backend (a triangle with a
  vertex behind the camera is dropped whole, not clipped into visible
  sub-triangles; the software backend clips, see the `c` key, and the
  GPU ones in hardware).
- No transparency on the software and GPU backends (`fade3d` is
  ignored there): blending needs the faces drawn back to front, which
  the z-buffer doesn't give.
- The WebGL backend's textures need the page served over HTTP, and its
  wireframe (lines, WebGL having no polygon mode) is never culled.

The rasterizer's code is in `graphics/3d/`, one module per idea, each
`.mli` explaining its algorithm; `notes_3d.md` section 0 has a reading
order. See `docs/claude_notes/plan_playground3d.md` and `notes_3d_opti.md` for
more on what's implemented, what's been fixed along the way, and
what's planned next (a `tiny-minecraft`-style voxel game is the
original motivating target).
