OCaml Elm Playground 3D
=======================

*Experimental, work in progress -- not yet linked from the main
[README.md](README.md) or released as opam packages.*

Create simple 3D pictures, animations, and games with OCaml, in the
same "no assets, no boilerplate, just shapes and combinators" spirit as
[`ocaml-elm-playground`](README.md) itself.

`playground3d/` is a 3D counterpart to this project's 2D
`elm_playground`, built on top of it. It's not a wrapper around OpenGL,
Vulkan, or WebGL: the 3D-to-2D projection, the camera math, and (on
native) the entire triangle rasterizer are hand-written OCaml, on
purpose -- you can read every line involved in turning a 3D shape into
pixels on screen. See
[`docs/claude_notes/notes_3d.md`](docs/claude_notes/notes_3d.md) for a
from-scratch tutorial on the 3D concepts involved, and
[`docs/claude_notes/playground3d_plan.md`](docs/claude_notes/playground3d_plan.md)
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
an elm-playground-style API).

Two backends, one API
----------------------

Like the 2D playground, the same application code runs on two
backends:

- **native** (`elm_playground_3d_native`): a real, from-scratch
  software rasterizer -- perspective projection, backface culling, a
  z-buffer depth test, and (see below) 3 more selectable rendering
  strategies, all hand-written, using raw SDL only for the window,
  input, and presenting the final image.
- **web** (`elm_playground_3d_web`): compiles the 3D scene down to
  ordinary 2D `Playground.shape` values every frame (backface-culled
  and depth-sorted) and hands them to the existing, unmodified
  `elm_playground_web` -- so it gets SVG rendering, the event loop, and
  browser timing for free, at the cost of some fidelity (see
  Limitations below).

Try it
------

```bash
dune exec examples3d/Cube3d.exe          # a single spinning cube
dune exec examples3d/Cubes3d.exe         # a grid of overlapping cubes, orbited by the camera
dune exec examples3d/TexturedCube3d.exe  # a cube wrapped with a test texture
dune exec examples3d/InteractiveCube3d.exe  # arrow keys/mouse move a cube around a small scene
dune exec examples3d/PaintersAlgorithmFail3d.exe  # two intersecting boxes; see the "z" toggle below
dune exec games3d/StarCollector3d.exe    # move a box, collect randomly-spawning stars for points
```

While any native example/game is running, a few keys are wired up as
live debug toggles for comparing rendering strategies side by side
(see `notes_3d.md` section 11 for what each one demonstrates and why):

| Key | Toggles |
| --- | --- |
| `m` | Shading: flat color (no lighting) vs. flat shading (one light, per-face) |
| `b` | Backface culling on/off -- a solid's far side faces always point away from the camera, so they can never actually be visible; culling skips drawing them at all (a free performance win). No visible difference in filled mode (the z-buffer already hides them anyway); try `f` (wireframe) first to actually see it do something |
| `f` | Wireframe vs. filled |
| `z` | Painter's algorithm vs. z-buffer -- try this on `PaintersAlgorithmFail3d.exe`, not `Cubes3d.exe` (that one's grid of cubes turns out not to stress it enough to visibly break) |
| `p` | Perspective-correct vs. linear interpolation -- try this on `TexturedCube3d.exe`; `Linear` makes the texture visibly swim/drift as the cube rotates |
| `Q` | Quit |

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

- No lighting beyond a single fixed directional light in `flat_shading`
  mode -- no Gouraud/Phong shading, no shadows, no multiple lights.
- No curved primitives yet (no `sphere`/`cylinder`) -- everything is
  built from flat polygons.
- The web backend can't warp a texture onto an arbitrary projected
  quad, so a textured face renders as a flat gray placeholder there
  (native samples the real texture per pixel).
- No near-plane clipping (a triangle with a vertex behind the camera is
  dropped whole, not clipped into visible sub-triangles).
- No 2D HUD/overlay channel -- a game can't draw score/instructions
  text on top of the 3D scene yet.

See `docs/claude_notes/playground3d_plan.md` and `notes_3d_opti.md` for
more on what's implemented, what's been fixed along the way, and
what's planned next (a `tiny-minecraft`-style voxel game is the
original motivating target).
