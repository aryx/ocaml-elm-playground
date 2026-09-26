# Plan: apps/cad/, drawings that are more than pictures

A new category of apps, `apps/cad/`: the programs where a drawing is
a *model* -- geometry that knows its own rules -- rather than a
picture (`apps/graphics/`: TinyMacPaint's dots, TinyMacDraw's
objects). Laid out like the other categories: the programs, `software/`
for the golden frames, `web/` for the browser; what they share in
appkits.

## Why this order

Sketchpad (Ivan Sutherland, MIT, 1963) is the root of the whole line,
and of more: the first interactive graphics program; the first
constraints, solved by relaxation; the first masters and instances
(an ancestor of objects, and of every CAD's blocks and symbols); the
first zoom onto a sheet bigger than the screen. Its 3D sequel,
Sketchpad III (Timothy Johnson, 1963), gave the four views every 3D
modeller still has. AutoCAD (Autodesk, 1982) is the same idea on a
PC, for draftsmen: commands typed, snaps, layers, dimensions, a file
format everybody reads (DXF). Blender and POV-Ray, in `apps/graphics/`,
come after (the author's other idea: plan_raytracing_remaining.md).

## Phase 1: TinySketchpad (Sketchpad, 1963)

- `appkits/sketch` (`appkit_sketch`, pure OCaml): `Sketch`, a sheet of
  points, lines and circles sharing their points (Sutherland's ring
  structure: move a point and every line on it follows), constraints,
  and instances of other sheets, placed, scaled and turned; `Relax`,
  Sutherland's relaxation: each variable in turn moved to where the
  squares of the errors of its constraints, linearized by numerical
  derivatives, are least -- Gauss-Seidel, as the physics' solver.
  Unit tests: the worked examples (a rough hexagon becomes regular; a
  linkage keeps its lengths; an instance cycle refused).
- `apps/cad/TinySketchpad.ml`: the TX-2's scope, lines as dots; the
  light pen as the mouse, with its *aiming* (the pen locks onto a
  point, then a line: a line drawn to a line has its end on it); the
  push buttons (DRAW, CIRCLE, MOVE, DELETE, FIX, the constraints,
  INSTANCE); the knobs (the zoom, an instance's size and turn); four
  sheets, A the honeycomb of B's hexagon, C the linkage, D blank.
- Left out, exercises: arcs; constraints on instances' attachment
  points; the one-pass method Sutherland tried before relaxation
  (ordering the constraints so each is solved once); numbers
  (lengths, distances), the bridge's forces; copying constraint
  types themselves as pictures.

## Phase 2: TinyAutoCAD (AutoCAD, 1982)

- The command line as the main input, the mouse as the other
  (`LINE`, `CIRCLE`, `ARC`, `TRIM`, `EXTEND`, `OFFSET`, `FILLET`,
  `MOVE`, `COPY`, `ZOOM`, `UNDO`), prompts asking for points, typed
  (`100,50`, `@30<45`) or picked; object snaps (endpoint, midpoint,
  intersection, center, perpendicular), ortho; layers with colours;
  blocks (Sketchpad's instances again); linear dimensions.
- DXF read and written (its ENTITIES section: LINE, CIRCLE, ARC,
  INSERT), a text format; an appkit `appkits/cad` for the geometry the
  commands share (intersections, offsets, trims).

## Later

A parametric solid modeller (CATIA, 1977; Pro/ENGINEER, 1988: the
history of features, re-run when a dimension changes), Sketchpad III's
four views in 3D, then TinyBlender.
