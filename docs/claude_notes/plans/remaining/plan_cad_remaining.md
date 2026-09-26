# Plan: what's left for the CAD applications

The plan is done: see [`done/plan_cad.md`](done/plan_cad.md) -- the
category `apps/cad/`, TinySketchpad (Sketchpad, 1963) over
`appkits/sketch` (`Sketch`, points shared by lines and circles,
constraints, instances of other sheets; `Relax`, Sutherland's
relaxation), and TinyAutoCAD (AutoCAD Release 12, 1992) over
`appkits/cad` (`Cad_geom`, the crossings of lines and circles;
`Cad_drawing`, entities on layers and blocks; `Cad_edit`, OFFSET,
TRIM, EXTEND, FILLET; `Cad_snap`; `Dxf`; `Cad_session`, the command
line as a machine, tested without a screen). TinyBlender, the 3D
modeller, is `apps/graphics/`'s (plan_raytracing_remaining.md).

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. A parametric modeller (Pro/ENGINEER, 1988)

The idea CAD had after AutoCAD, and still the heart of SolidWorks,
CATIA V5, Fusion 360 and FreeCAD: a part is not its geometry but its
*history*, a list of features, each made from numbers, re-run when a
number changes.

- **The sketch**: TinySketchpad's constraints with numbers -- a line
  of length 40, a circle of radius 8, a distance -- which `Relax`
  solves as it solves the others (an error function each: nothing
  more). Fully constrained, under-constrained (a point that can still
  move, shown in another colour) and over-constrained (the error that
  stays, shown) are the three states every CAD sketcher shows.
- **The features**: a sketch extruded (a profile of lines and arcs
  becomes a prism) or revolved; a hole; a fillet. The solids are
  `appkits/modeler`'s and the ray tracer's CSG: an extruded rectangle
  is a scaled box, a hole a cylinder subtracted -- no B-rep, no mesh.
  A general profile needs a prism of any polygon: a new `Solid` in the
  ray tracer (a polygon's inside test on each slab), its first real
  extension.
- **The history**: the feature tree on the left; change the 40 to 60
  and every feature is made again, in order -- the part is a function
  of its numbers. What breaks when a feature refers to an edge that is
  gone (the "topological naming problem") is the lesson, shown rather
  than solved.
- `apps/cad/TinyProEngineer.ml` (or TinySolidWorks, 1995, the one on a
  PC), its views TinyBlender's quad view.
- Worked example: a plate 100 x 60 x 10 with a hole of 20 at its
  center; the plate's width set to 80, the hole stays at the center
  because its position was a constraint, not a coordinate.

## 2. TinyAutoCAD's missing half

- **TEXT** (and DTEXT), in Hershey's strokes (`graphics/font`):
  AutoCAD's own SHX fonts were strokes too; DXF's TEXT entity read and
  written, the dimensions' numbers made of it.
- **POLYLINE**: connected segments as one entity, with widths and
  arcs (the *bulge*, the tangent of a quarter of the arc's angle);
  PEDIT; and `Dxf`'s LWPOLYLINE read with its bulges, which it drops
  today.
- **Linetypes**: the center lines dashed as a draftsman draws them
  (CENTER, HIDDEN, DASHED), a pattern in drawing units scaled by
  LTSCALE; DXF's LTYPE table.
- **The other ways to a circle and an arc**: 2P, 3P, and TTR --
  tangent to two objects with a radius, a nice little problem in
  `Cad_geom` (the offsets' crossings) -- ARC by start, center, end.
- **Grips** (Release 13, 1994): the selection's own points shown as
  squares, dragged to stretch, move, rotate -- noun then verb, the
  other way round from the command line.
- **HATCH**: a boundary filled with a pattern of lines, clipped by
  its crossings with the boundary (`Cad_geom` again).
- Small: OFFSET Through; TRIM's Undo option; ZOOM by the wheel; the
  Release 12 behaviour of TRIM with no edge (nothing), beside AutoCAD
  2000's (everything) which is the one kept.

## 3. TinySketchpad's exercises

- **Attachers**: an instance's points given to the drawing it is in,
  so that instances can be wired together -- Sutherland's circuit
  diagrams, the resistors and transistors of one master each.
- **Arcs**, as Sketchpad's circles were (a center and two ends on its
  rim).
- **The one-pass method**, which Sutherland tried before relaxation:
  order the constraints so that each is solved once, from what is
  known; and the drawings it cannot order (a cycle), where relaxation
  is needed.
- **Constraint types as pictures**: Sketchpad defined a new
  constraint by drawing it, a master of its own; here a new constraint
  is an error function in `Relax`, which is the exercise's first half.

## 4. Checks not yet made

- The three web versions (TinySketchpad, TinyAutoCAD in `apps/cad/web/`,
  TinyBlender in `apps/graphics/web/`) are built but were never tried
  in a browser: the knobs' drags, the typing, the DXF export as a
  download.
- A DXF written by TinyAutoCAD opened in LibreCAD or Inkscape, and one
  of theirs opened here (Dxf's tolerance of what it does not know is
  claimed, not tried on a real file).
- A golden frame of TinyAutoCAD's FILLET and OFFSET (they are unit
  tested through `Cad_session`, not drawn).
