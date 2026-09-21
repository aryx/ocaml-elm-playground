# Plan: what's left for the GUI toolkit and the apps

The GUI plan is done: see
[`done/plan_gui_teaching.md`](done/plan_gui_teaching.md) (`gui/`, the
four architectures, `playground/Gui`, `appkits/`, and the eight apps of
`apps/`, phases 0-12) and its tutorial,
[`notes_gui.md`](../tutorials/notes_gui.md), whose §16 has the numbers.
What's left, roughly from most to least worth doing. Anything that
changes pixels ends with new golden frames, approved after looking at
them (`make approve-golden2d`). Saving documents, and the apps on the
web, have their own plans: [`plan_io.md`](plan_io.md) and
[`plan_caps.md`](plan_caps.md).

## 1. 7GUIs: the rest of the comparison

Done (2026-09-21): **CRUD** in immediate mode (`examples/Gui7Crud.ml`,
with the list box it needed: `Look.list`, `Immediate.list`,
`Gui.list_in`), and **Temperature, Flight Booker and Timer four ways**
beside the counter (`examples/gui4/`, a module per task), run side by
side in `examples/GuiFourWays.ml` and checked frame by frame by
`examples/gui4/tests/Unit_gui4.ml`. Retained and MVU gained a slider, a
progress bar and a menu (MVU also disabled buttons and fields) to do
it; the test found and fixed five disagreements (`notes_gui.md` §4),
and §4 now has the table of lines per architecture per task.

What is left of it:

- **Circle Drawer four ways**: where callbacks start to hurt (the
  dialog's live value, and one undo for a whole drag) -- the
  comparison's best argument, and the biggest of the four-way tasks.
- **CRUD four ways**: the retained list holds a *row*, and the program
  has to translate it back to a person after every filter -- which
  needs a list widget in `Retained` and `Mvu`.
- **Cells four ways** is probably not worth it: its interest is the
  engine (`appkits/sheet`), which is the same in all four.

## 2. The first customers outside `apps/`

The practical argument the plan started from: three things needed
widgets and would each have hand-rolled them. None uses `Gui` yet.

- **A game's menu** as widgets instead of hand-placed `words` in a
  `Scene2d` scene -- the honest first customer, left out on purpose
  because it changes what a game looks like (one game, looked at,
  then the others if it is better).
- **`Inspect`'s panels** ([`plan_inspect_teaching.md`](plan_inspect_teaching.md)):
  its timeline is a scrubber and its tweakables are sliders -- and,
  once it exists, the plan's unmet verification item: **replay** each
  app's recorded run to the same document, the check that documents
  really are values.
- **A level editor** for one of the games whose exercises ask for one
  (`TinySoldat`, `TinyCameltry`, `TinySlingshot`).

## 3. Resizing

`Playground.game`'s `Resized` is still `failwith "Todo"`, so a layout
today only *arranges* (a column that spaces itself, buttons of one
width) inside a fixed 1000x1000. Delivering the window's size -- the
`screen` in `computer` changing -- is what makes `gui/Layout`'s
constraints earn their keep; then the apps lay out their chrome
(menu bars, palettes) with `Layout` rather than with coordinates.

## 4. HyperCard: the message box

TinyHyperCard's first exercise, and the quickest way to learn
HyperTalk: a field at the bottom of the screen whose Enter runs its
line as a one-handler script (`on messageBox <the line> end
messageBox`, sent to the current card). Small: `Hypertalk.parse` and
`send` already do everything. Then "find" (search every card's fields)
and a second background.

## 5. Compound documents: the parts that were left out

From TinyOpenDoc's header:

- **A container part**: a `Compound` column as a `Component.part`
  holding parts, so that a text can hold a sheet that holds a picture,
  as OpenDoc's could -- the idea's real generality, and today the
  rows and columns are the document's own;
- **a part flowing in a text** like a very large character, which is
  what Word does with an embedded sheet (`Page` would need a glyph that
  is a box);
- **linking**, OLE's "L": a part that shows a file kept elsewhere and
  follows it when it changes (after `plan_io.md`);
- a fourth kind of part (a chart of a sheet's column) added with one
  registry line and nothing else changed -- the whole point, shown.

## 6. TinyHarvardGraphics, before TinyPowerPoint

The pair the presentation programs lack, as TinyVisiCalc is to
TinyExcel: a keyboard, form-filled program for text charts and bar
charts (Harvard Graphics, Software Publishing, DOS, 1986, from memory),
in front of TinyPowerPoint's mouse and WYSIWYG. **Check its history
first** -- what exactly it did and when -- before building on it.

## 7. Pictures as images, not rectangles

TinyMacPaint, TinyHyperCard and the picture part draw a bitmap as its
runs of black dots (`Bitmap.rectangles`): about 3,200 rectangles for
MacPaint's opening picture, ~20 ms a frame natively, and grey is the
worst case. A **bitmap shape** in the Playground (pixels in, one image
drawn) makes it one draw call -- a change to every backend and to the
public API, so a decision for the author, and the natural time for it
is when pictures get bigger (MacPaint's full page, FatBits).

## 8. Small things found on the way

- **Two `Undo` modules**: `gamekits/puzzle/Undo` and
  `appkits/document/Undo`, both in unwrapped libraries, so nothing can
  link both; a rename of one (the game's is the smaller) or wrapping.
- **Scaled text**: the playground's `words`, scaled down (TinyPowerPoint's
  thumbnails of a slide holding a sheet), keeps a minimum pen width and
  comes out heavy -- the software renderer's, worth a look next to
  [`plan_2d_remaining.md`](plan_2d_remaining.md)'s text item.
- **Group transparency** is not in the software backend (a `fade` on a
  `group` is ignored), which is why TinyPowerPoint's show pushes rather
  than dissolves; also a `plan_2d_remaining.md` matter.
- **TinyPowerPoint's parts** stay with their slide's *number* when
  slides are inserted before them in the outline; a marker line in the
  outline would tie a part to its slide (its header's exercise).

## Later, and out of scope

Unchanged from the plan: native widgets, accessibility, IME, complex
text shaping; real file formats (`.xlsx`, `.docx`); printing; a window
manager (Plan 9's rio the model, if it happens); and collaborative
editing (CRDTs, OT), a good plan of its own next to
[`plan_networking_teaching.md`](plan_networking_teaching.md).
