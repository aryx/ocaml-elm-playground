# Plan: what's left for the GUI toolkit and the apps

The GUI plan is done: see
[`done/plan_gui_teaching.md`](done/plan_gui_teaching.md) (`gui/`, the
four architectures, `Gui`, `appkits/`, and the eight apps of
`apps/`, phases 0-12) and its tutorial,
[`notes_gui.md`](../tutorials/notes_gui.md), whose §16 has the numbers.
What's left, roughly from most to least worth doing. Anything that
changes pixels ends with new golden frames, approved after looking at
them (`make approve-golden2d`). Saving documents, and the apps on the
web, have their own plans: [`plan_io.md`](plan_io.md) and
[`plan_caps.md`](plan_caps.md) -- saving is done (2026-09-22: every
app but TinyBravo, a File menu shared in `File_menu`, the store
native and web); import by dropping a file is what is left of it.

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

**Circle Drawer four ways** too (2026-09-22): `examples/gui4/Gui4Circles`,
in `GuiFourWays` and in `Unit_gui4` (the whole drag of the dialog's
slider undone as one step), with what it needed in every toolkit --
a canvas, a context menu, the right button, `Retained.set_shown` --
and the sixth disagreement it found, the slider's value a last bit
apart on arm64 (`notes_gui.md` §4).

What is left of it:

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
- **a part flowing in a text** -- done the FrameMaker way (2026-09-21):
  `TinyFrameMaker.ml` anchors the same parts in a text that flows
  over pages (`appkits/richtext/Flow`), each set below its line and
  moving with it. The Word way, a part inline as a very large
  character inside a line, is still open (`Page` would need a glyph
  that is a box);
- **resizing -- done** (2026-09-21): a selected part's bottom handle
  gives it a height, the gap between two parts of a row shares the
  row out, everything reflowing as the mouse moves (`Compound`'s
  `Sized`, heights negotiated as in OpenDoc: never less than the part
  needs); widths are not negotiated, so a part of fixed size spills
  out of a share too narrow for it -- unless it is **scaled** (done
  too): `Component.natural`, `draw_in` and `input_in` draw a part with
  a size of its own scaled to its room and map the mouse back,
  TinyOpenDoc's "Scale to Fit" per part, TinyFrameMaker's frames
  always;
- **linking**, OLE's "L": a part that shows a file kept elsewhere and
  follows it when it changes (after `plan_io.md`);
- a fourth kind of part added with one registry line and nothing else
  changed -- **done** (2026-09-21): `Part_drawing`, TinyMacDraw's
  engine as a part, in TinyOpenDoc's registry and Insert menu and in
  TinyPowerPoint's; it saves with Marshal (`appkits/document/Saved`).
  A fifth (a chart of a sheet's column) would be the same.

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

## 8. The office programs not yet there

Compared with Microsoft Office and LibreOffice (2026-09-21), the suite
had Word, Excel, PowerPoint, a paint program and compound documents,
and missed six kinds of program; by what each would teach:

- **Vector drawing -- done**: `TinyMacDraw.ml` (MacDraw, 1984)
  over `appkits/draw` (`Figure`, `Drawing`): objects rather than dots,
  a hollow shape hit only on its outline, the order as the depth,
  groups, resizing as an affine map passed down a group. TinyMacPaint's
  pair.
- **The database**, the biggest gap (Access, Base): TinydBASE (dBASE
  II, 1980: a dot prompt, `USE`, `LIST FOR`, `INDEX ON`) and then
  TinyFileMaker or TinyAccess (forms over the same engine, 1985/1992)
  -- the VisiCalc/Excel pair again. An engine with real algorithms:
  records, a small query language (select, project, join), an index as
  a B-tree, reports. HyperCard's card-as-record is its hint.
- **Equations** (LibreOffice Math): a formula language parsed and laid
  out as nested boxes by TeX's rules (Knuth, 1978), next to
  `appkits/typeset` -- and a real part for TinyOpenDoc, whose
  "equation" part is today a placeholder.
- **Desktop publishing -- the long-document half done**:
  `TinyFrameMaker.ml` (FrameMaker, around 1986): one text flowing
  through the columns of pages made from a master page, and parts
  anchored in it, over `appkits/richtext/Flow`; its header compares it
  with TinyOpenDoc (more powerful: parts in the text's flow, many
  pages, one master; more restricted: one container, the text). The
  free-form half, TinyPageMaker (Aldus, 1985: frames placed by hand on
  each page, the text threaded through them), and FrameMaker's
  paragraph catalog, are still open.
- **Project planning** (Project): TinyMacProject (1984), tasks and
  dependencies, a Gantt chart, the critical path as the longest path
  through a graph -- the spreadsheet's topological order again.
- **Mail and calendar** (Outlook): TinyEudora (1988), a mailbox,
  threads by their References (Jamie Zawinski's algorithm), and
  calendar recurrence; with
  [`plan_networking_teaching.md`](plan_networking_teaching.md), since
  mail wants a network.

History to be checked before building on any of these, as for the
others.

## 8b. TinyOffice, the suite as it is today -- done, and what's next

`TinyOffice.ml` (2026-09-21), separate from TinyOpenDoc and
TinyFrameMaker so that they keep their history (its header says what
each of them cannot do): a start screen of five kinds, every kind a
host of the others, objects floating anywhere (dragged, resized,
scaled, front and back), text running round them (`Page.layout
~around`, tested), in-place editing with OLE 2's menu merging. Then (2026-09-21,
`notes_gui.md` §15b): **wrap on both sides** (`Page.layout ~both`,
tested); **several pages**, the text laid out once over them all with
the margins between pages as boxes it goes round, scrolled; **move
with text**, an object tied to a paragraph and placed in two layouts;
and **a chart linked to a sheet** (`Part_chart`), made again from
the sheet's cells whenever it is drawn. Next, by what each would give:

- **saving**, every kind with its objects, their anchors and links,
  through `Saved` (`plan_io.md`);
- done too (2026-09-22): the presentation's **show**, **headers and
  footers** with {page} and {pages} fields, and wrapping chosen **per
  object** (wider side, both, top and bottom, in front);
- text wrapped to a drawing's **outline** ("Tight") rather than its
  box, and a first page without its header;
- a chart of a **range dragged over**, rather than of columns A and B,
  and a link to a sheet in another file (after `plan_io.md`).

## 9. Small things found on the way

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
