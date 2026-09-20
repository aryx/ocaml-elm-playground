# Plan: GUI, from scratch, for teaching (`gui/`, `appkits/`, `apps/`)

## Context

This project teaches how pictures are computed (`graphics/`), how
things move (`physics/`), how sound is made (`audio/`), how a computer
decides (`ai/`) and how a running program can be seen
(`plan_inspect_teaching.md`). The missing subject is the oldest one in
interactive software: **how a program is structured around a person
using it** -- buttons and sliders, layout, focus, selection, text
editing, undo, documents, and the four rival answers to "where does
the state live".

**On the obvious objection, first**, since it is the right one to
raise: is this still the elm playground? Yes, and arguably it is a
return rather than a departure. **Elm was a GUI language before it was
anything else** -- `elm-html`, the virtual DOM, and the
Model-View-Update loop came from building user interfaces -- and
elm-playground is Evan's *simplification of an app framework* so that
beginners can draw. This repository has kept the app framework and
used it for pictures and games; `gui/` uses it for the thing it was
invented for, and puts the playground's own architecture beside its
three rivals so a reader can see why MVU looks the way it does.

There is a practical argument too: **three planned things already need
widgets and would each hand-roll them.**
[`plan_inspect_teaching.md`](plan_inspect_teaching.md)'s timeline is a
scrubber and its tweakables are sliders; `games/TinySoldat.ml`,
`TinyCameltry.ml` and `TinySlingshot.ml` all list a level or map
editor in their exercises; and every game's menu is currently a
`Scene2d` scene with hand-placed `words`. A button written four times
in four games is the usual sign of a missing layer.

What the playground gives us already, and what it does not (checked in
the code, and the shape of the Groundwork section below):

- `computer.mouse` has position, `mdown`, `mclick`, `mrdown` and a
  delta -- **no wheel, no double-click**;
- `computer.keyboard` has the arrows, wasd, space, enter, shift,
  backspace and a set of key names -- **no typed text at all**, no key
  repeat. Nothing in this repository has ever needed to type a
  sentence;
- text is drawn with **Hershey's vector font** (`graphics/font`,
  Roman simplex, 1967), so every glyph is strokes we draw ourselves;
- `kits/puzzle/Undo` exists, for games going back one move on a grid;
- `Scene2d` switches whole screens, which is as close to a window
  manager as the repo gets.

Companions: [`notes_gui.md`](../tutorials/notes_gui.md), the tutorial
(written ahead of the code, as its specification), and
[`notes_gui_related_work.md`](../related-work/notes_gui_related_work.md)
(Smalltalk, Tk, Motif, Qt, Flutter, React, Dear ImGui, Plan 9's
libpanel and acme, OLE and OpenDoc, and the ceiling here).

## Principles

The eight of [`../README.md`](../README.md), with four of this area's
own:

- **We draw everything.** No native widgets, ever: a button is shapes,
  like everything else here. That is Flutter's position rather than
  Tk's or Qt's, and it is what keeps all five backends equal and every
  widget golden-frameable.
- **One app, four architectures.** The comparison *is* the teaching
  artifact: the same small program written with callbacks, with MVC,
  with MVU and in immediate mode, in one directory, all runnable. This
  is principle 3 (the simple version stays beside the better one,
  switchable) applied to architectures instead of algorithms.
- **Documents are values.** An app's document is immutable, so undo is
  a list, redo is the other half of it, and `Inspect`'s time travel
  works on an app exactly as on a game. That single decision is what
  makes the apps small.
- **An app is Tiny, and says what it is not.** `games3d/TinyQuake.ml`
  is 622 lines and teaches the whole Quake pipeline; an app gets the
  same budget and the same honesty in its header -- one real algorithm
  each, and a list of what was deliberately left out.

## The Playground API, Evan-style

Tentative (`playground/Gui.mli`), to be settled by writing `Inspect`'s
panel and one app with it.

The paradigm chosen for the *playground-facing* API is **immediate
mode**, and the reason is not fashion: `Playground.game`'s update is

```ocaml
val game : (computer -> 'memory -> shape list) ->
           (computer -> 'memory -> 'memory) -> 'memory -> ...
```

-- there is **no message type**. In immediate mode there does not need
to be one: a widget is a function of the `computer` that answers a
question about this frame.

```ocaml
(* in update: ask, and act *)
val button   : computer -> at:(number * number) -> string -> bool
val slider   : computer -> at:(number * number) -> from:number -> to_:number ->
               number -> number
val checkbox : computer -> at:(number * number) -> string -> bool -> bool
val field    : computer -> at:(number * number) -> string -> string  (* typed text *)
val menu     : computer -> at:(number * number) -> string list -> int option

(* in view: the same widgets, drawn *)
val draw : ui -> shape list
```

so that a settings screen is what it says:

```ocaml
let update computer model =
  if Gui.button computer ~at:(0., 120.) "Reset" then initial
  else { model with
         gravity = Gui.slider computer ~at:(0., 60.) ~from:0. ~to_:2000. model.gravity;
         sound   = Gui.checkbox computer ~at:(0., 0.) "sound" model.sound }
```

That is Dear ImGui's model in the playground's own idiom, it needs no
new concept, and `Inspect`'s sliders and timeline become one line
each. The three other architectures are *not* hidden: they live in
`gui/` as the comparison (below), built on the same drawing and
hit-testing code, so only the wiring differs.

## The four architectures, and the harness

The centrepiece of the teaching, and the thing this repository is
unusually well placed to do, since it already *is* one of the four:

| | where the state lives | what you write | who |
|---|---|---|---|
| **callbacks** | inside the widgets | build a tree, register functions on it | Tk (Ousterhout, 1988), Motif, Win32's message loop |
| **MVC** | a model, observed by views | model, view, controller, and the notifications between them | Smalltalk-80 (Trygve Reenskaug, 1979) |
| **MVU** | one model, rebuilt each message | `update : msg -> model -> model`, `view : model -> ui` | Elm (2012) -- this playground |
| **immediate mode** | in your own variables; no widget objects at all | `if button "OK" then ...`, every frame | Casey Muratori (2005), Dear ImGui (Omar Cornut, 2014) |

The harness is **7GUIs** (Eugen Kiss, 2014; names and date to check):
seven tasks chosen to expose exactly where each architecture hurts --
Counter, Temperature Converter, Flight Booker (validation and
dependency), Timer (state plus time), CRUD (lists and selection),
Circle Drawer (**undo and a dialog**), and Cells (**a mini
spreadsheet: a dependency graph and recalculation**). Writing the
first four of them four ways is a bounded, citable exercise; Circle
Drawer is where callbacks start to hurt and MVU shines; Cells is
TinyVisiCalc's own engine in miniature, which is why the apps follow
naturally.

## Layout: the part courses skip

One module, `gui/Layout`, and one idea, from Flutter (2017):
**constraints go down, sizes come up, the parent decides position.**

```
        parent
          |  "you may be 0..400 wide, 0..200 tall"   (constraints down)
          v
        child
          |  "then I am 180 x 40"                    (sizes up)
          v
        parent places it at (10, 10)                 (parent positions)
```

Three lines of interface (`measure`, `arrange`, `place`), rows,
columns, spacers and alignment on top, and the two classic
alternatives named and compared: Tk's **geometry managers** (`pack`
and `grid` -- a separate object that owns placement, Ousterhout's
idea) and CSS **flexbox**, which is the same constraint idea with
twenty years of vocabulary on it.

## Target layout

```
gui/                      (private, package elm_playground; pure OCaml
                           so every backend, the web one included)
  Widget                  what a widget is: a rectangle, a drawing, a
                          hit test, and its state
  Layout                  constraints down, sizes up (above)
  Focus                   who gets the keys; tab order; capture
  Theme                   colors, sizes, the one place they live
  Text_edit               a piece table, a cursor, a selection, undo
  Immediate               the immediate-mode toolkit (Gui's engine)
  Retained                the same widgets, as a tree with callbacks
  Mvc                     the same widgets, as model/view/controller
                          (Mvu is the playground itself: no module)
gui/tests/                hit tests, layout by hand, the piece table
playground/Gui.ml         the Evan-style API above, over Immediate
appkits/document/         (appkit_document) what every app shares:
  Document                a document as a value: content, dirty, path
  Undo                    the command pattern over values (kits/puzzle
                          /Undo is its game-shaped cousin)
  Clipboard               cut, copy, paste, in-process
appkits/embed/            (appkit_embed) compound documents, below
  Component               render into a rectangle, take events when
                          activated, report a natural size, serialize
apps/                     TinyVisiCalc, TinyExcel, TinyWord, ...
apps/js/                  the same, for the web, as games/js does
examples/Gui7*.ml         the 7GUIs tasks, four ways
```

`appkits/` sits beside `kits/` deliberately, and the name says the
relation: `kits/` is what games of a genre share, `appkits/` is what
*apps* share. If `apps/` grows past a handful, a catalog plan splits
off the way [`plan_games.md`](plan_games.md) did for games.

## appkits/embed: compound documents, the Bean/OLE idea

The part of this plan that teaches something nobody teaches any more,
and the author's own ask: **a spreadsheet inside a document**, edited
where it sits.

```
   +--- TinyWord document ------------------------+
   | The quarterly figures are below.             |
   |                                              |
   |   +-- an embedded TinyExcel sheet --------+   |   click it once: it
   |   |  A    B      C                        |   |   draws itself
   |   |1 Jan  100    =B1*2                    |   |   click it again: it
   |   |2 Feb  120    =B2*2                    |   |   is *activated*, and
   |   +---------------------------------------+   |   takes the keys
   |                                              |
   | ... and the chart shows the trend.           |
   +----------------------------------------------+
```

The whole idea is one interface -- four functions -- and that is
exactly why it is worth teaching in OCaml, where it is a record of
closures or a first-class module rather than a COM interface and a
registry:

```ocaml
type component = {
  size : unit -> number * number;                 (* what I'd like to be *)
  draw : number * number -> Playground.shape list;(* into that rectangle *)
  event : computer -> component;                  (* when I am activated *)
  save : unit -> string;                          (* and a loader per kind *)
}
```

`TinyWord` then knows nothing about spreadsheets: it lays out a
component like a very large character. **In-place activation** (the
host's menus and keys handed over to the embedded thing, OLE 2's
famous feature, 1993) is one `bool` in the host's model. The
history -- Andrew Toolkit (CMU, 1988), OLE 1 and 2, OpenDoc
(Apple/IBM, 1992-97), Bonobo, KParts, JavaBeans -- is in the
related-work note, along with the observation that compound documents
*lost* as an application architecture and *won* as the web's: a
notebook cell and a Notion block are the same idea with a different
owner.

## The apps

Each one Tiny, each carrying one real algorithm, each with a header
saying what it deliberately does not do:

- **TinyVisiCalc and TinyExcel, a pair** (as TinyDoom and TinyDoom3d
  are): *the same spreadsheet engine, two interfaces, six years
  apart*. VisiCalc (Dan Bricklin and Bob Frankston, 1979) is keyboard
  only, a character grid, formulas typed on a command line -- the
  program that sold the Apple II. Excel (Microsoft, 1985, on the Mac
  first) is the same thing with a mouse, selection, menus and fonts.
  Running both over one engine is the clearest possible statement of
  what a GUI *is*, and it costs one engine and two small front ends.
  The engine is the lesson: **cells as a dependency graph,
  topological order, recalculation of only what changed** -- plus a
  formula parser, which is where
  [`plan_teaching_languages.md`](plan_teaching_languages.md) meets
  this plan.
- **TinyWord**: **Knuth and Plass's line-breaking algorithm** (1981 --
  paragraphs broken optimally by dynamic programming, the reason TeX's
  paragraphs look the way they do) over a **piece table** (the
  structure Word itself used), with selection, undo and styles. Text
  layout is the algorithm; the rest is the toolkit's.
- **TinyMacPaint** (Bill Atkinson, 1984): flood fill, brushes,
  marching ants, a selection you can drag -- and it exercises
  `graphics/2d` the way the games exercise the rasterizer.
- **TinyHyperCard** (Bill Atkinson, 1987), a stretch and the most
  interesting one if it happens: cards, fields, buttons, and a tiny
  scripting language -- a *way of programming*, like `Puzzlescript`
  and `Bigbang` already are here.
- **TinyPowerPoint: deliberately not, and here is why.** Strip the
  transitions and it is a list of slides with a text box on each,
  which `Scene2d` nearly is already; it would be the one app whose
  interesting content is entirely the toolkit's. If it ever happens,
  it happens as a *component host* (slides made of embedded
  components) -- i.e. as an `appkits/embed` demo, not as an app.

## Groundwork decisions

### The `computer` needs typed text (the one non-negotiable addition)

Everything else here is optional; this is not. A field that cannot be
typed into is not a field. The addition is one record field --
`typed : string`, the characters entered this frame -- fed from SDL's
`TEXTINPUT` event natively and from the browser's `keypress`/`input`
on the web, which is also the only way to get shifted punctuation and
non-US layouts right. Key *repeat* comes with it. Then, for apps:
the **wheel** (`mwheel`), **double-click** (`mdouble`), and a
**clipboard** (in-process first; the system clipboard is a backend
detail, SDL has one, the browser's needs a permission).

These are additions to `Playground.computer`, so they touch every
backend and are the plan's phase 0 -- and they are worth having
anyway: no game has ever been able to ask a player's name.

### Drawn widgets, and what that costs

No native widgets means no native text input, no IME, no system
menus, no accessibility tree, and text rendered as Hershey strokes.
Said plainly up front, as the guide asks: this is a **teaching**
toolkit; it can spell "hello" in a field and cannot be a Japanese
word processor. Flutter draws everything too, which is why it is the
right comparison, but Flutter ships a text stack of its own.

### Immutable documents, and the payoff

A document is a value; an edit returns a new one. Undo is then a list
of documents (or of commands, which is the same thing with sharing),
redo is its mirror, "dirty" is a pointer comparison, and -- the part
worth the decision -- `Inspect`'s recording replays an *app* exactly
as it replays a game. A word processor you can scrub backwards is a
good advertisement for the whole architecture.

## Phasing

0. **The `computer`'s gaps**: `typed`, key repeat, `mwheel`,
   `mdouble`, clipboard, on every backend, with tests.
1. **The immediate-mode toolkit**: `Widget`, `Theme`, `Immediate`,
   `playground/Gui.mli` (button, slider, checkbox, label). First
   users: `Inspect`'s panel sliders and timeline, and a game menu
   replacing hand-placed `words`.
2. **Layout**: `Layout` (constraints down, sizes up), rows, columns,
   alignment; the Tk and flexbox comparison in the `.mli`.
3. **Focus, keys, and fields**: `Focus`, tab order, `field` over
   phase 0's `typed`; the first 7GUIs tasks (Counter, Temperature,
   Flight Booker, Timer) in immediate mode.
4. **The four architectures**: `Retained` and `Mvc` beside
   `Immediate`, MVU being the playground itself; the same four 7GUIs
   tasks written each way, and the line counts and the failure modes
   written down. `examples/Gui7*.ml`.
5. **Text editing**: `Text_edit` -- a piece table, cursor, selection,
   word wrap, undo -- with property tests against a naive string
   implementation.
6. **Documents**: `appkits/document` (Document, Undo, Clipboard), and
   Circle Drawer (7GUIs' undo task) on it.
7. **TinyVisiCalc**: the engine (cells, dependency graph, topological
   recalculation, a formula parser) and the 1979 keyboard interface.
8. **TinyExcel**: the 1985 interface over the same engine -- mouse
   selection, menus, fonts, a chart if it is cheap. The pair's
   comparison written in both headers.
9. **TinyWord**: Knuth-Plass over the piece table, styles, undo.
10. **appkits/embed**: `Component`, in-place activation, and the demo
    -- a TinyExcel sheet inside a TinyWord document, and a
    TinyMacPaint drawing inside both.
11. **TinyMacPaint**, then *(stretch)* **TinyHyperCard**.
12. **Docs**: `notes_gui.md` checked against the code, numbers filled
    in; the related-work postscript; and a paragraph in the
    repository's README saying what the project has become.

## Status

**Not started** (2026-09-20). Written as the specification, with
[`notes_gui.md`](../tutorials/notes_gui.md) beside it. Decisions taken,
with their reasons:

- **the author asked for it** (2026-09-20), wanting the Tiny spirit
  applied to applications -- TinyExcel and TinyWord by name -- and an
  `appkits/` of shared document components "Bean/OLE style";
- **one repository, not two** (the author raised the drift himself):
  the apps share the backends, `graphics/`, the golden frames, the web
  build and the docs discipline, and splitting later is a `git mv`
  while joining is a merge. The honest fix for the drift is phase
  12's README paragraph;
- **immediate mode for `playground/Gui.mli`**, because `game`'s
  update has no message type; the other three architectures are the
  comparison, not the API;
- **7GUIs as the harness**, so the comparison is citable rather than
  ad hoc;
- **TinyVisiCalc and TinyExcel as a pair over one engine**, which is
  the GUI lesson itself;
- **TinyPowerPoint deliberately demoted** (see The apps).

### Phase 0, DONE (2026-09-20), awaiting review

The `computer` gained the three inputs an application needs, as
additive fields -- nothing renamed, nothing's meaning changed, so
every existing game behaves exactly as before:

- `keyboard.typed : string`, the characters a key press produced
  (`""` most frames). The `.mli` says why it is not `keys`: a key name
  is not a character, and only the platform knows what shift, a dead
  key or a non-US layout made.
- `mouse.mwheel : number`, notches since the last frame, positive
  scrolling up.
- `mouse.mdouble : bool`, whether this frame carried a double click --
  the first click of the pair still arrives as an ordinary click, so
  ignoring it is exactly the old behaviour.

All three are **transients**, like `mdx`/`mdy`: accumulated by events,
seen by one `update`, cleared by the `Tick` that follows. That is the
property `playground/tests/Unit_input.ml` (new) checks in four tests,
because a backend can break it without any golden frame noticing.

Plumbed through `Sub` (`ETyped`, `EMouseWheel`, `EMouseDouble`, with
their `on_*` subscriptions) and filled by every backend:

- **native 2D and 3D** (`Native_loop_2d.ml`, `Native_loop_3d.ml`):
  SDL's `text_input` event (which needs `Sdl.start_text_input ()` --
  off by default, so without it there are no characters at all),
  `mouse_wheel` (with `mouse_wheel_flipped` undone, so "natural"
  scrolling does not invert the meaning), and `mouse_button_clicks >= 2`
  for the double click, which SDL counts for us;
- **web** (`playground/web/Playground_platform.ml`, which the two 3D
  web backends delegate to): `wheel` with its `deltaMode` normalised
  to notches (a notch is about 100 pixels or 3 lines), `dblclick`, and
  -- for the characters -- `keydown`'s `key` when it is one byte or
  starts a non-ASCII sequence, since the browser puts the character
  there and an ASCII word ("Shift", "ArrowUp") otherwise. No IME, as
  Out of scope says.

`examples/Typing.ml` (new, with a golden frame) shows the three, and
is the "before" picture for the widgets: it draws its field and its
boxes by hand, which is precisely what phase 1 removes.

Measured: `dune build` clean everywhere including the js targets; the
golden suites unchanged (the same eight frames drift here as before,
the arm64-Linux difference `Testutil_golden.mli` documents).

## Verification

- `make test`: `gui/tests/` (hit testing, layout by hand-computed
  rectangles, the piece table against a naive string, the
  spreadsheet's topological order and its cycle detection), and every
  `.mli`'s worked example.
- **The four architectures must agree**: the same scripted input into
  the four versions of a 7GUIs task produces the same frames. That is
  the test that keeps the comparison honest.
- Golden frames for every widget, every 7GUIs task and each app's
  first screen; scripted runs (`-script`) for typing and clicking.
- **Replay**: once `Inspect` exists, each app's recorded run replays
  to the same document -- the check that documents really are values.
- The numbers, measured: lines per architecture for the same task,
  and each app against its original's ambitions (honestly: a page).

## Out of scope

- Native widgets, accessibility, IME, right-to-left and complex text
  shaping. A real font stack (we have Hershey strokes).
- Real file formats (`.xlsx`, `.docx`): apps save their own small
  text format, and say so.
- Printing, page setup, and anything about paper.
- Collaborative editing (CRDTs, OT) -- a good later plan of its own,
  next to [`plan_networking_teaching.md`](plan_networking_teaching.md).
- A window manager: `Scene2d` switches screens, apps are full-window,
  and overlapping windows are a *later* (Plan 9's rio is the model if
  it happens).
- Charts beyond one bar chart in TinyExcel, if it is cheap.
