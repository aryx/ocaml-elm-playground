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
| **callbacks** | inside the widgets | build a tree, register functions on it | Tk (Ousterhout; Tcl 1988, Tk 1991), Motif, Win32's message loop |
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

### Phase 1, DONE (2026-09-20), awaiting review

The immediate-mode toolkit, and a fourth input nobody had noticed was
missing.

**`gui/`** (new private library, package `elm_playground`, pure OCaml,
no dependency on the playground -- principle 1, so `playground/Gui.ml`
is the adapter):

- `Widget` (79 + 51 lines): what a widget is -- a `box` (a rectangle
  placed by its centre, playground coordinates), the hit test
  (`contains`), and `paint`, what a widget draws: **two constructors
  only**, `Fill` and `Text`, since an outline is four fills (`frame`)
  and a tick is one. `text_width` is the 0.6-em approximation the
  repository already used (`Bigbang`, `Physics.draw`), said in the
  `.mli` to be an approximation and why an exact one is not available
  from this side.
- `Theme` (45 + 44): every colour and size in one record, with the
  three faces (`face`, `face_hot`, `face_down`) that are the whole of
  how a button feels alive, and the lineage of the idea (X11
  resources, CSS, Flutter's ThemeData, "design tokens").
- `Immediate` (116 + 163): the engine, **as values** -- a widget takes
  the toolkit's state and gives back a new one plus its answer.
  Decisions taken here, each with its reason in the `.mli`:
  - **a widget's id is its rectangle** (`(box.x, box.y)`), not a hash
    of its label as Dear ImGui does. Two buttons called "OK" are then
    fine and two buttons in the same place are not -- which is a bug
    you can see on the screen, the better kind to have;
  - **the capture is the only thing that survives a frame**: `Free`,
    `Held of id`, `Elsewhere`. That is what makes a press that ends
    outside not a click, a press that began outside not a click
    either, and a slider dragged off itself still yours;
  - **a click shorter than a frame still counts**: a release with
    nothing held, which can only be a press and release between two
    updates (under 1/60 s), fires the widget under the mouse. The
    `Elsewhere` case is what makes that safe.

**`playground/Gui.ml/.mli`** (77 + 76): the Evan-style API of the
plan, unchanged from what was written above -- `button`, `checkbox`,
`slider`, `label` asked for in `update`, `draw ()` in `view`. It holds
**the one piece of mutable state in these libraries** (Dear ImGui's
context, one per program), because `update` and `view` are two
functions and the widgets asked for in one are drawn by the other.
`draw` ends the frame; the next widget call starts the next one, which
is the rule that needs no clock -- `-fixed-time` freezes the clock in
every golden run, so anything keyed on time would have broken exactly
in the tests.

**The fourth missing input: `mouse.mclick` never happened.** Found
while writing the button: nothing has emitted the `MouseClick` message
since the backends were factorized (the web's vdom sent it, before),
so `computer.mouse.mclick` was *always false* -- games that fire on a
click (`TinyMissileCommand`, `TinyLemmings`, `TinyPortal2D`,
`TinyTowerDefense`, ...) only ever worked through their space-key
path, and `tests/games/` hid it by setting `mclick` by hand in its
simulated computer. The fix is three lines in `game_update`, no
backend touched: the button *up* now sets `mdown = false` and
`mclick = true`, and the `Tick` clears it like the other transients --
which is also what the author's own TODO there asked for. A fifth test
in `Unit_input.ml` holds it.

`gui/tests/` (79 + 134, 14 tests): the `.mli`s' worked examples, and
the mouse logic **no golden frame can check** -- a click is a sequence
of frames and a golden frame is one moment.

`examples/GuiWidgets.ml` (80 lines, golden frame): a button, two
sliders and a checkbox tuning a spinning disc, and the "after"
picture to `Typing.ml`'s "before".

Measured: `dune build` clean everywhere including the js targets;
`make test` green (the 2D golden suite gains `GuiWidgets`, everything
else unchanged: no existing frame moved a pixel).

Not done, and deliberately left for the next phase: the game menus
still place their `words` by hand. Converting one is the honest first
customer, but it changes what a game looks like, and there is no
`Inspect` panel yet to be the other one.

### Phase 2, DONE (2026-09-20), awaiting review

`gui/Layout` (134 + 170 lines): constraints down, sizes up, the parent
positions -- Flutter's three rules as two functions, `measure` and
`arrange`, with **a column written as a row turned on its side** (an
`axis`, and `main`/`cross`/`of_axis` to project onto it), so rows and
columns are one piece of code rather than two mirror images.

Nine constructors, and the flex rule that makes them work: measure the
children that know their size, share what is left between the flexible
ones.

- `leaf key (w, h)` -- the key is whatever you want to find it by
  afterwards (`arrange` gives back `('a * box) list`), so a program
  looks the widget up by name and asks for it;
- `row`/`column ~gap`, `space` (a fixed gap), `spacer` (TeX's glue: it
  takes what is left, and two of them centre what is between);
- `expand` (the child takes the leftover *along* the axis, Flutter's
  Expanded) and `stretch` (it fills the *other* axis, Flutter's
  CrossAxisAlignment.stretch -- which is how a panel's buttons come
  out one width);
- `pad`, `center`.

The rule that keeps the two passes on a page, and the one to remember:
**a leaf takes exactly the rectangle its parent gives it.** Its
measured size is what it *asks* for; a row, a column or a `center`
grants it, while `pad` and `expand` hand over what is left.

Named and compared in the `.mli`, which is the teaching: Tk's geometry
managers (`pack`/`grid`, Ousterhout; Tcl 1988, Tk 1991, `grid` 1996),
NeXT/Cocoa's springs and struts (1988), CSS flexbox (2009-2018),
absolute coordinates -- and the ancestor of all of them, **TeX's boxes
and glue** (Knuth, 1978).

**Flutter and not Tk, decided with the author** (2026-09-20), Tk being
both older and more influential, so the question was fair. The answer,
on the author's three criteria -- simplicity, elegance, and keeping an
Evan-like API possible later:

- for a *single row or column* the two are the same algorithm under
  different names (Tk's cavity and parcels are the walk here,
  `-expand` is `expand`, `-fill` is `stretch`, `-anchor center` is
  the centering across the axis, `-padx` is `pad`), so this is not a
  choice between two mechanisms but between two spellings and two
  orders;
- the order is the real difference: Tk fixes a widget's requested size
  *before* knowing the room it gets, so wrapping text there needs
  `-wraplength` by hand or the `<Configure>` trick, while constraints
  going down first let a child answer "given 300 wide, I am 80 tall"
  in the same pass -- the shape of every text layout, and of
  Knuth-Plass in TinyWord later;
- simplicity and elegance: two functions and no manager object, and a
  column written as a row turned on its side rather than twice;
- flexibility: a new combinator is three lines (`stretch` is three),
  and an Evan-facing sugar -- `Gui.column computer ~at:(x, y) [ Reset,
  Gui.button_size "reset"; ... ]`, giving back a box per name with no
  layout vocabulary at all -- is three more over `arrange` the day
  something wants it. Writing it before then would be an unused API.

What Tk has that this does not is `grid`: weights, spans and sticky
edges, aligning columns *across* rows, which rows of rows cannot do.
That is a real gap rather than a spelling, and it comes back when
TinyVisiCalc does (phase 7), which is a grid.

Said plainly in the `.mli`, since it would otherwise be overselling:
the playground's screen is 1000 x 1000 whatever the window and
`Resized` is still a `failwith "Todo"`, so today a layout *arranges*
rather than *resizes*. The same three rules are what will make
resizing work the day the window's size arrives.

`playground/Gui` gained the widgets in a rectangle somebody else
decided -- `button_in`, `checkbox_in`, `slider_in`, `label_in` -- plus
the sizes to build the leaves with (`button_size`, `checkbox_size`,
`slider_size`, `label_size`) and `area computer`, the screen as a box.
The `~at` forms are now one line each on top of them: placing by hand
stays the simple way in, and a layout is what a panel uses.

`gui/tests/Unit_layout.ml` (147 lines, 11 tests): every rectangle
computed by hand, which is the only way to know a layout engine is
right -- a picture of a panel looks plausible whatever the arithmetic
did. (One of them caught the author of the test rather than the code:
the nested row's centre is at -15, not -5.)

`examples/GuiWidgets.ml` is now laid out rather than placed: a column
of stretched widgets on the left, the disc on the right, a `spacer`
between them. Its layout is a **value**, and a pure function of the
room available, so `update` arranges it to ask the widgets and `view`
arranges it again to draw the disc, with no state in between -- the
clearest thing to say about layout in this architecture. `examples/`
links `gui` directly now (as it already links `ai`), since the
layout's vocabulary is the program's.

Measured: `dune build` clean everywhere including the js targets;
`gui/tests` 25 tests green; the 2D golden suite green with
`GuiWidgets` re-approved (it is the only frame that moved, and it is
this phase's own example).

### Phase 3, DONE (2026-09-20), awaiting review

Focus, a text field, and the first four 7GUIs tasks -- plus a second
dead corner of the `computer`, found the same way the first one was.

**The keyboard had never worked for named keys.** `kbackspace`,
`kenter` and `kshift` are in the record from the beginning and
*nothing ever set them* (`update_keyboard` handled the arrows, wasd
and space, and no other name); and the two backends spell a named key
differently -- SDL's, lowercased by `Native_loop_2d`: `"backspace"`,
`"return"`, `"left shift"`; the browser's DOM names: `"Backspace"`,
`"Enter"`, `"Shift"` -- so no program could read one by name on both.
Games that read those fields (`TinySokoban`'s undo on backspace,
`TinyGauntlet2`'s potion on shift) were quietly dead. Fixed in
`Playground.ml` alone, `canonical_key` mapping both spellings onto
one, the browser's (which the arrows already followed); `"space"`
stays `"space"`, since every game reads that one. A seventh
`Unit_input` test holds it, checking both spellings of each key.

**`gui/Focus`** (58 + 41): who has the keys, and the tab order. The
point worth the module: in immediate mode the widgets are *asked for*
in an order, so that order is the tab order -- for free, and visible
in the source -- where a retained toolkit walks its tree and then
needs `tabindex` when the tree is not in reading order. The price is
one frame of memory: when Tab arrives this frame's order does not
exist yet, so the walk uses the previous frame's.

**`Immediate.field`**, the widget that needs everything at once: the
focus, the characters the platform says were typed (phase 0's
`typed`), the keys that produce no character (backspace, the arrows,
Home, End), and a caret -- the one piece of state that cannot live in
the caller's model, so the toolkit keeps it, for the focused field
alone. Decisions, each in the `.mli`:

- **the text stays in the caller's model**, like a slider's value;
- **key edges are computed here**, by comparing with the previous
  frame's held keys, so a key held across frames acts once. No
  auto-repeat: the playground's keyboard is a *set of keys held*, and
  a repeat is an event the platform sends that nothing forwards yet;
- **a character is not a byte**: backspace and the arrows step over a
  whole UTF-8 sequence, or an accented letter would lose half of
  itself;
- **one character to a cell**, like a terminal, rather than at the
  widths the stroke font really draws. That is what keeps the caret
  exactly where the person clicked: a real field asks the font where
  each glyph starts, and we have no font to ask;
- **a caret that does not blink**, so nothing here depends on the
  clock and a golden frame is the same picture every run.

Two more widgets the tasks needed: **`progress`** (a bar that answers
nothing) and **`menu`**, the first *modal* widget -- while its items
show they take the mouse from every other widget, the "grab" every
toolkit does with a popup, without which a click meant for an item
also presses whatever it lands on. Its popup is painted where it is
asked for, so a menu is asked for last; a real toolkit keeps popups
in a layer of their own. And `?enabled` on `button` and `field`: a
greyed button answers false whatever the mouse does, a greyed field
takes neither the keys nor a place in the tab order.

**The first four 7GUIs tasks** (Eugen Kiss, 2014), in immediate mode,
each with a golden frame:

- `Gui7Counter` (57 lines), the baseline: four lines of update;
- `Gui7Temperature` (83): the lesson is that **the model holds the two
  strings, not two numbers** -- "2." and "-" are things a person types
  on the way to a number, and a model that cannot represent them must
  refuse the keystroke or throw it away. Which field was edited is
  whichever came back changed: one comparison each, no message type;
- `Gui7Flight` (112): the rules between widgets are four lines of
  ordinary code, because every widget is asked for every frame and
  being enabled is an argument. This is the task where callbacks start
  to hurt, and phase 4 will measure exactly that;
- `Gui7Timer` (91): the one task this playground answers before it is
  asked -- update runs 60 times a second whether or not anything
  happened, so there is no timer to start, no subscription to cancel,
  no callback firing after its widget is gone. Elapsed time is counted
  in frames, not from the clock, which keeps it deterministic under
  `-fixed-time` (its golden frame is at 90 frames: a second and a half
  into ten, and under the 100 past which `make test` skips a scene).

`gui/tests/Unit_focus.ml` (165 lines, 9 tests): the tab order and its
wrap-round, a click giving the keys and the backdrop taking them away,
typing only where the focus is, backspace over a two-byte character, a
held key acting once, and Tab carrying the typing from one field to
the next.

Measured: `dune build` clean everywhere including the js targets;
`gui/tests` 34 green, `playground/tests` 56, the 2D golden suite 119
with the four new frames and **no existing frame moved a pixel**.

### Phase 4, DONE (2026-09-21), awaiting review

The centrepiece: the same program written four ways, running at once,
and a test that they agree.

First, the thing that had to exist for the comparison to mean
anything: **`gui/Look`** (56 + 128), how each widget is *drawn*, and
nothing about who keeps its state. Every function takes what to draw
plus the state to draw it in (`~hot`, `~held`, `~checked`, `~caret`)
and answers paint. `Immediate` was rewritten over it and lost a third
of its body; the other three were written against it from the start.
Without this, comparing four architectures would have been comparing
four piles of drawing code. **`gui/Text`** (50 + 65) came out the same
way: UTF-8 characters, cells, and `edit`, one frame of typing at a
caret -- because what a field does with Backspace is not an
architectural question, and all four must do it identically.

The three new wirings, each a module whose `.mli` is its argument:

- **`Retained`** (64 + 117): widgets as objects that own their state,
  with functions hung on them -- Tk, Motif, Win32, Swing. Its `.mli`
  says the cost plainly: *the truth is scattered*;
- **`Mvc`** (61 + 29): Reenskaug's model with observers (Xerox PARC,
  December 1979). Twenty-nine lines, of which the interesting one is
  that `change` tells *everybody*, in the order they signed up;
- **`Mvu`** (73 + 124): the textbook Elm loop, with a message type --
  which the playground's own `game` does *not* have, and that is the
  reason `playground/Gui` is immediate mode and this module exists
  separately.

**`examples/GuiFourWays.ml`** (174 lines): 7GUIs' Counter four times,
side by side, on one screen, sharing one `Look` and one
`Widget.input`. Measured in that file, code lines only: **callbacks
12, MVC 11, MVU 12, immediate 3** -- and the length is explicitly not
the point, since at this size they are all short. The number that
matters is *how many places hold the count*: two with callbacks, one
in the other three.

**`gui/tests/Unit_architectures.ml`** (173 lines, 4 tests) is the
verification section's demand, met: the same clicks into all four
produce the same count *and the same paint, frame for frame*. Plus
the failure mode of callbacks as a test rather than an opinion (bump
the ref, forget the label, and the screen says 0 while the program
believes 1), and MVC's cost as a number (one change, every view
woken).

Four things came out of writing them that reading about them had not
given, and they are in `notes_gui.md` section 4:

1. the four *can* be made to paint identically, and enforcing it is
   what stops the comparison from drifting into a comparison of
   drawing code;
2. the one place they disagreed was **timing**, and it was not
   architectural: a retained toolkit paints after its callbacks ran,
   so a click shows in the same frame, while in immediate mode
   whether it does is the order you ask the widgets in -- one line,
   visible, and yours;
3. **MVU only matches if `step` views the model after folding the
   messages**, which is what Elm does; discovering that from a failing
   test is what made `Mvu.step` run the whole loop rather than half
   of it;
4. **MVU cannot hold the caret.** The view is rebuilt every frame, so
   the focus and the caret live underneath it (`Mvu.t`, ten lines).
   In Elm that underneath is the browser, which is also why a virtual
   DOM needs keys and React needs refs.

Measured: `dune build` clean everywhere including the js targets;
`gui/tests` 38 green, `playground/tests` 56, the 2D golden suite 122
with `GuiFourWays` added and no existing frame moved a pixel.

### Phase 5, DONE (2026-09-21), awaiting review

**`gui/Text_edit`** (133 + 194): the piece table -- the original text,
never touched, an append buffer, only ever added to, and a list of
pieces saying what to read from where -- with a caret, a selection,
undo, redo and greedy word wrap.

Three things worth writing down, all of them found by writing it:

- **the append buffer is mutable and shared by every version**, and
  that is safe for exactly one reason: it is only ever appended to, so
  a piece an old version wrote down still says what it said. That
  one-way rule is what lets everything above it be a value while
  nothing is ever copied;
- **merging matters more than it looks**: without it every keystroke
  is a piece and a typed paragraph is a thousand of them. Extending
  the last piece when the new text lands exactly where it ends is four
  lines, and it is the difference between a structure and a linked
  list of characters (5000 random edits leave a few dozen pieces);
- **undo needed no code**: it is "put the old list back". No inverse
  operations, no journal of what was deleted, no copying the
  document -- which is the property phase 6 (`appkits/document`) is
  going to be built on, and the one that lets `Inspect` scrub an
  application as it scrubs a game.

**`gui/tests/Unit_text_edit.ml`** (227 lines, 11 tests): the `.mli`'s
worked example piece by piece, the wrap (including a word longer than
the line, and the breaks the text asks for itself), undo and redo at
their ends -- and the one that matters, **5000 random inserts,
deletes, selections, undos and redos checked at every step against a
plain string that copies itself and keeps every version**. A clever
structure is only worth having if it is indistinguishable from the
slow one, and "indistinguishable" is a claim about inputs nobody
thought of. It caught nothing in the table; it caught two wrong
expectations of mine (the wrap's, and a piece count), which is the
usual ratio and exactly why the naive implementation is written out.

**`Immediate.text_area`** (+69 lines) and **`Look.text_area`** (+72):
the widget that holds one. The difference from a `field` is not the
number of lines, it is where the text lives -- a field's is a string
in the caller's model with the caret kept by the toolkit, while a text
area's is a `Text_edit.t` that carries its own caret, selection and
history. So undo belongs to the text, not to the toolkit. It adds
lines (Enter), wrap, a selection you can drag or extend with shift,
up and down between lines, Control-Z and Control-Y.

**`examples/GuiEditor.ml`** (110 lines, golden frame): a text to type
in, with the structure's numbers on the screen -- pieces, versions
back, versions forward -- which is a better argument for a piece table
than a paragraph about one. Its whole model is a `Text_edit.t`.

One trap found while laying it out, now in `Layout.mli`: **a `spacer`
inside something you are centring makes the whole thing fill the
screen**, since a spacer takes whatever room it is offered and
`center` offers all of it. What was wanted there was `space`.

Measured: `gui/tests` 49 green, `playground/tests` 56, the 2D golden
suite 123 with `GuiEditor` added and no existing frame moved a pixel.
(`dune build` at the repository root currently stops in
`games3d/*/TinyMarioKart64.ml`, on an unbound `Track3d` -- the
author's own work in progress, and nothing this phase touched;
`dune build gui/ playground/ examples/` is clean.)

### Phase 6, DONE (2026-09-21), awaiting review

**`appkits/`** exists (the author's call, 2026-09-21: the plan's own
layout, a directory beside `kits/`, since `kits/` is what games of a
genre share and `appkits/` is what applications share).
`appkits/document/` is a private library `appkit_document` in package
`elm_playground`, depending on nothing at all -- a document is a
value, and these are the rules about values:

- **`Document`** (59 + 39): content, path, and the version last saved
  kept beside the current one, so "is there anything to save" is a
  comparison rather than a flag somebody has to remember to set. The
  catch, found by using it: the pointer comparison is exact and free
  when going back means *the old value itself*, but a structure that
  rebuilds a version rather than keeping it (`Text_edit.undo` rebuilds
  its record) hands back something equal and not identical -- hence
  `?equal`, and the star in the title goes out when you undo back to
  what you saved, which most editors get wrong;
- **`Undo`** (68 + 46): any value, both ways, with a name per edit
  (a menu says "Undo Add Circle") and a limit, since an editor's
  memory has to stop somewhere. Its `.mli` compares the three undos
  this repository now has -- `kits/puzzle/Undo` (a game's: one way, no
  names), `gui/Text_edit`'s (a text's own, over its pieces), and this
  one -- and says plainly what the command pattern is for and where
  its bugs live (in the inverse of an edit, which here does not exist);
- **`Clipboard`** (33 + 18): cut, copy and paste in this program,
  with the history (Tesler and Mott's Gypsy, 1974-75) and the honest
  line about the system clipboard being a backend's business.

**`examples/Gui7Circles.ml`** (195 lines), 7GUIs task 6: the task
exists to ask *what is one edit*, and the answer -- keep the dialog's
live value aside and record once when it closes -- is three lines.
Scripted, the golden frame proves it: a slider dragged over ten frames
and the history says **back 3** (two circles and one adjustment), not
thirty.

**`examples/GuiEditor.ml`** gained the other two: `Document` (the
title says `notes.txt *`, Save is greyed when there is nothing to
save) and `Clipboard` (Control-C, X, V). `Immediate.text_area` learned
to ignore `typed` while Control is held -- natively Control-C produces
no character, but a browser's keydown still carries one, so the guard
belongs in the toolkit.

**The golden frames can press things now.** `-script` grew the mouse
(`Input_script`: `at(x;y):frames`, `click`, `rclick`; playground
coordinates, and a semicolon inside the parentheses because commas
separate entries), fed to `Native_loop_2d` beside the keys. Three new
scenes use it, and they are the first frames in this repository that
show a widget being *used* rather than sitting there:

- `GuiWidgets dragging`: the slider grabbed and dragged, the knob
  held, the disc the size the drag made it (radius 115, from 80);
- `Gui7Circles drawn`: three circles put down by clicking, the one
  under the pointer lit;
- `Gui7Circles adjusted`: the right click, the dialog, the drag, and
  "back 3".

Two of those scripts aimed at the wrong pixels first (the slider had
moved when phase 2 laid the panel out, and a click fell in the margin
that keeps a circle whole) -- which is worth knowing about scripted
goldens: they are exact, and they will need re-aiming whenever a
layout changes.

Measured: `appkits/tests` 9 green, `gui/tests` 49, `playground/tests`
58 (two new for the script's mouse), the 2D golden suite 127 with five
frames added or re-approved and no other frame moved a pixel.

Noted, not fixed: `Undo` now exists in two `wrapped false` libraries
of this package (`kit_puzzle` and `appkit_document`). Nothing links
both, so it builds; an application that used a puzzle kit would
collide, and renaming one is a `git mv` away.

### Phase 7a, DONE (2026-09-21), awaiting review

The spreadsheet's engine, Tk's grid, and 7GUIs' last task. Phase 7 was
split: the engine is where the teaching is, and TinyVisiCalc's 1979
interface (7b) and TinyExcel's 1985 one (phase 8) are two front ends
over exactly this.

**`appkits/sheet/`** (a second library beside `appkits/document/`,
depending on nothing):

- **`Formula`** (75 + 205): what you can type into a cell, and a
  recursive-descent parser -- four grammar rules, one function each,
  with precedence coming from the *shape* of the grammar rather than
  a table (a sum is made of products, so `term` sits below `expr`).
  Cell names are base 26 with no zero, which is why the column after
  Z is AA;
- **`Sheet`** (85 + 256): the idea a spreadsheet actually is. A cell's
  formula names other cells, so the sheet is a **graph**; a change
  walks *forwards* through it and recomputes what depends on it in a
  **topological order** (Kahn, 1962), and what is left when nothing
  can be taken is exactly a **cycle**, which is how `A1 = B1+1, B1 =
  A1+1` is caught and said rather than looped on. `recalculated`
  puts the number on the screen, because "changing one cell of a
  thousand recomputed 3" is the difference between a spreadsheet and
  a demonstration. The `.mli` says what VisiCalc itself did (row or
  column order, your choice, hence pressing recalculate twice) and
  that Lotus 1-2-3 brought natural order in 1983.

Shaped for **`appkits/embed/`** at the author's request, before it
exists: `Sheet.to_string`/`of_string` (a component must be able to
write itself down), and the sheet's *drawing* is one function taking a
rectangle -- which is the component protocol's other half, a thing
that draws into whatever rectangle it is given.

**`gui/Grid`** (79 + 136): Tk's geometry manager, over the same box
math as `Layout`. Rows and columns that line up across rows, which a
column of rows cannot do -- and that is the whole reason it exists,
so its `.mli` is the two-row form whose labels are of different widths
and whose fields still start in the same place. Tk's vocabulary kept
(`-row`, `-column`, `-rowspan`, `-columnspan`, `-sticky "nsew"`,
`-weight`), with a spanning cell growing the *last* column of its
span as Tk does, and two deviations stated: a grid with no weights
sits in the middle of its room rather than the top-left, and there is
no `-padx`/`-uniform`/`-minsize`.

**`examples/Gui7Cells.ml`** (213 lines), 7GUIs task 7 and the hardest
of the seven: a working spreadsheet -- click a cell, type in the bar,
press Enter, and everything downstream follows. Numbers against the
right edge and text against the left, VisiCalc's rule. The form around
the sheet is a `Grid`, for the reason grids exist.

Tests: `appkits/tests` 21 (12 new: the parser's precedence, ranges,
the graph, what a change reaches, cycles and recovering from them,
errors spreading, saving and loading), `gui/tests` 54 (5 new, every
grid rectangle computed by hand). Two golden frames, one of them
scripted (a click moving the cursor and the bar following it).

### Phase 7b, DONE (2026-09-21), awaiting review

**`apps/`** exists (the author's call, 2026-09-21: the plan's layout,
a directory beside `games/` and `examples/`, laid out the same way
with `software/` for the golden frames).

**`apps/TinyVisiCalc.ml`** (292 lines): 1979, on a character display.
What it uses is `appkits/sheet` and the playground's shapes; what it
uses **nothing** of is `gui/` -- no widget, no layout, no focus, no
mouse -- and that is the subject rather than minimalism. VisiCalc ran
on a 40-column display with nothing to point with, and every decision
in it follows: the cursor is the interface, the three lines at the top
are what a character display has room for, and everything a modern
spreadsheet puts in a toolbar was a letter after a slash.

Three faithful details worth the lines:

- **the formulas are spelled 1979's way**, `+B3*2` and
  `@SUM(B4...B6)`, translated to the engine's `=` and `:` in six
  lines each way -- a good reminder that a formula language is a
  surface, not a semantics;
- **the slash commands**: `/B` blanks, `/C` clears, `/G` is global --
  and `/G` is what the program is for;
- **`>>>>>>>>>`** when a number does not fit its nine characters.

And the thing worth running it for, which needed one addition to the
engine (`store`, typing into a cell *without* recalculating, and
`recalculate Rows|Columns`, one pass in order): **`/G` then R, C or N
switches between VisiCalc's recalculation order and the natural one**
Lotus 1-2-3 brought in 1983. The opening sheet has a forward
reference on purpose -- A1 reads B3, below it -- so in row order it is
a pass behind until you press `!`, which is the habit of 1979 made
visible. That is the repository's third principle (the simple version
stays beside the better one, switchable) applied to an *idea* rather
than an algorithm, and `Unit_sheet` has it as a test.

One design consequence, found by the test that broke: a formula that
does not parse is now `Formula.Invalid`, a thing a cell *holds*,
rather than a failure of `content_of`. A spreadsheet must keep what
was typed so it can be corrected, so "it does not parse" is a kind of
content and not a reason to refuse it.

Measured: `appkits/tests` 22 green, the 2D golden suite 133 with two
frames added -- one of them scripted, the arrows walking the cursor to
B3 with the status line following it in 1979's spelling. (The slash
commands take *characters*, which a script cannot send: a key is not a
character, as phase 0 established.)

### Phase 8, DONE (2026-09-21), awaiting review

**`apps/TinyExcel.ml`** (255 lines), and the pair is complete. What is
*shared* first, because it is the point: `appkits/sheet`, the whole
engine -- formula language, dependency graph, recalculation. Not one
line of it differs between 1979 and 1985. The table of what does
differ is in both headers.

What 1985 bought, all of it visible in one scripted golden frame: a
**range dragged out with the mouse** (D2 to D5, shaded, its headers
lit, named `D2:D5` in the bar), a **menu bar** you can read before you
choose, **Edit > Fill Down**, and a chart.

The one real algorithm is **a formula that moves** (`Formula.shift`,
new, with `Formula.to_string` to write it back): filling `=B2*C2` down
gives `=B3*C3`, `=B4*C4`, `=B5*C5`, the totals follow, and the sum at
the bottom goes to 1236. That is what made spreadsheets useful -- one
formula written once, for a table of any height -- and it is exactly
where **$A$1** comes from, which this engine does not have and whose
absence is stated where it matters (`Formula.mli`).

**`appkits/sheet_view/`** (new library, `gui` + `appkit_sheet`): a
sheet drawn into a rectangle, with a selection and a way back from a
click to a cell. It exists because three programs need it -- TinyExcel
now, `examples/Gui7Cells` (moved onto it in this phase, its golden
frames gaining lit headers), and `appkits/embed`'s component next --
and because "draw yourself into this rectangle" is the shape that
protocol asks for. A **selection is two cells**, an anchor and a
focus, and one cell is a selection of one: the same shape as a caret
being a selection of length zero.

And one thing the scripted frame caught that no unit test would have:
**a menu's grab protects widgets, not drawings.** The first run had a
click on a menu item fall through and select a cell underneath,
because the sheet reads `computer.mouse` itself rather than being a
widget. `Immediate.modal`/`Gui.modal` now answer "has the toolkit got
the mouse", which is the one line a drawn surface needs to behave
like a widget -- and the widgets have always known it for themselves.

Measured: `appkits/tests` 24 green (two new: a formula printed back
out, and a formula that moves), `gui/tests` 54, `playground/tests` 58,
the 2D golden suite 135 -- `TinyExcel` and `TinyExcel filled` added,
the two `Gui7Cells` frames re-approved for the lit headers.

**The bug the author found by using it** (2026-09-21, after the
commit): a cell could not be edited through the bar. The update ended
by refreshing the bar from the selected cell -- meant to catch what a
menu command had changed, and run on *every* frame -- so each
keystroke was put back by the next frame before it could be seen. The
fix is to refresh on the events that should (the selection moved, a
command ran) and never unconditionally; it is the immediate-mode twin
of the scattered-truth bug `gui/Retained.mli` is about, and it is now
in `guide-code-style.md`'s traps. None of the scripted goldens had
typed into the bar, which is why none caught it: `TinyExcel edited`
does now (C2 selected, the bar clicked, one backspace turning 120
into 12, Enter, and D2 following to 54) -- scriptable because
backspace is a key and not a character. The 2D golden suite is 136.

### Phase 9a, DONE (2026-09-21), awaiting review: the typesetting example

(First written as `apps/TinyWord`, and moved to
`examples/TypesetParagraph` the same day, the author's call: *"for me
Word is a text editor with multiple fonts, bold, strike, layout"*. And
there is a factual reason on top of that one: **Word has never used
Knuth-Plass** -- it breaks its lines greedily, as browsers do; the
optimal breaker is TeX's, and Adobe InDesign's "paragraph composer".
What was built is a small TeX, not a small Word, and naming it TinyWord
taught something false about Word. The real TinyWord is phase 9b,
below.)

**`appkits/typeset/Linebreak`** (a third library beside `document` and
`sheet`, depending on nothing): where to break a paragraph into lines,
the greedy way and Knuth and Plass's (1981), side by side. Their model
simplified -- boxes and glue, a line's ratio, badness 100|r|^3,
demerits (10 + badness)^2, the last line free -- and the optimal one
as the dynamic programme `best(j) = min over i of best(i) + demerits(i
.. j-1)`, O(n^2) as written, with TeX's active nodes named as what
makes it close to linear.

Its worked example is checked by hand in the `.mli`: "aaa bb cc ddddd
ee ff gggg" at a measure of 10, where greedy leaves one space
stretched to double (656,706 over the paragraph) and the optimal
breaker takes a word up a line and shrinks two spaces instead
(12,706) -- fifty times better. And the law, on 300 paragraphs nobody
wrote: the optimal breaker is never worse than greedy, since greedy's
breaks are among its choices, and it sets every word once, in order.

**`examples/TypesetParagraph.ml`**, shaped the way TeX is: the text as typed on the
left (the text area, over phase 5's piece table), the page set on the
right -- justified, with each line's ratio in the margin and the lines
stretched past comfort marked. A dropdown switches the breaker, a
slider moves the measure, and the bottom line gives both scores for
the page (8 times better on the opening one). TeX's paragraphs (a
blank line ends one), one style (a `# ` heading), and a monospaced
face on purpose -- every width exact, so the right edge is straight
and the only thing that differs between the two breakers is the
breaking.

**A real bug in `Layout`, found by this example**: `stretch` inside
`expand` was ignored, because the two flags each looked only at the
outermost constructor -- so the page pane, which wants the leftover
width *and* the full height, floated at its natural height. They look
through each other now, with a test for both orders; no existing
golden frame moved.

What it deliberately does not do, in its header: editing *in* the
typeset page (a caret inside justified text is the hard part of
WYSIWYG, and why Bravo, 1974, is a landmark), hyphenation (Liang's
patterns, which give the breaker more places to break), pages, and any
style but the heading.

Measured: `appkits/tests` 29 green (5 new), `gui/tests` 55 (1 new),
`playground/tests` 58, the 2D golden suite 138 -- `TinyWord` and
`TypesetParagraph greedy` added, the second switched through the
dropdown by a scripted click and showing three rivers where
Knuth-Plass had one.

### Phases 9b-9d, planned: TinyBravo, then TinyWord

The author's idea (2026-09-21): **a TinyBravo before TinyWord, as
TinyVisiCalc came before TinyExcel** -- and it is the actual lineage,
not only a parallel. Bravo (Butler Lampson and Charles Simonyi, Xerox
PARC, 1974), on the Alto, was the first WYSIWYG editor, and it is where
the **piece table** came from: `gui/Text_edit` already is Bravo's
structure. Simonyi then went to Microsoft and wrote Word with it. So
one engine, two interfaces a decade apart, and this time the lesson
is **modes**: Bravo was modal (letters were commands until you entered
insert mode -- type "edit" in command mode, and **e** selects
everything, **d** deletes it, **i** starts inserting, and a **t** is
all that is left), Tesler and Mott's Gypsy (1975) made it modeless,
and Word inherited that.

- **9b, the shared engine, `appkits/richtext`**: a text plus *style
  runs* over it (Bravo's "looks") -- how an insert extends a run or
  splits one, how a selection across three runs is restyled, the
  "typing style" a bold with nothing selected leaves on the caret --
  and a WYSIWYG layout: glyphs positioned, the caret to a point and a
  click back to an offset. Undo over the whole styled text is
  `appkits/document/Undo`, since it is a value. **The glyph widths are
  a parameter** (as `Linebreak`'s word widths are): `graphics/font` is
  a private library of the software package and cannot be a
  dependency here -- which is the right push anyway: the apps supply
  Hershey's real metrics, the tests simple ones checkable by hand.
- **9c, TinyBravo (1974)**: modal commands, the mouse to select, looks
  by keyboard, and the "edit" trap reproducible.
- **9d, TinyWord (1985)**: modeless, a toolbar or menu for bold,
  italic, underline, strike, sizes, and alignment -- justified
  reusing `appkits/typeset`, greedy by default since that is what Word
  does.

Both draw their text from Hershey's own strokes as thin rectangles,
so no backend changes: bold a thicker pen (Hershey's duplex and
triplex faces are literally that), italic a shear of the stroke
coordinates, strike and underline a rule. The cost in shapes per
frame is to be measured.

### Phase 9b, DONE (2026-09-21), awaiting review: the engine

**`appkits/richtext/`** (a fourth library in `appkits/`, depending on
`gui` for the piece table):

- **`Style`**: a look -- bold, italic, underline, strike, size -- as a
  value with no identity, so that two characters are in the same style
  exactly when their looks are equal;
- **`Rich`** (the text and its looks): the piece table's characters
  beside a second table of **runs**, and every edit the same surgery
  on both -- split at a position, keep what is either side, merge
  neighbours that have come to look alike. Its two rules are the ones
  every word processor has and gets wrong somewhere: what you type
  looks like what is before it (over a selection, like its first
  character), and a look set with nothing selected is the *typing
  style*, pending on the caret and forgotten when it moves. Tested
  against 3000 random edits compared with a naive model storing one
  look per character, first time;
- **`Page`** (the WYSIWYG layout): greedy word by word, as Bravo and
  Word did, lines as tall as their tallest look -- and both ways
  between the text and the page, `caret_at` and `offset_at`, with a
  test that they agree at *every* place in a text. **The glyph widths
  are the caller's**, which is what keeps it a library every backend
  can use. It also carries the alignment TinyWord needs (left, centre,
  right, justified -- greedy lines stretched, the last line of a
  paragraph left alone), committed with the engine; TinyBravo sets
  everything left, as Bravo did.

`Undo` gained `amend`: what changes the state without being an edit
-- the selection moving, the second letter of a word being typed --
which is how an insertion becomes one undoable edit however long it
is. And `-script` gained **`type(text):n`**, characters rather than
keys, which is what finally makes modal commands and typing testable
in a golden frame.

### Phase 9c, DONE (2026-09-21), awaiting review: TinyBravo

**`apps/TinyBravo.ml`**: 1974, on a sheet of paper standing up (the
Alto's portrait screen), the text in its looks where it would print.
**Modal**, as Bravo was: `i` insert, `a` append, `d` delete, `e`
everything, `l` then a letter for looks, `u` undo -- and the mouse to
select. What it uses is the engine above and **`apps/Stroke_text`**,
which draws a look *with the pen* from Hershey's own strokes (bold a
thicker pen, italic the points sheared, underline and strike two
rules) -- no backend touched, and what is laid out is exactly what is
drawn, since the same glyph data gives the widths and the strokes.
Fast enough: five frames and start-up in 0.3 s.

The golden frames are the program's story: the page as it would print;
**the "edit" trap**, typed in command mode -- e selects everything, d
deletes it, i starts inserting, and a t is all that is left; the same
**undone** by `uu`, one undo per command; and a selection dragged with
the mouse, underlined and struck from the keyboard.

One mistake of mine worth keeping: the opening's looks were first put
on offsets counted by hand, and landed on "uld p" and "lic". They are
found in the string now, and the comment says why.

Measured: `appkits/tests` 44 green (15 new: the runs, the typing
rules, the page both ways, `amend`), `playground/tests` 59 (typing in
a script), the 2D golden suite 143 with the four TinyBravo frames.

Still to come: **9d, TinyWord** -- the same engine, modeless, as
Tesler's Gypsy made it and Word inherited.

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
