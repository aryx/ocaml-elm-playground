# gui/ vs. the rest of the GUI world

Where a small teaching toolkit and a handful of Tiny applications sit
among Tk, Qt, Flutter, React and Dear ImGui -- and among the compound
document systems that tried to make applications out of parts. What
they do that this will not, and which of their ideas fit in a few
hundred readable lines. Companions:
[`notes_gui.md`](../tutorials/notes_gui.md) (how it works) and
[`plan_gui_teaching.md`](../plans/plan_gui_teaching.md) (what gets
built, in what order).

## The one-line version

| | What it optimizes for | What you write |
|---|---|---|
| Win32, Motif, Tk, GTK | Native widgets, one platform's look, callbacks | Build a tree, register functions; the toolkit owns the state |
| Qt, wxWidgets | The same, portably, plus a whole application framework | Signals and slots, a designer, 30 years of API |
| Swing, Cocoa, WPF | MVC/MVVM with data binding, a designer file | Models, views, bindings; a lot of ceremony for a checkbox |
| Flutter, SwiftUI, Jetpack Compose | Declarative, and drawing everything themselves | A tree of widgets rebuilt on change; constraints down, sizes up |
| React, Elm, the web | The DOM as the output of a function of state | `view : model -> ui`, and a diff to make it cheap |
| Dear ImGui | Tools and debug UI inside a program that already renders | `if (Button("OK"))` -- no objects, no state, one frame |
| Smalltalk, Self/Morphic, HyperCard | A world you can open, poke and change while it runs | Objects all the way down, and no line between using and building |
| Plan 9: libpanel, rio, acme | The smallest thing that works, text as the interface | A panel tree in C, or: no widgets at all, just text and mouse chords |
| `gui/` + `playground/Gui` + `apps/` | Seeing *why* each of the above is shaped the way it is | `if Gui.button computer ~at "Reset" then ...`, and the same app written four ways |

## Part 1: where it came from

- **Sketchpad** (Ivan Sutherland, 1963): the light pen, constraints,
  and the first direct manipulation of graphics. **NLS** (Douglas
  Engelbart, 1968, "the mother of all demos"): the mouse, windows,
  hypertext.
- **Smalltalk-76/80** (Xerox PARC): overlapping windows, the class
  browser, and **MVC** -- written down by **Trygve Reenskaug in 1979**
  as a way to keep a model separate from the many views of it. Nearly
  every architecture since is MVC with the arrows renamed (MVP, MVVM,
  presentation model, data binding).
- **Xerox Star** (1981) then the **Macintosh** (1984): the desktop,
  the menu bar, and -- the part that matters for this plan -- the
  *Human Interface Guidelines*, the idea that applications should
  agree with each other.
- **Tk** (John Ousterhout; Tcl 1988, Tk 1991, `grid` in Tk 4.1, 1996):
  the high-water mark of "a GUI is a scripting language". Its
  **geometry managers** (`pack`, `grid`) are still the cleanest
  separation of layout from widgets anyone has shipped, and its
  callback style is what most people mean by "GUI programming".
  `pack` is, for a single row or column, the same algorithm as
  Flutter's constraints (`gui/Layout.mli` puts the two side by side):
  the difference is that Tk fixes a widget's requested size before
  knowing the room, which is why wrapping text there needs
  `-wraplength` or a `<Configure>` binding.
- **NeXTSTEP and Interface Builder** (1988): the interface as a saved
  object graph rather than code -- the ancestor of every designer tool,
  and of the `.nib`/`.xib` idea that `view : model -> ui` eventually
  displaced.
- **Visual Basic** (1991) made all of the above ordinary, which is
  worth saying in a teaching note: the most influential GUI system ever
  built was the one that let non-programmers draw a form and type into
  the button.
- **Qt** (1995), **GTK** (1998), **Swing** (1997): portability, at the
  price of either drawing everything yourself (Swing, and now Flutter)
  or wrapping every platform's widgets (wxWidgets).
- **WPF and MVVM** (2005), **React** (2013), **Elm** (2012), then
  **Flutter** (2017), **SwiftUI** and **Jetpack Compose** (2019): the
  declarative turn -- describe the interface as a function of the
  state, and let the framework work out the difference. This
  playground is on that side of the line by inheritance.
- **Immediate mode** (Casey Muratori's talk, 2005; **Dear ImGui**,
  Omar Cornut, 2014): the counter-argument, from games -- if you are
  redrawing sixty times a second anyway, widget objects are a cost
  with no benefit. Now the default for every game engine's tools.
  (Checked 2026-09-21: Muratori coined "IMGUI" in a 2005 video, after
  describing the idea on a mailing list in 2002; Dear ImGui 1.00 was
  published on 11 August 2014.)

## Part 2: compound documents, the road not taken

The subject `appkits/embed` revives, and a genuinely interesting
failure:

- **Xerox Star** (1981): text, pictures, tables and equations in one
  document, each edited where it sat, with the same few universal
  commands (Move, Copy, Delete, Properties) on all of them -- but a
  fixed set of kinds, built into one editor: integration rather than
  components.
- **The Andrew Toolkit / Andrew User Interface System** (CMU, 1988):
  the first widely used compound documents -- text with embedded
  drawings, spreadsheets and animations, each handled by its own
  "inset". It worked, and it is largely forgotten.
- **OLE 1** (Microsoft, 1990) and **OLE 2** (1993), whose *in-place
  activation* -- clicking the embedded spreadsheet changes the host's
  menus, and you edit it where it sits -- is the feature everyone
  remembers and the one this plan reproduces in a `bool`. Underneath
  sat COM, then ActiveX (1996), and a great deal of registry.
- **OpenDoc** (Apple, IBM and CI Labs, 1992-97): the bet that the
  *application* was the wrong unit and documents made of parts were
  the right one. Cancelled in 1997 shortly after Jobs returned, and
  remembered as the most principled GUI architecture that nobody
  shipped. **Bonobo** (GNOME) and **KParts** (KDE) carried the idea on
  Unix; **JavaBeans** (1996) was the same component idea for widgets.
- And then it **won somewhere else entirely**: an embedded video, a
  Jupyter notebook cell, a Notion block, an embedded map or tweet.
  Compound documents lost as an application architecture and became
  the ordinary shape of the web. Worth saying plainly in a teaching
  note, because the lesson is not "OLE was bad" but "the unit of
  composition moved".

(Checked 2026-09-21: OLE 1.0 is from 1990, and in-place activation
came with OLE 2; OpenDoc was cancelled in March 1997, soon after Jobs's
return; the Andrew Toolkit's insets date from the late 1980s, its
papers around 1988 -- "around 1988" is as exact as the sources are.)

## Part 3: the teaching lineage

- **Ousterhout's *Tcl and the Tk Toolkit*** (1994) for callbacks and
  geometry managers, and for the argument that a GUI wants a scripting
  language.
- **Reenskaug's original MVC note** (1979) -- two pages, and clearer
  than most of what was written about it since.
- **Elm's guide** and its "The Elm Architecture" chapter: the shortest
  statement of MVU, and the direct ancestor of this repository.
- **Casey Muratori's immediate-mode talk** (2005) and **Dear ImGui's
  own documentation**, which is unusually honest about the id problem
  and about what immediate mode is bad at.
- **7GUIs** (Eugen Kiss, 2014): seven tasks -- Counter, Temperature
  Converter, Flight Booker, Timer, CRUD, Circle Drawer, Cells --
  designed for exactly the comparison this plan makes, with
  implementations in many toolkits to read against ours.
- **Flutter's layout documentation** ("constraints go down, sizes go
  up, parent sets position"), which is the best single page on layout
  anywhere.
- **Bill Atkinson's HyperCard** (1987) and **Alan Kay's writing**, for
  the older idea this project keeps circling: that a person should be
  able to *change* the thing they are using.
- For the applications: **Bricklin and Frankston's VisiCalc** (1979)
  and Bricklin's own accounts of it; **Knuth and Plass, "Breaking
  Paragraphs into Lines"** (Software: Practice and Experience, 1981);
  and the **piece table**, documented in the *Microsoft Word* file
  format literature and in Crowley's editor notes.

## Part 4: Plan 9, and the smallest possible GUI

Worth its own part here because of this author's other repositories
(`xix`, `principia-softwarica`), and because it is the opposite
extreme from Qt:

- **libpanel** and the 8 1/2 / **rio** window systems: a few hundred
  lines of C for a panel tree, scroll bars and buttons -- a toolkit
  small enough to read in an afternoon, which is exactly this plan's
  ambition in OCaml.
- **acme** (Rob Pike, 1993): the interface *is* text. No buttons, no
  menus, no dialogs: any word in any window is a command if you click
  it with the right button, and mouse chords do cut, paste and
  execute. The most radical answer in the whole field to "what should
  a GUI be", and one of the few that a small program can imitate.
- **Oberon** (Wirth, 1988) had the same instinct -- text as the
  universal interface, with objects embedded in it, which makes it a
  compound-document system too, and a tidier one than OLE.

## Part 5: in Elm, and in OCaml

- **Elm** is the direct ancestor: `elm-html`, the virtual DOM, and
  MVU. What this repository ports is Evan's *simplification* of it
  (elm-playground), so adding a GUI layer is putting back the part
  that was simplified away -- with the four-way comparison as the
  teaching that Elm itself cannot do, being only one of the four.
- **OCaml's GUI history** is longer than people think, and is mostly
  bindings: **LablTk** (Jacques Garrigue, in the distribution for
  years), **lablgtk** (Garrigue, Olivier Andrieu and others), and more
  recently **Bogue** (SDL-based widgets), **Nottui**/**Lwd** (terminal
  UIs with incremental recomputation), **Brr** and **Note** (Daniel
  Bünzli: the browser, and functional reactive programming), and
  **js_of_ocaml** with **ocaml-vdom** -- which **this repository
  already depends on**: the web backend is LexiFi's `vdom`, i.e. an
  Elm-style virtual DOM in OCaml. The GUI plan's web story is
  therefore not new work but an existing dependency.
- **The author's own prior art**, again in the house: **efuns**, an
  Emacs clone with a GTK/Cairo GUI, and **codemap**, a treemap
  visualiser -- both real OCaml GUI applications, both with the text
  editing, layout and event handling this plan proposes to rebuild
  small. `Text_edit`'s piece table should be read against efuns'
  buffer, and the differences recorded.
- **In Elm**, no compound-document or component system exists, and it
  would be an odd fit: MVU's single model is the opposite instinct.
  That makes `appkits/embed` the part of this plan with no upstream
  to copy, and the most interesting to design.

## Where `gui/` and `playground/Gui` actually sit

Two levels, as everywhere here:

- **`gui/`, the toolkit**, at the legible end: one module per idea
  (`Widget`, `Layout`, `Focus`, `Text_edit`), and -- the part no other
  toolkit can offer -- **the same widgets wired four ways**
  (`Immediate`, `Retained`, `Mvc`, `Mvu`, all drawing through one
  `Look`), with 7GUIs as the shared harness, so the architectures can
  be compared by reading and by running rather than by argument.
- **`playground/Gui`, the API**, at the simple end: immediate mode,
  because `game`'s update has no message type, so a button is a
  question you ask in `update` and nothing else.

**The ceiling, stated now**: no native widgets, no accessibility, no
IME, no right-to-left or complex text shaping, Hershey strokes for
type, no real file formats, no printing, no window manager, and apps
that are a page each rather than a product. TinyExcel recalculates
a dependency graph and will not open a `.xlsx`; the typesetting
example breaks paragraphs the way TeX does and will not hyphenate
Hungarian -- and TinyWord breaks them greedily, as Word did.

## Postscript: the numbers, and what building it showed

Built and measured 2026-09-21 (the full tables are in
[`notes_gui.md`](../tutorials/notes_gui.md) §16):

- **Lines per architecture for the same 7GUIs task**, the number the
  comparison existed for: the counter is 12 lines of code with
  callbacks, 12 with MVC, 16 with MVU, 3 in immediate mode -- and the 3
  borrows the playground's own model and loop, so the honest reading
  is the one the plan guessed: at this size the length is not the
  difference; *where the count lives* is (two places with callbacks,
  one in the others).
- **`gui/`'s size**: 1,073 lines of code for all four architectures,
  layout, grid, focus, text and the piece table; 221 of them are the
  immediate-mode toolkit the playground uses. Against Tk's or Dear
  ImGui's size the comparison would be unfair both ways -- they are
  products with thirty and ten years of edge cases -- so it is not
  made here; the point is that the ideas fit in an afternoon's reading.
- **A paragraph, greedy against Knuth-Plass**: the worked example in
  `appkits/typeset/Linebreak.mli` -- demerits 656,706.25 for greedy,
  12,706.25 for the optimum, the difference being one word moved up a
  line.
- **An original's size**: MacPaint's released source (Computer History
  Museum, 2010) is about 5,800 lines of Pascal and 2,700 to 3,600 of
  assembly depending on the version counted, on top of QuickDraw;
  TinyMacPaint with its engine is some 600 lines of code on top of the
  playground. VisiCalc's source is not public, so its size is not
  given here.
- **How much of each app is the toolkit's**: roughly half, and
  growing with each app -- TinyExcel is 159 lines over a 500-line
  engine; the later apps reuse the earlier ones' engines (TinyOpenDoc's
  parts are TinyWord's, TinyExcel's and TinyMacPaint's; TinyHyperCard
  paints with TinyMacPaint's).

What writing it showed that reading had not, in one line each (the
details are in `notes_gui.md`):

- MVU cannot hold the caret: the focus and the selection have to live
  under the model, which in Elm is the browser's DOM.
- A click is the release, and the playground's `mclick` had not been
  set by any backend since they were factorized -- the first button
  found it.
- A press on a menu's title is not yet the menu's (it becomes modal on
  release), and more than one drawn surface acted on it.
- In a compound document, the click that activates a part is the
  host's, not the part's.
- An editing session in place is one undo, and two parts, being
  functions, can only be compared by what they save.
- The apps came out in pairs over one engine -- VisiCalc and Excel,
  Bravo and Word -- which is the clearest statement of what a GUI adds
  to a program: the same engine, and a different person using it.

Sources: checked on 2026-09-21 where said above (OpenDoc, OLE, the
Andrew Toolkit, Dear ImGui and Muratori, 7GUIs -- a 2014 master's thesis
by Eugen Kiss at Leibniz University Hannover --, PowerPoint 1.0 on 20
April 1987, MacPaint's released source); the rest from memory, and to
be checked before relying on it for teaching, particularly OCaml's GUI
library history, where the surviving projects should be checked rather
than remembered.
