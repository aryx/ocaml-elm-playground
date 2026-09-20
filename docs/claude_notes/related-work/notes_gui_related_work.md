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
  (Dates from memory, to check.)

## Part 2: compound documents, the road not taken

The subject `appkits/embed` revives, and a genuinely interesting
failure:

- **The Andrew Toolkit / Andrew User Interface System** (CMU, 1988):
  the first widely used compound documents -- text with embedded
  drawings, spreadsheets and animations, each handled by its own
  "inset". It worked, and it is largely forgotten.
- **OLE 1** (Microsoft, 1991) and **OLE 2** (1993), whose *in-place
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

(Names and dates above from memory, to check -- especially OpenDoc's
cancellation and the Andrew dates.)

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
  (`Immediate`, `Retained`, `Mvc`, and MVU which is the playground
  itself), with 7GUIs as the shared harness, so the architectures can
  be compared by reading and by running rather than by argument.
- **`playground/Gui`, the API**, at the simple end: immediate mode,
  because `game`'s update has no message type, so a button is a
  question you ask in `update` and nothing else.

**The ceiling, stated now**: no native widgets, no accessibility, no
IME, no right-to-left or complex text shaping, Hershey strokes for
type, no real file formats, no printing, no window manager, and apps
that are a page each rather than a product. TinyExcel will recalculate
a dependency graph and will not open a `.xlsx`; TinyWord will break
paragraphs the way TeX does and will not hyphenate Hungarian.

## Postscript: the numbers (to come)

Once built: lines per architecture for the same 7GUIs task (the
number the whole comparison exists for); `gui/`'s total against Tk's
and Dear ImGui's; TinyVisiCalc's engine against its 1979 original's
reported size; a paragraph broken greedily against Knuth-Plass, with
the badness scores; and how much of each app is the toolkit's rather
than its own.

Sources: from memory unless linked, and to be checked before relying
on them for teaching -- particularly the OpenDoc and Andrew dates, the
Dear ImGui and 7GUIs attributions, and OCaml's GUI library history,
where the surviving projects should be checked rather than
remembered.
