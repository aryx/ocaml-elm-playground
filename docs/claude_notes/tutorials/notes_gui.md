# GUIs, from scratch: a tutorial for `gui/`

How a program is built around the person using it: what a widget
actually is, where the state lives (four rival answers, one of which
this playground already is), how things are laid out, how text is
edited, and how one document ends up inside another.

It began as the specification of the toolkit planned in
[`plan_gui_teaching.md`](../plans/done/plan_gui_teaching.md), written
before the code; it has since been checked against the code
(2026-09-21), its numbers filled in (§16), and what writing each part
showed added where it belongs. Companions:
[`notes_gui_related_work.md`](../related-work/notes_gui_related_work.md)
(Smalltalk, Tk, Qt, Flutter, React, Dear ImGui, acme, OLE) and
[`notes_inspect.md`](notes_inspect.md), whose panels and timeline are
this toolkit's first customer.

## 0. Where the code is, and a reading order

| module | what | section |
|---|---|---|
| `gui/Widget`, `gui/Theme`, `gui/Look` | what a widget is: a rectangle, a drawing, a hit test, some state; the colours; the drawing all four architectures share | §2 |
| `gui/Immediate` | the toolkit `playground/apis/Gui` is built on | §3 |
| `gui/Retained`, `gui/Mvc`, `gui/Mvu` | the same widgets, wired the other three ways | §4 |
| `examples/gui4` | four 7GUIs tasks, each written the four ways | §4 |
| `gui/Layout`, `gui/Grid` | constraints down, sizes up; Tk's grid | §5 |
| `gui/Focus` | who gets the keys | §6 |
| `gui/Text`, `gui/Text_edit` | UTF-8; a piece table, a cursor, a selection, undo | §7 |
| `appkits/typeset` | where to break a paragraph, greedily and Knuth-Plass | §7 |
| `appkits/document` | a document as a value; undo; the clipboard | §8 |
| `appkits/embed` | a document made of parts | §9 |
| `playground/apis/Gui` | the Evan-style API over all of it | §10 |
| `appkits/sheet`, `appkits/sheet_view` | a spreadsheet's engine, and its drawing | §11 |
| `appkits/richtext` | text with looks, its page, and its flow through columns | §12, §13b |
| `appkits/paint` | a picture as bits, and what paints it | §13 |
| `appkits/draw` | a picture as objects | §13 |
| `appkits/slides` | a talk as an outline | §14 |
| `appkits/hypertalk` | HyperCard's language, and its message path | §15 |
| `apps/` | TinyVisiCalc, TinyExcel, TinyBravo, TinyWord, TinyFrameMaker, TinyMacPaint, TinyMacDraw, TinyOpenDoc, TinyPowerPoint, TinyHyperCard | §10-15 |

## 1. A GUI is a loop you already have

Strip the vocabulary away and a graphical interface is:

```
   events (mouse, keys)  -->  state  -->  pixels
        ^                                   |
        +---------  the person  ------------+
```

which is precisely `Playground.game`'s `update` and `view`. Everything
in this note is about the middle box: what the state *is*, who owns
it, and how a click finds the thing it clicked.

The one structural difference from a game is that a GUI's state is
mostly **not** the interesting state. A game's model is the world; an
app's model is a *document*, plus a pile of incidental facts -- which
button is being pressed, where the cursor blinks, what is selected,
which field has focus. Each architecture below is, at heart, a
different opinion about where that incidental pile should live.

## 2. What a widget is

Four things, and every toolkit in §4 agrees on these even when it
disagrees about everything else:

```
   +-------------------+   a rectangle      (where it is)
   |     Save          |   a drawing        (shapes, in our case)
   +-------------------+   a hit test       (is this point mine?)
                           some state       (pressed? focused? scrolled?)
```

The fourth is the whole argument. A button's "am I being pressed"
lasts one click; a scroll bar's position lasts a session; a text
field's cursor lasts until you click elsewhere. Who keeps that, and
for how long?

## 3. Immediate mode: no widgets at all

The answer that fits this playground, and the smallest one:

```ocaml
if Gui.button computer ~at:(0., 100.) "Reset" then model_reset else model
```

There is no button object. Each frame, `button` draws a rectangle with
a label, asks whether the mouse is inside it, and returns whether it
was released inside it this frame. The "widget" exists for the length
of an `if`.

```
   frame N:    draw the button, test the mouse, return false
   frame N+1:  draw the button, test the mouse, return TRUE  -> act
   frame N+2:  draw the button, test the mouse, return false
```

What it buys here specifically: `Playground.game`'s update is
`computer -> 'memory -> 'memory` with **no message type**, so a
callback has nowhere to go and a message has nothing to be. Immediate
mode needs neither. It is also why `Inspect`'s sliders are one line
each.

What it costs, said honestly, because this is the fashionable choice
and fashion is not an argument:

- **State that must persist needs a home anyway.** Scroll positions,
  which field has focus, the text being edited -- immediate-mode
  toolkits keep them in a side table keyed by an **id**, and
  generating those ids is where the design gets subtle (Dear ImGui
  hashes the label, which is why two buttons called "OK" in one window
  are a classic bug). `gui/Immediate` uses the widget's *rectangle*
  instead: same label twice is fine, same place twice is not, and that
  one you can see.
- **Layout is hard when nothing is retained**: you cannot ask a widget
  how big it wants to be before drawing it, because it does not exist
  until you draw it. Toolkits solve this by measuring in a first pass
  or by using last frame's sizes (a frame of lag, invisible in
  practice).
- **Everything is redrawn every frame**, which is free here (we redraw
  the world anyway, at 60 fps) and is exactly why immediate mode was
  born inside *games* (Casey Muratori, 2005) rather than in
  applications.

## 4. The four architectures

The same tiny program -- a counter with a button -- written four ways.
This is the section the whole plan is for.

**Callbacks** (Tk, 1988; Motif; Win32; GTK): the widgets are a tree
of objects that own their state, and you hang functions on them.

```
   [Button "+1"] --onclick--> fun () -> count := !count + 1;
                                        label#set_text (string_of_int !count)
```

Direct, immediate to learn, and it scales badly for one specific
reason: **the truth is scattered**. The count lives in a ref, its
display lives in a label, and keeping them equal is your job, in every
callback that touches it. Every bug where the screen says something
the program does not believe lives here.

**MVC** (Smalltalk-80; Trygve Reenskaug, 1979) answers exactly that:
put the truth in a **model**, let **views** observe it and redraw when
it changes, and let **controllers** turn input into model changes.

```
        +---------+  notifies   +--------+
        |  Model  |-----------> |  View  |
        +---------+             +--------+
             ^                      |
             | changes              | input
        +------------+  <-----------+
        | Controller |
        +------------+
```

The idea is right and has outlived its own details -- MVP, MVVM and
"data binding" are all MVC with the arrows renamed. What it does not
solve: the views still hold state, the notifications can arrive in any
order, and "who redraws what when" is a real design problem in a big
app.

**MVU** (Elm, 2012 -- and this playground): delete the views' state
entirely. One model, one `update`, and a `view` that builds the whole
interface from scratch each time.

```
   msg ---> update ---> model ---> view ---> ui  ---> (events) ---> msg
```

"Rebuild everything" sounds absurd until you notice that the alternative
was keeping two copies in sync, and that a diff (React's virtual DOM,
2013) makes the rebuild cheap. The cost is that *every* incidental
fact -- the scroll position, the open menu -- must become part of the
model, which is either the honesty that makes it work or the tedium
that makes people leave, depending on the day.

**Immediate mode** (§3) is the fourth corner: no widget state, no
messages, no model of the interface at all -- the program's own
variables, read and written in place, once a frame.

The honest summary, which the `.mli`s repeat: **callbacks scatter the
truth, MVC guards it, MVU rebuilds from it, immediate mode never
stores anything but it.** And the harness that makes this more than an
opinion is **7GUIs** (Eugen Kiss, 2014): seven tasks picked so that
each architecture's weakness shows up in at least one -- validation in
Flight Booker, selection in CRUD, undo and dialogs in Circle Drawer,
and a dependency graph in Cells.

### What writing all four actually showed

They are in `gui/` now (`Immediate`, `Retained`, `Mvc`, `Mvu`), all
four drawing through one `Look` so that only the wiring differs, and
running side by side in `examples/GuiFourWays.ml`. Four things came
out of writing them that reading about them had not given:

- **The length is not the difference.** At the size of a counter all
  four are a handful of lines, and anyone claiming one is dramatically
  shorter is choosing the example. Five 7GUIs tasks are now written
  four ways each (`examples/gui4/`, one file per task, the four one
  under the other), and each version's own code lines, the rules and
  the layout they share counted apart:

  ```
                   immediate  callbacks  MVC  MVU   shared
     Counter            8         6       7    11      6
     Temperature       12        17      13    19     20
     Flight Booker     13        15      17    22     30
     Timer             13        17      16    22     24
     Circle Drawer     39        31      26    35     89
  ```

  MVU is the longest every time, and five of its lines are the loop
  itself -- stepping, and keeping the model and the platform's state in
  refs -- which a real MVU framework (the playground's own `game`)
  provides; the rest is its message type and its view. Callbacks grow
  with the rules: they pass immediate mode exactly in the two tasks
  where fields depend on each other (Temperature, Flight Booker), and
  what the table cannot show is that Flight Booker's callbacks are
  correct only because every handler remembers to call one `check`.
  Circle Drawer turns the table over: immediate mode is the longest,
  because a click that takes a widget away (Close closing the dialog)
  leaves it drawn for that frame -- immediate mode's frame of lag --
  and its runner draws the frame again when the model changed, as
  MVU's `step` does; and callbacks come out short only because the
  rules they call are shared and pure (the undo of a whole drag is
  `close`, eight lines counted once). What they cannot share is the
  knot: every callback ends in `redraw ()`, the widgets and their
  callbacks need each other (a forward reference), and the dialog's
  slider has to be told its starting value by hand. What differs is *how many places
  hold the count*: two with callbacks, one in the other three. That is
  the whole argument, and `Unit_architectures.ml` has it as a test —
  bump the ref without telling the label, and the screen says `0`
  while the program believes `1`.
- **They can be made to paint identically, and that is worth
  enforcing.** The same clicks through all four produce the same
  rectangles, frame for frame; the test that checks it is what stops
  the comparison from quietly becoming a comparison of drawing code.
- **The one place they disagreed was timing**, and it was not
  architectural. A retained toolkit paints *after* its callbacks ran,
  so a click shows up in the same frame; in immediate mode whether it
  does is the order you ask the widgets in — button first, or label
  first — which is one line, visible in the source, and yours. MVU
  only matches if `step` views the model *after* folding the
  messages, which is what Elm does and what made the loop in `Mvu.mli`
  come out honest.
- **MVU cannot hold the caret.** The view is rebuilt from the model
  every frame, so the focus and the caret — which are not in the model
  and should not be — have to live *underneath* it. In Elm that
  underneath is the browser, which keeps the focus and the selection
  in the real DOM; it is the same reason a virtual DOM needs keys and
  React needs refs. Writing `Mvu.t` made that visible in about ten
  lines: the architecture is honest about where the truth is, and then
  leans on the platform for the truth it cannot hold.
- **The test that makes them paint alike found five things**, once it
  ran on tasks bigger than a counter (`examples/gui4/tests/Unit_gui4`): in
  immediate mode a widget is drawn when it is asked for, so a menu
  asked for last turned the return date on a frame late, and a field
  asked for first showed the focus leaving it a frame late -- fixed by
  asking the menu first (its items on an **overlay** drawn last, Dear
  ImGui's popup layer) and by moving the focus at the start of the
  frame whose release ends in a field; two immediate fields shared one
  caret, which an unfocused field with a shorter text could move (a
  real bug, now a test); the retained toolkit gave a clicked field the
  keys but no caret where the click landed; and the four disagreed
  about whether clicking a button takes the keys away from a field --
  they now agree it does not, the Mac's rule. None of the five is an
  architecture's virtue; each was a place where "the same program"
  was quietly not the same.
- **Circle Drawer needed three things no toolkit had, and found a
  sixth disagreement.** A *canvas* (the program's drawing, and what the
  mouse did there: `Widget.canvas_event`), a *context menu* (a popup
  at a point that has the mouse while it shows), and, for the retained
  toolkit, *hiding* a widget (`Retained.set_shown`: a dialog that
  comes and goes in a toolkit that cannot add and remove widgets) --
  and the widgets now see the right button (`Widget.input.mrdown`).
  Each architecture takes the canvas its own way: immediate mode asks
  what the mouse did and *then* draws what it now knows (Dear ImGui's
  InvisibleButton and draw list), callbacks and MVC hand a retained
  canvas a new picture after every change, MVU draws it in the view
  like everything else. The disagreement was below the pixels: the
  slider arithmetic, written once in each toolkit, came out a last
  bit apart -- on arm64, OCaml fuses a multiply and an add into one
  `fmadd` where the code's shape allows it, and one copy was fused and
  another not -- so a circle drawn from each version's diameter was
  not the same picture. The toolkits now share `Look.slider_value`: the
  same formula must be the same code, not the same text.

## 5. Layout: constraints down, sizes up

Flutter's rule (2017), and the clearest one anybody has written:

```
   parent:  "you may be 0..400 wide and 0..200 tall"     (constraints down)
   child:   "then I am 180 x 40"                          (sizes up)
   parent:  places it at (10, 10)                         (parent positions)
```

Three rules, one pass down and one up, and rows, columns, centring,
padding and spacers all fall out of them. The alternatives worth
knowing: Tk's **geometry managers** (`pack` and `grid`, Ousterhout;
Tcl 1988, Tk 1991, `grid` 1996) --
a separate object that owns placement, which is why Tk code never
computes a coordinate -- and CSS **flexbox**, the same constraint idea
with a much larger vocabulary and a famously subtle sizing algorithm.

Absolute coordinates (what every game in this repository uses, and
what §3's examples show) are the fourth option, and they are fine
until the first resize. NeXT's and Cocoa's **springs and struts**
(1988) are the fifth: each widget says which of its edges and sides
are elastic, which is direct and hopeless once a window gets small.

The family's ancestor is older than any of them: TeX's **boxes and
glue** (Knuth, 1978). A line of type is boxes with glue between them,
glue being space that stretches and shrinks by stated amounts; a
`spacer` is glue with infinite stretch, and a row is an hbox.

In `gui/Layout` the two rules are two functions — `measure`
(constraints down, sizes up) and `arrange` (the parent positions) —
and a column is written as a row turned on its side, along an `axis`,
so there is one of each rather than two. One rule holds it together:
**a leaf takes exactly the rectangle its parent gives it**. Its
measured size is what it *asks* for; a row, a column or a `center`
grants it, while `pad` and `expand` hand over what is left.

Honest about what layout buys here today: the playground's screen is
1000x1000 whatever the window, and its `Resized` message is a TODO,
so what a layout gives is arrangement — a column that spaces itself,
buttons of one width — rather than adapting to a resize. The three
rules are the same ones that will make resizing work the day the
window's size arrives.

## 6. Focus, hit testing, and the events nobody thinks about

- **Hit testing** is "which widget is under the mouse", back to front,
  first match wins -- so it walks the tree in the *opposite* order to
  drawing.
- **Capture**: once you press inside a scroll bar, it keeps the mouse
  until you release, even if you wander off it. Without capture,
  dragging is infuriating; it is two lines and nobody writes them
  first.
- **Focus** is which widget gets the keys, plus the tab order and a
  visible ring. Every app's keyboard experience is this and nothing
  else. A retained toolkit walks its tree for the tab order, and then
  needs an escape hatch when the tree's order is not the reading order
  (the web's `tabindex`, and the accessibility bugs that come of
  getting it wrong). In immediate mode there is no tree: the widgets
  are *asked for* in an order, and that order is the tab order, for
  free and visible in the source. The price is one frame of memory —
  when Tab arrives, this frame's order does not exist yet, so the walk
  uses the previous frame's (`gui/Focus`).
- **A click is the release**, not the press -- which is what lets you
  press a button, think better of it, and slide away without firing
  it (the Macintosh, 1984, and everything since). So a widget needs
  both halves of the gesture and the memory of where the press began,
  which is the capture above. The playground had a `mouse.mclick`
  field for this and, it turned out, nothing had set it since the
  backends were factorized: every game that fires on a click was
  quietly working through its space-key path only. Writing one button
  found it, which is the usual way these things surface.
- **Double-click, the wheel, key repeat, typed characters**: the
  events a game never needs, and an app cannot live without -- which
  is why they are this plan's phase 0.

And the keys themselves, which took two goes to get right here. The
playground's `keyboard` had `kbackspace`, `kenter` and `kshift` from
the beginning and nothing ever set them; worse, the two backends
spelled the named keys differently (SDL lowercased: `"backspace"`,
`"return"`, `"left shift"`; the browser's DOM names: `"Backspace"`,
`"Enter"`, `"Shift"`), so only the arrows and the space bar agreed.
A program that reads a key by name could not run on both until one
spelling was chosen — the browser's, since the arrows already followed
it. Nothing had noticed because a game asks for arrows and a letter,
and never for Tab.

## 7. Editing text

A text editor's data structure is the whole lesson:

```
   a string:      "Hello world"     insert in the middle = copy it all
   a gap buffer:  "Hello[gap] world" insert at the gap  = free
                  (Emacs; one cursor, cheap while you type in one place)
   a piece table: the ORIGINAL text, an APPEND buffer, and a list of
                  pieces saying "take 5 from original at 0, then 6 from
                  appended at 0, then ..."   (Word's own structure)
```

The piece table wins for an app for a reason that matters more than
speed: **it never modifies the original**, so undo is a list of piece
lists, and a document is naturally a value (§8). A selection is two
positions; a cursor is a selection of length zero; word wrap is a
function from pieces to lines.

`gui/Text_edit` is that, in 222 lines (145 of them code), and three
things came out of writing it:

- **The append buffer is mutable and shared by every version**, and
  that is safe for exactly one reason: it is only ever *appended to*,
  so a piece an old version wrote down still says what it said. That
  one-way rule is the whole trick — it is what lets everything above
  it be a value while nothing is ever copied.
- **Merging matters more than it looks.** Without it, every keystroke
  is a piece, and a typed paragraph is a thousand of them. Extending
  the last piece when the new text lands exactly where that piece ends
  is four lines, and it is the difference between a structure and a
  linked list of characters: 5000 random edits in the test leave a
  table of a few dozen pieces.
- **Undo needed no code at all.** `undo` is "put the old list back",
  and the test that convinced me the structure was right is not a
  worked example: five thousand random inserts, deletes, selections,
  undos and redos, checked at every step against a plain string that
  copies itself and keeps every version. A clever structure is only
  worth having if it is indistinguishable from the slow one, and
  "indistinguishable" is a claim about inputs nobody thought of.

`examples/GuiEditor.ml` puts the numbers on the screen — pieces,
versions back, versions forward — which is a better argument for the
structure than any paragraph about it.

And the line breaking, which is `appkits/typeset/Linebreak` and
`examples/TypesetParagraph`. The worked example in its `.mli` is the whole idea
at a size that checks by hand — "aaa bb cc ddddd ee ff gggg" at a
measure of 10:

```
   greedy                ratio   demerits      optimal               ratio   demerits
     aaa bb cc            0.5    506.25          aaa bb cc            0.5    506.25
     ddddd ee             2.0    656100          ddddd ee ff         -1.0     12100
     ff gggg              0.0       100          gggg                 0.0       100
```

Greedy put `ff` on the last line because it fit there, and left
`ddddd ee` with two units to fill and one space to fill them with. The
optimal breaker took `ff` up a line and shrank two spaces a little,
and the paragraph comes out fifty times better. Two things from
writing it:

- **the test that matters is a law, not an example**: over three
  hundred random paragraphs, the optimal breaker is never worse than
  greedy — it cannot be, since greedy's breaks are one of the choices
  it had — and every word is set exactly once, in order;
- **a monospaced page is the honest way to show it**: with every width
  exact, the right edge is straight to the pixel and the only thing
  that differs between the two breakers is the breaking. Switched to
  greedy, the example's own opening page grows three rivers where
  Knuth-Plass had one, and the margin says by how much.

Then, for a word processor, the one real algorithm: **Knuth and
Plass's line breaking** (1981). Greedy breaking -- fill each line until
the next word does not fit -- is what browsers and most editors do and
leaves rivers and lonely short lines. Knuth-Plass instead scores a
whole paragraph (each line's "badness", plus penalties for
hyphenation and for consecutive breaks) and picks the *globally* best
set of breaks by dynamic programming. It is the reason a TeX paragraph
looks the way it does, and here it is 136 lines, greedy included (77
of code) -- without hyphenation or stretchability per space, which is
where TeX's own gets long.

## 8. A document is a value

```ocaml
type 'a document = { content : 'a; path : string option; saved : 'a }
```

An edit returns a new document. Then:

- **undo** is a list of past contents, **redo** the list you popped
  off (or the command pattern, which is the same thing with sharing --
  `gamekits/puzzle/Undo` is this idea for a grid game, already here);
- **dirty** is `content != saved`, a pointer comparison;
- and, the reason it is worth insisting on, **`Inspect` can scrub an
  app**: a word processor whose whole history replays is the
  architecture arguing for itself.

`appkits/document` is those three in 121 lines (50 of code), and
writing them turned up two things worth keeping:

- **the pointer comparison has a catch.** It is exact and free when
  going back to a version means *the old value itself*, which is what
  a list of past documents gives you — undo back to what you saved,
  and the star in the title goes out by itself, which most editors get
  wrong. But a structure that *rebuilds* a version rather than keeping
  it — `Text_edit.undo` rebuilds its record — hands back something
  equal and not identical, and then the star stays lit. Hence
  `Document.create ?equal`, and the honest statement that the cost of
  getting it right there is whatever comparing two documents costs.
- **the interesting question about undo is what counts as one edit.**
  7GUIs' Circle Drawer is built to ask it: dragging a slider in a
  dialog must be *one* undoable edit and not the fifty values it
  passed through. The answer is to keep the live value aside while the
  dialog is open and record once when it closes — three lines, and the
  bug they prevent is the most common undo bug there is.

Saving is deliberately not in there: writing bytes is a backend's
business, and a browser has no files at all. What a document knows is
whether it *needs* saving, and what to call itself. (How the apps will
save -- Marshal behind a checked header, files natively and
`localStorage` on the web, with capabilities -- is
[`plan_io.md`](../plans/plan_io.md) and
[`plan_caps.md`](../plans/plan_caps.md).)

## 9. A document made of parts

The idea worth reviving, and the author's own ask. A **part** is
something that can say how tall it is at a width, draw itself into a
rectangle, take the mouse and keys while it is **active**, offer a
menu, and write itself down -- `appkits/embed/Component`:

```ocaml
type part = {
  kind : string;                                  (* what reads it back *)
  height : float -> float;                        (* at this width *)
  draw : Widget.box -> active:bool -> shape list;
  input : computer -> Widget.box -> part;         (* a new part *)
  menu : string list;
  command : string -> part;
  save : unit -> string;
}
```

That is the whole protocol, and everything the 1990s built around it
-- OLE's interfaces and registry, OpenDoc's parts, Bonobo's CORBA -- is
plumbing for doing it *across processes and languages*, which one
OCaml program does not need: a part is a record of closures over its
own state, `input` and `command` return a new record, and so a part is
a value and a document of them has undo for nothing. The registry is
an association list from a kind's name to its loader; and a kind
nobody here knows becomes a **placeholder** that shows what it is and
saves back exactly the text it came from -- the rule that a document
must survive a program that cannot read all of it.

The planned sketch had `size : unit -> w * h`; writing it made it
`height : width -> float`, which is §5's constraints down, sizes up,
and what a text needs in order to wrap.

`appkits/embed/Compound` is the document: a tree of parts in rows and
columns, laid out by width, a part found by its **path** (its child
numbers from the root -- a value where a toolkit would keep a
pointer), and saved with each part's text counted, so that a reader
can skip a part without understanding it.

The host is **TinyOpenDoc**, chosen over a TinyOffice because it is
the purest form of the idea: OpenDoc (Apple and IBM, 1994-97, cancelled
in March 1997) had no applications at all, only documents of parts.
Click a part once to select it, again to **activate** it -- OLE 2's
in-place activation (1993): a hatched border, and the part's menu in
the host's bar. Two things came out of writing it:

- **An editing session is one edit**, kept outside the history while
  it lasts and recorded when the part is put down -- and only if it
  changed something, which is decided by comparing what the parts
  *save*, since two parts, being functions, cannot be compared.
- **The click that activates a part is the host's, not the part's.**
  Found by a golden scene: the activating click on a picture painted a
  dot, and the pour that followed filled the dot instead of the sky.
  The part now sees the mouse only once that press is released.

TinyPowerPoint (§14) is the second host: a slide can carry a sheet or
a picture, the same parts.

And a fourth kind came later, TinyMacDraw's drawing (`Part_drawing`),
which is the protocol's claim tested: the hosts learned of it through
one line in TinyOpenDoc's registry and an item in each Insert menu, and
nothing else in them changed. Its page is fitted to the part's
rectangle by grouping and scaling its shapes, the mouse mapped back
the same way; and, a drawing being plain data, it saves with Marshal
behind a checked line (`appkits/document/Saved`, `plan_io.md`) -- the
first thing in the repository to be saved that way.

And the room a part is given, which a person can now change -- a
height by a handle, a row's shares by the gap between its parts, the
whole document reflowing as the mouse moves, since the layout is
worked out every frame anyway. When the room is not the size a part's
content has, there are three answers, and the embedding systems each
picked one:

- **negotiation** (OpenDoc's frames): the host proposes, the part
  insists on what it needs -- a height given is kept only when it is
  more (`Compound`'s `Sized`);
- **scaling** (OLE's embedded objects, FrameMaker's imported
  graphics): a part with a size of its own (`Component.natural`: the
  sheet, the picture) is drawn at that size and the drawing scaled to
  the room, up or down, the mouse mapped back through the same scale,
  so the part never knows (`Component.draw_in`, `input_in`) --
  TinyOpenDoc's "Scale to Fit", TinyFrameMaker's frames always;
- **reflow**: what has no size of its own -- a text, a drawing fitting
  its page -- simply takes the room and lays itself out in it.

The history is worth a paragraph because it is a genuine road not
taken: Xerox Star (1981) put text, pictures and tables in one document,
as a fixed set; the Andrew Toolkit (CMU, around 1988) opened the set
with its "insets"; OLE made it a product; OpenDoc bet on it and was
cancelled; and the idea faded from applications -- and then came back
everywhere else. A Jupyter notebook cell, a Notion block, an embedded
video: compound documents won the web and lost the desktop.

## 10. In the playground

`Gui` is the immediate-mode toolkit of §3, threaded through
the `computer` your `update` already has:

```ocaml
let update computer model =
  if Gui.button computer ~at:(0., 120.) "Reset" then initial
  else { model with gravity =
           Gui.slider computer ~at:(0., 60.) ~from:0. ~to_:2000. model.gravity }
```

no new concepts, no message type, and `view` draws what `update`
declared. The other three architectures live in `gui/` as runnable
comparisons: Counter, Temperature, Flight Booker and Timer are written
all four ways in `examples/gui4/` and run side by side in
`examples/GuiFourWays.ml` (§4); the 7GUIs tasks are also in
`examples/Gui7*.ml`, once each, in immediate mode -- CRUD included,
with the list box it needed. The apps are what
the toolkit is *for* ([`plan_gui_teaching.md`](../plans/done/plan_gui_teaching.md)),
mostly in pairs of the same engine under two interfaces a few years
apart: TinyVisiCalc and TinyExcel (§11), TinyBravo and TinyWord (§12);
then TinyMacPaint (§13), TinyOpenDoc (§9), TinyPowerPoint (§14) and
TinyHyperCard (§15). Outside `apps/`, a game or an example uses it as a
control panel: `examples/AiFlock.ml`'s sliders, TinyCoreWar's menus
and text areas.

## 11. A spreadsheet is a graph

The first of the applications, and the one whose lesson is not about
interfaces at all. `appkits/sheet` is the engine both planned
front ends share — TinyVisiCalc's keyboard one of 1979 and TinyExcel's
mouse one of 1985 — and it is two ideas:

**A formula is a tree, and the parser is the grammar.** Four rules,
one function each, calling the one below:

```
   expr   ::= term (('+'|'-') term)*      -- a sum is made of products
   term   ::= factor (('*'|'/') factor)*  -- so a product binds tighter
   factor ::= '-'? atom
   atom   ::= number | ref (':' ref)? | name '(' args ')' | '(' expr ')'
```

That is **recursive descent**, and precedence is not a table in it:
multiplication binds tighter because `term` sits *below* `expr` and is
asked for first. It is the parser worth knowing before any other,
because the grammar and the code are the same shape — and it is where
`plan_teaching_languages.md` meets this one.

**A cell is a node, and changing it walks forwards.** A formula names
other cells, so the sheet is a graph, and what a change costs is the
size of what depends on it:

```
   A1 ------> B1 = A1*2 ------> C1 = B1+A2
                            ^
   A2 ----------------------+

   change A1  ->  recompute B1, then C1   (2 cells, not the sheet)
```

The order is a **topological order** of that part of the graph —
Kahn's algorithm (1962): take a cell waiting for nothing, compute it,
cross it off the lists of those waiting for it. What is left when
nothing can be taken is exactly a **cycle**, which is how a
spreadsheet finds `A1 = B1+1, B1 = A1+1` and says so rather than
looping. `Sheet.recalculated` puts the number on the screen, because
it is the difference between a spreadsheet and a demonstration.

Worth knowing what VisiCalc itself did (Bricklin and Frankston, 1979),
since it explains a generation of habits: it recalculated in row order
or column order, your choice, so a formula reading a cell *below* it
got the previous value and users were told to press the recalculate
key twice. Lotus 1-2-3 (1983) brought the natural-order recalculation
this engine does.

## 12. Text with looks: Bravo, then Word

`appkits/richtext` is the engine both word processors share, and the
pair is the same argument as §11's: **one engine, two interfaces**.

- **`Rich`** keeps §7's piece table for the characters and, beside
  it, a second table of **runs** -- (length, look) -- and every edit is
  the same surgery on both: split at a position, keep what is either
  side, merge neighbours that have come to look alike. Its two rules
  are the ones every word processor has and gets wrong somewhere: what
  you type looks like what is before it, and a look chosen with
  nothing selected is the **typing style**, pending on the caret and
  forgotten when it moves. Tested against three thousand random edits
  compared with the naive way, one look stored per character.
- **`Page`** is WYSIWYG, and the half of it that is harder than it
  looks is the way *back*: `caret_at` puts the caret on the page for
  an offset in the text, `offset_at` takes a click on the page back to
  an offset, and a test checks they agree at every place in a text.
  Glyph widths are the caller's, so the engine never sees a font.

**TinyBravo** (Lampson and Simonyi, Xerox PARC, 1974) is the first
editor where the screen looked like the page -- and it was **modal**:
the keyboard gives commands until one of them says that what follows
is text. Type "edit" in command mode and `e` selects everything, `d`
deletes it, `i` starts inserting, and a `t` is all that is left: the
story Larry Tesler told for the rest of his life. **TinyWord** (1985)
is his answer (Gypsy, with Tim Mott, 1975): no modes, a caret you type
at wherever it is, cut, copy and paste -- and every look reachable
three ways, a toolbar, a menu and a key, none of which needs
remembering. Both draw their looks with the pen from Hershey's strokes
(`Stroke_text`): bold a thicker pen, italic the points sheared.

## 13. A picture is bits: MacPaint

`appkits/paint`, for **TinyMacPaint** (Bill Atkinson, 1984), is a
paint program's lessons, one per module:

- **`Bitmap`**: a picture as bits, eight to a byte as QuickDraw had
  them -- which is why the Mac's screen fitted in its 128 KB at all
  (21,888 bytes, where a byte per dot would have been 175,104).
  Mutable underneath, a value outside: `change` copies, then edits.
- **`Pattern`**: 8 by 8 tiles laid from the *picture's* corner, not
  from where the painting starts, so that two areas painted apart in
  the same pattern join without a seam.
- **`Paint`**: Bresenham's line (1965, integers only), and ovals whose
  outline is *defined* as the edge of the filled oval, so a frame and a
  fill always meet.
- **`Seed_fill`**: the bucket, a row at a time (Smith, 1979), in two
  steps -- find the area as a mask, then paint the pattern through it
  -- because painting as you go never finishes when the pattern has
  white in it.
- **`Packbits`**: the Mac's run-length compression, matching Apple's
  own worked example byte for byte.

And the drawing, which is a lesson about this playground: it has
rectangles and no bitmaps, so a picture is drawn as its runs of black
dots, each merged with the run under it (`Bitmap.rectangles`). Grey is
the worst case -- its dots never line up -- and a frame of the opening
picture is about 3,200 rectangles, 20 ms natively. Remembering the
rectangles for the bitmap they came from (compared with `==`, correct
precisely because a picture in the history is never changed in place)
is Elm's `lazy`, and the obvious next step, a bitmap shape in the
Playground, is a change to every backend.

And its opposite, the same year: **TinyMacDraw** (MacDraw, 1984),
where the rectangle stays a rectangle (`appkits/draw`). A drawing is a
list of objects back to front -- drawn from its start, hit-tested from
its end -- and two things come with objects that dots never had. A
*hollow* shape is only its outline, so a click in its middle goes
through to what is behind it; and resizing is a map of the points
from the old bounds to the new, so a group, resized, scales everything
it holds. Being plain data, a drawing is compared with `=`, which is
how a drag that changed nothing is kept out of the undo history.

## 13b. A document that lays itself out: FrameMaker

**TinyFrameMaker** (FrameMaker, around 1986) is §12's text engine made
to fill pages: one text poured through the columns of every page, the
pages made from a master (one column or two, a header, "page n of N"),
and parts -- a sheet, a picture, a drawing, TinyOpenDoc's own --
**anchored** in the text, each set just below its line and moving with
it. The algorithm is `appkits/richtext/Flow`, and it is small: lay the
text out once at the columns' width, then deal its lines into columns
of a given height, a line going where the last thing ended unless it
does not fit there, and an anchored frame after the line it is tied
to, the same way. Typing above a frame is then enough to move it to the
next page.

It is the other answer to TinyOpenDoc's question, and the comparison is
the lesson: TinyOpenDoc lays its parts out *by position* -- a tree of
rows and columns, a part wherever the tree puts it -- and TinyFrameMaker
*by order*, every part hanging from a place in one stream of text. The
first can put two parts side by side and cannot carry a part along a
text; the second cannot do the first and does the second for nothing.
They are the web's two layouts again, a grid of boxes and a flow.

## 14. A talk is an outline: PowerPoint

**TinyPowerPoint** (Robert Gaskins and Dennis Austin, Forethought,
1987 -- first called Presenter; black and white, for overhead
transparencies; Microsoft bought the company three months later). Its
lessons:

- **The outline is the model** (`appkits/slides/Outline`): a line
  against the edge is a slide's title, an indented line a point on it,
  and the slides are made from that text whenever it changes. Typing
  on a slide edits the outline line it came from (`lines_of`,
  `line_span`), so every view follows at once.
- **The master** is the look of every slide said once: a style sheet
  for pages.
- **A drawing is a value, so it scales**: the slide is drawn once, as a
  list of shapes, and the editor, the sorter's thumbnails and the
  full-screen show are that list grouped and scaled -- no second
  renderer.
- **Two undos, on purpose**: the outline's own (its piece table) and
  the deck's (the master and the parts), so that undoing a look never
  undoes typing.

## 15. A program you can open: HyperCard

**TinyHyperCard** (Bill Atkinson, 1987; HyperTalk with Dan Winkler) is
the "no line between using and building" corner of the related-work
table, and two ideas:

- **Backgrounds**: the buttons and fields every card shares live on
  the background, once, but each card keeps its own text in the
  background's fields -- a card is a record, a background field a
  column.
- **The message path** (`appkits/hypertalk`): a click sends "mouseUp"
  to the button; what it does not answer, or answers and **passes**,
  goes to the card, then the background, then the stack. So the stack's
  script can number every card on "openCard", and a word on a line of
  its own is a message of your own, sent up the same path --
  inheritance by position, with no classes.

The language knows nothing of cards: the stack is given to it as a
record of functions, threaded through as a value, so its tests run
scripts against three strings in a list. And the fields, when
browsing, are the toolkit's text areas asked for every frame -- which
is immediate mode taken at its word, and the reason a field could not
be opened by the click that finds it (a text area takes the focus only
from a click it saw both halves of).

## 15b. The office suite today: TinyOffice

The programs above are each a period piece, true to the program they
are named after -- and so each has its original's limits: five
editors that cannot hold each other; TinyOpenDoc's parts laid out by
position, in a tree of rows and columns, never floating, never with a
text running round them; TinyFrameMaker's frames laid out by order,
anchored below a line, as wide as their column. **TinyOffice** is the
suite people know now, as a separate program so that the others keep
their history:

- a **start screen**, the kind of document first -- document,
  spreadsheet, presentation, picture, drawing;
- **every kind a host**: the sheet, picture and drawing documents are
  a part of their own kind as their content, the document and the
  presentation a text, and any of them holds objects of the others --
  OLE's shape (applications embedding each other) rather than
  OpenDoc's (parts without applications);
- **free-floating objects**, anywhere on the page, dragged, resized by
  their corners (scaled if they have a size of their own, §9),
  brought forward or sent back;
- **text running round them**, as Publisher and Pages do: `Page.layout
  ~around` cuts each line to the widest stretch the objects beside it
  leave, and goes on below one that leaves no room -- so dragging an
  object reflows the text live, the layout being worked out every
  frame;
- **in-place editing with OLE 2's menu merging**: click an object
  again, and the bar keeps the host's File and takes the object's
  menus for the rest, until Escape.

Two small changes let the parts be a document's whole content: a sheet
part of any number of cells (`Part_sheet.make ?cols ?rows`), a
drawing part as tall as a page (`Part_drawing.make ?max_height`).

Four more things a modern suite does, each small once the text runs
round boxes:

- **text on both sides** of an object (Arrange > Wrap Both Sides,
  Word's "Square"): `Page.layout ~both` fills every stretch wide
  enough, left to right, instead of the widest. A line is still one
  `line`, its glyphs in x order, so the caret and the clicks work
  unchanged -- the caret simply jumps the object as it goes along;
- **pages**: the document's text is *one* tall layout over all its
  pages, with a box across the page's width over each bottom margin,
  gap and next top margin. A line that would reach one goes on below
  it, on the next page: pagination as text round boxes nobody drew.
  (TinyFrameMaker pours the text through one column after another
  instead, `Flow`, which it needs for its columns and master pages.)
  The pages are scrolled, and follow the caret as it is typed;
- **objects that move with the text** (Arrange > Move with Text): an
  object tied to the start of a paragraph, its y kept from that
  paragraph's line. Where is the line? Wherever the layout puts it --
  and the layout depends on where the objects are. The circle is
  broken with two layouts: the first without the tied objects, to find
  their paragraphs, the second with everything, the one shown. The
  anchors are offsets in the text, so every edit shifts the ones after
  the caret by what it typed or deleted, as a mark in Emacs moves;
- **a chart linked to a sheet** (Insert > Chart): the one object that
  holds not its content but *where its content is* -- the sheet that
  is the document, or a sheet object by an id that stays the same as
  objects come and go. It is made again from the sheet's cells every
  time it is drawn (`Part_chart.of_sheet`), so its bars move as a
  number is typed, even while the sheet is being edited in place.
  That is OLE's *linking* beside its embedding: an embedded object is a
  copy, a linked one a reference -- and, like a reference, it can
  dangle, so a chart whose sheet is deleted keeps the last numbers it
  had.

Then three follow-ups:

- **each object its own wrapping** (wider side, both sides, top and
  bottom, in front of the text). `Page` did not change: its one rule is
  still "fill every stretch a line is left" (`~both`), and the four
  ways are four boxes an object gives it. "Top and bottom" is a box as
  wide as the text; "wider side" is the object's box stretched to the
  edge on its narrower side, so that the only stretch left is the wider
  one; "in front" is no box at all. The frames of every other kind came
  out pixel for pixel as before, which is the test that the old rule --
  take the widest stretch -- was one of the four;
- **headers and footers with fields**: two more texts, laid out on
  their own in each page's top and bottom margin. The keys go to one of
  three texts now (`area`: the body, or a header or footer *on a
  page*, for its caret), and a click in a margin chooses. "page {page}
  of {pages}" is filled in per page by selecting each code and typing
  its value over it (`Rich.insert` over a selection keeps its look) --
  but not in the one being edited, which shows its codes: a field's
  offsets have to stay those of the text the caret is in, the reason
  Word toggles between codes and results;
- **the show**: the view split into the pages' content, drawn with or
  without its chrome (caret, selections), and the frame round it --
  so the show is the same content, without chrome, grouped and scaled
  to the screen, as TinyPowerPoint's thumbnails are.

## 15c. Saving: a value, written down

Every app but TinyBravo now saves (`plan_io.md`), and how shows what
the earlier sections built. A document is a value, so saving it is
Marshal (`appkits/document/Saved`: the value behind a line naming the
app and a version, which is the only check Marshal's reading gets).
What a document is *made* of decides what is written:

- the sheet, the text, the bitmap, the drawing, the stack are data,
  and are written as they are;
- a document holding parts is not: a part is a record of functions
  (§9), which only the very same program could read back, and
  js_of_ocaml not at all. So each part is written as its kind and what
  it saves, and read back through the registry. TinyOffice does it
  without a second set of types: its object and document records take
  the part as a parameter, and the saved form is the same records with
  `(kind, text)` where each part was.

Where it goes is the platform's (`Playground_platform.store`: a
directory natively, localStorage on the web), and the right to go
there is a *capability* (`caps`, `plan_caps.md`): the four functions
each take one, which only `Cap.main` hands out, once, in the program's
main. A game whose main does not call it cannot touch a file, and its
type says so.

The menu is shared (`File_menu`), immediate mode like the rest:
the app asks for it in its update and, while a dialog is up, gives it
the frame instead of its own input. Two apps keep their period's way:
TinyVisiCalc's `/S S` and `/S L` (the same `.sheet` file TinyExcel
opens -- one engine, one file), and TinyHyperCard, which like
HyperCard has no Save at all: once a stack has a name, every change is
written as it is made.

## 16. The numbers

Measured 2026-09-21, lines of code (not blank, not comments) and, in
brackets, all lines -- the rest being the comments this repository
teaches with:

| | code (all) |
|---|---|
| `gui/`, the four architectures and everything under them (12 modules) | 1,073 (1,645) |
| of which `Immediate`, the one the playground uses | 221 (326) |
| `Retained` 84, `Mvc` 13, `Mvu` 94 -- the other three, over the same `Look` | |
| `Gui`, the API | 67 (122) |
| 7GUIs four ways (`examples/gui4/`), per task and architecture | see §4 |
| `appkits/`, the engines (19 modules) | 1,708 (2,404) |
| `apps/`, the eight applications and their parts | 2,421 (3,649) |

Each app's own code, next to what it rests on:

| app | code (all) | its engine in `appkits/` |
|---|---|---|
| TinyVisiCalc | 190 (309) | sheet: 413 (551) |
| TinyExcel | 159 (279) | the same, and sheet_view: 90 (137) |
| TinyBravo | 137 (252) | richtext: 266 (409) |
| TinyWord | 285 (431) | the same |
| TinyMacPaint | 334 (518) | paint: 259 (361) |
| TinyOpenDoc (+ its three parts) | 200 + 253 (325 + 328) | embed: 123 (171) |
| TinyPowerPoint | 395 (550) | slides: 63 (88) |
| TinyHyperCard | 439 (606) | hypertalk: 367 (430) |

For scale, one original whose source is public: MacPaint 1.x, released
by the Computer History Museum in 2010, is about 5,800 lines of Pascal
and, depending on the version counted, 2,700 to 3,600 of 68000
assembly -- on top of QuickDraw. TinyMacPaint with its engine is some
600 lines of code, on top of the playground: a page, as the plan said,
and the page is the lesson.

The golden frames at the time of writing: 173 in the 2D suite, of which
48 are the GUI examples and apps (and 5 more since: CRUD's three,
GuiFourWays' Flight Booker and Timer); unit tests: 55 in `gui/tests`
(57 since, with the list box and the shared caret), 4 in
`examples/gui4/tests` (the four ways, frame by frame; 5 since, with
Circle Drawer), 79 in
`appkits/tests`.

## 17. Compared with GTK, Qt, Dear ImGui, Flutter, React, Elm, and the real office

**What they have that we don't.** Mostly things that do not change the
architecture: an accessibility tree for
screen readers, input methods for scripts typed in several keystrokes,
text shaping and right-to-left text (HarfBuzz, Pango), high-DPI
scaling, the system clipboard and drag and drop between programs,
printing. The real office adds file formats (OOXML's specification is
thousands of pages), compatibility with every earlier version of
itself, macros, and several people editing at once. None of these is
here; §18 has the few that would fit.

**Where each one sits against ours.** GTK and Qt are §4's callbacks
(signals), and Qt's item views are MVC with the controller folded into
the view. Flutter and React are MVU with a diff -- and both keep a
retained tree underneath, whose nodes hold what the rebuilt description
cannot: a Flutter `State`, a React hook's value, found by the node's
place in the tree (or its key). That is §3's id problem again, keyed by
position in the tree where `gui/Immediate` keys by rectangle and Dear
ImGui by a hash of the label. Dear ImGui is `gui/Immediate` with its
drawing batched for the GPU and ten years of widgets. Elm is
`Playground.game` with a message type, and the browser underneath
holding the caret (§4). The landscape at length is
[`notes_gui_related_work.md`](../related-work/notes_gui_related_work.md).

## 18. What's missing, and exercises

Checked against the code; in rough order of difficulty:

- scrolling a list box with the wheel: `Immediate.list` does not show
  rows past its bottom, and `Widget.input` already has `wheel`;
- undo that groups typing -- a word, or a pause, as one step:
  `Text_edit` makes every edit its own, and §8's "what counts as one
  edit" is the same question for keys;
- find and replace in TinyWord: a search over `Rich`'s text, the match
  made the selection, the replacement typed over it (which keeps its
  look, §15b);
- absolute references (`$A$1`) in `Formula`'s parser and in
  `Formula.shift`, which leaves them alone -- until then TinyExcel's
  Fill Down of a formula that reads a fixed cell gives nonsense;
- the system clipboard: `Clipboard` lives in the program; a
  `Playground_platform` function for it (SDL's clipboard natively, the
  browser's asynchronous one on the web), behind a capability as
  `store` is (§15c);
- acting on a resize: `Playground`'s `Resized` is a TODO; pass the
  window's size to the app and let `gui/Layout`'s constraints start
  from it (§5);
- Knuth-Plass in the word processors: `Page.layout` breaks greedily;
  give it `Linebreak.optimal`'s breaks, then justify them. In
  `Linebreak` itself: TeX's active nodes (it is O(n^2) as written),
  fitness classes, and hyphenation with Liang's patterns;
- a piece *tree* instead of `Text_edit`'s list (VS Code's), for edits
  in O(log n) in a long text;
- a bitmap shape in the Playground, so that TinyMacPaint stops drawing
  its picture as thousands of rectangles (§13) -- a change to every
  backend;
- an accessibility tree: `gui/Immediate` knows, every frame, every
  widget's kind, label and box; collect them into a list a screen
  reader could walk. The list is the easy half; handing it to the
  platform (AT-SPI, UI Automation, NSAccessibility, ARIA in the
  browser) is the hard one.

## Glossary

- **Widget**: a rectangle with a drawing, a hit test and some state.
- **Immediate mode**: widgets that exist only during the frame that
  draws them; **retained mode**: widgets as objects that persist.
- **Callbacks / MVC / MVU / immediate**: the four answers to "where
  does the interface's state live" (§4).
- **7GUIs**: seven benchmark tasks for comparing GUI approaches.
- **Constraints down, sizes up**: Flutter's layout rule; **geometry
  manager**: Tk's separate placement object.
- **Hit testing**, **capture**, **focus**, **tab order**: how input
  finds the right widget.
- **Gap buffer**, **piece table**: the two classic editable-text
  structures; **Knuth-Plass**: optimal paragraph line breaking.
- **Document as a value**: an edit returns a new document, so undo is
  a list and replay is free.
- **Component**, **in-place activation**, **compound document**: one
  document embedded in another, and editing it where it sits.
- **Recursive descent**: one function per rule of a grammar;
  **topological order**: computing nothing before what it reads;
  **Kahn's algorithm**: the way to find one, and to find a cycle.
- **Geometry manager**: Tk's separate placement object; **grid** is
  the one that makes columns line up across rows, which a column of
  rows cannot do.
- **Run**: a stretch of text in one look; **typing style**: the look
  waiting on the caret for what is typed next; **modal**: a keyboard
  whose keys mean different things depending on a mode (Bravo, vi).
- **Pattern**: an 8 by 8 tile laid from the picture's origin; **seed
  fill**: the bucket, finding an area a row at a time; **PackBits**:
  the Mac's run-length compression.
- **Master**: the look of every slide, said once.
- **Message path**: button, card, background, stack -- where a
  HyperTalk message goes until something answers it; **pass**: answer
  it and send it on anyway.

## References

- A. B. Kahn, "Topological sorting of large networks", Communications
  of the ACM 5(11):558-562, 1962.
- Jack E. Bresenham, "Algorithm for computer control of a digital
  plotter", IBM Systems Journal 4(1):25-30, 1965.
- Douglas C. Engelbart, William K. English, "A Research Center for
  Augmenting Human Intellect", AFIPS Fall Joint Computer Conference,
  1968.
- Alan Kay, Adele Goldberg, "Personal Dynamic Media", IEEE Computer
  10(3), 1977.
- Trygve Reenskaug, "Models - Views - Controllers", Xerox PARC
  technical note, 1979.
- C. P. Thacker, E. M. McCreight, B. W. Lampson, R. F. Sproull, D. R.
  Boggs, "Alto: A Personal Computer", Xerox PARC report CSL-79-11,
  1979.
- Alvy Ray Smith, "Tint Fill", SIGGRAPH '79.
- Donald E. Knuth, Michael F. Plass, "Breaking Paragraphs into Lines",
  Software: Practice and Experience 11(11):1119-1184, 1981.
- David Canfield Smith, Charles Irby, Ralph Kimball, Bill Verplank,
  Eric Harslem, "Designing the Star User Interface", Byte 7(4), 1982.
- Adele Goldberg, David Robson, "Smalltalk-80: The Language and its
  Implementation", Addison-Wesley, 1983.
- Franklin Mark Liang, "Word Hy-phen-a-tion by Com-put-er", PhD thesis,
  Stanford University, 1983.
- Donald E. Knuth, "The TeXbook", Addison-Wesley, 1984 (boxes and
  glue).
- Butler W. Lampson, "Personal Distributed Computing: The Alto and
  Ethernet Software", ACM Conference on the History of Personal
  Workstations, 1986 (Bravo).
- Danny Goodman, "The Complete HyperCard Handbook", Bantam Books, 1987.
- Glenn E. Krasner, Stephen T. Pope, "A Cookbook for Using the
  Model-View-Controller User Interface Paradigm in Smalltalk-80",
  Journal of Object-Oriented Programming 1(3), 1988.
- Andrew J. Palay et al., "The Andrew Toolkit: An Overview", USENIX
  Winter Conference, 1988.
- Alan C. Kay, "The Early History of Smalltalk", HOPL-II, ACM SIGPLAN
  Notices 28(3), 1993.
- John K. Ousterhout, "Tcl and the Tk Toolkit", Addison-Wesley, 1994.
- Charles Crowley, "Data Structures for Text Sequences", University of
  New Mexico, 1998 (the gap buffer and the piece table compared).
- Casey Muratori, "Immediate-Mode Graphical User Interfaces", video
  lecture, 2005.
- Computer History Museum, the MacPaint and QuickDraw source code,
  released 2010.
- Evan Czaplicki, "Elm: Concurrent FRP for Functional GUIs", senior
  thesis, Harvard University, 2012.
- Robert Gaskins, "Sweating Bullets: Notes about Inventing
  PowerPoint", Vinland Books, 2012.
- Evan Czaplicki, Stephen Chong, "Asynchronous Functional Reactive
  Programming for GUIs", PLDI 2013.
- Eugen Kiss, 7GUIs, master's thesis, Leibniz University Hannover,
  2014.
