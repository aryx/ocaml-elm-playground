# GUIs, from scratch: a tutorial for `gui/`

How a program is built around the person using it: what a widget
actually is, where the state lives (four rival answers, one of which
this playground already is), how things are laid out, how text is
edited, and how one document ends up inside another.

It is the specification of the toolkit planned in
[`plan_gui_teaching.md`](../plans/plan_gui_teaching.md): written
before the code, to be checked against it and have its numbers filled
in. Companions:
[`notes_gui_related_work.md`](../related-work/notes_gui_related_work.md)
(Smalltalk, Tk, Qt, Flutter, React, Dear ImGui, acme, OLE) and
[`notes_inspect.md`](notes_inspect.md), whose panels and timeline are
this toolkit's first customer.

## 0. Where the code is, and a reading order

| module (`gui/`) | what | section |
|---|---|---|
| `Widget` | what a widget is: a rectangle, a drawing, a hit test, some state | §2 |
| `Immediate` | the toolkit `playground/Gui` is built on | §3 |
| `Retained`, `Mvc` | the same widgets, wired the other two ways | §4 |
| `Layout` | constraints down, sizes up | §5 |
| `Focus` | who gets the keys | §6 |
| `Text_edit` | a piece table, a cursor, a selection, undo | §7 |
| `appkits/document` | a document as a value; undo; the clipboard | §8 |
| `appkits/embed` | one document inside another | §9 |
| `playground/Gui` | the Evan-style API over all of it | §10 |

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

The honest summary, which the `.mli`s will repeat: **callbacks scatter
the truth, MVC guards it, MVU rebuilds from it, immediate mode never
stores anything but it.** And the harness that makes this more than an
opinion is **7GUIs** (Eugen Kiss, 2014): seven tasks picked so that
each architecture's weakness shows up in at least one -- validation in
Flight Booker, selection in CRUD, undo and dialogs in Circle Drawer,
and a dependency graph in Cells.

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
  else.
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

Then, for a word processor, the one real algorithm: **Knuth and
Plass's line breaking** (1981). Greedy breaking -- fill each line until
the next word does not fit -- is what browsers and most editors do and
leaves rivers and lonely short lines. Knuth-Plass instead scores a
whole paragraph (each line's "badness", plus penalties for
hyphenation and for consecutive breaks) and picks the *globally* best
set of breaks by dynamic programming. It is the reason a TeX paragraph
looks the way it does, and it is perhaps two hundred lines.

## 8. A document is a value

```ocaml
type 'a document = { content : 'a; path : string option; saved : 'a }
```

An edit returns a new document. Then:

- **undo** is a list of past contents, **redo** the list you popped
  off (or the command pattern, which is the same thing with sharing --
  `kits/puzzle/Undo` is this idea for a grid game, already here);
- **dirty** is `content != saved`, a pointer comparison;
- and, the reason it is worth insisting on, **`Inspect` can scrub an
  app**: a word processor whose whole history replays is the
  architecture arguing for itself.

## 9. One document inside another

The idea worth reviving, and the author's own ask. A **component** is
something that can draw itself into a rectangle, take events while it
is active, say how big it would like to be, and serialize itself:

```ocaml
type component = {
  size  : unit -> number * number;
  draw  : number * number -> shape list;
  event : computer -> component;
  save  : unit -> string;
}
```

That is the whole protocol, and everything the 1990s built around it
-- OLE's interfaces and registries, OpenDoc's parts, Bonobo's CORBA --
is plumbing for doing it *across processes and languages*, which we do
not need: in one OCaml program a component is a record of closures.

The host (TinyWord) lays a component out like a very large character,
and hands over the keys when it is **activated** -- OLE 2's "in-place
activation" (1993), the feature that made a spreadsheet inside a
document feel like one program instead of two. In our model that is a
`bool` in the host's state.

The history is worth a paragraph because it is a genuine road not
taken: Andrew (CMU, 1988) did it first, OLE made it a product,
OpenDoc (Apple and IBM, 1992-97) bet a company on it and was cancelled,
and the whole idea faded from applications -- and then came back
everywhere else. A Jupyter notebook cell, a Notion block, an embedded
tweet: compound documents won the web and lost the desktop.

## 10. In the playground

`playground/Gui` is the immediate-mode toolkit of §3, threaded through
the `computer` your `update` already has:

```ocaml
let update computer model =
  if Gui.button computer ~at:(0., 120.) "Reset" then initial
  else { model with gravity =
           Gui.slider computer ~at:(0., 60.) ~from:0. ~to_:2000. model.gravity }
```

no new concepts, no message type, and `view` draws what `update`
declared. The other three architectures live in `gui/` as runnable
comparisons (§4), the 7GUIs tasks are in `examples/Gui7*.ml` four
times over, and the apps -- TinyVisiCalc and TinyExcel over one
engine, TinyWord, TinyMacPaint -- are what the toolkit is *for*
([`plan_gui_teaching.md`](../plans/plan_gui_teaching.md)).

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
