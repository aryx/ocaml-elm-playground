# Plan: TinySqueak, the environment written in Smalltalk

## Context

TinySmalltalk80 (`done/plan_tiny_smalltalk.md`) is the Blue Book's
Smalltalk-80 and its environment, with one departure from the
original: its windows are drawn by OCaml, reading the object memory,
where Smalltalk-80's were Smalltalk objects open to the Browser. The
first item of `remaining/plan_tiny_smalltalk_remaining.md` is to undo
that departure.

Squeak (Dan Ingalls, Alan Kay, Ted Kaehler, John Maloney, Scott
Wallace; Apple, 1996, then Disney) is the natural place to do it,
because Squeak did it too: "Back to the Future" (OOPSLA 1997) took the
Smalltalk-80 image of 1983 and made it live again, the whole system in
Smalltalk, the virtual machine included. What it added, and what each
could be here:

1. **Morphic**: the environment as live objects (from Self, John
   Maloney and Randall Smith, 1995). Everything on the screen is a
   morph: picked up by the hand, resized and inspected through its
   halo (a ring of handles), animated by its `step` method, composed
   of submorphs. The screen is redrawn only where something changed
   (damage rectangles). It is the visible difference from
   Smalltalk-80, and simpler to write than MVC.
2. **The virtual machine written in Smalltalk**: Slang, a subset of
   Smalltalk, run as a simulator inside Smalltalk to debug the VM,
   then translated to C. The Blue Book's part four is already such a
   program.
3. **Real closures** (Eliot Miranda, 2008): blocks that can be
   reentered (`remaining/plan_tiny_smalltalk_remaining.md`, item 2).
4. **Colour**: Forms of 8 and 32 bits, BitBlt over them.
5. **Etoys** (Alan Kay and the Squeakland team, around 1997-2000):
   scripts built by dragging tiles onto a morph, for children -- the
   ancestor of Scratch (MIT, 2007), itself first written in Squeak.

**The idea to teach**: the host shrinks. TinySqueak's OCaml shows the
Display and gives the mouse and the keyboard to Smalltalk, and nothing
else; the Browser, the Workspace, the Inspector, the menus, the text,
are Smalltalk code the Browser can show and change. That is how
Squeak's own VM is: a few thousand lines of C under a world of objects.

**The tiny rule**, as for TinySmalltalk80: a core small enough to read,
with everything left out listed.

## Decisions

- **One library, two kernels** (decided, 2026-10-03): TinySqueak on
  the same `languages/smalltalk`, booted from a second set of kernel
  files (the Blue Book's kernel, plus closures, colour and Morphic), so
  that the interpreter's improvements serve both, and the older,
  simpler kernel can still be read and inspected alone:
  TinySmalltalk80 keeps booting the Blue Book's kernel only, and
  nothing of Squeak's is added to those files.
- **Over the budget** (decided, 2026-10-03): TinySqueak carries a
  whole system, as TinySmalltalk80 and TinyChrome do; it gets its line
  in `Unit_catalog.ml`'s `over_budget`.

- **Squeak 1.x first** (decided, 2026-10-03): 1996-1998, Morphic
  arriving, colour; Etoys a later phase; the Squeak 3.x years
  (Monticello, traits) out.

## Proposed decisions (to confirm)

- **Direct pointers or the object table**: Squeak dropped the object
  table in 1996 and pays for `become:` with a scan of memory. Keeping
  the table is simpler here; the change is an exercise, and the notes
  compare the two.

## The prerequisite: speed

With the whole screen drawn by interpreted Smalltalk, the web version's
1.6 million bytecodes a second (TinySmalltalk80's interpreter under
node) is not enough. Before Morphic:

- the interpreter's speedups (`remaining/plan_tiny_smalltalk_remaining.md`,
  item 3): the header decoded once per send, contexts recycled, `at:`
  and `at:put:` done by their bytecodes;
- BitBlt a row or a word at a time, not a pixel at a time
  (`St_bitblt.mli`'s exercise);
- Morphic's damage rectangles: only what changed is redrawn.

Measured before and after, natively and under node, with the numbers
in `dev/notes_opti_ocaml.md`.

## Phases

- **Q0, the plan** (this file), then `notes_squeak.md`: Morphic, the
  shrinking host, closures, Slang, a section each with its worked
  example.
- **Q1, closures** (done, 2026-10-03: `kernel/squeak/Closures.st`,
  `St_compile`'s two passes, bytecodes 138 and 140 to 143,
  `notes_squeak.md` section 1, `Unit_squeak.ml`; a block-heavy loop
  runs 1.4 times slower than with the Blue Book's blocks, Q2's
  business; left: the debugger's variables in a closure's
  activation): `BlockClosure` (its outer context, its copied
  values, its start), temporaries that outlive their method in a
  shared temp vector, the bytecodes for them, the compiler emitting
  them. Worked example: `fact := [:n | n < 2 ifTrue: [1] ifFalse: [n *
  (fact value: n - 1)]]` answers 120 for 5; two blocks made in one loop
  keep their own `i`.
- **Q2, speed** (done, 2026-10-03: `St_bench`, contexts recycled,
  the header read once -- a send 1.5 times faster natively, 1.3 under
  node; `at:` and `at:put:` by their bytecodes; BitBlt a byte at a
  time, 7 times faster; the receiver's fields in a register tried and
  not kept; numbers in `notes_opti_ocaml.md` section 19): the items
  above, measured.
- **Q2b, the spike, kept** (done, 2026-10-03:
  `kernel/morphic/MiniMorphic.st`, `St_kernel.mini_morphic`,
  `Unit_minimorphic.ml`, `notes_squeak.md` section 2. Its answer: 50
  morphs moving at once cost 61,000 bytecodes a cycle, 19 ms under
  node; 200, 72 ms. Morphic, where most morphs stand still, is within
  reach in a browser; a screen where everything moves is not. Not yet
  on a screen: no program shows it, Q6's host will). Asked 2026-10-03: before Morphic proper,
  a minimal one -- a world, a hand, `RectangleMorph`s bouncing, damage
  rectangles -- timed under node to say whether Q4 is feasible. Not
  thrown away after: saved on its own (a small file of Smalltalk
  beside Squeak's kernel, booted alone, with its test and a section of
  `notes_squeak.md`), as the intermediate step to study -- Morphic in a
  few hundred lines before the real one, as the Blue Book's kernel
  stays beside Squeak's.
- **Q3, colour and text** (done, 2026-10-03: `St_colorblt`,
  `kernel/squeak/Color.st` and `Text.st`, `Unit_colour.ml`,
  `notes_squeak.md` section 3. A fill and a store a row at a time,
  120 times faster natively, 6 to 13 under node; the font drawn by a
  Pen from Hershey's strokes the first time it is asked for, 300,000
  bytecodes. Left: text is blended a pixel at a time, a line of 40
  characters 3.5 ms under node -- a glyph's zeros to skip if Q4's
  damage does not save enough; the Display still has one bit, Q6's
  host makes it 32; a Form of 32 bits on one of 8, an exercise):
  Forms of 8 and 32 bits, a colour BitBlt
  (its rules, and alpha blending, Squeak's rule 24), `Color`; a font as
  a Form of glyphs and a table of offsets (the strike format), text
  drawn by BitBlt, a glyph at a time. Hershey's strokes
  (`graphics/font`) rendered once into such a Form.
- **Q4, Morphic** (done, 2026-10-03: `kernel/squeak/Morphic.st` and
  `Morphs.st`, booted by `St_kernel.squeak`, MiniMorphic now on
  Squeak's kernel without them; `Unit_morphic.ml`, St_bench's
  "morphs", `notes_squeak.md` section 4; the host's `keyboard` and
  primitive 92. 50 atoms, ellipses: 185,000 bytecodes a cycle, 17 ms
  natively, 66 under node; nothing changed, 800. Left: the halo's
  inspect handle, with Q5's Inspector; a TextMorph redraws all its
  lines at each key, has no selection and does not wrap, Q5's
  Workspace needs the first two; a world is on a Form given to it,
  the Display untouched until Q6): `Morph` (bounds, colour, submorphs, owner, `drawOn:`,
  `step`), `PasteUpMorph` (the world), `HandMorph` (the mouse, what it
  carries, events dispatched to the morph under it), the halo (move,
  resize, duplicate, delete, inspect), `changed` and the world's damage
  list, redrawn once a cycle; `RectangleMorph`, `EllipseMorph`,
  `StringMorph`, `TextMorph` (editing), `SystemWindow`, menus.
- **Q5, the tools as morphs** (done, 2026-10-03:
  `kernel/squeak/Tools.st` -- Workspace, Transcript's window,
  Inspector, Browser -- over `TextMorph` (selection, scrolling, the
  yellow button's do it, print it, inspect it, accept) and `ListMorph`;
  evaluation by compiling a `DoIt` method, no evaluator; primitive
  159, a method's source; the halo's inspect handle; `Unit_tools.ml`,
  St_bench's "tools", `notes_squeak.md` section 5. The Browser opened
  142,000 bytecodes, a selector picked 117,000 (51 ms under node), a
  character typed 43,000. Left: no scroll bars (a list scrolls by
  dragging past its edge, a text follows its cursor); no debugger as
  a morph, an error stops the cycle and the host goes on with the
  next, Q6's host to show why; senders, implementors, removing a
  method): a Workspace, the System Browser, an
  Inspector, the Transcript -- the same Browser that shows `Morph`'s
  methods, `Morph>>drawOn:` changed and accepted, every morph redrawn
  its new way.
- **Q6, TinySqueak** (done, 2026-10-04: `apps/devtools/TinySqueak.ml`,
  the whole host in 250 lines -- the mouse, the keys, the
  world's cycle a process run a budget a frame, the Display's pixels
  when BitBlt drew, an error said by `Transcript showError:`; its
  first screen the Browser on `EllipseMorph>>drawOn:` beside
  `BouncingAtomsMorph`, and a `PartsBinMorph`; five golden frames,
  the CATALOG row, the web page, `over_budget`; `notes_squeak.md`
  section 6. Left: the painted car, with Q7's Etoys; the image saved
  and loaded, booting from it (2.5 s in a browser without); a text
  does not wrap, a long line is cut at the pane's edge): `apps/devtools/TinySqueak.ml`, the smallest host
  possible (the Display shown, the Sensor and the keyboard fed, the
  world's process run a budget a frame); Squeak 1.x's look; the
  classic demos: bouncing atoms (an ideal gas as morphs), a flap of
  parts to drag out, a painted car. Golden frames, CATALOG row, web
  page.
- **Q7, Etoys** (done, 2026-10-04: `kernel/squeak/Etoys.st`, 370
  lines -- `forward:`, `turn:` and a heading on any morph, `CarMorph`
  drawn turned, `ViewerMorph`, `PhraseTileMorph`, `NumberTileMorph`,
  `ScriptEditorMorph` whose step does its phrases; Morphic's drop
  into the morph that wants it; the halo's viewer handle;
  `Unit_etoys.ml`, the car and its ticking script on TinySqueak's
  first screen, `notes_squeak.md` section 7. No change to the
  interpreter nor to the Blue Book's kernel. Left: the car painted
  (no painting tools, no WarpBlt: it is drawn by a method); tests and
  variables; a script as Smalltalk text. Also: TinySqueak's flag
  `kernel=mini` shows MiniMorphic, what Q2b had left to Q6): a morph's viewer (its properties and commands as
  tiles), scripts made by dragging tiles, run by `step` -- the car
  driven by `forward: 5. turn: 5`, the classic first Etoy.
- **Q8, the VM in Smalltalk**: the Blue Book's interpreter written in
  Smalltalk, run as a simulator on ours (slowly: an interpreter in an
  interpreter), running a small image; translating that Slang to OCaml
  an exercise.

## Where it goes

- `languages/smalltalk/`: the interpreter's closures and speed;
  `kernel/` the Blue Book's kernel as now; a second set,
  `kernel/squeak/` (closures, Color, Morphic, the tools), booted by
  `St_boot.boot ~kernel:`.
- `apps/devtools/TinySqueak.ml`, its `software/` and `web/` twins.

## Later: the kernels in the code map

Most of TinySqueak is Smalltalk (the kernels' `.st` files), which
tinybox's code map does not show: it counts and draws `.ml`, `.mli`
and C. To come, at some point (asked 2026-10-03, not a phase yet):

- `Highlight_st`, a Smalltalk highlighter giving `Highlight_code`'s
  spans, as `Highlight_ml` does for OCaml. `St_lexer`'s tokens know
  where they start and stop, but it drops the comments, which a
  highlighter must keep. The categories: a chunk file's `!Class
  methodsFor: '...'!` line a section, a method's selector a
  `Def_function`, a class defined a `Def_type`, a capitalized name a
  global, the pseudo-variables keywords.
- the `.st` files among the code map's sources, in their folder
  (`kernel/`, `kernel/squeak/`), counted by `Code_deps` as part of
  the program's own code; a file's uses of another being the classes
  it names. The budget is no concern for TinySmalltalk80 and
  TinySqueak (decided, 2026-10-03): both are on `over_budget`, however
  many lines their kernels add.

## Left out (exercises, or never)

Squeak's own image (the same reason as Smalltalk-80's: not ours, and
only runnable, not explained); Monticello and the package system;
networking and the web browser Scamper; sound and the FM synthesizer
(here, `audio/` could stand in); Balloon's anti-aliased vector
graphics; 3D (Wonderland, Croquet); the Slang to C translation.

## References

- Dan Ingalls, Ted Kaehler, John Maloney, Scott Wallace, Alan Kay,
  "Back to the Future: The Story of Squeak, a Practical Smalltalk
  Written in Itself" (OOPSLA 1997).
- John Maloney, Randall Smith, "Directness and Liveness in the Morphic
  User Interface Construction Environment" (UIST 1995); John Maloney,
  "An Introduction to Morphic: The Squeak User Interface Framework"
  (2001).
- Mark Guzdial, *Squeak: Object-Oriented Design with Multimedia
  Applications* (2001); Stéphane Ducasse et al., *Squeak by Example*
  (2007).
- Eliot Miranda, the closure compiler (Cog blog, 2008).
- Alan Kay, "Squeak Etoys, Children and Learning" (2005).
- Bert Freudenberg et al., SqueakJS (2014): Squeak in a browser.
