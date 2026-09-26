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

## Proposed decisions (to confirm)

- **Squeak 1.x first** (1996-1998: Morphic arriving, colour); Etoys a
  later phase; the Squeak 3.x years (Monticello, traits) out.
- **One library, two kernels**: TinySqueak on the same
  `libs/languages/smalltalk`, booted from a second set of kernel files
  (the Blue Book's kernel, plus closures, colour and Morphic), so that
  the interpreter's improvements serve both. The alternative, a frozen
  Blue Book library and a diverging copy for Squeak, keeps
  TinySmalltalk80's code exactly the book's, at the price of two
  interpreters.
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
- **Q1, closures**: `BlockClosure` (its outer context, its copied
  values, its start), temporaries that outlive their method in a
  shared temp vector, the bytecodes for them, the compiler emitting
  them. Worked example: `fact := [:n | n < 2 ifTrue: [1] ifFalse: [n *
  (fact value: n - 1)]]` answers 120 for 5; two blocks made in one loop
  keep their own `i`.
- **Q2, speed**: the items above, measured.
- **Q3, colour and text**: Forms of 8 and 32 bits, a colour BitBlt
  (its rules, and alpha blending, Squeak's rule 24), `Color`; a font as
  a Form of glyphs and a table of offsets (the strike format), text
  drawn by BitBlt, a glyph at a time. Hershey's strokes
  (`graphics/font`) rendered once into such a Form.
- **Q4, Morphic**: `Morph` (bounds, colour, submorphs, owner, `drawOn:`,
  `step`), `PasteUpMorph` (the world), `HandMorph` (the mouse, what it
  carries, events dispatched to the morph under it), the halo (move,
  resize, duplicate, delete, inspect), `changed` and the world's damage
  list, redrawn once a cycle; `RectangleMorph`, `EllipseMorph`,
  `StringMorph`, `TextMorph` (editing), `SystemWindow`, menus.
- **Q5, the tools as morphs**: a Workspace, the System Browser, an
  Inspector, the Transcript -- the same Browser that shows `Morph`'s
  methods, `Morph>>drawOn:` changed and accepted, every morph redrawn
  its new way.
- **Q6, TinySqueak**: `apps/devtools/TinySqueak.ml`, the smallest host
  possible (the Display shown, the Sensor and the keyboard fed, the
  world's process run a budget a frame); Squeak 1.x's look; the
  classic demos: bouncing atoms (an ideal gas as morphs), a flap of
  parts to drag out, a painted car. Golden frames, CATALOG row, web
  page.
- **Q7, Etoys**: a morph's viewer (its properties and commands as
  tiles), scripts made by dragging tiles, run by `step` -- the car
  driven by `forward: 5. turn: 5`, the classic first Etoy.
- **Q8, the VM in Smalltalk**: the Blue Book's interpreter written in
  Smalltalk, run as a simulator on ours (slowly: an interpreter in an
  interpreter), running a small image; translating that Slang to OCaml
  an exercise.

## Where it goes

- `libs/languages/smalltalk/`: the interpreter's closures and speed;
  `kernel/` the Blue Book's kernel as now; a second set,
  `kernel/squeak/` (closures, Color, Morphic, the tools), booted by
  `St_boot.boot ~kernel:`.
- `apps/devtools/TinySqueak.ml`, its `software/` and `web/` twins.

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
