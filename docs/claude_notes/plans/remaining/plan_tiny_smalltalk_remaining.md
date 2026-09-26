# Plan: what's left for TinySmalltalk80

The plan is done: see [`done/plan_tiny_smalltalk.md`](../done/plan_tiny_smalltalk.md)
-- `libs/languages/smalltalk/` (the Blue Book's language, compiled to
its bytecodes, run by its interpreter over an object table, the kernel
in Smalltalk bootstrapped from `kernel/*.st`, `St_debug`, `St_image`,
`St_bitblt`), `apps/devtools/TinySmalltalk80.ml` (the Browser,
Workspace, Transcript, Inspector, notifier and Debugger), and the
tutorial `notes_smalltalk.md`.

What's left, roughly from most to least worth doing. Like the rest,
each piece with its worked example in its `.mli` and its test.

## 1. MVC in Smalltalk: the environment as the language's objects

The plan's one departure from the original, and the biggest step back
to it: the windows written in Smalltalk, as Smalltalk-80's were
(Reenskaug's Model-View-Controller, 1979; Krasner and Pope's cookbook,
1988), so that the Browser can browse the Browser and a change to a
View's method changes the screen at once.

- **Text drawn by BitBlt**: a font as a Form of glyphs and a table of
  their offsets (the Alto's strike format), `Form>>displayString:at:`
  a copyBits per character. Hershey's strokes (`graphics/font`)
  rendered once into such a Form give the look TinySmalltalk80 has
  today.
- **The classes**: `Model` (dependents, `changed:`, `update:`), `View`
  (a rectangle of the Display, subviews, `displayView`), `Controller`
  (`controlActivity` reading `Sensor`), `StandardSystemView` (the
  title tab, the blue menu), `StringHolder` and its view, `ListView`,
  `PopUpMenu`, `Browser`, `Inspector`, `Debugger` -- the Blue Book's
  names, the Orange Book's behaviour.
- **What OCaml keeps**: the Display shown, the Sensor fed, the
  processes run a budget a frame -- and the scheduler loop (`ScheduledControllers`)
  becomes a Smalltalk process that never ends.
- The first step, small: a `Workspace` view in Smalltalk beside the
  OCaml ones, `StringHolder new openLabel: 'Workspace'`.
- Worked example: `BrowserView`'s `displayView` changed in the Browser
  itself, accepted, the title tabs redrawn in the new way.

## 2. Real closures (Squeak, 2008)

The Blue Book's blocks share their home's temporaries and cannot be
reentered: `fact := [:n | n < 2 ifTrue: [1] ifFalse: [n * (fact
value: n - 1)]]` gives a wrong answer today, and the notes say why.
Eliot Miranda's closure compiler: `BlockClosure` (its outer context,
its copied values, its start pc), temporaries that outlive their method
put in a *temp vector* shared by the method and its blocks, and the
bytecodes for them (push closure, push/store into a temp vector).

- Worked example: the recursive block above answers `120` for 5; two
  blocks made in one loop keep their own `i`.

## 3. A faster interpreter

9.5 million bytecodes a second natively, 1.6 under node: enough, but
the web version runs a do-it at about 15 frames a second.

- The header decoded once per send, not twice (`execute` and
  `activate_method`).
- Contexts recycled: the Blue Book's free list of small and large
  contexts, a returned context reused when nothing references it
  (the Deutsch-Schiffman observation: most never escape).
- `at:` and `at:put:` on Arrays and Strings done by their bytecodes,
  as the arithmetic ones are.
- The numbers written down, before and after, in `dev/notes_opti_ocaml.md`
  (the dragon and `100 factorial printString`, native and node).

## 4. Exceptions, processes and semaphores

- `Process`, `ProcessorScheduler`, `Semaphore`, `Delay` (Blue Book,
  chapter 15): the processes as Smalltalk objects, priorities, `fork`,
  `wait`, `signal`; today's OCaml processes becoming what `Processor`
  schedules.
- Exceptions (ANSI Smalltalk, 1998): `on:do:`, `ensure:`, `ifCurtailed:`,
  `signal`, `retry`, `resume:` -- the handler search up the contexts,
  itself Smalltalk code walking `thisContext sender`.
- Worked example: `[1/0] on: ZeroDivide do: [:e | e return: 0]` is 0;
  `ensure:` runs when a non-local return leaves its block.

## 5. The environment's missing pieces

- The Browser's cross references: senders and implementors of a
  selector, and the method list they open.
- Windows resized (the blue menu's "frame"), scroll bars (today the
  wheel scrolls), text wrapped to its pane, undo.
- The changes file: every accept and do-it logged in the store, the
  crash recovered by replaying it.
- File in: a `.st` from the store read into the running system, as a
  file out writes it.
- The Inspector and the Debugger's variables changed by accept (only
  the Inspector's named fields today); the Debugger's full stack beyond
  200 frames.

## 6. The kernel's gaps

- A class whose instance variables change: its instances mutated with
  `become:` (the Blue Book's way), today left in their old shape.
- Large integers divided by large integers a digit at a time (Knuth's
  algorithm D), not a bit at a time.
- `Float>>printString` in Smalltalk (Steele and White's, 1990), not
  OCaml's `%.15g`.
- The rest of the Blue Book's second part: `Bag`'s and `Dictionary`'s
  full protocols, `Date` and `Time`, `Text` with its emphasis.

## 7. From scratch, further down

- BitBlt a word at a time, Ingalls's shifts and masks, measured
  against today's pixel at a time.
- The compiler in Smalltalk, compiled by the OCaml one, then compiling
  itself -- the image bootstrapped the Squeak way.
- The collector: the Blue Book's reference counting, or generation
  scavenging (Ungar, 1984), beside today's mark and sweep.

## 8. To check

- That the Blue Book's example of compiled code, `Rectangle>>center`,
  is in its chapter 26, as `St_bytecode.mli` and the notes say (the
  bytes themselves are checked by the tests).
