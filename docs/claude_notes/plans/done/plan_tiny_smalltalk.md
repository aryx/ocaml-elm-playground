# Plan: TinySmalltalk80, and libs/languages/smalltalk/

## Context

`apps/devtools/` has three languages, each shown in its own environment:
BASIC with its prompt (TinyBasic), Emacs Lisp with its editor
(TinyEmacs), and Pascal with its IDE and debugger (TinyTurboPascal).
Smalltalk-80's environment is the one they all descend from. It had
overlapping windows, pop-up menus, a class browser, a workspace where
any text can be run ("do it", "print it"), an inspector on any object,
and a debugger on the live stack, where you fix the method and carry
on. All of it was written in the language itself and saved as one
**image**.

It also completes a line the repository already follows. Sketchpad
(TinySketchpad, 1963) had masters and instances, which Alan Kay
credits as one source of objects. Smalltalk (Kay, Ingalls, Goldberg
and others, Xerox PARC, 1972-1980) made them the whole language. The
Mac (TinyMacPaint) and HyperCard (TinyHyperCard, TinyMyst) followed
from it. `libs/gui/Mvc.mli` already cites Reenskaug's Smalltalk-80 MVC,
and `apps/devtools/dune`'s comment lists "a debugger (the Smalltalk-80
one)" as a program still to write. Smalltalk-80 reached the public
through the Byte issue of August 1981 (the balloon cover) and the
"Blue Book" (Goldberg and Robson, 1983). The Blue Book's fourth part
specifies the whole virtual machine, and this plan follows it.

**The author's choice (2026-09-26): our own Smalltalk, not Xerox's
image.** The language, the compiler, the virtual machine and the
kernel classes are all written here, from the Blue Book. We never
load the Xerox virtual image of 1983: it is a binary made by someone
else, its license is unclear, and it could only be run, not
explained. The compiler follows the Blue Book's bytecode set, so the
book remains the reference for every byte.

**The tiny rule**, as for TinyFirefox: a core small enough to read,
with everything left out listed, each item an exercise or a "never".

## The four parts

| Part | What it is | Where |
|---|---|---|
| the language | lexer, parser, AST: Smalltalk-80's syntax, the chunk file format | `libs/languages/smalltalk/` |
| the compiler | the AST to the Blue Book's bytecodes, in `CompiledMethod` objects | same |
| the virtual machine | the object memory, the interpreter over contexts that are objects, the primitives | same |
| the kernel | the classes, written in Smalltalk (`.st` files embedded by dune), bootstrapped at startup | `libs/languages/smalltalk/kernel/` |
| the environment | the screen, windows, Browser, Workspace, Transcript, Inspector, Debugger | `apps/devtools/TinySmalltalk80.ml` |

### The language: Smalltalk-80 as the Blue Book gives it

- literals: numbers (`3`, `-7`, `16r1F`, `3.14`, `1e10`), characters
  `$a`, strings `'it''s'`, symbols `#foo` `#at:put:` `#+`, arrays
  `#(1 $a 'x' #sym (nested))`;
- the three kinds of message and their precedence: unary
  (`3 factorial`), binary (`3 + 4`, applied left to right: `3 + 4 * 2`
  is 14, the first surprise of every newcomer), keyword
  (`a at: 1 put: 2`); cascades `Transcript show: 'a'; cr`;
- assignment `x := 3` (and the Alto's `←`, which was `_` in ASCII, read
  as an alias); return `^x`; statements separated by `.`;
  temporaries `| a b |`;
- blocks `[:x | x + 1]`, with arguments and, as in the Blue Book,
  without temporaries of their own;
- method syntax: a pattern, a comment in double quotes, temporaries,
  `<primitive: 60>`, then statements;
- pseudo-variables: `self`, `super`, `nil`, `true`, `false`,
  `thisContext`;
- the **chunk format** (`!` ending each chunk, `!Foo methodsFor: 'x'!`),
  which is how Smalltalk-80 filed code in and out. The kernel is
  written in it, and the Browser's "file out" writes it.

Left out: Squeak's and Pharo's additions (brace arrays `{}`, byte
arrays `#[]`, pragmas beyond `<primitive:>`, `thisContext` tricks
beyond reading it), and ANSI Smalltalk's exceptions (`on:do:`,
`ensure:`). The Blue Book had none of these, and `ensure:` is an
exercise.

### The compiler: to the Blue Book's bytecodes

We use the Blue Book's bytecode set exactly (chapter 28): push
receiver variable, temporary, literal constant and literal variable;
store and pop; the special pushes (`self`, `true`, `nil`, -1 0 1 2);
jumps; sends with their 32 special selectors (`+` `at:` `==`
`class` ...); the returns. A `CompiledMethod` is an object: a header,
then the literal frame, then the bytes. The Blue Book lays out each
field of the header, and the disassembler prints them.

Like the real compiler, ours **inlines** `ifTrue:ifFalse:`,
`whileTrue:`, `and:` and `or:` into jumps when their arguments are
literal blocks. That is why `true ifTrue: [...]` costs no send, and
why redefining `True>>ifTrue:` changes nothing. The notes show both
sides of that trade.

The compiler is written in OCaml, unlike the original, which was
written in Smalltalk and compiled itself. That makes the bootstrap
simple (below). The compiler in Smalltalk is an exercise, and the most
Smalltalk one of them.

Worked example, from the Blue Book's chapter 26:
`Rectangle>>center ^origin + corner / 2` compiles to the bytecodes the
book prints, and a test checks them byte for byte.

### The virtual machine: the object memory and the interpreter

- **The object memory** (Blue Book chapter 30): an object is an index
  (an *oop*) into an **object table**. A SmallInteger is an oop with
  its low bit set, so it is not in the table. Each entry has a class
  and its fields (pointers, or bytes for strings, symbols and methods).
  Why a table, when OCaml has pointers? Because of `become:`: two
  entries swap and every reference follows. That is also how Smalltalk
  grows a collection in place, and how a class that gains an instance
  variable updates its instances. The notes compare this with direct
  pointers (Squeak dropped the table in 1996, keeping become: by a
  whole-memory scan).
- **Garbage collection**: OCaml's collector cannot see into the table,
  so we write a mark-and-sweep over it, from the roots: the
  specialObjects array (nil, true, false, Smalltalk, the active
  context, ...) and the environment's windows. The Blue Book counted
  references and marked only to free cycles. We keep marking alone,
  and reference counting becomes an exercise.
- **Classes are objects**: a class has a superclass, a method
  dictionary and a format (how many named fields; pointers, words or
  bytes). Every class has a **metaclass**, whose instance it is alone.
  The knot is `Metaclass class class == Metaclass`, drawn as the Blue
  Book draws it in its chapter on metaclasses.
- **The interpreter** (chapters 27-29): the registers (active context,
  method, instruction pointer, stack pointer, receiver), fetch, decode
  and execute. Sends look up the method up the superclass chain (with
  the Blue Book's **method cache**, a hash of class and selector, and
  its effect measured). A primitive runs first when the method names
  one, and the method's own code runs only when it fails.
- **Contexts are objects**, `MethodContext` and `BlockContext`, linked
  by their `sender`. So `thisContext` is an ordinary object, and the
  debugger only reads fields. A block's `^` returns from its *home*
  method (non-local return), and `cannotReturn:` fires when that
  method has already returned.
- **Blocks as the Blue Book has them**: a `BlockContext` shares its
  home's temporaries and is not reentrant (a block calling itself
  recursively breaks). The notes explain this known wart. Real
  closures, as Squeak added them in 2008 (Eliot Miranda), are an
  exercise.
- **`doesNotUnderstand:`**: a failed lookup sends it with a `Message`,
  which is how the debugger opens on a typo, and how a proxy is
  written in five lines.
- **Stepping**: the interpreter runs a budget of bytecodes per frame,
  as `Pmachine` and `Js_eval` do. So `[true] whileTrue: []` never
  freezes the screen, and the user interrupt (Ctrl-C) stops it and
  opens a debugger.
- **Primitives**, the Blue Book's numbered ones that we need:
  SmallInteger and Float arithmetic, `at:` `at:put:` `size`, `new`
  `new:`, `==` `class` `hash`, `become:`, `perform:`,
  `value` and `value:`, `instVarAt:`, the Characters, the Symbols' table,
  `LargePositiveInteger`'s digits, and BitBlt's `copyBits` (below).
- **The host**, as for every language here: what the virtual machine
  knows of the world is a record of functions (the Transcript's text,
  the display, the clock, the mouse), so tests run it with none.

### The kernel: Smalltalk written in Smalltalk

A subset of the Blue Book's second part, in chunk files embedded by
dune:

- `Object` (`=`, `hash`, `printOn:`, `printString`, `isNil`,
  `error:`, `halt`, `doesNotUnderstand:`, `respondsTo:`, `->`),
  `UndefinedObject`, `Boolean`, `True`, `False`;
- `Magnitude`, `Character`, `Number`, `Integer`, `SmallInteger`,
  `LargePositiveInteger` and `LargeNegativeInteger` (written in
  Smalltalk over the digit primitives, as in the Blue Book, so
  `100 factorial` works: the demo everybody typed), `Fraction`
  (`(1/3) + (2/3) = 1`, exact), `Float`, `Point` (`3@4`), `Rectangle`;
- `Collection`, `SequenceableCollection`, `ArrayedCollection`, `Array`,
  `String`, `Symbol`, `Interval` (`1 to: 10`), `OrderedCollection`,
  `SortedCollection`, `Bag`, `Set`, `Dictionary`, `Association`;
  `do:` `collect:` `select:` `reject:` `detect:` `inject:into:`, the
  iteration protocol the rest of the world copied;
- `Stream`, `ReadStream`, `WriteStream` (`printString` is
  `printOn:` into a WriteStream);
- `Behavior`, `ClassDescription`, `Class`, `Metaclass`,
  `CompiledMethod`, `MethodContext`, `BlockContext`, `Message`,
  `SystemDictionary` (`Smalltalk`, the global namespace, itself a
  Dictionary);
- `TextCollector` (the `Transcript`);
- `Form`, `BitBlt` and `Pen` (Blue Book chapters 18-20): a Form is a
  1-bit picture, BitBlt its one drawing primitive (Ingalls's
  RasterOp: source, destination, the 16 combination rules, the
  halftone), `Pen` a turtle over it. The Blue Book's dragon curve and
  spirals run in a window. BitBlt is written in OCaml over
  `appkits/paint`'s `Bitmap`, which TinyMacPaint also draws with.

About 1000 lines of Smalltalk. **The bootstrap**: OCaml makes the
class objects from their definitions (`Object subclass: #Point
instanceVariableNames: 'x y' ...`, read from the chunk files and not
yet run), ties the metaclass knot, then compiles every method with the
OCaml compiler. After that, everything is Smalltalk.

**The image**: "save" writes the whole object memory (the table, then
the objects) to bytes, and "load" reads it back. That is Smalltalk's
kind of persistence: you keep the world, not the files. The format is
our own, a header and the objects in order, exported by
`Playground_platform.export`.

### The environment: TinySmalltalk80

After the screens of Byte (August 1981) and the "Orange Book"
(Goldberg, 1983):

- black and white; the grey desktop (a `Pattern` of `appkits/paint`);
  overlapping windows with their title tabs, collapsible;
- **the three buttons**, red (select), yellow (the text's menu) and
  blue (the window's menu: move, frame, collapse, close), mapped to the
  left button, the right button and shift with the right button;
- **the Workspace**: select text, then *do it*, *print it* (the result
  inserted, selected), *inspect it*;
- **the Transcript**;
- **the System Browser**: five panes (class categories, classes,
  message categories, messages, the code), the instance/class switch.
  *accept* compiles the method into the running system, with its
  errors placed in the text as the Blue Book's compiler did
  ("Nothing more expected ->"). Also a class's definition, edited
  and accepted;
- **the Inspector**: an object's fields in a list, the value on the
  right, a field inspected in turn; the fields of a context too;
- **the Debugger**: a notifier first ("Message not understood: #foo",
  the top five contexts), then the debugger: the stack, the selected
  method with the current statement highlighted, two inspectors (the
  receiver, the context's temporaries), *step*, *send*, *proceed*,
  *restart*. **Fix the method in the debugger, restart, and carry
  on**: this is Smalltalk's lesson, and the golden frames record it.

**The environment is in OCaml, not in Smalltalk.** Smalltalk-80's own
windows were Smalltalk objects (MVC: `View`, `Controller`,
`StandardSystemView`), open to being changed from within, and that
openness was the point. Writing them in Smalltalk means about 5000 more
lines of Smalltalk (text editing, scrolling, menus, the browsers) before
anything is on the screen. So the panes are drawn by OCaml over
`libs/gui`, reading the object memory: the Browser lists the real
method dictionaries, the Inspector reads the real fields, and the
Debugger walks the real contexts. What the person runs and changes is
Smalltalk. The display is still a Smalltalk object, a `Form`, the
windows' pictures drawn with BitBlt. MVC in Smalltalk over it is the
main exercise, and the header says so first.

## Where it goes

- **`libs/languages/smalltalk/`**, the library `smalltalk`, pure OCaml,
  no Playground (the `libs/` rule). The modules are prefixed `St_`
  because the libraries are unwrapped (no `St_` module exists yet):
  `St_lexer`, `St_ast`, `St_parse` (recursive descent; its `.mli`
  gives the grammar, which is smaller than JavaScript's),
  `St_chunk` (the file format), `St_bytecode` (the set, the
  disassembler), `St_compile`, `St_memory` (the object table, the
  formats, the collector, become:), `St_interp` (contexts, sends, the
  cache, the budget), `St_primitives`, `St_bitblt`, `St_boot`
  (the classes made, the knot tied, the kernel compiled), `St_image`
  (save, load), `St_debug` (what the debugger asks: the stack, a
  context's method and statement, step, restart); `kernel/*.st`,
  embedded by dune. About 2500 lines of OCaml and 1000 of Smalltalk,
  with tests in `tests/`.
- **`apps/devtools/TinySmalltalk80.ml`**: the environment, about
  1500 lines, over `libs/gui` and `appkits/paint` (the patterns, the
  bitmap BitBlt draws into). If it outgrows one file, its panes become
  an appkit (`appkits/smalltalk`, asked first). Its `software/`
  and `web/` twins, its CATALOG row, its golden frames.
- The `apps/devtools/dune` comment ("a debugger (the Smalltalk-80
  one)") is updated when the program exists. `libs/languages/README.md`
  gains its row.

The web build is the joke that teaches, as TinyFirefox's is: Xerox's
1980 virtual machine, written in OCaml, compiled to JavaScript, in a
browser (SqueakJS did it by hand in 2014).

## Phases

- **S0, the plan** (this file), then `notes_smalltalk.md`: a tutorial
  in stages (the syntax, message precedence and its surprise, the
  object model and its metaclass knot, bytecodes, contexts and
  non-local return, the environment), each with a worked example that
  a test checks.
- **S1, reading**: `St_lexer`, `St_ast`, `St_parse` (expressions,
  methods, a class's definition), `St_chunk`, an AST printer. Tests:
  precedence (`3 + 4 * 2` is `(3 + 4) * 2`), cascades, literal arrays,
  a method with a primitive, errors placed in the text.
- **S2, the object memory and the bootstrap**: `St_memory` (the table,
  SmallIntegers tagged, formats, become:, the mark-and-sweep),
  `St_boot`'s first half (classes and metaclasses made from their
  definitions). Tests: `3 class class class == Metaclass`, the
  chain up to `Object` and `Class`, become: swapping two objects, the
  collector freeing a cycle.
- **S3, the compiler**: `St_bytecode`, `St_compile` (the literal
  frame, the temporaries, the special selectors, the inlined control
  structures). Tests: `Rectangle>>center` byte for byte, an inlined
  `ifTrue:ifFalse:`, a `whileTrue:` loop, a block's code with its
  jump over it.
- **S4, the interpreter**: `St_interp`, `St_primitives`, contexts as
  objects, sends through the cache, primitive failure, non-local
  return and `cannotReturn:`, `doesNotUnderstand:`, the budget.
  With a first kernel (Object, Boolean, SmallInteger, Array), tests:
  `3 + 4`, `#(3 1 2) inject: 0 into: [:a :b | a + b]`, a `^` out of
  a block, a message not understood caught as a `Message`, the cache's
  hit rate on a loop.
- **S5, the kernel**: the classes above in `kernel/*.st`, bootstrapped.
  Tests: `100 factorial printString size = 158`, `(1/3) + (2/3) = 1`,
  `(1 to: 10) select: [:i | i even]`, a Dictionary, a SortedCollection,
  `printString` of each, the Transcript collected by the host.
- **S6, TinySmalltalk80**: the screen, windows, the three buttons'
  menus, the Workspace, the Transcript, the Browser (accept compiles
  live), the Inspector. Golden frames: the opening screen, a Workspace
  after *print it* on `100 factorial`, the Browser on
  `Rectangle>>center`, an Inspector on a Point. CATALOG row, web page.
- **S7, the debugger**: `St_debug`, the notifier, the debugger's panes,
  step, send, proceed, restart. The worked example is the classic: a
  message not understood, the method defined in the debugger, restart,
  and the program finishes. Ctrl-C on an endless loop. Golden frames
  for each step.
- **S8, the display and the image**: `Form`, `St_bitblt`, `Pen`, the
  Blue Book's dragon drawn in a window by Smalltalk code. Save and
  load the image, and file out a class in chunk format. Golden frame:
  the dragon.
- Exercises (listed in the header): MVC written in Smalltalk (the
  environment in the language, as it was); the compiler in Smalltalk;
  real closures (Squeak 2008); `Process`, `Semaphore` and the
  scheduler (the Blue Book's chapter 15, and the environment's
  background tasks); exceptions (`on:do:`, `ensure:`, ANSI 1998);
  reference counting (the Blue Book's collector) and generation
  scavenging (Ungar, 1984); the changes file (every accept logged,
  the crash recovered); Deutsch and Schiffman's translation to native
  code (1984); the Alto's fonts; Morphic (Self, then Squeak); a
  Smalltalk-72 or -76 mode to show where the syntax came from.
- Never: loading Xerox's virtual image (the author's choice).

## Status

- **S0 done** (2026-09-26): this plan, and the tutorial
  [`notes_smalltalk.md`](../tutorials/notes_smalltalk.md), a section
  per stage, each ending with the worked example its test checks.
- **S1 to S8 done** (2026-09-26), in one go, as planned with these
  differences:
  - `St_class` added (a class as the machine, the compiler and the
    tools read it; `define_class`, what `subclass:...` does), and the
    method's trailer holds the temporaries' names too (the debugger's);
  - `to:do:` is inlined as well (Squeak's compiler does; the Blue
    Book's sent it), for speed;
  - SmallIntegers are 31 bits, as Squeak's, so that tagged they fit the
    web's 32-bit ints; `tests/js/` runs the kernel's edge cases under
    node (it caught the image's first encoding overflowing there);
  - large integers are the Blue Book's, in Smalltalk over bytes, with
    two primitives of ours (a SmallInteger as bytes, and normalize:
    -2^30's magnitude is not a SmallInteger); short division for small
    divisors, a bit at a time for large ones;
  - the environment: 800 by 600, y downwards, scaled to the window; a
    notifier, then the debugger, with "define" on a message not
    understood (the template for the missing method, accepted into the
    receiver's class, the sending frame restarted); the Display drawn
    over the windows, cropped to its black; while a do-it runs the
    mouse is Smalltalk's (Sensor), 100,000 bytecodes a frame (the
    interpreter does 9.5 million a second natively, 1.6 under node);
  - sizes: the library 3800 lines of OCaml, the kernel 2600 of
    Smalltalk (not counting blank lines), the environment 1300;
  - golden frames: the first screen, print it, a method accepted and
    one refused, an inspector, the dragon, a halt in the debugger, the
    user interrupt, and the classic session (`window=debugger`).

## Decisions

- **Our own Smalltalk, from the Blue Book** (the author, 2026-09-26),
  not Xerox's image.
- **The Blue Book's bytecodes and object table**, not a tree walker
  and not direct pointers. The book is then the reference for every
  byte, and become: and the debugger come from the design rather than
  being added on.
- **Blocks as in the Blue Book** (not reentrant), with closures an
  exercise, so that the code matches the book the notes cite.
- **The compiler in OCaml**, which makes the bootstrap simple. The
  Smalltalk compiler is an exercise.
- **The environment in OCaml over `libs/gui`**, reading the live
  object memory, with the display a Smalltalk `Form`. MVC in Smalltalk
  is the first exercise. This is the plan's largest departure from
  the original, and the author may overrule it.
- **Blue Book era (1983)**, not Squeak's syntax or Pharo's libraries.

## Verification

- `libs/languages/smalltalk/tests/`: the notes' worked examples (a
  parse, the metaclass chain, a method's bytes, a non-local return,
  `100 factorial`, a debugger session scripted), each phase adding its
  own.
- Golden frames for TinySmalltalk80, deterministic (`-fixed-time`,
  the `-script` of clicks and keys, the budget counted in bytecodes and
  not in time).
- `tests/catalog/`: the CATALOG row, the golden frame, the web page.

## References

- Adele Goldberg, David Robson, *Smalltalk-80: The Language and its
  Implementation* (Addison-Wesley, 1983), the "Blue Book". Part four,
  the virtual machine, is online (Mario Wolczko's pages).
- Adele Goldberg, *Smalltalk-80: The Interactive Programming
  Environment* (1983), the "Orange Book": the Browser, the Debugger,
  the three buttons.
- Glenn Krasner (ed.), *Smalltalk-80: Bits of History, Words of
  Advice* (1983), the "Green Book": the first implementations, their
  measurements.
- Byte, August 1981, the Smalltalk issue: Dan Ingalls, "Design
  Principles Behind Smalltalk"; Larry Tesler, "The Smalltalk
  Environment"; and others.
- Alan Kay, "The Early History of Smalltalk" (HOPL-II, 1993); Dan
  Ingalls, "The Evolution of Smalltalk" (HOPL IV, 2020).
- Trygve Reenskaug, the MVC notes (1979); Krasner and Pope, "A
  Cookbook for Using the Model-View-Controller User Interface Paradigm
  in Smalltalk-80" (1988).
- L. Peter Deutsch, Allan Schiffman, "Efficient Implementation of the
  Smalltalk-80 System" (POPL 1984); David Ungar, "Generation
  Scavenging" (1984).
- Dan Ingalls et al., "Back to the Future: The Story of Squeak"
  (OOPSLA 1997); Bert Freudenberg et al., SqueakJS (2014); Eliot
  Miranda, the closure compiler (2008).
