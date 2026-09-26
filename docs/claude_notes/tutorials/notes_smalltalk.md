# Smalltalk-80, from the Blue Book: a tutorial

TinySmalltalk80 is Smalltalk-80 and its environment, the language and
the programs it is written with in one live system
(`plan_tiny_smalltalk.md`). This tutorial explains how it works, in
the order the code is written: the language read
(`libs/languages/smalltalk/`), the object memory, the compiler to the
Blue Book's bytecodes, the interpreter, the kernel written in
Smalltalk and bootstrapped, the debugger, the image, BitBlt, then the
environment (`apps/devtools/TinySmalltalk80.ml`). Each section ends
with the worked example its tests check (`tests/Unit_smalltalk.ml`).

The thread through it: **the machine is small, and everything else is
objects**. The virtual machine knows how to send a message, return,
and do a hundred primitives; classes, methods, the stack of calls,
integers bigger than a word, the display, are all objects it reads
like any other -- which is what lets the environment show them and
change them while they run:

```
  "3 + 4"            text, in a Workspace
     | St_parse       a tree: Send (Lit 3, "+", [Lit 4])
     v
  DoIt               a CompiledMethod: 118 ... 176 124 (St_compile)
     | St_interp      a MethodContext made, the bytecodes run
     v
  7                  an oop, printed by sending it printString
```

## 0. Where the code is, and a reading order

| Module | What | Section |
|---|---|---|
| `St_lexer`, `St_ast`, `St_parse` | the text to a tree | 1 |
| `St_chunk` | the file format, "!" between chunks | 1 |
| `St_memory` | the object table, SmallIntegers, the collector, become: | 2 |
| `St_class` | a class as the machine reads it, metaclasses, globals | 3 |
| `St_bytecode`, `St_compile` | the instruction set, the tree to bytecodes | 4 |
| `St_interp`, `St_primitives` | contexts, sends, returns, processes | 5 |
| `kernel/*.st`, `St_boot` | the classes in Smalltalk, bootstrapped | 6 |
| `St_debug` | a stopped process read, stepped, restarted | 7 |
| `St_image` | the whole memory saved and loaded | 8 |
| `St_bitblt`, `kernel/Graphics.st` | Forms, BitBlt, Pen | 9 |
| `TinySmalltalk80` | the Browser, Workspace, Inspector, Debugger | 10 |

The reference is the Blue Book: Adele Goldberg and David Robson,
*Smalltalk-80: The Language and its Implementation* (1983). Part one
is the language, part two the kernel's classes, part four the virtual
machine -- written, remarkably, as a Smalltalk program that simulates
it, which is how everyone who implemented Smalltalk-80 read it.

## 1. The language

Smalltalk has no keywords, no operators, no statements for control:
only objects and **messages** sent to them, three kinds:

```
3 factorial                   unary: a name
3 + 4                         binary: a run of + - * / \ < > = ~ @ % & ? ,
a at: 1 put: 2                keyword: at:put:, its arguments between the parts
```

Unary binds tightest, then binary, then keyword. And all binary
selectors have **the same precedence, left to right**, so

```
3 + 4 * 2        is 14, not 11: ((3 + 4) * 2)
```

-- the first surprise of every newcomer, and the price of letting any
class define `+` or `*`: the parser cannot know which of them
multiplies. `St_parse.mli` has the grammar, ten lines, and the parser
is those ten lines written out by recursive descent.

The rest is small: `x := 3` assigns (and `_`, the Alto's left arrow,
too), `^x` returns, a period separates statements, `| a b |` declares
temporaries, `"..."` is a comment, and a **cascade** sends several
messages to one receiver:

```
Transcript show: 'hello'; cr      show: then cr, both to Transcript
```

A **block** is code as an object: `[:x | x + 1]`, run by `value:`. And
there it is: `ifTrue:` is a message sent to a Boolean with a block,
`whileTrue:` a message sent to a block, `do:` one sent to a
collection. The language needs no if and no loop.

The lexer's three traps (`St_lexer.mli`): `x:=1` is a name then an
assignment, not the keyword `x:`; `3-4` is a subtraction but `3 - -4`
subtracts minus four; `a_b` assigns b to a, as it did in 1980.

**Worked example** (the tests'): `3 + 4 * 2` parses as `((3 + 4) *
2)`; `a at: i + 1 put: b sqrt` as `(a at: (i + 1) put: (b sqrt))`; `3 +
4 5` is refused at offset 6, "Nothing more expected".

## 2. The object memory

Every object is an **oop**, an object pointer, and an oop is an int
with two meanings told apart by its lowest bit (`St_memory.mli`):

```
...value...1    a SmallInteger: no object behind it, 3 is the oop 7
...index...0    an object: its entry in the object table
```

An entry holds the object's class (an oop) and its body: its fields,
or its bytes (a String, a Symbol, a large integer), or a float, or --
a CompiledMethod -- both. nil is the entry 0.

**Why a table**, when OCaml has pointers? For `become:`. `a become: b`
swaps the two entries, and every reference to `a`, anywhere, now
reaches what was `b`: in constant time, without finding the
references. Smalltalk grows a collection in place this way --
`OrderedCollection>>grow` makes a bigger copy and becomes it, and
whoever held the collection holds the bigger one -- and changes the
shape of a class's instances. Squeak (1996) dropped the table for
direct pointers and pays for `become:` with a scan of memory.

SmallIntegers have 31 bits here, -2^30 to 2^30 - 1, so that a tagged
one fits the web's 32-bit integers (js_of_ocaml). Past that, the
arithmetic primitives fail, and the kernel's `LargePositiveInteger`
takes over (section 6). The tests under node (`tests/js/`) check both
edges.

The **collector** is a mark and sweep over the table. OCaml's own
collector cannot help: to it the table is one array, all alive. It
runs between two bytecodes, when enough was allocated -- never from
inside an allocation, where a primitive might be holding an object it
has not stored anywhere yet.

**Worked example**: two Associations swapped by `become:` answer each
other's keys; an OrderedCollection given twenty elements is still the
object another variable held; a thousand Arrays made and dropped are
freed by the collector.

## 3. Classes and metaclasses

A class is an object whose fields the machine knows (`St_class.mli`):
its superclass, its method dictionary, its format (how many named
fields its instances have, and whether indexed ones: pointers or
bytes), its instance variables' names, its methods' categories, its
name. Sending a message is looking the selector up in the receiver's
class's method dictionary, then its superclass's, and so on up.

A class's own methods -- `Point x: 3 y: 4` -- are found the same way,
in the class of the class: its **metaclass**. Every class has one, and
is its only instance. The metaclasses have a parallel hierarchy, and
at the top the two hierarchies are tied into a knot:

```
  3 ------ class ----> SmallInteger ---- class ----> SmallInteger class
                            |                              |
                       superclass                     superclass
                            v                              v
                       ... Object ------ class ----> Object class
                                                           |
                                                      superclass
                                                           v
                                                         Class

  SmallInteger class ---- class ----> Metaclass
  Metaclass class    ---- class ----> Metaclass    (the knot)
```

`Object class superclass` is `Class`: a class is, after all, a
Class. Every metaclass is an instance of `Metaclass`, and so is
`Metaclass class` -- so `Metaclass class class == Metaclass`.

**Worked example**: `3 class class` is `SmallInteger class`; `3
class class class == Metaclass`; `Metaclass class class == Metaclass`;
`Object class superclass` is `Class`.

## 4. Bytecodes

A method runs as bytecodes on a stack machine, the Blue Book's set
byte for byte (`St_bytecode.mli`): pushes (a receiver's variable, a
temporary, a literal, a global's value), stores, sends, jumps and
returns, one byte each mostly. The 32 commonest selectors -- `+ - < >
at: at:put: == class value ...` -- have a byte of their own and no
literal.

The Blue Book's example (chapter 26), Rectangle's

```
center
    ^origin + corner / 2
```

compiles to six bytes:

```
0    push receiver variable 0     origin
1    push receiver variable 1     corner
176  send +
119  push 2                       the constants -1 0 1 2 have bytes of their own
185  send /
124  return the top
```

The compiler (`St_compile.mli`) walks the tree once, emitting as it
goes. Two tricks are the Blue Book's:

**Inlined messages.** `ifTrue:ifFalse:`, `and:`, `or:`, `whileTrue:`
and their kin, with literal blocks for arguments, compile to jumps and
no send:

```
x > 0 ifTrue: ['pos'] ifFalse: ['neg']

0  push temporary 0
1  push 0
2  send >
3  jump on false to 6
4  push 'pos'
5  jump to 7
6  push 'neg'
7  ...
```

So they cost nothing, and redefining `True>>ifTrue:` changes nothing:
the trick's price. (`to:do:` is inlined too, as Squeak does; the Blue
Book sent it.)

**Blocks in their method.** `[:x | x + 1]` compiles to "push
thisContext, push 1, send blockCopy:, jump over the block's code",
then the code itself. `blockCopy:` makes a BlockContext that will start
after that jump. The block's arguments are temporaries **of the
method**, which is why the Blue Book's blocks cannot be reentered: a
block that calls itself overwrites its own argument. Real closures came
in Squeak in 2008 (Eliot Miranda); they are an exercise.

**Worked example**: `Rectangle>>center`'s bytes are `0 1 176 119 185
124`; the listing above is `sign:`'s.

## 5. The interpreter

The machine's registers are the active context and what is cached from
it: the method, the instruction pointer, the stack pointer, the
receiver (`St_interp.mli`). A cycle fetches a byte and does it.

**Contexts are objects**, in the memory like any other:

```
MethodContext  sender  ip  sp  method  -  receiver  args temps ... stack
BlockContext   caller  ip  sp  nargs  startpc  home  stack
```

A send makes a new MethodContext whose sender is the active one; a
return makes the sender active again. So the call stack is a linked
list of objects, `thisContext` is one of them, and a debugger has only
to read their fields.

**A send**: the selector looked up in the receiver's class and up,
through a cache of 1024 entries keyed by class and selector (the Blue
Book's method cache: a loop's sends almost all hit it). If the method
names a primitive, the primitive runs first, and the method's
Smalltalk only when it fails (`St_primitives.mli`) -- so `at:` is a
primitive whose failure is handled in Smalltalk, with an error saying
what went wrong. The arithmetic special selectors on two SmallIntegers
do not even look up. A selector found nowhere sends
`doesNotUnderstand:` with a Message instead.

**Returns**: `^` in a block returns from the block's *home* method,
however many contexts are in between -- a non-local return:
`detect:` is written with it. If the home has returned already,
`cannotReturn:` is sent instead.

**Processes**: the environment runs one for a budget of bytecodes
each frame, so an endless loop leaves the screen alive; a process
stops when its bottom context returns, when Smalltalk calls `error:`
(or `halt`), or when the user interrupts it.

**Worked example**: `#(1 2 3 4) detect: [:x | x > 2]` is 3, by a
non-local return out of `do:`'s block; `3 frobnicate` stops with
"Message not understood: frobnicate"; a block returned from its method
and then evaluated, whose `^` has nowhere to go, stops with "Context
cannot return".

## 6. The kernel, and the bootstrap

The classes are written in Smalltalk (`kernel/*.st`), in the chunk
format Smalltalk filed code in and out with (`St_chunk.mli`): about
2600 lines, `Object`, the Booleans, the numbers, the collections,
streams, the classes' own protocol, Form and Pen.

Two things show what "everything is objects" buys. **Large integers**
are written in Smalltalk over their bytes, as in the Blue Book: when
SmallInteger's `+` overflows, its primitive fails, and the method
continues in Smalltalk with the digits -- the schoolbook's addition,
multiplication and short division, a byte at a time. **Mixed
arithmetic** coerces by generality (a SmallInteger 20, a large one 40,
a Fraction 60, a Float 80): `3 + (1/2)` fails in SmallInteger, retries
as `(3/1) + (1/2)`, and fractions are exact.

The **bootstrap** (`St_boot.mli`): Smalltalk-80 never booted -- each
image was made by the one before, since 1976. With no image to start
from, the world is made from the text: the classes first, empty, so
that the objects made next have their classes; then filled from their
definitions, the metaclass knot tied by the same code; then every
method compiled, by the OCaml compiler -- the one place where a
Smalltalk compiler would have needed a running Smalltalk -- and the
chunks that are not definitions run.

**Worked example**: `100 factorial printString size` is 158; `(1/3) +
(2/3) = 1` is true; `-7 // 2` is -4 and `-7 \\ 2` is 1 (the floor's
division); `1073741823 + 1` is a LargePositiveInteger, and minus one a
SmallInteger again.

## 7. The debugger

Nothing in `St_debug.mli` stops a process: Smalltalk does, with
`error:`. A stopped process's contexts are objects, so the debugger
reads them: the stack is the chain of senders from the top; a frame's
method and the send in progress (the method's pc map, from the
context's instruction pointer); its variables (the home context's
fields, named by the compiler).

Moving on is running the process again under a condition checked
before each bytecode:

- **step**: the selected frame's next send done, whatever it calls,
  then stop before the one after;
- **send**: stop in the method the next send enters;
- **restart**: the frame's method from its start, the frames above it
  dropped, the method taken afresh from its class -- so a method fixed
  in the debugger runs fixed (a context too small for the new method
  is replaced by a bigger one with `become:`);
- **proceed**: run on.

The classic session: `10 fib` is not understood; the missing method is
written in the debugger, into SmallInteger's class; the frame that sent
`fib` is restarted; proceed -- and the answer, 55, arrives as if `fib`
had always been there.

**Worked example**: that session, and a `halt` stepped a send at a
time, `3 + 4` then `a * 2` highlighted in turn, `a` read as 7.

## 8. The image

`St_image.mli` writes the whole memory: every object, its class, its
body. Loading gives the same world back, with the methods accepted
since the boot. That is Smalltalk's persistence: you keep the world,
not files. The format is our own, varints, the same bytes read on 63
and on 32-bit ints.

**Worked example**: a method and a global added, the image saved and
loaded, both there; the image saved again is the same bytes.

## 9. BitBlt and the Pen

A Form is a picture of one bit per pixel; BitBlt copies a rectangle of
one Form onto another, combining each source bit with the destination's
by one of 16 rules -- all the functions of two bits (`St_bitblt.mli`).
Dan Ingalls's primitive was everything the Smalltalk display did: text,
windows, lines, scrolling. Pen, a turtle (Papert's, from Logo), draws
lines with it, a point at a time by Bresenham's algorithm, in
Smalltalk. `Pen new dragon: 9` is the Blue Book's.

**Worked example**: a Form filled black, then reversed, is white; a
Pen gone 50 up from the Display's center has drawn above it and nothing
beside.

## 10. The environment

TinySmalltalk80 draws the 1980 screen: overlapping windows with title
tabs, list panes and text panes, selections reversed, pop-up menus on
the right button. The **System Browser** lists the live system's
categories, classes, method categories and methods, and "accept"
compiles into it at once; a **Workspace** runs any selected text (do
it, print it, inspect it); the **Transcript** shows what Smalltalk
writes to it; an **Inspector** lists an object's fields; an error
opens a **notifier**, then a **Debugger** (section 7).

The one departure from the original: its windows were Smalltalk
objects -- Model-View-Controller, Trygve Reenskaug's (1979), the
`libs/gui/Mvc.mli` of this repository -- open to the Browser like
everything else. Here they are OCaml, reading the object memory. The
Display, though, is a Smalltalk Form: what a Pen draws appears over the
windows until the screen menu's "restore display".

## Exercises

- Real closures: a block's own temporaries, reentrant blocks
  (Miranda's closure compiler for Squeak, 2008).
- MVC in Smalltalk: the environment's windows as Smalltalk objects,
  a View drawing with BitBlt, a Controller reading the Sensor.
- The compiler in Smalltalk, compiled by the OCaml one, then compiling
  itself.
- BitBlt a word at a time, Ingalls's shifts and masks.
- Processes and Semaphores (Blue Book, chapter 15), the scheduler in
  Smalltalk.
- Exceptions: `on:do:`, `ensure:` (ANSI Smalltalk, 1998).
- Reference counting, the Blue Book's collector; or generation
  scavenging (David Ungar, 1984).
- The changes file: every accept logged, a crash recovered.

## References

- Adele Goldberg, David Robson, *Smalltalk-80: The Language and its
  Implementation* (1983), the "Blue Book".
- Adele Goldberg, *Smalltalk-80: The Interactive Programming
  Environment* (1983), the "Orange Book".
- Glenn Krasner (ed.), *Smalltalk-80: Bits of History, Words of
  Advice* (1983).
- Byte, August 1981: Dan Ingalls, "Design Principles Behind
  Smalltalk"; Larry Tesler, "The Smalltalk Environment"; Dan Ingalls,
  "The Smalltalk Graphics Kernel".
- Alan Kay, "The Early History of Smalltalk" (HOPL-II, 1993); Dan
  Ingalls, "The Evolution of Smalltalk" (HOPL IV, 2020).
- L. Peter Deutsch, Allan Schiffman, "Efficient Implementation of the
  Smalltalk-80 System" (POPL 1984).
- Dan Ingalls et al., "Back to the Future: The Story of Squeak"
  (OOPSLA 1997); SqueakJS (Bert Freudenberg, 2014).
