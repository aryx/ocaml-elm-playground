# Squeak, Smalltalk-80 made live again: a tutorial

TinySqueak (`plan_tiny_squeak.md`) is what Squeak (1996) made of the
Blue Book's Smalltalk-80: the same language and virtual machine
(`notes_smalltalk.md`, read first), with blocks that are real closures,
colour, and an environment, Morphic, written in Smalltalk itself. This
tutorial follows the plan's phases, a section each as it is written,
each ending with the worked example its tests check
(`Unit_squeak.ml`).

The thread through it: **the host shrinks**. Every step moves
something from OCaml into Smalltalk, where the Browser can show it and
change it.

## 0. One library, two kernels

There is one `languages/smalltalk/`: one compiler, one interpreter,
one object memory. What differs is the text the system is booted from
(`St_kernel.mli`):

| Kernel | Files | Booted by |
|---|---|---|
| the Blue Book's | `kernel/*.st` | `St_boot.boot ()` |
| Squeak's | the same, then `kernel/squeak/*.st` | `St_boot.boot ~kernel:St_kernel.squeak ()` |

Squeak's files add classes and methods to the Blue Book's, and define
some again (the last definition of a class is the one booted). The
Blue Book's kernel stays what it was, to be read alone: TinySmalltalk80
boots it, and nothing of Squeak's is in its files. What Squeak changes
in the virtual machine is asked for by its kernel: the compiler emits
closures when the kernel has a `BlockClosure` class, the Blue Book's
blocks otherwise, and the bytecodes it then emits are the same as
before, byte for byte.

## 1. Closures

### What was wrong with the Blue Book's blocks

A Blue Book block is a BlockContext, made by `blockCopy:`, whose
arguments and temporaries are slots of its **method's** context
(`notes_smalltalk.md`, section 4). One block, one set of slots: so

```
fact := [:n | n < 2 ifTrue: [1] ifFalse: [n * (fact value: n - 1)]].
fact value: 5
```

fails -- the inner `value:` runs in the same BlockContext, which is
busy, and overwrites `n` -- and

```
(#(1 2 3) collect: [:i | [i * 10]]) collect: [:b | b value]
```

answers `(30 30 30)`: the three inner blocks share the one `i`.

### A closure

Since 2008 (Eliot Miranda's compiler) a Squeak block is not a context
but a small object, a **BlockClosure**:

```
BlockClosure   outerContext   the context that made it
               startpc        where its code starts, in the same method
               numArgs
               1, 2, ...      what it copied when it was made
```

and each `value` makes a *new* MethodContext for it (its field 4, which
the Blue Book left unused, names the closure): the arguments, then the
copied values, then its temporaries. Two activations, two contexts:
the recursion works, and each `i` is its own.

### Reaching the temporaries outside

A block uses temporaries of the method around it, and may run when
that method's context is long gone. Miranda's answer keeps contexts
out of it (a context then never has to outlive its return, which is
what lets a fast virtual machine keep them on a stack):

- a temporary that **no longer changes** once the block has it is
  copied into the closure when it is made -- a value is as good as the
  variable;
- one that **changes** lives in a **temp vector**, an Array made when
  its method starts, and the closure copies the vector: everyone who
  shares the variable shares the Array.

```
adder: n                       counter
    ^[:x | x + n]                  | n |
                                   n := 0.
                                   ^[n := n + 1]

 0 push temporary 0   n         0 push a new Array of 1
 1 push a closure,              2 pop into temporary 0   the vector
     1 argument, copying 1      3 push 0
 5 push temporary 0   x         4 pop into temporary 0 of the vector
 6 push temporary 1   its n     7 push temporary 0
 7 send +                       8 push a closure, copying 1
 8 block return top            12 push temporary 0 of the vector
 9 return top                  15 push 1
                               16 send +
                               17 store into temporary 0 of the vector
                               20 block return top
                               21 return top
```

Five bytecodes, in numbers the Blue Book left free (`St_bytecode.mli`):
138 push a new Array, 140 to 142 push and store a temporary of a
vector, 143 push a closure -- followed, as before, by the block's code
in the middle of its method.

### Two passes

Whether a temporary is copied or put in a vector depends on code that
comes *after* its first use, so the one-pass compiler becomes two
(`St_compile.mli`): the method is compiled once to learn, for each
temporary, whether a block inside uses it and whether it changes
afterwards, and for each block what it needs from outside; then again,
for good. Nothing is duplicated: the first pass is the same code, its
bytes thrown away.

"Changes" is: assigned from a block inside the one that declares it,
or after a block that uses it, or in a loop.

### Loops

`1 to: 3 do: [:i | bs add: [i]]` is compiled as a loop over a
temporary, no block made for the body -- so the three blocks would
share `i` again. When the first pass sees a block hold the loop's
variable, it runs once more with that loop *sent* (`Number>>to:do:`,
the body a real block): each turn then has its own `i`, and the common
loop, whose variable no block holds, stays a loop.

The trap left: a temporary declared in the literal block of a
`whileTrue:` is the method's, one for all the turns.

### The price

A context a `value`, where the Blue Book reused one: on a loop of
`inject:into:`, 20 million bytecodes take 1.6 s with closures against
1.2 s without, natively. The plan's next phase is the interpreter's
speed.

**Worked examples** (`Unit_squeak.ml`): the recursive `fact` answers
120; the blocks made in a loop answer `(10 20 30)`; two counters made
by one block count apart; the two listings above; and the Blue Book
kernel's own tests give the same answers compiled with closures.

## 2. MiniMorphic: Morphic in one file

Before Squeak's Morphic, a small one to read whole:
`kernel/morphic/MiniMorphic.st`, 400 lines of Smalltalk, booted by
`St_boot.boot ~kernel:St_kernel.mini_morphic ()` (Squeak's kernel, then
this file). It is kept as a step of its own, as the Blue Book's kernel
is kept beside Squeak's. No colour and no text yet: the Display has
one bit a pixel, and a morph is black, white, or one of two grays.

### Four ideas

Morphic (John Maloney and Randall Smith, for Self, 1995) is these:

1. **Everything on the screen is a morph**: a rectangle of it
   (`bounds`), a colour, an owner and submorphs. The screen is one, the
   `WorldMorph`; the mouse is one, the `HandMorph`, and what it
   carries are its submorphs -- so dragging needs no code of its own:
   moving a morph moves what it holds.
2. **A morph is alive**: every cycle the world sends `step` to every
   morph. `AtomMorph>>step` adds its velocity to its position and
   turns back at its owner's walls; that is the whole animation.
3. **Nobody redraws**: a morph that changed says where
   (`changed`, which goes up the owners as `invalidRect:` to the
   world), and at the end of the cycle the world redraws those
   rectangles: the background, then every morph back to front, the
   canvas clipped to the rectangle.
4. **The cycle is the program**:

   ```
   doOneCycle
       hand processEvents.                      the mouse read
       submorphs copy do: [:m | m fullStep].    every morph steps
       self displayWorld                        the damage redrawn
   ```

A new kind of morph writes one method, `drawOn:`, and maybe `step`.

### Damage, and what the spike measured

The first version merged a new damaged rectangle with any it touched,
as Squeak's DamageRecorder does. With fifty atoms that was 206,000
bytecodes a cycle, 158,000 of them in the morphs' steps, where each
move went through the list of rectangles, and 36,000 drawing:
remembering the damage cost more than redrawing it. What is kept is
simpler:

- `invalidRect:` only adds the rectangle to a list;
- at the end of the cycle, a few rectangles (one morph dragged: the
  place left, the place taken) are each redrawn, and what did not
  change is not touched; many (everything moving) become the one
  rectangle that holds them all, drawn once.

With that, `Rectangle>>intersects:` written as four comparisons of
numbers (the kernel's makes two Points), and a morph drawn as two fills
(black, then its colour one pixel inside) instead of five:

| atoms | bytecodes a cycle | native | under node |
|---|---|---|---|
| 10 | 12,800 | 1.3 ms | 5.1 ms |
| 50 | 60,800 | 5.7 ms | 18.8 ms |
| 200 | 241,800 | 21.9 ms | 71.6 ms |

About 1,200 bytecodes a morph that moves: its step, its two damaged
rectangles, its drawing. So in a browser fifty morphs moving at once
run at 50 frames a second, two hundred at 14. That is the answer the
spike was for: Squeak's Morphic, where most morphs stand still most of
the time, is within reach; a screen where everything moves is not,
until the interpreter under node (3 to 5 million bytecodes a second)
is faster.

**Worked examples** (`Unit_minimorphic.ml`): a morph drawn, its frame
and its gray; a pixel scribbled outside the damage survives a cycle and
not a redraw of the world; an atom at 780 bounces off the wall at 800;
the hand picks a morph up, carries it, and puts it down in front of
another; fifty atoms after a hundred cycles are all in the box.

## Exercises

- Miranda's finer rule: a temporary assigned only before any block
  that uses it is made, even in a loop that ended, can be copied.
- A fresh temporary for each turn of an inlined loop whose body
  declares one that a block holds.
- MiniMorphic: a morph that follows the hand's speed when dropped (it
  is thrown); a `drawOn:` that is not a rectangle (a Pen's dragon in a
  morph); Squeak's DamageRecorder, merging the rectangles that touch,
  and when it wins.
- The debugger's variables for a closure's activation: its arguments,
  its copied values and its temp vectors by name (today only a
  method's).

## References

- Eliot Miranda, "Closures Part I" to "Part III", Cog blog (2008): the
  design, the bytecodes, the compiler.
- Dan Ingalls, Ted Kaehler, John Maloney, Scott Wallace, Alan Kay,
  "Back to the Future: The Story of Squeak" (OOPSLA 1997).
