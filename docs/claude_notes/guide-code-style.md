# How a file in this repository is laid out

The shape of the code itself: where the long comment goes, the banner
comments that divide a file, and the names those sections have. Its
companion is [`guide-principles.md`](guide-principles.md), which is
about the *documents* and what the code owes them; the OCaml rules
proper (avoid `open`, annotate a binding rather than qualify every
field, `claude:` on new comments in old code) are in the author's
`CLAUDE.md`.

Nothing here is new policy. It is what the existing files do, read
back — the games here, and the author's own OCaml under `~/xix/`,
which is where the convention comes from.

## 1. The banner

A section is announced by a line of stars, the title, and another
line of stars. Exactly 77 stars, so the line is 79 characters:

```ocaml
(*****************************************************************************)
(* The model *)
(*****************************************************************************)
```

It is in `~/xix/` throughout (`builder/CLI.ml`, `Env.ml`, …), in this
repository's games (`games/TinySokoban.ml`) and in its teaching
libraries (`graphics/2d/Fill.ml`). A file with three or more parts
gets them; a twenty-line module does not need any.

## 2. The section names

From `~/xix/`, in the order they usually appear:

| name | what goes in it |
|---|---|
| `Prelude` | the long comment: what this file is, its limitations, its references |
| `Types` (or `Types, constants, and globals`) | the data, before anything that uses it |
| `Helpers` | the small functions the main one needs |
| `Main algorithm` | the thing the file is for |
| `Debug` | printers and dumps |
| `Entry point(s)` | what the outside calls |

This repository's programs — games, examples, apps — use a shorter
vocabulary of their own, and it matches the Model-View-Update shape
they all have:

| name | what goes in it |
|---|---|
| a domain name (`The levels`, `The character display`) | the constants the program is about |
| `The model` | the type, the initial value, and the layout it is placed by |
| `The rules` | the logic that is neither input nor drawing |
| `Update` | `let update computer model` |
| `View` | `let view computer model` |

`let app` and `let main` are the last two lines and get no banner of
their own.

## 3. Where the long comment goes

**In a program** (`games/`, `examples/`, `apps/`): after the licence
header, before `open Playground`, with no banner. It says, in this
order:

- what the program is, and the original it is a small copy of, with
  the year and the people;
- **what it uses**: which kits, which playground layers, which
  `physics/`, `ai/`, `gui/` or `appkits/` modules;
- **what it deliberately does not do**, and why — the honesty that
  keeps a Tiny program from pretending to be the real one;
- **Exercises:** what a reader should try changing, as the last
  paragraph.

**In a library**: the long comment goes in the `.mli`, where a reader
looking for the idea will find it — with the ASCII diagram, the worked
example with concrete numbers, and the paper or book it came from,
with its year. The `.ml` then starts with the licence and one line:

```ocaml
(* See Foo.mli *)
```

`~/xix/` puts that long comment in the `.ml`'s `Prelude` instead,
which is the same habit with the parts swapped; this repository's
libraries are read through their `.mli`s, so it goes there.

## 4. The order inside a file

Types first, then the values that build them, then the ones that read
them, then the ones that change them, and the entry points last. A
reader should be able to stop halfway down and have seen every
definition the rest uses.

## 5. Things met the hard way

Small traps, written down because each cost a build:

- **`*)` closes a comment, wherever it is.** `(* a formula that
  *moves*) is one *)` ends at the first `*)`. Emphasis with `*word*`
  is safe only when the word is not followed by a closing paren.
- **A variant constructor shadows the stdlib's.** Defining
  `type value = … | Error of string` makes `Error why` in a later
  pattern mean *that* `Error`, and a `(_, string) result` in the same
  file then needs `Result.Ok` and `Result.Error` written out.
- **Shadowing a playground function is silent until it is not.**
  `let move (dc, dr) model` in a program hides `Playground.move`, and
  the error arrives hundreds of lines later, in `view`.
- **Goldens that script the mouse are exact.** A scripted click is a
  pixel coordinate, so any layout change re-aims every script that
  pointed into the changed panel.

## 6. Keeping this file honest

If a convention here is not what the files do, the files win and this
gets corrected. When a new one is settled — in a review, or because
something broke — it is added here in the same breath, with the
reason, so the next person (or the next session) does not have to
rediscover it.
