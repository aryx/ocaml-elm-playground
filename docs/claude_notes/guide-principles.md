# How these documents, and the code they describe, are written

The long form of [`README.md`](README.md)'s principles: the shape each
kind of document takes, what the code owes them, and the prose habits.
**The eight principles themselves are in the README**, which is what a
new reader sees first; a plan restating the ones that matter to it is
fine and deliberate -- `graphics/`, `physics/`, `audio/` and `ai/` all
do, and a new teaching area should too, listing in full only what it
does *differently*.

Nothing below is new policy; it is what the existing files do, read
back.

## 1. The kinds of document

| kind | where | the question it answers |
|---|---|---|
| `plan_<area>_teaching.md` | `plans/` | what is this library for, and in what order does it get built? |
| `plan_<area>_remaining.md` | `plans/` | what is left, or known broken, in a finished one? |
| `plan_games.md`, `plan_playground_other.md`, `plan_teaching_*.md` | `plans/` | the catalogs and the sketches: what could be done |
| finished plans | `plans/done/` | how it was actually built, phase by phase |
| `notes_<area>.md` | `tutorials/` | how does it work, from scratch? |
| `notes_<area>_related_work.md` | `related-work/` | where does this sit among the real systems? |
| `notes_<topic>.md` | `dev/` | how do I run, debug or measure this? |
| `README.md` | here | the index: one line per document, and a reading order |

Two habits that keep this from rotting:

- **A sketch becomes a plan of its own when it is started**, and the
  sketch is then replaced by a pointer, not deleted -- section 5 of
  [`plans/plan_teaching_other.md`](plans/plan_teaching_other.md) is the
  worked example.
- **A finished plan moves to `plans/done/` and is not tidied up.** It is
  kept for its history and its numbers: what was tried, what was
  simplified, what the measurements were. References to it from
  elsewhere are left alone.

## 2. The plan's skeleton

The teaching plans (`plan_physics_teaching.md`,
`plan_audio_teaching.md`, `plan_ai_teaching.md`,
`plan_networking_teaching.md`) share these sections, in this order:

- **Context** -- what this teaches, what is already there, and the
  companion notes, linked.
- **Principles** -- one line: the same as the others (this file), plus
  whatever this area needs of its own.
- **The Playground API, Evan-style** -- the `playground/X.mli` sketch,
  marked *tentative, to be settled by writing the games with it*. It
  usually is: writing the third game is what reveals the shape, not
  thinking harder about the second.
- **Target layout** -- the directory, one line per module.
- **Groundwork decisions** -- the few choices everything else depends
  on, each with its reasoning (the audio loop is not the frame loop;
  a search must fit in 16 ms; randomness is a seed, not a global).
- **The modules, with their references** -- the paper or book behind
  each, with its year.
- **New examples** and **Games** -- what the library is *for*. A
  teaching library with no example is a library nobody read.
- **Phasing** -- numbered, each phase shippable and testable on its own.
- **Status** -- the log: see below.
- **Verification** -- how anyone will know it works.
- **Out of scope** -- the ceiling, stated up front.

Two rules about the content:

- **State the simplifications up front, rather than discovering them by
  accident** ([`plans/done/plan_gouraud_phong.md`](plans/done/plan_gouraud_phong.md)
  has a section named after learning this the hard way).
- **Status is a log, not a summary.** Each phase gets a `DONE` entry
  with what was built *and its numbers*, every decision is recorded
  with its date and who asked for it, and mistakes found along the way
  stay in ("a race fixed on the way", "the `.mli`'s example first had
  =F for 0.5 s where it's 0.25"). Later sessions, and later readers,
  need the wrong turns more than the conclusions.

Plans that are not for a teaching library (a feature, a port, a
rework) use a shorter form -- Context / Design / Phasing /
Verification -- and the remaining-work plans are simply numbered items,
each a known gap with enough detail to reproduce it.

## 3. The tutorial's skeleton (`tutorials/`)

- Title: *"X, from scratch: a tutorial for `dir/`"*.
- An opening saying what question it answers, for **a reader of the
  engine, not a user of it**, and naming its companions with links.
- **§0, "Where the code is, and a reading order"**: a table of module ->
  what it does -> which section explains it. This is the part people
  actually use.
- Then the ideas, in dependency order, each with an **ASCII diagram**
  and **real numbers**.
- A final **"In the playground"** section: the Evan-style API, and how
  the whole thing looks to someone who never opens the library.
- A **glossary** of every term the note introduced.

**The tutorial is written ahead of the code, as its specification**,
when the area is new (`notes_audio.md` and `notes_ai.md` both were);
the plan's last phase is then *checking the note against what was
actually built and filling in the numbers*. Writing the explanation
first is also the cheapest design review there is -- an idea that
cannot be explained in a page tends to be wrong.

## 4. The related-work note's skeleton (`related-work/`)

- **"The one-line version"**: a table, one row per family of systems --
  what it optimizes for, and what you write in it. The last row is
  always ours.
- Parts, in this order: where the field came from (the history), the
  real systems today, the teaching lineage, and what exists **in Elm
  and in OCaml** specifically.
- **"Where `x/` and `X` actually sit"**: the two levels -- the library
  at the *legible* end, the Evan-style API at the *simple* end -- and
  the **deliberate ceiling**, said plainly (no reverb, a few hundred
  elo, grids instead of navmeshes).
- **"Postscript: the numbers (to come)"**: what will be measured once
  it is built.
- A closing note that the sources are **from memory, to be checked**,
  and the same marker inline wherever a date or a name is asserted.
  Getting this wrong in a teaching document is worse than omitting it.

## 5. The principles, and where each was decided

The eight are in [`README.md`](README.md); what is worth recording
here is where they come from, because each was a decision someone
made once, on a particular day, for a reason:

| principle | first stated in | and the reason |
|---|---|---|
| 1. independent of the Playground | `done/plan_software_2d.md` (layout) | the library has to be readable alone, and usable by every backend, the web one included |
| 2. one idea per module, one feature per function | `done/plan_software_2d.md`, "Code style: one feature, one function" | a course introduces features one at a time; `Line.draw` = `clip` then `bresenham` |
| 3. the simple version stays, switchable | `graphics/core/Opti` and the "o" key | keeping it forces the simple code to remain *runnable*, not just quoted in a note -- and the switch turns "faster" into a number on screen |
| 4. the `.mli` explains, and the tests check the example | all four libraries | it is the only mechanism here that stops code and explanation drifting apart |
| 5. deterministic, seeded | the physics plan's fixed step; audio's golden WAVs; `ai/`'s seeds | every golden test and every reproducible bug rests on it |
| 6. test against the field's laws | `done/plan_physics_teaching.md` | momentum and energy caught what worked examples could not |
| 7. comments describe the code as it is | a standing correction from the author | "moved from", "used to be" ages badly and helps nobody reading today |
| 8. honest about scale | `plan_ai_teaching.md` | `ai/` is the first area whose field has superhuman systems to be measured against; saying the ceiling out loud is the price of teaching it |

Principle 3 has a corollary worth stating separately, because it is
the one most often violated by accident: **an optimization that
deletes the slow version is a loss**, even when it is faster and
correct. The slow version is the explanation.

## 6. What the code owes the documents

The conventions that make the above possible, collected from where
they were decided:

- **Every new module gets an `.mli`**, tests included; the games' and
  examples' main programs don't (they export nothing). The license
  header goes on new `.ml` files, never on an `.mli`.
- **A game's or example's header comment says what it uses** -- which
  kits, which Playground layers, which `physics/2d` or `ai/` modules --
  **and what it deliberately doesn't, and why**. It ends with
  **exercises**: what a reader should try changing.
- **When extending a game, pick the design that adds the least code**,
  and put what was traded away in the header as an exercise.
- **Optimizations and bug fixes are small, local diffs** with a comment
  at the site explaining what the simpler code did, why it wasn't
  enough, and why the fix is right -- rather than a restructuring for
  speed. The slow, obvious path stays reachable where it can.
- **New rendering or debugging features go behind a toggle and a debug
  key** (`-debug-keys`; avoid the arrows, space and w/a/s/d, which
  games use), and the keys are listed in the window title or the help.
- **Check a worked example before writing it down.** Hand-checking has
  caught a wrong ASCII picture and a wrong note length here; a wrong
  worked example in a teaching `.mli` is worse than none.

## 7. The prose

Observable habits, not rules anyone has to agree with:

- **Numbers instead of adjectives.** Not "A* is much faster": *80 cells
  against 11, for the same 10-step path*. Every claim in these notes
  that can be a measurement is one, and the ones that aren't yet say
  "(to come)".
- **ASCII diagrams**, liberally. The author asked for them explicitly;
  they survive `grep`, diffs and terminals, and they force the idea to
  be small enough to draw.
- **A name and a year for every idea** (Bresenham 1965, Reynolds 1987,
  Kocsis and Szepesvári 2006). These are teaching documents; where an
  idea came from is part of the idea.
- **Say what something costs**, not only what it does -- the cut
  branches, the cells visited, the milliseconds, the lines of code.
- **Mark what is uncertain** where it stands: "(Names and dates from
  memory, to check.)"

## 8. Keeping this file honest

When a new teaching area starts, it points here and lists only its own
principles. When one of them turns out to be general -- as `ai/`'s
"honest about scale" did -- it moves here, and the plan keeps the
pointer. If a convention here stops matching what the files do, the
files win: this is a description, and descriptions are the thing that
rots.
