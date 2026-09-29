# Writing a `.codemapconfig`

For an LLM (or a person) writing the configs the code map reads
(`plan_codemap_v2.md`; the format: `launcher/codemap/Code_guide.mli`).
The map draws what the config says: a directory's name and card, a
file's card, the capitals seen from afar, the lines drawn larger up
close, the tours. So a config is judgement, written once: what a
newcomer should see first, and in what words.

The worked example to imitate: `games/shmup/.codemapconfig`
(TinyInvaders) and `gamekits/shmup/.codemapconfig` (its kit).

## Before writing

Start from the facts: `tinybox codemap -facts <root> <dir>` prints a
Markdown brief of the directory (its files, their digests, their header
comments, their sections, their top-level definitions with their uses
here and from other files, the most used starred, what each file uses
and what uses it). It says what is there, never what matters: that is
the config's judgement. It also gives the digests to copy.

Then read, in this order: the directory's README if any (and `CATALOG.md`'s
section for a genre), each file's header comment, then the code itself
-- the model, the update, the view for a game; the `.mli` for a
library. The header comments here are good: the config's job is not to
copy them but to pick from them what a card of one or two lines needs,
and to point at the places they talk about.

## One config per directory

- A directory's own config: its `summary`, its `files`, its `tours`.
- Its parent's `dirs:` for a small directory not worth a config of its
  own (one line each).
- The root's `title`: the project in one sentence, for the map's title.

## Summaries

- Say what it is **for**, not what it contains: "The shoot 'em up kit:
  shots that move, curves that enemies fly" -- not "Shots.ml and
  Path.ml".
- One sentence, two at most; about 60 to 100 characters. It is read on
  a card, beside the mouse.
- For a game: the original (name, maker, year) and what makes it that
  game, in the fewest words.
- No "This file...", no "This directory contains...".

## Capitals

What to know first, seen from the whole map: the entry points, the
place where the idea is. Three at most per file, and most files have
none. A capital must say something the file's name does not: for a
program whose entry hides among many files (`~/ix`'s), its `main`; for
a game on the Playground, whose file is its entry, the one function
that is the game's heart (TinyInvaders' `march`), not `update` and
`view` -- every game has them, so as capitals they would crowd the map
and say nothing; they are `important`, and `links`.

## Skeletons

The structure the rest hangs on, as in biology: a few definitions, each
with a role, and the joints between them -- the loop of a game's
Model-View-Update, a compiler's passes, how a program starts and runs.
The map's X-ray (`x`) shows only them, at every level: from afar a dot
in each file, the joints running between directories; at the ground the
bones' definitions lit in the shaded file.

- A skeleton is not the important lines: those say what to read, a
  skeleton says how the parts connect. Four to eight bones; a role each,
  a few words ("the state", "a frame: model -> model").
- Joints are the flow of data or control, their direction meaning it:
  `model -> update` (stepped), `update -> model` (the next state). A
  loop is two joints, drawn as two roads.
- Write a shared pattern once, in `skeletons.libsonnet` at the root (a
  function of the file: `skeletons.mvu('TinyInvaders.ml')`), and extend
  it with `+:` where a program has more: TinyInvaders' spine down to
  `march`, and across to its kit's `Shots.advance`.
- Let a skeleton span files and directories when the structure does: the
  repository's own ("How a program runs": `Program.main`, the platform's
  `run_app`, the frame loop, the game's update) sits in the root config.
- Every Playground game is Model-View-Update: the template is the
  default; name a game's skeleton for what it adds. `skeletons.game(file,
  heart, role)` is MVU and the game's heart reached from `update`, one
  line a game (named arguments with `=`: `model='type:game'`).
- A skeleton inside one file is that file's: shown at its ground, a
  small dot from afar. A region's skeleton spans files: for a genre,
  what its games share -- the kits (the arcade's: the maze kit under
  Pac-Man and Bomberman, the lightcycles kit under the three Trons),
  the only ties between programs otherwise each alone. Bones outside
  the region are fine: the X-ray draws stubs naming them.
- A skeleton has a level: its config's directory. The X-ray shows the
  skeletons of the unit looked at (a file's at the ground, a directory's
  config's from afar, or the nearest above that has some), one at a
  time, `x` going to the next; the deeper directories' skeletons are
  dots with their names. So give every important directory its own
  skeleton of its parts: the root the repository's layers, `playground/`
  its API and platforms, `launcher/codemap/` how the map draws.
- At a directory's level, a bone is a whole file or directory: `at:
  'games/'` or `'Playground.mli'`, no anchor. Its role says what that
  part is *for* in this architecture ("the one function left open:
  run_app"), not the directory's summary again.
- Joints between directories say how they depend: "written with",
  "implements", "stands on" -- the direction is who uses whom.
- Several skeletons at one level are fine (the root has its layers and
  how a program runs): each is shown alone, in the order written, the
  most telling first.

## The anatomy: what the configs give, what the code gives

The X-ray (`x`) has six plates (keys 1 to 6): skeleton, blood, muscles,
nerves, lungs, skin (`launcher/codemap/Code_anatomy.mli`). Only the
first two come from the configs; the others the map finds in the code
by itself -- do not write them:

- skeleton: the configs' `skeletons`, above.
- blood: the data carried round the skeleton, flowing along its joints
  in their direction. So a joint's `say` names what flows ("the next
  state", "each event"), and its direction is the data's, not the call's.
- muscles (loop density), nerves (keyboard, mouse, subscriptions,
  commands), lungs (capabilities, files, sockets, the console, the
  platform), skin (what a `.mli` exposes): found in the code. A
  codebase whose inputs or outputs have other names (a kernel's
  syscalls) will one day give its words in a `layers` rule; until then,
  say it in the summaries.

## What the map does with it (for judging what to write)

What works best, the author says: the earth view (the colours, which a
config's `colors` may override, the directories' and files' names), the
skeletons with their roles at every level, the street (`a`: what a file
uses, what uses it), the glow and the peeks, and the lines' heights.
So the skeletons' roles and the important lines' notes are where a
config's words count the most; the region level shows each file's
`summary` as its card, so write it to be read there, in the block.

## Notes

An item's `say` is drawn beside its line at the ground and the street,
in the room after the line's end (wrapped, three lines at most): write
it short, 30 to 60 characters, what the line *is for* or *why* -- "one
alien moved per frame: why the last one runs" -- never what it plainly
says. An item without a `say` still makes its line taller.

## Important

The lines a reader must see to understand the file, drawn larger near
the ground: the model's type, the function holding the trick, the
comment explaining the non-obvious. Not the most used (the map knows
that already), the most telling. `weight` 3 for the two or three that
matter most, 1 for the rest. Five to ten per file.

## Anchors the checker taught

- A record or variant is a `type:` (Galaxy's `zone`, `body`), not a
  `def:`: the checker says "no def zone"; the facts list each
  definition with its kind.

## Jsonnet pitfalls

- An apostrophe inside a single-quoted string ends it: write "its
  ghosts' ways" in double quotes.
- A function's named arguments are `name=value`, a field's `name: value`.

## Anchors

Prefer `def:` and `type:`; they survive edits. `comment:"..."` needs
words that are on one line of a comment (the checker says if not);
pick a distinctive phrase. `section:` takes a section's title as the
lexer sees it. Never `line:` unless nothing else points there.

## Links, related, tours, views

- `links`: the calls that explain the file's flow (`update` to
  `update_rules`); not every call.
- `related`: files no analysis would link -- the game's page, its golden
  frame, its level file, its editor, its tests.
- `tours`: 5 to 10 stops, in the order to read, each `say` one
  sentence on what to notice there. A stop names its file.
- `views`: a game and its kits; a library file and its users.

## Digests

Each described file gets its `digest`, which `tinybox codemap -check`
prints (the first 12 hex digits of its MD5). When the checker says a
file changed since it was described, reread it and update what it
says, then the digest.

## After writing

`tinybox codemap -check <dir>` must say no mistake. Then look at the
map (`tinybox codemap <dir>`): the cards should read well, the capitals
should not crowd.

Mark what an LLM wrote: `generated: { by: '<model>', on: '<date>' }`.
A person's additions go in the same file, or in an object added to it
(`(import 'x.libsonnet') + { ... }`).
