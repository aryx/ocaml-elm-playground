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
- The card is the summary alone, under the path: no counts of files,
  lines or subdirectories (the author: not useful). A directory or file
  without one shows "not described yet" -- the configs still to write,
  seen by hovering.
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

### What is missing, checked

`tinybox codemap -check .` says what the configs miss ("missing:"): a
program (a top-level `main`) with no skeleton; a hub with no capital,
in its `.ml` or its `.mli` (only where a directory's config describes
files). The first pass left the Playground's core without a capital and
most games without a skeleton, found only by the author looking: a pass
is finished when -check says 0 missing.

### Other languages, other projects (~/ix, ~/principia)

The brief and the checks are not OCaml's only: a C file's `#include`s
count toward its header's fan-in (a header and its `.c` one module), a
C `main` (Plan 9's, its type on the line above, or not) is a program,
as is an OCaml `Main.ml` running `Cap.main` (~/ix's programs). Several
files of one name (~/ix's 26 `CLI.ml`) are told apart by nearness: a
use counts for the one sharing the most directory with the user's. A
project's own shapes go in its own `skeletons.libsonnet` (~/ix's
`cli`: Main, the CLI's main, the core); the Playground's templates are
for Playground programs, and the brief's template line shows only for
them.

### What ~/ix taught (its first pass, 2026-09-29)

Six agents wrote ~/ix's 69 configs; their reports, the lessons now in
the tools or here:

- -check now reports every directory, a program no config describes
  too; and a directory's own hub (half its other modules name it:
  mini-rc's `Ast`), not only the project's.
- A program is also a `let () =` reading `Sys.argv` or registering
  callbacks, and a kernel's `kmain` (in ~/ix's steps, in `libc.c`).
- An `.ml` saying `(* See X.mli *)` has its header in its `.mli`: the
  brief says so rather than show the next comment.
- A project's template is its own `skeletons.libsonnet`; a linear chain
  (`cli`) fits a pipeline, not a loop or a fan: extend it with `bones+:`
  and `joints+:` (a shell's read, parse, eval, again; a commit's walk,
  save and refs).
- Still to build (the tools don't do it yet; read the code instead):
  - an `external` or `Callback.register` pairs OCaml with its C: the
    brief does not cross that boundary;
  - assembly (`.s`, `.tm`) is not a source: a boot skeleton jumps from
    C to OCaml, the assembly named in a role;
  - a C prototype counts as a definition, `def:` may land on it: check
    the line;
  - an `.mli` over implementations in subdirectories (`arm/`,
    `arm64/`, chosen by a Makefile) is not paired: the `.mli` shows no
    uses;
  - a module alias (`module L = Lexer`) hides its uses;
  - calls through a record of functions (Plan 9's devtab) are
    invisible to "called by";
  - the test directories are left out silently: describe them in the
    parent's `dirs:` anyway.
- `\'` inside a single-quoted jsonnet string does parse; double quotes
  remain the clearer choice. An unclosed `comment:"...` gives a
  confusing "no comment saying" error: check the quotes first.

### Centrality, not size (the author, 2026-09-29)

"Playground.computer, Playground.game ... are arguably the most
important types and functions in the whole project yet are not really
highlighted by anything"; "games and apps are like device drivers in a
linux kernel; they are not the core of the project". What the project is
written with matters more than what is written with it:

- The brief (`-facts`) says, for each file, how many files name its
  module (open, include, a qualified name), and flags A HUB: a file
  named by a twentieth of the project or more. A hub's main types and
  functions are capitals of the whole map, whatever its size:
  Playground.mli's `game`, `computer`, `shape`. The map draws a capital
  as large as its file is central, and from afar shows only the
  capitals of files others depend on.
- An `.mli`'s declarations get their `.ml`'s uses in the brief: read
  them there (Playground.mli's `game`: 219 files), not the 0 an
  interface alone would show.
- A program nobody names (a game, an app: the brief says "a driver")
  gets its capital, its heart, but is not the core: its capitals are
  seen from its genre, not from the top.
- The APIs over the libraries (Audio.mli, Physics.mli, Gui.mli) and the
  3D API are hubs of their kind too: capitals on what programs call.
- Describe the hubs first and best: a pass that leaves the core bare
  (as the first did the playground) has its priorities wrong.

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
- Lessons of the pass that gave every program its skeleton (the agents'
  reports), each now in the brief or the template:
  - The heart is on the program's path: the brief's "called by" says
    which definitions call it; from `update`, `skeletons.game`; from
    `view` (a 2.5D game's trick, a 3D game's world), `skeletons.drawn`;
    in a kit, `mvu` and a bone in the kit joined from `update`.
  - The capital is not always the heart: a capital may be data (a
    course's table) or a drawing; the heart is what the rules or the
    picture run through.
  - "The template's names" line says which of `type:model`,
    `def:initial_model`, `def:update`, `def:view` a program has: give
    its own where it says NO (apps: `init='def:initial'`; a first state
    written inside `app`: `init='def:app'`).
  - "Defined twice": `def:` finds the first; for the second, a
    `comment:` near it, or `line:` with a comment saying why.
  - A program written on a way (Teletype, Textmode, Bigbang, Karel,
    Povray) has no update or view of its own: `skeletons.way(file, name,
    parts, at, role)`, its parts, then `app`, then the way's function.
  - The other shapes in `skeletons.libsonnet` (the examples' pass): `via`
    (a heart in a library, `Physics.simulate`), `untyped` (a state with
    no type, its first value in `app`), `scene3d` and `still` (a 3D scene
    that only turns, a picture with no update; `playground=` the path to
    playground/ from the config).
  - "Called by" misses a call inside a lambda (`List.iter (fun n ->
    sound_of n)`): an empty "called by" is no proof; read the code before
    choosing another heart.
- Every program gets one: each game and app of a genre's or category's
  config, not a few examples (the author, missing TinyMissileCommand's:
  "I thought the libsonnet would help for that"). The template makes it
  a line; the names it assumes (`type:model`, `def:initial_model`,
  `def:update`, `def:view`) are given where a program names them
  otherwise, after the facts.
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
- `section:` takes the title as the banner writes it, without quotes
  around it: `"section:The formulas (Dexed's dx7note.cc)"`. An odoc
  heading, `{1 ...}` in a comment, is no section: anchor it with
  `comment:`.
- A `comment:` phrase must be words the comment has in a row: not across
  markdown emphasis (`**...**`) nor escaped quotes; pick a nearby phrase.
- `def:` finds a name's first definition: two `eval`s in a file, the
  first; avoid anchoring the second.
- A Lisp defun inside an OCaml string is out of the anchors' reach:
  `line:` only there (it moves, say why beside it).
- Only OCaml and C files are sources: a `.js`, `.st` or `.css` beside
  them is described in a comment of the config, or by the module
  embedding it, not in `files:`.
- A generated file (`Photos.mli`) is described like the others: the map
  reads it from the disk.

## Jsonnet pitfalls

- An apostrophe inside a single-quoted string ends it: write "its
  ghosts' ways" in double quotes.
- A function's named arguments are `name=value`, a field's `name: value`.
- A text with an apostrophe inside a quoted anchor reads best in double
  quotes, its inner quotes escaped (`\'` also parses):
  `at: "comment:\"MPEG-1's idea: the pixels didn't change\""`.

## Across configs (learned writing them all at once)

- A directory with its own config needs no line in its parent's `dirs:`:
  its own summary is the one shown; a parent's line for it would drift.
- A skeleton may have bones in other directories (`../../libs/audio/`):
  they hold when checked from the repository's root, which is how to
  check (`tinybox codemap -check .`); checked alone, the directory
  reports them "not a source here".
- A directory without sources of its own but with subdirectories
  (`libs/graphics/videos`) gets a config with its summary, or it shows
  "not described yet".

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

## Layers

A layer lights, at every level at once, the lines containing its rules'
texts, each rule in its colour (the map's `l` cycles through them; the
search's `"text` is where one tries a rule first, ctrl+Enter keeping
it). Write one when a question cuts across the directories: what may
touch the world (the root's Capabilities: `Cap.network`, `Cap.exec`,
`Cap.fork`...), where a deprecated API is still used, where a protocol
is spoken.

- Name the colours with jsonnet locals (the author), so that the rules
  read: `local fork_color = '#e05050';` then
  `{ text: 'Cap.fork', color: fork_color, say: 'forks a process' }`.
  Rules that mean the same kind of thing share a colour (exec and fork,
  both processes).
- `say` is the legend's: what a line lit so means, in a few words.
- A text specific enough to match only what is meant: `Cap.fork`, not
  `fork`. Two characters at least; smart case (a capital: the case
  counts). Semgrep-like patterns will come later.
- A layer belongs in the config of the directory whose question it is:
  the root's for the whole repository.

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
