# Brief: writing ~/principia's .codemapconfig files for your area

(The brief given to the ten agents of ~/principia's first pass,
2026-09-29, corrected by what they reported: start a new project's
brief from it, changing the project's paths and sources.
codemapconfig_guidelines.md, step 4, says what a brief needs.)

You write the code map's configs (`.codemapconfig`, jsonnet) for one area
of ~/principia (Principia Softwarica: a fork of Plan 9, a literate book
per top directory, the C sources beside the `.nw` books).

## Read first (in this order)

1. `/Users/pad/github/ocaml-elm-playground/docs/claude_notes/codemapconfig_guidelines.md`
   -- the guidelines, all of it. Especially "Old code, C and assembly",
   "Other languages, other projects", "Every module and every folder its
   skeleton", "Centrality, not size", "Anchors the checker taught".
2. `/Users/pad/github/ocaml-elm-playground/docs/manual/codemap.md`
   section 12 (the format) and 13.
3. `~/principia/.codemapconfig` (the root, written already) and
   `~/principia/skeletons.libsonnet` (shapes: `chain`, `cmd`, `parts`).
   Read them; do NOT edit them. Import the libsonnet with a relative
   path (`local skeletons = import '../../skeletons.libsonnet';`).
4. A worked C example: `~/work/linux-0.01/.codemapconfig` and its
   subdirectories' configs; `~/github/ix/` has 69 configs (OCaml).

## The books are your best source (the author's advice)

Each top directory has its book: `Intro.nw`, `<Book>.nw` (e.g.
`kernel/Kernel.nw`, `compilers/Compiler.nw`), `*_extra.nw`. Look for
`\section{Software architecture}` (grep it) and the Intro's overview:
they often print the program's call chain (5c's `main -> compile ->
yyparse -> ... -> codgen -> gen -> cgen`) or its structure -- use them
for the skeletons, and the books' explanations (the `%claude:` and the
author's paragraphs) for the summaries and notes. `docs/principia/Principia.nw`
("Code organization", "Software architecture") and `docs/ls-journey.html`
give the whole system. `lineage.txt` gives a tool's history (e.g. its
original). Don't copy prose: pick what a card of one line needs.
Grep the .nw rather than reading 20,000-line books whole.

## Tools (run from /Users/pad/github/ocaml-elm-playground)

- `./bin/tinybox codemap -facts ~/principia <dir>`: the brief of a
  directory (files, digests, headers, definitions with their uses and
  callers, hubs, programs). Start every directory with it.
- `./bin/tinybox codemap -check ~/principia 2>&1 | grep '<your dir>'`:
  mistakes, warnings, missing, for your area (the whole repo is checked;
  filter to yours). The final line `N config, M mistakes...` is the
  whole repo's; other agents work in parallel on other areas, so their
  lines are not yours to fix.
- Do NOT run make or dune (another session may be building); just run
  the binary. Do not edit any source file, the root config, the
  libsonnet, or `.codemapignore`. Create only `.codemapconfig` files in
  your area: one per directory holding a source, even a single file
  (a file's note and a folder's skeleton are read only from its own
  directory's config; a parent's `files: {'sub/x.c': ...}` is ignored).
  `dirs:` is for directories without sources.
- Use a scratch directory of your own (`<scratchpad>/<your area>/`):
  other agents write theirs beside it. Run `-facts` in the background,
  one directory at a time: each run analyses the whole project.

## Done means

For your area, `-check` shows 0 mistakes, 0 warnings, 0 missing: every
file described (summary + digest), every program (a C `main` or
`threadmain`) with a skeleton, every module of 150+ lines and every
folder of 2+ sources with a skeleton of its own, every hub with a
capital. Put `generated: { by: 'claude-opus-5-5', on: '2026-09-29' }`
in each config.

Priorities (centrality, not size): the hubs and the book's main program
first and best (its skeleton from the book's architecture section, its
entry points and core structures as capitals, important lines with
short `say`s); then the rest. Architecture-specific subdirectories
(`arm/`, `386/`): the books present ARM; x86 gets short but correct
descriptions. Test programs, `user/` helpers, `misc/`: one line each is
fine, but they still need their summary and (for a program) a skeleton
(a `skeletons.cmd` of 2-3 bones is enough for a tiny one).

## What is known about the tools on this codebase

- The syncweb chunk markers are the surest anchors:
  `comment:"function [[mountio]]"`, `comment:"struct [[Node]]"`,
  `comment:"function [[_vsvc]](arm)"` (copy their words exactly).
- Plan 9 assembly's `TEXT _start(SB)` symbols are NOT definitions (its
  plain labels are): anchor a function by its marker, or `code:` with a
  word of the line free of `(` and `*`.
- Plan 9 C declares its static functions at the top: `def:` prefers the
  body but not always; check the line the facts give. `type:X` lands on
  `typedef struct X X;`: use the marker. `code:"words"`: the first line
  of code holding them; whitespace exact, no tab, no inner quotes.
- The item fields: `{ at, say, weight }` in capitals and important;
  a bone `{ at, role }`; a joint `{ from, to, say }`, its ends bones of
  the same skeleton (else the whole config is unread: every file shows
  missing). `-check` reports one bad anchor per config: fix, rerun.
- A module's skeleton counts when most of its bones are in its file; a
  whole-file bone (a path) counts toward it (a file that is one table).
- A C file's header in the facts may be a syncweb marker.
- Names defined by macros are invisible; calls through tables of
  function pointers (the kernel's `devtab`, `Dev` records; rio's
  channels) are invisible to "called by": say them in joints.
- Name resolution is still loose in places: `<u.h>` goes to MIPS's,
  names cross between the kernel and its `user/` programs. The "Uses:"
  of a `.s` file are noise.
- The map's jsonnet has no `std.findSubstr` (`std.startsWith`,
  `std.endsWith`, `std.filter` are there).
- Jsonnet: an apostrophe ends a single-quoted string -- use double
  quotes for text with apostrophes.

## Report back (your final message, short)

1. What you wrote (configs, counts) and the final `-check` numbers for
   your area.
2. LESSONS: what the brief, the checks, the facts or the guidelines got
   wrong or missed for this codebase (tool bugs, false "missing", anchors
   that failed and why, things you wished the libsonnet had). Concrete,
   one line each.
