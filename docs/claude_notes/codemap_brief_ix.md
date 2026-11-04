# The code map's configs for ~/ix: the brief

Written 2026-11-04 by Claude for the agents updating ~/ix's
`.codemapconfig` files, one area each, after a month of new code
(1,400 commits since the first pass of 2026-09-29); kept for the next
pass.

## What to read first

- The guidelines, all of it:
  `~/github/ocaml-elm-playground/docs/claude_notes/codemapconfig_guidelines.md`
  (its sections on ~/ix, ~/principia and xix matter most).
- The format: `~/github/ocaml-elm-playground/docs/manual/codemap.md`,
  section 12.
- ~/ix's root config `~/ix/.codemapconfig` and its shapes
  `~/ix/skeletons.libsonnet` (`cli`, `chain`, `onefile`, `module`,
  `calls`, `module_calls`, `select`, `parts`): read them, do not edit
  them. Use the shapes (`local skeletons = import
  '../skeletons.libsonnet';`, with as many `../` as needed). Note `cli`'s
  core is `[anchor, role]` pairs (no say); the others take `[anchor,
  role, say]`.
- ~/ix's `CLAUDE.md` and `README.md` (the m-IX and t-IX tables: what
  each program is, its Plan 9 original, its tiny twin).
- An existing config of the first pass as an example of the style:
  `~/ix/version_control/.codemapconfig`, `~/ix/shell/.codemapconfig`,
  `~/ix/tiny/.codemapconfig`.

## The project explains itself

- Each area's `README.md` (`browsers/README.md`, `games/README.md`,
  `languages/README.md`, `lib_playground/README.md`, `tiny/README.md`)
  and the `.mli` headers: ix's modules open with a long header comment
  (the idea, a diagram, the history); `(* See X.mli *)` means the header
  is in the `.mli`.
- `docs/plans/` and `docs/plans/done/plan_*.md` (the design of each
  program, its steps), `docs/manuals/`, `docs/notes/`.
- The tiny twins: a mini program and its `tiny/Tiny*.ml` (the README's
  tables; the root config's views) -- a summary may say what one keeps of
  the other.

## The two kinds of work

1. **Stale configs** (the first pass's, 75 of them): `-check` says
   "changed since it was described" (reread the file, fix its summary,
   capitals and important lines if they changed, then copy the new
   digest), "no def X" / "no such source" (code renamed or removed: find
   where it went), "is in a subdirectory: describe it in that
   directory's own .codemapconfig" (move the description into a new
   config of that subdirectory), and skeletons whose bones are gone.
   Edit these configs in your area: that is the job.
2. **New code**: directories and files no config describes, modules of
   150 lines or more and folders of two sources or more with no
   skeleton, programs with no skeleton, hubs with no capital. Write
   their configs.

## The rules

- **Edit or create `.codemapconfig` files in your area only.** Edit no
  source, not the root's config, not `skeletons.libsonnet`, not
  `.codemapignore`. If one of them needs a change, say it in your
  report.
- **Every directory holding a source (`.ml`, `.mli`, `.c`, `.h`) needs
  its own config**: a parent's `files:` cannot describe a file below it
  (a mistake), and a folder's skeleton is read only from its own
  config. A directory without sources but with subdirectories gets a
  config with its `summary`. `tests/`, `*_tests/`, `build/`, `_make/`
  and the other paths of `~/ix/.codemapignore` are off the map; say what
  they are in the parent's `dirs:` if worth it.
- Each config you write or touch:
  `generated: { by: 'claude-opus-5-5', on: '2026-11-04' }`, its
  `summary`, `files:` (each with `summary`, `digest`, and where they earn
  it `capitals`, `important`, `links`, `related`), its `skeletons`,
  maybe `tours` for a program's directory, `views` pairing a program with
  its tiny twin when your area holds both.
- Items: `at`, `say`, `weight` (3 for the two or three that matter most,
  1 otherwise); a bone's `role`; a joint's `from`, `to`, `say` whose ends
  must be bones of its own skeleton (one bad joint makes the whole
  config unread: every file under it "missing").
- Skeletons: every folder of two sources or more, and every module of
  150 lines or more, needs its own (bones mostly in it). A program
  (`Main.ml` calling `Cap.main`, or a `let () =`/`let _ =` reading argv,
  a kernel's `kmain`): `skeletons.cli`/`skeletons.onefile` from its
  directory, the core after the CLI's main, extended with
  `bones+:`/`joints+:` for a loop. A whole-file bone (a path, no anchor)
  counts toward its file.
- Capitals: the hubs first. -check names them ("a hub ... with no
  capital"): its main types and functions. At most three a file; most
  files have none.
- Anchors: `def:` and `type:` first; `comment:"words"` (words on one line
  of a comment), `code:"words"` (the first line of code holding them),
  `section:`; never `line:` unless nothing else reaches.
- Summaries: what it is for, one sentence of 60 to 100 characters, no
  "This file", no counts of lines. Say what it ports or is after (Plan
  9's original, a book, a paper) and what is new.
- Notes (`say`): 30 to 60 characters, why, not what.
- Name the real code's findings in important lines (a bug, a stale
  comment, dead code).
- Jsonnet: an apostrophe ends a single-quoted string (use double
  quotes); named arguments are `name=value`.

## How to work

- Your scratch directory is your own (given in your prompt): the facts
  and check outputs go there, never in the repository.
- The tools, from `~/github/ocaml-elm-playground` (do not build it; it
  is built):
  - `./bin/tinybox codemap -facts ~/ix <dir>` prints a directory's brief
    (its files, digests, definitions and uses). One directory at a time;
    each run reads the whole project (10 to 60 s with a dozen agents at
    once): run several in the background, or one at a time.
  - `./bin/tinybox codemap -check ~/ix` checks every config, about a
    minute alone, longer with a dozen agents: run it rarely, in the
    background, its output in your scratch directory, and grep your
    paths. 0 mistakes, 0 warnings and 0 missing in your area is done.
    A structural mistake is reported one per config at a time: fix and
    rerun until two runs agree. `-check ~/ix/<your dir>` is faster but
    reports bones outside it "not a source here": the whole check is
    the judge.
- Other agents write the other areas at the same time: the check shows
  their mistakes and missing too; ignore them. Do not touch a path
  outside your area even to fix it; report it.
- Your area is large: you may fork a few agents of your own (subagents
  with this brief, a subdirectory each, their own scratch directory);
  two at most at once (twelve areas run together: the machine has 64
  cores).

## Report

At the end, report in under 300 words: the configs written and edited
(counts and the directories), what -check says for your area (its
numbers), what you could not fix, and your LESSONS: what this brief, the
guidelines, the checks or the facts got wrong for this codebase (a
tool's mistake, a missing shape), shortest first.
