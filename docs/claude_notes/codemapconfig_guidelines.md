# Writing a `.codemapconfig`

For an LLM (or a person) writing the configs the code map reads
(`plan_codemap_v2.md`; the format: `launcher/codemap/Code_guide.mli`).
The map draws what the config says: a directory's name and card, a
file's card, the capitals seen from afar, the lines drawn larger up
close, the tours. So a config is judgement, written once: what a
newcomer should see first, and in what words.

The worked example to imitate: `games/shmup/.codemapconfig`
(TinyInvaders) and `gamekits/shmup/.codemapconfig` (its kit).

## Processing a new project, start to finish

For a session asked to "make the code map useful for <project>" (xv6,
~/principia, any directory), with only this file to go on. The tools
are tinybox's, in ~/github/ocaml-elm-playground (`./bin/tinybox` after
a `make` there); the map's manual is its `docs/manual/codemap.md`
(section 12: the format). Run the tools from that repository, giving the
project's path.

1. **Look first.** `tinybox codemap <project>` (the map as is, no
   config), the project's README, its own CLAUDE.md or architecture
   notes, `tinybox codemap -facts <project> .` (its top folders, their
   sizes). Decide who the reader is and what they came for: to learn an
   OS's paths, a compiler's passes, a library's API, a game's rules.
   **Look for the project's own explanations of itself** (the author,
   ~/principia): an architecture diagram on its web site, a walkthrough
   ("The Journey of ls": a command traced function by function through
   every layer), a book's "Software architecture" section printing a
   call chain. They are the skeletons and the tours, already judged by
   the author: copy their layers, their colours and their chains before
   inventing any. And decide what is off the map: vendored or
   compatibility code, build copies, symlinked duplicates, `node_modules`
   -- a `.codemapignore` line each (the project may already have one,
   its lines commented out).
2. **Write the root config yourself, first** (`<project>/.codemapconfig`):
   the `title` (the project in a sentence), `colors` per top folder,
   and the skeletons that cross the whole project -- its layers (a
   directory bone each), and one chain per path a reader must follow
   (below). These frame every other config. And its `anatomy:`, the
   X-ray's nerves and lungs, rules naming this project's inputs
   (keyboard, events, interrupts, a syscall's user bytes) and its I/O
   (files, disk, console, network): `{ nerves: [...], lungs: [...] }`,
   each a `ref:` (a name used, `Unix.read`, `ll_rw_block`) or words on
   the line; `-check` reports a root without them (the words the map
   guesses from otherwise are a web program's, not a kernel's).
3. **Write the project's `skeletons.libsonnet`** at its root: the
   shapes its parts repeat (ix: `cli`, Main to the CLI to the core;
   Linux 0.01: `chain`, a path of [anchor, role, say]; this repository:
   `mvu`, `game`, `drawn`...). A shape one config would repeat ten times
   belongs there. A shape's bones go down to where the program starts:
   `mvu`'s state, first state, update and view, and the `app` that hands
   them to the Playground and the `main` that runs it (the author: the
   skeleton links main and app to view, update and model) -- a skeleton
   that stops short of the entry point leaves the reader asking how it
   is run.
4. **Split the rest** by top folders among parallel agents (a few
   hundred files each at most), each given: the project's path, this
   file, the manual's section 12, the root config and the libsonnet to
   read (not edit), its area, and the rule to create only new configs
   in its area and edit no source. Ask each for its LESSONS: what the
   brief, the checks or this file got wrong for this codebase. Write the
   brief once, in a file each agent reads (~/principia's is kept in
   `docs/claude_notes/codemap_brief_principia.md`: start from it), and
   say in it: where the project's own explanations are (step 1); the
   item fields (`at`, `say`, `weight`; `role` in a bone; `from`, `to`,
   `say` in a joint, whose ends must be bones of its skeleton); that
   every directory holding a source needs its own config (a parent's
   `files:` or `dirs:` cannot describe a file below it); a scratch
   directory of its own per agent (ten agents in one directory overwrite
   each other's facts); and to run `-facts` in the background, one
   directory at a time (each run analyses the whole project: 20 to 60 s
   with ten agents running). Ten agents did ~/principia's 2,200 files
   in about half an hour; each may fork a few of its own (twenty at most
   at once).
5. **Check until done.** `tinybox codemap -check <project>` from the
   tools' repository: 0 mistakes, 0 warnings, 0 missing. Then look at
   the map (earth, a region, a file, `x`, `l`, `g`) as the reader
   would, and fix what reads wrong.
6. **Fold the lessons back** into this file, and into the tools when a
   lesson is the tools' (the brief, -check, a language's reader): the
   next project starts where this one ended.

### What is useful, by kind of project

The configs are for a reader's questions. Ask what they are, then give
each an answer the map can draw:

| project | the reader asks | give |
|---|---|---|
| an operating system (Linux 0.01, xv6, ~/ix's kernels) | how does it boot? what happens on a system call, a page fault, a timer tick, a read from disk? | a chain skeleton per path, across C and assembly (the entry label to the C handler to what it does); the core structures as capitals (the process, the inode, the buffer); a layer for what only a kernel may do (interrupts on and off, I/O ports, task switches, user memory); views pairing a subsystem's files |
| a compiler or interpreter | what are the passes? where is the AST? how is a name resolved, a type checked, code emitted? | the pipeline as a skeleton (lexer, parser, checker, code generator); the AST's types as capitals; a tour through one expression compiled |
| a library | what do I call? what is inside only? | the `.mli`'s (or header's) main types and functions as capitals; the skin plate shows the rest; a view of the API with an example using it |
| a program with a UI (a game, an app) | where is the state, the frame's step, the picture? what makes it this program? | Model-View-Update and the program's heart (`skeletons.game`); the heart's trick as a capital; a tour through one frame |
| a set of programs (~/ix, a Unix's commands) | what does each do, and what do they share? | `cli`-like shapes per program; the shared libraries as hubs; views pairing each program with its twin or its library |
| a toolchain, a build system | what flows from what? | the data's path as a chain (source to object to executable); the formats' types as capitals |

Across all: the entry points (a `main`, a boot label, an interrupt
vector) and the few structures everything shares are what a newcomer
needs first; the rest is found by searching.

### Old code, C and assembly (Linux 0.01)

- Assembly is read (labels are definitions, a.out's leading `_`
  dropped in names but kept in anchors: `def:_system_call`); a chain
  may start in assembly and go on in C.
- A C prototype in the file (`extern int system_call(void);`) no longer
  hides the body elsewhere: its uses resolve across files.
- The comments are the author's own explanation, often long and
  excellent (Linus's `|` and `/* */` notes): summaries should say what
  they say, shorter, and important lines should point at the tricky
  places they warn about.
- The lines that matter most are often uncommented (1991 C): anchor
  them with `code:"words"`, the first line of code containing them
  (`code:"verify_area"`), not `line:`.
- `def:` finds a body before its prototype; a name defined by a macro
  (`_syscall1(int,close,int,fd)` defining `close`) is invisible: name
  the file, and say it in the summary.
- In a kernel, nearly every file is a driver or a subsystem with an
  entry point worth a capital (unlike a game's helpers): one per file,
  its entry (`schedule`, `do_page_fault`, `rw_hd`), and the shared
  structures (the process, the inode, the buffer) in the headers.
- Real bugs of the historical code (a precedence slip, a wrong
  variable) are worth an important line saying so: the reader will
  wonder otherwise.
- C resolves as it links: a name is found in the use's own top folder,
  a top-level library (`lib/`), or a file sharing a header with the
  use's (one program); an include only in its includer's top folder or
  an `include/` directory. A dependency that should be there and is not
  (a table of function pointers, a macro's call) is worth a joint in a
  skeleton, saying how it happens.

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

### Every module and every folder its skeleton (the author, 2026-09-29)

"Ideally every file, every folder (and enclosing folders)" has a
skeleton: the X-ray at any unit shows that unit's own structure, never
an enclosing folder's with its bones off the map. -check says what is
missing: a module (an `.ml` or `.c` of 150 lines or more: below, the
derived one is honest enough) or a folder
(two sources or more) with no skeleton of its own. Until one is
written, the map derives one from the code (its capitals and important
lines, else its most used definitions; for a folder its most tied
parts), marked "(derived)": a stand-in, not the judgement.

- A module's skeleton: 3 to 7 of its definitions, the ones its reader
  must hold to follow the rest: its main type, its entry points, the
  one algorithm it is about; joints for who calls whom or what flows
  where, each with its `say`. Most bones in the file itself (a bone in
  the library it stands on is fine); named for what it shows ("A
  mkfile read: lines, assignments, rules"), not "Model-View-Update" for
  a module that is none.
- A folder's skeleton: its parts (its files, or its subfolders), the
  data or control between them: "the pipeline", "the layers", "the kit
  and its users". Whole units as bones (a path, no anchor), their roles
  the role each plays in the folder, not their own summaries.
- An enclosing folder's shows the folders under it as bones: at the
  root, the areas (games/, libs/, ...); at libs/, its libraries.
- A tiny module (a type and two functions) needs 3 bones, not padding;
  under 150 lines the derived skeleton will do unless its structure
  says more than its calls.

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
    C to OCaml, the assembly named in a role (Plan 9's `.s` is read:
    see ~/principia's lessons);
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

### What ~/principia taught (its first pass, 2026-09-29)

Ten agents wrote ~/principia's 310 configs (2,200 C, header and
assembly files, Plan 9 as literate books) in half an hour, from a root
config and a brief written first. Their reports, general first, then
C's, then the tools' still to fix:

- **The project explains itself; use it.** The best skeletons came
  from the author's own: the web site's "Software Architecture of Plan
  9" diagram gave the root's layers and colours (a book a box, a colour
  a kind: tools, toolchain, libraries, kernel, graphics and network);
  "The Journey of ls" gave two chains and the root's tour, function by
  function; each book's `\section{Software architecture}` printed its
  program's call chain (5c's `main -> compile -> yyparse -> codgen ->
  gen -> cgen -> regopt -> peep -> outcode`, rc's, mk's, mothra's trace
  of a request). Where the book was a stub (networking, games), the
  summaries came from the code's headers, and read thinner.
- **Every directory holding a source needs its own config.** A file's
  note is read only from its own directory's config, and a folder's
  skeleton only from its own: a parent's `files: { 'a/b.c': ... }` is
  silently ignored, and `dirs:` describes a directory, not its files.
  `dirs:` is for directories without sources (build output, docs).
- **A module's skeleton counts when most of its bones are in it.** A
  chain across files, two bones in each, leaves every file "missing":
  give the big ones a skeleton of their own besides. A whole-file bone
  (a path) counts toward its file -- the only honest skeleton of a file
  that is one table (`optab.c`, `enam.c`, tcs's Unicode maps). A
  program's `cmd` skeleton covers its module too.
- **A joint's ends must be bones of its skeleton**, and one bad joint
  makes the whole config unread: every file under it shows "missing".
  Read the first mistake line before anything else; `-check` reports
  only the first bad anchor of a config, so fix and rerun.
- **Shapes the agents wrote again and again** (make them the project's
  libsonnet's, before splitting): `module(file, name, steps)`, a file's
  own chain with its anchors bare (six agents defined it as a local);
  a star, `calls(name, center, callees)`, for a `main` calling ten
  initializations (`chain` forces a joint between neighbours); a
  dispatch through a table of functions (`devtab`, a `Dev`, lib9p's
  `Srv`, `fcalls[]`, printf's `ocvt`); a 9P server (main, the
  `read9pmsg` loop, the handlers); `cmd` with no default role for main
  (a third of Plan 9's commands parse no flags, or by hand); a
  one-line-file helper for forty trivial files.
- **Name the real code's findings in important lines.** The pass found
  Plan 9's own bugs and stale comments (libthread's `tprivfree` never
  unlocking `privlock`; `fastrand.c` saying X9.17 over a ChaCha
  generator; two libsec programs calling functions libsec lacks): a
  reader will wonder, the note answers.

C, Plan 9's (anchors):

- **The syncweb markers are the best anchors of a literate project**:
  `comment:"function [[mountio]]"`, `comment:"struct [[Node]]"`,
  `comment:"function [[_vsvc]](arm)"` -- exact, they survive edits, and
  they reach assembly functions and struct bodies that `def:` and
  `type:` miss. Copy the marker's words exactly (its kind, struct or
  type or enum; its name, which may differ from the label's).
- **`def:` on Plan 9 C**: static functions are declared at the top of
  the file, so a name is "defined twice"; `def:` prefers the body (rank
  3) and mostly lands right, but not always (tcp.c, devmnt.c's
  `mountio`, rio's fsys.c): check the line the facts give, and when in
  doubt use the marker, or `code:"name(Type arg"` -- the body's
  signature with its parameters' names, which prototypes omit. `type:X`
  lands on `typedef struct X X;`, not the struct: the marker, or
  `code:"struct X{"` when the brace is on its line. Enum constants are
  no `def:`: `code:`.
- **`code:"words"`**: the first line of code (not comment) holding the
  words -- in the manual's table now. Whitespace is exact and a tab is
  not matched: pick words without the alignment. Keep quotes out (`\"`
  is not unescaped). In a `.s` file, a phrase with `(` or `*` does not
  match (`_main(SB)`): take a word of the line (`code:"setR12"`).
- **Assembly**: Plan 9's `TEXT name(SB)` symbols are not definitions,
  but its plain labels (`_vswitch:`, `_f32loop:`) are, and its `/* */`
  comments are comments: anchor a function by its marker. (~/ix's "not
  a source" is ~/ix's `.tm`; Plan 9's `.s` is read.)
- **Invisible, say it in the joints**: calls through function
  pointers (the kernel's `sched`, `error`, `print`, pointers that
  `main` fills, `core/portfns.c`; `devtab`; `Proto`, `Medium`,
  `Ether`; rio's channels), yacc actions (`cc.y` calling `codgen`, `a.y`
  calling `outcode`: `.y` files are not sources, so neither is a
  `main` in `hoc.y`), rc scripts (git9's commands), and the mkfile's
  cross-directory builds (5c compiling `../cc2/pgen.c`).

The tools, fixed after the pass (2026-09-29):

- An `#include` counts for a header, never a same-named `.c` (lib_gui's
  `draw.c` had 172 files, `<draw.h>`'s now 288); as near, the path
  sharing more directory names wins (an x86 file's `"dat.h"` is
  `core/386/`'s); `include/security/auth.h` and Linux 0.01's `errno.h`,
  `string.h`, `sys/stat.h` became the hubs they are.
- A C name resolves in its own top folder before a library (the
  kernel's `qlock` for the kernel, libc's for the programs), and two
  files are one program by a header they share only when it is not the
  system's (one included from six top folders or more: `libc.h`), so
  troff no longer "uses" sam's `linep`.
- `threadmain` is a program; a parent's `files:` naming `sub/x.c` is a
  mistake, no longer ignored; `code:` is in the manual.

The tools, still to fix (the facts and the check lie here; read past
them):

- `<u.h>` goes to MIPS's for a file of no architecture: the mkfile's
  `$objtype` decides, which no path says. An assembly file's register
  words resolve to random C (`memmove.s` "uses" libmemdraw's `arc.c`):
  the "Uses:" of a `.s` file are noise. A kernel and its `user/`
  programs share a top folder, so names still cross between them.
- A structural mistake (a joint's end no bone) stops the config's
  reading at the first: every file under it shows "missing", and one
  run shows one such mistake. The anchors' mistakes are all reported.
- Top folders and directories without sources are never reported
  missing: write them.
- **Missing is incremental**: describing files reveals programs the
  first check did not list. Check until two runs agree.
- **The facts' header** can be a syncweb marker, commented-out code or
  a license repeated in every file; section banners in C headers come
  two characters short (no `section:` there). Prototype-only headers
  (`portfns_*.h`) list their prototypes as definitions used 0 times.
- **Not built, still demanded**: rc's `unix.c`, mk's `Posix.c`, an
  empty `.s`, a generated `proctab.c` or yacc's output, troff's
  `tmac.s` (macros, not assembly) -- say so in the summary, skeleton
  them briefly.
- **`.gitignore` is not read**: `node_modules/` and generated files
  come in unless `.codemapignore` names them.
- `-facts` takes one directory and analyses the whole project each
  time; `.y` files are not read, so a yacc action's calls are unseen.

### What ~/github/xix taught (its first pass, 2026-09-30)

Seven agents wrote xix's 109 configs (1,100 OCaml and kernel C files,
Plan 9's programs ported, each with its book) in about twelve minutes,
from a root config, `skeletons.libsonnet` and a brief written first
(`xix/docs/claude_notes/codemap_brief_xix.md`: the next project starts
from it). Their reports:

- **Check a book's tables against the directory.** occ.nw, oas.nw,
  olk.nw, orio.nw and CompilerGenerator.nw have empty "Code
  organization" and "Software architecture" headings; orc.nw's and
  ogit.nw's tables name files renamed, moved or never written. The
  pipelines came from each `CLI.main` instead.
- **The shapes asked for, now in xix's libsonnet** (copy them to the
  next project's): `onefile` (a program in one file, no Main.ml or
  CLI.ml: ar.ml, ogit's main.ml, the utilities), `module_calls` (a star
  of a module's own functions, bare names), `select` (a thread looping
  on `Event.select`, a bone per case, each back to the loop). Still
  wanted: a per-architecture fan (`assemble5/6/7/v/i`: each config
  defined its own locals), the `X_`/`X` split (types, then functions:
  OCaml forbids the cycles C's dat.h allowed), a dispatcher's match
  cases (`code:"| O.Pipe ->"` reaches a case; a case is no `def:`).
  `calls(...) + { bones+:, joints+: }` combines a star with a loop.
- **A module that is one function** (Layout5, Rewrite5, Datagen) gets
  its bones from its comments (`comment:"step1: mark"`): they count.
- **Unbuilt and empty files are still demanded** (18 empty `sys*.ml`
  placeholders, `todo/` directories, a `.ml` in no build file): say so
  in the summary; an empty file's digest is `d41d8cd98f00`.
- **Invisible calls, said in the joints**: records of functions
  (ogit's `cmd.Cmd_.f`, `Client.fetch`), `Obj.magic` records choosing
  an architecture (`Arch_linker.of_arch`), OCaml's `external`s into C,
  and `caml_startup`, defined in a generated `ocaml.c` not in the tree
  (`def:` lands on the prototype).
- **Two builds, one name**: dune links opam's fpath, logs and the
  stdlib, the mkfile xix's own (lib_core/base, collections, core are
  ocaml-light's stdlib, mk only); an unbuilt copy (concurrency/todo's
  `event.ml`) steals "used by" by name. A directory's summary says
  which build it belongs to.
- **Anchors:**
  - `code:` never matches a commented-out line or a `//` line:
    `comment:` there.
  - In `.ml` files `(` and escaped quotes in `code:` work
    (`code:"| \"44\""`); in `.s` files `(`, `*`, `$` and `,` do not.
    A `\t` in jsonnet is a real tab, never matched.
  - `code:` also matches inside a string literal (handy in OCaml).
  - `code:"name(Type arg"` can land on a Plan 9 prototype that keeps
    its parameters' names (`zsort`): check the line.
  - A syncweb marker is the best anchor in OCaml too: `comment:"The
    globals"` reached editor's second `type t`.
- **The format, as -check teaches it**: a tour stop needs an anchor (a
  bare path is refused, "a stop names its file"); `links` are
  `{from, to}`, no `say`; the totals line ("N configs, N mistakes, N
  warnings, N missing") is what proves the configs were read -- an empty
  grep of one's paths proves nothing.

The tools, fixed after the pass (2026-09-30):

- A `.codemapignore` below the root is read, for the paths under its
  own directory, as git's: xix's `caps/` (a submodule, its own project)
  leaves out its `tests/` and `scripts/` itself. Only the root's was
  read, and an agent wrote seven configs for fixtures the author had
  already ignored: look for the subprojects' ignore files first.

The tools, still to fix (xix's reports):

- The facts' "Sections" garble OCaml comments too ("to be updated once
  you insert tex; tod; less"), and a file starting with a syncweb marker
  and code shows a fragment of code as its header.
- `-facts` stars module aliases (`module R = Runtime`) among the most
  used, and calls a `.mll` "a program nobody names"; -check calls
  `Lexer_asm.mll` "a hub (named by 0 files)". `.mll`/`.mly` are read and
  anchorable (`Parser_asm5.mly:def:program`) but never demanded.
- A program `let _ = Cap.main` in a `tests/` file is sometimes not
  flagged as one (hellorio.ml, hellodraw2.ml).
- `<string.h>` still counts toward `Libmemdraw/string.c`'s fan-in.
- Headers of 150 lines or more (`mlvalues.h`) are never asked for a
  skeleton.

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
- The map's jsonnet has no `std.findSubstr` (`std.startsWith`,
  `std.endsWith`, `std.filter` are there).
- A function's named arguments are `name=value`, a field's `name: value`.
- A text with an apostrophe inside a quoted anchor reads best in double
  quotes, its inner quotes escaped (`\'` also parses):
  `at: "comment:\"MPEG-1's idea: the pixels didn't change\""`.

## Across configs (learned writing them all at once)

- A directory with its own config needs no line in its parent's `dirs:`:
  its own summary is the one shown; a parent's line for it would drift.
- A directory holding a source needs its own config: its files' notes
  and its skeleton are read only there (a parent's `files:` naming
  `sub/x.c` is ignored without a word).
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

## Anatomy: nerves and lungs

The X-ray's plates 3 and 4 show where a program senses its user (nerves:
keyboard, mouse, events) and where it breathes with the world (lungs:
files, network, processes, the console). Only the project knows how it
does either, so its root config says it, as rules like a layer's
(`text` or `ref`, a `say`, no colour); a directory's config may add its
own, applying to the files under it:

```jsonnet
anatomy: {
  nerves: [{ text: 'computer.keyboard', say: 'the keys held' }, { ref: 'Scene2d.pressed' }],
  lungs:  [{ ref: 'Cap.open_in', say: 'reads a file' }, { text: 'Unix.', say: 'the OS' }],
}
```

- This repository: the Playground's `computer.keyboard`/`mouse`, `Sub`'s
  events; `Cap.*`, `Unix.`, HTTP, a sound out.
- An OS kernel: its nerves are the interrupt handlers for the keyboard
  and the timer, `inb` from the keyboard's port; its lungs the disk's
  and the console's drivers, `outb`, the user memory copies.
- A command-line program: `argv`, `stdin` for nerves; files and
  `stdout` for lungs.
- Without rules the plates fall back to a list of words, a guess: write
  them for every project.

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
