# Plan: names in the code map -- where a name is defined, where it is used

## Context

After the parsers (2026-09-28: `languages/ocaml/Parse_ml`,
`languages/c/Parse_c`, their highlighters coloured by the tree) and
`tinybox codemap <dir>`, the next step asked for: a click on a name in
the code map goes to its definition, and the uses of a name are lit --
codemap's and every editor's "go to definition".

The author's question, before any code: "how do you detect the
definition of a name? How do you resolve names for OCaml and for C?
this is not trivial, especially on a codebase containing multiple
projects". It is not, and this plan's rule is to say which answers are
exact and which are guesses, and never to present a guess as an
answer: one definition found by a rule that cannot be wrong jumps; a
guess among several shows them, ranked, for the reader to choose.

## Three levels, from exact to guessed

### 1. In a function: exact

A parameter or a local is resolved already, with the language's real
scopes, by the highlighters' `resolve` (`Highlight_ml`, `Highlight_c`):
a match case's names are its own, a `let`'s name is in scope in its
body only, a C local from its declaration to its block's end, `for (int
i ...)` its own scope, a parameter shadowed by a local. Their
environment is `(string * category) list`; it becomes
`(string * (category * int)) list`, the int the binding's token. Then
each use knows its binding, and:

- a click on a local or a parameter goes to its binding;
- the name under the mouse has its binding and all its uses lit (the
  uses are the tokens whose binding is the same token).

Wrong only where the parser gave up (Parse_ml's and Parse_c's
`skipped`): there the tokens have the guess's categories and no
binding, so nothing is lit and a click does nothing -- silent, not
wrong.

### 2. In a file: exact, or nearly

- OCaml: a top-level name is the latest top-level `let` (or `val`,
  `type`, `external`, `exception`, `module`) of that name before the use:
  a later one shadows it. `let rec ... and ...` sees its own names.
  Exact in the file, except under an `open`: an unqualified name defined
  nowhere in the file may come from an opened module (level 3).
- C: one namespace for the file's functions, globals, types, struct
  tags, enum constants and macros (C's separate tag namespace kept:
  `struct Foo` looks among tags). A `static` definition is the file's
  alone, so a use of it in the file is exact. A non-static one used in
  the file is also its definition, but a file may use a name defined
  elsewhere with the same name in another program (level 3).

The index of a file: its top-level definitions (name, kind, line and
column, static or not, exported by an `.mli` or not), made from the
same parse that colours it (`Code_file.make`, lazily, as now).

### 3. Across files: a search, exact where the language allows

An index of every file's top-level definitions (level 2's), for the
files of the map (a program's, the repository's, a directory's), made
as the files are lexed. What is searched, and in what order:

**OCaml.**
- `M.x`: `M` is a module; with unwrapped libraries (this repository,
  most of xix and ix) a module is a file, `m.ml`, and `Code_deps`
  already finds it by name. Then `x` among `M`'s top-level definitions,
  its `.mli` first (what it exports), its `.ml` for the body.
- `open M` (or `M.(...)`, `let open M in`), then a bare `x` not defined
  in the file: `x` among the opened modules' definitions, the last open
  first (OCaml's own rule).
- Several files of one module name (`Test.ml`, `Main.ml`, `Parser.ml`
  in two compilers): ambiguous without the build; level 3's nearness
  (below) ranks them, and the build (level 4) decides.
- Not attempted: functors' results, `include` of a functor's
  application, first-class modules, objects' methods: the name is left
  unresolved (a click says so), rare in the corpora (ocaml-light, which
  xix and ix are written in, has none of them).

**C.**
C has one namespace for a whole linked program, and a codebase like
Principia Softwarica holds hundreds of programs: each Plan 9 command has
its own `error()`, `emalloc()`, `main()`. The search, from the use:
1. the file (`static` definitions first);
2. the files its `#include "x.h"` name, in its directory (a program's
   own header, where its shared functions and structs are declared);
3. its directory (a Plan 9 program is a directory and its mkfile);
4. the libraries: the directories `#include <x.h>` point to (`include/`
   and its subdirectories, found by name), and the `lib*` directories
   (`lib_core/libc/`...) -- a prototype in a header found, the
   definition searched among the libraries' `.c` files by name;
5. anywhere else in the project, ranked by nearness.
A declaration (a prototype, an `extern`) is not a definition: a click
on a use goes to the definition; the prototype is listed second.

**Nearness.** Several candidates at the same step are ranked by the
length of the path they share with the use's file (the same directory,
then the parent's...), then by name. The first, if it is alone at its
rank, is the answer; else a list.

**Projects.** A map may hold several projects (the repository, `~/xix`,
which has principia as a link -- not followed). A project's root is a
directory with a `dune-project`, an `mkfile` or `Makefile` at its top,
or a `.git`. The search stays in the use's project, and crosses a root
only when nothing inside matches -- then says so in the list.

### 4. Later, if the guesses mislead: the build

- OCaml: the `dune` files say which library holds which modules and
  which libraries each depends on (`languages/sexpr` reads them): the
  search restricted to the dependency closure, and `Parser` decided,
  not ranked. Wrapped libraries (`Lib.M`) too. The compiler's `.cmt`
  files would be exact (merlin's way, pfff's `graph_code_cmt`) but
  depend on the compiler; not this repository's spirit.
- C: the mkfiles (`OFILES`, `LIB`, `HFILES`) say what is linked
  together: exact for Plan 9. And principia's `skip_list.txt`
  (`dir_element: 386`, `dir: APE`), made for pfff's codegraph to cut
  exactly these duplicates, read as it is.

Only if levels 1 to 3 prove wrong often enough on the three corpora to
be worth it: measured first (below).

## What codemap and codegraph did

pfff's `graph_code` (codegraph's) built a graph of every definition
and use per language: OCaml from `.cmt` files, C from the AST with a
global namespace. Duplicates (two `error`s) were reported as warnings
and removed from the graph unless a skip list chose one -- hence
`skip_list.txt` in principia. Here the same ambiguity is shown to the
reader instead of removed, and nearness orders it; no graph is kept,
only the per-file indexes and a search at the click.

## The code

- `libs/code/`, beside `Highlight_code` (shared by every language): the
  types of names -- a place (line, column, length), a definition (name,
  kind, place, static or exported), a use's reference:
  `Bound of place` (level 1 or 2, exact), `Qualified of string list *
  string` (`M.x`, for level 3), `Free of string` (for level 3). And a
  file's names: its definitions, and each name token's reference.
- `Highlight_ml` and `Highlight_c`: `resolve` keeps the binding token
  (level 1) and the file's top-level definitions (level 2); their
  `categorize` also gives the names (one parse for both).
- `launcher/codemap/Code_file`: a file's names kept with its spans.
- `launcher/codemap/Code_names` (new, pure): the index of the map's
  files and the search (level 3): `find : index -> from:string -> ref
  -> candidate list`, ranked. Tested without a screen.
- `Code_view`: the name under the mouse, its binding and uses lit; a
  click on it: one candidate, the file opened (or scrolled) at the
  definition, the definition lit; several, a list over the view (the
  path, the line, the kind), arrows and Enter or a click to choose;
  none, "not found" and why (a functor's, outside the map). Backspace
  goes back where the click was (a stack of places, as an editor's
  "go back").
- The map: the same on the map itself when it is readable (zoomed in),
  later; the file view first.

## Steps, each tested and measured

1. Level 1: the binding token in both resolvers; `Code_view` lights
   the uses of the local under the mouse and jumps to its binding.
   Tests: the highlighters' "scopes" examples, with their bindings.
2. Level 2: the file's definitions, a click on a top-level name used in
   its file. Tests: shadowing (OCaml), `static` (C).
3. Level 3 for OCaml's `M.x` and `open M`, then for C's steps 1 to 5;
   the list for several candidates. Tests: a small tree of files in
   the test (two `error`s in two program directories and one in a
   library; two `Parser.ml`).
4. Measure on the corpora: for every name token of this repository,
   xix, ix and principia, how many resolve to one definition, to
   several, to none -- per language, per level. That number decides
   level 4. A throwaway harness, as the parsers' was.
5. The web: the same in tinybox's web code map (the sources are there;
   the index built lazily per file, a browser's time in mind).

## Open questions for the author

- A click's gesture: the click itself (which now does nothing in the
  file view but on the overview), or with a modifier (Ctrl-click, as
  editors), leaving the click free for something else?
- The list of candidates: over the file view, or in the status line
  cycled with a key (codemap's way was a menu)?
- Level 4 at all, or is a ranked list always good enough for reading?
