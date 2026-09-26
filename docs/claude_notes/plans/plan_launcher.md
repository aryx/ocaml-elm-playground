# Plan: one installed launcher for every game and app -- TinyBox

## Context

The games (`games/<genre>/`) and apps (`apps/<category>/`) can only be
run from a clone, with `dune exec`. Installing each executable under
opam's `bin/` is out: about 200 programs, 8-10 MB each (1.5 GB in
`_build`), with names like `Pong` and `Snake` in a shared directory.

The author's direction (2026-09-26): **one binary**, a launcher, with a
retro front end in the spirit of RetroArch or an arcade cabinet's menu:
the games by genre, each with its screenshot, the original it is after
and a link to that original, started from the menu or from the command
line:

    tinybox                     the menu
    tinybox list                every program, by genre
    tinybox TinyMario           run one (case-insensitive, "mario" is enough if unique)
    tinybox TinyDoom3d -debug-keys   the platform's flags pass through

The name is decided (2026-09-26): **tinybox**, after RetroArch-style
retro boxes and after BusyBox, since it is the same idea: one
multi-call binary instead of hundreds. Like BusyBox, it also dispatches
on `argv.(0)`. A symlink `tinymario -> tinybox` runs TinyMario, and
`tinybox --install <dir>` makes those links for whoever wants them in
`PATH`. Nothing is linked by default, so opam's `bin/` gets only
`tinybox`.

The launcher is installed by a new opam package, `elm_playground_games`.
It depends on the backend packages, and they do not depend on it.

## What stands in the way

1. **A game runs when it is linked.** Every program ends with
   `let main = Playground_platform.run_app app` (or `run_app3d`, or
   `Cap.main (fun caps -> ...)`), evaluated when its module is
   initialized. If 200 of them were linked into one binary, the first
   would start and the others would never run.
2. **The native loops call `exit`** on quit (`Native_loop_2d`,
   `Native_loop_3d`: window closed, "Q"). `run_app` never returns, so
   the menu cannot run a game in-process and get control back.
3. **Only one implementation of a virtual module per binary.** The 2D
   games link `elm_playground_native` (Cairo), but the 3D ones link
   `elm_playground_software` as their 2D platform, with the OpenGL 3D
   backend. The launcher must pick one 2D implementation.
4. **Module names must be unique in one link.** `Undo` is both
   `gamekits/puzzle/Undo.ml` and `appkits/document/Undo.ml`. The
   `apps/devtools/tty/` copies of `TinyVi`/`TinyEmacs`/`TinyTurboPascal`
   stay out, since they are the terminal builds.
5. **argv.** The native loops parse `Sys.argv` from the start, and
   `Playground_platform.flags ()` reads it too, so `tinybox TinyMario`
   would see `TinyMario` as an app flag.

## Design

### A program is registered, then run (fixes 1)

This is a one-line change per program. A small module in `elm_core`,
`Program`:

```ocaml
(* [main name run]: the program's entry point. Alone (its own .exe or
 * .bc.js), [run ()] at once, as today; inside the launcher, only
 * recorded under [name], run later if chosen. *)
val main : string -> (unit -> unit) -> unit
val registered : unit -> (string * (unit -> unit)) list
val launcher_mode : bool ref   (* set by the launcher's library, initialized before any game *)
```

Each program's last line goes from

```ocaml
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
```

to

```ocaml
let main = Program.main __MODULE__ (fun () ->
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
```

The executables' names, the web builds (`copy_files`), the golden tests
and `CATALOG.md` do not change. `launcher_mode` is set by a
one-module library, `launcher_mode`, that only the launcher links.
Libraries are initialized before the executable's modules, so it is
true before any game's `main` runs.

The change is mechanical: a script over `games/*/*.ml apps/*/*.ml`,
checked by one full build and `make test-lite`. A semgrep rule in
`semgrep.jsonnet` then keeps `let main = Playground_platform.run_app`
from coming back in `games/` and `apps/`.

Games that do real work at top level (tables, levels, decoded
embedded images) now pay for it at the launcher's start, all 200 of
them. Measure `tinybox list`'s time, and make the offenders `lazy`.
Each one is a small local diff.

### Capabilities: each program keeps its own Cap.main (fixes 5 too)

`Cap.main` hands out the capabilities once per process (a dynamic
check, "Cap.main() already called"), and it is called by the program,
in its main, not by the playground (`plan_caps.md`). That is the
design: a program whose main does not call it has no authority. tinybox
keeps both rules:

- `Program.main` never calls `Cap.main` and never hands out anything. It
  only delays the program's entry, a `unit -> unit`, where the program's
  own `Cap.main` stays, written where it is today:

```ocaml
val main : string -> (unit -> unit) -> unit

let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
let main = Program.main __MODULE__ (fun () -> Cap.main (fun caps ->
  Playground_platform.run_app ~flags:(Playground_platform.flags ())
    (app (caps :> < Cap.readdir; Cap.open_in >))))
```

- **Once per process, in tinybox too**: a process of tinybox is either
  the menu or one program, never both. `tinybox <Name>` runs that
  program's entry and nothing else, so the program's `Cap.main` is the
  process's first and only one. `tinybox` alone is the menu, a program
  like the others with its own `Cap.main` (its authority: `exec`,
  `fork` and `wait` for the child process, `exec` again to open the
  original's page in a browser, the store for last played and
  favourites). The menu never runs a program in its own process. It
  starts a child, which is required anyway, since the loops `exit`.
- **argv**: `Playground_platform.flags ()` stays as it is, the
  platform's (the trusted base) one read of the command line. The
  platform, `Native_loop_2d`/`_3d` and `flags` read a new
  `Program.argv ()` instead of `Sys.argv`. Under
  `tinybox TinyWinamp dir=~/Music`, it drops the leading `tinybox`, so
  TinyWinamp sees `[| "TinyWinamp"; "dir=~/Music" |]`, as it would alone.
  (Making `flags` take `Cap.argv` would be more honest, but then the 24
  programs reading flags would all call `Cap.main`. That is a separate
  decision.)
- **Attenuation, as `Cap.mli`'s own example**: a program's main can
  wrap a capability before passing it on. The first worked example is
  TinyWinamp's directory:

#### TinyWinamp: songs from a directory

`tinybox TinyWinamp dir=~/Music` (or `TinyWinamp.exe dir=~/Music`, or
the flags line in the menu, remembered). TinyWinamp's main asks for
`< Cap.readdir; Cap.open_in >` (dir= comes from its flags). At start it lists `dir`
(recursively, or one level: to decide), keeps the `.mp3` and `.mp2`
files, reads their `Id3` tags, and makes the playlist of them in place of
`Our_media`'s. Without `dir`, or on the web, it plays its own songs as
today. As with TinyPalmPilot, the caps reach `update` for the file
opened when a song starts, so a big directory is not read up front.

Optionally, as a lesson: the main narrows `readdir`/`open_in` to paths
under `dir`, a wrapper object that refuses the rest (`Cap.mli`'s
example of restricting `open_in`). Then TinyWinamp can read your music
and nothing else, and the type does not change, only the object.

### The menu starts the game as a child process (fixes 2)

The menu is itself a 2D Playground app. Choosing a program runs
`tinybox <Name> <flags>` as a child process (`Cap.exec`, the same binary
through `Sys.executable_name`). The menu waits, redraws once the child
is gone, and keeps its place. This design brings:

- a fresh process for each game, so no global state is left over
  (`Random`, SDL, the GL context, audio devices, the `Worker` threads);
- `exit` in the loops stays as it is;
- a game that crashes does not take the menu with it: the menu shows
  "TinyFoo exited with 2" and its stderr tail.

Dispatch, in the launcher's main: `argv.(1)` names the program, which
is removed from `Sys.argv` before calling its `run`. The native loops
use `Arg.parse_argv ~current:(ref 0) Sys.argv`, so an overwritten
`Sys.argv` is enough, but it is a global. The clean fix is an optional
`?argv` on the loops, a small change in `Native_loop_2d`/`_3d` and in
`Playground_platform.flags`.

### Which 2D backend (fixes 3) -- done

The 2D games must keep Cairo. Run on the software 2D rasterizer, they
would be too slow (the author, 2026-09-27). So tinybox links
`elm_playground_native` (Cairo) + `elm_playground_3d_opengl`.

What stood in the way: `elm_playground_3d_opengl` depended on
`elm_playground_software`, which `(implements elm_playground)`, for
one module, `Shape_render_software`, the HUD's renderer. Linked with
`elm_playground_native`, that gave two implementations of
`Playground_platform`, a link error.

Moving `Shape_render_software` into a library of its own does not
work: it draws `Playground.shape`, so that library depends on
`elm_playground`, and dune forbids an implementation of
`elm_playground` (`elm_playground_software`, which needs it too) to
depend on a library that depends on `elm_playground` ("Library
elm_playground was pulled in").

**Done (2026-09-27): a copy, compiled under another name.** A rule in
`platforms/native/dune` copies `Shape_render_software.ml`/`.mli` to
`Hud_render.ml`/`.mli` inside `elm_playground_3d_opengl`, which no
longer depends on `elm_playground_software` (only on the `graphics_*`
libraries it installs). There is one source file, compiled twice, with
no name clash when a program links both. The dune file says why at
length. Checked: `TinyTron3d` linked with Cairo builds, runs, and
dumps a frame byte-identical to the usual build's. The genres' 3D
stanzas are unchanged.

Other ways, if the copy ever becomes a burden: the HUD drawn through
a new `Playground_platform` function, so that Cairo draws it with an
ARGB surface (antialiased, faster, no matting); or `Playground`'s types
in a library that is not virtual (too big a move).

The golden frames stay rendered by the software rasterizer. The
thumbnails come from them, so a 2D game's thumbnail is not
antialiased like the game run from tinybox. That is fine for a
thumbnail.

### Name clashes (fixes 4)

Rename one of the two `Undo`s (`Puzzle_undo`, or `Doc_undo`). This
touches only its users. A dune rule in the launcher's directory fails
the build with a readable message if another clash comes later, though
the linker already fails anyway.

### Where the launcher gets its data

- **The list**: `CATALOG.md`, embedded at build time by a dune rule, and
  parsed with the parser `tests/catalog/Unit_catalog.ml` already has
  (moved into a small library both use). Each row gives the name,
  genre, 2D/2.5D/3D, After, In one line and What it brought. Programs
  missing from `Program.registered ()` are greyed out. The catalog test
  already guarantees every program has a row.
- **The screenshots**: the first golden frame of each program,
  `tests/2d/golden/<Name>.png` or `tests/3d/golden/<Name>.png`. A
  build-time step downscales it to a thumbnail (our own `Png` +
  `graphics/imaging`'s `Scale`, e.g. 256 px wide), embedded as base64
  like `file_to_base64_ml` does. That is about 200 × ~30 KB, so a few MB
  in the binary. The full frame is shown in the detail view.
- **The link to the original**: `CATALOG.md` names it but has no URL.
  Add a column (a Wikipedia URL, the obvious stable choice) or a
  `name -> url` table beside the launcher. Opening it natively is
  `xdg-open`/`open` through `Cap.exec`. The menu also shows the URL as
  text, for a machine without a browser.

### The front end

A 2D Playground app, written like the games (sections, header, what it
uses). It is a program of its own and also appears in `CATALOG.md`
under a "system" or new "launcher" row, with its golden frame:

```
+--------------------------------------------------------------+
| TINYBOX          < PLATFORM >   arcade shmup fps ...  [/]find |
+--------------------------------------------------------------+
| [thumb] [thumb] [thumb] [thumb] |  TinyMario            2D    |
| [thumb] [THUMB] [thumb] [thumb] |  +----------------------+   |
| [thumb] [thumb] [thumb] [thumb] |  |    screenshot        |   |
|                                 |  +----------------------+   |
|                                 |  After: Super Mario Bros.   |
|                                 |  (Miyamoto, Nintendo, 1985) |
|                                 |  Run, jump, stomp ...       |
|                                 |  What it brought: ...       |
|                                 |  [Enter] play [o] original  |
|                                 |  [s] source  [f] flags      |
+--------------------------------------------------------------+
```

- **Games and apps, two shelves**: the apps (`apps/<category>/`) are
  first-class, not an afterthought. The top bar switches between GAMES
  (their genres) and APPS (office, media, music, internet, pim, system,
  devtools, graphics, cad, education, gamedev). An app's detail panel
  shows the same fields (After, In one line, What it brought) and adds
  what it reads or writes: the store's documents (`File_menu`), the
  network (TinyMosaic, TinyIRC). The flags line defaults to the app's
  own (`cart=` for TinyDX7). An app granted `Cap.network` or the store
  is still run as a child, with the capabilities its own `main` asks
  for, so the launcher gives nothing more than the standalone .exe
  would.
- **Navigation**: arrows, Tab across genres or categories, `/` to type
  a filter (across both shelves), Enter to run. Mouse too.
- **Look**: a CRT-ish frame (scanlines as thin translucent rectangles, a
  vignette), a bitmap-ish title in Hershey strokes, a cursor that blinks
  and plays a sound (`Audio`). This is `Juice` material, and it can be
  switched off with `juice=off` like the games.
- **Per-game extras**: "last played" and "times played" kept in the
  store (`Playground_platform.store`, already capability-checked);
  favourites marked with `*`.
- **Flags**: a small line editor for the flags passed to the game
  (`artwork=shapes`, `-debug-keys`), remembered per game.

## Packaging

- `dune-project`: a tenth `(package (name elm_playground_games) ...)`
  whose dependencies are the backend packages. The launcher's
  `executable` gets `(public_name tinybox) (package
  elm_playground_games)`. Nothing else is installed: no per-game
  executables, no `share/` (screenshots and data are embedded).
- The launcher's directory: `launcher/` at the top level, beside
  `games/` and `apps/`. Its dune file lists every program's library
  dependencies, which is the union of the genres' stanzas. To keep that
  union from drifting, each genre could become a library (`games_arcade`,
  ...) holding its modules. But then the genre's executables link that
  library instead of listing modules, so this is best done as a separate
  first step. Otherwise the launcher copies the sources with
  `copy_files`, like `web/`, and lists libraries by hand; the build
  fails at once if one is missing.
- Web: out of scope. The web builds stay one page per program. A
  browser menu would be the website, which is the author's to do.

## Steps

1. `Program.main` (a `unit -> unit`, the programs' own `Cap.main` inside) + `launcher_mode`, the script
   over every program (the `Cap.main` ones included), one build, `make
   test-lite`. (Pure refactor, no behaviour change.)
2. `Program.argv ()`, `flags` and the native loops reading
   `Program.argv`; the `Undo` rename. (`Shape_render_software`: done,
   copied as `Hud_render`, see "Which 2D backend".)
3. `launcher/` with a text-only launcher: `tinybox list`, `tinybox
   <Name>`. The binary exists and every program runs from it. Measure
   start time and binary size, and fix slow top levels.
4. The package in `dune-project`; `opam install .` in a scratch switch;
   check `tinybox` is the only thing in `bin/`.
5. The catalog parser as a library; thumbnails at build time.
6. The front end: grid, detail panel, filter, child process, crash
   report.
7. URLs of the originals (the `CATALOG.md` column), "open original".
8. Juice: CRT look, sounds, last played and favourites.
9. `CATALOG.md` row and golden frame for TinyBox itself.
10. TinyWinamp's `dir=`, and then its narrowed `open_in`. This does not
    need tinybox and can come any time after step 2.

## Open questions for the author

- **Genre libraries** (the dependency union) before the launcher, or
  `copy_files` and a hand-kept list?
- **URLs**: a new `CATALOG.md` column (another thing its test checks),
  or a table private to the launcher?
- **Package name**: `elm_playground_games` installs the apps too.
  Simply `tinybox`? Then `opam install tinybox` gives `tinybox`.
- **Examples**: in the launcher too (their own section), or games and
  apps only, as `CATALOG.md`?
