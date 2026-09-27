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
val collect : unit -> unit      (* from now on, record instead of running *)
val collected : unit -> (string * (unit -> unit)) list
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
and `CATALOG.md` do not change. `collect ()` is called by a
one-module library of the launcher's, which only tinybox links.
Libraries are initialized before the executable's modules, so it is
called before any game's `main` runs. (Step 1, done 2026-09-27:
`Program` in `elm_core`, the 199 mains rewritten, `games/template.ml`
included, and semgrep's `main-through-program` rule. Top-level work
found on the way, for step 3: TinyTetris seeds `Random` from its flags
in a `let () =`, and TinyMinecraft queues its world's sectors.)

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

Rename one of the two `Undo`s. Done (step 2): the puzzle kit's, the
one with fewer users, is now `Puzzle_undo` (TinySokoban, TinySokobanEd,
TinyBabaIsYou, TinyBraid). A later clash fails tinybox's link, which
names the module.

### Where the launcher gets its data

- **The list**: `CATALOG.md`, embedded at build time by a dune rule, and
  parsed with the parser `tests/catalog/Unit_catalog.ml` already has
  (moved into a small library both use). Each row gives the name,
  genre, 2D/2.5D/3D, After, In one line and What it brought. Programs
  missing from `Program.collected ()` are greyed out. The catalog test
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

### After Batocera and RetroBox: browsing a big collection

The author's direction (2026-09-27): take the retro front ends as the
model -- Batocera and RetroBox (both on EmulationStation's ideas, as are
Recalbox and RetroPie), which make thousands of games browsable. With
~200 programs, the section-by-section grid is enough to start with, but
it is not how one *finds* something: "a 2-player game", "something from
the 80s", "what did id Software make", "what did I play yesterday".
What they have, and what it would be here:

| Their feature | Here | Data needed |
|---|---|---|
| Systems carousel (a big logo per console) | the shelves and sections as a carousel of big titles, the section's thumbnails as its background | none |
| Gamelist metadata (`gamelist.xml`: name, description, image, genre, players, release date, developer, publisher, rating) | name, One line, What it brought, the golden frame, the section; **players** new; **year**, **developer/publisher** parsed from After ("(Shigeru Miyamoto, Nintendo, 1985)"); a twin ("After TinyDoom") takes its twin's | a Players column (below) |
| Filters: genre, players, decade, favourites, played or not, developer | a filter bar: players (1, 2, 3, 4+, online), era (70s, 80s, 90s, 2000s, 2010s+), look (2D, 2.5D, 3D, app), favourites, never played, developer; filters combine, the grid shows what passes, the count says how many | players, year, developer |
| Sort: name, year, players, last played, times played | the same, and the catalogue's order (the default: the sections' own order, the genres' history) | the store's counts |
| Automatic collections: All, Favourites, Last played; dynamic ones by genre, decade, developer | "collections" as extra sections before the genres: Favourites, Last played, Most played, 2 players, the 80s, Nintendo, id Software, Twins (a 2.5D game and its 3D twin side by side), and the author's own (TinyTronscroll, 1997) | as above |
| Custom collections (the player's own lists) | a collection made in the menu, kept in the store | the store |
| Video snaps playing in the detail view | the program's other golden frames (`<Name>_<scene>.png`, a game deep in play) shown in turn after a second on it: a slideshow of the game playing, from frames that `make test` already guarantees | more thumbnails (a few per program, ~25 KB each) |
| Screensaver / attract mode (random games' videos when idle) | after a minute idle, random programs' frames full screen, with their name and After: an arcade's attract mode | the slideshow's frames |
| "Random game" button | `r`: a random program, among those the filters pass | none |
| Jump to a letter | a letter typed (outside search) jumps to the first name starting with it | none |
| Per-game settings (Batocera's advanced settings) | the flags line, remembered per program (`artwork=shapes`, `juice=off`, `net=host`) | the store |
| Play count, last played, game time | counted by the menu around its child process (it knows when one starts and ends) | the store |
| Kid mode / kiosk | a flag of the menu's: no apps, no flags line, a whitelist | none |
| Netplay menu (host or join a game) | for the games with `Multiplayer` (TinySpacewar, TinyTronscroll): "host" and "join" in the detail panel, i.e. `net=host` / `net=join` flags | an "online" mark (Players: `2 (online)`) |
| Gamepad navigation | the playground's gamepad, if it has one; else a plan item of its own | -- |
| Menu music, navigation sounds, themes | `Audio`'s chiptunes and clicks; the palette as a theme, chosen in the menu | none |
| Scraper (metadata fetched from the web) | not needed: our "scraper" is the build, CATALOG.md and the golden frames read at build time | -- |

And one thing they cannot have, because theirs are ROMs: **the source**.
A key (`s`) opens the program's header comment (what it is after, the
trick it writes out, what it uses and what it leaves as exercises),
embedded at build time like the thumbnails, scrollable: the
repository's point, a toy you can read, one key away from playing it.

**The data: decided (2026-09-27), the catalogue is improved** -- new
columns in `CATALOG.md`, checked by `tests/catalog`, rather than lines
in the programs' headers. Players is the only field that cannot be
computed from what exists; the original's platform (arcade, console,
home computer, PC: Batocera's "systems", for a filter) and its URL
("open the original", step 7) are the other candidates. The two ways
that were weighed:
- a **Players column in `CATALOG.md`** (`1`, `1-2`, `2`, `1-4`,
  `2 (online)`), filled once from the programs' headers and code (their
  `Multiplayer`, their second player's keys), checked by `tests/catalog`
  like the other columns: one place, readable in the table;
- or a line in each program's header comment (`Players: 1-2`), read at
  build time: the data next to the code, but 198 files touched and
  parsed from comments.
The first is recommended. Year and developer are parsed from After,
with a test that every game's After gives a year (an app's After is a
program, which has one too). The original's platform (arcade, console,
home computer, PC) would be one more column, later, if filters by
platform are wanted (Batocera's "systems").

## Packaging

- `dune-project`: a tenth package, `tinybox` (decided 2026-09-27),
  whose dependencies are the backend packages; the launcher's
  `executable` is `(public_name tinybox) (package tinybox)`. Nothing
  else is installed: no per-game executables, no `share/` (screenshots
  and data are embedded). After a `make`, also `./bin/tinybox` (`bin`
  a committed symlink to `_build/install/default/bin`).
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

1. `Program.main` (a `unit -> unit`, the programs' own `Cap.main` inside) + `collect`, the script
   over every program (the `Cap.main` ones included), one build, `make
   test-lite`. (Pure refactor, no behaviour change.)
2. `Program.argv ()`, `flags` and the native loops reading
   `Program.argv`; the `Undo` rename. (`Shape_render_software`: done,
   copied as `Hud_render`, see "Which 2D backend".) Done 2026-09-27:
   `Program.run name ~argv` sets the argv the loops parse (lazily,
   inside the entry, so after `run`); `Unit_program` in core's tests.
3. `launcher/` with a text-only launcher: `tinybox list`, `tinybox
   <Name>`. The binary exists and every program runs from it. Measure
   start time and binary size, and fix slow top levels. Done
   2026-09-27, with the package (step 4's first half, installed by `make
   install`) and `-tty` (TinyVi, TinyEmacs, TinyTurboPascal in the
   terminal, a table in `Tinybox.ml`, since `apps/devtools/tty/`'s
   modules have the GUI versions' names):
   - 198 programs, a 41 MB binary. A frame dumped through tinybox is
     byte-identical to the program's own .exe's (TinyTron, TinyTron3d
     on OpenGL over Cairo, TinyFirefox with its `Cap.main`), and so is
     one started BusyBox's way, through a link named `tinytron`.
   - `copy_files` of the genres' and apps' folders, `Tiny*` only where
     a folder has a library of its own (music, media, internet) or
     shares a generated module (gamedev's `Mario_xpm`, platform's).
   - Start: 2.4 s, then 0.9 s once TinyVirtuaRacing's meshes and
     TinyMinecraft's world were made lazy. Left, if it matters:
     TinyVirtuaRacing's courses (0.23 s) and TinyMediaPlayer's first
     item, opened by its `initial_model` (0.18 s); the rest is a long
     tail of under 0.1 s each.
   - Top-level reads of the command line, which got tinybox's words:
     TinyTetris's seed, TinyVi's and TinyEmacs's `phosphor`, moved into
     their mains. `Program.argv` now fails if called while collecting,
     and `launcher/dune` runs `tinybox list` in `make test`, so a new
     one fails the tests.
   - Not yet: `tinybox --install <dir>`, the links for BusyBox's way.
4. `opam install .` in a scratch switch; check `tinybox` is the only
   thing in `bin/`.
5. The catalog parser as a library; thumbnails at build time. Done
   2026-09-27: `launcher/catalogue/` (`Catalogue`, the sections with
   their intro and rows, Markdown taken out); `launcher/data/`'s
   `make_tinybox_data`, the first golden frame halved twice (250 by
   250), in 8 shards dune runs side by side (a thumbnail is 0.3 s,
   mostly our PNG decoder: 55 s alone, 8 s so).
6. The front end: grid, detail panel, search, child process, the exit
   status. Done 2026-09-27: `Tinybox_menu`, a 2D Playground game;
   `tinybox` alone (or with platform flags, `-dump-frame`) opens it.
   The menu is a program registered as "tinybox", run by `Program.run`
   like the others. 3 by 4 thumbnails, the chosen one at 400 by 400,
   After, One line, What it brought; Tab and Shift-Tab across the 29
   sections, g/a the shelves, `/` search, Enter or a double click
   starts `tinybox <Name>` as a child process, polled each frame ("is
   running", "exited with n"). Text is left-aligned by an estimate
   (0.47 em for Cairo's sans-serif).
7. URLs of the originals (the `CATALOG.md` column), "open original".
8. Juice: CRT look, sounds, last played and favourites.
9. `CATALOG.md` row and golden frame for TinyBox itself.
10. TinyWinamp's `dir=`, and then its narrowed `open_in`. This does not
    need tinybox and can come any time after step 2.
11. After Batocera, the data: `CATALOG.md`'s new columns, decided
    2026-09-27: Players, Platform (arcade, console, home computer, PC,
    handheld) and Year (explicit, rather than parsed from After); the
    developer parsed from After; `tests/catalog` checks them. No URL
    column (step 7 waits). Done 2026-09-27: the three columns in all
    198 rows, their words in CATALOG.md's introduction (Players: `1`,
    `2`, `1-2`, and ` (net)`; the 13 platforms); `Catalogue` reads them
    (`plays`, `online`, `decade`, `platforms`), and `Unit_catalogue`
    checks every row's. Players found in each program's code (a second
    player's keys, a "2: two players" menu, `Multiplayer`): 17 play with
    two (12 of them alone too), 3 of those over the network. The developer, parsed from After, is left
    for later.
12. Filters, sorts, and the collections as sections (Favourites, Last
    played, 2 players, the 80s, a developer, Twins); `r` random, a
    letter to jump. Done 2026-09-27, the first half: `b` groups by
    genre (the default), era (a decade a section, oldest first), machine
    or players; `p`, `e`, `m`, `l` filter by players, era, machine and
    look, `c` clears, `r` a random program of the grid; a filter bar
    under the section's title, its words clickable; the year, machine and
    players in the detail panel. Left: sorts, the store's collections
    (step 13), Twins, the jump to a letter.
13. The store: favourites, play counts, last played, time played,
    flags per program, custom collections.
14. Previews and demos (revised 2026-09-27, after the author: "we
    could just run the game and it would render", within the second
    after which Netflix starts its autoplay):
    - **The grid keeps its embedded first frames** (decided): instant,
      5 MB; rendering 12 thumbnails means 12 programs started. The
      extra embedded frames (a slideshow of the golden scenes: 441
      frames, 11 MB, 25 s of build) are dropped.
    - **The detail panel's preview runs the program, in the menu's
      process**: a child process is out (tinybox's start alone is
      0.9 s, then the window), but inside tinybox every program is
      already linked and initialized. An `app` is a record of functions
      (`init`, `update`, `view`, `subscriptions`): the native platform's
      `run_app` gets a capture hook, set only by tinybox, which hands
      the app over packed (`Any : ('m, 'msg) app -> any_app`) instead of
      opening a window; the menu calls the program's entry with it on,
      then plays the app itself -- a small platform: the `computer`
      (1000 by 1000, the time, the keys of the program's golden scene
      script through `Input_script`) turned into its messages by its
      `subscriptions`, as `Native_loop_2d` does from SDL's events -- and
      draws its `view` scaled by 0.4 into the 400 by 400 panel, after
      about a second on it.
    - Its limits: the 25 programs calling `Cap.main` keep their image
      (their entry's Cap.main would be the process's second: a preview
      gets no authority, by design); 3D games keep theirs, unless the
      SVG backend's compiler (a scene to 2D shapes) can be reused;
      `Audio` needs a mute switch, since games play sounds from
      `update`; 40 programs have no scripted scene, and preview their
      title screen.
    - Done 2026-09-27, the preview: `Playground.capture` (hidden from the
      docs), checked by the native platform's `run_app`;
      `Audio.silently`; `Tinybox_menu`'s "Previews": after 60 frames on a
      program (frames, not seconds: `-fixed-time` shows them), its main
      called with the hook set, its app cached, played a frame per menu
      frame on its first scripted scene's keys, started over at the
      scene's end plus 90 frames, drawn scaled by 0.4 with bands of the
      background round the panel. Checked on dumped frames: TinyInvaders
      (its burst), TinyMario (scrolled), TinyDoom (the 2.5D renderer);
      TinyFirefox keeps its picture (its Cap.main). A main that prints
      its help prints it once, to the terminal, when first previewed.
    - Done 2026-09-27, the 3D previews: `Playground3d.capture3d` (the
      app and its rendering), checked by the OpenGL platform's
      `run_app3d`; `Playground.update_keyboard` exported (hidden); the
      menu builds the program's computer (the script's keys, a
      1000-by-1000 screen) and steps `update3d`; its views rasterized
      at 400 by 400 by the software rasterizer, compiled into the
      launcher as `Preview3d_render` (a copy of
      `Shape3d_render_software`, as `Hud_render`), shown as a bitmap,
      the HUD's shapes over it; the program's rendering (shading,
      culling: TinyStarFox's sky is back faces) as the software backend
      makes its options. **A stress test of the rasterizer** (the
      author): every 3D game drawn live, its time a frame in the panel
      -- TinyStarFox 30 ms, TinyMarioKart64 31 ms, TinyMinecraft 236 ms;
      a scene over 20 ms rasterized one frame in n (its program still
      updated every frame), so the menu stays smooth. To do: every 3D
      game's time, a ranking (the rasterizer's to-do list).
    - **Full-screen demos** (the attract mode when idle, and `d` on
      demand) stay child processes, the one place a second of loading
      is fine: `tinybox <Name> -script <scene>` with a new `-demo` flag
      of the native loops, quitting on the first real key or click;
      the arcade's attract loop, and Doom's demos.
    - The scenes' scripts, which tinybox needs at run time: decided and
      done (2026-09-27), moved out of the tests into data both read,
      `tests/common/scenes/` (`Golden_scene`, `Scenes_2d`, `Scenes_3d`,
      library `golden_scenes`).
    - **The grid's thumbnail is the game being played** (the author):
      a program's first scripted scene's golden frame (TinyMario's
      "run", Donkey Kong's barrels), its first frame only for the 40
      without one. Done 2026-09-27. A scene that is a debug view rather
      than play (AiOthello's "values") could get an override, with the
      catalogue's new columns.
15. The source view (`s`) -- now a plan of its own,
    `plan_tinybox_codemap.md` (a code visualizer of the whole
    repository, TinyCodemap, started by `s` on the chosen program); what
    was written here first: after codemap (the author's code
    visualizer, the author's wish, 2026-09-27): the program's code shown
    "in a nice way" -- the files it is made of (the program, its kits,
    appkits, layers and libraries: its dune stanza's) as a treemap sized
    by lines, a click going into one; the code itself highlighted
    (keywords, comments, strings; the syncweb-free OCaml of this
    repository), the header comment first, as the page one reads before
    playing. Embedded at build time like the thumbnails (the sources are
    text, a few MB for all). A program of the menu itself, or a view of
    its own? The menu's detail panel opens it; it could also become an
    app of its own (TinyCodemap in `apps/devtools/`), which tinybox
    starts on the chosen program. The cheapest first version (the
    author): a screenshot of the real codemap per program, taken by a
    make target over the program's files (codemap is in ~/github/, if it
    can write its picture without a window) and embedded like the
    thumbnails; a picture, not a view one can go into.
16. Host and join for the `Multiplayer` games; kid mode; themes, menu
    music and sounds (with step 8); the gamepad.
17. `tinybox --install <dir>`: the links for BusyBox's way.
18. More pixels (the author, 2026-09-27): the native window is 1000 by
    1000, not resizable (Native_loop_2d ignores SDL's resize events).
    For every program, not only tinybox: a resizable window whose size
    reaches `computer.screen` (the menu's layout then follows it), flags
    `-size WxH` and `-fullscreen`, and a key, `f`, that toggles full
    screen while a program runs -- the platform's, like its debug keys,
    so every program has it. The menu's layout, written for 1000 by 1000,
    to be made relative to the screen. Done 2026-09-27, as the author
    chose (approach A, and Alt+Enter rather than `f`, which 14 programs
    use): the program's screen stays 1000 by 1000, scaled to the window,
    centred, black bars round it (`Native_loop_2d.scale`); the Cairo
    platform clips to the square and scales; the OpenGL one puts every
    viewport, the clear and the dump in the square (`frame`); both
    loops map the mouse back and toggle full screen on Alt+Enter; flags
    `-size WxH` and `-fullscreen`. The software platforms stay 1000 by
    1000 (the golden frames). A window of 1000 by 1000 is pixel-identical
    to before (checked: TinyTron, TinyTron3d). Later, as an opt-in: B,
    the program's screen following the window (`game`'s `Resized`, a
    `failwith "Todo"` today), for the apps that want the room.
19. tinybox on the web (the author: "even though the .bc.js might be too
    big", "a split approach for the js world"): not one bundle of 198
    programs, but the menu as a page of its own, small (the catalogue
    and the thumbnails), and a program chosen loads its own page,
    `<dir>/web/<Name>.html`, which `make js` already builds: on the web,
    the child process is the page. The previews (step 14) then only
    natively, or the chosen program's page in an iframe.
20. Sounds for the silent games (the author noticed TinyPacman's
    silence, 2026-09-27): 95 of the games never call `Audio` (TinyPacman,
    TinyInvaders, TinyDoom, TinyTetris...); a pass giving each its
    original's sounds (Pac-Man's waka, Space Invaders' four-note march),
    `Audio.play` inline where the events happen. Not tinybox's, but its
    menu makes the silence plain.

## Open questions for the author

- **Genre libraries** (the dependency union) before the launcher, or
  `copy_files` and a hand-kept list? `copy_files` for now (step 3);
  genre libraries if the list becomes a burden.
- **The spelling of the new columns** (decided 2026-09-27: Players,
  Platform and Year; no URL column): `1`, `1-2`, `1-4`, and a mark for
  network play; the platforms' names.
- **URLs**: a new `CATALOG.md` column (another thing its test checks),
  or a table private to the launcher?
- **Examples**: in the launcher too (their own section), or games and
  apps only, as `CATALOG.md`?
