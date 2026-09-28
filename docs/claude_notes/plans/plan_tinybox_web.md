# tinybox on the web

The menu of `tinybox` (`Tinybox_menu`, after Batocera's), in a browser:
the same menu, the same code map, over the website's programs. It is
step 19 of `plan_launcher.md`, made a plan of its own (2026-09-28).

## Status (2026-09-28): a first version

Done, the simplest that works: the reorganization (`menu/`, `native/`,
`web/`), the host (declared in `Tinybox_menu.mli`, the native one
`Tinybox_native`), the sizes in `Tinybox_data`, the web host
(`Tinybox_web`, 0.42 MB in release-js, 0.12 MB gzipped), `make website`
publishing it as `docs/tinybox.html`, and the web platform's `run_app`
taking `~screen` (the menu's 16:9, letterboxed by the browser). Simpler
than planned: the menu stays a `game`, the web host leaving the page by
setting `window.location` itself (`Ojs`) -- no navigation command in
`Cmd` yet. Checked in Chrome: the menu, the thumbnails from the assets,
the arrows and Enter loading the program's page.

Next, in order: Back returning to the program chosen (`?chosen=`, the
URL replaced before leaving); the code map on the web (step 7); a way
back from a program's page (a link, or Escape); the previews.

## The problem

Natively, tinybox is one binary: the 198 programs linked in, the menu
choosing one and running it (a child process of itself), its thumbnails
embedded (PNG, 250 by 250), every source of the repository embedded for
the code map (`Tinybox_sources`, 10 MB), and the chosen program
previewed live in the detail panel -- the program itself run by the menu,
since it is linked in.

On the web, one bundle of everything is out of the question: the 198
programs are 44 MB of release JavaScript on their own (each 0.2 to
1.3 MB), and the sources 10 MB more, all downloaded before the menu
shows anything. But the website already has what a split needs: each
program its own page and bundle (`aryx.github.io/assets/js/<dir>/`), and
each its thumbnail (`assets/pngs/<Name>.png`), made by `make website`.

So, as step 19 said: **on the web, the child process is the page**. The
menu is a page of its own, small; choosing a program loads that
program's page.

## What differs, native and web

| | native (today) | web |
|---|---|---|
| the programs | linked in; Enter forks tinybox itself (`start`, `Unix.create_process`) | not linked; Enter loads `<dir>/<Name>.html` (the page replaced; Back returns) |
| thumbnails | PNGs in the binary (`Tinybox_data.thumbnails`), `bitmap` | `image size size "<assets>/pngs/<Name>.png"`, fetched by the browser as shown |
| the code map's sources | `Tinybox_sources.sources`, in the binary | one file in the assets (`tinybox/sources.txt`, ~2 MB gzipped by Pages), fetched (`Http.get`) the first time the code is asked for, the map saying "loading" meanwhile |
| each program's size (the By size grouping) | `Tinybox_sources.sizes` | the same numbers, moved to `Tinybox_data` (small), so that the web need not link the sources for them |
| live previews | the program run in the panel (captured app, `Playground.capture`) | not in the first version: the thumbnail stays (see the last step) |
| the terminal (`-tty`), `tinybox list` | yes | no |
| flags | the command line | the page's URL query (`?group=era`), as every web program's |

Everything else -- the grid, the groups and filters, search, the
details, the code map and its tour -- is the same code.

## The split: one menu, two hosts

`Tinybox_menu` is a Playground program today, but it reaches directly
for what only the native build has: `Unix` (the child), the embedded
data, `Program.collected` and the rasterizer (previews). The plan
makes those a record the menu is given, its **host**, one per platform:

```ocaml
(* what the menu needs from where it runs *)
type host = {
  picture : Catalogue.program -> number -> shape;
      (* its thumbnail: an embedded bitmap, or an image URL *)
  play : Catalogue.program -> host_effect;
      (* native: fork a child; web: go to its page *)
  sources : unit -> sources_state;
      (* native: Ready (embedded); web: Absent, Loading, Ready *)
  preview : (Catalogue.program -> preview option) option;
      (* native only, for now *)
}
```

(The exact shape is for step 2: the native `play` is a side effect
done in `update` today, and the web's two -- going to a page, fetching
the sources -- are commands, so the menu becomes an `app` with
`Cmd.t`s rather than a `game`; see the open questions.)

The menu itself -- `Tinybox_menu` less its "Previews" section and its
`start` -- goes in a library with no `Unix`, no embedded data and no
programs, linked by both.

## A reorganization of `launcher/`

Today:

```
launcher/
  Tinybox.ml Tinybox_collect.ml Tinybox_menu.ml   tinybox.exe, and the
                                                   copy_files of every program
  catalogue/   CATALOG.md read          (library catalogue)
  codegen/     make_tinybox_data        (build-time generator)
  codemap/     the code map             (library tinybox_codemap)
    deps/      Code_deps                (library tinybox_codedeps)
  website/     make_website             (build-time generator)
```

Proposed (**moves: the author's call**, nothing moved before a yes):

```
launcher/
  catalogue/  codegen/  codemap/  website/     unchanged
  menu/       Tinybox_menu (the shared menu, over a host),
              Tinybox_host (the host's type)   library tinybox_menu
  native/     Tinybox, Tinybox_collect, the previews, the native host
              (fork, embedded thumbnails and sources): tinybox.exe,
              and the copy_files of every program
  web/        Tinybox_web, the web host: tinybox.bc.js (modes js),
              linking the menu, the code map, the catalogue and
              Tinybox_data -- no program, no sources
```

`bin/tinybox` and `make install` unchanged (the public name stays
`tinybox`). The copy_files and the long library list move with the
native executable, where they belong: the web build has neither.

A lighter alternative, if moving is not wanted now: keep the files where
they are and add `launcher/web/` only, the menu library cut out of
`launcher/dune`'s stanzas by `(modules ...)`.

## The pages

- `docs/tinybox.html` (or the website's front page itself, later): the
  menu's page, its bundle `assets/js/launcher/tinybox.bc.js`. `make
  website` builds it with the programs', in release-js.
- Going to a program: the menu first replaces its own URL with
  `?chosen=<Name>` (history's `replaceState`), then loads the program's
  page. Back returns to the menu *on that program*, its flags read from
  the URL as every web program's.
- The sources for the code map: `make website` writes them once,
  `assets/tinybox/sources.txt` -- a path, its length, its text, and the
  next -- the format read by a few lines in the web host (no Marshal:
  native `output_value` and js_of_ocaml's reader are best not trusted
  with 10 MB).

## Changes to shared code (the Playground)

To be reviewed before they are made (`feedback: plan before shared
changes`):

1. **Going to a page**: a command, Elm's `Browser.Navigation.load` --
   `Cmd.Load of string` (the web platform sets `window.location`;
   native: ignored, or refused) and `Cmd.Replace_url of string`
   (`history.replaceState`). A small addition to `Cmd` and to the web
   platform's commands.
2. Nothing else: images by URL (`image`), `Http.get` and the URL's
   flags exist already.

## Steps

1. **Measure**: the menu alone, built in release-js with stubs for the
   programs, to know the bundle's size before designing around it
   (guess: 0.5 to 1 MB, the code map and the OCaml highlighter being
   most of it).
2. **The host**: cut `Tinybox_menu` into the shared menu and the native
   host, native behaviour unchanged (tinybox's golden frame, the menu,
   is the test).
3. **`sizes` into `Tinybox_data`**, out of `Tinybox_sources`.
4. **The reorganization of `launcher/`** (if the author wants it).
5. **The navigation commands** in `Cmd` and the web platform.
6. **The web host and page**: thumbnails by URL, Enter loads the
   program's page, Back returns to it; no code map yet (`s` says "not
   on the web yet").
7. **The code map on the web**: `sources.txt` written by `make website`
   into the assets, fetched on the first `s`.
8. **`make website`** builds `tinybox.bc.js` and its page; the front
   page links it.
9. Later, maybe: **previews on the web**, the chosen program's own page
   in an iframe over the detail panel after a second's pause (it costs
   the program's download, 0.2 to 1.3 MB, each time), which needs the
   web platform to place an HTML element over its drawing.

## Open questions for the author

- **The reorganization** (`menu/`, `native/`, `web/`), or the lighter
  alternative?
- **The menu as an `app`** (commands) instead of a `game`: the web needs
  commands (navigate, fetch). Natively, `start` would become a command
  too, or stay a side effect of the native host.
- **Where the web menu lives**: `docs/tinybox.html` beside the index
  pages, or the front page itself becoming the menu?
- **Examples in the web menu**: games and apps only (as `CATALOG.md`),
  or the examples too, their own shelf?
- **Previews on the web** (the iframe) worth their cost?
