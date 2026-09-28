# Plan: what's left for tinybox on the web

tinybox's menu runs in a browser: see
[`done/plan_tinybox_web.md`](../done/plan_tinybox_web.md) (the menu over
a host, `launcher/` as `menu/`, `native/`, `web/`; the web host,
0.42 MB; the programs as their own pages; the code map, its sources a
file in the assets; the web platform's `~screen` and its bitmaps
encoded by the browser; zooming at 30 frames a second). Published by
`make website` as `docs/tinybox.html`, at
https://aryx.github.io/ocaml-elm-playground/tinybox.html. What's left,
roughly from most to least worth doing.

## 1. Back returns to the program chosen -- done

The menu's flags `chosen=<Name>` (the menu on that program) and
`code=<Name>` (in its code map, once the sources are here), natively
too (`tinybox code=vi`); the web host replaces its URL with
`?chosen=<Name>` before leaving (`history.replaceState`), so Back comes
back to the menu on it. And a program's code is a link to give:
https://aryx.github.io/ocaml-elm-playground/tinybox.html?code=TinyTurboPascal
(the name in any case, "Tiny" optional).

## 2. A way back from a program's page

A program's page is the program alone: the only way back is the
browser's Back. A small link on the pages opened from the menu (a
`?from=tinybox` flag the pages' `.html` could read, or a line the
Makefile's website target adds to every page it copies: "back to
tinybox"), or Escape. Escape belongs to many programs (menus, pausing),
so the link, outside the program's picture, is the safer one.

## 3. The navigation as commands (Elm's Browser.Navigation)

The web host leaves the page by setting `window.location` itself
(`Ojs`), the menu staying a `game`. The Elm way: `Cmd.Load url` and
`Cmd.Replace_url url` (1 needs the second), performed by the web
platform, the menu an `app` with commands. Shared code (Cmd, the web
platform): to be reviewed first. Worth it when a second program wants
to navigate (TinyMosaic's links to the real web?), not before.

## 4. Faster still: the code map's remaining costs

Measured (the done plan's table): the treemap's layout, 0.5 s each time
a map is made (`Treemap`'s `cost`, `longest`, `walk`); and painting
(`paint_code`), most of a zoom's frame now (30 frames a second). Ideas,
each to be measured first:
- the layout of the whole repository computed once and kept (the map
  of a program is a part of it);
- while the camera moves, painting at a quarter of the resolution on the
  web (half today, natively and on the web);
- `paint_code`'s per-pixel lookups by row (the cell and its line found
  once per row, the glyph's bits per column).

## 5. The sources, less of them first

The panel's preview asks for the sources 20 frames after the menu
opens, so every visit downloads 3 MB (gzipped), even one that never
reads code. Alternatives: fetch them only when the preview has been
looked at for a while (a second), or split the file -- each program's
own code first (what the panel shows), the rest (`w`, the whole
repository) when asked.

## 6. Previews on the web

Natively, the chosen program plays in the panel after a second. On the
web the programs aren't linked in; the program's own page in an iframe
over the panel would do it, at the cost of its download (0.2 to 1.3 MB)
each time, and needs the web platform to place an HTML element over its
drawing (shared code). Or, cheaper: the golden scene's frames as a slide
show (only a few per program today).

## 7. Phones and tablets

The menu is keyboard and mouse. On a touch screen: a tap chooses (the
mouse's click, already), a second tap plays (the double click), a swipe
scrolls the grid (the wheel). Whether the web platform turns touches
into the mouse's events is to be checked first; then the menu's layout,
16:9, is small on a phone held upright.

## 8. From the website's index pages -- done

Each card of `docs/games/`, `docs/apps/` and `docs/by-size/` links to
its program's code map (`../tinybox.html?code=<Name>`), beside its
source on GitHub.

## 9. Small things

- The page's title and icon (`tinybox.html` has neither), and something
  shown while the 0.42 MB bundle loads.
- `localStorage` is per origin: every page of `aryx.github.io` shares
  it, tinybox's and every program's. Names stored by the programs must
  not collide (the store's names, `Playground_platform.store`).
- The examples on a shelf of their own in the web menu (not in
  `CATALOG.md`, so not in the menu today).
