# tinybox, and its programs, on a phone

The web tinybox (`done/plan_tinybox_web.md`) opens on a phone but can
hardly be used there (the author, 2026-09-28): no arrow keys, and a
finger's tap does not seem to do what a click does. The programs are
worse off: a game wants arrows and space, an application a keyboard.
What can be done, from the cheapest.

## What a phone gives the page today

- **No touch handling in the web platform**: it listens to the mouse
  (`mousemove`, `mousedown`, `mouseup`, `dblclick`, the wheel) and the
  keyboard, nothing else. A phone synthesizes mouse events after a
  tap (mousemove, mousedown, mouseup, click, at the finger), late
  (~300 ms without a `viewport` tag) and not for a drag, which scrolls
  or zooms the page instead.
- **No `viewport` tag** in the pages (`tinybox.html`, `<dir>/web/*.html`):
  a phone lays them out 980 pixels wide and shrinks them, and a double
  tap zooms -- so the menu's double click, which plays a program, is
  never a double click on a phone.
- **No keys**: no arrows, no space, no Enter, and no on-screen keyboard
  unless a text field has the focus (the playground draws SVG, it has
  none).
- **The menu is 16:9**, letterboxed: on a phone held upright, a band in
  the middle of the screen.

## Step 0: measure (Chrome's touch emulation)

Chrome's DevTools protocol emulates a phone (`Emulation.setDeviceMetricsOverride`,
`setTouchEmulationEnabled`) and sends real touch events
(`Input.dispatchTouchEvent`): which mouse events a tap gives the page,
where, when, and what the menu does with them. The same harness as the
web tinybox's measurements (headless Chrome, screenshots). Settles what
"a tap does not seem to work" is before anything is changed.

## Step 1: taps and drags as the mouse (the web platform)

- The page gets `<meta name="viewport" content="width=device-width">` and
  `touch-action: none` on the drawing: no zoom on a double tap, no page
  scroll on a drag, no 300 ms delay (the pages' `.html`, and the
  Makefile's `tinybox.html`).
- Pointer events (`pointerdown`, `pointermove`, `pointerup`: the mouse,
  a finger and a pen alike) instead of the mouse's: a finger is then a
  mouse to every program, with no program changed -- a tap a click, a
  drag a drag, two taps close in time and place a double click
  (`EMouseDouble`, as the platform makes the wheel's notches).
- A vertical drag with nothing under it: the wheel (the menu's grid,
  the code map's zoom)? Or two fingers for the wheel -- to try.

Shared code (the web platform): reviewed before it is made.

## Step 2: the menu on a phone (tinybox's own)

- A tap on the program already chosen plays it (natively too: a second
  click on the chosen one; the double click stays).
- A drag on the grid scrolls it; the tabs, the arrows at the top and the
  filter bar are buttons already.
- The code map: a drag pans, a pinch zooms (two pointers: the web
  platform's `EMouseWheel` from their distance, or a message of its own).
- Upright: the layout as is, letterboxed, with a word to turn the phone;
  a portrait layout (the grid above the details) later, if wanted.

## Step 3: a gamepad over the games (a small hack, all games at once)

A program that reads the keyboard gets, on a touch screen only, an
on-screen gamepad drawn by the web platform over its picture, outside the
program: a D-pad (the arrows) on the left, two or three buttons on the
right (space, Enter, and Escape or "z"), each sending the key's down and
up (`EKeyChanged`) as a keyboard would. No game changes; a game that
reads "w a s d" gets a second D-pad mapping, chosen by a flag
(`pad=wasd`) or by the catalogue.

Which keys each game needs could come from `CATALOG.md` (a column, or a
line in each game's header, which already lists its keys) -- or a
default (arrows, space, Enter) and the per-game list later.

HTML buttons over the SVG, not shapes drawn by the program: they stay the
page's, the same size on every screen, and a program's golden frames do
not change. Multi-touch: the D-pad and a button held together (run and
jump).

## Step 4: the applications (a keyboard)

- A hidden text field, focused on a tap when the program reads typed
  text (TinyTurboPascal, TinyVi, TinyExcel's cells): the phone's
  keyboard comes up, and what it types arrives as `ETyped`.
- The keys a phone keyboard lacks (Esc, Ctrl, the arrows, F keys): a
  row above it, as terminal apps on phones have (Termux's extra keys):
  Esc, Ctrl, Tab, the arrows -- and TinyTurboPascal's Esc-then-digit
  works there already.

## Step 5: tests

Chrome's emulated phone in the harness: a tap chooses, a second tap
plays, a drag scrolls; a game's D-pad moves Mario; TinyTurboPascal's
keyboard types. Screenshots of each, as the web tinybox's.

## Open questions for the author

- Step 3's gamepad: every game, or only those that say which keys they
  want?
- Portrait: letterboxed with "turn your phone", or a portrait layout
  for the menu?
- The native builds (tinybox on a Linux phone, a tablet): SDL has touch
  events too -- in scope, or the web only?
