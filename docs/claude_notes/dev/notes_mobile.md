# The web programs on a phone, and debugging them in a browser

How to try the web builds (tinybox's menu, the games, the apps) on a
phone, how to see what happens there, and what a phone does differently
from a desktop browser. And the tool behind most of it: headless Chrome
driven through its DevTools protocol -- keys, taps, an emulated phone,
frame times, profiles -- which `notes_headless.md` (screenshots and DOM
dumps from the command line) cannot do. The plan these serve is
`plans/plan_mobile.md`.

## 1. Trying a build on a real phone

- **The published site**: https://aryx.github.io/ocaml-elm-playground/
  (a program's page loads its bundle from https://aryx.github.io/assets/).
  Nothing to set up; but a change must go through `make website` and two
  pushes first.
- **A local build, over the network**: serve a directory holding the
  page and its `.bc.js` on every interface, and open it from a phone on
  the same Wi-Fi:

  ```
  python3 -m http.server --bind 0.0.0.0 8000 --directory <dir>
  # on the phone: http://<this machine's address>:8000/<Name>.html
  ```

  tinybox's web menu reads its thumbnails and sources from the assets;
  `?assets=http://<address>:8000/assets` points it at a local copy
  (`Tinybox_web`'s flag). Plain `http://` works for everything here but
  what browsers keep for secure pages (the clipboard, some sensors); a
  phone refusing something on `http://` is the first thing to suspect.

## 2. Seeing what happens on the phone

- **Android and Chrome**: the phone on USB with developer options and
  USB debugging on, then `chrome://inspect` in the desktop's Chrome: the
  phone's tab with the full DevTools -- console, the elements, the
  profiler, its screen mirrored.
- **iPhone and Safari**: Settings, Safari, Advanced, Web Inspector on;
  the phone on a Mac's cable; Safari's Develop menu on the Mac lists its
  pages. Without a Mac there is no inspector: the page's own console on
  screen (a log drawn by the program), or a desktop Safari's responsive
  mode, which is not the phone.
- **A desktop's device mode** (DevTools, the phone icon): a phone's
  screen size, pixel ratio, user agent and touch events from the mouse.
  Good for layout and for what events arrive; not for Safari's quirks
  nor a phone's speed.

## 3. What a phone does differently

Found on tinybox (`plan_mobile.md`, step 0) or known:

- **No `viewport` tag, a desktop page shrunk**: the page is laid out
  about 980 pixels wide and shown at a fraction (42% on an upright
  phone), and a double tap zooms. `<meta name="viewport"
  content="width=device-width, initial-scale=1">` gives the phone's own
  width.
- **A tap is a mouse, late, in Chrome**: pointerdown/up, touchstart/end,
  then mousemove, mousedown, mouseup and click at the finger (300 ms
  later without the viewport tag). A double tap may give a dblclick, or
  zoom.
- **Not in Safari on iOS**: it synthesizes the mouse events only for an
  element that looks clickable (a click handler of its own, or a
  `cursor: pointer`). The playground listens on the window, so a tap on
  an iPhone gives it nothing. Pointer events (`pointerdown`...) arrive
  everywhere, iOS included.
- **A drag is not a mouse at all**: pointer and touch events only; the
  page scrolls or zooms unless the element says `touch-action: none`.
- **No keys**: no arrows, space, Enter, Escape, F keys; the on-screen
  keyboard comes up only for a focused text field.
- **Slower**: a phone's CPU is a few times slower than a desktop's, and
  the web's own costs (a new PNG per bitmap, see `notes_opti_ocaml.md`
  section 15) weigh more there.

## 4. Driving a page: headless Chrome's DevTools protocol

Chrome started with `--remote-debugging-port=9340` answers on a
WebSocket (`http://localhost:9340/json` lists the pages). Each command
is a JSON message `{"id", "method", "params"}`; the answer carries the
same id, and the page's events (console messages, exceptions) come in
between. Python's `websockets` is enough, a dozen lines of glue (the
scripts of the session that wrote this note were `cdp_*.py`; see
section 6). What they did with it:

| to | commands |
|---|---|
| a phone | `Emulation.setDeviceMetricsOverride` (width, height, `deviceScaleFactor`, `mobile: true`), `Emulation.setTouchEmulationEnabled`, `Emulation.setUserAgentOverride` |
| a tap, a drag | `Input.dispatchTouchEvent` (`touchStart`, `touchMove`, `touchEnd`, with `touchPoints`) |
| a key | `Input.dispatchKeyEvent` (`rawKeyDown`, `keyDown` with `text`, `keyUp`; `modifiers` 1 Alt, 2 Control, 4 Meta, 8 Shift) |
| the mouse wheel | `Input.dispatchMouseEvent` (`mouseWheel`, `deltaY`) |
| look | `Page.captureScreenshot`, then Read the PNG |
| read the page | `Runtime.evaluate` (`location.href`, `innerWidth`, a value the page keeps) |
| record what the page receives | `Page.addScriptToEvaluateOnNewDocument` with listeners pushing each event into an array, read back with `Runtime.evaluate` |
| console, exceptions | `Runtime.enable`, then the events `Runtime.consoleAPICalled` and `Runtime.exceptionThrown` |
| frame times, freezes | a `requestAnimationFrame` counter and a `longtask` `PerformanceObserver`, injected like the recorder |
| where the time goes | `Profiler.enable`, `setSamplingInterval`, `start`, `stop`: nodes and samples, summed by function (self) or up the parents (inclusive) |

Three measurements this gave, each in a few minutes: the web code map's
frame times and profile (`notes_opti_ocaml.md` section 15), the
TinyTurboPascal stack overflow's function (section 5), and a phone's
taps (section 3).

## 5. Pitfalls, found the hard way

- **Minified names**: a release-js bundle's profile says `h2`, `fc`;
  build the development one (no `--profile=release-js`) to profile or
  read a stack trace -- its JavaScript even carries the OCaml source's
  line numbers in comments.
- **The stack**: a browser's is far smaller than native OCaml's. A
  function recursing once an element -- OCaml 4.14's `List.map` over
  9,600 tokens (`Highlight_ml`), a loop written as a function calling
  itself through an optional argument, which js_of_ocaml does not turn
  into a loop (`Pmachine.resume`) -- works natively and throws "Maximum
  call stack size exceeded" in the browser. The page keeps drawing, the
  program stands still. Look for it first when something works natively
  and freezes on the web; `node` runs a `.bc.js` with the same limit, a
  quick check.
- **Injected keys are not a keyboard**: `keyDown` with a `text` types
  it; `rawKeyDown` does not; a synthesized Control and letter can reach
  the page differently from a real one (a Ctrl-Y typed "y" in one test,
  deleted the line on a real keyboard). Confirm on a real keyboard
  before fixing what only the injection shows.
- **A key-up that never comes**: a key-up can go elsewhere (the browser
  or the desktop taking the focus on a shortcut), and the key stays held
  for the program -- a Control held forever. The web platform now
  releases a modifier an event says is up, and every key when the page
  loses the focus.
- **`pgrep -f` matches itself**: a check for "another `make website`
  running" found the command line doing the checking.
- **Cross-origin in the test itself**: a page on one local port reading
  from another is a cross-site request, which `python3 -m http.server`
  does not allow; serve the page and what it reads from one server.

## 6. To keep, maybe

The harness lived in the session's scratchpad: a script per question,
each the same glue. A small `scripts/web/cdp.py` (start Chrome, the
glue, a phone, a tap, a key, a screenshot, the recorder) would make the
next measurement a few lines, and a smoke test of every web program's
first taps possible in `make test`.
