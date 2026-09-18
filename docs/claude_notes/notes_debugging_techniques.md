# Debugging a native SDL+Cairo Playground app (crashes, and "feels wrong")

This file is about the *technique*, not the specific bugs (see git log /
the "claude:" comments in the source for those: the imagelib GIF decoder
bug, and the game-loop timing issue). Kept here so the approach can be
reused next time something in the native (`playground/native/`) backend
crashes or just "feels off" in a way that's hard to pin down from reading
the code alone.

## 1. Running a GUI app headlessly, without it hanging your shell

`Mario.exe` (like any Playground `game`) is `while true do ... done` with
no natural exit. Don't run it bare in a tool call -- wrap it:

```bash
timeout 5 ./_build/default/examples/Mario.exe > /tmp/mario.log 2>&1
echo "exit: $?"; cat /tmp/mario.log
```

`exit: 124` means `timeout` killed it after 5s -- i.e. it ran fine for the
full duration. Any other exit code (2 here, for an uncaught OCaml
exception) means it crashed, and the log has the real stack trace. This
is the difference between "confirmed it didn't crash" and "I assume it's
fine because nothing printed" -- always check the exit code, not just
whether output looks alarming.

## 2. Isolating a suspected third-party-library bug with a standalone repro

When a crash's stack trace points into an opam-installed library (here:
`imagelib`'s `imageGIF.ml`, several frames deep inside
`ImageGIF.ReadGIF.process_image_descriptor_subblock`), don't try to
debug it by rebuilding the whole dune project on every iteration. Copy
just the relevant few lines into a throwaway `.ml` in `/tmp` and compile
it directly against the installed findlib packages:

```bash
ocamlfind ocamlopt -package curl,imagelib.unix -linkpkg /tmp/test.ml \
  -o /tmp/test && /tmp/test
```

This is a fast, dune-independent loop: swap inputs (different URLs,
different downloaded files), swap library versions (`opam install
pkg.VERSION`), or swap the implementation being tested (imagelib vs
stb_image, see below) in seconds, without touching the real project
until you know exactly what the fix should be. It's also how the actual
mechanism of the bug was pinned down here: reproducing the exact
download-then-decode pipeline (`curl_url` + `ImageLib.openfile`) outside
the app made it possible to bisect "is it the download (curl bindings)
or the decode (imagelib)" -- confirmed identical bytes via `md5sum` on
repeated downloads, which ruled out "flaky network content" and pointed
squarely at the decoder.

Reading the *library's own source* (found via `opam show <pkg>` ->
`all-installed-versions` gives the exact path under
`~/.opam/<switch>/lib/<pkg>/`, and the opam cache also keeps the unpacked
source tree under `~/.opam/<switch>/.opam-switch/sources/<pkg>.<version>/`)
is what turned "some exception four frames deep" into an actual line-level
bug: `calc_clear_code lzw_min_size = 1 lsl (lzw_min_size - 1)` in
`imageGIF.ml`, which should be `1 lsl lzw_min_size` per the GIF89a spec's
own definition of the LZW clear code. Don't stop at "the library raised
an exception, so catch it" -- when it's cheap to read the library's
source, do, since it turns a defensive workaround into an actual
diagnosis of *what's wrong and why*, which is what lets you decide
whether to patch around it (convert fallback), replace the library
entirely (stb_image), or both in sequence, instead of guessing.

## 3. Don't assume "latest opam version" means "latest known state"

`opam show <pkg>` lists `all-versions` -- confirms whether a newer
release exists at all. But an opam release can lag the project's git
history by years with no new tag cut. Before concluding "this is
unfixably outdated, need to switch libraries", check upstream git
directly:

```bash
curl -s https://api.github.com/repos/<owner>/<repo>/commits?path=<file> \
  | python3 -c "import json,sys; [print(c['sha'][:10], c['commit']['author']['date'], c['commit']['message'].splitlines()[0]) for c in json.load(sys.stdin)]"

curl -s https://raw.githubusercontent.com/<owner>/<repo>/master/<file> \
  -o /tmp/master_version_of_file.ml
```

For `ocaml-imagelib`, this showed the opam release (20221222) *is* the
latest tag, and separately that the current git `master` -- unreleased,
years of later commits included -- still has the exact same buggy
`calc_clear_code`. That's a materially different, stronger claim than
"no new opam release exists": it means there is no fix to pin to, from
opam or from git, so switching libraries (rather than waiting/pinning) is
the only real option. Don't skip this check and assume "no new opam
version" settles the question.

## 4. Temporary instrumentation beats guessing about control flow

"Which image URL is being requested when it crashes?" and "why does
`mario.y` become nonzero on frame 2 with no visible input?" were both
answered in one shot by adding a throwaway `Printf.eprintf` at the exact
decision point (inside `render_image`, and inside the SDL key-event
branch of `run_app`'s loop) instead of reasoning it out from the source:

```ocaml
Printf.eprintf "DEBUG loading image: %s\n%!" src;
```

rebuild, rerun, read the log, then **remove it** before committing --
`git diff` the file afterwards to confirm no debug prints survived. This
is faster and more reliable than tracing the Elm-style `update`/`view`
dataflow by eye, especially across an event queue (SDL) where "what fired
this frame" isn't visible from a static read of the code. It also
directly disproved a wrong hypothesis here: the "Up" keypress causing
Mario to jump on frame 2 looked at first like a spurious/synthetic SDL
event in the sandboxed X server, until the user confirmed in chat they
were actually pressing keys in the window -- a reminder to state a
hypothesis as a hypothesis and check it against the human in the loop
before treating it as confirmed, even when the instrumented evidence by
itself looks like it points to an environment bug.

## 5. When something "feels wrong" (not crashing, just off), measure it

"Mario moves/jumps too fast, and sometimes freezes" is not a stack trace
-- there's nothing to catch. The fix is the same instinct as above,
applied to *timing* instead of control flow: don't guess which of several
plausible causes (frame-rate, physics constants, network stalls, GC
pauses) it is -- instrument and get numbers.

- **Frame rate**: the app already had an FPS counter (`Fps.update_fps`,
  drawn on-screen) but only visible if you're watching the window.
  Temporarily also `Printf.eprintf` it to stderr to get numbers in a
  headless run:

  ```ocaml
  if dt > 0.5 then (
    fps := float !frames /. dt;
    Printf.eprintf "DEBUG fps: %.0f\n%!" !fps;
    ...
  ```

  This immediately showed ~400-450fps at idle, against the ~60fps the
  Elm original gets from the browser's `requestAnimationFrame`
  throttling -- and the game's `update` uses a *fixed* per-call `dt`
  (ported as-is from the Elm source, which relies on the browser holding
  frame rate roughly constant near 60Hz), so an uncapped native loop
  directly means physics integrates ~7x too fast. One number turned a
  vague "feels too fast" into a precise, fixable claim: the native
  `run_app` loop needs frame-rate pacing to match the cadence the shared
  example code assumes.

- **Blocking/stalling**: timed a cold (not-yet-cached) image load
  directly against the same standalone-repro technique from section 2:

  ```ocaml
  let t0 = Unix.gettimeofday () in
  let _ = surface_of_url "https://elm-lang.org/images/mario/walk/left.gif" in
  Printf.printf "cold load took %.3f s\n%!" (Unix.gettimeofday () -. t0)
  ```

  -> ~0.7s. Since that download+decode happens synchronously inside the
  render loop on first use of each sprite variant, this is a precise,
  reproducible number for the "sometimes lag or blocks" complaint (it
  happens exactly when the game first needs a not-yet-cached image, e.g.
  the first jump or the first time moving left), not a vague GC/rendering
  guess.

**General lesson**: for "it feels wrong" bug reports with no exception and
no log, the first move is to find *some* number to print (a rate, a
duration, a count) at the suspected site, rather than reading the code
and reasoning about what "should" be slow. A wrong intuition about
performance is extremely common and cheap to disprove with one timestamp
pair; don't spend time on it, just measure.

## 6. Debugging the web (js_of_ocaml) backend without a browser

Symptoms reported for the web backend (`playground/web/`) were: Tetris
"not working, and even forces me to close the tab", Asteroid "not
starting". A tab stuck in an infinite loop can't show its console, and
headless Chrome was useless here (`chrome --headless --dump-dom
--enable-logging=stderr` on macOS only printed display-driver noise and
never exited, since the page never stops requesting animation frames).
(claude: on Linux, with `--virtual-time-budget`, headless Chrome does
work well for pages that don't hang. See `note_headless.md`.)

What worked: run the compiled `.bc.js` directly in node with a tiny fake
DOM, `docs/claude_notes/web_headless.js`. It fakes only the DOM calls
made by `playground/web/Playground_platform.ml` (`createElementNS`,
`setAttribute`, `appendChild`, `requestAnimationFrame`,
`addEventListener`, ...), calls `window.onload`, then drives N animation
frames at a simulated 60Hz (`Date.now` is faked to follow the simulated
time too), optionally pressing keys, and prints progress:

```bash
make
timeout 10 node docs/claude_notes/web_headless.js \
  _build/default/games/js/Tetris.bc.js 120 "ArrowLeft,ArrowUp, "
echo "exit $?"      # 124 = hang, like in section 1

# DUMP=1 prints the DOM tree (= the rendered shapes) every 30 frames
DUMP=1 timeout 10 node docs/claude_notes/web_headless.js \
  _build/default/examples/js/Animation.bc.js 91

# smoke test of all the web apps
for f in examples/js/*.html games/js/*.html; do
  b=$(basename $f .html); d=$(dirname $f)
  [ -f _build/default/$d/$b.bc.js ] || continue
  timeout 10 node docs/claude_notes/web_headless.js \
    _build/default/$d/$b.bc.js 120 "ArrowLeft, " >/dev/null 2>&1
  echo "$b: exit $?"
done
```

For Tetris, this narrowed "the tab freezes" down to "the frame after
pressing space never finishes" (`FullDrop` loops until the piece lands);
the DOM dump showed the falling piece was never rendered; and reading
the Tick handler then gave the root cause: games compute
`[now -. last_tick]` where `last_tick = Unix.gettimeofday()` (seconds
since 1970) but the web backend passed the `requestAnimationFrame`
timestamp (seconds since page load), so the delta was about -1.8e9: the
piece started 1.8 billion rows above the well, and `FullDrop` looped 1.8
billion times. The same bug made Asteroid ignore all its Ticks (delta <
tick). Fix: pass the wall-clock time, like the native backend and Elm do.

**Lessons:**
- When two backends must behave the same, compare what each one passes
  to the shared code (here the float inside `ETick`), not only how each
  one draws.
- js_of_ocaml ints are 32 bits: any int derived from wall-clock
  milliseconds (~1.8e12) silently wraps around (`Playground.to_frac` used
  `int_of_float` on it; now uses floats). Code that works natively can
  break only on the web because of this.
- Chrome's own console is still the quickest check when the tab is
  responsive; the node harness is for hangs, for getting numbers or the
  rendered tree in a loop, and for checking all the apps after a change.

## 7. Screenshotting and smoke-testing playground3d (native/OpenGL) windows

This sandbox has a real X display (`DISPLAY=:1`) but no xdotool to press
keys or move the mouse (see section 8 for how to do it anyway), so
verifying a `playground3d/` rendering change
means: run the app in the background, screenshot it, `Read` the PNG.
Two scripts capture this so it doesn't get reinvented (and gotten
wrong) every time: `scripts/screenshot_playground3d.sh` and
`scripts/smoke_test_playground3d.sh`.

```bash
scripts/screenshot_playground3d.sh _build/default/examples3d/Cubes3d.exe /tmp/cubes.png
scripts/smoke_test_playground3d.sh          # every examples3d/games3d (+opengl) demo, 3s each
```

**The gotcha the screenshot script exists to avoid**: `import -window
<title>` looks like the obvious one-liner, but a window manager creates
an *outer*, decorated frame window with the exact same title as the
app's *inner* content window (confirmed via `xwininfo -root -tree`:
both literally named e.g. `"Playground3D (OpenGL)"`). Matching by
title can silently grab the *outer* frame instead of the inner
content, producing a wrong-sized screenshot (e.g. `1056x1132` instead
of the real `1000x1000`) with no error at all -- easy to not notice
until the `Read`ed image looks like a screenshot of your own terminal.
The fix: `xwininfo`'s tree also prints each window's `WM_CLASS`, and
only the *inner* content window's class is the executable's own
basename (e.g. `"Mario.exe"`) -- grep for that instead of the title.

**Measuring FPS/frame time** (used for the OpenGL-vs-native LOC/FPS
comparison in `notes_playground3d_related_work.md` and the
tiny-minecraft performance checkpoint in `plan_tiny_minecraft.md`):
same "temporary instrumentation" idea as section 4, applied twice,
together:
1. A temporary `Printf.eprintf "DEBUG ...: %.3fs\n%!" (Unix.gettimeofday () -. t0)`
   around whichever step you suspect is slow (e.g. `view3d`'s shape-list
   construction) -- run headlessly with `timeout Ns`, `grep -c DEBUG`
   the log to count completed frames/calls in that window, revert
   before committing.
2. `playground3d/native_common/Native_loop.ml`'s `target_fps = 60.`
   caps every backend (native *and* OpenGL, both go through this same
   loop) at 60fps by sleeping out the remainder of each frame -- fine
   for playing a game, useless for measuring how fast a backend
   *could* go. Temporarily bump it to something absurd (`100000.`) to
   remove the cap while benchmarking, and revert after.

**A real bug this technique found, not just numbers**: benchmarking
`games3d/StarCollector3d.exe` on the OpenGL backend surfaced an
intermittent `Fatal error: OpenGL shader compile error` pointing at
GLSL syntax errors that were never in the actual shader source string
(confirmed by dumping the exact string passed to `Gl.shader_source`
right before the failing call -- always correct). It reproduced
reliably on that specific executable, never on simpler ones, and --
the key diagnostic clue -- adding *any* extra unrelated allocation
(even a debug `Printf.eprintf`) right before the shader-compile calls
made it disappear just as reliably. That pattern (timing-sensitive,
"more allocation somehow fixes it") points at a GC-safety bug in a
third-party FFI binding (here: `tgls`'s `glShaderSource`, a
pointer-to-a-pointer string-marshaling pattern that's a known-tricky
case for `ctypes`), not a logic bug in application code. The fix
wasn't a guess: a deliberate `Gc.full_major ()` right before the
shader-compile calls (flushing pending finalizers/compactions first)
reliably avoided the race (10/10 clean runs, versus 100% reproducible
failure before) -- a real, principled mitigation for the specific
failure mode observed, not a superstitious "add a sleep and hope".
**Lesson**: when a crash's exact symptom keeps changing between runs
(different garbage each time, same call site) despite looking 100%
reproducible in *whether* it happens, suspect a memory-safety/GC-timing
bug in an FFI layer before suspecting your own logic -- and use
"does adding an unrelated allocation change the failure rate" as a
cheap, decisive test for that hypothesis.

## 8. Driving a running app: mouse and keyboard from a script (ctypes + XTEST)

Screenshots (section 7) only test what an app draws on its own. Most
bugs need *input*: does the circle follow the mouse, does holding an
arrow key move the square at the right speed? The usual tool is
`xdotool`, which this sandbox doesn't have (nor `python-xlib`), and
installing packages isn't an option. But the two C libraries xdotool is
built on *are* installed (they come with any X desktop), and Python's
standard `ctypes` module can call any C function in any shared library
directly -- no compiler, no binding package. `scripts/xdrive.py` wraps
this up:

```bash
_build/default/examples/Mouse.exe &
sleep 2                                    # let it create its window
WID=$(scripts/xdrive.py find Mouse.exe)    # X window id, e.g. 0x3800007
scripts/xdrive.py move  $WID 700 200       # window pixels, top-left origin
scripts/xdrive.py click $WID 700 200       # left click there
scripts/xdrive.py down  $WID               # hold the button (screenshot now)
scripts/xdrive.py up    $WID               #  ... and release it
scripts/xdrive.py key   $WID Right 1       # hold the Right arrow for 1s
scripts/xdrive.py query $WID               # where does X think the pointer is?
import -window $WID /tmp/after.png         # then Read the PNG, as in section 7
```

### How it works

**ctypes in 30 seconds.** `ctypes.CDLL("libX11.so.6")` loads a shared
library and gives you every exported C function as a Python attribute:
`x11.XOpenDisplay(None)` really calls the C `XOpenDisplay(NULL)`. The
one trap: ctypes doesn't read C headers, so it assumes every function
takes and returns a C `int`. Anything pointer-sized (a `Display*`, an X
`Window` id, a `KeySym`) silently gets truncated to 32 bits on a 64-bit
machine -- a crash, or worse, a call on the wrong window, with no error.
So declare the real types first, by hand, from the man page:

```python
x11.XOpenDisplay.restype = ctypes.c_void_p        # returns Display*
x11.XWarpPointer.argtypes = [ctypes.c_void_p, ctypes.c_ulong, ...]
```

Output parameters (C's `int *x`) become `ctypes.byref(ctypes.c_int())`,
read back with `.value` -- see `query` in the script.

**The X11 calls, one per command:**

- *finding the window*: not an X call at all -- `xwininfo -root -tree`,
  grepping for the executable's name, the same `WM_CLASS` trick as
  section 7 (the title also matches the window manager's outer frame).
- *moving*: `XWarpPointer(display, src=None, dest=window, 0,0,0,0, x, y)`
  moves the real pointer to `(x, y)` relative to `window`, so no need to
  know where the window is on screen (this sandbox's screen is
  multi-monitor, the window can be at e.g. `(2060, 654)`).
- *clicking and keys*: the XTEST extension (`libXtst`), which exists
  precisely so test tools can inject input events indistinguishable
  from a real mouse/keyboard: `XTestFakeButtonEvent(display, button,
  is_press, delay)` and `XTestFakeKeyEvent(display, keycode, is_press,
  delay)`. A key name like `"Right"` becomes a keysym
  (`XStringToKeysym`, names from `/usr/include/X11/keysymdef.h` without
  `XK_`), then the keyboard's keycode for it (`XKeysymToKeycode`).
  Holding a key = press, `sleep`, release -- the app sees exactly what
  a human holding the key would, including SDL's key-repeat events.
- *every call is followed by `XSync`*: Xlib buffers requests; without a
  sync, the script can exit before anything was sent.

### The flakiness it fixes (and how it was found)

The first version was just one `XWarpPointer` per move, and it was
*unreliable*: measuring the drawn circle's position after each move
(ImageMagick: turn the circle's color black, everything else white,
`-trim`, print the bounding box's center), some moves were simply
ignored -- the circle stayed where it was -- in both the old and the new
build of `Mouse.exe`, at random, while `XQueryPointer` said the pointer
*was* at the right place. So X had moved the pointer, but SDL hadn't
reported it. Two fixes, both in `move`:

1. **Focus the window first** (`XRaiseWindow` + `XSetInputFocus`). A
   window that isn't focused doesn't reliably get pointer events in
   this setup, and key events only ever go to the focused window.
2. **Warp to `(x+1, y)` first, then to `(x, y)`.** Warping onto the
   pointer's current position generates no motion event at all, and a
   first warp into a newly mapped window was sometimes dropped; the
   extra warp makes sure at least one real motion lands.

After that: 9 out of 9 moves landed exactly (circle center = target
pixel), on both builds, across repeated runs; holding `Right` for 1s
in `Keyboard.exe` moved the square 59px (1px per frame at 60fps), as
expected. **Lesson**: when checking input handling with synthetic
events, first check the harness on the *unchanged* build -- here, the
"bug" showed up in both builds, which proved it was the harness and
not the code change under test.

### What it's good for

- **Verifying a refactoring of the input path**, which is what it was
  written for: moving the SDL event loop into
  `playground/native_common/Native_loop_2d.ml` replaced
  `Cairo.device_to_user` with plain arithmetic to turn window pixels
  into Elm coordinates; driving `Mouse.exe` to known pixels and finding
  the circle *exactly* there proves the mapping (including the y-flip
  and the centered origin) without eyeballing.
- **Reproducing a game bug** that needs a precise input sequence
  (`key $WID space 0.05` to jump, then screenshot mid-air).
- **Smoke tests that exercise input**, not just startup, e.g. extending
  `scripts/smoke_test_playground3d.sh` to hold an arrow key and check
  the camera moved.
- Anything else X11 can do -- the same `ctypes` recipe works for any
  C library function you can read the man page of.

## 9. Rendering frames offscreen: `SDL_VIDEODRIVER=dummy`

The golden frame tests, and most checks of a new game, don't look at a
window at all: they render a given frame of a native software backend
into memory and write it to a file.

```bash
cd _build/default     # the examples find their images relative to it
SDL_VIDEODRIVER=dummy ./games/software/TinyMario.exe \
  -fixed-time 1000 -dump-frame 150 /tmp/frame.ppm -script "right:1-150,up:30-34"
python3 -c "from PIL import Image; Image.open('/tmp/frame.ppm').save('/tmp/frame.png')"
```

then `Read` the PNG. Each piece has a reason:

- **`SDL_VIDEODRIVER=dummy`** makes SDL use its "dummy" video driver:
  `SDL_CreateWindow` succeeds, but the window is only a surface in
  memory, never shown. The software backends (2D `playground/software/`,
  the Cairo one, 3D `playground3d/software/`) write their pixels into
  the window surface themselves, so they don't notice: the pixels are
  the same as in a real window, and `-dump-frame` writes them out.
  Without it, SDL opens a real window on `$DISPLAY`: it pops up on the
  user's screen (and may steal the keyboard focus while they type),
  runs are slower, and where there's no display at all (CI, ssh, a
  sandbox) `SDL_CreateWindow` fails. With it, runs are fast, silent,
  and can run in parallel (the golden runner starts one per scene).
  It can't work for the OpenGL backends: the dummy driver has no GL
  context to give; those are checked with real windows (section 7) or
  headless Chrome for WebGL (`note_headless.md`).
- **`-fixed-time t`**: the app's clock frozen, so animations (`spin`,
  `wave`, `Sprite.frame`, blinking texts) are the same on every run.
- **`-dump-frame n file`**: after drawing frame n (from 1), write it as
  a binary PPM and exit. The mouse and keyboard are ignored then: where
  the pointer happens to be would change the frame.
- **`-script "key:frames,..."`** (`playground/native_common/
  Input_script.mli`): game keys held over given frames, since the real
  keyboard is ignored: what a player would do, replayed exactly. With
  a deterministic game (no wall clock, `seed=1` for the ones drawing
  random numbers), the inputs are the whole run.
- **`-keys k`**: the backend's *debug* keys pressed before the first
  frame (wireframe, antialiasing off, ...), for their golden frames.

`tests/common/Testutil_golden.ml` runs exactly this for each scene,
then compares the PPM with `tests/*/golden/*.png` pixel by pixel.

### Simulating a game without drawing it

To check a game's rules over thousands of frames (does the ghost leave
its house? does the car stay on the road with this steering?), faster
than rendering, call its `update` directly from a throwaway program
and print the model:

```bash
mkdir tmpcheck   # not _tmpcheck: dune ignores directories starting with _
cat > tmpcheck/dune <<'DUNE'
(copy_files ../games/TinyPacman.ml)
(executable (name Sim) (modules TinyPacman Sim) (libraries elm_playground elm_playground_software))
DUNE
# tmpcheck/Sim.ml: build a computer (initial_computer with keys held),
# call TinyPacman.update in a loop, Printf the fields of interest
sed -i 's|^let main = |let main () = |' games/TinyPacman.ml   # see below
dune build ./tmpcheck/Sim.exe && ./_build/default/tmpcheck/Sim.exe
sed -i 's|^let main () = |let main = |' games/TinyPacman.ml; rm -rf tmpcheck
```

The `sed` is there because a game's `let main = run_app app` runs at
module initialization: linking the game into another program would
open its window and never return. Turning it into a function for the
duration of the check avoids that (and must be undone before
committing). The game's `.mli` exports nothing, but `copy_files` brings
the `.ml` alone, so `Sim` sees all of it.
