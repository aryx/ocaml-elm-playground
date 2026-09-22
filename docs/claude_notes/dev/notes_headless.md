# Testing the web backends with headless Chrome

A short tutorial on checking that a web Playground app (a `.bc.js` built
from `examples/web/`, `games/web/`, `examples/svg/`, ...) actually
*renders* something, from a shell, without opening a browser window.
First used to smoke-test the new SVG 3D examples (the
`elm_playground_3d_web` backend) in one command.

See also `notes_debugging_techniques.md` section 6. That section covers
`web_headless.js`, which runs a `.bc.js` in node with a fake DOM. The two
tools complement each other. The table at the end says when to use which.

## 1. The idea

Chrome (and Chromium) can run with no window at all: `--headless`. In
that mode it can load a page, run its JavaScript, and then either:

- print the resulting DOM as HTML (`--dump-dom`), or
- save a PNG of what the page looks like (`--screenshot`).

Our web backends render into the DOM (`playground/web/Playground_platform.ml`
builds an `<svg>` with one element per shape, and the 3D web backend
compiles the 3D scene down to 2D polygons on that same path). So the
dumped DOM *is* the rendered picture, in text form. You can `grep` it,
count shapes, and diff it. The screenshot is for when you want to see it
with your own eyes, or have Claude `Read` the PNG.

## 2. The key flag: `--virtual-time-budget`

A Playground app never "finishes loading": it keeps requesting animation
frames forever. Without extra flags, `--dump-dom` dumps the DOM right after
the load event, so you only see the first frame or two. For example,
`Cube3d` has 1 polygon then, but 3 once it has been running for a while.

`--virtual-time-budget=N` tells Chrome to run the page on a *virtual*
clock for N milliseconds of page time, fast-forwarding timers and
`requestAnimationFrame`, and only then dump or screenshot. So:

- the app gets ~N ms worth of frames (animations move, models update),
- the command always terminates, and quickly (about 1s wall-clock for
  `--virtual-time-budget=2000`),
- the result is deterministic enough to compare across runs.

Still wrap it in `timeout`, like any GUI command in a tool call (see
`notes_debugging_techniques.md` section 1). If it gets killed, that is
a finding in itself.

(The note in `notes_debugging_techniques.md` section 6 says headless
Chrome "never exited" there. That was on macOS, without
`--virtual-time-budget`, and on a page stuck in an infinite loop. On
Linux, with the flag, and on a healthy page, it works well. A page that
truly hangs in a single JS call will still hang, and for that case
`web_headless.js` is the better tool.)

## 3. Recipes

Build first. `make` (i.e., `dune build @default`) also copies the `.html`
pages next to the `.bc.js` files in `_build/`:

```bash
make
cd _build/default/examples/svg
```

Which binary: on this machine `google-chrome`, `/snap/bin/chromium` and
`firefox` all exist. The flags below are Chrome/Chromium flags.
`--headless=new` is the modern headless mode (same renderer as normal
Chrome). `--disable-gpu` avoids GPU/driver noise.

### 3a. Dump the DOM

```bash
timeout 30 google-chrome --headless=new --disable-gpu \
  --virtual-time-budget=2000 --dump-dom file://$PWD/Cube3d.html 2>/dev/null
```

Note the `file://$PWD/...`: Chrome needs an absolute URL. Loading from
`file://` is fine here because the page only loads its sibling `.bc.js`.
(`TexturedCube3d` would need an HTTP server if the web backend ever loads
real images. It doesn't yet.)

### 3b. Smoke-test every app in a directory: count the shapes

```bash
for f in *.html; do
  n=$(basename $f .html)
  echo "$n: $(timeout 30 google-chrome --headless=new --disable-gpu \
      --virtual-time-budget=2000 --dump-dom file://$PWD/$f 2>/dev/null \
    | grep -o '<polygon\|<path\|<circle\|<rect\|<ellipse\|<text\|<image' \
    | sort | uniq -c | tr '\n' ' ')"
done
```

This printed, for the SVG 3D examples:

```
Cube3d:                   3 <polygon
Cubes3d:                 70 <polygon
FloatingCity3d:          92 <polygon
InteractiveCube3d:       11 <polygon
PaintersAlgorithmFail3d:  6 <polygon
Spheres3d:               60 <polygon
TexturedCube3d:           3 <polygon
```

What to look for:
- an **empty line** means nothing was rendered: an exception at startup, a
  missing `.bc.js`, or a wrong library in the dune file (see 3d below);
- **plausible counts**: a cube seen from a corner shows 3 faces (the
  other 3 are backface-culled), and 70 for Cubes3d is a few cubes' worth
  of visible faces. A count that is wildly off points at culling or
  tessellation bugs;
- **the shape kinds**: the 3D web backend should only emit `<polygon>`s.

This is the check I'd rerun after touching `Playground3d.render3d_to_2d`
or `playground/web/`.

### 3c. Screenshot, and look at it

```bash
S=/tmp/some/scratch/dir
timeout 30 google-chrome --headless=new --disable-gpu \
  --virtual-time-budget=2000 --window-size=800,600 \
  --screenshot=$S/cube.png file://$PWD/Cube3d.html
```

Then open the PNG (or, in Claude Code, `Read` it: the model sees
images). `--window-size` is the "screen" the Playground sees, so it sets
`computer.screen` and hence the SVG `viewBox`. Without it you get Chrome's
default headless size. If a scene looks off-center, first check whether
it is just where the scene's camera points: with `--window-size=800,600`,
Cube3d is perfectly centered, while FloatingCity3d's towers sit in the
upper half of the image because of its camera, not because of a bug.

### 3d. See console output and JS exceptions

```bash
timeout 30 google-chrome --headless=new --disable-gpu \
  --virtual-time-budget=2000 --enable-logging=stderr --v=0 \
  --dump-dom file://$PWD/Cube3d.html 2>&1 >/dev/null | grep CONSOLE
```

Every `console.log` and every uncaught exception shows up as a line like:

```
[...:INFO:CONSOLE:1] "Uncaught TypeError: Cannot read properties of null (reading 'foo')", source: file:///.../err.html (1)
```

OCaml exceptions escaping from js_of_ocaml code show up here too. So when
3b prints an empty line, rerun that app with 3d to find out why. The
`2>&1 >/dev/null` order sends stderr to the pipe and throws away the
dumped DOM.

## 4. Headless Chrome vs `web_headless.js` (node + fake DOM)

| question                                        | use                                        |
|-------------------------------------------------|--------------------------------------------|
| does it render at all? how many shapes?         | Chrome `--dump-dom` (3b)                   |
| does it *look* right?                           | Chrome `--screenshot` (3c)                 |
| why does it crash at startup?                   | Chrome console logging (3d)                |
| it hangs / freezes the tab                      | `web_headless.js` (progress per frame)     |
| what happens after pressing keys, frame N?      | `web_headless.js` (scripted keys, `DUMP=1`)|
| does it use a DOM API not in the real browser?  | Chrome (the real DOM, not a fake one)      |

Rule of thumb: Chrome gives you the *real* browser with no interaction.
`web_headless.js` gives you a *fake* browser that you fully control:
frames, time, and keys.

## 5. Limitations

- No input: you can't press keys or move the mouse with plain command-line
  flags. For interactive checks, use `web_headless.js`, or drive a real
  browser (Puppeteer/Playwright, or the claude-in-chrome extension).
- Virtual time is not wall-clock time. It shows *what* gets rendered, not
  how fast. For frame-rate questions, open the page in a real Chrome and
  use its Performance tab.
- One snapshot per invocation, taken at the end of the budget. To see an
  animation evolve, rerun with several budgets (500, 1000, 2000, ...) and
  compare.
