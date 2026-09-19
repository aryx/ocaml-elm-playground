# scripts/

Support programs for developing and debugging the playground and its
games, run from the repository's root. Each one's header says what it
does, why, and how to use it; the techniques behind them are in
`docs/claude_notes/dev/notes_debugging_techniques.md`.

- `frames/`: getting frames, and looking at them
  - `render_frame.sh`: one frame of a native program, offscreen (SDL's
    dummy driver), keys held over frames (`-script`), as a PNG
  - `compare_golden.py`: before approving new golden frames: golden,
    new, and their difference, side by side, with the box that changed
  - `screenshot_playground3d.sh`: a screenshot of a real window (the
    OpenGL backend, which the dummy driver can't run)
  - `ref_frames_3d.sh`: reference frames of the 3D examples, captured
    before a change, checked after it
- `smoke/`: running everything quickly, to catch crashes and hangs
  - `smoke_test_playground3d.sh`: every 3D example and game, native
    and OpenGL, a few seconds each
  - `smoke_test_web.sh`: every web game in node (a fake DOM, keys
    pressed), every WebGL page screenshotted by headless Chrome
- `perf/`: how fast
  - `fps.sh`: frames per second, uncapped (beware OpenGL's vsync)
  - `bench_playground.sh`: the 2D software rasterizer's benchmark,
    with its debug keys (medians of several runs)
- `web/`: running a web program without a browser
  - `web_headless.js`: a game's `.bc.js` in node with a fake DOM:
    hangs, exceptions, the DOM after scripted keys (see
    `docs/claude_notes/dev/notes_headless.md`)
- `input/`: driving a real window
  - `xdrive.py`: mouse moves, clicks, keys, through X11's XTEST
- `games/`: checking the games' data, read from their sources
  - `sokoban_solve.py`: TinySokoban's levels solvable, and how
  - `check_maze.py`: TinyPacman's maze (reachable dots, no dead ends)
  - `plot_track.py`: the racing kit's course in space, how close it
    comes back near itself, a top view

Throwaway simulations of a game's rules, once worth keeping, go in
`tests/games/` instead: they're tests.
