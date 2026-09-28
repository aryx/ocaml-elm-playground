(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Snake in characters, on the playground: a full-screen program of the
 * terminal, run by the Textmode way.
 *
 *   the arrows, or h j k l, turn; q quits, r starts again after a crash
 *
 * The program is Tui_snake.ml's, a Tui program (Model-View-Update with
 * a screen of characters); this file only runs it here. Under the
 * screen, the bytes curses sent this frame (a few: the head drawn, the
 * tail erased) against a whole redraw's (Curses.mli). The same program
 * runs in a real terminal: dune exec examples/tty/TextmodeSnake.exe.
 *
 * What it uses: the Playground, Textmode (over libs/terminal's Curses,
 * Tui and Vt), and appkits/tui's Tui_snake. Nothing else.
 *)

let app = Textmode.textmode Tui_snake.program
let main = Playground_platform.run_app app
