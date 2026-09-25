(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Snake in the terminal you type this in: Tui_snake's program, the one
 * ../TextmodeSnake.ml runs on the playground, run by Tty_unix (raw mode,
 * the alternate screen, curses' bytes to stdout). q or Control-C quits,
 * and the shell's screen comes back.
 *
 * What it uses: Tty_unix, appkits/tui's Tui_snake, and the capabilities
 * of Cap.main: stdin and stdout. No Playground at all.
 *)

let () = Cap.main (fun caps -> Tty_unix.run caps Tui_snake.program)
