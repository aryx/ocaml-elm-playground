(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyVi in the terminal you type this in: Tui_vi's program, the
 * one ../TinyVi.ml runs on the playground, run by Tty_unix (raw mode,
 * the alternate screen, curses' bytes to stdout). Its files are its
 * own disk's, in memory: the terminal's filesystem is not touched.
 *
 * What it uses: Tty_unix, appkits/editor's Tui_vi, and the
 * capabilities of Cap.main: stdin and stdout. No Playground at all.
 *)

let () = Cap.main (fun caps -> Tty_unix.run caps Tui_vi.program)
