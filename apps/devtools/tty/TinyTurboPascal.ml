(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyTurboPascal in the terminal you type this in: Tui_turbo's program,
 * the one ../TinyTurboPascal.ml runs on the playground, run by Tty_unix.
 * The terminal's own colours stand for the PC's (bold yellow is
 * usually bright), and its font must have the box-drawing characters
 * (every terminal's has, nowadays). Its files are its own disk's, in
 * memory.
 *
 * What it uses: Tty_unix, appkits/editor's Tui_turbo, and the
 * capabilities of Cap.main: stdin and stdout. No Playground at all.
 *)

let () = Cap.main (fun caps -> Tty_unix.run caps Tui_turbo.program)
