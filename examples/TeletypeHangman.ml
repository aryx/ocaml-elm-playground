(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Hangman, as the teletype played it: guess the word a letter at a
 * time, before the man is hanged.
 *
 *   type a letter and enter; flags: paper (the roll of paper), baud=110
 *   (the Model 33's speed), seed=n (another word)
 *
 * The game is Tty_hangman.ml's, a program written as a conversation
 * (the lesson is there, and in Talk.mli); this file only gives it a
 * screen of its own. TinyTerminal's shell runs the same program as its
 * command hangman.
 *
 * What it uses: the Playground, Teletype (over libs/terminal's Talk),
 * and appkits/teletype's Tty_hangman. Nothing else.
 *)

let app = Teletype.teletype Tty_hangman.program
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
