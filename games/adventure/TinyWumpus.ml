(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Hunt the Wumpus (Gregory Yob, 1973): twenty rooms
 * in the dark, a Wumpus, and five crooked arrows.
 *
 *   type s or m and enter, then the room(s); the smells, drafts and
 *   rustles of the next rooms are all you see; flags: paper (the roll
 *   of paper), baud=110 (the Teletype's speed), seed=n (another cave)
 *
 * The game is Tty_wumpus.ml's, written as a conversation (Talk.mli):
 * the cave as a table of tunnels, a turn a question, the Wumpus woken
 * three times in four. Its header tells the cave's shape, the
 * dodecahedron flattened into three rings, and why Yob chose it. This
 * file only gives the game a screen of its own; TinyTerminal's shell
 * runs the same program as its command wumpus.
 *
 * What it uses: the Playground, Teletype (over libs/terminal's Talk),
 * and appkits/teletype's Tty_wumpus. Not kit_adventure (TinyZork's
 * world of objects and rules): a cave of numbered rooms needs a table,
 * not a world.
 *
 * Left undone, exercises: Wumpus II's other caves (Yob's sequel: a
 * Möbius strip, a torus, a cave you type in yourself); the map the
 * player draws, drawn by the game as it learns it (the first view that
 * isn't text); a player program that reasons about the warnings, Russell
 * and Norvig's knowledge-based agent, over ai/.
 *)

let app = Teletype.teletype Tty_wumpus.program
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
