(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of vi (Bill Joy, Berkeley, 1976) on a terminal's
 * screen: README is open, and says the keys.
 *
 *   normal mode: h j k l, w b e, 0 $, gg G, x dd dw cw D, yy p P, u .,
 *   i a o and Escape, /word n; after a colon, :w :q :q! :wq :e poem.txt
 *   :12 :%s/old/new/g :set number; flags: phosphor=white or amber (green by default)
 *
 * vi's two ideas are here: modes (a key is a command, until i or a or
 * o, then text until Escape), and commands as sentences, an operator
 * and a motion with counts that multiply -- d2w, 3dd, c$ -- which "."
 * repeats whole. Tui_vi.mli tells their story and has the grammar;
 * u is vi's one level of undo, u again redoing.
 *
 * The program is a Tui program (Model-View-Update with a screen of
 * characters), run here by the Textmode way: each key, curses sends
 * the VT100 the bytes that changed, counted under the screen -- the
 * economy that made a screen editor usable at 300 baud. The same
 * program runs in a real terminal: dune exec apps/devtools/tty/TinyVi.exe.
 *
 * What it uses: the Playground, its Textmode way (over libs/terminal's
 * Tui, Curses and Vt), and appkits/editor's Tui_vi. Nothing else: no
 * kit, no gui/.
 *
 * Left undone, exercises: ed's regular expressions in / and :s (Ken
 * Thompson's, 1968, compiled to a machine: plan_teaching_languages.md);
 * lines longer than the screen wrapped rather than cut; the registers
 * "a to "z; marks (ma, 'a); visual mode (vim's, 1991); an undo tree;
 * :r and :! to read a file and run a command; real files through a
 * capability when run in a terminal.
 *)

let phosphor (flags : Playground.flags) : Playground.color option =
  match List.assoc_opt "phosphor" flags with
  | Some "white" -> Some (Playground.rgb 215 220 235)
  | Some "amber" -> Some (Playground.rgb 255 176 0)
  | _ -> None

let app = Textmode.textmode ?phosphor:(phosphor (Playground_platform.flags ())) Tui_vi.program
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
