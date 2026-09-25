(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Turbo Pascal (Anders Hejlsberg, Borland, 1983), in
 * the look of its version 7 (1992): the IDE on the IBM PC's text screen,
 * Wirth's eight queens open in its blue window.
 *
 *   type; F9 compiles, Ctrl-F9 runs (a key comes back from the program's
 *   screen), Alt-F5 shows that screen again; F10 or Alt and a letter
 *   for the menus (Compile / P-code: the code of the cursor's line);
 *   F2 saves, F3 opens, Alt-X quits; the editor's keys are WordStar's
 *   too (Ctrl-E X S D, Ctrl-Y); the debugger: F7 trace into, F8 step
 *   over, F4 go to the cursor, Ctrl-F8 a breakpoint, Ctrl-F7 a watch,
 *   Ctrl-F3 the call stack, Ctrl-F2 reset, Ctrl-C break (and all of it
 *   in the Run and Debug menus, for the keys a desktop keeps)
 *
 * Turbo Pascal's lesson is the one keystroke from the text to the
 * program running, or to the error with the cursor on it: the editor,
 * the compiler and the program in one place, the "integrated
 * development environment". Here the compiler is libs/languages/pascal's,
 * one pass to P-code as Wirth's Pascal-P did it (Pascal_compile.mli),
 * and the program runs on its P-machine (Pmachine.mli) -- where the
 * real one compiled to the 8086's own code, which is why it was fast.
 * Tui_turbo.mli has the IDE; its colours are the PC's CGA sixteen, its
 * frames the PC's box-drawing characters.
 *
 * It runs by the Textmode way (with its pc colours) and in a real
 * terminal the same: dune exec apps/devtools/tty/TinyTurboPascal.exe.
 *
 * What it uses: the Playground, its Textmode way (over libs/terminal),
 * appkits/editor's Tui_turbo and libs/languages/pascal. No gui/: the
 * menus and dialogs are drawn cell by cell (the cell Look for gui/ of
 * plan_terminal.md, Turbo Vision's way, would be the other way).
 *
 * The debugger is Turbo Pascal's, over what the compiler leaves for it
 * (Pcode.mli: where each statement begins, each procedure's code and
 * variables) and a P-machine that pauses (Pdebug.mli): stepping stops
 * at a statement's start, a watch is a name looked up along the static
 * links, and the call stack shows them beside the dynamic ones.
 *
 * Left undone, exercises: watches of any expression (a[i + 1], a
 * compiled expression rather than a name and selectors); changing a
 * variable while paused (Ctrl-F4, Evaluate and modify); a breakpoint
 * with a condition; several windows (F6); blocks (Ctrl-K B, Ctrl-K K); undo;
 * the mouse; compiling to a machine's own code, as the real one did.
 *)

let app = Textmode.textmode ~pc:true Tui_turbo.program
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
