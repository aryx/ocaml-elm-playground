(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of GNU Emacs (Richard Stallman, 1985) on a terminal's
 * screen: *scratch*, where Lisp is typed and evaluated.
 *
 *   C-f C-b C-n C-p, M-f M-b, C-a C-e, C-v M-v, M-< M->; C-d DEL, C-k
 *   C-y M-y, C-SPC C-w M-w, C-/ undo, C-s search; C-x C-f a file (C-h t
 *   the tutorial), C-x C-s save, C-x b, C-x C-b; M-x a command by name
 *   (TAB completes), M-: or C-j in *scratch* to evaluate Lisp; C-h k,
 *   C-h f; C-g cancels, C-x C-c quits; Meta is Alt, or Escape first;
 *   flags: phosphor=white or amber (green by default)
 *
 * Emacs is a Lisp machine with a screen: the keys are a table of Lisp
 * functions (C-h k then a key says which), most of those functions are
 * Lisp (Emacs_simple.mli), and the user's own .emacs is loaded when it
 * starts -- visit it (C-x C-f .emacs) to see a key bound and a command
 * defined, change it, M-x eval-buffer, and the editor has changed.
 * The Lisp is libs/languages/lisp's, dynamically scoped as Emacs's
 * was; the core it drives, buffers over a gap buffer, the undo list
 * and the kill ring, is Emacs_editor.mli; the command loop and the
 * display Tui_emacs.mli.
 *
 * It runs by the Textmode way, curses' bytes to a VT100 (their count
 * under the screen), and in a real terminal the same:
 * dune exec apps/devtools/tty/TinyEmacs.exe.
 *
 * What it uses: the Playground, its Textmode way (over libs/terminal),
 * appkits/editor (Tui_emacs, Emacs_editor, Emacs_simple, Gap_buffer)
 * and libs/languages/lisp. No kit, no gui/.
 *
 * Left undone, exercises: windows (C-x 2, two views of buffers);
 * keyboard macros (C-x ( C-x ) C-x e: keys recorded and replayed, the
 * TECO heritage); query-replace (M-%); major modes with keymaps of
 * their own and buffer-local variables, instead of mode-name read by
 * C-j; markers (a save-excursion's point moving with the text before
 * it); lexical-binding and closures; lines wrapped instead of cut.
 *)

let phosphor (flags : Playground.flags) : Playground.color option =
  match List.assoc_opt "phosphor" flags with
  | Some "white" -> Some (Playground.rgb 215 220 235)
  | Some "amber" -> Some (Playground.rgb 255 176 0)
  | _ -> None

let app = Textmode.textmode ?phosphor:(phosphor (Playground_platform.flags ())) Tui_emacs.program
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
