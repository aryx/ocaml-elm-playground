(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tui_turbo.mli *)

open Turbo_model

type model = Turbo_model.model

(*****************************************************************************)
(* The start *)
(*****************************************************************************)

let init : model =
  Turbo_edit.load
    { lines = [| "" |]; row = 0; col = 0; top = 0; left = 0; file = Turbo_edit.noname; modified = false; overwrite = false; disk = Pascal_disk.files;
      mode = Editing; error = None; compiled = None; last_screen = None; search = ""; runs = 0; session = None; breakpoints = []; watches = [];
      quit = false;
      escaped = false }
    "QUEENS.PAS"

let update = Turbo_update.update
let view = Turbo_view.view
let program : model Tui.program = { init; update; view; over = (fun m -> m.quit) }
let lines (m : model) = Array.to_list m.lines
let cursor (m : model) = (m.row, m.col)
let error (m : model) = m.error

let screen (m : model) =
  match m.mode with
  | Editing -> "edit"
  | Menu _ -> "menu"
  | Open_dialog _ | Input _ | Info _ -> "dialog"
  | Executing -> "run"
  | Stack -> "dialog"
  | Finished _ | Showing _ -> "user"
  | Listing _ -> "p-code"

let file (m : model) (name : string) = List.assoc_opt name m.disk
let execution_line = Turbo_debug.execution_line
