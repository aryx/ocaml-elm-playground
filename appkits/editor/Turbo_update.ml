(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Turbo_update.mli *)

open Turbo_model

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* claude: Esc then a digit, F1 to F10 (0 for F10): the convention of
 * Midnight Commander and the terminals without function keys, for the
 * keyboards whose top row is the volume's and the screen's, the
 * desktop's or the browser's. Two keys typed one after the other, or
 * Alt and the digit at once (Escape and the digit, the same bytes) --
 * though not on a Mac, where Option and a digit types another
 * character (Option-9, "ª"): there, Esc and then the digit *)
let digit (k : string) : bool = String.length k = 1 && k.[0] >= '0' && k.[0] <= '9'

let f_of_digit (d : char) : string = if d = '0' then "F10" else "F" ^ String.make 1 d

(* and Control and a digit (Vt.key's ESC [ 27 ; 5 ; <code> ~), Control
 * and that F key: Ctrl-9, Ctrl-F9, Run *)
let function_key (k : string) : string =
  let n = String.length k in
  if n = 2 && k.[0] = '\x1b' && digit (String.sub k 1 1) then Option.value ~default:k (Vt.key ~ctrl:false (f_of_digit k.[1]))
  else if n = 10 && String.sub k 0 7 = "\x1b[27;5;" && k.[9] = '~' then
    match int_of_string_opt (String.sub k 7 2) with
    | Some c when c >= 48 && c <= 57 -> Option.value ~default:k (Vt.key ~ctrl:true (f_of_digit (Char.chr c)))
    | _ -> k
  else k

let rec key (m : model) (k : string) : model =
  match (m.mode, k) with
  (* in the editor, a lone Esc waits for its digit (else it does
   * nothing, as before) *)
  | Editing, "\x1b" -> { m with escaped = true }
  | Editing, _ when m.escaped && digit k -> key { m with escaped = false } ("\x1b" ^ k)
  | _ -> key_now { m with escaped = false } k

and key_now (m : model) (k : string) : model =
  (* the running program's keys are its own *)
  let k = match m.mode with Executing -> k | _ -> function_key k in
  match m.mode with
  | Executing -> ( match m.session with Some s -> Turbo_debug.executing_key m s k | None -> { m with mode = Editing })
  | Finished (_, err) -> (
      match err with
      | Some (l, msg) -> { m with mode = Editing; error = Some msg; row = l - 1; col = 0 }
      | None -> { m with mode = Editing })
  | Showing _ | Info _ | Stack -> { m with mode = Editing }
  | Listing first -> (
      match k with
      | "\x1b[A" -> { m with mode = Listing (max 0 (first - 1)) }
      | "\x1b[B" -> { m with mode = Listing (first + 1) }
      | "\x1b[5~" -> { m with mode = Listing (max 0 (first - 15)) }
      | "\x1b[6~" -> { m with mode = Listing (first + 15) }
      | _ -> { m with mode = Editing })
  | Menu (bar, sel) -> Turbo_menus.menu_key m bar sel k
  | Open_dialog sel -> (
      let fs = Turbo_menus.files m in
      match k with
      | "\x1b[A" -> { m with mode = Open_dialog (max 0 (sel - 1)) }
      | "\x1b[B" -> { m with mode = Open_dialog (min (List.length fs - 1) (sel + 1)) }
      | "\r" -> Turbo_edit.load { m with mode = Editing } (List.nth fs sel)
      | _ -> { m with mode = Editing })
  | Input { title; label; text; purpose } -> Turbo_menus.input_key m (title, label, text, purpose) k
  | Editing -> (
      (* a key clears the red bar, as in Turbo Pascal *)
      let m = { m with error = None } in
      match k with
      | "\x1bOP" -> Turbo_menus.act m Keys
      | "\x1bOQ" -> Turbo_menus.act m Save
      | "\x1bOR" -> Turbo_menus.act m Open
      | "\x1b[20~" | "\x1b[20;3~" -> Turbo_menus.act m Compile
      | "\x1b[20;5~" -> Turbo_menus.act m Run
      | "\x1b[15;3~" -> Turbo_menus.act m User_screen
      | "\x1b[21~" -> { m with mode = Menu (0, 0) }
      (* the debugger's: F7 F8 F4, Ctrl-F2 Ctrl-F3 Ctrl-F7 Ctrl-F8 *)
      | "\x1b[18~" -> Turbo_menus.act m Trace_into
      | "\x1b[19~" -> Turbo_menus.act m Step_over
      | "\x1bOS" -> Turbo_menus.act m Go_to_cursor
      | "\x1b[1;5Q" -> Turbo_menus.act m Reset
      | "\x1b[1;5R" -> Turbo_menus.act m Call_stack
      | "\x1b[18;5~" -> Turbo_menus.act m Add_watch
      | "\x1b[19;5~" -> Turbo_menus.act m Toggle_breakpoint
      | _ -> (
          match Turbo_menus.alt_letter k with
          | Some 'x' -> Turbo_menus.act m Exit
          | Some c -> ( match Turbo_menus.menu_of_letter c with Some b -> { m with mode = Menu (b, 0) } | None -> m)
          | None ->
              (* the text changed: the program running is another's,
                 reset (Turbo Pascal asked first) *)
              let m' = Turbo_edit.edit_key m k in
              if m'.lines != m.lines then { m' with session = None } else m'))

let update (ev : Tui.event) (m : model) : model =
  match (ev, m.mode) with
  | Tick _, Executing -> ( match m.session with Some s -> Turbo_debug.advance m s | None -> { m with mode = Editing })
  | Tick _, _ -> m
  | Key k, _ -> Turbo_edit.follow (key m k)
