(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Turbo_menus.mli *)

open Turbo_model

let item label hot shortcut action = { label; hot; shortcut; action }

let menus : (string * item list) list =
  [ ( "File",
      [ item "Open..." 'O' "F3" Open; item "New" 'N' "" New; item "Save" 'S' "F2" Save; item "Save as..." 'a' "" Save_as;
        item "Exit" 'x' "Alt+X" Exit ] );
    ("Search", [ item "Find..." 'F' "" Find; item "Search again" 'S' "Ctrl+L" Find_again; item "Go to line number..." 'G' "" Goto_line ]);
    ( "Run",
      [ item "Run" 'R' "Ctrl+F9" Run; item "Step over" 'S' "F8" Step_over; item "Trace into" 'T' "F7" Trace_into;
        item "Go to cursor" 'G' "F4" Go_to_cursor; item "Program reset" 'P' "Ctrl+F2" Reset; item "User screen" 'U' "Alt+F5" User_screen ] );
    ("Compile", [ item "Compile" 'C' "Alt+F9" Compile; item "Make" 'M' "F9" Compile; item "P-code" 'P' "" Pcode_listing ]);
    ( "Debug",
      [ item "Call stack" 'C' "Ctrl+F3" Call_stack; item "Add watch..." 'W' "Ctrl+F7" Add_watch;
        item "Toggle breakpoint" 'B' "Ctrl+F8" Toggle_breakpoint; item "Remove all watches" 'R' "" Clear_watches ] );
    ("Help", [ item "Keys" 'K' "F1" Keys; item "About..." 'A' "" About ]) ]

(*****************************************************************************)
(* Menus and dialogs *)
(*****************************************************************************)

let files (m : model) : string list = List.sort compare (List.map fst m.disk)

let write (m : model) (file : string) : model =
  { m with disk = (file, Turbo_edit.text m) :: List.remove_assoc file m.disk; file; modified = false }

let act (m : model) (a : action) : model =
  let m = { m with mode = Editing } in
  match a with
  | Open -> { m with mode = Open_dialog 0 }
  | New -> { (Turbo_edit.load m "") with lines = [| "" |]; file = Turbo_edit.noname }
  | Save -> if m.file = Turbo_edit.noname then { m with mode = Input { title = "Save File As"; label = "Save file as"; text = ""; purpose = Saving_as } } else write m m.file
  | Save_as -> { m with mode = Input { title = "Save File As"; label = "Save file as"; text = ""; purpose = Saving_as } }
  | Exit -> { m with quit = true }
  | Find -> { m with mode = Input { title = "Find"; label = "Text to find"; text = m.search; purpose = Finding } }
  | Find_again -> Turbo_edit.find m m.search
  | Goto_line -> { m with mode = Input { title = "Go to Line Number"; label = "Enter new line number"; text = ""; purpose = Going_to } }
  | Run -> Turbo_debug.go m (Continue m.breakpoints)
  | Trace_into -> Turbo_debug.go m Trace_into
  | Step_over -> Turbo_debug.go m Step_over
  | Go_to_cursor -> Turbo_debug.go m (To_line (m.row + 1))
  | Reset -> { m with session = None }
  | User_screen -> (
      match (m.session, m.last_screen) with
      | Some s, _ -> { m with mode = Showing s.user }
      | None, Some vt -> { m with mode = Showing vt }
      | None, None -> { m with mode = Showing (Vt.create ~rows:24 ~cols:80) })
  | Call_stack -> if m.session = None then { m with mode = Info ("Call Stack", [ "No program is running:"; "F7 or F8 starts one." ]) } else { m with mode = Stack }
  | Add_watch -> { m with mode = Input { title = "Add Watch"; label = "Watch expression"; text = Turbo_debug.word_at m; purpose = Watching } }
  | Toggle_breakpoint ->
      let l = m.row + 1 in
      { m with breakpoints = (if List.mem l m.breakpoints then List.filter (( <> ) l) m.breakpoints else l :: m.breakpoints) }
  | Clear_watches -> { m with watches = [] }
  | Compile -> ( match Turbo_debug.compile m with Ok (p, m) -> Turbo_debug.compiled_box m p | Error m -> m)
  | Pcode_listing -> (
      match Turbo_debug.compile m with
      | Error m -> m
      | Ok (p, m) ->
          (* from the first instruction of the cursor's line *)
          let first = ref 0 in
          (try Array.iteri (fun i l -> if l >= m.row + 1 then (first := i; raise Exit)) p.lines with Exit -> ());
          { m with mode = Listing (max 0 (!first - 3)) })
  | Keys ->
      { m with
        mode =
          Info
            ( "Keys",
              [ "F9 Make   Alt+F9 Compile   Ctrl+F9 Run"; "Alt+F5 User screen   F2 Save   F3 Open"; "F10 or Alt+letter: the menus   Alt+X Exit"; "";
                "F7 Trace into   F8 Step over   F4 Go to cursor"; "Ctrl+F8 Breakpoint   Ctrl+F7 Watch   Ctrl+F3 Calls"; "Ctrl+F2 Reset   Ctrl+C Break"; "";
                "Arrows, or Ctrl+E X S D    Ctrl+A F words"; "Ctrl+Y delete a line    Insert: overwrite"; "Ctrl+L search again"; "";
                "No F keys? Esc then 1 to 0 (or Alt+1 to Alt+0): F1 to F10,"; "Ctrl+1 to Ctrl+0: Ctrl+F1 to Ctrl+F10 (Ctrl+9 Run)" ] ) }
  | About ->
      { m with
        mode =
          Info
            ( "About",
              [ "Tiny Turbo Pascal"; ""; "after Turbo Pascal 7.0"; "Anders Hejlsberg, Borland, 1983-1992"; "";
                "compiled to P-code, as Wirth's Pascal-P"; "and UCSD Pascal did" ] ) }

(* Alt and a letter, as xterm sends it: Escape then the letter *)
let alt_letter (k : string) : char option = if String.length k = 2 && k.[0] = '\x1b' then Some (Char.lowercase_ascii k.[1]) else None

let menu_of_letter (c : char) : int option =
  let rec go i = function [] -> None | (title, _) :: rest -> if Char.lowercase_ascii title.[0] = c then Some i else go (i + 1) rest in
  go 0 menus

let menu_key (m : model) (bar : int) (sel : int) (k : string) : model =
  let items = snd (List.nth menus bar) in
  let n = List.length items and nbar = List.length menus in
  match k with
  | "\x1b" -> { m with mode = Editing }
  | "\x1b[D" -> { m with mode = Menu ((bar + nbar - 1) mod nbar, 0) }
  | "\x1b[C" -> { m with mode = Menu ((bar + 1) mod nbar, 0) }
  | "\x1b[A" -> { m with mode = Menu (bar, (sel + n - 1) mod n) }
  | "\x1b[B" -> { m with mode = Menu (bar, (sel + 1) mod n) }
  | "\r" -> act m (List.nth items sel).action
  | _ -> (
      match alt_letter k with
      | Some c -> ( match menu_of_letter c with Some b -> { m with mode = Menu (b, 0) } | None -> m)
      | None -> (
          let c = if String.length k = 1 then Some (Char.lowercase_ascii k.[0]) else None in
          match List.find_opt (fun it -> Some (Char.lowercase_ascii it.hot) = c) items with Some it -> act m it.action | None -> m))

let input_key (m : model) (title, label, text, purpose) (k : string) : model =
  let again text = { m with mode = Input { title; label; text; purpose } } in
  match k with
  | "\x1b" -> { m with mode = Editing }
  | "\r" -> (
      let m = { m with mode = Editing } in
      match purpose with
      | Saving_as -> if text = "" then m else write m (String.uppercase_ascii text)
      | Finding -> Turbo_edit.find m text
      | Going_to -> ( match int_of_string_opt (String.trim text) with Some n -> { m with row = n - 1; col = 0 } | None -> m)
      | Watching -> if String.trim text = "" then m else { m with watches = m.watches @ [ String.trim text ] })
  | "\x7f" | "\b" -> again (if text = "" then "" else String.sub text 0 (String.length text - 1))
  | _ when String.length k = 1 && k.[0] >= ' ' -> again (text ^ k)
  | _ -> m
