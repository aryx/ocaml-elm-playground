(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyVisiCalc: the program that sold the Apple II
 * (Dan Bricklin and Bob Frankston, 1979; plan_gui_teaching.md, phase
 * 7b).
 *
 * What it uses: appkits/sheet (Sheet, the engine) and its formula
 * language (Formula, libs/languages), and the playground's shapes. What it deliberately uses *nothing* of:
 * gui/ -- no widget, no layout, no focus, no mouse. That is not
 * minimalism, it is the subject: VisiCalc ran on a 40-column
 * character display with no mouse to point at anything, and every
 * decision in it follows from that. TinyExcel (phase 8) is the same
 * engine with 1985's answers, and the pair is the lesson.
 *
 * What 1979 looks like, and why:
 *
 *   - **the cursor is the interface.** Arrows move it, and what you
 *     type goes into the cell it is on. There is no pointing device,
 *     so there is no clicking, so there is nothing to click on;
 *   - **three lines at the top**: what is in the cell the cursor is
 *     on, the prompt, and what you are typing. A character display
 *     has no room for anything else, and everything a modern
 *     spreadsheet puts in toolbars was a letter after a slash;
 *   - **the slash commands**: / then a letter. B blanks the cell, C
 *     clears the sheet, S is "storage" (S S and a name saves the
 *     sheet, S L loads one -- the same file TinyExcel opens, one
 *     engine), G is "global" -- and it is G that this program is
 *     really about;
 *   - **the formulas are spelled differently**: +B2*C2 rather than
 *     =B2*C2, and @SUM(B2...B4) rather than =SUM(B2:B4). The '='
 *     spelling and the ':' range came later; this program translates
 *     1979's into the engine's, which is six lines and a good
 *     reminder that a formula language is a surface, not a semantics.
 *
 * And the thing worth running it for. VisiCalc had no dependency
 * graph: it recalculated the whole sheet in **row order** or **column
 * order**, your choice (/G O R and /G O C), one pass. So a formula
 * that read a cell below or to the right of it was one pass behind,
 * and people learned to press the recalculate key (!) twice, and to
 * lay their sheets out so they flowed down and to the right.
 *
 * Press "/" then "G" then R, C or N here to switch between row order,
 * column order and the natural order Lotus 1-2-3 brought in 1983 --
 * which is this repository's third principle (the simple version
 * stays beside the better one, switchable) applied to an idea rather
 * than an algorithm. Row A1 of the opening sheet reads a cell below
 * it on purpose: in row order it is a pass behind until you press !.
 *
 * What it deliberately does not do: /R replicate (the command that
 * made spreadsheets useful, and the reason $A$1 exists), /I and /D
 * inserting and deleting rows, /T titles (frozen headings), /W
 * windows (the split screen), /P print, and formatting of
 * any kind.
 *
 * Exercises: /R replicate, the command this does not have -- and then
 * $A$1, which replicate is what makes necessary; /T titles (the
 * headings that stay put while the rest scrolls); a sheet written to
 * a file and read back (Sheet.to_string is already there); and
 * column widths, which on a character display means deciding what
 * nine characters was.
 *)
open Playground

(*****************************************************************************)
(* The character display *)
(*****************************************************************************)

let cols = 7 (* columns of the sheet shown *)
let rows = 14
let cell_chars = 9 (* how wide a column is, in characters *)
let char_w = 13.
let char_h = 26.
let screen_cols = 4 + (cols * cell_chars)
let green = rgb 120 255 140
let dim = rgb 70 150 90

(* the top-left corner of the character grid, so that it is centered *)
let origin_x = -.(float_of_int screen_cols *. char_w /. 2.)
let origin_y = 300.

let at_char col row =
  (origin_x +. (float_of_int col *. char_w), origin_y -. (float_of_int row *. char_h))

(* a string starting at a character position, left to right *)
let text ?(color = green) col row s =
  let x, y = at_char col row in
  words color s |> scale (char_w /. 0.6 /. words_font_size)
  |> move (x +. (Widget.text_width ~size:(char_w /. 0.6) s /. 2.)) y

(*****************************************************************************)
(* 1979's spelling of a formula *)
(*****************************************************************************)

(* +B2*C2 and @SUM(B2...B4) into the engine's =B2*C2 and =SUM(B2:B4).
   A formula language is a surface, not a semantics. *)
let of_1979 (s : string) : string =
  let s = String.trim s in
  if s = "" then s
  else if s.[0] = '+' || s.[0] = '@' then
    let body = if s.[0] = '+' then String.sub s 1 (String.length s - 1) else s in
    let buf = Buffer.create (String.length body) in
    let i = ref 0 in
    let n = String.length body in
    while !i < n do
      if !i + 2 < n && String.sub body !i 3 = "..." then (
        Buffer.add_char buf ':';
        i := !i + 3)
      else if body.[!i] = '@' then incr i
      else (
        Buffer.add_char buf body.[!i];
        incr i)
    done;
    "=" ^ Buffer.contents buf
  else s

(* and back, to show it the way it was typed *)
let to_1979 (s : string) : string =
  if String.length s > 0 && s.[0] = '=' then
    let body = String.sub s 1 (String.length s - 1) in
    let body = String.concat "..." (String.split_on_char ':' body) in
    let starts_with_name =
      String.length body > 0 && body.[0] >= 'A' && body.[0] <= 'Z'
      && String.contains body '('
    in
    if starts_with_name then "@" ^ body else "+" ^ body
  else s

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type mode =
  | Ready
  | Entering of string
  | Command (* the slash prompt *)
  | Global (* after /G: waiting for R, C or N *)
  | Storage (* after /S: waiting for S or L *)
  | File_name of storage * string (* the name being typed *)

(* /S S saves the sheet to a file, /S L loads one: VisiCalc's floppy
   disk, here Playground_platform's store *)
and storage = Save_to | Load_from

type model = {
  sheet : Sheet.t;
  cursor : Formula.cell;
  mode : mode;
  (* None is the natural order Lotus brought in 1983; Some is 1979 *)
  order : Sheet.order option;
  message : string;
  was : string list;
}

(* a sheet with a forward reference in it on purpose: A1 reads B3,
   which is below it, so in row order it is a pass behind *)
let opening =
  [
    ((0, 0), "=B3*2"); ((1, 0), "twice the total");
    ((0, 2), "widgets"); ((1, 2), "=SUM(B4:B6)");
    ((0, 3), "north"); ((1, 3), "120");
    ((0, 4), "south"); ((1, 4), "80");
    ((0, 5), "east"); ((1, 5), "45");
  ]

let initial =
  {
    sheet = List.fold_left (fun s (c, text) -> Sheet.set c text s) Sheet.empty opening;
    cursor = (0, 0);
    mode = Ready;
    order = None;
    message = "";
    was = [];
  }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let commit model text =
  let engine_text = of_1979 text in
  match model.order with
  | None -> { model with sheet = Sheet.set model.cursor engine_text model.sheet }
  | Some order ->
      (* 1979: typing stored the cell, and the sheet was recalculated
         afterwards, in one pass *)
      let sheet = Sheet.store model.cursor engine_text model.sheet in
      { model with sheet = Sheet.recalculate order sheet }

(* named for what it moves: [move] is the playground's, for shapes *)
let move_cursor (dc, dr) model =
  let c, r = model.cursor in
  { model with cursor = (max 0 (min (cols - 1) (c + dc)), max 0 (min (rows - 1) (r + dr))) }

(* the same file as TinyExcel's: one engine, one Sheet.t -- a sheet
   saved here opens there, and back *)
let magic = "TinyExcel 1"

let storage (caps : < Cap.open_in ; Cap.open_out ; .. >) op name model =
  let file = name ^ ".sheet" in
  match op with
  | Save_to ->
      Playground_platform.store caps file (Saved.to_string ~magic model.sheet);
      { model with mode = Ready; message = "saved " ^ file }
  | Load_from -> (
      match Option.bind (Playground_platform.fetch caps file) (Saved.of_string ~magic) with
      | Some sheet -> { model with sheet; cursor = (0, 0); mode = Ready; message = "loaded " ^ file }
      | None -> { model with mode = Ready; message = "no sheet called " ^ file })

let update caps computer model =
  let k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key model.was) in
  let typed = k.typed in
  let model =
    match model.mode with
    | Ready ->
        let model =
          if pressed "ArrowLeft" then move_cursor (-1, 0) model
          else if pressed "ArrowRight" then move_cursor (1, 0) model
          else if pressed "ArrowUp" then move_cursor (0, -1) model
          else if pressed "ArrowDown" then move_cursor (0, 1) model
          else model
        in
        if typed = "/" then { model with mode = Command; message = "" }
        else if typed = "!" then
          (* the recalculate key, and the habit it taught *)
          (match model.order with
          | None -> { model with message = "natural order: nothing to catch up" }
          | Some order ->
              { model with sheet = Sheet.recalculate order model.sheet; message = "recalculated" })
        else if typed <> "" then { model with mode = Entering typed; message = "" }
        else model
    | Entering text ->
        if k.kenter then
          let model = commit model text in
          { (move_cursor (0, 1) model) with mode = Ready }
        else if List.mem "Escape" now then { model with mode = Ready }
        else if pressed "Backspace" && String.length text > 0 then
          { model with mode = Entering (String.sub text 0 (String.length text - 1)) }
        else if typed <> "" then { model with mode = Entering (text ^ typed) }
        else model
    | Command ->
        let letter = String.uppercase_ascii typed in
        if letter = "B" then
          { (commit { model with mode = Ready } "") with message = "blanked" }
        else if letter = "C" then { initial with order = model.order; message = "cleared" }
        else if letter = "G" then { model with mode = Global }
        else if letter = "S" then { model with mode = Storage }
        else if typed <> "" || List.mem "Escape" now then { model with mode = Ready }
        else model
    | Storage ->
        let letter = String.uppercase_ascii typed in
        if letter = "S" then { model with mode = File_name (Save_to, "") }
        else if letter = "L" then { model with mode = File_name (Load_from, "") }
        else if typed <> "" || List.mem "Escape" now then { model with mode = Ready }
        else model
    | File_name (op, name) ->
        if k.kenter && name <> "" then storage caps op name model
        else if List.mem "Escape" now then { model with mode = Ready }
        else if pressed "Backspace" && String.length name > 0 then { model with mode = File_name (op, String.sub name 0 (String.length name - 1)) }
        else if typed <> "" then { model with mode = File_name (op, name ^ typed) }
        else model
    | Global ->
        let letter = String.uppercase_ascii typed in
        if letter = "R" then
          { model with mode = Ready; order = Some Sheet.Rows; message = "order: by rows (1979)" }
        else if letter = "C" then
          { model with mode = Ready; order = Some Sheet.Columns; message = "order: by columns (1979)" }
        else if letter = "N" then
          { model with mode = Ready; order = None; message = "order: natural (1983)" }
        else if typed <> "" || List.mem "Escape" now then { model with mode = Ready }
        else model
  in
  { model with was = now }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* nine characters, and the >>>>>>>>> a number too wide showed *)
let fit s ~right =
  if String.length s > cell_chars then
    if right then String.make cell_chars '>' else String.sub s 0 cell_chars
  else if right then String.make (cell_chars - String.length s) ' ' ^ s
  else s ^ String.make (cell_chars - String.length s) ' '

let view _computer model =
  let cell_text (col, row) =
    let v = Sheet.value model.sheet (col, row) in
    let right = match v with Sheet.Number _ -> true | _ -> false in
    fit (Sheet.show v) ~right
  in
  let cursor_x, cursor_y = at_char (4 + (fst model.cursor * cell_chars)) (5 + snd model.cursor) in
  let raw = Sheet.raw model.sheet model.cursor in
  let kind =
    match Formula.content_of raw with
    | Formula.Text _ -> "(L)"
    | Formula.Blank -> "   "
    | _ -> "(V)"
  in
  [
    (* a character display, and the black it glowed on *)
    rectangle black 1000. 1000.;
    (* the three lines at the top: the cell, the prompt, the entry *)
    text 0 0 (Printf.sprintf "%s %s %s" (Formula.name_of_cell model.cursor) kind (to_1979 raw));
    text ~color:dim 0 1
      (match model.mode with
      | Command -> "COMMAND: B(lank) C(lear) G(lobal) S(torage)"
      | Global -> "GLOBAL ORDER: R(ows) C(olumns) N(atural)"
      | Storage -> "STORAGE: S(ave) L(oad)"
      | File_name (Save_to, _) -> "FILE FOR SAVING, THEN RETURN"
      | File_name (Load_from, _) -> "FILE TO LOAD, THEN RETURN"
      | _ ->
          Printf.sprintf "%s   ! recalculates   / commands"
            (match model.order with
            | None -> "order: natural (1983)"
            | Some Sheet.Rows -> "order: by rows (1979)"
            | Some Sheet.Columns -> "order: by columns (1979)"));
    text 0 2 (match model.mode with Entering s | File_name (_, s) -> "> " ^ s ^ "_" | _ -> model.message);
    (* the column letters, and the row numbers down the side *)
    text ~color:dim 0 4
      ("   "
      ^ String.concat ""
          (List.init cols (fun c ->
               let name = Formula.name_of_cell (c, 0) in
               let letters = String.sub name 0 (String.length name - 1) in
               fit letters ~right:false)));
  ]
  @ List.init rows (fun r -> text ~color:dim 0 (5 + r) (Printf.sprintf "%2d " (r + 1)))
  @ List.concat
      (List.init rows (fun r ->
           List.init cols (fun c -> text (4 + (c * cell_chars)) (5 + r) (cell_text (c, r)))))
  @ [
      (* the cursor: a block, as it was on a character display *)
      rectangle green (char_w *. float_of_int cell_chars) (char_h *. 0.9)
      |> fade 0.25
      |> move (cursor_x +. (char_w *. float_of_int cell_chars /. 2.) -. (char_w /. 2.)) cursor_y;
    ]

let app caps = game view (update caps) initial
let main = Cap.main (fun caps -> Playground_platform.run_app (app (caps :> < Cap.open_in ; Cap.open_out >)))
