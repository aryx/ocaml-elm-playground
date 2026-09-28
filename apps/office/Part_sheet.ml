(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground

let kind = "sheet"

(* smaller than TinyExcel's by default: a table in a document, not a
   screen -- unless a sheet document asks for more *)
let geometry ~cols ~rows : Sheet_view.geometry = { cols; rows; cell_w = 90.; cell_h = 24.; head_w = 30.; head_h = 24. }

type state = {
  sheet : Sheet.t;
  geo : Sheet_view.geometry;
  anchor : Formula.cell;
  focus : Formula.cell;
  (* what is being typed into the focused cell, if anything *)
  typing : string option;
  was : string list;
  was_down : bool;
}

(* the sheet at its own size, against the left of the part's box *)
let sheet_box geo (b : Widget.box) : Widget.box =
  let w, h = Sheet_view.size geo in
  { Widget.x = Widget.left b +. (w /. 2.); y = b.y; w; h }

let input computer (b : Widget.box) st =
  let m = computer.mouse and k = computer.keyboard in
  let now = Set_.elements k.keys in
  let pressed key = List.mem key now && not (List.mem key st.was) in
  let st =
    match Sheet_view.cell_at st.geo (sheet_box st.geo b) (m.mx, m.my) with
    | Some c when m.mdown && not st.was_down -> { st with anchor = c; focus = c; typing = None }
    | Some c when m.mdown -> { st with focus = c }
    | _ -> st
  in
  let st =
    if pressed "Enter" then
      match st.typing with
      | Some text ->
          (* in, and down to the next cell, as a spreadsheet does *)
          let c, r = st.focus in
          let next = (c, min (st.geo.rows - 1) (r + 1)) in
          { st with sheet = Sheet.set st.focus text st.sheet; typing = None; anchor = next; focus = next }
      | None -> st
    else if pressed "Escape" then { st with typing = None }
    else if pressed "Backspace" then
      match st.typing with
      | Some t when t <> "" -> { st with typing = Some (String.sub t 0 (String.length t - 1)) }
      | _ -> { st with typing = Some "" }
    else if k.typed <> "" then { st with typing = Some (Option.value st.typing ~default:"" ^ k.typed) }
    else st
  in
  { st with was = now; was_down = m.mdown }

let draw st (b : Widget.box) ~active =
  let sb = sheet_box st.geo b in
  (* inactive, nothing selected: a table on the page *)
  let selection = if active then Some (st.anchor, st.focus) else None in
  let cells = Gui.shapes (Sheet_view.draw ?selection st.geo (Gui.theme ()) sb st.sheet) in
  let editing =
    match st.typing with
    | Some text when active ->
        let cb = Sheet_view.cell_box st.geo sb st.focus in
        [ rectangle white (cb.w -. 4.) (cb.h -. 4.) |> move cb.x cb.y; words black (text ^ "|") |> move cb.x cb.y ]
    | _ -> []
  in
  cells @ editing

let command c st =
  match c with
  | "Clear" -> { st with sheet = List.fold_left (fun s cell -> Sheet.set cell "" s) st.sheet (Sheet_view.cells_of (st.anchor, st.focus)) }
  | _ -> st

let rec part st : Component.part =
  {
    kind;
    height = (fun _ -> snd (Sheet_view.size st.geo));
    (* so many cells of a size: a size of its own, to scale *)
    natural = Some (Sheet_view.size st.geo);
    draw = draw st;
    input = (fun computer b -> part (input computer b st));
    menu = [ "Sheet"; "Clear" ];
    command = (fun c -> part (command c st));
    save = (fun () -> Sheet.to_string st.sheet);
  }

let make ?(cols = 3) ?(rows = 5) sheet =
  part { sheet; geo = geometry ~cols ~rows; anchor = (0, 0); focus = (0, 0); typing = None; was = []; was_down = false }

let load s = make (Sheet.of_string s)
