(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 7GUIs, task 5: CRUD (docs/claude_notes/plans/plan_gui_remaining.md).
 *
 * Create, Read, Update, Delete: a list of names, a filter on it, two
 * fields, and three buttons. The task is about **a selection in a
 * list that changes under it**: filter the list, and the row selected
 * is a different person; delete, and the rows below it move up. 7GUIs'
 * point is that the selection must be kept as *which person*, not
 * which row -- here, a person's index in the whole list, from which the
 * row in the filtered one is worked out every frame.
 *
 *   Filter prefix: [   ]
 *   +----------------+   Name:    [      ]
 *   | Emil, Hans     |   Surname: [      ]
 *   | Mustermann, Max|
 *   | Tisch, Roman   |
 *   +----------------+
 *   [Create] [Update] [Delete]
 *
 * Update and Delete are on only while someone is selected. What it
 * uses: the playground's Gui, with its list box (Gui.list_in), the one
 * widget the other tasks did not need.
 *
 * Exercises: a list longer than its box, scrolled with the wheel
 * (mouse.wheel); the four-way versions (examples/gui4), where the
 * selection is the interesting part -- in the retained two, the list
 * widget holds a row, and the program has to translate it back to a
 * person after every filter.
 *)
open Playground

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type person = { name : string; surname : string }

type model = {
  people : person list;
  filter : string;
  (* who is selected: a person's place in [people], not a row *)
  selected : int option;
  name : string;
  surname : string;
}

let initial =
  {
    people = [ { name = "Hans"; surname = "Emil" }; { name = "Max"; surname = "Mustermann" }; { name = "Roman"; surname = "Tisch" } ];
    filter = "";
    selected = None;
    name = "";
    surname = "";
  }

let shown (p : person) = p.surname ^ ", " ^ p.name

let starts_with prefix s =
  String.length s >= String.length prefix
  && String.lowercase_ascii (String.sub s 0 (String.length prefix)) = String.lowercase_ascii prefix

(* the people the filter lets through, each with its place in the
   whole list *)
let visible (m : model) = List.filter (fun (_, (p : person)) -> starts_with m.filter p.surname) (List.mapi (fun i p -> (i, p)) m.people)

type slot = Filter_label | Filter | List | Name_label | Name | Surname_label | Surname | Create | Update | Delete

let panel =
  Layout.(
    center
      (column ~gap:14.
         [
           row ~gap:10. [ leaf Filter_label (Gui.label_size "Filter prefix:"); leaf Filter (Gui.field_size ()) ];
           row ~gap:20.
             [
               leaf List (Gui.list_size ());
               column ~gap:10.
                 [
                   row ~gap:10. [ leaf Name_label (Gui.label_size "Surname:"); leaf Name (Gui.field_size ()) ];
                   row ~gap:10. [ leaf Surname_label (Gui.label_size "Surname:"); leaf Surname (Gui.field_size ()) ];
                 ];
             ];
           row ~gap:10.
             [ leaf Create (Gui.button_size "Create"); leaf Update (Gui.button_size "Update"); leaf Delete (Gui.button_size "Delete") ];
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update computer (model : model) =
  let at = places computer in
  let box slot = List.assoc slot at in
  Gui.label_in computer (box Filter_label) "Filter prefix:";
  let model = { model with filter = Gui.field_in computer (box Filter) model.filter } in
  (* the selection shown as a row of what the filter lets through --
     and none, if the person selected is filtered out *)
  let rows = visible model in
  let row_of i =
    let rec go k = function [] -> None | (j, _) :: rest -> if j = i then Some k else go (k + 1) rest in
    go 0 rows
  in
  let row = Option.bind model.selected row_of in
  let clicked = Gui.list_in computer (box List) (List.map (fun (_, p) -> shown p) rows) row in
  let model =
    if clicked <> row then
      match Option.bind clicked (List.nth_opt rows) with
      (* choosing someone puts their name in the fields, to be changed *)
      | Some (i, (p : person)) -> { model with selected = Some i; name = p.name; surname = p.surname }
      | None -> model
    else model
  in
  Gui.label_in computer (box Name_label) "Name:";
  Gui.label_in computer (box Surname_label) "Surname:";
  let name = Gui.field_in computer (box Name) model.name in
  let surname = Gui.field_in computer (box Surname) model.surname in
  let model = { model with name; surname } in
  let someone = model.selected <> None && row <> None in
  let person : person = { name; surname } in
  if Gui.button_in computer (box Create) "Create" then { model with people = model.people @ [ person ] }
  else if Gui.button_in ~enabled:someone computer (box Update) "Update" then
    { model with people = List.mapi (fun i p -> if Some i = model.selected then person else p) model.people }
  else if Gui.button_in ~enabled:someone computer (box Delete) "Delete" then
    (* the person goes, and with them the selection: the row under it
       is someone else now *)
    { model with people = List.filteri (fun i _ -> Some i <> model.selected) model.people; selected = None }
  else model

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let view computer model =
  let s = computer.screen in
  (rectangle (Gui.theme ()).background s.width s.height :: Gui.draw ())
  @ [
      words black "7GUIs 5: CRUD" |> move_y 260.;
      words (rgb 120 120 120)
        (Printf.sprintf "%d people, %d shown -- type a prefix to filter by surname" (List.length model.people)
           (List.length (visible model)))
      |> move_y (-240.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
