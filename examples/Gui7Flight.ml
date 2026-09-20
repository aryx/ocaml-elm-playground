(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 7GUIs, task 3: Flight Booker
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 3).
 *
 * The task where an interface stops being a list of widgets and
 * becomes a set of *rules between them*:
 *
 *   - a dropdown: one-way, or return;
 *   - two date fields, the second turned off while it is one-way;
 *   - a Book button, turned off unless the dates make sense -- both
 *     readable as dates, and, for a return, the second not before the
 *     first;
 *   - a field that cannot be read shows it (here: its text greyed).
 *
 * What it costs in immediate mode is the interesting number: the
 * rules are *four lines of ordinary code* in update, because every
 * widget is asked for every frame and its enabled-ness is just an
 * argument. With callbacks, each rule must be re-checked in each
 * handler that can break it, and remembering all of them is the whole
 * difficulty of the task -- which is why 7GUIs includes it, and what
 * phase 4 will measure by writing this same program three more ways.
 *
 * Dates are 7GUIs' own format, D.M.Y (27.3.2014), parsed by hand:
 * that is the task's, not the toolkit's.
 *
 * Note the order the widgets are asked for: the menu is asked for
 * last, because in immediate mode a popup is painted where it is
 * asked for and anything later would paint over it.
 *)
open Playground

type model = { kind : int (* 0 one-way, 1 return *); out : string; back : string; booked : string option }

let initial = { kind = 0; out = "27.3.2014"; back = "27.3.2014"; booked = None }
let kinds = [ "one-way flight"; "return flight" ]

(* a date as (year, month, day), so that comparing them is comparing
   tuples; None if it is not one *)
let date s =
  match String.split_on_char '.' s with
  | [ d; m; y ] -> (
      match (int_of_string_opt d, int_of_string_opt m, int_of_string_opt y) with
      | Some d, Some m, Some y when d >= 1 && d <= 31 && m >= 1 && m <= 12 -> Some (y, m, d)
      | _ -> None)
  | _ -> None

type slot = Kind | Out | Back | Book | Said

let panel =
  Layout.(
    center
      (column ~gap:12.
         [
           stretch (leaf Kind (Gui.menu_size kinds));
           stretch (leaf Out (Gui.field_size ()));
           stretch (leaf Back (Gui.field_size ()));
           stretch (leaf Book (Gui.button_size "Book"));
           space 20.;
           leaf Said (Gui.label_size "You have booked a return flight on 27.3.2014.");
         ]))

let places computer = Layout.arrange (Gui.area computer) panel

let update computer model =
  let at = places computer in
  let box slot = List.assoc slot at in
  let return = model.kind = 1 in
  (* the rules, all of them *)
  let out = Gui.field_in computer (box Out) model.out in
  let back = Gui.field_in ~enabled:return computer (box Back) model.back in
  let bookable =
    match (date out, date back) with
    | Some _, _ when not return -> true
    | Some a, Some b -> b >= a
    | _ -> false
  in
  let booking = Gui.button_in ~enabled:bookable computer (box Book) "Book" in
  (match model.booked with Some said -> Gui.label_in computer (box Said) said | None -> ());
  (* asked for last: its items are painted over everything above *)
  let kind = Gui.menu_in computer (box Kind) kinds model.kind in
  {
    kind;
    out;
    back;
    booked =
      (if booking then
         Some
           (Printf.sprintf "You have booked a %s on %s."
              (if return then "return flight" else "one-way flight")
              (if return then back else out))
       else model.booked);
  }

let view computer _model =
  let s = computer.screen in
  (rectangle (Gui.theme ()).background s.width s.height :: Gui.draw ())
  @ [
      words black "7GUIs 3: Flight Booker" |> move_y 220.;
      words (rgb 120 120 120) "dates are D.M.Y; Book turns off when they do not make sense"
      |> move_y (-200.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
