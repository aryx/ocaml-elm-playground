(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* 7GUIs, task 2: Temperature Converter
 * (docs/claude_notes/plans/plan_gui_teaching.md, phase 3).
 *
 * Two fields, each the other's answer: type Celsius and Fahrenheit
 * follows, type Fahrenheit and Celsius does. The task exists to ask
 * one question -- what happens to a *bidirectional* dependency while
 * the person is halfway through typing it?
 *
 *     C [ 2      ]  -->  F [ 35.6 ]      "2" is a number: convert
 *     C [ 2.     ]  -->  F [ 35.6 ]      "2." is not: leave F alone
 *     C [ -      ]  -->  F [ 35.6 ]      nor is "-", and it must still
 *                                        be typeable
 *
 * The answer, and the thing worth taking away: **the model holds the
 * two strings, not two numbers.** A number cannot represent "2." or
 * "-" or "", which are all things a person types on the way to a
 * number, and a model that cannot represent them must either refuse
 * the keystroke or throw away what was typed. So the text is the
 * truth, the conversion is a function of it, and a field that does
 * not parse simply leaves the other one as it was.
 *
 * The second half is knowing *which* field the person edited: both
 * are asked for every frame, so the answer is whichever came back
 * changed (a field returns its text unchanged unless it has the keys
 * and something was typed). One comparison each, and no message type.
 *)
open Playground

type model = { c : string; f : string }

let initial = { c = "20"; f = "68" }

type slot = C_label | C | F_label | F

let panel =
  Layout.(
    center
      (column ~gap:12.
         [
           row ~gap:10. [ leaf C_label (Gui.label_size "Celsius"); leaf C (Gui.field_size ()) ];
           row ~gap:10. [ leaf F_label (Gui.label_size "Fahrenheit"); leaf F (Gui.field_size ()) ];
         ]))

let places computer = Layout.arrange (Gui.area computer) panel
let show v = Printf.sprintf "%.1f" v

let update computer model =
  let at = places computer in
  Gui.label_in computer (List.assoc C_label at) "Celsius";
  Gui.label_in computer (List.assoc F_label at) "Fahrenheit";
  let c = Gui.field_in computer (List.assoc C at) model.c in
  let f = Gui.field_in computer (List.assoc F at) model.f in
  if c <> model.c then
    (* Celsius was typed into: convert it if it is a number, and if it
       is not, keep what was typed and leave Fahrenheit alone *)
    match float_of_string_opt c with
    | Some v -> { c; f = show ((v *. 9. /. 5.) +. 32.) }
    | None -> { model with c }
  else if f <> model.f then
    match float_of_string_opt f with
    | Some v -> { f; c = show ((v -. 32.) *. 5. /. 9.) }
    | None -> { model with f }
  else model

let view computer _model =
  let s = computer.screen in
  (rectangle (Gui.theme ()).background s.width s.height :: Gui.draw ())
  @ [
      words black "7GUIs 2: Temperature Converter" |> move_y 200.;
      words (rgb 120 120 120) "click a field, or Tab between them" |> move_y (-160.);
    ]

let app = game view update initial
let main = Playground_platform.run_app app
