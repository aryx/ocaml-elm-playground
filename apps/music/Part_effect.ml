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
open Basics (* float arithmetics *)

let natural = (880., 80.)

(* the panel's own coordinates: centred at (0, 0) *)
let knob_x (i : int) : number = -150. + (float_of_int i * 110.)

type state = {
  fx : Effect.t;
  values : (string * float) list; (* the knobs' positions: an effect only takes them *)
  bypass : bool ref;
  ui : Immediate.t;
}

let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let ui, values =
    List.fold_left
      (fun (ui, acc) (n, (k : Effect.knob)) ->
        let v = List.assoc k.name st.values in
        let ui, v' = Panel.control ui ~selector:Stepped_knob ~at:(knob_x n, 6.) k.control v in
        if v' <> v then st.fx.set k.name v';
        (ui, acc @ [ (k.name, v') ]))
      (ui, [])
      (List.mapi (fun n k -> (n, k)) st.fx.knobs)
  in
  let ui, on = Immediate.rocker ui { Widget.x = -250.; y = 6.; w = 20.; h = 40. } (not !(st.bypass)) in
  st.bypass := not on;
  { st with ui; values }

let draw ~name ~(color : color) (st : state) (b : Widget.box) ~active:_ : shape list =
  let w, h = natural in
  let at s = move b.x b.y s in
  List.map at
    ([ rectangle (rgb 45 45 50) w h; rectangle color w 4. |> move 0. ((h / 2.) - 2.); words color name |> scale 1.4 |> move (-360.) 6. ]
    @ [ words (rgb 200 200 200) (if !(st.bypass) then "BYPASS" else "ON") |> scale 0.8 |> move (-250.) (-26.) ]
    @ List.mapi (fun n (k : Effect.knob) -> words (rgb 200 200 200) (String.uppercase_ascii k.name) |> scale 0.8 |> move (knob_x n) (-28.)) st.fx.knobs)
  @ Panel.shapes st.ui ~dx:b.x ~dy:b.y

let rec part ~kind ~name ~color (st : state) : Component.part =
  {
    kind;
    height = (fun w -> snd natural * w / fst natural);
    natural = Some natural;
    draw = draw ~name ~color st;
    input = (fun computer b -> part ~kind ~name ~color (step_ui (Panel.input computer ~dx:b.x ~dy:b.y) st));
    menu = [];
    command = (fun _ -> part ~kind ~name ~color st);
    save = (fun () -> String.concat "\n" (List.map (fun (n, v) -> Printf.sprintf "%s = %g" n v) st.values));
  }

let make ~kind ~name ~(color : color) (fx : Effect.t) (bypass : bool ref) : Component.part =
  let theme = { Theme.default with dial = 26.; dial_face = rgb 25 25 28; pointer = color; face = rgb 70 70 75; text = rgb 220 220 220; accent = color } in
  let values = List.map (fun (k : Effect.knob) -> (k.name, k.initial)) fx.knobs in
  part ~kind ~name ~color (step_ui Panel.neutral { fx; values; bypass; ui = Immediate.set_theme theme Immediate.empty })
