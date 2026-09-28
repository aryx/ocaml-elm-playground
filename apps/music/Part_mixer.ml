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

let natural = (880., 240.)

(* the panel's own coordinates: centred at (0, 0); strip k's x, the
 * master's the fifteenth *)
let strip_x (k : int) : number = -400. + (float_of_int k * 54.)
let master = Rack_mixer.channels
let fader_bottom = -105.
let fader_track = 70.
let name (k : int) (field : string) : string = Printf.sprintf "ch%d.%s" (k +.. 1) field

type state = {
  mixer : Rack_device.t;
  ui : Immediate.t;
  sliding : int option; (* the fader held *)
  was_down : bool;
}

let fader_at (x : number) (y : number) : int option =
  List.find_opt
    (fun k -> Float.abs (x - strip_x k) <= 10. && y >= fader_bottom - 8. && y <= fader_bottom + fader_track + 8.)
    (List.init (Rack_mixer.channels +.. 1) (fun k -> k))

let level_name (k : int) : string = if k = master then "master" else name k "level"

let step_ui (i : Widget.input) (st : state) : state =
  let ui = Immediate.frame i st.ui in
  let d = st.mixer in
  let size = Immediate.knob_size (Immediate.theme ui) in
  let knob ui k field ~from ~to_ y =
    let v = d.get (name k field) in
    let ui, v' = Immediate.knob ui { Widget.x = strip_x k; y; w = fst size; h = snd size } ~from ~to_ v in
    if v' <> v then d.set (name k field) v';
    ui
  in
  let ui =
    List.fold_left (fun ui k -> knob (knob ui k "aux" ~from:0. ~to_:1. 62.) k "pan" ~from:(-1.) ~to_:1. 18.) ui (List.init Rack_mixer.channels (fun k -> k))
  in
  (* a mute clicked *)
  if i.mclick then
    List.iter
      (fun k -> if Float.abs (i.mx - strip_x k) <= 14. && Float.abs (i.my - (-18.)) <= 9. then d.set (name k "mute") (1. - d.get (name k "mute")))
      (List.init Rack_mixer.channels (fun k -> k));
  (* a fader pressed follows the mouse until let go *)
  let sliding = if not i.mdown then None else match st.sliding with Some k -> Some k | None -> if st.was_down then None else fader_at i.mx i.my in
  Option.iter (fun k -> d.set (level_name k) (Float.max 0. (Float.min 1. ((i.my - fader_bottom) / fader_track)))) sliding;
  { st with ui; sliding; was_down = i.mdown }

let draw (st : state) (b : Widget.box) ~active:_ : shape list =
  let d = st.mixer in
  let w, h = natural in
  let strip k =
    let x = strip_x k and level = d.get (level_name k) in
    let y = fader_bottom + (level * fader_track) and meter = Float.min 1. (d.get (if k = master then "master.peak" else name k "peak")) * fader_track in
    let muted = k < master && d.get (name k "mute") >= 0.5 in
    [
      words (rgb 220 220 220) (if k = master then "MASTER" else string_of_int (k +.. 1)) |> scale 1. |> move x 100.;
      rectangle (rgb 15 15 15) 5. fader_track |> move x (fader_bottom + (fader_track / 2.));
      rectangle (rgb 40 60 40) 4. fader_track |> move (x + 12.) (fader_bottom + (fader_track / 2.));
      rectangle (if meter > 0.9 * fader_track then rgb 240 70 50 else rgb 90 220 110) 4. meter |> move (x + 12.) (fader_bottom + (meter / 2.));
      rectangle (rgb 225 225 220) 22. 10. |> move x y;
    ]
    @
    if k < master then
      [ rectangle (if muted then rgb 240 150 40 else rgb 80 80 85) 26. 14. |> move x (-18.); words (rgb 20 20 20) "M" |> scale 0.8 |> move x (-18.) ]
    else []
  in
  List.map (move b.x b.y)
    ([ rectangle (rgb 60 62 68) w h; words (rgb 220 220 220) "MIXER" |> scale 1.1 |> move (strip_x master) 40.; words (rgb 220 220 220) "14:2" |> scale 1.1 |> move (strip_x master) 20.; words (rgb 160 160 160) "AUX" |> scale 0.8 |> move (-430.) 62.; words (rgb 160 160 160) "PAN" |> scale 0.8 |> move (-430.) 18. ]
    @ List.concat_map strip (List.init (Rack_mixer.channels +.. 1) (fun k -> k)))
  @ Panel.shapes st.ui ~dx:b.x ~dy:b.y

let rec part (st : state) : Component.part =
  {
    kind = "mixer";
    height = (fun w -> snd natural * w / fst natural);
    natural = Some natural;
    draw = draw st;
    input = (fun computer b -> part (step_ui (Panel.input computer ~dx:b.x ~dy:b.y) st));
    menu = [];
    command = (fun _ -> part st);
    save =
      (fun () ->
        String.concat "\n"
          (List.concat_map (fun k -> List.map (fun f -> Printf.sprintf "%s = %g" (name k f) (st.mixer.get (name k f))) [ "level"; "pan"; "aux"; "mute" ]) (List.init Rack_mixer.channels (fun k -> k))
          @ [ Printf.sprintf "master = %g" (st.mixer.get "master") ]));
  }

let make (mixer : Rack_device.t) : Component.part =
  let theme = { Theme.default with dial = 18.; dial_face = rgb 25 25 28; pointer = rgb 240 240 240; face = rgb 70 70 75 } in
  part (step_ui Panel.neutral { mixer; ui = Immediate.set_theme theme Immediate.empty; sliding = None; was_down = false })
