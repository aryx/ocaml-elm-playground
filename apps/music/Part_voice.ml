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

type 'p voice = {
  knobs : 'p Patch_text.knob list;
  presets : (string * 'p) list;
  patch : unit -> 'p;
  set_patch : 'p -> unit;
  to_string : 'p -> string;
  of_string : string -> ('p, string) result;
}

let natural = (960., 290.)

(* the grid's own coordinates are TinyReface's screen's, centred at
 * (0, 235): a box elsewhere moves it there *)
let centre_y = 235.
let offset (b : Widget.box) : number * number = (b.x, b.y - centre_y)

(* the controls in two rows of up to nine *)
let control_at (i : int) : number * number = (-400. + (float_of_int (i mod 9) * 100.), if i < 9 then 300. else 170.)

type 'p state = { voice : 'p voice; controls : (string * string) list; find : string -> 'p Patch_text.knob; ui : Immediate.t }

let step_ui (i : Widget.input) (st : 'p state) : 'p state =
  let ui = Immediate.frame i st.ui in
  let ui =
    List.fold_left
      (fun (ui, n) (_, name) ->
        let k = st.find name in
        let p = st.voice.patch () in
        let v = k.get p in
        let ui, v' = Panel.control ui ~selector:Stepped_knob ~at:(control_at n) k.control v in
        if v' <> v then st.voice.set_patch (k.put p v');
        (ui, n +.. 1))
      (ui, 0) st.controls
    |> fst
  in
  { st with ui }

let ink = rgb 30 30 30

let draw (st : 'p state) (b : Widget.box) ~active:_ : shape list =
  let dx, dy = offset b in
  let labels =
    List.mapi
      (fun i (label, name) ->
        let x, y = control_at i in
        let k = st.find name in
        let value =
          match k.control with
          | Selector ls -> [ words ink (List.nth ls (int_of_float (k.get (st.voice.patch ())))) |> scale 0.9 |> move x (y - 58.) ]
          | _ -> []
        in
        (words ink label |> scale 1. |> move x (y - 44.)) :: value)
      st.controls
  in
  List.map (move dx dy) (List.concat labels) @ Panel.shapes st.ui ~dx ~dy

let rec part ~kind (st : 'p state) : Component.part =
  {
    kind;
    height = (fun w -> snd natural * w / fst natural);
    natural = Some natural;
    draw = draw st;
    input =
      (fun computer b ->
        let dx, dy = offset b in
        part ~kind (step_ui (Panel.input computer ~dx ~dy) st));
    menu = "Preset" :: List.map fst st.voice.presets;
    command =
      (fun c ->
        Option.iter st.voice.set_patch (List.assoc_opt c st.voice.presets);
        part ~kind st);
    save = (fun () -> st.voice.to_string (st.voice.patch ()));
  }

let make ~kind ~(color : color) (voice : 'p voice) (controls : (string * string) list) : Component.part =
  let find n =
    match List.find_opt (fun (k : 'p Patch_text.knob) -> k.name = n) voice.knobs with
    | Some k -> k
    | None -> failwith ("Part_voice: no knob " ^ n)
  in
  List.iter (fun (_, n) -> ignore (find n)) controls;
  let theme = { Theme.default with dial = 32.; dial_face = rgb 35 35 38; pointer = color; face = rgb 225 225 222; text = rgb 30 30 30 } in
  part ~kind (step_ui Panel.neutral { voice; controls; find; ui = Immediate.set_theme theme Immediate.empty })
