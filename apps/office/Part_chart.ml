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

let kind = "chart"
let natural_w = 300.
let natural_h = 200.

let of_sheet sheet =
  let bar r =
    let raw = Sheet.raw sheet (1, r) in
    match Sheet.value sheet (1, r) with
    | Sheet.Number v when raw <> "" && raw.[0] <> '=' -> Some (Sheet.show (Sheet.value sheet (0, r)), v)
    | _ -> None
  in
  (* the first row with a bar, then on until a row without one *)
  let rec from r = if r > 100 then [] else match bar r with Some _ -> run r | None -> from (r + 1)
  and run r = match bar r with Some b -> b :: run (r + 1) | None -> [] in
  from 0

let draw bars (b : Widget.box) ~active:_ =
  let ink = rgb 40 40 50 in
  let n = List.length bars in
  let top = List.fold_left (fun m (_, v) -> Float.max m v) 0. bars in
  (* the bars stand on a base line, with room below it for the labels
     and above them for the numbers *)
  let base = Widget.bottom b +. 30. and room = b.h -. 55. in
  let slot = (b.w -. 20.) /. float_of_int (max 1 n) in
  let bar i (label, v) =
    let h = if top > 0. then Float.max 0. (v /. top *. room) else 0. in
    let x = Widget.left b +. 10. +. (slot *. (float_of_int i +. 0.5)) in
    [
      rectangle (rgb 70 110 190) (slot *. 0.6) h |> move x (base +. (h /. 2.));
      words ink label |> scale 0.8 |> move x (base -. 14.);
      words ink (Sheet.show (Sheet.Number v)) |> scale 0.8 |> move x (base +. h +. 12.);
    ]
  in
  (rectangle white b.w b.h |> move b.x b.y)
  :: (rectangle ink (b.w -. 20.) 2. |> move b.x base)
  :: List.concat (List.mapi bar bars)
  @ Gui.shapes (Widget.frame (rgb 120 120 120) 1. b)

let rec part bars : Component.part =
  {
    kind;
    height = (fun w -> w *. natural_h /. natural_w);
    natural = Some (natural_w, natural_h);
    draw = draw bars;
    input = (fun _ _ -> part bars);
    menu = [];
    command = (fun _ -> part bars);
    (* a bar a line: its label, a tab, its number *)
    save = (fun () -> String.concat "\n" (List.map (fun (l, v) -> Printf.sprintf "%s\t%g" l v) bars));
  }

let make bars = part bars

let load s =
  make
    (List.filter_map
       (fun line -> match String.split_on_char '\t' line with [ l; v ] -> Option.map (fun v -> (l, v)) (float_of_string_opt v) | _ -> None)
       (String.split_on_char '\n' s))
