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

(* the panel's own coordinates: centred at (0, 0) *)
let step_x (k : int) : number = -330. + (float_of_int k * 44.)
let lowest = 36 (* C2 *)
let rows = 24
let row_h = 5.
let grid_bottom = -5.
let gate_bottom = -100.
let gate_h = 60.
let tie_y = -22.
let get (d : Rack_device.t) (k : int) (field : string) : float = d.get (Printf.sprintf "step%d.%s" (k +.. 1) field)
let set (d : Rack_device.t) (k : int) (field : string) (v : float) : unit = d.set (Printf.sprintf "step%d.%s" (k +.. 1) field) v

let step_at (x : number) : int option = List.find_opt (fun k -> Float.abs (x - step_x k) <= 20.) (List.init Rack_matrix.steps (fun k -> k))

(* a click: a note, a gate or a tie *)
let input (d : Rack_device.t) (i : Widget.input) : unit =
  if i.mclick then
    match step_at i.mx with
    | None -> ()
    | Some k ->
        if i.my >= grid_bottom && i.my < grid_bottom + (float_of_int rows * row_h) then
          set d k "note" (float_of_int (lowest +.. int_of_float ((i.my - grid_bottom) / row_h)))
        else if i.my >= gate_bottom - 4. && i.my <= gate_bottom + gate_h then begin
          let g = Float.max 0. (Float.min 1. ((i.my - gate_bottom) / gate_h)) in
          set d k "gate" (if g < 0.1 then 0. else g)
        end
        else if Float.abs (i.my - tie_y) <= 8. then set d k "tie" (1. - get d k "tie")

let draw (d : Rack_device.t) (b : Widget.box) ~active:_ : shape list =
  let w, h = natural in
  let playing = d.step () in
  let column k =
    let x = step_x k and note = int_of_float (get d k "note") and gate = get d k "gate" in
    let lit = playing = Some k in
    let row = note -.. lowest in
    [ rectangle (if lit then rgb 70 70 50 else rgb 30 30 34) 40. (float_of_int rows * row_h) |> move x (grid_bottom + (float_of_int rows * row_h / 2.)) ]
    @ (if row >= 0 && row < rows && gate > 0. then [ rectangle (rgb 250 200 60) 38. row_h |> move x (grid_bottom + ((float_of_int row + 0.5) * row_h)) ] else [])
    @ [
        rectangle (rgb 30 30 34) 40. gate_h |> move x (gate_bottom + (gate_h / 2.));
        rectangle (rgb 230 90 60) 40. (gate * gate_h) |> move x (gate_bottom + (gate * gate_h / 2.));
        rectangle (if get d k "tie" >= 0.5 then rgb 250 200 60 else rgb 80 80 85) 30. 10. |> move x tie_y;
        words (if lit then rgb 250 200 60 else rgb 200 200 200) (string_of_int (k +.. 1)) |> scale 0.8 |> move x 112.;
      ]
  in
  List.map (move b.x b.y)
    ([ rectangle (rgb 55 55 60) w h; words (rgb 250 200 60) "MATRIX" |> scale 1.3 |> move (-395.) 90.; words (rgb 180 180 180) "Pattern Sequencer" |> scale 0.8 |> move (-395.) 70. ]
    @ [ words (rgb 180 180 180) "C2" |> scale 0.8 |> move (-370.) grid_bottom; words (rgb 180 180 180) "C4" |> scale 0.8 |> move (-370.) (grid_bottom + 120.) ]
    @ [ words (rgb 180 180 180) "GATE" |> scale 0.8 |> move (-375.) (gate_bottom + 30.); words (rgb 180 180 180) "TIE" |> scale 0.8 |> move (-375.) tie_y ]
    @ List.concat_map column (List.init Rack_matrix.steps (fun k -> k)))

let rec part (d : Rack_device.t) : Component.part =
  {
    kind = "matrix";
    height = (fun w -> snd natural * w / fst natural);
    natural = Some natural;
    draw = draw d;
    input =
      (fun computer b ->
        input d (Panel.input computer ~dx:b.x ~dy:b.y);
        part d);
    menu = [];
    command = (fun _ -> part d);
    save =
      (fun () ->
        String.concat "\n"
          (List.concat_map (fun k -> List.map (fun f -> Printf.sprintf "step%d.%s = %g" (k +.. 1) f (get d k f)) [ "note"; "gate"; "tie"; "curve" ]) (List.init Rack_matrix.steps (fun k -> k))));
  }

let make (d : Rack_device.t) : Component.part = part d
