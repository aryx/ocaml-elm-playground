(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Draw a digit with the mouse and a network says which it is
 * (Net.mli, Backprop.mli, Train.mli, notes_ai_learning.md
 * section 7). 256 inputs (a 16 by 16 square of ink), 64 hidden, ten
 * outputs -- about 17,000 numbers -- trained while you watch, a few
 * dozen examples a frame.
 *
 * **No dataset is downloaded.** The training digits are drawn by our
 * own vector font (graphics/font, Hershey 1967) at random sizes,
 * slants, shifts, thicknesses and with a little noise: the renderer
 * teaching the network, which is a pleasing loop and makes the whole
 * thing self-contained.
 *
 * It also builds in the honest failure. It learns *this font's*
 * digits, so it does well on the held-out ones (which are the same
 * font, differently shaken) and worse on yours, especially if you
 * write a 7 with a bar or a 1 with a foot. That gap is what "the
 * training distribution" means, concretely, and no amount of training
 * closes it -- only different training data would.
 *
 * What there is to see:
 *
 *  - the accuracy climbing on digits it has never been trained on,
 *    while the loss falls;
 *  - the ten outputs as bars, so a wrong answer shows what it was
 *    torn between -- 3 and 8, 4 and 9, which are the pairs a human
 *    confuses too at this resolution;
 *  - what it is training on: "n" puts a fresh random training digit
 *    in the square, so you can see how shaken they are.
 *
 * Keys: draw with the mouse, "c" clear, "n" a training digit, space
 * pause training, "r" start over with new weights.
 *
 * What it uses: Net, Backprop, Train (the loop), graphics/font
 * (the digits it trains on), Scene2d (the keys). *)
open Playground

(*****************************************************************************)
(* A digit, as 16 by 16 squares of ink *)
(*****************************************************************************)

let side = 16
let cells = side * side

(* the ink at every cell of the square, from a glyph's pen strokes:
 * a cell is inked by how near it is to a stroke, which gives a soft
 * edge instead of a jagged one (the same idea as an anti-aliased
 * line, and it matters here: a network fed hard edges learns the
 * jaggedness) *)
let ink_of (strokes : (float * float) list list) ~(thick : float) : float array =
  let ink = Array.make cells 0. in
  let step = 2. /. float_of_int side in
  List.iter
    (fun points ->
      let rec segments = function a :: (b :: _ as rest) -> (a, b) :: segments rest | _ -> [] in
      List.iter
        (fun ((x1, y1), (x2, y2)) ->
          for i = 0 to side - 1 do
            for j = 0 to side - 1 do
              let px = -1. +. (step *. (float_of_int i +. 0.5)) and py = -1. +. (step *. (float_of_int j +. 0.5)) in
              (* the distance from the cell to the segment *)
              let dx = x2 -. x1 and dy = y2 -. y1 in
              let len2 = (dx *. dx) +. (dy *. dy) in
              let t = if len2 < 1e-9 then 0. else Float.max 0. (Float.min 1. ((((px -. x1) *. dx) +. ((py -. y1) *. dy)) /. len2)) in
              let d = Float.hypot (px -. (x1 +. (t *. dx))) (py -. (y1 +. (t *. dy))) in
              let v = Float.max 0. (1. -. (d /. thick)) in
              if v > ink.((i * side) + j) then ink.((i * side) + j) <- v
            done
          done)
        (segments points))
    strokes;
  ink

(* one training digit: the glyph, shaken -- scaled, slanted, moved,
 * thickened or thinned, and speckled. Every one of those is a thing
 * your handwriting does and the font does not. *)
let drawn_digit (st : Random.State.t) (d : int) : float array =
  let g = Hershey.glyph (Char.chr (Char.code '0' + d)) in
  let scale = 0.55 +. Random.State.float st 0.25 in
  let slant = (Random.State.float st 0.5) -. 0.25 in
  let ox = (Random.State.float st 0.3) -. 0.15 and oy = (Random.State.float st 0.3) -. 0.15 in
  let thick = 0.1 +. Random.State.float st 0.09 in
  let turn = (Random.State.float st 0.3) -. 0.15 in
  let strokes =
    List.map
      (List.map (fun (x, y) ->
           (* the font's units: y goes down, the baseline at 9, the top
              of a digit at -12, so the middle is about -1.5 *)
           let fx = float_of_int x /. 9. and fy = (float_of_int y +. 1.5) /. 10.5 in
           let fy = -.fy in
           let x = (fx +. (slant *. fy)) *. scale and y = fy *. scale in
           ((x *. cos turn) -. (y *. sin turn) +. ox, (x *. sin turn) +. (y *. cos turn) +. oy)))
      g.strokes
  in
  let ink = ink_of strokes ~thick in
  Array.map (fun v -> Float.max 0. (Float.min 1. (v +. ((Random.State.float st 0.24) -. 0.12)))) ink

let digits (seed : int) (per_digit : int) : Backprop.example list =
  let st = Random.State.make [| seed |] in
  List.concat_map (fun d -> List.init per_digit (fun _ -> (drawn_digit st d, Train.one_hot 10 d))) (List.init 10 (fun d -> d))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let hidden = 64
let batch = 16
let batches_a_frame = 4

type model = {
  net : Net.t;
  training : Backprop.example list;
  held : Backprop.example list;
  drawn : float array; (* what is in the square: yours, or a training digit *)
  seen : int; (* training examples shown *)
  accuracy : float; (* on the held-out digits *)
  loss : float;
  training_on : bool;
  seed : int;
  shown_label : string; (* what "n" put there, if anything *)
}

let fresh (seed : int) : model =
  let all = digits seed 40 in
  let (training, held) = Train.split ~part:0.2 all in
  { net = Net.make ~seed ~hidden:Net.Relu ~last:Net.Sigmoid [ cells; hidden; 10 ];
    training; held; drawn = Array.make cells 0.; seen = 0; accuracy = 0.; loss = 1.;
    training_on = true; seed; shown_label = "" }

let initial_model : model = fresh 1

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let cell_of ((x, y) : number * number) (left : number) (bottom : number) (box : number) : (int * int) option =
  let step = box /. float_of_int side in
  let i = int_of_float (Float.floor ((x -. left) /. step)) and j = int_of_float (Float.floor ((y -. bottom) /. step)) in
  if i >= 0 && i < side && j >= 0 && j < side then Some (i, j) else None

let left = -420.
let bottom = -200.
let box = 400.

(* the mouse leaves ink in the cell it is over and a little in the
 * neighbours, so a drawn stroke is as soft as a trained one *)
let paint (m : model) (computer : computer) : model =
  if not computer.mouse.mdown then m
  else
    match cell_of (computer.mouse.mx, computer.mouse.my) left bottom box with
    | None -> m
    | Some (i, j) ->
        let drawn = Array.copy m.drawn in
        List.iter
          (fun (di, dj, v) ->
            let i = i + di and j = j + dj in
            if i >= 0 && i < side && j >= 0 && j < side then
              drawn.((i * side) + j) <- Float.max drawn.((i * side) + j) v)
          [ (0, 0, 1.); (1, 0, 0.5); (-1, 0, 0.5); (0, 1, 0.5); (0, -1, 0.5) ];
        { m with drawn; shown_label = "" }

let train_a_frame (m : model) : model =
  if not m.training_on then m
  else
    let n = List.length m.training in
    let rec go net seen k =
      if k = 0 then (net, seen)
      else
        let b = List.init batch (fun i -> List.nth m.training ((((seen + i) * 13) + 7) mod n)) in
        go (Backprop.step ~rate:0.15 net (Backprop.over net b)) (seen + batch) (k - 1)
    in
    let (net, seen) = go m.net m.seen batches_a_frame in
    let m = { m with net; seen } in
    (* the honest score, on digits it is never trained on -- and only
       every so often, since it costs a forward pass each *)
    if m.seen mod (batch * batches_a_frame * 10) = 0 then
      { m with
        accuracy = Train.accuracy net m.held ~answer:Train.best;
        loss = Backprop.loss net m.held }
    else m

let update (computer : computer) (s : model Scene2d.t) : model Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let m = if key "c" then { m with drawn = Array.make cells 0.; shown_label = "" } else m in
  let m = if key "r" then fresh (m.seed + 1) else m in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with training_on = not m.training_on } else m in
  let m =
    if key "n" then
      let d = (m.seen / 97) mod 10 in
      let st = Random.State.make [| m.seen + 1 |] in
      { m with drawn = drawn_digit st d; shown_label = Printf.sprintf "a training %d" d }
    else m
  in
  let m = paint m computer in
  { scenes with scene = train_a_frame m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let square (m : model) : shape list =
  let step = box /. float_of_int side in
  List.concat_map
    (fun i ->
      List.map
        (fun j ->
          let v = m.drawn.((i * side) + j) in
          let g = int_of_float (30. +. (v *. 210.)) in
          rectangle (rgb g g (int_of_float (40. +. (v *. 200.)))) (step -. 1.) (step -. 1.)
          |> move (left +. (step *. (float_of_int i +. 0.5))) (bottom +. (step *. (float_of_int j +. 0.5))))
        (List.init side (fun j -> j)))
    (List.init side (fun i -> i))

(* the ten outputs, as bars: a wrong answer shows what it was torn
 * between *)
let bars (m : model) : shape list =
  let out = Net.forward m.net m.drawn in
  let said = Train.best out in
  let anything = Array.exists (fun v -> v > 0.05) m.drawn in
  List.concat
    (List.init 10 (fun d ->
         let y = 180. -. (float_of_int d *. 44.) in
         let v = out.(d) in
         let lit = anything && d = said in
         [ text (if lit then rgb 240 200 90 else rgb 150 155 175) 1.6 (string_of_int d) |> move 100. y;
           rectangle (if lit then rgb 240 200 90 else rgb 90 100 130) (Float.max 2. (v *. 260.)) 20.
           |> fade 0.85
           |> move (150. +. (Float.max 2. (v *. 260.) /. 2.)) y ]))

let view (computer : computer) (s : model Scene2d.t) : shape list =
  let m = s.scene and screen = computer.screen in
  let out = Net.forward m.net m.drawn in
  let anything = Array.exists (fun v -> v > 0.05) m.drawn in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ square m @ bars m
  @ [ text white 2.2 "A NETWORK READING DIGITS" |> move_y 440.;
      text (rgb 150 155 175) 1.4
        (Printf.sprintf "%d-%d-10, %d weights   %d examples shown" cells hidden (Net.weights m.net) m.seen)
      |> move_y 390.;
      text (rgb 140 220 160) 1.6
        (Printf.sprintf "%.0f%% right on digits it has never seen" (100. *. m.accuracy))
      |> move_y 330.;
      (if anything then
         text (rgb 240 200 90) 3.5 (Printf.sprintf "%d" (Train.best out)) |> move 330. 255.
       else group []);
      text (rgb 200 205 220) 1.4 m.shown_label |> move (-220.) 250.;
      text (rgb 150 155 175) 1.3 "draw with the mouse    c: clear    n: a training digit" |> move_y (-330.);
      text (rgb 150 155 175) 1.3 "space: pause training    r: new weights" |> move_y (-370.);
      text (rgb 120 125 145) 1.2 "it learns this font's digits, and is worse at yours: that is what a training set is"
      |> move_y (-430.) ]

let app = game view update (Scene2d.start initial_model)
let main = Playground_platform.run_app app
