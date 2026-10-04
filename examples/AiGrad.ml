(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Automatic differentiation, a node at a time (Grad.mli,
 * notes_ai_learning.md section 5). One neuron,
 *
 *     o = tanh (x1 w1 + x2 w2 + b)
 *
 * drawn as the graph Grad builds of it: a box per value, an arrow
 * from what it was made of. It is the neuron of Andrej Karpathy's
 * micrograd lecture, with his numbers (x1 = 2, w1 = -3, x2 = 0,
 * w2 = 1, b = 6.8814), so the slopes that appear are the ones on his
 * blackboard: 1.0 for w1, 0 for w2, -1.5 for x1, 0.5 for x2.
 *
 * First the values go forward, left to right, each box computed from
 * the boxes before it. Then the slopes come back, right to left: the
 * output's slope is 1 (it is what we are asking about), and each box
 * hands its slope to the boxes it was made of, by the one rule its
 * operation knows -- written at the bottom as it happens. That walk
 * is all of backpropagation.
 *
 * What to try: click a white box (an input or a weight), change it
 * with the up and down arrows, and watch every value and every slope
 * follow. Set x2 to 0 and see w2's slope stay 0 whatever w2 is: a
 * weight on an input that is nothing cannot matter. Push the sum up
 * until tanh is flat, and see every slope behind it go to nothing:
 * the vanishing gradient, one neuron wide. "u" nudges the two weights
 * and the bias a little along their slopes, which is the whole of
 * learning: o rises.
 *
 * Space pauses the walk, "r" plays it again.
 *
 * What it uses: Grad (everything), Scene2d (the keys). *)
open Playground

(*****************************************************************************)
(* The neuron, as a graph laid out by hand *)
(*****************************************************************************)

(* the five numbers one may change *)
type inputs = { x1 : float; w1 : float; x2 : float; w2 : float; b : float }

let karpathy : inputs = { x1 = 2.; w1 = -3.; x2 = 0.; w2 = 1.; b = 6.8813735870195432 }

(* a box of the picture: the Grad value, what it is called, what made
 * it (indices into the same list), where it sits, and the rule by
 * which it hands its slope back *)
type box = {
  name : string;
  node : Grad.t;
  from : int list;
  x : number;
  y : number;
  rule : string;
}

(* the boxes, inputs first: the order values are computed in, and,
 * backwards, an order in which slopes may be handed back *)
let graph (i : inputs) : box list =
  let open Grad in
  let x1 = value i.x1 and w1 = value i.w1 and x2 = value i.x2 and w2 = value i.w2 and b = value i.b in
  let p1 = x1 *: w1 and p2 = x2 *: w2 in
  let s = p1 +: p2 in
  let n = s +: b in
  let o = tanh_ n in
  backward o;
  let leaf = "a number given: nothing behind it to hand a slope to" in
  let product = "a product hands each input its slope times the OTHER input" in
  let sum = "a sum hands its slope to both inputs, unchanged" in
  [
    { name = "x1"; node = x1; from = []; x = -400.; y = 300.; rule = leaf };
    { name = "w1"; node = w1; from = []; x = -400.; y = 190.; rule = leaf };
    { name = "x2"; node = x2; from = []; x = -400.; y = 40.; rule = leaf };
    { name = "w2"; node = w2; from = []; x = -400.; y = -70.; rule = leaf };
    { name = "x1 w1"; node = p1; from = [ 0; 1 ]; x = -190.; y = 245.; rule = product };
    { name = "x2 w2"; node = p2; from = [ 2; 3 ]; x = -190.; y = -15.; rule = product };
    { name = "+"; node = s; from = [ 4; 5 ]; x = 10.; y = 115.; rule = sum };
    { name = "b"; node = b; from = []; x = 10.; y = -120.; rule = leaf };
    { name = "n = + b"; node = n; from = [ 6; 7 ]; x = 210.; y = 0.; rule = sum };
    { name = "o = tanh n"; node = o; from = [ 8 ]; x = 400.; y = 0.;
      rule = "tanh hands its slope times 1 - o squared: nearly nothing where it is flat" };
  ]

let boxes = 10

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  inputs : inputs;
  (* how far the walk is: [stage] values shown going forward, then,
   * past [boxes], [stage - boxes] slopes shown coming back *)
  stage : int;
  chosen : int; (* the box the arrows change, an index *)
  running : bool;
  frame : int;
}

let initial_model : model = { inputs = karpathy; stage = 0; chosen = 1; running = true; frame = 0 }
let finished = 2 * boxes

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the leaves by their box: read and changed *)
let change (i : inputs) (box : int) (by : float) : inputs =
  match box with
  | 0 -> { i with x1 = i.x1 +. by }
  | 1 -> { i with w1 = i.w1 +. by }
  | 2 -> { i with x2 = i.x2 +. by }
  | 3 -> { i with w2 = i.w2 +. by }
  | 7 -> { i with b = i.b +. by }
  | _ -> i

let is_leaf (box : int) : bool = List.mem box [ 0; 1; 2; 3; 7 ]

(* a step uphill: each weight and the bias moved a little along its
 * slope, the inputs left alone -- they are the world's, not ours *)
let nudge (i : inputs) : inputs =
  let g = graph i in
  let slope k = Grad.slope (List.nth g k).node in
  { i with w1 = i.w1 +. (0.05 *. slope 1); w2 = i.w2 +. (0.05 *. slope 3); b = i.b +. (0.05 *. slope 7) }

let box_w = 150.
let box_h = 84.

let update (computer : computer) (s : model Scene2d.t) : model Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let m = { m with frame = m.frame + 1 } in
  (* the walk: a box every half second *)
  let m = if m.running && m.stage < finished && m.frame mod 30 = 0 then { m with stage = m.stage + 1 } else m in
  let m = if key "r" then { m with stage = 0; frame = 0; running = true } else m in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with running = not m.running } else m in
  (* a click on an input chooses it *)
  let m =
    if not computer.mouse.mclick then m
    else
      let hit =
        List.find_opt
          (fun (k, (b : box)) ->
            is_leaf k
            && Float.abs (computer.mouse.mx -. b.x) < box_w /. 2.
            && Float.abs (computer.mouse.my -. b.y) < box_h /. 2.)
          (List.mapi (fun k b -> (k, b)) (graph m.inputs))
      in
      match hit with Some (k, _) -> { m with chosen = k } | None -> m
  in
  (* changing a number shows the whole graph at once: the point is
     then to watch everything follow *)
  let turn by = { m with inputs = change m.inputs m.chosen by; stage = finished } in
  let m = if Scene2d.pressed (fun k -> k.kup) scenes then turn 0.25 else m in
  let m = if Scene2d.pressed (fun k -> k.kdown) scenes then turn (-0.25) else m in
  let m = if key "u" then { m with inputs = nudge m.inputs; stage = finished } else m in
  let m = if key "k" then { m with inputs = karpathy; stage = finished } else m in
  { scenes with scene = m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175
let gold = rgb 240 210 120
let green = rgb 140 220 160

let arrow (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  rectangle color (Float.hypot (x2 -. x1) (y2 -. y1)) 2.
  |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

let view (computer : computer) (s : model Scene2d.t) : shape list =
  let m = s.scene and screen = computer.screen in
  let g = graph m.inputs in
  let valued k = k < m.stage in
  (* slopes come back from the last box to the first *)
  let sloped k = m.stage - boxes > boxes - 1 - k in
  (* the box whose turn it is, and what is being said about it *)
  let now =
    if m.stage = 0 || m.stage = finished then None
    else if m.stage <= boxes then Some (m.stage - 1)
    else Some ((2 * boxes) - m.stage)
  in
  let going_back = m.stage > boxes in
  let arrows =
    List.concat
      (List.mapi
         (fun k (b : box) ->
           List.map
             (fun f ->
               let (a : box) = List.nth g f in
               let lit = going_back && sloped k && now = Some k in
               arrow (if lit then gold else rgb 70 75 100) (a.x +. (box_w /. 2.), a.y) (b.x -. (box_w /. 2.), b.y))
             b.from)
         g)
  in
  let drawn =
    List.concat
      (List.mapi
         (fun k (b : box) ->
           let leaf = is_leaf k in
           let edge = if now = Some k then gold else if leaf && k = m.chosen then white else rgb 70 75 100 in
           [ rectangle edge (box_w +. 6.) (box_h +. 6.) |> move b.x b.y;
             rectangle (if leaf then rgb 44 50 74 else rgb 30 33 48) box_w box_h |> move b.x b.y;
             text (if leaf then white else grey) 1.3 b.name |> move b.x (b.y +. 26.);
             (if valued k then text green 1.4 (Printf.sprintf "%.4f" (Grad.of_ b.node)) |> move b.x b.y else group []);
             (if sloped k then text gold 1.2 (Printf.sprintf "slope %.4f" (Grad.slope b.node)) |> move b.x (b.y -. 26.)
              else group []) ])
         g)
  in
  let saying =
    match now with
    | None when m.stage = finished -> "every slope is in: a weight's slope says how o moves when the weight does"
    | None -> "the numbers on the left are given; everything else is computed from them"
    | Some k ->
        let (b : box) = List.nth g k in
        if not going_back then
          if b.from = [] then b.name ^ " is given" else b.name ^ ": computed from the boxes its arrows come from"
        else if k = boxes - 1 && m.stage = boxes + 1 then "the output's slope is 1: it is what the question is about. " ^ b.rule
        else b.name ^ ": " ^ b.rule
  in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ arrows @ drawn
  @ [ text white 2.2 "ONE NEURON, ITS VALUES FORWARD, ITS SLOPES BACK" |> move_y 450.;
      text grey 1.4 "o = tanh (x1 w1 + x2 w2 + b)      values in green, slopes of o in gold" |> move_y 405.;
      text (if going_back then gold else green) 1.4
        (if m.stage = 0 || m.stage = finished then ""
         else if going_back then "backward: the chain rule, a box at a time"
         else "forward")
      |> move_y (-250.);
      text white 1.3 saying |> move_y (-290.);
      text grey 1.3 "click a pale box, then up and down: change it and watch the slopes follow" |> move_y (-370.);
      text grey 1.3 "u: nudge the weights along their slopes    k: Karpathy's numbers    r: again    space: pause"
      |> move_y (-405.) ]

let app = game view update (Scene2d.start initial_model)
let main = Playground_platform.run_app app
