(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A network learning two spirals, at sixty frames a second
 * (Net.mli, Backprop.mli, notes_ai_learning.md sections 2 to 6).
 * The colour behind the points is what the network answers everywhere
 * -- its decision boundary, redrawn every frame while it trains -- and
 * the curve underneath is the loss falling.
 *
 * Two spirals are the standard hard toy: no straight line separates
 * them, no circle does either, and a network has to bend the space
 * several times over. Watch the boundary start as a smear, harden into
 * a curve, and then grow arms.
 *
 * The keys are the lesson:
 *
 *  - "0", "1", "2": the hidden layers. With none (2-1, one neuron) the
 *    boundary is a straight line and stays one -- that is Neuron.mli's
 *    failure again, at a larger size. With one layer of eight it bends
 *    but cannot follow the arms round. With two it gets there. That is
 *    **capacity**, seen rather than defined: what a machine can
 *    represent is decided before any training happens.
 *  - "s", "t", "e": the squash -- sigmoid, tanh, relu. Same network,
 *    same examples, and the sigmoid is visibly slower to move: its
 *    slope is at most 1/4, so less of the error reaches the early
 *    layers (Backprop.mli, the vanishing gradient measured).
 *  - "-" and "+": the learning rate, the one knob that matters most.
 *    Too small and nothing happens; too large and the loss climbs
 *    instead of falling, and the curve says so at once.
 *  - "h": hold out a quarter of the points and score on those instead,
 *    the second curve. When the held-out loss stops falling while the
 *    training loss keeps going, the network has stopped learning the
 *    rule and started memorising the examples: overfitting, which is
 *    the only thing here that cannot be seen from the boundary alone.
 *
 * The ancestor is TensorFlow Playground (playground.tensorflow.org),
 * which this project shares a name with by coincidence.
 *
 * What it uses: Net and Backprop (everything), Scene2d (keys).
 * Not the Ai layer: this is the algorithm itself. *)
open Playground

(*****************************************************************************)
(* The two spirals *)
(*****************************************************************************)

let per_arm = 120
let noise = 0.12

(* the classic pair: the same spiral, one turned half a turn *)
let spirals (seed : int) : Backprop.example list =
  let st = Random.State.make [| seed |] in
  List.concat_map
    (fun arm ->
      List.init per_arm (fun i ->
          let t = float_of_int i /. float_of_int per_arm in
          let r = 0.15 +. (0.85 *. t) in
          let a = (t *. 4.5) +. (float_of_int arm *. Float.pi) in
          let jitter () = (Random.State.float st 2. -. 1.) *. noise in
          ([| (r *. cos a) +. jitter (); (r *. sin a) +. jitter () |], [| float_of_int arm |])))
    [ 0; 1 ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  net : Net.t;
  points : Backprop.example list;
  training : Backprop.example list; (* what it learns from *)
  held : Backprop.example list; (* and what it is scored on, with "h" *)
  hidden : int; (* hidden layers: 0, 1 or 2 *)
  squash : Net.activation;
  rate : float;
  hold_out : bool;
  running : bool;
  seed : int;
  steps : int; (* batches done *)
  losses : (float * float) list; (* newest first: training, held out *)
  (* what it answers over the whole square, a cell per [cells] x
   * [cells]: two thousand forward passes, so it is kept and redone
   * every few frames rather than every one *)
  field : float array;
}

let batch = 32
let steps_a_frame = 8
let width = 8 (* neurons in a hidden layer *)

let shape (hidden : int) : int list =
  match hidden with 0 -> [ 2; 1 ] | 1 -> [ 2; width; 1 ] | _ -> [ 2; width; width; 1 ]

let split (hold_out : bool) (points : Backprop.example list) : Backprop.example list * Backprop.example list =
  if not hold_out then (points, [])
  else List.partition (fun _ -> true) points |> fun _ ->
       let keep i = i mod 4 <> 0 in
       (List.filteri (fun i _ -> keep i) points, List.filteri (fun i _ -> not (keep i)) points)

let fresh (m : model) : model =
  let (training, held) = split m.hold_out m.points in
  { m with net = Net.make ~seed:m.seed ~hidden:m.squash ~last:Net.Sigmoid (shape m.hidden);
           training; held; steps = 0; losses = []; field = [||] }

let initial_model : model =
  let points = spirals 1 in
  fresh
    { net = []; points; training = points; held = []; hidden = 2; squash = Net.Tanh; rate = 0.6;
      hold_out = false; running = true; seed = 1; steps = 0; losses = []; field = [||] }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a batch, taken in order and wrapping round: the examples of the two
 * arms alternate, so any slice of them has both colours in it *)
let batch_at (m : model) : Backprop.example list =
  let n = List.length m.training in
  if n = 0 then []
  else List.init batch (fun i -> List.nth m.training ((((m.steps * batch) + i) * 7) mod n))

let cells = 32

(* the answer at the middle of every cell *)
let field_of (net : Net.t) : float array =
  let step = 2. /. float_of_int cells in
  Array.init (cells * cells) (fun k ->
      let i = k / cells and j = k mod cells in
      let x = -1. +. (step *. (float_of_int i +. 0.5)) and y = -1. +. (step *. (float_of_int j +. 0.5)) in
      (Net.forward net [| x; y |]).(0))

let train_a_frame (m : model) : model =
  if not m.running then m
  else
    let rec go net steps n = if n = 0 then (net, steps) else go (Backprop.learn ~rate:m.rate net (batch_at { m with steps })) (steps + 1) (n - 1) in
    let (net, steps) = go m.net m.steps steps_a_frame in
    let m = { m with net; steps } in
    (* the loss over every example, and the answer over the whole
       square, are each worth a few hundred forward passes: both are
       redone every fourth frame rather than every one, which is the
       difference between watching this at 10 frames a second and at
       60 *)
    let fresh_numbers = m.field = [||] || steps mod (4 * steps_a_frame) = 0 in
    let losses =
      if not fresh_numbers then m.losses
      else (Backprop.loss net m.training, if m.held = [] then 0. else Backprop.loss net m.held) :: m.losses
    in
    let field = if fresh_numbers then field_of net else m.field in
    { m with losses = List.filteri (fun i _ -> i < 300) losses; field }

let update (computer : computer) (s : model Scene2d.t) : model Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let m = if key "0" then fresh { m with hidden = 0 } else m in
  let m = if key "1" then fresh { m with hidden = 1 } else m in
  let m = if key "2" then fresh { m with hidden = 2 } else m in
  let m = if key "s" then fresh { m with squash = Net.Sigmoid } else m in
  let m = if key "t" then fresh { m with squash = Net.Tanh } else m in
  let m = if key "e" then fresh { m with squash = Net.Relu } else m in
  let m = if key "h" then fresh { m with hold_out = not m.hold_out } else m in
  let m = if key "r" then fresh { m with seed = m.seed + 1; points = spirals (m.seed + 1) } else m in
  let m = if key "-" then { m with rate = Float.max 0.02 (m.rate /. 2.) } else m in
  let m = if key "=" || key "+" then { m with rate = Float.min 20. (m.rate *. 2.) } else m in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with running = not m.running } else m in
  { scenes with scene = train_a_frame m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let box = 560. (* the world -1..1 drawn this many pixels wide *)
let box_y = 90.
let blue = rgb 90 160 240
let red = rgb 230 110 100

let at ((x, y) : float * float) : number * number = ((x *. box /. 2.), (y *. box /. 2.) +. box_y)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the answer everywhere: blue where it says 0, red where it says 1,
 * in [bands] steps between. Cells of the same band next to each other
 * are drawn as one rectangle -- a couple of hundred shapes instead of
 * two thousand, which is the difference between 10 frames a second
 * and 60 -- and the bands themselves are worth having: their edges
 * are the contours of what the network believes. *)
let bands = 20

let band_of (a : float) : int = int_of_float (Float.min 1. (Float.max 0. a) *. float_of_int (bands - 1))

let band_color (b : int) : color =
  let a = float_of_int b /. float_of_int (bands - 1) in
  rgb (int_of_float (40. +. (a *. 90.))) (int_of_float (60. -. (a *. 10.))) (int_of_float (120. -. (a *. 70.)))

let boundary (m : model) : shape list =
  if m.field = [||] then []
  else
    let step = 2. /. float_of_int cells in
    let side = box /. float_of_int cells in
    let x_of i = -1. +. (step *. (float_of_int i +. 0.5)) in
    let y_of j = -1. +. (step *. (float_of_int j +. 0.5)) in
    List.concat_map
      (fun j ->
        (* one row, left to right, cut into runs of the same band *)
        let runs = ref [] and start = ref 0 in
        for i = 1 to cells do
          let ends = i = cells || band_of m.field.((i * cells) + j) <> band_of m.field.((!start * cells) + j) in
          if ends then (
            runs := (!start, i - 1) :: !runs;
            start := i)
        done;
        List.map
          (fun (a, b) ->
            let wide = float_of_int (b - a + 1) *. side in
            let (sx, sy) = at ((x_of a +. x_of b) /. 2., y_of j) in
            rectangle (band_color (band_of m.field.((a * cells) + j))) wide side |> move sx sy)
          !runs)
      (List.init cells (fun j -> j))

let dots (m : model) : shape list =
  List.map
    (fun ((x, y) : Backprop.example) ->
      let (sx, sy) = at (x.(0), x.(1)) in
      circle (if y.(0) = 1. then red else blue) 5. |> move sx sy)
    m.points

(* the loss, newest on the right, on a log scale so that the long slow
 * part of training is visible at all *)
let curve (m : model) : shape list =
  let n = List.length m.losses in
  if n < 2 then []
  else
    let bottom = -330. and height = 110. and wide = box in
    let place i v =
      let x = (wide /. 2.) -. (wide *. float_of_int i /. float_of_int (max 1 (n - 1))) in
      let t = (log (Float.max 1e-4 v) -. log 1e-4) /. (log 1. -. log 1e-4) in
      (x, bottom +. (height *. Float.min 1. (Float.max 0. t)))
    in
    let line color pick =
      List.concat
        (List.filteri (fun i _ -> i < n - 1)
           (List.mapi
              (fun i l ->
                let (x1, y1) = place i (pick l) and (x2, y2) = place (i + 1) (pick (List.nth m.losses (min (n - 1) (i + 1)))) in
                [ rectangle color (Float.hypot (x2 -. x1) (y2 -. y1) +. 1.) 2.
                  |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi)
                  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.) ])
              m.losses))
    in
    [ rectangle (rgb 40 44 60) wide height |> move_y (bottom +. (height /. 2.)) ]
    @ line (rgb 240 210 120) fst
    @ (if m.hold_out then line (rgb 140 220 160) snd else [])

let view (computer : computer) (s : model Scene2d.t) : shape list =
  let m = s.scene and screen = computer.screen in
  let loss = match m.losses with (l, _) :: _ -> l | [] -> 0. in
  let held = match m.losses with (_, h) :: _ -> h | [] -> 0. in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ boundary m @ dots m @ curve m
  @ [ text white 2.2 "A NETWORK LEARNING TWO SPIRALS" |> move_y 450.;
      text (rgb 150 155 175) 1.4
        (Printf.sprintf "%s   %s   rate %.2f   %d weights   %d batches"
           (String.concat "-" (List.map string_of_int (Net.sizes m.net)))
           (match m.squash with Net.Sigmoid -> "sigmoid" | Net.Tanh -> "tanh" | Net.Relu -> "relu" | Net.Linear -> "linear")
           m.rate (Net.weights m.net) m.steps)
      |> move_y 400.;
      text (rgb 240 210 120) 1.4 (Printf.sprintf "loss %.4f" loss) |> move 370. (-255.);
      (if m.hold_out then text (rgb 140 220 160) 1.4 (Printf.sprintf "held out %.4f" held) |> move 370. (-290.) else group []);
      text (rgb 150 155 175) 1.3 "0 1 2: hidden layers    s t e: sigmoid tanh relu    h: hold out a quarter" |> move_y (-420.);
      text (rgb 150 155 175) 1.3 "- +: the learning rate    r: new spirals    space: pause" |> move_y (-450.) ]

let app = game view update (Scene2d.start initial_model)
let main = Playground_platform.run_app app
