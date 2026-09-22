(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A walker learning a grid world by falling off a cliff
 * (ai/Qlearn.mli, notes_ai_learning.md section 8). Nobody tells it
 * where to go: it is told only what each step costs, and the way to
 * the goal appears out of that, one episode at a time.
 *
 * The world is Sutton and Barto's cliff: start at the bottom left,
 * goal at the bottom right, the row between them a cliff that costs
 * 100 and puts you back at the start. Every other step costs 1.
 *
 * What is drawn is the whole of what it knows -- four numbers per
 * cell, one per direction, and an arrow on the best of them. Watch
 * the values seep backwards from the goal: after one episode only the
 * cells next to the goal mean anything, and the knowledge creeps left
 * along the grid at about one cell per episode, because that is
 * exactly how far one step of the rule can carry it.
 *
 *     Q(s,a) <- Q(s,a) + rate * ( r + discount * max Q(s',a') - Q(s,a) )
 *
 * Two things are worth doing with the keys:
 *
 *  - "e" turns exploring off and on. With it off, this world still
 *    gets solved -- every step costs something, so an action never
 *    tried (worth 0) looks better than every action tried, and a
 *    greedy walker tries everything once anyway. That is optimism in
 *    the initial values, and it is switched on here by the *sign* of
 *    the rewards. "p" pays only at the goal instead, and then the same
 *    walker with exploring off does the same thing for ever and never
 *    finds the goal at all.
 *  - "g" walks it greedily, with no learning and no dice: what it has
 *    actually learned, which is not what it does while learning. It
 *    ends up walking the cliff edge -- the shortest way -- because the
 *    rule learns the best policy even while the walker is being
 *    careless. (That is off-policy learning: Qlearn.mli's note about
 *    SARSA, which would keep a safer distance.)
 *
 * Keys: space pause, f faster, e exploring on/off, p the other
 * rewards, g the greedy walk, r start over.
 *
 * What it uses: ai/Qlearn (all of it), Scene2d (the keys). *)
open Playground

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

let wide = 12
let high = 4
let start = (0, 0)
let goal = (wide - 1, 0)
let cliff ((x, y) : int * int) : bool = y = 0 && x > 0 && x < wide - 1

type move = Up | Down | Left | Right

let moves = [ Up; Down; Left; Right ]

let world (paid_at_goal : bool) : ((int * int), move) Qlearn.world =
  {
    actions = (fun _ -> moves);
    step =
      (fun (x, y) a ->
        let (nx, ny) = match a with Up -> (x, y + 1) | Down -> (x, y - 1) | Left -> (x - 1, y) | Right -> (x + 1, y) in
        let (nx, ny) = if nx < 0 || nx >= wide || ny < 0 || ny >= high then (x, y) else (nx, ny) in
        if paid_at_goal then (((if cliff (nx, ny) then start else (nx, ny))), if (nx, ny) = goal then 1. else 0.)
        else if cliff (nx, ny) then (start, -100.)
        else ((nx, ny), -1.));
    over = (fun p -> p = goal);
  }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type model = {
  q : ((int * int), move) Qlearn.t;
  at : int * int; (* where the walker is, this frame *)
  episode : int;
  steps : int; (* steps in this episode *)
  fell : int; (* times it has gone over the edge *)
  rewards : float list; (* what each episode collected, newest first *)
  got : float; (* this episode's so far *)
  exploring : bool;
  paid_at_goal : bool;
  running : bool;
  fast : bool;
  greedy : (int * int) list; (* the greedy walk, when it has been asked for *)
  seed : int;
}

let table (exploring : bool) (seed : int) : ((int * int), move) Qlearn.t =
  Qlearn.make ~rate:0.5 ~discount:0.95 ~explore:(if exploring then 0.1 else 0.) ~seed ()

let fresh (m : model) : model =
  { m with q = table m.exploring m.seed; at = start; episode = 1; steps = 0; fell = 0; rewards = []; got = 0.;
           greedy = [] }

let initial_model : model =
  fresh
    { q = table true 1; at = start; episode = 1; steps = 0; fell = 0; rewards = []; got = 0.; exploring = true;
      paid_at_goal = false; running = true; fast = false; greedy = []; seed = 1 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* one step of the walker, which is one use of the rule *)
let step_once (m : model) : model =
  let w = world m.paid_at_goal in
  if m.at = goal || m.steps > 400 then
    { m with at = start; episode = m.episode + 1; steps = 0; rewards = m.got :: m.rewards; got = 0. }
  else
    match Qlearn.choose m.q m.at moves with
    | None -> m
    | Some action ->
        let (next, reward) = w.step m.at action in
        let next_actions = if w.over next then [] else moves in
        Qlearn.learn m.q ~state:m.at ~action ~reward ~next ~next_actions;
        { m with at = next; steps = m.steps + 1; got = m.got +. reward;
                 fell = (if reward <= -100. then m.fell + 1 else m.fell) }

let update (computer : computer) (s : model Scene2d.t) : model Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with running = not m.running } else m in
  let m = if key "f" then { m with fast = not m.fast } else m in
  let m = if key "e" then fresh { m with exploring = not m.exploring } else m in
  let m = if key "p" then fresh { m with paid_at_goal = not m.paid_at_goal } else m in
  let m = if key "r" then fresh { m with seed = m.seed + 1 } else m in
  let m =
    if key "g" then
      let (_, path) = Qlearn.greedy_run ~limit:60 m.q (world m.paid_at_goal) start in
      { m with greedy = path }
    else m
  in
  let m = if m.running then (let rec go m n = if n = 0 then m else go (step_once m) (n - 1) in go m (if m.fast then 60 else 2)) else m in
  { scenes with scene = m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let cell = 74.
let left = -.(float_of_int wide *. cell /. 2.)
let bottom = -120.
let at_cell ((x, y) : int * int) : number * number =
  (left +. (cell *. (float_of_int x +. 0.5)), bottom +. (cell *. (float_of_int y +. 0.5)))

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* a cell: its floor, its four values in their four corners, and an
 * arrow on the best of them *)
let cell_shapes (m : model) (c : int * int) : shape list =
  let (x, y) = at_cell c in
  let floor =
    if cliff c then rgb 90 40 45 else if c = goal then rgb 50 100 60 else if c = start then rgb 50 55 80 else rgb 38 42 58
  in
  let values = Qlearn.values m.q c moves in
  let known = List.exists (fun (_, v) -> v <> 0.) values in
  let biggest = List.fold_left (fun b (_, v) -> Float.max b (Float.abs v)) 0.001 values in
  let corner (a : move) =
    let (dx, dy) = match a with Up -> (0., 22.) | Down -> (0., -22.) | Left -> (-24., 0.) | Right -> (24., 0.) in
    let v = List.assoc a values in
    let shade = Float.min 1. (Float.abs v /. biggest) in
    text (if v < 0. then rgb (120 + int_of_float (shade *. 110.)) 90 90 else rgb 90 (120 + int_of_float (shade *. 110.)) 100) 0.9
      (if v = 0. then "" else Printf.sprintf "%.0f" v)
    |> move (x +. dx) (y +. dy)
  in
  let arrow =
    if (not known) || cliff c || c = goal then []
    else
      match Qlearn.best m.q c moves with
      | None -> []
      | Some a ->
          let turn = match a with Right -> 0. | Up -> 90. | Left -> 180. | Down -> 270. in
          [ triangle (rgb 240 200 90) 9. |> rotate (turn -. 90.) |> fade 0.9 |> move x y ]
  in
  [ rectangle floor (cell -. 3.) (cell -. 3.) |> move x y ] @ List.map corner moves @ arrow

let view (computer : computer) (s : model Scene2d.t) : shape list =
  let m = s.scene and screen = computer.screen in
  let grid = List.concat_map (fun y -> List.concat_map (fun x -> cell_shapes m (x, y)) (List.init wide (fun x -> x))) (List.init high (fun y -> y)) in
  let walker = let (x, y) = at_cell m.at in [ circle (rgb 240 240 235) 13. |> move x y ] in
  let greedy =
    List.map
      (fun c -> let (x, y) = at_cell c in circle (rgb 140 220 160) 7. |> fade 0.8 |> move x y)
      m.greedy
  in
  let last_reward = match m.rewards with r :: _ -> r | [] -> 0. in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ grid @ greedy @ walker
  @ [ text white 2.2 "LEARNING A WORLD BY FALLING OFF IT" |> move_y 430.;
      text (rgb 150 155 175) 1.4
        (Printf.sprintf "episode %d   last %.0f   fell in %d times   %d pairs known" m.episode last_reward m.fell
           (Qlearn.known m.q))
      |> move_y 380.;
      text (rgb 150 155 175) 1.4
        (Printf.sprintf "%s   %s" (if m.exploring then "exploring 0.1" else "exploring OFF")
           (if m.paid_at_goal then "paid at the goal only" else "every step costs 1, the cliff 100"))
      |> move_y 330.;
      text (rgb 140 220 160) 1.3 (if m.greedy = [] then "" else Printf.sprintf "the greedy way: %d steps" (List.length m.greedy - 1))
      |> move_y 280.;
      text (rgb 150 155 175) 1.2 "space: pause   f: faster   e: exploring   p: the other rewards" |> move_y (-290.);
      text (rgb 150 155 175) 1.2 "g: walk it greedily   r: start over" |> move_y (-330.) ]

let app = game view update (Scene2d.start initial_model)
let main = Playground_platform.run_app app
