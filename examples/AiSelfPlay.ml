(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A network teaching itself tic-tac-toe by playing against itself,
 * while you watch: AlphaZero's loop on the smallest game there is
 * (Selfplay.mli, Policy_value.mli, notes_ai_learning.md section 16).
 * Nobody tells it anything but the rules.
 *
 * Every frame it either plays a game against itself, the search
 * guided by the network, and takes ten steps on what its games have
 * taught; or it plays one game that counts, to be measured. Three
 * curves say how it is doing, each over its last thirty such games:
 *
 *  - green: against a *perfect* player, the share of games it does
 *    not lose. Tic-tac-toe is a draw with best play, so 100% here is
 *    all there is to reach. It gets there within a minute, and then
 *    wobbles: a few hundred games are not many, and it still loses
 *    one now and then;
 *  - blue: against a player moving at random, the share it wins;
 *  - gold: the same, but the network *alone*, playing its policy's
 *    first choice with no search at all. This one climbs slowly: it is
 *    what the network itself has learned, without looking ahead.
 *
 * The two boards are the network's opinion of two positions, a share
 * per square. On the empty board there is nothing to know (every
 * first move draws). On the second, x has taken a corner and o is to
 * play: every answer but the centre loses, and that is something to
 * learn. Watch the centre light up.
 *
 * Space pauses, "r" starts again from a network that knows nothing.
 *
 * What it uses: Selfplay, Policy_value, Arena and Tictactoe (ai's
 * selfplay folder), Minimax for the perfect player, Scene2d (the
 * keys).
 *
 * Exercise: play against it yourself (AiTictactoe has the board and
 * the clicks). *)
open Playground

(*****************************************************************************)
(* The game, and its two fixed opponents *)
(*****************************************************************************)

let board : (Tictactoe.position, int) Selfplay.board =
  { game = Tictactoe.game; start = Tictactoe.start; inputs = 18; moves = 9; encode = Tictactoe.encode; index = (fun m -> m) }

(* the truth, by search to the end of the game; each position's answer
 * kept, the empty board's being half a million positions to visit *)
let perfect_moves : (string, int) Hashtbl.t Lazy.t = lazy (Hashtbl.create 1000)

let perfect : (Tictactoe.position, int) Arena.player =
 fun ~seed:_ state ->
  let known = Lazy.force perfect_moves and key = Tictactoe.to_string state in
  match Hashtbl.find_opt known key with
  | Some move -> move
  | None ->
      let move = Option.get (Minimax.alphabeta Tictactoe.game ~depth:9 state).best in
      Hashtbl.add known key move;
      move

let random = Arena.random Tictactoe.game

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the three measures: which player, against whom, and what counts *)
type measure = Perfect | Random | Alone

type model = {
  net : Policy_value.t;
  memory : Policy_value.lesson array; (* the last lessons, newest first *)
  games : int; (* played against itself *)
  loss : float;
  draws : Lehmer.state;
  (* the last thirty results of each measure, newest first: 1 for a
   * game that counts for it *)
  recent : (measure * float list) list;
  (* the three rates, each time one was measured, newest first *)
  curves : (measure * float list) list;
  measured : int; (* games that counted, so far *)
  running : bool;
  frame : int;
}

let fresh () : model =
  {
    net = Policy_value.make ~seed:1 ~rate:0.01 ~inputs:18 ~moves:9 ();
    memory = [||];
    games = 0;
    loss = 0.;
    draws = Lehmer.make 5;
    recent = [ (Perfect, []); (Random, []); (Alone, []) ];
    curves = [ (Perfect, []); (Random, []); (Alone, []) ];
    measured = 0;
    running = true;
    frame = 0;
  }

let initial_model : model Lazy.t = lazy (fresh ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let remembered = 3000
let window = 30
let kept = 300

(* a game against itself, its lessons added, ten steps taken *)
let learn (m : model) : model =
  let (lessons, _) = Selfplay.play ~seed:(1000 + m.games) board m.net in
  let all = Array.append (Array.of_list lessons) m.memory in
  let memory = Array.sub all 0 (min remembered (Array.length all)) in
  let rec steps net loss n =
    if n = 0 then (net, loss)
    else
      let batch = Array.init 32 (fun _ -> memory.(Lehmer.int m.draws (Array.length memory))) in
      let (net, loss) = Policy_value.step net batch in
      steps net loss (n - 1)
  in
  let (net, loss) = steps m.net m.loss 10 in
  { m with net; memory; loss; games = m.games + 1 }

(* one game that counts, each measure in turn, the network moving
 * first every other time *)
let measure (m : model) : model =
  let which = match m.measured mod 3 with 0 -> Perfect | 1 -> Random | _ -> Alone in
  let searching : (Tictactoe.position, int) Arena.player = fun ~seed s -> Option.get (Selfplay.choose ~seed board m.net s) in
  let alone : (Tictactoe.position, int) Arena.player = fun ~seed:_ s -> Option.get (Selfplay.instinct board m.net s) in
  let (player, opponent) = match which with Perfect -> (searching, perfect) | Random -> (searching, random) | Alone -> (alone, random) in
  let first = m.measured / 3 mod 2 = 0 in
  let share =
    let game = Arena.game Tictactoe.game Tictactoe.start ~seed:m.measured in
    if first then game ~max:player ~min:opponent else 1. -. game ~max:opponent ~min:player
  in
  (* against the perfect player, not losing is all there is; against
     the random one, winning *)
  let counts = match which with Perfect -> if share >= 0.5 then 1. else 0. | Random | Alone -> if share > 0.5 then 1. else 0. in
  let recent = List.map (fun (w, l) -> if w = which then (w, List.filteri (fun i _ -> i < window) (counts :: l)) else (w, l)) m.recent in
  let rate l = if l = [] then 0. else List.fold_left ( +. ) 0. l /. float_of_int (List.length l) in
  let curves =
    List.map
      (fun (w, c) -> if w = which then (w, List.filteri (fun i _ -> i < kept) (rate (List.assoc w recent) :: c)) else (w, c))
      m.curves
  in
  { m with recent; curves; measured = m.measured + 1 }

let update (computer : computer) (s : model Lazy.t Scene2d.t) : model Lazy.t Scene2d.t =
  let scenes = Scene2d.update computer s in
  let m = Lazy.force scenes.scene in
  let key k = Scene2d.pressed (fun kb -> Set_.mem k kb.keys) scenes in
  let m = if key "r" then fresh () else m in
  let m = if Scene2d.pressed (fun k -> k.kspace) scenes then { m with running = not m.running } else m in
  let m =
    if not m.running then m
    else
      let m = { m with frame = m.frame + 1 } in
      (* two frames of learning for one of measuring *)
      if m.frame mod 3 = 0 then measure m else learn m
  in
  { scenes with scene = Lazy.from_val m }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let grey = rgb 150 155 175
let gold = rgb 240 210 120
let green = rgb 140 220 160
let blue = rgb 120 180 250

(* a position and the network's opinion of it: its marks, and on each
 * empty square the share of the policy, lit accordingly *)
let opinion_board (m : model) (position : Tictactoe.position) (cx : number) (cy : number) (title : string) : shape list =
  let cell = 100. in
  let (prior, _) = Selfplay.guides board m.net in
  let shares = prior position in
  let (_, value) = Policy_value.opinion m.net (Tictactoe.encode position) in
  List.concat
    (List.init 9 (fun i ->
         let x = cx +. (cell *. float_of_int ((i mod 3) - 1)) and y = cy -. (cell *. float_of_int ((i / 3) - 1)) in
         match (position.cells.(i), List.assoc_opt i shares) with
         | (Tictactoe.Empty, Some p) ->
             let a = Float.min 1. (p *. 1.6) in
             [ rectangle (rgb (int_of_float (36. +. (a *. 200.))) (int_of_float (40. +. (a *. 160.))) 62) (cell -. 6.) (cell -. 6.) |> move x y;
               text white 1.5 (Printf.sprintf "%.0f%%" (100. *. p)) |> move x y ]
         | (mark, _) ->
             [ rectangle (rgb 30 33 48) (cell -. 6.) (cell -. 6.) |> move x y;
               text grey 3. (match mark with Tictactoe.X -> "x" | Tictactoe.O -> "o" | Tictactoe.Empty -> "") |> move x y ]))
  @ [ text white 1.3 title |> move cx (cy +. 185.);
      text grey 1.2 (Printf.sprintf "who is winning, for the one to play: %+.2f" value) |> move cx (cy -. 180.) ]

(* a rate over time, oldest on the left, 0 at the bottom and 1 at the top *)
let curve (color : color) (rates : float list) : shape list =
  let n = List.length rates in
  let bottom = -400. and height = 150. and wide = 900. in
  List.mapi
    (fun i v ->
      rectangle color 3. 3.
      |> move ((-.wide /. 2.) +. (wide *. float_of_int (n - 1 - i) /. float_of_int (kept - 1))) (bottom +. (height *. v)))
    rates

let view (computer : computer) (s : model Lazy.t Scene2d.t) : shape list =
  let m = Lazy.force s.scene and screen = computer.screen in
  let now which = match List.assoc which m.curves with r :: _ -> Printf.sprintf "%.0f%%" (100. *. r) | [] -> "..." in
  [ rectangle (rgb 18 20 30) screen.width screen.height ]
  @ opinion_board m Tictactoe.start (-250.) 130. "the empty board: any move draws"
  @ opinion_board m (Tictactoe.of_string "x........") 250. 130. "x in a corner, o to play: only the centre draws"
  @ [ rectangle (rgb 30 33 48) 900. 150. |> move_y (-325.) ]
  @ curve green (List.assoc Perfect m.curves)
  @ curve blue (List.assoc Random m.curves)
  @ curve gold (List.assoc Alone m.curves)
  @ [ text white 2.2 "A NETWORK TEACHING ITSELF TIC-TAC-TOE" |> move_y 450.;
      text grey 1.4
        (Printf.sprintf "%d numbers   %d games against itself   %d lessons kept   loss %.2f" (Policy_value.parameters m.net)
           m.games (Array.length m.memory) m.loss)
      |> move_y 405.;
      text green 1.3 ("does not lose to a perfect player: " ^ now Perfect) |> move (-300.) (-225.);
      text blue 1.3 ("beats a random one: " ^ now Random) |> move 20. (-225.);
      text gold 1.3 ("alone, no search: " ^ now Alone) |> move 300. (-225.);
      text grey 1.3 "r: again, from knowing nothing    space: pause" |> move_y (-460.) ]

let app = game view update (Scene2d.start initial_model)
let main = Playground_platform.run_app app
