(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Go on a 9x9 board, against a computer that knows nothing about Go
 * (Mcts.mli). You are black, it is white: click a point to put a
 * stone down (or move the cursor with the arrows and press space),
 * "p" to pass; two passes in a row end the game, and the
 * score is counted Chinese style -- your stones plus the empty points
 * only you surround, white's plus komi, 6.5 points for playing second.
 * Space plays again.
 *
 * The point of this game in this repository is what the computer does
 * *not* have. Every other searching game here leans on an evaluation
 * function: AiChess counts material and where the pieces stand,
 * AiOthello has a table of what each square is worth. Nobody has ever
 * written one for Go -- whether a position is good depends on whether
 * groups will live, which is as hard as playing -- and that is why Go
 * programs stayed weak from 1970 to 2005 while chess programs beat the
 * world champion.
 *
 * What broke it open in 2006 was giving up on knowledge: to judge a
 * position, play it out *at random* to the end, hundreds of times, and
 * count the wins. This program does exactly that, and nothing else:
 * search for the sentence "how good is this position" in the code and
 * you will not find it. What you will find is a playout that plays
 * random legal moves until neither side has one worth making.
 *
 * The one piece of knowledge in the playouts is a rule about eyes: a
 * random player that fills in its own eyes kills its own groups, and
 * the playouts then say nothing. Not filling a point surrounded by
 * your own stones is the smallest rule that makes random play mean
 * something -- and it is the same rule every Monte Carlo Go program
 * starts from.
 *
 * It plays like a weak amateur, which is the honest result: pure MCTS
 * on 9x9 in 2006 was about that, and what lifted it to superhuman ten
 * years later was AlphaGo replacing the random playouts and the win
 * counts with a neural network (notes_ai_learning.md section 9).
 *
 * That network is here too, in miniature: "a" changes who plays white,
 * or the flag ai=network, to a search guided by a network that taught
 * itself by playing itself (its own section below, Alphazero.mli), and
 * ai=policy to that network alone.
 *
 * What it uses: the boards kit's Go9 (the rules, the counting, the
 * playout: shared with the program that trains the network); ai/'s
 * Mcts (the search) and Minimax (the [game] record it takes);
 * Alphazero and Policy_value for the second computer, whose weights are
 * data/weights/go9; Scene2d (the keys pressed). Not Deepening or
 * Zobrist: there is no depth to deepen and no value to remember.
 *
 * Exercises: the ko rule in full (this is the simple one: a move may
 * not take back the single stone that just took); playouts that answer
 * a capture or an atari instead of playing anywhere (the next thing
 * every Go program did); RAVE, which lets a move's results elsewhere
 * count towards it here.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The board, and the rules as ai/ wants them *)
(*****************************************************************************)

(* the board, the groups and their liberties, captures, the ko, the
 * counting, and the playout with its one rule about eyes: the boards
 * kit's Go9, shared with the program that trains a network to play
 * (scripts/train/train_go) *)
include Go9

(*****************************************************************************)
(* The other computer: a network that taught itself (ai=network) *)
(*****************************************************************************)
(* The computer below judges a position by playing it out at random,
 * a thousand times. This one has a network instead, which was told
 * the same rules and the same one hint (not to fill its own eyes) and
 * learned the rest by playing against itself (Alphazero.mli,
 * notes_ai_learning.md section 16): a guess at which points matter,
 * and a guess at who is winning, in place of the random games. It is
 * what happened to Go in 2016, in miniature -- the header says how
 * small.
 *
 * The network was not trained here: scripts/train/train_go did it,
 * for hours, and what it learned is a file of data/weights, whose
 * first lines say how it then did against the computer below.
 *
 * "a" changes who plays white, and the flag ai= who starts: classic
 * (random playouts), network (the search guided by the network),
 * policy (the network alone, its first idea, no search). With the
 * network playing, the board shows its first idea of the position it
 * last moved from: a mark on each point, the larger the more it
 * thought of it at a glance. *)

type engine = Classic | Network | Policy

let engine_of (flags : (string * string) list) : engine =
  match List.assoc_opt "ai" flags with Some "network" -> Network | Some "policy" -> Policy | _ -> Classic

let name (e : engine) : string =
  match e with
  | Classic -> "random games played out, no knowledge"
  | Network -> "a network that taught itself, and a search"
  | Policy -> "the network alone, no search"

let net : Policy_value.t Lazy.t =
  lazy
    (match Result.bind (Weights.of_string Weights_go9.bytes) Policy_value.of_weights with
    | Ok net -> net
    | Error why -> failwith ("go9.weights: " ^ why))

(* a search guided by the network costs a pass through it a playout,
 * about as much as a random game played out *)
let network_playouts = 600

(* the network's policy over the points of a position, at a glance *)
let glance_at (p : position) : (int * number) list =
  let (prior, _) = Alphazero.guides Go9.board (Lazy.force net) in
  List.filter_map (fun (m, share) -> match m with Put i -> Some (i, share) | Pass -> None) (prior p)

(*****************************************************************************)
(* The game *)
(*****************************************************************************)

(* A playout of a 9x9 board costs about 1.2 ms here (tests/games times
 * it), so a dozen of them is a frame at 60 fps and a thousand is the
 * second and a half it takes to answer -- during which the game is
 * never stopped, because the tree is an answer at every moment
 * (Mcts.mli: anytime). A chess engine cannot do this: interrupt its
 * search and it has nothing. *)
let playouts_a_move = 1000
let playouts_a_frame = 12

type game = {
  position : position;
  cursor : int;
  last : int option;
  (* its tree, while it is white's turn: MCTS is anytime, so the game
   * goes on drawing while it grows (Mcts.mli's think) *)
  mind : (position, move) Mcts.thinking option;
  thought : int; (* playouts into this move *)
  said : (int * int * float) option; (* playouts, tree nodes, its win rate *)
  moves_played : int;
  engine : engine option; (* who plays white; None until the flags are seen *)
  (* the network's first idea of each point of the position it last
   * moved from: its policy, before any search *)
  glance : (int * number) list;
}

type model = game Scene2d.t

let new_game () : game =
  { position = start; cursor = (4 *.. size) +.. 4; last = None; mind = None; thought = 0; said = None; moves_played = 0;
    engine = None; glance = [] }
let initial_model : model = Scene2d.start (new_game ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let cell = 80.
let board_left = -.(float_of_int (size -.. 1) * cell / 2.)
let point_at (x : number) (y : number) : int option =
  let c = int_of_float (Float.round ((x - board_left) / cell)) in
  let r = int_of_float (Float.round ((y - board_left) / cell)) in
  if c >= 0 && c < size && r >= 0 && r < size && Float.hypot (x - (board_left + (float_of_int c * cell))) (y - (board_left + (float_of_int r * cell))) < cell / 2.
  then Some ((r *.. size) +.. c)
  else None

(* a frame's worth of thinking; when it has had enough, it plays the
 * move its tree believes in *)
let machine_thinks (engine : engine) (g : game) : game =
  let t =
    match g.mind with
    | Some t -> t
    | None -> (
        match engine with
        | Classic -> Mcts.start ~seed:g.moves_played ~playout go g.position
        | Network | Policy ->
            (* the same search, the network's two guesses in the
               place of the random games *)
            let (prior, evaluate) = Alphazero.guides Go9.board (Lazy.force net) in
            Mcts.start ~seed:g.moves_played ~prior ~evaluate Go9.sensible g.position)
  in
  let enough = match engine with Classic -> playouts_a_move | Network -> network_playouts | Policy -> 0 in
  let t = if engine = Policy then t else Mcts.think ~playouts:playouts_a_frame t in
  let thought = g.thought +.. playouts_a_frame in
  if thought < enough then { g with mind = Some t; thought }
  else if engine = Policy then
    (* no search: the point its policy likes best *)
    let move = match Alphazero.instinct Go9.board (Lazy.force net) g.position with Some m -> m | None -> Pass in
    { g with
      position = play g.position move;
      last = (match move with Put i -> Some i | Pass -> None);
      mind = None;
      thought = 0;
      said = None;
      glance = glance_at g.position;
      moves_played = g.moves_played +.. 1 }
  else
    let r = Mcts.plan t in
    let rate =
      match r.best with
      | Some m -> ( match List.find_opt (fun (x, _, _) -> x = m) r.tried with Some (_, _, share) -> share | None -> 0.5)
      | None -> 0.5
    in
    let move = match r.best with Some m -> m | None -> Pass in
    { g with
      position = play g.position move;
      last = (match move with Put i -> Some i | Pass -> None);
      mind = None;
      thought = 0;
      said = Some (r.playouts, r.nodes, rate);
      glance = (if engine = Classic then [] else glance_at g.position);
      moves_played = g.moves_played +.. 1 }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let m = computer.mouse in
  let engine = match g.engine with Some e -> e | None -> engine_of computer.flags in
  let g = { g with engine = Some engine } in
  if over g.position then
    if Scene2d.pressed (fun k -> k.kspace) scenes then { (new_game ()) with engine = Some engine } else g
  else if g.position.turn = White then machine_thinks engine g
  else if Scene2d.pressed (fun k -> Set_.mem "a" k.keys) scenes then
    { g with engine = Some (match engine with Classic -> Network | Network -> Policy | Policy -> Classic); glance = []; said = None }
  else if Scene2d.pressed (fun k -> Set_.mem "p" k.keys) scenes then
    { g with position = play g.position Pass; last = None; moves_played = g.moves_played +.. 1 }
  else if Scene2d.pressed (fun k -> k.kleft || k.kright || k.kup || k.kdown) scenes then begin
    let (x, y) = xy g.cursor in
    let k = computer.keyboard in
    let x = if k.kleft then max 0 (x -.. 1) else if k.kright then min (size -.. 1) (x +.. 1) else x in
    let y = if k.kdown then max 0 (y -.. 1) else if k.kup then min (size -.. 1) (y +.. 1) else y in
    { g with cursor = (y *.. size) +.. x }
  end
  else if Scene2d.pressed (fun k -> k.kspace) scenes && put g.position g.cursor <> None then
    { g with position = play g.position (Put g.cursor); last = Some g.cursor; moves_played = g.moves_played +.. 1 }
  else if m.mclick then
    match point_at m.mx m.my with
    | Some i when put g.position i <> None ->
        { g with position = play g.position (Put i); last = Some i; moves_played = g.moves_played +.. 1 }
    | _ -> g
  else g

let update (computer : computer) (s : model) : model =
  let scenes = Scene2d.update computer s in
  { scenes with scene = update_game computer scenes scenes.scene }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let centre (i : int) : number * number =
  let (x, y) = xy i in
  (board_left + (float_of_int x * cell), board_left + (float_of_int y * cell))

let view (computer : computer) (s : model) : shape list =
  let g = s.scene and screen = computer.screen in
  let board = float_of_int (size -.. 1) * cell in
  let grid =
    List.concat_map
      (fun i ->
        let at = board_left + (float_of_int i * cell) in
        [ rectangle (rgb 60 40 20) board 2. |> move_y at; rectangle (rgb 60 40 20) 2. board |> move_x at ])
      (List.init size Fun.id)
  in
  let stones =
    List.filter_map
      (fun i ->
        let (x, y) = centre i in
        match g.position.board.(i) with
        | Empty -> None
        | Black -> Some (circle (rgb 20 20 25) (cell * 0.45) |> move x y)
        | White -> Some (circle (rgb 240 240 235) (cell * 0.45) |> move x y))
      (List.init points Fun.id)
  in
  let last = match g.last with Some i -> let (x, y) = centre i in [ circle (rgb 220 70 60) 8. |> move x y ] | None -> [] in
  let cursor =
    if over g.position || g.position.turn <> Black then []
    else
      let (x, y) = centre g.cursor in
      [ circle (rgb 20 20 25) (cell * 0.45) |> fade 0.35 |> move x y ]
  in
  let engine = match g.engine with Some e -> e | None -> engine_of computer.flags in
  let said =
    match g.said with
    | None -> []
    | Some (playouts, nodes, rate) ->
        [ text (rgb 170 170 190) 1.5
            (Printf.sprintf (if engine = Classic then "%d random games, a tree of %d positions" else "%d positions judged by the network, a tree of %d") playouts nodes)
          |> move_y (-425.);
          text (rgb 170 170 190) 1.5
            (Printf.sprintf (if engine = Classic then "it expects to win %.0f%% of them" else "it thinks it wins %.0f%% of the time") (100. * rate))
          |> move_y (-455.) ]
  in
  (* the network's glance: a square on each point it thought of, the
     larger the more *)
  let glance =
    List.filter_map
      (fun (i, share) ->
        if g.position.board.(i) <> Empty || share < 0.01 then None
        else
          let (x, y) = centre i in
          let side = 6. + (cell * 0.6 * sqrt share) in
          Some (rectangle (rgb 60 110 200) side side |> fade 0.55 |> move x y))
      g.glance
  in
  let ended =
    if not (over g.position) then []
    else
      let s = final_score g.position in
      [ text white 3. (if s > 0. then Printf.sprintf "YOU WIN BY %.1f" s else Printf.sprintf "WHITE WINS BY %.1f" (-.s))
        |> move_y 430. ]
  in
  [ rectangle (rgb 25 28 45) screen.width screen.height;
    rectangle (rgb 200 160 90) (board + cell) (board + cell) ]
  @ grid @ glance @ stones @ cursor @ last @ said @ ended
  @ [ text white 2.2 "GO  9x9" |> move_y 460.;
      text (rgb 150 150 170) 1.2 ("white is " ^ name engine ^ "   (a: another)") |> move_y 395.;
      text (rgb 150 150 170) 1.5
        (if over g.position then "space: play again"
         else if g.position.turn = Black then "click a point, or the arrows and space;  p: pass"
         else if engine = Classic then Printf.sprintf "white is playing out random games... %d" g.thought
         else Printf.sprintf "white is thinking... %d" g.thought)
      |> move_y (-390.) ]

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
