(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Connect 4 against the computer, which is the game that needs all of
 * Deepening at once (notes_ai.md section 9). You are yellow: click a
 * column, or move with the arrows and drop with space. Four in a row,
 * any direction, wins. "v" shows what the computer thinks of each
 * column (the lower, the better for you); space after the end plays
 * again.
 *
 * Between Othello and chess in size: 7 columns, so a tree of 7^d, and
 * the same position over and over by different orders of the same
 * drops -- which is exactly where the three tricks show up, and after
 * each of its moves the computer says what they saved:
 *
 *   alpha-beta alone, the columns left to right   the plain search
 *   + the middle columns first                    ordering
 *   + 1, 2, ... up to the depth                   iterative deepening
 *   + what it learned about a position kept       the table (Zobrist)
 *
 * The middle columns first is the game's own hint (Deepening's
 * [order]): a piece in the middle is in more fours than one at the
 * edge -- 13 of them against 3 -- so middle moves are likelier to be
 * good, and a good move tried first is what makes alpha-beta cut.
 *
 * The evaluation, at the leaves: a four is a win; otherwise every line
 * of four squares with pieces of one colour only is worth 1, 10 or 100
 * for one, two or three of them, plus the middle column, and the other
 * player's are subtracted.
 *
 * The game: Milton Bradley's Connect Four (1974), though the game is
 * older (Captain's Mistress). Solved twice over in 1988, by James Dow
 * Allen and by Victor Allis (whose thesis is the readable one): the
 * first player wins, by starting in the middle column, and any other
 * first move draws or loses. This computer is far from that -- it
 * searches 7 moves ahead, where solving needs 42 -- but it plays the
 * middle first, which is the one thing everybody knows.
 *
 * A second computer plays on "a", or with the flag ai=network: one
 * that was told the rules only and taught itself by playing itself,
 * AlphaZero's way (its own section below, and Selfplay.mli). And
 * ai=policy is that network with no search at all, to see what it
 * learned by itself.
 *
 * What it uses: the boards kit's Connect4 (the rules, the evaluation,
 * the keys: shared with the program that trains the network); ai/'s
 * Minimax, Deepening (the search) and Zobrist (the table); Mcts,
 * Selfplay and Policy_value for the second computer, whose weights are
 * data/weights/connect4; Scene2d (the keys pressed). Not the puzzle
 * kit: nothing is pushed.
 *
 * Exercises: the endgame searched to the end (with a dozen squares
 * left it is quick); killer moves (a move that cut elsewhere, tried
 * early); thinking while you think, a frame at a time
 * (Deepening.think), instead of all at once when it is its turn.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

(* the rules, what a position is worth, the middle columns first and
 * the keys: the boards kit's Connect4, shared with the program that
 * trains a network to play (scripts/train/train_connect4) *)
include Connect4

(*****************************************************************************)
(* The computer *)
(*****************************************************************************)

let depth = 7

(* the same rules, with the moves listed middle first: ordering is
 * nothing more than that, and this is how it is measured against the
 * plain search (Unit_games has the whole table) *)
let ordered_rules : (position, int) Minimax.game =
  { connect4 with moves = (fun p -> middle_first p (connect4.moves p)) }

(* what it played, what it thinks of your columns, and the two node
 * counts the game shows: alpha-beta as it comes, and the search with
 * every trick on *)
type counts = { plain : int; tricks : int }

let think (p : position) : int option * number list * counts =
  let plain = Minimax.alphabeta connect4 ~depth p in
  let table = Zobrist.table () in
  let tabled = Deepening.search ~order:middle_first ~key ~table connect4 ~depth p in
  (* what it thinks of each of your columns, for the "v" key: two plies
   * shallower, from your side, sharing the table it just filled *)
  let values =
    List.init columns (fun c ->
        if landing p.board c = None then Float.nan
        else (Deepening.search ~order:middle_first ~key ~table connect4 ~depth:(depth -.. 2) (play p c)).value)
  in
  (tabled.best, values, { plain = plain.nodes; tricks = tabled.nodes })

(*****************************************************************************)
(* The other computer: a network that taught itself (ai=network) *)
(*****************************************************************************)
(* Everything above was told to the computer: what a position is
 * worth, which columns to try first. This one was told the rules and
 * nothing else, and learned by playing against itself
 * (Selfplay.mli, notes_ai_learning.md section 16). Its network gives
 * two guesses about a position, which columns look good and who is
 * winning, and the search (Mcts) is guided by both instead of by an
 * evaluation.
 *
 * The network was not trained here: scripts/train/train_connect4 did
 * it, and what it learned is a file of data/weights, whose first
 * lines say how long it trained and how it then did against this
 * game's own alpha-beta.
 *
 * "a" changes who you play, and the flag ai= who you start with:
 * classic (the computer above), network (the search guided by the
 * network), policy (the network alone, its first idea, no looking
 * ahead: what it has learned and nothing more). *)

type engine = Classic | Network | Policy

let engine_of (flags : (string * string) list) : engine =
  match List.assoc_opt "ai" flags with Some "network" -> Network | Some "policy" -> Policy | _ -> Classic

let name (e : engine) : string =
  match e with Classic -> "alpha-beta, told what a position is worth" | Network -> "a network that taught itself, and a search" | Policy -> "the network alone, no search"

let net : Policy_value.t Lazy.t =
  lazy
    (match Result.bind (Weights.of_string Weights_connect4.bytes) Policy_value.of_weights with
    | Ok net -> net
    | Error why -> failwith ("connect4.weights: " ^ why))

let playouts = 400

(* what the network thought of each column before any search, and
 * where the search then spent its playouts: two shares a column *)
type opinion = { before : number list; after : number list }

let think_network (e : engine) (p : position) (seed : int) : int option * opinion =
  let net = Lazy.force net in
  let (prior, _) = Selfplay.guides Connect4.board net in
  let shares = prior p in
  let before = List.init columns (fun c -> match List.assoc_opt c shares with Some s -> s | None -> Float.nan) in
  match e with
  | Policy -> (Selfplay.instinct Connect4.board net p, { before; after = [] })
  | Classic | Network ->
      let tried = Selfplay.visits ~seed ~playouts Connect4.board net p in
      let total = float_of_int (List.fold_left (fun n (_, k) -> n +.. k) 0 tried) in
      let after =
        List.init columns (fun c -> match List.assoc_opt c tried with Some k -> float_of_int k / total | None -> Float.nan)
      in
      let best = List.fold_left (fun b (c, k) -> match b with Some (_, n) when n >= k -> b | _ -> Some (c, k)) None tried in
      (Option.map fst best, { before; after })

(*****************************************************************************)
(* The game *)
(*****************************************************************************)

type game = {
  position : position;
  cursor : int;
  last : int option; (* the column it dropped in *)
  wait : int; (* frames before it answers *)
  counts : counts option;
  values : number list option;
  show_values : bool;
  engine : engine option; (* who plays red; None until the flags are seen *)
  opinion : opinion option; (* the network's, of its last position *)
  moves_played : int;
}

type model = game Scene2d.t

let new_game () : game =
  { position = start; cursor = 3; last = None; wait = 0; counts = None; values = None; show_values = false;
    engine = None; opinion = None; moves_played = 0 }

let initial_model : model = Scene2d.start (new_game ())

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let cell = 100.
let left = -.(float_of_int columns * cell / 2.)
let bottom = -.(float_of_int rows * cell / 2.) - 30.
let column_at (x : number) : int option =
  let c = int_of_float (Float.floor ((x - left) / cell)) in
  if c >= 0 && c < columns then Some c else None

let drop (g : game) (c : int) : game =
  if landing g.position.board c = None || over g.position then g
  else
    { g with position = play g.position c; last = Some c; wait = 20; counts = None; values = None;
      moves_played = g.moves_played +.. 1 }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let m = computer.mouse in
  let engine = match g.engine with Some e -> e | None -> engine_of computer.flags in
  let g = { g with engine = Some engine } in
  if over g.position then
    if Scene2d.pressed (fun k -> k.kspace) scenes then { (new_game ()) with engine = Some engine } else g
  else if g.position.turn = Machine then
    if g.wait > 0 then { g with wait = g.wait -.. 1 }
    else
      match engine with
      | Classic ->
          let (best, values, counts) = think g.position in
          let g = { g with counts = Some counts; values = Some values; opinion = None } in
          (match best with Some c -> { (drop g c) with counts = Some counts; values = Some values } | None -> g)
      | Network | Policy ->
          let (best, opinion) = think_network engine g.position g.moves_played in
          (match best with Some c -> { (drop g c) with opinion = Some opinion } | None -> g)
  else
    let g =
      if Scene2d.pressed (fun k -> Set_.mem "a" k.keys) scenes then
        { g with engine = Some (match engine with Classic -> Network | Network -> Policy | Policy -> Classic); opinion = None }
      else g
    in
    let g = if Scene2d.pressed (fun k -> Set_.mem "v" k.keys) scenes then { g with show_values = not g.show_values } else g in
    let cursor =
      if Scene2d.pressed (fun k -> k.kleft) scenes then max 0 (g.cursor -.. 1)
      else if Scene2d.pressed (fun k -> k.kright) scenes then min (columns -.. 1) (g.cursor +.. 1)
      else g.cursor
    in
    let g = { g with cursor } in
    if Scene2d.pressed (fun k -> k.kspace) scenes then drop g g.cursor
    else if m.mclick then (match column_at m.mx with Some c -> drop { g with cursor = c } c | None -> g)
    else g

let update (computer : computer) (s : model) : model =
  let scenes = Scene2d.update computer s in
  { scenes with scene = update_game computer scenes scenes.scene }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let center (c : int) (r : int) : number * number =
  (left + (float_of_int c * cell) + (cell / 2.), bottom + (float_of_int r * cell) + (cell / 2.))

let view_piece (p : piece) : shape =
  match p with
  | Empty -> circle (rgb 25 30 60) 40.
  | You -> circle (rgb 240 200 70) 40.
  | Machine -> circle (rgb 220 80 70) 40.

let view (computer : computer) (s : model) : shape list =
  let g = s.scene and screen = computer.screen in
  let board =
    List.concat_map
      (fun c ->
        List.map
          (fun r ->
            let (x, y) = center c r in
            view_piece (at g.position.board c r) |> move x y)
          (List.init rows Fun.id))
      (List.init columns Fun.id)
  in
  let hint =
    if not g.show_values then []
    else
      match g.values with
      | None -> []
      | Some values ->
          List.filteri (fun _ _ -> true) values
          |> List.mapi (fun c v ->
                 if Float.is_nan v then group []
                 else
                   let (x, _) = center c 0 in
                   text (if v < 0. then rgb 120 230 140 else rgb 230 130 120) 1.6 (Printf.sprintf "%.0f" v)
                   |> move x (bottom - 30.))
  in
  (* the network's two rows under the board: its first idea of each
   * column, and where the search went *)
  let opinion =
    match g.opinion with
    | None -> []
    | Some o ->
        let row (shares : number list) (y : number) (color : color) : shape list =
          List.mapi
            (fun c s ->
              if Float.is_nan s then group []
              else text color 1.5 (Printf.sprintf "%.0f%%" (100. * s)) |> move (fst (center c 0)) y)
            shares
        in
        row o.before (bottom - 22.) (rgb 150 160 200)
        @ row o.after (bottom - 47.) (rgb 240 210 120)
        @ [ text (rgb 170 170 190) 1.4
              (if o.after = [] then "its policy: what it thinks of each column, at a glance"
               else Printf.sprintf "its policy at a glance, and under it where %d playouts then went" playouts)
            |> move_y (-402.) ]
  in
  let told =
    match g.counts with
    | None -> []
    | Some c ->
        [ text (rgb 170 170 190) 1.6 (Printf.sprintf "alpha-beta, the columns in order: %d positions" c.plain)
          |> move_y (-350.);
          text (rgb 170 170 190) 1.6
            (Printf.sprintf "middle first, deepening 1 to %d, with the table: %d  (%.0f%% of it)" depth c.tricks
               (100. * float_of_int c.tricks / Float.max 1. (float_of_int c.plain)))
          |> move_y (-380.) ]
  in
  let over_text =
    if four g.position.board Machine then [ text (rgb 220 80 70) 3. "RED WINS" |> move_y 380. ]
    else if four g.position.board You then [ text (rgb 240 200 70) 3. "YOU WIN" |> move_y 380. ]
    else if over g.position then [ text white 3. "A DRAW" |> move_y 380. ]
    else []
  in
  [ rectangle (rgb 18 22 40) screen.width screen.height;
    rectangle (rgb 40 70 160) (float_of_int columns * cell) (float_of_int rows * cell) |> move_y (bottom + (float_of_int rows * cell / 2.)) ]
  @ board
  @ (if g.position.turn = You && not (over g.position) then
       [ view_piece You |> move (fst (center g.cursor rows)) (bottom + (float_of_int rows * cell) + 40.) |> fade 0.6 ]
     else [])
  @ hint @ opinion @ told @ over_text
  @ [ text white 2.5 "CONNECT 4" |> move_y 440.;
      text (rgb 150 150 170) 1.3
        ("red is " ^ name (match g.engine with Some e -> e | None -> engine_of computer.flags) ^ "   (a: another)")
      |> move_y (-427.);
      text (rgb 150 150 170) 1.6 "click a column, or the arrows and space;  v: what it thinks of yours" |> move_y (-455.) ]

let app = game view update initial_model
let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) app)
