(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Teaches a network Connect 4 by self-play (Alphazero.mli), and writes
 * what it learned: AiConnect4's network.
 *
 *   dune exec scripts/train/train_connect4.exe -- data/weights/connect4/connect4.weights 150
 *   dune exec scripts/train/train_connect4.exe -- out.weights 10               (iterations)
 *   dune exec scripts/train/train_connect4.exe -- out.weights 10 out.weights   (go on from a file)
 *   WORKERS=8 dune exec ...                                                    (processes playing at once)
 *   NET=board dune exec ...                                                    (convolutions: the board as a board)
 *
 * An iteration is 480 games against itself at 100 playouts a move,
 * each lesson learned also as in a mirror (a board turned left to
 * right is as good a lesson, with the policy turned too), then 600
 * steps on batches of 64 drawn from the last 100,000 lessons. The
 * loop, its processes and its file are Alphazero_trainer's.
 *
 * With NET=board the network reads the board as a board (two
 * convolutions of 16 channels, Policy_value.mli) and its steps are
 * taken by 32 learners apart, 300 each, their networks averaged: 85
 * minutes for 100 iterations. The weights in data/weights/connect4
 * are the flat network's, 150 iterations in 38 minutes; the board
 * network's result is in notes_ai_learning.md, section 16.
 *
 * After AlphaZero.jl's Connect Four tutorial, which measures as it
 * goes against players that learn nothing, and so does this, every
 * fifth iteration:
 *
 *  - the same search with no network: Monte Carlo tree search with
 *    random playouts, as many of them (Mcts.mli);
 *  - the game's own computer: alpha-beta with its evaluation, at
 *    depths 1, 3, 5 and 7 (AiConnect4 plays at 7);
 *
 * both with the search guided by the network and with the network
 * alone, its policy's first choice. Won-drawn-lost over 20 games, the
 * network moving first in half, each game from its own two random
 * opening moves -- or two players without dice play the same game
 * twenty times. *)

let playouts = 100
let board = Connect4.board
let shown (s : Arena.score) : string = Printf.sprintf "%d-%d-%d" s.won s.drawn s.lost

(* a lesson as in a mirror: the columns turned left to right, in the
 * position and in the policy *)
let mirrored (l : Policy_value.lesson) : Policy_value.lesson =
  let squares = Connect4.columns * Connect4.rows in
  let input =
    Array.init (2 * squares) (fun i ->
        let plane = i / squares and at = i mod squares in
        let column = at / Connect4.rows and row = at mod Connect4.rows in
        l.input.((plane * squares) + ((Connect4.columns - 1 - column) * Connect4.rows) + row))
  in
  { l with input; policy = Array.init Connect4.columns (fun c -> l.policy.(Connect4.columns - 1 - c)) }

(* a player whose first move of a game is any legal one: with the
 * other player's, two random opening moves a game *)
let opening (player : (Connect4.position, int) Arena.player) : (Connect4.position, int) Arena.player =
 fun ~seed p ->
  let pieces = Array.fold_left (fun n piece -> if piece = Connect4.Empty then n else n + 1) 0 p.board in
  if pieces < 2 then Arena.random Connect4.connect4 ~seed p else player ~seed p

let plain_mcts : (Connect4.position, int) Arena.player =
 fun ~seed p -> Option.get (Mcts.search ~seed Connect4.connect4 ~playouts p).best

let measure (net : Policy_value.t) : string =
  let searching : (Connect4.position, int) Arena.player = fun ~seed p -> Option.get (Alphazero.choose ~playouts ~seed board net p) in
  let alone : (Connect4.position, int) Arena.player = fun ~seed:_ p -> Option.get (Alphazero.instinct board net p) in
  let against a b () = shown (Arena.play Connect4.connect4 Connect4.start ~a:(opening a) ~b:(opening b) ~games:20) in
  match
    Alphazero_trainer.together
      [ against searching plain_mcts; against searching (Connect4.alphabeta ~depth:1);
        against searching (Connect4.alphabeta ~depth:3); against searching (Connect4.alphabeta ~depth:5);
        against searching (Connect4.alphabeta ~depth:7); against alone plain_mcts;
        against alone (Connect4.alphabeta ~depth:1) ]
  with
  | [ m; a1; a3; a5; a7; im; i1 ] ->
      Printf.sprintf "with the search: mcts %s, alpha-beta 1 %s, 3 %s, 5 %s, 7 %s; alone: mcts %s, alpha-beta 1 %s" m a1 a3
        a5 a7 im i1
  | _ -> assert false

let () =
  let out =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else failwith "usage: train_connect4 <out.weights> [iterations] [from.weights]"
  in
  let iterations = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 60 in
  let from = if Array.length Sys.argv > 3 then Some Sys.argv.(3) else None in
  let seed = 1 in
  let as_board = Sys.getenv_opt "NET" = Some "board" in
  let fresh () : Policy_value.t =
    if as_board then
      (* the board as a board: 7 columns of 6, two planes *)
      let shape : Policy_value.board =
        { planes = 2; height = Connect4.columns; width = Connect4.rows; channels = 16; layers = 2; per_square = 0 }
      in
      Policy_value.make ~seed ~rate:0.003 ~board:shape ~inputs:board.inputs ~moves:board.moves ()
    else Policy_value.make ~seed ~hidden:128 ~rate:0.003 ~inputs:board.inputs ~moves:board.moves ()
  in
  Alphazero_trainer.run ~fresh ~out ~iterations ~from
    {
      board;
      settings = { Alphazero.default with playouts; exploring = 8 };
      games = 480;
      source = None;
      remembered = 100_000;
      also = (fun l -> [ mirrored l ]);
      steps = (if as_board then 300 else 600);
      batch = 64;
      learners = (if as_board then 32 else 1);
      measure;
      every = 5;
      notes =
        [
          ( "model",
            if as_board then "Policy_value, a board: 2 planes of 7 by 6, two convolutions of 16 channels, a policy over 7 columns and a value"
            else "Policy_value: 84 numbers in, two layers of 128, a policy over 7 columns and a value" );
          ("game", "Connect 4 (gamekits/boards/Connect4)");
          ("trainer", "scripts/train/train_connect4");
          ("seed", string_of_int seed);
        ];
    }
