(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Teaches a network Go on 9 by 9 by self-play (Alphazero.mli), and
 * writes what it learned: AiGo's network.
 *
 *   dune exec scripts/train/train_go.exe -- data/weights/go9/go9.weights 100
 *   dune exec scripts/train/train_go.exe -- out.weights 10               (iterations)
 *   dune exec scripts/train/train_go.exe -- out.weights 10 out.weights   (go on from a file)
 *   WORKERS=8 dune exec ...                                              (processes playing at once)
 *
 * The network reads the board as a board (Policy_value.mli): three
 * planes of 9 by 9 -- my stones, the other's, the ko -- through
 * convolutions, to a policy over the 81 points and the pass, and a
 * value. An iteration is 192 games against itself, each of at most
 * 150 moves (two players who know nothing do not pass, and would
 * capture each other for ever), each lesson learned also under three
 * of the board's eight symmetries; then the steps of 32 learners
 * apart, 300 each, their networks averaged. The loop, its processes
 * and its file are Alphazero_trainer's.
 *
 * It is told what the random playouts of AiGo are told and no more:
 * the rules, and not to fill an eye of its own.
 *
 * Measured every tenth iteration, over 20 games, the network moving
 * first in half, each game from its own two random opening moves,
 * against players that learn nothing:
 *
 *  - one playing at random (not into its own eyes);
 *  - Monte Carlo tree search with random playouts, as many as the
 *    network's search has (100), and ten times as many (1,000: AiGo's
 *    own computer);
 *
 * with the search guided by the network, and with the network alone,
 * its policy's first choice. *)

let playouts = 100
let longest = 150
let board = Go9.capped ~longest

type state = Go9.position * int

(* a player whose first move of a game is any legal one: with the
 * other player's, two random opening moves a game *)
let opening (player : (state, Go9.move) Arena.player) : (state, Go9.move) Arena.player =
 fun ~seed ((_, moves) as s) -> if moves < 2 then Arena.random board.game ~seed s else player ~seed s

(* the search without a network: random games played out, AiGo's way *)
let plain_mcts (playouts : int) : (state, Go9.move) Arena.player =
 fun ~seed s ->
  let playout st _ ((p, n) : state) : state = (Go9.playout st Go9.go p, n) in
  Option.get (Mcts.search ~seed ~playout board.game ~playouts s).best

(* the network's share of game [n] against [other], first in the even
 * games (black moves first, and white is MAX) *)
let versus (mine : (state, Go9.move) Arena.player) (other : (state, Go9.move) Arena.player) (n : int) : float =
  let game = Arena.game board.game board.start ~seed:n in
  if n mod 2 = 0 then 1. -. game ~max:(opening other) ~min:(opening mine) else game ~max:(opening mine) ~min:(opening other)

let measure (net : Policy_value.t) : string =
  let searching : (state, Go9.move) Arena.player = fun ~seed s -> Option.get (Alphazero.choose ~playouts ~seed board net s) in
  let alone : (state, Go9.move) Arena.player = fun ~seed:_ s -> Option.get (Alphazero.instinct board net s) in
  let random = Arena.random board.game in
  let score mine other = Alphazero_trainer.score ~games:20 (versus mine other) in
  Printf.sprintf "with the search: random %s, mcts %d %s, mcts 1000 %s; alone: random %s, mcts %d %s" (score searching random)
    playouts
    (score searching (plain_mcts playouts))
    (score searching (plain_mcts 1000))
    (score alone random) playouts
    (score alone (plain_mcts playouts))

let () =
  let out = if Array.length Sys.argv > 1 then Sys.argv.(1) else failwith "usage: train_go <out.weights> [iterations] [from.weights]" in
  let iterations = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 100 in
  let from = if Array.length Sys.argv > 3 then Some Sys.argv.(3) else None in
  let seed = 1 in
  let shape : Policy_value.board = { planes = 3; height = Go9.size; width = Go9.size; channels = 16; layers = 2 } in
  Alphazero_trainer.run ~out ~iterations ~from
    ~fresh:(fun () -> Policy_value.make ~seed ~rate:0.003 ~board:shape ~inputs:board.inputs ~moves:board.moves ())
    {
      board;
      settings = { Alphazero.default with playouts; exploring = 16 };
      games = 192;
      remembered = 300_000;
      also = (fun l -> [ Go9.lesson_turned 1 l; Go9.lesson_turned 2 l; Go9.lesson_turned 4 l ]);
      steps = 300;
      batch = 64;
      learners = 32;
      measure;
      every = 10;
      notes =
        [
          ("model", "Policy_value, a board: 3 planes of 9 by 9, two convolutions of 16 channels, a policy over 81 points and the pass, and a value");
          ("game", "Go on 9 by 9 (gamekits/boards/Go9), games of at most 150 moves");
          ("trainer", "scripts/train/train_go");
          ("seed", string_of_int seed);
        ];
    }
