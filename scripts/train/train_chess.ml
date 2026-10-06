(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Teaches a network chess, and writes what it learned: AiChess's
 * network. In two stages, which are the two AlphaGos:
 *
 *   dune exec scripts/train/train_chess.exe -- teach data/weights/chess/chess.weights 30
 *   dune exec scripts/train/train_chess.exe -- play data/weights/chess/chess.weights 50 data/weights/chess/chess.weights
 *   WORKERS=8 dune exec ...            (processes at once)
 *
 * **teach**: from a teacher, as the first AlphaGo (2016) started from
 * the games of human masters. The teacher here is AiChess's own
 * computer, alpha-beta three half-moves ahead with quiescence: it
 * plays games against itself (their first moves, and one in ten after,
 * made at random, so that no two are alike), and each position it
 * moves in is a lesson -- its move for the policy, its judgment of the
 * position for the value (the search's score, squeezed between -1 and
 * 1: a knight ahead is about a half).
 *
 * **play**: from there by self-play (Alphazero.mli), as AlphaGo Zero
 * (2017) did from nothing: games against itself at 100 playouts a
 * move, each of at most 160 half-moves and then given to whoever is a
 * knight ahead (Chess.board).
 *
 * Self-play from nothing was tried first, and is what [play] without
 * a file to start from still does. In twenty iterations, two hours,
 * the network got *worse* than it began: 3-3-14 against a player
 * moving at random, which it had beaten 14-4-2 knowing nothing (the
 * search alone sees the end of a game coming). Looked at: its value
 * was 0 for every position, eleven of its twelve games against itself
 * were draws, and it left a queen hanging for a pawn's push. Two
 * players who know nothing do not win games of chess, nearly every
 * game was a draw, a value taught only draws says "a draw", and a
 * search told every move is a draw spends its visits where the policy
 * already points -- which is then what the policy is taught
 * (notes_ai_dark_arts.md). DeepMind's had 44 million games to get out
 * of that; this one has a teacher.
 *
 * The network reads the board as a board (Policy_value.mli, Chess.mli):
 * 17 planes of 8 by 8 through four convolutions of 16 channels (after
 * four a square has heard from the whole board, a rook's move away),
 * to a policy read off the squares, 64 scores each, a square left and
 * a square reached, and a value. An iteration is its games, then the
 * steps of 32 learners apart, 300 each, their networks averaged. The
 * loop, its processes and its file are Alphazero_trainer's.
 *
 * An honest word on scale. DeepMind's AlphaZero played 44 million
 * games of chess on 5,000 special processors, with a network of some
 * 20 million numbers and 800 playouts a move. This is a few thousand
 * games on one computer's processors, fifteen thousand numbers, 100
 * playouts. What can be hoped for is a player that takes what is
 * offered and holds on to its pieces, and the measure says how far
 * that goes.
 *
 * Measured every fifth iteration, over 20 games, the network white in
 * half, each game from its own two random opening moves, with the
 * search guided by the network and with the network alone:
 *
 *  - against a player moving at random;
 *  - against AiChess's own computer, alpha-beta with quiescence,
 *    looking 1, 2 and 3 half-moves ahead (3 is the game's, and the
 *    teacher): the depth it first loses to is its grade.
 *
 * A measured game is stopped as a taught one is, at 160 half-moves. *)

let playouts = 100
let longest = 160
let board = Chess.board ~longest

type state = Chess.position * int

(* a player whose first move of a game is any legal one: with the
 * other player's, two random opening moves a game *)
let opening (player : (state, Chess.move) Arena.player) : (state, Chess.move) Arena.player =
 fun ~seed ((_, moves) as s) -> if moves < 2 then Arena.random board.game ~seed s else player ~seed s

(* AiChess's computer, looking so many half-moves ahead *)
let alphabeta (depth : int) : (state, Chess.move) Arena.player =
 fun ~seed:_ (p, _) -> Option.get (Chess.search ~ordered:true ~quiescence:true ~depth p).best

(* the network's share of game [n] against [other], white in the even
 * games (white is MAX) *)
let versus (mine : (state, Chess.move) Arena.player) (other : (state, Chess.move) Arena.player) (n : int) : float =
  let game = Arena.game board.game board.start ~seed:n in
  if n mod 2 = 0 then game ~max:(opening mine) ~min:(opening other) else 1. -. game ~max:(opening other) ~min:(opening mine)

let measure (net : Policy_value.t) : string =
  let searching : (state, Chess.move) Arena.player = fun ~seed s -> Option.get (Alphazero.choose ~playouts ~seed board net s) in
  let alone : (state, Chess.move) Arena.player = fun ~seed:_ s -> Option.get (Alphazero.instinct board net s) in
  let random = Arena.random board.game in
  let score mine other = Alphazero_trainer.score ~games:20 (versus mine other) in
  Printf.sprintf "with the search: random %s, depth 1 %s, depth 2 %s, depth 3 %s; alone: random %s, depth 1 %s" (score searching random)
    (score searching (alphabeta 1))
    (score searching (alphabeta 2))
    (score searching (alphabeta 3))
    (score alone random)
    (score alone (alphabeta 1))

(* a game of the teacher against itself, and a lesson for each position
 * it moved in: the move it chose, and what it thought the position
 * worth for whoever was to play *)
let teacher_depth = 3

let taught ~(seed : int) (_ : Policy_value.t) : Policy_value.lesson list =
  let dice = Lehmer.make seed in
  let rec go ((p, n) as s : state) (lessons : Policy_value.lesson list) : Policy_value.lesson list =
    match board.game.moves s with
    | [] -> lessons
    | moves ->
        let a = Chess.search ~ordered:true ~quiescence:true ~depth:teacher_depth p in
        let best = Option.get a.best in
        let lessons =
          (* a promotion to less than a queen has no place in the policy *)
          if not (List.mem best moves) then lessons
          else
            let policy = Array.make board.moves 0. in
            policy.(board.index s best) <- 1.;
            let mine = if p.turn = Chess.White then a.value else -.a.value in
            { Policy_value.input = board.encode s; policy; value = tanh (mine /. 600.) } :: lessons
        in
        (* the first four half-moves and one in ten after are any legal
         * move: the teacher alone plays one game *)
        let move =
          if n < 4 || Lehmer.float dice 1. < 0.1 || not (List.mem best moves) then
            List.nth moves (Lehmer.int dice (List.length moves))
          else best
        in
        go (board.game.play s move) lessons
  in
  go board.start []

let () =
  let usage = "usage: train_chess teach|play <out.weights> [iterations] [from.weights]" in
  let teaching = match if Array.length Sys.argv > 1 then Sys.argv.(1) else "" with "teach" -> true | "play" -> false | _ -> failwith usage in
  let out = if Array.length Sys.argv > 2 then Sys.argv.(2) else failwith usage in
  let iterations = if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else 30 in
  let from = if Array.length Sys.argv > 4 then Some Sys.argv.(4) else None in
  let seed = 1 in
  let shape : Policy_value.board = { planes = Chess.planes; height = 8; width = 8; channels = 16; layers = 4; per_square = 64 } in
  Alphazero_trainer.run ~out ~iterations ~from
    ~fresh:(fun () -> Policy_value.make ~seed ~rate:0.003 ~board:shape ~inputs:board.inputs ~moves:board.moves ())
    {
      board;
      settings = { Alphazero.default with playouts; exploring = 16 };
      games = (if teaching then 960 else 192);
      source = (if teaching then Some taught else None);
      remembered = 300_000;
      also = (fun _ -> []);
      steps = 300;
      batch = 64;
      learners = 32;
      measure;
      every = 5;
      notes =
        [
          ( "model",
            "Policy_value, a board: 17 planes of 8 by 8, four convolutions of 16 channels, a policy of 64 scores a square (4,096 pairs of squares), and a value" );
          ("game", "chess (gamekits/boards/Chess), games of at most 160 half-moves, then given to whoever is a knight ahead");
          ("trainer", "scripts/train/train_chess " ^ if teaching then "teach" else "play");
          ("seed", string_of_int seed);
        ];
    }
