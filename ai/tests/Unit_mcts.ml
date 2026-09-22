(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* ai/Mcts: tic-tac-toe played by random playouts alone -- no
 * evaluation function anywhere in this file *)

let t = Testo.create

(* the board, 9 squares: 0 empty, 1 X (MAX), 2 O *)
type board = { squares : int array; x_to_play : bool }

let lines = [ [ 0; 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7; 8 ]; [ 0; 3; 6 ]; [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 0; 4; 8 ]; [ 2; 4; 6 ] ]
let won (b : board) (who : int) : bool = List.exists (fun l -> List.for_all (fun i -> b.squares.(i) = who) l) lines

let empty_squares (b : board) : int list = List.filter (fun i -> b.squares.(i) = 0) (List.init 9 Fun.id)

let tictactoe : (board, int) Minimax.game =
  {
    moves = (fun b -> if won b 1 || won b 2 then [] else empty_squares b);
    play =
      (fun b i ->
        let squares = Array.copy b.squares in
        squares.(i) <- (if b.x_to_play then 1 else 2);
        { squares; x_to_play = not b.x_to_play });
    score = (fun b -> if won b 1 then 1. else if won b 2 then -1. else 0.);
    max_to_play = (fun b -> b.x_to_play);
  }

let board (s : string) : board =
  let squares = Array.init 9 (fun i -> match s.[i] with 'x' -> 1 | 'o' -> 2 | _ -> 0) in
  let xs = Array.fold_left (fun n v -> if v = 1 then n + 1 else n) 0 squares in
  let os = Array.fold_left (fun n v -> if v = 2 then n + 1 else n) 0 squares in
  { squares; x_to_play = xs = os }

(* from an empty board, MCTS plays the centre -- the move every
 * tic-tac-toe player knows, found here with no knowledge at all *)
let test_centre () =
  let r = Mcts.search tictactoe ~playouts:2000 (board ".........") in
  Alcotest.(check (option int)) "the centre" (Some 4) r.best;
  Alcotest.(check int) "it played what it was asked" 2000 r.playouts;
  Alcotest.(check bool) "and grew a tree" true (r.nodes > 100);
  Alcotest.(check int) "it tried all nine moves" 9 (List.length r.tried);
  (* the most visited move is also the one it believes in *)
  let (_, visits, share) = List.find (fun (m, _, _) -> m = 4) r.tried in
  Alcotest.(check bool) "the centre got the most visits" true (List.for_all (fun (_, v, _) -> v <= visits) r.tried);
  Alcotest.(check bool) "and X wins more than half the playouts through it" true (share > 0.5)

(* X to play, with O about to make three in a row: it must block, and
 * it does, though nothing told it what a threat is *)
let test_block () =
  List.iter
    (fun (position, must, what) ->
      let r = Mcts.search tictactoe ~playouts:3000 (board position) in
      Alcotest.(check (option int)) what (Some must) r.best)
    (* X has nothing of its own to finish in these, so blocking is the
       only move worth anything -- a first version of this test used a
       position where X could win at once instead, and MCTS rightly
       took the win *)
    [ ("oo.x....x", 2, "block the top row"); ("x..o.o.x.", 4, "block the middle row"); (".x.ox.o..", 7, "take the win") ]

(* more playouts, better play: with ten it is guessing *)
let test_more_is_better () =
  let blocks playouts =
    List.length
      (List.filter
         (fun seed -> (Mcts.search ~seed tictactoe ~playouts (board "oo.x....x")).best = Some 2)
         (List.init 20 Fun.id))
  in
  let few = blocks 10 and many = blocks 2000 in
  Alcotest.(check int) "with 2000 playouts it always blocks" 20 many;
  Alcotest.(check int) "with 10, it blocks 5 times out of 20: a guess" 5 few

(* the same seed plays the same game; anytime: growing the tree in
 * pieces is growing the same tree *)
let test_anytime () =
  let one = Mcts.search ~seed:5 tictactoe ~playouts:600 (board ".........") in
  let two = Mcts.search ~seed:5 tictactoe ~playouts:600 (board ".........") in
  Alcotest.(check (option int)) "the same seed, the same move" one.best two.best;
  let t = Mcts.start ~seed:5 tictactoe (board ".........") in
  let t = List.fold_left (fun t _ -> Mcts.think ~playouts:100 t) t (List.init 6 Fun.id) in
  let piecemeal = Mcts.plan t in
  Alcotest.(check int) "600 playouts, six frames of 100" 600 piecemeal.playouts;
  Alcotest.(check (option int)) "the same answer as all at once" one.best piecemeal.best;
  (* usable after the first frame *)
  let early = Mcts.plan (Mcts.think ~playouts:20 (Mcts.start tictactoe (board "........."))) in
  Alcotest.(check bool) "after 20 playouts: an answer already" true (early.best <> None)

let tests =
  Testo.categorize "Mcts"
    [
      t "tic-tac-toe: the centre, from playouts alone" test_centre;
      t "it blocks a threat, and takes a win" test_block;
      t "more playouts, better play" test_more_is_better;
      t "the same seed, and thinking a frame at a time" test_anytime;
    ]
