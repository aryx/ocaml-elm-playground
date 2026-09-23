(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Mcts: tic-tac-toe played by random playouts alone -- no
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
  (* ten playouts is still mostly a guess -- eight of twenty, where
     five would be chance. (It was five before [plan] learned to break
     a tie by the win rate: at ten playouts most children have one
     visit each, so the tie-break is nearly the whole answer.) *)
  Alcotest.(check int) "with 10, it blocks 8 times out of 20: barely better than chance" 8 few

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

(*****************************************************************************)
(* And where a network goes (Mcts.mli, "Where a network goes") *)
(*****************************************************************************)

(* A perfect value function, by minimax: what this position is really
   worth to X, between 0 and 1. It stands in for a trained value head,
   so that what the *hook* buys can be measured without a network's
   noise on top of it. *)
let truth (b : board) : float =
  let r = Minimax.alphabeta tictactoe ~depth:9 b in
  (r.value +. 1.) /. 2.

(* What the hook buys, measured over positions rather than asserted
   from one: play random legal games, keep the positions where the
   side to move has a forced win, and count how often each search
   finds a winning move at the same small number of playouts. *)
let forced_wins (n : int) : board list =
  let st = Random.State.make [| 7 |] in
  let rec collect got tries =
    if List.length got >= n || tries > 400 then List.rev got
    else
      let rec play b =
        if tictactoe.moves b = [] then None
        else
          let r = Minimax.alphabeta tictactoe ~depth:9 b in
          let wins_for_mover = if b.x_to_play then r.value > 0.5 else r.value < -0.5 in
          if wins_for_mover && List.length (empty_squares b) <= 6 then Some b
          else
            let moves = tictactoe.moves b in
            play (tictactoe.play b (List.nth moves (Random.State.int st (List.length moves))))
      in
      collect (match play (board ".........") with Some b -> b :: got | None -> got) (tries + 1)
  in
  collect [] 0

let wins_from (b : board) (m : int) : bool =
  let after = tictactoe.play b m in
  let r = Minimax.alphabeta tictactoe ~depth:9 after in
  if b.x_to_play then r.value > 0.5 else r.value < -0.5

let test_value_head () =
  let positions = forced_wins 12 in
  let right ~value playouts =
    List.length
      (List.filter
         (fun b ->
           let r =
             if value then Mcts.search ~seed:3 ~evaluate:truth tictactoe ~playouts b
             else Mcts.search ~seed:3 tictactoe ~playouts b
           in
           match r.best with Some m -> wins_from b m | None -> false)
         positions)
  in
  let n = List.length positions in
  let by_value = right ~value:true 12 and by_playouts = right ~value:false 12 in
  Printf.eprintf "value head: at 12 playouts, %d of %d won positions found with an opinion, %d with random games\n"
    by_value n by_playouts;
  Alcotest.(check int) "with an opinion, every one of them" n by_value;
  Alcotest.(check int) "with random games at the same budget, ten" 10 by_playouts;
  (* And a surprise worth keeping. From an empty board the random
     playouts pick the centre, which everyone knows is right. The
     *perfect* evaluation does not: with best play every opening move
     draws, so all nine are worth exactly 0.5 and it takes any of
     them. "The centre is best" is not a fact about tic-tac-toe, it is
     a fact about opponents who make mistakes -- which is what random
     playouts measure and a perfect value function has no opinion
     about. *)
  let empty = board "........." in
  Alcotest.(check bool) "every opening move is in truth a draw" true
    (List.for_all (fun m -> Float.abs (truth (tictactoe.play empty m) -. 0.5) < 0.001) (empty_squares empty));
  let r = Mcts.search ~seed:3 ~evaluate:truth tictactoe ~playouts:200 empty in
  (* so it takes one of the nine, and there is no reason for it to be
     the centre. (The shares it reports are not all 0.5: a child's
     share averages every position under it, including the ones after
     a mistake -- which is exactly the information a perfect value
     function throws away and random playouts keep.) *)
  Alcotest.(check bool) "and the search takes one of them" true (r.best <> None);
  Alcotest.(check (option int)) "while random playouts prefer the centre" (Some 4)
    (Mcts.search ~seed:3 tictactoe ~playouts:2000 (board ".........")).best

(* a policy says what to look at first: given one that points at the
   winning move, the search spends its visits there instead of
   spreading them evenly *)
let test_policy_prior () =
  let position = board "xx.oo...." in
  let flat (b : board) = List.map (fun m -> (m, 1. /. float_of_int (List.length (empty_squares b)))) (empty_squares b) in
  let pointed (b : board) = List.map (fun m -> (m, if m = 2 then 0.9 else 0.02)) (empty_squares b) in
  let share prior =
    let r = Mcts.search ~seed:3 ~prior tictactoe ~playouts:60 position in
    let (_, visits, _) = List.find (fun (m, _, _) -> m = 2) r.tried in
    float_of_int visits /. 60.
  in
  let even = share flat and aimed = share pointed in
  Printf.eprintf "puct: the winning move took %.0f%% of the visits with a flat policy, %.0f%% with a pointed one\n"
    (100. *. even) (100. *. aimed);
  (* 73% with a flat policy -- PUCT already follows what is winning --
     and 93% with one that points *)
  Alcotest.(check bool) "a pointed policy concentrates the search" true (aimed > even +. 0.15);
  Alcotest.(check (option int)) "and it still finds the win" (Some 2)
    (Mcts.search ~seed:3 ~prior:pointed tictactoe ~playouts:60 position).best

(* both together -- AlphaGo's shape -- against the 2006 version of the
   same search, twenty games at forty playouts each, alternating who
   starts *)
let test_alphago_shape () =
  (* a policy is a distribution, and PUCT means it: priors that do not
     sum to 1 make the exploring term swamp the win rate, and the
     search spreads its visits evenly over good moves and bad. This
     test found that out the hard way. *)
  let flat (b : board) =
    let k = float_of_int (List.length (empty_squares b)) in
    List.map (fun m -> (m, 1. /. k)) (empty_squares b)
  in
  let play_game ~(zero_first : bool) (seed : int) : int =
    let rec go b n =
      if tictactoe.moves b = [] || n > 9 then if won b 1 then 1 else if won b 2 then -1 else 0
      else
        let zero_turn = if zero_first then b.x_to_play else not b.x_to_play in
        let move =
          if zero_turn then (Mcts.search ~seed ~evaluate:truth ~prior:flat tictactoe ~playouts:40 b).best
          else (Mcts.search ~seed:(seed + 1) tictactoe ~playouts:40 b).best
        in
        match move with None -> 0 | Some m -> go (tictactoe.play b m) (n + 1)
    in
    go (board ".........") 0
  in
  let (zero_wins, random_wins) =
    List.fold_left
      (fun (z, r) (seed, zero_first) ->
        let result = play_game ~zero_first seed in
        let zero_won = if zero_first then result = 1 else result = -1 in
        let random_won = if zero_first then result = -1 else result = 1 in
        ((if zero_won then z + 1 else z), if random_won then r + 1 else r))
      (0, 0)
      (List.init 20 (fun i -> (i, i mod 2 = 0)))
  in
  Printf.eprintf "alphago shape: %d wins for the one with an opinion, %d for random playouts, over 20 games\n"
    zero_wins random_wins;
  (* tic-tac-toe is a draw between good players, so the result to want
     is not wins but never losing *)
  Alcotest.(check int) "the searcher with an opinion never loses" 0 random_wins;
  Alcotest.(check int) "and wins eleven of the twenty" 11 zero_wins

let tests =
  Testo.categorize "Mcts"
    [
      t "tic-tac-toe: the centre, from playouts alone" test_centre;
      t "it blocks a threat, and takes a win" test_block;
      t "more playouts, better play" test_more_is_better;
      t "the same seed, and thinking a frame at a time" test_anytime;
      t "a value head instead of playouts" test_value_head;
      t "a policy that points the search" test_policy_prior;
      t "both, against the 2006 version" test_alphago_shape;
    ]
