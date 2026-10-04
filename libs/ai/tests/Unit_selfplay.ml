(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* self-play: a network that teaches itself tic-tac-toe, checked
 * against the game's truth *)

let t = Testo.create

let board : (Tictactoe.position, int) Selfplay.board =
  { game = Tictactoe.game; start = Tictactoe.start; inputs = 18; moves = 9; encode = Tictactoe.encode; index = (fun m -> m) }

(* the truth: minimax to the end of the game *)
let perfect : (Tictactoe.position, int) Arena.player =
 fun ~seed:_ state -> Option.get (Minimax.minimax Tictactoe.game ~depth:9 state).best

let random = Arena.random Tictactoe.game
let against (a : (Tictactoe.position, int) Arena.player) b games = Arena.play Tictactoe.game Tictactoe.start ~a ~b ~games
let shown (s : Arena.score) : string = Printf.sprintf "%d-%d-%d" s.won s.drawn s.lost

let test_tictactoe () =
  let p = Tictactoe.of_string "xx.oo...." in
  Alcotest.(check bool) "x has played twice and o twice: x to play" true (p.turn = Tictactoe.X);
  Alcotest.(check (list int)) "the empty squares" [ 2; 5; 6; 7; 8 ] (Tictactoe.game.moves p);
  let won = Tictactoe.game.play p 2 in
  Alcotest.(check string) "x completes the row" "xxxoo...." (Tictactoe.to_string won);
  Alcotest.(check (list int)) "and the game is over" [] (Tictactoe.game.moves won);
  Alcotest.(check (float 0.)) "won by x" 1. (Tictactoe.game.score won);
  (* the same situation seen from either side reads the same *)
  Alcotest.(check (array (float 0.))) "x's two marks, as the player to move sees them"
    (Tictactoe.encode (Tictactoe.of_string "xx.o.....")) (* o to play: theirs are x's *)
    (Array.init 18 (fun i -> if i = 3 then 1. else if i = 9 || i = 10 then 1. else 0.));
  (* with best play it is a draw, and the perfect player says so *)
  Alcotest.(check (float 0.)) "the value of the empty board" 0. (Minimax.minimax Tictactoe.game ~depth:9 Tictactoe.start).value;
  Alcotest.(check string) "perfect against perfect" "0-6-0" (shown (against perfect perfect 6))

let test_network () =
  let net = Policy_value.make ~seed:1 ~inputs:18 ~moves:9 () in
  Alcotest.(check int) "18-64-64 and two heads" 6026 (Policy_value.parameters net);
  let (p, v) = Policy_value.opinion net (Tictactoe.encode Tictactoe.start) in
  Alcotest.(check (float 1e-9)) "a share per move" 1. (Array.fold_left ( +. ) 0. p);
  Alcotest.(check bool) "a value between -1 and 1" true (v > -1. && v < 1.);
  (* the search's two guesses made of it: the legal moves only *)
  let (prior, evaluate) = Selfplay.guides board net in
  let position = Tictactoe.of_string "xx.oo...." in
  let shares = prior position in
  Alcotest.(check (list int)) "a share for each legal move" [ 2; 5; 6; 7; 8 ] (List.map fst shares);
  Alcotest.(check (float 1e-9)) "summing to 1 again" 1. (List.fold_left (fun s (_, x) -> s +. x) 0. shares);
  Alcotest.(check bool) "MAX's share, between 0 and 1" true (evaluate position >= 0. && evaluate position <= 1.);
  (* one lesson, repeated: both heads come to say it *)
  let lesson : Policy_value.lesson =
    { input = Tictactoe.encode position; policy = Array.init 9 (fun i -> if i = 2 then 1. else 0.); value = 1. }
  in
  let rec learn net n = if n = 0 then net else learn (fst (Policy_value.step ~rate:0.01 net [| lesson |])) (n - 1) in
  let before = Policy_value.loss net [| lesson |] in
  let taught = learn net 100 in
  let (p, v) = Policy_value.opinion taught lesson.input in
  Alcotest.(check bool) "the loss fell" true (Policy_value.loss taught [| lesson |] < before /. 10.);
  Alcotest.(check bool) "the move it was shown" true (p.(2) > 0.9);
  Alcotest.(check bool) "and that it wins" true (v > 0.9);
  (* written and read back *)
  match Result.bind (Weights.of_string (Weights.to_string (Policy_value.to_weights taught))) Policy_value.of_weights with
  | Error why -> Alcotest.fail why
  | Ok back ->
      Alcotest.(check (array (float 1e-5))) "the same opinion, from a file" p (fst (Policy_value.opinion back lesson.input))

let test_a_game () =
  let net = Policy_value.make ~seed:1 ~inputs:18 ~moves:9 () in
  let (lessons, share) = Selfplay.play ~seed:3 board net in
  Alcotest.(check bool) "a game of five to nine moves" true (List.length lessons >= 5 && List.length lessons <= 9);
  Alcotest.(check bool) "won, lost or drawn" true (List.mem share [ 0.; 0.5; 1. ]);
  List.iteri
    (fun i (l : Policy_value.lesson) ->
      Alcotest.(check (float 1e-9)) "the visits as shares" 1. (Array.fold_left ( +. ) 0. l.policy);
      (* x's result at x's positions, the opposite at o's *)
      let for_x = (2. *. share) -. 1. in
      Alcotest.(check (float 0.)) "the result, for whoever was to play" (if i mod 2 = 0 then for_x else -.for_x) l.value)
    lessons;
  (* the same seed, the same game *)
  Alcotest.(check bool) "repeatable" true (Selfplay.play ~seed:3 board net = (lessons, share))

(* the loop: games against itself, then lessons, again. The numbers of
   Selfplay.mli come from here *)
let test_the_loop () =
  let draws = Lehmer.make 5 in
  let measure (name : string) (net : Policy_value.t) =
    let searching : (Tictactoe.position, int) Arena.player = fun ~seed s -> Option.get (Selfplay.choose ~seed board net s) in
    let alone : (Tictactoe.position, int) Arena.player = fun ~seed:_ s -> Option.get (Selfplay.instinct board net s) in
    let scores = (against searching perfect 10, against searching random 40, against alone perfect 2, against alone random 40) in
    let (sp, sr, ap, ar) = scores in
    Printf.eprintf "selfplay, %s: with the search, perfect %s random %s; alone, perfect %s random %s\n" name (shown sp)
      (shown sr) (shown ap) (shown ar);
    scores
  in
  let net = ref (Policy_value.make ~seed:1 ~rate:0.01 ~inputs:18 ~moves:9 ()) in
  let (_, _, _, alone_before) = measure "knowing nothing" !net in
  let memory = ref [||] in
  let t0 = Unix.gettimeofday () in
  for iteration = 1 to 6 do
    let fresh =
      List.concat (List.init 20 (fun g -> fst (Selfplay.play ~seed:((iteration * 1000) + g) board !net)))
    in
    let all = Array.append (Array.of_list fresh) !memory in
    memory := Array.sub all 0 (min 3000 (Array.length all));
    for _ = 1 to 200 do
      let batch = Array.init 32 (fun _ -> !memory.(Lehmer.int draws (Array.length !memory))) in
      net := fst (Policy_value.step !net batch)
    done
  done;
  Printf.eprintf "selfplay: six iterations, %.1f s\n" (Unix.gettimeofday () -. t0);
  let (perfect_score, random_score, _, alone_after) = measure "after six iterations" !net in
  Alcotest.(check int) "with the search, it never loses to the perfect player" 0 perfect_score.lost;
  Alcotest.(check int) "nor to the random one" 0 random_score.lost;
  Alcotest.(check bool) "which it mostly beats" true (random_score.won >= 30);
  Alcotest.(check bool) "and alone, it has learned something" true (alone_after.won > alone_before.won)

let tests =
  [
    t "Tictactoe, the rules and the truth" test_tictactoe;
    t "Policy_value, two heads and a lesson" test_network;
    t "Selfplay, a game and what it teaches" test_a_game;
    t "Selfplay, the loop: it stops losing" test_the_loop;
  ]
