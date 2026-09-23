(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Zobrist and Deepening: the keys and the table, and a search
 * that goes deeper -- it must answer what alpha-beta answers, however
 * many tricks are switched on *)

let t = Testo.create

(*****************************************************************************)
(* Zobrist *)
(*****************************************************************************)

let test_keys () =
  let z = Zobrist.make ~pieces:2 ~squares:3 ~seed:1 in
  (* the .mli's example: a board hashes to the xor of its pieces'
   * numbers, whatever order they are given in *)
  let a = Zobrist.of_board z [ (0, 1); (1, 2) ] and b = Zobrist.of_board z [ (1, 2); (0, 1) ] in
  Alcotest.(check int64) "the same board, the same key" a b;
  Alcotest.(check int64) "and it is the xor of the two numbers"
    (Int64.logxor (Zobrist.number z ~piece:0 ~square:1) (Zobrist.number z ~piece:1 ~square:2))
    a;
  Alcotest.(check bool) "another board, another key" true (a <> Zobrist.of_board z [ (0, 1); (1, 0) ]);
  (* xor is its own inverse: a move is two xors *)
  let moved = Int64.logxor (Int64.logxor a (Zobrist.number z ~piece:1 ~square:2)) (Zobrist.number z ~piece:1 ~square:0) in
  Alcotest.(check int64) "a piece moved: the board it gives" (Zobrist.of_board z [ (0, 1); (1, 0) ]) moved;
  Alcotest.(check int64) "the empty board: 0" 0L (Zobrist.of_board z []);
  (* the same seed, the same numbers; another seed, others *)
  Alcotest.(check int64) "the same seed" (Zobrist.number z ~piece:0 ~square:0)
    (Zobrist.number (Zobrist.make ~pieces:2 ~squares:3 ~seed:1) ~piece:0 ~square:0);
  Alcotest.(check bool) "another seed" true
    (Zobrist.number z ~piece:0 ~square:0 <> Zobrist.number (Zobrist.make ~pieces:2 ~squares:3 ~seed:2) ~piece:0 ~square:0)

let test_table () =
  let table : int Zobrist.table = Zobrist.table () in
  Alcotest.(check int) "empty" 0 (Zobrist.size table);
  Zobrist.remember table 7L { value = 1.; depth = 3; bound = Exact; best = Some 2 };
  Alcotest.(check (option int)) "what was learned" (Some 2) (Option.bind (Zobrist.find table 7L) (fun e -> e.best));
  Alcotest.(check int) "one hit" 1 (Zobrist.hits table);
  (* a shallower search does not overwrite a deeper one *)
  Zobrist.remember table 7L { value = 9.; depth = 1; bound = Exact; best = Some 5 };
  Alcotest.(check (option (float 0.))) "the deeper value kept" (Some 1.)
    (Option.map (fun (e : int Zobrist.entry) -> e.value) (Zobrist.find table 7L));
  Zobrist.remember table 7L { value = 9.; depth = 4; bound = Exact; best = Some 5 };
  Alcotest.(check (option (float 0.))) "a deeper one replaces it" (Some 9.)
    (Option.map (fun (e : int Zobrist.entry) -> e.value) (Zobrist.find table 7L));
  Alcotest.(check (option int)) "nothing about another position" None
    (Option.map (fun (e : int Zobrist.entry) -> e.depth) (Zobrist.find table 8L));
  Zobrist.forget table;
  Alcotest.(check int) "forgotten" 0 (Zobrist.size table)

(*****************************************************************************)
(* Deepening, on a game small enough to count *)
(*****************************************************************************)

(* Nim: [n] counters, each player takes 1, 2 or 3, whoever takes the
 * last one wins. MAX wins when the count reaches 0 on MIN's turn. A
 * position is (counters left, MAX to play): different move orders reach
 * the same one, which is what the table is for *)
let nim : (int * bool, int) Minimax.game =
  {
    moves = (fun (n, _) -> List.filter (fun k -> k <= n) [ 1; 2; 3 ]);
    play = (fun (n, max) k -> (n - k, not max));
    score = (fun (n, max) -> if n > 0 then 0. else if max then -1. else 1.);
    max_to_play = snd;
  }

let key (z : Zobrist.t) ((n, max) : int * bool) : int64 =
  Int64.logxor (Zobrist.number z ~piece:0 ~square:n) (if max then Zobrist.side z else 0L)

(* every way of searching gives the same answer: Nim with 12 counters
 * is lost for the player to move (12 is a multiple of 4), so MAX,
 * moving first, cannot win *)
let test_same_answer () =
  let z = Zobrist.make ~pieces:1 ~squares:32 ~seed:3 in
  let start = (12, true) in
  let plain = Minimax.alphabeta nim ~depth:8 start in
  let ways =
    [
      ("iterative deepening", Deepening.search nim ~depth:8 start);
      ("+ ordering", Deepening.search ~order:(fun _ moves -> List.rev moves) nim ~depth:8 start);
      ("+ the table", Deepening.search ~key:(key z) ~table:(Zobrist.table ()) nim ~depth:8 start);
      ( "+ both",
        Deepening.search ~order:(fun _ moves -> List.rev moves) ~key:(key z) ~table:(Zobrist.table ()) nim ~depth:8 start );
    ]
  in
  List.iter
    (fun (what, (p : int Deepening.plan)) ->
      Alcotest.(check (float 0.)) (what ^ ": the same value as alpha-beta") plain.value p.value;
      Alcotest.(check int) (what ^ ": it finished the depth asked for") 8 p.depth)
    ways;
  (* 12 counters, MAX to move: MIN wins whatever MAX does *)
  Alcotest.(check (float 0.)) "Nim(12): MAX loses" (-1.) plain.value

(* the tricks pay: the nodes each way visits, on Nim with 20 counters
 * searched 10 moves ahead *)
let test_nodes () =
  let z = Zobrist.make ~pieces:1 ~squares:64 ~seed:3 in
  let start = (20, true) in
  let nodes ?order ?key ?table () = (Deepening.search ?order ?key ?table nim ~depth:10 start).nodes in
  let plain = (Minimax.alphabeta nim ~depth:10 start).nodes in
  let deepening = nodes () in
  let with_table = nodes ~key:(key z) ~table:(Zobrist.table ()) () in
  Printf.eprintf "nim: alphabeta %d, deepening %d, + table %d\n" plain deepening with_table;
  Alcotest.(check bool) "the table saves most of the work" true (with_table < deepening / 2)

(* the budget: a search that runs out keeps the last depth that
 * finished, and says how deep that was *)
let test_budget () =
  let start = (20, true) in
  let full = Deepening.search nim ~depth:10 start in
  Alcotest.(check int) "with nodes to spare: the depth asked for" 10 full.depth;
  let short = Deepening.search ~budget:200 nim ~depth:10 start in
  Alcotest.(check bool) "with 200 nodes: not that deep" true (short.depth < full.depth && short.depth >= 1);
  Alcotest.(check bool) "but a finished depth" true (short.best <> None);
  Alcotest.(check bool) "and it stopped near its budget" true (short.nodes <= 200 + 64)

(* thinking a frame at a time: the same answer as thinking all at once,
 * reached in pieces *)
let test_thinking () =
  let start = (16, true) in
  let all_at_once = Deepening.search nim ~depth:8 start in
  let rec go (t : (int * bool, int) Deepening.thinking) (frames : int) =
    if Deepening.done_ t || frames > 200 then (t, frames) else go (Deepening.think ~nodes:150 t) (frames + 1)
  in
  let (t, frames) = go (Deepening.start nim ~depth:8 start) 0 in
  let p = Deepening.plan t in
  Alcotest.(check bool) "it took several frames" true (frames > 1);
  Alcotest.(check bool) "it finished" true (Deepening.done_ t);
  Alcotest.(check int) "as deep as asked" 8 p.depth;
  Alcotest.(check (float 0.)) "the same value" all_at_once.value p.value;
  (* the plan is usable before it is finished: a shallower answer *)
  let early = Deepening.plan (Deepening.think ~nodes:150 (Deepening.start nim ~depth:8 start)) in
  Alcotest.(check bool) "after one frame: an answer already" true (early.best <> None && early.depth >= 1)

let tests =
  Testo.categorize "Deeper search"
    [
      t "Zobrist: the keys" test_keys;
      t "Zobrist: the table" test_table;
      t "Deepening: every way gives alpha-beta's answer" test_same_answer;
      t "Deepening: what the tricks save" test_nodes;
      t "Deepening: the node budget" test_budget;
      t "Deepening: thinking a frame at a time" test_thinking;
    ]
