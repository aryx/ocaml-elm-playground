(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* ai/Minimax: the .mli's tree, alpha-beta agreeing with minimax, and
 * tic-tac-toe searched to the end *)

let t = Testo.create

(*****************************************************************************)
(* Trees *)
(*****************************************************************************)

(* a game given as its tree, MAX and MIN alternating *)
type tree = Leaf of float | Node of tree list

let tree_game : (tree * bool, int) Minimax.game =
  {
    moves = (function Leaf _, _ -> [] | Node children, _ -> List.mapi (fun i _ -> i) children);
    play = (fun (t, max) i -> match t with Node children -> (List.nth children i, not max) | Leaf _ -> assert false);
    score = (function Leaf v, _ -> v | Node _, _ -> 0.);
    max_to_play = snd;
  }

let leaves l = Node (List.map (fun v -> Leaf v) l)
let textbook = Node [ leaves [ 3.; 12.; 8. ]; leaves [ 2.; 4.; 6. ]; leaves [ 14.; 5.; 2. ] ]

let test_textbook () =
  let m = Minimax.minimax tree_game ~depth:2 (textbook, true) in
  let a = Minimax.alphabeta tree_game ~depth:2 (textbook, true) in
  Alcotest.(check (float 0.)) "worth 3" 3. m.value;
  Alcotest.(check (option int)) "the left move" (Some 0) m.best;
  Alcotest.(check (list (pair int (float 0.)))) "3, 2, 2" [ (0, 3.); (1, 2.); (2, 2.) ] m.children;
  Alcotest.(check int) "minimax: 13 nodes" 13 m.nodes;
  Alcotest.(check (float 0.)) "alpha-beta: worth 3 too" 3. a.value;
  Alcotest.(check (option int)) "the same move" (Some 0) a.best;
  Alcotest.(check int) "alpha-beta: 11 nodes, the 4 and the 6 cut" 11 a.nodes

(* random trees, 3 to 5 levels deep, 1 to 4 children each: alpha-beta
 * finds minimax's value and move, never visiting more *)
let test_random () =
  let seed = ref 42 in
  let rand n =
    seed := ((!seed * 1103515245) + 12345) land 0x7fffffff;
    !seed / 65536 mod n
  in
  let rec tree depth = if depth = 0 then Leaf (float_of_int (rand 20)) else Node (List.init (1 + rand 4) (fun _ -> tree (depth - 1))) in
  for _ = 1 to 500 do
    let depth = 3 + rand 3 in
    let s = (tree depth, rand 2 = 0) in
    let m = Minimax.minimax tree_game ~depth s and a = Minimax.alphabeta tree_game ~depth s in
    Alcotest.(check (float 0.)) "the value" m.value a.value;
    Alcotest.(check (option int)) "the move" m.best a.best;
    Alcotest.(check bool) "fewer nodes" true (a.nodes <= m.nodes)
  done

(*****************************************************************************)
(* Tic-tac-toe *)
(*****************************************************************************)

(* the board's 9 cells, ' ' or 'X' (MAX, first) or 'O' *)
let lines = [ [ 0; 1; 2 ]; [ 3; 4; 5 ]; [ 6; 7; 8 ]; [ 0; 3; 6 ]; [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 0; 4; 8 ]; [ 2; 4; 6 ] ]

let winner (b : string) : char option =
  List.find_map (fun l -> match List.map (String.get b) l with [ a; b; c ] when a <> ' ' && a = b && b = c -> Some a | _ -> None) lines

let x_to_play (b : string) : bool =
  let count c = String.fold_left (fun n c' -> if c' = c then n + 1 else n) 0 b in
  count 'X' = count 'O'

let tictactoe : (string, int) Minimax.game =
  {
    moves = (fun b -> if winner b <> None then [] else List.filter (fun i -> b.[i] = ' ') (List.init 9 Fun.id));
    play = (fun b i -> String.mapi (fun j c -> if j = i then if x_to_play b then 'X' else 'O' else c) b);
    score = (fun b -> match winner b with Some 'X' -> 1. | Some _ -> -1. | None -> 0.);
    max_to_play = x_to_play;
  }

(* searched to the end: a draw; the whole tree is 549,946 positions,
 * alpha-beta visits 18,297 of them *)
let test_tictactoe () =
  let empty = String.make 9 ' ' in
  let m = Minimax.minimax tictactoe ~depth:9 empty and a = Minimax.alphabeta tictactoe ~depth:9 empty in
  Alcotest.(check (float 0.)) "a draw" 0. m.value;
  Alcotest.(check int) "the whole tree" 549946 m.nodes;
  Alcotest.(check (float 0.)) "a draw for alpha-beta too" 0. a.value;
  Alcotest.(check int) "3% of it" 18297 a.nodes;
  (* X in a corner, O not in the center: X wins *)
  let b = String.mapi (fun i c -> if i = 0 then 'X' else if i = 1 then 'O' else c) empty in
  Alcotest.(check (float 0.)) "O's mistake" 1. (Minimax.alphabeta tictactoe ~depth:9 b).value

let tests =
  Testo.categorize "Minimax"
    [ t "the textbook tree" test_textbook; t "alpha-beta agrees with minimax" test_random; t "tic-tac-toe, a draw" test_tictactoe ]
