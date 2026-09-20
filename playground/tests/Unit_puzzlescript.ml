(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* playground/Puzzlescript: the engine, on the smallest games that show
 * each of its parts -- the one rule that is Sokoban, the layers under
 * it, the sweep that turns a mark into a push, and a late rule. The two
 * real games are examples/PuzzleScript*.ml, played in tests/games. *)

open Playground
open Puzzlescript

let t = Testo.create
let rows = Alcotest.(list string)

(* the whole of Sokoban, as in the header of Puzzlescript.mli *)
let sokoban (levels : string list list) : Puzzlescript.t =
  make
    ~things:
      [ thing '#' (rgb 90 90 90) ~layer:1;
        thing '@' (rgb 240 190 70) ~layer:1;
        thing '$' (rgb 170 110 70) ~layer:1;
        thing '.' (rgb 80 140 110) ~layer:0 ]
    ~player:'@' ~legend:[ ('*', "$."); ('+', "@.") ]
    ~rules:[ rule "> @ | $  ->  > @ | > $" ]
    ~wins:[ all_on '$' '.' ]
    levels

(* One rule, and it pushes. The hero is marked moving right; the rule
 * marks the crate too; then the sweep moves the crate (it has room) and
 * the hero after it. *)
let push () =
  let g = sokoban [ [ "#####"; "#@$.#"; "#####" ] ] in
  let b = board g 0 in
  Alcotest.(check bool) "not won at the start" false (won g b);
  let b = turn g (Some Right) b in
  Alcotest.check rows "the crate is on the target, the hero behind it" [ "#####"; "# @*#"; "#####" ] (to_strings g b);
  Alcotest.(check bool) "won" true (won g b)

(* Nothing in the rules says a crate may not be pushed into a wall: the
 * wall and the crate are on one layer, and a thing only moves where its
 * layer has room. The hero is stopped in its turn, by the crate that
 * did not go. *)
let blocked () =
  let g = sokoban [ [ "#####"; "#@$##"; "#####" ] ] in
  let b = board g 0 in
  let b' = turn g (Some Right) b in
  Alcotest.check rows "nothing moved" (to_strings g b) (to_strings g b');
  (* and the other way there is nothing to push *)
  let g = sokoban [ [ "#####"; "#.$@#"; "#####" ] ] in
  let b = turn g (Some Left) (board g 0) in
  Alcotest.check rows "one push left" [ "#####"; "#*@ #"; "#####" ] (to_strings g b)

(* Layers: a crate and a target share a cell, and so may the hero. That
 * is why '*' and '+' are legend entries and not things of their own. *)
let layers () =
  let g = sokoban [ [ "######"; "#@$*.#"; "######" ] ] in
  let b = board g 0 in
  Alcotest.(check (list char)) "a crate on a target, lowest layer first" [ '.'; '$' ] (at b 3 1);
  Alcotest.(check (list char)) "a target on its own" [ '.' ] (at b 4 1);
  Alcotest.(check (list char)) "outside the board, nothing" [] (at b 99 1);
  (* the crates share a layer, so one cannot be pushed into the other *)
  Alcotest.check rows "nothing moved" (to_strings g b) (to_strings g (turn g (Some Right) b));
  (* the hero and a target do not, so it may stand on one: that is '+' *)
  let g = sokoban [ [ "####"; "#@.#"; "####" ] ] in
  let b = turn g (Some Right) (board g 0) in
  Alcotest.check rows "the hero on the target" [ "####"; "# +#"; "####" ] (to_strings g b);
  Alcotest.(check bool) "but there is no crate to put anywhere" false (won g b)

(* A late rule runs after things have moved, so it sees where they
 * landed: here the hero takes the coin in the very turn it walks in.
 * The same rule made early would take it a turn later. *)
let late () =
  let coins levels ~late =
    make
      ~things:[ thing '#' (rgb 90 90 90) ~layer:1; thing '@' (rgb 240 190 70) ~layer:1; thing 'c' (rgb 250 220 90) ~layer:0 ]
      ~player:'@' ~rules:[ rule ~late "@ c  ->  @" ] ~wins:[ none_left 'c' ] levels
  in
  let level = [ [ "####"; "#@c#"; "####" ] ] in
  let g = coins level ~late:true in
  let b = turn g (Some Right) (board g 0) in
  Alcotest.(check bool) "taken on the way in" true (won g b);
  let g = coins level ~late:false in
  let b = turn g (Some Right) (board g 0) in
  Alcotest.(check bool) "not yet, read too early" false (won g b);
  let b = turn g None b in
  Alcotest.(check bool) "taken on the next turn" true (won g b)

(* Gravity is one rule read downwards, and the sweep does the rest: a
 * boulder falls one cell a turn until its layer has no room below. *)
let gravity () =
  let g =
    make
      ~things:[ thing '#' (rgb 90 90 90) ~layer:1; thing '@' (rgb 240 190 70) ~layer:1; thing 'o' (rgb 170 170 170) ~layer:1 ]
      ~player:'@'
      ~rules:[ rule ~dirs:[ Down ] "stationary o  ->  > o" ]
      ~wins:[]
      [ [ "#####"; "#o@ #"; "#   #"; "#   #"; "#####" ] ]
  in
  let b = board g 0 in
  let b = turn g None b in
  Alcotest.check rows "one cell" [ "#####"; "# @ #"; "#o  #"; "#   #"; "#####" ] (to_strings g b);
  let b = turn g None b in
  let b = turn g None b in
  Alcotest.check rows "then it rests on the floor" [ "#####"; "# @ #"; "#   #"; "#o  #"; "#####" ] (to_strings g b)

(* The rules are strings, so they can be wrong; they say so at once
 * rather than quietly doing nothing. *)
let bad_rules () =
  let fails f = try ignore (f ()); false with Failure _ -> true in
  Alcotest.(check bool) "no arrow" true (fails (fun () -> rule "> @ | $"));
  Alcotest.(check bool) "sides of different lengths" true (fails (fun () -> rule "@ | $  ->  @"));
  Alcotest.(check bool) "a word that is neither a thing nor a prefix" true (fails (fun () -> rule "hero  ->  hero"))

let tests =
  Testo.categorize "Puzzlescript"
    [ t "the one rule that is Sokoban" push;
      t "a crate against a wall" blocked;
      t "two things in one cell" layers;
      t "a late rule" late;
      t "gravity, and the sweep" gravity;
      t "rules that do not parse" bad_rules ]
