(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Sokoban, written the PuzzleScript way (see Puzzlescript.mli):
 * there is no update function here and no view, only things, three maps,
 * one rule and what winning means. Arrows move, z undoes, r restarts.
 *
 * The rule is the whole game:
 *
 *    "> @ | $  ->  > @ | > $"
 *
 * wherever a hero moving some way has a crate in front of it, the crate
 * moves that way too. Nothing says a crate cannot be pushed into a wall,
 * or into another crate: the walls and the crates are on the same layer,
 * and a thing only moves where its layer has room. Nothing says the hero
 * may walk on an empty floor either -- it is marked as moving by the key
 * and simply goes, if it can.
 *
 * Compare with TinySokoban.ml, which is the same game written out
 * by hand, with gamekits/puzzle's Push and Undo: three hundred lines against
 * these thirty. What the hand-written one has that this has not is a
 * solver in its tests, a move and push counter, and levels that look
 * like something; what this has is that you can change the game by
 * changing a line, which is what PuzzleScript is for. The three levels
 * below are our own, and tests/ checks a breadth-first search solves
 * each of them.
 *)
open Playground
open Puzzlescript

let things =
  [ thing '#' (rgb 92 88 82) ~layer:1;
    thing '@' (rgb 245 190 70) ~layer:1
      ~art:[ ".xxx."; "..x.."; "xxxxx"; "..x.."; ".x.x." ];
    thing '$' (rgb 178 116 68) ~layer:1
      ~art:[ "xxxxx"; "x...x"; "x...x"; "x...x"; "xxxxx" ];
    thing '.' (rgb 80 140 110) ~layer:0
      ~art:[ "....."; "..x.."; ".xxx."; "..x.."; "....." ] ]

(* '*' is a crate already on a target, '+' the hero standing on one:
 * two things in one cell, which is what the layers are for *)
let legend = [ ('*', "$."); ('+', "@.") ]

let levels =
  [ [ "#####";
      "#.$@#";
      "#####" ];
    [ "######";
      "#    #";
      "# $@ #";
      "#  # #";
      "#  . #";
      "######" ];
    [ "#######";
      "#  .  #";
      "# $ $ #";
      "#  @  #";
      "#     #";
      "#  .  #";
      "#######" ] ]

let sokoban =
  make ~things ~player:'@' ~legend
    ~rules:[ rule "> @ | $  ->  > @ | > $" ]
    ~wins:[ all_on '$' '.' ]
    levels

let app = play sokoban
let main = Playground_platform.run_app app
