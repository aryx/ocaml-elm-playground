(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A cave after Boulder Dash (Peter Liepa and Chris Gray, First Star,
 * 1984): dig through the dirt, take every diamond, and mind the
 * boulders. Written on Puzzlescript.mli, like
 * examples/PuzzleScriptSokoban.ml -- the same engine, four other rules,
 * and it is another game. That is the whole argument for the thing.
 *
 * Each rule is one idea, and worth reading one at a time:
 *
 *   "> @ | d  ->  > @ |"     digging: the hero walks into dirt, and the
 *                            dirt is gone -- a thing named on the left
 *                            and not on the right is taken away
 *
 *   "> @ | o  ->  > @ | > o" pushing, sideways only (~dirs:horizontal),
 *                            which is Sokoban's rule again, and Boulder
 *                            Dash's: you cannot push a boulder up
 *
 *   "stationary o  ->  > o"  gravity, read downwards only: a boulder
 *                            that is not already going somewhere tries
 *                            to fall. Whether it can is not this rule's
 *                            business -- a thing only moves where its
 *                            layer has room, so a boulder on dirt, on a
 *                            wall, or on another boulder simply stays
 *
 *   "@ D  ->  @"  (late)     taking a diamond: the hero and a diamond
 *                            in one cell, and then only the hero. Late,
 *                            so it is read after things have moved: the
 *                            hero takes it in the turn it walks in
 *
 * The diamonds are on their own layer, under the dirt, so the hero can
 * stand where one is -- which is why taking one can be a rule about a
 * single cell.
 *
 * Exercises: a boulder that falls on the hero kills it ("o | > @" read
 * downwards, and a Dead thing); a boulder that rolls off another one;
 * an exit that opens once the diamonds are taken (a win of some_on);
 * butterflies. Real Boulder Dash needs one thing this engine has not,
 * PuzzleScript's [again]: boulders there keep falling while the player
 * stands still, and here they fall one cell per turn.
 *)
open Playground
open Puzzlescript

let things =
  [ thing '#' (rgb 96 90 84) ~layer:1;
    thing 'd' (rgb 120 84 52) ~layer:1
      ~art:[ "x.x.x"; ".x.x."; "x.x.x"; ".x.x."; "x.x.x" ];
    thing 'o' (rgb 168 166 172) ~layer:1
      ~art:[ ".xxx."; "xxxxx"; "xxxxx"; "xxxxx"; ".xxx." ];
    thing '@' (rgb 245 190 70) ~layer:1
      ~art:[ ".xxx."; "..x.."; "xxxxx"; "..x.."; ".x.x." ];
    thing 'D' (rgb 90 200 230) ~layer:0
      ~art:[ "..x.."; ".xxx."; "xxxxx"; ".xxx."; "..x.." ] ]

let levels =
  [ [ "##########";
      "#@ddddddd#";
      "#dddodddd#";
      "#ddddd  d#";
      "#D dddd D#";
      "##########" ];
    [ "############";
      "#@dddddddd #";
      "#ddoddddodd#";
      "#dddddddddd#";
      "#D d  d  dD#";
      "#dddddddddd#";
      "# D dddd D #";
      "############" ] ]

let boulders =
  make ~things ~player:'@'
    ~rules:
      [ rule "> @ | d  ->  > @ |";
        rule ~dirs:horizontal "> @ | o  ->  > @ | > o";
        rule ~dirs:[ Down ] "stationary o  ->  > o";
        rule ~late:true "@ D  ->  @" ]
    ~wins:[ none_left 'D' ]
    levels

let app = play boulders
let main = Playground_platform.run_app app
