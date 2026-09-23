(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Pattis's harvest (playground/Karel.mli), written in OCaml: Karel
 * picks up a field of beepers, row by row, as you watch (space: again).
 *
 * The whole lesson is in the three lets: a row is harvested once, in
 * [harvest_row], and the field is then a row, a turn, a row, a turn,
 * twice -- the program reads like the task. In Pattis's language (the
 * same program is TinyKarel's second level):
 *
 *   DEFINE-NEW-INSTRUCTION harvestrow AS
 *   BEGIN
 *     pickbeeper;
 *     ITERATE 4 TIMES BEGIN move; pickbeeper END
 *   END;
 *
 * and an OCaml let is Pattis's DEFINE-NEW-INSTRUCTION.
 *
 * What it uses: the Playground, and Karel.
 *)
open Karel

let field =
  world
    [ ". . . . . . .";
      "             ";
      ". 1 1 1 1 1 .";
      "             ";
      ". 1 1 1 1 1 .";
      "             ";
      ". 1 1 1 1 1 .";
      "             ";
      "> 1 1 1 1 1 ." ]

let turn_right = block [ turn_left; turn_left; turn_left ]
let harvest_row = block [ pick_beeper; iterate 4 [ move; pick_beeper ] ]

let harvest =
  [ move;
    iterate 2 [ harvest_row; turn_left; move; turn_left; harvest_row; turn_right; move; turn_right ];
    turn_off ]

let app = animation ~speed:6. field harvest
let main = Playground_platform.run_app app
