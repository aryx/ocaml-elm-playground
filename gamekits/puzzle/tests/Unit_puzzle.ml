(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* gamekits/puzzle: Push, Undo and Sokoban *)

let t = Testo.create
let cells = Alcotest.(option (list (pair int int)))

(* chain's worked examples, on one row *)
let test_push () =
  let on row ?limit () =
    let at (c, r) = if r <> 0 || c < 0 || c >= String.length row then '#' else row.[c] in
    Push.chain ~blocked:(fun p -> at p = '#') ~pushable:(fun p -> at p = '$') ?limit (0, 0) (1, 0)
  in
  Alcotest.check cells "two boxes" (Some [ (1, 0); (2, 0) ]) (on "@$$ " ());
  Alcotest.check cells "two, a limit of one" None (on "@$$ " ~limit:1 ());
  Alcotest.check cells "a wall behind" None (on "@$#" ());
  Alcotest.check cells "nothing" (Some []) (on "@ " ())

(* undo's *)
let test_undo () =
  let h = Undo.undo (Undo.record 'c' (Undo.record 'b' (Undo.start 'a'))) in
  Alcotest.(check char) "b now" 'b' h.now;
  Alcotest.(check (list char)) "a before" [ 'a' ] h.past;
  Alcotest.(check char) "the start stays" 'a' (Undo.undo (Undo.undo h)).now

(* Sokoban's: the format read, written and trimmed *)
let test_sokoban_format () =
  let levels = Alcotest.(list (list string)) in
  Alcotest.check levels "read" [ [ "###"; "#@#" ]; [ "###" ] ] (Sokoban.of_xsb "; 1\n###\n#@#\n\n###");
  Alcotest.(check string) "written" "; 1\n###\n#@#\n\n" (Sokoban.to_xsb [ [ "###"; "#@#" ] ]);
  Alcotest.check levels "floors, and a title" [ [ "#  #" ] ] (Sokoban.of_xsb "Title: a\r\n#-_#\r\n");
  Alcotest.(check (list string)) "trimmed" [ "##"; "#" ] (Sokoban.trim [ ""; "   ##  "; "   #"; " " ])

(* the first level, and TinySokoban's three: the problems and the
 * solutions of the header's worked examples *)
let level1 = [ "#######"; "#     #"; "# $@$ #"; "# . . #"; "#######" ]

let test_sokoban_solve () =
  Alcotest.(check (list string)) "no goal" [ "1 box, 0 goals" ] (Sokoban.problems [ "#@$ #" ]);
  Alcotest.(check (list string)) "fine" [] (Sokoban.problems level1);
  let moves rows = match Sokoban.solve rows with Sokoban.Moves m -> m | Unsolvable -> "unsolvable" | Gave_up _ -> "gave up" in
  Alcotest.(check string) "level 1" "ulDurrD" (moves level1);
  Alcotest.(check string) "a box in a corner" "unsolvable" (moves [ "#####"; "#$ @#"; "#.  #"; "#####" ])

let tests =
  Testo.categorize "kit_puzzle"
    [ t "Push" test_push; t "Undo" test_undo; t "Sokoban's format" test_sokoban_format; t "Sokoban's solver" test_sokoban_solve ]
