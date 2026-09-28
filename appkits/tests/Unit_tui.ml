(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tui.mli *)

let p = Tui_snake.program
let events (es : Tui.event list) = List.fold_left (fun m e -> p.update e m) p.init es
let cells = Alcotest.(list (pair int int))

let tests =
  Testo.categorize "Tui"
    [
      Testo.create "Snake: a move a tenth of a second, three for a slow frame" (fun () ->
          Alcotest.check cells "start" [ (10, 12); (10, 11); (10, 10) ] (Tui_snake.body p.init);
          Alcotest.check cells "one" [ (10, 13); (10, 12); (10, 11) ] (Tui_snake.body (events [ Tick 0.1 ]));
          Alcotest.check cells "not yet" [ (10, 12); (10, 11); (10, 10) ] (Tui_snake.body (events [ Tick 0.05 ]));
          Alcotest.check cells "three" [ (10, 15); (10, 14); (10, 13) ] (Tui_snake.body (events [ Tick 0.3 ])));
      Testo.create "Snake: turns at the next move, never back" (fun () ->
          Alcotest.check cells "down" [ (11, 12); (10, 12); (10, 11) ] (Tui_snake.body (events [ Key "j"; Tick 0.1 ]));
          Alcotest.check cells "not left" [ (10, 13); (10, 12); (10, 11) ] (Tui_snake.body (events [ Key "\x1b[D"; Tick 0.1 ])));
      Testo.create "Snake: the wall ends it, r starts again, q quits" (fun () ->
          let m = events [ Key "k"; Tick 1.1 ] in
          Alcotest.(check bool) "crashed" true (Tui_snake.crashed m);
          Alcotest.(check bool) "again" false (Tui_snake.crashed (p.update (Key "r") m));
          Alcotest.(check bool) "over" true (p.over (p.update (Key "q") p.init)));
      Testo.create "Snake: a move costs a few bytes, not a screen" (fun () ->
          let before = p.view p.init and after = p.view (events [ Tick 0.1 ]) in
          let move = String.length (Curses.refresh ~before after) and whole = String.length (Curses.redraw after) in
          (* the new head, the old head now body, the tail erased *)
          Alcotest.(check bool) (Printf.sprintf "%d bytes, against %d" move whole) true (move < 40 && whole > 300));
    ]
