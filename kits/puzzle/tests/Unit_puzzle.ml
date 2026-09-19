(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* kits/puzzle: Push and Undo *)

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

let tests = Testo.categorize "kit_puzzle" [ t "Push" test_push; t "Undo" test_undo ]
