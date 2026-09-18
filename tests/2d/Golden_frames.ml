(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Golden_frames.mli *)

(* the keys (see playground/software/Playground_platform.ml): n
 * antialiasing off, f wireframe, b bounding boxes, o the simple code
 * instead of the optimized one (Opti). Not t (alpha blending off): only
 * examples/Mouse fades a shape, while the mouse button is down. *)
let scenes : Testutil_golden.scene list =
  [
    ("examples/software/Picture", "", 5);
    ("examples/software/Picture", "n", 5);
    ("examples/software/Picture", "f", 5);
    ("examples/software/Picture", "b", 5);
    (* the same golden frame as without "o", on purpose: an optimization
     * must not change a single pixel *)
    ("examples/software/Picture", "o", 5);
    ("examples/software/Smiley", "", 5);
    ("examples/software/Words", "", 5);
    ("examples/software/Words", "n", 5);
    ("examples/software/Misc", "", 5);
    ("examples/software/Animation", "", 5);
    ("examples/software/Mouse", "", 5);
    ("examples/software/Keyboard", "", 5);
    ("games/software/Pong", "", 5);
    ("games/software/Asteroid", "", 5);
    ("games/software/Asteroid", "f", 5);
  ]

let tests = Testutil_golden.tests ~dir:"tests/2d" ~approve:"approve-golden2d" scenes
