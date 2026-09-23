(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tween.mli *)

let close = Alcotest.float 1e-9

(* the figure of Tween.mli *)
let figure (now : float) : float = Tween.value (Ease.out Ease.quad) 0. 100. ~start:1. ~duration:2. now

let tests =
  Testo.categorize "Tween"
    [
      Testo.create "the worked example" (fun () ->
          Alcotest.check close "not started" 0. (figure 0.5);
          Alcotest.check close "at the start" 0. (figure 1.);
          Alcotest.check close "halfway through the time, three quarters of the way" 75. (figure 2.);
          Alcotest.check close "over, held" 100. (figure 3.5);
          Alcotest.(check bool) "not finished at 2 s" false (Tween.finished ~start:1. ~duration:2. 2.);
          Alcotest.(check bool) "finished at 3 s" true (Tween.finished ~start:1. ~duration:2. 3.));
      Testo.create "backwards and beyond: lerp is not clamped, progress is" (fun () ->
          Alcotest.check close "lerp 100 to 0 at a quarter" 75. (Tween.lerp 100. 0. 0.25);
          Alcotest.check close "lerp past the end" 150. (Tween.lerp 0. 100. 1.5);
          Alcotest.check close "progress before" 0. (Tween.progress ~start:5. ~duration:1. 0.);
          Alcotest.check close "progress after" 1. (Tween.progress ~start:5. ~duration:1. 9.));
      Testo.create "a tween of no time is at its end at once" (fun () ->
          Alcotest.check close "duration 0" 7. (Tween.value Ease.linear 3. 7. ~start:1. ~duration:0. 0.));
    ]
