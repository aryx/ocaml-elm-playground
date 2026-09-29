(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* claude: the worked examples of Timing.mli, Animation.mli and
 * Transition.mli *)

let t = Testo.create
let near = Alcotest.float 1e-3

let test_timing () =
  Alcotest.check near "linear: the identity" 0.3 (Timing.at Timing.linear 0.3);
  Alcotest.check near "ease_in_out: symmetric" 0.5 (Timing.at Timing.ease_in_out 0.5);
  Alcotest.check near "ease_in at 0.5" 0.3153 (Timing.at Timing.ease_in 0.5);
  Alcotest.check near "ease_out at 0.5" 0.6847 (Timing.at Timing.ease_out 0.5);
  Alcotest.check near "the ends" 1. (Timing.at Timing.default 1.);
  Alcotest.(check bool) "an overshoot leaves [0, 1]" true (Timing.at (Timing.bezier 0.34 1.56 0.64 1.) 0.6 > 1.)

let test_implicit () =
  let k = Animation.keyed ~timing:Timing.linear ~duration:1. ~lerp:Animation.lerp_float () in
  let k = Animation.set k ~now:0. "a" 0. in
  Alcotest.(check (option near)) "a new key is there at once" (Some 0.) (Animation.value_of k ~now:0. "a");
  let k = Animation.set k ~now:0. "a" 10. in
  Alcotest.(check (option near)) "halfway" (Some 5.) (Animation.value_of k ~now:0.5 "a");
  (* interrupted at 5, sent back to 0: from 5, no jump *)
  let k = Animation.set k ~now:0.5 "a" 0. in
  Alcotest.(check (option near)) "from where it was" (Some 5.) (Animation.value_of k ~now:0.5 "a");
  Alcotest.(check (option near)) "then halfway back" (Some 2.5) (Animation.value_of k ~now:1. "a");
  Alcotest.(check bool) "done after its time" false (Animation.moving k ~now:1.6)

let test_transition () =
  let r x y w h : Transition.rect = { x; y; w; h } in
  let tr = Transition.make ~before:[ ("a", r 0. 0. 50. 50.); ("b", r 50. 0. 50. 50.) ] ~after:[ ("b", r 0. 0. 100. 50.); ("d", r 50. 50. 50. 50.) ] () in
  let at p k = List.find (fun (f : string Transition.frame) -> f.key = k) (Transition.at tr p) in
  let b = at 0.5 "b" and d = at 0.5 "d" and a = at 0.5 "a" in
  Alcotest.check near "b moves: x" 25. b.rect.x;
  Alcotest.check near "b grows: w" 75. b.rect.w;
  Alcotest.check near "d enters from its centre: x" 62.5 d.rect.x;
  Alcotest.check near "d: w" 25. d.rect.w;
  Alcotest.check near "d half faded in" 0.5 d.alpha;
  Alcotest.check near "a half faded out" 0.5 a.alpha

let tests = [ t "animation: timing curves" test_timing; t "animation: implicit, interrupted" test_implicit; t "animation: a layout's transition" test_transition ]
