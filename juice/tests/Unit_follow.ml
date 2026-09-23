(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_follow.mli *)

let near = Alcotest.float 1e-3
let dt = 1. /. 60.

(* the values of the spring going from 0 to 1, frame after frame *)
let chased ?damping (frames : int) : float list =
  let rec go n t acc = if n = 0 then List.rev acc else let t = Follow.chase ?damping ~dt 1. t in go (n - 1) t (t.value :: acc) in
  go frames (Follow.at 0.) []

let tests =
  Testo.categorize "Follow"
    [
      Testo.create "the worked example: the spring at 2 Hz, critically damped" (fun () ->
          let ys = chased 120 in
          Alcotest.check near "a quarter second" 0.830 (List.nth ys 14);
          Alcotest.(check bool) "not yet 95% at frame 23" true (List.nth ys 22 < 0.95);
          Alcotest.(check bool) "95% at frame 24" true (List.nth ys 23 >= 0.95);
          Alcotest.(check bool) "never past the target" true (List.for_all (fun y -> y <= 1.) ys));
      Testo.create "the worked example: a damping of 0.5 overshoots" (fun () ->
          Alcotest.check near "up to 1.142" 1.142 (List.fold_left Float.max 0. (chased ~damping:0.5 120)));
      Testo.create "the worked example: smooth, 1 - 1/e after 1/rate" (fun () ->
          let x = List.fold_left (fun x _ -> Follow.smooth ~rate:10. ~dt 1. x) 0. (List.init 6 Fun.id) in
          Alcotest.check (Alcotest.float 1e-9) "63.2%" (1. -. exp (-1.)) x);
      Testo.create "smooth is the same at any frame rate" (fun () ->
          let after fps = List.fold_left (fun x _ -> Follow.smooth ~rate:10. ~dt:(1. /. fps) 1. x) 0. (List.init (int_of_float (fps /. 2.)) Fun.id) in
          Alcotest.check (Alcotest.float 1e-9) "30 and 144 frames a second, half a second later" (after 30.) (after 144.));
    ]
