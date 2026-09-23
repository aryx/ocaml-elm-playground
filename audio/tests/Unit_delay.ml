(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_delay.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate

let peak x a b =
  let p = ref 0. in
  for i = a to b do
    p := Float.max !p (Float.abs x.(i))
  done;
  !p

let burst = Signal.samples 0.05

(* a 50 ms burst of [f] Hz on both sides (or the left only), 2 s
 * through the delay in blocks of 735 *)
let run ?(left_only = false) (settings : Delay.settings) (f : float) : Signal.stereo =
  let n = Signal.samples 2. in
  let x = Array.init n (fun i -> if i < burst then sin (2. *. Float.pi *. f *. float_of_int i /. rate) else 0.) in
  let s = { Signal.left = Array.copy x; right = (if left_only then Array.make n 0. else Array.copy x) } in
  let d = Delay.create () and k = ref 0 in
  while !k < n do
    let m = min 735 (n - !k) in
    let b = { Signal.left = Array.sub s.left !k m; right = Array.sub s.right !k m } in
    Delay.process d settings b;
    Array.blit b.left 0 s.left !k m;
    Array.blit b.right 0 s.right !k m;
    k := !k + m
  done;
  s

let dotted_eighth = { Delay.time = Delay.beats ~bpm:120. 0.75; feedback = 0.5; tone = 3000.; ping_pong = false; mix = 1. }

(* the k-th echo's peak *)
let echo (x : Signal.t) (k : int) : float =
  let a = Signal.samples (0.375 *. float_of_int k) in
  peak x (a + 20) (a + burst + 100)

let test_echoes () =
  Alcotest.(check (float 1e-9)) "a dotted eighth at 120 BPM" 0.375 (Delay.beats ~bpm:120. 0.75);
  let low = run dotted_eighth 200. and high = run dotted_eighth 8000. in
  List.iter2
    (fun k e -> Alcotest.(check (float 0.001)) (Printf.sprintf "200 Hz, echo %d" k) e (echo low.left k))
    [ 1; 2; 3 ] [ 0.9999; 0.4988; 0.2488 ];
  List.iter2
    (fun k e -> Alcotest.(check (float 0.001)) (Printf.sprintf "8 kHz, echo %d" k) e (echo high.left k))
    [ 1; 2; 3 ] [ 0.8419; 0.1314; 0.0326 ];
  (* nothing between the echoes *)
  Alcotest.(check (float 1e-9)) "silence before the first" 0. (peak low.left (burst + 10) (Signal.samples 0.37))

(* ping-pong: the echoes alternate, left first *)
let test_ping_pong () =
  let s = run ~left_only:true { dotted_eighth with ping_pong = true; tone = 20000. } 200. in
  Alcotest.(check bool) "echo 1 on the left" true (echo s.left 1 > 0.4 && echo s.right 1 < 0.01);
  Alcotest.(check bool) "echo 2 on the right" true (echo s.right 2 > 0.1 && echo s.left 2 < 0.01);
  Alcotest.(check bool) "echo 3 on the left" true (echo s.left 3 > 0.05 && echo s.right 3 < 0.01)

let tests =
  Testo.categorize "Delay"
    [ t "the echoes: in time, halving, darker at every pass" test_echoes; t "ping-pong: left, right, left" test_ping_pong ]
