(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_op1.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate

(* [play engine params f seconds]: a note, in blocks of 735 *)
let play (e : Op1_engine.t) (params : float array) (f : float) (seconds : float) : Signal.t =
  let fill = e.start params ~frequency:f ~velocity:1. in
  let n = Signal.samples seconds in
  let out = Array.make n 0. in
  let k = ref 0 in
  while !k < n do
    let m = min 735 (n - !k) in
    let b = Array.make m 0. in
    fill b;
    Array.blit b 0 out !k m;
    k := !k + m
  done;
  out

(* the amplitude of [f] in [x] from sample [a] for [n], Hann-windowed *)
let amplitude (x : Signal.t) (f : float) (a : int) (n : int) : float =
  let re = ref 0. and im = ref 0. and sum = ref 0. in
  for i = a to a + n - 1 do
    let hann = 0.5 -. (0.5 *. cos (2. *. Float.pi *. float_of_int (i - a) /. float_of_int n)) in
    let w = 2. *. Float.pi *. f *. float_of_int i /. rate in
    re := !re +. (hann *. x.(i) *. cos w);
    im := !im +. (hann *. x.(i) *. sin w);
    sum := !sum +. hann
  done;
  2. *. sqrt ((!re *. !re) +. (!im *. !im)) /. !sum

(* the harmonics 2 to 5 against the first, dB *)
let richness (x : Signal.t) (f : float) : float =
  let h k = amplitude x (float_of_int k *. f) 4410 8192 in
  let upper = List.fold_left (fun acc k -> acc +. (h k *. h k)) 0. [ 2; 3; 4; 5 ] in
  10. *. log10 (Float.max 1e-30 upper /. (h 1 *. h 1))

let test_fm () =
  let x = play Op1_engine.fm [| 0.; 0.; 0.; 0. |] 440. 0.4 in
  Alcotest.(check bool) "amount 0: harmonics 2-5 under -120 dB, a sine" true (richness x 440. < -120.);
  let y = play Op1_engine.fm [| 0.6; 0.; 0.; 0. |] 440. 0.4 in
  (* three modulators in a chain, each one's index compounding: the
   * harmonics above the fundamental *)
  Alcotest.(check (float 0.1)) "amount 0.6, the stack (dB)" 15.1 (richness y 440.)

(* the cluster: the bins within 10% of the note above a tenth of the
 * loudest, spread 0 and 1, six waves *)
let test_cluster () =
  let width spread =
    let x = play Op1_engine.cluster [| 1.; 0.; spread; 0.5 |] 220. 1. in
    let n = 32768 in
    let m = Spectrum.magnitudes (Spectrum.fft (Spectrum.hann (Array.sub x 4096 n))) in
    let near = List.filter (fun k -> let f = Spectrum.bin_frequency ~n k in f > 198. && f < 242.) (List.init 400 (fun k -> k)) in
    let top = List.fold_left (fun a k -> Float.max a m.(k)) 0. near in
    List.length (List.filter (fun k -> m.(k) > 0.1 *. top) near)
  in
  Alcotest.(check (pair int int)) "bins near the note, spread 0 and 1" (4, 10) (width 0., width 1.)

(* the string's pitch: the lag at which it best matches itself (its
 * autocorrelation's peak), refined between samples -- its zero
 * crossings count its bright harmonics, not its period *)
let test_string () =
  let x = play Op1_engine.string [| 0.7; 0.3; 0.; 0. |] 220. 0.5 in
  let a = Signal.samples 0.1 and n = 4096 in
  let corr lag = let s = ref 0. in for i = a to a + n - 1 do s := !s +. (x.(i) *. x.(i + lag)) done; !s in
  let best = ref 100 in
  for lag = 100 to 400 do
    if corr lag > corr !best then best := lag
  done;
  (* a parabola through the peak and its neighbours *)
  let l = corr (!best - 1) and c = corr !best and r = corr (!best + 1) in
  let lag = float_of_int !best +. (0.5 *. (l -. r) /. (l -. (2. *. c) +. r)) in
  Alcotest.(check (float 0.5)) "220 Hz plucked (Hz)" 220. (rate /. lag)

let test_phase () =
  Alcotest.(check (list (float 1e-9))) "the bent phase at amount 1: the knee at 0.05" [ 0.; 0.5; 0.7632 ]
    (List.map (fun p -> Float.round (Op1_engine.phase_distortion ~amount:1. p *. 1e4) /. 1e4) [ 0.; 0.05; 0.55 ]);
  let amounts = List.map (fun a -> richness (play Op1_engine.phase [| 0.; a; 0.; 0. |] 220. 0.4) 220.) [ 0.; 0.5; 1. ] in
  Alcotest.(check bool) "amount 0: a pure cosine (under -100 dB)" true (List.hd amounts < -100.);
  Alcotest.(check (list (float 0.1))) "harmonics 2-5 at amount 0.5 and 1 (dB)" [ -11.7; -5.9 ] (List.tl amounts)

let test_digital () =
  let levels d = List.length (List.sort_uniq compare (Array.to_list (play Op1_engine.digital [| 0.5; 0.5; 0.; d |] 220. 0.2))) in
  (* 16 bits, then 2 bits (-1, -0.5, 0, 0.5, 1) *)
  Alcotest.(check (pair int int)) "distinct sample values, digitalness 0 and 1" (2029, 5) (levels 0., levels 1.)

(* each engine at its middle, four notes of an arpeggio *)
let phrase (e : Op1_engine.t) : Signal.t =
  Array.concat (List.map (fun f -> Array.map (fun x -> 0.5 *. x) (play e [| 0.5; 0.5; 0.5; 0.5 |] f 0.3)) [ 220.; 277.18; 329.63; 440. ])

let tests =
  Testo.categorize "OP-1"
    (List.map (fun (e : Op1_engine.t) -> t ("golden WAV: " ^ e.name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("op1_" ^ e.name) (phrase e))) Op1_engine.all
    @ [
        t "FM: four operators, amount 0 a sine" test_fm;
        t "cluster: the spread" test_cluster;
        t "string: its pitch" test_string;
        t "phase distortion: the harmonics with the amount" test_phase;
        t "digital: its levels" test_digital;
      ])
