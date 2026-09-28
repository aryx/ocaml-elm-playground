(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Fm: the sidebands where Chowning says, at the Bessel functions'
 * amplitudes *)

let t = Testo.create

(* J_k(x), by its series: the sum over m of (-1)^m / (m! (m + k)!)
 * (x / 2)^(2m + k), 20 terms plenty for x below 10 *)
let bessel (k : int) (x : float) : float =
  let rec fact n = if n <= 1 then 1. else float_of_int n *. fact (n - 1) in
  let sum = ref 0. in
  for m = 0 to 20 do
    sum := !sum +. ((if m mod 2 = 0 then 1. else -1.) /. (fact m *. fact (m + k)) *. ((x /. 2.) ** float_of_int ((2 * m) + k)))
  done;
  !sum

(* 4096 samples; the carrier on bin 40 (430.7 Hz), the modulator on bin
 * 10 (a ratio of 0.25), so each sideband falls on a bin: the .mli's
 * figure, J0(1) at the carrier, J1(1) one bin of 10 away, ... *)
let test_sidebands () =
  let n = 4096 in
  Alcotest.(check (float 1e-4)) "J0(1)" 0.7652 (bessel 0 1.);
  Alcotest.(check (float 1e-4)) "J1(1)" 0.4401 (bessel 1 1.);
  List.iter
    (fun index ->
      let x = Fm.render ~carrier:(Spectrum.bin_frequency ~n 40) ~ratio:0.25 ~index 0.1 in
      let m = Spectrum.magnitudes (Spectrum.fft (Array.sub x 0 n)) in
      List.iter
        (fun k ->
          let what side = Printf.sprintf "I = %g, the sideband %s%d" index side k in
          Alcotest.(check (float 1e-3)) (what "+") (Float.abs (bessel k index)) m.(40 + (10 * k));
          Alcotest.(check (float 1e-3)) (what "-") (Float.abs (bessel k index)) m.(40 - (10 * k)))
        [ 0; 1; 2; 3 ])
    [ 0.5; 1. ]

(* no index, no sidebands: a sine *)
let test_no_index () =
  let x = Fm.render ~carrier:440. ~ratio:1.4 ~index:0. 0.1 in
  Array.iteri (fun i v -> Alcotest.(check (float 1e-9)) (Printf.sprintf "sample %d" i) (sin (2. *. Float.pi *. 440. *. float_of_int i /. 44100.)) v) (Array.sub x 0 100)

let tests =
  Testo.categorize "Fm"
    [ t "the sidebands at fc +- k fm, of amplitude J_k(I)" test_sidebands; t "an index of 0: a sine" test_no_index ]
