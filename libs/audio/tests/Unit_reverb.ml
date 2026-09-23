(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_reverb.mli *)

let t = Testo.create

(* the reverberation time of a response: its energy in 50 ms windows,
 * in dB under the loudest; a line fitted to the windows between -5 and
 * -35 dB after it (T30), extended to -60 *)
let t60 (x : Signal.t) : float =
  let w = Signal.samples 0.05 in
  let e =
    Array.init
      (Array.length x / w)
      (fun k ->
        let s = ref 0. in
        for i = k * w to (k * w) + w - 1 do
          s := !s +. (x.(i) *. x.(i))
        done;
        !s)
  in
  let top = Array.fold_left Float.max 0. e in
  let level = Array.map (fun v -> 10. *. log10 ((v /. top) +. 1e-30)) e in
  let loudest = ref 0 in
  Array.iteri (fun k v -> if v = 0. then loudest := k) level;
  let points = ref [] in
  Array.iteri (fun k v -> if k >= !loudest && v <= -5. && v >= -35. then points := (float_of_int k *. 0.05, v) :: !points) level;
  let n = float_of_int (List.length !points) in
  let sum f = List.fold_left (fun a p -> a +. f p) 0. !points in
  let sx = sum fst and sy = sum snd and sxx = sum (fun (x, _) -> x *. x) and sxy = sum (fun (x, y) -> x *. y) in
  -60. /. (((n *. sxy) -. (sx *. sy)) /. ((n *. sxx) -. (sx *. sx)))

(* a click through the reverb, the reverb alone (the left side) *)
let response (kind : Reverb.kind) ~(seconds : float) ~(damping : float) : Signal.t =
  let n = Signal.samples ((2.5 *. seconds) +. 0.5) in
  let s = { Signal.left = Array.make n 0.; right = Array.make n 0. } in
  s.left.(0) <- 1.;
  s.right.(0) <- 1.;
  Reverb.process (Reverb.create ()) { kind; seconds; damping; mix = 1. } s;
  s.left.(0) <- s.left.(0) -. 1.;
  s.left

let highs (x : Signal.t) : Signal.t =
  let hp = Filter.biquad High_pass ~cutoff:4000. ~q:0.707 in
  Filter.run hp (Filter.run hp x)

let test_times () =
  List.iter
    (fun (kind, one, two) ->
      let check seconds expected =
        Alcotest.(check (float 0.01))
          (Printf.sprintf "%s set to %.0f s" (Reverb.name kind) seconds)
          expected
          (t60 (response kind ~seconds ~damping:0.))
      in
      check 1. one;
      check 2. two)
    [ (Schroeder, 1.02, 2.00); (Freeverb, 1.00, 2.00); (Plate, 1.14, 1.94) ]

let test_damping () =
  let fv = response Freeverb ~seconds:2. ~damping:1. and plate = response Plate ~seconds:2. ~damping:1. in
  Alcotest.(check (float 0.01)) "Freeverb damped: the whole tail" 1.59 (t60 fv);
  Alcotest.(check (float 0.01)) "Freeverb damped: its highs" 0.58 (t60 (highs fv));
  Alcotest.(check (float 0.01)) "plate damped: the whole tail" 1.71 (t60 plate);
  Alcotest.(check (float 0.01)) "plate damped: its highs" 1.24 (t60 (highs plate))

(* Freeverb's two sides differ (the right's lines longer): wide *)
let test_stereo () =
  let n = Signal.samples 0.5 in
  let s = { Signal.left = Array.make n 0.; right = Array.make n 0. } in
  s.left.(0) <- 1.;
  s.right.(0) <- 1.;
  Reverb.process (Reverb.create ()) { kind = Freeverb; seconds = 1.; damping = 0.; mix = 1. } s;
  let differ = ref 0. in
  Array.iteri (fun i l -> differ := Float.max !differ (Float.abs (l -. s.right.(i)))) s.left;
  Alcotest.(check bool) "left and right differ" true (!differ > 0.01)

let tests =
  Testo.categorize "Reverb"
    [
      t "the time to fall 60 dB, against the setting" test_times;
      t "the damping: the highs die first" test_damping;
      t "Freeverb: two sides, wide" test_stereo;
    ]
