(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_fm_algorithm.mli *)

let t = Testo.create

(*****************************************************************************)
(* Dexed's table *)
(*****************************************************************************)

(* Dexed's algorithms (fm_core.cc): a byte per operator, 6 first; bits
 * 0-1 the bus written (0 the output), 2 added rather than written, 4-5
 * the bus read, 6 feedback in, 7 feedback out *)
let dexed =
  [|
    [| 0xc1; 0x11; 0x11; 0x14; 0x01; 0x14 |]; [| 0x01; 0x11; 0x11; 0x14; 0xc1; 0x14 |]; [| 0xc1; 0x11; 0x14; 0x01; 0x11; 0x14 |];
    [| 0xc1; 0x11; 0x94; 0x01; 0x11; 0x14 |]; [| 0xc1; 0x14; 0x01; 0x14; 0x01; 0x14 |]; [| 0xc1; 0x94; 0x01; 0x14; 0x01; 0x14 |];
    [| 0xc1; 0x11; 0x05; 0x14; 0x01; 0x14 |]; [| 0x01; 0x11; 0xc5; 0x14; 0x01; 0x14 |]; [| 0x01; 0x11; 0x05; 0x14; 0xc1; 0x14 |];
    [| 0x01; 0x05; 0x14; 0xc1; 0x11; 0x14 |]; [| 0xc1; 0x05; 0x14; 0x01; 0x11; 0x14 |]; [| 0x01; 0x05; 0x05; 0x14; 0xc1; 0x14 |];
    [| 0xc1; 0x05; 0x05; 0x14; 0x01; 0x14 |]; [| 0xc1; 0x05; 0x11; 0x14; 0x01; 0x14 |]; [| 0x01; 0x05; 0x11; 0x14; 0xc1; 0x14 |];
    [| 0xc1; 0x11; 0x02; 0x25; 0x05; 0x14 |]; [| 0x01; 0x11; 0x02; 0x25; 0xc5; 0x14 |]; [| 0x01; 0x11; 0x11; 0xc5; 0x05; 0x14 |];
    [| 0xc1; 0x14; 0x14; 0x01; 0x11; 0x14 |]; [| 0x01; 0x05; 0x14; 0xc1; 0x14; 0x14 |]; [| 0x01; 0x14; 0x14; 0xc1; 0x14; 0x14 |];
    [| 0xc1; 0x14; 0x14; 0x14; 0x01; 0x14 |]; [| 0xc1; 0x14; 0x14; 0x01; 0x14; 0x04 |]; [| 0xc1; 0x14; 0x14; 0x14; 0x04; 0x04 |];
    [| 0xc1; 0x14; 0x14; 0x04; 0x04; 0x04 |]; [| 0xc1; 0x05; 0x14; 0x01; 0x14; 0x04 |]; [| 0x01; 0x05; 0x14; 0xc1; 0x14; 0x04 |];
    [| 0x04; 0xc1; 0x11; 0x14; 0x01; 0x14 |]; [| 0xc1; 0x14; 0x01; 0x14; 0x04; 0x04 |]; [| 0x04; 0xc1; 0x11; 0x14; 0x04; 0x04 |];
    [| 0xc1; 0x14; 0x04; 0x04; 0x04; 0x04 |]; [| 0xc4; 0x04; 0x04; 0x04; 0x04; 0x04 |];
  |]

(* the bytes run as Dexed runs them, the buses holding who wrote them *)
let decode (number : int) (bytes : int array) : Fm_algorithm.t =
  let buses = [| []; []; [] |] and edges = ref [] and carriers = ref [] and into = ref 0 and from = ref 0 in
  Array.iteri
    (fun i byte ->
      let op = 6 - i in
      let read = (byte lsr 4) land 3 and written = byte land 3 in
      if byte land 0x40 <> 0 then into := op;
      if byte land 0x80 <> 0 then from := op;
      if read > 0 then List.iter (fun m -> edges := (m, op) :: !edges) buses.(read);
      if written = 0 then carriers := op :: !carriers
      else if byte land 4 <> 0 then buses.(written) <- buses.(written) @ [ op ]
      else buses.(written) <- [ op ])
    bytes;
  { number; edges = List.rev !edges; carriers = List.rev !carriers; feedback = (!from, !into) }

let test_table () =
  List.iter
    (fun (alg : Fm_algorithm.t) ->
      let theirs = decode alg.number dexed.(alg.number - 1) in
      let sort l = List.sort compare l in
      Alcotest.(check (list (pair int int))) (Printf.sprintf "algorithm %d: who modulates whom" alg.number) (sort theirs.edges) (sort alg.edges);
      Alcotest.(check (list int)) (Printf.sprintf "algorithm %d: heard" alg.number) (sort theirs.carriers) (sort alg.carriers);
      Alcotest.(check (pair int int)) (Printf.sprintf "algorithm %d: fed back" alg.number) theirs.feedback alg.feedback;
      List.iter (fun (m, o) -> Alcotest.(check bool) "a modulator above its target" true (m > o)) alg.edges)
    Fm_algorithm.all;
  Alcotest.(check int) "32" 32 (List.length Fm_algorithm.all)

(*****************************************************************************)
(* Running them *)
(*****************************************************************************)

let rate = float_of_int Signal.rate

let run (alg : Fm_algorithm.t) ~feedback ~(frequencies : float array) ~(amplitudes : float array) (n : int) : Signal.t =
  let s = Fm_algorithm.create () and increments = Array.map (fun f -> f /. rate) frequencies in
  Array.init n (fun _ -> Fm_algorithm.sample alg s ~feedback ~increments ~amplitudes)

(* Chowning's pair inside algorithm 1: 2 on 1, the others silent *)
let test_pair () =
  let index = 1. and f = 440. in
  let ours =
    run (Fm_algorithm.get 1) ~feedback:0 ~frequencies:[| f; f; 0.; 0.; 0.; 0. |]
      ~amplitudes:[| 1.; index /. (2. *. Float.pi); 0.; 0.; 0.; 0. |]
      4410
  in
  let theirs = Fm.render ~carrier:f ~ratio:1. ~index 0.1 in
  let d = ref 0. in
  Array.iteri (fun i x -> d := Float.max !d (Float.abs (x -. theirs.(i)))) ours;
  Alcotest.(check bool) (Printf.sprintf "Fm.render's, sample for sample (%.1e)" !d) true (!d < 1e-9)

(* a lone operator fed back: algorithm 32, op 6 alone at full, at 320 x
 * 44100 / 32768 Hz (431 Hz, bin 320 exactly); its harmonics 2 to 5
 * under the first, and the energy between the harmonics against the
 * energy on them (dB) *)
let spectrum (feedback : int) : float list * float =
  let n = 32768 and f = 320. *. rate /. 32768. in
  let s = run (Fm_algorithm.get 32) ~feedback ~frequencies:(Array.make 6 f) ~amplitudes:[| 0.; 0.; 0.; 0.; 0.; 2. |] (4096 + n) in
  let m = Spectrum.magnitudes (Spectrum.fft (Spectrum.hann (Array.sub s 4096 n))) in
  let on = ref 0. and off = ref 0. in
  for i = 1 to (n / 2) - 1 do
    let d = i mod 320 in
    if d <= 2 || d >= 318 then on := !on +. (m.(i) *. m.(i)) else off := !off +. (m.(i) *. m.(i))
  done;
  (List.map (fun k -> 20. *. log10 (m.(320 * k) /. m.(320))) [ 2; 3; 4; 5 ], 10. *. log10 (!off /. !on))

let test_feedback () =
  let harmonics, noise = spectrum 5 in
  Alcotest.(check (list (float 0.1))) "fb 5: harmonics 2-5 (dB), a darkened sawtooth" [ -7.2; -11.6; -14.8; -17.4 ] harmonics;
  Alcotest.(check (float 0.1)) "fb 5: the noise (dB)" (-58.1) noise;
  Alcotest.(check (float 0.1)) "fb 6: buzzing (dB)" (-7.7) (snd (spectrum 6));
  Alcotest.(check (float 0.1)) "fb 7: noise above the harmonics (dB)" 5.5 (snd (spectrum 7))

let tests =
  Testo.categorize "Fm_algorithm"
    [ t "the 32, against Dexed's table" test_table; t "2 on 1: Chowning's pair" test_pair; t "feedback: a sine turned sawtooth" test_feedback ]
