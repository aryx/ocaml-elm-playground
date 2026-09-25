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

(*****************************************************************************)
(* The studio *)
(*****************************************************************************)

(* [studio patch f seconds]: [f frame t] each frame (735 samples), the
 * left side out *)
(* dr wave: a sawtooth (type 0) long enough, filtered to 7 harmonics at
 * 220 Hz: the 7th at 1/7, the 8th gone; its phase moved: the peaks
 * changed, the harmonics the same *)
let test_dr_wave () =
  let db x = 20. *. log10 x in
  let p = [| 0.2; 0.1; 0.; 0. |] in
  let x = play Op1_engine.dr_wave p 220. 0.5 in
  let h a k = amplitude a (220. *. float_of_int k) 4410 8192 in
  Alcotest.(check (float 0.1)) "the 7th harmonic: 1/7 of the first (dB)" (db (1. /. 7.)) (db (h x 7 /. h x 1));
  Alcotest.(check bool) (Printf.sprintf "the 8th: removed (%.0f dB)" (db (h x 8 /. h x 1))) true (db (h x 8 /. h x 1) < -80.);
  let y = play Op1_engine.dr_wave [| 0.2; 0.1; 0.5; 0. |] 220. 0.5 in
  let peak a = Array.fold_left (fun m v -> Float.max m (Float.abs v)) 0. a in
  let same = List.for_all (fun k -> Float.abs (db (h y k /. h x k)) < 0.05) [ 1; 2; 3; 4; 5; 6; 7 ] in
  Alcotest.(check (pair bool bool)) "the phase moved: the harmonics the same, the peak not" (true, true)
    (same, Float.abs ((peak y /. peak x) -. 1.) > 0.1)

(* voltage: a sawtooth crossfading to a square an octave up in 98 ms:
 * the note's own frequency at first, half a second on 44 dB down (a
 * square an octave up has nothing there; what's left is the naive
 * square's aliases, folded back) *)
let test_voltage () =
  let x = play Op1_engine.voltage [| 0.3; 0.6; 0.; 0. |] 220. 1. in
  let at a = amplitude x 220. a 2048 in
  let down = 20. *. log10 (at 22050 /. at 0) in
  Alcotest.(check bool) (Printf.sprintf "220 Hz, half a second on: %.0f dB" down) true (down < -40.)

(* d-synth: the pitch falling, the zero crossings in the first 10 ms
 * against 100 to 110 ms *)
let test_d_synth () =
  let x = play Op1_engine.d_synth [| 0.5; 0.; 0.8; 0. |] 261.63 0.2 in
  let crossings a b = List.length (List.filter (fun i -> x.(i - 1) < 0. && x.(i) >= 0.) (List.init (b - a) (fun i -> a + i))) in
  let early = crossings 1 441 and late = crossings 4410 4851 in
  Alcotest.(check bool) (Printf.sprintf "crossings early %d, late %d" early late) true (early >= 2 * late && late >= 0)

(* the sampler: a 440 Hz sine as its recording at C4, played at C4 and
 * C5: 440 and 880 Hz *)
let test_sampler () =
  let before = Op1_engine.sample () in
  Op1_engine.set_sample { data = Oscillator.render Sine ~frequency:440. 2.; root = 60 };
  let f key =
    let x = play Op1_engine.sampler [| 0.; 0.4; 0.8; 1. |] (440. *. Float.pow 2. (float_of_int (key - 69) /. 12.)) 1. in
    List.length (List.filter (fun i -> x.(i - 1) < 0. && x.(i) >= 0.) (List.init 44099 (fun i -> i + 1)))
  in
  let c4 = f 60 and c5 = f 72 in
  Op1_engine.set_sample before;
  Alcotest.(check (pair int int)) "rising crossings in a second: 440 and 880" (439, 879) (c4, c5)

let studio ?(frames = fun _ _ -> ()) (p : Studio_op1.patch) (seconds : float) : Signal.t * Studio_op1.t =
  let s = Studio_op1.create p in
  let i = Studio_op1.instrument s in
  let n = Signal.samples seconds in
  let out = Array.make n 0. in
  let k = ref 0 and frame = ref 0 in
  while !k < n do
    frames !frame s;
    let m = min 735 (n - !k) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !k m;
    k := !k + m;
    incr frame
  done;
  (out, s)

let rms (x : Signal.t) (a : float) (b : float) : float =
  let i = Signal.samples a and j = Signal.samples b in
  sqrt (Array.fold_left ( +. ) 0. (Array.map (fun v -> v *. v) (Array.sub x i (j - i))) /. float_of_int (j - i))

(* a sine (FM at amount 0) through an envelope: attack 0.1 s, sustain
 * 0.5, release; its level in dB through the note *)
let sine_sound ?(effect_on = false) ?(effect = 0) ?(lfo_on = false) ?(lfo_params = [| 0.4; 0.; 0.8; 0.5 |]) () : Studio_op1.sound =
  { (Studio_op1.initial.sounds.(0)) with engine = 0; engine_params = [| 0.; 0.; 0.; 0. |]; envelope = [| 0.5; 0.5; 0.5; 0.5 |]; effect_on; effect; lfo_on; lfo = 0; lfo_params }

let with_sound (s : Studio_op1.sound) : Studio_op1.patch = { Studio_op1.initial with sounds = Array.make 8 s; current = 0 }

let note_held ?(until = 1.) (s : Studio_op1.sound) (seconds : float) : Signal.t =
  let release = Float.to_int (until *. 60.) in
  fst (studio ~frames:(fun f st -> let i = Studio_op1.instrument st in if f = 0 then i.note_on 69 1. else if f = release then i.note_off 69) (with_sound s) seconds)

let test_envelope () =
  let x = note_held (sine_sound ()) 2. in
  let db a b = Float.round (20. *. log10 (rms x a b) *. 10.) /. 10. in
  (* full, a sine at 0.35 is -12.1 dB; the sustain 0.5 six under it;
   * let go at 1 s, the release 0.1 s long *)
  Alcotest.(check (list (float 0.1))) "the level: rising, at the top, sustained, released (dB)" [ -18.4; -14.3; -18.1; -40.8 ]
    [ db 0.02 0.05; db 0.1 0.12; db 0.8 0.9; db 1.03 1.05 ]

(* each effect on against off *)
let test_effects () =
  let off = note_held (sine_sound ()) 1.5 in
  List.iteri
    (fun k name ->
      let on = note_held (sine_sound ~effect_on:true ~effect:k ()) 1.5 in
      let d = Array.fold_left Float.max 0. (Array.mapi (fun i x -> Float.abs (x -. off.(i))) on) in
      Alcotest.(check bool) (Printf.sprintf "%s changes the sound (by %.3f)" name d) true (d > 0.01))
    Studio_op1.effects

(* the tremolo: the loudness's swing over 20 ms windows *)
let test_tremolo () =
  let x = note_held (sine_sound ~lfo_on:true ()) 1.5 in
  let windows = List.init 20 (fun k -> rms x (0.5 +. (0.02 *. float_of_int k)) (0.52 +. (0.02 *. float_of_int k))) in
  let lo = List.fold_left Float.min 1. windows and hi = List.fold_left Float.max 0. windows in
  Alcotest.(check (float 0.01)) "the quietest window against the loudest" 0.32 (lo /. hi)

(* a phrase recorded on track 1, then played back alone: the same, at
 * the track's level (0.8) *)
let test_tape () =
  let s = { (Studio_op1.initial.sounds.(2)) with effect_on = false; lfo_on = false } in
  let p = with_sound s in
  let recorded, st =
    studio
      ~frames:(fun f st ->
        let i = Studio_op1.instrument st in
        if f = 0 then Studio_op1.record st 0;
        if f = 2 then i.note_on 60 1.;
        if f = 20 then i.note_off 60;
        if f = 25 then i.note_on 67 1.;
        if f = 45 then i.note_off 67)
      p 1.5
  in
  Studio_op1.stop st;
  Tape.set_head (Studio_op1.tape st) 0.;
  Studio_op1.play st;
  let i = Studio_op1.instrument st in
  let n = Array.length recorded in
  let back = Array.make n 0. in
  let k = ref 0 in
  while !k < n do
    let m = min 735 (n - !k) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 back !k m;
    k := !k + m
  done;
  let d = ref 0. in
  Array.iteri (fun j x -> d := Float.max !d (Float.abs ((0.8 *. x) -. back.(j)))) recorded;
  Alcotest.(check (float 1e-9)) "played back: the phrase at 0.8" 0. !d

(* each of the eight sounds on a phrase *)
let sound_phrase (k : int) : Signal.t =
  fst
    (studio
       ~frames:(fun f st ->
         let i = Studio_op1.instrument st in
         List.iter (fun (at, key) -> if f = at then i.note_on key 0.8 else if f = at + 18 then i.note_off key) [ (0, 60); (20, 64); (40, 67); (60, 72) ])
       { Studio_op1.initial with current = k }
       2.)

(* each engine at its middle, four notes of an arpeggio *)
let phrase (e : Op1_engine.t) : Signal.t =
  Array.concat (List.map (fun f -> Array.map (fun x -> 0.5 *. x) (play e [| 0.5; 0.5; 0.5; 0.5 |] f 0.3)) [ 220.; 277.18; 329.63; 440. ])

let tests =
  Testo.categorize "OP-1"
    (List.map
       (fun (e : Op1_engine.t) ->
         (* a file's name without spaces: dr wave's op1_dr_wave *)
         let file = "op1_" ^ String.map (fun c -> if c = ' ' then '_' else c) e.name in
         t ("golden WAV: " ^ e.name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" file (phrase e)))
       Op1_engine.all
    @ [
        t "FM: four operators, amount 0 a sine" test_fm;
        t "cluster: the spread" test_cluster;
        t "string: its pitch" test_string;
        t "phase distortion: the harmonics with the amount" test_phase;
        t "digital: its levels" test_digital;
        t "dr wave: the brick wall, the phase unheard" test_dr_wave;
        t "voltage: the crossfade" test_voltage;
        t "d-synth: the pitch falling" test_d_synth;
        t "sampler: the recording at the key's pitch" test_sampler;
        t "the studio: a note's envelope" test_envelope;
        t "the studio: the effects" test_effects;
        t "the studio: the tremolo" test_tremolo;
        t "the studio: the tape, recorded and played back" test_tape;
      ]
    @ List.init 8 (fun k -> t (Printf.sprintf "golden WAV: sound %d" (k + 1)) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" (Printf.sprintf "op1_sound%d" (k + 1)) (sound_phrase k))))
