(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_sampler.mli *)

let t = Testo.create

(* a second of a 440 Hz sine, recorded at A4 *)
let a440 : Sampler.sample = { data = Oscillator.render Sine ~frequency:440. 1.; root = 69 }

(* [play voice n ~release]: [n] samples, the key let go at sample
 * [release], a frame's block at a time *)
let play ?(release = max_int) (v : Polyphony.voice) (n : int) : Signal.t =
  let out = Array.make n 0. in
  let at = ref 0 in
  while !at < n do
    if !at <= release && release < !at + 735 then v.release ();
    let m = min 735 (n - !at) in
    let b = Array.make m 0. in
    v.fill b;
    Array.blit b 0 out !at m;
    at := !at + m
  done;
  out

let crossings (s : Signal.t) : int =
  let c = ref 0 in
  for i = 1 to Array.length s - 1 do
    if s.(i - 1) < 0. && s.(i) >= 0. then incr c
  done;
  !c

let rms (s : Signal.t) (a : int) (b : int) : float =
  let sum = ref 0. in
  for i = a to b - 1 do
    sum := !sum +. (s.(i) *. s.(i))
  done;
  sqrt (!sum /. float_of_int (b - a))

let test_pitch () =
  let v = Sampler.voice a440 Sampler.default ~key:81 ~velocity:1. in
  let x = play v 44100 in
  Alcotest.(check (float 1e-9)) "an octave up: half as long" 0.5 (Sampler.seconds a440 Sampler.default 81);
  Alcotest.(check int) "880 Hz: 440 periods in its half second, 439 rising crossings between them" 439 (crossings (Array.sub x 0 22050));
  Alcotest.(check bool) "then silent" true (v.silent () && rms x 22060 44100 = 0.)

let test_backward () =
  let ramp : Sampler.sample = { data = Array.init 1000 (fun i -> float_of_int i /. 1000.); root = 60 } in
  let x = play (Sampler.voice ramp { Sampler.default with direction = Backward } ~key:60 ~velocity:1.) 1000 in
  Alcotest.(check (list (float 1e-9))) "a rising ramp, falling: its last sample first" [ 0.999; 0.5; 0. ] [ x.(0); x.(499); x.(999) ]

(* a loop from a crest of the sine (a quarter period after 0.25 s),
 * 100.5 periods of 440 Hz long: its end half a period out of phase with
 * its start, a trough; the largest step from a sample to the next, and
 * the quietest period (a sine's step is at most 2 pi 440 / 44100 =
 * 0.0627, its period's rms 0.707). From 0.25 s itself the seam would
 * fall on zero crossings, and no click -- the old way to loop, the loop
 * points put on zero crossings, which a sine allows and a rich sound
 * rarely does *)
let test_seam () =
  let loop crossfade =
    let start = 0.25 +. (0.25 /. 440.) in
    let st = { Sampler.default with loop_start = start; loop_end = start +. (100.5 /. 440.); loop = Forever; crossfade } in
    let x = play (Sampler.voice a440 st ~key:69 ~velocity:1.) 44100 in
    let step = ref 0. in
    for i = 1 to Array.length x - 1 do
      step := Float.max !step (Float.abs (x.(i) -. x.(i - 1)))
    done;
    let quietest = ref 1. in
    for p = 0 to 400 do
      quietest := Float.min !quietest (rms x (p * 100) ((p * 100) + 100))
    done;
    (!step, !quietest)
  in
  let r x = Float.round (x *. 100.) /. 100. in
  let step, quietest = loop 0. in
  Alcotest.(check (pair (float 0.005) (float 0.005))) "no crossfade: a click at each seam, from a crest to a trough" (2., 0.71) (r step, r quietest);
  let step, quietest = loop 0.2 in
  Alcotest.(check (pair (float 0.005) (float 0.005))) "a crossfade of 0.2: no click, but a dip (the two in opposite phase)" (0.06, 0.03) (r step, r quietest)

(* the loop modes, the key let go at 0.2 s: "until release" plays its
 * region's rest after it, "forever" fades in 10 ms *)
let test_modes () =
  let st loop = { Sampler.default with loop_start = 0.1; loop_end = 0.2; loop; release = (if loop = Forever then Some 0.01 else None) } in
  let x = play ~release:8820 (Sampler.voice a440 (st Until_release) ~key:69 ~velocity:1.) 88200 in
  Alcotest.(check bool) "until release: sounding 0.5 s after the key" true (rms x 30870 31870 > 0.5);
  let y = play ~release:8820 (Sampler.voice a440 (st Forever) ~key:69 ~velocity:1.) 88200 in
  Alcotest.(check (float 1e-9)) "forever: silent 10 ms after the key" 0. (rms y (8820 + 735 + 441) 88200)

(* the hats in the mute group: the open one ringing, the closed one
 * choking it; a pad panned left *)
let test_kit () =
  let noise seconds : Sampler.sample = { data = Noise.render ~rate:44100. seconds; root = 60 } in
  let kit =
    Sampler.kit
      [| Sampler.pad ~play:Mute_group (noise 1.); Sampler.pad ~play:Mute_group (noise 0.05); Sampler.pad ~pan:(-1.) (noise 0.1) |]
  in
  let block () =
    let b = { Signal.left = Array.make 735 0.; right = Array.make 735 0. } in
    Sampler.fill kit b;
    b
  in
  Sampler.press kit 36 1.;
  ignore (block ());
  Sampler.press kit 37 1.;
  Alcotest.(check int) "the open hat and the closed one" 2 (Sampler.sounding kit);
  ignore (block ());
  Alcotest.(check int) "the open one choked, 5 ms" 1 (Sampler.sounding kit);
  for _ = 1 to 5 do ignore (block ()) done;
  Sampler.press kit 38 1.;
  let b = block () in
  Alcotest.(check (pair (float 1e-9) bool)) "panned left: the right side silent, the left not" (0., true)
    (rms b.right 0 735, rms b.left 0 735 > 0.1)

let tests =
  Testo.categorize "Sampler"
    [
      t "a key's pitch: faster and shorter" test_pitch;
      t "backwards" test_backward;
      t "the loop's seam: the click, the crossfade" test_seam;
      t "until release, forever" test_modes;
      t "the drum kit: choke and pan" test_kit;
    ]
