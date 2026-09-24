(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tape.mli *)

let t = Testo.create
let n = 4096

(* the phrase: a sine on bin 32 of 4096 (344.5 Hz), so an octave up
 * lands on bin 64 exactly *)
let hz = Spectrum.bin_frequency ~n 32
let phrase = Signal.of_function 1. (fun t -> 0.5 *. sin (2. *. Float.pi *. hz *. t))

(* [run tape ~input samples]: the tape processed over [samples], in
 * blocks of 512 as a platform does *)
let run (tape : Tape.t) ?(input = [||]) (samples : int) : Signal.t =
  let out = Array.make samples 0. in
  let block = 512 in
  let rec go at =
    if at < samples then begin
      let len = min block (samples - at) in
      let inp = Array.init len (fun i -> if at + i < Array.length input then input.(at + i) else 0.) in
      let o = Array.make len 0. in
      Tape.process tape ~input:inp o;
      Array.blit o 0 out at len;
      go (at + len)
    end
  in
  go 0;
  out

let recorded ?(speed = 1.) () : Tape.t =
  let tape = Tape.create ~seconds:3. ~tracks:4 () in
  Tape.set_speed tape speed;
  Tape.record tape 0;
  ignore (run tape ~input:phrase (Array.length phrase));
  Tape.stop tape;
  Tape.set_head tape 0.;
  tape

let max_diff (a : Signal.t) (b : Signal.t) : float =
  let d = ref 0. in
  Array.iteri (fun i x -> d := Float.max !d (Float.abs (x -. b.(i)))) a;
  !d

let peak_bin (s : Signal.t) : int = Spectrum.peak (Spectrum.magnitudes (Spectrum.fft (Spectrum.hann (Array.sub s 1000 n))))

(* where the sound stops: the last sample louder than 0.01 *)
let last_sound (s : Signal.t) : int =
  let last = ref (-1) in
  Array.iteri (fun i x -> if Float.abs x > 0.01 then last := i) s;
  !last

(* recorded at 1: played back as it went in; at 2, an octave up in half
 * the time *)
let test_speed () =
  let tape = recorded () in
  Tape.play tape;
  let out = run tape 44100 in
  Alcotest.(check (float 1e-12)) "played back at 1: the phrase" 0. (max_diff out phrase);
  let tape = recorded () in
  Tape.set_speed tape 2.;
  Tape.play tape;
  let out = run tape 44100 in
  Alcotest.(check int) "at 2: an octave up (bin 64)" 64 (peak_bin out);
  Alcotest.(check int) "in half the time" 22049 (last_sound out)

(* backwards: the recording's samples reversed; recorded at 0.5 and
 * played at 1: an octave up too *)
let test_reverse_and_slow () =
  let tape = recorded () in
  Tape.set_head tape 44099.;
  Tape.set_speed tape (-1.);
  Tape.play tape;
  let out = run tape 44100 in
  let reversed = Array.init 44100 (fun i -> phrase.(44099 - i)) in
  Alcotest.(check (float 1e-12)) "at -1: the phrase backwards" 0. (max_diff out reversed);
  Alcotest.(check bool) "stopped at the start" false (Tape.moving tape);
  let tape = recorded ~speed:0.5 () in
  Tape.set_speed tape 1.;
  Tape.play tape;
  let out = run tape 44100 in
  Alcotest.(check int) "recorded at 0.5, played at 1: bin 64" 64 (peak_bin out);
  (* one more than at 2: the last input sample, written at
   * 22,049.5, spread half onto 22,050 *)
  Alcotest.(check int) "in half the time" 22050 (last_sound out)

(* recording again over the phrase: the two added; lift and drop *)
let test_overdub_lift_drop () =
  let tape = recorded () in
  Tape.record tape 0;
  ignore (run tape ~input:phrase 44100);
  Alcotest.(check (float 1e-12)) "overdubbed: twice the phrase" 0.
    (max_diff (Array.sub (Tape.track tape 0) 0 44100) (Array.map (fun x -> 2. *. x) phrase));
  let tape = recorded () in
  Tape.lift tape 0 ~from:1000 ~until:2000;
  Alcotest.(check (float 0.)) "lifted: silence left" 0. (Tape.track tape 0).(1500);
  Tape.set_head tape 50000.;
  Tape.drop tape 0;
  Tape.drop tape 1;
  Alcotest.(check (float 0.)) "dropped at the head" phrase.(1500) (Tape.track tape 0).(50500);
  Alcotest.(check (float 0.)) "dropped again, on track 2" phrase.(1500) (Tape.track tape 1).(50500)

(* a loop from 100 to 200: the head going round, the same 100 samples
 * again and again *)
let test_loop () =
  let tape = recorded () in
  Tape.set_loop tape (Some (100, 200));
  Tape.set_head tape 100.;
  Tape.play tape;
  let out = run tape 1000 in
  Alcotest.(check (float 0.)) "the loop's 100 samples, the tenth time round" phrase.(150) out.(950);
  Alcotest.(check (float 1e-9)) "the head still in the loop" 100. (Tape.head tape)

let tests =
  Testo.categorize "Tape"
    [
      t "played faster: higher and shorter" test_speed;
      t "backwards, and recorded slowly" test_reverse_and_slow;
      t "overdub, lift and drop" test_overdub_lift_drop;
      t "the loop" test_loop;
    ]
