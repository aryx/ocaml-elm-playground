(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_sequencer.mli *)

let t = Testo.create

(* the events of [samples], in blocks of [block], each at its sample
 * from the start, as text *)
let events ?(bpm = 120.) (pattern : Sequencer.step array) (block : int) (samples : int) : string list =
  let s = Sequencer.create ~bpm pattern in
  Sequencer.start s;
  let out = ref [] and at = ref 0 in
  while !at < samples do
    let n = min block (samples - !at) in
    let base = !at in
    Sequencer.advance s n (fun offset e ->
        let what =
          match e with
          | Note_on { note; accent; glide } ->
              Printf.sprintf "on %d%s%s" note (if accent then " accent" else "") (if glide then " glide" else "")
          | Note_off -> "off"
        in
        out := Printf.sprintf "%d %s" (base + offset) what :: !out);
    at := !at + n
  done;
  List.rev !out

let four = Sequencer.[| note 36; note 36; note 39; note 41 |]

let test_times () =
  Alcotest.(check (float 1e-9)) "a step at 120 BPM: 5512.5 samples" 5512.5 (Sequencer.samples_per_step 120.);
  let expected =
    [ "0 on 36"; "2757 off"; "5513 on 36"; "8269 off"; "11025 on 39"; "13782 off"; "16538 on 41"; "19294 off" ]
  in
  List.iter
    (fun block -> Alcotest.(check (list string)) (Printf.sprintf "in blocks of %d" block) expected (events four block 22000))
    [ 735; 500; 1 ]

let test_slide () =
  let p = Sequencer.[| note 36 ~slide:true; note 43 ~accent:true; rest; note 36 ~slide:true; rest |] in
  Alcotest.(check (list string))
    "a slide: no gate-off, the next note glided to; a slide into a rest is none, its gate closing at half a step"
    [ "0 on 36"; "5513 on 43 accent glide"; "8269 off"; "16538 on 36"; "19294 off" ]
    (events p 735 24000)

let test_stop () =
  let s = Sequencer.create four in
  Sequencer.start s;
  let count = ref 0 in
  Sequencer.advance s 735 (fun _ _ -> incr count);
  Sequencer.stop s;
  Sequencer.advance s 44100 (fun _ _ -> incr count);
  Alcotest.(check int) "stopped: no more events" 1 !count

(* the worked example: cutoff locked at 0.2 on step 1 and 0.8 on step
 * 9, read as steps 1, 5, 7 and 15 begin *)
let test_locks () =
  let p = Array.make 16 (Sequencer.note 36) in
  p.(0) <- Sequencer.lock p.(0) "cutoff" 0.2;
  p.(8) <- Sequencer.lock p.(8) "cutoff" 0.8;
  let read locks = List.map (fun k -> Sequencer.lock_value locks p "cutoff" (float_of_int (k - 1))) [ 1; 5; 7; 15 ] in
  let check name expected locks = Alcotest.(check (list (option (float 1e-9)))) name expected (read locks) in
  check "Per_step: the step's only" [ Some 0.2; None; None; None ] Per_step;
  check "Points 0: held till the next" [ Some 0.2; Some 0.2; Some 0.2; Some 0.8 ] (Points 0.);
  check "Points 1: a line to the next, round the end" [ Some 0.2; Some 0.5; Some 0.65; Some 0.35 ] (Points 1.);
  check "Points 0.5: the last half of the way" [ Some 0.2; Some 0.2; Some 0.5; Some 0.5 ] (Points 0.5);
  Alcotest.(check (option (float 1e-9))) "no lock: the knob's own" None (Sequencer.lock_value (Points 1.) p "resonance" 3.)

(* the position in the audio clock: half a step into step 3, whatever
 * the blocks *)
let test_position () =
  List.iter
    (fun block ->
      let s = Sequencer.create four in
      Sequencer.start s;
      let target = 11025 + 2756 (* step 3 begins at 11025, half a step on *) in
      let at = ref 0 and found = ref None in
      while !found = None do
        let n = min block (target + 1 - !at) in
        Sequencer.advance s n (fun _ _ -> ());
        if !at + n > target then found := Sequencer.position s (target - !at);
        at := !at + n
      done;
      Alcotest.(check (option (float 1e-3))) (Printf.sprintf "in blocks of %d" block) (Some 2.5) !found)
    [ 735; 500; 1 ]

let tests =
  Testo.categorize "Sequencer"
    [
      t "the steps in the audio clock, whatever the blocks" test_times;
      t "slides and rests" test_slide;
      t "stopped" test_stop;
      t "parameter locks: per step, and points" test_locks;
      t "the position, for the locks" test_position;
    ]
