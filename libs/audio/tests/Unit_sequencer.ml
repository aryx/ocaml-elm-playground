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

let tests =
  Testo.categorize "Sequencer"
    [
      t "the steps in the audio clock, whatever the blocks" test_times;
      t "slides and rests" test_slide;
      t "stopped" test_stop;
    ]
