(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Voicing: the priorities of the .mli, legato and retrigger, the
 * glide's worked example *)

let t = Testo.create

let event : Voicing.event Alcotest.testable =
  Alcotest.testable
    (fun fmt (e : Voicing.event) ->
      Format.pp_print_string fmt
        (match e with Begin n -> Printf.sprintf "Begin %d" n | Change n -> Printf.sprintf "Change %d" n | End -> "End" | Nothing -> "Nothing"))
    ( = )

(* C4 60, D4 62, E4 64 *)
let test_priorities () =
  List.iter
    (fun (priority, name, after_e, after_d) ->
      let v = Voicing.create ~priority () in
      Alcotest.check event (name ^ ": C4 from silence") (Begin 60) (Voicing.press v 60);
      Alcotest.check event (name ^ ": then E4") after_e (Voicing.press v 64);
      Alcotest.check event (name ^ ": then D4") after_d (Voicing.press v 62);
      ignore (Voicing.release v 62);
      ignore (Voicing.release v 64);
      Alcotest.(check (option int)) (name ^ ": C4 left") (Some 60) (Voicing.sounding v);
      Alcotest.check event (name ^ ": all let go") End (Voicing.release v 60);
      Alcotest.check event (name ^ ": a key up that wasn't down") Nothing (Voicing.release v 60))
    [
      (Voicing.Low, "low", Voicing.Nothing, Voicing.Nothing);
      (High, "high", Change 64, Nothing);
      (Last, "last", Change 64, Change 62);
    ]

(* the trill: C4 held, E4 tapped; legato, then retriggered *)
let test_trill () =
  let v = Voicing.create () in
  ignore (Voicing.press v 60);
  Alcotest.check event "E4 pressed: legato" (Change 64) (Voicing.press v 64);
  Alcotest.check event "let go: back to the held C4" (Change 60) (Voicing.release v 64);
  let r = Voicing.create ~retrigger:true () in
  ignore (Voicing.press r 60);
  Alcotest.check event "retriggered: a new beginning" (Begin 64) (Voicing.press r 64);
  Alcotest.check event "and back" (Begin 60) (Voicing.release r 64)

let test_glide () =
  Alcotest.(check (float 1e-9)) "A4 at 440 Hz" 440. (Voicing.frequency 69.);
  let g = Voicing.glide ~note:60. () in
  Voicing.glide_to g 72;
  let second = Array.make 4410 0. in
  Voicing.fill_frequency g ~seconds:0.1 second;
  Alcotest.(check (float 0.01)) "after tau: 63.2% of the way, 67.59" 67.59 (Voicing.pitch g);
  Alcotest.(check (float 0.1)) "405.5 Hz" 405.5 second.(4409);
  for _ = 1 to 4 do
    Voicing.fill_frequency g ~seconds:0.1 second
  done;
  Alcotest.(check (float 0.01)) "after 5 tau: 71.92, 8 cents under C5" 71.92 (Voicing.pitch g);
  Alcotest.(check (float 0.1)) "520.8 Hz" 520.8 second.(4409);
  (* an octave up and an octave down: the same time to halfway, in
   * semitones; in hertz, halfway down comes first *)
  let halfway ~from ~to_ =
    let g = Voicing.glide ~note:from () and block = Array.make 1 0. in
    Voicing.glide_to g to_;
    let rec go n = if Float.abs (Voicing.pitch g -. from) >= 6. then n else (Voicing.fill_frequency g ~seconds:0.1 block; go (n + 1)) in
    go 0
  in
  Alcotest.(check int) "up and down, the same time" (halfway ~from:60. ~to_:72) (halfway ~from:72. ~to_:60);
  let g = Voicing.glide ~note:60. () and block = Array.make 10 0. in
  Voicing.glide_to g 67;
  Voicing.fill_frequency g ~seconds:0. block;
  Alcotest.(check (float 1e-9)) "no glide: at once" (Voicing.frequency 67.) block.(0)

let tests =
  Testo.categorize "Voicing"
    [
      t "the priorities: low, high, last" test_priorities;
      t "a trill: legato, or retriggered" test_trill;
      t "glide: in semitones, its time constant" test_glide;
    ]
