(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The worked examples of Rhythm.mli. *)

let t = Testo.create

(* Rhythm.mli's example, and the edges of the drawn windows *)
let windows () =
  let is expected e = Alcotest.(check bool) (Printf.sprintf "%+.0f ms" (e *. 1000.)) true (Rhythm.judge e = expected) in
  is (Some Rhythm.Perfect) 0.02;
  is (Some Rhythm.Great) (-0.05);
  is None 0.2;
  is (Some Rhythm.Perfect) 0.030;
  is (Some Rhythm.Great) 0.031;
  is (Some Rhythm.Almost) (-0.135);
  is None (-0.136)

(* A chart played through: two notes in two lanes. A press takes the
 * nearest note of its own lane only; a note left alone becomes a miss
 * once it has gone by; the average of the errors is what a steady
 * player is off by. *)
let performance () =
  let notes = [ { Rhythm.at = 1.0; lane = `A; length = 0.5 }; { Rhythm.at = 2.0; lane = `B; length = 0.5 } ] in
  let p = Rhythm.start ~offset:0.05 ~started:0. notes in
  let p = Rhythm.play 1.02 [ `B ] p in
  Alcotest.(check int) "the wrong lane takes nothing" 0 p.score;
  let p = Rhythm.play 1.02 [ `A ] p in
  Alcotest.(check int) "the right one, 20 ms late: perfect" 100 p.score;
  let p = Rhythm.play 2.5 [] p in
  Alcotest.(check bool) "the other went by: a miss, and no combo" true
    (List.exists (fun (_, j) -> j = Some Rhythm.Miss) p.judged && p.combo = 0);
  Alcotest.(check (option (float 1e-9))) "on average 20 ms late" (Some 0.02) (Rhythm.average_error p);
  Alcotest.(check (float 1e-9)) "the clock, less the calibration" 0.95 (Rhythm.song_time ~position:1.0 ~offset:0.05)

(* A chart from a tune: one voice's notes, rests left out, a chord as
 * several pitches at once. *)
let from_a_tune () =
  match Abc.parse "X:1\nL:1/4\nQ:1/4=60\nK:C\nC E z [CEG] |" with
  | Error e -> Alcotest.fail e
  | Ok tune ->
      let notes = Rhythm.sounding tune 0 in
      Alcotest.(check int) "three notes, the rest left out" 3 (List.length notes);
      let at, length, pitches = List.nth notes 2 in
      Alcotest.(check (float 1e-9)) "the chord on the fourth beat (60 a minute)" 3. at;
      Alcotest.(check (float 1e-9)) "a beat long" 1. length;
      Alcotest.(check (list int)) "C E G together" [ 60; 64; 67 ] (List.sort compare pitches)

(* A part on frets follows its tune up and down; a chord is several
 * frets at once. *)
let on_frets () =
  match Abc.parse "X:1\nL:1/4\nQ:1/4=60\nK:C\nC E G c | [CEGc]4 |" with
  | Error e -> Alcotest.fail e
  | Ok tune ->
      let chart = Rhythm.on_frets tune 0 in
      let firsts = List.filteri (fun i _ -> i < 4) chart in
      Alcotest.(check (list int)) "a rising tune, rising frets" [ 0; 1; 2; 4 ] (List.map (fun (n : int Rhythm.note) -> n.lane) firsts);
      Alcotest.(check int) "the chord: four frets together" 4
        (List.length (List.filter (fun (n : int Rhythm.note) -> n.at = 4.) chart))

(* Rhythm.mli's table: the same chord at the four levels. *)
let difficulty () =
  let chord = List.map (fun lane -> { Rhythm.at = 1.; lane; length = 0.5 }) [ 0; 2; 4 ] in
  let at level = List.map (fun (n : int Rhythm.note) -> n.lane) (Rhythm.reduce level chord) in
  Alcotest.(check (list int)) "expert: the chart itself" [ 0; 2; 4 ] (at Rhythm.Expert);
  Alcotest.(check (list int)) "hard: its outline" [ 0; 4 ] (at Rhythm.Hard);
  Alcotest.(check (list int)) "medium: its lowest note" [ 0 ] (at Rhythm.Medium);
  (* and the frets fold: fret 4 of five is fret 2 of three *)
  let top = [ { Rhythm.at = 2.; lane = 4; length = 0.5 } ] in
  Alcotest.(check (list int)) "easy: three frets" [ 2 ]
    (List.map (fun (n : int Rhythm.note) -> n.lane) (Rhythm.reduce Rhythm.Easy top))

(* A guitar note is two hands; and a long note scores while it is held. *)
let strum_and_sustain () =
  Alcotest.(check (list int)) "the fret alone plays nothing" [] (Rhythm.strummed ~strum:false ~held:[ 2 ]);
  Alcotest.(check (list int)) "fret, then strum" [ 2 ] (Rhythm.strummed ~strum:true ~held:[ 2 ]);
  let long = { Rhythm.at = 1.; lane = 2; length = 2. } in
  let p = Rhythm.play 1. [ 2 ] (Rhythm.start ~offset:0. ~started:0. [ long ]) in
  Alcotest.(check int) "hit and held, half way through" 1 (Rhythm.sustaining 2. [ 2 ] p);
  Alcotest.(check int) "let go" 0 (Rhythm.sustaining 2. [] p);
  Alcotest.(check int) "over" 0 (Rhythm.sustaining 3.5 [ 2 ] p)

(* You hear what you play: the song with the part muted keeps its other
 * voice as it was; a note hit is heard alone, the whole chord; a note
 * missed is not heard at all. *)
let hearing () =
  match Abc.parse "X:1\nL:1/4\nQ:1/4=60\nK:C\nV:1\nC [EG] c2 |\nV:2\nC,4 |" with
  | Error e -> Alcotest.fail e
  | Ok tune ->
      let band = Rhythm.muted 0 tune in
      Alcotest.(check int) "the part muted: no note left" 0 (List.length (Rhythm.sounding band 0));
      Alcotest.(check int) "the band untouched" 1 (List.length (Rhythm.sounding band 1));
      Alcotest.(check (float 1e-9)) "and the song as long" (Abc.duration tune) (Abc.duration band);
      let chord = Rhythm.struck tune 0 1. in
      Alcotest.(check (list (list int))) "a hit on the second beat: the chord, alone" [ [ 64; 67 ] ]
        (List.map (fun (_, _, ps) -> List.sort compare ps) (Rhythm.sounding chord 0));
      Alcotest.(check int) "nothing of the other voice" 0 (List.length (Rhythm.sounding chord 1));
      let p = Rhythm.start ~offset:0. ~started:0. (Rhythm.on_frets tune 0) in
      let hit = Rhythm.play 1.01 [ 2; 3; 4 ] p in
      Alcotest.(check (list (float 1e-9))) "hit: heard once, at its time" [ 1. ] (Rhythm.newly_hit p hit);
      let missed = Rhythm.play 3. [] hit in
      Alcotest.(check (list (float 1e-9))) "missed: never heard" [] (Rhythm.newly_hit hit missed)

let tests =
  Testo.categorize "rhythm"
    [ t "the windows" windows; t "a chart played through" performance; t "a chart from a tune" from_a_tune;
      t "a part on frets" on_frets; t "the same chart, reduced" difficulty; t "strum and sustain" strum_and_sustain;
      t "you hear what you play" hearing ]
