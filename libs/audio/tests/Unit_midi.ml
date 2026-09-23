(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/Midi: variable-length quantities, running status, the tempo
 * map, and ABC to MIDI and back *)

let t = Testo.create
let hex (l : int list) : string = String.init (List.length l) (fun i -> Char.chr (List.nth l i))

(* Midi.mli's table *)
let test_vlq () =
  [ (0, [ 0x00 ]); (127, [ 0x7F ]); (128, [ 0x81; 0x00 ]); (200, [ 0x81; 0x48 ]); (480, [ 0x83; 0x60 ]); (16383, [ 0xFF; 0x7F ]);
    (16384, [ 0x81; 0x80; 0x00 ]) ]
  |> List.iter (fun (n, bytes) ->
         Alcotest.(check string) (Printf.sprintf "%d written" n) (hex bytes) (Midi.vlq n);
         Alcotest.(check (pair int int)) (Printf.sprintf "%d read" n) (n, List.length bytes) (Midi.read_vlq (hex bytes) 0))

(* a file of one track: MThd (format 0, 1 track, 480 ticks a quarter),
 * then [events] and the end of the track *)
let file (events : int list) : string =
  let track = hex (events @ [ 0x00; 0xFF; 0x2F; 0x00 ]) in
  let be32 n = hex [ (n lsr 24) land 255; (n lsr 16) land 255; (n lsr 8) land 255; n land 255 ] in
  "MThd" ^ be32 6 ^ hex [ 0; 0; 0; 1; 0x01; 0xE0 ] ^ "MTrk" ^ be32 (String.length track) ^ track

let notes (s : string) : (int * float * float) list =
  match Midi.parse s with
  | Ok score -> List.map (fun (n : Midi.note) -> (n.key, n.start, n.length)) score.notes
  | Error e -> Alcotest.fail e

let test_running_status () =
  (* 90 3C 64 40 64 43 64: C E G on, in 7 bytes; 480 ticks later (a
   * quarter, 0.5 s at the default 120), off, in running status too *)
  let s = file [ 0x00; 0x90; 0x3C; 0x64; 0x00; 0x40; 0x64; 0x00; 0x43; 0x64; 0x83; 0x60; 0x80; 0x3C; 0x40; 0x00; 0x40; 0x40; 0x00; 0x43; 0x40 ] in
  Alcotest.(check (list (triple int (float 1e-9) (float 1e-9)))) "a C major chord, half a second"
    [ (60, 0., 0.5); (64, 0., 0.5); (67, 0., 0.5) ]
    (List.sort compare (notes s))

let test_tempo_map () =
  (* at 120 a minute, a note of 960 ticks: 1 s; then the tempo halves
   * (1,000,000 us a quarter, 60 a minute) and a note of 480 ticks: 1 s
   * too; a note on of velocity 0 ends it *)
  let s =
    file
      [ 0x00; 0xFF; 0x51; 0x03; 0x07; 0xA1; 0x20; 0x00; 0x90; 0x3C; 0x64; 0x87; 0x40; 0x80; 0x3C; 0x40;
        0x00; 0xFF; 0x51; 0x03; 0x0F; 0x42; 0x40; 0x00; 0x90; 0x3E; 0x64; 0x83; 0x60; 0x90; 0x3E; 0x00 ]
  in
  Alcotest.(check (list (triple int (float 1e-9) (float 1e-9)))) "each stretch at its own tempo"
    [ (60, 0., 1.); (62, 1., 1.) ] (notes s)

(* Frere Jacques, ABC to MIDI and back: the same notes at the same times *)
let test_round_trip () =
  match Abc.parse Unit_abc.frere_jacques with
  | Error e -> Alcotest.fail e
  | Ok tune ->
      let from_abc =
        List.concat_map (List.concat_map (fun (e : Abc.event) -> List.map (fun k -> (k, e.start, e.length)) e.notes)) tune.voices
      in
      Alcotest.(check (list (triple int (float 1e-6) (float 1e-6)))) "the same notes" (List.sort compare from_abc)
        (List.sort compare (notes (Midi.of_tune tune)))

let tests =
  Testo.categorize "MIDI"
    [
      t "variable-length quantities" test_vlq;
      t "running status: a chord in 7 bytes" test_running_status;
      t "the tempo map" test_tempo_map;
      t "Frere Jacques, ABC to MIDI and back" test_round_trip;
    ]
