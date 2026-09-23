(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/formats/mod: the .mli's cell, a module of ours written and read
 * back, Ultimate Soundtracker's 15-instrument form, the periods *)

let t = Testo.create
let hex (s : string) : string = String.concat " " (List.map (fun c -> Printf.sprintf "%02X" (Char.code c)) (List.of_seq (String.to_seq s)))

let test_cell () =
  let c : Mod.cell = { instrument = 1; period = 428; effect = 0xC; param = 0x20 } in
  Alcotest.(check string) "instrument 1, C-2, C20" "01 AC 1C 20" (hex (Mod.cell_to_bytes c));
  Alcotest.(check string) "instrument 17" "11 AC 1C 20" (hex (Mod.cell_to_bytes { c with instrument = 17 }));
  List.iter
    (fun (c : Mod.cell) -> if Mod.cell_of_bytes (Mod.cell_to_bytes c) 0 <> c then Alcotest.fail "a cell not read back")
    [ c; { instrument = 31; period = 113; effect = 0xF; param = 0xFF }; Mod.empty_cell ]

let test_periods () =
  Alcotest.(check (option int)) "C-2: 428" (Some 428) (Mod.period_of_name "C-2");
  Alcotest.(check (option int)) "C-1: 856" (Some 856) (Mod.period_of_name "C-1");
  Alcotest.(check (option int)) "B-3: 113" (Some 113) (Mod.period_of_name "B-3");
  Alcotest.(check (option int)) "not a note" None (Mod.period_of_name "H-2");
  Alcotest.(check (float 0.1)) "C-2 plays at 8,287.1 Hz" 8287.1 (Mod.rate 428);
  Array.iter (fun p -> Alcotest.(check (option int)) "a name and back" (Some p) (Mod.period_of_name (Mod.note_name p))) Mod.periods;
  Alcotest.(check string) "the nearest" "C-2" (Mod.note_name 430);
  (* each semitone the last divided by 2^(1/12), rounded: within one *)
  Array.iteri
    (fun i p -> if abs (p - int_of_float (Float.round (856. /. Float.pow 2. (float_of_int i /. 12.)))) > 1 then Alcotest.failf "period %d: %d" i p)
    Mod.periods

(* our module: two instruments (a looped square, a sine once), three
 * positions over two patterns, a few cells *)
let instrument name data ~loop : Mod.instrument =
  { name; finetune = 0; volume = 64; loop_start = 0; loop_length = (if loop then String.length data else 0); data }

let square = Mod.data_of_floats (Array.init 32 (fun i -> if i < 16 then 0.5 else -0.5))
let sine = Mod.data_of_floats (Array.init 1000 (fun i -> sin (2. *. Float.pi *. float_of_int i /. 32.)))

let song ~count : Mod.song =
  let cell i p e x : Mod.cell = { instrument = i; period = p; effect = e; param = x } in
  let pattern first = Array.init 64 (fun r -> Array.init 4 (fun c -> if r mod 16 = 0 && c = 0 then cell first 428 0 0 else if r = 8 && c = 1 then cell 2 214 0xC 0x30 else Mod.empty_cell)) in
  let blank = instrument "" "" ~loop:false in
  {
    title = "our first module";
    instruments = Array.init count (fun i -> if i = 0 then instrument "square" square ~loop:true else if i = 1 then instrument "sine" sine ~loop:false else blank);
    restart = (if count = 15 then 120 else 127);
    positions = [| 0; 1; 0 |];
    patterns = [| pattern 1; pattern 2 |];
    tag = (if count = 15 then "" else "M.K.");
  }

let test_round_trip () =
  List.iter
    (fun count ->
      let s = song ~count in
      let bytes = Mod.to_string s in
      Alcotest.(check int) (Printf.sprintf "%d instruments: the header" count) (if count = 31 then 1084 else 600) (Mod.header_size s);
      Alcotest.(check int) "the size: header, 2 patterns, the samples" (Mod.header_size s + (2 * 1024) + 32 + 1000) (String.length bytes);
      match Mod.of_string bytes with
      | Error e -> Alcotest.fail e
      | Ok s' ->
          if s' <> s then Alcotest.failf "%d instruments: not the same song read back" count;
          Alcotest.(check string) "written again: the same bytes" bytes (Mod.to_string s'))
    [ 31; 15 ];
  (* the tag at 1080, the cell at 1084 *)
  let bytes = Mod.to_string (song ~count:31) in
  Alcotest.(check string) "M.K. at 1080" "M.K." (String.sub bytes 1080 4);
  Alcotest.(check string) "the first cell: instrument 1, C-2, no effect" "01 AC 10 00" (hex (String.sub bytes 1084 4));
  Alcotest.(check int) "four channels" 4 (Mod.channels (song ~count:31))

let test_samples_and_fields () =
  Alcotest.(check (float 1e-9)) "0.5 as a byte, and back" (64. /. 128.) (Mod.sample (instrument "" (Mod.data_of_floats [| 0.5 |]) ~loop:false) 0);
  Alcotest.(check int) "an odd length padded to a word" 2 (String.length (Mod.data_of_floats [| 0.1 |]));
  Alcotest.(check (float 1e-9)) "clamped" (-128. /. 128.) (Mod.sample (instrument "" (Mod.data_of_floats [| -3. |]) ~loop:false) 0);
  (* finetune -1 is the nibble 0xF, and a loop in words doubled *)
  let s = song ~count:31 in
  let s = { s with instruments = Array.mapi (fun i (x : Mod.instrument) -> if i = 0 then { x with finetune = -1 } else x) s.instruments } in
  match Mod.of_string (Mod.to_string s) with
  | Ok s' ->
      Alcotest.(check int) "finetune -1" (-1) s'.instruments.(0).finetune;
      Alcotest.(check int) "the loop: 32 bytes" 32 s'.instruments.(0).loop_length;
      Alcotest.(check int) "no loop: 0" 0 s'.instruments.(1).loop_length
  | Error e -> Alcotest.fail e

let test_tags_and_errors () =
  Alcotest.(check (option int)) "M.K." (Some 4) (Mod.channels_of_tag "M.K.");
  Alcotest.(check (option int)) "6CHN" (Some 6) (Mod.channels_of_tag "6CHN");
  Alcotest.(check (option int)) "16CH" (Some 16) (Mod.channels_of_tag "16CH");
  Alcotest.(check (option int)) "not a tag" None (Mod.channels_of_tag "WXYZ");
  let error s = match Mod.of_string s with Ok _ -> "read" | Error e -> e in
  Alcotest.(check string) "too short" "10 bytes: too short for a module's header (600)" (error (String.make 10 'x'));
  let bytes = Mod.to_string (song ~count:31) in
  Alcotest.(check string) "cut inside the patterns" "2 patterns, and the file ends inside them" (error (String.sub bytes 0 2000))

let tests =
  Testo.categorize "MOD"
    [
      t "a cell's 4 bytes: the .mli's example" test_cell;
      t "the periods: C-2 at 8,287 Hz, names" test_periods;
      t "our module, written and read back" test_round_trip;
      t "samples, finetune, loops" test_samples_and_fields;
      t "tags, and what isn't a module" test_tags_and_errors;
    ]
