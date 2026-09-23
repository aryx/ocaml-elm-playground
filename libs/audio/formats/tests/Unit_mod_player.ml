(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* audio/formats/mod's Paula and Mod_player: the .mlis' pitches and row
 * lengths, each effect on a module of a row or two, the stereo, the
 * song's end; and a little tune of ours as golden WAVs *)

let t = Testo.create
let rate = float_of_int Signal.rate

(* {1 Modules for the tests} *)

let instrument name (a : float array) ~loop : Mod.instrument =
  let data = Mod.data_of_floats a in
  { name; finetune = 0; volume = 64; loop_start = 0; loop_length = (if loop then String.length data else 0); data }

(* one period each, looped: a note's pitch is 8,287.1 / 32 at C-2 *)
let square = instrument "square" (Array.init 32 (fun i -> if i < 16 then 0.5 else -0.5)) ~loop:true
let sine = instrument "sine" (Array.init 32 (fun i -> 0.5 *. sin (2. *. Float.pi *. float_of_int i /. 32.))) ~loop:true
let silent = instrument "" [||] ~loop:false
let cell ?(i = 1) ?(e = 0) ?(x = 0) period : Mod.cell = { instrument = i; period; effect = e; param = x }
let note name = Option.get (Mod.period_of_name name)

(* a song of patterns given as (row, channel, cell) lists *)
let song ?(instruments = [| square; sine |]) ?(positions = [| 0 |]) (patterns : (int * int * Mod.cell) list list) : Mod.song =
  let pattern cells = Array.init 64 (fun r -> Array.init 4 (fun c -> match List.find_opt (fun (r', c', _) -> r = r' && c = c') cells with Some (_, _, x) -> x | None -> Mod.empty_cell)) in
  {
    title = "";
    instruments = Array.init 31 (fun k -> if k < Array.length instruments then instruments.(k) else silent);
    restart = 127;
    positions;
    patterns = Array.of_list (List.map pattern patterns);
    tag = "M.K.";
  }

(* [n] samples of [p], the left and the right *)
let play (p : Mod_player.t) (n : int) : Signal.stereo =
  let out : Signal.stereo = { left = Array.make n 0.; right = Array.make n 0. } in
  Mod_player.fill p out;
  out

(* the pitch from the rising zero crossings between [a] and [b] *)
let pitch (x : Signal.t) a b =
  let ups = ref [] in
  for i = a + 1 to b - 1 do
    if x.(i - 1) < 0. && x.(i) >= 0. then ups := i :: !ups
  done;
  match (!ups, List.rev !ups) with
  | last :: _, first :: _ when last > first -> float_of_int (List.length !ups - 1) *. rate /. float_of_int (last - first)
  | _ -> 0.

let peak (x : Signal.t) a b =
  let p = ref 0. in
  for i = a to b - 1 do
    p := Float.max !p (Float.abs x.(i))
  done;
  !p

let row = 5292 (* 6 ticks of 882 *)

(* {1 The tests} *)

let test_pitch () =
  Alcotest.(check (float 1e-9)) "a tick at 125: 882 samples" 882. (Mod_player.tick_samples 125);
  Alcotest.(check (float 0.01)) "at 160: 689.06" 689.06 (Mod_player.tick_samples 160);
  let p = Mod_player.create (song [ [ (0, 0, cell (note "C-2")); (1, 0, cell (note "C-3")) ] ]) in
  let x = (play p (2 * row)).left in
  Alcotest.(check (float 1.)) "C-2: 8,287.1 / 32 = 259.0 Hz" 259.0 (pitch x 500 row);
  Alcotest.(check (float 2.)) "C-3, the next row: 517.9 Hz" 517.9 (pitch x (row + 500) (2 * row))

let test_row_length () =
  (* the note changes at 5,292; with F03 on the first row, at 2,646 *)
  List.iter
    (fun (speed, at) ->
      let first = if speed = 6 then cell (note "C-2") else cell ~e:0xF ~x:speed (note "C-2") in
      let p = Mod_player.create (song [ [ (0, 0, first); (1, 0, cell (note "C-3")) ] ]) in
      let x = (play p (3 * row)).left in
      Alcotest.(check (float 2.)) (Printf.sprintf "speed %d: still C-2 just before %d" speed at) 259. (pitch x (at - 1500) (at - 10));
      Alcotest.(check (float 3.)) (Printf.sprintf "C-3 from %d" at) 517.9 (pitch x (at + 100) (at + 1500)))
    [ (6, row); (3, row / 2) ]

let test_reading () =
  (* the sine sample at C-2: each byte held 5.3 of our samples by Paula,
   * interpolated by Linear *)
  let longest reading =
    let p = Mod_player.create (song [ [ (0, 0, cell ~i:2 (note "C-2")) ] ]) in
    Mod_player.set_reading p reading;
    let x = (play p row).left in
    let run = ref 1 and best = ref 1 in
    for i = 1 to row - 1 do
      if x.(i) = x.(i - 1) then incr run else run := 1;
      best := max !best !run
    done;
    !best
  in
  Alcotest.(check int) "Hold: a byte held 6 samples at most" 6 (longest Hold);
  Alcotest.(check int) "Linear: no two equal in a row" 1 (longest Linear)

let test_volume () =
  let level v =
    let p = Mod_player.create (song [ [ (0, 0, cell ~e:0xC ~x:v (note "C-2")) ] ]) in
    peak (play p row).left 1000 row
  in
  (* the square at 0.5 as bytes (64 / 128), the volume, the left's half *)
  Alcotest.(check (float 1e-9)) "C40: full" (0.5 *. 0.5) (level 0x40);
  Alcotest.(check (float 1e-9)) "C20: half" (0.5 *. 0.5 *. 0.5) (level 0x20)

let test_arpeggio () =
  (* 047 on C-2: C, E, G a tick each, from the first tick *)
  let p = Mod_player.create (song [ [ (0, 0, cell ~e:0 ~x:0x47 (note "C-2")) ] ]) in
  let x = (play p row).left in
  List.iteri
    (fun k expected ->
      Alcotest.(check (float (expected *. 0.03))) (Printf.sprintf "tick %d" k) expected (pitch x ((k * 882) + 50) (((k + 1) * 882) - 10)))
    [ 259.0; 259.0 *. Float.pow 2. (4. /. 12.); 259.0 *. Float.pow 2. (7. /. 12.) ]

let test_slides () =
  (* 110: the period 16 lower each tick after the first: 428 - 5 x 16 *)
  let p = Mod_player.create (song [ [ (0, 0, cell ~e:1 ~x:0x10 (note "C-2")) ] ]) in
  ignore (play p row);
  Alcotest.(check int) "slide up: 348 after a row" 348 (Mod_player.channel_period p 0);
  (* 3xx: from C-2 towards C-3, 32 a tick, then the next row goes on
   * with 300 and stops at C-3 *)
  let p =
    Mod_player.create (song [ [ (0, 0, cell (note "C-2")); (1, 0, cell ~e:3 ~x:0x20 (note "C-3")); (2, 0, cell ~i:0 ~e:3 0) ] ])
  in
  ignore (play p (2 * row));
  Alcotest.(check int) "to the note: 428 - 5 x 32" 268 (Mod_player.channel_period p 0);
  ignore (play p row);
  Alcotest.(check int) "then there, not past" 214 (Mod_player.channel_period p 0)

let test_song_shape () =
  (* D00 at row 0: the next position's row 0, after one row *)
  let p = Mod_player.create (song ~positions:[| 0; 1 |] [ [ (0, 0, cell ~e:0xD (note "C-2")) ]; [] ]) in
  ignore (play p (row + 1));
  Alcotest.(check (pair int int)) "a break" (1, 0) (Mod_player.position p);
  (* D16: row 16 (decimal, as trackers typed it) *)
  let p = Mod_player.create (song ~positions:[| 0; 1 |] [ [ (0, 0, cell ~e:0xD ~x:0x16 (note "C-2")) ]; [] ]) in
  ignore (play p (row + 1));
  Alcotest.(check (pair int int)) "a break to row 16" (1, 16) (Mod_player.position p);
  (* B02: position 2 *)
  let p = Mod_player.create (song ~positions:[| 0; 1; 0 |] [ [ (0, 0, cell ~e:0xB ~x:2 (note "C-2")) ]; [] ]) in
  ignore (play p (row + 1));
  Alcotest.(check (pair int int)) "a jump" (2, 0) (Mod_player.position p);
  (* the end: silent and finished, or round again *)
  let quiet = song [ [ (0, 0, cell (note "C-2")) ] ] in
  let p = Mod_player.create ~loop:false quiet in
  let x = (play p ((64 * row) + 1000)).left in
  Alcotest.(check bool) "finished" true (Mod_player.finished p);
  Alcotest.(check (float 0.)) "silent after" 0. (peak x ((64 * row) + 10) ((64 * row) + 1000));
  let p = Mod_player.create quiet in
  ignore (play p ((64 * row) + 1000));
  Alcotest.(check (pair int int)) "looping: round again" (0, 0) (Mod_player.position p)

let test_volume_effects () =
  (* A01: 64 - 5 after a row; EC2: cut at the third tick *)
  let p = Mod_player.create (song [ [ (0, 0, cell ~e:0xA ~x:0x01 (note "C-2")) ] ]) in
  ignore (play p row);
  Alcotest.(check int) "a volume slide down" 59 (Mod_player.channel_volume p 0);
  let p = Mod_player.create (song [ [ (0, 0, cell ~e:0xE ~x:0xC2 (note "C-2")) ] ]) in
  let x = (play p row).left in
  if peak x 100 1700 < 0.1 then Alcotest.fail "the note didn't sound before its cut";
  Alcotest.(check (float 0.)) "cut from the third tick" 0. (peak x ((2 * 882) + 2) row)

let test_stereo () =
  let p = Mod_player.create (song [ [ (0, 0, cell (note "C-2")) ] ]) in
  let s = play p row in
  Alcotest.(check (float 0.)) "channel 1 hard left: nothing right" 0. (peak s.right 0 row);
  let p = Mod_player.create (song [ [ (0, 1, cell (note "C-2")) ] ]) in
  let s = play p row in
  Alcotest.(check (float 0.)) "channel 2 hard right" 0. (peak s.left 0 row);
  let p = Mod_player.create (song [ [ (0, 0, cell (note "C-2")) ] ]) in
  Mod_player.set_separation p 0.;
  let s = play p row in
  Alcotest.(check (float 1e-9)) "separation 0: the middle" (peak s.left 0 row) (peak s.right 0 row)

let test_instrument_kept () =
  (* instrument 2 given on the first row, a note alone on the second:
   * instrument 2 again (instrument 1 is silent: were it taken, nothing) *)
  let p = Mod_player.create (song ~instruments:[| silent; square |] [ [ (0, 0, cell ~i:2 (note "C-2")); (1, 0, cell ~i:0 (note "C-3")) ] ]) in
  let x = (play p (2 * row)).left in
  Alcotest.(check (float 3.)) "the note alone plays the channel's instrument" 517.9 (pitch x (row + 500) (2 * row))

(* {1 A tune of ours} *)

(* four instruments made here: a lead (a pulse a quarter wide), a bass (a
 * triangle), a kick (a falling sine, dying), a snare (noise, dying);
 * two bars over 32 rows *)
let demo : Mod.song =
  let pulse = instrument "lead" (Array.init 32 (fun i -> if i < 8 then 0.6 else -0.2)) ~loop:true in
  let triangle = instrument "bass" (Array.init 64 (fun i -> let u = float_of_int i /. 64. in 0.8 *. (if u < 0.5 then (4. *. u) -. 1. else 3. -. (4. *. u)))) ~loop:true in
  let kick =
    instrument "kick"
      (Array.init 2400 (fun i ->
           let t = float_of_int i /. 8287. in
           0.9 *. exp (-.t *. 18.) *. sin (2. *. Float.pi *. (60. *. t +. (140. *. (1. -. exp (-.t *. 30.)) /. 30.)))))
      ~loop:false
  in
  let r = ref 7 in
  let snare =
    instrument "snare"
      (Array.init 2000 (fun i ->
           r := (!r * 1103515245 + 12345) land 0x7FFFFFFF;
           0.7 *. exp (-.float_of_int i /. 350.) *. ((float_of_int (!r lsr 8 land 0xFF) /. 128.) -. 1.)))
      ~loop:false
  in
  let melody = [ (0, "C-3"); (4, "E-3"); (8, "G-3"); (12, "E-3"); (16, "A-2"); (20, "C-3"); (24, "E-3"); (28, "D-3") ] in
  let cells =
    List.map (fun (r, n) -> (r, 0, cell ~i:1 ~e:(if r = 28 then 4 else 0) ~x:(if r = 28 then 0x46 else 0) (note n))) melody
    @ [ (0, 1, cell ~i:2 (note "C-1")); (8, 1, cell ~i:2 (note "G-1")); (16, 1, cell ~i:2 (note "A-1")); (24, 1, cell ~i:2 ~e:3 ~x:8 (note "G-1")) ]
    @ List.map (fun r -> (r, 2, cell ~i:3 (note "C-2"))) [ 0; 8; 16; 24 ]
    @ List.map (fun r -> (r, 3, cell ~i:4 ~e:0xC ~x:0x30 (note "C-2"))) [ 4; 12; 20; 28 ]
    @ [ (31, 0, cell ~i:0 ~e:0xD 0) ]
  in
  song ~instruments:[| pulse; triangle; kick; snare |] [ cells ]

let golden reading name () =
  let p = Mod_player.create demo in
  Mod_player.set_reading p reading;
  Testutil_wav.check_stereo ~dir:"libs/audio/formats/tests" name (play p (Signal.samples 4.))

let tests =
  Testo.categorize "MOD player"
    [
      t "the pitch: C-2 and C-3 of a 32-byte square" test_pitch;
      t "a row's length, and the speed" test_row_length;
      t "Paula's hold, and the linear reading" test_reading;
      t "the volume" test_volume;
      t "the arpeggio: three notes, a tick each" test_arpeggio;
      t "the slides, and the slide to a note" test_slides;
      t "breaks, jumps, the end" test_song_shape;
      t "volume slides, the note cut" test_volume_effects;
      t "the Amiga's stereo, and the middle" test_stereo;
      t "a note alone keeps the channel's instrument" test_instrument_kept;
      t "golden WAV: our tune, as Paula read it" (golden Hold "mod_demo_hold");
      t "golden WAV: our tune, cubic" (golden Cubic "mod_demo_cubic");
    ]
