(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_opxy.mli *)

let t = Testo.create

(* [studio ?frames p seconds]: the studio running [p], in blocks of
 * 735, [frames f st] called before each block; its left side *)
let studio ?(frames = fun _ _ -> ()) (p : Studio_opxy.patch) (seconds : float) : Signal.t * Studio_opxy.t =
  let st = Studio_opxy.create p in
  let i = Studio_opxy.instrument st in
  Studio_opxy.run st true;
  let n = Signal.samples seconds in
  let out = Array.make n 0. in
  let at = ref 0 and frame = ref 0 in
  while !at < n do
    frames !frame st;
    let m = min 735 (n - !at) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !at m;
    at := !at + m;
    incr frame
  done;
  (out, st)

let rms (x : Signal.t) (a : int) (b : int) : float =
  let s = ref 0. in
  for i = a to b - 1 do
    s := !s +. (x.(i) *. x.(i))
  done;
  sqrt (!s /. float_of_int (b - a))

(* our song's tracks, their patterns all rests, but [k]'s first given
 * [pattern]; one scene playing it (unmuted), at 120 BPM *)
let alone (k : int) (pattern : Studio_opxy.step array) : Studio_opxy.patch =
  let p = Studio_opxy.initial in
  let tracks =
    Array.mapi
      (fun j (tr : Studio_opxy.track) ->
        { tr with patterns = Array.init 4 (fun i -> if j = k && i = 1 then pattern else Array.make 16 Studio_opxy.rest) })
      p.tracks
  in
  let silent = { Studio_opxy.chosen = Array.make 8 0; mutes = Array.make 8 false } in
  { p with tracks; tempo = 120.; scenes = [| silent; { silent with chosen = Array.make 8 1 }; silent; silent |]; scene = 0 }

(* the brain's figure: C minor's C Eb G Bb read in D dorian and D major *)
let test_brain () =
  let read scale = List.map (Studio_opxy.brain ~from:1 ~key:2 ~scale) [ 60; 63; 67; 70 ] in
  Alcotest.(check (list int)) "D dorian: D F A C" [ 62; 65; 69; 72 ] (read 2);
  Alcotest.(check (list int)) "D major: D F# A C#" [ 62; 66; 69; 73 ] (read 0);
  Alcotest.(check (list int)) "C minor pentatonic: no second (D is Eb), the fifth kept" [ 60; 63; 67; 70 ] (List.map (Studio_opxy.brain ~from:1 ~key:0 ~scale:4) [ 60; 62; 67; 70 ])

(* scene 1 (a kick on step 1) asked for during step 6 of scene 0 (all
 * rests): heard from the next bar, 16 steps of 5512.5 samples *)
let test_scene () =
  let p = alone 0 (Array.init 16 (fun k -> if k = 0 then Studio_opxy.note [ 36 ] else Studio_opxy.rest)) in
  let x, _ =
    studio ~frames:(fun f st -> if f = 40 then Studio_opxy.set_patch st { (Studio_opxy.patch st) with scene = 1 }) p 2.5
  in
  let first = ref (-1) in
  Array.iteri (fun i v -> if !first < 0 && Float.abs v > 1e-9 then first := i) x;
  Alcotest.(check int) "the kick's first sample: the bar's" 88200 !first

(* a chord step on the keys: three voices *)
let test_chord () =
  let p = alone 2 (Array.init 16 (fun k -> if k = 0 then Studio_opxy.note [ 60; 64; 67 ] else Studio_opxy.rest)) in
  let _, st = studio { p with scene = 1 } 0.05 in
  Alcotest.(check int) "C E G: three voices" 3 (Studio_opxy.voices st 2)

(* the lead held on one note, its cutoff locked low on step 1, against
 * none: the lock heard *)
let test_lock () =
  let held = Array.init 16 (fun k -> if k mod 2 = 0 then Studio_opxy.note [ 67 ] else Studio_opxy.rest) in
  let locked = Array.copy held in
  locked.(0) <- { (locked.(0)) with locks = [ ("cutoff", 0.) ] };
  let loud pattern = let x, _ = studio { (alone 3 pattern) with scene = 1 } 1. in rms x 0 44100 in
  let r = loud locked /. loud held in
  Alcotest.(check bool) (Printf.sprintf "the cutoff locked to 80 Hz: %.2f of the loudness" r) true (r < 0.3)

(* the step components on the drums (unlinked), at 120 BPM: the
 * samples of the triggers, in order *)
let test_components () =
  let triggers (steps : (int * Studio_opxy.step) list) seconds =
    let pattern = Array.init 16 (fun k -> Option.value (List.assoc_opt k steps) ~default:Studio_opxy.rest) in
    let _, st = studio { (alone 0 pattern) with scene = 1 } seconds in
    List.rev (Studio_opxy.triggers st 0)
  in
  let with_ c n = { (Studio_opxy.note n) with components = [ c ] } in
  Alcotest.(check (list int)) "multiply 4: four triggers in the step" [ 0; 1379; 2757; 4135 ] (triggers [ (0, with_ (Multiply 4) [ 36 ]) ] 0.12);
  Alcotest.(check (list int)) "pulse 3: struck three ticks, the next step three ticks late" [ 0; 5513; 11025; 16538 ]
    (triggers [ (0, with_ (Pulse 3) [ 36 ]); (1, Studio_opxy.note [ 38 ]) ] 0.4);
  Alcotest.(check (list int)) "hold 3: struck once, the next step three ticks late" [ 0; 16538 ]
    (triggers [ (0, with_ (Hold 3) [ 36 ]); (1, Studio_opxy.note [ 38 ]) ] 0.4);
  Alcotest.(check (list int)) "skip 2: the first and third bars of four" [ 0; 176400 ] (triggers [ (0, with_ (Skip 2) [ 36 ]) ] 8.)

(*****************************************************************************)
(* The OP-XY's own engines *)
(*****************************************************************************)

(* an engine's note, in blocks of 735 *)
let play (e : Op1_engine.t) (params : float array) (f : float) (seconds : float) : Signal.t =
  let fill = e.start params ~frequency:f ~velocity:1. in
  let n = Signal.samples seconds in
  let out = Array.make n 0. and k = ref 0 in
  while !k < n do
    let m = min 735 (n - !k) in
    let b = Array.make m 0. in
    fill b;
    Array.blit b 0 out !k m;
    k := !k + m
  done;
  out

(* [f]'s amplitude in [x] from [a] for [n] samples, Hann-windowed *)
let amplitude (x : Signal.t) (f : float) (a : int) (n : int) : float =
  let re = ref 0. and im = ref 0. and sum = ref 0. in
  for i = a to a + n - 1 do
    let hann = 0.5 -. (0.5 *. cos (2. *. Float.pi *. float_of_int (i - a) /. float_of_int n)) in
    let w = 2. *. Float.pi *. f *. float_of_int i /. float_of_int Signal.rate in
    re := !re +. (hann *. x.(i) *. cos w);
    im := !im +. (hann *. x.(i) *. sin w);
    sum := !sum +. hann
  done;
  2. *. sqrt ((!re *. !re) +. (!im *. !im)) /. !sum

let db (x : float) : float = 20. *. log10 x

let test_engines () =
  (* simple: shape 0, a sine *)
  let x = play Opxy_engine.simple [| 0.; 0.; 0.; 0. |] 440. 0.5 in
  let worst = List.fold_left (fun a k -> Float.max a (db (amplitude x (440. *. float_of_int k) 4410 8192 /. amplitude x 440. 4410 8192))) (-300.) [ 2; 3; 4; 5 ] in
  Alcotest.(check bool) (Printf.sprintf "simple, shape 0: a sine (harmonics at %.0f dB)" worst) true (worst < -100.);
  (* hardsync: 441 Hz, a period of 100 samples; the second at 3.59 times *)
  let y = play Opxy_engine.hardsync [| 0.37; 0.; 0.; 0. |] 441. 0.5 in
  let apart = ref 0. in
  for i = 10000 to 10999 do
    apart := Float.max !apart (Float.abs (y.(i + 100) -. y.(i)))
  done;
  Alcotest.(check bool) (Printf.sprintf "hardsync: periodic at the first's period (%.4f apart)" !apart) true (!apart < 0.01);
  (* the organ: jazz (16', 5 1/3', 8') nothing at 4', full something *)
  let at4 t = let z = play Opxy_engine.organ [| t; 0.; 0.; 0. |] 220. 0.5 in db (amplitude z 440. 4410 8192 /. amplitude z 220. 4410 8192) in
  Alcotest.(check (pair bool bool)) "organ: 4' in the full registration, not in jazz" (true, true) (at4 0. < -80., at4 0.3 > -20.);
  (* the wavetable: table 0, its first wave a sine, its second five
   * harmonics; half-way, the third at half its level in the second *)
  let third pos = let w = play Opxy_engine.wavetable [| 0.; pos; 0.; 0. |] 220. 0.5 in amplitude w 660. 4410 8192 in
  Alcotest.(check (float 0.05)) "wavetable: half-way, the spectra crossfaded (dB)" (-6.02) (db (third (0.5 /. 7.) /. third (1. /. 7.)))

(* each at its middle, four notes of an arpeggio *)
let engine_phrase (e : Op1_engine.t) : Signal.t =
  Array.concat (List.map (fun f -> Array.map (fun x -> 0.5 *. x) (play e [| 0.5; 0.5; 0.5; 0.5 |] f 0.3)) [ 220.; 277.18; 329.63; 440. ])

(* our song: a bar of its intro, the second scene asked for at once and
 * heard from the second bar *)
let song () : Signal.t =
  let bar = 16. *. Sequencer.samples_per_step Studio_opxy.initial.tempo /. float_of_int Signal.rate in
  fst
    (studio
       ~frames:(fun f st -> if f = 1 then Studio_opxy.set_patch st { (Studio_opxy.patch st) with scene = 1 })
       Studio_opxy.initial (2. *. bar))

let tests =
  Testo.categorize "OP-XY"
    ([
      t "the brain: the same degrees in another scale" test_brain;
      t "a scene at the bar's end" test_scene;
      t "a chord step" test_chord;
      t "a lock on a track's cutoff" test_lock;
      t "the step components: multiply, pulse, hold, skip" test_components;
      t "the OP-XY's engines: a sine, the sync's period, the registrations, the crossfade" test_engines;
      t "golden WAV: our song" (fun () -> Testutil_wav.check ~dir:"apps/music/tests" "opxy_song" (song ()));
    ]
    @ List.map
        (fun (e : Op1_engine.t) -> t ("golden WAV: " ^ e.name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("opxy_" ^ e.name) (engine_phrase e)))
        Opxy_engine.all)
