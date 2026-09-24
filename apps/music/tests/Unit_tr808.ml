(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_tr808.mli *)

let t = Testo.create
let rate = float_of_int Signal.rate

(* [play ?block t seconds]: [t]'s output, in blocks of [block] *)
let play ?(block = 735) (v : Voice_tr808.t) (seconds : float) : Signal.t =
  let i = Voice_tr808.instrument v in
  let n = Signal.samples seconds in
  let out = Array.make n 0. in
  let k = ref 0 in
  while !k < n do
    let m = min block (n - !k) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !k m;
    k := !k + m
  done;
  out

(* the frequency from the zero crossings going up in [a, b) seconds *)
let crossings (s : Signal.t) (a : float) (b : float) : float =
  let first = ref (-1) and last = ref (-1) and count = ref 0 in
  for i = Signal.samples a to Signal.samples b - 1 do
    if s.(i - 1) < 0. && s.(i) >= 0. then begin
      if !first < 0 then first := i else incr count;
      last := i
    end
  done;
  float_of_int !count *. rate /. float_of_int (!last - !first)

(* the kick: its punch (the rule), its sigh (measured) *)
let test_kick () =
  let f = Voice_tr808.drum_frequency ~f0:49.5 ~sigh:0.12 ~punch:2.2 in
  Alcotest.(check (list (float 0.01))) "3 ms in; 10 ms in at full level; at a tenth (Hz)" [ 108.9; 55.44; 50.09 ]
    [ f ~age:0.003 1.; f ~age:0.01 1.; f ~age:0.01 0.1 ];
  let p = { Voice_tr808.initial with drums = Array.init 11 (fun k -> if k = 0 then { Voice_tr808.initial.drums.(0) with decay = 1.; tone = 0.2 } else Voice_tr808.initial.drums.(k)) } in
  let v = Voice_tr808.create p in
  Voice_tr808.hit v BD ~accent:true;
  let s = play v 1.2 in
  (* loud, 12% of its level above 49.5; dying, back down to it *)
  Alcotest.(check (list (float 0.1))) "the sigh: its pitch 0.05-0.25 s, then 0.8-1.1 s (Hz)" [ 52.7; 49.6 ] [ crossings s 0.05 0.25; crossings s 0.8 1.1 ]

(* the closed hat: its energy above 5 kHz, little at the squares' own
 * frequencies *)
let test_metal () =
  let v = Voice_tr808.create Voice_tr808.initial in
  Voice_tr808.hit v CH ~accent:true;
  let s = play v 0.1 in
  let n = 2048 in
  let m = Spectrum.magnitudes (Spectrum.fft (Spectrum.hann (Array.sub s 0 n))) in
  let energy lo hi = Array.fold_left ( +. ) 0. (Array.mapi (fun k x -> let f = Spectrum.bin_frequency ~n k in if f >= lo && f < hi then x *. x else 0.) m) in
  Alcotest.(check (float 0.01)) "the share above 5 kHz" 0.90 (energy 5000. 22050. /. energy 0. 22050.);
  Alcotest.(check (float 0.001)) "the share below 1 kHz (the squares' fundamentals)" 0. (energy 0. 1000. /. energy 0. 22050.)

(* the choke: an open hat stopped by a closed one *)
let test_choke () =
  let sounding ~closed =
    let v = Voice_tr808.create Voice_tr808.initial in
    Voice_tr808.hit v OH ~accent:true;
    ignore (play v 0.05);
    if closed then Voice_tr808.hit v CH ~accent:false;
    ignore (play v 0.2);
    Voice_tr808.sounding v
  in
  Alcotest.(check (pair int int)) "0.25 s in: the open hat alone, and choked" (1, 0) (sounding ~closed:false, sounding ~closed:true)

(* the sequencer: the same samples whatever the blocks *)
let test_steps () =
  let p = { (List.assoc "electro" Voice_tr808.presets) with tempo = 120. } in
  let render block =
    let v = Voice_tr808.create p in
    Voice_tr808.run v true;
    play ~block v 1.
  in
  let a = render 735 and b = render 100 in
  let d = ref 0. in
  Array.iteri (fun i x -> d := Float.max !d (Float.abs (x -. b.(i)))) a;
  Alcotest.(check (float 1e-12)) "blocks of 735 and of 100: the same samples" 0. !d;
  (* the first kick at sample 0, the second step's cowbell... the step
   * boundaries: 5512.5 samples at 120 BPM *)
  Alcotest.(check (float 1e-9)) "a step at 120 BPM (samples)" 5512.5 (Sequencer.samples_per_step 120.)

let peak (s : Signal.t) : float = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s

(* each pattern, two bars *)
let bars (p : Voice_tr808.patch) : Signal.t =
  let v = Voice_tr808.create p in
  Voice_tr808.run v true;
  play v (2. *. 16. *. 60. /. (p.tempo *. 4.))

let test_peaks () =
  Alcotest.(check (list (pair string (float 0.01)))) "the patterns' peaks"
    [ ("electro", 0.63); ("house", 0.47); ("hip hop", 0.47); ("latin", 0.79) ]
    (List.map (fun (name, p) -> (name, peak (bars p))) Voice_tr808.presets)

let test_text () =
  List.iter
    (fun (name, p) ->
      match Voice_tr808.of_string (Voice_tr808.to_string p) with
      | Ok q -> Alcotest.(check bool) (name ^ ": read back") true (q = p)
      | Error e -> Alcotest.failf "%s: %s" name e)
    Voice_tr808.presets

let tests =
  Testo.categorize "TR-808"
    (List.map
       (fun (name, p) ->
         let file = "tr808_" ^ String.map (fun c -> if c = ' ' then '_' else c) name in
         t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" file (bars p)))
       Voice_tr808.presets
    @ [
        t "the kick: its punch and its sigh" test_kick;
        t "the metal: the band-passes, no fundamentals" test_metal;
        t "the choke: the closed hat stopping the open" test_choke;
        t "the sequencer: the same samples whatever the blocks" test_steps;
        t "the patterns' peaks" test_peaks;
        t "the patches as text" test_text;
      ])
