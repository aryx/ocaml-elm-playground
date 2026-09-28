(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_rebirth.mli *)

let t = Testo.create

(* two bars of a song, in blocks of [block]; [check] after each *)
let play ?(block = 735) ?(check = fun _ -> ()) (r : Studio_rebirth.t) (p : Studio_rebirth.patch) : Signal.t =
  let i = Studio_rebirth.instrument r in
  let n = Signal.samples (2. *. 16. *. 60. /. (p.tempo *. 4.)) in
  let out = Array.make n 0. in
  let k = ref 0 in
  while !k < n do
    let m = min block (n - !k) in
    let b = { Signal.left = Array.make m 0.; right = Array.make m 0. } in
    i.fill b;
    Array.blit b.left 0 out !k m;
    check r;
    k := !k + m
  done;
  out

(* the one clock: the four machines' steps the same after every block,
 * whatever the blocks' size *)
let test_one_clock () =
  List.iter
    (fun block ->
      let p = snd (List.hd Studio_rebirth.songs) in
      let r = Studio_rebirth.create p in
      Studio_rebirth.run r true;
      let differ = ref 0 and blocks = ref 0 in
      ignore
        (play ~block
           ~check:(fun r ->
             incr blocks;
             let s = Studio_rebirth.steps r in
             if Array.exists (fun x -> x <> s.(0)) s then incr differ)
           r p);
      Alcotest.(check int) (Printf.sprintf "blocks of %d: the steps apart (of %d blocks)" block !blocks) 0 !differ)
    [ 735; 100; 1 ]

(* all muted: silence, the clocks going on *)
let test_mutes () =
  let p = { (snd (List.hd Studio_rebirth.songs)) with mutes = Array.make 4 true; delay = 0. } in
  let r = Studio_rebirth.create p in
  Studio_rebirth.run r true;
  let s = play r p in
  Alcotest.(check (float 0.)) "all muted: silence" 0. (Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s);
  Alcotest.(check bool) "the clocks going on" true (Studio_rebirth.running r)

let peak (s : Signal.t) : float = Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. s

let song (p : Studio_rebirth.patch) : Signal.t =
  let r = Studio_rebirth.create p in
  Studio_rebirth.run r true;
  play r p

let test_peaks () =
  Alcotest.(check (list (pair string (float 0.01)))) "the songs' peaks"
    [ ("acid", 0.69); ("techno", 0.54); ("house", 0.80) ]
    (List.map (fun (name, p) -> (name, peak (song p))) Studio_rebirth.songs)

let tests =
  Testo.categorize "ReBirth"
    (List.map (fun (name, p) -> t ("golden WAV: " ^ name) (fun () -> Testutil_wav.check ~dir:"apps/music/tests" ("rebirth_" ^ name) (song p))) Studio_rebirth.songs
    @ [ t "one clock: the four machines' steps together" test_one_clock; t "the mutes" test_mutes; t "the songs' peaks" test_peaks ])
