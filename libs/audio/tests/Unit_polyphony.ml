(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Unit_polyphony.mli *)

let t = Testo.create

(* an organ's envelope: at once, held, a 15 ms release -- silent (-100
 * dB, Envelope.mli) 5/3 of that after the key is let go, 25 ms: inside
 * the second block of 16.7 ms *)
let adsr : Envelope.t = { attack = 0.001; decay = 0.; sustain = 1.; release = 0.015 }
let c, e, g = (261.63, 329.63, 392.)

(* [n] blocks of 735 *)
let blocks (p : Polyphony.t) (n : int) : Signal.t =
  Array.concat
    (List.init n (fun _ ->
         let b = Array.make 735 0. in
         Polyphony.fill p b;
         b))

let test_chord () =
  let p = Polyphony.create () in
  Polyphony.press p 60 (Polyphony.sine ~adsr c 0.3);
  Polyphony.press p 64 (Polyphony.sine ~adsr e 0.3);
  Polyphony.press p 67 (Polyphony.sine ~adsr g 0.3);
  Alcotest.(check int) "three voices" 3 (Polyphony.voices p);
  Alcotest.(check (list int)) "three keys held" [ 60; 64; 67 ] (Polyphony.held p);
  (* the block is the three sines' sum: each rendered alone the same *)
  let chord = blocks p 2 in
  let alone f =
    let q = Polyphony.create () in
    Polyphony.press q 0 (Polyphony.sine ~adsr f 0.3);
    blocks q 2
  in
  let sum = Array.mapi (fun i x -> x +. (alone e).(i) +. (alone g).(i)) (alone c) in
  Alcotest.(check (float 1e-12)) "their sum" 0. (Array.fold_left Float.max 0. (Array.mapi (fun i x -> Float.abs (x -. sum.(i))) chord))

(* the blocks after a key is let go until its voice is freed *)
let test_release () =
  let p = Polyphony.create () in
  List.iter (fun (k, f) -> Polyphony.press p k (Polyphony.sine ~adsr f 0.3)) [ (60, c); (64, e); (67, g) ];
  ignore (blocks p 2);
  Polyphony.release p 64;
  Alcotest.(check (list int)) "E let go" [ 60; 67 ] (Polyphony.held p);
  Alcotest.(check int) "still three: E releasing" 3 (Polyphony.voices p);
  ignore (blocks p 1);
  Alcotest.(check int) "after a block (16.7 ms), E not yet silent" 3 (Polyphony.voices p);
  ignore (blocks p 1);
  Alcotest.(check int) "after two, freed" 2 (Polyphony.voices p);
  Polyphony.release p 60;
  Polyphony.release p 67;
  ignore (blocks p 2);
  Alcotest.(check int) "all let go: none left" 0 (Polyphony.voices p);
  Alcotest.(check (float 0.)) "and silence" 0. (Array.fold_left (fun m x -> Float.max m (Float.abs x)) 0. (blocks p 1))

(* a key pressed again while its voice releases: two voices, the old
 * one fading beside the new *)
let test_again () =
  let p = Polyphony.create () in
  Polyphony.press p 60 (Polyphony.sine ~adsr c 0.3);
  ignore (blocks p 1);
  Polyphony.release p 60;
  Polyphony.press p 60 (Polyphony.sine ~adsr c 0.3);
  Alcotest.(check int) "two voices for C" 2 (Polyphony.voices p);
  ignore (blocks p 2);
  Alcotest.(check int) "the old one freed" 1 (Polyphony.voices p);
  (* pressed twice with no release between: one voice held *)
  Polyphony.press p 60 (Polyphony.sine ~adsr c 0.3);
  Alcotest.(check (list int)) "C held once" [ 60 ] (Polyphony.held p)

(* Polyphony.mli's picture: two voices, C, E, C let go, G steals C's,
 * A steals E's (none released); and 17 notes into the DX7's 16 *)
let test_stealing () =
  let p = Polyphony.create ~voices:2 () in
  let press k = Polyphony.press p k (Polyphony.sine ~adsr c 0.3) in
  press 60;
  press 64;
  Polyphony.release p 60;
  press 67;
  Alcotest.(check (list int)) "G stole the released C" [ 64; 67 ] (Polyphony.held p);
  Alcotest.(check int) "two voices" 2 (Polyphony.voices p);
  press 69;
  Alcotest.(check (list int)) "A stole E, the oldest held" [ 67; 69 ] (Polyphony.held p);
  let q = Polyphony.create ~voices:16 () in
  for k = 40 to 56 do
    Polyphony.press q k (Polyphony.sine ~adsr c 0.3)
  done;
  Alcotest.(check int) "17 notes, 16 voices" 16 (Polyphony.voices q);
  Alcotest.(check (list int)) "the first stolen" (List.init 16 (fun i -> 41 + i)) (Polyphony.held q)

let tests =
  Testo.categorize "Polyphony"
    [
      t "a fixed number of voices: stealing" test_stealing;
      t "a chord: three voices, their sum" test_chord;
      t "a key let go: its voice releasing, then freed" test_release;
      t "a key pressed again while it releases" test_again;
    ]
