(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Motion and Mpeg1_encode: Motion.mli's worked example, both searches
 * counted; a clip encoded and decoded back, the quantizer's price *)

let t = Testo.create

(* noise, the same seed: [w] x [h] bytes *)
let noise (w : int) (h : int) : Bytes.t =
  let seed = ref 7 in
  Bytes.init (w * h) (fun _ ->
      seed := ((!seed * 1103515245) + 12345) land 0x7FFFFFFF;
      Char.chr ((!seed lsr 16) land 0xFF))

(* [reference] moved 3 right and 2 down: what is at (x, y) was at
 * (x - 3, y - 2); the search for the macroblock at (24, 24) *)
let moved (reference : Bytes.t) (search : Motion.search) : (int * int) * int * int =
  let w = 64 in
  let cur = Bytes.init (w * w) (fun i -> let x = i mod w and y = i / w in Bytes.get reference ((max 0 (y - 2) * w) + max 0 (x - 3))) in
  let plane b = { Motion.bytes = b; stride = w; rows = w } in
  Motion.estimate search ~range:7 (plane cur) (plane reference) ~x:24 ~y:24

let test_motion () =
  (* smooth: slow waves, a single valley *)
  let smooth = Bytes.init (64 * 64) (fun i -> let x = float_of_int (i mod 64) and y = float_of_int (i / 64) in Char.chr (128 + int_of_float (60. *. sin (x /. 5.) *. cos (y /. 7.)))) in
  List.iter
    (fun (search, name, tried_expected) ->
      let v, sad, tried = moved smooth search in
      Alcotest.(check (pair int int)) (name ^ ": where it came from") (-6, -4) v;
      Alcotest.(check int) (name ^ ": a perfect match") 0 sad;
      Alcotest.(check int) (name ^ ": the candidates") tried_expected tried)
    [ (Motion.Full, "full", 225 + 8); (Motion.Logarithmic, "logarithmic", 25 + 8) ];
  (* noise: only the full search finds it *)
  let v, sad, _ = moved (noise 64 64) Motion.Full in
  Alcotest.(check (pair int int)) "noise, full: found" (-6, -4) v;
  Alcotest.(check int) "noise, full: exact" 0 sad;
  let v, sad, _ = moved (noise 64 64) Motion.Logarithmic in
  if v = (-6, -4) || sad = 0 then Alcotest.fail "noise, logarithmic: found after all (the test's lesson gone)"

(* frame [k] of a clip: noise drifting right, 2 pixels a frame, and a
 * flat stripe that doesn't move *)
let frame (k : int) : Rgba_image.t =
  let w = 48 and h = 32 in
  let n = noise 96 32 in
  let img = Rgba_image.create ~width:w ~height:h in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let v = if y < 8 then 90 else (Char.code (Bytes.get n ((y * 96) + x + 40 - (2 * k))) / 2) + 64 in
      let o = 4 * ((y * w) + x) in
      img.rgba.{o} <- v;
      img.rgba.{o + 1} <- v;
      img.rgba.{o + 2} <- v;
      img.rgba.{o + 3} <- 255
    done
  done;
  img

let test_round_trip () =
  let frames = List.init 8 frame in
  let s, stats = Mpeg1_encode.encode ~gop:4 ~rate:(25, 1) frames in
  let h, movie, info = Mpeg1.of_string s in
  Alcotest.(check string) "I P P P, twice" "IPPPIPPP" (String.concat "" (Array.to_list (Array.map (function Mpeg1.I -> "I" | P -> "P" | B -> "B") h.kinds)));
  List.iteri (fun i f -> let db = Psnr.psnr f (movie.frame i) in if db < 30. then Alcotest.failf "frame %d: %.1f dB" i db) frames;
  (* the drift found: the noise's macroblocks moved 4 half pixels *)
  Alcotest.(check bool) "a vector of the drift" true (Array.exists (fun (how, v, _) -> how = Mpeg1.Forward && v = (-4, 0)) (info 1).macroblocks);
  Alcotest.(check int) "3 x 2 macroblocks, 8 frames" 48 stats.macroblocks;
  Alcotest.(check bool) "the search ran" true (stats.candidates > 0)

let test_quantizer () =
  let frames = List.init 4 frame in
  let at q = let s, _ = Mpeg1_encode.encode ~quantizer:q ~rate:(25, 1) frames in let _, m, _ = Mpeg1.of_string s in (String.length s, Psnr.psnr (List.nth frames 3) (m.frame 3)) in
  let (s2, d2), (s12, d12) = (at 2, at 12) in
  if not (s2 > s12 && d2 > d12) then Alcotest.failf "quantizer 2: %d bytes %.1f dB, 12: %d bytes %.1f dB" s2 d2 s12 d12

let tests =
  Testo.categorize "Mpeg1_encode"
    [ t "Motion.mli's worked example: smooth, both searches; noise, only the full one" test_motion; t "a clip encoded and decoded back" test_round_trip; t "the quantizer: bytes against dB" test_quantizer ]
