(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Mpeg1: the tables are codes, the half-pixel worked example, and our
 * clip (clips/make_clips.sh: ffmpeg's encoding of it) decoded as ffmpeg
 * decodes it, in display order *)

let t = Testo.create
let read_file (file : string) : string = In_channel.with_open_bin file In_channel.input_all
let clip () = read_file (Filename.concat "clips" "ball_and_square.m1v")

let test_tables () =
  let check name codes =
    ignore (Vlc.of_list codes) (* raises if not prefix-free *);
    let k = Vlc.kraft codes in
    if k > 1. then Alcotest.failf "%s: Kraft sum %g" name k
  in
  check "address_increment" Vlc.address_increment;
  check "mb_type_i" Vlc.mb_type_i;
  check "mb_type_p" Vlc.mb_type_p;
  check "mb_type_b" Vlc.mb_type_b;
  check "coded_block_pattern" Vlc.coded_block_pattern;
  check "motion_code" Vlc.motion_code;
  check "dc_size_luminance" Vlc.dc_size_luminance;
  check "dc_size_chrominance" Vlc.dc_size_chrominance;
  check "dct_first" Vlc.dct_first;
  check "dct_next" Vlc.dct_next;
  (* every block pattern but "none", once *)
  Alcotest.(check (list int)) "patterns 1-63" (List.init 63 (( + ) 1)) (List.sort compare (List.map snd Vlc.coded_block_pattern));
  (* a motion code of each difference from -16 to 16 *)
  Alcotest.(check (list int)) "motion -16..16" (List.init 33 (fun i -> i - 16)) (List.sort compare (List.map snd Vlc.motion_code));
  (* the commonest cases, the shortest codes *)
  Alcotest.(check (list string)) "the next macroblock, no vector change, (0, 1)" [ "1"; "1"; "11" ]
    [ fst (List.find (fun (_, v) -> v = 1) Vlc.address_increment); fst (List.find (fun (_, v) -> v = 0) Vlc.motion_code);
      fst (List.find (fun (_, v) -> v = Vlc.Coeff (0, 1)) Vlc.dct_next) ]

let test_half_pixel () =
  let row = [| 10; 20; 30; 40 |] in
  Alcotest.(check (list int)) "moved 3 half pixels: 25, 35" [ 25; 35 ] [ Mpeg1.predict row 0 3; Mpeg1.predict row 1 3 ];
  Alcotest.(check int) "moved 2: a whole pixel, 30" 30 (Mpeg1.predict row 1 2);
  Alcotest.(check int) "moved -1: halfway back, 15" 15 (Mpeg1.predict row 1 (-1))

let kinds (h : Mpeg1.header) : string = String.concat "" (Array.to_list (Array.map (function Mpeg1.I -> "I" | P -> "P" | B -> "B") h.kinds))

let test_clip () =
  let h, movie, info = Mpeg1.of_string (clip ()) in
  Alcotest.(check (pair int int)) "160 x 120" (160, 120) (h.width, h.height);
  Alcotest.(check (pair int int)) "25 a second" (25, 1) h.rate;
  Alcotest.(check int) "50 frames" 50 (Movie.frame_count movie);
  (* display order: I B B P B B ..., groups of 12 *)
  Alcotest.(check string) "the kinds" "IBBPBBPBBPBBIBBPBBPBBPBBIBBPBBPBBPBBIBBPBBPBBPBBIP" (kinds h);
  (* ffmpeg's decoding of frames 0 (I), 1 (B, decoded after 3) and 3 (P):
   * ours within the IDCT's rounding *)
  let _, theirs = Y4m.of_string (read_file (Filename.concat "clips" "ball_and_square.expected.y4m")) in
  List.iteri
    (fun k i ->
      let db = Psnr.psnr (theirs.frame k) (movie.frame i) in
      if db < 50. then Alcotest.failf "frame %d: %.1f dB against ffmpeg's" i db)
    [ 0; 1; 3 ];
  (* what the analyzer shows: 10 x 8 macroblocks; the I all intra; a B
   * predicted both ways somewhere, and the ball moving: a vector *)
  let i0 = info 0 and b1 = info 1 in
  Alcotest.(check int) "80 macroblocks" 80 (Array.length i0.macroblocks);
  Alcotest.(check bool) "the I: all intra" true (Array.for_all (fun (how, _, _) -> how = Mpeg1.Intra) i0.macroblocks);
  Alcotest.(check bool) "the B: from both references somewhere" true (Array.exists (fun (how, _, _) -> how = Mpeg1.Both) b1.macroblocks);
  Alcotest.(check bool) "a vector somewhere" true
    (List.exists (fun i -> Array.exists (fun (_, f, b) -> f <> (0, 0) || b <> (0, 0)) (info i).macroblocks) [ 1; 2; 3; 4; 5; 6 ])

let test_residual () =
  (* what was sent: an I frame, everything (the picture itself); a B,
   * little -- its pixels mostly the gray of "nothing to correct" *)
  let _, movie, _ = Mpeg1.of_string (clip ()) and _, sent, _ = Mpeg1.of_string ~residual:true (clip ()) in
  Alcotest.(check (float 0.)) "the I: all of it" infinity (Psnr.psnr (movie.frame 0) (sent.frame 0));
  let b = sent.frame 1 in
  let gray = ref 0 in
  for p = 0 to (b.width * b.height) - 1 do if abs (b.rgba.{4 * p} - 128) <= 2 then incr gray done;
  if !gray * 10 < b.width * b.height * 9 then Alcotest.failf "the B: only %d of %d pixels gray" !gray (b.width * b.height)

let test_refused () =
  match Mpeg1.of_string "not a video" with _ -> Alcotest.fail "read" | exception Failure _ -> ()

let tests =
  Testo.categorize "Mpeg1"
    [
      t "the tables: prefix-free codes" test_tables;
      t "half a pixel: Mpeg1.mli's worked example" test_half_pixel;
      t "our clip, as ffmpeg decodes it, in display order" test_clip;
      t "what was sent: the residual" test_residual;
      t "not a video stream" test_refused;
    ]
