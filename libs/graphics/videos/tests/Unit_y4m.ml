(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Y4m: the sizes of Y4m.mli, a clip written and read back, headers of
 * other writers *)

let t = Testo.create

(* frame [k] of a small clip: a bright square moving right over gray *)
let clip_frame (k : int) : Rgba_image.t =
  let w = 16 and h = 12 in
  let img = Rgba_image.create ~width:w ~height:h in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let inside = x >= 2 * k && x < (2 * k) + 4 && y >= 4 && y < 8 in
      let r, g, b = if inside then (250, 200, 40) else (60, 60, 70) and o = 4 * ((y * w) + x) in
      img.rgba.{o} <- r;
      img.rgba.{o + 1} <- g;
      img.rgba.{o + 2} <- b;
      img.rgba.{o + 3} <- 255
    done
  done;
  img

let test_sizes () =
  let h : Y4m.header = { width = 320; height = 240; rate = (25, 1); chroma = C420; range = Studio } in
  Alcotest.(check int) "a 320 x 240 frame in 4:2:0" 115_200 (Y4m.frame_bytes h);
  Alcotest.(check int) "in 4:4:4, RGB's size" 230_400 (Y4m.frame_bytes { h with chroma = C444 })

let test_round_trip () =
  let frames = List.init 5 clip_frame in
  let file = Y4m.to_string ~rate:(10, 1) frames in
  let header = "YUV4MPEG2 W16 H12 F10:1 Ip A1:1 C420jpeg XCOLORRANGE=LIMITED\n" in
  Alcotest.(check string) "the header" header (String.sub file 0 (String.length header));
  Alcotest.(check int) "the whole file: the header, then FRAME and 288 bytes, 5 times" (String.length header + (5 * (6 + 288))) (String.length file);
  let h, movie = Y4m.of_string file in
  Alcotest.(check (pair int int)) "the size" (16, 12) (h.width, h.height);
  Alcotest.(check int) "five frames" 5 (Movie.frame_count movie);
  Alcotest.(check (float 1e-9)) "half a second" 0.5 movie.duration;
  (* read in any order: every frame at a known place *)
  List.iter
    (fun k ->
      let db = Psnr.psnr (clip_frame k) (movie.frame k) in
      if db < 30. then Alcotest.failf "frame %d: %.1f dB" k db)
    [ 4; 0; 2 ];
  (* and full range, 4:4:4: nearly exact *)
  let _, movie = Y4m.of_string (Y4m.to_string ~chroma:C444 ~range:Full ~rate:(10, 1) frames) in
  if Psnr.psnr (clip_frame 3) (movie.frame 3) < 45. then Alcotest.fail "4:4:4, full range"

let test_others () =
  (* NTSC's rate, no C (4:2:0), fields on the frame line, a frame cut short *)
  let y = String.make 4 '\235' and c = "\128" in
  let file = "YUV4MPEG2 W2 H2 F30000:1001 A1:1\nFRAME\n" ^ y ^ c ^ c ^ "FRAME Ixyz\n" ^ y ^ c ^ c ^ "FRAME\n" ^ y in
  let h, movie = Y4m.of_string file in
  Alcotest.(check (pair int int)) "30000:1001" (30000, 1001) h.rate;
  Alcotest.(check int) "two whole frames" 2 (Movie.frame_count movie);
  Alcotest.(check (float 1e-6)) "29.97 a second" (1001. /. 30000.) movie.times.(1);
  Alcotest.(check int) "studio white is white" 255 (movie.frame 1).rgba.{0};
  let refused s = match Y4m.of_string s with _ -> Alcotest.fail "read" | exception Failure _ -> () in
  refused "YUV4MPEG2 W2 H2 It\nFRAME\n";
  refused "YUV4MPEG2 W2 H2 C422\nFRAME\n";
  refused "RIFF"

let tests =
  Testo.categorize "Y4m"
    [ t "how big video is" test_sizes; t "a clip written and read back" test_round_trip; t "other writers' files" test_others ]
