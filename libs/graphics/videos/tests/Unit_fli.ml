(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Fli: Fli.mli's worked examples as files made by hand, clips written
 * and read back exactly, and what a delta frame costs *)

let t = Testo.create

(* a file by hand: the 128-byte header, then the frames, each a list of
 * chunks (type, body) *)
let file ~(magic : int) ~(width : int) ~(height : int) (frames : (int * string) list list) : string =
  let b = Buffer.create 256 in
  List.iter
    (fun chunks ->
      let body = Buffer.create 64 in
      List.iter
        (fun (typ, c) ->
          Buffer.add_int32_le body (Int32.of_int (6 + String.length c));
          Buffer.add_uint16_le body typ;
          Buffer.add_string body c)
        chunks;
      Buffer.add_int32_le b (Int32.of_int (16 + Buffer.length body));
      Buffer.add_uint16_le b 0xF1FA;
      Buffer.add_uint16_le b (List.length chunks);
      Buffer.add_string b (String.make 8 '\000');
      Buffer.add_buffer b body)
    frames;
  let h = Bytes.make 128 '\000' in
  Bytes.set_int32_le h 0 (Int32.of_int (128 + Buffer.length b));
  Bytes.set_uint16_le h 4 magic;
  Bytes.set_uint16_le h 6 (List.length frames);
  Bytes.set_uint16_le h 8 width;
  Bytes.set_uint16_le h 10 height;
  Bytes.set_uint16_le h 12 8;
  Bytes.set_uint16_le h 16 7 (* FLI: 7/70 s *);
  Bytes.to_string h ^ Buffer.contents b

let bytes (l : int list) : string = String.init (List.length l) (fun i -> Char.chr (List.nth l i))

(* the red of each pixel of a frame *)
let reds (img : Rgba_image.t) : int list = List.init (img.width * img.height) (fun i -> img.rgba.{4 * i})

(* a COLOR_64 palette where entry k is red k (k < 64: 6 bits) *)
let ramp = bytes ([ 1; 0; 0; 64 ] @ List.concat (List.init 64 (fun k -> [ k; 0; 0 ])))
let red6 k = (k lsl 2) lor (k lsr 4)

let test_worked () =
  (* the line of 8: all 0, then 0 0 5 5 0 0 9 0 by LC *)
  let s =
    file ~magic:0xAF11 ~width:8 ~height:1
      [ [ (11, ramp); (15, bytes [ 1; 8; 0 ]) ]; [ (12, bytes [ 0; 0; 1; 0; 2; 2; 0xFE; 5; 2; 1; 9 ]) ] ]
  in
  let h, movie = Fli.of_string s in
  Alcotest.(check (float 1e-9)) "7/70 s" 0.1 h.delay;
  Alcotest.(check (list int)) "frame 0" (List.init 8 (fun _ -> 0)) (reds (movie.frame 0));
  Alcotest.(check (list int)) "frame 1: 5 twice, 9" (List.map red6 [ 0; 0; 5; 5; 0; 0; 9; 0 ]) (reds (movie.frame 1));
  (* and BRUN's line 7 7 7 7 1 2 3 *)
  let s = file ~magic:0xAF11 ~width:7 ~height:1 [ [ (11, ramp); (15, bytes [ 2; 4; 7; 0xFD; 1; 2; 3 ]) ] ] in
  Alcotest.(check (list int)) "BRUN" (List.map red6 [ 7; 7; 7; 7; 1; 2; 3 ]) (reds ((snd (Fli.of_string s)).frame 0))

(* a clip of [n] frames, [w] x [h]: a square moving right over a
 * background of [colors] stripes, a new color each frame on its left *)
let clip ?(colors = 4) (w : int) (h : int) (n : int) : Rgba_image.t list =
  List.init n (fun k ->
      let img = Rgba_image.create ~width:w ~height:h in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let o = 4 * ((y * w) + x) in
          let v =
            if x >= 3 * k && x < (3 * k) + 5 && y >= h / 3 && y < h / 3 + 5 then 200
            else if x = 0 then 8 * k (* the new color *)
            else 20 * (x * colors / w)
          in
          img.rgba.{o} <- v;
          img.rgba.{o + 1} <- 255 - v;
          img.rgba.{o + 2} <- v / 2;
          img.rgba.{o + 3} <- 255
        done
      done;
      img)

let same_frames (label : string) (frames : Rgba_image.t list) (movie : Movie.t) : unit =
  Alcotest.(check int) (label ^ ": the frames") (List.length frames) (Movie.frame_count movie);
  List.iteri (fun i f -> if Psnr.mse f (movie.frame i) <> 0. then Alcotest.failf "%s: frame %d differs" label i) frames

let test_round_trip () =
  let frames = clip 40 20 8 in
  same_frames "FLC" frames (snd (Fli.of_string (Fli.to_string ~delay:0.04 frames)));
  (* an odd width: the last pixel of a line, DELTA_FLC's 10 opcode *)
  let odd = clip 41 9 6 in
  same_frames "FLC, 41 wide" odd (snd (Fli.of_string (Fli.to_string ~delay:0.04 odd)));
  (* wide lines, skips longer than a byte *)
  let wide = clip 700 4 3 in
  same_frames "FLC, 700 wide" wide (snd (Fli.of_string (Fli.to_string ~delay:0.04 wide)))

let test_fli () =
  (* FLI: 6 bits of each color (the VGA's), so close, not exact *)
  let frames = clip 40 20 8 in
  let h, movie = Fli.of_string (Fli.to_string ~format:Fli ~delay:0.04 frames) in
  Alcotest.(check bool) "an FLI" true (h.format = Fli);
  Alcotest.(check (float 1e-9)) "3/70 s, the nearest to 0.04" (3. /. 70.) h.delay;
  List.iteri (fun i f -> let db = Psnr.psnr f (movie.frame i) in if db < 40. then Alcotest.failf "frame %d: %.1f dB" i db) frames;
  (* and LC's long skips, on wide lines *)
  let wide = clip 700 4 3 in
  let _, movie = Fli.of_string (Fli.to_string ~format:Fli ~delay:0.04 wide) in
  List.iteri (fun i f -> let db = Psnr.psnr f (movie.frame i) in if db < 40. then Alcotest.failf "700 wide, frame %d: %.1f dB" i db) wide

let test_sizes () =
  (* 160 x 120, a small square moving over a still background: the
   * first frame run-length coded, then only the square's edges *)
  let frames = clip ~colors:3 160 120 20 in
  let flc = Fli.to_string ~delay:0.04 frames and fli = Fli.to_string ~format:Fli ~delay:0.04 frames in
  let raw = 20 * 160 * 120 in
  if String.length flc * 10 > raw then Alcotest.failf "FLC: %d bytes for %d raw" (String.length flc) raw;
  if String.length fli * 10 > raw then Alcotest.failf "FLI: %d bytes for %d raw" (String.length fli) raw;
  (* a frame that didn't change: its 16-byte header, nothing more *)
  let still = List.hd frames in
  let one = String.length (Fli.to_string ~delay:0.04 [ still ]) in
  Alcotest.(check int) "a still frame: 16 bytes" (one + 16) (String.length (Fli.to_string ~delay:0.04 [ still; still ]))

let test_refused () =
  let img = Rgba_image.create ~width:300 ~height:1 in
  for x = 0 to 299 do img.rgba.{4 * x} <- x land 0xFF; img.rgba.{(4 * x) + 1} <- x / 256 done;
  (match Fli.to_string ~delay:0.1 [ img ] with _ -> Alcotest.fail "300 colors written" | exception Invalid_argument _ -> ());
  match Fli.of_string (String.make 200 'x') with _ -> Alcotest.fail "read" | exception Failure _ -> ()

let tests =
  Testo.categorize "Fli"
    [
      t "Fli.mli's LC and BRUN lines" test_worked;
      t "FLC written and read back, exactly" test_round_trip;
      t "FLI: 6 bits a color" test_fli;
      t "a still background costs nothing" test_sizes;
      t "more than 256 colors, not a file" test_refused;
    ]
