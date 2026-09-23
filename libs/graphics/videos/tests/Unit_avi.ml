(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Avi: a clip with its sound written and read back, the file's
 * layout (the headers, the interleaving, the index), and what isn't
 * read *)

let t = Testo.create

(* frame [k]: a yellow square moving right over gray *)
let frame (k : int) : Rgba_image.t =
  let w = 48 and h = 32 in
  let img = Rgba_image.create ~width:w ~height:h in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let inside = x >= 4 * k && x < (4 * k) + 12 && y >= 10 && y < 22 in
      let r, g, b = if inside then (240, 200, 40) else (70, 80, 90) and o = 4 * ((y * w) + x) in
      img.rgba.{o} <- r;
      img.rgba.{o + 1} <- g;
      img.rgba.{o + 2} <- b;
      img.rgba.{o + 3} <- 255
    done
  done;
  img

let frames = List.init 6 frame

(* half a second of a 440 Hz tone, the clip's length at 12 a second *)
let tone : Signal.t = Array.init (Signal.rate / 2) (fun i -> 0.5 *. sin (2. *. Float.pi *. 440. *. float_of_int i /. float_of_int Signal.rate))

let test_round_trip () =
  let file = Avi.to_string ~quality:90 ~sound:tone ~rate:(12, 1) frames in
  let h, movie, sound = Avi.of_string file in
  Alcotest.(check (pair int int)) "the size" (48, 32) (h.width, h.height);
  Alcotest.(check string) "Motion JPEG" "MJPG" h.codec;
  Alcotest.(check int) "six frames" 6 (Movie.frame_count movie);
  Alcotest.(check (float 1e-9)) "half a second" 0.5 movie.duration;
  (* any frame at once, each a JPEG: close, not exact *)
  List.iter (fun k -> let db = Psnr.psnr (frame k) (movie.frame k) in if db < 30. then Alcotest.failf "frame %d: %.1f dB" k db) [ 5; 0; 3 ];
  match sound with
  | None -> Alcotest.fail "no sound"
  | Some s ->
      Alcotest.(check int) "every sample" (Array.length tone) (Array.length s);
      Array.iteri (fun i v -> if Float.abs (v -. tone.(i)) > 1e-4 then Alcotest.failf "sample %d: %g, not %g" i v tone.(i)) s

let test_layout () =
  let file = Avi.to_string ~sound:tone ~rate:(12, 1) frames in
  Alcotest.(check string) "RIFF, AVI, the headers first" "RIFF" (String.sub file 0 4);
  Alcotest.(check string) "AVI " "AVI " (String.sub file 8 4);
  Alcotest.(check string) "LIST hdrl" "LISThdrl" (String.sub file 12 4 ^ String.sub file 20 4);
  (* the data interleaved, a frame then its sound; the index last, an
   * entry each: 6 frames, 6 stretches of sound *)
  let find_from at sub =
    let rec go i = if i + String.length sub > String.length file then -1 else if String.sub file i (String.length sub) = sub then i else go (i + 1) in
    go at
  in
  let movi = find_from 0 "movi" in
  Alcotest.(check string) "a frame first" "00dc" (String.sub file (movi + 4) 4);
  let first_size = Int32.to_int (String.get_int32_le file (movi + 8)) in
  Alcotest.(check string) "then its sound" "01wb" (String.sub file (movi + 12 + first_size + (first_size land 1)) 4);
  let idx1 = find_from movi "idx1" in
  Alcotest.(check int) "the index: 12 entries of 16 bytes" (12 * 16) (Int32.to_int (String.get_int32_le file (idx1 + 4)));
  (* and the index points where the chunks are: its first entry, 4 bytes
   * into movi *)
  Alcotest.(check int) "the first chunk's offset" 4 (Int32.to_int (String.get_int32_le file (idx1 + 16)));
  (* no sound: one stream *)
  let _, _, sound = Avi.of_string (Avi.to_string ~rate:(12, 1) frames) in
  Alcotest.(check bool) "no sound" true (sound = None)

let test_refused () =
  let file = Avi.to_string ~rate:(12, 1) frames in
  (* the codec's four letters in the BITMAPINFOHEADER made another *)
  let at = let rec go i = if String.sub file i 4 = "strf" then i else go (i + 1) in go 0 in
  let other = Bytes.of_string file in
  Bytes.blit_string "XVID" 0 other (at + 8 + 16) 4;
  (match Avi.of_string (Bytes.to_string other) with _ -> Alcotest.fail "read" | exception Failure _ -> ());
  match Avi.of_string "RIFF\000\000\000\000WAVE" with _ -> Alcotest.fail "a WAV read" | exception Failure _ -> ()

let tests =
  Testo.categorize "Avi"
    [ t "a clip and its sound, written and read back" test_round_trip; t "headers, interleaving, index" test_layout; t "another codec, not an AVI" test_refused ]
