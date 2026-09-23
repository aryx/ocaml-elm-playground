(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Media and Our_media: each of our files recognized by its bytes, even
 * under a wrong name, and opened into what it holds *)

let t = Testo.create
let kind : Media.kind Alcotest.testable = Alcotest.testable (fun fmt k -> Format.pp_print_string fmt (Media.kind_name k)) ( = )
let bytes name = List.assoc name (Lazy.force Our_media.playlist)

let test_sniff () =
  List.iter
    (fun (name, expected) -> Alcotest.(check (option kind)) name (Some expected) (Media.sniff ~name (bytes name)))
    [
      ("frere_jacques.abc", Media.Abc);
      ("frere_jacques.mid", Midi);
      ("la_lune.doremi", Solfege);
      ("tiny_soundtracker.mod", Mod);
      ("bell.wav", Wav);
      ("demo_picture.png", Png);
      ("demo_picture.gif", Gif);
      ("bouncing_ball.gif", Gif);
      ("demo_picture.jpg", Jpeg);
      ("mario_stand.xpm", Xpm);
      ("ball_and_square.y4m", Y4m);
      ("ball_and_square.flc", Flic);
      ("ball_and_square.avi", Avi);
    ];
  (* the bytes decide, not the name *)
  Alcotest.(check (option kind)) "a PNG called bell.wav" (Some Png) (Media.sniff ~name:"bell.wav" (bytes "demo_picture.png"));
  Alcotest.(check (option kind)) "a MIDI file with no name" (Some Midi) (Media.sniff ~name:"" (bytes "frere_jacques.mid"));
  (* and the name only when the bytes say nothing: solfege is plain text *)
  Alcotest.(check (option kind)) "solfege by its name" (Some Solfege) (Media.sniff ~name:"x.txt" "do re mi");
  Alcotest.(check (option kind)) "nothing" None (Media.sniff ~name:"x.bin" "\000\001\002")

let test_open () =
  let open_ name = match Media.open_ ~name (bytes name) with Ok (_, m) -> m | Error e -> Alcotest.fail e in
  (* the round, 16 s of it, the same played from ABC and from its MIDI
   * file, give or take the MIDI player's own ending *)
  let seconds name = Option.value (Media.duration (open_ name)) ~default:0. in
  Alcotest.(check (float 0.1)) "the round in ABC: 16 s" 16. (seconds "frere_jacques.abc");
  Alcotest.(check (float 0.6)) "and as MIDI" 16. (seconds "frere_jacques.mid");
  Alcotest.(check (float 1e-6)) "the bell: 2 s" 2. (seconds "bell.wav");
  (match open_ "frere_jacques.abc" with
  | Sound s ->
      (* voice 1's eight bars, 32 notes; voice 2 two bars late, the first
       * six of them, 26 *)
      Alcotest.(check int) "the round's notes, for the roll: 32 + 26" 58 (List.length s.notes)
  | _ -> Alcotest.fail "not a sound");
  (match open_ "tiny_soundtracker.mod" with Module song -> Alcotest.(check int) "two positions" 2 (Array.length song.positions) | _ -> Alcotest.fail "not a module");
  (match open_ "demo_picture.png" with Picture img -> Alcotest.(check (pair int int)) "64 x 48" (64, 48) (img.width, img.height) | _ -> Alcotest.fail "not a picture");
  (match open_ "mario_stand.xpm" with Picture img -> if img.width < 8 then Alcotest.fail "the sprite too small" | _ -> Alcotest.fail "not a picture");
  (match open_ "ball_and_square.y4m" with
  | Movie { movie; _ } ->
      Alcotest.(check int) "the clip: 50 frames" 50 (Movie.frame_count movie);
      Alcotest.(check (float 1e-9)) "2 s" 2. movie.duration;
      Alcotest.(check (pair int int)) "160 x 120" (160, 120) (movie.width, movie.height)
  | _ -> Alcotest.fail "not a movie");
  (* the same clip as FLC: the same pixels, exactly (5 colors: no
   * palette to reduce), in a fraction of the bytes *)
  (match (open_ "ball_and_square.flc", open_ "ball_and_square.y4m") with
  | Movie { movie = flc; _ }, Movie _ ->
      Alcotest.(check int) "FLC: 50 frames" 50 (Movie.frame_count flc);
      Alcotest.(check (float 1e-9)) "FLC: 2 s" 2. flc.duration;
      List.iter (fun i -> if Psnr.mse (List.nth (Lazy.force Our_media.clip) i) (flc.frame i) <> 0. then Alcotest.failf "FLC frame %d" i) [ 0; 1; 25; 49 ];
      let flc = String.length (bytes "ball_and_square.flc") and y4m = String.length (bytes "ball_and_square.y4m") in
      if flc * 20 > y4m then Alcotest.failf "FLC %d bytes, Y4M %d" flc y4m
  | _ -> Alcotest.fail "not movies");
  (* and as AVI: the frames as JPEGs, close; the sound, the blips *)
  (match open_ "ball_and_square.avi" with
  | Movie { movie; sound = Some sound } ->
      Alcotest.(check int) "AVI: 50 frames" 50 (Movie.frame_count movie);
      Alcotest.(check (float 1e-9)) "AVI: 2 s" 2. movie.duration;
      Alcotest.(check int) "its sound: 2 s" (2 * Signal.rate) (Array.length sound.left);
      List.iter (fun i -> let db = Psnr.psnr (List.nth (Lazy.force Our_media.clip) i) (movie.frame i) in if db < 30. then Alcotest.failf "AVI frame %d: %.1f dB" i db) [ 0; 25; 49 ]
  | _ -> Alcotest.fail "not a movie with a sound");
  (* our GIF: six frames, 0.15 s each, the ball moving *)
  match open_ "bouncing_ball.gif" with
  | Movie { movie; _ } ->
      Alcotest.(check int) "six frames" 6 (Movie.frame_count movie);
      Alcotest.(check (float 1e-9)) "0.15 s each" 0.9 movie.duration;
      Alcotest.(check (float 1e-9)) "the third at 0.3 s" 0.3 movie.times.(2);
      if (movie.frame 0).rgba = (movie.frame 1).rgba then Alcotest.fail "the ball didn't move"
  | _ -> Alcotest.fail "not a movie"

let test_refused () =
  match Media.open_ ~name:"x.bin" "\000\001" with
  | Ok _ -> Alcotest.fail "opened"
  | Error e -> Alcotest.(check string) "why" "x.bin: not a kind of file this player knows" e

let tests =
  Testo.categorize "Media"
    [
      t "each file recognized by its bytes" test_sniff;
      t "each file opened: sounds, a module, pictures, a movie" test_open;
      t "what isn't a known kind" test_refused;
    ]
