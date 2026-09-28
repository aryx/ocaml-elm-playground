(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* Mpeg_system: our clip and its blips muxed by ffmpeg
 * (clips/make_clips.sh), demultiplexed: each stream the bytes ffmpeg
 * gives back itself, each decoded, and when each starts *)

let t = Testo.create

let mpg () : string = In_channel.with_open_bin (Filename.concat "clips" "ball_and_square.mpg") In_channel.input_all

(* the streams as ffmpeg copies them out ("-c:v copy -f mpeg1video",
 * "-c:a copy -f mp2"): their lengths and CRC-32s *)
let test_streams () =
  let streams = Mpeg_system.of_string (mpg ()) in
  Alcotest.(check (list int)) "video, then audio" [ 0xE0; 0xC0 ] (List.map (fun (st : Mpeg_system.stream) -> st.id) streams);
  let check name expected_length expected_crc (st : Mpeg_system.stream option) =
    match st with
    | None -> Alcotest.failf "no %s stream" name
    | Some st ->
        Alcotest.(check int) (name ^ ": length") expected_length (String.length st.bytes);
        Alcotest.(check int32) (name ^ ": CRC-32") expected_crc (Crc32.string st.bytes)
  in
  check "video" 37284 0xdfa24770l (Mpeg_system.video streams);
  check "audio" 32182 0x7fcf54d5l (Mpeg_system.audio streams)

(* each stream decoded by its own decoder: the clip's 50 frames, 2 s
 * of MP2; and their clocks: the audio 11 ms before the video, as
 * ffprobe says (start_time 0.529089 and 0.540000) *)
let test_decoded () =
  let streams = Mpeg_system.of_string (mpg ()) in
  let video = Option.get (Mpeg_system.video streams) and audio = Option.get (Mpeg_system.audio streams) in
  let header, movie, _ = Mpeg1.of_string video.bytes in
  Alcotest.(check (pair int int)) "160 x 120" (160, 120) (header.width, header.height);
  Alcotest.(check int) "50 frames" 50 (Movie.frame_count movie);
  (match Mpeg_audio_header.frames audio.bytes with
  | (_, h) :: _ as frames ->
      Alcotest.(check int) "Layer II" 2 h.layer;
      Alcotest.(check int) "2 s of frames (1152 samples each)" 77 (List.length frames)
  | [] -> Alcotest.fail "no MP2 frame");
  Alcotest.(check (option (float 1e-6))) "the video's start" (Some 0.54) video.first_pts;
  Alcotest.(check (option (float 1e-5))) "the audio's start" (Some 0.529089) audio.first_pts

let test_refused () =
  let refused name s =
    match Mpeg_system.of_string s with _ -> Alcotest.failf "%s: read" name | exception Failure _ -> ()
  in
  refused "an MPEG-1 video stream" "\000\000\001\xB3 not a pack";
  (* an MPEG-2 pack header: its first bits 01 *)
  refused "an MPEG-2 program stream" "\000\000\001\xBA\x44\000\004\000\004\001\001\x89\xC3\xF8"

let tests =
  Testo.categorize "Mpeg_system"
    [
      t "the streams, as ffmpeg copies them out" test_streams;
      t "decoded, and their clocks" test_decoded;
      t "what isn't an MPEG-1 system stream" test_refused;
    ]
