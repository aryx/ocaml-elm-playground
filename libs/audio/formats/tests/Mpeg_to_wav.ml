(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* an MP2 or MP3 file decoded by Mpeg_audio into a WAV (at 44,100 Hz,
 * Wav's rate, resampled if the file's is another), to listen to what
 * we decode; and the time the decoding took:
 *   Mpeg_to_wav.exe song.mp3 song.wav *)
let () =
  match Sys.argv with
  | [| _; input; output |] -> (
      let s = In_channel.with_open_bin input In_channel.input_all in
      let t0 = Unix.gettimeofday () in
      match Mpeg_audio.decode s with
      | Error e ->
          prerr_endline e;
          exit 1
      | Ok (h, sound) ->
          let seconds = float_of_int (Array.length sound.left) /. float_of_int h.sample_rate in
          Printf.printf "%s Layer %d, %d Hz, %d channel(s), %d kbit/s: %.1f s decoded in %.2f s\n"
            (Mpeg_audio_header.version_name h.version) h.layer h.sample_rate h.channels (h.bitrate / 1000) seconds
            (Unix.gettimeofday () -. t0);
          let resampled s = if h.sample_rate = Signal.rate then s else Resample.to_rate Cubic h.sample_rate s in
          Wav.write_stereo output { left = resampled sound.left; right = resampled sound.right })
  | _ ->
      prerr_endline "usage: Mpeg_to_wav.exe file.mp3 file.wav";
      exit 2
