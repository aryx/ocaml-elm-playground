(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* an Ogg Vorbis file decoded by Vorbis into a WAV (at 44,100 Hz, Wav's
 * rate, resampled if the file's is another), to listen to what we
 * decode; and the time the decoding took:
 *   Vorbis_to_wav.exe song.ogg song.wav *)
let () =
  match Sys.argv with
  | [| _; input; output |] ->
      let s = In_channel.with_open_bin input In_channel.input_all in
      let t0 = Unix.gettimeofday () in
      let t, sound = Vorbis.of_ogg s in
      let rate = Vorbis.rate t in
      Printf.printf "Vorbis, %d Hz, %d channel(s): %.1f s decoded in %.2f s\n" rate (Vorbis.channels t)
        (float_of_int (Array.length sound.(0)) /. float_of_int rate)
        (Unix.gettimeofday () -. t0);
      let resampled s = if rate = Signal.rate then s else Resample.to_rate Cubic rate s in
      let left = resampled sound.(0) in
      Wav.write_stereo output { left; right = (if Array.length sound > 1 then resampled sound.(1) else left) }
  | _ ->
      prerr_endline "usage: Vorbis_to_wav.exe file.ogg file.wav";
      exit 2
