(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* where an .mpg's decoding spends its time: demultiplexing, the whole
 * sound, the first [n] video frames (200 by default); NOAUDIO=1 skips
 * the sound, for callgrind (notes_opti_ocaml.md measured with it):
 *   Mpg_bench.exe movie.mpg 60 *)
let () =
  let s = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let n = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 200 in
  let time name f =
    let t0 = Unix.gettimeofday () in
    let r = f () in
    Printf.printf "%s: %.2f s\n%!" name (Unix.gettimeofday () -. t0);
    r
  in
  let streams = time "demux" (fun () -> Mpeg_system.of_string s) in
  let video = Option.get (Mpeg_system.video streams) and audio = Option.get (Mpeg_system.audio streams) in
  if Sys.getenv_opt "NOAUDIO" = None then ignore (time "audio (whole)" (fun () -> Mpeg_audio.decode audio.bytes));
  let h, movie, _ = time "video: open" (fun () -> Mpeg1.of_string video.bytes) in
  Printf.printf "%dx%d, %d/%d fps, %d frames\n%!" h.width h.height (fst h.rate) (snd h.rate) (Movie.frame_count movie);
  time (Printf.sprintf "video: %d frames" n) (fun () ->
      for i = 0 to min n (Movie.frame_count movie) - 1 do
        ignore (movie.frame i)
      done)
