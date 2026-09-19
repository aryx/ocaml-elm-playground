(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Golden_wav.mli *)

let t = Testo.create

let check (name : string) (samples : Signal.t) () =
  let file = name ^ ".wav" in
  if not (Sys.file_exists "actual") then Sys.mkdir "actual" 0o755;
  Wav.write (Filename.concat "actual" file) samples;
  let hint = Printf.sprintf "the new sound is _build/default/audio/tests/actual/%s ('make approve-golden-audio' after listening)" file in
  match Wav.read (Filename.concat "golden" file) with
  | exception Sys_error _ -> Alcotest.failf "no golden WAV golden/%s yet; %s" file hint
  | Error e -> Alcotest.failf "golden/%s: %s" file e
  | Ok golden ->
      let differ = ref 0 and first = ref (-1) in
      let n = max (Array.length golden) (Array.length samples) in
      for i = 0 to n - 1 do
        let a = if i < Array.length golden then Signal.to_int16 golden.(i) else max_int in
        let b = if i < Array.length samples then Signal.to_int16 samples.(i) else min_int in
        if a <> b then (
          if !first < 0 then first := i;
          incr differ)
      done;
      if !differ > 0 then Alcotest.failf "%s: %d samples differ, the first at %d; %s" file !differ !first hint

(* a quarter second of each, at A4 (440 Hz), and the NES's noises *)
let sounds : (string * (unit -> Signal.t)) list =
  List.map (fun w -> (Oscillator.name w ^ "_440", fun () -> Oscillator.render w ~frequency:440. 0.25)) Oscillator.waveforms
  @ [ ("noise_long", fun () -> Noise.render ~rate:22050. 0.25); ("noise_short", fun () -> Noise.render ~mode:Short ~rate:22050. 0.25) ]

let tests = Testo.categorize "golden WAVs" (List.map (fun (name, f) -> t name (fun () -> check name (f ()) ())) sounds)
