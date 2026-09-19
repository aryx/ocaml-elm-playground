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
  (* the tests run in parallel: another one may have just made it *)
  (try Sys.mkdir "actual" 0o755 with Sys_error _ -> ());
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

(* three 0.1 s beeps at 880 Hz, each shaped by [f], 0.1 s apart *)
let beeps (f : Signal.t -> Signal.t) : Signal.t =
  let beep = f (Oscillator.render Sine ~frequency:880. 0.1) and gap = Array.make (Signal.samples 0.1) 0. in
  Array.concat [ beep; gap; beep; gap; beep ]

let chord () : Signal.t =
  Mix.add [ Oscillator.render Sine ~frequency:440. 0.25; Oscillator.render Sine ~frequency:659.26 0.25 ]

(* a quarter second of each, at A4 (440 Hz), and the NES's noises *)
let sounds : (string * (unit -> Signal.t)) list =
  List.map (fun w -> (Oscillator.name w ^ "_440", fun () -> Oscillator.render w ~frequency:440. 0.25)) Oscillator.waveforms
  @ [ ("noise_long", fun () -> Noise.render ~rate:22050. 0.25); ("noise_short", fun () -> Noise.render ~mode:Short ~rate:22050. 0.25) ]
  (* the click: three short beeps cut at once, then the same three
   * enveloped (Envelope.mli) *)
  @ [ ("beeps_cut", fun () -> beeps (fun s -> s));
      ("beeps_enveloped", fun () -> beeps (Envelope.apply (Envelope.percussive ~attack:0.005 ~decay:0.095) ~held:0.1)) ]
  (* A4 and E5 at full volume, clipped hard, then soft (Mix.mli) *)
  @ [ ("chord_hard", fun () -> Mix.limit (chord ())); ("chord_soft", fun () -> Mix.limit ~soft:true (chord ())) ]
  (* Frere Jacques as a round, played by the NES-like band
   * (Music.to_sound): 3 s from 3.5 s, the second voice coming in at 4 s *)
  @ [ ( "frere_jacques",
        fun () ->
          match Abc.parse Unit_abc.frere_jacques with
          | Ok tune -> Array.sub (Synth.render (Music.to_sound tune)) (Signal.samples 3.5) (Signal.samples 3.)
          | Error e -> failwith e ) ]

let tests = Testo.categorize "golden WAVs" (List.map (fun (name, f) -> t name (fun () -> check name (f ()) ())) sounds)
