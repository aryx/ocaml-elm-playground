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
  (* the same, band-limited (PolyBLEP: Oscillator.mli): the samples on
   * each jump pulled towards the middle *)
  @ List.map
      (fun w -> (Oscillator.name w ^ "_440_band_limited", fun () -> Oscillator.render ~band_limited:true w ~frequency:440. 0.25))
      [ Oscillator.Square; Sawtooth ]
  (* phase 6's other sounds: an FM bell (Fm.mli), noise low-passed (a
   * rumble) and a sawtooth through a sweeping resonant low-pass (the
   * wah: Filter.mli; the sawtooth at a quarter, the resonance ringing
   * up to 5 times as loud) *)
  @ [ ("fm_bell", fun () -> Synth.render (Synth.voice (Fm { ratio = 1.4; index = 5. }) 440. |> Synth.lasting 1. |> Synth.fading));
      ("noise_low_pass", fun () -> Filter.low_pass ~cutoff:300. (Noise.render ~rate:22050. 0.25));
      ("sawtooth_wah", fun () -> Filter.sweep Low_pass ~q:5. ~from:200. ~to_:4000. (Mix.gain 0.25 (Oscillator.render ~band_limited:true Sawtooth ~frequency:110. 1.))) ]
  (* phase 7: sfxr's categories (Sfx.mli), each preset; a C major chord
   * as an arpeggio (Effect.mli: one voice, 0 4 7 semitones every 1/60
   * s); a blip echoed (0.15 s, each echo 0.4 of the last) *)
  @ List.map (fun (name, s) -> ("sfx_" ^ name, fun () -> Synth.render (Sfx.to_sound s))) Sfx.presets
  @ [ ( "arpeggio_c_major",
        fun () ->
          Synth.render
            (Synth.voice (Wave Square) 261.63 |> Synth.with_effect (Arpeggio { semitones = [ 0.; 4.; 7. ]; step = 1. /. 60. }) |> Synth.lasting 0.5) );
      ("sfx_blip_echo", fun () -> Synth.render (Sfx.to_sound { Sfx.blip with echo = 0.15 })) ]
  (* a plucked string, A3 for 1.5 s (Pluck.mli: Karplus-Strong), and a G
   * major chord strummed, three strings 30 ms apart, turned down not to
   * clip *)
  @ [ ("pluck_a3", fun () -> Pluck.render ~frequency:220. 1.5);
      ( "pluck_strum",
        fun () ->
          Synth.render
            (Synth.Together
               (List.mapi
                  (fun i f -> Synth.After [ Synth.voice (Wave Sine) 0. |> Synth.louder 0. |> Synth.lasting (0.03 *. float_of_int i); Synth.voice Pluck f |> Synth.lasting 1.5 ])
                  [ 196.; 246.94; 293.66 ])
            |> Synth.louder 0.6) ) ]
  (* a blip in a cave (Schroeder's reverb, 1.5 s: Effect.mli); a
   * recording (an FM note at C4) read an octave faster, linearly
   * (Resample.mli): C5, half as long *)
  @ [ ("sfx_blip_reverb", fun () -> Synth.render (Sfx.to_sound { Sfx.blip with reverb = 1.5 }));
      ( "resampled_octave_up",
        fun () ->
          let note = Synth.render (Synth.voice (Fm { ratio = 1.; index = 3. }) 261.63 |> Synth.lasting 1. |> Synth.fading) in
          Resample.faster Linear 2. note ) ]
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
  (* the same round through MIDI (Midi.of_tune, Midi.parse), played as a
   * MIDI score (Music.render_score: program 80, a square lead) *)
  @ [ ( "frere_jacques_midi",
        fun () ->
          match Abc.parse Unit_abc.frere_jacques with
          | Ok tune -> (
              match Midi.parse (Midi.of_tune tune) with
              | Ok score -> Array.sub (Music.render_score score) (Signal.samples 3.5) (Signal.samples 3.)
              | Error e -> failwith e)
          | Error e -> failwith e ) ]

  (* a synthesizer's sources (Vco.mli), 2 s each at a quarter volume:
   * a sawtooth synced to 110 Hz, swept from 1 to 4 times its master's
   * frequency (the pitch staying, the vowel moving); a 110 Hz pulse
   * whose width an LFO moves at 0.5 Hz, from 0.1 to 0.9 *)
  @ [ ( "vco_sync_sweep",
        fun () ->
          let len = Signal.samples 2. in
          let master = Vco.create () and slave = Vco.create () and m = Array.make len 0. and s = Array.make len 0. in
          Vco.fill master Sawtooth ~frequency:(Array.make len 110.) m;
          let sweep = Array.init len (fun i -> 110. *. (1. +. (3. *. float_of_int i /. float_of_int len))) in
          Vco.fill ~sync:master slave Sawtooth ~frequency:sweep s;
          Mix.gain 0.25 s );
      ( "vco_pwm",
        fun () ->
          let len = Signal.samples 2. in
          let w = Array.make len 0. and s = Array.make len 0. in
          Lfo.fill (Lfo.create ()) Sine ~rate:0.5 w;
          Vco.fill ~width:(Array.map (fun x -> 0.5 +. (0.4 *. x)) w) (Vco.create ()) Pulse ~frequency:(Array.make len 110.) s;
          Mix.gain 0.25 s ) ]

(* a mono voice from the blocks (Voicing, Vco, Envelope), played by
 * [keys] (frame, key, down?) over [frames] frames of 735 samples:
 * the way a synthesizer's voice is put together *)
let mono_voice ~(curve : Envelope.curve) ~(adsr : Envelope.t) ~(glide : float) ~(frames : int) (keys : (int * int * bool) list) : Signal.t =
  let v = Voicing.create () and g = Voicing.glide () and o = Vco.create () and env = Envelope.start () in
  let f = Array.make 735 0. and wave = Array.make 735 0. and level = Array.make 735 0. in
  Array.concat
    (List.init frames (fun frame ->
         List.iter
           (fun (at, key, down) ->
             if at = frame then
               match if down then Voicing.press v key else Voicing.release v key with
               | Begin n ->
                   Voicing.glide_to g n;
                   Envelope.gate_on env
               | Change n -> Voicing.glide_to g n
               | End -> Envelope.gate_off env
               | Nothing -> ())
           keys;
         Voicing.fill_frequency g ~seconds:glide f;
         Vco.fill o Sawtooth ~frequency:f wave;
         Envelope.fill curve adsr env level;
         Array.init 735 (fun i -> 0.25 *. wave.(i) *. level.(i))))

let sounds =
  sounds
  (* the same note, 0.4 s held, 0.5 s released, straight then
   * exponential: the exponential's punchier attack and natural fade *)
  @ [ ( "envelope_linear_vs_exponential",
        fun () ->
          let adsr : Envelope.t = { attack = 0.05; decay = 0.2; sustain = 0.4; release = 0.5 } in
          let note curve = mono_voice ~curve ~adsr ~glide:0. ~frames:60 [ (0, 57, true); (24, 57, false) ] in
          Array.append (note Linear) (note Exponential) );
      (* a phrase, legato with a 50 ms glide: C4, E4 pressed over it and
       * let go (back to the held C4, gliding), then G4 alone, gliding
       * from C4 though the keys were up *)
      ( "mono_legato_glide",
        fun () ->
          mono_voice ~curve:Exponential
            ~adsr:{ attack = 0.01; decay = 0.3; sustain = 0.6; release = 0.2 }
            ~glide:0.05 ~frames:120
            [ (0, 60, true); (15, 64, true); (30, 64, false); (45, 60, false); (60, 67, true); (90, 67, false) ] ) ]

let sounds =
  sounds
  (* the ladder (Moog_ladder.mli), nonlinear: a 110 Hz sawtooth at half
   * volume, its cutoff swept from 100 Hz to 5 kHz over 1.5 s, at
   * resonances 0, 2.5 and 3.8 (the peak singing through the harmonics,
   * the bass thinning) *)
  @ [ ( "ladder_sweep",
        fun () ->
          let len = Signal.samples 1.5 in
          let cutoff = Array.init len (fun i -> 100. *. Float.pow 50. (float_of_int i /. float_of_int len)) in
          Array.concat
            (List.map
               (fun k ->
                 let saw = Array.make len 0. in
                 Vco.fill (Vco.create ()) Sawtooth ~frequency:(Array.make len 110.) saw;
                 let x = Array.map (fun v -> 0.5 *. v) saw in
                 Moog_ladder.process (Moog_ladder.create ()) Nonlinear ~cutoff ~resonance:k x;
                 Mix.gain 0.5 x)
               [ 0.; 2.5; 3.8 ]) );
      (* no input but a click, k = 4.3: the filter sings its cutoff, C4,
       * E4, G4, C5, 0.4 s each, the cutoff as the keyboard *)
      ( "ladder_self_oscillation",
        fun () ->
          let len = Signal.samples 1.6 in
          let cutoff = Array.init len (fun i -> Voicing.frequency (float_of_int (List.nth [ 60; 64; 67; 72 ] (i * 4 / len)))) in
          let x = Array.make len 0. in
          x.(0) <- 1.;
          Moog_ladder.process (Moog_ladder.create ()) Nonlinear ~cutoff ~resonance:4.3 x;
          Mix.gain 2. x ) ]

let tests = Testo.categorize "golden WAVs" (List.map (fun (name, f) -> t name (fun () -> check name (f ()) ())) sounds)
