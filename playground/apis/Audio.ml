(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Audio.mli *)

type sound = Synth.t

let tone f = Synth.voice (Wave Sine) f
let square f = Synth.voice (Wave Square) f
let triangle f = Synth.voice (Wave Triangle) f
let sawtooth f = Synth.voice (Wave Sawtooth) f
let noise roughness = Synth.voice Noise roughness
let pluck f = Synth.voice Pluck f
let fm f ratio index = Synth.voice (Fm { ratio; index }) f
let note name = tone (Music.frequency name)
let lasting = Synth.lasting
let fading = Synth.fading
let louder = Synth.louder
let sliding = Synth.sliding
let together sounds = Synth.Together sounds
let after sounds = Synth.After sounds
let filtered kind ~q cutoff cutoff_to s = Synth.Filtered ({ kind; cutoff; cutoff_to; q }, s)

(* Butterworth's Q, the flattest; the wah's 5, a +14 dB ring *)
let low_pass cutoff = filtered Low_pass ~q:0.707 cutoff cutoff
let high_pass cutoff = filtered High_pass ~q:0.707 cutoff cutoff
(* the resonance rings up to 5 times as loud: the sound turned down
 * first, not to be cut by the mixer *)
let wah from to_ s = filtered Low_pass ~q:5. from to_ (Synth.louder 0.4 s)
let naive = Synth.naive

let tune (who : string) (parse : string -> (Abc.tune, string) result) (text : string) : sound =
  match parse text with
  | Ok tune -> Music.to_sound tune
  | Error e ->
      prerr_endline (who ^ ": not a tune: " ^ e);
      Synth.After []

let abc = tune "Audio.abc" Abc.parse
let doremi = tune "Audio.doremi" Doremi.parse
let of_tune = Music.to_sound

let wav (bytes : string) : sound =
  match Wav.of_string bytes with
  | Ok samples -> Synth.Samples samples
  | Error e ->
      prerr_endline ("Audio.wav: " ^ e);
      Synth.After []

let recorded (s : sound) : sound = Synth.Samples (Synth.render s)

let midi (bytes : string) : sound =
  match Midi.parse bytes with
  | Ok score -> Synth.Samples (Music.render_score score)
  | Error e ->
      prerr_endline ("Audio.midi: " ^ e);
      Synth.After []

let vibrato rate depth = Synth.with_effect (Vibrato { rate; depth })
let arpeggio semitones step = Synth.with_effect (Arpeggio { semitones; step })
let echo delay feedback s = Synth.Echo ({ delay; feedback = Float.min 0.95 (Float.max 0. feedback) }, s)
let reverb seconds s = Synth.Reverb (Float.max 0.01 seconds, s)

(* the live effects (effects/), run over the sound rendered: a fresh
 * effect for each rendering *)
let processed ?(tail = 0.) (make : unit -> Signal.stereo -> unit) (s : sound) : sound = Synth.Processed ({ make; tail }, s)

(* [gain] dB in, [gain] dB back out: a pedal's drive knob with its level
 * knob turned to match. Oversampled x2, not x4: a whole sound is
 * rendered at once, and a 20 s loop took 1.2 s natively at x4 (2.1 s in
 * a browser), a game frozen that long; x2 halves it, its aliases at -58.6
 * dB still (Drive.mli). A sound in the middle (both sides the same, the
 * common case) is driven once and copied, halving it again. *)
let drive gain =
  processed (fun () ->
      let left = Drive.create ~oversampling:2 () and right = Drive.create ~oversampling:2 () in
      let back = 1. /. Mix.of_decibels gain in
      fun st ->
        let mono = st.left = st.right in
        Drive.process left Tanh ~drive:gain ~mix:1. st.left;
        Array.iteri (fun i x -> st.left.(i) <- back *. x) st.left;
        if mono then Array.blit st.left 0 st.right 0 (Array.length st.left)
        else begin
          Drive.process right Tanh ~drive:gain ~mix:1. st.right;
          Array.iteri (fun i x -> st.right.(i) <- back *. x) st.right
        end)

let modulated (settings : Modulated_delay.settings) =
  processed ~tail:0.02 (fun () -> Modulated_delay.process (Modulated_delay.create ()) settings)

let chorus = modulated Modulated_delay.chorus
let flanger = modulated Modulated_delay.flanger
let phaser = processed (fun () -> Phaser.process (Phaser.create ()) Phaser.initial)

let compressed threshold ratio =
  processed (fun () -> Dynamics.process (Dynamics.create ()) { Dynamics.compressor with threshold; ratio = Float.max 1. ratio })

(* the ready-made sounds: audio/Sfx's presets, after sfxr's categories *)
let sfx = Sfx.to_sound
let random_sound (category : string) (seed : int) : sound = sfx (Sfx.random category ~seed)
let blip = sfx Sfx.blip
let coin = sfx Sfx.coin
let jump = sfx Sfx.jump
let laser = sfx Sfx.laser
let hit = sfx Sfx.hit
let explosion = sfx Sfx.explosion
let step = sfx Sfx.step
let powerup = sfx Sfx.powerup

let varied (name : string) (seed : int) : sound =
  match List.assoc_opt name Sfx.presets with
  | Some s -> sfx (Sfx.vary ~seed s)
  | None ->
      prerr_endline ("Audio.varied: no such sound " ^ name);
      Synth.After []

(* the mixer every sound goes to; the platform pulls its samples *)
let mixer = Mixer.create ()
let play (s : sound) : unit = Mixer.play mixer (Synth.render_stereo s)

(* a continuous sound's voices, each kept under its own name, with the
   filter it's under and its pan (after: only the first sound goes on;
   filters nested: the outermost; pans nested: the innermost) *)
let rec voices ?filter ?pan (s : sound) : (Synth.voice * Synth.filter option * float option) list =
  match s with
  | Voice v -> [ (v, filter, pan) ]
  | Together l -> List.concat_map (voices ?filter ?pan) l
  | After (s :: _) | Echo (_, s) | Reverb (_, s) | Processed (_, s) -> voices ?filter ?pan s
  | Filtered (f, s) -> voices ~filter:(Option.value filter ~default:f) ?pan s
  | Panned (p, s) -> voices ?filter ~pan:p s
  | After [] | Samples _ -> []

let keep_playing (name : string) (s : sound) : unit =
  List.iteri (fun i (v, filter, pan) -> Mixer.keep ?filter ?pan mixer (Printf.sprintf "%s#%d" name i) v) (voices s)

(* a loop's samples, rendered once (a tune of a minute: 2.6 million
 * samples, rendered each frame it's asked for would be too slow) *)
let loop (name : string) (s : sound) : unit =
  if not (List.mem name (Mixer.looping mixer)) then Mixer.loop mixer name (Synth.render_stereo s)

(* the platform's way to get a file's bytes; none until run_app *)
let fetcher : (string -> (string option -> unit) -> unit) ref = ref (fun _ k -> k None)
let set_fetcher f = fetcher := f
let fetch (source : string) (k : string option -> unit) : unit = !fetcher source k

(* the loops asked for with loop_from, playing or still downloading:
 * asked once *)
let requested : (string, unit) Hashtbl.t = Hashtbl.create 2

(* a module's song, played by its own player (not rendered ahead: a
 * song of minutes would be hundreds of megabytes of samples), pulled by
 * the mixer as an instrument *)
let play_module (name : string) (bytes : string) : unit =
  match Mod.of_string bytes with
  | Error e -> prerr_endline ("Audio.play_module: " ^ e)
  | Ok song ->
      let p = Mod_player.create song in
      Mixer.instrument mixer name
        { note_on = (fun _ _ -> ()); note_off = ignore; set = (fun _ _ -> ()); fill = Mod_player.fill p }

let loop_from (name : string) (source : string) : unit =
  if not (Hashtbl.mem requested name) then (
    Hashtbl.replace requested name ();
    !fetcher source (function
      | None ->
          prerr_endline ("Audio.loop_from: can't get " ^ source);
          Hashtbl.remove requested name
      | Some bytes ->
          let ends_with = Filename.check_suffix (String.lowercase_ascii source) in
          if ends_with ".mod" then play_module name bytes
          else
            let read =
              if ends_with ".mid" || ends_with ".midi" then midi
              else if ends_with ".abc" then abc
              else if ends_with ".wav" then wav
              else doremi
            in
            Mixer.loop mixer name (Synth.render_stereo (read bytes))))

let faster = Synth.faster
let change_loop (name : string) (s : sound) : unit = Mixer.change mixer name (Synth.render_stereo s)

let pan (p : float) (s : sound) : sound = Synth.Panned (p, s)
let pitched = Synth.pitched

(* the screen's centre the listener; half the width (500) the side *)
let from (x : float) (y : float) (s : sound) : sound =
  let d = Float.hypot x y in
  Synth.Panned (x /. 500., s) |> Synth.louder (Space.attenuation ~reference:700. d)

type instrument = Instrument.t

(* the instruments asked for, by name; the mixer's list says which still
 * play (a stopped one is made afresh when asked for again) *)
let instruments : (string, Instrument.t) Hashtbl.t = Hashtbl.create 2

let instrument (name : string) (make : unit -> Instrument.t) : instrument =
  match Hashtbl.find_opt instruments name with
  | Some i when List.mem name (Mixer.instruments mixer) -> i
  | _ ->
      let i = make () in
      Hashtbl.replace instruments name i;
      Mixer.instrument mixer name i;
      i

let note_on (i : instrument) (name : string) : unit = Option.iter (fun key -> i.note_on key 1.) (Music.midi_number name)
let note_off (i : instrument) (name : string) : unit = Option.iter i.note_off (Music.midi_number name)
let set (i : instrument) (knob : string) (value : float) : unit = i.set knob value

let stop (name : string) : unit =
  Hashtbl.remove requested name;
  Mixer.stop mixer name
let pull (n : int) : Signal.stereo = Mixer.pull mixer n

let position (name : string) : float option =
  Option.map (fun n -> float_of_int n /. float_of_int Signal.rate) (Mixer.played mixer name)

(* the platforms' reports, averaged over about a second (a one-pole, a
 * 60th of the way each frame): the queue swings by a frame from one
 * frame to the next, the average is what a player feels *)
let latest = ref None

let set_latency (seconds : float) : unit =
  latest := Some (match !latest with None -> seconds | Some l -> l +. ((seconds -. l) /. 60.))

let latency () : float = Option.value !latest ~default:0.
