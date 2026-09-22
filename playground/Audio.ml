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

let midi (bytes : string) : sound =
  match Midi.parse bytes with
  | Ok score -> Synth.Samples (Music.render_score score)
  | Error e ->
      prerr_endline ("Audio.midi: " ^ e);
      Synth.After []

let vibrato rate depth = Synth.with_effect (Vibrato { rate; depth })
let arpeggio semitones step = Synth.with_effect (Arpeggio { semitones; step })
let echo delay feedback s = Synth.Echo ({ delay; feedback = Float.min 0.95 (Float.max 0. feedback) }, s)

(* the ready-made sounds: audio/Sfx's presets, after sfxr's categories *)
let sfx = Sfx.to_sound
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
let play (s : sound) : unit = Mixer.play mixer (Synth.render s)

(* a continuous sound's voices, each kept under its own name, with the
   filter it's under (after: only the first sound goes on; filters
   nested: the outermost) *)
let rec voices ?filter (s : sound) : (Synth.voice * Synth.filter option) list =
  match s with
  | Voice v -> [ (v, filter) ]
  | Together l -> List.concat_map (voices ?filter) l
  | After (s :: _) | Echo (_, s) -> voices ?filter s
  | Filtered (f, s) -> voices ~filter:(Option.value filter ~default:f) s
  | After [] | Samples _ -> []

let keep_playing (name : string) (s : sound) : unit =
  List.iteri (fun i (v, filter) -> Mixer.keep ?filter mixer (Printf.sprintf "%s#%d" name i) v) (voices s)

(* a loop's samples, rendered once (a tune of a minute: 2.6 million
 * samples, rendered each frame it's asked for would be too slow) *)
let loop (name : string) (s : sound) : unit =
  if not (List.mem name (Mixer.looping mixer)) then Mixer.loop mixer name (Synth.render s)

(* the platform's way to get a file's bytes; none until run_app *)
let fetcher : (string -> (string option -> unit) -> unit) ref = ref (fun _ k -> k None)
let set_fetcher f = fetcher := f

(* the loops asked for with loop_from, playing or still downloading:
 * asked once *)
let requested : (string, unit) Hashtbl.t = Hashtbl.create 2

let loop_from (name : string) (source : string) : unit =
  if not (Hashtbl.mem requested name) then (
    Hashtbl.replace requested name ();
    !fetcher source (function
      | None ->
          prerr_endline ("Audio.loop_from: can't get " ^ source);
          Hashtbl.remove requested name
      | Some bytes ->
          let ends_with = Filename.check_suffix (String.lowercase_ascii source) in
          let read = if ends_with ".mid" || ends_with ".midi" then midi else if ends_with ".abc" then abc else doremi in
          Mixer.loop mixer name (Synth.render (read bytes))))

let faster = Synth.faster
let change_loop (name : string) (s : sound) : unit = Mixer.change mixer name (Synth.render s)

let stop (name : string) : unit =
  Hashtbl.remove requested name;
  Mixer.stop mixer name
let pull (n : int) : float array = Mixer.pull mixer n

let position (name : string) : float option =
  Option.map (fun n -> float_of_int n /. float_of_int Signal.rate) (Mixer.played mixer name)
