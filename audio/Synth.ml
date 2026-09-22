(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Synth.mli *)

type source = Wave of Oscillator.waveform | Naive of Oscillator.waveform | Fm of { ratio : float; index : float } | Noise

type voice = { source : source; frequency : float; slide : float option; seconds : float; volume : float; fade : bool }

type t = Voice of voice | Together of t list | After of t list | Samples of Signal.t | Filtered of filter * t
and filter = { kind : Filter.kind; cutoff : float; cutoff_to : float; q : float }

let voice (source : source) (frequency : float) : t =
  Voice { source; frequency; slide = None; seconds = 0.3; volume = 0.5; fade = false }

let rec map_voices (f : voice -> voice) (s : t) : t =
  match s with
  | Voice v -> Voice (f v)
  | Together l -> Together (List.map (map_voices f) l)
  | After l -> After (List.map (map_voices f) l)
  | Samples s -> Samples s
  | Filtered (filter, s) -> Filtered (filter, map_voices f s)

let lasting (seconds : float) = map_voices (fun v -> { v with seconds })
let fading = map_voices (fun v -> { v with fade = true })
let louder (k : float) = map_voices (fun v -> { v with volume = v.volume *. k })
let sliding (target : float) = map_voices (fun v -> { v with slide = Some target })
let naive = map_voices (fun v -> match v.source with Wave w -> { v with source = Naive w } | _ -> v)

let rec duration (s : t) : float =
  match s with
  | Voice v -> v.seconds
  | Together l -> List.fold_left (fun m s -> Float.max m (duration s)) 0. l
  | After l -> List.fold_left (fun sum s -> sum +. duration s) 0. l
  | Samples s -> float_of_int (Array.length s) /. float_of_int Signal.rate
  | Filtered (_, s) -> duration s

(* the source's state: an oscillator's phase (and FM's modulator's), or
 * noise's register and clock *)
type running = { phase : float; modulator : float; register : int; clock : float; last_volume : float }

let start () : running = { phase = 0.; modulator = 0.; register = 1; clock = 0.; last_volume = 0. }
let rate = float_of_int Signal.rate
let wrap (phase : float) : float = phase -. Float.floor phase

(* one sample of [source] at [frequency], and the state after;
 * [brightness] scales FM's index *)
let sample ?(brightness = 1.) (source : source) (frequency : float) (r : running) : float * running =
  match source with
  | Wave w | Naive w ->
      let dt = frequency /. rate in
      let x = match source with Naive _ -> Oscillator.wave w r.phase | _ -> Oscillator.wave_band_limited w ~dt r.phase in
      (x, { r with phase = wrap (r.phase +. dt) })
  | Fm { ratio; index } ->
      let x = Fm.wave ~index:(index *. brightness) r.phase r.modulator in
      (x, { r with phase = wrap (r.phase +. (frequency /. rate)); modulator = wrap (r.modulator +. (frequency *. ratio /. rate)) })
  | Noise ->
      let out = if r.register land 1 = 1 then 1. else -1. in
      let clock = ref (r.clock +. (frequency /. rate)) and register = ref r.register in
      while !clock >= 1. do
        register := Noise.step Long !register;
        clock := !clock -. 1.
      done;
      (out, { r with register = !register; clock = !clock })

let ramp = 0.005

let render_voice (v : voice) : Signal.t =
  let n = Signal.samples v.seconds in
  let envelope =
    if v.fade then Envelope.percussive ~attack:ramp ~decay:(Float.max 0. (v.seconds -. ramp))
    else { Envelope.attack = ramp; decay = 0.; sustain = 1.; release = ramp }
  in
  let held = if v.fade then v.seconds else Float.max 0. (v.seconds -. ramp) in
  let r = ref (start ()) in
  Array.init n (fun i ->
      let t = float_of_int i /. rate in
      let f = match v.slide with None -> v.frequency | Some target -> v.frequency +. ((target -. v.frequency) *. t /. v.seconds) in
      let level = Envelope.level envelope ~held t in
      let (x, r') = sample ~brightness:(if v.fade then level else 1.) v.source f !r in
      r := r';
      x *. v.volume *. level)

let rec render (s : t) : Signal.t =
  match s with
  | Voice v -> render_voice v
  | Together l -> Mix.add (List.map render l)
  | After l -> Array.concat (List.map render l)
  | Samples s -> s
  | Filtered (f, s) ->
      let samples = render s in
      if f.cutoff = f.cutoff_to then Filter.run (Filter.biquad f.kind ~cutoff:f.cutoff ~q:f.q) samples
      else Filter.sweep f.kind ~q:f.q ~from:f.cutoff ~to_:f.cutoff_to samples

let continue (r : running) (v : voice) (n : int) : Signal.t * running =
  let r = ref r and from = r.last_volume in
  let out =
    Array.init n (fun i ->
        let (x, r') = sample v.source v.frequency !r in
        r := r';
        x *. (from +. ((v.volume -. from) *. float_of_int (i + 1) /. float_of_int n)))
  in
  (out, { !r with last_volume = v.volume })

let release (r : running) (v : voice) (n : int) : Signal.t =
  fst (continue r { v with volume = 0. } n)
