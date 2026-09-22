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

type source = Wave of Oscillator.waveform | Naive of Oscillator.waveform | Fm of { ratio : float; index : float } | Noise | Pluck

type voice = {
  source : source;
  frequency : float;
  slide : float option;
  seconds : float;
  volume : float;
  fade : bool;
  effects : Effect.pitch list;
  envelope : Envelope.t option;
}

type t =
  | Voice of voice
  | Together of t list
  | After of t list
  | Samples of Signal.t
  | Filtered of filter * t
  | Echo of echo * t
  | Panned of float * t

and echo = { delay : float; feedback : float }
and filter = { kind : Filter.kind; cutoff : float; cutoff_to : float; q : float }

let voice (source : source) (frequency : float) : t =
  Voice { source; frequency; slide = None; seconds = 0.3; volume = 0.5; fade = false; effects = []; envelope = None }

let rec map_voices (f : voice -> voice) (s : t) : t =
  match s with
  | Voice v -> Voice (f v)
  | Together l -> Together (List.map (map_voices f) l)
  | After l -> After (List.map (map_voices f) l)
  | Samples s -> Samples s
  | Filtered (filter, s) -> Filtered (filter, map_voices f s)
  | Echo (echo, s) -> Echo (echo, map_voices f s)
  | Panned (p, s) -> Panned (p, map_voices f s)

let lasting (seconds : float) = map_voices (fun v -> { v with seconds })
let fading = map_voices (fun v -> { v with fade = true })
let louder (k : float) = map_voices (fun v -> { v with volume = v.volume *. k })
let sliding (target : float) = map_voices (fun v -> { v with slide = Some target })
let with_effect (e : Effect.pitch) = map_voices (fun v -> { v with effects = v.effects @ [ e ] })

let rec faster (k : float) (s : t) : t =
  let effect (e : Effect.pitch) : Effect.pitch =
    match e with
    | Vibrato { rate; depth } -> Vibrato { rate = rate *. k; depth }
    | Jump { semitones; at } -> Jump { semitones; at = at /. k }
    | Arpeggio { semitones; step } -> Arpeggio { semitones; step = step /. k }
  in
  match s with
  | Voice v -> Voice { v with seconds = v.seconds /. k; effects = List.map effect v.effects }
  | Together l -> Together (List.map (faster k) l)
  | After l -> After (List.map (faster k) l)
  | Samples s -> Samples s
  | Filtered (f, s) -> Filtered (f, faster k s)
  | Echo (e, s) -> Echo ({ e with delay = e.delay /. k }, faster k s)
  | Panned (p, s) -> Panned (p, faster k s)

let pitched (k : float) =
  map_voices (fun v -> { v with frequency = v.frequency *. k; slide = Option.map (fun f -> f *. k) v.slide })

let naive = map_voices (fun v -> match v.source with Wave w -> { v with source = Naive w } | _ -> v)

let rec duration (s : t) : float =
  match s with
  | Voice v -> v.seconds
  | Together l -> List.fold_left (fun m s -> Float.max m (duration s)) 0. l
  | After l -> List.fold_left (fun sum s -> sum +. duration s) 0. l
  | Samples s -> float_of_int (Array.length s) /. float_of_int Signal.rate
  | Filtered (_, s) -> duration s
  | Echo (e, s) -> duration s +. Effect.tail ~delay:e.delay ~feedback:e.feedback
  | Panned (_, s) -> duration s

(* the source's state: an oscillator's phase (and FM's modulator's), or
 * noise's register and clock *)
type running = { phase : float; modulator : float; register : int; clock : float; last_volume : float; time : float }

let start () : running = { phase = 0.; modulator = 0.; register = 1; clock = 0.; last_volume = 0.; time = 0. }
let rate = float_of_int Signal.rate
let band_limited = ref true
let wrap (phase : float) : float = phase -. Float.floor phase

(* one sample of [source] at [frequency], and the state after;
 * [brightness] scales FM's index *)
let rec sample ?(brightness = 1.) (source : source) (frequency : float) (r : running) : float * running =
  match source with
  | Pluck -> sample (Wave Triangle) frequency r
  | Wave w | Naive w ->
      let dt = frequency /. rate in
      let x =
        match source with
        | Wave _ when !band_limited -> Oscillator.wave_band_limited w ~dt r.phase
        | _ -> Oscillator.wave w r.phase
      in
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

(* the pitch effects' factors at [t], multiplied *)
let effects_factor (v : voice) (t : float) : float = List.fold_left (fun k e -> k *. Effect.factor e t) 1. v.effects

let render_voice (v : voice) : Signal.t =
  let n = Signal.samples v.seconds in
  let (envelope, held) =
    match v.envelope with
    | Some e -> (e, Float.max 0. (v.seconds -. e.release))
    | None when v.fade -> (Envelope.percussive ~attack:ramp ~decay:(Float.max 0. (v.seconds -. ramp)), v.seconds)
    | None -> ({ Envelope.attack = ramp; decay = 0.; sustain = 1.; release = ramp }, Float.max 0. (v.seconds -. ramp))
  in
  (* FM's brightness following the level when the voice dies away *)
  let dies = v.fade || Option.is_some v.envelope in
  let r = ref (start ()) in
  let string = match v.source with Pluck -> Pluck.render ~frequency:v.frequency v.seconds | _ -> [||] in
  Array.init n (fun i ->
      let t = float_of_int i /. rate in
      let f = match v.slide with None -> v.frequency | Some target -> v.frequency +. ((target -. v.frequency) *. t /. v.seconds) in
      let level = Envelope.level envelope ~held t in
      let x =
        match v.source with
        | Pluck -> string.(i)
        | _ ->
            let (x, r') = sample ~brightness:(if dies then level else 1.) v.source (f *. effects_factor v t) !r in
            r := r';
            x
      in
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
  | Echo (e, s) -> Effect.echo ~delay:e.delay ~feedback:e.feedback (render s)
  | Panned (_, s) -> render s

let rec panned (s : t) : bool =
  match s with
  | Voice _ | Samples _ -> false
  | Together l | After l -> List.exists panned l
  | Filtered (_, s) | Echo (_, s) -> panned s
  | Panned _ -> true

let rec render_stereo (s : t) : Signal.stereo =
  if not (panned s) then Signal.both (render s)
  else
    let each f (st : Signal.stereo) : Signal.stereo = { left = f st.left; right = f st.right } in
    match s with
    | Together l ->
        let l = List.map render_stereo l in
        { left = Mix.add (List.map (fun (st : Signal.stereo) -> st.left) l); right = Mix.add (List.map (fun (st : Signal.stereo) -> st.right) l) }
    | After l ->
        let l = List.map render_stereo l in
        { left = Array.concat (List.map (fun (st : Signal.stereo) -> st.left) l); right = Array.concat (List.map (fun (st : Signal.stereo) -> st.right) l) }
    | Filtered (f, s) -> each (fun x -> render (Filtered (f, Samples x))) (render_stereo s)
    | Echo (e, s) -> each (Effect.echo ~delay:e.delay ~feedback:e.feedback) (render_stereo s)
    | Panned (p, s) ->
        let (l, r) = Space.pan p in
        let st = render_stereo s in
        { left = Mix.gain l st.left; right = Mix.gain r st.right }
    | Voice _ | Samples _ -> Signal.both (render s)

let continue (r : running) (v : voice) (n : int) : Signal.t * running =
  let r = ref r and from = r.last_volume in
  let out =
    Array.init n (fun i ->
        let (x, r') = sample v.source (v.frequency *. effects_factor v !r.time) !r in
        r := { r' with time = r'.time +. (1. /. rate) };
        x *. (from +. ((v.volume -. from) *. float_of_int (i + 1) /. float_of_int n)))
  in
  (out, { !r with last_volume = v.volume })

let release (r : running) (v : voice) (n : int) : Signal.t =
  fst (continue r { v with volume = 0. } n)
