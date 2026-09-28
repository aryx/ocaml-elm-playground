(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Voice_minimoog.mli *)

(* ------------------------------------------------------------------ *)
(* The panel *)
(* ------------------------------------------------------------------ *)

type wave = Triangle | Shark_tooth | Reverse_sawtooth | Sawtooth | Square | Wide | Narrow

let waves = [ Triangle; Shark_tooth; Sawtooth; Square; Wide; Narrow ]
let waves3 = [ Triangle; Reverse_sawtooth; Sawtooth; Square; Wide; Narrow ]

let wave_name = function
  | Triangle -> "triangle"
  | Shark_tooth -> "shark tooth"
  | Reverse_sawtooth -> "reverse sawtooth"
  | Sawtooth -> "sawtooth"
  | Square -> "square"
  | Wide -> "wide rectangle"
  | Narrow -> "narrow rectangle"

let ranges = [ "LO"; "32'"; "16'"; "8'"; "4'"; "2'" ]

type oscillator = { range : int; wave : int; frequency : float; on : bool; level : float }
type contour = { attack : float; decay : float; sustain : float }

type patch = {
  tune : float;
  glide : float;
  glide_on : bool;
  decay_on : bool;
  modulation_mix : float;
  oscillator_modulation : bool;
  filter_modulation : bool;
  osc1 : oscillator;
  osc2 : oscillator;
  osc3 : oscillator;
  osc3_keyboard : bool;
  noise : float;
  noise_on : bool;
  cutoff : float;
  emphasis : float;
  contour_amount : float;
  keyboard_1 : bool;
  keyboard_2 : bool;
  filter_contour : contour;
  loudness_contour : contour;
  volume : float;
  effects : (string * float) list;
}

let off : oscillator = { range = 3; wave = 2; frequency = 0.; on = false; level = 0.6 }

let initial : patch =
  {
    tune = 0.;
    glide = 0.3;
    glide_on = false;
    decay_on = true;
    modulation_mix = 0.;
    oscillator_modulation = false;
    filter_modulation = false;
    osc1 = { off with on = true };
    osc2 = off;
    osc3 = off;
    osc3_keyboard = true;
    noise = 0.;
    noise_on = false;
    cutoff = 0.5;
    emphasis = 0.2;
    contour_amount = 0.3;
    keyboard_1 = false;
    keyboard_2 = false;
    filter_contour = { attack = 0.; decay = 0.4; sustain = 0.3 };
    loudness_contour = { attack = 0.; decay = 0.4; sustain = 0.8 };
    volume = 0.7;
    effects = List.map (fun (k : Effect.knob) -> (k.name, k.initial)) Rack.standard_knobs;
  }

type control = Control.t = Knob of float * float | Switch | Selector of string list
type knob = patch Patch_text.knob

let knob = Patch_text.knob
let detune = Patch_text.detune
let switch = Patch_text.switch
let selector = Patch_text.selector

(* an oscillator's five controls, under its name *)
let oscillator_knobs (name : string) (labels : string list) (get : patch -> oscillator) (put : patch -> oscillator -> patch) : knob list =
  let o = name ^ "." in
  [
    selector (o ^ "range") ranges (fun p -> (get p).range) (fun p x -> put p { (get p) with range = x });
    selector (o ^ "wave") labels (fun p -> (get p).wave) (fun p x -> put p { (get p) with wave = x });
  ]
  @ (if name = "osc1" then [] else [ detune (o ^ "frequency") (fun p -> (get p).frequency) (fun p x -> put p { (get p) with frequency = x }) ])
  @ [
      switch (o ^ "on") (fun p -> (get p).on) (fun p x -> put p { (get p) with on = x });
      knob (o ^ "level") (fun p -> (get p).level) (fun p x -> put p { (get p) with level = x });
    ]

let contour_knobs (name : string) (get : patch -> contour) (put : patch -> contour -> patch) : knob list =
  let c = name ^ "." in
  [
    knob (c ^ "attack") (fun p -> (get p).attack) (fun p x -> put p { (get p) with attack = x });
    knob (c ^ "decay") (fun p -> (get p).decay) (fun p x -> put p { (get p) with decay = x });
    knob (c ^ "sustain") (fun p -> (get p).sustain) (fun p x -> put p { (get p) with sustain = x });
  ]

(* the rack after the output (the Model D has none; Arturia's Mini V
 * adds one the same way): its controls, "delay.time", ..., each a
 * number stored under its name *)
let effect_knobs : knob list =
  List.map
    (fun (k : Effect.knob) : knob ->
      {
        name = k.name;
        control = k.control;
        get = (fun p -> List.assoc k.name p.effects);
        put = (fun p x -> { p with effects = List.map (fun (n, v) -> if n = k.name then (n, x) else (n, v)) p.effects });
      })
    Rack.standard_knobs

let knobs : knob list =
  let labels l = List.map wave_name l in
  [
    detune "tune" (fun p -> p.tune) (fun p x -> { p with tune = x });
    knob "glide" (fun p -> p.glide) (fun p x -> { p with glide = x });
    switch "glide.on" (fun p -> p.glide_on) (fun p x -> { p with glide_on = x });
    switch "decay.on" (fun p -> p.decay_on) (fun p x -> { p with decay_on = x });
    knob "mod.mix" (fun p -> p.modulation_mix) (fun p x -> { p with modulation_mix = x });
    switch "mod.oscillators" (fun p -> p.oscillator_modulation) (fun p x -> { p with oscillator_modulation = x });
    switch "mod.filter" (fun p -> p.filter_modulation) (fun p x -> { p with filter_modulation = x });
  ]
  @ oscillator_knobs "osc1" (labels waves) (fun p -> p.osc1) (fun p o -> { p with osc1 = o })
  @ oscillator_knobs "osc2" (labels waves) (fun p -> p.osc2) (fun p o -> { p with osc2 = o })
  @ oscillator_knobs "osc3" (labels waves3) (fun p -> p.osc3) (fun p o -> { p with osc3 = o })
  @ [
      switch "osc3.keyboard" (fun p -> p.osc3_keyboard) (fun p x -> { p with osc3_keyboard = x });
      switch "noise.on" (fun p -> p.noise_on) (fun p x -> { p with noise_on = x });
      knob "noise.level" (fun p -> p.noise) (fun p x -> { p with noise = x });
      knob "filter.cutoff" (fun p -> p.cutoff) (fun p x -> { p with cutoff = x });
      knob "filter.emphasis" (fun p -> p.emphasis) (fun p x -> { p with emphasis = x });
      knob "filter.contour" (fun p -> p.contour_amount) (fun p x -> { p with contour_amount = x });
      switch "filter.keyboard1" (fun p -> p.keyboard_1) (fun p x -> { p with keyboard_1 = x });
      switch "filter.keyboard2" (fun p -> p.keyboard_2) (fun p x -> { p with keyboard_2 = x });
    ]
  @ contour_knobs "filter" (fun p -> p.filter_contour) (fun p c -> { p with filter_contour = c })
  @ contour_knobs "loudness" (fun p -> p.loudness_contour) (fun p c -> { p with loudness_contour = c })
  @ [ knob "volume" (fun p -> p.volume) (fun p x -> { p with volume = x }) ]
  @ effect_knobs

let to_string (p : patch) : string = Patch_text.to_string knobs p
let of_string (text : string) : (patch, string) result = Patch_text.of_string knobs ~initial text

(* our settings, in the patch charts' text *)
let preset_texts =
  [
    ( "bass",
      {|# two oscillators an octave down, the filter snapping shut
osc1.range = 32'
osc1.wave = sawtooth
osc1.level = 0.9
osc2.range = 32'
osc2.wave = square
osc2.on = on
osc2.level = 0.6
osc2.frequency = 0.004
filter.cutoff = 0.32
filter.emphasis = 0.45
filter.contour = 0.55
filter.keyboard1 = on
filter.attack = 0
filter.decay = 0.35
filter.sustain = 0.1
loudness.decay = 0.45
loudness.sustain = 0.8|}
    );
    ( "lead",
      {|# two saws a little apart and a square above, gliding, and oscillator 3
# off the keyboard in LO: the mod wheel's vibrato
osc1.wave = sawtooth
osc2.wave = sawtooth
osc2.on = on
osc2.frequency = 0.006
osc2.level = 0.7
osc3.range = LO
osc3.wave = triangle
osc3.keyboard = off
osc3.frequency = 0.2
mod.oscillators = on
glide.on = on
glide = 0.35
filter.cutoff = 0.5
filter.emphasis = 0.55
filter.contour = 0.4
filter.keyboard1 = on
filter.keyboard2 = on
filter.decay = 0.5
filter.sustain = 0.5|}
    );
    ( "brass",
      {|# the filter opening slower than the sound: the swell of a horn
osc1.wave = sawtooth
osc2.wave = sawtooth
osc2.on = on
osc2.frequency = -0.005
osc2.level = 0.6
filter.cutoff = 0.3
filter.emphasis = 0.2
filter.contour = 0.6
filter.keyboard1 = on
filter.attack = 0.55
filter.decay = 0.5
filter.sustain = 0.6
loudness.attack = 0.45
loudness.decay = 0.5
loudness.sustain = 0.9|}
    );
    ( "flute",
      {|# a triangle and a breath of noise, a slow attack
osc1.wave = triangle
osc1.level = 0.8
noise.on = on
noise.level = 0.12
osc3.range = LO
osc3.wave = triangle
osc3.keyboard = off
mod.oscillators = on
filter.cutoff = 0.58
filter.emphasis = 0.1
filter.contour = 0.1
filter.keyboard1 = on
filter.keyboard2 = on
loudness.attack = 0.35
loudness.decay = 0.4
loudness.sustain = 0.9|}
    );
    ( "whistle",
      {|# no oscillator: the filter oscillating, tracking the keyboard fully,
# its cutoff at C4's pitch; a trace of noise to start it
osc1.on = off
noise.on = on
noise.level = 0.01
filter.cutoff = 0.372
filter.emphasis = 1
filter.contour = 0
filter.keyboard1 = on
filter.keyboard2 = on
loudness.attack = 0.2
loudness.sustain = 1
volume = 1|}
    );
    ( "wind",
      {|# noise alone, the filter's contour slow both ways
osc1.on = off
noise.on = on
noise.level = 1
volume = 1
filter.cutoff = 0.2
filter.emphasis = 0.7
filter.contour = 0.7
filter.attack = 0.75
filter.decay = 0.8
filter.sustain = 0.2
loudness.attack = 0.5
loudness.decay = 0.8
loudness.sustain = 0.8|}
    );
    ( "space",
      {|# the lead through the rack: chorused, a dotted eighth's echoes
# ping-ponging, in the plate, held together by the compressor
osc1.wave = sawtooth
osc2.wave = sawtooth
osc2.on = on
osc2.frequency = 0.006
osc2.level = 0.7
glide.on = on
glide = 0.3
filter.cutoff = 0.5
filter.emphasis = 0.45
filter.contour = 0.4
filter.keyboard1 = on
filter.keyboard2 = on
filter.decay = 0.5
filter.sustain = 0.5
modulation.on = on
modulation.kind = chorus
modulation.depth = 0.6
delay.on = on
delay.pingpong = on
delay.feedback = 0.45
delay.mix = 0.35
reverb.on = on
reverb.time = 3
reverb.mix = 0.3
dynamics.on = on
dynamics.threshold = -18
dynamics.makeup = 3|}
    );
  ]

let presets : (string * patch) list =
  List.map
    (fun (name, text) ->
      match of_string text with Ok p -> (name, p) | Error e -> failwith (Printf.sprintf "the preset %s: %s" name e))
    preset_texts

(* ------------------------------------------------------------------ *)
(* The knobs' laws *)
(* ------------------------------------------------------------------ *)

let cutoff_hz (k : float) : float = 20. *. Float.pow 1000. k
let attack_seconds (k : float) : float = 0.001 *. Float.pow 10000. k
let decay_seconds (k : float) : float = 0.004 *. Float.pow 8750. k
let glide_seconds (k : float) : float = 0.001 *. Float.pow 10000. k
let emphasis_k (k : float) : float = 4.5 *. k
let range_octaves (r : int) : float = if r = 0 then -6. else float_of_int (r - 3)

let tracking (p : patch) : float =
  match (p.keyboard_1, p.keyboard_2) with false, false -> 0. | true, false -> 1. /. 3. | false, true -> 2. /. 3. | true, true -> 1.

(* ------------------------------------------------------------------ *)
(* Playing it *)
(* ------------------------------------------------------------------ *)

type options = { ladder : Moog_ladder.model; curve : Envelope.curve; drift : bool; band_limited : bool }

let analog = { ladder = Nonlinear; curve = Exponential; drift = true; band_limited = true }

(* an oscillator's state: its main Vco, and a second one in step with it
 * for the shark tooth's sawtooth *)
type vco = { main : Vco.t; saw : Vco.t; drift : Drift.t }

(* the blocks' buffers, grown to the largest block asked for *)
type buffers = {
  mutable size : int;
  mutable pitch : Signal.t;
  mutable frequency : Signal.t;
  mutable wave : Signal.t;
  mutable second : Signal.t;
  mutable width : Signal.t;
  mutable osc3 : Signal.t;
  mutable noise : Signal.t;
  mutable modulation : Signal.t;
  mutable mixed : Signal.t;
  mutable cutoff : Signal.t;
  mutable filter_env : Signal.t;
  mutable loudness_env : Signal.t;
}

type t = {
  mutable patch : patch;
  mutable last : patch; (* the last block's, ramped from *)
  mutable options : options;
  keys : Voicing.t;
  glide : Voicing.glide;
  oscillators : vco array;
  ladder : Moog_ladder.t;
  rack : Rack.t;
  filter_contour : Envelope.running;
  loudness_contour : Envelope.running;
  mutable random : int;
  mutable pitch_wheel : float;
  mutable mod_wheel : float;
  mutable cutoff_now : float;
  b : buffers;
  (* the last samples played, a ring, [at] the next to write *)
  ring : Signal.t;
  mutable at : int;
}

let create ?(options = analog) (patch : patch) : t =
  let vco seed = { main = Vco.create (); saw = Vco.create (); drift = Drift.create ~seed () } in
  let empty = [||] in
  {
    patch;
    last = patch;
    options;
    keys = Voicing.create ~priority:Low ();
    glide = Voicing.glide ();
    oscillators = [| vco 1; vco 2; vco 3 |];
    ladder = Moog_ladder.create ();
    rack = Rack.standard ();
    filter_contour = Envelope.start ();
    loudness_contour = Envelope.start ();
    random = 0;
    pitch_wheel = 0.;
    mod_wheel = 0.;
    cutoff_now = cutoff_hz patch.cutoff;
    b =
      {
        size = 0;
        pitch = empty;
        frequency = empty;
        wave = empty;
        second = empty;
        width = empty;
        osc3 = empty;
        noise = empty;
        modulation = empty;
        mixed = empty;
        cutoff = empty;
        filter_env = empty;
        loudness_env = empty;
      };
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (v : t) : patch = v.patch
let set_patch (v : t) (p : patch) : unit = v.patch <- p
let options (v : t) : options = v.options
let set_options (v : t) (o : options) : unit = v.options <- o
let pitch (v : t) : float = Voicing.pitch v.glide
let cutoff_now (v : t) : float = v.cutoff_now
let rack (v : t) : Rack.t = v.rack
let recent (v : t) : Signal.t = Array.init 2048 (fun i -> v.ring.((v.at + i) mod 2048))

let grow (b : buffers) (n : int) : unit =
  if b.size < n then (
    let a () = Array.make n 0. in
    b.size <- n;
    b.pitch <- a ();
    b.frequency <- a ();
    b.wave <- a ();
    b.second <- a ();
    b.width <- a ();
    b.osc3 <- a ();
    b.noise <- a ();
    b.modulation <- a ();
    b.mixed <- a ();
    b.cutoff <- a ();
    b.filter_env <- a ();
    b.loudness_env <- a ())

(* one oscillator's [n] samples into [b.wave], its frequencies already
 * in [b.frequency] *)
let oscillate (v : t) (o : vco) (w : wave) (n : int) : unit =
  let b = v.b and band_limited = v.options.band_limited in
  let frequency = Array.sub b.frequency 0 n and out = Array.make n 0. in
  let pulse width =
    Array.fill b.width 0 n width;
    Vco.fill ~band_limited ~width:(Array.sub b.width 0 n) o.main Pulse ~frequency out
  in
  (match w with
  | Triangle -> Vco.fill ~band_limited o.main Triangle ~frequency out
  | Sawtooth -> Vco.fill ~band_limited o.main Sawtooth ~frequency out
  | Reverse_sawtooth ->
      Vco.fill ~band_limited o.main Sawtooth ~frequency out;
      Array.iteri (fun i x -> out.(i) <- -.x) out
  (* the two in step, their phases advanced by the same frequencies *)
  | Shark_tooth ->
      let saw = Array.make n 0. in
      Vco.fill ~band_limited o.main Triangle ~frequency out;
      Vco.fill ~band_limited o.saw Sawtooth ~frequency saw;
      Array.iteri (fun i x -> out.(i) <- (0.75 *. x) +. (0.25 *. saw.(i))) out
  | Square -> pulse 0.5
  | Wide -> pulse 0.3
  | Narrow -> pulse 0.1);
  Array.blit out 0 b.wave 0 n

(* a knob ramped from the last block's position to this one's *)
let ramp (last : float) (now : float) (i : int) (n : int) : float = last +. ((now -. last) *. float_of_int (i + 1) /. float_of_int n)

let fill (v : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  grow v.b n;
  let b = v.b and p = v.patch and last = v.last in
  (* the pitch: glided, bent *)
  Voicing.fill_pitch v.glide ~seconds:(if p.glide_on then glide_seconds p.glide else 0.) b.pitch;
  let bend = 7. *. v.pitch_wheel and tune = p.tune in
  Array.iter (fun o -> Drift.advance o.drift n) v.oscillators;
  let drift o = if v.options.drift then Drift.factor o.drift else 1. in
  let osc_pitch (o : oscillator) i = b.pitch.(i) +. bend +. tune +. (12. *. range_octaves o.range) +. (7. *. o.frequency) in
  (* oscillator 3 first: it's also the modulation's source *)
  let o3 = v.oscillators.(2) in
  for i = 0 to n - 1 do
    let key = if p.osc3_keyboard then osc_pitch p.osc3 i else 60. +. tune +. (12. *. range_octaves p.osc3.range) +. (7. *. p.osc3.frequency) in
    b.frequency.(i) <- Voicing.frequency key *. drift o3
  done;
  oscillate v o3 (List.nth waves3 p.osc3.wave) n;
  Array.blit b.wave 0 b.osc3 0 n;
  (* the noise, and the modulation: the mix of the two, times the wheel *)
  for i = 0 to n - 1 do
    v.random <- Noise.lcg v.random;
    b.noise.(i) <- Noise.uniform v.random;
    b.modulation.(i) <- v.mod_wheel *. (((1. -. p.modulation_mix) *. b.osc3.(i)) +. (p.modulation_mix *. b.noise.(i)))
  done;
  (* the mixer: oscillator 3 and the noise, then 1 and 2, each level
   * ramped *)
  let level (o : oscillator) (l : oscillator) i = ramp (if l.on then l.level else 0.) (if o.on then o.level else 0.) i n in
  for i = 0 to n - 1 do
    b.mixed.(i) <-
      (level p.osc3 last.osc3 i *. b.osc3.(i))
      +. (ramp (if last.noise_on then last.noise else 0.) (if p.noise_on then p.noise else 0.) i n *. b.noise.(i))
  done;
  List.iteri
    (fun k (o : oscillator) ->
      let vco = v.oscillators.(k) and l = if k = 0 then last.osc1 else last.osc2 in
      for i = 0 to n - 1 do
        let m = if p.oscillator_modulation then 12. *. b.modulation.(i) else 0. in
        b.frequency.(i) <- Voicing.frequency (osc_pitch o i +. m) *. drift vco
      done;
      oscillate v vco (List.nth waves o.wave) n;
      for i = 0 to n - 1 do
        b.mixed.(i) <- b.mixed.(i) +. (level o l i *. b.wave.(i))
      done)
    [ p.osc1; p.osc2 ];
  (* the contours: the release the decay, or 4 ms *)
  let envelope (c : contour) : Envelope.t =
    let decay = decay_seconds c.decay in
    { attack = attack_seconds c.attack; decay; sustain = c.sustain; release = (if p.decay_on then decay else 0.004) }
  in
  let filter_env = Array.sub b.filter_env 0 n and loudness_env = Array.sub b.loudness_env 0 n in
  Envelope.fill v.options.curve (envelope p.filter_contour) v.filter_contour filter_env;
  Envelope.fill v.options.curve (envelope p.loudness_contour) v.loudness_contour loudness_env;
  (* the cutoff: the knob (ramped in octaves: the knob's law is
   * exponential), the keyboard, the contour, the modulation *)
  let track = tracking p in
  for i = 0 to n - 1 do
    let octaves =
      (track *. (b.pitch.(i) -. 60.) /. 12.)
      +. (4. *. p.contour_amount *. filter_env.(i))
      +. if p.filter_modulation then 3. *. b.modulation.(i) else 0.
    in
    b.cutoff.(i) <- cutoff_hz (ramp last.cutoff p.cutoff i n) *. Float.pow 2. octaves
  done;
  v.cutoff_now <- b.cutoff.(n - 1);
  let mixed = Array.sub b.mixed 0 n in
  Moog_ladder.process v.ladder v.options.ladder ~cutoff:(Array.sub b.cutoff 0 n) ~resonance:(emphasis_k p.emphasis) mixed;
  for i = 0 to n - 1 do
    let x = mixed.(i) *. loudness_env.(i) *. ramp last.volume p.volume i n in
    out.left.(i) <- x;
    out.right.(i) <- x
  done;
  (* the effects, their knobs from the patch, then what the scope shows:
   * the sound as heard *)
  List.iter (fun (name, x) -> Rack.set v.rack name x) p.effects;
  Rack.process v.rack out;
  for i = 0 to n - 1 do
    v.ring.(v.at) <- out.left.(i);
    v.at <- (v.at + 1) mod 2048
  done;
  v.last <- p

let instrument (v : t) : Instrument.t =
  let key (e : Voicing.event) =
    match e with
    | Begin n ->
        Voicing.glide_to v.glide n;
        Envelope.gate_on v.filter_contour;
        Envelope.gate_on v.loudness_contour
    | Change n -> Voicing.glide_to v.glide n
    | End ->
        Envelope.gate_off v.filter_contour;
        Envelope.gate_off v.loudness_contour
    | Nothing -> ()
  in
  let set name x =
    match name with
    | "pitch_wheel" -> v.pitch_wheel <- Float.min 1. (Float.max (-1.) x)
    | "mod_wheel" -> v.mod_wheel <- Float.min 1. (Float.max 0. x)
    | _ -> Option.iter (fun (k : knob) -> v.patch <- k.put v.patch x) (List.find_opt (fun (k : knob) -> k.name = name) knobs)
  in
  { note_on = (fun n _velocity -> key (Voicing.press v.keys n)); note_off = (fun n -> key (Voicing.release v.keys n)); set; fill = fill v }
