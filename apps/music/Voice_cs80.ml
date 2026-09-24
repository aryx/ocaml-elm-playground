(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Voice_cs80.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type layer = {
  feet : int;
  saw : float;
  pulse : float;
  width : float;
  pwm : float;
  pwm_speed : float;
  noise : float;
  hpf : float;
  hpf_res : float;
  lpf : float;
  lpf_res : float;
  il : float;
  al : float;
  f_attack : float;
  f_decay : float;
  f_release : float;
  attack : float;
  decay : float;
  sustain : float;
  release : float;
  sine : float;
  level : float;
  initial_brilliance : float;
  initial_level : float;
  after_brilliance : float;
  after_level : float;
}

type patch = {
  layers : layer array;
  mix : float;
  detune : float;
  sub_wave : int;
  sub_speed : float;
  sub_vco : float;
  sub_vcf : float;
  sub_vca : float;
  ring_speed : float;
  ring_depth : float;
  ring_attack : float;
  ring_decay : float;
  chorus : bool;
  tremolo : bool;
  volume : float;
}

let feet = [ "16'"; "8'"; "4'" ]
let sub_waves = [ "sine"; "saw"; "ramp"; "pulse"; "noise" ]

let layer0 : layer =
  {
    feet = 1;
    saw = 1.;
    pulse = 0.;
    width = 0.;
    pwm = 0.;
    pwm_speed = 0.4;
    noise = 0.;
    hpf = 0.;
    hpf_res = 0.;
    lpf = 0.6;
    lpf_res = 0.2;
    il = 0.;
    al = 0.3;
    f_attack = 0.2;
    f_decay = 0.5;
    f_release = 0.5;
    attack = 0.1;
    decay = 0.5;
    sustain = 0.8;
    release = 0.5;
    sine = 0.;
    level = 0.8;
    initial_brilliance = 0.3;
    initial_level = 0.5;
    after_brilliance = 0.5;
    after_level = 0.3;
  }

let initial : patch =
  {
    layers = [| layer0; layer0 |];
    mix = 0.5;
    detune = 0.2;
    sub_wave = 0;
    sub_speed = 0.5;
    sub_vco = 0.;
    sub_vcf = 0.;
    sub_vca = 0.;
    ring_speed = 0.5;
    ring_depth = 0.;
    ring_attack = 0.;
    ring_decay = 1.;
    chorus = false;
    tremolo = false;
    volume = 0.7;
  }

type knob = patch Patch_text.knob

let layer_knobs (i : int) : knob list =
  let name s = (if i = 0 then "I." else "II.") ^ s in
  let get f (p : patch) = f p.layers.(i) in
  let put f (p : patch) x =
    let ls = Array.copy p.layers in
    ls.(i) <- f ls.(i) x;
    { p with layers = ls }
  in
  let knob n g s = Patch_text.knob (name n) (get g) (put s) in
  let bipolar n g s = Patch_text.detune (name n) (get g) (put s) in
  [
    Patch_text.selector (name "feet") feet (get (fun l -> l.feet)) (put (fun l x -> { l with feet = x }));
    knob "saw" (fun l -> l.saw) (fun l x -> { l with saw = x });
    knob "pulse" (fun l -> l.pulse) (fun l x -> { l with pulse = x });
    knob "width" (fun l -> l.width) (fun l x -> { l with width = x });
    knob "pwm" (fun l -> l.pwm) (fun l x -> { l with pwm = x });
    knob "pwm_speed" (fun l -> l.pwm_speed) (fun l x -> { l with pwm_speed = x });
    knob "noise" (fun l -> l.noise) (fun l x -> { l with noise = x });
    knob "hpf" (fun l -> l.hpf) (fun l x -> { l with hpf = x });
    knob "hpf_res" (fun l -> l.hpf_res) (fun l x -> { l with hpf_res = x });
    knob "lpf" (fun l -> l.lpf) (fun l x -> { l with lpf = x });
    knob "lpf_res" (fun l -> l.lpf_res) (fun l x -> { l with lpf_res = x });
    bipolar "il" (fun l -> l.il) (fun l x -> { l with il = x });
    bipolar "al" (fun l -> l.al) (fun l x -> { l with al = x });
    knob "f_attack" (fun l -> l.f_attack) (fun l x -> { l with f_attack = x });
    knob "f_decay" (fun l -> l.f_decay) (fun l x -> { l with f_decay = x });
    knob "f_release" (fun l -> l.f_release) (fun l x -> { l with f_release = x });
    knob "attack" (fun l -> l.attack) (fun l x -> { l with attack = x });
    knob "decay" (fun l -> l.decay) (fun l x -> { l with decay = x });
    knob "sustain" (fun l -> l.sustain) (fun l x -> { l with sustain = x });
    knob "release" (fun l -> l.release) (fun l x -> { l with release = x });
    knob "sine" (fun l -> l.sine) (fun l x -> { l with sine = x });
    knob "level" (fun l -> l.level) (fun l x -> { l with level = x });
    knob "initial.brilliance" (fun l -> l.initial_brilliance) (fun l x -> { l with initial_brilliance = x });
    knob "initial.level" (fun l -> l.initial_level) (fun l x -> { l with initial_level = x });
    knob "after.brilliance" (fun l -> l.after_brilliance) (fun l x -> { l with after_brilliance = x });
    knob "after.level" (fun l -> l.after_level) (fun l x -> { l with after_level = x });
  ]

let knobs : knob list =
  layer_knobs 0 @ layer_knobs 1
  @ [
      Patch_text.knob "mix" (fun p -> p.mix) (fun p x -> { p with mix = x });
      Patch_text.detune "detune" (fun p -> p.detune) (fun p x -> { p with detune = x });
      Patch_text.selector "sub.wave" sub_waves (fun p -> p.sub_wave) (fun p x -> { p with sub_wave = x });
      Patch_text.knob "sub.speed" (fun p -> p.sub_speed) (fun p x -> { p with sub_speed = x });
      Patch_text.knob "sub.vco" (fun p -> p.sub_vco) (fun p x -> { p with sub_vco = x });
      Patch_text.knob "sub.vcf" (fun p -> p.sub_vcf) (fun p x -> { p with sub_vcf = x });
      Patch_text.knob "sub.vca" (fun p -> p.sub_vca) (fun p x -> { p with sub_vca = x });
      Patch_text.knob "ring.speed" (fun p -> p.ring_speed) (fun p x -> { p with ring_speed = x });
      Patch_text.knob "ring.depth" (fun p -> p.ring_depth) (fun p x -> { p with ring_depth = x });
      Patch_text.knob "ring.attack" (fun p -> p.ring_attack) (fun p x -> { p with ring_attack = x });
      Patch_text.knob "ring.decay" (fun p -> p.ring_decay) (fun p x -> { p with ring_decay = x });
      Patch_text.switch "chorus" (fun p -> p.chorus) (fun p x -> { p with chorus = x });
      Patch_text.switch "tremolo" (fun p -> p.tremolo) (fun p x -> { p with tremolo = x });
      Patch_text.knob "volume" (fun p -> p.volume) (fun p x -> { p with volume = x });
    ]

let to_string (p : patch) : string = Patch_text.to_string knobs p
let of_string (text : string) : (patch, string) result = Patch_text.of_string knobs ~initial text

(* ours, after the sounds the CS-80 is known for *)
let presets : (string * patch) list =
  let both l = [| l; l |] in
  [
    (* Blade Runner's end titles: two sawtooths, a slow filter swelling
     * and the pressure opening it further, each note its own *)
    ( "brass",
      {
        initial with
        layers =
          both
            {
              layer0 with
              lpf = 0.35;
              lpf_res = 0.25;
              il = 0.;
              al = 0.6;
              f_attack = 0.45;
              f_decay = 0.6;
              attack = 0.25;
              sustain = 0.9;
              release = 0.55;
              initial_brilliance = 0.2;
              after_brilliance = 0.8;
            };
        detune = 0.3;
        chorus = true;
      } );
    (* pulses, their widths moving: strings, the high-pass thinning them *)
    ( "strings",
      {
        initial with
        layers = both { layer0 with saw = 0.; pulse = 1.; width = 0.3; pwm = 0.6; pwm_speed = 0.45; hpf = 0.3; lpf = 0.55; al = 0.1; attack = 0.5; release = 0.6 };
        detune = 0.4;
        chorus = true;
      } );
    (* two sounds: a sawtooth, and a pulse an octave down; the
     * sub-oscillator moving the filters *)
    ( "pad",
      {
        initial with
        layers =
          [|
            { layer0 with lpf = 0.45; attack = 0.65; release = 0.7; al = 0.2 };
            { layer0 with feet = 0; saw = 0.; pulse = 0.8; width = 0.5; lpf = 0.4; attack = 0.75; release = 0.7 };
          |];
        sub_speed = 0.3;
        sub_vcf = 0.3;
        chorus = true;
      } );
    (* a sine, struck, through the ring modulator: bells *)
    ( "ring bells",
      {
        initial with
        layers = both { layer0 with saw = 0.; sine = 1.; attack = 0.; decay = 0.8; sustain = 0.; release = 0.7 };
        ring_speed = 0.62;
        ring_depth = 0.9;
        ring_decay = 0.55;
      } );
    (* a sawtooth and a pulse an octave up, bright, the vibrato on the
     * sub-oscillator: a lead for the ribbon *)
    ( "lead",
      {
        initial with
        layers = [| { layer0 with lpf = 0.7; lpf_res = 0.4 }; { layer0 with feet = 2; saw = 0.; pulse = 0.7; width = 0.4; lpf = 0.7 } |];
        mix = 0.4;
        sub_speed = 0.55;
        sub_vco = 0.25;
      } );
  ]

(*****************************************************************************)
(* The curves *)
(*****************************************************************************)

let seconds (k : float) : float = 0.002 *. Float.pow 5000. k
let cutoff_hz (k : float) : float = 20. *. Float.pow 1000. k

(* ours: IL to AL in the attack, down to 0 (the cutoff set) in the
 * decay, held, back to IL in the release -- all straight lines *)
let filter_envelope (l : layer) ~(held : float) (t : float) : float =
  let a = seconds l.f_attack and d = seconds l.f_decay and r = seconds l.f_release in
  let down t = if t < a then l.il +. ((l.al -. l.il) *. t /. a) else if t < a +. d then l.al *. (1. -. ((t -. a) /. d)) else 0. in
  if t < held then down t
  else
    let at_release = down held in
    let s = t -. held in
    if s < r then at_release +. ((l.il -. at_release) *. s /. r) else l.il

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

let rate = float_of_int Signal.rate
let frequency (key : int) : float = 440. *. Float.pow 2. (float_of_int (key - 69) /. 12.)

(* the resonance kept from oscillating: Q up to 4 (Svf's 0.707 flat) *)
let q (res : float) : float = 0.707 +. (3.3 *. res)

(* a section running for one key *)
type section = {
  saw_osc : Vco.t;
  pulse_osc : Vco.t;
  hp : Svf.t;
  lp : Svf.t;
  amp : Envelope.running;
  mutable sine_phase : float;
  mutable pwm_phase : float;
}

let section () : section =
  let amp = Envelope.start () in
  Envelope.gate_on amp;
  { saw_osc = Vco.create (); pulse_osc = Vco.create (); hp = Svf.create (); lp = Svf.create (); amp; sine_phase = 0.; pwm_phase = 0. }

(* what the instrument shares with its voices, read every chunk: the
 * sub-oscillator's value, the ribbon, the pressures *)
type shared = {
  mutable lfo : float; (* -1 to 1 *)
  mutable bend : float; (* semitones *)
  pressures : (int, float) Hashtbl.t;
  mutable random : int;
}

type scratch = {
  mutable freq : Signal.t;
  mutable width : Signal.t;
  mutable a : Signal.t;
  mutable b : Signal.t;
  mutable hp_cut : Signal.t;
  mutable lp_cut : Signal.t;
  mutable env : Signal.t;
}

let scratch = { freq = [||]; width = [||]; a = [||]; b = [||]; hp_cut = [||]; lp_cut = [||]; env = [||] }

let sized (n : int) : unit =
  if Array.length scratch.freq <> n then begin
    scratch.freq <- Array.make n 0.;
    scratch.width <- Array.make n 0.;
    scratch.a <- Array.make n 0.;
    scratch.b <- Array.make n 0.;
    scratch.hp_cut <- Array.make n 0.;
    scratch.lp_cut <- Array.make n 0.;
    scratch.env <- Array.make n 0.
  end

let voice (patch : unit -> patch) (sh : shared) (key : int) (velocity : float) : Polyphony.voice =
  let sections = [| section (); section () |] in
  let age = ref 0 (* samples since the key *) and let_go = ref None and ring_phase = ref 0. in
  let fill (out : Signal.t) =
    let p = patch () in
    let n = Array.length out in
    sized n;
    Array.fill out 0 n 0.;
    let pressure = Option.value ~default:0. (Hashtbl.find_opt sh.pressures key) in
    let held = match !let_go with Some s -> float_of_int s /. rate | None -> infinity in
    Array.iteri
      (fun li (l : layer) ->
        let s = sections.(li) in
        let gain = l.level *. Float.min 1. (2. *. if li = 0 then 1. -. p.mix else p.mix) in
        if gain > 0. then begin
          (* the pitch: the feet, section II's detune, the ribbon, the
           * sub-oscillator's vibrato *)
          let semis = (12. *. float_of_int (l.feet - 1)) +. (if li = 1 then 0.5 *. p.detune else 0.) +. sh.bend +. (p.sub_vco *. sh.lfo) in
          let f = frequency key *. Float.pow 2. (semis /. 12.) in
          Array.fill scratch.freq 0 n f;
          (* the touch: velocity and pressure, into the brilliance and
           * the level (ours: 3 octaves of brilliance each) *)
          let brilliance = (3. *. l.initial_brilliance *. (velocity -. 0.5)) +. (3. *. l.after_brilliance *. pressure) +. (2. *. p.sub_vcf *. sh.lfo) in
          let loud = (1. -. l.initial_level +. (l.initial_level *. velocity)) *. (1. +. (l.after_level *. pressure)) in
          (* the width and the filter envelope once a chunk (64
           * samples, 1.5 ms): they move slower than that *)
          let w = 0.5 +. (0.4 *. l.width) +. (0.2 *. l.pwm *. sin (2. *. Float.pi *. s.pwm_phase)) in
          Array.fill scratch.width 0 n (Float.min 0.95 (Float.max 0.5 w));
          s.pwm_phase <- Float.rem (s.pwm_phase +. (0.1 *. Float.pow 100. l.pwm_speed *. float_of_int n /. rate)) 1.;
          let octaves = (4. *. filter_envelope l ~held (float_of_int !age /. rate)) +. brilliance in
          let shift = Float.pow 2. octaves in
          Array.fill scratch.hp_cut 0 n (cutoff_hz l.hpf *. shift);
          Array.fill scratch.lp_cut 0 n (cutoff_hz l.lpf *. shift);
          Vco.fill s.saw_osc Sawtooth ~frequency:scratch.freq scratch.a;
          Vco.fill ~width:scratch.width s.pulse_osc Pulse ~frequency:scratch.freq scratch.b;
          for i = 0 to n - 1 do
            scratch.a.(i) <- (l.saw *. scratch.a.(i)) +. (l.pulse *. scratch.b.(i))
          done;
          if l.noise > 0. then
            for i = 0 to n - 1 do
              sh.random <- Noise.lcg sh.random;
              scratch.a.(i) <- scratch.a.(i) +. (l.noise *. Noise.uniform sh.random)
            done;
          Svf.process s.hp Zero_delay High_pass ~cutoff:scratch.hp_cut ~q:(q l.hpf_res) scratch.a;
          Svf.process s.lp Zero_delay Low_pass ~cutoff:scratch.lp_cut ~q:(q l.lpf_res) scratch.a;
          let adsr : Envelope.t = { attack = seconds l.attack; decay = seconds l.decay; sustain = l.sustain; release = seconds l.release } in
          Envelope.fill Exponential adsr s.amp scratch.env;
          if l.sine > 0. then
            for i = 0 to n - 1 do
              scratch.a.(i) <- scratch.a.(i) +. (l.sine *. sin (2. *. Float.pi *. s.sine_phase));
              s.sine_phase <- Float.rem (s.sine_phase +. (f /. rate)) 1.
            done;
          for i = 0 to n - 1 do
            out.(i) <- out.(i) +. (gain *. loud *. scratch.env.(i) *. scratch.a.(i))
          done
        end
        else Envelope.fill Exponential { attack = 0.; decay = 0.; sustain = 0.; release = 0.001 } s.amp scratch.env)
      p.layers;
    (* the ring modulator: an LFO multiplying the sound, its depth
     * through its own attack and decay (decay 1: held) *)
    if p.ring_depth > 0. then begin
      let hz = Float.pow 1000. p.ring_speed and a = seconds (p.ring_attack *. 0.8) and d = seconds (p.ring_decay *. 0.8) in
      for i = 0 to n - 1 do
        let t = float_of_int (!age + i) /. rate in
        let env = if t < a then t /. a else if p.ring_decay >= 1. then 1. else Float.max 0. (1. -. ((t -. a) /. d)) in
        let amount = p.ring_depth *. env in
        out.(i) <- out.(i) *. (1. -. amount +. (amount *. sin (2. *. Float.pi *. !ring_phase)));
        ring_phase := Float.rem (!ring_phase +. (hz /. rate)) 1.
      done
    end;
    (* the sub-oscillator's tremolo *)
    if p.sub_vca > 0. then Array.iteri (fun i x -> out.(i) <- x *. (1. -. (p.sub_vca *. (1. -. sh.lfo) /. 2.))) out;
    age := !age + n
  in
  let release () =
    let_go := Some !age;
    Array.iter (fun s -> Envelope.gate_off s.amp) sections
  in
  let silent () = !let_go <> None && Array.for_all (fun s -> Envelope.stage s.amp = Idle) sections in
  { release; fill; silent }

let sub_shapes : Lfo.shape array = [| Sine; Saw_down; Saw_up; Square; Sample_and_hold |]

type t = {
  mutable patch : patch;
  poly : Polyphony.t;
  shared : shared;
  sub : Lfo.t;
  mutable sub_buf : Signal.t;
  chorus : Modulated_delay.t;
  mutable tremolo_phase : float;
  mutable chunk : Signal.t;
  ring : Signal.t;
  mutable at : int;
}

let create (patch : patch) : t =
  {
    patch;
    poly = Polyphony.create ~voices:8 ();
    shared = { lfo = 0.; bend = 0.; pressures = Hashtbl.create 8; random = 1 };
    sub = Lfo.create ();
    sub_buf = [||];
    chorus = Modulated_delay.create ();
    tremolo_phase = 0.;
    chunk = [||];
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (t : t) : patch = t.patch
let set_patch (t : t) (p : patch) : unit = t.patch <- p
let voices (t : t) : int = Polyphony.voices t.poly
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))
let pressure (t : t) (key : int) (p : float) : unit = Hashtbl.replace t.shared.pressures key (Float.max 0. (Float.min 1. p))
let bend (t : t) (semitones : float) : unit = t.shared.bend <- semitones

(* ours: two sections at full on a chord of four peaking near 1 *)
let gain = 0.12

(* 64 samples at a time: the sub-oscillator, the ribbon and the
 * pressures read between chunks, not once a frame *)
let chunk = 64

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let p = t.patch in
  let at = ref 0 in
  while !at < n do
    let m = min chunk (n - !at) in
    if Array.length t.chunk <> m then t.chunk <- Array.make m 0.;
    if Array.length t.sub_buf <> m then t.sub_buf <- Array.make m 0.;
    Lfo.fill t.sub sub_shapes.(p.sub_wave) ~rate:(0.1 *. Float.pow 200. p.sub_speed) t.sub_buf;
    t.shared.lfo <- t.sub_buf.(0);
    Polyphony.fill t.poly t.chunk;
    for i = 0 to m - 1 do
      let trem =
        if p.tremolo then begin
          t.tremolo_phase <- Float.rem (t.tremolo_phase +. (6. /. rate)) 1.;
          1. -. (0.3 *. (1. +. sin (2. *. Float.pi *. t.tremolo_phase)) /. 2.)
        end
        else 1.
      in
      let x = gain *. p.volume *. trem *. t.chunk.(i) in
      out.left.(!at + i) <- x;
      out.right.(!at + i) <- x
    done;
    at := !at + m
  done;
  if p.chorus then Modulated_delay.process t.chorus Modulated_delay.chorus out;
  Array.iter
    (fun x ->
      t.ring.(t.at) <- x;
      t.at <- (t.at + 1) mod 2048)
    out.left

let instrument (t : t) : Instrument.t =
  {
    note_on =
      (fun key velocity ->
        Hashtbl.remove t.shared.pressures key;
        Polyphony.press t.poly key (voice (fun () -> t.patch) t.shared key velocity));
    note_off =
      (fun key ->
        Hashtbl.remove t.shared.pressures key;
        Polyphony.release t.poly key);
    set =
      (fun name x ->
        if name = "volume" then t.patch <- { t.patch with volume = x }
        else Option.iter (fun (k : knob) -> t.patch <- k.put t.patch x) (List.find_opt (fun (k : knob) -> k.name = name) knobs));
    fill = fill t;
  }
