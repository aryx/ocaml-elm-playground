(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Voice_juno.mli *)

(*****************************************************************************)
(* The patch *)
(*****************************************************************************)

type patch = {
  lfo_rate : float;
  lfo_delay : float;
  range : int;
  dco_lfo : float;
  pwm : float;
  pwm_lfo : bool;
  pulse : bool;
  saw : bool;
  sub : float;
  noise : float;
  hpf : int;
  cutoff : float;
  resonance : float;
  env : float;
  env_invert : bool;
  vcf_lfo : float;
  key_follow : float;
  gate : bool;
  level : float;
  attack : float;
  decay : float;
  sustain : float;
  release : float;
  chorus : int;
  volume : float;
}

let ranges = [ "16'"; "8'"; "4'" ]
let choruses = [ "off"; "I"; "II"; "I+II" ]

let initial : patch =
  {
    lfo_rate = 0.4;
    lfo_delay = 0.;
    range = 1;
    dco_lfo = 0.;
    pwm = 0.;
    pwm_lfo = false;
    pulse = false;
    saw = true;
    sub = 0.;
    noise = 0.;
    hpf = 1;
    cutoff = 0.6;
    resonance = 0.;
    env = 0.3;
    env_invert = false;
    vcf_lfo = 0.;
    key_follow = 0.5;
    gate = false;
    level = 0.8;
    attack = 0.1;
    decay = 0.5;
    sustain = 0.6;
    release = 0.4;
    chorus = 1;
    volume = 0.7;
  }

type knob = patch Patch_text.knob

let knobs : knob list =
  let k name get put = Patch_text.knob name get put and s name get put = Patch_text.switch name get put in
  [
    k "lfo.rate" (fun p -> p.lfo_rate) (fun p x -> { p with lfo_rate = x });
    k "lfo.delay" (fun p -> p.lfo_delay) (fun p x -> { p with lfo_delay = x });
    Patch_text.selector "dco.range" ranges (fun p -> p.range) (fun p x -> { p with range = x });
    k "dco.lfo" (fun p -> p.dco_lfo) (fun p x -> { p with dco_lfo = x });
    k "dco.pwm" (fun p -> p.pwm) (fun p x -> { p with pwm = x });
    s "dco.pwm_lfo" (fun p -> p.pwm_lfo) (fun p x -> { p with pwm_lfo = x });
    s "dco.pulse" (fun p -> p.pulse) (fun p x -> { p with pulse = x });
    s "dco.saw" (fun p -> p.saw) (fun p x -> { p with saw = x });
    k "dco.sub" (fun p -> p.sub) (fun p x -> { p with sub = x });
    k "dco.noise" (fun p -> p.noise) (fun p x -> { p with noise = x });
    Patch_text.selector "hpf" [ "0"; "1"; "2"; "3" ] (fun p -> p.hpf) (fun p x -> { p with hpf = x });
    k "vcf.cutoff" (fun p -> p.cutoff) (fun p x -> { p with cutoff = x });
    k "vcf.resonance" (fun p -> p.resonance) (fun p x -> { p with resonance = x });
    k "vcf.env" (fun p -> p.env) (fun p x -> { p with env = x });
    s "vcf.env_invert" (fun p -> p.env_invert) (fun p x -> { p with env_invert = x });
    k "vcf.lfo" (fun p -> p.vcf_lfo) (fun p x -> { p with vcf_lfo = x });
    k "vcf.key" (fun p -> p.key_follow) (fun p x -> { p with key_follow = x });
    s "vca.gate" (fun p -> p.gate) (fun p x -> { p with gate = x });
    k "vca.level" (fun p -> p.level) (fun p x -> { p with level = x });
    k "env.attack" (fun p -> p.attack) (fun p x -> { p with attack = x });
    k "env.decay" (fun p -> p.decay) (fun p x -> { p with decay = x });
    k "env.sustain" (fun p -> p.sustain) (fun p x -> { p with sustain = x });
    k "env.release" (fun p -> p.release) (fun p x -> { p with release = x });
    Patch_text.selector "chorus" choruses (fun p -> p.chorus) (fun p x -> { p with chorus = x });
    k "volume" (fun p -> p.volume) (fun p x -> { p with volume = x });
  ]

let to_string (p : patch) : string = Patch_text.to_string knobs p
let of_string (text : string) : (patch, string) result = Patch_text.of_string knobs ~initial text

(* ours, after the kinds of sound the Juno is known for *)
let presets : (string * patch) list =
  [
    (* the sawtooth, the envelope opening the filter: a brass stab *)
    ("brass", { initial with cutoff = 0.35; env = 0.45; resonance = 0.15; attack = 0.25; decay = 0.45; sustain = 0.7; chorus = 1 });
    (* saw and a pulse whose width the LFO moves, chorus II: strings *)
    ( "strings",
      { initial with pulse = true; pwm = 0.5; pwm_lfo = true; lfo_rate = 0.35; cutoff = 0.55; env = 0.1; attack = 0.55; release = 0.55; hpf = 2; chorus = 2 } );
    (* the sawtooth and the sub an octave down, the filter closing fast:
     * the bass of a thousand records, the high-pass's shelf under it *)
    ("bass", { initial with sub = 0.8; cutoff = 0.25; resonance = 0.35; env = 0.5; attack = 0.; decay = 0.4; sustain = 0.; release = 0.25; hpf = 0; chorus = 0 });
    (* the pulse alone, its width moving slowly, chorus I, long *)
    ( "pad",
      { initial with saw = false; pulse = true; pwm = 0.4; pwm_lfo = true; lfo_rate = 0.25; cutoff = 0.45; env = 0.15; attack = 0.65; release = 0.7; chorus = 1 } );
    (* a bright sawtooth, its vibrato faded in after the key *)
    ( "lead",
      { initial with range = 2; cutoff = 0.65; resonance = 0.4; env = 0.2; lfo_rate = 0.55; lfo_delay = 0.4; dco_lfo = 0.25; attack = 0.02; chorus = 3 } );
  ]

(*****************************************************************************)
(* The curves *)
(*****************************************************************************)

let attack_seconds (k : float) : float = 0.001 +. ((exp (5. *. k) -. 1.) /. (exp 5. -. 1.) *. 3.25)
let decay_seconds (k : float) : float = 0.002 +. ((exp (4. *. k) -. 1.) /. (exp 4. -. 1.) *. 17.46 *. k)
let cutoff_hz (k : float) : float = 20. *. Float.pow 1000. k
let lfo_hz (k : float) : float = 0.1 *. Float.pow 200. k
let rate = float_of_int Signal.rate

(*****************************************************************************)
(* The chorus *)
(*****************************************************************************)

(* a triangle, 0 to 1, [t] seconds into a cycle of [hz] *)
let triangle (hz : float) (t : float) : float =
  let p = Float.rem (t *. hz) 1. in
  if p < 0.5 then 2. *. p else 2. -. (2. *. p)

let chorus_delays (mode : int) (t : float) : float * float =
  let lo, hi, hz, stereo = if mode = 3 then (0.0033, 0.0037, 9.75, false) else (0.00166, 0.00535, (if mode = 1 then 0.513 else 0.863), true) in
  let x = triangle hz t in
  let left = lo +. ((hi -. lo) *. x) in
  (* the right line's modulation inverted; I+II the same on both sides *)
  let right = if stereo then lo +. ((hi -. lo) *. (1. -. x)) else left in
  (left, right)

(* the two lines: a ring each, the time, one LFO *)
type chorus = { left : Signal.t; right : Signal.t; mutable at : int; mutable time : float }

let chorus_line = 512 (* 11.6 ms, more than the longest delay *)
let new_chorus () : chorus = { left = Array.make chorus_line 0.; right = Array.make chorus_line 0.; at = 0; time = 0. }

(* the line read [d] seconds ago, between two samples *)
let read (line : Signal.t) (at : int) (d : float) : float =
  let back = d *. rate in
  let whole = Float.to_int back in
  let frac = back -. float_of_int whole in
  let get k = line.((at - k + (2 * chorus_line)) mod chorus_line) in
  ((1. -. frac) *. get whole) +. (frac *. get (whole + 1))

(* mono in, stereo out: each side the dry sound and its line's copy *)
let chorus_process (c : chorus) (mode : int) (mono : Signal.t) (out : Signal.stereo) : unit =
  Array.iteri
    (fun i x ->
      c.left.(c.at) <- x;
      c.right.(c.at) <- x;
      let dl, dr = chorus_delays mode c.time in
      out.left.(i) <- 0.5 *. (x +. read c.left c.at dl);
      out.right.(i) <- 0.5 *. (x +. read c.right c.at dr);
      c.at <- (c.at + 1) mod chorus_line;
      c.time <- c.time +. (1. /. rate))
    mono

(*****************************************************************************)
(* The high-pass *)
(*****************************************************************************)

(* one pole, a low-pass's memory: position 0 adds it back (+6 dB below
 * 65 Hz), 2 and 3 take it away (6 dB/octave below 225, 720 Hz) *)
type hp = { mutable low : float }

let hp_process (h : hp) (position : int) (s : Signal.t) : unit =
  let corner = match position with 0 -> 65. | 2 -> 225. | 3 -> 720. | _ -> 0. in
  if corner > 0. then begin
    let a = 1. -. exp (-2. *. Float.pi *. corner /. rate) in
    Array.iteri
      (fun i x ->
        h.low <- h.low +. (a *. (x -. h.low));
        s.(i) <- (if position = 0 then x +. h.low else x -. h.low))
      s
  end

let high_pass (position : int) (s : Signal.t) : Signal.t =
  let out = Array.copy s in
  hp_process { low = 0. } position out;
  out

(*****************************************************************************)
(* Playing it *)
(*****************************************************************************)

let frequency (key : int) : float = 440. *. Float.pow 2. (float_of_int (key - 69) /. 12.)

type scratch = {
  mutable freq : Signal.t;
  mutable half : Signal.t;
  mutable width : Signal.t;
  mutable a : Signal.t;
  mutable b : Signal.t;
  mutable cut : Signal.t;
  mutable env : Signal.t;
}

let scratch = { freq = [||]; half = [||]; width = [||]; a = [||]; b = [||]; cut = [||]; env = [||] }

let sized (n : int) : unit =
  if Array.length scratch.freq <> n then begin
    scratch.freq <- Array.make n 0.;
    scratch.half <- Array.make n 0.;
    scratch.width <- Array.make n 0.;
    scratch.a <- Array.make n 0.;
    scratch.b <- Array.make n 0.;
    scratch.cut <- Array.make n 0.;
    scratch.env <- Array.make n 0.
  end

(* what the voices share: the LFO's value now (-1 to 1), the noise *)
type shared = { mutable lfo : float; mutable random : int }

(* a voice: the DCO (a sawtooth, a pulse, the sub a pulse at half the
 * frequency), the filter, the envelope; blocks of 64 samples, the LFO
 * and the envelope's effect on the cutoff read once each *)
let voice (patch : unit -> patch) (sh : shared) (key : int) (velocity : float) : Polyphony.voice =
  let saw = Vco.create () and pulse = Vco.create () and sub = Vco.create () and ladder = Moog_ladder.create () in
  let env = Envelope.start () in
  Envelope.gate_on env;
  let age = ref 0 and held = ref true and gate = ref 0. in
  let fill (out : Signal.t) =
    let p = patch () in
    let n = Array.length out in
    sized n;
    (* the LFO faded in after the key, over its delay *)
    let delay = 3. *. p.lfo_delay in
    let fade = if delay <= 0. then 1. else Float.min 1. (float_of_int !age /. rate /. delay) in
    let lfo = fade *. sh.lfo in
    let f = frequency key *. Float.pow 2. (float_of_int (p.range - 1) +. (p.dco_lfo *. lfo /. 12.)) in
    Array.fill scratch.freq 0 n f;
    Array.fill scratch.half 0 n (f /. 2.);
    let w = if p.pwm_lfo then 0.5 +. (0.45 *. p.pwm *. (0.5 +. (0.5 *. lfo))) else 0.5 +. (0.45 *. p.pwm) in
    Array.fill scratch.width 0 n w;
    Array.fill out 0 n 0.;
    if p.saw then begin
      Vco.fill saw Sawtooth ~frequency:scratch.freq scratch.a;
      Array.iteri (fun i x -> out.(i) <- out.(i) +. x) scratch.a
    end;
    if p.pulse then begin
      Vco.fill ~width:scratch.width pulse Pulse ~frequency:scratch.freq scratch.a;
      Array.iteri (fun i x -> out.(i) <- out.(i) +. x) scratch.a
    end;
    if p.sub > 0. then begin
      Vco.fill sub Pulse ~frequency:scratch.half scratch.a;
      Array.iteri (fun i x -> out.(i) <- out.(i) +. (p.sub *. x)) scratch.a
    end;
    if p.noise > 0. then
      for i = 0 to n - 1 do
        sh.random <- Noise.lcg sh.random;
        out.(i) <- out.(i) +. (p.noise *. Noise.uniform sh.random)
      done;
    (* the envelope, for both the filter and (unless gated) the VCA *)
    let adsr : Envelope.t = { attack = attack_seconds p.attack; decay = decay_seconds p.decay; sustain = p.sustain; release = decay_seconds p.release } in
    Envelope.fill Exponential adsr env scratch.env;
    let e = scratch.env.(0) in
    let octaves =
      (7. *. p.env *. if p.env_invert then -.e else e) +. (2. *. p.vcf_lfo *. lfo) +. (p.key_follow *. float_of_int (key - 60) /. 12.)
    in
    Array.fill scratch.cut 0 n (cutoff_hz p.cutoff *. Float.pow 2. octaves);
    Moog_ladder.process ladder Nonlinear ~cutoff:scratch.cut ~resonance:(4.2 *. p.resonance) out;
    for i = 0 to n - 1 do
      (* the gate: 2 ms each way, no click *)
      gate := if !held then Float.min 1. (!gate +. (500. /. rate)) else Float.max 0. (!gate -. (500. /. rate));
      let amp = if p.gate then !gate else scratch.env.(i) in
      out.(i) <- p.level *. velocity *. amp *. out.(i)
    done;
    age := !age + n
  in
  let release () =
    held := false;
    Envelope.gate_off env
  in
  let silent () =
    let p = patch () in
    (not !held) && if p.gate then !gate = 0. else Envelope.stage env = Idle
  in
  { release; fill; silent }

type t = {
  mutable patch : patch;
  poly : Polyphony.t;
  shared : shared;
  mutable lfo_phase : float;
  hp : hp;
  chorus : chorus;
  mutable mono : Signal.t;
  mutable stereo : Signal.stereo;
  ring : Signal.t;
  mutable at : int;
}

let create (patch : patch) : t =
  {
    patch;
    poly = Polyphony.create ~voices:6 ();
    shared = { lfo = 0.; random = 1 };
    lfo_phase = 0.;
    hp = { low = 0. };
    chorus = new_chorus ();
    mono = [||];
    stereo = { left = [||]; right = [||] };
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (t : t) : patch = t.patch
let set_patch (t : t) (p : patch) : unit = t.patch <- p
let voices (t : t) : int = Polyphony.voices t.poly
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))

(* ours: the presets' phrases (Unit_juno) peaking from a third to two
 * thirds, the nonlinear ladder's loss and the chorus's half counted *)
let gain = 0.8
let chunk = 64

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let p = t.patch in
  if Array.length t.mono <> n then t.mono <- Array.make n 0.;
  let at = ref 0 in
  while !at < n do
    let m = min chunk (n - !at) in
    (* the LFO, a triangle, once a chunk *)
    t.lfo_phase <- Float.rem (t.lfo_phase +. (lfo_hz p.lfo_rate *. float_of_int m /. rate)) 1.;
    t.shared.lfo <- (2. *. triangle 1. t.lfo_phase) -. 1.;
    let part = Array.make m 0. in
    Polyphony.fill t.poly part;
    Array.blit part 0 t.mono !at m;
    at := !at + m
  done;
  Array.iteri (fun i x -> t.mono.(i) <- gain *. p.volume *. x) t.mono;
  hp_process t.hp p.hpf t.mono;
  if p.chorus > 0 then chorus_process t.chorus p.chorus t.mono out
  else begin
    Array.blit t.mono 0 out.left 0 n;
    Array.blit t.mono 0 out.right 0 n
  end;
  Array.iter
    (fun x ->
      t.ring.(t.at) <- x;
      t.at <- (t.at + 1) mod 2048)
    out.left

let instrument (t : t) : Instrument.t =
  {
    note_on = (fun key velocity -> Polyphony.press t.poly key (voice (fun () -> t.patch) t.shared key velocity));
    note_off = (fun key -> Polyphony.release t.poly key);
    set =
      (fun name x -> Option.iter (fun (k : knob) -> t.patch <- k.put t.patch x) (List.find_opt (fun (k : knob) -> k.name = name) knobs));
    fill = fill t;
  }
