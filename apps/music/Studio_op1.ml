(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Studio_op1.mli *)

(*****************************************************************************)
(* The sound *)
(*****************************************************************************)

let effects = [ "delay"; "spring"; "punch"; "nitro" ]

let effect_encoders = function
  | 0 -> [| "size"; "speed"; "feedback"; "mix" |]
  | 1 -> [| "tone"; "turns"; "damping"; "send" |]
  | 2 -> [| "frequency"; "punch"; "rounds"; "power" |]
  | _ -> [| "frequency"; "follow"; "resonance"; "frequency" |]

let lfos = [ "tremolo"; "value" ]
let lfo_encoders = function 0 -> [| "speed"; "pitch"; "volume"; "envelope" |] | _ -> [| "amount"; "speed"; "destination"; "parameter" |]
let envelope_encoders = [| "attack"; "decay"; "sustain"; "release" |]
let play_modes = [ "poly"; "mono" ]

type sound = {
  engine : int;
  engine_params : float array;
  envelope : float array;
  play_mode : int;
  effect : int;
  effect_params : float array;
  effect_on : bool;
  lfo : int;
  lfo_params : float array;
  lfo_on : bool;
  octave : int;
}

type patch = { sounds : sound array; current : int; levels : float array; volume : float }

(* ours: eight sounds, one or two per engine, as the OP-1 comes with *)
let sound ?(effect = 0) ?(effect_on = false) ?(effect_params = [| 0.5; 0.5; 0.4; 0.3 |]) ?(lfo = 0) ?(lfo_on = false)
    ?(lfo_params = [| 0.4; 0.2; 0.3; 0.5 |]) ?(octave = 0) ?(play_mode = 0) engine engine_params envelope : sound =
  { engine; engine_params; envelope; play_mode; effect; effect_params; effect_on; lfo; lfo_params; lfo_on; octave }

let initial : patch =
  {
    sounds =
      [|
        (* FM, two pairs, a bell *)
        sound 0 [| 0.5; 0.25; 0.4; 0.3 |] [| 0.; 0.45; 0.; 0.55 |] ~effect:1 ~effect_on:true;
        (* the cluster, a pad *)
        sound 1 [| 1.; 0.5; 0.5; 0.4 |] [| 0.55; 0.4; 0.8; 0.6 |] ~effect:1 ~effect_on:true ~lfo_on:true;
        (* the string, plucked *)
        sound 2 [| 0.7; 0.3; 0.3; 0.2 |] [| 0.; 0.5; 0.6; 0.4 |];
        (* pulse, a lead, mono *)
        sound 3 [| 0.6; 0.4; 0.3; 0.5 |] [| 0.05; 0.4; 0.7; 0.3 |] ~play_mode:1 ~effect:0 ~effect_on:true;
        (* phase, a bass an octave down *)
        sound 4 [| 0.; 0.7; 0.3; 0.2 |] [| 0.; 0.35; 0.5; 0.25 |] ~octave:(-1) ~effect:2 ~effect_on:true;
        (* digital, crunchy *)
        sound 5 [| 0.4; 0.5; 0.3; 0.6 |] [| 0.; 0.4; 0.5; 0.3 |];
        (* FM, the stack, bright *)
        sound 0 [| 0.4; 0.5; 0.; 0.2 |] [| 0.02; 0.5; 0.6; 0.4 |] ~effect:3 ~effect_on:true;
        (* the cluster, a supersaw lead through the delay *)
        sound 1 [| 0.85; 0.; 0.7; 0.6 |] [| 0.02; 0.4; 0.7; 0.4 |] ~effect:0 ~effect_on:true ~octave:1;
      |];
    current = 0;
    levels = [| 0.8; 0.8; 0.8; 0.8 |];
    volume = 0.7;
  }

let seconds (k : float) : float = 0.001 *. Float.pow 10000. k

(*****************************************************************************)
(* The voices *)
(*****************************************************************************)

let rate = float_of_int Signal.rate
let frequency (key : int) : float = 440. *. Float.pow 2. (float_of_int (key - 69) /. 12.)

(* a note: the engine's generator, its four values an array the studio
 * rewrites each block (the value LFO moving them), the envelope *)
let voice (s : sound) (params : float array) (key : int) (velocity : float) : Polyphony.voice =
  let engine = List.nth Op1_engine.all s.engine in
  let fill = engine.start params ~frequency:(frequency (key + (12 * s.octave))) ~velocity in
  let env = Envelope.start () and levels = ref [||] in
  Envelope.gate_on env;
  let adsr : Envelope.t = { attack = seconds s.envelope.(0); decay = seconds s.envelope.(1); sustain = s.envelope.(2); release = seconds s.envelope.(3) } in
  {
    release = (fun () -> Envelope.gate_off env);
    fill =
      (fun out ->
        let n = Array.length out in
        if Array.length !levels <> n then levels := Array.make n 0.;
        fill out;
        Envelope.fill Exponential adsr env !levels;
        Array.iteri (fun i x -> out.(i) <- x *. !levels.(i)) out);
    silent = (fun () -> Envelope.stage env = Idle);
  }

(*****************************************************************************)
(* The studio *)
(*****************************************************************************)

type t = {
  mutable patch : patch;
  mutable poly : Polyphony.t;
  mutable poly_mode : int;
  params : float array; (* the engine's four, modulated, shared by the notes *)
  tape : Tape.t;
  (* the effects' states *)
  delay : Delay.t;
  spring : Reverb.t;
  filters : Svf.t array; (* left, right; and the nitro's second pair *)
  (* the tremolo's vibrato: a short line read at a moving delay *)
  vibrato : Signal.t;
  mutable vib_at : int;
  mutable lfo_phase : float;
  mutable since_note : int; (* samples since the last key *)
  mutable mono : Signal.t;
  mutable live : Signal.stereo;
  ring : Signal.t;
  mutable at : int;
}

let tape_seconds = 30.

let create (patch : patch) : t =
  let tape = Tape.create ~seconds:tape_seconds ~tracks:4 () in
  Array.iteri (fun k l -> Tape.set_level tape k l) patch.levels;
  {
    patch;
    poly = Polyphony.create ();
    poly_mode = 0;
    params = Array.copy patch.sounds.(patch.current).engine_params;
    tape;
    delay = Delay.create ();
    spring = Reverb.create ();
    filters = Array.init 4 (fun _ -> Svf.create ());
    vibrato = Array.make 1024 0.;
    vib_at = 0;
    lfo_phase = 0.;
    since_note = 0;
    mono = [||];
    live = { left = [||]; right = [||] };
    ring = Array.make 2048 0.;
    at = 0;
  }

let patch (t : t) : patch = t.patch

let set_patch (t : t) (p : patch) : unit =
  Array.iteri (fun k l -> Tape.set_level t.tape k l) p.levels;
  t.patch <- p

let tape (t : t) : Tape.t = t.tape
let record (t : t) (k : int) : unit = Tape.record t.tape k

let sample_track (t : t) (k : int) : bool =
  let track = Tape.track t.tape k in
  let n = Array.length track in
  let rec first i = if i >= n then None else if Float.abs track.(i) > 0.01 then Some i else first (i + 1) in
  match first 0 with
  | None -> false
  | Some a ->
      let len = min (n - a) (Signal.samples 6.) in
      Op1_engine.set_sample { data = Array.sub track a len; root = 60 };
      true
let play (t : t) : unit = Tape.play t.tape
let stop (t : t) : unit = Tape.stop t.tape
let voices (t : t) : int = Polyphony.voices t.poly
let recent (t : t) : Signal.t = Array.init 2048 (fun i -> t.ring.((t.at + i) mod 2048))

(* the value LFO: one encoder of one module (0 the engine, 1 the
 * envelope, 2 the effect) moved by a sine, [amount] of its range *)
let modulated (s : sound) (lfo : float) : sound =
  if not (s.lfo_on && s.lfo = 1) then s
  else begin
    let p = s.lfo_params in
    let dest = min 2 (Float.to_int (p.(2) *. 3.)) and param = min 3 (Float.to_int (p.(3) *. 4.)) in
    let move a = let a = Array.copy a in a.(param) <- Float.max 0. (Float.min 1. (a.(param) +. (0.5 *. p.(0) *. lfo))); a in
    match dest with
    | 0 -> { s with engine_params = move s.engine_params }
    | 1 -> { s with envelope = move s.envelope }
    | _ -> { s with effect_params = move s.effect_params }
  end

(* the effect on the live sound, both sides *)
let effect (t : t) (s : sound) (out : Signal.stereo) : unit =
  let p = s.effect_params and n = Array.length out.left in
  let lowpass (a : int) cutoff q =
    Svf.process t.filters.(a) Zero_delay Low_pass ~cutoff:(Array.make n cutoff) ~q out.left;
    Svf.process t.filters.(a + 1) Zero_delay Low_pass ~cutoff:(Array.make n cutoff) ~q out.right
  in
  match s.effect with
  | 0 -> Delay.process t.delay { time = 0.05 +. (0.95 *. p.(0)); feedback = 0.9 *. p.(2); tone = 500. *. Float.pow 40. p.(1); ping_pong = false; mix = p.(3) } out
  | 1 ->
      Reverb.process t.spring { kind = Freeverb; seconds = 0.3 +. (4. *. p.(1)); damping = p.(2); mix = p.(3) } out;
      lowpass 0 (1000. *. Float.pow 20. p.(0)) 0.707
  | 2 ->
      lowpass 0 (100. *. Float.pow 200. p.(0)) (0.707 +. (6. *. p.(1)));
      let rounds = 1 + Float.to_int (Float.round (3. *. p.(2))) and drive = 1. +. (10. *. p.(3)) in
      let shape x = let y = ref x in for _ = 1 to rounds do y := tanh (drive *. !y) /. tanh drive done; !y in
      Array.iteri (fun i x -> out.left.(i) <- shape x) out.left;
      Array.iteri (fun i x -> out.right.(i) <- shape x) out.right
  | _ ->
      (* the first filter follows the note's attack, falling back over
       * half a second *)
      let follow = 1. +. (4. *. p.(1) *. exp (-.float_of_int t.since_note /. (0.5 *. rate))) in
      let q = 0.707 +. (5. *. p.(2)) in
      lowpass 0 (Float.min 20000. (100. *. Float.pow 200. p.(0) *. follow)) q;
      lowpass 2 (100. *. Float.pow 200. p.(3)) q

(* the tremolo: the pitch as a vibrato (a delay moving by up to 3 ms),
 * the volume, faded in or out with the note by its envelope encoder *)
let tremolo (t : t) (s : sound) (mono : Signal.t) : unit =
  let p = s.lfo_params in
  let hz = 0.2 *. Float.pow 100. p.(0) in
  let size = Array.length t.vibrato in
  Array.iteri
    (fun i x ->
      let age = float_of_int (t.since_note + i) /. rate in
      let fade = if p.(3) >= 0.5 then Float.min 1. (age /. (0.01 +. (4. *. (p.(3) -. 0.5)))) else Float.max 0. (1. -. (age /. (0.01 +. (4. *. (0.5 -. p.(3)))))) in
      let w = sin (2. *. Float.pi *. t.lfo_phase) in
      t.lfo_phase <- Float.rem (t.lfo_phase +. (hz /. rate)) 1.;
      t.vibrato.(t.vib_at) <- x;
      let d = rate *. (0.004 +. (0.003 *. p.(1) *. fade *. w)) in
      let whole = Float.to_int d in
      let frac = d -. float_of_int whole in
      let get k = t.vibrato.((t.vib_at - k + (2 * size)) mod size) in
      let y = if p.(1) > 0. then ((1. -. frac) *. get whole) +. (frac *. get (whole + 1)) else x in
      mono.(i) <- y *. (1. -. (p.(2) *. fade *. (1. +. w) /. 2.));
      t.vib_at <- (t.vib_at + 1) mod size)
    mono

(* ours: a note's level, the sounds near one another's *)
let gain = 0.5

let fill (t : t) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let p = t.patch in
  let base = p.sounds.(p.current) in
  if Array.length t.mono <> n then begin
    t.mono <- Array.make n 0.;
    t.live <- { left = Array.make n 0.; right = Array.make n 0. }
  end;
  (* the value LFO, once a block: the engine's values the notes read *)
  let lfo = sin (2. *. Float.pi *. t.lfo_phase) in
  if base.lfo_on && base.lfo = 1 then t.lfo_phase <- Float.rem (t.lfo_phase +. (0.1 *. Float.pow 100. base.lfo_params.(1) *. float_of_int n /. rate)) 1.;
  let s = modulated base lfo in
  Array.blit s.engine_params 0 t.params 0 4;
  Polyphony.fill t.poly t.mono;
  if s.lfo_on && s.lfo = 0 then tremolo t s t.mono;
  Array.iteri
    (fun i x ->
      t.live.left.(i) <- gain *. x;
      t.live.right.(i) <- gain *. x)
    t.mono;
  if s.effect_on then effect t s t.live;
  (* the tape: the live sound recorded on the armed track, the tracks
   * played back beside it *)
  let mono_live = Array.map2 (fun l r -> 0.5 *. (l +. r)) t.live.left t.live.right in
  let from_tape = Array.make n 0. in
  Tape.process t.tape ~input:mono_live from_tape;
  for i = 0 to n - 1 do
    out.left.(i) <- p.volume *. (t.live.left.(i) +. from_tape.(i));
    out.right.(i) <- p.volume *. (t.live.right.(i) +. from_tape.(i));
    t.ring.(t.at) <- out.left.(i);
    t.at <- (t.at + 1) mod 2048
  done;
  t.since_note <- t.since_note + n

let instrument (t : t) : Instrument.t =
  {
    note_on =
      (fun key velocity ->
        let s = t.patch.sounds.(t.patch.current) in
        if s.play_mode <> t.poly_mode then begin
          t.poly <- (if s.play_mode = 1 then Polyphony.create ~voices:1 () else Polyphony.create ());
          t.poly_mode <- s.play_mode
        end;
        t.since_note <- 0;
        Polyphony.press t.poly key (voice s t.params key velocity));
    note_off = (fun key -> Polyphony.release t.poly key);
    set = (fun _ _ -> ());
    fill = fill t;
  }
