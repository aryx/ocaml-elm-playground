(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Op1_engine.mli *)

type t = {
  name : string;
  kind : string;
  encoders : string array;
  start : float array -> frequency:float -> velocity:float -> Signal.t -> unit;
}

let rate = float_of_int Signal.rate
let two_pi = 2. *. Float.pi
let cents (c : float) : float = Float.pow 2. (c /. 1200.)

(*****************************************************************************)
(* FM *)
(*****************************************************************************)

let topologies = [ "stack"; "pairs"; "fan"; "organ" ]

(* the four topologies as the DX7's algorithms, and which four of their
 * six operators: the stack is algorithm 1's 6-5-4-3, the pairs its 4-3
 * and 2-1, the fan algorithm 13's 6, 5, 4 onto 3, the organ 32's *)
let topology (k : int) : Fm_algorithm.t * int list =
  match k with
  | 0 -> (Fm_algorithm.get 1, [ 3; 4; 5; 6 ])
  | 1 -> (Fm_algorithm.get 1, [ 1; 2; 3; 4 ])
  | 2 -> (Fm_algorithm.get 13, [ 3; 4; 5; 6 ])
  | _ -> (Fm_algorithm.get 32, [ 3; 4; 5; 6 ])

(* ours: the freq encoder the modulators' ratio, 0.5 to 7.5 by halves;
 * the amount their level, up to half a cycle (pi radians); the detune
 * up to 15 cents between the operators *)
let fm : t =
  {
    name = "FM";
    kind = "four operator FM synthesis";
    encoders = [| "FM amount"; "freq"; "topology"; "detune" |];
    start =
      (fun p ~frequency ~velocity ->
        let alg, ops = topology (min 3 (Float.to_int (p.(2) *. 4.))) in
        let ratio = 0.5 +. (Float.round (p.(1) *. 14.) /. 2.) in
        let carriers = List.filter (fun c -> List.mem c ops) alg.carriers in
        let increments = Array.make 6 0. and amplitudes = Array.make 6 0. in
        List.iteri
          (fun k op ->
            let carrier = List.mem op carriers in
            let f = frequency *. (if carrier then 1. else ratio) *. cents (15. *. p.(3) *. float_of_int k) in
            increments.(op - 1) <- f /. rate;
            amplitudes.(op - 1) <- (if carrier then 1. /. float_of_int (List.length carriers) else 0.5 *. p.(0)))
          ops;
        let state = Fm_algorithm.create () in
        fun out -> Array.iteri (fun i _ -> out.(i) <- velocity *. Fm_algorithm.sample alg state ~feedback:0 ~increments ~amplitudes) out);
  }

(*****************************************************************************)
(* Cluster *)
(*****************************************************************************)

(* ours: waves 0 a sine alone, else that many sawtooths, the spread up
 * to 40 cents either side; the "wave envelope" a low-pass closing from
 * bright to dark over up to 2 s; the "unitor" the middle wave against
 * the others *)
let cluster : t =
  {
    name = "cluster";
    kind = "multi layered oscillator cluster";
    encoders = [| "number of waves (0-6)"; "wave envelope"; "spread"; "unitor" |];
    start =
      (fun p ~frequency ~velocity ->
        let n = Float.to_int (Float.round (p.(0) *. 6.)) in
        let oscs = Array.init (max 1 n) (fun _ -> Vco.create ()) in
        let offsets = Array.init (max 1 n) (fun k -> if n <= 1 then 0. else (-1. +. (2. *. float_of_int k /. float_of_int (n - 1))) *. 40. *. p.(2)) in
        let lp = Svf.create () and age = ref 0 and buf = ref [||] and freq = ref [||] in
        fun out ->
          let m = Array.length out in
          if Array.length !buf <> m then begin
            buf := Array.make m 0.;
            freq := Array.make m 0.
          end;
          Array.fill out 0 m 0.;
          Array.iteri
            (fun k osc ->
              Array.fill !freq 0 m (frequency *. cents offsets.(k));
              Vco.fill osc (if n = 0 then Sine else Sawtooth) ~frequency:!freq !buf;
              let middle = Float.abs offsets.(k) < 1e-9 in
              let g = if n <= 1 || middle then 0.4 +. (0.6 *. p.(3)) else 1. -. (0.6 *. p.(3)) in
              Array.iteri (fun i x -> out.(i) <- out.(i) +. (g *. x /. sqrt (float_of_int (max 1 n)))) !buf)
            oscs;
          let t = float_of_int !age /. rate in
          let closing = if p.(1) <= 0. then 1. else exp (-.t /. (2. *. p.(1))) in
          Svf.process lp Zero_delay Low_pass ~cutoff:(Array.make m (frequency *. (1. +. (40. *. closing)))) ~q:0.707 out;
          Array.iteri (fun i x -> out.(i) <- velocity *. x) out;
          age := !age + m);
  }

(*****************************************************************************)
(* String *)
(*****************************************************************************)

(* a live Karplus-Strong string: a line a period long, read between
 * samples, its loop the average of two neighbours (the low-pass)
 * times a loss; the impulse written in over its first milliseconds.
 * Ours: the tension the loss (0.990 to 0.9999 a pass) and the
 * averaging's brightness; the impulse decay 1 to 30 ms; the impulse
 * type noise (a pluck) at 0 to a sine burst (a hammer) at 1; the detune
 * the second string's, up to 20 cents *)
let waveguide ~(frequency : float) ~(tension : float) ~(excite : int -> float) ~(excite_len : int) : unit -> float =
  let period = rate /. frequency in
  let size = Float.to_int period + 4 in
  let line = Array.make size 0. and at = ref 0 and n = ref 0 in
  let loss = 0.990 +. (0.0099 *. tension) and bright = 0.5 +. (0.45 *. tension) in
  fun () ->
    let back = period -. 0.5 in
    let read k = line.((!at - k + (2 * size)) mod size) in
    let whole = Float.to_int back in
    let frac = back -. float_of_int whole in
    let delayed = ((1. -. frac) *. read whole) +. (frac *. read (whole + 1)) in
    let older = ((1. -. frac) *. read (whole + 1)) +. (frac *. read (whole + 2)) in
    let y = (loss *. ((bright *. delayed) +. ((1. -. bright) *. older))) +. if !n < excite_len then excite !n else 0. in
    line.(!at) <- y;
    at := (!at + 1) mod size;
    incr n;
    y

let string : t =
  {
    name = "string";
    kind = "waveguide string model";
    encoders = [| "tension"; "impulse decay"; "detune"; "impulse type" |];
    start =
      (fun p ~frequency ~velocity ->
        let len = Signal.samples (0.001 +. (0.029 *. p.(1))) in
        let random = ref 7 in
        let excite k =
          let fade = 1. -. (float_of_int k /. float_of_int len) in
          random := Noise.lcg !random;
          fade *. (((1. -. p.(3)) *. Noise.uniform !random) +. (p.(3) *. sin (two_pi *. frequency *. float_of_int k /. rate)))
        in
        let a = waveguide ~frequency ~tension:p.(0) ~excite ~excite_len:len in
        let b = waveguide ~frequency:(frequency *. cents (20. *. p.(2))) ~tension:p.(0) ~excite ~excite_len:len in
        (* the two strings' sum, the impulse's energy built up in their
         * loops: 0.35 keeps a note's peak near the others' *)
        fun out -> Array.iteri (fun i _ -> out.(i) <- velocity *. 0.35 *. (a () +. b ())) out);
  }

(*****************************************************************************)
(* Pulse *)
(*****************************************************************************)

(* ours: the filter a low-pass from 100 Hz to 20 kHz; the amplitude
 * the drive into a tanh; the second pulse a pulse an octave up, its
 * width 5 to 50%, mixed in; the mod the widths moved by a 5 Hz sine *)
let pulse : t =
  {
    name = "pulse";
    kind = "dual pulsetrain oscillator";
    encoders = [| "filter"; "amplitude"; "second pulse"; "mod" |];
    start =
      (fun p ~frequency ~velocity ->
        let a = Vco.create () and b = Vco.create () and lp = Svf.create () and lfo = ref 0. in
        let bufs = ref ([||], [||], [||], [||]) in
        fun out ->
          let m = Array.length out in
          let fa, fb, wa, x = !bufs in
          let fa, fb, wa, x = if Array.length fa = m then (fa, fb, wa, x) else (Array.make m 0., Array.make m 0., Array.make m 0., Array.make m 0.) in
          bufs := (fa, fb, wa, x);
          Array.fill fa 0 m frequency;
          Array.fill fb 0 m (2. *. frequency);
          for i = 0 to m - 1 do
            lfo := Float.rem (!lfo +. (5. /. rate)) 1.;
            wa.(i) <- 0.5 -. (0.4 *. p.(3) *. (0.5 +. (0.5 *. sin (two_pi *. !lfo))))
          done;
          Vco.fill ~width:wa a Pulse ~frequency:fa out;
          Vco.fill ~width:(Array.map (fun w -> Float.max 0.05 (w *. (0.1 +. (0.9 *. (1. -. p.(2)))))) wa) b Pulse ~frequency:fb x;
          Array.iteri (fun i y -> out.(i) <- out.(i) +. (p.(2) *. y)) x;
          Svf.process lp Zero_delay Low_pass ~cutoff:(Array.make m (100. *. Float.pow 200. p.(0))) ~q:0.707 out;
          let drive = 1. +. (6. *. p.(1)) in
          Array.iteri (fun i y -> out.(i) <- velocity *. tanh (drive *. y) /. tanh drive) out);
  }

(*****************************************************************************)
(* Phase *)
(*****************************************************************************)

(* Casio's phase distortion: the phase read fast to a knee then slow,
 * the knee at 0.5 (no distortion) sliding to 0.05 with the amount: a
 * cosine read so turns into a sawtooth-like wave *)
let phase_distortion ~(amount : float) (p : float) : float =
  let d = 0.5 -. (0.45 *. amount) in
  if p < d then 0.5 *. p /. d else 0.5 +. (0.5 *. (p -. d) /. (1. -. d))

(* ours: the tilt the blend towards a pulse-like bending (fast, flat,
 * fast); the shift a second reading, its phase shifted, added (a comb);
 * the filter a low-pass *)
let phase : t =
  {
    name = "phase";
    kind = "phase distortion";
    encoders = [| "phase shift"; "distortion amount"; "phase filter"; "phase tilt" |];
    start =
      (fun p ~frequency ~velocity ->
        let ph = ref 0. and lp = Svf.create () in
        let read x =
          let saw = phase_distortion ~amount:p.(1) x in
          let square = if x < 0.5 then phase_distortion ~amount:p.(1) (2. *. x) /. 2. else 0.5 +. (phase_distortion ~amount:p.(1) ((2. *. x) -. 1.) /. 2.) in
          -.cos (two_pi *. (((1. -. p.(3)) *. saw) +. (p.(3) *. square)))
        in
        fun out ->
          Array.iteri
            (fun i _ ->
              let x = read !ph and y = read (Float.rem (!ph +. (0.5 *. p.(0))) 1.) in
              out.(i) <- velocity *. (if p.(0) > 0. then 0.5 *. (x +. y) else x);
              ph := Float.rem (!ph +. (frequency /. rate)) 1.)
            out;
          if p.(2) > 0. then Svf.process lp Zero_delay Low_pass ~cutoff:(Array.make (Array.length out) (20000. *. Float.pow 0.005 p.(2))) ~q:0.707 out);
  }

(*****************************************************************************)
(* Digital *)
(*****************************************************************************)

(* ours: the octave -2 to +2; the wave shaper a sine folding, up to 9
 * times over; the detune and ring mod a second sine up to half again
 * the frequency, multiplying; the digitalness 16 bits down to 2, and
 * each sample held up to 16 times *)
let digital : t =
  {
    name = "digital";
    kind = "true digital synthesis";
    encoders = [| "wave shaper"; "octave"; "detune and ring mod"; "digitalness" |];
    start =
      (fun p ~frequency ~velocity ->
        let f = frequency *. Float.pow 2. (Float.round (p.(1) *. 4.) -. 2.) in
        let a = ref 0. and b = ref 0. and held = ref 0. and count = ref 0 in
        let bits = 16. -. (14. *. p.(3)) and hold = 1 + Float.to_int (Float.round (15. *. p.(3))) in
        let levels = Float.pow 2. (bits -. 1.) in
        fun out ->
          Array.iteri
            (fun i _ ->
              let x = sin (two_pi *. !a) in
              let folded = sin (Float.pi /. 2. *. (1. +. (8. *. p.(0))) *. x) in
              let ring = sin (two_pi *. !b) in
              let y = ((1. -. p.(2)) *. folded) +. (p.(2) *. folded *. ring) in
              if !count mod hold = 0 then held := Float.round (y *. levels) /. levels;
              incr count;
              out.(i) <- velocity *. !held;
              a := Float.rem (!a +. (f /. rate)) 1.;
              b := Float.rem (!b +. (f *. (1. +. (0.5 *. p.(2))) /. rate)) 1.)
            out);
  }

let all = [ fm; cluster; string; pulse; phase; digital ]
