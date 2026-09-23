(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Dynamics.mli *)

type mode = Compressor | Limiter | Gate

let modes = [ Compressor; Limiter; Gate ]
let name = function Compressor -> "compressor" | Limiter -> "limiter" | Gate -> "gate"

type detector = Peak | Rms

type settings = {
  mode : mode;
  threshold : float;
  ratio : float;
  knee : float;
  attack : float;
  release : float;
  makeup : float;
  detector : detector;
  lookahead : float;
}

let compressor =
  {
    mode = Compressor;
    threshold = -20.;
    ratio = 4.;
    knee = 6.;
    attack = 0.005;
    release = 0.1;
    makeup = 0.;
    detector = Rms;
    lookahead = 0.;
  }

let limiter =
  { compressor with mode = Limiter; threshold = -1.; knee = 0.; attack = 0.0005; release = 0.05; detector = Peak; lookahead = 0.005 }

let gate = { compressor with mode = Gate; threshold = -40.; ratio = 10.; knee = 0.; attack = 0.001; release = 0.1; detector = Peak }
let rate = float_of_int Signal.rate

let curve (s : settings) (x : float) : float =
  let t = s.threshold and w = s.knee in
  match s.mode with
  | Gate -> if x >= t then x else t +. ((x -. t) *. s.ratio)
  | Compressor | Limiter ->
      let r = if s.mode = Limiter then Float.infinity else Float.max 1. s.ratio in
      let over = x -. t in
      if 2. *. over < -.w then x
      else if 2. *. Float.abs over <= w && w > 0. then x +. ((1. /. r) -. 1.) *. ((over +. (w /. 2.)) ** 2.) /. (2. *. w)
      else t +. (over /. r)

(* the look-ahead's line, per side: the last [Array.length] samples *)
type line = { samples : float array; mutable at : int }

let longest_lookahead = Signal.samples 0.02

type t = {
  mutable reduction : float; (* dB, smoothed *)
  mutable mean_square : float; (* the RMS detector's *)
  left : line;
  right : line;
  heard : line; (* the detector's input, for the look-ahead window's loudest *)
  mutable last_makeup : float; (* nan: none yet *)
}

let create () : t =
  let line () = { samples = Array.make (longest_lookahead + 1) 0.; at = 0 } in
  { reduction = 0.; mean_square = 0.; left = line (); right = line (); heard = line (); last_makeup = Float.nan }

let reduction (t : t) : float = t.reduction

(* [x] in, the sample [d] ago out *)
let delayed (l : line) (d : int) (x : float) : float =
  let n = Array.length l.samples in
  l.samples.(l.at) <- x;
  let y = l.samples.((l.at - d + n) mod n) in
  l.at <- (l.at + 1) mod n;
  y

(* a one-pole's coefficient for a time constant: 63% of the way in
 * [seconds] *)
let pole (seconds : float) : float = exp (-1. /. (Float.max 1e-5 seconds *. rate))
let rms_pole = pole 0.01

let process ?(key : Signal.stereo option) (t : t) (s : settings) (out : Signal.stereo) : unit =
  let n = Array.length out.left in
  let attack = pole s.attack and release = pole s.release in
  let d = min longest_lookahead (Signal.samples s.lookahead) in
  let from_makeup = if Float.is_nan t.last_makeup then s.makeup else t.last_makeup in
  for i = 0 to n - 1 do
    let heard = match key with Some k -> k | None -> out in
    let v = Float.max (Float.abs heard.left.(i)) (Float.abs heard.right.(i)) in
    (* the limiter hears the loudest of its look-ahead window: the
     * samples not out yet, one of which is the next out *)
    let v =
      if s.mode = Limiter && d > 0 then begin
        ignore (delayed t.heard d v);
        let n = Array.length t.heard.samples and loudest = ref 0. in
        for k = 1 to d + 1 do
          loudest := Float.max !loudest t.heard.samples.((t.heard.at - k + n) mod n)
        done;
        !loudest
      end
      else v
    in
    (* the level, in dB *)
    let level =
      match s.detector with
      | Peak -> 20. *. log10 (Float.max 1e-6 v)
      | Rms ->
          t.mean_square <- (rms_pole *. t.mean_square) +. ((1. -. rms_pole) *. v *. v);
          10. *. log10 (Float.max 1e-12 t.mean_square)
    in
    (* the curve's reduction, smoothed: fast up, slow down; the limiter
     * down at once (its window saw the peak coming: no overshoot) *)
    let wanted = level -. curve s level in
    let a = if wanted > t.reduction then if s.mode = Limiter then 0. else attack else release in
    t.reduction <- (a *. t.reduction) +. ((1. -. a) *. wanted);
    let gain = Mix.of_decibels (Effect.ramp from_makeup s.makeup i n -. t.reduction) in
    out.left.(i) <- gain *. delayed t.left d out.left.(i);
    out.right.(i) <- gain *. delayed t.right d out.right.(i)
  done;
  t.last_makeup <- s.makeup

let knobs : Effect.knob list =
  [
    { name = "mode"; control = Selector (List.map name modes); initial = 0. };
    { name = "threshold"; control = Knob (-60., 0.); initial = -20. };
    { name = "ratio"; control = Knob (1., 20.); initial = 4. };
    { name = "attack"; control = Knob (0.0005, 0.1); initial = 0.005 };
    { name = "release"; control = Knob (0.01, 1.); initial = 0.1 };
    { name = "makeup"; control = Knob (0., 24.); initial = 0. };
  ]

let effect () : Effect.t =
  let t = create () and s = ref compressor in
  let set (knob : string) (x : float) =
    match knob with
    | "mode" ->
        let mode = List.nth modes (max 0 (min (List.length modes - 1) (Control.index x))) in
        (* the mode's own knee, detector and look-ahead; the knobs kept *)
        let m = match mode with Compressor -> compressor | Limiter -> limiter | Gate -> gate in
        s := { !s with mode; knee = m.knee; detector = m.detector; lookahead = m.lookahead }
    | "threshold" -> s := { !s with threshold = x }
    | "ratio" -> s := { !s with ratio = x }
    | "attack" -> s := { !s with attack = x }
    | "release" -> s := { !s with release = x }
    | "makeup" -> s := { !s with makeup = x }
    | _ -> ()
  in
  List.iter (fun (k : Effect.knob) -> set k.name k.initial) knobs;
  { name = "dynamics"; knobs; set; process = (fun out -> process t !s out); meters = (fun () -> [ ("reduction", t.reduction) ]) }
