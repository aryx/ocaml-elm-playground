(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Reverb.mli *)

type kind = Schroeder | Freeverb | Plate

let kinds = [ Schroeder; Freeverb; Plate ]
let name = function Schroeder -> "schroeder" | Freeverb -> "freeverb" | Plate -> "plate"

type settings = { kind : kind; seconds : float; damping : float; mix : float }

let rate = float_of_int Signal.rate

(*****************************************************************************)
(* Delay lines, combs, all-passes *)
(*****************************************************************************)

(* the last [Array.length samples] samples written, [at] the next *)
type line = { samples : float array; mutable at : int }

let line (n : int) : line = { samples = Array.make (max 2 n) 0.; at = 0 }

(* [k] samples ago, 1 the last one written *)
let read (l : line) (k : int) : float =
  let n = Array.length l.samples in
  l.samples.((l.at - k + n) mod n)

(* the same between two samples, linear *)
let read_between (l : line) (k : float) : float =
  let whole = Float.to_int k in
  let frac = k -. float_of_int whole in
  ((1. -. frac) *. read l whole) +. (frac *. read l (whole + 1))

let write (l : line) (x : float) : unit =
  l.samples.(l.at) <- x;
  l.at <- (l.at + 1) mod Array.length l.samples

(* the all-pass, (z^-D - g) / (1 - g z^-D): w = x + g z, y = z - g w,
 * w written *)
let all_pass (l : line) (d : int) (g : float) (x : float) : float =
  let z = read l d in
  let w = x +. (g *. z) in
  write l w;
  z -. (g *. w)

(* the comb's feedback for a decay of 60 dB in [seconds]: a comb of
 * [d] samples loses 60 dB after seconds / (d / rate) trips *)
let feedback_for (d : int) (seconds : float) : float = 10. ** (-3. *. float_of_int d /. rate /. Float.max 0.05 seconds)

(*****************************************************************************)
(* Schroeder's *)
(*****************************************************************************)

type schroeder = { combs : line array; passes : line array }

let schroeder_combs = Array.map (fun ms -> Signal.samples (ms /. 1000.)) [| 29.7; 37.1; 41.1; 43.7 |]
let schroeder_passes = [| Signal.samples 0.005; Signal.samples 0.0017 |]

let schroeder () : schroeder =
  { combs = Array.map (fun d -> line (d + 1)) schroeder_combs; passes = Array.map (fun d -> line (d + 1)) schroeder_passes }

let schroeder_sample (r : schroeder) (seconds : float) (x : float) : float =
  let sum = ref 0. in
  Array.iteri
    (fun i l ->
      let d = schroeder_combs.(i) in
      let y = x +. (feedback_for d seconds *. read l d) in
      write l y;
      sum := !sum +. y)
    r.combs;
  let y = ref (0.25 *. !sum) in
  Array.iteri (fun i l -> y := all_pass l schroeder_passes.(i) 0.7 !y) r.passes;
  !y

(*****************************************************************************)
(* Freeverb *)
(*****************************************************************************)

(* Jezar's numbers, in samples at 44,100; the right channel's all 23
 * longer *)
let freeverb_combs = [| 1116; 1188; 1277; 1356; 1422; 1491; 1557; 1617 |]
let freeverb_passes = [| 556; 441; 341; 225 |]
let spread = 23

type channel = { fcombs : line array; stores : float array (* each comb's low-pass *); fpasses : line array }
type freeverb = { fleft : channel; fright : channel }

let channel (extra : int) : channel =
  {
    fcombs = Array.map (fun d -> line (d + extra + 1)) freeverb_combs;
    stores = Array.make 8 0.;
    fpasses = Array.map (fun d -> line (d + extra + 1)) freeverb_passes;
  }

let freeverb () : freeverb = { fleft = channel 0; fright = channel spread }

(* Freeverb's all-pass, as Jezar wrote it: not quite one (its
 * feed-forward is -1 where a true all-pass has -g), kept *)
let freeverb_pass (l : line) (d : int) (x : float) : float =
  let z = read l d in
  write l (x +. (0.5 *. z));
  z -. x

let freeverb_sample (c : channel) (extra : int) (seconds : float) (damp : float) (x : float) : float =
  let sum = ref 0. in
  Array.iteri
    (fun i l ->
      let d = freeverb_combs.(i) + extra in
      let out = read l d in
      c.stores.(i) <- (out *. (1. -. damp)) +. (c.stores.(i) *. damp);
      write l (x +. (c.stores.(i) *. feedback_for d seconds));
      sum := !sum +. out)
    c.fcombs;
  let y = ref !sum in
  Array.iteri (fun i l -> y := freeverb_pass l (freeverb_passes.(i) + extra) !y) c.fpasses;
  !y

(*****************************************************************************)
(* Dattorro's plate *)
(*****************************************************************************)

(* the paper's lengths are at 29,761 Hz *)
let scale (d : int) : int = Float.to_int (Float.round (float_of_int d *. rate /. 29761.))

(* the input's four diffusers *)
let diffusers = [| (scale 142, 0.75); (scale 107, 0.75); (scale 379, 0.625); (scale 277, 0.625) |]

(* a half of the tank: a modulated all-pass, a delay, the damping, an
 * all-pass, a delay *)
type half = {
  modulated : line;
  mod_length : int;
  first : line;
  first_length : int;
  mutable damped : float;
  pass : line;
  pass_length : int;
  second : line;
  second_length : int;
  phase : float; (* the modulation's, 0 or a quarter turn *)
}

(* room for the modulation's excursion, in samples *)
let excursion_room = 64

let half (m : int) (d1 : int) (p : int) (d2 : int) (phase : float) : half =
  let m = scale m and d1 = scale d1 and p = scale p and d2 = scale d2 in
  {
    modulated = line (m + excursion_room);
    mod_length = m;
    first = line (d1 + 1);
    first_length = d1;
    damped = 0.;
    pass = line (p + 1);
    pass_length = p;
    second = line (d2 + 1);
    second_length = d2;
    phase;
  }

type plate = { mutable bandwidth : float; inputs : line array; left : half; right : half; mutable time : float }

let plate () : plate =
  {
    bandwidth = 0.;
    inputs = Array.map (fun (d, _) -> line (d + 1)) diffusers;
    left = half 672 4453 1800 3720 0.;
    right = half 908 4217 2656 3163 (Float.pi /. 2.);
    time = 0.;
  }

(* the loop's length, the four delays and four all-passes, in seconds:
 * a sound goes round it through [decay] four times *)
let loop_seconds = float_of_int (672 + 4453 + 1800 + 3720 + 908 + 4217 + 2656 + 3163) /. 29761.
let excursion = float_of_int (scale 16)

(* one half's sample: its input, the other half's end fed back in *)
let tank (h : half) (time : float) (decay : float) (damping : float) (x : float) : unit =
  (* the modulated all-pass, decay diffusion 1 (-0.7), its length
   * wobbling by 16 samples at 1 Hz *)
  let d = float_of_int h.mod_length +. (excursion *. sin ((2. *. Float.pi *. time) +. h.phase)) in
  let z = read_between h.modulated d in
  let w = x +. (-0.7 *. z) in
  write h.modulated w;
  let y = z -. (-0.7 *. w) in
  let delayed = read h.first h.first_length in
  write h.first y;
  h.damped <- ((1. -. damping) *. delayed) +. (damping *. h.damped);
  write h.second (all_pass h.pass h.pass_length 0.5 (decay *. h.damped))

let plate_sample (p : plate) (seconds : float) (damping : float) (x : float) : float * float =
  let decay = Float.min 0.99 (10. ** (-3. *. loop_seconds /. (4. *. Float.max 0.05 seconds))) in
  (* the input: band-limited (bandwidth 0.9995), then diffused *)
  p.bandwidth <- (0.9995 *. x) +. (0.0005 *. p.bandwidth);
  let v = ref p.bandwidth in
  Array.iteri (fun i (d, g) -> v := all_pass p.inputs.(i) d g !v) diffusers;
  let l = p.left and r = p.right in
  let left_end = read l.second l.second_length and right_end = read r.second r.second_length in
  tank l p.time decay damping (!v +. (decay *. right_end));
  tank r p.time decay damping (!v +. (decay *. left_end));
  p.time <- p.time +. (1. /. rate);
  if p.time > 1. then p.time <- p.time -. 1.;
  (* the output: seven taps each side, mostly from the other half *)
  let at (line : line) k = read line (scale k) in
  let yl =
    at r.first 266 +. at r.first 2974 -. at r.pass 1913 +. at r.second 1996 -. at l.first 1990 -. at l.pass 187 -. at l.second 1066
  and yr =
    at l.first 353 +. at l.first 3627 -. at l.pass 1228 +. at l.second 2673 -. at r.first 2111 -. at r.pass 335 -. at r.second 121
  in
  (0.6 *. yl, 0.6 *. yr)

(*****************************************************************************)
(* The effect *)
(*****************************************************************************)

type t = { schroeder : schroeder; freeverb : freeverb; plate : plate; mutable last_mix : float (* nan: none yet *) }

let create () : t = { schroeder = schroeder (); freeverb = freeverb (); plate = plate (); last_mix = Float.nan }

let process (t : t) (s : settings) (out : Signal.stereo) : unit =
  let n = Array.length out.left and from_mix = if Float.is_nan t.last_mix then s.mix else t.last_mix in
  Array.iteri
    (fun i xl ->
      let xr = out.right.(i) in
      let x = (xl +. xr) /. 2. in
      let wl, wr =
        match s.kind with
        | Schroeder ->
            let y = schroeder_sample t.schroeder s.seconds x in
            (y, y)
        | Freeverb ->
            (* Jezar's fixed gain (0.015) into the eight combs, and his
             * wet scale (3) out *)
            let damp = 0.4 *. s.damping and x = 0.015 *. (xl +. xr) in
            (3. *. freeverb_sample t.freeverb.fleft 0 s.seconds damp x, 3. *. freeverb_sample t.freeverb.fright spread s.seconds damp x)
        | Plate -> plate_sample t.plate s.seconds (0.7 *. s.damping) x
      in
      let mix = Effect.ramp from_mix s.mix i n in
      out.left.(i) <- xl +. (mix *. wl);
      out.right.(i) <- xr +. (mix *. wr))
    out.left;
  t.last_mix <- s.mix

let knobs : Effect.knob list =
  [
    { name = "kind"; control = Selector (List.map name kinds); initial = 2. };
    { name = "time"; control = Knob (0.3, 8.); initial = 2. };
    { name = "damping"; control = Knob (0., 1.); initial = 0.3 };
    { name = "mix"; control = Knob (0., 1.); initial = 0.25 };
  ]

let effect () : Effect.t =
  let t = create () and s = ref { kind = Plate; seconds = 0.; damping = 0.; mix = 0. } in
  let set (knob : string) (x : float) =
    match knob with
    | "kind" -> s := { !s with kind = List.nth kinds (max 0 (min (List.length kinds - 1) (Control.index x))) }
    | "time" -> s := { !s with seconds = x }
    | "damping" -> s := { !s with damping = x }
    | "mix" -> s := { !s with mix = x }
    | _ -> ()
  in
  List.iter (fun (k : Effect.knob) -> set k.name k.initial) knobs;
  { name = "reverb"; knobs; set; process = (fun out -> process t !s out); meters = (fun () -> []) }
