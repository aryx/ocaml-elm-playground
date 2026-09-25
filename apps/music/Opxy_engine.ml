(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Opxy_engine.mli *)

let rate = float_of_int Signal.rate
let two_pi = 2. *. Float.pi

(*****************************************************************************)
(* Wavetable *)
(*****************************************************************************)

let size = 2048
let tables = [ "saw growing"; "formant"; "pulse narrowing"; "square to saw" ]

(* the harmonics' amplitudes (1 to 32) of table [t]'s wave [k] (0 to 7) *)
let amplitudes (t : int) (k : int) : float array =
  Array.init 33 (fun n ->
      let f = float_of_int n in
      if n = 0 then 0.
      else
        match t with
        | 0 -> if n <= 1 + (4 * k) then 1. /. f else 0.
        | 1 -> exp (-.Float.pow ((f -. (2. +. (3. *. float_of_int k))) /. 1.5) 2.)
        | 2 ->
            let w = 0.5 -. (0.06 *. float_of_int k) in
            sin (Float.pi *. f *. w) /. f
        | _ -> if n mod 2 = 1 then 1. /. f else float_of_int k /. 7. /. f)

(* a period from its harmonics, scaled to one loudness (its rms) *)
let period (amps : float array) : Signal.t =
  let power = Array.fold_left (fun a x -> a +. (x *. x /. 2.)) 0. amps in
  let scale = if power > 0. then 0.35 /. sqrt power else 0. in
  let out = Array.make size 0. in
  Array.iteri
    (fun n a ->
      if a <> 0. then
        for i = 0 to size - 1 do
          out.(i) <- out.(i) +. (scale *. a *. sin (two_pi *. float_of_int (n * i) /. float_of_int size))
        done)
    amps;
  out

(* the tables, made when first played *)
let made : Signal.t array array Lazy.t = lazy (Array.init 4 (fun t -> Array.init 8 (fun k -> period (amplitudes t k))))

(* a period read between two entries *)
let read (w : Signal.t) (ph : float) : float =
  let x = ph *. float_of_int size in
  let i = Float.to_int x in
  let f = x -. float_of_int i in
  ((1. -. f) *. w.(i mod size)) +. (f *. w.((i + 1) mod size))

let wavetable : Op1_engine.t =
  {
    name = "wavetable";
    kind = "waveforms arranged one after the other in a look up table";
    encoders = [| "table"; "position"; "warp"; "drift" |];
    start =
      (fun p ~frequency ~velocity ->
        let tables = Lazy.force made in
        let a = ref 0. and b = ref 0. in
        fun out ->
          let t = min 3 (Float.to_int (p.(0) *. 4.)) in
          (* the position: between two waves, a straight line -- their
           * spectra crossfaded *)
          let x = p.(1) *. 7. in
          let k = min 6 (Float.to_int x) in
          let mix = x -. float_of_int k in
          let warp = 1. +. (3. *. p.(2)) in
          let wave ph =
            let ph = Float.pow ph warp in
            ((1. -. mix) *. read tables.(t).(k) ph) +. (mix *. read tables.(t).(k + 1) ph)
          in
          Array.iteri
            (fun i _ ->
              let y = if p.(3) > 0. then 0.5 *. (wave !a +. wave !b) else wave !a in
              out.(i) <- velocity *. y;
              a := Float.rem (!a +. (frequency /. rate)) 1.;
              (* drift, ours: a second reader 0 to 3% off, inharmonic *)
              b := Float.rem (!b +. (frequency *. (1. +. (0.03 *. p.(3))) /. rate)) 1.)
            out);
  }

(*****************************************************************************)
(* Organ *)
(*****************************************************************************)

(* the drawbars' footages as ratios to 8': 16', 5 1/3', 8', 4', 2 2/3',
 * 2', 1 3/5', 1 1/3', 1' *)
let footages = [| 0.5; 1.5; 1.; 2.; 3.; 4.; 5.; 6.; 8. |]

(* ours: four registrations, the last a transistor organ's *)
let registrations = [ ("jazz", "888000000"); ("full", "888888888"); ("church", "808808008"); ("transistor", "808080000") ]

let organ : Op1_engine.t =
  {
    name = "organ";
    kind = "from transistor to church";
    encoders = [| "type"; "bass"; "tremolo amount"; "tremolo speed" |];
    start =
      (fun p ~frequency ~velocity ->
        let phases = Array.make 9 0. and trem = ref 0. in
        fun out ->
          let t = min 3 (Float.to_int (p.(0) *. 4.)) in
          let bars = snd (List.nth registrations t) in
          let level k = float_of_int (Char.code bars.[k] - Char.code '0') /. 8. in
          (* the 16' the bass knob's, whatever the registration *)
          let levels = Array.init 9 (fun k -> if k = 0 then Float.max (level 0) p.(1) else level k) in
          let total = Array.fold_left ( +. ) 0. levels in
          (* the transistor organ's squares; the others' sines *)
          let tone ph = if t = 3 then if ph < 0.5 then 0.6 else -0.6 else sin (two_pi *. ph) in
          Array.iteri
            (fun i _ ->
              let s = ref 0. in
              Array.iteri
                (fun k l ->
                  if l > 0. then s := !s +. (l *. tone phases.(k));
                  phases.(k) <- Float.rem (phases.(k) +. (frequency *. footages.(k) /. rate)) 1.)
                levels;
              let tremolo = 1. -. (0.5 *. p.(2) *. (1. +. sin (two_pi *. !trem))) in
              trem := Float.rem (!trem +. ((1. +. (9. *. p.(3))) /. rate)) 1.;
              (* divided by the drawbars' sum: never over 0.9 *)
              out.(i) <- 0.9 *. velocity *. tremolo *. !s /. Float.max 1. total)
            out);
  }

(*****************************************************************************)
(* Hardsync *)
(*****************************************************************************)

let hardsync : Op1_engine.t =
  {
    name = "hardsync";
    kind = "stabs, jabs and solid basses";
    encoders = [| "freq"; "sub"; "noise"; "lowcut" |];
    start =
      (fun p ~frequency ~velocity ->
        (* the second oscillator, a sawtooth at 1 to 8 times the first,
         * restarted when the first completes a period; the sub a sine an
         * octave down; the low cut a one-pole high-pass from 20 Hz to 1
         * kHz (ours) *)
        let a = ref 0. and b = ref 0. and sub = ref 0. and noise = ref 0x2468 and low = ref 0. in
        fun out ->
          let ratio = 1. +. (7. *. p.(0)) in
          let cut = 20. *. Float.pow 50. p.(3) in
          let k = 1. -. exp (-.two_pi *. cut /. rate) in
          Array.iteri
            (fun i _ ->
              noise := Noise.lcg !noise;
              let y = (2. *. !b -. 1.) +. (p.(1) *. sin (two_pi *. !sub)) +. (p.(2) *. Noise.uniform !noise) in
              low := !low +. (k *. (y -. !low));
              out.(i) <- 0.35 *. velocity *. (y -. !low);
              a := !a +. (frequency /. rate);
              b := !b +. (frequency *. ratio /. rate);
              sub := Float.rem (!sub +. (frequency /. 2. /. rate)) 1.;
              (* the sync: the first's period over, the second restarts *)
              if !a >= 1. then begin
                a := !a -. 1.;
                b := 0.
              end
              else b := Float.rem !b 1.)
            out);
  }

(*****************************************************************************)
(* Simple *)
(*****************************************************************************)

let simple : Op1_engine.t =
  {
    name = "simple";
    kind = "basic patches, great for leads and plucks";
    encoders = [| "shape"; "pw"; "noise"; "stereo" |];
    start =
      (fun p ~frequency ~velocity ->
        let a = ref 0. and b = ref 0. and noise = ref 0x1357 in
        let shape_at w ph =
          (* sine, triangle, saw, square: the shape between two of them *)
          let s = sin (two_pi *. ph) and tri = 1. -. (4. *. Float.abs (ph -. 0.5)) and saw = (2. *. ph) -. 1. in
          let sq = if ph < w then 1. else -1. in
          fun x ->
            if x < 1. then ((1. -. x) *. s) +. (x *. tri)
            else if x < 2. then ((2. -. x) *. tri) +. ((x -. 1.) *. saw)
            else ((3. -. x) *. saw) +. ((x -. 2.) *. sq)
        in
        fun out ->
          let x = 3. *. p.(0) and w = 0.5 +. (0.45 *. p.(1)) in
          let detune = Float.pow 2. (20. *. p.(3) /. 1200.) in
          Array.iteri
            (fun i _ ->
              noise := Noise.lcg !noise;
              let y = shape_at w !a x in
              let y = if p.(3) > 0. then 0.5 *. (y +. shape_at w !b x) else y in
              out.(i) <- 0.5 *. velocity *. (y +. (p.(2) *. Noise.uniform !noise));
              a := Float.rem (!a +. (frequency /. rate)) 1.;
              b := Float.rem (!b +. (frequency *. detune /. rate)) 1.)
            out);
  }

let all = [ wavetable; organ; hardsync; simple ]
