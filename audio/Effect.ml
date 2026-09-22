(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Effect.mli *)

type pitch =
  | Vibrato of { rate : float; depth : float }
  | Jump of { semitones : float; at : float }
  | Arpeggio of { semitones : float list; step : float }

let semitones (k : float) : float = 2. ** (k /. 12.)

let factor (p : pitch) (t : float) : float =
  match p with
  | Vibrato { rate; depth } -> semitones (depth *. sin (2. *. Float.pi *. rate *. t))
  | Jump { semitones = k; at } -> if t < at then 1. else semitones k
  | Arpeggio { semitones = []; _ } -> 1.
  | Arpeggio { semitones = l; step } -> semitones (List.nth l (int_of_float (t /. step) mod List.length l))

let tail ~(delay : float) ~(feedback : float) : float =
  if feedback <= 0. then delay else delay *. Float.ceil (log 0.001 /. log feedback)

(* a feedback comb of [d] samples (the input already padded with the
 * tail): y[n] = x[n] + g y[n - d] *)
let comb (d : int) (g : float) (x : Signal.t) : Signal.t =
  let line = Array.make d 0. and pos = ref 0 in
  Array.map
    (fun v ->
      let y = v +. (g *. line.(!pos)) in
      line.(!pos) <- y;
      pos := (!pos + 1) mod d;
      y)
    x

(* an all-pass: y[n] = -g x[n] + x[n - d] + g y[n - d] *)
let all_pass (d : int) (g : float) (x : Signal.t) : Signal.t =
  let xs = Array.make d 0. and ys = Array.make d 0. and pos = ref 0 in
  Array.map
    (fun v ->
      let y = (-.g *. v) +. xs.(!pos) +. (g *. ys.(!pos)) in
      xs.(!pos) <- v;
      ys.(!pos) <- y;
      pos := (!pos + 1) mod d;
      y)
    x

let reverb ~(seconds : float) ?(mix = 0.3) (s : Signal.t) : Signal.t =
  let x = Array.append s (Array.make (Signal.samples seconds) 0.) in
  let combs =
    List.map
      (fun ms ->
        let d = Signal.samples (ms /. 1000.) in
        comb d (10. ** (-3. *. (ms /. 1000.) /. seconds)) x)
      [ 29.7; 37.1; 41.1; 43.7 ]
  in
  let wet = Mix.gain 0.25 (Mix.add combs) |> all_pass (Signal.samples 0.005) 0.7 |> all_pass (Signal.samples 0.0017) 0.7 in
  Array.mapi (fun i w -> x.(i) +. (mix *. w)) wet

let echo ~(delay : float) ~(feedback : float) (s : Signal.t) : Signal.t =
  let d = max 1 (Signal.samples delay) in
  let n = Array.length s + Signal.samples (tail ~delay ~feedback) in
  (* the delay line: the last d outputs, the oldest at [pos] *)
  let line = Array.make d 0. and pos = ref 0 in
  Array.init n (fun i ->
      let x = if i < Array.length s then s.(i) else 0. in
      let y = x +. (feedback *. line.(!pos)) in
      line.(!pos) <- y;
      pos := (!pos + 1) mod d;
      y)
