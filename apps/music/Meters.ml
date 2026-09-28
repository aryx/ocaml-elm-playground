(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground
open Basics (* float arithmetics *)

let segment (c : color) (w : number) (x1, y1) (x2, y2) : shape =
  let dx = x2 - x1 and dy = y2 - y1 in
  rectangle c (sqrt ((dx * dx) + (dy * dy))) w |> rotate (atan2 dy dx * 180. / Float.pi) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let spectrum ~at:(cx, cy) ~size:(w, h) ~(color : color) ~(back : color) (samples : Signal.t) : shape list =
  let mags = Spectrum.of_signal samples in
  let n = 2 *.. (Array.length mags -.. 1) in
  let bars = 90 in
  let freq b = 20. * (1000. ** (float_of_int b / float_of_int bars)) in
  let bar b =
    let lo = freq b and hi = freq (b +.. 1) in
    let top = ref 0. in
    Array.iteri
      (fun k v ->
        let f = Spectrum.bin_frequency ~n k in
        if f >= lo && f < hi && v > !top then top := v)
      mags;
    let db = if !top <= 0. then -80. else Float.max (-80.) (20. * log10 !top) in
    let bh = (db + 80.) / 80. * h in
    let bw = w / float_of_int bars in
    rectangle color (bw - 1.) (Float.max 1. bh) |> move (cx - (w / 2.) + ((float_of_int b + 0.5) * bw)) (cy - (h / 2.) + (bh / 2.))
  in
  (rectangle back w h |> move cx cy) :: List.init bars bar

let scope ~at:(cx, cy) ~size:(w, h) ~points ~(color : color) ~(back : color) ?gain (samples : Signal.t) : shape list =
  let at i = samples.(Array.length samples -.. 1024 +.. (i *.. 1024 /.. points)) in
  let x i = cx - (w / 2.) + (float_of_int i / float_of_int points * w) in
  let y =
    match gain with
    | Some g -> fun i -> cy + (Float.max (-1.) (Float.min 1. (at i * g)) * h / 2.)
    | None ->
        let peak = List.fold_left (fun p i -> Float.max p (Float.abs (at i))) 1e-3 (List.init points (fun i -> i)) in
        fun i -> cy + (at i / peak * h / 2.)
  in
  (rectangle back w h |> move cx cy) :: List.init (points -.. 1) (fun i -> segment color 2. (x i, y i) (x (i +.. 1), y (i +.. 1)))
