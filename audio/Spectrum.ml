(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Spectrum.mli *)

type complex = float * float

let dft (x : float array) : complex array =
  let n = Array.length x in
  Array.init n (fun k ->
      let re = ref 0. and im = ref 0. in
      Array.iteri
        (fun i xi ->
          let a = 2. *. Float.pi *. float_of_int (k * i mod n) /. float_of_int n in
          re := !re +. (xi *. cos a);
          im := !im -. (xi *. sin a))
        x;
      (!re, !im))

let fft (x : float array) : complex array =
  let n = Array.length x in
  if n land (n - 1) <> 0 then invalid_arg "Spectrum.fft: not a power of 2";
  (* recursive, the definition's shape: the even samples' transform
   * and the odd ones', combined: X[k] = E[k] + w^k O[k], X[k + n/2] =
   * E[k] - w^k O[k], w = e^(-2 pi i / n) (the butterfly) *)
  let rec go (x : complex array) : complex array =
    let n = Array.length x in
    if n = 1 then x
    else
      let even = go (Array.init (n / 2) (fun i -> x.(2 * i))) and odd = go (Array.init (n / 2) (fun i -> x.((2 * i) + 1))) in
      let out = Array.make n (0., 0.) in
      for k = 0 to (n / 2) - 1 do
        let a = -2. *. Float.pi *. float_of_int k /. float_of_int n in
        let (ore, oim) = odd.(k) and (ere, eim) = even.(k) in
        (* w^k O[k] *)
        let tre = (cos a *. ore) -. (sin a *. oim) and tim = (cos a *. oim) +. (sin a *. ore) in
        out.(k) <- (ere +. tre, eim +. tim);
        out.(k + (n / 2)) <- (ere -. tre, eim -. tim)
      done;
      out
  in
  go (Array.map (fun v -> (v, 0.)) x)

let magnitudes (spectrum : complex array) : float array =
  let n = Array.length spectrum in
  Array.init ((n / 2) + 1) (fun k ->
      let (re, im) = spectrum.(k) in
      let m = sqrt ((re *. re) +. (im *. im)) /. float_of_int n in
      if k = 0 || k = n / 2 then m else 2. *. m)

let bin_frequency ~(n : int) (k : int) : float = float_of_int k *. float_of_int Signal.rate /. float_of_int n

let hann (x : float array) : float array =
  let n = Array.length x in
  Array.mapi (fun i v -> v *. 0.5 *. (1. -. cos (2. *. Float.pi *. float_of_int i /. float_of_int (n - 1)))) x

let of_signal ?(window = true) (s : Signal.t) : float array =
  let rec pow2 p = if p * 2 <= min 4096 (Array.length s) then pow2 (p * 2) else p in
  let n = pow2 1 in
  let x = Array.sub s 0 n in
  magnitudes (fft (if window then hann x else x))

let peak (mags : float array) : int =
  let best = ref 1 in
  Array.iteri (fun k m -> if k > 0 && m > mags.(!best) then best := k) mags;
  !best
