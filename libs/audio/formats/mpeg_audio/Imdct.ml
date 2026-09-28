(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Imdct.mli *)

(* cos(pi / 2n (2i + 1 + n/2)(2k + 1)), for the two sizes *)
let cosines (n : int) : float array array =
  Array.init n (fun i ->
      Array.init (n / 2) (fun k ->
          cos (Float.pi /. float_of_int (2 * n) *. float_of_int (((2 * i) + 1 + (n / 2)) * ((2 * k) + 1)))))

let cos36 = cosines 36
let cos12 = cosines 12
let table (n : int) = if n = 36 then cos36 else if n = 12 then cos12 else cosines n

(* output i, the formula: a for loop, not Array.iteri, since a float ref
 * that a closure captures is boxed, an allocation per addition
 * (notes_opti_ocaml.md) *)
let output (c : float array array) (coefficients : float array) (i : int) : float =
  let row = c.(i) and sum = ref 0. in
  for k = 0 to Array.length coefficients - 1 do
    sum := !sum +. (coefficients.(k) *. row.(k))
  done;
  !sum

(* claude: a quarter of the outputs computed, the others their mirrors,
 * where it was every output from the formula:
 *
 *   Array.init n (fun i -> output c coefficients i)
 *
 * With a = 2i + 1 + n/2, output n/2 - 1 - i has 2n - a instead, and
 * cos((2k + 1) pi - x) = -cos x: x[n/2 - 1 - i] = -x[i]; output 3n/2 - 1
 * - i has 4n - a, and cos(2 (2k + 1) pi - x) = cos x: x[3n/2 - 1 - i] =
 * x[i] -- the aliases, the halves mirrored, that the overlap cancels.
 * And coefficients all zero (the high subbands, mostly) give zeros,
 * nothing computed. Half the multiplications, and far fewer in quiet
 * bands (notes_opti_ocaml.md). *)
let imdct (coefficients : float array) : float array =
  let n = 2 * Array.length coefficients in
  let x = Array.make n 0. in
  if Array.exists (fun v -> v <> 0.) coefficients then (
    let c = table n in
    for i = 0 to (n / 4) - 1 do
      let v = output c coefficients i in
      x.(i) <- v;
      x.((n / 2) - 1 - i) <- -.v
    done;
    for i = n / 2 to (3 * n / 4) - 1 do
      let v = output c coefficients i in
      x.(i) <- v;
      x.((3 * n / 2) - 1 - i) <- v
    done);
  x

let mdct (samples : float array) : float array =
  let n = Array.length samples in
  let c = table n in
  Array.init (n / 2) (fun k ->
      let sum = ref 0. in
      for i = 0 to n - 1 do
        sum := !sum +. (samples.(i) *. c.(i).(k))
      done;
      !sum)

let sine (period : int) (i : int) : float = sin (Float.pi /. float_of_int period *. (float_of_int i +. 0.5))

let windows =
  [| Array.init 36 (sine 36);
     Array.init 36 (fun i -> if i < 18 then sine 36 i else if i < 24 then 1. else if i < 30 then sine 12 (i - 18) else 0.);
     Array.init 12 (sine 12);
     Array.init 36 (fun i -> if i < 6 then 0. else if i < 12 then sine 12 (i - 6) else if i < 18 then 1. else sine 36 i) |]

let window (block_type : int) : float array = windows.(block_type)
