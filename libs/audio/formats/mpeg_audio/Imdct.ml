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

(* for loops, not Array.iteri: a float ref a closure captures is boxed,
 * an allocation per addition *)
let imdct (coefficients : float array) : float array =
  let n = 2 * Array.length coefficients in
  let c = table n in
  Array.init n (fun i ->
      let row = c.(i) and sum = ref 0. in
      for k = 0 to (n / 2) - 1 do
        sum := !sum +. (coefficients.(k) *. row.(k))
      done;
      !sum)

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
