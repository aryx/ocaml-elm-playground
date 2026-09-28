(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Tonewheel.mli *)

let count = 91
let gears = [| (85, 104); (71, 82); (67, 73); (105, 108); (103, 100); (84, 77); (74, 64); (98, 80); (96, 74); (88, 64); (67, 46); (108, 70) |]
let shaft = 20. (* revolutions a second *)

let frequency (w : int) : float =
  let w = max 1 (min count w) in
  (* the last seven: F's to B's gears, 192 bumps *)
  let gear, bumps = if w <= 84 then ((w - 1) mod 12, 2. ** float_of_int (((w - 1) / 12) + 1)) else (w - 85 + 5, 192.) in
  let driving, driven = gears.(gear) in
  shaft *. float_of_int driving /. float_of_int driven *. bumps

let lowest = 24 (* C1, wheel 1 *)

let rec of_note (n : int) : int =
  if n < lowest then of_note (n + 12) else if n > lowest + count - 1 then of_note (n - 12) else n - lowest + 1

let cents (w : int) : float =
  let n = w + lowest - 1 in
  let tempered = 440. *. (2. ** (float_of_int (n - 69) /. 12.)) in
  1200. *. Float.log2 (frequency w /. tempered)
