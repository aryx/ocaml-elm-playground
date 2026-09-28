(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Cycling.mli *)

let turn (palette : (int * int * int) array) (r : Ilbm.range) (steps : int) : (int * int * int) array =
  let out = Array.copy palette in
  let n = r.high - r.low + 1 in
  if n > 1 && r.high < Array.length palette then begin
    let steps = if r.reverse then -steps else steps in
    for i = 0 to n - 1 do
      (* the colour now at low + i came from [steps] places before *)
      let from = ((((i - steps) mod n) + n) mod n) in
      out.(r.low + i) <- palette.(r.low + from)
    done
  end;
  out

let palette_at (palette : (int * int * int) array) (ranges : Ilbm.range list) (seconds : float) : (int * int * int) array =
  List.fold_left
    (fun p (r : Ilbm.range) -> if r.active then turn p r (int_of_float (seconds *. Ilbm.steps_per_second r)) else p)
    palette ranges
