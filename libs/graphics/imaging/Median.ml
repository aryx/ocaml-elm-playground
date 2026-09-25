(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Median.mli *)

let median ~(radius : int) (img : Pixels.image) : Pixels.image =
  let out = Pixels.copy img in
  let side = (2 * radius) + 1 in
  (* a histogram of the window, and the value where half is reached:
     no sort, 256 counts *)
  let counts = Array.make 256 0 in
  let half = (side * side / 2) + 1 in
  for y = 0 to img.height - 1 do
    for x = 0 to img.width - 1 do
      for c = 0 to 2 do
        Array.fill counts 0 256 0;
        for j = -radius to radius do
          for i = -radius to radius do
            let v = Pixels.get img (x + i) (y + j) c in
            counts.(v) <- counts.(v) + 1
          done
        done;
        let rec find v seen = let seen = seen + counts.(v) in if seen >= half then v else find (v + 1) seen in
        Pixels.set out x y c (find 0 0)
      done
    done
  done;
  out
