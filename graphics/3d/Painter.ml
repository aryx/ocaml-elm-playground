(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Painter.mli *)

let sort_far_to_near ~(eye : Vec3.t) (points_of : 'face -> Vec3.t list) (faces : 'face list) : 'face list =
  (* the squared distance: the same order, without a square root *)
  let dist2_to_eye face =
    let (dx, dy, dz) = Vec3.sub eye (Vec3.centroid (points_of face)) in
    (dx *. dx) +. (dy *. dy) +. (dz *. dz)
  in
  faces |> List.sort (fun face1 face2 -> compare (dist2_to_eye face2) (dist2_to_eye face1))
