(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* graphics/3d/Painter *)

let t = Testo.create

(* Painter.mli's example: faces 5, 1 and 3 away from the eye, drawn in
 * the order 5, 3, 1 *)
let test_order () =
  let face d = [ (0., 0., -.d) ] in
  let sorted = Painter.sort_far_to_near ~eye:(0., 0., 0.) (fun (_name, points) -> points) in
  Alcotest.(check (list string)) "farthest first" [ "5"; "3"; "1" ]
    (sorted [ ("5", face 5.); ("1", face 1.); ("3", face 3.) ] |> List.map fst)

let tests = Testo.categorize "Painter" [ t "far to near, the worked example" test_order ]
