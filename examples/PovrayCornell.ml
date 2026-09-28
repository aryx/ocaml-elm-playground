(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The Cornell box: a white room with a red wall and a green one, two
 * white blocks, a lamp under the ceiling -- built at Cornell in 1984
 * (Goral, Torrance, Greenberg and Battaile, "Modeling the Interaction of
 * Light Between Diffuse Surfaces"), photographed, and rendered, to
 * check a renderer against the real thing. It is the picture of light
 * bouncing: the blocks' sides facing the red wall turn pink, those
 * facing the green one green (colour bleeding); the corners and the
 * floor under the blocks are darker than the open floor; and the
 * lamp, a ball rather than a point, casts soft shadows.
 *
 * It opens on path tracing (Kajiya 1986, Raytrace.mli), 2 x 2 paths a
 * pixel: grainy -- each pixel an estimate from a few random paths --
 * and 3 and 4 (9 and 16 paths) calm it, as 1 / sqrt paths. The left
 * arrow goes back to Whitted's algorithm and before: the room lit by
 * the lamp alone and a constant ambient, no bleeding, flat corners --
 * the guess that path tracing replaces with the light itself.
 *
 * What it uses: the Playground, and the Povray way (box, area_lamp,
 * still's algorithm and samples).
 *
 * Exercises: make one block a mirror; count how many paths a pixel
 * takes before you cannot tell 4 x 4 from 3 x 3.
 *)
open Playground
open Povray

let plaster = color (rgb 220 220 220)
let wall s = box s |> scale 1. 1. 0.02
let room =
  [ box plaster |> scale 1. 0.02 1. |> move 0. (-1.02) 0.; (* floor *)
    box plaster |> scale 1. 0.02 1. |> move 0. 1.02 0.; (* ceiling *)
    wall plaster |> move 0. 0. (-1.02); (* back *)
    box (color (rgb 200 30 30)) |> scale 0.02 1. 1. |> move (-1.02) 0. 0.; (* left, red *)
    box (color (rgb 30 180 40)) |> scale 0.02 1. 1. |> move 1.02 0. 0. (* right, green *) ]

let blocks =
  [ box plaster |> scale 0.28 0.6 0.28 |> rotate 0. 20. 0. |> move (-0.35) (-0.4) (-0.3);
    box plaster |> scale 0.28 0.28 0.28 |> rotate 0. (-18.) 0. |> move 0.4 (-0.72) 0.25 ]

let app (caps : < Cap.open_out >) =
  still ~export:caps ~file:"cornell.png" ~size:(200, 200) ~algorithm:Raytrace.Path_tracing ~samples:2
    (scene ~ambient:0.25 ~sky:black
       ~camera:(camera ~fov:42. ~eye:(0., 0., 3.6) ~target:(0., 0., 0.) ())
       [ area_lamp 0.18 (lamp white 0. 0.78 0.) ]
       (room @ blocks))

let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_out >)))
