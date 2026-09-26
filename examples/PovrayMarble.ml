(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Procedural surfaces: a colour computed at each point of space, and
 * the solid carved out of it, as from a block of stone or wood (Ken
 * Perlin's and Darwyn Peachey's papers, both at SIGGRAPH 1985). No
 * picture file, no (u, v): a sphere of marble has veins running
 * through it, not painted on it, and they meet at every seam.
 *
 * Four spheres on a wooden board: marble (stripes shaken by Perlin's
 * turbulence, Perlin.mli), wood (rings, a sphere cut from a log), the
 * checkerboard, and a surface that is a program -- [pattern], GML's
 * surface function, here polka dots, from the point's own coordinates.
 *
 * What it uses: the Playground, and the Povray way (marble, wood,
 * checker, pattern, orbit).
 *
 * Exercises: the marble's veins finer (size); a pattern of your own,
 * stripes by y, rings by the distance from the origin; the dots
 * turning with the sphere (rotate it: the pattern is in its own space).
 *)
open Playground
open Povray

(* dots on a grid a quarter apart, from the point's own x, y, z *)
let dots =
  pattern (fun ~x ~y ~z ->
      let near v = Float.abs (v -. Float.round v) in
      let d = sqrt ((near (x *. 4.) ** 2.) +. (near (y *. 4.) ** 2.) +. (near (z *. 4.) ** 2.)) in
      if d < 0.3 then rgb 40 60 160 else rgb 240 230 200)

let app (caps : < Cap.open_out >) =
  orbit ~export:caps ~file:"marble.png" ~size:(400, 300)
    (scene ~ambient:0.3 ~sky:(rgb 60 70 90)
       ~camera:(camera ~fov:40. ~eye:(0., 3., 8.) ~target:(0., 0.7, 0.) ())
       [ lamp (rgb 230 230 220) (-3.) 7. 6. ]
       [ (* a plank: long along its own y, the wood's rings around it,
          * then laid down along x -- the rings become the grain *)
         box (wood ~size:0.15 (rgb 190 140 90) (rgb 120 70 35)) |> scale 0.1 4.5 1.2 |> rotate 0. 0. 90. |> move 0. (-0.1) 0.;
         sphere (marble (rgb 235 235 230) (rgb 70 80 90)) |> scale 0.8 0.8 0.8 |> move (-2.7) 0.8 0.;
         sphere (wood (rgb 200 150 90) (rgb 110 60 30)) |> scale 0.8 0.8 0.8 |> move (-0.9) 0.8 0.;
         sphere (checker ~size:0.25 (rgb 220 60 50) (rgb 240 240 240)) |> scale 0.8 0.8 0.8 |> move 0.9 0.8 0.;
         sphere dots |> scale 0.8 0.8 0.8 |> move 2.7 0.8 0.;
         plane (color (rgb 90 90 100)) |> move 0. (-0.2) 0. ])

let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_out >)))
