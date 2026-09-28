(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Solids made of solids: constructive solid geometry, which a ray
 * tracer gets nearly for free and a rasterizer hardly at all (Csg.mli:
 * along a ray, a solid is where the ray is inside, and union,
 * intersection and difference are merges of those intervals). Three
 * of them, from the ICFP 2000 task's third tier and POV-Ray's first
 * tutorials:
 *
 * - "holes": a cube minus three cylinders, drilled through along x, y
 *   and z -- look through them ([orbit]: drag), and see the inside of
 *   the holes lit (the hole's normal, turned round: Csg.mli);
 * - a die: a cube intersected with a sphere, its corners rounded,
 *   minus its pips, small spheres sunk into its faces -- and the pips'
 *   black is the spheres' own surface, left inside the holes;
 * - a lens: two spheres intersected, in glass -- what no triangle mesh
 *   says as well: two exact curved faces, which bend the board behind
 *   it upside down and larger (Whitted's algorithm, the fourth).
 *
 * And a torus, the quartic, lying on the board; lit by a lamp, and by
 * a spot aimed at the die, its light a disc on the board.
 *
 * What it uses: the Playground, and the Povray way (box, cylinder,
 * sphere, torus, union, inter, diff, scale, rotate, move, spot, orbit).
 *
 * Exercises: drill a fourth hole, diagonally (rotate 0. 45. 0.); give
 * the die its six faces' pips; make the lens a concave one (a box minus
 * two spheres).
 *)
open Playground
open Povray

let board = checker (rgb 90 110 150) (rgb 220 225 235)

(* a cube minus three cylinders along its three axes *)
let holes =
  let drill = scale 0.4 1.5 0.4 (cylinder (color (rgb 250 190 60))) in
  diff (box (color (rgb 230 120 40))) (union [ drill; rotate 90. 0. 0. drill; rotate 0. 0. 90. drill ])
  |> scale 0.7 0.7 0.7 |> rotate 0. 30. 0. |> move (-2.3) 0.7 0.

(* a cube with its corners rounded by a sphere, minus its pips *)
let die =
  let pip x y z = move x y z (scale 0.2 0.2 0.2 (sphere (color (rgb 20 20 20)))) in
  let pips =
    union
      [ pip 0. 1. 0.; (* the top: one *)
        pip (-0.45) 0.45 1.; pip 0.45 (-0.45) 1.; (* the front: two *)
        pip 1. 0.45 0.45; pip 1. 0. 0.; pip 1. (-0.45) (-0.45) (* the right: three *) ]
  in
  diff (inter [ box (color white); scale 1.35 1.35 1.35 (sphere (color white)) ]) pips
  |> scale 0.7 0.7 0.7 |> rotate 0. (-25.) 0. |> move 0. 0.7 0.3

(* two unit spheres, their centres 1.4 apart: a lens 0.6 thick and
 * 0.71 in radius (sqrt (1 - 0.7^2)), made 1.4 times as large. Their
 * centres 2 or more apart, the intersection is empty: nothing at all,
 * which is what the first version of this example drew *)
let lens =
  inter [ move 0. 0. 0.7 (sphere (glassy 1.5 (color white))); move 0. 0. (-0.7) (sphere (glassy 1.5 (color white))) ]
  |> scale 1.4 1.4 1.4 |> rotate 0. (-20.) 0. |> move 2.2 1.1 0.8

let ring = torus 0.25 (shiny 0.3 (color (rgb 200 40 60))) |> scale 0.8 0.8 0.8 |> move 0.8 0.2 (-2.)

let app (caps : < Cap.open_out >) =
  orbit ~export:caps ~file:"csg.png" ~size:(400, 300)
    (scene ~ambient:0.25 ~sky:(rgb 30 30 40)
       ~camera:(camera ~fov:45. ~eye:(0., 3.5, 7.5) ~target:(0., 0.6, 0.) ())
       [ lamp (rgb 150 150 150) (-4.) 8. 6.;
         spot ~falloff:4. (rgb 255 240 200) ~at:(1.5, 6., 3.) ~towards:(0., 0.7, 0.3) 20. ]
       [ plane board; holes; die; lens; ring ])

let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_out >)))
