(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The ICFP Programming Contest 2000's fib.gml: a 5 x 5 grid of spheres
 * whose heights are Fibonacci numbers, y = fib (1 + (x + z) / 2), their
 * colours cycling through six. In GML the scene was a program -- the
 * recursion, the loops, the union of the 25 spheres all in the scene
 * language, run by the contestant's interpreter -- so that the
 * interpreter, not only the ray tracer, was being tested:
 *
 *   { /self /i
 *     i 3 lessi
 *     { 1 }
 *     { i 1 subi self self apply  i 2 subi self self apply  addi } if
 *   } /fib
 *
 * Here it is the four lines of OCaml below, which is the trade the
 * Povray way makes (plan_raytracing_teaching.md, "No GML parser"):
 * scenes are programs in the language the program is written in.
 *
 * The same conversions as PovraySpheres.ml: GML's world is
 * left-handed, so every z is negated, and its 90 degrees of field of
 * view are horizontal, 73.7 vertical at 320 x 240. The author's own
 * entry rendered this scene in 2000, and its picture is kept as a
 * test (graphics/tests/icfp2000/, Unit_raytrace.ml): this ray tracer
 * is checked against one written twenty-six years before it.
 *
 * What it uses: the Playground, and the Povray way.
 *
 * Exercises: bigfib.gml's scene, the grid 10 x 10; the spheres of a
 * height a Fibonacci number in glass.
 *)
open Playground
open Povray

let rec fib i = if i < 3 then 1 else fib (i - 1) + fib (i - 2)
(* GML's colours, pure primaries: red, green, blue, magenta, yellow,
 * cyan (not the Playground's softer red, green, blue, yellow) *)
let colors = [| rgb 255 0 0; rgb 0 255 0; rgb 0 0 255; rgb 255 0 255; rgb 255 255 0; rgb 0 255 255 |]

let spheres =
  List.concat_map
    (fun x ->
      List.map
        (fun z ->
          let y = fib (1 + ((x + z) / 2)) in
          sphere (color colors.((x + z) mod 6))
          |> move ((2.5 *. float_of_int x) -. 7.) (float_of_int y -. 3.) (-.((2.5 *. float_of_int z) +. 3.)))
        [ 1; 2; 3; 4; 5 ])
    [ 1; 2; 3; 4; 5 ]

let fov = 2. *. atan (tan (Float.pi /. 4.) *. 240. /. 320.) *. 180. /. Float.pi

let app (caps : < Cap.open_out >) =
  orbit ~export:caps ~file:"fib.png" ~size:(320, 240)
    (scene ~ambient:0.4 ~sky:black
       ~camera:(camera ~fov ~eye:(0., 0., 1.) ~target:(0., 0., -10.5) ())
       [ sun white 1. (-1.) 0. ]
       spheres)

let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_out >)))
