(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The two renderers on one frame: the rasterizer on the left half, the
 * ray tracer on the right, and the time each took -- the trade-off of
 * real-time graphics in one window (plan_raytracing_teaching.md).
 *
 * On the software backend, run it with -debug-keys and press "v"
 * (versus), best after "r" twice or three times (a third or a quarter
 * of the resolution): the ray tracer is slow, and that is half the
 * lesson. The left half: the rasterizer, a few milliseconds, lit and
 * textured, and nothing more -- no shadow, the mirror a red ball, the
 * glass a white one. The right half: the ray tracer, the same frame,
 * some thirty times slower (at a third of the resolution, measured:
 * 15 ms against 470 for the frame), with everything a ray can find: the shadows
 * under each solid, the floor in the mirror, the checker bent in the
 * glass. "y" chooses the ray tracer's algorithm (the right half), from
 * ray casting to Whitted's; -rt-samples 2 smooths its edges, four
 * times slower again.
 *
 * The scene is an ordinary shape3d scene: every other backend draws
 * it, the mirror and the glass as opaque balls (Playground3d.shiny,
 * glassy: only the ray tracer reads them).
 *
 * What it uses: the 3D Playground (shiny, glassy, textured_cube); the
 * software backend's "v" key.
 *
 * Exercises: which of the ray tracer's algorithms costs the most, and
 * why ("y", and watch the right half's time); make the glass ball
 * bigger than the mirror and look at the mirror through it.
 *)
open Playground
open Playground3d

let view (_computer : Playground.computer) () =
  let floor = plane (rgb 190 190 190) 12. 12. in
  let crate = textured_cube "examples/checker.png" 1.4 |> rotate3d 0. 25. 0. |> move3d (-1.8) 0.7 0. in
  let mirror = sphere red 0.8 |> move3d 0.2 0.8 (-0.6) |> shiny 0.8 in
  let glass = sphere white 0.7 |> move3d 1.6 0.7 1.2 |> glassy 1.5 in
  let pillar = box purple 0.5 2.6 0.5 |> move3d 2.6 1.3 (-1.8) in
  let cam = camera ~eye:(0.5, 3.5, 7.) ~target:(0., 0.6, 0.) () in
  (cam, [ floor; crate; mirror; glass; pillar ])

let update _computer () = ()
let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
