(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Shadows: what a rasterizer cannot draw, and a ray tracer draws with
 * one more ray. A cube, a pillar and a floating ball over a floor, in
 * the 3D Playground's one light, its sun (graphics/3d/geometry/
 * Lighting.mli) -- an ordinary shape3d scene, drawn by any backend.
 *
 * On the software backend, run it with -debug-keys and press "y"
 * (graphics/3d/raytrace/, plan_raytracing_teaching.md), best with "r"
 * three times first (a quarter of the resolution: the ray tracer is
 * slow). Each "y" is the next renderer:
 *
 *   the rasterizer     lit, no shadow: the floor under the ball is as
 *                      bright as the rest (TinyQuake bakes its
 *                      shadows into lightmaps, ahead of time, for that
 *                      reason)
 *   ray casting        the same scene, one ray per pixel, unlit
 *   Lambert's light    lit as the rasterizer lights it: the same picture
 *   shadow rays        a ray from each point towards the sun: the
 *                      shadows, on the floor and on each other
 *   Whitted            and the ball is a mirror (Playground3d.shiny,
 *                      which every other renderer ignores): the floor,
 *                      the crate and the pillar in it
 *   shadow acne        the same, with the shadow rays starting exactly
 *                      on their surface: the floor speckled by its own
 *                      shadow, the classic bug (Raytrace.mli)
 *
 * or start it with -raytrace, the latest from the first frame.
 * The sun comes from (1, 1.3, 0.6), up and to the right and towards
 * the eye, so the shadows fall to the left and away.
 *
 * What it uses: the 3D Playground; the ray tracer through the software
 * backend's "y" key.
 *
 * Exercises: move the ball down until it touches the floor and watch
 * its shadow meet it; lift it higher and watch the shadow slide away
 * from under it -- the sun is low.
 *)
open Playground
open Playground3d

let view (_computer : Playground.computer) () =
  let floor = plane (rgb 200 200 200) 12. 12. in
  let crate = cube orange 1.5 |> move3d 1. 0.75 1. in
  let pillar = box purple 0.6 3. 0.6 |> move3d 1.8 1.5 (-2.) in
  let ball = sphere red 0.9 |> move3d (-1.2) 1.8 0.8 |> shiny 0.6 in
  let cam = camera ~eye:(2.5, 5., 8.) ~target:(-0.5, 0.3, -0.5) () in
  (cam, [ floor; crate; pillar; ball ])

let update _computer () = ()
let app = game3d view update ()
let main = Playground3d_platform.run_app3d app
