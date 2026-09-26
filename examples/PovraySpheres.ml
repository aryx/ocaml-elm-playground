(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* The first picture a ray tracer draws: two spheres over a plane. This
 * one is the ICFP Programming Contest 2000's (Cornell and Bell Labs),
 * whose task was a ray tracer for GML, a scene language; its first
 * example, spheres.gml, says:
 *
 *   s -1.2 0.0 3.0 translate       % a sphere at (-1.2, 0, 3)
 *   s  1.2 1.0 3.0 translate       % another at (1.2, 1, 3)
 *   p  0.0 -3.0 0.0 translate      % the plane y = -3
 *   1.0 -1.0 0.0 point 1.0 1.0 1.0 point light   % a white sun
 *   0.4 0.4 0.4 point ... 90.0 320 240 ... render % ambient, fov, size
 *
 * The same scene on the Povray way, with two changes GML's
 * conventions force, both classic traps:
 *
 * - **GML's world is left-handed**: its eye is at (0, 0, -1) looking
 *   along +z with x to the right, where ours (and OpenGL's) is
 *   right-handed, looking along -z. Copied as is, the picture comes
 *   out mirrored. So every z is negated: the eye at (0, 0, 1), the
 *   spheres at z = -3.
 * - **GML's field of view is horizontal**, ours vertical (the
 *   Playground's, and gluPerspective's): 90 degrees across a 320 x 240
 *   picture is 2 atan (tan 45 * 240 / 320) = 73.7 degrees up.
 *
 * GML's camera only looks along +z; ours looks at a target, and any
 * point on that line gives the same picture. The one chosen is the
 * middle of the scene, (0, 0, -3), because it is the point [orbit]
 * turns round.
 *
 * The picture is made by the ray tracer's latest algorithm, and the
 * arrows go back through the older ones (plan_raytracing_teaching.md,
 * one per phase), each named under the picture:
 *
 *   1. ray casting: one ray per pixel, the colour of what it meets,
 *      flat -- what a ray tracer gives before any light: exact
 *      silhouettes, a sphere round at any size, a plane going to the
 *      horizon, where a rasterizer's are facets and a quad;
 *   2. Lambert's light: the sun's share at each point, by the angle
 *      it arrives at -- the spheres become round;
 *   3. shadow rays: a second ray from each point towards the sun, and
 *      the spheres' shadows on the plane, the one thing a rasterizer
 *      cannot compute.
 *
 * With the flag evolution (PovraySpheres.exe evolution), all of them
 * side by side.
 *
 * The picture is made a slice per frame, coarse to fine: blurred at
 * once, sharp a few frames later (Raytrace.start). Drag the mouse to
 * turn round the spheres (Povray.orbit), and the picture starts again
 * from its coarsest pass; space makes it again, "s" saves it,
 * povray.png.
 *
 * What it uses: the Playground, and the Povray way (its orbit).
 *
 * Exercises: move the camera's eye up and look down at the spheres;
 * give the picture 32 x 24 pixels (~size) and count the rays; add a
 * third sphere between the two and the eye.
 *)
open Playground
open Povray

let yellow = color (rgb 204 204 51)
let fov = 2. *. atan (tan (Float.pi /. 4.) *. 240. /. 320.) *. 180. /. Float.pi

let app (caps : < Cap.open_out >) =
  orbit ~export:caps ~size:(320, 240)
    (scene ~ambient:0.4 ~sky:black
       ~camera:(camera ~fov ~eye:(0., 0., 1.) ~target:(0., 0., -3.) ())
       [ sun white 1. (-1.) 0. ]
       [ move (-1.2) 0. (-3.) (sphere yellow); move 1.2 1. (-3.) (sphere yellow); move 0. (-3.) 0. (plane (color white)) ])

let main =
  Cap.main (fun caps ->
      Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app (caps :> < Cap.open_out >)))
