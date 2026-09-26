(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* POV-Ray's way of making a picture, on top of the Playground: a scene
 * described -- solids, their surfaces, the lights, a camera -- and a
 * ray tracer to draw it, pixel after pixel, with no triangles in
 * between (graphics/3d/raytrace/). Where the 3D Playground's shape3d
 * scenes are what a rasterizer can draw, this is what a ray can meet:
 * exact spheres, infinite planes, and later solids made of solids
 * (constructive solid geometry), glass and mirrors.
 *
 * The names are POV-Ray's (the POV-Team, 1991-; David Buck's
 * DKBTrace, 1986, before it) where POV-Ray has one, and GML's (the
 * ICFP Programming Contest's scene language, 2000) otherwise; the
 * scenes are written in OCaml, not parsed from a file
 * (plan_raytracing_teaching.md, "No GML parser").
 *
 * A way, like Logo or Karel: [still] builds the whole app, so a
 * program written on it has no update and no view. The app is a 2D
 * Playground app whose picture is one bitmap, so it runs on every 2D
 * backend, the browser included, with no 3D backend at all.
 *
 * So far (plan_raytracing_teaching.md, phase 6): spheres, planes,
 * boxes, cylinders, cones and tori, and solids made of them (union,
 * intersection, difference), plain or checkered, matte, mirrors or
 * glass, moved, scaled and turned into place, lit by suns, lamps and
 * spots, with their shadows, made progressively, a slice per frame,
 * and turned with the mouse ([orbit]).
 *
 * {1 The same scene, by each algorithm}
 *
 * The ray tracer grew one algorithm at a time -- ray casting, flat;
 * then Lambert's light; then shadow rays; ... (Raytrace.algorithms) --
 * and keeps them all. So any scene can be seen by each of them:
 *
 * - in [still], the left and right arrows go back to the algorithm
 *   before and forward again (the latest first), the one shown named
 *   under the picture;
 * - with the flag [evolution] (e.g. PovraySpheres.exe evolution, or
 *   ?evolution in the browser), all of them side by side, the oldest
 *   first -- what each idea adds, at a glance.
 *
 * A first program, the ICFP 2000 task's own first scene
 * (examples/PovraySpheres.ml):
 *
 *   let yellow = color (rgb 204 204 51)
 *   let app =
 *     still ~size:(320, 240)
 *       (scene ~ambient:0.4 ~sky:black ~camera:(camera ~eye:(0., 0., 1.) ~target:(0., 0., 0.) ())
 *          [ sun white 1. (-1.) 0. ]
 *          [ move (-1.2) 0. (-3.) (sphere yellow);
 *            move 1.2 1. (-3.) (sphere yellow);
 *            move 0. (-3.) 0. (plane (color white)) ])
 *)

(*****************************************************************************)
(* {1 The camera} *)
(*****************************************************************************)

(* where the eye is, and what it looks at; the world's y is up *)
type camera

(* [camera ~eye ~target ()]: [fov], the vertical field of view in
 * degrees (default 60, the 3D Playground's). The way's own, not
 * Playground3d.camera: this way lives in the 2D Playground (see the
 * plan's phase 0 review). *)
val camera :
  ?fov:Playground.number ->
  eye:Playground.number * Playground.number * Playground.number ->
  target:Playground.number * Playground.number * Playground.number ->
  unit ->
  camera

(*****************************************************************************)
(* {1 The solids} *)
(*****************************************************************************)

(* what a solid's surface looks like *)
type surface

(* one colour all over *)
val color : Playground.color -> surface

(* [checker c1 c2]: a checkerboard of the two, squares of [size]
 * (default 1) -- cubes, in fact, alternating in space, so that any
 * solid can be checkered (a solid texture) *)
val checker : ?size:Playground.number -> Playground.color -> Playground.color -> surface

(* [marble c1 c2]: Perlin's marble (1985), c1 veined with c2, the veins
 * [size] apart (default 1): stripes across x, their edges shaken by
 * turbulence (Perlin.mli) *)
val marble : ?size:Playground.number -> Playground.color -> Playground.color -> surface

(* [wood c1 c2]: rings around the y axis, from c1 to c2, [size] apart
 * (default 0.2), shaken a little by noise: turn a cylinder on its side
 * for a log's end, keep it upright for a plank's grain *)
val wood : ?size:Playground.number -> Playground.color -> Playground.color -> surface

(* [pattern f]: any colour, [f ~x ~y ~z] at each point of the surface
 * (in the solid's own space: it moves with it) -- GML's surface
 * function, the scene's texture as a program *)
val pattern : (x:Playground.number -> y:Playground.number -> z:Playground.number -> Playground.color) -> surface

(* [shiny s surface]: a mirror, s of it (0. none, 1. a perfect mirror):
 * the reflected ray's colour is that share of its own (Raytrace's
 * Whitted) *)
val shiny : Playground.number -> surface -> surface

(* [glassy n surface]: light goes through, bent by the index of
 * refraction n (1.5 glass, 1.33 water, 2.42 diamond), filtered by the
 * surface's colour (white: clear glass) *)
val glassy : Playground.number -> surface -> surface

(* a solid, intersected exactly: a sphere is round at any distance, a
 * plane never ends *)
type obj

(* the sphere of radius 1 centred at the origin *)
val sphere : surface -> obj

(* the plane y = 0, going on for ever (its inside, for CSG, below) *)
val plane : surface -> obj

(* the other solids, each of size one at the origin, intersected
 * exactly (Solid.mli): the cube from (-1, -1, -1) to (1, 1, 1); the
 * cylinder of radius 1 around the y axis from y = -1 to 1; the cone,
 * its apex at (0, 1, 0) and its base of radius 1 at y = -1; [torus r],
 * a ring of radius 1 around the y axis, its tube of radius r *)
val box : surface -> obj
val cylinder : surface -> obj
val cone : surface -> obj
val torus : Playground.number -> surface -> obj

(* constructive solid geometry (Csg.mli): the solid inside any of them,
 * inside all of them, inside the first and not the second. The
 * surfaces are the solids' own: a hole drilled in a red box with a
 * blue cylinder is blue inside. *)
val union : obj list -> obj
val inter : obj list -> obj
val diff : obj -> obj -> obj

(* [move x y z obj]: the same solid, moved by (x, y, z) *)
val move : Playground.number -> Playground.number -> Playground.number -> obj -> obj

(* [scale x y z obj]: stretched by x along the x axis, ... (a sphere
 * scaled by (2, 1, 1) is an ellipsoid); about the origin, so scale
 * first, then move *)
val scale : Playground.number -> Playground.number -> Playground.number -> obj -> obj

(* [rotate x y z obj]: turned by x degrees about the x axis, then y about
 * the y axis, then z about the z axis; about the origin, as [scale] *)
val rotate : Playground.number -> Playground.number -> Playground.number -> obj -> obj

(*****************************************************************************)
(* {1 The lights} *)
(*****************************************************************************)

type light

(* [sun color x y z]: a light infinitely far away, shining along
 * (x, y, z) -- GML's "light", POV-Ray's parallel light source *)
val sun : Playground.color -> Playground.number -> Playground.number -> Playground.number -> light

(* [lamp color x y z]: a light at the point (x, y, z), shining all
 * around -- GML's "pointlight", POV-Ray's light_source *)
val lamp : Playground.color -> Playground.number -> Playground.number -> Playground.number -> light

(* [spot color ~at ~towards angle]: a lamp at [at] lighting a cone
 * pointed at [towards], [angle] degrees wide on each side of its axis,
 * fading to its edge as cos^[falloff] (default 1) -- GML's spotlight *)
val spot :
  ?falloff:Playground.number ->
  Playground.color ->
  at:Playground.number * Playground.number * Playground.number ->
  towards:Playground.number * Playground.number * Playground.number ->
  Playground.number ->
  light

(*****************************************************************************)
(* {1 The scene, and the app} *)
(*****************************************************************************)

type scene

(* [scene ~camera lights solids]: [ambient] (default 0.2), the light
 * that reaches even what no lamp does, 0. to 1.; [sky] (default
 * white), what a ray that meets nothing sees *)
val scene :
  ?ambient:Playground.number -> ?sky:Playground.color -> camera:camera -> light list -> obj list -> scene

(* what [still] and [orbit] remember: the pictures under way *)
type model

(* The scene, ray traced a slice per frame, coarse to fine
 * (Raytrace.start): a blurred picture at once, sharpening as you
 * watch, the window answering all the while.
 *
 * - [size] (width, height): the picture's own size in pixels (default
 *   the window's), drawn as large as the window holds -- the rays are
 *   the cost, so a small picture is a fast one;
 * - [rays_per_frame] (default 20,000): the camera rays shot each
 *   frame, a fixed number so that a frame's picture is the same on
 *   every run (the golden frames), however fast the computer;
 * - [export]: the capability to write a file; given, "s" saves the
 *   picture as [file] (default "povray.png"), a PNG, from the
 *   program's main, [Cap.main (fun caps -> ... ~export:(caps :> ...))].
 *
 * Keys: space makes the picture again; the left and right arrows, the
 * algorithm (see above); 1 to 4, the rays a pixel, n x n, averaged
 * (antialiasing: Raytrace.mli, n^2 times the work); "s", see [export]. A line under the picture
 * says the pass, the rays shot and the time taken. The flag
 * [evolution]: see above; the program's main must hand the flags over,
 * [run_app ~flags:(Playground_platform.flags ())]. *)
val still :
  ?export:< Cap.open_out > ->
  ?file:string ->
  ?rays_per_frame:int ->
  ?size:int * int ->
  scene ->
  (model Playground.game, Playground.msg) Playground.app

(* [still], and the mouse turns the camera: dragging goes round the
 * target (left and right, up and down), the wheel nearer or further.
 * Each move starts the picture again, from its coarsest pass -- which
 * is 1/64th of the rays, so the scene follows the mouse, blurred, and
 * sharpens when it stops: exploring a ray-traced scene with no GPU. *)
val orbit :
  ?export:< Cap.open_out > ->
  ?file:string ->
  ?rays_per_frame:int ->
  ?size:int * int ->
  scene ->
  (model Playground.game, Playground.msg) Playground.app
