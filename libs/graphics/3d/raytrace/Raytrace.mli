(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* The ray tracer: a picture made by asking, for each pixel, what the
 * eye sees through it -- a ray from the eye through the pixel, the
 * nearest solid it meets, and the colour there, which may take more
 * rays: one towards each light (is this point in shadow?), one
 * reflected, one refracted. The rasterizer (graphics/3d/Render) asks
 * the opposite question, which pixels a triangle covers, and so cannot
 * ask the others: it has no rays.
 *
 *      eye  o-------->  pixel  ----------->  x the nearest hit
 *                                            |\
 *                              shadow ray   /  \  reflected ray
 *                               to a light      ...
 *
 * {1 The algorithms, each kept}
 *
 * The picture is made by one of several algorithms, one per step of
 * the plan (plan_raytracing_teaching.md), each the one before plus an
 * idea -- and all of them stay, so that the same scene can be drawn by
 * each and the pictures put side by side (the Povray way's
 * "evolution" flag, the software backend's "y" key):
 *
 *   Ray_casting    the colour of what the ray meets, unlit: exact
 *                  silhouettes, flat (phase 1)
 *   Lambert        lit: each light's share, by the angle it reaches
 *                  the surface at (phase 2)
 *   Shadow_rays    unless something stands between the point and the
 *                  light: a second ray, towards it (phase 2)
 *   Whitted        and what a mirror reflects, what glass lets
 *                  through: more rays, from the point on (phase 5)
 *   Soft_shadows   a lamp with a size, and its shadow's soft edge:
 *                  several shadow rays, to random points of it (phase 9)
 *   Path_tracing   and the light bounced from everything else: one
 *                  random ray more at each point, the ambient's guess
 *                  computed instead (phase 9)
 *
 * The default is Whitted's, the last one with no noise: the two after
 * it are estimates, noisy at one ray a pixel.
 *
 * {1 Ray casting}
 *
 * The first step is Appel's (1968) without his shadows: one ray per
 * pixel, the nearest solid it meets, that solid's colour.
 *
 * The ray through a pixel is the rasterizer's projection run
 * backwards, so that the two renderers draw the same frame: the
 * rasterizer takes a point in view coordinates (x, y, z) to
 *
 *      ndc = (f x / aspect / z, f y / z)       f = 1 / tan (fov / 2)
 *
 * (Camera.ndc) and ndc to the pixel; here a pixel's centre gives ndc,
 * and the ray from the eye goes along
 *
 *      forward + (ndc_x aspect / f) right + (ndc_y / f) up
 *
 * the points of view coordinates (ndc_x aspect / f, ndc_y / f, 1)
 * times any depth -- all of which the rasterizer sends to that pixel.
 * Example: fov = 90 degrees (f = 1), a square picture, the pixel at the
 * middle of the right edge: ndc = (1, 0), the ray along forward +
 * right, 45 degrees to the right. An orthographic camera (ortho > 0)
 * shoots parallel rays instead, along forward, from points spread
 * over the picture.
 *
 * The camera's near and far planes cut the rays as they cut the
 * rasterizer's triangles: a hit nearer than near or beyond far, in
 * depth, is not seen (a ray at an angle reaches the near plane
 * further along, near / cos angle: [camera_ray] says how far).
 *
 * Brute force: every ray is tested against every solid, pixels x
 * solids tests. Measured (phase 1, natively): examples/Cubes3d ray
 * cast at 1000 x 1000, a million rays against its 300 triangles, is
 * 29 s, about ten million ray/triangle tests a second; the rasterizer
 * draws the same frame 60 times a second. A bounding volume hierarchy
 * (Bvh) tests a handful of solids per ray instead, and finds the same
 * hits: [options.acceleration], brute force kept beside it as the
 * definition and for the comparison. Cubes3d at 1000 x 1000 with its
 * shadows: 38 s by brute force, 1.5 s with the tree (Bvh.mli has the
 * numbers).
 *
 * {1 Light, and shadow}
 *
 * At the point the ray meets, with n the surface's normal turned
 * towards the eye and L the direction towards a light, each channel of
 * the surface's colour is multiplied by
 *
 *      ambient + sum over the lights of  light * max (0, n . L)
 *
 * Lambert's cosine law (see Lighting.mli), light by light; the
 * rasterizer's own formula when the lights are its one sun of
 * strength 1 - ambient (Shape3d_render_software), so that the two draw
 * the same picture. With [Shadow_rays] a light counts only if a ray
 * from the point towards it meets nothing on the way: the one thing a
 * rasterizer cannot know, since it never asks about any point but the
 * one it is filling.
 *
 *            light
 *              \    x  a sphere in the way:
 *               \  (  )  the point is in its shadow
 *                \
 *      ----------- p ---------- the plane
 *
 * Three bugs are classic, and each was one of the author's in his ICFP
 * 2000 entry (txt/history.txt); each has its test in
 * graphics/tests/Unit_raytrace.ml:
 *
 * - **shadow acne**: p is computed, so it is a hair above or below the
 *   surface, and a shadow ray starting exactly at p meets the surface
 *   itself about half the time -- a stippled picture. The ray starts a
 *   little way off, at t > [epsilon] (the entry's 0.001 and its
 *   comment "acne pb"). [options.epsilon] = 0. brings the bug back, to
 *   be seen.
 * - **the clamp**: three lights on a point make it three times as
 *   bright as its colour, past 255, which as a byte wraps round to
 *   dark. A channel stops at 255.
 * - **the sun is infinitely far**: a lamp's shadow ray stops at the
 *   lamp, but a sun's never does -- give it a finite distance and a
 *   far enough object casts no shadow.
 *
 * {1 Mirrors and glass: Whitted}
 *
 * The rasterizer, and the algorithms above, ask where a ray from the
 * eye lands and what lights that point. Whitted (1980) asks one more
 * thing: what does that point *see*? A mirror shows what is along the
 * reflected ray, glass what is along the refracted one -- so shoot
 * them, and shade what they meet the same way, recursively:
 *
 *        eye                       n
 *         \         reflected  \  |  /
 *          \  d                 \ | /  r = d - 2 (d . n) n
 *           \                    \|/
 *     ---------------------------- p ------------------ glass
 *                                   \
 *                                    \  refracted: Snell,
 *                                     \ n1 sin i = n2 sin t
 *
 * A mirror of [shiny] s: (1 - s) of its own lit colour, s of what the
 * reflected ray sees. Glass of index n: Fresnel's share F reflected,
 * the rest through it, filtered by its colour, bent entering and bent
 * back leaving; F by Schlick's approximation (1994), 4% head on for
 * glass, all of it at grazing angles -- why a window is a mirror at
 * night. Leaving glass past the critical angle nothing goes through,
 * all is reflected: total internal reflection, what keeps light in an
 * optical fibre.
 *
 * Two limits stop the recursion (two mirrors facing each other would
 * not): [options.depth] bounces at most (the ICFP scenes' 3), and the
 * **attenuation cutoff**, from the ICFP 2000 contest's second place
 * (Camls 'R Us): a ray whose colour will be less than [options.cutoff]
 * of the pixel (1/256: under a byte's step) is not shot at all.
 * [saved_rays] counts them.
 *
 * Glass casts a full shadow, as in Whitted's own picture: a shadow ray
 * that meets glass is blocked. Light bent onto a surface by glass (a
 * caustic) needs rays from the lights, not from the eye -- out of
 * scope (photon mapping, Jensen 1996).
 *
 * {1 Several rays a pixel}
 *
 * One ray through a pixel's centre sees one point, and a pixel is a
 * square: an edge across it is all or nothing, a staircase, and a
 * pattern finer than the pixels (a checkerboard to the horizon, a
 * texture seen from far) turns to noise, the high frequencies folded
 * onto low ones -- aliasing. More rays a pixel, averaged, is the ray
 * tracer's antialiasing, [options.samples] n: n x n of them, one in
 * each cell of a grid over the pixel (stratified: never two in the same
 * corner, as n^2 random ones could be):
 *
 *        +-----+-----+     n = 2: four rays, at the cells' centres,
 *        |  x  |  x  |     (x + 1/4, y + 1/4), (x + 3/4, y + 1/4) ...
 *        +-----+-----+     n^2 times the work: 4 x 4 is 16 times
 *        |  x  |  x  |     slower, for the still picture, not the
 *        +-----+-----+     window
 *
 * Each sample is clamped before the average, as a lone ray's pixel
 * would be. Cook's jittered samples (1984) move each ray at random
 * within its cell, trading the grid's regular leftovers for noise:
 * phase 9's, with an explicit seed.
 *
 * {1 Randomness: soft shadows, and paths}
 *
 * A lamp is not a point, and a shadow's edge is not sharp: a point in
 * the penumbra sees part of the lamp. Which part is an integral over
 * the lamp's surface, and Cook, Porter and Carpenter (1984) estimated
 * such integrals with a few rays each at random -- here 16 shadow rays
 * to random points of the lamp, the share that reach it. The same idea
 * gives glossy reflections, motion blur and depth of field (random
 * reflected rays, random times, random points of a lens): "distributed
 * ray tracing".
 *
 * Kajiya (1986) went all the way. The light leaving a point is what it
 * emits plus what it reflects of the light arriving from every
 * direction -- which left other points the same way: the rendering
 * equation, an integral over the hemisphere, recursive. Estimate it
 * with one random direction per point, followed from point to point: a
 * path. The ambient term (a constant, a guess at the light bouncing
 * around) is gone: that light is computed, from the sky and from the
 * other surfaces -- a red wall's light tints what faces it (colour
 * bleeding), a corner is darker than an open floor. The price is
 * noise: each pixel an estimate from few paths, its error shrinking as
 * 1 / sqrt samples -- four times the rays for half the noise.
 *
 *        eye                    sky
 *          \     p2 ---------> x
 *           \   /
 *            p1       each bounce a random direction around the
 *                     normal, more of them near it (cosine weighted)
 *
 * Random, but deterministic: each pixel's numbers come from its own
 * seed (Lehmer), from its place in the picture and [options.seed], so
 * a picture is the same on every run and whatever order its pixels
 * are made in.
 *
 * Its references: Robert L. Cook, Thomas Porter and Loren Carpenter,
 * "Distributed Ray Tracing" (SIGGRAPH 1984); James T. Kajiya, "The
 * Rendering Equation" (SIGGRAPH 1986); Tom Malley's cosine-weighted
 * directions (1988), and Kevin Beason's smallpt (2007), a path tracer in
 * 99 lines of C++, the size to compare with.
 *
 * References: Arthur Appel, "Some Techniques for Shading Machine
 * Renderings of Solids" (AFIPS 1968), ray casting and shadows; Andrew
 * S. Glassner (ed.), An Introduction to Ray Tracing (1989), the book
 * this is written out of, chapter 2 (Eric Haines) for the camera rays
 * and the shadow rays; Turner Whitted, "An Improved Illumination Model
 * for Shaded Display" (CACM, 1980); Christophe Schlick, "An Inexpensive
 * BRDF Model for Physically-based Rendering" (Eurographics, 1994). *)

(*****************************************************************************)
(* {1 The scene} *)
(*****************************************************************************)

(* a light's colour, each channel from 0. (none) to 1. (full) *)
type rgb = float * float * float

type light =
  (* infinitely far: every point sees it in the same direction,
   * [towards] it, a unit vector *)
  | Sun of { towards : Vec3.t; color : rgb }
  (* at a point: seen in a direction of its own from each point *)
  (* ... of [radius] (0: a point), seen as a ball by the soft shadows *)
  | Lamp of { position : Vec3.t; radius : float; color : rgb }
  (* a lamp that lights a cone only: [aim] its axis (a unit vector),
   * [angle] its half-angle in degrees, and inside it cos^[falloff] of
   * the angle off the axis -- GML's spotlight, the ICFP task's third
   * tier *)
  | Spot of { position : Vec3.t; aim : Vec3.t; angle : float; falloff : float; color : rgb }

type scene = {
  camera : Camera.t;
  solids : Solid.t list;
  lights : light list;
  (* the light that reaches every point, lit or in shadow, 0. to 1. *)
  ambient : float;
  (* 0xRRGGBB, what a ray that meets nothing sees *)
  background : int;
}

(*****************************************************************************)
(* {1 The algorithms} *)
(*****************************************************************************)

type algorithm = Ray_casting | Lambert | Shadow_rays | Whitted | Soft_shadows | Path_tracing

(* in the order they were written, each the one before plus an idea *)
val algorithms : algorithm list

(* the last of them *)
val latest : algorithm

(* Whitted's: the last without noise, the default *)
val default_algorithm : algorithm

(* e.g. "shadow rays" *)
val name : algorithm -> string

(* how a ray finds what it meets: every solid tested, or a tree of
 * boxes (Bvh) -- the same answers, the same pictures *)
type acceleration = Brute_force | Bvh of Bvh.split

type options = {
  algorithm : algorithm;
  (* how far off its surface a shadow ray starts: 0. is the acne bug *)
  epsilon : float;
  acceleration : acceleration;
  (* Whitted: the bounces a ray may make, and the least share of the
   * pixel worth another ray *)
  depth : int;
  cutoff : float;
  (* rays per pixel: n x n, one through each cell of a grid over the
   * pixel, averaged -- antialiasing (see below) *)
  samples : int;
  (* the random numbers' seed (soft shadows, path tracing) *)
  seed : int;
}

(* [default_algorithm], an epsilon of 1e-4, a BVH cut by the surface
 * area heuristic, 3 bounces, a cutoff of 1/256, 1 sample, seed 1 *)
val default_options : options

(*****************************************************************************)
(* {1 The rays} *)
(*****************************************************************************)

(* [camera_ray_through camera ~width ~height px py]: the ray through
 * the point (px, py) of the picture, in pixels from its top left
 * corner; and the t of the near and far planes *)
val camera_ray_through : Camera.t -> width:int -> height:int -> float -> float -> Ray.t * float * float

(* [camera_ray camera ~width ~height ~x ~y]: the ray through the centre
 * of pixel (x, y) of a width x height picture, (0, 0) the top left;
 * and the t along it of the near and far planes *)
val camera_ray : Camera.t -> width:int -> height:int -> x:int -> y:int -> Ray.t * float * float

(* the nearest solid the ray meets between [min_t] (default 0) and
 * [max_t] (default infinity), and at which t *)
val nearest : ?min_t:float -> ?max_t:float -> Ray.t -> Solid.t list -> (float * Solid.t) option

(* [shadowed ~epsilon solids point light]: does a solid stand between
 * the point and the light? *)
val shadowed : epsilon:float -> Solid.t list -> Vec3.t -> light -> bool

(* a scene made ready to be ray traced with some options: its BVH
 * built, once for all the picture's rays *)
type world

val world : ?options:options -> scene -> world

(*****************************************************************************)
(* {1 The optics (Whitted)} *)
(*****************************************************************************)

(* [reflect d n]: [d] reflected by a surface of unit normal [n], d - 2
 * (d . n) n *)
val reflect : Vec3.t -> Vec3.t -> Vec3.t

(* [refract d n ~eta]: the unit direction [d] bent through a surface of
 * unit normal [n] (on [d]'s side), eta = n1 / n2 the index it leaves
 * over the one it enters, and the cosine of the angle it leaves at;
 * [None] past the critical angle, total internal reflection. Example:
 * from air into glass (eta = 1 / 1.5) at 30 degrees, sin t = sin 30 /
 * 1.5 = 1/3; out of glass (eta = 1.5) the critical angle is asin (1 /
 * 1.5) = 41.8 degrees, and at 45 nothing goes through. *)
val refract : Vec3.t -> Vec3.t -> eta:float -> (Vec3.t * float) option

(* [schlick ~n1 ~n2 cos]: the share of light reflected where a ray
 * meets a surface between indices n1 and n2, [cos] the cosine of its
 * angle on the lower index's side: ((n1 - n2) / (n1 + n2))^2 head on,
 * 0.04 for glass, rising to 1 at grazing angles *)
val schlick : n1:float -> n2:float -> float -> float

(*****************************************************************************)
(* {1 Tracing} *)
(*****************************************************************************)

(* [trace world ray ~min_t ~max_t]: the 0xRRGGBB colour seen along the
 * ray *)
val trace : world -> Ray.t -> min_t:float -> max_t:float -> int

(* what the rays traced in this world cost so far: the solids tested
 * (camera and shadow rays), and the boxes entered (0 by brute force) *)
val tests : world -> int
val boxes : world -> int

(* Whitted's reflected and refracted rays shot so far, and those the
 * cutoff saved *)
val secondary_rays : world -> int
val saved_rays : world -> int

(*****************************************************************************)
(* {1 The picture} *)
(*****************************************************************************)

(* [pixel world ~width ~height ~x ~y]: the 0xRRGGBB colour of one pixel
 * of a width x height picture, with the world's samples *)
val pixel : world -> width:int -> height:int -> x:int -> y:int -> int

(* [render scene ~width ~height]: the picture, opaque, a ray per pixel
 * in reading order -- the definition of the picture, which [start]
 * and [advance] below make too, in another order *)
val render : ?options:options -> scene -> width:int -> height:int -> Rgba_image.t

(*****************************************************************************)
(* {1 The picture, a slice at a time} *)
(*****************************************************************************)
(* A ray tracer is slow, and an interactive program cannot wait for
 * it: the picture is made a few thousand rays at a time, one slice
 * per frame of the program, and shown as far as it has got. In which
 * order? Row by row, as POV-Ray's window filled in, makes the bottom
 * of the picture wait for the top. **Coarse to fine** shows all of it
 * at once, blurred, and sharpens it (POV-Ray's "mosaic preview", and
 * every modern renderer's viewport):
 *
 *     pass 8:  a ray at every 8th pixel    +-------+-------+
 *              of every 8th row, its       |       |       |    one ray,
 *              colour on the 8 x 8 block   |   a   |   b   |    64 pixels
 *                                          |       |       |
 *     pass 4:  every 4th, but those        +---+---+---+---+
 *              pass 8 did: 3 rays per      | a | c | b | c |    a, b kept
 *              8 x 8, on 4 x 4 blocks      +---+---+---+---+
 *                                          | c | c | c | c |
 *     pass 2, then pass 1: every pixel     +---+---+---+---+
 *
 * The first pass is 1/64th of the rays and already the whole picture.
 * No ray is shot twice: a pixel's ray is the one shot at its block's
 * corner, the first time a pass comes to it, and a later pass's
 * blocks never cover it again (a later block's corner is on a finer
 * grid). So when the last pass is done each pixel holds its own ray's
 * colour, exactly the colour [render] gives it, whatever the slices
 * were -- the "same bytes" test, in Unit_raytrace.ml. *)

(* the block sizes of the passes, [8; 4; 2; 1] *)
val passes : int list

(* a picture being made *)
type progress

(* [start scene ~width ~height]: nothing shot yet *)
val start : ?options:options -> scene -> width:int -> height:int -> progress

(* [advance progress ~rays]: that many more camera rays (n^2 a pixel
 * with n samples; each may shoot shadow rays of its own), fewer when
 * the picture is done *)
val advance : progress -> rays:int -> unit

val finished : progress -> bool

(* the camera rays shot so far *)
val rays_shot : progress -> int

(* the block size of the pass under way, 1 once finished *)
val pass : progress -> int

(* the world it traces in, and its counters *)
val world_of : progress -> world

(* the picture as far as it has got (what no pass has reached yet is
 * transparent). A new Rgba_image each time the pixels have changed
 * since the last call, the same one otherwise: the Playground's
 * backends keep a bitmap's conversion by its identity (Playground.bitmap),
 * so a picture changed in place would stay the first one on screen. *)
val picture : progress -> Rgba_image.t
