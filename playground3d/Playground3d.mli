(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(** {1 A 3D playground on top of elm_playground}

    Same spirit as {!Playground}: a tiny combinator API to build 3D
    pictures, animations, and games with no asset pipeline needed to get
    started. A 3D shape is built out of world-space triangles/polygons
    (not a local-transform-plus-form pair like 2D {!Playground.shape}),
    since {!move3d}/{!rotate3d}/{!scale3d} directly transform the points
    they contain -- this keeps the pipeline simple (no matrix stack to
    expose) at the cost of walking the whole shape tree on every
    combinator call, which is fine at playground scale.

    World convention: Y is up (so gravity/jump physics, e.g. for a
    Minecraft-style game, is "along Y"), same as most 3D engines.

    Design credit: this API's overall shape -- shapes as raw world-space
    points instead of a local transform header, {!move3d}/{!rotate3d}/
    {!scale3d}/{!fade3d} recursing directly over those points, {!cube}
    built from 6 explicit faces, an eye/target {!camera} instead of an
    exposed matrix, and the "project a 3D scene down to plain 2D
    {!Playground.shape}s" trick the web backend uses -- is taken from
    Luca Mugnaini's (lucamug's)
    {{:https://github.com/lucamug/elm-playground-3d}elm-playground-3d},
    itself built on top of Evan Czaplicki's
    {{:https://github.com/evancz/elm-playground}elm-playground} (the
    same library {!Playground} ports to OCaml). See {!render3d_to_2d}
    and {!Playground3d_platform} for where this version deliberately
    diverges (backface culling, a depth sort, and a real software
    rasterizer on native). *)

type number = Playground.number

(** {2 Shapes} *)

type shape3d = { alpha : number; form : form3d }
and form3d =
  | Polygon3d of Playground.color * (number * number * number) list
  | TexturedPolygon3d of string * ((number * number * number) * (number * number)) list
  | Group3d of shape3d list

(** A flat polygon in world space, e.g. one face of a cube. Give its
    vertices in counter-clockwise order as seen from the side the face
    should be visible from -- {!render3d_to_2d} backface-culls using the
    winding order to compute the outward normal. Needs at least 3
    points. *)
val polygon3d : Playground.color -> (number * number * number) list -> shape3d

(** Put shapes together so you can {!move3d}/{!rotate3d}/{!scale3d} them
    as a group. *)
val group3d : shape3d list -> shape3d

(** {2 Basic 3D shapes}

    Like {!Playground.circle}/{!Playground.square}, procedurally
    generated with no assets needed: a single flat color, centered on
    the origin so it is easy to {!move3d} into place. *)

(** [box color width height depth] is a rectangular box (a generalized
    {!cube}), centered on the origin. Prefer this over a single flat
    {!polygon3d} for anything that needs to look like a thin
    line/marker visible from any angle (e.g. an axis indicator in a
    freely-rotatable scene): a flat polygon only has a front face, so
    backface culling makes it vanish once it rotates edge-on or past
    that towards the camera, whereas a thin box always has some face
    pointing towards the camera. *)
val box : Playground.color -> number -> number -> number -> shape3d

(** [cube color size] is a cube of the given color, [size] units on
    each side, centered on the origin. *)
val cube : Playground.color -> number -> shape3d

(** [plane color width depth] is a flat horizontal quad (normal facing
    +Y, i.e. "up") lying on the Y=0 plane, e.g. useful as a ground. *)
val plane : Playground.color -> number -> number -> shape3d

(** {2 Textures}

    Like {!Playground.image}: give a local file path {i or} an http(s)
    URL (e.g. a .png), no other setup needed. Evan-light on purpose --
    there is no atlas/UV mini-language, just "here are 4 corners, here
    is an image, wrap one onto the other" (an advanced user who wants
    an atlas sub-region can still reach for the {!TexturedPolygon3d}
    constructor directly and give explicit UV coordinates per point).

    A URL is downloaded (blocking) the first time it's needed; see
    {!Playground3d_platform.preload_texture} to warm the cache ahead of
    time instead (e.g. during a game's [init]) so the render loop never
    has to block on a network fetch mid-game.

    {b Current limitation:} only the native backend actually samples the
    image (per pixel, in its rasterizer). The web backend cannot yet
    warp an image onto an arbitrary projected quad -- {!Playground.image}
    only draws an upright, unrotated rectangle -- so for now it renders
    textured faces as a flat gray placeholder there instead of dropping
    them; a real fix would need a new quad-mapped-image primitive in the
    underlying 2D {!Playground.shape} type, out of scope for this first
    version. *)

(** [textured_quad src p0 p1 p2 p3] maps the 4 corners of the image at
    [src] onto [p0]/[p1]/[p2]/[p3] in order: top-left, top-right,
    bottom-right, bottom-left ([src]'s row 0 is its top). *)
val textured_quad :
  string ->
  number * number * number ->
  number * number * number ->
  number * number * number ->
  number * number * number ->
  shape3d

(** [textured_cube src size] is like {!cube} but wraps [src] identically
    on all 6 faces, e.g. a Minecraft-style dirt block. *)
val textured_cube : string -> number -> shape3d

(** {2 Move shapes} *)

val move3d : number -> number -> number -> shape3d -> shape3d
val move_x3d : number -> shape3d -> shape3d
val move_y3d : number -> shape3d -> shape3d
val move_z3d : number -> shape3d -> shape3d

(** {2 Customize shapes} *)

(** [rotate3d dx dy dz shape] rotates [shape] (always around the origin,
    like {!Playground.rotate} does for 2D polygons -- use {!group3d} +
    {!move3d} if you need to rotate around a different point), in
    degrees, first around the X axis, then Y, then Z. *)
val rotate3d : number -> number -> number -> shape3d -> shape3d

val scale3d : number -> shape3d -> shape3d

(** [fade3d alpha shape] sets the alpha ([0..1]) of every leaf polygon in
    [shape], overwriting whatever alpha they had (it does not multiply
    with a parent's alpha) -- same simplification lucamug's
    elm-playground-3d makes. *)
val fade3d : number -> shape3d -> shape3d

(** {2 Camera}

    A camera is just an eye position and a look-at target, like
    elm-playground-3d's, not an exposed matrix. It is a plain record
    (like {!Playground.screen}/{!Playground.mouse}), not abstract, so a
    backend that needs to run its own view/projection math (the native
    rasterizer does, to keep per-vertex depth for its z-buffer, unlike
    {!project} below which only returns a 2D point) can read [eye]/
    [target]/[fov]/[near]/[far] directly instead of going through
    {!project}. Use the {!camera} smart constructor rather than building
    one by hand: it figures out the up/right/forward basis for you (world
    "up" is always +Y; don't use this for a camera looking straight up or
    down, since then forward and up become parallel and the basis is
    undefined). *)
type camera = {
  eye : number * number * number;
  target : number * number * number;
  fov : number;
  near : number;
  far : number;
}

val camera :
  eye:number * number * number ->
  target:number * number * number ->
  ?fov:number (** vertical field of view, in degrees. Default 60. *) ->
  ?near:number (** default 0.1 *) ->
  ?far:number (** default 1000. *) ->
  unit ->
  camera

(** {2 The 3D-to-2D pipeline}

    These two are the pure, portable core of the library: both the
    native and the web backend are built on top of them (native uses
    them to know where to rasterize triangles; web uses
    {!render3d_to_2d} directly, then hands the result to the existing 2D
    {!Playground} renderer). *)

(** [project camera screen point] returns the 2D playground-coordinate
    position (using {!Playground.screen}'s same (0,0)-at-center, Y-up
    convention) [point] projects to, or [None] if [point] is at or
    behind the camera's near plane.

    {b Note:} there is no real near-plane clipping (a triangle straddling
    the near plane is not split into visible sub-triangles, just like
    lucamug's elm-playground-3d) -- fine for the modest, mostly-distant
    scenes this library targets so far, revisit if a future game (e.g. a
    Minecraft-style one) needs the camera to get very close to
    geometry. *)
val project : camera -> Playground.screen -> number * number * number -> (number * number) option

(** [render3d_to_2d camera screen shape] backface-culls [shape]'s faces,
    depth-sorts the remaining ones back-to-front (painter's algorithm --
    lucamug's version skips both of these, which only looks right by
    accident for specific camera angles), then projects each one into an
    ordinary {!Playground.shape} (a {!Playground.group} of
    {!Playground.polygon}s). *)
val render3d_to_2d : camera -> Playground.screen -> shape3d -> Playground.shape

(** {1 The 3D Application} *)

type ('model, 'msg) app3d

(** {2 Games}

    Like {!Playground.game}, except [view3d] additionally returns the
    {!camera} to render with. There is no separate "move the camera" API:
    the camera is derived from [computer]/[model] afresh every frame
    exactly like the shape list is, so a moving (e.g. first-person)
    camera is just a matter of storing eye/look-direction state in
    ['model] and computing a new {!camera} value from it in [view3d] --
    same pattern as everything else in this library. *)
val game3d :
  (Playground.computer -> 'model -> camera * shape3d list) ->
  (Playground.computer -> 'model -> 'model) ->
  'model ->
  ('model, Playground.msg) app3d

(**/**)
(* claude: exposed only so Playground3d_platform implementations (native,
 * web) can pattern-match on an app3d; not meant to be used directly by
 * applications (use game3d). *)
val init3d : ('model, 'msg) app3d -> unit -> 'model
val update3d : ('model, 'msg) app3d -> Playground.computer -> 'model -> 'model
val view3d : ('model, 'msg) app3d -> Playground.computer -> 'model -> camera * shape3d list
(**/**)
