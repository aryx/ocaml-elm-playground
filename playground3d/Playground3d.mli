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
  | SmoothPolygon3d of Playground.color * ((number * number * number) * (number * number * number)) list
      (** (point, normal) pairs -- see {!sphere}. Unlike {!Polygon3d},
          each point carries its own normal instead of sharing one
          normal computed from the face's winding order, which is what
          lets a curved shape look smoothly shaded (Gouraud/Phong, see
          {!Playground3d_platform}'s shading modes) instead of faceted.
          No texture support on this constructor (kept minimal, like
          everything else added incrementally here) -- combine with
          {!TexturedPolygon3d} yourself if you ever need both. *)
  | Hud of Playground.shape
      (** A 2D overlay shape, drawn in screen space on top of the whole
          3D scene -- see {!hud}. *)
  | Group3d of shape3d list
  | Cached3d of cached  (** see {!cached3d} *)

(** What {!cached3d} builds: [content] is its shapes as a group, [id] its
    identity, [huds] the {!hud} shapes inside, found once. *)
and cached = { id : int; content : shape3d; huds : Playground.shape list }

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

(** [sphere color radius] is a UV-tessellated sphere of the given color,
    centered on the origin, built from {!SmoothPolygon3d} faces so it
    renders smoothly shaded (Gouraud/Phong) instead of faceted -- see
    {!Playground3d_platform}'s shading modes. Coarser than a
    "real" 3D engine's sphere (a fixed, small latitude/longitude
    tessellation, no configurable detail level), which is exactly what
    makes the faceting-vs-smooth-shading tradeoff visible: a flat/
    flat_shading mode shows the individual quad faces, Gouraud/Phong
    hide them. *)
val sphere : Playground.color -> number -> shape3d

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

(** [embedded_texture ~name ~base64] registers an image the program
    carries with it -- the bytes of a .png, base64-encoded (see
    [graphics/images/Base64.mli]), usually generated from the file at
    build time by a dune rule, the way [graphics/font/dune] embeds the
    Hershey font and [games3d/dune] this project's one texture -- and
    returns [name], to be used as the [src] of {!textured_quad} and
    friends.

    Why: a [src] that is a file path only works from the directory the
    path is relative to, and not at all in a browser; embedded bytes
    work from anywhere, and the WebGL backend hands them to the browser
    as a "data:" URL, which costs it nothing. [name] is any short label,
    and is what the backends' caches are keyed by. *)
val embedded_texture : name:string -> base64:string -> string

(** the base64 an {!embedded_texture} was registered with, for the
    backends: [None] for a [src] that is a path or a URL *)
val embedded : string -> string option

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

(** {2 HUD}

    [game3d]'s [view3d] only ever returns a 3D scene -- there is no
    separate "2D overlay" return value. Instead, [hud] wraps an
    ordinary 2D {!Playground.shape} (built with any of the existing 2D
    combinators -- {!Playground.words}/{!Playground.rectangle}/
    {!Playground.image}/{!Playground.group}/{!Playground.move}/
    {!Playground.fade}/...) into a {!shape3d} you drop directly into
    the list you already return, e.g.:
    {[
      (cam, [ ground; player; stars_group;
              hud (words black (Printf.sprintf "Score: %d" m.score)
                   |> move (computer.screen.left +. 40.) (computer.screen.top -. 40.)) ])
    ]}
    positioned using the exact same coordinate system (origin at
    screen center, {!Playground.screen}'s bounds, already available in
    [view3d]'s [computer] argument) as a 2D [picture]/[animation]/
    [game].

    {b A [Hud] shape is screen-space, not scene-space:} {!move3d},
    {!rotate3d}, and {!scale3d} are no-ops on it, even nested inside a
    {!group3d} that itself gets moved/rotated -- the whole point of a
    HUD is that it stays fixed on screen regardless of what the 3D
    scene around it is doing. {!fade3d} does apply (it reaches every
    leaf, {!Hud} included, the same way it already reaches every other
    form3d case). *)
val hud : Playground.shape -> shape3d

(** {2 Big static scenes}

    [view3d] describes the whole scene every frame, and a backend
    processes that description every frame. Fine for a few hundred
    faces; not for a world of thousands of blocks, where that work, not
    the drawing itself, takes all the time (see
    docs/claude_notes/plan_opengl_perf.md). *)

(** [cached3d shapes] is [group3d shapes], plus a promise: this value
    won't change. The GPU backends (OpenGL, WebGL) then turn it into GPU
    buffers the first time they see it, and on later frames just draw
    those buffers again, skipping all the per-face work. The same idea
    as Elm's [Html.lazy].

    To benefit, build it {b once} (in the initial model, or in
    [update3d] when something in it changes), and return that same
    value from [view3d] on every frame: each call to [cached3d] makes a
    new, never seen value, so calling it inside [view3d] is correct but
    as slow as a group. A value no longer returned by [view3d] has its
    GPU buffers freed at the end of the frame.

    Draws exactly like [group3d shapes] on every backend (the software
    and web ones don't cache anything). {!move3d}, {!rotate3d},
    {!scale3d} and {!fade3d} work on it too, but their result is new
    geometry, so it's an ordinary uncached group: transform first, then
    cache. *)
val cached3d : shape3d list -> shape3d

(**/**)
(* claude: exposed only so Playground3d_platform implementations
 * (native, web) can extract the Hud shapes out of a scene to draw
 * separately from the 3D geometry; not meant to be used directly by
 * applications. *)
val collect_hud_shapes : shape3d -> Playground.shape list
(**/**)

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
    undefined; a camera that rolls or looks straight up or down gives its
    own [up], see {!camera}). *)
type camera = {
  eye : number * number * number;
  target : number * number * number;
  (* claude: which way is up on the screen: (0, 1, 0) for a camera
   * that doesn't roll; turned around [target - eye], the picture turns
   * the other way (a plane banking, games3d/TinyDescent3d's ship) *)
  up : number * number * number;
  fov : number;
  (** 0 for the usual camera, which has perspective. Otherwise the
      height of the view in world units, the same at every depth: an
      orthographic camera, which never divides by the depth, so two
      things of one size are drawn one size however far apart they are.
      That is what an isometric game is drawn with (see
      [kits/isometric], which does the same two lines by hand), and
      what {!Camera3d.orthographic} sets. *)
  ortho : number;
  near : number;
  far : number;
}

val camera :
  eye:number * number * number ->
  target:number * number * number ->
  ?up:number * number * number (** which way is up on the screen. Default (0, 1, 0). *) ->
  ?fov:number (** vertical field of view, in degrees. Default 60. *) ->
  ?ortho:number (** the view's height in world units, for a camera with no perspective; default 0., which means use [fov]. *) ->
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

(** {1 Rendering hints} *)

(** How surfaces are lit (by a fixed "sun" up and to the side):
    - [No_lighting]: every face drawn in its own color, as given;
    - [Flat]: one brightness per face, from the direction it faces:
      crisp facets;
    - [Smooth]: the brightness varies across curved shapes' faces
      (e.g. {!sphere}'s), so they look round; on flat-faced shapes
      ({!cube}, {!box}, ...) the same as [Flat]. *)
type shading = No_lighting | Flat | Smooth

(** How to draw, for the backends that can honor it, given to
    [Playground3d_platform.run_app3d ~rendering]:
    - [shading]: see {!shading};
    - [backface_culling]: skip the faces turned away from the camera
      (true: the usual, since they're hidden by the front faces of a
      closed shape anyway), or draw both sides (false: e.g. for a lone
      {!plane} seen from below);
    - [smooth_textures]: enlarged textures blend their pixels (true) or
      show them as sharp squares (false), like
      {!Playground.rendering}'s [smooth_images].

    Each backend maps these to what it has (the software rasterizer to
    its own algorithms, OpenGL to shaders and GL settings, the web to
    what SVG polygons can do: [Smooth] is [Flat] there, and it has no
    textures), and the backends' debug keys can still change them while
    the app runs: these are the starting values. *)
type rendering = { shading : shading; backface_culling : bool; smooth_textures : bool }

(** [Smooth], culling, smooth textures *)
val default_rendering : rendering

(** [render3d_to_2d camera screen shape] backface-culls [shape]'s faces,
    depth-sorts the remaining ones back-to-front (painter's algorithm --
    lucamug's version skips both of these, which only looks right by
    accident for specific camera angles), then projects each one into an
    ordinary {!Playground.shape} (a {!Playground.group} of
    {!Playground.polygon}s). With [rendering]'s [shading] (except
    [No_lighting]), each face's color is darkened by how much it faces
    away from the light (flat shading); [backface_culling] false keeps
    all the faces. *)
val render3d_to_2d : ?rendering:rendering -> camera -> Playground.screen -> shape3d -> Playground.shape

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
