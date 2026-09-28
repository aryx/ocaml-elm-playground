(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* TinyQuake's level, ray traced, and walked through as Myst was: a
 * still per place and per way of looking, made as you arrive.
 *
 *   left and right arrows: turn a quarter; up arrow: go forward (when
 *   there is somewhere to go that way); l: Quake's light or the ray
 *   tracer's (see below)
 *
 * Why stills and not TinyQuake's walking: a picture of this level at
 * 320 x 200 -- 64,000 rays, each with a shadow ray to each of the
 * seven lamps, and the glass's -- is about a second of native code
 * here, three to five in a browser (Bvh.mli) -- one frame a second at
 * best, where TinyQuake draws sixty. Myst's answer
 * (TinyMyst) was to stop moving between pictures: a place, a picture,
 * a click. So here, 6 places, 4 ways of looking from each, each still
 * made once, coarse to fine, and kept.
 *
 * The lesson is the one TinyQuake's header gives, from the other side.
 * Quake's "light" tool shot, once, before the game, a ray from each
 * patch of each face to each lamp, and kept the answer: lit, or in
 * shadow (the lightmaps). That is exactly the ray tracer's "shadow
 * rays" algorithm (Raytrace.mli), done per pixel instead of per patch:
 * "l" shows it. What a lightmap could never hold is what depends on
 * where the eye is -- a reflection, the room seen bent through glass.
 * So the runes here are a glass ball, a mirror and a golden one, and
 * with Whitted's algorithm (the default) they show the room; with
 * Quake's light they are three dull balls.
 *
 * The level is TinyQuake's map, its boxes copied (the rock, the
 * lamps, the runes), each box one of the ray tracer's exact boxes --
 * no BSP, no faces cut by CSG: a ray meets the nearest solid, and the
 * inside of the rock is never seen. TinyQuake's lamps fade with
 * distance ([reach]); the ray tracer's do not, so theirs are dimmed
 * here to about the light they give in their own room.
 *
 * What it uses: the Povray way's scene (box, sphere, glassy, shiny,
 * lamp) and Raytrace (start, advance, picture), as TinyMyst's
 * render=live does; not [Povray.still], which has one camera.
 *
 * Exercises: Myst's clicks (TinyMyst's [target]: the picture's edges
 * turn, its middle goes forward) and its dissolve between two
 * pictures; the lamps with TinyQuake's falloff (a new kind of light in
 * Raytrace.mli, a few lines in its shading); more places; the stills
 * made once and embedded, as TinyMyst's are (myst/make_stills.ml).
 *)
open Playground
open Povray

(*****************************************************************************)
(* The level, from TinyQuake *)
(*****************************************************************************)

(* the rock, as boxes, (x0, y0, z0) to (x1, y1, z1), and their colours
 * (TinyQuake's [rock], with its map) *)
let rock : ((float * float * float) * (float * float * float) * (int * int * int)) list =
  let grey = (120, 118, 116) and brown = (124, 104, 84) and dark = (96, 92, 104) in
  [ (* the floor and the four outer walls *)
    ((0., -32., 0.), (1024., 0., 1024.), brown);
    ((0., -32., 0.), (64., 256., 1024.), grey);
    ((960., -32., 0.), (1024., 256., 1024.), grey);
    ((0., -32., 0.), (1024., 256., 64.), grey);
    ((0., -32., 960.), (1024., 256., 1024.), grey);
    (* the ceilings: room B is taller than the others *)
    ((64., 160., 64.), (448., 256., 448.), grey);
    ((576., 224., 64.), (960., 256., 448.), grey);
    ((576., 160., 576.), (960., 256., 960.), dark);
    (* the rock north of room A *)
    ((64., -32., 448.), (448., 256., 960.), grey);
    (* the wall between A and B, with the doorway at z 224..288 *)
    ((448., -32., 64.), (576., 256., 224.), grey);
    ((448., -32., 288.), (576., 256., 960.), grey);
    ((448., 96., 224.), (576., 256., 288.), grey);
    (* the wall between B and C, with the doorway at x 736..800 *)
    ((576., -32., 448.), (736., 256., 576.), grey);
    ((800., -32., 448.), (960., 256., 576.), grey);
    ((736., 96., 448.), (800., 256., 576.), grey);
    (* the pillar in A, the platform in B *)
    ((224., 0., 320.), (288., 160., 384.), brown);
    ((640., 0., 128.), (800., 64., 288.), brown) ]

(* the lamps: where, and TinyQuake's strength and tint *)
let lamps : ((float * float * float) * float * (float * float * float)) list =
  [ ((352., 130., 352.), 2.86, (1., 0.92, 0.8));
    ((144., 120., 144.), 1.56, (1., 0.9, 0.75));
    ((512., 80., 256.), 1.82, (1., 0.9, 0.7));
    ((768., 190., 256.), 3.12, (0.9, 0.95, 1.));
    ((640., 120., 400.), 1.56, (1., 0.95, 0.85));
    ((768., 80., 512.), 1.56, (1., 0.85, 0.7));
    ((700., 140., 760.), 2.08, (1., 0.6, 0.45)) ]

(* claude: no falloff in the ray tracer, so a lamp lights its whole
 * room as brightly as TinyQuake's does its nearest wall: dimmed by this *)
let dimming = 0.2

(* the runes: where, and what they are made of *)
let runes : ((float * float * float) * surface) list =
  [ ((384., 40., 144.), glassy 1.5 (color white));
    ((720., 104., 208.), shiny 0.9 (color (rgb 200 200 210)));
    ((760., 40., 760.), shiny 0.5 (color (rgb 230 180 60))) ]

let solids : obj list =
  List.map
    (fun ((x0, y0, z0), (x1, y1, z1), (r, g, b)) ->
      box (color (rgb r g b))
      |> scale ((x1 -. x0) /. 2.) ((y1 -. y0) /. 2.) ((z1 -. z0) /. 2.)
      |> move ((x0 +. x1) /. 2.) ((y0 +. y1) /. 2.) ((z0 +. z1) /. 2.))
    rock
  @ List.map (fun ((x, y, z), s) -> sphere s |> scale 14. 14. 14. |> move x y z) runes

let lights : light list =
  List.map
    (fun ((x, y, z), strength, (r, g, b)) ->
      let c k = int_of_float (Float.min 255. (255. *. k *. strength *. dimming)) in
      lamp (rgb (c r) (c g) (c b)) x y z)
    lamps

(*****************************************************************************)
(* The places *)
(*****************************************************************************)

(* where one can stand, on the floor (x, z), and the places one can
 * walk to from each (both ways) *)
let places : (string * (float * float)) list =
  [ ("start", (144., 256.));
    ("a", (380., 256.));
    ("ab door", (512., 256.));
    ("b", (700., 370.));
    ("bc door", (768., 512.));
    ("c", (768., 640.)) ]

let paths = [ ("start", "a"); ("a", "ab door"); ("ab door", "b"); ("b", "bc door"); ("bc door", "c") ]

(* the four ways of looking, a quarter turn apart: 0 along x, 1 along
 * z (to the right of x, y being up), ... *)
let direction (heading : int) : float * float =
  match heading mod 4 with 0 -> (1., 0.) | 1 -> (0., 1.) | 2 -> (-1., 0.) | _ -> (0., -1.)

(* where going forward leads: the neighbour within 45 degrees of the
 * way one looks, if any *)
let forward (place : string) (heading : int) : string option =
  let x, z = List.assoc place places in
  let dx, dz = direction heading in
  let neighbours = List.filter_map (fun (a, b) -> if a = place then Some b else if b = place then Some a else None) paths in
  List.find_opt
    (fun n ->
      let nx, nz = List.assoc n places in
      let vx = nx -. x and vz = nz -. z in
      (vx *. dx) +. (vz *. dz) > sqrt ((vx *. vx) +. (vz *. vz)) *. cos (Float.pi /. 4.))
    neighbours

let eye_height = 40.

let scene_at (place : string) (heading : int) : scene =
  let x, z = List.assoc place places in
  let dx, dz = direction heading in
  scene ~ambient:0.18 ~sky:black
    ~camera:(camera ~fov:65. ~eye:(x, eye_height, z) ~target:(x +. dx, eye_height, z +. dz) ())
    lights solids

(*****************************************************************************)
(* The stills *)
(*****************************************************************************)

(* Quake's own screen *)
let width = 320
let height = 200

(* claude: each still under way or done, by place, heading and
 * algorithm; a Raytrace.progress is mutable, advanced in place (as
 * TinyMyst's live pictures are) *)
let stills : (string * int * Raytrace.algorithm, Raytrace.progress) Hashtbl.t = Hashtbl.create 16

let still (place : string) (heading : int) (algorithm : Raytrace.algorithm) : Raytrace.progress =
  match Hashtbl.find_opt stills (place, heading, algorithm) with
  | Some p -> p
  | None ->
      let options = { Raytrace.default_options with algorithm } in
      let p = Raytrace.start ~options (raytrace_scene (scene_at place heading)) ~width ~height in
      Hashtbl.replace stills (place, heading, algorithm) p;
      p

(*****************************************************************************)
(* The model, update and view *)
(*****************************************************************************)

type model = {
  place : string;
  heading : int;
  (* Whitted's, or Shadow_rays, Quake's light *)
  algorithm : Raytrace.algorithm;
  (* the keys held the frame before, to act once per press *)
  held : string Set_.t;
}

let initial = { place = "start"; heading = 0; algorithm = Raytrace.Whitted; held = Set_.empty }

let update (computer : computer) (m : model) : model =
  let keys = computer.keyboard.keys in
  let pressed k = Set_.mem k keys && not (Set_.mem k m.held) in
  let m =
    if pressed "ArrowLeft" then { m with heading = (m.heading + 3) mod 4 }
    else if pressed "ArrowRight" then { m with heading = (m.heading + 1) mod 4 }
    else if pressed "ArrowUp" then
      match forward m.place m.heading with Some p -> { m with place = p } | None -> m
    else if pressed "l" then
      { m with algorithm = (if m.algorithm = Raytrace.Whitted then Raytrace.Shadow_rays else Raytrace.Whitted) }
    else m
  in
  let p = still m.place m.heading m.algorithm in
  if not (Raytrace.finished p) then Raytrace.advance p ~rays:20_000;
  { m with held = keys }

let view (computer : computer) (m : model) : shape list =
  let z = Float.min (computer.screen.width /. float_of_int width) ((computer.screen.height -. 120.) /. float_of_int height) in
  let p = still m.place m.heading m.algorithm in
  (* claude: Playground's move, the 2D one; Povray's moves solids *)
  let line y c s = Playground.move 0. (computer.screen.bottom +. y) (words c s) in
  let light = if m.algorithm = Raytrace.Whitted then "the ray tracer's (Whitted)" else "Quake's (shadow rays)" in
  let ahead = match forward m.place m.heading with Some n -> "up: to " ^ n | None -> "a wall ahead" in
  [ rectangle black computer.screen.width computer.screen.height;
    Playground.move 0. 50. (bitmap (float_of_int width *. z) (float_of_int height *. z) (Raytrace.picture p));
    line 70. (rgb 230 200 120) (Printf.sprintf "%s, looking %s -- %s" m.place [| "east"; "north"; "west"; "south" |].(m.heading) ahead);
    line 35. (rgb 160 140 110)
      (Printf.sprintf "l: light %s   pass %d, %d rays" light (Raytrace.pass p) (Raytrace.rays_shot p)) ]

let app = game view update initial
let main = Playground_platform.run_app app
