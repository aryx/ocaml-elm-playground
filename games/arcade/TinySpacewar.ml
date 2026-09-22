(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Spacewar! (MIT, 1962), one of the first video games,
 * and the first with physics: two spaceships duel around a star whose
 * gravity pulls them in. Two players on one keyboard:
 *
 *   the wedge (red):   left/right turn, up thrust, down fires
 *   the needle (blue): a/d turn,        w thrust,  s fires
 *
 * Steve Russell wrote it on the PDP-1 at MIT with Martin Graetz, Wayne
 * Wiitanen and friends of the Tech Model Railroad Club; Dan Edwards
 * added the star's gravity, Peter Samson the real night sky behind
 * (the "Expensive Planetarium"). It spread to every PDP-1, then to every
 * computer lab; its heirs are Asteroids (1979, Asteroid.ml) and
 * the whole arcade.
 *
 * What it teaches is physics, with playground/Physics.mli: the ships
 * have inertia (let go of thrust and they keep going), turn without
 * changing course, and fall around the star -- every ship, every
 * torpedo:
 *
 *   ship |> turn .. |> thrust .. |> attracted_by star |> step |> wrap screen
 *
 * A ship left alone orbits; thrusting along its course raises the other
 * side of the orbit, and diving close to the star slings it faster (the
 * gravity assist of real spacecraft). Newton's gravitation is
 * docs/claude_notes/notes_2d_physics.md section 6; how the time steps
 * keep orbits closed, section 5 (and examples/PhysicsOrbit.ml).
 *
 * The torpedoes feel the star too, unlike the original's, which flew
 * straight: here they curve, and can orbit. The hits are exact
 * (Physics.touching): the ships' real outlines, a wedge and a needle,
 * against the torpedoes, the star, and each other. No randomness: the background stars come from a formula, so
 * every game is the same (and golden frames are possible). The flag
 * hitboxes draws what the physics sees over the game (Physics.debug).
 *
 * Left as exercises: hyperspace (the original's panic button: vanish,
 * reappear somewhere at random, maybe exploding), limited fuel and
 * torpedoes per round, the sounds (plan_audio_teaching.md), two
 * players on two computers (plan_networking_teaching.md).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the star: heavy enough that 300 pixels away, a circular orbit goes
 * at sqrt (2,000,000 / 300) = 82 pixels per second *)
let star = Physics.body (circle yellow 18.) |> Physics.heavy 2e6

let wedge_shape = polygon red [ (18., 0.); (-15., 13.); (-8., 0.); (-15., -13.) ]
let needle_shape = polygon (rgb 90 150 255) [ (22., 0.); (-15., 6.); (-15., -6.) ]

type ship = {
  body : Physics.body;
  (* None while flying; Some n once hit, n frames ago *)
  exploded : int option;
  score : int;
}

type pilot = Wedge | Needle

type torpedo = { t : Physics.body; ttl : int (* frames left *); owner : pilot }

type game = { wedge : ship; needle : ship; torpedoes : torpedo list }

type scene = Title | Playing of game

type model = scene Scene2d.t

(* both ships on the same circular orbit, on opposite sides, going
 * counterclockwise, pointing where they go *)
let start_ships (wedge_score : int) (needle_score : int) : game =
  let v = sqrt (2e6 / 300.) in
  {
    wedge =
      { body = Physics.body wedge_shape |> Physics.at (-300.) 0. |> Physics.moving 0. (-.v) |> Physics.pointing (-90.);
        exploded = None; score = wedge_score };
    needle =
      { body = Physics.body needle_shape |> Physics.at 300. 0. |> Physics.moving 0. v |> Physics.pointing 90.;
        exploded = None; score = needle_score };
    torpedoes = [];
  }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a pilot's controls: turn left, turn right, thrust, fire *)
type controls = { left : keyboard -> bool; right : keyboard -> bool; forward : keyboard -> bool; fire : keyboard -> bool }

let arrows = { left = (fun k -> k.kleft); right = (fun k -> k.kright); forward = (fun k -> k.kup); fire = (fun k -> k.kdown) }
let wasd = { left = (fun k -> k.ka); right = (fun k -> k.kd); forward = (fun k -> k.kw); fire = (fun k -> k.ks) }

let fly (screen : screen) (keys : keyboard) (c : controls) (s : ship) : ship =
  match s.exploded with
  | Some n -> { s with exploded = Some (n +.. 1) }
  | None ->
      let turning = (if c.left keys then 200. else 0.) - if c.right keys then 200. else 0. in
      let body =
        s.body
        |> Physics.turn turning
        |> Physics.thrust (if c.forward keys then 120. else 0.)
        |> Physics.attracted_by star
        |> Physics.step
        |> Physics.wrap screen
      in
      { s with body }

let torpedo_shape = circle white 2.5

(* a new torpedo from [s], if it fired this frame and has fewer than 4
 * in flight *)
let fire (scenes : model) (c : controls) (owner : pilot) (s : ship) (torpedoes : torpedo list) : torpedo list =
  let mine = List.filter (fun tp -> tp.owner = owner) torpedoes in
  if s.exploded = None && Scene2d.pressed c.fire scenes && List.length mine < 4 then
    [ { t = Physics.body torpedo_shape |> Physics.shot_from 250. 28. s.body; ttl = 180; owner } ]
  else []

let hit_by (things : Physics.body list) (s : ship) : bool =
  s.exploded = None && List.exists (fun b -> Physics.touching b s.body) things

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let screen = computer.screen and keys = computer.keyboard in
  let wedge = fly screen keys arrows g.wedge and needle = fly screen keys wasd g.needle in
  (* the torpedoes fly, fall towards the star, and burn out after 3 s *)
  let torpedoes =
    g.torpedoes
    |> List.map (fun tp -> { tp with t = tp.t |> Physics.attracted_by star |> Physics.step |> Physics.wrap screen; ttl = tp.ttl -.. 1 })
    |> List.filter (fun tp -> tp.ttl > 0 && not (Physics.touching tp.t star))
  in
  let torpedoes = torpedoes @ fire scenes arrows Wedge wedge torpedoes @ fire scenes wasd Needle needle torpedoes in
  let bodies = List.map (fun tp -> tp.t) torpedoes in
  let explode (other : ship) (s : ship) =
    if hit_by [ star ] s || hit_by bodies s || (other.exploded = None && hit_by [ other.body ] s) then
      { s with exploded = Some 0 }
    else s
  in
  let wedge' = explode needle wedge and needle' = explode wedge needle in
  (* the torpedoes that hit something are spent *)
  let spent tp = List.exists (fun s -> s.exploded = Some 0 && Physics.touching tp.t s.body) [ wedge'; needle' ] in
  { wedge = wedge'; needle = needle'; torpedoes = List.filter (fun tp -> not (spent tp)) torpedoes }

(* 1.5 s after a ship blew up, the round ends: the survivor scores *)
let end_of_round (g : game) : game option =
  let over (s : ship) = match s.exploded with Some n -> n > 90 | None -> false in
  if over g.wedge || over g.needle then
    let point (s : ship) = if s.exploded = None then 1 else 0 in
    Some (start_ships (g.wedge.score +.. point g.wedge) (g.needle.score +.. point g.needle))
  else None

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (Playing (start_ships 0 0)) scenes else scenes
  | Playing g -> (
      let g = update_game computer scenes g in
      match end_of_round g with Some g -> Scene2d.go (Playing g) scenes | None -> { scenes with scene = Playing g })

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the night sky: 120 dots from a linear congruential generator (the
 * same every time, no Random) *)
let sky : shape list =
  let rec go n seed acc =
    if n = 0 then acc
    else
      let seed = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff in
      let x = float_of_int (seed mod 1000) - 500. and y = float_of_int (seed /.. 1000 mod 1000) - 500. in
      go (n -.. 1) seed ((circle (rgb 150 150 150) 1. |> move x y) :: acc)
  in
  go 120 7 []

let view_ship (keys : keyboard) (c : controls) (s : ship) : shape list =
  match s.exploded with
  | None ->
      (* the flame, behind, while thrusting *)
      let flame = if c.forward keys then [ polygon orange [ (-10., 0.); (-27., 6.); (-27., -6.) ] |> rotate s.body.angle |> move s.body.x s.body.y ] else [] in
      flame @ [ Physics.draw s.body ]
  | Some n ->
      (* an expanding, fading ring *)
      let r = 5. + float_of_int n in
      [ circle orange r |> fade (max 0. (1. - (float_of_int n / 90.))) |> move s.body.x s.body.y ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let flicker = 18. + (2. * sin (float_of_int model.frames / 4.)) in
  (rectangle black screen.width screen.height :: sky)
  @ [ circle yellow flicker; circle white (flicker / 3.) ]
  @
  match model.scene with
  | Title ->
      [ text white 6. "TINY SPACEWAR!" |> move_y 250.;
        text red 2. "wedge: left/right turn  up thrust  down fire" |> move_y 150.;
        text (rgb 90 150 255) 2. "needle: a/d turn  w thrust  s fire" |> move_y 110. ]
      @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y (-250.) ]
  | Playing g ->
      view_ship computer.keyboard arrows g.wedge
      @ view_ship computer.keyboard wasd g.needle
      @ List.map (fun tp -> Physics.draw tp.t) g.torpedoes
      @ (if List.mem_assoc "hitboxes" computer.flags then
           List.map Physics.debug (star :: g.wedge.body :: g.needle.body :: List.map (fun tp -> tp.t) g.torpedoes)
         else [])
      @ [ text red 3. (Printf.sprintf "WEDGE %d" g.wedge.score) |> move (-350.) 460.;
          text (rgb 90 150 255) 3. (Printf.sprintf "NEEDLE %d" g.needle.score) |> move 350. 460. ]

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
