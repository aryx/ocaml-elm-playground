(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A small "collect the stars" game -- the *idea* (not the code) is
 * adapted from nateabele's elm-3d-playground README example:
 * https://github.com/nateabele/elm-3d-playground (a package that wraps
 * elm-3d-scene's real WebGL rendering, with actual lighting, in an
 * elm-playground-style `game` API). This is a fresh OCaml
 * implementation of the same idea using this project's own
 * flat-colored, unlit shapes (no real lighting here yet -- see
 * docs/claude_notes/notes_3d.md's section 8) and a real `sphere` for
 * the stars (this file originally used boxes -- Playground3d.sphere
 * didn't exist yet when it was first written -- which is why an
 * earlier version of this comment had to explain why a "star
 * collector" game rendered its stars as cubes).
 *
 * Controls: arrow keys move the player box in the X/Z plane; walk into
 * a star to collect it (it vanishes and the score increments); stars
 * keep respawning at random positions to keep a target count on
 * screen. The score is shown on screen via `hud` (see
 * docs/claude_notes/done/plan_hud.md). *)
open Basics (* elm-core: float +, -, *, /, clamp *)
open Playground
open Playground3d

let play_half_size = 6.
let star_count_target = 10
let collect_distance = 0.8

type star = { sx : number; sz : number }
type model = { player_x : number; player_z : number; stars : star list; score : int }

let () = Random.self_init ()

let random_coord () = Random.float (2. * play_half_size) - play_half_size
let make_star () = { sx = random_coord (); sz = random_coord () }

let init : model =
  { player_x = 0.; player_z = 0.; stars = List.init star_count_target (fun _ -> make_star ()); score = 0 }

let update (computer : Playground.computer) (m : model) : model =
  let (dx, dz) = to_xy computer.keyboard in
  let speed = 0.08 in
  let player_x = clamp (-.play_half_size) play_half_size (m.player_x + (dx * speed)) in
  let player_z = clamp (-.play_half_size) play_half_size (m.player_z - (dz * speed)) in
  let collected, remaining =
    List.partition
      (fun (s : star) ->
        let ddx = s.sx - player_x and ddz = s.sz - player_z in
        sqrt ((ddx * ddx) + (ddz * ddz)) < collect_distance)
      m.stars
  in
  let missing = star_count_target -.. List.length remaining in
  let respawned = List.init missing (fun _ -> make_star ()) in
  { player_x; player_z; stars = remaining @ respawned; score = m.score +.. List.length collected }

let view (computer : Playground.computer) (m : model) : camera * shape3d list =
  let ground = plane green (2. * play_half_size) (2. * play_half_size) in
  let player =
    box blue 0.8 0.8 0.8 |> rotate3d 0. (spin 4. computer.time) 0. |> move3d m.player_x 0.4 m.player_z
  in
  let stars = m.stars |> List.map (fun (s : star) -> sphere yellow 0.3 |> move3d s.sx 0.3 s.sz) in
  let cam = camera ~eye:(m.player_x, 8., m.player_z + 8.) ~target:(m.player_x, 0., m.player_z) () in
  let score_hud =
    hud
      (words black (Printf.sprintf "Score: %d" m.score)
      |> move (computer.screen.left +. 60.) (computer.screen.top -. 40.))
  in
  (cam, (ground :: player :: stars) @ [ score_hud ])

let app = game3d view update init
let main = Playground3d_platform.run_app3d app
