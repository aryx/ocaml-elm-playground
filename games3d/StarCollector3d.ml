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
 * docs/claude_notes/done/plan_hud.md).
 *
 * claude: two ways of moving the player, chosen with the flag
 * physics=engine (?physics=engine in a browser; see Playground.flags),
 * the pattern games/Asteroid.ml, Mario.ml and TinyMario.ml use in 2D:
 *
 *  - by hand, the default and the original code: the arrows set the
 *    position directly, 0.08 units a frame, and the cube is drawn
 *    spinning by the clock. Let go and it stops dead;
 *  - playground3d/Physics3d (the plan's phase 3, its first user): the
 *    arrows *push* a body of 1 kg, drag gives it a top speed instead
 *    of the position doing it, and the same top speed as before comes
 *    out of the balance -- a push of 28.8 N against a drag of 6 per
 *    second balances at 4.8 m/s, which is 0.08 units a frame. Let go
 *    and it slides to a stop; walk into the fence and it stops, the
 *    velocity along that wall kept. The cube's spin is no longer the
 *    clock's: it is the body's own, about a tilted axis, so what is on
 *    screen is a quaternion converted to the three angles rotate3d
 *    takes (Physics3d.draw) -- this game is where that conversion gets
 *    measured on something that moves.
 *
 * The collecting differs too, and that is phase 4's part: by hand it
 * is a distance between two centres, which treats the player as a
 * point; with the engine it is Physics3d.touching, the player's real
 * box -- turned, since it tumbles -- against each star's real sphere.
 * Walk a corner into a star and only one of the two notices.
 *
 * Everything else -- the stars, the score, the camera -- is the same
 * code for both. "d" draws what the engine sees: the hitboxes and the
 * velocity (Physics3d.debug).
 *)
open Basics (* elm-core: float +, -, *, /, clamp *)
open Playground
open Playground3d

let play_half_size = 6.
let star_count_target = 10
let collect_distance = 0.8

type star = { sx : number; sz : number }
type engine = By_hand | Engine

(* the player is a body in both: by hand, only its position is used
 * (the engine never steps it), which keeps one representation of where
 * the player is *)
type model = { player : Physics3d.body; stars : star list; score : int; engine : engine }

(* claude: seed=n (see Playground.flags), e.g. for the golden frame
 * tests: the same stars every run; before init, which draws the first
 * ones *)
let () =
  match List.assoc_opt "seed" (Playground_platform.flags ()) with
  | Some n -> Random.init (int_of_string n)
  | None -> Random.self_init ()

let engine : engine =
  match List.assoc_opt "physics" (Playground_platform.flags ()) with Some "engine" -> Engine | _ -> By_hand

let random_coord () = Random.float (2. * play_half_size) - play_half_size
let make_star () = { sx = random_coord (); sz = random_coord () }

(* the cube, and what the engine knows about it: 1 kg, turning about a
 * tilted axis so that the orientation on screen is a real one *)
let player_shape = box blue 0.8 0.8 0.8

let new_player () : Physics3d.body =
  Physics3d.body player_shape |> Physics3d.at 0. 0.4 0. |> Physics3d.turning (0.4, 1., 0.2) 240.

let init : model =
  { player = new_player (); stars = List.init star_count_target (fun _ -> make_star ()); score = 0; engine }

(* the same top speed as the hand-written one: push / drag = 28.8 / 6 =
 * 4.8 m/s, and 4.8 / 60 = the 0.08 units a frame below *)
let walk_push = 28.8
let walk_drag = 6.

let move_by_hand (computer : computer) (p : Physics3d.body) : Physics3d.body =
  let dx, dz = to_xy computer.keyboard in
  let speed = 0.08 in
  Physics3d.at
    (clamp (-.play_half_size) play_half_size (p.Physics3d.x + (dx * speed)))
    0.4
    (clamp (-.play_half_size) play_half_size (p.Physics3d.z - (dz * speed)))
    p

let move_by_engine (computer : computer) (p : Physics3d.body) : Physics3d.body =
  let dx, dz = to_xy computer.keyboard in
  let p = p |> Physics3d.push (dx * walk_push) 0. (-.dz * walk_push) |> Physics3d.slow walk_drag |> Physics3d.step in
  (* the fence: stopped across it, still sliding along it *)
  let wall v speed = if Float.abs v > play_half_size then (clamp (-.play_half_size) play_half_size v, 0.) else (v, speed) in
  let x, vx = wall p.Physics3d.x p.Physics3d.vx in
  let z, vz = wall p.Physics3d.z p.Physics3d.vz in
  { p with Physics3d.x; z; vx; vz; y = 0.4; vy = 0. }

(* a star, as the engine sees it: a real sphere where it is drawn *)
let star_body (s : star) : Physics3d.body =
  Physics3d.body (sphere yellow 0.3) |> Physics3d.at s.sx 0.3 s.sz |> Physics3d.ball

let update (computer : Playground.computer) (m : model) : model =
  let player = (match m.engine with By_hand -> move_by_hand | Engine -> move_by_engine) computer m.player in
  let px = player.Physics3d.x and pz = player.Physics3d.z in
  let reached (s : star) =
    match m.engine with
    | By_hand ->
        let ddx = s.sx - px and ddz = s.sz - pz in
        sqrt ((ddx * ddx) + (ddz * ddz)) < collect_distance
    | Engine -> Physics3d.touching player (star_body s)
  in
  let collected, remaining = List.partition reached m.stars in
  let missing = star_count_target -.. List.length remaining in
  let respawned = List.init missing (fun _ -> make_star ()) in
  { m with player; stars = remaining @ respawned; score = m.score +.. List.length collected }

let view (computer : Playground.computer) (m : model) : camera * shape3d list =
  let ground = plane green (2. * play_half_size) (2. * play_half_size) in
  let px = m.player.Physics3d.x and pz = m.player.Physics3d.z in
  let player =
    match m.engine with
    | By_hand -> box blue 0.8 0.8 0.8 |> rotate3d 0. (spin 4. computer.time) 0. |> move3d px 0.4 pz
    | Engine -> Physics3d.draw m.player
  in
  let debug = if Set_.mem "d" computer.keyboard.keys then [ Physics3d.debug m.player ] else [] in
  let stars = m.stars |> List.map (fun (s : star) -> sphere yellow 0.3 |> move3d s.sx 0.3 s.sz) in
  let cam = camera ~eye:(px, 8., pz + 8.) ~target:(px, 0., pz) () in
  let score_hud =
    hud
      (words black (Printf.sprintf "Score: %d" m.score)
      |> move (computer.screen.left +. 60.) (computer.screen.top -. 40.))
  in
  let engine_hud =
    match m.engine with
    | By_hand -> []
    | Engine ->
        [ hud
            (words black (Printf.sprintf "physics=engine   %.1f m/s   (d: what it sees)" (Physics3d.speed m.player))
            |> move (computer.screen.left +. 220.) (computer.screen.bottom +. 30.)) ]
  in
  (cam, (ground :: player :: stars) @ debug @ [ score_hud ] @ engine_hud)

let app = game3d view update init
let main = Playground3d_platform.run_app3d app
