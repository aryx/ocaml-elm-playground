(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* games/TinyTron.ml in 3D: the same light cycles, the same rules, the
 * same computer, but the trails are walls of light standing on the
 * grid, and you can look at them from anywhere. v changes the view:
 *   - behind the blue cycle, the film's shot (Tron, 1982: its light
 *     cycle scene, one of the first long computer-animated sequences in
 *     a feature film, by MAGI's Synthavision);
 *   - inside it, the rider's eyes, the other cycle's wall rushing at
 *     you;
 *   - from above, the arcade's view, and TinyTron's;
 *   - from far away and high up, with a narrow field of view: almost
 *     isometric, the look of the 3D-looking 2D games of the 1980s
 *     (Zaxxon, Marble Madness), perspective barely showing.
 *
 * The model and the update are the light cycles kit's
 * (kits/lightcycles/), shared with TinyTron: this file is only another
 * view of the same model -- the Elm architecture's promise, kept. Each
 * trail is drawn from its corners (where the cycle turned): one box per
 * straight part, a few dozen walls in all, instead of one box per cell.
 *)
open Playground
open Playground3d
open Lightcycles (* the game: its rules, the computer, the scenes *)

(*****************************************************************************)
(* The arena in space *)
(*****************************************************************************)

(* a cell is 1 unit; the arena's center at the origin; rows go towards +z *)
let half = float_of_int size /. 2.
let at_cell (c : int) (r : int) : number * number = (float_of_int c +. 0.5 -. half, float_of_int r +. 0.5 -. half)

let blue3 = rgb 60 200 255
let orange3 = rgb 255 150 40

(* a grid on the floor, a line every 9 cells (flat quads just above
 * it), and the arena's walls *)
let arena_shapes : shape3d =
  let n = float_of_int size in
  let line x1 z1 x2 z2 = polygon3d (rgb 30 50 110) [ (x1, 0.01, z1); (x2, 0.01, z1); (x2, 0.01, z2); (x1, 0.01, z2) ] in
  let lines =
    List.concat
      (List.init 9 (fun i ->
           let p = -.half +. (float_of_int (i + 1) *. 9.) in
           [ line (p -. 0.05) (-.half) (p +. 0.05) half; line (-.half) (p -. 0.05) half (p +. 0.05) ]))
  in
  let wall w d = box (rgb 40 60 140) w 2. d in
  cached3d
    (lines
    @ [ wall n 1. |> move3d 0. 1. (-.half +. 0.5); wall n 1. |> move3d 0. 1. (half -. 0.5);
        wall 1. n |> move3d (-.half +. 0.5) 1. 0.; wall 1. n |> move3d (half -. 0.5) 1. 0. ])

(* a trail, from its corners to the cycle: one wall per straight part *)
let trail (color : color) (c : cycle) : shape3d list =
  let points = (c.col, c.row) :: c.corners in
  let rec walls = function
    | (c1, r1) :: ((c2, r2) :: _ as rest) ->
        let x1, z1 = at_cell c1 r1 and x2, z2 = at_cell c2 r2 in
        let w = Float.abs (x2 -. x1) +. 0.3 and d = Float.abs (z2 -. z1) +. 0.3 in
        (box color w 1.2 d |> move3d ((x1 +. x2) /. 2.) 0.6 ((z1 +. z2) /. 2.)) :: walls rest
    | _ -> []
  in
  walls points

let heading (d : dir) : number = match d with Up -> 0. | Right -> 90. | Down -> 180. | Left -> 270.

(* a light cycle, facing -z: a body, a canopy, glowing wheels *)
let cycle_shape (color : color) (c : cycle) : shape3d =
  let x, z = at_cell c.col c.row in
  group3d
    [ box color 0.6 0.5 1.8 |> move_y3d 0.45; box (rgb 20 20 30) 0.45 0.3 0.8 |> move3d 0. 0.8 0.2;
      box white 0.7 0.35 0.35 |> move3d 0. 0.2 (-0.75); box white 0.7 0.35 0.35 |> move3d 0. 0.2 0.75 ]
  |> rotate3d 0. (-.heading c.dir) 0.
  |> move3d x 0. z

(*****************************************************************************)
(* The views *)
(*****************************************************************************)

type view = Chase | Inside | Above | Far

let view_name = function Chase -> "BEHIND" | Inside -> "INSIDE" | Above -> "ABOVE" | Far -> "FAR"
let next_view = function Chase -> Inside | Inside -> Above | Above -> Far | Far -> Chase

(* the view, and the camera, smoothed (see [update]), are the only state
 * this game adds to the kit's model: kept next to it *)
type model = { game : Lightcycles.model; view : view; cam : camera option }

(* the blue cycle, as a pose for Camera3d *)
let pose_of (c : cycle) : Camera3d.pose =
  let x, z = at_cell c.col c.row in
  { x; y = 0.; z; heading = heading c.dir }

let camera_for (view : view) (c : cycle) : camera =
  match view with
  | Chase -> Camera3d.chase (pose_of c)
  | Inside -> Camera3d.cockpit (pose_of c)
  | Above -> Camera3d.looking_down ~fov:45. ~height:115. (0., 0., 0.)
  | Far -> Camera3d.from_far ~fov:13. ~offset:(-300., 300., 300.) (0., 0., 0.)

(*****************************************************************************)
(* Update and view *)
(*****************************************************************************)

(* The camera follows, smoothed (Camera3d.follow): when the cycle turns
 * 90 degrees, the camera swings around it in a few frames instead of
 * jumping, and changing the view glides from one to the other *)
let update (computer : computer) (m : model) : model =
  let game = Lightcycles.update computer m.game in
  let v = Scene2d.pressed (fun k -> Set_.mem "v" k.keys) game in
  let view = if v then next_view m.view else m.view in
  let cam =
    match game.scene with
    | Title -> None
    | Playing g | Winner g -> (
        let wanted = camera_for view g.round.p1 in
        match m.cam with None -> Some wanted | Some cam -> Some (Camera3d.follow 0.2 wanted cam))
  in
  { game; view; cam }

let text color size str = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let s = m.game in
  match s.scene with
  | Title ->
      (* the arena seen turning, from high up *)
      let cam = Camera3d.orbit ~distance:90. ~height:60. ~look:0. (spin 20. computer.time) (0., 0., 0.) in
      ( cam,
        Camera3d.sky cam
        @ [ Camera3d.floor cam; arena_shapes;
          hud (text blue3 8. "TINY TRON 3D" |> move_y 250.);
          hud (text white 3. "1: against the computer" |> move_y 60.);
          hud (text white 3. "2: two players" |> move_y 0.);
          hud (text gray 2.5 "blue: arrows   orange: w/a/s/d   v: view" |> move_y (-80.)) ]
        @ List.map hud (Scene2d.blink 1. s [ text orange3 3. "PRESS 1 OR 2" |> move_y (-200.) ]) )
  | Playing g | Winner g ->
      let r = g.round in
      let cam = match m.cam with Some cam -> cam | None -> camera_for m.view r.p1 in
      let horizon = match m.view with Chase | Inside -> Camera3d.sky cam | Above | Far -> [] in
      let result =
        match (s.scene, r.over) with
        | Winner _, _ ->
            [ (if g.score1 > g.score2 then text blue3 7. "BLUE WINS!"
               else text orange3 7. (if g.computer then "THE COMPUTER WINS!" else "ORANGE WINS!"))
              |> move_y 150. ]
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-150.) ]
        | _, Some (1, 0) -> [ text blue3 5. "BLUE WINS THE ROUND" ]
        | _, Some (0, 1) -> [ text orange3 5. "ORANGE WINS THE ROUND" ]
        | _, Some _ -> [ text white 5. "BOTH CRASH" ]
        | _, None -> []
      in
      let huds =
        [ text blue3 3. (Printf.sprintf "BLUE %d" g.score1) |> move (-300.) (screen.top -. 30.);
          text orange3 3. (Printf.sprintf "%s %d" (if g.computer then "COMPUTER" else "ORANGE") g.score2) |> move 300. (screen.top -. 30.);
          text gray 2. ("view: " ^ view_name m.view ^ "   (v)") |> move_y (screen.bottom +. 30.) ]
        @ result
      in
      (* inside the blue cycle, it's not seen *)
      let cycles = (if m.view = Inside then [] else [ cycle_shape blue3 r.p1 ]) @ [ cycle_shape orange3 r.p2 ] in
      ( cam,
        horizon @ [ Camera3d.floor cam; arena_shapes ] @ trail blue3 r.p1 @ trail orange3 r.p2 @ cycles @ List.map hud huds )

let app = game3d view update { game = Lightcycles.initial_model; view = Chase; cam = None }

(* flat shading; the back faces drawn too, for the sky (see Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
