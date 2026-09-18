(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy artillery game, for two players on one keyboard, taking turns:
 * aim your cannon (left/right), choose the power (up/down), fire
 * (space), and let gravity and the wind do the rest. The shell flies in
 * a parabola, pushed sideways by a wind that changes every turn; where
 * it lands, it digs a crater, and hurts whoever is near.
 *
 * The genre is one of the oldest: "Artillery" games ran on mainframes
 * and the first home computers in the late 1970s (two cannons, a hill,
 * type an angle and a speed); Gorillas (1991, shipped with MS-DOS's
 * QBasic) threw exploding bananas between skyscrapers, with wind;
 * Scorched Earth (Wendell Hicks, 1991) gave tanks a terrain that
 * explosions carve away, as here; Worms (Andy Davidson, Team17, 1995)
 * made it a cartoon war, with caves in its terrain.
 *
 * What it teaches is physics, in the playground's Physics layer
 * (playground/Physics.mli): the shell is a body, [launched] at an angle
 * and a speed, and at every tick
 *
 *     shell |> fall gravity |> push wind 0. |> step
 *
 * -- the projectile of docs/claude_notes/notes_2d_physics.md section 4
 * (a parabola, bent by a constant sideways push, the wind), stepped
 * with semi-implicit Euler. Everything else is ordinary game code: the
 * terrain is a height map (one height per column of pixels, as in
 * Scorched Earth: no caves), and a crater is a circle cut out of it.
 * The hills are sines with random phases: pass seed=n (see
 * Playground.flags) for the same hills every time.
 *
 * Left as exercises: caves (the terrain as a bitmap, a Worms kit in
 * docs/claude_notes/plan_games.md), wind as air drag rather than a push
 * (Physics.slow, relative to the moving air), a computer opponent
 * (aiming by simulating shots: a search, see plan_teaching_other.md's
 * game AI), the dotted predicted path of the shot (the parabola,
 * computed ahead), and the sounds (plan_audio_teaching.md).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The terrain: a height map *)
(*****************************************************************************)

(* one height for every [column] pixels, from the screen's left to its
 * right edge: the ground is below the height, the sky above *)
let column = 4.
let columns = 251 (* 1000 / 4 + 1 *)
let left = -500.
let x_of i = left + (float_of_int i * column)
let index_of x = clamp 0 (columns -.. 1) (round ((x - left) / column))

type terrain = number array

(* rolling hills: three sines of different wavelengths, with random
 * phases *)
let hills () : terrain =
  let phase () = Random.float (2. * Float.pi) in
  let p1 = phase () and p2 = phase () and p3 = phase () in
  Array.init columns (fun i ->
      let x = x_of i in
      -150. + (90. * sin ((x / 170.) + p1)) + (45. * sin ((x / 70.) + p2)) + (15. * sin ((x / 25.) + p3)))

let height_at (t : terrain) (x : number) : number = t.(index_of x)

(* a crater: the circle of radius r at (cx, cy) cut out of the ground;
 * a column under the circle comes down to the circle's bottom *)
let carve (t : terrain) (cx : number) (cy : number) (r : number) : terrain =
  Array.mapi
    (fun i h ->
      let dx = x_of i - cx in
      if abs_float dx >= r then h
      else
        let bottom = cy - sqrt ((r * r) - (dx * dx)) in
        if h > bottom then bottom else h)
    t

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type player = {
  px : number; (* where it stands; its y is the terrain's height there *)
  angle : number; (* of the cannon, in degrees: 0 right, 90 up *)
  power : number; (* 10 to 100 *)
  health : number; (* 100, down to 0 *)
  color : color;
}

type flight =
  | Aiming
  (* the shell, and the dots of its trail *)
  | Flying of Physics.body * (number * number) list
  (* where, and how many frames the explosion still lasts *)
  | Exploding of number * number * int

type game = {
  terrain : terrain;
  players : player array; (* 2 *)
  turn : int; (* whose: 0 or 1 *)
  wind : number; (* a push to the right (left if negative), px/s^2 *)
  flight : flight;
}

type scene = Title | Playing of game | Winner of int

type model = scene Scene2d.t

let gravity = 400.
let blast = 50. (* the explosion's radius *)
let new_wind () = Random.float 300. - 150.

let new_game () : game =
  {
    terrain = hills ();
    players =
      [|
        { px = -350.; angle = 45.; power = 60.; health = 100.; color = red };
        { px = 350.; angle = 135.; power = 60.; health = 100.; color = blue };
      |];
    turn = 0;
    wind = new_wind ();
    flight = Aiming;
  }

let initial_model : model = Scene2d.start Title

(* where a player's cannon is, a bit above the ground *)
let cannon_of (g : game) (p : player) : number * number = (p.px, height_at g.terrain p.px + 15.)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let aim (computer : computer) (p : player) : player =
  {
    p with
    angle = clamp 0. 180. (p.angle - to_x computer.keyboard);
    power = clamp 10. 100. (p.power + (0.5 * to_y computer.keyboard));
  }

let fire (g : game) : game =
  let p = g.players.(g.turn) in
  let (x, y) = cannon_of g p in
  (* out of the barrel, 30 pixels along it *)
  let shell =
    Physics.body (circle black 5.)
    |> Physics.at (x + (30. * cos (p.angle * Float.pi / 180.))) (y + (30. * sin (p.angle * Float.pi / 180.)))
    |> Physics.launched (p.power * 10.) p.angle
  in
  { g with flight = Flying (shell, []) }

(* the shell hits: the ground under it, or a player near it *)
let hits (g : game) (shell : Physics.body) : bool =
  shell.y < height_at g.terrain shell.x
  || Array.exists
       (fun p ->
         let (x, y) = cannon_of g p in
         Float.hypot (shell.x - x) (shell.y - y) < 15.)
       g.players

(* the damage: up to 60 at the center, nothing beyond 60 pixels *)
let explode (g : game) (cx : number) (cy : number) : game =
  let players =
    Array.map
      (fun p ->
        let (x, y) = cannon_of g p in
        let d = Float.hypot (x - cx) (y - cy) in
        { p with health = max 0. (p.health - max 0. (60. - d)) })
      g.players
  in
  { g with terrain = carve g.terrain cx cy blast; players; flight = Exploding (cx, cy, 30) }

let next_turn (g : game) : game = { g with turn = 1 -.. g.turn; wind = new_wind (); flight = Aiming }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  match g.flight with
  | Aiming ->
      let players = Array.copy g.players in
      players.(g.turn) <- aim computer g.players.(g.turn);
      let g = { g with players } in
      if Scene2d.pressed (fun k -> k.kspace) scenes then fire g else g
  | Flying (shell, trail) ->
      let shell = shell |> Physics.fall gravity |> Physics.push g.wind 0. |> Physics.step in
      let trail = if scenes.frames mod 3 = 0 then (shell.x, shell.y) :: trail else trail in
      if hits g shell then explode g shell.x shell.y
      (* off the sides, or fallen through: a miss (up is fine, it comes
       * back down) *)
      else if abs_float shell.x > 550. || shell.y < -600. then next_turn g
      else { g with flight = Flying (shell, trail) }
  | Exploding (x, y, n) -> if n > 0 then { g with flight = Exploding (x, y, n -.. 1) } else next_turn g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let space = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) scenes else scenes
  | Playing g -> (
      let g = update_game computer scenes g in
      match (g.flight, Array.to_list g.players |> List.map (fun p -> p.health)) with
      | Aiming, [ h0; _ ] when h0 <= 0. -> Scene2d.go (Winner 1) scenes
      | Aiming, [ _; h1 ] when h1 <= 0. -> Scene2d.go (Winner 0) scenes
      | _ -> { scenes with scene = Playing g })
  | Winner _ -> if space && scenes.elapsed > 1. then Scene2d.go Title scenes else scenes

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the ground: one polygon, the heights and the bottom corners *)
let view_terrain (t : terrain) : shape =
  polygon (rgb 90 140 60) ((500., -500.) :: (-500., -500.) :: List.init columns (fun i -> (x_of i, t.(i))))

let view_player (g : game) (p : player) : shape =
  let (x, y) = cannon_of g p in
  group [ circle p.color 15.; group [ rectangle black 30. 6. |> move_x 15. ] |> rotate p.angle ] |> move x y

let view_hud (g : game) : shape list =
  let bar i (p : player) =
    let x = if i = 0 then -350. else 350. in
    [ rectangle (rgb 60 60 60) 204. 24. |> move x 460.;
      rectangle p.color (2. * p.health) 20. |> move (x - 100. + p.health) 460. ]
  in
  let p = g.players.(g.turn) in
  (* the wind: an arrow as long as it is strong *)
  let arrow = group [ rectangle white (abs_float g.wind / 2.) 6.; triangle white 8. |> rotate (if g.wind < 0. then 180. else 0.) |> move_x (g.wind / 4.) ] in
  List.concat (List.mapi bar (Array.to_list g.players))
  @ [ text white 2. (Printf.sprintf "WIND %+.0f" g.wind) |> move 0. 470.;
      arrow |> move 0. 440.;
      text p.color 2. (Printf.sprintf "PLAYER %d   ANGLE %.0f   POWER %.0f" (g.turn +.. 1) p.angle p.power) |> move 0. (-470.) ]

let view_game (g : game) : shape list =
  (view_terrain g.terrain :: List.map (view_player g) (Array.to_list g.players))
  @ (match g.flight with
    | Aiming -> []
    | Flying (shell, trail) -> Physics.draw shell :: List.map (fun (x, y) -> circle white 2. |> move x y) trail
    | Exploding (x, y, n) ->
        let r = blast * (1. - (float_of_int n / 30.)) in
        [ circle orange r |> fade 0.8 |> move x y; circle yellow (r / 2.) |> move x y ])
  @ view_hud g

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let sky = rectangle (rgb 110 170 230) screen.width screen.height in
  sky
  ::
  (match model.scene with
  | Title ->
      [ text white 6. "TINY WORMS" |> move_y 200.;
        text white 2. "two players, taking turns" |> move_y 80.;
        text white 2. "left/right: aim   up/down: power   space: fire" |> move_y 20. ]
      @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y (-150.) ]
  | Playing g -> view_game g
  | Winner i ->
      [ text white 5. (Printf.sprintf "PLAYER %d WINS" (i +.. 1)) |> move_y 100. ]
      @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ])

let app = game view update initial_model

let main =
  (* seed=n (see Playground.flags): the same hills and winds every run,
   * e.g. for golden frames *)
  (match List.assoc_opt "seed" (Playground_platform.flags ()) with
  | Some n -> Random.init (int_of_string n)
  | None -> Random.self_init ());
  Playground_platform.run_app app
