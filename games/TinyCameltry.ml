(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Cameltry (Taito, arcade, 1989; "On the Ball" on the
 * SNES): you don't move the ball, you turn the maze, and the ball rolls
 * down wherever down now is. Left and right turn the maze, space
 * jumps; touch every green target, as fast as you can. After Florent
 * Monnier's Rolling-Moon (2008, OCaml on the Chipmunk engine), whose
 * ball is a moon: so is this one.
 *
 * What it teaches is rotation (playground/Physics.mli, the phase 7 of
 * docs/claude_notes/plan_physics_teaching.md): the moon isn't upright,
 * so friction at its bottom point makes it roll -- watch its craters
 * turn -- instead of sliding; and turning the maze is only turning
 * gravity. The physics happens in the maze's coordinates, where the
 * walls never move; turning the maze by an angle a on the screen is
 * pulling the moon, in the maze, towards the direction the screen's
 * "down" now points to:
 *
 *        screen                       maze
 *     +---------+ turned by a      gravity (-g sin a, -g cos a):
 *     |  /\  o  |    <=====>       the maze still, the moon pulled
 *     |  \/  |  |                  sideways
 *     +------v--+
 *
 * and the view draws everything turned by a, around the moon.
 *
 * The maze: '#' walls, each row's run of them one immovable body (fewer
 * bodies, and no seams to catch the moon along a floor), 'o' the start,
 * '*' the targets. The flag hitboxes draws what the physics sees.
 *
 * What it uses: no kit, only the Playground, Scene2d (the title and
 * cleared scenes), and the Physics layer. Of the engine underneath
 * (physics/2d/): Body and Integrate (Physics.step, one step of
 * semi-implicit Euler); Shape, Collide and Contact (Physics.touching
 * and bounce_off: a circle against boxes, the contact point where the
 * friction pushes); and Resolve, with its rotation, what makes the moon
 * roll. Not Broadphase: one moving body only, and the walls near it
 * are found by a two-line filter ([near]), the "all pairs, for a few
 * bodies" of notes_2d_physics.md section 9. Not Force.gravitation nor
 * Energy either: gravity is a [push], turned with the maze.
 *
 * Without rotation (the U key during play, or from the start the flag
 * rotation=off:
 *   dune exec games/TinyCameltry.exe -- rotation=off
 * or ?rotation=off on the web), the moon is upright: exactly the
 * engine before rotation, phase 6's formulas (see Physics.upright), and
 * drawn unturned. It can't roll, it slides; and rough walls hold a
 * sliding thing still up to a slope of atan 0.8 = 39 degrees
 * (Coulomb's static friction). Turn the maze a little and nothing
 * moves, then past 39 degrees the moon lets go at once: the game
 * becomes a different, stickier one. A rolling moon
 * moves at the slightest tilt -- what makes Cameltry feel like
 * Cameltry.
 *
 * Left as exercises: more levels (Rolling-Moon's were drawn in
 * Inkscape), the exit and the timer of Cameltry, bumpers (bounciness
 * above 1) and spikes, the best times kept.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The maze *)
(*****************************************************************************)

let level : string list =
  [ "########################";
    "#o     #       #      *#";
    "#      #   *   #       #";
    "#  #####       #   #####";
    "#      #   #####       #";
    "#*     #               #";
    "####   #######   #######";
    "#          *#          #";
    "#   ####    #    ####  #";
    "#   #  #    #    #*    #";
    "#   # *#         #     #";
    "#   ####   ##########  #";
    "#                    * #";
    "########################" ]

let tile = 50.
let rows = List.length level
let cols = String.length (List.hd level)

(* the center of the tile at [row], [col]: the maze centered on (0, 0) *)
let center (row : int) (col : int) : number * number =
  ((float_of_int col - (float_of_int cols / 2.) + 0.5) * tile, ((float_of_int rows / 2.) - float_of_int row - 0.5) * tile)

(* each row's runs of '#', as (row, first column, last column) *)
let runs : (int * int * int) list =
  level
  |> List.mapi (fun row line ->
         let rec go col start acc =
           let wall = col < cols && line.[col] = '#' in
           match (start, wall) with
           | None, true -> go (col +.. 1) (Some col) acc
           | Some s, false -> go (col +.. 1) None ((row, s, col -.. 1) :: acc)
           | _ when col >= cols -> List.rev acc
           | _ -> go (col +.. 1) start acc
         in
         go 0 None [])
  |> List.concat

let walls : Physics.body list =
  List.map
    (fun (row, c0, c1) ->
      let (x0, y) = center row c0 and (x1, _) = center row c1 in
      Physics.body (rectangle (rgb 90 110 160) (x1 - x0 + tile) tile)
      |> Physics.at ((x0 + x1) / 2.) y |> Physics.immovable |> Physics.rough 0.8)
    runs

(* the tiles holding [c] *)
let tiles (c : char) : (number * number) list =
  List.concat (List.mapi (fun row line -> List.filter_map (fun col -> if line.[col] = c then Some (center row col) else None) (List.init cols Fun.id)) level)

let target_shape = circle (rgb 80 220 120) 12.

(* the moon, with two craters, to see it roll *)
let moon : Physics.body =
  let (x, y) = List.hd (tiles 'o') in
  Physics.body (group [ circle (rgb 230 230 200) 16.; circle (rgb 190 190 160) 5. |> move 7. 5.; circle (rgb 190 190 160) 3. |> move (-6.) (-7.) ])
  |> Physics.at x y |> Physics.rough 0.8 |> Physics.bouncy 0.3

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  moon : Physics.body;
  (* the maze's angle on the screen, in degrees *)
  turned : number;
  targets : (number * number) list;
  frames : int;
  (* false: the moon upright, sliding instead of rolling (the key u) *)
  rotation : bool;
}

type scene = Title | Playing of play | Cleared of int (* frames it took *)

type model = scene Scene2d.t

let start (rotation : bool) : play =
  { moon = (if rotation then moon else moon |> Physics.upright); turned = 0.; targets = tiles '*'; frames = 0; rotation }
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let gravity = 900.

(* the walls near the moon: the only ones worth testing *)
let near (m : Physics.body) : Physics.body list =
  List.filter (fun (w : Physics.body) -> Float.abs (w.x - m.x) < 700. && Float.abs (w.y - m.y) < tile) walls

(* a moon slightly bigger, to know whether the moon rests on something *)
let on_something (m : Physics.body) : bool =
  let probe = Physics.body (circle white 19.) |> Physics.at m.x m.y in
  List.exists (Physics.touching probe) (near m)

let update_play (keys : keyboard) (jump : bool) (p : play) : play =
  let turned = p.turned + ((if keys.kleft then 2. else 0.) - if keys.kright then 2. else 0.) in
  let a = turned * Float.pi / 180. in
  (* the screen's down, in the maze *)
  let (dx, dy) = (-.sin a, -.cos a) in
  let moon = p.moon |> Physics.push (gravity * dx) (gravity * dy) |> Physics.step in
  (* jumping: away from down, if on something *)
  let moon = if jump && on_something p.moon then moon |> Physics.moving (moon.vx - (400. * dx)) (moon.vy - (400. * dy)) else moon in
  let moon = List.fold_left (fun m w -> Physics.bounce_off w m) moon (near moon) in
  let targets = List.filter (fun (x, y) -> not (Physics.touching moon (Physics.body target_shape |> Physics.at x y))) p.targets in
  { p with moon; turned; targets; frames = p.frames +.. 1 }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let space = Scene2d.pressed (fun k -> k.kspace) scenes in
  (* rotation=off: the moon upright from the start *)
  let rotation = List.assoc_opt "rotation" computer.flags <> Some "off" in
  match scenes.scene with
  | Title | Cleared _ -> if space then Scene2d.go (Playing (start rotation)) scenes else scenes
  | Playing p ->
      (* u: rotation on or off, the moon upright or not *)
      let p =
        if Scene2d.pressed (fun k -> Set_.mem "u" k.keys) scenes then
          (* upright, it also stops spinning: nothing would change its spin anymore *)
          let moon = if p.rotation then p.moon |> Physics.upright |> Physics.turn 0. else { p.moon with upright = false } in
          { p with rotation = not p.rotation; moon }
        else p
      in
      let p = update_play computer.keyboard space p in
      if p.targets = [] then Scene2d.go (Cleared p.frames) scenes else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (size : number) (s : string) : shape = words white s |> scale size
let seconds (frames : int) : string = Printf.sprintf "%.1f s" (float_of_int frames / 60.)

(* the maze turned by [turned] around the moon, at the screen's center *)
let view_play (hitboxes : bool) (p : play) : shape list =
  let maze =
    List.map Physics.draw walls
    @ List.map (fun (x, y) -> target_shape |> move x y) p.targets
    @ [ Physics.draw p.moon ]
    @ if hitboxes then List.map Physics.debug (p.moon :: near p.moon) else []
  in
  [ group [ group maze |> move (-.p.moon.x) (-.p.moon.y) ] |> rotate p.turned;
    text 3. (Printf.sprintf "%d left   %s" (List.length p.targets) (seconds p.frames)) |> move_y 450. ]
  @ if p.rotation then [] else [ text 2. "rotation off: the moon slides (u)" |> move_y 410. ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 20 20 40) screen.width screen.height
  ::
  (match model.scene with
  | Title ->
      [ text 6. "TINY CAMELTRY" |> move_y 200.;
        text 2. "left/right: turn the maze   space: jump" |> move_y 80.;
        text 2. "touch every green target, fast" |> move_y 40.;
        text 2. "(u: rotation off, the moon slides)" |> move_y 0. ]
      @ Scene2d.blink 1. model [ text 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing p -> view_play (List.mem_assoc "hitboxes" computer.flags) p
  | Cleared frames ->
      [ text 5. "CLEARED!" |> move_y 100.; text 3. (seconds frames) ]
      @ Scene2d.blink 1. model [ text 3. "PRESS SPACE" |> move_y (-200.) ])

(* the keys and flags, printed at launch, to remember them (on the web,
 * in the browser's console) *)
let help =
  {|TinyCameltry
  keys:  left/right  turn the maze
         space       jump (start, restart)
         u           rotation off/on: the moon slides instead of rolling
  flags: rotation=off  the moon upright from the start
         hitboxes      draw what the physics sees
  e.g.   dune exec games/TinyCameltry.exe -- rotation=off hitboxes
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
