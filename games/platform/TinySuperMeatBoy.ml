(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Meat Boy (Edmund McMillen and Tommy Refenes,
 * Team Meat, 2010): a cube of meat runs, jumps and wall-jumps through
 * tiny rooms full of saws to reach Bandage Girl, dies a hundred times
 * on the way, and at the end watches all hundred tries at once.
 *
 *   left right   run (it takes a moment to get going, and to stop)
 *   space        jump -- hold it longer to jump higher; against a
 *                wall, jump off it
 *
 * Super Meat Boy grew out of Meat Boy, a Flash game (McMillen and
 * Jonathan McEntee, 2008), and is one of the three games of Indie Game:
 * The Movie (Lisanne Pajot and James Swirsky, 2012), the documentary
 * that followed Team Meat to its release, Phil Fish finishing Fez, and
 * Jonathan Blow after Braid -- TinyFez.ml and TinyBraid.ml are the other
 * two. Its first level is called "Hello World", and so is this one's.
 * (Names and dates from memory, to check.)
 *
 * It is famously hard, and what makes that bearable is its lesson:
 *
 *   - dying costs nothing. No lives, no reload, no walk back: a
 *     quarter of a second of splat ([dying_frames]) and you are at the
 *     start of a room that takes ten seconds to cross. The difficulty
 *     is the room, never the punishment;
 *   - the rooms remember you: the meat smears every wall and floor it
 *     touches ([smear]), and the smears stay from one try to the next,
 *     so the level fills up with the paths you tried;
 *   - and at the end of a room, the replay: every try, played again at
 *     the same time -- a crowd of Meat Boys, all but one of them
 *     splatting, one after the other, at the places you learned. The
 *     hundred deaths become the story of how you got there.
 *
 * The replay is the trick worth seeing, and the reason it costs almost
 * nothing: a try is kept as its *inputs*, three fields a frame, not as
 * pictures or positions. [step] is a function -- the same inputs from
 * the same start give the same run, every time, frame for frame, the
 * saws included (they move with the try's own frame count) -- so a
 * try is replayed by feeding its inputs to [step] again ([ghosts]).
 * Doom's demo files (1993) are the famous example, and a fighting
 * game's rollback netcode and a speedrun's tool-assisted runs are the
 * same idea. Compare TinyBraid.ml, which keeps whole models instead:
 * it runs time backwards, and a pure step can't be run backwards, so
 * rewinding must remember where it was; replaying forward only needs
 * to remember what was pressed.
 *
 * The other half is the feel, the same kind of numbers as TinyCeleste's
 * but tuned the other way: momentum ([run_accel], [air_brake] -- letting
 * go of the arrow in the air barely slows you), a jump cut short by
 * letting go ([cut]), a slide down walls and a jump off them.
 *
 * What it uses: gamekits/platformer's Tile_move (the movement, a pixel
 * at a time, x then y), Tilemap (the rooms, as strings), Scene2d. Not
 * Sprite: Meat Boy is a red square with eyes, which is nearly what the
 * original draws. Not Camera2d: every room is one screen. Not Physics:
 * the saws are circles on fixed paths, and nothing pushes back.
 *
 * Exercises: the dark world (each room again, harder); Bandage Girl
 * carried off by Dr. Fetus at the end of each room; the bandages,
 * optional and out of the way; the warp zones into Game Boy-looking
 * rooms, Super Meat Boy's tribute to the games before it; the running
 * button, held to go faster; crumbling blocks; and keeping the
 * attempts of the whole game, to replay the whole thing.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The rooms *)
(*****************************************************************************)

(* a room is 24 x 18 tiles, one screen *)
let tile = 40.

(* where the center of cell (col, row) is, in world coordinates, for
 * fractional cells too (a saw between two tiles) *)
let spot (col : number) (row : number) : number * number = ((col * tile) - 460., 340. - (row * tile))

(* A saw is a circle and where it is at each frame of a try: a function
 * of the frame, so that a try replayed meets the saws where it met
 * them. *)
type saw = { at : int -> number * number; radius : number }

let fixed (at : number * number) (radius : number) : saw = { at = (fun _ -> at); radius }

(* back and forth between [a] and [b], slowing at the ends, once every
 * [period] frames, starting [phase] (0 to 1) of the way through *)
let swinging (x1, y1) (x2, y2) (period : number) (phase : number) (radius : number) : saw =
  { at =
      (fun f ->
        let t = (1. - cos (2. * Float.pi * ((float_of_int f / period) + phase))) / 2. in
        (x1 + ((x2 - x1) * t), y1 + ((y2 - y1) * t)));
    radius }

type level = { name : string; map : Tilemap.t; saws : saw list }

let wall = "########################"
let side = "#......................#"

(* a jump over a pit with a saw in it, and up a step to Bandage Girl *)
let hello_world =
  { name = "1-1 Hello World";
    map =
      Tilemap.of_strings tile
        ([ wall ] @ List.init 11 (fun _ -> side)
        @ [ "#................##....#";
            "#..M.............##..B.#";
            "#########....###########";
            "#########....###########";
            "#########....###########";
            wall ]);
    saws = [ fixed (spot 10.5 15.5) 75. ] }

(* up the shaft from wall to wall, with a saw coming up after you out
 * of the floor *)
let wall_to_wall =
  { name = "1-2 Wall to Wall";
    map =
      Tilemap.of_strings tile
        ([ wall; side; side; "#.................B....#" ]
        @ List.init 10 (fun _ -> "##########....##########")
        @ [ "##########.M..##########"; wall; wall; wall ]);
    saws = [ swinging (spot 11.5 16.5) (spot 11.5 1.) 900. 0. 55. ] }

(* three saws swinging down onto the path, out of step: go when they
 * are up *)
let pendulums =
  { name = "1-3 Pendulums";
    map = Tilemap.of_strings tile ([ wall ] @ List.init 13 (fun _ -> side) @ [ "#.M..................B.#"; wall; wall; wall ]);
    saws =
      List.mapi
        (fun i col -> swinging (spot col 3.) (spot col 14.) 110. (float_of_int i * 0.3) 45.)
        [ 7.; 12.; 17. ] }

let levels = [| hello_world; wall_to_wall; pendulums |]

let solid (c : char) : bool = c = '#'
let bandage_girl (c : char) : bool = c = 'B'

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* Meat Boy, during one try *)
type play = {
  x : number;
  y : number;
  vx : number;
  vy : number;
  facing : number;
  airborne : int; (* frames since the feet last touched the ground *)
  held : bool; (* the jump key, last frame *)
  wall_lock : int; (* frames after a wall jump in which you cannot steer back *)
  dead : bool;
  frame : int; (* since the try began: the saws' clock *)
}

(* what the player does in a frame: all a try is made of *)
type input = { dx : number; jump : bool; jump_held : bool }

let nothing = { dx = 0.; jump = false; jump_held = false }

(* a room being played *)
type run = {
  level : int;
  play : play;
  splat : int; (* frames of dying left, 0 while alive *)
  inputs : input list; (* this try's, the last first *)
  tries : input list list; (* the tries that died, each in order *)
  smears : (number * number) list;
  deaths : int; (* the whole game's *)
}

(* the room crossed: every try at once. A ghost is a try being played
 * again, with the inputs it has left. *)
type replay = {
  r_level : int;
  ghosts : (play * input list) list;
  r_frame : int;
  r_deaths : int;
  r_smears : (number * number) list;
}

type scene = Title | Running of run | Replaying of replay | Rescued of int
type model = scene Scene2d.t

let size = (26., 26.)

let start_of (l : level) : play =
  let x, y = match Tilemap.find l.map 'M' with (c, r) :: _ -> Tilemap.center l.map c r | [] -> (0., 0.) in
  { x; y; vx = 0.; vy = 0.; facing = 1.; airborne = 0; held = false; wall_lock = 0; dead = false; frame = 0 }

let enter (level : int) (deaths : int) : run =
  { level; play = start_of levels.(level); splat = 0; inputs = []; tries = []; smears = []; deaths }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The feel *)
(*****************************************************************************)

let gravity = 1.0
let max_fall = 14.
let run_top = 8.5
let run_accel = 0.9
let air_accel = 0.75
let ground_brake = 1.3
let air_brake = 0.25 (* momentum: in the air, letting go barely slows you *)
let jump_speed = 15.
let slide_speed = 3.5
let coyote_frames = 3
let dying_frames = 15

(* against a wall, on the left (-1) or the right (1), or not *)
let wall_side (map : Tilemap.t) (p : play) : number =
  if Tile_move.hits solid map size (p.x + 1.) p.y then 1.
  else if Tile_move.hits solid map size (p.x - 1.) p.y then -1.
  else 0.

(* let go of jump on the way up and the rise is cut *)
let cut (held : bool) (p : play) : play = if p.held && (not held) && p.vy > 0. then { p with vy = p.vy * 0.5 } else p

(* a saw touches the box if the box's nearest point to its center is
 * within its radius *)
let touches (f : int) (p : play) (s : saw) : bool =
  let cx, cy = s.at f in
  let w, h = size in
  let nx = Float.max (p.x - (w / 2.)) (Float.min cx (p.x + (w / 2.))) in
  let ny = Float.max (p.y - (h / 2.)) (Float.min cy (p.y + (h / 2.))) in
  Float.hypot (nx - cx) (ny - cy) < s.radius

(*****************************************************************************)
(* A frame of a try *)
(*****************************************************************************)

(* A function of the room, the input and Meat Boy, and nothing else --
 * no clock, no randomness -- which is all the replay needs. *)
let step (l : level) (i : input) (p : play) : play =
  if p.dead then p
  else
    let map = l.map in
    let on_ground = p.airborne = 0 in
    (* running, with momentum *)
    let target = if p.wall_lock > 0 then p.vx else i.dx * run_top in
    let rate =
      match (i.dx = 0., on_ground) with
      | true, true -> ground_brake
      | true, false -> air_brake
      | false, true -> run_accel
      | false, false -> air_accel
    in
    let vx = if p.vx < target then Float.min target (p.vx + rate) else Float.max target (p.vx - rate) in
    let vy = Float.max (-.max_fall) (p.vy - gravity) in
    let side = wall_side map p in
    (* sliding down a wall you push against *)
    let vy = if side <> 0. && i.dx = side && vy < 0. then Float.max vy (-.slide_speed) else vy in
    let p = { p with vx; vy } in
    (* jumping, off the ground or off a wall *)
    let p =
      if i.jump && p.airborne <= coyote_frames then { p with vy = jump_speed; airborne = coyote_frames +.. 1 }
      else if i.jump && side <> 0. then
        { p with vy = jump_speed * 0.95; vx = -.side * 9.; wall_lock = 6; facing = -.side }
      else p
    in
    let p = cut i.jump_held p in
    let (x, y), hit_x = Tile_move.move_by solid map size (p.x, p.y) (p.vx, 0.) in
    let p = { p with x; y; vx = (if hit_x then 0. else p.vx) } in
    let (x, y), hit_y = Tile_move.move_by solid map size (p.x, p.y) (0., p.vy) in
    let p = { p with x; y; vy = (if hit_y then 0. else p.vy) } in
    let frame = p.frame +.. 1 in
    { p with
      frame;
      airborne = (if Tile_move.on_ground solid map size p.x p.y then 0 else p.airborne +.. 1);
      wall_lock = max 0 (p.wall_lock -.. 1);
      facing = (if i.dx <> 0. && p.wall_lock = 0 then i.dx else p.facing);
      held = i.jump_held;
      dead = List.exists (touches frame p) l.saws }

let rescued (l : level) (p : play) : bool = (not p.dead) && Tile_move.hits bandage_girl l.map size p.x p.y

(*****************************************************************************)
(* Tries, smears and the replay *)
(*****************************************************************************)

(* where the meat touches: under the feet, or on the wall's side; and a
 * burst of it where it dies *)
let smear (l : level) (r : run) : (number * number) list =
  let p = r.play and w, h = size in
  let side = wall_side l.map p in
  let fresh (x, y) = match r.smears with (x', y') :: _ -> Float.hypot (x - x') (y - y') > 6. | [] -> true in
  let at =
    if p.dead then
      List.init 8 (fun k ->
          let a = float_of_int ((k *.. 7) +.. r.deaths) in
          (p.x + (cos a * 22.), p.y + (sin a * 22.)))
    else if p.airborne = 0 then [ (p.x, p.y - (h / 2.)) ]
    else if side <> 0. then [ (p.x + (side * w / 2.), p.y) ]
    else []
  in
  let added = List.filter fresh at in
  (* the oldest go when there are too many to draw *)
  List.filteri (fun k _ -> k < 1500) (added @ r.smears)

(* one frame of a room: a step of the try, or of the splat, and when the
 * splat is over a new try from the start, the dead one kept *)
let advance (i : input) (r : run) : run =
  let l = levels.(r.level) in
  if r.splat > 0 then
    if r.splat = 1 then
      { r with play = start_of l; splat = 0; inputs = []; tries = List.rev r.inputs :: r.tries }
    else { r with splat = r.splat -.. 1 }
  else
    let r = { r with play = step l i r.play; inputs = i :: r.inputs } in
    let r = { r with smears = smear l r } in
    if r.play.dead then { r with splat = dying_frames; deaths = r.deaths +.. 1 } else r

(* every try, the one that made it too, from the start *)
let ghosts (r : run) : replay =
  let start = start_of levels.(r.level) in
  { r_level = r.level;
    ghosts = List.map (fun inputs -> (start, inputs)) (List.rev (List.rev r.inputs :: r.tries));
    r_frame = 0;
    r_deaths = r.deaths;
    r_smears = r.smears }

let replay_step (rp : replay) : replay =
  let l = levels.(rp.r_level) in
  { rp with
    ghosts = List.map (function p, i :: rest -> (step l i p, rest) | g -> g) rp.ghosts;
    r_frame = rp.r_frame +.. 1 }

let replay_over (rp : replay) : bool = List.for_all (fun (_, rest) -> rest = []) rp.ghosts

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  let space = pressed (fun k -> k.kspace) in
  match scenes.scene with
  | Title | Rescued _ -> if space then Scene2d.go (Running (enter 0 0)) scenes else scenes
  | Running r ->
      let k = computer.keyboard in
      let r = advance { dx = to_x k; jump = space; jump_held = k.kspace } r in
      if rescued levels.(r.level) r.play then Scene2d.go (Replaying (ghosts r)) scenes
      else { scenes with scene = Running r }
  | Replaying rp ->
      if replay_over rp && space then
        if rp.r_level +.. 1 < Array.length levels then Scene2d.go (Running (enter (rp.r_level +.. 1) rp.r_deaths)) scenes
        else Scene2d.go (Rescued rp.r_deaths) scenes
      else { scenes with scene = Replaying (replay_step rp) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let dusk = rgb 64 36 40
let ground = rgb 128 84 62
let grass = rgb 150 170 80
let meat = rgb 200 30 30
let blood = rgb 150 10 20

let tile_shape (c : char) : shape =
  match c with
  | '#' -> group [ square ground tile; square (rgb 100 64 50) (tile - 10.) ]
  | _ -> group []

(* a disc with teeth, turning *)
let saw_shape (frame : int) (s : saw) : shape =
  let x, y = s.at frame in
  group
    ([ circle (rgb 90 90 100) (s.radius + 4.) ]
    @ List.init 8 (fun k ->
          rectangle (rgb 170 170 180) ((s.radius * 2.) + 20.) 8. |> rotate ((float_of_int k * 22.5) + (float_of_int frame * 9.)))
    @ [ circle (rgb 190 190 200) s.radius; circle (rgb 90 90 100) (s.radius * 0.3) ])
  |> move x y

let meat_boy (p : play) : shape =
  group
    [ square meat 26.;
      circle white 4. |> move ((p.facing * 4.) - 5.) 4.;
      circle white 4. |> move ((p.facing * 4.) + 5.) 4.;
      circle black 1.8 |> move ((p.facing * 5.) - 5.) 4.;
      circle black 1.8 |> move ((p.facing * 5.) + 5.) 4. ]
  |> move p.x p.y

let splat (p : play) : shape = group [ circle blood 16.; circle meat 9. ] |> move p.x p.y

let bandage_girl_shape (l : level) : shape list =
  List.map
    (fun (c, r) ->
      let x, y = Tilemap.center l.map c r in
      group [ square (rgb 240 150 170) 24.; rectangle white 24. 5. |> move_y 3.; circle black 2. |> move 3. 6. ]
      |> move x (y - 7.))
    (Tilemap.find l.map 'B')

let room (computer : computer) (l : level) (frame : int) : shape list =
  let screen = computer.screen in
  [ rectangle dusk screen.width screen.height; Tilemap.view tile_shape l.map ]
  @ bandage_girl_shape l
  @ List.map (saw_shape frame) l.saws

let smears (at : (number * number) list) : shape list = List.map (fun (x, y) -> square blood 7. |> move x y) at

let hud (computer : computer) (l : level) (deaths : int) : shape list =
  let screen = computer.screen in
  [ text white 2.2 l.name |> move_y (screen.top - 40.);
    text (rgb 230 190 190) 2. (Printf.sprintf "deaths %d" deaths) |> move_y (screen.bottom + 40.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      [ rectangle dusk screen.width screen.height;
        text meat 6. "SUPER MEAT BOY" |> move_y 200.;
        text white 2. "arrows run -- it takes a moment to get going, and to stop" |> move_y 80.;
        text white 2. "space jumps, and off walls; hold it to go higher" |> move_y 40.;
        text (rgb 230 190 190) 2. "dying costs nothing.  at the end, watch every try" |> move_y (-10.);
        meat_boy { (start_of hello_world) with x = 0.; y = -100. } ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-250.) ]
  | Running r ->
      let l = levels.(r.level) in
      room computer l r.play.frame
      @ smears r.smears
      @ [ (if r.play.dead then splat r.play else meat_boy r.play) ]
      @ hud computer l r.deaths
  | Replaying rp ->
      let l = levels.(rp.r_level) in
      room computer l rp.r_frame
      @ smears rp.r_smears
      @ List.map (fun (p, _) -> if p.dead then splat p |> fade 0.8 else meat_boy p |> fade 0.6) rp.ghosts
      @ hud computer l rp.r_deaths
      @ [ text white 2.5 (Printf.sprintf "every try at once: %d" (List.length rp.ghosts)) |> move_y (screen.top - 90.) ]
      @ (if replay_over rp then Scene2d.blink 1. model [ text white 2.5 "PRESS SPACE" |> move_y (screen.top - 140.) ]
         else [])
  | Rescued deaths ->
      [ rectangle dusk screen.width screen.height;
        text (rgb 240 150 170) 5. "BANDAGE GIRL IS SAVED" |> move_y 100.;
        text white 2.5 (Printf.sprintf "%d deaths on the way" deaths) |> move_y 20. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinySuperMeatBoy
  left right   run (with momentum)
  space        jump -- hold it to jump higher; against a wall, jump off it
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
