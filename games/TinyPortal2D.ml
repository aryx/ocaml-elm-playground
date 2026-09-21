(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Portal in two dimensions: a puzzle platformer where you cannot
 * jump far enough or high enough, and the way through is a pair of
 * holes you put in the walls yourself. Left click shoots the blue
 * portal, right click the orange one, at the white walls only; arrows
 * or a/d walk, up or w jumps, e picks up the cube next to you.
 *
 * Portal (Valve, 2007) grew out of Narbacular Drop (DigiPen, 2005),
 * the student game whose team Valve hired; the 2D version this one
 * copies is Portal: The Flash Version (We Create Stuff, 2007), which
 * showed that the idea survives losing a dimension. Cave Johnson's
 * line in Portal 2 is the physics in six words: "speedy thing goes in,
 * speedy thing comes out". (Names and dates from memory, to check.)
 *
 * A portal is a *transform*, and that is the whole game. Going in at A
 * and out at B is one rigid motion: the rotation R that turns the way
 * you went into A into the way you come out of B, applied to where you
 * are and, unchanged, to how fast you are going:
 *
 *      enter A moving -nA          leave B moving +nB
 *
 *            nA                            nB
 *         <--|      wall        wall       |-->
 *         o->|                             |  o->
 *                                   R = the rotation from -nA to nB
 *        p' = B + R (p - A)        v' = R v        (no translation!)
 *
 * The velocity is rotated and *not* scaled, which is why momentum
 * carries: fall 15 tiles into a hole in the floor and you come out of
 * the other hole going up as fast as you were going down, which is how
 * you reach a ledge no jump can reach (level 2's fling). In 3D this is
 * a quaternion and an orientation to carry too (see
 * plan_physics3d_teaching.md's TinyPortal, a bigger game than this
 * one); in 2D it is [angle_of] and four lines of arithmetic.
 *
 * The physics is the playground's Physics layer, directly: the level's
 * tiles are [immovable] bodies, the player is an [upright] box and the
 * cube a box that tumbles, each stepped and then bounced off the tiles
 * near it (Physics.step, Physics.bounce_off -- Collide and Resolve
 * underneath). Not Physics.world: its solver is for piles that rest on
 * each other, it knows its bodies by their place in a list, and this
 * game's walls come and go as the portals move.
 *
 * The simplifications, said up front, since a portal cuts a hole in
 * exactly what an engine assumes is solid:
 *  - a tile with a portal on it is taken out of the world entirely, so
 *    a body can go through it (a real engine carves the hole out of
 *    the wall instead, and leaves the rest solid);
 *  - the player is a box that never turns, and comes out of a portal
 *    upright however it went in;
 *  - portals are only on the flat faces of tiles, never on a corner,
 *    and a portal always fits its tile.
 *
 * What it uses: the Physics layer (above), Tilemap (the levels, as
 * strings), Scene2d (title, play, done), Audio, and the mouse's two
 * buttons. Not gamekits/platformer: its Tile_move walks a character
 * against a grid a pixel at a time, which is the *other* way to write
 * a platformer -- and the engine is what a portal needs, since what
 * goes through it is a velocity.
 *
 * Exercises: the cube dropped into a portal to land on a button you
 * cannot reach; a portal on a moving wall (what velocity does the
 * transform add?); light, or a turret's beam, through a portal (the
 * same transform on a ray); the fling with air control taken away;
 * levels read from a file; and the one every Portal player tries, two
 * portals facing each other.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

(* 'X' a wall, '#' a white wall, the only kind that takes a portal,
 * '~' the goo, 'P' the player, 'C' the cube, 'B' its button, 'D' the
 * door the button opens, 'E' the way out *)
let tile = 50.

let levels : string list list =
  [ (* 1. the pit: the goo is too wide to jump, and the two side walls
       are white, which is the whole lesson *)
    [ "XXXXXXXXXXXXXXXXXXXX";
      "X                  X";
      "X                  X";
      "#                  #";
      "#                  #";
      "#                  #";
      "#                  #";
      "#   P         E    #";
      "XXXXXX~~~~~~~~XXXXXX";
      "XXXXXXXXXXXXXXXXXXXX" ];
    (* 2. the fling: step off the platform, fall the height of the
       room into one hole in the white floor, come out of the other
       going up just as fast, and steer onto the ledge with the way
       out. The platform is on the left and the holes go on its right,
       because a shot that would have to pass through the floor you are
       standing on is a shot you cannot take *)
    [ "XXXXXXXXXXXXXXXXXXXX";
      "X                  X";
      "X                  X";
      "X                  X";
      "X P                X";
      "XXXXXXXXXXX      E X";
      "X               XXXX";
      "X                  X";
      "X                  X";
      "X                  X";
      "X                  X";
      "X##################X";
      "XXXXXXXXXXXXXXXXXXXX" ];
    (* 3. the cube: the wall is too tall to jump and the only line of
       sight into the other room is over it, so the first portal has to
       go high on the far wall; the way back is a second pair, on the
       two white floors *)
    [ "XXXXXXXXXXXXXXXXXXXX";
      "X                  #";
      "X                  #";
      "X        X         X";
      "X   P    X   C D   X";
      "XB       X     D E X";
      "X#####XXXXXXX##XXXXX";
      "XXXXXXXXXXXXXXXXXXXX" ] ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a portal: where its middle is, and the way out of the wall it is on
 * (its normal); [cell] is the tile it was cut into, which is then not
 * solid any more *)
type portal = { px : number; py : number; nx : number; ny : number; cell : int * int }

type play = Playing | Won of int (* frames since *) | Dead of int

type game = {
  level : int;
  map : Tilemap.t;
  player : Physics.body;
  cube : Physics.body option;
  held : bool; (* the cube carried in front of the player *)
  blue : portal option;
  orange : portal option;
  door_open : bool;
  play : play;
  shots : int; (* portals fired, for the score line *)
  (* the right button at the last frame: the playground gives [mclick],
   * the left button's moment of going down, but for the right one only
   * [mrdown], whether it is held, so the game watches its edge itself *)
  was_rdown : bool;
  frames : int;
}

type scene = Title | In_game of game | Done of int
type model = { scenes : scene Scene2d.t; best : int }

let gravity = 1400.
let walk_speed = 260.
let jump_speed = 620.
let mouth = 44. (* how wide a portal is *)

let solid (c : char) : bool = c = 'X' || c = '#' || c = 'D'
let portalable (c : char) : bool = c = '#'

let player_body (x : number) (y : number) : Physics.body =
  Physics.body (rectangle (rgb 255 140 60) 26. 44.) |> Physics.at x y |> Physics.upright |> Physics.rough 0.02

let cube_body (x : number) (y : number) : Physics.body =
  Physics.body (square (rgb 200 200 210) 34.) |> Physics.at x y |> Physics.heavy 0.6 |> Physics.rough 0.5 |> Physics.bouncy 0.1

let load (n : int) : game =
  let map = Tilemap.of_strings tile (List.nth levels n) in
  let at c = match Tilemap.find map c with (col, row) :: _ -> Some (Tilemap.center map col row) | [] -> None in
  let px, py = match at 'P' with Some p -> p | None -> (0., 0.) in
  { level = n; map; player = player_body px py; cube = (match at 'C' with Some (x, y) -> Some (cube_body x y) | None -> None); held = false;
    blue = None; orange = None; door_open = false; play = Playing; shots = 0; was_rdown = false; frames = 0 }

let initial_model = { scenes = Scene2d.start Title; best = 0 }

(*****************************************************************************)
(* The transform: what a portal does *)
(*****************************************************************************)

let angle_of ((x, y) : number * number) : number = atan2 y x

(* [turn a (x, y)]: the point turned by [a] radians, around (0, 0) *)
let turn (a : number) ((x, y) : number * number) : number * number = ((x * cos a) - (y * sin a), (x * sin a) + (y * cos a))

(* The rotation of the pair: going into [a] (against its normal) and
 * out of [b] (along its normal). E.g. in at a floor portal (normal up,
 * so you arrive going down) and out at another floor portal: a half
 * turn, and what went down comes up. *)
let rotation (a : portal) (b : portal) : number = angle_of (b.nx, b.ny) - angle_of (0. - a.nx, 0. - a.ny)

(* a body through the pair: its position turned around the portals, its
 * velocity only turned -- never scaled, which is the whole of "speedy
 * thing goes in, speedy thing comes out" *)
(* The least you come out with. Stepping into a hole in the floor with
 * no speed at all, you would come out of the other one with no speed
 * either, fall straight back in, and bob between the two for ever --
 * and because a floor-to-floor pair is a half turn, which mirrors left
 * and right, walking does not get you out of it. So a body always
 * leaves a portal with at least this much speed along the way out,
 * enough to land beside the hole. Portal does the same (from memory,
 * to check). *)
let least_exit_speed = 280.

let go_through ?(half = 2.) (a : portal) (b : portal) (body : Physics.body) : Physics.body =
  let r = rotation a b in
  let dx, dy = turn r (body.x - a.px, body.y - a.py) in
  let vx, vy = turn r (body.vx, body.vy) in
  let out = (vx * b.nx) + (vy * b.ny) in
  let vx, vy = if out >= least_exit_speed then (vx, vy) else (vx + ((least_exit_speed - out) * b.nx), vy + ((least_exit_speed - out) * b.ny)) in
  (* out of the wall by a little, so the next step starts in the room *)
  { body with x = b.px + dx + (b.nx * (half + 4.)); y = b.py + dy + (b.ny * (half + 4.)); vx; vy; angle = body.angle + radians_to_degrees r }

(* Did the body go into [p]'s wall during this step, through the
 * portal's mouth? What counts is not where its middle is but where its
 * *leading edge* is -- half its height below it for a hole in the
 * floor, half its width ahead for one in a wall -- or a player walking
 * over a hole would skim across it: at 260 pixels a second he crosses
 * the 50-pixel mouth in 11 frames and falls 22 in that time, exactly
 * the distance from his middle to his feet. [half] is that half-size
 * along the portal's normal, less a few pixels, so that standing on a
 * hole counts as being above it and stepping onto one drops you in. *)
let crossed (p : portal) (half : number) ((x0, y0) : number * number) (body : Physics.body) : bool =
  let edge x y = ((x - p.px) * p.nx) + ((y - p.py) * p.ny) - half in
  let before = edge x0 y0 and after = edge body.x body.y in
  if before <= 0. || after > 0. then false
  else
    (* where it went through, along the portal's own direction *)
    let t = before / Float.max 1e-9 (before - after) in
    let cx = x0 + (t * (body.x - x0)) and cy = y0 + (t * (body.y - y0)) in
    let along = ((cx - p.px) * (0. - p.ny)) + ((cy - p.py) * p.nx) in
    Float.abs along < mouth / 2.

(* half a box's size along a direction, which for a normal along one of
 * the axes is just its half-width or its half-height; 6 pixels are
 * taken off it, see [crossed] *)
let half_along ((hw, hh) : number * number) ((nx, ny) : number * number) : number = (Float.abs nx * hw) + (Float.abs ny * hh) - 6.

(*****************************************************************************)
(* Shooting a portal *)
(*****************************************************************************)

(* The portal gun: a ray out from the player, walked cell by cell --
 * Amanatides and Woo's grid traversal (1987), the one
 * games2.5d/TinyWolfenstein.ml casts its walls with. Of the two boundaries
 * ahead, the vertical one and the horizontal one, the ray crosses
 * whichever is nearer; that crossing *is* the face it comes in by, and
 * the face is what a portal needs. (Stepping along the ray a few
 * pixels at a time instead, and then guessing the face from the side
 * of the tile the point is nearest, gets a shot into a corner wrong
 * and puts a portal on the side of the floor.)
 *
 *          |        |            the ray crosses the column boundary
 *      ----+--------+----        first: it comes in by the left face,
 *          |  .-----+--> o       and the portal's normal points left
 *      ----+--------+----
 *)
let shoot (map : Tilemap.t) ((x, y) : number * number) ((dx, dy) : number * number) : portal option =
  let d = Float.max 1e-9 (Float.hypot dx dy) in
  let dx = dx / d and dy = dy / d in
  let col0, row0 = Tilemap.cell map x y in
  let cx0, cy0 = Tilemap.center map col0 row0 in
  (* which way the columns and the rows go (a row below has a bigger
   * number and a smaller y), and how far along the ray the next
   * boundary of each kind is, then one tile's worth at a time *)
  let step_col = if dx > 0. then 1 else -1 in
  let step_row = if dy > 0. then -1 else 1 in
  let tx = if dx = 0. then infinity else (cx0 + (float_of_int step_col * tile / 2.) - x) / dx in
  let ty = if dy = 0. then infinity else (cy0 + ((if dy > 0. then 1. else -1.) * tile / 2.) - y) / dy in
  let dtx = if dx = 0. then infinity else tile / Float.abs dx in
  let dty = if dy = 0. then infinity else tile / Float.abs dy in
  let rec go (col : int) (row : int) (tx : number) (ty : number) (n : int) : portal option =
    if n = 0 then None
    else
      let horizontal = tx < ty in
      let col = if horizontal then col +.. step_col else col and row = if horizontal then row else row +.. step_row in
      let hit = if horizontal then tx else ty in
      let tx = if horizontal then tx + dtx else tx and ty = if horizontal then ty else ty + dty in
      match Tilemap.get map col row with
      | None -> None
      | Some c when not (solid c) -> go col row tx ty (n -.. 1)
      | Some c when not (portalable c) -> None
      | Some _ ->
          let cx, cy = Tilemap.center map col row in
          let hx = x + (hit * dx) and hy = y + (hit * dy) in
          if horizontal then
            let side = float_of_int (0 -.. step_col) in
            Some
              { px = cx + (side * tile / 2.); py = clamp (cy - (tile / 2.) + (mouth / 2.)) (cy + (tile / 2.) - (mouth / 2.)) hy; nx = side; ny = 0.;
                cell = (col, row) }
          else
            let side = if step_row > 0 then 1. else -1. in
            Some
              { px = clamp (cx - (tile / 2.) + (mouth / 2.)) (cx + (tile / 2.) - (mouth / 2.)) hx; py = cy + (side * tile / 2.); nx = 0.; ny = side;
                cell = (col, row) }
  in
  go col0 row0 tx ty 40

(*****************************************************************************)
(* The world the bodies bounce off *)
(*****************************************************************************)

(* every solid tile near a body, as an immovable box -- minus the tiles
 * the portals are in, which are holes now, and minus the door once its
 * button is held down *)
let tiles_near (g : game) (b : Physics.body) : Physics.body list =
  let holes = List.filter_map (fun p -> match p with Some (p : portal) -> Some p.cell | None -> None) [ g.blue; g.orange ] in
  let col0, row0 = Tilemap.cell g.map (b.x - 80.) (b.y + 80.) in
  let col1, row1 = Tilemap.cell g.map (b.x + 80.) (b.y - 80.) in
  List.concat_map
    (fun row ->
      List.filter_map
        (fun col ->
          match Tilemap.get g.map col row with
          | Some c when solid c && not (List.mem (col, row) holes) ->
              if c = 'D' && g.door_open then None
              else
                let x, y = Tilemap.center g.map col row in
                Some (Physics.body (square black tile) |> Physics.at x y |> Physics.immovable |> Physics.rough 0.3)
          | _ -> None)
        (List.init (col1 -.. col0 +.. 1) (fun i -> col0 +.. i)))
    (List.init (row1 -.. row0 +.. 1) (fun i -> row0 +.. i))

let bounce_off_tiles (g : game) (b : Physics.body) : Physics.body =
  List.fold_left (fun b t -> Physics.bounce_off t b) b (tiles_near g b)

(* standing on something: the same body, a little lower, touching a
 * tile (games/TinyCameltry.ml probes the same way) *)
let on_ground (g : game) (b : Physics.body) : bool =
  let probe = { b with y = b.y - 5. } in
  List.exists (fun t -> Physics.touching t probe) (tiles_near g b)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* a body through whichever portal it went into this step *)
let teleport (g : game) (size : number * number) (was : number * number) (b : Physics.body) : Physics.body =
  match (g.blue, g.orange) with
  | Some blue, Some orange ->
      let into (p : portal) = crossed p (half_along size (p.nx, p.ny)) was b in
      if into blue then go_through ~half:(half_along size (orange.nx, orange.ny)) blue orange b
      else if into orange then go_through ~half:(half_along size (blue.nx, blue.ny)) orange blue b
      else b
  | _ -> b

let walk (computer : computer) (g : game) (b : Physics.body) : Physics.body =
  let k = computer.keyboard in
  let dir = to_x k + to_x2 k in
  let jumping = (k.kup || k.kw) && on_ground g b in
  if jumping then Audio.play Audio.jump;
  { b with vx = clamp (-1400.) 1400. ((dir * walk_speed) + (if dir = 0. then b.vx * 0.82 else 0.)); vy = (if jumping then jump_speed else b.vy) }

(* the cube carried in front of the player, at arm's length, the way
 * the gun holds it in Portal: not a joint, just a body put there every
 * frame, which is what its grab really is *)
let carry (p : Physics.body) (c : Physics.body) : Physics.body =
  let side = if p.vx < -10. then -1. else 1. in
  { c with x = p.x + (side * 34.); y = p.y + 6.; vx = p.vx; vy = p.vy; spin = 0. }

(* the player is 26 by 44, the cube 34 square *)
let player_size = (13., 22.)
let cube_size = (17., 17.)

let step_body (g : game) (size : number * number) (b : Physics.body) : Physics.body =
  let was = (b.x, b.y) in
  b |> Physics.fall gravity |> Physics.step |> bounce_off_tiles g |> teleport g size was

let button_held (g : game) : bool =
  match (Tilemap.find g.map 'B', g.cube) with
  | (col, row) :: _, Some c ->
      let x, y = Tilemap.center g.map col row in
      Float.abs (c.x - x) < tile * 0.6 && Float.abs (c.y - y) < tile * 0.8
  | _ -> false

let at_exit (g : game) : bool =
  match Tilemap.find g.map 'E' with
  | (col, row) :: _ ->
      let x, y = Tilemap.center g.map col row in
      Float.hypot (g.player.x - x) (g.player.y - y) < 34.
  | [] -> false

let in_the_goo (g : game) : bool = match Tilemap.tile_at g.map g.player.x (g.player.y - 20.) with Some '~' -> true | _ -> false

let fire (computer : computer) (g : game) : game =
  let aim = (computer.mouse.mx - g.player.x, computer.mouse.my - g.player.y) in
  let shot (g : game) (which : bool) : game =
    match shoot g.map (g.player.x, g.player.y) aim with
    | None ->
        Audio.play Audio.hit;
        g
    | Some p ->
        Audio.play Audio.laser;
        { (if which then { g with blue = Some p } else { g with orange = Some p }) with shots = g.shots +.. 1 }
  in
  let rclick = computer.mouse.mrdown && not g.was_rdown in
  let g = { g with was_rdown = computer.mouse.mrdown } in
  if computer.mouse.mclick then shot g true else if rclick then shot g false else g

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  match g.play with
  | Won n -> { g with play = Won (n +.. 1) }
  | Dead n -> if n > 60 then { (load g.level) with shots = g.shots } else { g with play = Dead (n +.. 1) }
  | Playing ->
      let g = fire computer g in
      let g = { g with door_open = button_held g } in
      (* the cube: picked up and put down with e *)
      let grab = Scene2d.pressed (fun k -> Set_.mem "e" k.keys) scenes in
      let g =
        match (grab, g.cube) with
        | true, Some _ when g.held -> { g with held = false }
        | true, Some c when Float.hypot (c.x - g.player.x) (c.y - g.player.y) < 60. ->
            Audio.play Audio.coin;
            { g with held = true }
        | _ -> g
      in
      let player = g.player |> walk computer g |> step_body g player_size in
      let g = { g with player } in
      let cube =
        match g.cube with
        | None -> None
        | Some c when g.held -> Some (carry player c)
        | Some c -> Some (step_body g cube_size c)
      in
      let g = { g with cube } in
      if in_the_goo g then begin
        Audio.play Audio.explosion;
        { g with play = Dead 0 }
      end
      else if at_exit g then begin
        Audio.play Audio.coin;
        { g with play = Won 0 }
      end
      else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let go = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if go then { model with scenes = Scene2d.go (In_game (load 0)) scenes } else { model with scenes }
  | In_game g -> (
      let g = update_game computer scenes g in
      match g.play with
      | Won n when n > 60 ->
          if g.level +.. 1 >= List.length levels then
            { best = (if model.best = 0 then g.shots else min model.best g.shots); scenes = Scene2d.go (Done g.shots) scenes }
          else { model with scenes = { scenes with scene = In_game { (load (g.level +.. 1)) with shots = g.shots } } }
      | _ -> { model with scenes = { scenes with scene = In_game g } })
  | Done _ -> if go || scenes.elapsed > 12. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let blue_color = rgb 70 170 255
let orange_color = rgb 255 150 40

let view_tile (door_open : bool) (c : char) : shape =
  match c with
  | 'X' -> group [ square (rgb 40 44 56) tile; square (rgb 52 57 72) (tile - 6.) ]
  | '#' -> group [ square (rgb 215 220 230) tile; square (rgb 238 242 250) (tile - 6.) ]
  | 'D' -> if door_open then group [] else group [ rectangle (rgb 150 110 60) (tile - 10.) tile; rectangle (rgb 190 150 90) (tile - 22.) (tile - 12.) ]
  | '~' -> group [ rectangle (rgb 120 220 90) tile tile; rectangle (rgb 170 250 130) tile 10. |> move_y 12. ]
  | 'B' -> group [ rectangle (rgb 120 70 70) (tile - 8.) 10. |> move_y (0. - (tile / 2.) + 6.); rectangle (rgb 220 90 90) (tile - 20.) 8. |> move_y (0. - (tile / 2.) + 14.) ]
  | 'E' -> group [ rectangle (rgb 60 80 60) 34. 48.; rectangle (rgb 120 255 160) 24. 38.; words black "E" ]
  | _ -> group []

(* a portal is an oval on the face it was cut into, turned to lie along
 * the wall *)
let view_portal (color : color) (p : portal) : shape list =
  let a = radians_to_degrees (angle_of (p.nx, p.ny)) in
  [ oval color 16. (mouth + 6.) |> rotate a |> move p.px p.py; oval (rgb 20 24 34) 8. mouth |> rotate a |> move p.px p.py ]

let view_player (g : game) (b : Physics.body) : shape list =
  let look = if b.vx < -10. then -6. else 6. in
  [ rectangle (rgb 255 140 60) 26. 44. |> move b.x b.y; rectangle (rgb 255 200 150) 20. 14. |> move b.x (b.y + 12.);
    circle black 4. |> move (b.x + look) (b.y + 13.);
    (* the gun *)
    rectangle (rgb 90 100 120) 18. 7. |> move (b.x + look) (b.y + 2.) ]
  @ if on_ground g b then [] else [ oval (rgb 255 220 180) 10. 4. |> fade 0.5 |> move b.x (b.y - 24.) ]

let view_cube (c : Physics.body) : shape list =
  [ square (rgb 200 200 210) 34. |> rotate c.angle |> move c.x c.y; square (rgb 150 155 170) 22. |> rotate c.angle |> move c.x c.y;
    circle (rgb 240 120 150) 7. |> move c.x c.y ]

(* the line the gun would shoot along, dotted, so aiming is possible
 * with a mouse that draws nothing *)
let view_aim (computer : computer) (g : game) : shape list =
  let dx = computer.mouse.mx - g.player.x and dy = computer.mouse.my - g.player.y in
  let d = Float.max 1e-9 (Float.hypot dx dy) in
  List.init 14 (fun i ->
      let t = 20. + (float_of_int i * 26.) in
      circle (rgb 120 130 150) 2. |> fade 0.5 |> move (g.player.x + (t * dx / d)) (g.player.y + (t * dy / d)))

let view_game (computer : computer) (g : game) : shape list =
  [ rectangle (rgb 20 24 34) 1000. 1000.; Tilemap.view (view_tile g.door_open) g.map ]
  @ (match g.blue with Some p -> view_portal blue_color p | None -> [])
  @ (match g.orange with Some p -> view_portal orange_color p | None -> [])
  @ view_aim computer g
  @ (match g.cube with Some c -> view_cube c | None -> [])
  @ view_player g g.player
  @ [ text white 2. (Printf.sprintf "TEST CHAMBER %d" (g.level +.. 1)) |> move (-330.) 470.;
      text (rgb 120 130 150) 1.8 (Printf.sprintf "portals fired %d" g.shots) |> move 330. 470. ]
  @ (match g.play with
    | Won _ -> [ text (rgb 120 255 160) 4. "CHAMBER COMPLETE" |> move_y 120. ]
    | Dead _ -> [ text (rgb 255 90 90) 4. "THE GOO" |> move_y 120. ]
    | Playing -> [])
  @ if g.frames < 180 && g.level = 0 then [ text (rgb 180 190 210) 1.8 "left click: blue portal   right click: orange   they only stick to white walls" |> move_y (-470.) ] else []

let view_title (scenes : scene Scene2d.t) (model : model) : shape list =
  [ text blue_color 7. "TINY PORTAL 2D" |> move_y 300.; text white 2.2 "speedy thing goes in, speedy thing comes out" |> move_y 230. ]
  @ List.concat
      (List.mapi
         (fun i ((key : string), (what : string)) ->
           let y = 110. - (float_of_int i * 55.) in
           [ text orange_color 2.4 key |> move (-250.) y; text white 2.4 what |> move 110. y ])
         [ ("left click", "the blue portal, on a white wall"); ("right click", "the orange one"); ("arrows / a d", "walk, and up or w to jump");
           ("e", "pick up the cube, and put it down") ])
  @ [ text (rgb 180 190 210) 2. "a portal is a rotation: what goes in one comes out the other, as fast" |> move_y (-180.) ]
  @ (if model.best > 0 then [ text white 2.2 (Printf.sprintf "fewest portals %d" model.best) |> move_y (-240.) ] else [])
  @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-330.) ]

let view (computer : computer) (model : model) : shape list =
  let scenes = model.scenes in
  match scenes.scene with
  | Title -> rectangle (rgb 20 24 34) computer.screen.width computer.screen.height :: view_title scenes model
  | In_game g -> view_game computer g
  | Done shots ->
      [ rectangle (rgb 20 24 34) computer.screen.width computer.screen.height; text (rgb 120 255 160) 5. "ALL CHAMBERS DONE" |> move_y 80.;
        text white 3. (Printf.sprintf "%d portals fired" shots) |> move_y (-20.) ]
      @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]

let help =
  {|TinyPortal2D
  keys:  left/right or a/d  walk      up or w  jump
         e                  pick up the cube / put it down
         space              start, and next chamber
  mouse: left click   the blue portal (white walls only)
         right click  the orange one
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
