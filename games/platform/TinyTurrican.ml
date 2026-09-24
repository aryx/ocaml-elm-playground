(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Turrican (Manfred Trenz, Rainbow Arts, 1990): a
 * soldier in an armoured suit, a world far bigger than the screen, and
 * more weapons than enemies. Left/right to run, up to jump, space to
 * fire (hold it: it keeps firing), x held for the lightning beam
 * (standing still, up/down sweep it round), down to curl into the
 * gyroscope wheel (three per life: invulnerable, rolling over enemies,
 * and through gaps too low to stand in), z for a power line (every
 * enemy on the screen destroyed; two per life). Shoot the P block open,
 * take its weapon, and find the teleporter at the far right; the
 * diamonds and a secret cave are for those who explore.
 *
 * Manfred Trenz wrote it on the Commodore 64, after the C64's The
 * Great Giana Sisters (1987) and R-Type conversion; its world scrolling
 * in every direction at the C64's 50 frames a second was thought
 * impossible, and the Amiga and Atari ST versions (Factor 5) became as
 * famous for Chris Huelsbeck's music. Turrican II (1991) and Super
 * Turrican followed. It borrowed from Metroid (a world to explore, the
 * wheel is the morph ball) and from Psycho-Nicks Oscar (the arcade
 * game its look came from). (Names and dates from memory, to check.)
 *
 * The new idea here is the lightning beam: a weapon aimed by an angle
 * you steer, not by the way you face -- a ray from the gun, cast
 * through the tile map until it meets rock ([cast]): stepping along the
 * ray a few pixels at a time, asking the map at each step, the ray's
 * end the first solid point. Enemies are hurt by being near the ray
 * (the distance from a point to a segment, [to_segment]), a little
 * every frame they stay in it: a weapon of held time, not of bullets.
 *
 *      @--.__        the ray, from the gun at the angle a, stepped
 *            '--.__  8 pixels at a time; its end the first step
 *                  '-# into rock; drawn as a jagged bolt, the
 *                    #  jags changing every frame
 *
 * And the wheel is a body that changes its box: rolled up, the hero is
 * a 32 x 32 ball, and rolls through a tunnel one tile high; standing
 * needs 28 x 56, and the wheel only opens where there is room
 * ([unroll]), or the hero would stand up into the rock.
 *
 * What it uses: the platformer kit (gamekits/platformer/: Tile_move,
 * the hero and the walkers against the rock, one pixel at a time), the
 * shoot 'em up kit's Shots (gamekits/shmup/: the hero's shots, the
 * turrets' aimed ones), Tilemap (the world, changed by diamonds and
 * pickups taken, a block shot open), Camera2d (a window, clamped to the
 * world, and only the visible tiles drawn), Sprite (the hero), Scene2d.
 * Not Physics: the jump is TinyRick's, and nothing bounces.
 *
 * Exercises: a boss at the teleporter (Turrican's are the size of the
 * screen); the rebound weapon, whose shots bounce off the rock; mines
 * dropped by the wheel; a 100-diamond extra life; the timer; casting
 * the beam exactly, tile by tile, as TinyWolfenstein casts its rays.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

(* 90 x 25 tiles: # rock, = metal, ^ spikes, P a block that opens after
 * five shots (the digits 4 to 1 counting them down), then gives a
 * spread upgrade, * diamonds, S spread and L laser, W an extra wheel, C
 * the checkpoint, E the teleporter; @ the hero, w walkers, f flyers, t
 * turrets. Under the rock at the right, a tunnel only the wheel rolls
 * through, and a cave in the rock. *)
let world =
  [ "##########################################################################################";
    "#               #                                                                        #";
    "#               #                                                                        #";
    "#               #                                                                        #";
    "#               #                                                     f                  #";
    "#               #                                           f                            #";
    "#               #                                               * * * * *                #";
    "#               #                                                                        #";
    "#               #                                                 w                      #";
    "#               #                             ======##########===============            #";
    "#               #                                   ##########                           #";
    "#               #                                   ##########                           #";
    "#               #                          ===      ##########                           #";
    "#               #                   f               ##########                           #";
    "#               #                                   ##########                           #";
    "#               #             f               ===   ##########                           #";
    "#               #            *****                  ##########                           #";
    "#      ****     #                                   ###     ##                           #";
    "#      ====     #                          ===      ###     ##                           #";
    "#               #        ==                         ###     ##                           #";
    "#               P                                   ###**WL*##   ****                  E #";
    "#  @            P    w                 t  C                           w        w  t    E #";
    "########################    ##############################################################";
    "########################    ##############################################################";
    "########################^^^^##############################################################" ]

let tile = 40.
let level = Tilemap.of_strings tile world
(* spikes are a floor, one that hurts *)
let solid (c : char) : bool = c = '#' || c = '=' || c = '^' || c = 'P' || (c >= '1' && c <= '4')
let standing_size = (28., 56.)
let wheel_size = (32., 32.)
let enemy_size = (32., 36.)

let places (c : char) : (number * number) list = List.map (fun (col, row) -> Tilemap.center level col row) (Tilemap.find level c)

(* the world to play: the hero and the enemies are not tiles *)
let start_map : Tilemap.t =
  List.fold_left (fun m (col, row) -> Tilemap.set m col row ' ') level (List.concat_map (Tilemap.find level) [ '@'; 'w'; 'f'; 't' ])

(* standing on the floor of the tile at (x, y), a body [h] high *)
let on_floor (h : number) ((x, y) : number * number) : number * number = (x, y - (tile / 2.) + (h / 2.))
let start = on_floor (snd standing_size) (List.hd (places '@'))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type weapon = Spread of int (* 1 to 3 *) | Laser

type hero = {
  x : number;
  y : number;
  vy : number;
  facing : number;
  steps : int;
  rolled : int; (* frames left in the wheel; 0 standing *)
  beam : number option; (* the beam's angle, up from the way he faces, while x is held *)
  energy : number;
  hurt : int;
  cooldown : int;
  weapon : weapon;
  wheels : int;
  lines : int;
}

type kind = Walker | Flyer | Turret

type enemy = { kind : kind; ex : number; ey : number; home : number * number; dir : number; hp : number; phase : int }

type game = {
  map : Tilemap.t;
  hero : hero;
  enemies : enemy list;
  shots : (weapon * Shots.t) list;
  bolts : Shots.t list; (* the turrets' *)
  booms : (number * number * int) list;
  flash : int; (* a power line's *)
  diamonds : int;
  score : int;
  lives : int;
  checkpoint : number * number;
  cam : Camera2d.t;
  frames : int;
}

type scene = Title | Playing of game | Teleported of int | Game_over of int
type model = scene Scene2d.t

let new_hero ((x, y) : number * number) (weapon : weapon) : hero =
  { x; y; vy = 0.; facing = 1.; steps = 0; rolled = 0; beam = None; energy = 100.; hurt = 0; cooldown = 0; weapon; wheels = 3; lines = 2 }

let new_enemy (kind : kind) (i : int) ((x, y) : number * number) : enemy =
  let x, y = if kind = Flyer then (x, y) else on_floor (snd enemy_size) (x, y) in
  { kind; ex = x; ey = y; home = (x, y); dir = -1.; hp = (match kind with Walker -> 3. | Flyer -> 1. | Turret -> 6.); phase = i *.. 37 }

let new_game () : game =
  let enemies =
    List.concat_map (fun (c, k) -> List.mapi (new_enemy k) (places c)) [ ('w', Walker); ('f', Flyer); ('t', Turret) ]
  in
  { map = start_map; hero = new_hero start (Spread 1); enemies; shots = []; bolts = []; booms = []; flash = 0; diamonds = 0; score = 0; lives = 3;
    checkpoint = start; cam = Camera2d.look_at (fst start) (snd start) Camera2d.origin; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The hero: running, jumping, rolling *)
(*****************************************************************************)

let run_speed = 4.5
let roll_speed = 6.
let jump_speed = 14.5 (* up to about 3 tiles *)
let gravity = 0.8
let size (h : hero) : number * number = if h.rolled > 0 then wheel_size else standing_size

(* between the two boxes, the feet stay where they are *)
let dy_roll = (snd standing_size - snd wheel_size) / 2.

let roll (h : hero) : hero = if h.rolled > 0 || h.wheels = 0 then h else { h with rolled = 300; wheels = h.wheels -.. 1; y = h.y - dy_roll; beam = None }

(* the wheel opens only where the standing box fits: under a low ceiling
 * it keeps rolling, whatever its time *)
let unroll (map : Tilemap.t) (h : hero) : hero =
  if h.rolled = 0 then h
  else if Tile_move.hits solid map standing_size h.x (h.y + dy_roll) then { h with rolled = 1 }
  else { h with rolled = 0; y = h.y + dy_roll }

(* [step_hero map keys jump h]: running (rolling faster), jumping (not
 * rolled), falling; with the beam held, standing still, up and down
 * turning it *)
let step_hero (map : Tilemap.t) (keys : keyboard) (jump : bool) (h : hero) : hero =
  let sz = size h in
  let on = Tile_move.on_ground solid map sz h.x h.y in
  let beaming = Set_.mem "x" keys.keys && h.rolled = 0 && on in
  let h =
    match (beaming, h.beam) with
    | true, None -> { h with beam = Some 0. }
    | true, Some a -> { h with beam = Some (Float.max (-45.) (Float.min 90. (a + (3. * to_y keys)))) }
    | false, _ -> { h with beam = None }
  in
  let dir = if beaming then 0. else to_x keys in
  let facing = if dir <> 0. then dir else h.facing in
  let (x, _), _ = Tile_move.move_by solid map sz (h.x, h.y) (dir * (if h.rolled > 0 then roll_speed else run_speed), 0.) in
  let vy = if jump && on && h.rolled = 0 && not beaming then jump_speed else if on && h.vy <= 0. then 0. else Float.max (-16.) (h.vy - gravity) in
  let (_, y), hit = Tile_move.move_by solid map sz (x, h.y) (0., vy) in
  let h = { h with x; y; vy = (if hit then 0. else vy); facing; steps = (if dir <> 0. then h.steps +.. 1 else h.steps) } in
  if h.rolled > 0 then let h = { h with rolled = h.rolled -.. 1 } in if h.rolled = 0 then unroll map { h with rolled = 1 } else h else h

(*****************************************************************************)
(* The weapons *)
(*****************************************************************************)

let gun (h : hero) : number * number = (h.x + (h.facing * 16.), h.y + 4.)

(* space: the spread's fan of 1, 3 or 5 shots, or the laser's long bolt *)
let fire (h : hero) : (weapon * Shots.t) list =
  let x, y = gun h in
  match h.weapon with
  | Laser -> [ (Laser, Shots.straight x y (h.facing * 16.) 0.) ]
  | Spread n ->
      let angles = match n with 1 -> [ 0. ] | 2 -> [ -8.; 0.; 8. ] | _ -> [ -16.; -8.; 0.; 8.; 16. ] in
      List.map (fun a -> (Spread n, Shots.straight x y (h.facing * 12. * cos (degrees_to_radians a)) (12. * sin (degrees_to_radians a)))) angles

(* [cast map (x, y) a]: the ray from (x, y) at [a] degrees, stepped 8
 * pixels at a time, stopped at the first step into a solid tile or at
 * its length: its end *)
let beam_length = 340.

let cast (map : Tilemap.t) ((x, y) : number * number) (a : number) : number * number =
  let dx, dy = (cos (degrees_to_radians a), sin (degrees_to_radians a)) in
  let rec go d =
    let px, py = (x + (dx * d), y + (dy * d)) in
    if d >= beam_length then (px, py)
    else match Tilemap.tile_at map px py with Some c when solid c -> (px, py) | None -> (px, py) | _ -> go (d + 8.)
  in
  go 8.

(* the beam's angle in the world: [a] up from the way he faces *)
let beam_ends (map : Tilemap.t) (h : hero) (a : number) : (number * number) * (number * number) =
  let from = gun h in
  let world_angle = if h.facing > 0. then a else 180. - a in
  (from, cast map from world_angle)

(* the distance from (px, py) to the segment from a to b: to the nearest
 * point of the segment, found by projecting on it and clamping *)
let to_segment ((px, py) : number * number) (((ax, ay), (bx, by)) : (number * number) * (number * number)) : number =
  let dx, dy = (bx - ax, by - ay) in
  let len2 = (dx * dx) + (dy * dy) in
  let t = if len2 = 0. then 0. else Float.max 0. (Float.min 1. ((((px - ax) * dx) + ((py - ay) * dy)) / len2)) in
  Float.hypot (px - (ax + (t * dx))) (py - (ay + (t * dy)))

(* a shot flying into a P block: one crack more, the fifth opens it on
 * a spread upgrade. The blocks of a column are one door (the hero is
 * taller than a tile, the shots fly at his waist): they crack together,
 * and open together. *)
let is_block (c : char) : bool = c = 'P' || (c >= '1' && c <= '4')

let crack (map : Tilemap.t) ((x, y) : number * number) : Tilemap.t =
  let col, row = Tilemap.cell map x y in
  match Tilemap.get map col row with
  | Some c when is_block c ->
      let door = List.filter (fun (c', r') -> c' = col && Tilemap.get map c' r' |> Option.fold ~none:false ~some:is_block) (List.init (Tilemap.rows map) (fun r -> (col, r))) in
      let next = match c with 'P' -> '4' | '1' -> ' ' | c -> Char.chr (Char.code c -.. 1) in
      let map = List.fold_left (fun m (c', r') -> Tilemap.set m c' r' next) map door in
      if next = ' ' then Tilemap.set map col row 'S' else map
  | _ -> map

(*****************************************************************************)
(* The enemies *)
(*****************************************************************************)

(* a walker walks and turns at walls and edges; a flyer loops round its
 * home; a turret stays, and fires at the hero when he's near *)
let step_enemy (map : Tilemap.t) (frames : int) (e : enemy) : enemy =
  match e.kind with
  | Walker ->
      let (x, _), hit = Tile_move.move_by solid map enemy_size (e.ex, e.ey) (e.dir * 1.5, 0.) in
      let edge = not (Tile_move.on_ground solid map enemy_size (x + (e.dir * 16.)) e.ey) in
      if hit || edge then { e with dir = -.e.dir } else { e with ex = x }
  | Flyer ->
      let t = float_of_int (frames +.. e.phase) in
      let hx, hy = e.home in
      { e with ex = hx + (100. * sin (t / 40.)); ey = hy + (40. * sin (t / 23.)); dir = (if cos (t / 40.) > 0. then 1. else -1.) }
  | Turret -> e

let turret_fires (frames : int) (h : hero) (e : enemy) : Shots.t option =
  if e.kind = Turret && (frames +.. e.phase) mod 90 = 0 && Float.hypot (h.x - e.ex) (h.y - e.ey) < 550. then Some (Shots.aimed 6. (e.ex, e.ey + 8.) (h.x, h.y))
  else None

let points (k : kind) : int = match k with Walker -> 100 | Flyer -> 50 | Turret -> 300

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let damage (amount : number) (h : hero) : hero = if h.hurt > 0 || h.rolled > 0 then h else { h with energy = h.energy - amount; hurt = 40 }

(* what the hero touches: diamonds, weapons, wheels, the checkpoint *)
let pick_up (g : game) : game =
  let h = g.hero in
  let col, row = Tilemap.cell g.map h.x h.y in
  let cells = List.sort_uniq compare [ (col, row); (col, row -.. 1); Tilemap.cell g.map h.x (h.y + 20.) ] in
  List.fold_left
    (fun g (c, r) ->
      let take g = { g with map = Tilemap.set g.map c r ' ' } in
      match Tilemap.get g.map c r with
      | Some '*' -> take { g with diamonds = g.diamonds +.. 1; score = g.score +.. 50 }
      | Some 'S' -> take { g with hero = { g.hero with weapon = (match g.hero.weapon with Spread n -> Spread (min 3 (n +.. 1)) | Laser -> Spread 2) } }
      | Some 'L' -> take { g with hero = { g.hero with weapon = Laser } }
      | Some 'W' -> take { g with hero = { g.hero with wheels = g.hero.wheels +.. 1 } }
      | Some 'C' -> { g with checkpoint = on_floor (snd standing_size) (Tilemap.center g.map c r) }
      | _ -> g)
    g cells

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let keys = computer.keyboard in
  let pressed k = Scene2d.pressed k scenes in
  let g = { g with frames = g.frames +.. 1; flash = max 0 (g.flash -.. 1); booms = List.filter_map (fun (x, y, n) -> if n < 20 then Some (x, y, n +.. 1) else None) g.booms } in
  (* the hero, the wheel, the power line *)
  let h = g.hero in
  let h = if pressed (fun k -> k.kdown) then if h.rolled > 0 then unroll g.map h else roll h else h in
  let h = step_hero g.map keys (pressed (fun k -> k.kup)) h in
  let h = { h with hurt = max 0 (h.hurt -.. 1); cooldown = max 0 (h.cooldown -.. 1) } in
  let g = { g with hero = h } in
  let g =
    if pressed (fun k -> Set_.mem "z" k.keys) && h.lines > 0 then
      let seen = Camera2d.visible computer.screen g.cam in
      let hit, rest = List.partition (fun e -> e.ex > seen.left && e.ex < seen.right && e.ey > seen.bottom && e.ey < seen.top) g.enemies in
      { g with enemies = rest; hero = { h with lines = h.lines -.. 1 }; flash = 20; bolts = [];
        score = g.score +.. List.fold_left (fun s e -> s +.. points e.kind) 0 hit; booms = List.map (fun e -> (e.ex, e.ey, 0)) hit @ g.booms }
    else g
  in
  (* firing, held: a shot every 9 frames *)
  let h = g.hero in
  let g =
    if keys.kspace && h.cooldown = 0 && h.rolled = 0 && h.beam = None then { g with shots = fire h @ g.shots; hero = { h with cooldown = 9 } } else g
  in
  (* the shots: stopped by rock (cracking a P block); a spread shot is
   * spent on the first enemy, the laser goes through them all *)
  let flying = List.map (fun (w, s) -> (w, Shots.advance s)) g.shots in
  let in_rock (s : Shots.t) = match Tilemap.tile_at g.map s.x s.y with Some c -> solid c | None -> true in
  let map = List.fold_left (fun m (_, (s : Shots.t)) -> if in_rock s then crack m (s.x, s.y) else m) g.map flying in
  let flying = List.filter (fun (_, s) -> (not (in_rock s)) && Float.abs (s.x - g.hero.x) < 700.) flying in
  let hit_by (e : enemy) (s : Shots.t) = Shots.near 24. (e.ex, e.ey) s in
  let beam = match g.hero.beam with Some a -> Some (beam_ends g.map g.hero a) | None -> None in
  let enemies =
    List.map
      (fun e ->
        let shot = List.fold_left (fun d (w, s) -> if hit_by e s then d + (match w with Laser -> 0.35 | Spread _ -> 1.) else d) 0. flying in
        let beamed = match beam with Some seg when to_segment (e.ex, e.ey) seg < 26. -> 0.12 | _ -> 0. in
        { e with hp = e.hp - shot - beamed })
      g.enemies
  in
  let shots = List.filter (fun (w, s) -> w = Laser || not (List.exists (fun e -> hit_by e s) g.enemies)) flying in
  let dead, alive = List.partition (fun e -> e.hp <= 0.) enemies in
  (* the rolling wheel runs enemies over *)
  let h = g.hero in
  let touching e = Float.abs (e.ex - h.x) < 28. && Float.abs (e.ey - h.y) < 40. in
  let crushed, alive = if h.rolled > 0 then List.partition touching alive else ([], alive) in
  let dead = dead @ crushed in
  let g =
    { g with map; shots; enemies = List.map (step_enemy map g.frames) alive; booms = List.map (fun e -> (e.ex, e.ey, 0)) dead @ g.booms;
      score = g.score +.. List.fold_left (fun s e -> s +.. points e.kind) 0 dead }
  in
  (* the turrets' bolts *)
  let bolts = List.filter_map (turret_fires g.frames g.hero) g.enemies @ List.map Shots.advance g.bolts in
  let bolts = List.filter (fun (s : Shots.t) -> match Tilemap.tile_at g.map s.x s.y with Some c -> not (solid c) | None -> false) bolts in
  (* what hurts him: an enemy touched, a bolt, the spikes *)
  let h = g.hero in
  let h = if List.exists touching g.enemies then damage 20. h else h in
  let hit_bolts, bolts = List.partition (Shots.near 22. (h.x, h.y)) bolts in
  let h = if hit_bolts <> [] then damage 15. h else h in
  let feet = Tilemap.tile_at g.map h.x (h.y - (snd (size h) / 2.) - 2.) in
  let h = if feet = Some '^' then damage 25. h else h in
  let g = pick_up { g with hero = h; bolts } in
  { g with cam = Camera2d.window 240. 200. g.hero.x g.hero.y g.cam |> Camera2d.clamp computer.screen (Tilemap.bounds level) }

let teleported (g : game) : bool = Tilemap.tile_at g.map g.hero.x g.hero.y = Some 'E'

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if teleported g then Scene2d.go (Teleported g.score) s
      else if g.hero.energy <= 0. then
        if g.lives <= 1 then Scene2d.go (Game_over g.score) s
        else
          (* back at the checkpoint, the weapon kept *)
          let g = { g with lives = g.lives -.. 1; hero = new_hero g.checkpoint g.hero.weapon; bolts = []; shots = [] } in
          { s with scene = Playing g }
      else { s with scene = Playing g }
  | Teleported _ | Game_over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let tile_shape (c : char) : shape =
  let rock = rgb 70 60 90 in
  match c with
  | '#' -> group [ square rock tile; square (rgb 90 78 112) (tile - 6.); rectangle (rgb 60 52 78) 14. 4. |> move (-6.) 8.; rectangle (rgb 60 52 78) 4. 10. |> move 10. (-8.) ]
  | '=' -> group [ rectangle (rgb 150 160 170) tile 14. |> move_y 13.; rectangle (rgb 90 100 110) tile 4. |> move_y 4.; circle (rgb 60 60 70) 2. |> move (-12.) 14.; circle (rgb 60 60 70) 2. |> move 12. 14. ]
  | '^' -> group (List.init 3 (fun i -> triangle (rgb 200 210 220) 7. |> move ((float_of_int i * 13.) - 13.) (-14.)))
  | 'P' | '1' .. '4' ->
      let cracks = match c with 'P' -> 0 | c -> 5 -.. (Char.code c -.. Char.code '0') in
      group
        ([ square (rgb 200 120 40) tile; square (rgb 240 170 60) (tile - 8.); text (rgb 120 60 20) 1.4 "P" ]
        @ List.init cracks (fun i -> rectangle black 2. 18. |> rotate (float_of_int (i *.. 70)) |> move ((float_of_int i * 6.) - 12.) 6.))
  | '*' -> group [ polygon (rgb 120 220 255) [ (0., 11.); (9., 0.); (0., -11.); (-9., 0.) ]; polygon white [ (0., 11.); (4., 5.); (-4., 5.) ] ]
  | 'S' -> group [ circle (rgb 220 60 60) 14.; text white 1.6 "S" ]
  | 'L' -> group [ circle (rgb 60 120 240) 14.; text white 1.6 "L" ]
  | 'W' -> group [ circle (rgb 60 180 80) 14.; text white 1.6 "W" ]
  | 'C' -> group [ rectangle (rgb 120 120 120) 4. 36. |> move_x (-12.); polygon (rgb 60 220 120) [ (-10., 18.); (14., 10.); (-10., 2.) ] ]
  | 'E' -> group [ rectangle (rgb 40 40 80) 36. tile; rectangle (rgb 120 200 255) 26. tile |> fade 0.6 ]
  | _ -> group []

(* the hero in his suit, facing right, two frames of a run *)
let hero_rows =
  let top = [ "..GGGG.."; ".GGGGGG."; ".GVVVVG."; ".GGGGGG."; "..GGGG.."; "GGGGGGGG"; "GGYYGGGG"; "GGGGGGAA"; "GGGGGGAA"; ".GGGGGG."; ".DDDDDD." ] in
  [ top @ [ ".GG..GG."; ".GG..GG."; ".GG..GG."; "DDD..DDD" ]; top @ [ "..GGGG.."; "..GGGG.."; "..GGGG.."; "..DDDD.." ] ]

let palette = [ ('G', rgb 160 170 180); ('V', rgb 80 200 255); ('Y', rgb 240 200 60); ('A', rgb 90 90 100); ('D', rgb 80 80 100) ]

(* the bolt of the beam: a polyline from its start to its end, each joint
 * pushed sideways by a jag changing every frame, drawn as thin
 * rectangles *)
let bolt (frames : int) (((ax, ay), (bx, by)) : (number * number) * (number * number)) : shape list =
  let n = 12 in
  let len = Float.hypot (bx - ax) (by - ay) in
  let nx, ny = if len = 0. then (0., 0.) else (-.(by - ay) / len, (bx - ax) / len) in
  let point i =
    let t = float_of_int i / float_of_int n in
    let jag = if i = 0 || i = n then 0. else 9. * sin (float_of_int ((frames *.. 7) +.. (i *.. 13))) in
    (ax + (t * (bx - ax)) + (jag * nx), ay + (t * (by - ay)) + (jag * ny))
  in
  let segment (x1, y1) (x2, y2) w c = rectangle c (Float.hypot (x2 - x1) (y2 - y1) + 2.) w |> rotate (atan2 (y2 - y1) (x2 - x1) |> radians_to_degrees) |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.) in
  List.concat (List.init n (fun i -> let p, q = (point i, point (i +.. 1)) in [ segment p q 7. (rgb 120 160 255) |> fade 0.5; segment p q 2. white ]))

let view_enemy (frames : int) (e : enemy) : shape =
  match e.kind with
  | Walker ->
      let leg = if (frames /.. 8) mod 2 = 0 then 5. else -5. in
      group [ oval (rgb 200 80 60) 34. 26. |> move_y 4.; circle yellow 4. |> move (e.dir * 8.) 8.; rectangle (rgb 120 50 40) 4. 14. |> move (-8. + leg) (-12.); rectangle (rgb 120 50 40) 4. 14. |> move (8. - leg) (-12.) ] |> move e.ex e.ey
  | Flyer -> group [ oval (rgb 180 100 220) 30. 18.; oval (rgb 230 200 255) (if (frames /.. 4) mod 2 = 0 then 40. else 14.) 6. |> move_y 8.; circle white 3. |> move (e.dir * 8.) 2. ] |> move e.ex e.ey
  | Turret -> group [ rectangle (rgb 110 110 120) 36. 24. |> move_y (-6.); circle (rgb 150 150 160) 12. |> move_y 8.; rectangle (rgb 60 60 70) 22. 6. |> move (-12.) 10. ] |> move e.ex e.ey

let view_game (screen : screen) (g : game) : shape list =
  let h = g.hero in
  let hero =
    if h.hurt > 0 && (h.hurt /.. 3) mod 2 = 0 then []
    else if h.rolled > 0 then [ group [ circle (rgb 160 170 180) 16.; rectangle (rgb 80 200 255) 30. 4.; rectangle (rgb 80 200 255) 4. 30. ] |> rotate (float_of_int h.steps * -20. * h.facing) |> move h.x h.y ]
    else [ Sprite.pixels 3.5 palette (let r = Sprite.cycle (h.steps /.. 5) hero_rows in if h.facing < 0. then Sprite.flip r else r) |> move h.x h.y ]
  in
  let world =
    [ Tilemap.view_visible (Camera2d.visible screen g.cam) tile_shape g.map ]
    @ List.map (view_enemy g.frames) g.enemies
    @ List.map (fun (w, (s : Shots.t)) -> match w with Laser -> rectangle (rgb 120 200 255) 40. 4. |> move s.x s.y | Spread _ -> circle (rgb 255 230 120) 5. |> move s.x s.y) g.shots
    @ List.map (fun (s : Shots.t) -> circle (rgb 255 90 60) 6. |> move s.x s.y) g.bolts
    @ List.map (fun (x, y, n) -> circle (if n mod 4 < 2 then orange else yellow) (10. + (float_of_int n * 2.)) |> fade (1. - (float_of_int n / 20.)) |> move x y) g.booms
    @ (match h.beam with Some a -> bolt g.frames (beam_ends g.map h a) | None -> [])
    @ hero
  in
  let bar = 200. * Float.max 0. h.energy / 100. in
  let weapon = match h.weapon with Laser -> "LASER" | Spread n -> Printf.sprintf "SPREAD %d" n in
  [ rectangle (rgb 20 16 34) screen.width screen.height; Camera2d.view g.cam world ]
  @ (if g.flash > 0 then [ rectangle white screen.width screen.height |> fade (float_of_int g.flash / 25.) ] else [])
  @ [ rectangle black screen.width 50. |> move_y (screen.top - 25.);
      text white 1.8 (Printf.sprintf "LIVES %d" g.lives) |> move (screen.left + 60.) (screen.top - 25.);
      rectangle (rgb 60 60 60) 204. 16. |> move (screen.left + 260.) (screen.top - 25.);
      rectangle (if h.energy > 30. then green else red) bar 12. |> move (screen.left + 160. + (bar / 2.)) (screen.top - 25.);
      text white 1.8 (Printf.sprintf "%s   WHEELS %d   LINES %d   DIAMONDS %d   %d" weapon h.wheels h.lines g.diamonds g.score) |> move 170. (screen.top - 25.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let g = new_game () in
      view_game screen { g with cam = Camera2d.clamp screen (Tilemap.bounds level) g.cam }
      @ [ rectangle black 860. 300. |> fade 0.85 |> move_y 40.; text (rgb 120 200 255) 7. "TINY TURRICAN" |> move_y 140.;
          text white 2.2 "left/right run   up jump   space fire   down wheel" |> move_y 75.;
          text white 2.2 "x held: the lightning beam, up/down to sweep it   z power line" |> move_y 40.;
          text white 2.2 "find the teleporter at the far right" |> move_y 5. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-60.) ]
  | Playing g -> view_game screen g
  | Teleported score -> [ rectangle black screen.width screen.height; text (rgb 120 200 255) 6. "TELEPORTED!"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over score -> [ rectangle black screen.width screen.height; text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app