(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Gradius (Konami, 1985): a horizontal shoot 'em up,
 * the screen scrolling by itself through a cave, waves of enemies, and
 * the power-up bar. Arrows to fly, space to fire, x to take the power-up
 * the bar points at.
 *
 * Gradius's idea is the bar: SPEED UP, MISSILE, DOUBLE, LASER, OPTION,
 * ?. Each red capsule -- dropped by a red enemy, or by a whole formation
 * shot down -- moves the cursor one step along it, and you choose when
 * to take what it points at: spend capsules at once on speed, or save
 * them for the options. Scramble (Konami, 1981) had the cave, R-Type
 * (Irem, 1987) the charged beam; the bar is Gradius's own. At the end of
 * the stage, the Big Core: a core behind three walls, to shoot through.
 * (Names and dates from memory, to check.)
 *
 * What's new here:
 *
 *  - The level is a timeline ([waves]): at frame 120, five fans at the
 *    top; at 270, five at the bottom... A shooter's level is less a map
 *    than a script, and scripting it is the designer's art (in Gradius,
 *    and in bullet-hell games, where the script is thousands of lines).
 *    Each wave flies along a Path of the shoot 'em up kit, in the
 *    screen's coordinates, so that the scrolling doesn't bend it.
 *
 *  - The cave is two strings of digits ([ground], [ceiling]): the
 *    heights of the rock, one digit per column, a tile map made from a
 *    profile (and drawn one rectangle per column, not per tile).
 *
 *  - The camera moves by itself, 2 pixels a frame, and the ship is kept
 *    inside the screen ([clamp_ship]); it stops at the end, for the boss.
 *
 *  - The options, the orbs following the ship and firing with it: they
 *    are where the ship was 16 and 32 moves ago ([trail]), which is why
 *    they snake behind it.
 *
 * What it uses: the shoot 'em up kit (gamekits/shmup/: Path for the waves'
 * flights, Shots for every shot, bullet and laser -- its third game,
 * after TinyInvaders and TinyGalaga), Tilemap (the cave, for
 * the collisions), Camera2d (the scrolling), Sprite (the ship), Scene2d,
 * Audio. Not Physics: nothing has inertia in a shooter; the ship moves at
 * its speed or not at all.
 *
 * Exercises: the missiles following the ground, the volcanoes' rocks,
 * the Moai of stage 3, the ? as a shield taking hits from the front
 * (here it takes any 4), a Waves module for the kit when a second game
 * has a timeline, the options' own lasers.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The cave *)
(*****************************************************************************)

(* the rock's height, in tiles, one digit per column: from the bottom,
 * and from the top *)
let ground =
  "1111111112222222233333322111111111111112222222221112222334444433221111111111111111111111111122333444444332211111111111111111111111"
let ceiling =
  "0000000000000000000000000000000001111111111111111111222222333222211100000000000000000000011111000000001111122223333332221110000000"

let tile = 50.
let rows = 18
let cols = String.length ground
let digit (s : string) (col : int) : int = Char.code s.[col] -.. Char.code '0'

let cave : Tilemap.t =
  Tilemap.of_strings tile
    (List.init rows (fun row -> String.init cols (fun col -> if row >= rows -.. digit ground col || row < digit ceiling col then '#' else ' ')))

let bounds = Tilemap.bounds cave
let rock (x : number) (y : number) : bool = Tilemap.tile_at cave x y = Some '#'

(* the top of the ground at a column *)
let ground_top (col : int) : number = bounds.bottom + (float_of_int (digit ground col) * tile)

(* the ground turrets, on the rock, at these columns *)
let turret_cols = [ 20; 34; 47; 62; 80; 95; 110 ]

(*****************************************************************************)
(* The waves *)
(*****************************************************************************)

(* the flights, in the screen's coordinates (the camera's center at (0,
 * 0)): fans coming in straight then swooping away; a sine wave; a red
 * one flying straight across *)
let fans (y : number) : Path.t =
  let s = if y > 0. then -1. else 1. in
  Path.make [ (560., y); (250., y); (80., y + (s * 60.)); (0., y + (s * 200.)); (100., y + (s * 330.)); (560., y + (s * 380.)) ]

let sine (y : number) : Path.t = Path.make (List.init 12 (fun i -> (560. - (float_of_int i * 100.), y + if i mod 2 = 0 then 90. else -90.)))
let straight (y : number) : Path.t = Path.make [ (560., y); (-560., y) ]

type kind = Fan | Red

(* the timeline: at this frame, these enemies (path, kind, how many, frames
 * between two) *)
let waves : (int * (Path.t * kind * int * int)) list =
  List.concat
    (List.init 16 (fun i ->
         let t = 120 +.. (i *.. 160) in
         match i mod 4 with
         | 0 -> [ (t, (fans 280., Fan, 5, 12)) ]
         | 1 -> [ (t, (fans (-280.), Fan, 5, 12)) ]
         | 2 -> [ (t, (sine 60., Fan, 5, 16)); (t +.. 80, (straight (-120.), Red, 1, 0)) ]
         | _ -> [ (t, (fans 200., Fan, 5, 12)); (t +.. 40, (fans (-200.), Fan, 5, 12)) ]))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type enemy = { path : Path.t; s : number; wait : int; kind : kind; group : int; x : number; y : number }
type shot_kind = Normal | Missile | Laser
type shot = { shot : Shots.t; kind : shot_kind }

(* the bar, and what's been taken *)
type power = { cursor : int; (* -1: none *) speed : int; missile : bool; double : bool; laser : bool; options : int; shield : int }

type boss = { bx : number; by : number; walls : int list; (* each wall's hits left, front first *) core : int; reload : int }

(* the end of the stage: the boss still ahead, fighting it, or beaten n
 * frames ago *)
type fight = Ahead | Boss of boss | Beaten of int

type game = {
  cam : number; (* the camera's x *)
  sx : number; (* the ship *)
  sy : number;
  trail : (number * number) list; (* where it was, on the screen, most recent first *)
  dead : int; (* frames since it blew up, 0 if flying *)
  power : power;
  shots : shot list;
  enemies : enemy list;
  groups : (int * int) list; (* each formation: its id, and how many are left to shoot down *)
  capsules : (number * number) list;
  turrets : (number * number * int) list; (* where, and frames before firing *)
  bullets : Shots.t list; (* the enemies', and the boss's lasers *)
  explosions : (number * number * int) list;
  boss : fight;
  score : int;
  lives : int;
  frames : int;
}

type scene = Title | Playing of game | Clear of int | Game_over of int
type model = scene Scene2d.t

let no_power = { cursor = -1; speed = 0; missile = false; double = false; laser = false; options = 0; shield = 0 }
let start_cam = bounds.left + 500.

let new_game () : game =
  { cam = start_cam; sx = start_cam - 300.; sy = 0.; trail = []; dead = 0; power = no_power; shots = []; enemies = []; groups = []; capsules = [];
    turrets = List.map (fun c -> (fst (Tilemap.center cave c 0), ground_top c + 15., 60 +.. (c *.. 7 mod 60))) turret_cols;
    bullets = []; explosions = []; boss = Ahead; score = 0; lives = 3; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The ship and its power-ups *)
(*****************************************************************************)

let bar = [| "SPEED"; "MISSILE"; "DOUBLE"; "LASER"; "OPTION"; "?" |]
let scroll = 2.
let last_cam = bounds.right - 500.

(* [take p]: what the cursor points at, if it can still be taken *)
let take (p : power) : power =
  let p' =
    match p.cursor with
    | 0 when p.speed < 4 -> Some { p with speed = p.speed +.. 1 }
    | 1 when not p.missile -> Some { p with missile = true }
    | 2 when not p.double -> Some { p with double = true; laser = false }
    | 3 when not p.laser -> Some { p with laser = true; double = false }
    | 4 when p.options < 2 -> Some { p with options = p.options +.. 1 }
    | 5 when p.shield = 0 -> Some { p with shield = 4 }
    | _ -> None
  in
  match p' with Some p -> Audio.play Audio.jump; { p with cursor = -1 } | None -> p

(* the ship stays on the screen *)
let clamp_ship (g : game) (x : number) (y : number) : number * number = (clamp (g.cam - 470.) (g.cam + 470.) x, clamp (bounds.bottom + 20.) (bounds.top - 20.) y)

(* the options: where the ship was on the screen 16 and 32 moves ago *)
let options (g : game) : (number * number) list =
  List.filteri (fun i _ -> i < g.power.options) (List.filter_map (fun n -> List.nth_opt g.trail n) [ 15; 31 ])
  |> List.map (fun (x, y) -> (g.cam + x, y))

(* a volley from the ship or an option at (x, y) *)
let volley (p : power) ((x, y) : number * number) : shot list =
  (if p.laser then [ { shot = Shots.straight (x + 40.) y 20. 0.; kind = Laser } ] else [ { shot = Shots.straight (x + 20.) y 14. 0.; kind = Normal } ])
  @ (if p.double then [ { shot = Shots.straight (x + 15.) (y + 8.) 10. 10.; kind = Normal } ] else [])
  @ if p.missile then [ { shot = Shots.straight x (y - 10.) 6. (-6.); kind = Missile } ] else []

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let blow (x : number) (y : number) (g : game) : game = { g with explosions = (x, y, 0) :: g.explosions }

(* the waves due at this frame, and each wave's enemies, one after the
 * other along its path *)
let spawn (g : game) : game =
  List.fold_left
    (fun g (t, (path, kind, n, gap)) ->
      if t <> g.frames then g
      else
        let group = t in
        { g with enemies = g.enemies @ List.init n (fun i -> { path; s = 0.; wait = i *.. gap; kind; group; x = 0.; y = 0. }); groups = (group, n) :: g.groups })
    g waves

(* an enemy along its path, [g.cam] away: the path's point plus the
 * camera; gone off its end *)
let fly (g : game) (e : enemy) : enemy option =
  if e.wait > 0 then Some { e with wait = e.wait -.. 1 }
  else if e.s > Path.length e.path then None
  else
    let (px, py), _ = Path.at e.path e.s in
    Some { e with s = e.s + (if e.kind = Red then 4. else 5.); x = g.cam + px; y = py }

let visible (e : enemy) : bool = e.wait = 0 && e.s > 0.

(* the ship's shots hitting enemies, turrets, the boss; a laser goes on *)
let shoot_down (g : game) : game =
  let hit_enemy (s : shot) e = visible e && Shots.near 26. (e.x, e.y) s.shot in
  let g =
    List.fold_left
      (fun g (s : shot) ->
        match List.find_opt (hit_enemy s) g.enemies with
        | Some e ->
            Audio.play Audio.hit;
            let left = List.assoc e.group g.groups -.. 1 in
            let drop = e.kind = Red || left = 0 in
            let g = blow e.x e.y { g with enemies = List.filter (fun e' -> e' != e) g.enemies; groups = (e.group, left) :: List.remove_assoc e.group g.groups; score = g.score +.. 100 } in
            let g = if drop then { g with capsules = (e.x, e.y) :: g.capsules } else g in
            if s.kind = Laser then { g with shots = s :: g.shots } else g
        | None -> (
            match List.find_opt (fun (tx, ty, _) -> Shots.near 26. (tx, ty) s.shot) g.turrets with
            | Some ((tx, ty, _) as t) -> blow tx ty { g with turrets = List.filter (fun t' -> t' != t) g.turrets; score = g.score +.. 100 }
            | None -> { g with shots = s :: g.shots }))
      { g with shots = [] } g.shots
  in
  (* the boss: its walls first, front to back, then the core *)
  match g.boss with
  | Ahead | Beaten _ -> g
  | Boss b ->
      let core_x = b.bx - 40. in
      let wall_x i = core_x - 40. - (float_of_int i * 22.) in
      List.fold_left
        (fun g (s : shot) ->
          match g.boss with
          | Ahead | Beaten _ -> { g with shots = s :: g.shots }
          | Boss b ->
              let front = List.length b.walls -.. 1 in
              if Float.abs (s.shot.y - b.by) > 30. then { g with shots = s :: g.shots }
              else if b.walls <> [] && s.shot.x > wall_x front - 10. then (
                Audio.play Audio.hit;
                let walls = match List.rev b.walls with 1 :: rest -> List.rev rest | n :: rest -> List.rev ((n -.. 1) :: rest) | [] -> [] in
                { g with boss = Boss { b with walls }; score = g.score +.. 50 })
              else if b.walls = [] && s.shot.x > core_x - 20. then (
                Audio.play Audio.hit;
                if b.core <= 1 then (Audio.play Audio.explosion; blow b.bx b.by { g with boss = Beaten 0; score = g.score +.. 5000 })
                else { g with boss = Boss { b with core = b.core -.. 1 } })
              else { g with shots = s :: g.shots })
        { g with shots = [] } g.shots

(* the boss: in from the right when the scrolling stops and the last
 * wave is gone, then up and down after the ship, four lasers at a time *)
let step_boss (g : game) : game =
  match g.boss with
  | Ahead -> if g.cam >= last_cam && g.enemies = [] then { g with boss = Boss { bx = g.cam + 700.; by = 0.; walls = [ 3; 3; 3 ]; core = 4; reload = 120 } } else g
  | Beaten n -> { g with boss = Beaten (n +.. 1) }
  | Boss b ->
      let bx = Float.max (g.cam + 320.) (b.bx - 3.) in
      let by = b.by + clamp (-1.5) 1.5 (g.sy - b.by) in
      let lasers = if b.reload = 0 then List.map (fun dy -> Shots.straight (bx - 80.) (by + dy) (-12.) 0.) [ -110.; -50.; 50.; 110. ] else [] in
      { g with boss = Boss { b with bx; by; reload = (if b.reload = 0 then 90 else b.reload -.. 1) }; bullets = lasers @ g.bullets }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed k = Scene2d.pressed k scenes in
  let g = { g with frames = g.frames +.. 1; cam = Float.min last_cam (g.cam + scroll) } in
  let g = { g with explosions = List.filter_map (fun (x, y, n) -> if n < 25 then Some (x, y, n +.. 1) else None) g.explosions } in
  (* the ship: flying, firing, taking a power-up; or blown up, and back
   * after 1.5 s, without its power-ups *)
  let g =
    if g.dead > 0 then if g.dead > 90 && g.lives > 0 then { g with dead = 0; sx = g.cam - 300.; sy = 0.; trail = []; power = no_power } else { g with dead = g.dead +.. 1 }
    else
      let k = computer.keyboard in
      let speed = 4. + (1.5 * float_of_int g.power.speed) in
      let sx, sy = clamp_ship g (g.sx + scroll + (speed * to_x k)) (g.sy + (speed * to_y k)) in
      let moved = to_x k <> 0. || to_y k <> 0. in
      let trail = if moved then List.filteri (fun i _ -> i < 40) ((sx - g.cam, sy) :: g.trail) else g.trail in
      let g = { g with sx; sy; trail } in
      let power = if pressed (fun k -> Set_.mem "x" k.keys) then take g.power else g.power in
      let firing = pressed (fun k -> k.kspace) && List.length (List.filter (fun s -> s.kind <> Missile) g.shots) < 3 *.. (1 +.. g.power.options) in
      if firing then Audio.play Audio.laser;
      { g with power; shots = (if firing then List.concat_map (volley g.power) ((sx, sy) :: options g) else []) @ g.shots }
  in
  (* the shots, the waves, the turrets, the boss *)
  let alive_shot (s : shot) = Float.abs (s.shot.x - g.cam) < 560. && Float.abs s.shot.y < 480. && not (rock s.shot.x s.shot.y) in
  let g = { g with shots = List.filter alive_shot (List.map (fun s -> { s with shot = Shots.advance s.shot }) g.shots) } in
  let g = spawn g in
  let g = { g with enemies = List.filter_map (fly g) g.enemies } in
  let turrets, fired =
    List.split
      (List.map
         (fun (x, y, n) ->
           let near = Float.abs (x - g.cam) < 500. && g.dead = 0 in
           if n > 0 then ((x, y, n -.. 1), []) else ((x, y, 110), if near then [ Shots.aimed 5. (x, y) (g.sx, g.sy) ] else []))
         g.turrets)
  in
  let bullets = List.filter (fun (b : Shots.t) -> Float.abs (b.x - g.cam) < 560. && Float.abs b.y < 480. && not (rock b.x b.y)) (List.map Shots.advance g.bullets) in
  let g = { g with turrets; bullets = bullets @ List.concat fired } |> step_boss |> shoot_down in
  (* the capsules: taken, the cursor one step further *)
  let taken, capsules = List.partition (fun (x, y) -> g.dead = 0 && Float.hypot (x - g.sx) (y - g.sy) < 36.) g.capsules in
  if taken <> [] then Audio.play Audio.coin;
  let g = { g with capsules = List.filter (fun (x, _) -> x > g.cam - 560.) capsules; power = { g.power with cursor = (if taken = [] then g.power.cursor else (g.power.cursor +.. List.length taken) mod 6) } } in
  (* the ship hit: the rock, an enemy, a bullet (the ? takes 4), the boss *)
  if g.dead > 0 then g
  else
    let bullet = List.find_opt (Shots.near 16. (g.sx, g.sy)) g.bullets in
    let rammed = List.exists (fun e -> visible e && Float.hypot (e.x - g.sx) (e.y - g.sy) < 30.) g.enemies in
    let crashed = Tilemap.hits (fun c -> c = '#') cave g.sx g.sy 40. 16. || (match g.boss with Boss b -> Float.abs (b.bx - g.sx) < 90. && Float.abs (b.by - g.sy) < 130. | Ahead | Beaten _ -> false) in
    match bullet with
    | Some b when g.power.shield > 0 && not (rammed || crashed) -> { g with bullets = List.filter (fun b' -> b' != b) g.bullets; power = { g.power with shield = g.power.shield -.. 1 } }
    | _ when bullet <> None || rammed || crashed ->
        Audio.play Audio.explosion;
        blow g.sx g.sy { g with dead = 1; lives = g.lives -.. 1; bullets = [] }
    | _ -> g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if (match g.boss with Beaten n -> n > 120 | _ -> false) then Scene2d.go (Clear g.score) s
      else if g.lives = 0 && g.dead > 90 then Scene2d.go (Game_over g.score) s
      else { s with scene = Playing g }
  | Clear _ | Game_over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let ship_rows = [ "..##......."; ".####......"; "BBBBBBBBB.."; "WWWWWWWWWWW"; "BBBBBBBBB.."; ".####......"; "..##......." ]
let ship = Sprite.pixels 4. [ ('#', rgb 120 140 170); ('B', rgb 70 110 230); ('W', white) ] ship_rows

(* the stars, three layers scrolling at different speeds *)
let stars (g : game) : shape list =
  List.init 90 (fun i ->
      let depth = float_of_int (1 +.. (i mod 3)) in
      let x = Float.rem ((float_of_int ((i *.. 137) mod 1000)) - ((g.cam - start_cam) * depth / 4.)) 1000. in
      let x = (if x < 0. then x + 1000. else x) - 500. in
      rectangle (rgb 150 150 200) depth depth |> move x (float_of_int ((i *.. 311) mod 900) - 450.))

(* the cave, one rectangle per column for the ground and one for the
 * ceiling, only the columns on the screen *)
let view_cave (g : game) : shape list =
  let first = max 0 (int_of_float ((g.cam - 520. - bounds.left) / tile)) and last = min (cols -.. 1) (int_of_float ((g.cam + 520. - bounds.left) / tile)) in
  List.concat
    (List.init (max 0 (last -.. first +.. 1)) (fun i ->
         let c = first +.. i in
         let x = bounds.left + ((float_of_int c + 0.5) * tile) in
         let gh = float_of_int (digit ground c) * tile and ch = float_of_int (digit ceiling c) * tile in
         [ rectangle (rgb 150 90 50) tile gh |> move x (bounds.bottom + (gh / 2.)); rectangle (rgb 90 200 90) tile 6. |> move x (bounds.bottom + gh - 3.);
           rectangle (rgb 150 90 50) tile ch |> move x (bounds.top - (ch / 2.)) ]))

let view_enemy (g : game) (e : enemy) : shape list =
  if not (visible e) then []
  else
    let color = if e.kind = Red then red else rgb 90 200 255 in
    [ group [ polygon color [ (-18., 0.); (0., 14.); (18., 0.); (0., -14.) ]; circle white 5. ] |> rotate (float_of_int g.frames * 8.) |> move e.x e.y ]

let view_boss (b : boss) : shape list =
  let core_x = b.bx - 40. in
  [ rectangle (rgb 100 110 140) 160. 240. |> move b.bx b.by; rectangle (rgb 60 70 100) 60. 280. |> move (b.bx + 30.) b.by;
    circle (if b.walls = [] then rgb 90 200 255 else rgb 40 80 140) 22. |> move core_x b.by ]
  @ List.mapi (fun i hits -> rectangle (rgb (100 +.. (hits *.. 50)) 100 100) 14. 50. |> move (core_x - 40. - (float_of_int i * 22.)) b.by) (List.rev b.walls)

let view_bar (p : power) : shape list =
  List.concat
    (List.mapi
       (fun i name ->
         let x = -375. + (float_of_int i * 150.) in
         [ rectangle (if i = p.cursor then rgb 240 160 40 else rgb 40 50 90) 140. 34. |> move x (-475.);
           text (if i = p.cursor then black else white) 2. name |> move x (-475.) ])
       (Array.to_list bar))

let view_game (g : game) : shape list =
  let world =
    view_cave g
    @ List.map (fun (x, y, _) -> group [ rectangle (rgb 200 60 60) 30. 20.; rectangle (rgb 200 60 60) 20. 6. |> move (-14.) 10. ] |> move x y) g.turrets
    @ List.concat_map (view_enemy g) g.enemies
    @ List.map (fun (x, y) -> group [ oval red 30. 20.; oval (rgb 255 200 200) 14. 8. |> move (-5.) 4. ] |> move x y) g.capsules
    @ (match g.boss with Boss b -> view_boss b | Ahead | Beaten _ -> [])
    @ List.map (fun (s : shot) -> match s.kind with Laser -> rectangle (rgb 120 220 255) 80. 4. |> move s.shot.x s.shot.y | Missile -> rectangle yellow 10. 5. |> rotate (-45.) |> move s.shot.x s.shot.y | Normal -> rectangle white 14. 4. |> rotate (Shots.angle s.shot) |> move s.shot.x s.shot.y) g.shots
    @ List.map (fun (b : Shots.t) -> if Float.abs b.vx > 10. then rectangle (rgb 255 120 200) 60. 5. |> move b.x b.y else circle orange 5. |> move b.x b.y) g.bullets
    @ List.map (fun (x, y) -> group [ circle (rgb 255 120 40) 12.; circle yellow 6. ] |> move x y) (options g)
    @ (if g.dead = 0 then [ ship |> move g.sx g.sy ] @ (if g.power.shield > 0 then [ circle (rgb 90 180 255) 32. |> fade 0.3 |> move g.sx g.sy ] else []) else [])
    @ List.map (fun (x, y, n) -> circle (if n mod 6 < 3 then orange else yellow) (10. + (float_of_int n * 2.)) |> fade (1. - (float_of_int n / 25.)) |> move x y) g.explosions
  in
  stars g
  @ [ Camera2d.view (Camera2d.origin |> Camera2d.look_at g.cam 0.) world ]
  @ view_bar g.power
  @ [ text white 2.5 (Printf.sprintf "SCORE %d   SHIPS %d" g.score g.lives) |> move (-300.) 475. ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_game ())
      @ [ rectangle black 760. 260. |> fade 0.85 |> move_y 60.; text (rgb 90 200 255) 7. "TINY GRADIUS" |> move_y 140.;
          text white 2.3 "arrows fly   space fire   x take the power-up" |> move_y 70.;
          text white 2.3 "red capsules move the cursor along the bar" |> move_y 35. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-20.) ]
  | Playing g -> view_game g
  | Clear score -> [ text (rgb 90 200 255) 6. "STAGE CLEAR"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over score -> [ text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
