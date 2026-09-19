(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of The Legend of Zelda (Shigeru Miyamoto and Takashi
 * Tezuka, Nintendo, 1986): an overworld of screens, a sword to find, a
 * key, a locked door, a dungeon, the Triforce. Arrows to walk, space to
 * swing the sword (once you have it). Octoroks spit rocks -- your shield
 * stops those coming at your face; keese flutter over walls; three
 * hearts.
 *
 * Zelda was Miyamoto's answer to Super Mario Bros., the same year: not a
 * course to run from left to right, but a land to explore, in any order,
 * with secrets (it came with a battery, to save your game: a first on a
 * console). Its overworld is 16 x 8 screens of 16 x 11 tiles each; when
 * you walk off one, the next slides in. Adventure (Warren Robinett,
 * Atari 2600, 1980) had already the rooms, the key, the dragon and the
 * sword. (Names and dates from memory, to check.)
 *
 * What's new here:
 *
 *  - The world in rooms: the camera is on the room Link is in
 *    (Camera2d.room and flip, with games/TinyRick), but slides to the
 *    next one when he walks off the edge, the game frozen meanwhile
 *    ([slide]); outside the room, black ([view_game]'s masks), as a TV
 *    screen showed one room at a time.
 *
 *  - An inventory ([sword], [keys], [hearts]): what Link carries opens
 *    the world -- no sword, no fight; no key, no dungeon. The whole
 *    Zelda series is this lock-and-key design, grown up.
 *
 *  - Monsters that wander ([wander]): a direction, kept a while or until
 *    a wall, then another, from a pseudo-random generator whose state is
 *    in the model (a linear congruential generator, [next]): every game
 *    the same, and still looking random.
 *
 *  - The shield ([shielded]): a rock flying at Link's face, while he
 *    isn't swinging the sword, is stopped -- one dot product: the rock's
 *    direction against the one he faces.
 *
 * What it uses: the platformer kit's Tile_move (kits/platformer/: seen
 * from above, Link and the octoroks are boxes against the trees and
 * rocks, one pixel at a time, with no gravity), the shoot 'em up kit's
 * Shots (the rocks), Tilemap (the world, changed by the items taken and
 * the door opened), Camera2d (rooms), Sprite (Link), Scene2d. Not
 * Physics: nothing slides or bounces, but the knockback when Link is hit.
 *
 * Exercises: more rooms (the original's overworld is 128 screens), the
 * sword beam at full hearts, bombs and secret walls, rupees and a shop,
 * the Aquamentus dragon guarding the Triforce, saving the game.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

(* 3 x 2 rooms of 16 x 11 tiles: T trees, R rocks, W water, X a
 * dungeon's walls and . its floor, D a locked door; S the sword, k a key,
 * h a heart container, * the Triforce; L Link, m the old man, o
 * octoroks, b keese *)
let world_rows =
  [ "RRRRRRRRRRRRRRRRTTTTTTTTTTTTTTTTRRRRRRRRRRRRRRRR";
    "RRRRRR    RRRRRRT              TR              R";
    "RRRRR  m S  RRRRT  TT      TT  TR   WWWWWWW    R";
    "R              RT  TT  o   TT  TR  WWWWWWWWW   R";
    "R    TT    TT         TTTT         WWWWWWWWW o R";
    "R      L              Tk T          WWWWWWW    R";
    "R    TT    TT         T  T   o              h  R";
    "R              RT      o       TR   o          R";
    "R              RT  TT      TT  TR        o     R";
    "RR            RRT  TT      TT  TR              R";
    "RRRRRRR  RRRRRRRTTTTTTT  TTTTTTTRRRRRRRRRRRRRRRR";
    "RRRRRRR  RRRRRRRTTTTTTT  TTTTTTTXXXXXXXXXXXXXXXX";
    "R              RT              TX..............X";
    "R  T   o    T  RT   XXXXXXXXXXXXX...b......b...X";
    "R              RT   X..........XX..............X";
    "R    RR  RR    RT   X..........XX....X....X....X";
    "R    R h  R    RT   X..........D.......*.......X";
    "R    RR  RR    RT   X..........XX....X....X....X";
    "R              RT   XXXXX..XXXXXX..............X";
    "R  T        T  RT              TX...b......b...X";
    "R              RT  o           TX..............X";
    "RRRRRRRRRRRRRRRRTTTTTTTTTTTTTTTTXXXXXXXXXXXXXXXX" ]

let tile = 60.
let level = Tilemap.of_strings tile world_rows
let bounds = Tilemap.bounds level
let room_size = (16. * tile, 11. * tile)
let solid (c : char) : bool = match c with 'T' | 'R' | 'W' | 'X' | 'D' -> true | _ -> false
let link_size = (40., 40.)

let places (c : char) : (number * number) list = List.map (fun (col, row) -> Tilemap.center level col row) (Tilemap.find level c)

(* the world to play: the people and the monsters are not tiles *)
let start_map : Tilemap.t = List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') level (List.concat_map (Tilemap.find level) [ 'L'; 'o'; 'b' ])

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kind = Octorok | Keese

type monster = {
  kind : kind;
  mx : number;
  my : number;
  dir : number * number; (* where it goes *)
  turn : int; (* frames before it picks another way *)
  reload : int; (* an octorok's frames before spitting a rock *)
}

type game = {
  map : Tilemap.t;
  x : number; (* Link *)
  y : number;
  facing : number * number;
  swing : int; (* frames of the sword's swing left *)
  hurt : int; (* frames of blinking left, after a hit: he can't be hurt again *)
  push : number * number; (* the knockback *)
  hearts : int;
  max_hearts : int;
  keys : int;
  sword : bool;
  monsters : monster list;
  rocks : Shots.t list;
  seed : int;
  cam : Camera2d.t;
  frames : int;
}

type scene = Title | Playing of game | Won of int | Game_over
type model = scene Scene2d.t

let new_game () : game =
  let x, y = List.hd (places 'L') in
  let monster kind (mx, my) = { kind; mx; my; dir = (0., -1.); turn = 0; reload = 90; } in
  { map = start_map; x; y; facing = (0., -1.); swing = 0; hurt = 0; push = (0., 0.); hearts = 3; max_hearts = 3; keys = 0; sword = false;
    monsters = List.map (monster Octorok) (places 'o') @ List.map (monster Keese) (places 'b'); rocks = []; seed = 1;
    cam = Camera2d.flip bounds room_size x y Camera2d.origin; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Randomness, and wandering *)
(*****************************************************************************)

(* [next seed]: the next number of a linear congruential generator, the
 * one of many C libraries' rand: seed * 1103515245 + 12345, modulo 2^31 *)
let next (seed : int) : int = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff

let dirs4 = [| (0., 1.); (0., -1.); (1., 0.); (-1., 0.) |]
let dirs8 = Array.append dirs4 [| (0.7, 0.7); (-0.7, 0.7); (0.7, -0.7); (-0.7, -0.7) |]

let room_of (x : number) (y : number) : int * int = Camera2d.room bounds room_size x y

(* the rectangle of a room, for the keese not to fly out of it *)
let room_rect ((col, row) : int * int) : Camera2d.rect =
  let w, h = room_size in
  { left = bounds.left + (float_of_int col * w); right = bounds.left + (float_of_int (col +.. 1) * w); top = bounds.top - (float_of_int row * h);
    bottom = bounds.top - (float_of_int (row +.. 1) * h) }

(* [wander g m]: a monster goes its way; when its time is up, or it's
 * blocked, it picks another; an octorok walks against the world, a keese
 * flies over it (but stays in its room) *)
let wander (g : game) (m : monster) : game * monster =
  let seed = next g.seed in
  let g = { g with seed } in
  let pick = if m.kind = Octorok then dirs4.(seed mod 4) else dirs8.(seed mod 8) in
  let m = if m.turn <= 0 then { m with dir = pick; turn = 40 +.. (seed /.. 8 mod 60) } else { m with turn = m.turn -.. 1 } in
  let dx, dy = m.dir in
  match m.kind with
  | Octorok ->
      let (mx, my), hit = Tile_move.move_by solid g.map link_size (m.mx, m.my) (1.5 * dx, 1.5 * dy) in
      (g, { m with mx; my; turn = (if hit then 0 else m.turn) })
  | Keese ->
      let r = room_rect (room_of m.mx m.my) in
      let mx = m.mx + (2.5 * dx) and my = m.my + (2.5 * dy) in
      if mx < r.left + 40. || mx > r.right - 40. || my < r.bottom + 40. || my > r.top - 40. then (g, { m with turn = 0 })
      else (g, { m with mx; my })

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the sword's reach: a box in front of Link *)
let swing_box (g : game) : number * number = (g.x + (fst g.facing * 44.), g.y + (snd g.facing * 44.))

(* a rock coming at Link's face while he isn't swinging: his shield *)
let shielded (g : game) (r : Shots.t) : bool = g.swing = 0 && (r.vx * fst g.facing) + (r.vy * snd g.facing) < 0.

(* walking off the room: the camera slides to the next one, the game
 * waiting; true while it does *)
let slide (g : game) : game * bool =
  let target = Camera2d.flip bounds room_size g.x g.y g.cam in
  if Float.hypot (target.x - g.cam.x) (target.y - g.cam.y) < 3. then ({ g with cam = target }, false)
  else
    let d = Float.hypot (target.x - g.cam.x) (target.y - g.cam.y) in
    let step = Float.min d 16. in
    ({ g with cam = { g.cam with x = g.cam.x + (step * (target.x - g.cam.x) / d); y = g.cam.y + (step * (target.y - g.cam.y) / d) } }, true)

let hurt_by (sx : number) (sy : number) (g : game) : game =
  if g.hurt > 0 then g
  else begin
    Audio.play Audio.hit;
    let d = Float.max 1. (Float.hypot (g.x - sx) (g.y - sy)) in
    { g with hearts = g.hearts -.. 1; hurt = 60; push = (8. * (g.x - sx) / d, 8. * (g.y - sy) / d) }
  end

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let g, sliding = slide { g with frames = g.frames +.. 1 } in
  if sliding then g
  else
    let k = computer.keyboard in
    (* Link: pushed back after a hit, swinging, or walking (one way at a
     * time, as on the NES's pad) *)
    let dir = if k.kup then (0., 1.) else if k.kdown then (0., -1.) else if k.kleft then (-1., 0.) else if k.kright then (1., 0.) else (0., 0.) in
    let g =
      if g.push <> (0., 0.) && g.hurt > 45 then
        let (x, y), _ = Tile_move.move_by solid g.map link_size (g.x, g.y) g.push in
        { g with x; y }
      else if g.swing > 0 then { g with swing = g.swing -.. 1 }
      else if g.sword && Scene2d.pressed (fun k -> k.kspace) scenes then (Audio.play Audio.laser; { g with swing = 15 })
      else if dir <> (0., 0.) then
        let (x, y), _ = Tile_move.move_by solid g.map link_size (g.x, g.y) (3. * fst dir, 3. * snd dir) in
        { g with x; y; facing = dir }
      else g
    in
    let g = { g with hurt = max 0 (g.hurt -.. 1) } in
    (* what he walks on: the sword, a key, a heart, the Triforce; a locked
     * door in front of him, opened with a key *)
    let col, row = Tilemap.cell g.map g.x g.y in
    let take g = { g with map = Tilemap.set g.map col row ' ' } in
    let g =
      match Tilemap.get g.map col row with
      | Some 'S' -> Audio.play Audio.coin; take { g with sword = true }
      | Some 'k' -> Audio.play Audio.coin; take { g with keys = g.keys +.. 1 }
      | Some 'h' -> Audio.play Audio.coin; take { g with max_hearts = g.max_hearts +.. 1; hearts = g.max_hearts +.. 1 }
      | _ -> g
    in
    let fcol, frow = Tilemap.cell g.map (g.x + (fst g.facing * 35.)) (g.y + (snd g.facing * 35.)) in
    let g = if Tilemap.get g.map fcol frow = Some 'D' && g.keys > 0 then (Audio.play Audio.coin; { g with map = Tilemap.set g.map fcol frow '.'; keys = g.keys -.. 1 }) else g in
    (* the monsters of this room: wander, spit rocks; the sword *)
    let here = room_of g.x g.y in
    let g, monsters =
      List.fold_left
        (fun (g, acc) m ->
          if room_of m.mx m.my <> here then (g, m :: acc)
          else
            let g, m = wander g m in
            if m.kind = Octorok && m.reload <= 0 then ({ g with rocks = Shots.straight m.mx m.my (5. * fst m.dir) (5. * snd m.dir) :: g.rocks }, { m with reload = 100 +.. (g.seed mod 60) } :: acc)
            else (g, { m with reload = m.reload -.. 1 } :: acc))
        (g, []) g.monsters
    in
    let sx, sy = swing_box g in
    let slain m = g.swing > 0 && g.swing < 13 && Float.abs (m.mx - sx) < 44. && Float.abs (m.my - sy) < 44. in
    if List.exists slain monsters then Audio.play Audio.explosion;
    let g = { g with monsters = List.rev (List.filter (fun m -> not (slain m)) monsters) } in
    (* the rocks: stopped by the world, by the shield, or hitting Link *)
    let rocks = List.filter (fun (r : Shots.t) -> match Tilemap.tile_at g.map r.x r.y with Some c -> not (solid c) | None -> false) (List.map Shots.advance g.rocks) in
    let hitting = List.filter (Shots.near 26. (g.x, g.y)) rocks in
    let g = { g with rocks = List.filter (fun r -> not (List.memq r hitting)) rocks } in
    let g = List.fold_left (fun g (r : Shots.t) -> if shielded g r then g else hurt_by r.x r.y g) g hitting in
    match List.find_opt (fun m -> room_of m.mx m.my = here && Float.hypot (m.mx - g.x) (m.my - g.y) < 38.) g.monsters with
    | Some m -> hurt_by m.mx m.my g
    | None -> g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      let col, row = Tilemap.cell g.map g.x g.y in
      if Tilemap.get g.map col row = Some '*' then Scene2d.go (Won g.frames) s
      else if g.hearts <= 0 then Scene2d.go Game_over s
      else { s with scene = Playing g }
  | Won _ | Game_over -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let sand = rgb 252 216 168

let tile_shape (c : char) : shape =
  let ground = square sand tile in
  match c with
  | 'T' -> group [ ground; circle (rgb 0 150 0) 26. |> move_y 6.; circle (rgb 0 110 0) 14. |> move (-8.) 10.; rectangle (rgb 120 70 20) 10. 16. |> move_y (-22.) ]
  | 'R' -> group [ square (rgb 200 76 12) tile; square (rgb 230 110 40) (tile - 16.) |> move 3. 3. ]
  | 'W' -> group [ square (rgb 32 56 236) tile; rectangle (rgb 90 120 250) 20. 3. |> move (-10.) 10.; rectangle (rgb 90 120 250) 20. 3. |> move 12. (-8.) ]
  | 'X' -> group [ square (rgb 0 64 88) tile; square (rgb 0 90 120) (tile - 10.) ]
  | '.' -> square (rgb 20 40 70) tile
  | 'D' -> group [ square (rgb 120 70 30) tile; circle black 6. |> move_y 6.; rectangle black 4. 12. |> move_y (-4.) ]
  | 'S' -> group [ ground; rectangle (rgb 220 220 240) 6. 34. |> move_y 6.; rectangle (rgb 160 110 40) 20. 5. |> move_y (-10.) ]
  | 'k' -> group [ ground; circle (rgb 240 200 40) 8. |> move_y 10.; rectangle (rgb 240 200 40) 5. 22. |> move_y (-6.) ]
  | 'h' -> group [ ground; circle red 9. |> move (-6.) 4.; circle red 9. |> move 6. 4.; triangle red 12. |> rotate 180. |> move_y (-6.) ]
  | '*' -> group [ square (rgb 20 40 70) tile; triangle (rgb 255 220 60) 24. |> rotate 90. ]
  | 'm' -> group [ ground; triangle (rgb 200 40 40) 24. |> rotate 90. |> move_y (-6.); circle (rgb 250 190 140) 9. |> move_y 16. ]
  | _ -> ground

let link_rows = [ "..GGGGG..."; ".GGGGGGG.."; "..SSKSS..."; "..SSSSS..."; ".GGGGGGGS."; "SGGBGGGGSS"; "SGGGGGGGS."; "..GGGGG..."; "..SS.SS..."; ".BBB.BBB.." ]
let link = Sprite.pixels 4. [ ('G', rgb 90 180 40); ('S', rgb 250 190 140); ('K', black); ('B', rgb 120 70 20) ] link_rows
let link_left = Sprite.pixels 4. [ ('G', rgb 90 180 40); ('S', rgb 250 190 140); ('K', black); ('B', rgb 120 70 20) ] (Sprite.flip link_rows)

let view_monster (frames : int) (m : monster) : shape =
  match m.kind with
  | Octorok -> group [ circle (rgb 220 40 40) 18.; circle (rgb 220 40 40) 7. |> move (fst m.dir * 18.) (snd m.dir * 18.); circle white 4. |> move (-6.) 6.; circle white 4. |> move 6. 6. ] |> move m.mx m.my
  | Keese ->
      let flap = if frames /.. 6 mod 2 = 0 then 20. else -10. in
      group [ circle (rgb 40 40 80) 8.; polygon (rgb 40 40 80) [ (0., 0.); (-24., flap); (-12., 0.) ]; polygon (rgb 40 40 80) [ (0., 0.); (24., flap); (12., 0.) ] ] |> move m.mx m.my

let view_link (g : game) : shape list =
  if g.hurt > 0 && g.hurt mod 8 < 4 then []
  else
    let sprite = (if fst g.facing < 0. then link_left else link) |> move g.x g.y in
    let sword =
      if g.swing > 0 then
        let fx, fy = g.facing in
        [ rectangle (rgb 220 220 240) 8. 36. |> rotate (if fx <> 0. then 90. else 0.) |> move (g.x + (fx * 38.)) (g.y + (fy * 38.)) ]
      else []
    in
    sword @ [ sprite ]

(* the room on the screen: 960 x 660, under the top line (the hearts,
 * the keys, the map) *)
let view_game (g : game) : shape list =
  (* the sand under everything: Tilemap.view draws no ' ' *)
  let world =
    [ rectangle sand (bounds.right - bounds.left) (bounds.top - bounds.bottom); Tilemap.view_visible (Camera2d.visible (to_screen 1000. 1000.) g.cam) tile_shape g.map ]
    @ List.map (view_monster g.frames) g.monsters
    @ List.map (fun (r : Shots.t) -> circle (rgb 140 110 90) 7. |> move r.x r.y) g.rocks
    @ view_link g
  in
  let w, h = room_size in
  let dy = -60. in
  (* black around the room's window, from dy - h/2 to dy + h/2, -w/2
   * to w/2: the neighbors, sliding in, only show through it *)
  let top = dy + (h / 2.) and bottom = dy - (h / 2.) in
  let masks =
    [ rectangle black 1000. (500. - top) |> move_y ((500. + top) / 2.); rectangle black 1000. (bottom + 500.) |> move_y ((bottom - 500.) / 2.);
      rectangle black (500. - (w / 2.)) 1000. |> move_x (-.(500. + (w / 2.)) / 2.); rectangle black (500. - (w / 2.)) 1000. |> move_x ((500. + (w / 2.)) / 2.) ]
  in
  let here_col, here_row = room_of g.x g.y in
  let minimap = List.concat (List.init 2 (fun r -> List.init 3 (fun c -> rectangle (if (c, r) = (here_col, here_row) then rgb 90 180 40 else rgb 80 80 80) 36. 22. |> move (-420. + (float_of_int c * 40.)) (440. - (float_of_int r * 26.))))) in
  let hearts = List.init g.max_hearts (fun i -> text (if i < g.hearts then red else rgb 80 80 80) 3. "v" |> move (180. + (float_of_int i * 30.)) 440.) in
  let message = if not g.sword && here_col = 0 && here_row = 0 then [ text white 2.5 "IT'S DANGEROUS OUT THERE! TAKE THIS." |> move_y (dy + (h / 2.) - 40.) ] else [] in
  [ Camera2d.view g.cam world |> move_y dy ] @ masks @ minimap @ hearts
  @ [ text white 2.5 (Printf.sprintf "KEYS %d" g.keys) |> move (-150.) 440.; text white 2.5 (if g.sword then "SWORD" else "") |> move 0. 440.; text white 2.5 "LIFE" |> move 230. 470. ]
  @ message

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text (rgb 240 200 40) 7. "TINY ZELDA" |> move_y 150.; text white 2.5 "arrows walk   space swings the sword" |> move_y 60.;
        text white 2.5 "find the sword, the key, the dungeon, and the Triforce" |> move_y 20.; link |> scale 2. |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing g -> view_game g
  | Won frames ->
      [ text (rgb 255 220 60) 6. "THE TRIFORCE!"; text white 3. (Printf.sprintf "%.0f seconds" (float_of_int frames / 60.)) |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over -> [ text red 6. "GAME OVER" ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
