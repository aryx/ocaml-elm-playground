(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Galaga (Namco, 1981, Shigeru Yokoyama), the fixed
 * shooter after Space Invaders and Galaxian: the enemies don't march,
 * they fly. Each stage, they come in waves along curves, loop, and take
 * their places in a formation that breathes; then they dive at you, one
 * after the other, shooting, and loop back to their place from the top
 * of the screen. The bosses take two hits. Left/right to move, space to
 * fire (two shots at a time).
 *
 * Galaxian (Namco, 1979) had the diving aliens; Galaga added the
 * entrances, the bosses and their tractor beam (a captured fighter,
 * rescued, gives you two side by side), and the challenging stages. It
 * was one of the most played arcade games of the 1980s, and still is,
 * in the corner of many bars (with Ms. Pac-Man, in the same cabinet).
 * (Names and dates from memory, to check.)
 *
 * The new idea here is the paths. An enemy's flight is a curve through
 * a few points, typed as a list ([entry_paths], [dive_path]), made
 * smooth by a Catmull-Rom spline, and flown at a constant speed, by the
 * distance along it rather than the spline's parameter: see
 * gamekits/shmup/Path.mli, the arc-Path.length parametrization in its simplest
 * form, and the enemies turned to face where they fly.
 *
 * What it uses: the shoot 'em up kit (gamekits/shmup/: Path for the
 * flights, Shots for the fighter's shots and the divers' aimed
 * bullets, also TinyInvaders'), Sprite (pixel art, turned to face
 * where they fly), Scene2d (title, play, game over), Audio (the shots,
 * the hits, the explosions). Not Physics: the enemies follow curves,
 * not forces, and the fighter slides left and right.
 *
 * Exercises: the boss's tractor beam and the dual fighter, the
 * challenging stages (a wave to shoot that doesn't shoot back), bosses
 * diving with two escorts, enemies shooting as they enter, the high
 * scores' initials.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* Paths *)
(*****************************************************************************)

type point = number * number

(* the waves' ways in: from the top center curling out, from the bottom
 * corners looping, from the top corners sweeping across (the screen is
 * 1000 x 1000, (0, 0) at its center) *)
let top_center = [ (40., 560.); (40., 300.); (-100., 50.); (-300., 0.); (-350., 150.); (-200., 250.) ]
let bottom_left = [ (-560., -300.); (-300., -150.); (-100., 0.); (-100., 200.); (-250., 250.); (-350., 100.); (-200., 0.) ]
let top_left = [ (-560., 380.); (-200., 250.); (0., 50.); (-150., -100.); (-300., 0.); (-200., 200.) ]

(* each wave's path, for its even and its odd enemies: the first wave
 * comes in two lines, mirrored *)
let entry_paths : (Path.t * Path.t) array =
  let both p q = (Path.make p, Path.make q) in
  [| both top_center (Path.mirror top_center); both bottom_left bottom_left; both (Path.mirror bottom_left) (Path.mirror bottom_left);
     both top_left top_left; both (Path.mirror top_left) (Path.mirror top_left) |]

(* a dive from (x, y), towards the fighter at [fx], on the side of the
 * screen it's on: a loop up and out, down at the fighter, and away off
 * the bottom *)
let dive_path ((x, y) : point) (fx : number) : Path.t =
  let side = if x < 0. then -1. else 1. in
  Path.make
    [ (x, y); (x + (side * 50.), y + 60.); (x + (side * 120.), y); (x + (side * 80.), y - 150.); (fx, -200.); (fx - (side * 120.), -380.);
      (fx - (side * 200.), -580.) ]

(*****************************************************************************)
(* The sprites *)
(*****************************************************************************)

(* drawn facing down, at the fighter *)
let bee = [ "b.........b"; "bb...y...bb"; "bbb.yyy.bbb"; ".bbbyryybb."; "..byyyyyb.."; "...yryry..."; "....yyy...."; ".....y....." ]
let butterfly = [ "r.........r"; "rr..www..rr"; "rrr.wbw.rrr"; ".rrrwwwrrr."; "..rrwbwrr.."; ".rrrwwwrrr."; "rr..w.w..rr"; "r.........r" ]
let boss = [ "....ggg...."; "..ggggggg.."; ".gygggggyg."; "ggggggggggg"; "g.ggg.ggg.g"; "g.g.ggg.g.g"; "..g.....g.."; "...g...g..." ]
let fighter_rows = [ ".....#....."; ".....#....."; "....###...."; "....#r#...."; ".#..###..#."; ".#.#####.#."; "r#########r"; "###.###.###"; "##...#...##"; "#.........#" ]

let palette = [ ('y', yellow); ('b', rgb 60 110 255); ('r', red); ('w', white); ('g', rgb 60 200 90) ]
let sprite (rows : string list) : shape = Sprite.pixels 4. palette rows
let fighter = Sprite.pixels 4. [ ('#', white); ('r', red) ] fighter_rows

type kind = Bee | Butterfly | Boss

(* a boss hit once turns purple *)
let look (k : kind) (hits : int) : shape =
  match k with
  | Bee -> sprite bee
  | Butterfly -> sprite butterfly
  | Boss -> if hits = 0 then sprite boss else Sprite.pixels 4. [ ('g', rgb 170 80 230); ('y', yellow) ] boss

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type flight =
  | Waiting of int * Path.t (* frames before its wave comes, and its way in *)
  | Entering of Path.t
  | Diving of Path.t * int (* the shots fired so far *)
  | Returning (* flying to its place *)
  | In_place

type enemy = {
  kind : kind;
  row : int; (* its place in the formation *)
  col : int;
  x : number;
  y : number;
  angle : number; (* the direction it flies, degrees *)
  flight : flight;
  s : number; (* how far along its path *)
  hits : int;
}

type game = {
  enemies : enemy list;
  fx : number; (* the fighter *)
  shots : Shots.t list; (* the fighter's, 2 at most *)
  bullets : Shots.t list; (* the enemies' *)
  explosions : (number * number * int) list; (* where, and frames since *)
  score : int;
  lives : int;
  dead : int; (* frames since the fighter was hit, 0 if it's flying *)
  stage : int;
  frames : int;
}

type scene = Title | Playing of game | Game_over of int
type model = { scenes : scene Scene2d.t; hi_score : int }

let fighter_y = -420.

(* The formation: 4 bosses on top, 2 rows of 8 butterflies, 2 rows of 10
 * bees; and the order they come in, 5 waves of 8, each a list of
 * (row, col) *)
let kind_of_row (row : int) : kind = if row = 0 then Boss else if row <= 2 then Butterfly else Bee

let waves : (int * int) list list =
  [ [ (3, 4); (3, 5); (1, 4); (1, 5); (3, 3); (3, 6); (1, 3); (1, 6) ];
    [ (0, 3); (1, 2); (0, 4); (1, 7); (0, 5); (1, 1); (0, 6); (1, 8) ];
    [ (2, 1); (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); (2, 7); (2, 8) ];
    [ (3, 0); (3, 1); (3, 2); (4, 0); (3, 7); (3, 8); (3, 9); (4, 9) ];
    [ (4, 1); (4, 2); (4, 3); (4, 4); (4, 5); (4, 6); (4, 7); (4, 8) ] ]

(* a wave every 2.5 s, an enemy every 8 frames, the even ones on the
 * wave's first path, the odd ones on its second *)
let new_stage (stage : int) (g : game) : game =
  let enemies =
    List.concat
      (List.mapi
         (fun w wave ->
           let even, odd = entry_paths.(w) in
           List.mapi
             (fun i (row, col) ->
               let p = if i mod 2 = 0 then even else odd in
               { kind = kind_of_row row; row; col; x = 0.; y = 600.; angle = -90.; flight = Waiting ((w *.. 150) +.. (i *.. 8), p); s = 0.; hits = 0 })
             wave)
         waves)
  in
  { g with enemies; shots = []; bullets = []; stage }

let new_game () : game =
  new_stage 1 { enemies = []; fx = 0.; shots = []; bullets = []; explosions = []; score = 0; lives = 3; dead = 0; stage = 1; frames = 0 }

let initial_model = { scenes = Scene2d.start Title; hi_score = 0 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let arrived (g : game) : bool = List.for_all (fun e -> match e.flight with Waiting _ | Entering _ -> false | _ -> true) g.enemies

(* where an enemy's place is now: the formation sways sideways while the
 * waves come in, then breathes, spreading out and back *)
let place (g : game) (e : enemy) : point =
  let t = float_of_int g.frames / 60. in
  let sway = if arrived g then 0. else 40. * sin t and breathe = if arrived g then 1. + (0.12 * sin (t * 1.5)) else 1. in
  (sway + ((float_of_int e.col - 4.5) * 56. * breathe), 330. - (float_of_int e.row * 50. * breathe))

let speed (g : game) : number = 5. + (0.4 * float_of_int g.stage)

(* one enemy, one frame: along its path, or to its place; a diving one
 * shoots twice at the fighter, at a third and at half of its dive *)
let fly (g : game) (e : enemy) : enemy * Shots.t list =
  let along (p : Path.t) (s : number) =
    let (x, y), angle = Path.at p s in
    { e with x; y; angle; s }
  in
  match e.flight with
  | Waiting (0, p) -> ({ (along p 0.) with flight = Entering p }, [])
  | Waiting (n, p) -> ({ e with flight = Waiting (n -.. 1, p) }, [])
  | Entering p -> if e.s >= Path.length p then ({ e with flight = Returning }, []) else ({ (along p (e.s + speed g)) with flight = Entering p }, [])
  | Diving (p, fired) ->
      if e.s >= Path.length p then
        (* off the bottom: back from the top *)
        let px, _ = place g e in
        ({ e with x = px; y = 560.; flight = Returning }, [])
      else
        let e' = along p (e.s + speed g) in
        let due = float_of_int (fired +.. 1) * Path.length p / 3. in
        if fired < 2 && e'.s >= due && g.dead = 0 then
          ({ e' with flight = Diving (p, fired +.. 1) }, [ Shots.aimed 6. (e'.x, e'.y) (g.fx, fighter_y) ])
        else ({ e' with flight = Diving (p, fired) }, [])
  | Returning ->
      let px, py = place g e in
      let dx = px - e.x and dy = py - e.y in
      let d = Float.hypot dx dy in
      if d <= speed g then ({ e with x = px; y = py; angle = -90.; flight = In_place }, [])
      else ({ e with x = e.x + (speed g * dx / d); y = e.y + (speed g * dy / d); angle = atan2 dy dx * 180. / pi }, [])
  | In_place ->
      let px, py = place g e in
      ({ e with x = px; y = py; angle = -90. }, [])

let flying (e : enemy) : bool = match e.flight with Waiting _ -> false | _ -> true

(* every so often, one enemy in its place dives: which one from the
 * frame count, so every game is the same; sooner at later stages *)
let launch (g : game) : game =
  let every = max 50 (140 -.. (15 *.. g.stage)) in
  let ready = List.filter (fun e -> e.flight = In_place) g.enemies in
  if (not (arrived g)) || ready = [] || g.dead > 0 || g.frames mod every <> 0 then g
  else
    let chosen = List.nth ready (g.frames /.. every mod List.length ready) in
    let p = dive_path (chosen.x, chosen.y) g.fx in
    { g with enemies = List.map (fun e -> if e == chosen then { e with flight = Diving (p, 0); s = 0. } else e) g.enemies }

let points (e : enemy) : int =
  let base = match e.kind with Bee -> 50 | Butterfly -> 80 | Boss -> 150 in
  match e.flight with Diving _ -> base *.. 2 | _ -> base

let near (x0 : number) (y0 : number) (x1 : number) (y1 : number) (d : number) : bool = Float.hypot (x1 - x0) (y1 - y0) < d

(* the fighter's shots against the enemies: a hit bee or butterfly
 * explodes, a boss needs two *)
let shoot_down (g : game) : game =
  List.fold_left
    (fun g (s : Shots.t) ->
      match List.find_opt (fun e -> flying e && Shots.near 22. (e.x, e.y) s) g.enemies with
      | None -> { g with shots = s :: g.shots }
      | Some e when e.kind = Boss && e.hits = 0 ->
          Audio.play Audio.hit;
          { g with enemies = List.map (fun e' -> if e' == e then { e with hits = 1 } else e') g.enemies }
      | Some e ->
          Audio.play Audio.explosion;
          { g with enemies = List.filter (fun e' -> e' != e) g.enemies; score = g.score +.. points e; explosions = (e.x, e.y, 0) :: g.explosions })
    { g with shots = [] } g.shots

(* the fighter hit by a bullet or a diving enemy (which explodes too) *)
let fighter_hit (g : game) : game =
  let hit_by_bullet = List.exists (Shots.near 16. (g.fx, fighter_y)) g.bullets in
  let rammed = List.find_opt (fun e -> (match e.flight with Diving _ -> true | _ -> false) && near e.x e.y g.fx fighter_y 30.) g.enemies in
  if g.dead > 0 || (not hit_by_bullet && rammed = None) then g
  else begin
    Audio.play Audio.explosion;
    { g with dead = 1; lives = g.lives -.. 1; bullets = []; explosions = (g.fx, fighter_y, 0) :: g.explosions;
      enemies = (match rammed with Some e -> List.filter (fun e' -> e' != e) g.enemies | None -> g.enemies) }
  end


let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  (* the fighter, while it's flying; 2 s after being hit, the next one *)
  let g =
    if g.dead > 0 then { g with dead = (if g.dead > 120 && g.lives > 0 then 0 else g.dead +.. 1) }
    else
      let fx = clamp (-440.) 440. (g.fx + (7. * to_x computer.keyboard)) in
      let fire = Scene2d.pressed (fun k -> k.kspace) scenes && List.length g.shots < 2 in
      if fire then Audio.play Audio.laser;
      { g with fx; shots = (if fire then Shots.straight fx (fighter_y + 30.) 0. 16. :: g.shots else g.shots) }
  in
  let moved, fired = List.split (List.map (fly g) g.enemies) in
  let g =
    { g with enemies = moved; bullets = List.filter (Shots.on_screen 20. computer.screen) (List.map Shots.advance g.bullets @ List.concat fired);
      shots = List.filter (Shots.on_screen 20. computer.screen) (List.map Shots.advance g.shots);
      explosions = List.filter_map (fun (x, y, n) -> if n < 30 then Some (x, y, n +.. 1) else None) g.explosions }
  in
  let g = g |> launch |> shoot_down |> fighter_hit in
  (* all shot down: the next stage *)
  if g.enemies = [] then new_stage (g.stage +.. 1) g else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let fire = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if fire then { model with scenes = Scene2d.go (Playing (new_game ())) scenes } else { model with scenes }
  | Playing g ->
      let g = update_game computer scenes g in
      let hi_score = max model.hi_score g.score in
      if g.lives = 0 && g.dead > 120 then { hi_score; scenes = Scene2d.go (Game_over g.score) scenes }
      else { hi_score; scenes = { scenes with scene = Playing g } }
  | Game_over _ -> if fire || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the stars, scrolling down: 90 of them, from a linear congruential
 * generator (the same every time, no Random) *)
let stars (frames : int) : shape list =
  let rec go n seed acc =
    if n = 0 then acc
    else
      let seed = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff in
      let x = float_of_int (seed mod 1000) - 500. and y0 = float_of_int (seed /.. 1000 mod 1000) in
      let y = Float.rem (y0 - (float_of_int frames * 1.5)) 1000. in
      let y = (if y < 0. then y + 1000. else y) - 500. in
      let color = [| rgb 120 120 255; rgb 255 120 120; rgb 255 255 150; white |].(seed mod 4) in
      go (n -.. 1) seed ((rectangle color 3. 3. |> move x y) :: acc)
  in
  go 90 11 []

(* an enemy, turned to face where it flies (its sprite faces down: -90) *)
let view_enemy (e : enemy) : shape list =
  if flying e then [ look e.kind e.hits |> rotate (e.angle + 90.) |> move e.x e.y ] else []

let view_game (g : game) : shape list =
  List.concat_map view_enemy g.enemies
  @ List.map (fun (s : Shots.t) -> rectangle white 3. 14. |> move s.x s.y) g.shots
  @ List.map (fun (b : Shots.t) -> rectangle (rgb 255 200 80) 4. 10. |> rotate (Shots.angle b) |> move b.x b.y) g.bullets
  @ List.map (fun (x, y, n) -> circle (if n mod 6 < 3 then orange else yellow) (8. + (float_of_int n * 1.2)) |> fade (1. - (float_of_int n / 30.)) |> move x y) g.explosions
  @ (if g.dead = 0 then [ fighter |> move g.fx fighter_y ] else [])
  @ List.init (max 0 (g.lives -.. 1)) (fun i -> fighter |> scale 0.6 |> move (-440. + (float_of_int i * 40.)) (-475.))
  @ if g.frames < 150 && not (List.exists flying g.enemies) then [ text (rgb 90 200 255) 4. (Printf.sprintf "STAGE %d" g.stage) ] else []

let header (model : model) (score : int) : shape list =
  [ text red 2.5 "1UP" |> move (-400.) 475.; text white 2.5 (Printf.sprintf "%d" score) |> move (-400.) 445.;
    text red 2.5 "HIGH SCORE" |> move_y 475.; text white 2.5 (Printf.sprintf "%d" model.hi_score) |> move_y 445. ]

let view_title (scenes : scene Scene2d.t) : shape list =
  [ text (rgb 90 200 255) 7. "TINY GALAGA" |> move_y 250. ]
  @ List.concat
      (List.mapi
         (fun i (kind, pts) ->
           [ look kind 0 |> move (-100.) (80. - (float_of_int i * 80.));
             text white 2.5 (Printf.sprintf "%d   diving %d" pts (2 *.. pts)) |> move 90. (80. - (float_of_int i * 80.)) ])
         [ (Bee, 50); (Butterfly, 80); (Boss, 150) ])
  @ [ text white 2. "left/right move   space fire" |> move_y (-200.) ]
  @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-280.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  let frames = match scenes.scene with Playing g -> g.frames | _ -> scenes.frames in
  (rectangle black screen.width screen.height :: stars frames)
  @
  match scenes.scene with
  | Title -> header model 0 @ view_title scenes
  | Playing g -> header model g.score @ view_game g
  | Game_over score -> header model score @ [ text red 6. "GAME OVER" ] @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-150.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
