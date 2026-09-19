(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Space Invaders (Tomohiro Nishikado, Taito, 1978), the
 * fixed shooter that started the golden age of arcade games: a
 * formation of 5x11 aliens marches sideways and down, you move a cannon
 * at the bottom, one shot at a time, hiding behind four bunkers that
 * crumble. Arrows to move, space to shoot.
 *
 * Nishikado built the game and its hardware alone, over a year. Its most
 * famous feature is an accident: the processor moves one alien per
 * frame, so the fewer aliens are left, the faster the formation
 * marches, and the last one runs across the screen. He kept it: the
 * difficulty rises by itself as you win. We move one alien per frame
 * too (see [march]).
 *
 * It uses three layers on top of the playground: Sprite (the aliens are
 * pixel art typed as strings, two frames each), Scene2d (title, play,
 * game over), and Tilemap (each bunker is a tile map of small tiles,
 * eroded one tile at a time where shots hit it); and the shoot 'em up
 * kit's Shots (kits/shmup/, with games/TinyGalaga) for the cannon's
 * shot and the aliens' bombs. No randomness: the
 * aliens choose who shoots from a fixed table of columns, or the column
 * above you, as the original did, so every game is the same (and golden
 * frames are possible).
 *
 * Left as exercises: the mystery ship crossing the top, the aliens'
 * explosion sprite, the sounds (the four-note bass march, speeding up
 * with the aliens: with plan_audio_teaching.md), the formation starting
 * lower at each new wave.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The sprites *)
(*****************************************************************************)

(* The original's bitmaps (a squid, a crab, an octopus), two frames each,
 * alternating at each step of the march; the game's monitor was black
 * and white, with strips of colored cellophane glued on it, green at
 * the bottom where the cannon and bunkers are. *)
let alien_frames : string list list array =
  [|
    [ [ "...##..."; "..####.."; ".######."; "##.##.##"; "########"; "..#..#.."; ".#.##.#."; "#.#..#.#" ];
      [ "...##..."; "..####.."; ".######."; "##.##.##"; "########"; ".#.##.#."; "#......#"; ".#....#." ] ];
    [ [ "..#.....#.."; "...#...#..."; "..#######.."; ".##.###.##."; "###########"; "#.#######.#"; "#.#.....#.#"; "...##.##..." ];
      [ "..#.....#.."; "#..#...#..#"; "#.#######.#"; "###.###.###"; "###########"; ".#########."; "..#.....#.."; ".#.......#." ] ];
    [ [ "....####...."; ".##########."; "############"; "###..##..###"; "############"; "...##..##..."; "..##.##.##.."; "##........##" ];
      [ "....####...."; ".##########."; "############"; "###..##..###"; "############"; "..###..###.."; ".##..##..##."; "..##....##.." ] ];
  |]

let cannon_rows =
  [ "......#......"; ".....###....."; ".....###....."; ".###########."; "#############"; "#############"; "#############"; "#############" ]

let pixel = 5.

(* each kind of alien's pixel art, in both frames, drawn once *)
let alien_shapes : shape list array =
  Array.map (fun frames -> List.map (Sprite.pixels pixel [ ('#', white) ]) frames) alien_frames

let cannon = Sprite.pixels pixel [ ('#', green) ] cannon_rows
let cannon_hit = Sprite.pixels pixel [ ('#', red) ] cannon_rows

(* the kind of alien in each of the 5 rows, from the top, and its points *)
let row_kind = [| 0; 1; 1; 2; 2 |]
let points = [| 30; 20; 10 |]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type alien = {
  id : int; (* the order they move in: from the bottom-left, row by row *)
  kind : int;
  col : int;
  ax : number;
  ay : number;
  steps : int; (* how many times it moved: its frame is [steps mod 2] *)
}

type game = {
  aliens : alien list; (* sorted by id *)
  next : int; (* the id of the next alien to move *)
  dir : number; (* 1. marching right, -1. left *)
  reverse : bool; (* one alien reached an edge: down and back at the end of the step *)
  x : number; (* the cannon *)
  shot : Shots.t option; (* the cannon's one shot *)
  bombs : Shots.t list; (* the aliens' shots, 3 at most *)
  bombs_fired : int;
  bunkers : (number * Tilemap.t) list; (* their x, and their tiles *)
  score : int;
  lives : int;
  hit_frames : int; (* > 0: the cannon was hit, and is exploding *)
  frames : int;
}

type scene = Title | Playing of game | Game_over of int

type model = { scenes : scene Scene2d.t; hi_score : int }

let cannon_y = -400.
let bunker_y = -280.
let edge = 430.

let formation () : alien list =
  List.init 55 (fun id ->
      let row = 4 -.. (id /.. 11) and col = id mod 11 in
      { id; kind = row_kind.(row); col;
        ax = -400. + (float_of_int col * 80.);
        ay = 60. + (float_of_int (4 -.. row) * 60.);
        steps = 0 })

(* the classic bunker shape, tiles of 4: 88x64, with an arch below *)
let bunker () : Tilemap.t =
  Tilemap.of_strings 4.
    [ "....##############....";
      "...################...";
      "..##################..";
      ".####################.";
      "######################";
      "######################";
      "######################";
      "######################";
      "######################";
      "######################";
      "######################";
      "######################";
      "#######........#######";
      "######..........######";
      "#####............#####";
      "#####............#####" ]

let new_game () : game =
  { aliens = formation (); next = 0; dir = 1.; reverse = false;
    x = 0.; shot = None; bombs = []; bombs_fired = 0;
    bunkers = List.map (fun bx -> (bx, bunker ())) [ -300.; -100.; 100.; 300. ];
    score = 0; lives = 3; hit_frames = 0; frames = 0 }

let initial_model = { scenes = Scene2d.start Title; hi_score = 0 }

(*****************************************************************************)
(* The march *)
(*****************************************************************************)

(* Move one alien, the next one in [id] order, 8 pixels sideways: 55
 * aliens take 55 frames for the whole formation to step, 1 alien only
 * one. At the end of a step (no alien left to move), if one of them
 * reached an edge, they all go down, and back the other way. *)
let march (g : game) : game =
  match List.find_opt (fun a -> a.id >= g.next) g.aliens with
  | Some a ->
      let a' = { a with ax = a.ax + (8. * g.dir); steps = a.steps +.. 1 } in
      { g with
        aliens = List.map (fun b -> if b.id = a.id then a' else b) g.aliens;
        next = a.id +.. 1;
        reverse = g.reverse || Float.abs a'.ax > edge }
  | None when g.reverse ->
      { g with next = 0; reverse = false; dir = -.g.dir;
        aliens = List.map (fun a -> { a with ay = a.ay - 30. }) g.aliens }
  | None -> { g with next = 0 }

(*****************************************************************************)
(* Shots *)
(*****************************************************************************)

(* The aliens shoot from a column: every other shot, the next column of
 * a fixed table, and otherwise the column above the cannon (the
 * original's "rolling shot" is the one aimed at you); the lowest alien
 * of that column shoots, if the column isn't empty. *)
let column_table = [| 0; 6; 0; 0; 0; 3; 10; 0; 5; 2; 0; 0; 10; 8; 1; 7 |]

let drop_bomb (g : game) : game =
  let col =
    if g.bombs_fired mod 2 = 0 then Some column_table.((g.bombs_fired /.. 2) mod Array.length column_table)
    else
      (* the column whose aliens are the closest to the cannon *)
      match List.sort (fun a b -> compare (Float.abs (a.ax - g.x)) (Float.abs (b.ax - g.x))) g.aliens with
      | a :: _ -> Some a.col
      | [] -> None
  in
  let lowest =
    g.aliens |> List.filter (fun a -> Some a.col = col) |> List.sort (fun a b -> compare a.ay b.ay)
  in
  match lowest with
  | a :: _ when List.length g.bombs < 3 ->
      { g with bombs = Shots.straight a.ax (a.ay - 20.) 0. (-6.) :: g.bombs; bombs_fired = g.bombs_fired +.. 1 }
  | _ -> { g with bombs_fired = g.bombs_fired +.. 1 }

(* [erode] removes the tiles around the one at (x, y) in a bunker
 * (relative to its center), a 3x3 bite: shots dig holes *)
let erode (map : Tilemap.t) (x : number) (y : number) : Tilemap.t =
  let col, row = Tilemap.cell map x y in
  List.fold_left
    (fun m (dc, dr) -> Tilemap.set m (col +.. dc) (row +.. dr) ' ')
    map
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (0, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]

(* whether a shot at (x, y), 4x16, hits a bunker's '#' tiles; if so, the bunkers
 * with the bite taken *)
let hit_bunker (g : game) (x : number) (y : number) : (number * Tilemap.t) list option =
  let solid c = c = '#' in
  let hit = ref false in
  let bunkers =
    List.map
      (fun (bx, map) ->
        if (not !hit) && Tilemap.hits solid map (x - bx) (y - bunker_y) 4. 16. then begin
          hit := true;
          (bx, erode map (x - bx) (y - bunker_y))
        end
        else (bx, map))
      g.bunkers
  in
  if !hit then Some bunkers else None

let alien_size (a : alien) = (float_of_int (String.length (List.hd (List.hd alien_frames.(a.kind)))) * pixel, 8. * pixel)

let move_shot (g : game) : game =
  match g.shot with
  | None -> g
  | Some s -> (
      let s = Shots.advance s in
      let x = s.x and y = s.y in
      let hit_alien =
        List.find_opt
          (fun a ->
            let w, h = alien_size a in
            Float.abs (x - a.ax) < w / 2. && Float.abs (y - a.ay) < h / 2.)
          g.aliens
      in
      match (hit_alien, hit_bunker g x y) with
      | Some a, _ ->
          { g with shot = None; aliens = List.filter (fun b -> b.id <> a.id) g.aliens; score = g.score +.. points.(a.kind) }
      | None, Some bunkers -> { g with shot = None; bunkers }
      | None, None -> { g with shot = (if y > 500. then None else Some s) })

let move_bombs (g : game) : game =
  List.fold_left
    (fun g (b : Shots.t) ->
      let b = Shots.advance b in
      let x = b.x and y = b.y in
      match hit_bunker g x y with
      | Some bunkers -> { g with bunkers }
      | None when g.hit_frames = 0 && Float.abs (x - g.x) < 32. && Float.abs (y - cannon_y) < 20. ->
          { g with lives = g.lives -.. 1; hit_frames = 90; bombs = [] }
      | None when y < -450. -> g
      | None -> { g with bombs = b :: g.bombs })
    { g with bombs = [] } g.bombs

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let fire (scenes : scene Scene2d.t) = Scene2d.pressed (fun k -> k.kspace) scenes

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  if g.hit_frames > 0 then { g with hit_frames = g.hit_frames -.. 1 }
  else
    let x = clamp (-.edge) edge (g.x + (5. * to_x computer.keyboard)) in
    let shot = if g.shot = None && fire scenes then Some (Shots.straight x (cannon_y + 20.) 0. 15.) else g.shot in
    let g = march { g with x; shot } in
    let g = if g.frames mod 40 = 0 then drop_bomb g else g in
    g |> move_shot |> move_bombs

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  match scenes.scene with
  | Title -> if fire scenes then { model with scenes = Scene2d.go (Playing (new_game ())) scenes } else { model with scenes }
  | Playing g ->
      let g = update_game computer scenes g in
      let hi_score = max model.hi_score g.score in
      let landed = List.exists (fun a -> a.ay < bunker_y) g.aliens in
      if (g.lives = 0 && g.hit_frames = 0) || landed then
        { hi_score; scenes = Scene2d.go (Game_over g.score) scenes }
      else if g.aliens = [] then
        (* a new wave, keeping the score, the lives, the damaged bunkers *)
        { hi_score; scenes = Scene2d.go (Playing { g with aliens = formation (); next = 0; dir = 1.; shot = None; bombs = [] }) scenes }
      else { hi_score; scenes = { scenes with scene = Playing g } }
  | Game_over _ ->
      if fire scenes || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let header (model : model) (score : int) : shape list =
  [ text white 3. (Printf.sprintf "SCORE %04d" score) |> move (-300.) 450.;
    text white 3. (Printf.sprintf "HI-SCORE %04d" model.hi_score) |> move 250. 450. ]

let view_game (g : game) : shape list =
  List.map (fun a -> Sprite.cycle a.steps alien_shapes.(a.kind) |> move a.ax a.ay) g.aliens
  (* a bunker's tiles as pixel art: its runs of '#' are drawn as one
   * rectangle each (see Sprite.runs), a few dozen shapes, not hundreds *)
  @ List.map (fun (bx, map) -> Sprite.pixels 4. [ ('#', green) ] (Tilemap.to_strings map) |> move bx bunker_y) g.bunkers
  @ [ (if g.hit_frames > 0 then cannon_hit else cannon) |> move g.x cannon_y ]
  @ (match g.shot with Some s -> [ rectangle white 4. 16. |> move s.x s.y ] | None -> [])
  @ List.map (fun (b : Shots.t) -> rectangle white 4. 16. |> move b.x b.y) g.bombs
  @ [ rectangle green 1000. 3. |> move_y (-440.) ]
  @ List.init (max 0 (g.lives -.. 1)) (fun i -> cannon |> scale 0.6 |> move (-420. + (float_of_int i * 60.)) (-470.))

(* the original's attract screen: the "score advance table" *)
let view_title (scenes : scene Scene2d.t) : shape list =
  [ text white 6. "TINY INVADERS" |> move_y 250. ]
  @ List.concat
      (List.mapi
         (fun i kind ->
           [ List.hd alien_shapes.(kind) |> move (-100.) (80. - (float_of_int i * 90.));
             text white 3. (Printf.sprintf "= %d POINTS" points.(kind)) |> move 80. (80. - (float_of_int i * 90.)) ])
         [ 0; 1; 2 ])
  @ Scene2d.blink 1. scenes [ text green 3. "PRESS SPACE" |> move_y (-250.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let background = rectangle black screen.width screen.height in
  let scenes = model.scenes in
  background
  ::
  (match scenes.scene with
  | Title -> header model 0 @ view_title scenes
  | Playing g -> header model g.score @ view_game g
  | Game_over score ->
      header model score
      @ [ text red 6. "GAME OVER" ]
      @ Scene2d.blink 1. scenes [ text green 3. "PRESS SPACE" |> move_y (-150.) ])

let app = game view update initial_model

let main = Playground_platform.run_app app
