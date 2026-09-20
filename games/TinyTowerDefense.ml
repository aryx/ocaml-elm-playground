(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of the maze tower defense (Desktop Tower Defense, Paul
 * Preece, 2007, out of the Warcraft III custom maps of a few years
 * before): monsters walk from the green door on the left to the red one
 * on the right, and you build towers on the open field to shoot them.
 * There is no fixed road: the towers *are* the road. Each one you place
 * makes the way longer, and the longer the way, the longer your towers
 * shoot -- but you may never close it completely.
 *
 * Click (or the arrows and space) to build a tower, 20 gold; a monster
 * killed pays 2, a monster reaching the door costs a life. "p" draws
 * the way they take, "f" runs faster.
 *
 * What's new here:
 *
 *  - The game is the pathfinding (ai/Pathfind.mli): the monsters take
 *    the cheapest way A* can find through what you've built, and they
 *    find it again the moment you build ([repath]), turning around
 *    mid-step. Placing a tower is really an edit of a graph.
 *
 *  - The rule that keeps it a maze and not a wall ([can_build]): a
 *    tower is refused if, with it, the search finds no way at all --
 *    the search used as a referee, not as an opponent's brain. Each
 *    monster already walking must still have a way too, otherwise a
 *    tower dropped behind one could trap it.
 *
 *  - Towers as the only thing you control, monsters as a stream: the
 *    genre's whole idea, from the Warcraft III maps where a player
 *    couldn't touch the creeps either.
 *
 * What it uses: ai/'s Pathfind, its A* search, Scene2d, Audio. Not the maze kit
 * (kits/maze/'s Grid_move is for a character in a fixed maze; here the
 * maze changes every time you build), not Physics: a shot hits at once,
 * drawn as a line for a few frames.
 *
 * Exercises: more towers (slowing, splashing) and what they cost; the
 * monsters that fly over the maze, ignoring the path; upgrading a tower
 * instead of building another; selling one (and the maze opening up
 * again); a monster that eats a tower rather than walk around; showing
 * each monster's own path when you hover over it.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The field *)
(*****************************************************************************)

type cell = Empty | Tower of int (* the frame it last fired *)

let cols = 20
let rows = 14
let size = 48.
let left = -.(float_of_int cols * size / 2.)
let top = 320.
let entrance = (0, 7)
let exit_ = (cols -.. 1, 7)
(* The knobs: the whole difficulty is these numbers, and they're worth
 * turning one at a time.
 *
 *  - the towers: [tower_cost], [tower_range], [tower_delay] (frames
 *    between two shots) and [tower_damage]. Range matters most, because
 *    a long reach turns every bend of the maze into more seconds of
 *    fire.
 *  - the monsters: [hp_of] (how tough wave n is), [wave_size],
 *    [spawn_every] and their speed in [update_game]. Making [hp_of]
 *    grow faster is the usual way a tower defense ends: the maze stops
 *    being long enough.
 *  - what you earn: [bounty] a monster and the bonus between waves;
 *    less gold means fewer towers, which means a shorter maze.
 *)
let tower_cost = 20
let tower_range = 2.6
let tower_delay = 24 (* frames between two shots *)
let tower_damage = 6
let bounty = 2
let spawn_every = 45
let wave_size (wave : int) : int = 7 +.. wave

type field = cell array

let index ((x, y) : int * int) : int = (y *.. cols) +.. x
let inside ((x, y) : int * int) : bool = x >= 0 && x < cols && y >= 0 && y < rows
let free (f : field) (c : int * int) : bool = inside c && f.(index c) = Empty

let center ((x, y) : int * int) : number * number =
  (left + (size * (float_of_int x + 0.5)), top - (size * (float_of_int y + 0.5)))

let cell_at (mx : number) (my : number) : int * int =
  (int_of_float (Float.floor ((mx - left) / size)), int_of_float (Float.floor ((top - my) / size)))

(* every step costs the same here: what changes is which steps exist *)
let problem (f : field) : (int * int) Pathfind.problem =
  {
    neighbors =
      (fun (x, y) ->
        List.filter_map
          (fun (dx, dy) -> let c = (x +.. dx, y +.. dy) in if free f c then Some (c, 1.) else None)
          [ (1, 0); (-1, 0); (0, 1); (0, -1) ]);
    goal = (fun c -> c = exit_);
    estimate = (fun c -> Pathfind.manhattan c exit_);
  }

let way (f : field) (from : int * int) : (int * int) list = (Pathfind.astar (problem f) from).path

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a monster walks along [path], cell by cell: [x] and [y] are in cells,
 * fractional between two of them *)
type monster = { x : number; y : number; path : (int * int) list; hp : int; full : int; speed : number }

type shot = { from : int * int; at : number * number; age : int }

type game = {
  field : field;
  monsters : monster list;
  shots : shot list;
  cursor : int * int;
  gold : int;
  lives : int;
  wave : int;
  to_come : int; (* monsters of this wave not yet in *)
  frames : int;
  pause : int; (* frames before the next wave *)
  show_path : bool;
  score : int;
}

type scene = Title | Playing of game | Game_over of int
type model = scene Scene2d.t

let hp_of (wave : int) : int = 18 +.. (12 *.. wave)


let new_game () : game =
  { field = Array.make (cols *.. rows) Empty; monsters = []; shots = []; cursor = (10, 7); gold = 80; lives = 10;
    wave = 1; to_come = wave_size 1; frames = 0; pause = 180; show_path = true; score = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Building *)
(*****************************************************************************)

let cell_of (m : monster) : int * int = (int_of_float (Float.round m.x), int_of_float (Float.round m.y))

(* a tower can go on an empty cell, away from the doors, if the way out
 * is still there -- for the monsters already walking too *)
let can_build (g : game) (c : int * int) : bool =
  free g.field c && c <> entrance && c <> exit_
  && (not (List.exists (fun m -> cell_of m = c) g.monsters))
  && g.gold >= tower_cost
  &&
  let f = Array.copy g.field in
  f.(index c) <- Tower 0;
  way f entrance <> [] && List.for_all (fun m -> way f (cell_of m) <> []) g.monsters

(* after the field changes, everyone looks for the way again: the
 * monsters turn around where they stand *)
let repath (g : game) : game =
  { g with monsters = List.map (fun m -> { m with path = way g.field (cell_of m) }) g.monsters }

let build (g : game) (c : int * int) : game =
  if not (can_build g c) then g
  else begin
    Audio.play Audio.blip;
    let field = Array.copy g.field in
    field.(index c) <- Tower 0;
    repath { g with field; gold = g.gold -.. tower_cost }
  end

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* one step along the path, at the monster's speed *)
let walk (m : monster) : monster =
  match m.path with
  | [] -> m
  | here :: rest ->
      let tx, ty = (match rest with next :: _ -> next | [] -> here) in
      let tx = float_of_int tx and ty = float_of_int ty in
      let dx = tx - m.x and dy = ty - m.y in
      let d = Float.hypot dx dy in
      if d <= m.speed then { m with x = tx; y = ty; path = (match rest with _ :: _ -> rest | [] -> []) }
      else { m with x = m.x + (m.speed * dx / d); y = m.y + (m.speed * dy / d) }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed key = Scene2d.pressed key scenes in
  let mouse = computer.mouse in
  let g = { g with frames = g.frames +.. 1 } in
  (* the cursor, and building *)
  let x, y = g.cursor in
  let step key d = if pressed key then d else 0 in
  let cursor = (clamp 0 (cols -.. 1) (x +.. step (fun k -> k.kright) 1 +.. step (fun k -> k.kleft) (-1)),
                clamp 0 (rows -.. 1) (y +.. step (fun k -> k.kdown) 1 +.. step (fun k -> k.kup) (-1))) in
  let cursor = if mouse.mdx <> 0. || mouse.mdy <> 0. then (let c = cell_at mouse.mx mouse.my in if inside c then c else cursor) else cursor in
  let g = { g with cursor } in
  let g = if pressed (fun k -> Set_.mem "p" k.keys) then { g with show_path = not g.show_path } else g in
  let g = if mouse.mclick || pressed (fun k -> k.kspace) then build g g.cursor else g in
  (* the wave: a monster now and then, then a pause before the next *)
  let g = if g.pause > 0 then { g with pause = g.pause -.. 1 } else g in
  let g =
    if g.pause = 0 && g.to_come > 0 && g.frames mod spawn_every = 0 then
      let ex, ey = entrance in
      { g with to_come = g.to_come -.. 1;
        monsters = { x = float_of_int ex; y = float_of_int ey; path = way g.field entrance; hp = hp_of g.wave; full = hp_of g.wave;
                     speed = 0.035 + (0.002 * float_of_int g.wave) } :: g.monsters }
    else g
  in
  (* the monsters walk; those at the door cost a life *)
  let monsters = List.map walk g.monsters in
  let arrived, monsters = List.partition (fun m -> cell_of m = exit_) monsters in
  if arrived <> [] then Audio.play Audio.hit;
  let g = { g with monsters; lives = g.lives -.. List.length arrived } in
  (* the towers shoot the monster nearest their cell, in range *)
  let field = Array.copy g.field in
  let shots = ref [] in
  let monsters = ref g.monsters in
  Array.iteri
    (fun i c ->
      match c with
      | Tower last when g.frames -.. last >= tower_delay ->
          let tx, ty = (i mod cols, i /.. cols) in
          let reach (m : monster) = Float.hypot (m.x - float_of_int tx) (m.y - float_of_int ty) <= tower_range in
          (match List.filter reach !monsters with
          | [] -> ()
          | in_range ->
              let target = List.fold_left (fun best m -> if m.hp < best.hp then m else best) (List.hd in_range) in_range in
              field.(i) <- Tower g.frames;
              shots := { from = (tx, ty); at = (target.x, target.y); age = 0 } :: !shots;
              monsters := List.map (fun m -> if m == target then { m with hp = m.hp -.. tower_damage } else m) !monsters)
      | _ -> ())
    g.field;
  let dead, alive = List.partition (fun m -> m.hp <= 0) !monsters in
  if dead <> [] then Audio.play Audio.coin;
  let g =
    { g with field; monsters = alive; gold = g.gold +.. (bounty *.. List.length dead); score = g.score +.. (10 *.. List.length dead);
      shots = List.filter (fun s -> s.age < 6) (List.map (fun s -> { s with age = s.age +.. 1 }) g.shots) @ !shots }
  in
  (* the wave cleared: the next one, and a bonus *)
  if g.to_come = 0 && g.monsters = [] && g.pause = 0 then
    { g with wave = g.wave +.. 1; to_come = wave_size (g.wave +.. 1); pause = 240; gold = g.gold +.. (10 +.. (2 *.. g.wave)) }
  else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      (* "f": a fast-forward, the wait between waves skipped *)
      let fast = Set_.mem "f" computer.keyboard.keys in
      let g = update_game computer s g in
      let g = if fast then update_game computer s (update_game computer s g) else g in
      if g.lives <= 0 then Scene2d.go (Game_over g.score) s else { s with scene = Playing g }
  | Game_over _ -> if space && s.elapsed > 2. then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let at (c : int * int) (shape : shape) : shape = let x, y = center c in move x y shape

let view_game (g : game) : shape list =
  let path = way g.field entrance in
  [ rectangle (rgb 45 70 45) (float_of_int cols * size) (float_of_int rows * size) |> move_y (top - (float_of_int rows * size / 2.)) ]
  @ List.concat
      (List.init (cols *.. rows) (fun i ->
           let c = (i mod cols, i /.. cols) in
           match g.field.(i) with
           | Empty -> [ at c (rectangle (rgb 55 85 55) (size - 2.) (size - 2.)) ]
           | Tower _ ->
               [ at c (rectangle (rgb 120 120 140) (size - 6.) (size - 6.)); at c (circle (rgb 70 90 160) 12.) ]))
  @ (if g.show_path then List.map (fun c -> at c (circle (rgb 210 200 120) 4. |> fade 0.7)) path else [])
  @ [ at entrance (rectangle (rgb 80 200 110) (size - 8.) (size - 8.)); at exit_ (rectangle (rgb 220 70 70) (size - 8.) (size - 8.)) ]
  @ List.concat_map
      (fun (m : monster) ->
        let x, y = center (0, 0) in
        let x = x + (size * m.x) and y = y - (size * m.y) in
        [ circle (rgb 200 90 180) 14. |> move x y;
          rectangle (rgb 40 40 40) 28. 5. |> move x (y + 20.);
          rectangle (rgb 90 220 90) (28. * float_of_int m.hp / float_of_int m.full) 5.
          |> move (x - (14. * (1. - (float_of_int m.hp / float_of_int m.full)))) (y + 20.) ])
      g.monsters
  @ List.map
      (fun (s : shot) ->
        let x0, y0 = center s.from in
        let cx, cy = center (0, 0) in
        let x1 = cx + (size * fst s.at) and y1 = cy - (size * snd s.at) in
        rectangle (rgb 250 240 150) (Float.hypot (x1 - x0) (y1 - y0)) 3.
        |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi)
        |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.))
      g.shots
  (* what the cursor would build, and whether it may *)
  @ [ at g.cursor (rectangle (if can_build g g.cursor then white else rgb 230 80 80) (size - 4.) (size - 4.) |> fade 0.3) ]
  @ [ text yellow 2.5 (Printf.sprintf "gold %d" g.gold) |> move (-380.) 430.;
      text (rgb 230 90 90) 2.5 (Printf.sprintf "lives %d" g.lives) |> move (-100.) 430.;
      text white 2.5 (Printf.sprintf "wave %d" g.wave) |> move 150. 430.;
      text (rgb 180 190 200) 2.5 (Printf.sprintf "score %d" g.score) |> move 380. 430.;
      text (rgb 150 160 170) 2. (Printf.sprintf "a tower costs %d   p: the way   f: faster   the way is now %d steps long" tower_cost (max 0 (List.length path -.. 1)))
      |> move_y (-420.) ]
  @ if g.pause > 0 then [ text (rgb 230 210 120) 3. (Printf.sprintf "wave %d in %d" g.wave ((g.pause /.. 60) +.. 1)) |> move_y (-370.) ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 25 35 30) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game { (new_game ()) with pause = 0 }
      @ [ text yellow 6. "TINY TOWER DEFENSE" |> move_y 120.;
          text white 2.5 "build towers to make the way long; you may never close it" |> move_y 40. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-40.) ]
  | Playing g -> view_game g
  | Game_over score ->
      [ text (rgb 230 80 80) 7. "THEY GOT THROUGH" |> move_y 60.; text white 3. (Printf.sprintf "score %d" score) |> move_y (-40.) ]
      @ if s.elapsed > 2. then Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-140.) ] else [])

let app = game view update initial_model
let main = Playground_platform.run_app app
