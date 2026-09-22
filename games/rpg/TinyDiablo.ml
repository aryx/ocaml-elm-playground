(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Diablo (Blizzard North, 1996): down through a
 * dungeon drawn from one fixed angle, clicking where to walk and what
 * to kill, picking up what falls, and finding the stairs down.
 *
 *   left click   walk there -- or, on a monster, go and hit it
 *   right click  a bolt of fire, if there is mana for it
 *   q            drink a potion
 *
 * Diablo is Rogue with the turns taken out. TinyRogue.ml is the
 * same dungeon a level at a time, generated, deadly and *waiting*: it
 * moves when you press a key and not before. Take that away -- let the
 * monsters have their own clock, and let the player say where to go
 * with a mouse instead of which way to step -- and almost everything
 * else about the genre follows: the health you watch rather than
 * count, the potion drunk mid-fight, the loot on the floor you walk
 * over, and running away as a real option. Blizzard North's design
 * document reportedly described a turn-based game, and the real-time
 * prototype was the argument that won. (From memory, to check.)
 *
 * Four things in it are new to the games of this directory:
 *
 *  - **The mouse is the whole interface.** Every other game here is
 *    driven by keys, because a key is a direction and a direction is
 *    what a grid wants. A click is not a direction, it is a *place*,
 *    and turning the pixel clicked into a place is the isometric
 *    projection run backwards ([Isometric.ground]): two lines and a
 *    2x2 determinant, which exists only because there is no
 *    perspective to divide by. That inverse is why this view and the
 *    mouse suit each other, and why every game that looks like this --
 *    Populous, Syndicate, Age of Empires, Diablo -- is played with one.
 *
 *  - **Click to walk is a path, not a step**: A* from the cell you are
 *    in to the cell you clicked (one line of Ai.way, over ai/Pathfind:
 *    the same search TinyTowerDefense uses for its creeps), then walked one cell at a
 *    time. The pathfinding is the *player's*, which is unusual: in
 *    most games it belongs to the enemies. Here the monsters are the
 *    stupid ones -- they take the greedy step towards you and get
 *    stuck on corners, as Diablo's did.
 *
 *  - **The dungeon is made, not drawn**: rooms laid down at random and
 *    joined by corridors, all of it from a linear-feedback shift
 *    register whose state is in the model ([roll]), so a seed is a
 *    dungeon and the same seed is the same dungeon for ever. The same
 *    trick as TinyFlappyBird.ml's pipes, in two dimensions.
 *
 *  - **The dark**: a cell is drawn dimmer the further it is from you,
 *    which is most of what Diablo's dungeon feels like, and is one
 *    [fade] on each tile.
 *
 * What it uses: gamekits/isometric (the projection, the back-to-front
 * sort, the shadow under the bolt, and the inverse under the mouse --
 * its second game, after TinyZaxxon.ml, and what paid for
 * it), Ai.way (A* for the clicks), and Scene2d. Not Tilemap:
 * that layer draws a grid of squares on the screen's own axes, and
 * this grid is diamonds on the world's. Not Camera2d: the camera is
 * [Isometric.follow], a subtraction inside the projection. Not
 * Physics: nothing here has inertia, as nothing in Diablo does.
 *
 * Exercises: the character classes (the warrior, the rogue, the
 * sorcerer are three numbers each), a real inventory with a grid to
 * fit things into (Diablo's own, and Resident Evil's, and Deus Ex's),
 * items with affixes rolled from a table ("of the whale", the loot
 * that made the genre), monsters with resistances, a boss on the last
 * level, town between dungeons, and a second player on the same
 * dungeon (Diablo shipped with four).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The dungeon *)
(*****************************************************************************)

let cols = 30
let rows = 30

type cell = Rock | Floor | Stairs

(* A linear-feedback shift register, as in TinyFlappyBird.ml: the
 * dungeon is a pure function of its seed, so a level replays exactly. *)
let roll (seed : int) (n : int) : int * int =
  let bit = (seed lxor (seed lsr 2) lxor (seed lsr 3) lxor (seed lsr 5)) land 1 in
  let seed = (seed lsr 1) lor (bit lsl 15) in
  (seed mod n, seed)

type room = { rx : int; rz : int; rw : int; rh : int }

let centre (r : room) : int * int = (r.rx +.. (r.rw /.. 2), r.rz +.. (r.rh /.. 2))
let overlaps (a : room) (b : room) : bool =
  a.rx -.. 1 <= b.rx +.. b.rw && b.rx -.. 1 <= a.rx +.. a.rw && a.rz -.. 1 <= b.rz +.. b.rh && b.rz -.. 1 <= a.rz +.. a.rh

(* rooms dropped at random, the ones that land on another thrown away,
 * then joined in the order they were made: a corridor along x and then
 * along z, which is the oldest dungeon generator there is (Rogue's own
 * is tidier, see TinyRogue.ml) *)
let dig (seed : int) (wanted : int) : cell array array * room list * int =
  let cells = Array.make_matrix cols rows Rock in
  let seed = ref seed and rooms = ref [] in
  for _ = 1 to wanted *.. 4 do
    if List.length !rooms < wanted then begin
      let w, s = roll !seed 4 in
      let h, s = roll s 4 in
      let x, s = roll s (cols -.. 10) in
      let z, s = roll s (rows -.. 10) in
      seed := s;
      let r = { rx = x +.. 2; rz = z +.. 2; rw = w +.. 4; rh = h +.. 4 } in
      if not (List.exists (overlaps r) !rooms) then rooms := !rooms @ [ r ]
    end
  done;
  let carve i j = if i > 0 && i < cols -.. 1 && j > 0 && j < rows -.. 1 then cells.(i).(j) <- Floor in
  List.iter (fun r -> for i = r.rx to r.rx +.. r.rw do for j = r.rz to r.rz +.. r.rh do carve i j done done) !rooms;
  let rec join = function
    | a :: (b :: _ as rest) ->
        let ax, az = centre a and bx, bz = centre b in
        for i = min ax bx to max ax bx do carve i az done;
        for j = min az bz to max az bz do carve bx j done;
        join rest
    | _ -> ()
  in
  join !rooms;
  (match List.rev !rooms with
  | last :: _ ->
      let x, z = centre last in
      cells.(x).(z) <- Stairs
  | [] -> ());
  (cells, !rooms, !seed)

let at (cells : cell array array) (i : int) (j : int) : cell =
  if i < 0 || i >= cols || j < 0 || j >= rows then Rock else cells.(i).(j)

let walkable (cells : cell array array) ((i, j) : int * int) : bool = at cells i j <> Rock

(*****************************************************************************)
(* The view: one fixed angle (gamekits/isometric) *)
(*****************************************************************************)

(* a tile is a diamond 56 by 28, the grid every isometric game draws
 * itself on, and a wall is one tile high *)
let iso : Isometric.t = Isometric.make ~across:(28., 14.) ~along:(-28., 14.) ~up:34.

(* the camera is a subtraction: the player's cell sits at the middle of
 * the screen, a little below it *)
let view (x : number) (z : number) : Isometric.t = iso |> Isometric.origin 0. (-60.) |> Isometric.follow x z

(*****************************************************************************)
(* What lives in it *)
(*****************************************************************************)

type kind = Imp | Skeleton | Brute

let hp_of = function Imp -> 12. | Skeleton -> 22. | Brute -> 44.
let hit_of = function Imp -> 3. | Skeleton -> 5. | Brute -> 10.
let speed_of = function Imp -> 0.055 | Skeleton -> 0.040 | Brute -> 0.048
let worth_of = function Imp -> 8 | Skeleton -> 14 | Brute -> 30
let monster_color = function Imp -> rgb 200 150 70 | Skeleton -> rgb 225 225 210 | Brute -> rgb 190 70 60

type monster = { mx : number; mz : number; hp : number; kind : kind; cool : int; hurt : int }

type loot = Gold of int | Potion | Sword of number
type item = { ix : number; iz : number; what : loot }

(* a bolt flies a little above the floor, and its shadow says where it
 * is -- the kit's [shadow], and the only thing in the dungeon that is
 * not on the ground *)
type bolt = { bx : number; bz : number; bvx : number; bvz : number; life : int }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  cells : cell array array;
  rooms : room list;
  seed : int;
  depth : int; (* how far down: the first level is 1 *)
  px : number;
  pz : number;
  (* the cells still to walk, from A* *)
  path : (int * int) list;
  (* the monster being fought, by its place in the list *)
  target : int option;
  hp : number;
  mana : number;
  damage : number;
  gold : int;
  potions : int;
  cool : int; (* frames before the next swing *)
  hurt : int; (* frames of red, when something lands one *)
  monsters : monster list;
  items : item list;
  bolts : bolt list;
  (* the right button last frame, to fire on the press and not on the hold *)
  casting : bool;
}

type scene = Title | Playing of play | Over of int (* the depth reached *)
type model = scene Scene2d.t

let reach = 1.1 (* how close a swing lands *)
let sight = 8.5 (* how far a monster notices you *)
let bolt_cost = 8.
let bolt_hit = 16.

let populate (depth : int) (cells : cell array array) (rooms : room list) (seed : int) : monster list * int =
  let seed = ref seed and out = ref [] in
  List.iteri
    (fun i (r : room) ->
      if i > 0 then
        for _ = 1 to 1 +.. (depth /.. 2) do
          let dx, s = roll !seed r.rw in
          let dz, s = roll s r.rh in
          let k, s = roll s 10 in
          seed := s;
          let kind = if k < 5 then Imp else if k < 8 then Skeleton else Brute in
          let i = r.rx +.. dx and j = r.rz +.. dz in
          if walkable cells (i, j) then
            out :=
              { mx = float_of_int i; mz = float_of_int j;
                hp = hp_of kind * (1. + (0.12 * float_of_int (depth -.. 1)));
                kind; cool = 0; hurt = 0 }
              :: !out
        done)
    rooms;
  (!out, !seed)

let enter (depth : int) (seed : int) (carry : play option) : play =
  let cells, rooms, seed = dig seed (5 +.. min 3 depth) in
  let monsters, seed = populate depth cells rooms seed in
  let sx, sz = match rooms with r :: _ -> centre r | [] -> (2, 2) in
  {
    cells; rooms; seed; depth;
    px = float_of_int sx; pz = float_of_int sz;
    path = []; target = None;
    hp = (match carry with Some p -> p.hp | None -> 60.);
    mana = (match carry with Some p -> Float.max p.mana 20. | None -> 30.);
    damage = (match carry with Some p -> p.damage | None -> 6.);
    gold = (match carry with Some p -> p.gold | None -> 0);
    potions = (match carry with Some p -> p.potions | None -> 1);
    cool = 0; hurt = 0; monsters; items = []; bolts = []; casting = false;
  }

let start () : play = enter 1 24601 None
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let cell_of (x : number) (z : number) : int * int = (int_of_float (Float.round x), int_of_float (Float.round z))
let apart (ax : number) (az : number) (bx : number) (bz : number) : number = Float.hypot (bx - ax) (bz - az)

(* A* from where you stand to where you clicked: the player's own
 * pathfinding, which is the click-to-walk of every game of this shape *)
let path_to (cells : cell array array) (from : int * int) (goal : int * int) : (int * int) list =
  Ai.way ~walkable:(walkable cells) from goal

(* one step along the path, and the path shortened when a cell is
 * reached *)
let walk (speed : number) (p : play) : play =
  match p.path with
  | [] -> p
  | (i, j) :: rest ->
      let tx = float_of_int i and tz = float_of_int j in
      let d = apart p.px p.pz tx tz in
      if d < speed then { p with px = tx; pz = tz; path = rest }
      else { p with px = p.px + ((tx - p.px) / d * speed); pz = p.pz + ((tz - p.pz) / d * speed) }

(* a monster takes the greedy step towards you, and gets stuck on
 * corners doing it -- which is what Diablo's did *)
let chase (p : play) (m : monster) : monster =
  let d = apart m.mx m.mz p.px p.pz in
  if d > sight || d < 0.9 then m
  else
    let speed = speed_of m.kind in
    let dx = p.px - m.mx and dz = p.pz - m.mz in
    let try_step (nx : number) (nz : number) : (number * number) option =
      if walkable p.cells (cell_of nx nz) then Some (nx, nz) else None
    in
    let along_x = try_step (m.mx + (Float.max (-.speed) (Float.min speed dx))) m.mz in
    let along_z = try_step m.mx (m.mz + (Float.max (-.speed) (Float.min speed dz))) in
    let first, second = if Float.abs dx > Float.abs dz then (along_x, along_z) else (along_z, along_x) in
    (match (first, second) with
    | Some (x, z), _ | None, Some (x, z) -> { m with mx = x; mz = z }
    | None, None -> m)

let drop (seed : int) (m : monster) : item list * int =
  let r, seed = roll seed 100 in
  let what =
    if r < 45 then Some (Gold (5 +.. (r *.. worth_of m.kind /.. 20)))
    else if r < 62 then Some Potion
    else if r < 68 then Some (Sword (1. + (0.5 * float_of_int (worth_of m.kind) / 10.)))
    else None
  in
  (match what with Some what -> [ { ix = m.mx; iz = m.mz; what } ] | None -> []), seed

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let mouse = computer.mouse in
  let v = view p.px p.pz in
  (* where the mouse is, in the dungeon: the projection run backwards *)
  let wx, wz = Isometric.ground v (mouse.mx, mouse.my) in
  let clicked_monster =
    let best = ref None in
    List.iteri
      (fun i (m : monster) ->
        if apart wx wz m.mx m.mz < 0.8 then
          match !best with Some (d, _) when d <= apart wx wz m.mx m.mz -> () | _ -> best := Some (apart wx wz m.mx m.mz, i))
      p.monsters;
    Option.map snd !best
  in
  let p =
    if not mouse.mclick then p
    else
      match clicked_monster with
      | Some i ->
          let m = List.nth p.monsters i in
          { p with target = Some i; path = path_to p.cells (cell_of p.px p.pz) (cell_of m.mx m.mz) }
      | None -> { p with target = None; path = path_to p.cells (cell_of p.px p.pz) (cell_of wx wz) }
  in
  (* the bolt: on the press of the right button, not on the hold *)
  let casting = mouse.mrdown in
  let firing = casting && (not p.casting) && p.mana >= bolt_cost in
  let d = Float.max 0.001 (apart p.px p.pz wx wz) in
  let bolts =
    (if firing then [ { bx = p.px; bz = p.pz; bvx = (wx - p.px) / d * 0.22; bvz = (wz - p.pz) / d * 0.22; life = 90 } ] else [])
    @ List.filter_map
        (fun (b : bolt) ->
          let bx = b.bx + b.bvx and bz = b.bz + b.bvz in
          if b.life <= 0 || not (walkable p.cells (cell_of bx bz)) then None
          else Some { b with bx; bz; life = b.life -.. 1 })
        p.bolts
  in
  (* what the bolts hit, and what the swing does *)
  let target_alive = match p.target with Some i -> i < List.length p.monsters | None -> false in
  let swinging =
    p.cool = 0 && target_alive
    && (match p.target with Some i -> apart p.px p.pz (List.nth p.monsters i).mx (List.nth p.monsters i).mz <= reach | None -> false)
  in
  let hurt_monster (i : int) (m : monster) : monster =
    let by_bolt = List.exists (fun (b : bolt) -> apart b.bx b.bz m.mx m.mz < 0.6) bolts in
    let by_swing = swinging && p.target = Some i in
    if by_bolt || by_swing then
      { m with hp = m.hp - (if by_bolt then bolt_hit else 0.) - if by_swing then p.damage else 0.; hurt = 8 }
    else { m with hurt = max 0 (m.hurt -.. 1) }
  in
  let monsters = List.mapi hurt_monster p.monsters in
  let bolts = List.filter (fun (b : bolt) -> not (List.exists (fun (m : monster) -> apart b.bx b.bz m.mx m.mz < 0.6) p.monsters)) bolts in
  (* the dead drop what they carry *)
  let dead, alive = List.partition (fun (m : monster) -> m.hp <= 0.) monsters in
  let items, seed =
    List.fold_left (fun (acc, s) m -> let l, s = drop s m in (acc @ l, s)) (p.items, p.seed) dead
  in
  let gained = List.fold_left (fun n (m : monster) -> n +.. worth_of m.kind) 0 dead in
  (* the living chase and hit back *)
  let monsters = List.map (chase { p with monsters = alive }) alive in
  let landed =
    List.fold_left
      (fun n (m : monster) -> if m.cool = 0 && apart m.mx m.mz p.px p.pz <= 1.0 then n + hit_of m.kind else n)
      0. monsters
  in
  let monsters =
    List.map
      (fun (m : monster) ->
        if m.cool = 0 && apart m.mx m.mz p.px p.pz <= 1.0 then { m with cool = 45 } else { m with cool = max 0 (m.cool -.. 1) })
      monsters
  in
  (* walking, and what is underfoot *)
  let p = { p with monsters; items; seed; bolts; casting } in
  let p = if swinging then { p with cool = 26 } else { p with cool = max 0 (p.cool -.. 1) } in
  let p = walk 0.075 p in
  let picked, left = List.partition (fun (it : item) -> apart p.px p.pz it.ix it.iz < 0.6) p.items in
  let gold = List.fold_left (fun n (it : item) -> match it.what with Gold g -> n +.. g | _ -> n) 0 picked in
  let potions = List.fold_left (fun n (it : item) -> match it.what with Potion -> n +.. 1 | _ -> n) 0 picked in
  let damage = List.fold_left (fun d (it : item) -> match it.what with Sword s -> Float.max d (p.damage + s) | _ -> d) p.damage picked in
  (* the potion, and the stairs *)
  let drinking = Scene2d.pressed (fun k -> Set_.mem "q" k.keys) scenes && p.potions > 0 && p.hp < 60. in
  let i, j = cell_of p.px p.pz in
  let down = at p.cells i j = Stairs in
  let p =
    { p with
      hp = Float.min 60. (p.hp - landed + if drinking then 25. else 0.);
      mana = Float.min 30. (p.mana - (if firing then bolt_cost else 0.) + 0.02);
      hurt = (if landed > 0. then 10 else max 0 (p.hurt -.. 1));
      potions = p.potions -.. (if drinking then 1 else 0) +.. potions;
      gold = p.gold +.. gold +.. gained;
      damage;
      items = left;
      target = (if target_alive then p.target else None);
      monsters;
    }
  in
  if down then enter (p.depth +.. 1) p.seed (Some p) else p

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title | Over _ ->
      if Scene2d.pressed (fun k -> k.kspace) scenes || computer.mouse.mclick then Scene2d.go (Playing (start ())) scenes
      else scenes
  | Playing p ->
      let p = update_play computer scenes p in
      if p.hp <= 0. then Scene2d.go (Over p.depth) scenes else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let rock_top = rgb 92 84 78
let rock_side = rgb 62 56 52
let floor_a = rgb 58 52 46
let floor_b = rgb 50 45 40
let stairs_color = rgb 120 110 80

(* the dark: a tile is dimmer the further it is from you, which is most
 * of what the dungeon feels like *)
let lit (d : number) : number = Float.max 0.12 (Float.min 1. (1.7 - (d / 6.)))

let tile_shape (v : Isometric.t) (i : int) (j : int) (color : color) (light : number) : shape =
  let x = float_of_int i and z = float_of_int j in
  let corner dx dz = Isometric.project v (x + dx, 0., z + dz) in
  polygon color [ corner (-0.5) (-0.5); corner 0.5 (-0.5); corner 0.5 0.5; corner (-0.5) 0.5 ] |> fade light

(* a wall is a cube: the top, and the two faces that look at you *)
let wall_shape (v : Isometric.t) (i : int) (j : int) (light : number) : shape =
  let x = float_of_int i and z = float_of_int j in
  let p dx y dz = Isometric.project v (x + dx, y, z + dz) in
  group
    [ polygon rock_side [ p (-0.5) 0. (-0.5); p (-0.5) 0. 0.5; p (-0.5) 1. 0.5; p (-0.5) 1. (-0.5) ];
      polygon rock_side [ p (-0.5) 0. (-0.5); p 0.5 0. (-0.5); p 0.5 1. (-0.5); p (-0.5) 1. (-0.5) ];
      polygon rock_top [ p (-0.5) 1. (-0.5); p 0.5 1. (-0.5); p 0.5 1. 0.5; p (-0.5) 1. 0.5 ] ]
  |> fade light

let body (color : color) (tall : number) : shape =
  group [ oval (rgb 20 18 16) 26. 12. |> fade 0.45; rectangle color 16. tall |> move_y (tall / 2.); circle color 9. |> move_y (tall + 6.) ]

let hero_shape (p : play) : shape =
  let c = if p.hurt > 0 then rgb 250 140 130 else rgb 90 130 210 in
  group
    [ body c 30.;
      (* a cloak, and the sword he swings *)
      rectangle (rgb 170 60 55) 20. 18. |> move_y 12.;
      rectangle (rgb 225 225 240) 4. 26. |> rotate (if p.cool > 13 then 70. else 25.) |> move 14. 20. ]

let monster_shape (m : monster) : shape =
  let c = if m.hurt > 0 then white else monster_color m.kind in
  let tall = match m.kind with Imp -> 18. | Skeleton -> 24. | Brute -> 32. in
  group [ body c tall; rectangle (rgb 40 200 80) (26. * (m.hp / hp_of m.kind)) 3. |> move_y (tall + 20.) ]

let item_shape (it : item) : shape =
  match it.what with
  | Gold _ -> group [ circle (rgb 240 200 70) 6.; circle (rgb 250 230 140) 3. |> move (-2.) 2. ]
  | Potion -> group [ rectangle (rgb 220 60 70) 8. 12.; rectangle (rgb 180 180 190) 4. 4. |> move_y 7. ]
  | Sword _ -> group [ rectangle (rgb 200 210 230) 4. 20. |> rotate 45.; rectangle (rgb 140 100 60) 10. 4. |> rotate 45. |> move (-5.) (-5.) ]

(* the two orbs, which is how Diablo says "health" and "mana" without a
 * number anywhere *)
let orbs (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let orb (x : number) (full : number) (c : color) : shape list =
    let r = 56. and y = screen.bottom + 70. in
    (* the liquid is an oval as wide as the glass is at its surface, so
     * that it fills a round orb rather than a square one *)
    let wide = Float.sqrt (Float.max 0. (1. - ((1. - full) ** 2.))) in
    [ circle (rgb 25 22 20) r |> move x y;
      oval c (2. * r * wide) (2. * r * full) |> move x (y - (r * (1. - full)));
      circle (rgb 120 110 100) r |> fade 0.15 |> move x y ]
  in
  orb (screen.left + 90.) (p.hp / 60.) (rgb 190 40 40) @ orb (screen.right - 90.) (p.mana / 30.) (rgb 60 90 220)

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let v = view p.px p.pz in
  let far = 11 in
  let pi, pj = cell_of p.px p.pz in
  let world = ref [] in
  let add (point : number * number * number) (s : shape) = world := (Isometric.depth v point, s) :: !world in
  for i = max 0 (pi -.. far) to min (cols -.. 1) (pi +.. far) do
    for j = max 0 (pj -.. far) to min (rows -.. 1) (pj +.. far) do
      let x = float_of_int i and z = float_of_int j in
      let light = lit (apart p.px p.pz x z) in
      match at p.cells i j with
      | Rock ->
          (* only the rock that touches a floor is worth drawing *)
          if List.exists (fun (di, dj) -> at p.cells (i +.. di) (j +.. dj) <> Rock) [ (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1) ] then
            add (x, 0.5, z) (wall_shape v i j light)
      | Floor -> add (x, -0.01, z) (tile_shape v i j (if (i +.. j) mod 2 = 0 then floor_a else floor_b) light)
      | Stairs -> add (x, -0.01, z) (tile_shape v i j stairs_color light)
    done
  done;
  List.iter (fun (it : item) -> add (it.ix, 0., it.iz) (Isometric.at v (it.ix, 0., it.iz) (item_shape it) |> fade (lit (apart p.px p.pz it.ix it.iz)))) p.items;
  List.iter
    (fun (m : monster) ->
      let light = lit (apart p.px p.pz m.mx m.mz) in
      add (m.mx, 0., m.mz) (Isometric.at v (m.mx, 0., m.mz) (monster_shape m) |> fade light))
    p.monsters;
  List.iter
    (fun (b : bolt) ->
      add (b.bx, 0.4, b.bz) (Isometric.at v (b.bx, 0.4, b.bz) (circle (rgb 255 170 60) 7.));
      add (b.bx, 0., b.bz) (Isometric.shadow v (b.bx, 0.4, b.bz) (oval (rgb 10 10 10) 10. 5. |> fade 0.5)))
    p.bolts;
  add (p.px, 0., p.pz) (Isometric.at v (p.px, 0., p.pz) (hero_shape p));
  (* where the mouse is, on the floor *)
  let wx, wz = Isometric.ground v (computer.mouse.mx, computer.mouse.my) in
  let cursor =
    if walkable p.cells (cell_of wx wz) then
      [ tile_shape v (fst (cell_of wx wz)) (snd (cell_of wx wz)) (rgb 220 220 160) 0.25 ]
    else []
  in
  [ rectangle (rgb 10 9 8) screen.width screen.height ]
  @ Isometric.sorted !world @ cursor @ orbs computer p
  @ [ text (rgb 220 210 180) 2. (Printf.sprintf "level %d    gold %d    potions %d (q)" p.depth p.gold p.potions)
      |> move_y (screen.top - 40.);
      text (rgb 150 140 120) 1.8 "click: walk and fight    right click: a bolt of fire" |> move_y (screen.bottom + 25.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      [ rectangle (rgb 10 9 8) screen.width screen.height;
        text (rgb 200 60 50) 6. "TINY DIABLO" |> move_y 200.;
        text (rgb 210 200 180) 2. "click where to walk, and on what to kill" |> move_y 60.;
        text (rgb 210 200 180) 2. "right click: a bolt of fire    q: a potion" |> move_y 20.;
        text (rgb 150 140 120) 2. "find the stairs. they only go down." |> move_y (-40.) ]
      @ Scene2d.blink 1. model [ text (rgb 220 210 180) 3. "CLICK TO GO IN" |> move_y (-200.) ]
  | Playing p -> view_play computer p
  | Over depth ->
      [ rectangle (rgb 10 9 8) screen.width screen.height;
        text (rgb 200 60 50) 5. "YOU DIED" |> move_y 100.;
        text (rgb 210 200 180) 3. (Printf.sprintf "level %d" depth) |> move_y 10. ]
      @ Scene2d.blink 1. model [ text (rgb 220 210 180) 3. "CLICK TO GO BACK IN" |> move_y (-200.) ]

let help =
  {|TinyDiablo
  left click   walk there, or go and hit what you clicked
  right click  a bolt of fire, if there is mana for it
  q            drink a potion
  the stairs are somewhere down there; they only go down
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
