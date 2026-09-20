(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Dungeon Master (FTL Games, 1987), in the *2D*
 * playground: one hero alone underground, with a torch, a sword and no
 * map. Up/down walk forward and back, left/right turn a quarter turn,
 * a/d sidestep, and space is the hand: it swings the sword at what is
 * in front, pulls a lever, turns a key in a door. Find the iron key,
 * open the iron door, pull the lever that raises the portcullis, take
 * the stairs down -- before the torch burns out.
 *
 * This is the oldest of this directory's tricks, and the only one with
 * no arithmetic per frame at all: there is no projection in this game.
 * The hero stands in the middle of a cell and looks along one of four
 * directions, so the same few cells are always in view -- at depth d,
 * the 2d + 1 of them across -- and each one has *one* place on the
 * screen, the same every time. The front walls of the cells straight
 * ahead are nested frames, and every other cell hangs off them:
 *
 *      +-----------------------------+     at depth 4 the cell ahead is
 *      |            depth 4          |     a small square in the middle
 *      |   +---------------------+   |     of the screen, at depth 1 a
 *      |   |        depth 3      |   |     big one, and the cells to
 *      |   |  +---------------+  |   |     the sides are the slots next
 *      |   |  |    depth 2    |  |   |     to them. A frame is: for
 *      |   |  |  +---------+  |  |   |     each depth, farthest first,
 *      |   |  |  | depth 1 |  |  |   |     draw the slots whose cell is
 *      |   |  |  |         |  |  |   |     a wall. Nearer walls are
 *      |   |  |  +---------+  |  |   |     drawn over farther ones and
 *      |   |  +---------------+  |   |     hide them: the painter's
 *      |   +---------------------+   |     algorithm, with the order
 *      +-----------------------------+     known in advance.
 *
 * A wall shows two faces: the one facing us ([front_face], a rectangle)
 * and, if it stands to one side, the face along the corridor
 * ([side_face], a trapezoid receding towards the middle of the screen).
 * Those two are the whole renderer. FTL's artists drew them by hand,
 * one picture per slot, and the game only picked which to blit; we work
 * them out from the distance instead ([at]), which comes to the same
 * picture -- and we can then say what the artists could not: the field
 * of view is exactly 90 degrees, which is why one unit at distance 1 is
 * half the screen wide, and why the walls on either side of the hero
 * fall exactly on the screen's edges, so nothing ever needs clipping.
 *
 * The price of the trick is in the rules, as always here: the hero
 * moves a whole cell and turns a quarter turn at a time, because the
 * view only exists for a cell's center and one of four directions.
 * Dungeon Master made that a virtue. Its fights are a dance on the
 * grid: a monster that has just moved cannot strike yet ([move_rest]
 * and [attack_rest] below), so you hit it, sidestep out of its reach,
 * and hit it again as it comes round -- footwork, on a chessboard, in
 * real time. That last part was the 1987 novelty: Wizardry (Sir-Tech,
 * 1981) and its many children drew the same view but waited politely
 * for your move, while Dungeon Master's dungeon kept going while you
 * thought, and you fought and cast spells by hand, with the mouse.
 *
 * Which is why this is the longest game in games2.5d/ although its
 * renderer is the shortest one here, and that is the whole point: the
 * other games of this directory are locomotion, and nearly all of their
 * code draws. Dungeon Master's innovation was not in the pixels -- its
 * view is a set of pictures a 1987 artist could paint, and no game
 * after it drew a dungeon this way for long -- it was in the interface:
 * real time, a world you reach into with the mouse, and everything the
 * hero has on the screen where you can act on it, which is what the bar
 * of hit points, the guttering torch, the key in the corner and the
 * message at the bottom are doing down in [view_hud]. So this file is
 * mostly the things you can do and what tells you about them, and the
 * renderer is a page. That ratio is the game.
 *
 * Before it: Maze War (Steve Colley, Greg Thompson, Howard Palmer, NASA
 * Ames, 1973-74, the first first-person anything, and soon the first
 * networked deathmatch) and 3D Monster Maze (Malcolm Evans, ZX81,
 * 1981), both already drawing cells as nested frames. After it: Eye of
 * the Beholder (Westwood, 1991), and then Ultima Underworld (1992),
 * which broke the grid and took the genre to the raycasters of the next
 * section. Legend of Grimrock (Almost Human, 2012) came back to the
 * grid on purpose, to get the dance back. (Names and dates from memory,
 * to check.)
 *
 * What it uses: Tilemap (the dungeon, changed as doors open, levers are
 * pulled and things are picked up), ai/Pathfind (the monsters walk the
 * shortest way to you: the same A* as TinyTowerDefense's), Scene2d (the
 * title and the endings). Not Camera2d: there is no camera, the view is
 * the slots. Not the maze kit's Grid_move either: that one slides a
 * mover between tiles, and here a step is a whole cell, at once.
 *
 * Exercises: a party of four heroes, as the original has, each with its
 * own hit points; the hand as a real hand (Dungeon Master's great idea:
 * you drag things out of the world and into it with the mouse, and the
 * playground gives you computer.mouse); doors you open by clicking, and
 * a monster that follows you through; a pressure pad under the floor
 * that opens something far away; spells; textured walls, for which the
 * hard part is already done -- [side_face] cuts a receding wall into
 * slices along z to light it, and a slice of a picture would go in
 * exactly the same place. Not a games3d/ twin, though, the way
 * TinyWolfenstein3d is TinyWolfenstein's: the grid view is not a way of drawing that
 * an engine would do better, it is a rule about where the hero may
 * stand and which way it may look, and everything above is built on it.
 * Give the camera its freedom and the game goes with it.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The dungeon *)
(*****************************************************************************)

(* '#' stone, 'D' the iron door, 'P' the portcullis, 'L' a lever in the
 * wall ('l' once pulled), '.' floor, '@' where the hero starts (facing
 * north, the top of the map), 'k' the iron key, 'f' bread, 't' a spare
 * torch, 'm' a monster, '>' the stairs down. *)
let dungeon =
  [ "################";
    "#..t...........#";
    "#.############.#";
    "#.#.m####.>.##.#";
    "#.#..####...##.#";
    "#.#..#####P###.#";
    "#.#.######.#L#.#";
    "#............f.#";
    "######D#########";
    "#..............#";
    "#.#.######.###.#";
    "#.#.k###.....#.#";
    "#.#..###.m...#.#";
    "#.#..###...t.#.#";
    "#..@...........#";
    "################" ]

(* one cell per unit: the grid's coordinates are the map's own, x to the
 * right and y *down*, like the rows of the strings *)
let level = Tilemap.of_strings 1. dungeon

(* what you cannot walk into. A lever is part of its wall, and a closed
 * door and a lowered portcullis are walls until they aren't. *)
let wall (c : char option) : bool = match c with Some ('.' | '@' | 'k' | 'f' | 't' | 'm' | '>') -> false | _ -> true

let loose (c : char option) : bool = match c with Some ('k' | 'f' | 't') -> true | _ -> false

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type dir = North | East | South | West

(* [hurt] and [cool] are frame counters: how long it still flinches, and
 * how long before it may act again *)
type monster = { mx : int; my : int; hp : int; cool : int; hurt : int }

type game = {
  map : Tilemap.t;
  x : int;
  y : int;
  facing : dir;
  rest : int; (* frames before the hero may step or turn again *)
  swing : int; (* frames left of the sword's swing *)
  hp : int;
  torch : number; (* frames of light left in it *)
  keys : int;
  monsters : monster list;
  said : string * int; (* the last thing that happened, and for how long *)
  frames : int;
}

type scene = Title | Playing of game | Died of int | Escaped of int
type model = scene Scene2d.t

let max_hp = 100
let full_torch = 5400. (* a minute and a half of it *)
let walk_rest = 9 (* about seven steps a second: Dungeon Master's pace *)
let turn_rest = 7
let swing_frames = 22 (* the sword's swing, and its recovery *)
let sword = 12
let claw = 8
let monster_hp = 30

(* a monster that has just moved cannot strike at once: the gap between
 * these two is the dance (see the header) *)
let move_rest = 22
let attack_rest = 48

let cells_of (c : char) : (int * int) list = Tilemap.find level c

let new_game () : game =
  let x, y = match cells_of '@' with p :: _ -> p | [] -> (1, 1) in
  { map = List.fold_left (fun m (c, r) -> Tilemap.set m c r '.') level (cells_of 'm' @ cells_of '@');
    x; y; facing = North; rest = 0; swing = 0; hp = max_hp; torch = full_torch; keys = 0;
    monsters = List.map (fun (mx, my) -> { mx; my; hp = monster_hp; cool = move_rest; hurt = 0 }) (cells_of 'm');
    said = ("the door shuts behind you", 150); frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The four directions *)
(*****************************************************************************)

let ahead (d : dir) : int * int =
  match d with North -> (0, -1) | East -> (1, 0) | South -> (0, 1) | West -> (-1, 0)

let right_of (d : dir) : dir = match d with North -> East | East -> South | South -> West | West -> North
let left_of (d : dir) : dir = right_of (right_of (right_of d))
let back_of (d : dir) : dir = right_of (right_of d)

(* the cell in the slot [r] cells to the right, [d] cells ahead: the one
 * and only thing the view needs from the map *)
let cell_of (g : game) (r : int) (d : int) : int * int =
  let fx, fy = ahead g.facing and rx, ry = ahead (right_of g.facing) in
  (g.x +.. (d *.. fx) +.. (r *.. rx), g.y +.. (d *.. fy) +.. (r *.. ry))

let tile_of (g : game) (r : int) (d : int) : char option =
  let cx, cy = cell_of g r d in
  Tilemap.get g.map cx cy

(*****************************************************************************)
(* Walking, and the hand *)
(*****************************************************************************)

let say (what : string) (g : game) : game = { g with said = (what, 100) }

(* what the hero picks up by standing on it *)
let take (g : game) : game =
  let c = Tilemap.get g.map g.x g.y in
  if not (loose c) then g
  else
    let g = { g with map = Tilemap.set g.map g.x g.y '.' } in
    match c with
    | Some 'k' -> say "an iron key" { g with keys = g.keys +.. 1 }
    | Some 'f' -> say "a loaf of bread" { g with hp = min max_hp (g.hp +.. 25) }
    | _ -> say "a fresh torch" { g with torch = Float.min full_torch (g.torch + 2400.) }

(* a step of one cell, in any of the four directions (forward, back, and
 * the two sidesteps): a locked door says so, a monster is in the way *)
let step (g : game) (d : dir) : game =
  let dx, dy = ahead d in
  let x = g.x +.. dx and y = g.y +.. dy in
  let g = { g with rest = walk_rest } in
  if Tilemap.get g.map x y = Some 'D' then say "the iron door is locked" g
  else if wall (Tilemap.get g.map x y) then g
  else if List.exists (fun m -> (m.mx, m.my) = (x, y)) g.monsters then say "something is in the way" g
  else take { g with x; y }

let turn (g : game) (d : dir) : game = { g with facing = d; rest = turn_rest }

(* The hand, on the cell in front: the sword first (a monster there), then
 * the wall's own answer -- a lever to pull, a door the iron key opens. *)
let hand (g : game) : game =
  let dx, dy = ahead g.facing in
  let x = g.x +.. dx and y = g.y +.. dy in
  let g = { g with swing = swing_frames } in
  let there, others = List.partition (fun m -> (m.mx, m.my) = (x, y)) g.monsters in
  if there <> [] then
    let hit (m : monster) = { m with hp = m.hp -.. sword; hurt = 12 } in
    let struck = List.map hit there in
    let killed = List.filter (fun (m : monster) -> m.hp <= 0) struck in
    say (if killed = [] then "you strike it" else "it falls")
      { g with monsters = List.filter (fun (m : monster) -> m.hp > 0) struck @ others }
  else
    match Tilemap.get g.map x y with
    | Some 'L' ->
        (* every portcullis in the dungeon, wherever it is: the lever
         * stays pulled ('l'), so you can see you have used it *)
        let map = List.fold_left (fun m (c, r) -> Tilemap.set m c r '.') g.map (Tilemap.find g.map 'P') in
        say "somewhere a portcullis grinds open" { g with map = Tilemap.set map x y 'l' }
    | Some 'D' when g.keys > 0 ->
        say "the iron key turns" { g with map = Tilemap.set g.map x y '.'; keys = g.keys -.. 1 }
    | Some 'D' -> say "the iron door is locked" g
    | _ -> g

(*****************************************************************************)
(* The monsters *)
(*****************************************************************************)

(* where a monster may walk: the open cells of the map (the door stops
 * it too, while it is shut) *)
let problem (g : game) : (int * int) Pathfind.problem =
  { neighbors =
      (fun (c, r) ->
        List.filter_map
          (fun (dc, dr) ->
            let n = (c +.. dc, r +.. dr) in
            if wall (Tilemap.get g.map (fst n) (snd n)) then None else Some (n, 1.))
          [ (1, 0); (-1, 0); (0, 1); (0, -1) ]);
    goal = (fun c -> c = (g.x, g.y));
    estimate = (fun c -> Pathfind.manhattan c (g.x, g.y)) }

let next_to (m : monster) (g : game) : bool = abs (m.mx -.. g.x) +.. abs (m.my -.. g.y) = 1

(* Each monster acts on its own clock, whatever the hero is doing --
 * that is the whole of Dungeon Master's real time. When its clock runs
 * out it strikes if it can reach you, and otherwise takes one step
 * along the shortest way to you. *)
let step_monsters (g : game) : game =
  let g, monsters =
    List.fold_left
      (fun ((g : game), acc) (m : monster) ->
        let m = { m with hurt = max 0 (m.hurt -.. 1) } in
        if m.cool > 0 then (g, { m with cool = m.cool -.. 1 } :: acc)
        else if next_to m g then
          (say "it strikes you!" { g with hp = g.hp -.. claw }, { m with cool = attack_rest } :: acc)
        else
          let taken (x, y) = List.exists (fun (o : monster) -> (o.mx, o.my) = (x, y)) (acc @ g.monsters) in
          match (Pathfind.astar (problem g) (m.mx, m.my)).path with
          | _ :: next :: _ when not (taken next) ->
              (g, { m with mx = fst next; my = snd next; cool = move_rest } :: acc)
          | _ -> (g, { m with cool = move_rest } :: acc))
      (g, []) g.monsters
  in
  { g with monsters = List.rev monsters }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (s : model) (g : game) : game =
  let k = computer.keyboard in
  let g =
    { g with frames = g.frames +.. 1; torch = Float.max 0. (g.torch - 1.);
      swing = max 0 (g.swing -.. 1);
      said = (let what, n = g.said in if n > 0 then (what, n -.. 1) else ("", 0)) }
  in
  (* one step or one quarter turn at a time, and only when the hero has
   * caught its breath *)
  let g =
    if g.rest > 0 then { g with rest = g.rest -.. 1 }
    else if k.kup then step g g.facing
    else if k.kdown then step g (back_of g.facing)
    else if k.ka then step g (left_of g.facing)
    else if k.kd then step g (right_of g.facing)
    else if k.kleft then turn g (left_of g.facing)
    else if k.kright then turn g (right_of g.facing)
    else g
  in
  let g = if Scene2d.pressed (fun k -> k.kspace) s && g.swing = 0 then hand g else g in
  step_monsters g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if Tilemap.get g.map g.x g.y = Some '>' then Scene2d.go (Escaped (int_of_float g.torch /.. 60)) s
      else if g.hp <= 0 then Scene2d.go (Died g.frames) s
      else { s with scene = Playing g }
  | Died _ | Escaped _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* The slots: where a cell goes on the screen -- the trick of this game,
 * in 24 lines, the shortest in games2.5d/ (the two faces it feeds,
 * [front_face] and [side_face], are 30 more; see the header) *)
(*****************************************************************************)

let depth_max = 4

(* How many pixels one unit takes at distance [z]. The field of view is
 * 90 degrees: half the screen's width is one unit at distance 1, so a
 * cell (one unit wide, one unit tall, the eye halfway up it) at
 * distance z is [at screen z] pixels each way, and the walls on either
 * side of the hero, at half a unit, land on the screen's edges. *)
let at (screen : screen) (z : number) : number = screen.width / 2. / z

(* A cell at depth [d] is the slice of the world from z = d - 0.5 (the
 * face it shows us) to z = d + 0.5 (the one behind it). *)
let near (d : int) : number = float_of_int d - 0.5
let far (d : int) : number = float_of_int d + 0.5

(* the slots of a depth: 2d + 1 cells across, from the leftmost *)
let across (d : int) : int list = List.init ((2 *.. d) +.. 1) (fun i -> i -.. d)

(* the depths, farthest first: the order everything is drawn in *)
let depths : int list = List.rev (List.init depth_max (fun i -> i +.. 1))

(*****************************************************************************)
(* The light *)
(*****************************************************************************)

(* The torch is the only light, so a face's colour is its own colour
 * dimmed by how far away it is and by how much of the torch is left: as
 * it burns down the dungeon closes in, until only the wall in front of
 * you is lit. Dungeon Master's darkness was famous for this. *)
let light (g : game) (z : number) : number =
  let flame = Float.min 1. (g.torch / 900.) in
  let reach = 1.2 + (3.2 * flame) in
  Float.max 0.05 (flame * (1. - ((z - 0.5) / reach)))

(* Everything that recedes -- a side wall, the floor, the ceiling --
 * runs from z = d - 0.5 to z = d + 0.5, and at depth 1 that slice is
 * half the screen, over which the light changes a lot. FTL's artists
 * painted the shading into their pictures; we cut the quadrilateral
 * into a few slices along z and light each one. *)
let slices = 5

let steps (d : int) : (number * number) list =
  let z k = near d + (float_of_int k / float_of_int slices) in
  List.init slices (fun i -> (z i, z (i +.. 1)))

let dim (b : number) ((r, gr, bl) : int * int * int) : color =
  let v x = int_of_float (Float.max 0. (Float.min 255. (float_of_int x * b))) in
  rgb (v r) (v gr) (v bl)

let stone = (142, 134, 122)
let iron = (96, 96, 108)
let floor_grey = (92, 86, 76)
let ceiling_grey = (56, 53, 50)

(*****************************************************************************)
(* View: the walls *)
(*****************************************************************************)

let quad (c : color) (x1, y1) (x2, y2) (x3, y3) (x4, y4) : shape =
  polygon c [ (x1, y1); (x2, y2); (x3, y3); (x4, y4) ]

(* Every face is drawn in its own 1 x 1 square and then scaled to its
 * slot: all the arithmetic there is in this renderer is [at screen z].
 * What is fixed to a wall, then, in the face's square: *)
let wall_art (b : number) (c : char) : shape list =
  let i a = dim (b * a) iron in
  match c with
  | 'D' ->
      [ rectangle (i 0.7) 0.9 0.06 |> move_y 0.22;
        rectangle (i 0.7) 0.9 0.06 |> move_y (-0.22);
        circle (i 0.35) 0.05 |> move_x 0.3 ]
  | 'P' ->
      (rectangle (i 1.) 1. 0.08 |> move_y 0.3)
      :: List.init 5 (fun k -> rectangle (i 1.) 0.07 1. |> move_x (((float_of_int k + 0.5) / 5.) - 0.5))
  | ('L' | 'l') as c ->
      let down = c = 'l' in
      [ rectangle (i 0.6) 0.16 0.16;
        rectangle (i 1.) 0.06 0.3 |> rotate (if down then 135. else 45.) |> move_y (if down then -0.08 else 0.12) ]
  | _ ->
      (* the courses of the blocks: without them a wall right in front of
       * you is a flat colour and you cannot tell how far away it is *)
      List.concat_map
        (fun k ->
          let y = (float_of_int k / 3.) - 0.5 in
          [ rectangle (dim (b * 0.8) stone) 1. 0.02 |> move_y y;
            rectangle (dim (b * 0.8) stone) 0.02 (1. / 3.)
            |> move (if k mod 2 = 0 then 0.25 else -0.25) (y + (1. / 6.)) ])
        [ 1; 2 ]

(* the face a wall turns towards us, at z = d - 0.5: a square, and what
 * hangs on it. A portcullis is bars over the dark room behind it. *)
let front_face (g : game) (screen : screen) (r : int) (d : int) : shape =
  let s = at screen (near d) in
  let c = Option.value (tile_of g r d) ~default:'#' in
  let b = light g (near d) in
  let behind = match c with 'P' -> dim (b * 0.15) stone | 'D' -> dim b iron | _ -> dim b stone in
  group (square behind 1. :: wall_art b c) |> scale s |> move_x (s * float_of_int r)

(* The face along the corridor, of a wall standing to one side: a
 * trapezoid from the near edge of the cell to its far edge, narrowing
 * as it recedes. A cell straight ahead (r = 0) shows none: we see it
 * edge on. Darker than the front faces, which is Wolfenstein's cheap
 * lighting again, and Doom's "fake contrast": it makes the corners of
 * a corridor read at a glance. *)
let side_face (g : game) (screen : screen) (r : int) (d : int) : shape list =
  if r = 0 then []
  else
    let edge = float_of_int r + (if r > 0 then -0.5 else 0.5) in
    List.map
      (fun (z1, z2) ->
        let s1 = at screen z1 and s2 = at screen z2 in
        quad
          (dim (light g ((z1 + z2) / 2.) * 0.72) stone)
          (s1 * edge, -.(s1 / 2.))
          (s1 * edge, s1 / 2.)
          (s2 * edge, s2 / 2.)
          (s2 * edge, -.(s2 / 2.)))
      (steps d)

(* The floor and the ceiling of a depth come out as two bands right
 * across the screen -- everything at one distance is at one height on
 * the screen -- which is Mode 7's whole idea (see TinyKart), arrived at
 * from the other end. *)
let bands (g : game) (screen : screen) (d : int) : shape list =
  List.concat_map
    (fun (z1, z2) ->
      let y1 = at screen z1 / 2. and y2 = at screen z2 / 2. in
      let b = light g ((z1 + z2) / 2.) in
      (* a pixel of overlap: else the slices leave hairlines between them *)
      let h = y1 - y2 + 1. in
      [ rectangle (dim b floor_grey) screen.width h |> move_y (-.((y1 + y2) / 2.));
        rectangle (dim b ceiling_grey) screen.width h |> move_y ((y1 + y2) / 2.) ])
    (steps d)

(*****************************************************************************)
(* View: what stands in the open cells *)
(*****************************************************************************)

(* Drawn one cell tall, like the faces, and scaled to the slot: a
 * "billboard", as flat as Wolfenstein's, except that here there is
 * nothing to size it by but which slot it stands in. *)
let thing (b : number) (c : char) : shape =
  let gold = (210, 180, 70) in
  match c with
  | 'k' ->
      group
        [ circle (dim b gold) 0.05 |> move_y 0.05; rectangle (dim b gold) 0.022 0.13;
          rectangle (dim b gold) 0.05 0.022 |> move 0.02 (-0.05) ]
  | 'f' -> oval (dim b (172, 122, 66)) 0.2 0.1 |> move_y 0.04
  | 't' ->
      group
        [ rectangle (dim b (110, 80, 50)) 0.035 0.22;
          oval (dim (b + 0.5) (250, 170, 50)) 0.09 0.13 |> move_y 0.16 ]
  | '>' ->
      (* the stairs down, cut into the floor, each step darker *)
      group
        (List.init 4 (fun k ->
             let k = float_of_int k in
             rectangle (dim (b * (0.75 - (0.15 * k))) floor_grey) (0.8 - (0.12 * k)) 0.07 |> move_y (k * 0.08)))
  | _ -> group []

let monster_art (b : number) (hurt : bool) : shape =
  let cloak = if hurt then (190, 80, 70) else (64, 58, 72) in
  let eye = dim (b + 0.4) (if hurt then (255, 230, 120) else (230, 170, 40)) in
  group
    [ polygon (dim b cloak) [ (-0.26, 0.); (0.26, 0.); (0.15, 0.5); (-0.15, 0.5) ];
      circle (dim b cloak) 0.15 |> move_y 0.56;
      circle eye 0.028 |> move (-0.055) 0.58;
      circle eye 0.028 |> move 0.055 0.58 ]

let contents (g : game) (screen : screen) (r : int) (d : int) : shape list =
  let u = at screen (float_of_int d) in
  (* in the middle of the cell, standing on its floor *)
  let place shape = shape |> scale u |> move (u * float_of_int r) (-.(u / 2.)) in
  let b = light g (float_of_int d) in
  let cx, cy = cell_of g r d in
  (match Tilemap.get g.map cx cy with
  | Some (('k' | 'f' | 't' | '>') as c) -> [ place (thing b c) ]
  | _ -> [])
  @ List.map
      (fun (m : monster) -> place (monster_art b (m.hurt > 0)))
      (List.filter (fun (m : monster) -> (m.mx, m.my) = (cx, cy)) g.monsters)

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (c : color) (size : number) (s : string) : shape = words c s |> scale size

(* The frame: for each depth, farthest first, the floor and ceiling
 * bands, then the side faces (which recede, from z = d - 0.5 away),
 * then whatever stands in the open cells (in their middle, at z = d),
 * then the front faces (at z = d - 0.5, nearer than anything else of
 * their depth). That is the whole order, and it never changes: no
 * sorting, no depth per pixel, nothing to clip. *)
let view_dungeon (g : game) (screen : screen) : shape list =
  List.concat_map
    (fun d ->
      let solid, open_ = List.partition (fun r -> wall (tile_of g r d)) (across d) in
      bands g screen d
      @ List.concat_map (fun r -> side_face g screen r d) solid
      @ List.concat_map (fun r -> contents g screen r d) open_
      @ List.map (fun r -> front_face g screen r d) solid)
    depths

(* the sword, sweeping in from the right while [swing] runs down *)
let view_sword (g : game) (screen : screen) : shape list =
  if g.swing = 0 then []
  else
    let k = float_of_int g.swing / float_of_int swing_frames in
    let a = 40. - (70. * (1. - k)) in
    [ group
        [ rectangle (rgb 205 205 215) 26. 300. |> move_y 150.;
          rectangle (rgb 120 95 60) 70. 22.;
          rectangle (rgb 90 70 45) 24. 60. |> move_y (-35.) ]
      |> rotate a
      |> move (screen.right - 180.) (screen.bottom + 40.)
      |> fade 0.95 ]

let bar (screen : screen) (c : color) (label : string) (full : number) (y : number) : shape =
  let full = Float.max 0. (Float.min 1. full) in
  group
    [ rectangle (rgb 30 30 34) 204. 22.;
      rectangle c (200. * full) 18. |> move_x (-100. * (1. - full));
      text white 1.6 label |> move_x (-70.) ]
  |> move (screen.left + 130.) (screen.bottom + y)

let view_hud (g : game) (screen : screen) : shape list =
  [ bar screen (rgb 170 50 50) "HEALTH" (float_of_int g.hp / float_of_int max_hp) 34.;
    bar screen (rgb 210 150 50) "TORCH" (g.torch / full_torch) 66.;
    text (rgb 220 210 190) 3. (match g.facing with North -> "N" | East -> "E" | South -> "S" | West -> "W")
    |> move (screen.right - 60.) (screen.top - 50.) ]
  @ (if g.keys > 0 then [ thing 1. 'k' |> scale 260. |> move (screen.left + 60.) (screen.bottom + 110.) ] else [])
  @ (match g.said with _, 0 -> [] | what, _ -> [ text (rgb 230 220 190) 2. what |> move_y (screen.bottom + 130.) ])

let view_game (g : game) (screen : screen) : shape list =
  (* the dungeon is dark: whatever the torch does not reach stays black *)
  (rectangle black screen.width screen.height :: view_dungeon g screen)
  @ view_sword g screen
  @ view_hud g screen

let ending (screen : screen) (s : model) (c : color) (title : string) (line : string) : shape list =
  [ rectangle black screen.width screen.height;
    text c 6. title |> move_y 80.;
    text (rgb 220 210 190) 2.2 line |> move_y (-20.) ]
  @ Scene2d.blink 1. s [ text yellow 2.5 "PRESS SPACE" |> move_y (-140.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      (* the dungeon behind the panel, but no hero's gear on it yet *)
      (rectangle black screen.width screen.height :: view_dungeon (new_game ()) screen)
      @ [ rectangle black 860. 300. |> fade 0.88 |> move_y 40.;
          text (rgb 200 190 160) 5.5 "TINY DUNGEON MASTER" |> move_y 150.;
          text white 2. "up/down walk   left/right turn   a/d sidestep" |> move_y 85.;
          text white 2. "space: strike, pull, unlock" |> move_y 50.;
          text (rgb 180 175 160) 1.8 "the key, the door, the lever, the stairs -- before the torch dies" |> move_y 10. ]
      @ Scene2d.blink 1. s [ text yellow 2.5 "PRESS SPACE" |> move_y (-50.) ]
  | Playing g -> view_game g screen
  | Died frames ->
      ending screen s (rgb 190 60 50) "YOU DIED"
        (Printf.sprintf "%d seconds in the dark" (frames /.. 60))
  | Escaped left ->
      ending screen s (rgb 220 190 90) "THE STAIRS DOWN"
        (Printf.sprintf "%d seconds of torch to spare" left)

let app = game view update initial_model
let main = Playground_platform.run_app app
