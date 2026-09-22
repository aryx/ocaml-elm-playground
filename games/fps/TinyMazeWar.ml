(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Maze War (Steve Colley, Greg Thompson and Howard
 * Palmer, NASA Ames Research Center, 1973-74): eyeballs hunting each
 * other in a maze, seen from inside it.
 *
 *   up down      a step forward, a step back
 *   left right  a quarter turn
 *   space        shoot down the corridor
 *
 * Steve Colley wrote a maze you could walk through in the first person
 * on the Imlacs of NASA Ames, a summer job's side project; Greg
 * Thompson and Howard Palmer made it a game of several players, first
 * on two machines joined by a cable, then over the ARPANET, then at MIT
 * with robots to play against. It is the first first-person game, the
 * first first-person shooter, and one of the first networked games --
 * twenty years before Doom, which is also in the catalogue. The players
 * were eyeballs, because an eye was the one thing that showed which way
 * it was looking. (Names and dates from memory, to check.)
 *
 * The view is the trick of this game, and the same one as
 * TinyDungeonMaster's, fifteen years earlier and in lines only: you
 * stand in the middle of a cell, facing one of four ways, so every cell
 * you can see has one fixed place on the screen. The corridor ahead is
 * a series of frames, one per cell boundary, shrinking with distance
 * ([frame]); a cell with a wall on its left draws the wall's two
 * edges from one frame's corners to the next ([corridor]), a cell with
 * an opening draws the far wall of the side corridor instead, and the
 * corridor ends with a whole frame, the wall facing you:
 *
 *     +---------------------------+   frame 0: the edges of the view
 *     |\                         /|
 *     | +----+-------------+----+ |   frame 1, one cell further;
 *     | |    |\           /|    | |   the left wall of cell 2 is open,
 *     | |    | +---------+ |    | |   its side corridor's far wall
 *     | |    | |  wall   | |    | |   drawn between frames 1 and 2
 *     | |    | +---------+ |    | |
 *     | +----+/           \+----+ |
 *     |/                         \|
 *     +---------------------------+
 *
 * The frames are a division by the distance, the one of every
 * perspective: a boundary at distance z is (half the view) * 0.5 / z
 * from the middle.
 *
 * The network: every player -- you, the robots -- gives the game a
 * command each tick ([command]: a step, a turn, a shot, or nothing),
 * and [tick] applies them all, in the players' order, to a model that
 * is only the maze and the players. That is the shape networked games
 * need to be in (lockstep: each machine gets everyone's commands and
 * computes the same world, see plan_networking_teaching.md): there is
 * no networking in the playground yet, so the other players are
 * robots on this machine, and a remote player would be one more list
 * of commands. The robots ([robot]) do what Maze War's did: shoot
 * anyone down the corridor they face, turn towards anyone they can see
 * down another, and otherwise wander.
 *
 * What it uses: Scene2d. Not gamekits/maze: its movers glide from tile
 * to tile, and a Maze War player jumps a whole cell at a time. Not
 * Tilemap: the maze is strings, read only by [wall].
 *
 * Left undone, exercises: the network, when plan_networking_teaching.md
 * gets there (a player per machine, the commands exchanged each tick);
 * peeking round a corner (Maze War had it); the eyeballs in the side
 * corridors, not only straight ahead; the overhead map showing the
 * others, as a cheat; a bigger maze, and a maze editor.
 *)
open Playground

(*****************************************************************************)
(* The maze *)
(*****************************************************************************)

let maze =
  [| "################";
     "#......#.......#";
     "#.####.#.#####.#";
     "#.#......#...#.#";
     "#.#.####.#.#.#.#";
     "#...#......#...#";
     "###.#.####.#.###";
     "#.....#......#.#";
     "#.###.#.####.#.#";
     "#.#...#....#...#";
     "#.#.#####.##.#.#";
     "#...........#..#";
     "################" |]

let wall (c : int) (r : int) : bool = r < 0 || r >= Array.length maze || c < 0 || c >= String.length maze.(r) || maze.(r).[c] = '#'

(* the four ways, in turning order: north (up the map), east, south, west *)
let delta (dir : int) : int * int = match dir land 3 with 0 -> (0, -1) | 1 -> (1, 0) | 2 -> (0, 1) | _ -> (-1, 0)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type eye = {
  name : string;
  c : int;
  r : int;
  dir : int;
  dead : int; (* frames before coming back, 0 when alive *)
  reload : int; (* frames before the next shot *)
  kills : int;
  deaths : int;
}

type game = { eyes : eye array; (* you are the first *) rng : int; frames : int; news : string }

let eye name c r dir = { name; c; r; dir; dead = 0; reload = 0; kills = 0; deaths = 0 }

let start () : game =
  { eyes = [| eye "you" 1 1 1; eye "robot 1" 14 11 3; eye "robot 2" 14 1 2; eye "robot 3" 1 11 0 |]; rng = 7; frames = 0;
    news = "" }

let next (rng : int) : int = ((rng * 1103515245) + 12345) land 0x3fffffff

(*****************************************************************************)
(* The rules: a command per player per tick *)
(*****************************************************************************)

type command = Forward | Back | Turn_left | Turn_right | Fire | Nothing

(* who is first down the corridor from (c, r) facing [dir], before a
 * wall, if anyone, and how far *)
let down_the_corridor (g : game) (c : int) (r : int) (dir : int) : (int * int) option =
  let dc, dr = delta dir in
  let rec look c r d =
    let c, r = (c + dc, r + dr) in
    if wall c r then None
    else
      let here = List.filter (fun k -> g.eyes.(k).dead = 0 && g.eyes.(k).c = c && g.eyes.(k).r = r) (List.init (Array.length g.eyes) Fun.id) in
      match here with k :: _ -> Some (k, d) | [] -> look c r (d + 1)
  in
  look c r 1

let free_cell (g : game) (c : int) (r : int) : bool =
  (not (wall c r)) && not (Array.exists (fun e -> e.dead = 0 && e.c = c && e.r = r) g.eyes)

(* where the dead come back: the free cell the rng picks, far enough
 * from everyone *)
let respawn (g : game) (k : int) : game =
  let cells = List.concat (List.init (Array.length maze) (fun r -> List.init (String.length maze.(r)) (fun c -> (c, r)))) in
  let far (c, r) = Array.for_all (fun e -> e.dead > 0 || abs (e.c - c) + abs (e.r - r) >= 5) g.eyes in
  let ok = List.filter (fun (c, r) -> free_cell g c r && far (c, r)) cells in
  let ok = if ok = [] then List.filter (fun (c, r) -> free_cell g c r) cells else ok in
  let c, r = List.nth ok ((g.rng lsr 4) mod List.length ok) in
  let eyes = Array.copy g.eyes in
  eyes.(k) <- { (eyes.(k)) with c; r; dead = 0; dir = g.rng land 3 };
  { g with eyes; rng = next g.rng }

let apply (g : game) (k : int) (cmd : command) : game =
  let e = g.eyes.(k) in
  if e.dead > 0 then g
  else
    let set e' = let eyes = Array.copy g.eyes in eyes.(k) <- e'; { g with eyes } in
    let step d =
      let dc, dr = delta d in
      if free_cell g (e.c + dc) (e.r + dr) then set { e with c = e.c + dc; r = e.r + dr } else g
    in
    match cmd with
    | Forward -> step e.dir
    | Back -> step (e.dir + 2)
    | Turn_left -> set { e with dir = (e.dir + 3) land 3 }
    | Turn_right -> set { e with dir = (e.dir + 1) land 3 }
    | Nothing -> g
    | Fire when e.reload > 0 -> g
    | Fire -> (
        let g = set { e with reload = 30 } in
        match down_the_corridor g e.c e.r e.dir with
        | None -> g
        | Some (v, _) ->
            let eyes = Array.copy g.eyes in
            eyes.(k) <- { (eyes.(k)) with kills = eyes.(k).kills + 1 };
            eyes.(v) <- { (eyes.(v)) with dead = 90; deaths = eyes.(v).deaths + 1 };
            { g with eyes; news = Printf.sprintf "%s shot %s" e.name eyes.(v).name })

(* One tick: every player's command, in the players' order; then the
 * clocks, and the dead brought back. *)
let tick (commands : command array) (g : game) : game =
  let g = Array.fold_left (fun g k -> apply g k commands.(k)) g (Array.init (Array.length g.eyes) Fun.id) in
  let g =
    { g with eyes = Array.map (fun e -> { e with reload = max 0 (e.reload - 1); dead = (if e.dead > 1 then e.dead - 1 else e.dead) }) g.eyes;
             frames = g.frames + 1 }
  in
  Array.fold_left (fun g k -> if g.eyes.(k).dead = 1 then respawn g k else g) g (Array.init (Array.length g.eyes) Fun.id)

(* A robot, a move every [robot_pace] frames: anyone down its corridor
 * is shot; anyone down another is turned to; else on, turning at walls
 * (and now and then at a junction, to wander). *)
let robot_pace = 20

let robot (g : game) (k : int) : command =
  let e = g.eyes.(k) in
  if e.dead > 0 || (g.frames + (k * 7)) mod robot_pace <> 0 then Nothing
  else
    let sees d = down_the_corridor g e.c e.r d <> None in
    if sees e.dir then Fire
    else if sees (e.dir + 1) || sees (e.dir + 2) then Turn_right
    else if sees (e.dir + 3) then Turn_left
    else
      let dc, dr = delta e.dir in
      let roll = (g.rng lsr (k + 3)) mod 4 in
      if wall (e.c + dc) (e.r + dr) then if roll < 2 then Turn_left else Turn_right
      else if roll = 0 then (let lc, lr = delta (e.dir + 3) in if wall (e.c + lc) (e.r + lr) then Forward else Turn_left)
      else Forward

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type model = game Scene2d.t

let initial_model : model = Scene2d.start (start ())

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  let g = scenes.scene in
  let you =
    if pressed (fun k -> k.kup) then Forward
    else if pressed (fun k -> k.kdown) then Back
    else if pressed (fun k -> k.kleft) then Turn_left
    else if pressed (fun k -> k.kright) then Turn_right
    else if pressed (fun k -> k.kspace) then Fire
    else Nothing
  in
  let commands = Array.init (Array.length g.eyes) (fun k -> if k = 0 then you else robot g k) in
  { scenes with scene = { (tick commands g) with rng = next g.rng } }

(*****************************************************************************)
(* View: the corridor, in lines *)
(*****************************************************************************)

let ink = rgb 230 230 230
let box_half = 300. (* the view, 600 x 600 *)

(* a line, as the Imlac's vector display drew them *)
let line (x1 : float) (y1 : float) (x2 : float) (y2 : float) : shape =
  let len = Float.hypot (x2 -. x1) (y2 -. y1) in
  rectangle ink (len +. 2.) 2. |> rotate (Float.atan2 (y2 -. y1) (x2 -. x1) *. 180. /. Float.pi) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* the trick of this game, in 49 lines (see the header): the boundary between cells
 * [d] and [d + 1] ahead is a square, this far from the middle *)
let frame (d : int) : float = box_half *. 0.5 /. (float_of_int d +. 0.5)

let square (h : float) : shape list = [ line (-.h) h h h; line (-.h) (-.h) h (-.h); line (-.h) (-.h) (-.h) h; line h (-.h) h h ]

(* cell [d] ahead (1 the next one), between frames d - 1 and d: its
 * left and right, a wall's two edges or a side corridor's far wall;
 * each cell's lines put before the nearer ones', so drawn first *)
let corridor (g : game) : shape list =
  let you = g.eyes.(0) in
  let fc, fr = delta you.dir and lc, lr = delta (you.dir + 3) and rc, rr = delta (you.dir + 1) in
  let rec cells d acc =
    let c = you.c + (fc * d) and r = you.r + (fr * d) in
    let near = frame (d - 1) and far = frame d in
    if d > 12 then acc
    else if wall c r then square near @ acc (* the wall facing you *)
    else
      let side (sc, sr) sign =
        if wall (c + sc) (r + sr) then
          (* the wall's top and bottom edges, receding *)
          [ line (sign *. near) near (sign *. far) far; line (sign *. near) (-.near) (sign *. far) (-.far) ]
        else
          (* the opening: the corner where the wall ended, full height,
           * and the side corridor's far wall, at the far frame's *)
          [ line (sign *. near) far (sign *. far) far; line (sign *. near) (-.far) (sign *. far) (-.far);
            line (sign *. near) near (sign *. near) (-.near); line (sign *. far) far (sign *. far) (-.far) ]
      in
      (* someone in this cell: an eyeball, its pupil showing where it looks *)
      let eyeball =
        match List.find_opt (fun e -> e.dead = 0 && e.c = c && e.r = r) (Array.to_list g.eyes) with
        | None -> []
        | Some e ->
            let mid = (near +. far) /. 2. in
            let size = mid *. 0.45 in
            let rel = (e.dir - you.dir + 4) land 3 in
            let pupil =
              match rel with
              | 2 -> [ circle black (size *. 0.4) ] (* looking at you *)
              | 1 -> [ circle black (size *. 0.3) |> move_x (size *. 0.6) ]
              | 3 -> [ circle black (size *. 0.3) |> move_x (-.(size *. 0.6)) ]
              | _ -> [] (* looking away: its back *)
            in
            [ group ([ circle ink size ] @ pupil) |> move_y (-.(mid *. 0.2)) ]
      in
      cells (d + 1) (side (lc, lr) (-1.) @ side (rc, rr) 1. @ eyeball @ acc)
  in
  cells 1 []

(* the overhead map: the maze, and you as an arrow *)
let overhead (g : game) : shape =
  let cell = 14. in
  let w = float_of_int (String.length maze.(0)) and h = float_of_int (Array.length maze) in
  let at c r = ((float_of_int c -. (w /. 2.) +. 0.5) *. cell, ((h /. 2.) -. float_of_int r -. 0.5) *. cell) in
  let walls =
    List.concat
      (List.init (Array.length maze) (fun r ->
           List.filter_map
             (fun c -> if wall c r then let x, y = at c r in Some (square (cell /. 2.) |> group |> move x y |> fade 0.5) else None)
             (List.init (String.length maze.(r)) Fun.id)))
  in
  let you = g.eyes.(0) in
  let x, y = at you.c you.r in
  group (walls @ [ triangle (rgb 120 250 120) 6. |> rotate (float_of_int (-you.dir * 90)) |> move x y ])

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let g = model.scene in
  let you = g.eyes.(0) in
  let board =
    Array.to_list g.eyes
    |> List.mapi (fun i e ->
           words ink (Printf.sprintf "%-8s %2d kills %2d deaths%s" e.name e.kills e.deaths (if e.dead > 0 then "  (dead)" else ""))
           |> scale 1.3 |> move (screen.right -. 170.) (220. -. (float_of_int i *. 26.)))
  in
  [ rectangle black screen.width screen.height ]
  @ [ group (square box_half @ if you.dead > 0 then [ words ink "you were shot -- wait" |> scale 2. ] else corridor g) |> move_x (-.150.) ]
  @ [ overhead g |> move (screen.right -. 170.) (-.150.) ]
  @ board
  @ [ words ink g.news |> scale 1.6 |> move_y (screen.top -. 40.);
      words (rgb 150 150 150) "up down step   left right turn   space shoot" |> scale 1.4 |> move_y (screen.bottom +. 30.) ]

let help = {|TinyMazeWar
  up down a step, left right a quarter turn, space shoots down the corridor
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
