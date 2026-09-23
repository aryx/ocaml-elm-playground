(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Metal Gear Solid (Hideo Kojima, Konami, 1998), after
 * Metal Gear (1987): get Snake across an enemy base to the elevator
 * without being seen.
 *
 *   arrows   move
 *   x        knock on the wall you stand against: the guards who hear
 *            it come to look
 *   b        get into the cardboard box, or out of it
 *   space    choke a guard, from behind
 *
 * Metal Gear (MSX2, 1987) was an action game its young director turned
 * inside out because the hardware could not show many enemies and many
 * bullets: instead of fighting them, avoid them. Metal Gear Solid, on
 * the PlayStation eleven years later, made the idea a genre -- stealth
 * -- and a blockbuster: a camera high above Snake, guards who see, hear
 * and search, and a radar in the corner of the screen that is the game's
 * real window. (Names and dates from memory, to check.)
 *
 * What stealth is made of, and where each is written here:
 *
 *   - Seeing ([sees]): a cone in front of each guard, as long as
 *     [sight] and [half_angle] wide on each side, stopped by the walls
 *     (the line to Snake is walked a few pixels at a time). Seen, you
 *     are found; the whole game is the shape of those cones.
 *   - The radar ([radar]): the Soliton radar draws the base from above,
 *     every guard, and every cone -- the cones are drawn only there,
 *     never in the world, so that the player's eye lives in the corner
 *     of the screen, as it did in 1998. And in an alert it is jammed:
 *     when you most need it, you lose it.
 *   - The guards' minds ([machine]): a state machine, written as data
 *     with Fsm (so the rules can be read in one place, in order of
 *     priority, and the time in a state is counted for us):
 *
 *                 heard a knock           nothing there, 7 s
 *       Patrol -------------------> Suspicious ------------> Patrol
 *         |  \   saw him, or             | saw him             ^
 *         |   \  the radio said so       v                     | 10 s
 *         |    '------------------->  Alert ----------> Evasion
 *         | choked from behind              lost him 2.5 s
 *         v
 *       Asleep --- 15 s ---> Patrol
 *
 *     Suspicious is a "?", Alert a "!" -- the two sounds of the game.
 *     Alert is shared: one guard sees Snake and all of them, by radio,
 *     run to where he was ([last_seen]); the phase in the corner says
 *     ALERT, then EVASION while they search, then INFILTRATION again.
 *   - Walking ([route]): every guard walks the base with A*
 *     (Pathfind) on the tiles, to the next point of its patrol, to
 *     the noise, to where Snake was last seen, to the places around it
 *     it searches -- one way of moving for every state.
 *   - The player's tools, each a lie told to the guards: the knock (a
 *     noise where Snake is not going to be), the box (a box that does
 *     not move is not seen; one that moves makes a guard wonder), the
 *     choke (a guard asleep sees nothing, but wakes up).
 *
 * The count of alerts is the score, and the ending gives a rank by it,
 * as Metal Gear Solid did (its best, Big Boss, asked for no alert).
 *
 * What it uses: Fsm (the guards' states), Pathfind (their
 * walking), gamekits/platformer's Tile_move (Snake against the walls,
 * seen from above, as TinyZelda moves Link), Tilemap (the base, and
 * its picture shown only where the camera looks), Camera2d (a base
 * bigger than the screen, followed), Scene2d. Not Physics.
 *
 * Left undone, exercises: the guards' footprints in the snow, and
 * Snake's (a trail, as the snow of Shadow Moses keeps it); crawling
 * under things; the keycards and their doors; a gun, and the noise it
 * makes; guards that call in by radio, and the alert when one doesn't
 * answer; the boss fights -- Psycho Mantis reading the memory card.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The base *)
(*****************************************************************************)

let tile = 40.

(* '#' walls, 'C' crates, 'S' Snake's start, 'E' the elevator *)
let base =
  Tilemap.of_strings tile
    [ "####################################";
      "#S.......#.........................#";
      "#........#.........................#";
      "#........#....CC.......CC..........#";
      "#........#....CC.......CC..........#";
      "#..................................#";
      "#........#.........................#";
      "#........#....CC.......CC..........#";
      "#........#....CC.......CC..........#";
      "#####.####.........................#";
      "#..................................#";
      "#..................................#";
      "#########.###########..#############";
      "#...................#..#...........#";
      "#...CC......CC......#..#...........#";
      "#...CC......CC......#..#...........#";
      "#...................#..............#";
      "#...................#..#...........#";
      "##########.##########..#####.#######";
      "#..................................#";
      "#..................................#";
      "#.....CC.......CC.......CC.........#";
      "#.....CC.......CC.......CC.........#";
      "#..............................E...#";
      "#..................................#";
      "####################################" ]

(* the patrols, cells walked to in turn *)
let patrols =
  [ [| (11, 5); (33, 5) |];
    [| (2, 13); (18, 13); (18, 17); (2, 17) |];
    [| (21, 10); (21, 20) |];
    [| (25, 14); (33, 14) |];
    [| (3, 20); (33, 20) |] ]

let wall (c : char) : bool = c = '#' || c = 'C'
let free ((c, r) : int * int) : bool = match Tilemap.get base c r with Some ch -> not (wall ch) | None -> false

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type cell = int * int
type mode = Patrol | Suspicious | Alert | Evasion | Asleep

type guard = {
  gx : number;
  gy : number;
  facing : number; (* degrees *)
  run : mode Fsm.run;
  patrol : cell array;
  leg : int; (* the patrol's point walked to *)
  goal : cell;
  path : cell list; (* the cells still to walk to reach [goal] *)
  noise : cell; (* the last noise heard *)
}

type snake = { x : number; y : number; dx : number; dy : number; box : bool; moving : bool }

type game = {
  snake : snake;
  guards : guard list;
  last_seen : cell; (* where Snake was last seen: where an alert goes *)
  since_seen : int; (* and how many frames ago, by any guard *)
  alerts : int;
  frames : int;
}

type scene = Title | Sneaking of game | Caught of game | Complete of game
type model = scene Scene2d.t

let size = (24., 24.)
let center ((c, r) : cell) : number * number = Tilemap.center base c r
let cell_of (x : number) (y : number) : cell = Tilemap.cell base x y

let start () : game =
  let x, y = match Tilemap.find base 'S' with s :: _ -> center s | [] -> (0., 0.) in
  let guard (patrol : cell array) =
    let gx, gy = center patrol.(0) in
    { gx; gy; facing = 0.; run = Fsm.start Patrol; patrol; leg = 0; goal = patrol.(0); path = []; noise = patrol.(0) }
  in
  { snake = { x; y; dx = 1.; dy = 0.; box = false; moving = false }; guards = List.map guard patrols;
    last_seen = cell_of x y; since_seen = 999; alerts = 0; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Seeing *)
(*****************************************************************************)

let sight = 240.
let half_angle = 35.

(* how far a knock on a wall carries: further than a guard sees *)
let hearing = 360.

(* the angle from a to b, and between two angles, in degrees *)
let angle_to (ax : number) (ay : number) (bx : number) (by : number) : number = Float.atan2 (by - ay) (bx - ax) * 180. / Float.pi

let angle_between (a : number) (b : number) : number = Float.abs (Float.rem (a - b + 540.) 360. - 180.)

(* a line of sight: no wall on the way, looked at every 8 pixels *)
let clear (ax : number) (ay : number) (bx : number) (by : number) : bool =
  let n = int_of_float (Float.hypot (bx - ax) (by - ay) / 8.) in
  List.for_all
    (fun i ->
      let t = float_of_int i / float_of_int (max n 1) in
      match Tilemap.tile_at base (ax + ((bx - ax) * t)) (ay + ((by - ay) * t)) with Some c -> not (wall c) | None -> true)
    (List.init (n +.. 1) Fun.id)

(* Snake in the guard's cone: near enough, in front, nothing between;
 * and not a box that stays still. Asleep, a guard sees nothing. *)
let sees (g : guard) (s : snake) : bool =
  g.run.state <> Asleep
  && Float.hypot (s.x - g.gx) (s.y - g.gy) < sight
  && angle_between g.facing (angle_to g.gx g.gy s.x s.y) < half_angle
  && clear g.gx g.gy s.x s.y

(*****************************************************************************)
(* The guards' minds *)
(*****************************************************************************)

(* what a guard knows this frame, which is all its rules look at *)
type sense = {
  seen : bool; (* Snake, in its cone (not in a box, or a box moving) *)
  heard : bool; (* a knock, or a box moving *)
  radio : bool; (* another guard sees Snake *)
  choked : bool;
  since_seen : int; (* frames since any guard saw Snake *)
}

let rule (from : mode) (label : string) (guard : sense -> int -> bool) (target : mode) : (mode, sense) Fsm.rule =
  { from; label; guard; target }

(* the rules, out of each state in order of priority *)
let machine : (mode, sense) Fsm.machine =
  let choked s _ = s.choked and saw s _ = s.seen || s.radio and heard s _ = s.heard in
  [ rule Patrol "choked" choked Asleep;
    rule Patrol "!" saw Alert;
    rule Patrol "?" heard Suspicious;
    rule Suspicious "choked" choked Asleep;
    rule Suspicious "!" saw Alert;
    rule Suspicious "? again" heard Suspicious;
    rule Suspicious "nothing there" (Fsm.after 420) Patrol;
    rule Alert "lost him" (fun s _ -> s.since_seen > 150) Evasion;
    rule Evasion "choked" choked Asleep;
    rule Evasion "!" saw Alert;
    rule Evasion "give up" (Fsm.after 600) Patrol;
    rule Asleep "wakes up" (Fsm.after 900) Patrol ]

(* where a guard in each state goes: its patrol, the noise, Snake as last
 * seen, and, searching, the places around there in turn *)
let goal_of (g : guard) (last_seen : cell) : cell =
  match g.run.state with
  | Patrol -> g.patrol.(g.leg)
  | Suspicious -> g.noise
  | Alert -> last_seen
  | Evasion ->
      let c, r = last_seen in
      let around = List.filter free [ (c +.. 3, r); (c, r +.. 3); (c -.. 3, r); (c, r -.. 3) ] in
      if around = [] then last_seen else List.nth around (g.run.since /.. 150 mod List.length around)
  | Asleep -> cell_of g.gx g.gy

(* the way there, A* on the free tiles, four directions *)
let route (from : cell) (goal : cell) : cell list =
  let problem : cell Pathfind.problem =
    { neighbors = (fun (c, r) -> List.filter free [ (c +.. 1, r); (c -.. 1, r); (c, r +.. 1); (c, r -.. 1) ] |> List.map (fun n -> (n, 1.)));
      goal = (fun n -> n = goal);
      estimate = Pathfind.manhattan goal }
  in
  match (Pathfind.astar problem from).path with _ :: rest -> rest | [] -> []

let speed (m : mode) : number = match m with Alert -> 3.2 | Evasion -> 2.2 | Asleep -> 0. | _ -> 1.5

(* one step along the path, facing where it goes; with nowhere to go, a
 * guard looks around *)
let walk (g : guard) : guard =
  match g.path with
  | [] -> if g.run.state = Asleep then g else { g with facing = g.facing + 1.5 }
  | next :: rest ->
      let tx, ty = center next in
      let d = Float.hypot (tx - g.gx) (ty - g.gy) and v = speed g.run.state in
      let facing = if d > 0. then angle_to g.gx g.gy tx ty else g.facing in
      if d <= v then { g with gx = tx; gy = ty; path = rest; facing } else { g with gx = g.gx + ((tx - g.gx) / d * v); gy = g.gy + ((ty - g.gy) / d * v); facing }

(* a guard's frame: what it senses, the state it goes to, where it
 * then goes, a step there *)
let think (s : snake) (noise : (number * number) option) (radio : bool) (choke : bool) (last_seen : cell)
    (since_seen : int) (g : guard) : guard =
  let in_cone = sees g s in
  (* a box is not Snake; but a box that moves is worth a look *)
  let seen = in_cone && not s.box in
  let heard_at =
    match noise with
    | Some (nx, ny) when Float.hypot (nx - g.gx) (ny - g.gy) < hearing -> Some (cell_of nx ny)
    | _ -> if in_cone && s.box && s.moving then Some (cell_of s.x s.y) else None
  in
  let choked =
    choke && (not s.box)
    && Float.hypot (s.x - g.gx) (s.y - g.gy) < 36.
    && angle_between g.facing (angle_to g.gx g.gy s.x s.y) > 100.
  in
  let sense = { seen; heard = heard_at <> None; radio = radio && g.run.state <> Asleep; choked; since_seen } in
  let g = { g with run = Fsm.step machine sense g.run } in
  let g = match heard_at with Some n -> { g with noise = n } | None -> g in
  let g = if g.run.state = Asleep then { g with path = [] } else g in
  (* a patrol point reached: the next one *)
  let here = cell_of g.gx g.gy in
  let g = if g.run.state = Patrol && here = g.goal && g.path = [] then { g with leg = (g.leg +.. 1) mod Array.length g.patrol } else g in
  let goal = goal_of g last_seen in
  let g = if goal <> g.goal || (g.path = [] && here <> goal) then { g with goal; path = route here goal } else g in
  walk g

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* what the player does this frame, which is all [step] needs: the game
 * reads it off the keyboard, the tests make it up *)
type input = { dx : number; dy : number; box : bool; knock : bool; choke : bool }

let nothing = { dx = 0.; dy = 0.; box = false; knock = false; choke = false }

(* against a wall: a wall tile right next to Snake, in one of the four
 * directions *)
let against_wall (s : snake) : bool =
  List.exists
    (fun (dx, dy) -> match Tilemap.tile_at base (s.x + (dx * 24.)) (s.y + (dy * 24.)) with Some c -> wall c | None -> false)
    [ (1., 0.); (-1., 0.); (0., 1.); (0., -1.) ]

let move_snake (i : input) (s : snake) : snake =
  let box = if i.box then not s.box else s.box in
  let v = if box then 1.2 else 2.6 in
  let (x, y), _ = Tile_move.move_by wall base size (s.x, s.y) (i.dx * v, 0.) in
  let (x, y), _ = Tile_move.move_by wall base size (x, y) (0., i.dy * v) in
  let moving = i.dx <> 0. || i.dy <> 0. in
  { x; y; box; moving; dx = (if moving then i.dx else s.dx); dy = (if moving then i.dy else s.dy) }

let alert (g : game) : bool = List.exists (fun gd -> gd.run.state = Alert) g.guards
let evasion (g : game) : bool = List.exists (fun gd -> gd.run.state = Evasion) g.guards

let step (i : input) (g : game) : game =
  let s = move_snake i g.snake in
  let noise = if i.knock && (not s.box) && against_wall s then Some (s.x, s.y) else None in
  let radio = List.exists (fun gd -> sees gd s && not s.box) g.guards in
  let last_seen = if radio then cell_of s.x s.y else g.last_seen in
  let since_seen = if radio then 0 else g.since_seen +.. 1 in
  let guards = List.map (think s noise radio i.choke last_seen since_seen) g.guards in
  let g' = { snake = s; guards; last_seen; since_seen; alerts = g.alerts; frames = g.frames +.. 1 } in
  { g' with alerts = (if alert g' && not (alert g) then g.alerts +.. 1 else g.alerts) }

(* a guard on alert close enough to grab him *)
let caught (g : game) : bool =
  List.exists (fun gd -> gd.run.state = Alert && Float.hypot (gd.gx - g.snake.x) (gd.gy - g.snake.y) < 28.) g.guards

let escaped (g : game) : bool = Tile_move.hits (fun c -> c = 'E') base size g.snake.x g.snake.y

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  let key k = Set_.mem k in
  match scenes.scene with
  | Title | Caught _ | Complete _ -> if pressed (fun k -> k.kspace) then Scene2d.go (Sneaking (start ())) scenes else scenes
  | Sneaking g ->
      let k = computer.keyboard in
      let g =
        step
          { dx = to_x k; dy = to_y k; box = pressed (fun k -> key "b" k.keys); knock = pressed (fun k -> key "x" k.keys);
            choke = pressed (fun k -> k.kspace) }
          g
      in
      if caught g then Scene2d.go (Caught g) scenes
      else if escaped g then Scene2d.go (Complete g) scenes
      else { scenes with scene = Sneaking g }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let floor_color = rgb 70 78 72
let wall_color = rgb 40 46 44
let green = rgb 90 230 120
let dark = rgb 10 20 14

let tile_shape (c : char) : shape =
  match c with
  | '#' -> square wall_color tile
  | 'C' -> group [ square (rgb 110 100 70) tile; square (rgb 140 128 90) (tile - 10.) ]
  | 'E' -> group [ square (rgb 200 200 90) tile; square floor_color (tile - 12.); words dark "EV" ]
  | _ -> group []

let snake_shape (s : snake) : shape =
  if s.box then group [ square (rgb 160 120 70) 30.; rectangle (rgb 120 90 50) 30. 3. ] |> move s.x s.y
  else
    let a = angle_to 0. 0. s.dx s.dy in
    group
      [ circle (rgb 60 70 90) 11.; circle (rgb 210 170 130) 6. |> move_x 3.;
        rectangle (rgb 30 30 30) 12. 2. |> move (-10.) 3. (* the bandana's tails *) ]
    |> rotate a |> move s.x s.y

let guard_shape (g : guard) : shape =
  let body =
    if g.run.state = Asleep then group [ oval (rgb 150 150 130) 30. 16.; text white 1.2 "zz" |> move 14. 16. ]
    else group [ circle (rgb 150 150 130) 11.; circle (rgb 90 90 80) 5. |> move_x 7. ] |> rotate g.facing
  in
  let mark =
    match g.run.state with
    | Suspicious -> [ text (rgb 250 220 60) 2.2 "?" |> move_y 26. ]
    | Alert -> [ text (rgb 250 60 40) 2.2 "!" |> move_y 26. ]
    | _ -> []
  in
  group (body :: mark) |> move g.gx g.gy

(* the radar: the base at an eighth of its size, the walls merged into
 * runs along each row (drawn once, not every frame) *)
let radar_scale = 1. / 8.

let radar_walls : shape =
  let bounds = Tilemap.bounds base in
  let rows = Tilemap.to_strings base in
  List.concat
    (List.mapi
       (fun r row ->
         let runs = ref [] and from = ref (-1) in
         String.iteri
           (fun c ch ->
             if wall ch && !from < 0 then from := c
             else if (not (wall ch)) && !from >= 0 then (runs := (!from, c -.. 1) :: !runs; from := -1))
           (row ^ ".");
         List.map
           (fun (c0, c1) ->
             let w = float_of_int (c1 -.. c0 +.. 1) * tile in
             rectangle (rgb 40 120 60) (w * radar_scale) (tile * radar_scale)
             |> move
                  ((bounds.left + (float_of_int c0 * tile) + (w / 2.)) * radar_scale)
                  ((bounds.top - (float_of_int r * tile) - (tile / 2.)) * radar_scale))
           !runs)
       rows)
  |> group

let cone (g : guard) : shape =
  let at a = (Float.cos (a * Float.pi / 180.) * sight, Float.sin (a * Float.pi / 180.) * sight) in
  polygon (rgb 60 200 230) [ (0., 0.); at (g.facing - half_angle); at g.facing; at (g.facing + half_angle) ]
  |> fade 0.45 |> move g.gx g.gy

let radar (g : game) : shape =
  let w = Tilemap.bounds base in
  let bw = (w.right - w.left) * radar_scale and bh = (w.top - w.bottom) * radar_scale in
  let inside =
    if alert g || evasion g then
      (* jammed: static, and the word *)
      List.init 40 (fun i ->
          let h = ((i *.. 7919) +.. (g.frames *.. 104729)) mod 9973 in
          rectangle (rgb 40 120 60) 6. 2. |> move (float_of_int (h mod 170) - 85.) (float_of_int (h /.. 170 mod 120) - 60.))
      @ [ text green 1.6 (if alert g then "JAMMING" else "EVASION") ]
    else
      [ radar_walls;
        group (List.map (fun gd -> group [ cone gd; circle white 20. |> move gd.gx gd.gy ]) (List.filter (fun gd -> gd.run.state <> Asleep) g.guards))
        |> scale radar_scale;
        circle green 3. |> move (g.snake.x * radar_scale) (g.snake.y * radar_scale) ]
  in
  group (rectangle dark (bw + 12.) (bh + 12.) :: rectangle (rgb 20 50 30) bw bh :: inside)

let camera (computer : computer) (g : game) : Camera2d.t =
  { Camera2d.origin with zoom = 1.3 } |> Camera2d.look_at g.snake.x g.snake.y |> Camera2d.clamp computer.screen (Tilemap.bounds base)

let view_game (computer : computer) (g : game) : shape list =
  let screen = computer.screen in
  let cam = camera computer g in
  let phase = if alert g then "ALERT" else if evasion g then "EVASION" else "INFILTRATION" in
  [ rectangle floor_color screen.width screen.height;
    Camera2d.view cam
      ([ Tilemap.view_visible (Camera2d.visible screen cam) tile_shape base ] @ List.map guard_shape g.guards @ [ snake_shape g.snake ]);
    radar g |> move (screen.right - 110.) (screen.top - 85.);
    text (if alert g then rgb 250 60 40 else green) 2. phase |> move (screen.left + 120.) (screen.top - 30.);
    text green 1.5 (Printf.sprintf "alerts %d" g.alerts) |> move (screen.left + 120.) (screen.top - 60.);
    text green 1.5 "arrows move   x knock on a wall   b the box   space choke, from behind" |> move_y (screen.bottom + 25.) ]

(* Metal Gear Solid's ranks, the best only with no alert *)
let rank (alerts : int) : string =
  match alerts with 0 -> "BIG BOSS" | 1 -> "FOX" | 2 | 3 -> "HOUND" | _ -> "CHICKEN"

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let black = rectangle dark screen.width screen.height in
  match model.scene with
  | Title ->
      [ black; text green 5. "TINY METAL GEAR SOLID" |> move_y 200.;
        text green 2. "get to the elevator without being seen" |> move_y 80.;
        text green 2. "watch the radar: the guards' eyes are drawn only there" |> move_y 40.;
        text green 2. "x knocks on a wall    b the cardboard box    space chokes, from behind" |> move_y 0. ]
      @ Scene2d.blink 1. model [ text green 3. "PRESS SPACE" |> move_y (-200.) ]
  | Sneaking g -> view_game computer g
  | Caught g ->
      view_game computer g
      @ [ rectangle dark screen.width 200. |> fade 0.8; text (rgb 250 60 40) 3.5 "SNAKE? SNAKE!? SNAAAKE!" |> move_y 30.;
          text green 2. "CONTINUE? press space" |> move_y (-40.) ]
  | Complete g ->
      [ black; text green 4. "MISSION COMPLETE" |> move_y 150.;
        text green 2.5 (Printf.sprintf "%d alerts, %.0f seconds" g.alerts (float_of_int g.frames / 60.)) |> move_y 60.;
        text green 3. ("CODENAME: " ^ rank g.alerts) |> move_y (-20.) ]
      @ Scene2d.blink 1. model [ text green 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinyMetalGearSolid
  arrows   move
  x        knock on a wall: the guards who hear it come to look
  b        the cardboard box, on and off (still, it is not seen)
  space    choke a guard, from behind
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
