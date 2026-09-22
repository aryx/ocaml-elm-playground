(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Mario World (Takashi Tezuka and Shigeru
 * Miyamoto, Nintendo, 1990), the Super Nintendo's launch game: Cape
 * Mario on hills, in the sky, and on a map of paths between the
 * courses.
 *
 *   left right   walk; with x held, run
 *   space        jump (held: higher; held falling: the cape floats)
 *   down         crouch -- on a slope, slide
 *   x            run; run long enough and the P meter fills: jump, and
 *                you fly
 *   in the air   flying, hold back to climb, forward to dive
 *   the map      arrows walk the open paths, space enters a course
 *
 * TinyMario is Super Mario Bros.: a box against square tiles, the
 * ground always flat. Five years later the ground had slopes, and the
 * game had three new ideas worth a toy of their own:
 *
 *  - Slopes, and the slide ([ground_step]). The ground is shaped tiles,
 *    felt by sensors under the feet (gamekits/platformer's Slope, the
 *    one TinySonic runs round its loop with -- this is its second
 *    user, on floors only). Mario's speed stays a plain x speed, as in
 *    the original; the slope only caps it going up ([uphill]). Crouch
 *    on a slope and he slides instead: no control, gravity along the
 *    surface pulling him down it, and every enemy in his way knocked
 *    out. The same slope is a hill to walk and a ramp to slide.
 *
 *  - The cape, and flight as trading ([soar]). Running at full speed
 *    fills the P meter; jump then and Mario takes off, rises, and
 *    soars. In the air there is no engine, only a trade: hold back and
 *    the nose comes up, speed turned into height; hold forward and he
 *    dives, height turned into speed; let go and he glides, sinking
 *    slowly. A pilot's energy, not a platformer's jump:
 *
 *          climb (back)                 dive (forward)
 *             ___                      \
 *            /    speed -> height       \   height -> speed
 *           /                            \___
 *
 *    Pump the two -- dive, pull up, dive -- and a good pilot stays up;
 *    here each pull gives back a little less height than the dive
 *    before it took, so every flight lands in the end.
 *
 *  - The world map ([paths]): the courses are the nodes of a graph and
 *    the paths its edges, each opened by finishing a course. Some
 *    courses have two exits: the goal tape, and a keyhole somewhere
 *    harder (here, on an island in the sky that only flight reaches),
 *    which opens a different path -- the Star Road, a shortcut past a
 *    course to the castle. The level select is the game's structure,
 *    and the secrets are its edges.
 *
 * What it uses: gamekits/platformer's Slope (the ground), Camera2d, Scene2d,
 * Audio. Not Tile_move (Slope's sensors replace the box against
 * blocks), not Tilemap (the tiles are Slope's surfaces, built once per
 * course), not Physics: Mario's speeds are Super Mario World's own
 * handful of numbers, below.
 *
 * Exercises: Yoshi; the shells, carried and thrown; the spin jump; the
 * ground pound at the end of a dive (every enemy on screen knocked
 * out); the steeper slopes Mario slides down by himself; a ceiling
 * inside the slopes; saving the map; a second world.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The knobs *)
(*****************************************************************************)
(* In pixels per frame at 60 frames a second, 16 pixels a tile, after
 * Super Mario World's own (its subpixels are sixteenths): walking tops
 * out at 1.25, running at 2.25, and the P meter's full sprint at 3. *)
let walk_max = 1.25
let run_max = 2.25
let sprint_max = 3.
let accel = 0.09375
let friction = 0.0625
let skid = 0.25
let jump_speed = 5. (* and a quarter of the running speed on top *)
let gravity = 0.3125
let gravity_held = 0.1875 (* jump held on the way up: a higher jump *)
let max_fall = 4.
let float_fall = 1.5 (* the cape, jump held while falling *)
let p_fill = 1. / 60. (* a second at full run fills the P meter *)
let slide_pull = 0.125 (* gravity along a slope, sliding *)
let slide_friction = 0.08 (* a slide stops some five tiles past the slope's foot *)

(* the flight *)
let takeoff = 3.5 (* how fast he rises, jump held *)
let rise_frames = 60
(* height gained per unit of speed, climbing: at most 0.45 x 4.5 x
 * (1 / 0.06) = 33.75 pixels for each unit of speed spent, against the
 * dive's 3 / 0.08 = 37.5 pixels to win it back -- so a dive and a
 * climb always end lower, and every flight lands *)
let climb_rate = 0.45
let climb_cost = 0.06 (* speed spent per frame, climbing *)
let dive_gain = 0.08 (* speed gained per frame, diving *)
let dive_sink = 3.
let glide_sink = 0.6
let stall = 1.5 (* slower than this, climbing gives nothing *)
let dive_max = 4.5

let tile = 16
let radius = 12. (* the feet are this far under Mario's middle *)
let half_width = 6.
let zoom = 3.

(*****************************************************************************)
(* The courses *)
(*****************************************************************************)
(* A row per line, a tile per character, as TinySonic types its level:
 *   '#' ground, '/' and '\' 45 degrees, 'a' 'b' a gentle rise over two
 *   tiles, 'c' 'd' the same down, 'o' a coin, 'e' a galoomba, 'S' the
 *   start, 'F' the goal tape, 'K' the keyhole (the secret exit). *)
(* Donut Hills: the hills, a long slope down into three galoombas, a
 * runway, and the keyhole on an island in the sky *)
let donut_level =
  [ "                                                                                               ";
    "                                                                                               ";
    "                                                                                               ";
    "                                                                                               ";
    "                                                                                               ";
    "                                                                                               ";
    "                                                                         o   o o               ";
    "                                                                           K                   ";
    "                                                                        #########              ";
    "                                                                                               ";
    "                                                                                               ";
    "                                                                                               ";
    "                             o                                                                 ";
    "             e                                                                                 ";
    "      oo  /#####\\         ab###\\                                                               ";
    "         /#######\\      ab######\\                     o o o o o o o o o o                      ";
    "  S     /#########\\   ab#########\\                                                             ";
    "##################################\\                                                            ";
    "###################################\\  e e e                                              F     ";
    "###############################################   ##########################    ###############" ]

(* Cloud Gap: two gaps no jump crosses, and a runway before the first *)
let cloud_level =
  [ "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                                                         ";
    "                                                 o                                       ";
    "                                                                                         ";
    "        o  o  o  o  o  o  o  o                                               e     F     ";
    "                                                                         /###############";
    "  S                                                                 e   /################";
    "################################              ######            #########################";
    "################################              ######            #########################";
    "################################              ######            #########################" ]

(* the Star Road: the secret path's own course, rolling and golden *)
let star_level =
  [ "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "                                                      ";
    "          o     o     o     o     o     o             ";
    "            o     o     o     o     o     o           ";
    "                                                      ";
    "  S      abcd  abcd  abcd  abcd  abcd  abcd     F     ";
    "######################################################";
    "######################################################" ]

(* the castle: hills to slide down, galoombas at their feet *)
let castle_level =
  [ "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "                                                                ";
    "            e                                                   ";
    "          /###\\                                                 ";
    "         /#####\\                 /##\\                           ";
    "        /#######\\               /####\\                          ";
    "  S    /#########\\ e e         /######\\ e  e e            F     ";
    "########################   ####################    #############";
    "########################   ####################    #############" ]

type course_id = Donut | Cloud | Star | Castle

let course_name = function Donut -> "DONUT HILLS" | Cloud -> "CLOUD GAP" | Star -> "STAR ROAD" | Castle -> "THE CASTLE"

(* A course, read once: its rows, and the surface of every tile built in
 * advance (a sensor asks for thousands of tiles a frame; building one
 * is 256 booleans -- TinySonic's lesson) *)
type course = {
  lines : string array;
  cols : int;
  rows : int;
  surfaces : Slope.surface option array;
}

let surface_of (c : char) : Slope.surface option =
  match c with
  | '#' -> Some (Slope.block tile)
  | '/' -> Some (Slope.slope tile ~from_:0 ~to_:tile)
  | '\\' -> Some (Slope.slope tile ~from_:tile ~to_:0)
  | 'a' -> Some (Slope.slope tile ~from_:0 ~to_:(tile /.. 2))
  | 'b' -> Some (Slope.slope tile ~from_:(tile /.. 2) ~to_:tile)
  | 'c' -> Some (Slope.slope tile ~from_:tile ~to_:(tile /.. 2))
  | 'd' -> Some (Slope.slope tile ~from_:(tile /.. 2) ~to_:0)
  | _ -> None

let read (level : string list) : course =
  let lines = Array.of_list level in
  let cols = String.length lines.(0) and rows = Array.length lines in
  (* the course is written top to bottom, the world counts y upwards *)
  let char_at tx ty = lines.(rows -.. 1 -.. ty).[tx] in
  { lines; cols; rows; surfaces = Array.init (cols *.. rows) (fun i -> surface_of (char_at (i mod cols) (i /.. cols))) }

let courses = [ (Donut, read donut_level); (Cloud, read cloud_level); (Star, read star_level); (Castle, read castle_level) ]
let course (id : course_id) : course = List.assoc id courses

let char_at (c : course) ((tx, ty) : int * int) : char =
  if tx < 0 || tx >= c.cols || ty < 0 || ty >= c.rows then ' ' else c.lines.(c.rows -.. 1 -.. ty).[tx]

let tiles (c : course) ((tx, ty) : int * int) : Slope.surface option =
  if tx < 0 || tx >= c.cols || ty < 0 || ty >= c.rows then None else c.surfaces.((ty *.. c.cols) +.. tx)

let tile_at (x : number) (y : number) : int * int =
  (int_of_float (Float.floor (x / float_of_int tile)), int_of_float (Float.floor (y / float_of_int tile)))

(* where a character is, as the middle of its tile *)
let things (c : course) (ch : char) : (number * number) list =
  List.concat_map
    (fun ty ->
      List.filter_map
        (fun tx ->
          if char_at c (tx, ty) = ch then
            Some ((float_of_int tx + 0.5) * float_of_int tile, (float_of_int ty + 0.5) * float_of_int tile)
          else None)
        (List.init c.cols Fun.id))
    (List.init c.rows Fun.id)

(*****************************************************************************)
(* Mario *)
(*****************************************************************************)

type flight = Walking | Rising of int (* frames of rise left *) | Soaring

type mario = {
  x : number;
  y : number; (* his middle, [radius] above his feet *)
  vx : number;
  vy : number;
  grounded : bool;
  angle : number; (* of the ground under him: 45 up a hill to the right *)
  facing : number;
  sliding : bool;
  p : number; (* the P meter, 0 to 1 *)
  flight : flight;
}

(* The hands, apart from the keyboard, so the tests can play *)
type hands = { left : bool; right : bool; down : bool; run : bool; jump : bool (* this frame *); holding : bool }

let no_hands = { left = false; right = false; down = false; run = false; jump = false; holding = false }

let sin_deg (a : number) : number = sin (a * pi / 180.)

(* two sensors under the feet, a little apart; the higher ground wins *)
let feet (c : course) (m : mario) : (number * number) option =
  let sensor dx = Slope.ground ~tiles:(tiles c) ~size:tile ~reach:(2 *.. tile) Slope.Floor (m.x + dx, m.y - radius) in
  match (sensor (-5.), sensor 5.) with
  | None, None -> None
  | Some a, None | None, Some a -> Some a
  | Some (ya, aa), Some (yb, ab) -> if ya >= yb then Some (ya, aa) else Some (yb, ab)

(* a block beside him, at the height of his middle or his head: a wall *)
let wall (c : course) (m : mario) (dir : number) : bool =
  List.exists (fun dy -> char_at c (tile_at (m.x + (dir * half_width)) (m.y + dy)) = '#') [ 0.; radius - 2. ]

let toward (target : number) (step : number) (v : number) : number =
  if v < target then Float.min target (v + step) else Float.max target (v - step)

(* Going up a slope, the top speed shrinks with its steepness *)
let uphill (m : mario) (cap : number) : number =
  if m.vx * sin_deg m.angle > 0. then cap * (1. - (0.4 * Float.abs (sin_deg m.angle))) else cap

let top_speed (h : hands) (m : mario) : number =
  if h.run && m.p >= 1. then sprint_max else if h.run then run_max else walk_max

(* One frame on the ground: walking, or sliding, then the jump or the
 * takeoff, then the step along x and the feet set on the surface. *)
let ground_step (c : course) (h : hands) (m : mario) : mario =
  let dir = (if h.right then 1. else 0.) - if h.left then 1. else 0. in
  let m = if h.down && Float.abs m.angle > 1. && not m.sliding then { m with sliding = true } else m in
  let m =
    if m.sliding then
      (* no control: gravity along the slope, friction on the flat *)
      let vx = if Float.abs m.angle > 1. then m.vx - (slide_pull * sin_deg m.angle) else toward 0. slide_friction m.vx in
      { m with vx; sliding = Float.abs m.angle > 1. || Float.abs vx > 0.1 }
    else
      let cap = uphill m (top_speed h m) in
      let vx =
        if dir = 0. || h.down then toward 0. friction m.vx
        else if m.vx * dir < 0. then m.vx + (dir * skid)
        else if Float.abs m.vx > cap then toward (cap * dir) friction m.vx
        else toward (cap * dir) accel m.vx
      in
      { m with vx; facing = (if dir <> 0. then dir else m.facing) }
  in
  (* the P meter: a second at full run fills it; slower, it drains *)
  let p = if h.run && Float.abs m.vx >= run_max - 0.01 then Float.min 1. (m.p + p_fill) else Float.max 0. (m.p - (2. * p_fill)) in
  let m = { m with p } in
  if h.jump then
    if m.p >= 1. then { m with grounded = false; sliding = false; flight = Rising rise_frames; vy = takeoff }
    else { m with grounded = false; sliding = false; vy = jump_speed + (Float.abs m.vx / 4.) }
  else
    let moved = { m with x = m.x + m.vx } in
    let m = if wall c moved (if m.vx > 0. then 1. else -1.) then { m with vx = 0. } else moved in
    match feet c m with
    | Some (ground, angle) -> { m with y = ground + radius; angle }
    | None -> { m with grounded = false; vy = 0.; angle = 0. }

(* The cape in the air, once flying: back to climb, forward to dive,
 * nothing to glide -- speed and height traded, never made *)
let soar (h : hands) (m : mario) : mario =
  let back = (m.facing > 0. && h.left) || (m.facing < 0. && h.right) in
  let forward = (m.facing > 0. && h.right) || (m.facing < 0. && h.left) in
  let speed = Float.abs m.vx in
  let speed, vy =
    if back then if speed > stall then (speed - climb_cost, speed * climb_rate) else (speed, m.vy - gravity)
    else if forward then (Float.min dive_max (speed + dive_gain), -.dive_sink)
    else (speed, -.glide_sink)
  in
  { m with vx = speed * m.facing; vy = Float.max (-.max_fall) vy }

let air_step (c : course) (h : hands) (m : mario) : mario =
  let m =
    match m.flight with
    | Rising n when h.holding && n > 0 -> { m with vx = sprint_max * m.facing; vy = takeoff; flight = Rising (n -.. 1) }
    | Rising _ -> soar h { m with flight = Soaring }
    | Soaring -> soar h m
    | Walking ->
        let dir = (if h.right then 1. else 0.) - if h.left then 1. else 0. in
        let vx = if dir = 0. then m.vx else toward (top_speed h m * dir) accel m.vx in
        let g = if h.holding && m.vy > 0. then gravity_held else gravity in
        let fall = if h.holding && m.vy < 0. then float_fall else max_fall in
        { m with vx; vy = Float.max (-.fall) (m.vy - g); facing = (if dir <> 0. then dir else m.facing) }
  in
  let moved = { m with x = m.x + m.vx } in
  let m = if wall c moved (if m.vx > 0. then 1. else -1.) then { m with vx = 0. } else moved in
  let m = { m with y = m.y + m.vy } in
  (* a block over his head stops the rise *)
  let m =
    if m.vy > 0. && char_at c (tile_at m.x (m.y + radius)) = '#' then
      { m with vy = 0.; flight = (match m.flight with Rising _ -> Soaring | f -> f) }
    else m
  in
  if m.vy > 0. then m
  else
    match feet c m with
    | Some (ground, angle) when m.y - radius <= ground + 2. ->
        { m with y = ground + radius; vy = 0.; angle; grounded = true; flight = Walking; sliding = h.down && Float.abs angle > 1. }
    | _ -> m

let step_mario (c : course) (h : hands) (m : mario) : mario = if m.grounded then ground_step c h m else air_step c h m

(*****************************************************************************)
(* A course played *)
(*****************************************************************************)

type exit = Normal | Secret

type galoomba = { gx : number; gy : number; gdir : number }

type play = {
  id : course_id;
  mario : mario;
  galoombas : galoomba list;
  coins : (number * number) list;
  taken : int;
  camera : Camera2d.t;
  ended : exit option; (* the tape or the keyhole reached *)
  dead : bool;
  frames : int;
}

let start_play (id : course_id) : play =
  let c = course id in
  let x, y = List.hd (things c 'S') in
  { id;
    mario =
      { x; y = y - 8. + radius; vx = 0.; vy = 0.; grounded = false; angle = 0.; facing = 1.; sliding = false; p = 0.;
        flight = Walking };
    galoombas = List.map (fun (gx, gy) -> { gx; gy; gdir = -1. }) (things c 'e');
    coins = things c 'o';
    taken = 0;
    camera = { (Camera2d.look_at x y Camera2d.origin) with zoom };
    ended = None;
    dead = false;
    frames = 0 }

let near ((x1, y1) : number * number) ((x2, y2) : number * number) (r : number) : bool = Float.hypot (x1 - x2) (y1 - y2) < r

(* a galoomba walks, follows the ground's shape, and turns at a wall or
 * an edge *)
let walk (c : course) (g : galoomba) : galoomba =
  let gx = g.gx + (0.5 * g.gdir) in
  let ahead = Slope.ground ~tiles:(tiles c) ~size:tile ~reach:tile Slope.Floor (gx + (6. * g.gdir), g.gy - 8.) in
  let blocked = char_at c (tile_at (gx + (8. * g.gdir)) g.gy) = '#' in
  match ahead with
  | Some (ground, _) when not blocked -> { g with gx; gy = ground + 8. }
  | _ -> { g with gdir = -.g.gdir }

(* the frame of a course: Mario, the galoombas, what he touches *)
let play_step (screen : screen) (h : hands) (p : play) : play =
  let c = course p.id in
  let m = step_mario c h p.mario in
  let galoombas = List.map (walk c) p.galoombas in
  (* sliding or diving he knocks them out; falling onto one, he stomps
   * it and bounces; any other touch is the end *)
  let touching = List.filter (fun g -> near (m.x, m.y) (g.gx, g.gy) (radius + 6.)) galoombas in
  let beats g = m.sliding || (m.flight = Soaring && m.vy < -2.) || (m.vy < 0. && m.y > g.gy + 4.) in
  let beaten, hurt = List.partition beats touching in
  let m = if beaten <> [] && not m.sliding && m.flight = Walking then { m with vy = (if h.holding then 5. else 3.5); grounded = false } else m in
  if beaten <> [] then Audio.play Audio.hit;
  let galoombas = List.filter (fun g -> not (List.memq g beaten)) galoombas in
  let taken_now, coins = List.partition (fun q -> near (m.x, m.y) q 14.) p.coins in
  if taken_now <> [] then Audio.play Audio.coin;
  let ended =
    if List.exists (fun (kx, ky) -> near (m.x, m.y) (kx, ky) 14.) (things c 'K') then Some Secret
    else if List.exists (fun (fx, fy) -> Float.abs (m.x - fx) < 8. && m.y - fy < 64.) (things c 'F') then Some Normal
    else None
  in
  let camera =
    p.camera
    |> Camera2d.window 32. 48. (m.x + (24. * m.facing)) m.y
    |> Camera2d.clamp screen { left = 0.; right = float_of_int (c.cols *.. tile); bottom = 0.; top = float_of_int (c.rows *.. tile) }
  in
  { p with mario = m; galoombas; coins; taken = p.taken +.. List.length taken_now; camera; ended; dead = hurt <> [] || m.y < -32.; frames = p.frames +.. 1 }

(*****************************************************************************)
(* The world map *)
(*****************************************************************************)

type node = Home | Course of course_id

(* where each node sits on the map *)
let place = function
  | Home -> (-360., -120.)
  | Course Donut -> (-160., -120.)
  | Course Cloud -> (60., -120.)
  | Course Star -> (-40., 110.)
  | Course Castle -> (280., -120.)

(* The paths, and what opens each: nothing for the first, and after that
 * an exit of the course they lead from. Donut Hills has two exits, so
 * two paths out, and the secret one skips Cloud Gap. *)
let paths : (node * node * (course_id * exit) option) list =
  [ (Home, Course Donut, None);
    (Course Donut, Course Cloud, Some (Donut, Normal));
    (Course Donut, Course Star, Some (Donut, Secret));
    (Course Cloud, Course Castle, Some (Cloud, Normal));
    (Course Star, Course Castle, Some (Star, Normal)) ]

type progress = { found : (course_id * exit) list; at : node; news : string }

let opened (pr : progress) = List.filter (fun (_, _, key) -> match key with None -> true | Some k -> List.mem k pr.found) paths

(* the node an arrow leads to from [at], along an open path: the one
 * whose direction is closest to the arrow's *)
let walk_map (pr : progress) ((dx, dy) : number * number) : progress =
  let ends = List.filter_map (fun (a, b, _) -> if a = pr.at then Some b else if b = pr.at then Some a else None) (opened pr) in
  let ax, ay = place pr.at in
  let score n = let bx, by = place n in let d = Float.hypot (bx - ax) (by - ay) in (((bx - ax) * dx) + ((by - ay) * dy)) / d in
  match List.filter (fun n -> score n > 0.5) ends with
  | [] -> pr
  | n :: rest -> { pr with at = List.fold_left (fun b n -> if score n > score b then n else b) n rest; news = "" }

let finish (pr : progress) (id : course_id) (e : exit) : progress =
  let fresh = not (List.mem (id, e) pr.found) in
  { pr with
    found = (if fresh then pr.found @ [ (id, e) ] else pr.found);
    news =
      (match e with
      | Secret -> if fresh then "SECRET EXIT! a new path opens" else "the secret exit, again"
      | Normal -> "COURSE CLEAR!") }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type scene = Title | Map of progress | Playing of progress * play | The_end of progress
type model = scene Scene2d.t

let initial_model : model = Scene2d.start Title
let new_progress = { found = []; at = Home; news = "" }

let hands_of (computer : computer) (scenes : model) : hands =
  let k = computer.keyboard in
  { left = k.kleft; right = k.kright; down = k.kdown; run = Set_.mem "x" k.keys || k.kshift;
    jump = Scene2d.pressed (fun k -> k.kspace) scenes; holding = k.kspace }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let key f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title -> if key (fun k -> k.kspace) then Scene2d.go (Map new_progress) scenes else scenes
  | The_end _ -> if key (fun k -> k.kspace) && scenes.elapsed > 1. then Scene2d.go Title scenes else scenes
  | Map pr -> (
      let arrow =
        if key (fun k -> k.kleft) then Some (-1., 0.)
        else if key (fun k -> k.kright) then Some (1., 0.)
        else if key (fun k -> k.kup) then Some (0., 1.)
        else if key (fun k -> k.kdown) then Some (0., -1.)
        else None
      in
      match (arrow, pr.at) with
      | Some d, _ -> { scenes with scene = Map (walk_map pr d) }
      | None, Course id when key (fun k -> k.kspace) -> Scene2d.go (Playing (pr, start_play id)) scenes
      | None, _ -> scenes)
  | Playing (pr, p) -> (
      let p = play_step computer.screen (hands_of computer scenes) p in
      match p.ended with
      | Some e ->
          Audio.play Audio.coin;
          let pr = finish pr p.id e in
          if p.id = Castle then Scene2d.go (The_end pr) scenes else Scene2d.go (Map pr) scenes
      | None ->
          if p.dead then begin
            Audio.play Audio.explosion;
            Scene2d.go (Map { pr with news = "TOO BAD! try again" }) scenes
          end
          else { scenes with scene = Playing (pr, p) })

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let earth = rgb 170 110 60
let grass = rgb 80 190 70

(* the visible tiles: a block, or the slope's quadrilateral, grass on
 * top where nothing covers it *)
let view_tiles (c : course) (visible : Camera2d.rect) : shape list =
  let t = float_of_int tile in
  let from_x = max 0 (int_of_float (visible.left / t) -.. 1) and to_x = min (c.cols -.. 1) (int_of_float (visible.right / t) +.. 1) in
  let from_y = max 0 (int_of_float (visible.bottom / t) -.. 1) and to_y = min (c.rows -.. 1) (int_of_float (visible.top / t) +.. 1) in
  let edges = function
    | '#' -> Some (t, t) | '/' -> Some (0., t) | '\\' -> Some (t, 0.) | 'a' -> Some (0., t / 2.) | 'b' -> Some (t / 2., t)
    | 'c' -> Some (t, t / 2.) | 'd' -> Some (t / 2., 0.) | _ -> None
  in
  List.concat_map
    (fun tx ->
      List.concat_map
        (fun ty ->
          match edges (char_at c (tx, ty)) with
          | None -> []
          | Some (l, r) ->
              let x0 = float_of_int tx * t and y0 = float_of_int ty * t in
              let covered = edges (char_at c (tx, ty +.. 1)) <> None in
              polygon earth [ (x0, y0); (x0 + t, y0); (x0 + t, y0 + r); (x0, y0 + l) ]
              :: (if covered then []
                  else [ polygon grass [ (x0, y0 + l - 4.); (x0 + t, y0 + r - 4.); (x0 + t, y0 + r); (x0, y0 + l) ] ]))
        (List.init (max 0 (to_y -.. from_y +.. 1)) (fun i -> i +.. from_y)))
    (List.init (max 0 (to_x -.. from_x +.. 1)) (fun i -> i +.. from_x))

let view_mario (m : mario) : shape =
  let red = rgb 220 40 40 and blue = rgb 40 70 200 and skin = rgb 250 200 160 and cape = rgb 250 200 40 in
  let flying = m.flight <> Walking && not m.grounded in
  let body =
    if m.sliding then
      (* sitting on the slope, feet first *)
      group [ rectangle blue 14. 8. |> move 0. (-8.); circle skin 5. |> move (-4. * m.facing) (-1.); rectangle red 10. 4. |> move (-4. * m.facing) 4. ]
    else
      group
        [ rectangle blue 10. 12. |> move 0. (-5.);
          rectangle red 12. 6. |> move 0. 2.;
          circle skin 5. |> move (1. * m.facing) 8.;
          rectangle red 12. 3. |> move (1. * m.facing) 12. ]
  in
  let cape_shape =
    if flying then polygon cape [ (0., 6.); (-22. * m.facing, 10.); (-20. * m.facing, 0.) ]
    else polygon cape [ (-2. * m.facing, 6.); (-9. * m.facing, -8.); (-4. * m.facing, -10.) ]
  in
  let tilt = if flying then Float.max (-40.) (Float.min 40. (m.vy * 8.)) * m.facing else if m.grounded then m.angle else 0. in
  group [ cape_shape; body ] |> rotate tilt |> move m.x m.y

(* the P meter: six arrows, and the P when it's full *)
let view_p (p : number) : shape list =
  List.init 6 (fun k ->
      let lit = p * 6. > float_of_int k in
      triangle (if lit then rgb 250 250 250 else rgb 90 90 110) 9. |> rotate (-90.) |> move (-120. + (float_of_int k * 22.)) 440.)
  @ [ text (if p >= 1. then rgb 250 80 80 else rgb 90 90 110) 2.6 "P" |> move 20. 440. ]

let view_play (computer : computer) (p : play) : shape list =
  let c = course p.id in
  let visible = Camera2d.visible computer.screen p.camera in
  let world =
    view_tiles c visible
    @ List.map (fun (x, y) -> oval (rgb 250 210 60) 8. 12. |> move x y) p.coins
    @ List.map (fun (x, y) -> group [ circle black 5. |> move_y 2.; polygon black [ (-3., 0.); (3., 0.); (5., -8.); (-5., -8.) ] ] |> move x y) (things c 'K')
    @ List.concat_map
        (fun (x, y) ->
          let tape_y = y + 8. + (28. * (1. + sin (float_of_int p.frames / 30.))) in
          [ rectangle (rgb 60 60 60) 3. 72. |> move (x - 14.) (y + 28.); rectangle (rgb 60 60 60) 3. 72. |> move (x + 14.) (y + 28.);
            rectangle (rgb 250 120 40) 26. 4. |> move x tape_y ])
        (things c 'F')
    @ List.map
        (fun g -> group [ circle (rgb 150 80 40) 8.; circle white 2.5 |> move (3. * g.gdir) 2.; rectangle (rgb 60 40 20) 12. 3. |> move_y (-7.) ] |> move g.gx g.gy)
        p.galoombas
    @ [ view_mario p.mario ]
  in
  let far = { (Camera2d.parallax 0.25 p.camera) with y = 150. } in
  let hills =
    List.map (fun (x, r) -> circle (rgb 110 190 120) r |> move (x * 170.) (40. - (r / 2.))) [ (0.4, 50.); (1.5, 70.); (2.8, 44.); (4., 62.); (5.3, 48.) ]
  in
  [ rectangle (rgb 110 180 250) computer.screen.width computer.screen.height; Camera2d.view far hills; Camera2d.view p.camera world ]
  @ view_p p.mario.p
  @ [ text white 2.4 (course_name p.id) |> move (-380.) 440.;
      text (rgb 250 220 80) 2.4 (Printf.sprintf "coins %d" p.taken) |> move 330. 440.;
      text (rgb 40 50 90) 1.6 "left right walk   x run   space jump   down slide   run to fill P, then jump: fly" |> move_y 400. ]

let view_map (pr : progress) : shape list =
  let color = function
    | Home -> rgb 250 250 250
    | Course id ->
        let has_secret = List.exists (fun (_, _, k) -> k = Some (id, Secret)) paths in
        if List.mem (id, Normal) pr.found || List.mem (id, Secret) pr.found then rgb 60 110 230
        else if has_secret then rgb 230 60 60
        else rgb 250 210 60
  in
  let nodes = [ Home; Course Donut; Course Cloud; Course Star; Course Castle ] in
  let opened_ = opened pr in
  let visible n = n = Home || List.exists (fun (a, b, _) -> a = n || b = n) opened_ in
  let road (a, b, _) =
    let x1, y1 = place a and x2, y2 = place b in
    rectangle (rgb 230 200 150) (Float.hypot (x2 - x1) (y2 - y1)) 10.
    |> rotate (atan2 (y2 - y1) (x2 - x1) * 180. / pi)
    |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)
  in
  let label n = match n with Home -> "HOME" | Course id -> course_name id in
  [ rectangle (rgb 60 140 220) 1200. 1200.; oval (rgb 90 180 90) 900. 520.; oval (rgb 110 200 100) 700. 380. |> move_y 20. ]
  @ List.map road opened_
  @ List.concat_map
      (fun n ->
        if not (visible n) then []
        else
          let x, y = place n in
          [ circle (rgb 60 50 40) 17. |> move x y; circle (color n) 13. |> move x y; text white 1.6 (label n) |> move x (y - 34.) ])
      nodes
  @ (let x, y = place pr.at in
     [ group [ circle (rgb 220 40 40) 9. |> move_y 14.; circle (rgb 250 200 160) 7. |> move_y 4.; rectangle (rgb 40 70 200) 12. 10. |> move_y (-6.) ] |> move x (y + 16.) ])
  @ [ text white 3. "THE WORLD MAP" |> move_y 400.; text (rgb 250 240 120) 3. pr.news |> move_y 320.;
      text white 1.8 "arrows walk the open paths   space enters a course" |> move_y (-440.) ]

let view (computer : computer) (model : model) : shape list =
  match model.scene with
  | Title ->
      [ rectangle (rgb 110 180 250) computer.screen.width computer.screen.height;
        text (rgb 220 40 40) 6.5 "TINY MARIO WORLD" |> move_y 150.;
        text white 2.2 "slopes to slide, a cape to fly, a map of secrets" |> move_y 60. ]
      @ Scene2d.blink 1. model [ text (rgb 250 240 120) 3. "PRESS SPACE" |> move_y (-150.) ]
  | Map pr -> view_map pr
  | Playing (_, p) -> view_play computer p
  | The_end pr ->
      [ rectangle (rgb 30 30 60) computer.screen.width computer.screen.height;
        text (rgb 250 210 60) 5. "THE CASTLE IS YOURS" |> move_y 80.;
        text white 2.2
          (if List.mem (Donut, Secret) pr.found then "by the Star Road: you found the secret" else "the long way: there was a keyhole in the sky")
        |> move_y 10. ]
      @ Scene2d.blink 1. model [ text (rgb 250 240 120) 3. "PRESS SPACE" |> move_y (-150.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
