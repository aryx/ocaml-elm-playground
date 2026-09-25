(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Mario Kart (Nintendo, 1992), in "Mode 7": a
 * kart race seen from just behind your kart, the track a flat floor
 * stretching to the horizon, turning around you as you steer. Up to
 * accelerate, down to brake, left/right to steer; three laps against
 * three computer karts; the grass slows you down. Press 2 on the title
 * for two players, the screen split in two as on the SNES: the second
 * player on w a s d. Press b for the battle (v: two players), the
 * SNES's other game: a square arena, three balloons each, shells and
 * bananas from the item boxes (space; e for the second player), the
 * last one with balloons winning -- its own section, [The battle].
 *
 * The SNES had no 3D at all. Its "Mode 7" drew one background, a map of
 * tiles, turned and scaled; the trick (F-Zero, 1990, then Pilotwings and
 * Super Mario Kart) was to change the scale on every line of the
 * screen, in the time the beam goes back to the left edge: small at the
 * top, big at the bottom, and the flat map looks like a floor. Here, the
 * same, a row at a time ([to_ground], [view_ground]):
 *
 *      the eye, from the side              one screen row, far below
 *                                          the horizon: the ground
 *     eye o---___ horizon                  near; just below it: far
 *         |\     ---___
 *  height | \          ---___   a row [below] pixels under the horizon
 *         |  \               sees the ground at the distance
 *     ----+---*-----------------  height * focal / below
 *            near     far
 *
 * and across the row, the ground at that distance, from its left to its
 * right: a straight line through the map, sampled at every pixel, which
 * is what the SNES computed ([to_ground]'s across). Here a "pixel" is 5x5
 * real ones: the ground is a 200x130 picture of characters, a tile's
 * color each, drawn by Sprite.pixels (each row merged in runs of the
 * same color, one rectangle per run). Far away, a pixel is wider than
 * the tiles' patterns: the SNES's floors shimmered there; here the
 * patterns fade to their average color, a mipmap ([ground]).
 *
 * Compare TinyWolfenstein.ml, the same idea turned sideways: the
 * raycaster casts a ray per column into the walls, Mode 7 a line per row
 * across the floor. Each draws what the other can't: Wolfenstein has no
 * floor texture, Mode 7 no walls. The karts are TinyWolfenstein's billboards:
 * flat pictures sized by their distance ([to_screen]), the farthest
 * drawn first, and one of four drawings by the angle you see them from
 * (the back, three-quarters, the side, the front), like the SNES's
 * sprites of every kart at every angle.
 *
 * Underneath, it's Micro Machines: the model is a top-down car on a
 * plane (the racing kit's Topdown, shared with TinyMicroMachines),
 * the track a Tilemap with its waypoints in its characters ('a', 'b',
 * ...), which give the laps, the places and the computer's driving. Only
 * the picture differs; the minimap shows the same race from above.
 *
 * Uses: the racing kit's Topdown, Tilemap, Sprite, Scene2d. Not Road and
 * Car (the pseudo-3D road of TinyOutRun: a track is a list of segments
 * there, a map here), not Camera2d (a camera turned and zoomed, the whole
 * map the same: Mode 7 without the per-row scale).
 *
 * The split screen ([view_split]) is the same Mode 7 drawn twice, each
 * time in a screen half as tall, with its own eye: a camera is only a
 * place, a heading and a rectangle of pixels to fill. What costs is
 * that the playground draws every shape whole and can't clip it to its
 * half (TinyXpilot.ml's problem too): the ground and the sky fill
 * their half exactly, the hills are cut at its top ([hill]), the karts
 * whose wheels are below its bottom are left out, the top half is drawn
 * last so that its sky and ground cover what the bottom one spills
 * upwards, and a strip covers the seam.
 *
 * The sounds (Sfx's recipes and the ready-made ones, no recording) and
 * the juice (a hit shaking the screen, a balloon bursting) are in their
 * own section; music=off and juice=off turn them off.
 *
 * Exercises: items in the race (the battle's shells and bananas),
 * coins, jumps (a kart's height, its sprite lifted), a camera looking
 * straight down (Camera2d, turned: Mode 7 with the same scale on every
 * row), the battle's other arenas and its red shells (which home in),
 * F-Zero's walls.
 *)
open Playground

(*****************************************************************************)
(* The track *)
(*****************************************************************************)

let tile = 100.

(* the grass '.', the road '#', the kerbs '*' (on the corners' outsides),
 * the start line '=' (and 'a'), the waypoints 'a', 'b', ... on the
 * road's middle, in the order they're driven *)
let map =
  Tilemap.of_strings tile
    [ "..................................";
      ".......*****..............*****...";
      "......*#########=##############*..";
      "......*#k#######a############b#*..";
      "......*#########=##############*..";
      "......*####*..............**###*..";
      ".......*###*...............*###*..";
      "...*****####*..............*###*..";
      "..*#########*....*****....**###*..";
      "..*#i#####j#*...*##############*..";
      "..*#########*...*#d##########c#*..";
      "..*###******....*##############*..";
      "..*###*.........*###**....*****...";
      "...###..........*###**....*****...";
      "...###..........*##############*..";
      "...###..........*#e##########f#*..";
      "...###..........*##############*..";
      "...###...........*****....**###*..";
      "...###.....................*###*..";
      "..*###*....................*###*..";
      "..*###**..................**###*..";
      "..*############################*..";
      "..*#h########################g#*..";
      "..*############################*..";
      "...*****..................*****...";
      ".................................." ]

(* the waypoints, 'a' then 'b', ..., until a letter isn't in the map;
 * passed within 200 (the road is 300 wide) *)
let track : Topdown.track =
  let rec letters (c : char) =
    match Tilemap.find map c with
    | [ (col, row) ] -> Tilemap.center map col row :: letters (Char.chr (Char.code c + 1))
    | _ -> []
  in
  { points = Array.of_list (letters 'a'); reach = 200.; corner = 350. }

let laps = 3

(* the fastest a kart can go where it is: slow on the grass, and off the
 * map *)
let top_speed (x : number) (y : number) : number =
  match Tilemap.tile_at map x y with Some '.' | None -> 250. | Some '*' -> 600. | Some _ -> 700.

(* The ground's picture, as a character per color (see [palette]): what
 * a sample of the ground at (x, y) shows, a sample being [unit] world
 * units wide there. Patterns in the tiles, not only their colors: the
 * grass mowed in squares (all around the map too), the road in faint
 * ones, the kerbs red and white; without them, a floor of plain colors
 * doesn't seem to move.
 *
 * But far away, a pattern's squares get smaller than a sample: the
 * samples fall on its colors at random, the far rows shimmer, and cut in
 * a run per sample they cost a rectangle each. So a pattern whose
 * squares are less than 2 samples wide is drawn in the average of its
 * two colors ('.' checkers of 'r' and 'w' become 'p', pink): mipmapping
 * (Lance Williams, "Pyramidal Parametrics", SIGGRAPH 1983), in its
 * simplest form, a texture and one smaller version of it; the SNES had
 * no such thing, and shimmered. *)
let ground (unit : number) (x : number) (y : number) : char =
  let checker size (c1 : char) (c2 : char) (average : char) : char =
    if size < 2. *. unit then average
    else if (int_of_float (floor (x /. size)) + int_of_float (floor (y /. size))) land 1 = 0 then c1
    else c2
  in
  match Tilemap.tile_at map x y with
  | None | Some '.' -> checker 200. 'g' 'G' 'h'
  | Some '*' -> checker 25. 'r' 'w' 'p'
  | Some ('=' | 'a') -> checker 25. 'b' 'w' 'm'
  | Some _ -> checker 50. 'd' 'D' 'e'

let palette : (char * color) list =
  [ ('g', rgb 70 160 60); ('G', rgb 60 145 55); ('h', rgb 65 152 57);
    ('r', rgb 210 40 40); ('w', rgb 240 240 240); ('p', rgb 225 140 140);
    ('b', rgb 20 20 20); ('m', rgb 130 130 130);
    ('d', rgb 120 120 125); ('D', rgb 110 110 115); ('e', rgb 115 115 120) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kart = { car : Topdown.t; top : number (* its top speed, whatever the ground *); color : color }

type race = {
  karts : kart list; (* the players' first *)
  humans : int; (* 1 or 2 players *)
  view_angles : number list; (* each player's camera heading: the kart's, a little late *)
  places : int option list; (* each player's place, once finished *)
  frames : int; (* since the start *)
  ready : int; (* > 0: the countdown *)
}

(* the battle's model: see its section *)
type item = Shell | Banana

type fighter = {
  kart : kart;
  balloons : int; (* 3 at the start; none: out *)
  item : item option; (* the one it carries *)
  spin : int; (* > 0: hit, spinning, for that long *)
}

type shell = { sx : number; sy : number; vx : number; vy : number; life : int; from : int (* its thrower *) }
type banana = { bx : number; by : number; dropped_by : int; age : int }

type mood = Collect | Attack

(* what a computer kart sees (all of it: the arena is in view) *)
type senses = {
  me : int;
  own : Topdown.t; (* its own car *)
  carrying : item option;
  rivals : (int * Topdown.t) list; (* the others with balloons *)
  boxes : (number * number) list; (* the boxes there to take *)
  mind : mood Fsm.run;
}

(* what it wants: a place to drive to, and whether to use its item *)
type order = { goal : (number * number) option; use : bool }

type battle = {
  fighters : fighter list; (* the players' first *)
  players : int;
  views : number list; (* each player's camera heading, a little late *)
  shells : shell list;
  bananas : banana list;
  boxes : ((number * number) * int) list; (* each box, and the frames before it is back (0: there) *)
  minds : (senses, order) Bot.running array;
  clock : int;
  countdown : int;
  over : int option; (* frames since it was decided *)
}

type scene = Title | Racing of race | Finished of race | Battle of battle | Battle_over of battle
type model = { scenes : scene Scene2d.t; fx : Juice.t (* the juice's, see its section *) }

(* the grid, two by two behind the start line, the players last, as in
 * Super Mario Kart's first race; the computer's karts can't go as fast
 * as the players' (full gas, the friction keeps a kart under 585, see
 * Topdown.drive; the computer's 0.9 gas under 526), else no one would
 * pass them *)
let new_race (humans : int) : race =
  let place slot (top, color) =
    let c = Topdown.start track 0 (if slot mod 2 = 0 then 60. else -60.) in
    let back = 60. +. (80. *. float_of_int slot) in
    let a = c.heading *. Float.pi /. 180. in
    { car = { c with x = c.x -. (back *. cos a); y = c.y -. (back *. sin a) }; top; color }
  in
  let second = if humans = 2 then (700., rgb 40 90 220) else (520., rgb 40 90 220) in
  let grid = [ (480., rgb 40 170 60); (500., rgb 240 200 30); second; (700., rgb 220 30 30) ] in
  let karts = List.mapi place grid in
  (* the players are the last ones on the grid *)
  let players = List.filteri (fun i _ -> i >= 4 - humans) karts |> List.rev in
  { karts = players @ List.filteri (fun i _ -> i < 4 - humans) karts; humans;
    view_angles = List.init humans (fun _ -> 0.); places = List.init humans (fun _ -> None); frames = 0; ready = 180 }

let initial_model : model = { scenes = Scene2d.start Title; fx = Juice.none ~seed:1 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

(* degrees from [a] to [b], the short way, between -180 and 180 *)
let angle_diff (a : number) (b : number) : number = Float.rem (Float.rem (b -. a +. 180.) 360. +. 360.) 360. -. 180.

(* karts closer than 36 pushed apart, half the overlap each: bumping,
 * not crashing *)
let bump (karts : kart list) : kart list =
  let push (k : kart) (o : kart) =
    let dx = k.car.x -. o.car.x and dy = k.car.y -. o.car.y in
    let d = Float.hypot dx dy in
    if d >= 36. || d = 0. || k == o then k
    else
      let p = (36. -. d) /. 2. /. d in
      { k with car = { k.car with x = k.car.x +. (dx *. p); y = k.car.y +. (dy *. p) } }
  in
  List.map (fun k -> List.fold_left push k karts) karts

(* a player's place, 1 for the first *)
let place (r : race) (i : int) : int =
  let p = Topdown.progress track (List.nth r.karts i).car in
  1 + List.length (List.filter (fun k -> Topdown.progress track k.car > p) r.karts)

(* the players' hands: the arrows, then w a s d *)
let hands (k : keyboard) (i : int) : number * number =
  if i = 0 then (axis k.kup k.kdown, axis k.kleft k.kright) else (axis k.kw k.ks, axis k.ka k.kd)

(* one frame of the race: a player who has finished is driven by the
 * computer, as in Super Mario Kart *)
let update_race (k : keyboard) (r : race) : race =
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else
    let drive i (kart : kart) =
      let gas, steer =
        if i < r.humans && List.nth r.places i = None then hands k i else Topdown.computer track kart.car
      in
      let top = Float.min kart.top (top_speed kart.car.x kart.car.y) in
      { kart with car = Topdown.drive Topdown.toy top gas steer kart.car |> Topdown.follow track }
    in
    let karts = bump (List.mapi drive r.karts) in
    (* each camera turns after its kart, a fifth of the way a frame: the
     * kart seems to turn in front of you, and a drift shows *)
    let view_angles =
      List.mapi (fun i a -> a +. (0.2 *. angle_diff a (List.nth karts i).car.heading)) r.view_angles
    in
    let r = { r with karts; view_angles; frames = r.frames + 1 } in
    { r with
      places =
        List.mapi
          (fun i p -> match p with None when Topdown.lap track (List.nth karts i).car >= laps -> Some (place r i) | p -> p)
          r.places }

(*****************************************************************************)
(* The battle *)
(*****************************************************************************)
(* claude: Super Mario Kart's other game: a square arena, three balloons
 * each, item boxes; a green shell fired ahead, bouncing off the walls,
 * or a banana dropped behind; hit, a kart spins and loses a balloon,
 * and the last one with balloons wins.
 *
 * The arena is the SNES's first, Battle Course 1, as the wikis describe
 * it (their maps are pictures; this is it redrawn from the words, not
 * copied): a square field in the Donut Plains, "pipes and walls
 * separating the inner, middle, and outer square, making a concentric
 * square pattern", the walls "multicolored", "a large open place in the
 * middle, where players are more likely to be hit", and "many corners
 * jutting out from the outsides, leading into hidden corners" with a
 * single way out. Here: the border '#', the middle ring's wall two gaps
 * a side and the inner one's one, each side of them in its color -- red
 * 'R' the top, blue 'B' the right, yellow 'Y' the bottom, green 'G' the
 * left, the colors of the guardrails of its remake in Mario Kart 8
 * Deluxe -- a pocket in each outer corner, the boxes '?', the karts'
 * starts '1' to '4' in the middle ring, a side each. Walls are flat
 * tiles, as on the SNES, which karts bounce off (Topdown.bounce,
 * TinySuperSprint's walls); the land around is the Donut Plains' lake. *)
let arena_rows =
  [ "############################";
    "#...#..................#...#";
    "#...#........?.........#...#";
    "#...#..................#...#";
    "###.RRRR..RRRRRRRR..RRRR.###";
    "#...G..................B...#";
    "#...G.?......1.......?.B...#";
    "#...G..................B...#";
    "#.......RRRRR..RRRRR.......#";
    "#.......G..........B.......#";
    "#...G...G..........B...B...#";
    "#...G...G..........B...B...#";
    "#...G...G...?..?...B...B...#";
    "#...G.4................B.?.#";
    "#.?.G................2.B...#";
    "#...G...G...?..?...B...B...#";
    "#...G...G..........B...B...#";
    "#...G...G..........B...B...#";
    "#.......G..........B.......#";
    "#.......YYYYY..YYYYY.......#";
    "#...G..................B...#";
    "#...G.?.......3......?.B...#";
    "#...G..................B...#";
    "###.YYYY..YYYYYYYY..YYYY.###";
    "#...#..................#...#";
    "#...#.........?........#...#";
    "#...#..................#...#";
    "############################" ]

let arena = Tilemap.of_strings tile arena_rows
let half_arena = float_of_int (Tilemap.cols arena) *. tile /. 2.

let wall (x : number) (y : number) : bool = match Tilemap.tile_at arena x y with Some ('#' | 'R' | 'B' | 'Y' | 'G') | None -> true | _ -> false
let outside (x : number) (y : number) : bool = Tilemap.tile_at arena x y = None

(* the Donut Plains' sandy floor in checkers, the border grey, the rings'
 * walls in their side's color, the lake around (mipmapped far away, as
 * the track) *)
let arena_ground (unit : number) (x : number) (y : number) : char =
  let checker size (c1 : char) (c2 : char) (average : char) : char =
    if size < 2. *. unit then average
    else if (int_of_float (floor (x /. size)) + int_of_float (floor (y /. size))) land 1 = 0 then c1
    else c2
  in
  match Tilemap.tile_at arena x y with
  | None -> checker 200. 'o' 'O' 'n'
  | Some '#' -> checker 50. 'k' 'K' 'l'
  | Some 'R' -> checker 50. 'r' 'R' 'q'
  | Some 'B' -> checker 50. 'u' 'U' 'v'
  | Some 'G' -> checker 50. 'g' 'G' 'h'
  | Some 'Y' -> checker 50. 'y' 'Y' 'z'
  | _ -> checker 100. 's' 'S' 't'

let arena_palette : (char * color) list =
  [ ('k', rgb 150 150 160); ('K', rgb 120 120 130); ('l', rgb 135 135 145);
    ('o', rgb 60 120 200); ('O', rgb 50 108 188); ('n', rgb 55 114 194);
    ('s', rgb 222 196 140); ('S', rgb 208 180 124); ('t', rgb 215 188 132);
    ('r', rgb 220 50 45); ('R', rgb 180 35 35); ('q', rgb 200 42 40);
    ('u', rgb 50 100 220); ('U', rgb 35 80 185); ('v', rgb 42 90 202);
    ('g', rgb 50 170 60); ('G', rgb 35 140 45); ('h', rgb 42 155 52);
    ('y', rgb 245 205 40); ('Y', rgb 215 175 25); ('z', rgb 230 190 32) ]

let colors = [| rgb 220 30 30; rgb 40 90 220; rgb 40 170 60; rgb 240 200 30 |]

let new_battle (players : int) : battle =
  let fighter i =
    let col, row = List.hd (Tilemap.find arena (Char.chr (Char.code '1' + i))) in
    let x, y = Tilemap.center arena col row in
    (* facing the middle *)
    let heading = atan2 (-.y) (-.x) *. 180. /. Float.pi in
    { kart = { car = { x; y; vx = 0.; vy = 0.; heading; speed = 0.; next = 0 }; top = 650.; color = colors.(i) };
      balloons = 3; item = None; spin = 0 }
  in
  let fighters = List.init 4 fighter in
  { fighters; players; views = List.map (fun f -> f.kart.car.heading) (List.filteri (fun i _ -> i < players) fighters);
    shells = []; bananas = [];
    boxes = List.map (fun (c, r) -> (Tilemap.center arena c r, 0)) (Tilemap.find arena '?');
    minds = Array.init 4 (fun _ -> Bot.start { goal = None; use = false }); clock = 0; countdown = 180; over = None }

(* The computer's karts, on ai/: A* (Pathfind) over the arena's tiles
 * round the blocks, the way to a box while it has nothing to throw
 * and to a rival once it has, as two moods and the rule between them
 * (Fsm), all of it seen a moment late and changed only so often (Bot);
 * its hands (Bot's reflex) steer along the way from where the kart is
 * now. It throws a shell when a rival is ahead and near, drops a
 * banana when one is on its tail. *)
let tile_of (x : number) (y : number) : int * int = Tilemap.cell arena x y

let path_to (x, y) (tx, ty) : (number * number) option =
  let goal = tile_of tx ty in
  let open_ (c, r) = match Tilemap.get arena c r with Some ('#' | 'R' | 'B' | 'Y' | 'G') | None -> false | _ -> true in
  let problem : (int * int) Pathfind.problem =
    { neighbors = (fun (c, r) -> List.filter_map (fun n -> if open_ n then Some (n, 1.) else None) [ (c + 1, r); (c - 1, r); (c, r + 1); (c, r - 1) ]);
      goal = (fun n -> n = goal); estimate = Pathfind.manhattan goal }
  in
  match (Pathfind.astar problem (tile_of x y)).path with
  | _ :: _ :: next :: _ -> Some (let c, r = next in Tilemap.center arena c r)
  | _ :: [ _ ] | [ _ ] -> Some (tx, ty)
  | _ -> None

(* the gas and the wheel towards a point, as Topdown.computer does
 * towards its waypoint *)
let steer_to (c : Topdown.t) ((tx, ty) : number * number) : number * number =
  let wanted = atan2 (ty -. c.y) (tx -. c.x) *. 180. /. Float.pi in
  let diff = angle_diff c.heading wanted in
  (Float.max 0.3 (0.85 -. (Float.abs diff /. 300.)), Float.max (-1.) (Float.min 1. (diff /. 20.)))

(* a rival ahead, near enough, and roughly where the kart points *)
let in_sights (c : Topdown.t) ((_, r) : int * Topdown.t) : bool =
  let d = Float.hypot (r.x -. c.x) (r.y -. c.y) in
  d < 900. && Float.abs (angle_diff c.heading (atan2 (r.y -. c.y) (r.x -. c.x) *. 180. /. Float.pi)) < 12.

let on_tail (c : Topdown.t) ((_, r) : int * Topdown.t) : bool =
  Float.hypot (r.x -. c.x) (r.y -. c.y) < 300. && Float.abs (angle_diff c.heading (atan2 (r.y -. c.y) (r.x -. c.x) *. 180. /. Float.pi)) > 150.

let moods : (mood, senses) Fsm.machine =
  [ { from = Collect; label = "an item"; guard = (fun s _ -> s.carrying <> None); target = Attack };
    { from = Attack; label = "used it"; guard = (fun s _ -> s.carrying = None); target = Collect } ]

let nearest (c : Topdown.t) (points : (number * number) list) : (number * number) option =
  List.fold_left
    (fun best (x, y) -> match best with Some (bx, by) when Float.hypot (bx -. c.x) (by -. c.y) <= Float.hypot (x -. c.x) (y -. c.y) -> best | _ -> Some (x, y))
    None points

let senses_of (was : senses option) ((b, i) : battle * int) : senses =
  let f = List.nth b.fighters i in
  let s =
    { me = i; own = f.kart.car; carrying = f.item;
      rivals = List.filter_map (fun (j, g) -> if j <> i && g.balloons > 0 then Some (j, g.kart.car) else None) (List.mapi (fun j g -> (j, g)) b.fighters);
      boxes = List.filter_map (fun (p, t) -> if t = 0 then Some p else None) b.boxes;
      mind = (match was with Some s -> s.mind | None -> Fsm.start Collect) }
  in
  { s with mind = Fsm.step moods s s.mind }

let decide (s : senses) : order =
  match s.mind.state with
  | Collect -> { goal = nearest s.own s.boxes; use = false }
  | Attack ->
      let use =
        match s.carrying with
        | Some Shell -> List.exists (in_sights s.own) s.rivals
        | Some Banana -> List.exists (on_tail s.own) s.rivals
        | None -> false
      in
      { goal = nearest s.own (List.map (fun (_, r) -> (r.Topdown.x, r.Topdown.y)) s.rivals); use }

(* its hands: the way to the goal from where it is now *)
let hands_of ((b, i) : battle * int) (o : order) : order =
  let f = List.nth b.fighters i in
  { o with goal = Option.bind o.goal (path_to (f.kart.car.x, f.kart.car.y)) }

let mind : (battle * int, senses, order) Bot.t = Bot.make ~delay:10 ~rate:4 ~reflex:hands_of ~sense:senses_of ~decide ()

(* a shell: straight on, bouncing off the walls, an axis at a time *)
let step_shell (s : shell) : shell =
  let dt = 1. /. 60. in
  let x = s.sx +. (s.vx *. dt) and y = s.sy +. (s.vy *. dt) in
  let vx = if wall x s.sy then -.s.vx else s.vx and vy = if wall s.sx y then -.s.vy else s.vy in
  { s with sx = (if wall x s.sy then s.sx else x); sy = (if wall s.sx y then s.sy else y); vx; vy; life = s.life - 1 }

let hit_radius = 45.

let step_battle (k : keyboard) (s : 'scene Scene2d.t) (b : battle) : battle =
  if b.countdown > 0 then { b with countdown = b.countdown - 1 }
  else
    let minds = Array.copy b.minds in
    let fire_key i = if i = 0 then Scene2d.pressed (fun k -> k.kspace) s else Scene2d.pressed (fun k -> Set_.mem "e" k.keys) s in
    let orders =
      List.mapi
        (fun i f ->
          if f.balloons = 0 || f.spin > 0 then (0., 0., false)
          else if i < b.players then
            let gas, steer = hands k i in
            (gas, steer, fire_key i)
          else
            let o, running = Bot.step mind (b, i) minds.(i) in
            minds.(i) <- running;
            let gas, steer = match o.goal with Some p -> steer_to f.kart.car p | None -> (0.3, 0.5) in
            (gas, steer, o.use))
        b.fighters
    in
    (* the karts: driven, bounced off the walls, a spinning one turning
     * on itself *)
    let fighters =
      List.map2
        (fun f (gas, steer, _) ->
          if f.balloons = 0 then f
          else
            let before = f.kart.car in
            let after = Topdown.drive Topdown.toy f.kart.top gas steer before in
            let after = if f.spin > 0 then { after with heading = after.heading +. 14.; speed = after.speed *. 0.9 } else after in
            { f with kart = { f.kart with car = Topdown.bounce wall before after }; spin = max 0 (f.spin - 1) })
        b.fighters orders
    in
    let fighters = List.map2 (fun f k -> { f with kart = k }) fighters (bump (List.map (fun f -> f.kart) fighters)) in
    (* the items used: a shell ahead of the kart, a banana behind it *)
    let shells, bananas, fighters =
      List.fold_left
        (fun (shells, bananas, acc) (i, f, (_, _, use)) ->
          let c = f.kart.car in
          let a = c.heading *. Float.pi /. 180. in
          match (use, f.item) with
          | true, Some Shell ->
              let v = Float.max 1100. (c.speed +. 700.) in
              ( { sx = c.x +. (60. *. cos a); sy = c.y +. (60. *. sin a); vx = v *. cos a; vy = v *. sin a; life = 300; from = i } :: shells,
                bananas, acc @ [ { f with item = None } ] )
          | true, Some Banana -> (shells, { bx = c.x -. (60. *. cos a); by = c.y -. (60. *. sin a); dropped_by = i; age = 0 } :: bananas, acc @ [ { f with item = None } ])
          | _ -> (shells, bananas, acc @ [ f ]))
        (b.shells, b.bananas, [])
        (List.mapi (fun i (f, o) -> (i, f, o)) (List.combine fighters orders))
    in
    let shells = List.filter (fun s -> s.life > 0) (List.map step_shell shells) in
    let bananas = List.map (fun b -> { b with age = b.age + 1 }) bananas in
    (* the boxes taken: an item, shells twice as often as bananas; the
     * box back three seconds later *)
    let boxes, fighters =
      List.fold_left
        (fun (boxes, fighters) ((bx, by), t) ->
          if t > 0 then (boxes @ [ ((bx, by), t - 1) ], fighters)
          else
            match List.find_opt (fun (_, f) -> f.balloons > 0 && f.item = None && Float.hypot (f.kart.car.x -. bx) (f.kart.car.y -. by) < 60.) (List.mapi (fun i f -> (i, f)) fighters) with
            | Some (i, _) ->
                let item = if (b.clock + i) mod 3 = 2 then Banana else Shell in
                (boxes @ [ ((bx, by), 180) ], List.mapi (fun j f -> if j = i then { f with item = Some item } else f) fighters)
            | None -> (boxes @ [ ((bx, by), 0) ], fighters))
        ([], fighters) b.boxes
    in
    (* the hits: a shell or a banana on a kart that isn't spinning
     * already -- a shell spares its thrower for a moment, a banana its
     * dropper *)
    let hittable f = f.balloons > 0 && f.spin = 0 in
    let near (x, y) f = Float.hypot (f.kart.car.x -. x) (f.kart.car.y -. y) < hit_radius in
    let shell_hits s = List.filter (fun (i, f) -> hittable f && near (s.sx, s.sy) f && (i <> s.from || s.life < 260)) (List.mapi (fun i f -> (i, f)) fighters) in
    let banana_hits bn = List.filter (fun (i, f) -> hittable f && near (bn.bx, bn.by) f && (i <> bn.dropped_by || bn.age > 60)) (List.mapi (fun i f -> (i, f)) fighters) in
    let hit = List.sort_uniq compare (List.concat_map (fun s -> List.map fst (shell_hits s)) shells @ List.concat_map (fun bn -> List.map fst (banana_hits bn)) bananas) in
    let shells = List.filter (fun s -> shell_hits s = []) shells in
    let bananas = List.filter (fun bn -> banana_hits bn = []) bananas in
    let fighters = List.mapi (fun i f -> if List.mem i hit then { f with balloons = f.balloons - 1; spin = 60 } else f) fighters in
    let views = List.mapi (fun i a -> a +. (0.2 *. angle_diff a (List.nth fighters i).kart.car.heading)) b.views in
    let over =
      match b.over with
      | Some n -> Some (n + 1)
      | None -> if List.length (List.filter (fun f -> f.balloons > 0) fighters) <= 1 then Some 0 else None
    in
    { b with fighters; shells; bananas; boxes; minds; views; clock = b.clock + 1; over }

(* the title, the race, the battle *)
let rules (computer : computer) (m : scene Scene2d.t) : scene Scene2d.t =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  let two = Scene2d.pressed (fun k -> Set_.mem "2" k.keys) m in
  match m.scene with
  | Title ->
      if space then Scene2d.go (Racing (new_race 1)) m
      else if two then Scene2d.go (Racing (new_race 2)) m
      else if Scene2d.pressed (fun k -> Set_.mem "b" k.keys) m then Scene2d.go (Battle (new_battle 1)) m
      else if Scene2d.pressed (fun k -> Set_.mem "v" k.keys) m then Scene2d.go (Battle (new_battle 2)) m
      else m
  | Battle b -> (
      let b = step_battle computer.keyboard m b in
      match b.over with Some n when n > 90 -> Scene2d.go (Battle_over b) m | _ -> { m with scene = Battle b })
  | Battle_over b -> if space then Scene2d.go Title m else { m with scene = Battle_over { b with clock = b.clock + 1 } }
  | Racing r ->
      let r = update_race computer.keyboard r in
      if List.for_all Option.is_some r.places then Scene2d.go (Finished r) m else { m with scene = Racing r }
  | Finished r -> if space then Scene2d.go Title m else { m with scene = Finished (update_race computer.keyboard r) }

(*****************************************************************************)
(* Mode 7 -- the trick of this game, in 99 lines (see the header) *)
(*****************************************************************************)

(* the camera: [height] above the ground, [back] behind the kart, the
 * horizon 15% of the screen's height above its center (150 pixels on a
 * screen 1000 high; the split screen's halves have theirs), and a
 * field of view of 60 degrees *)
let height = 90.
let back = 250.

(* the eye: where it is, the way it looks (a unit vector), its focal
 * length, in pixels (how many pixels 1 unit at distance 1 takes), and
 * the height of the horizon on its screen *)
type eye = { ex : number; ey : number; dx : number; dy : number; focal : number; horizon : number }

let eye (screen : screen) (x : number) (y : number) (angle : number) : eye =
  let a = angle *. Float.pi /. 180. in
  let dx = cos a and dy = sin a in
  { ex = x -. (back *. dx); ey = y -. (back *. dy); dx; dy; focal = screen.width /. 2. /. tan (Float.pi /. 6.);
    horizon = 0.15 *. screen.height }

(* [to_ground e sx sy]: the point of the ground the screen pixel (sx, sy)
 * shows, if it's below the horizon. The row [below] pixels under the
 * horizon sees the ground at the distance [d] (similar triangles: height
 * / d = below / focal), and at that distance a pixel across is d / focal
 * units: along the way we look [d], to the right (dy, -dx) sx * d /
 * focal. E.g. 90 units high with a focal of 866, the row 90 pixels
 * under the horizon sees 866 ahead, where a pixel is 1 unit. *)
let to_ground (e : eye) (sx : number) (sy : number) : (number * number) option =
  let below = e.horizon -. sy in
  if below <= 0. then None
  else
    let d = height *. e.focal /. below in
    let across = sx *. d /. e.focal in
    Some (e.ex +. (d *. e.dx) +. (across *. e.dy), e.ey +. (d *. e.dy) -. (across *. e.dx))

(* [to_screen e x y]: the other way, a point of the ground on the screen,
 * if it's in front: (sx, sy, pixels per unit there). Its distance
 * along the way we look ([depth]) and to the right ([across]) are dot
 * products; then the same similar triangles. *)
let to_screen (e : eye) (x : number) (y : number) : (number * number * number) option =
  let rx = x -. e.ex and ry = y -. e.ey in
  let depth = (rx *. e.dx) +. (ry *. e.dy) and across = (rx *. e.dy) -. (ry *. e.dx) in
  if depth < 20. then None else Some (across *. e.focal /. depth, e.horizon -. (height *. e.focal /. depth), e.focal /. depth)

(* the ground's "pixels", 5 real ones wide and high, and how far we see:
 * further, the rows are left to the horizon's color (a pixel there would
 * be tens of tiles) *)
let pixel = 5.
let far = 4500.

(* The ground, a row at a time: each row's samples across the screen,
 * one per pixel, the ground's character there ([ground]); all the rows
 * a picture of characters, drawn by Sprite.pixels. *)
let view_ground ?(floor = ground) ?(colors = palette) (screen : screen) (e : eye) : shape =
  let horizon = e.horizon in
  let cols = int_of_float (screen.width /. pixel) in
  let rows = int_of_float ((horizon -. screen.bottom) /. pixel) in
  let row i =
    let sy = horizon -. ((float_of_int i +. 0.5) *. pixel) in
    (* the distance this row sees, and a sample's width there *)
    let d = height *. e.focal /. (horizon -. sy) in
    if d > far then String.make cols ' '
    else
      String.init cols (fun j ->
          match to_ground e (screen.left +. ((float_of_int j +. 0.5) *. pixel)) sy with
          | Some (x, y) -> floor (d /. e.focal *. pixel) x y
          | None -> ' ')
  in
  Sprite.pixels pixel colors (List.init rows row)
  |> move ((screen.left +. screen.right) /. 2.) (horizon -. (float_of_int rows *. pixel /. 2.))

(* a hill, a disc on the horizon, cut at the top of the screen when it
 * goes past it (a half of the split screen): the circle's points above
 * the top brought down onto it, which for a disc is the disc cut *)
let hill (screen : screen) (color : color) (r : number) (sx : number) (horizon : number) : shape =
  if horizon +. r <= screen.top then circle color r |> move sx horizon
  else
    polygon color
      (List.init 48 (fun i ->
           let a = float_of_int i *. Float.pi /. 24. in
           (sx +. (r *. cos a), Float.min screen.top (horizon +. (r *. sin a)))))

(* the sky, and hills on the horizon, going by as you turn: at their
 * bearing, as many pixels a degree as the focal length gives *)
let view_sky (screen : screen) (angle : number) (e : eye) : shape list =
  let horizon = e.horizon in
  let per_degree = e.focal *. Float.pi /. 180. in
  let hill (bearing, r, color) =
    let sx = -.angle_diff angle bearing *. per_degree in
    if Float.abs sx > (screen.width /. 2.) +. r then [] else [ hill screen color r sx horizon ]
  in
  [ rectangle (rgb 110 170 240) screen.width (screen.top -. horizon) |> move_y ((screen.top +. horizon) /. 2.) ]
  @ List.concat_map hill
      [ (10., 120., rgb 60 130 90); (40., 200., rgb 80 150 100); (95., 90., rgb 60 130 90); (150., 160., rgb 90 160 110);
        (200., 110., rgb 60 130 90); (250., 220., rgb 80 150 100); (300., 140., rgb 90 160 110); (340., 90., rgb 60 130 90) ]
  @ [ rectangle (rgb 40 110 50) screen.width (horizon -. screen.bottom) |> move_y ((horizon +. screen.bottom) /. 2.) ]

(*****************************************************************************)
(* The karts *)
(*****************************************************************************)

(* A kart, seen from four angles, 16 x 10: the driver's helmet 'H' and
 * the body 'B' in its color, the skin 'S', the tires 'K', the engine
 * 'E'. The side and three-quarter drawings look right; [Sprite.flip]
 * for the left. *)
let from_back =
  [ "......HHHH......"; ".....HHHHHH....."; ".....HHHHHH....."; "......SSSS......"; "....BBBBBBBB....";
    "...BBBBBBBBBB..."; "KKKBBEEEEEEBBKKK"; "KKKBBEEEEEEBBKKK"; "KKKBBBBBBBBBBKKK"; "KKK.EE....EE.KKK" ]

let from_three_quarters =
  [ ".....HHHH......."; "....HHHHHH......"; "....HHHHHWW....."; ".....SSSS......."; "...BBBBBBBBBB...";
    "..BBBBBBBBBBBBB."; "KKKBEEEEBBBBKKK."; "KKKBEEEEBBBBKKKK"; "KKKBBBBBBBBBKKKK"; "KKK.EE......KKK." ]

let from_side =
  [ "......HHH......."; ".....HHHHH......"; ".....HHHWWW....."; "......SSS......."; "...BBBBBBBBBB...";
    "..BBBBBBBBBBBBBB"; ".KKKBBBBBBBKKKB."; "KKKKKEEEEEKKKKK."; "KKKKK.....KKKKK."; ".KKK.......KKK.." ]

let from_front =
  [ "......HHHH......"; ".....HWWWWH....."; ".....HSKKSH....."; "......SSSS......"; "....BBBBBBBB....";
    "...BBBBBBBBBB..."; "KKKBBBBBBBBBBKKK"; "KKKBEEEEEEEEBKKK"; "KKKBBBBBBBBBBKKK"; "KKK..........KKK" ]

let kart_palette (color : color) : (char * color) list =
  [ ('H', color); ('B', color); ('S', rgb 250 200 160); ('K', rgb 25 25 25); ('E', rgb 150 150 160); ('W', white) ]

(* The drawing for the kart's heading seen from the camera's: its
 * back when it goes our way, its side when it crosses, flipped when it
 * turns left (its nose to the left of the screen). *)
let drawing (view_angle : number) (heading : number) : string list =
  let rel = angle_diff view_angle heading in
  let rows =
    match Float.abs rel with
    | a when a < 20. -> from_back
    | a when a < 65. -> from_three_quarters
    | a when a < 140. -> from_side
    | _ -> from_front
  in
  if rel > 0. && Float.abs rel < 140. then Sprite.flip rows else rows

(* a kart 48 units wide, standing on the ground where it is *)
let view_kart (e : eye) (view_angle : number) (k : kart) : (number * shape) option =
  to_screen e k.car.x k.car.y
  |> Option.map (fun (sx, sy, scale) ->
         let size = 48. /. 16. *. scale in
         (scale, Sprite.pixels size (kart_palette k.color) (drawing view_angle k.car.heading) |> move sx (sy +. (5. *. size))))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the whole race from above, north up, 5 pixels a tile: the road, the
 * start line, and a dot per kart *)
let road_or_line (c : char) : char = match c with '.' -> '.' | '=' | 'a' -> '=' | _ -> '#'

let view_minimap (screen : screen) (r : race) : shape list =
  let cell = 5. in
  let ox = screen.right -. 105. and oy = screen.bottom +. 85. in
  let dot (k : kart) = circle k.color 5. |> move (ox +. (k.car.x /. tile *. cell)) (oy +. (k.car.y /. tile *. cell)) in
  [ rectangle (rgb 30 30 30) 180. 140. |> move ox oy |> fade 0.7;
    Sprite.pixels cell [ ('#', gray); ('=', white) ] (List.map (String.map road_or_line) (Tilemap.to_strings map))
    |> move ox oy ]
  @ List.map dot (List.rev r.karts)

let text color size str = words color str |> scale size

let ordinal (n : int) : string = match n with 1 -> "1ST" | 2 -> "2ND" | 3 -> "3RD" | n -> string_of_int n ^ "TH"

(* the race as player [i] sees it, on [screen] (the whole screen, or a
 * half of it) *)
let view_race (screen : screen) (r : race) (i : int) : shape list =
  let player = (List.nth r.karts i).car and view_angle = List.nth r.view_angles i in
  let e = eye screen player.x player.y view_angle in
  (* the karts, the farthest first (the smallest scale); in a half of
   * the split screen, the ones standing below its bottom left out *)
  let karts =
    List.filter_map
      (fun k ->
        match to_screen e k.car.x k.car.y with
        | Some (_, sy, _) when r.humans = 2 && sy < screen.bottom -> None
        | _ -> view_kart e view_angle k)
      r.karts
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd
  in
  let lap = min laps (Topdown.lap track player + 1) in
  let time = float_of_int r.frames /. 60. in
  let middle = screen.top /. 2. in
  view_sky screen view_angle e
  @ [ view_ground screen e ]
  @ karts
  @ (if r.humans = 1 then view_minimap screen r else [])
  @ [ text white 3. (Printf.sprintf "LAP %d/%d" lap laps) |> move (screen.left +. 110.) (screen.top -. 40.);
      text white 3. (Printf.sprintf "%d:%04.1f" (int_of_float time / 60) (Float.rem time 60.)) |> move (screen.right -. 120.) (screen.top -. 40.);
      text yellow 5. (ordinal (match List.nth r.places i with Some n -> n | None -> place r i)) |> move (screen.left +. 80.) (screen.top -. 110.) ]
  @ (match List.nth r.places i with
    | Some n -> [ text yellow 6. (ordinal n ^ " PLACE!") |> move_y middle ]
    | None ->
        if r.ready > 0 then [ text yellow 8. (string_of_int ((r.ready + 59) / 60)) |> move_y middle ]
        else if r.frames < 40 then [ text yellow 8. "GO!" |> move_y middle ]
        else [])

(* Two players: a half each, the first on top, as on the SNES. Each half
 * is a screen of its own, centered, drawn and moved into place; the
 * bottom one first, so that the top one's sky and ground cover what it
 * spills over the middle, then a strip over the seam. *)
let view_split (screen : screen) (view_one : screen -> int -> shape list) : shape list =
  let h = screen.height /. 2. in
  let half = { screen with height = h; top = h /. 2.; bottom = -.h /. 2. } in
  let at dy shapes = [ group shapes |> move_y dy ] in
  at (-.h /. 2.) (view_one half 1) @ at (h /. 2.) (view_one half 0) @ [ rectangle black screen.width 6. ]

let view_players (screen : screen) (r : race) : shape list =
  if r.humans = 2 then view_split screen (fun half i -> view_race half r i) else view_race screen r 0

(*****************************************************************************)
(* The battle's view *)
(*****************************************************************************)

(* the battle's billboards: an item box, a shell, a banana, each a
 * picture standing on the ground and sized by its distance, as the
 * karts are *)
let box_art = [ "OOOOOOOO"; "OYYYYYYO"; "OYY..YYO"; "OYYYY.YO"; "OYYY.YYO"; "OYYYYYYO"; "OYYY.YYO"; "OOOOOOOO" ]
let shell_art = [ ".WWWW."; "WGGGGW"; "WGWGGW"; "WGGGGW"; ".WWWW." ]
let banana_art = [ "....YB"; "...YY."; "..YY.."; "YYY..."; ".YY..." ]

let billboard (e : eye) (x : number) (y : number) (width : number) (colors : (char * color) list) (art : string list) : (number * shape) option =
  to_screen e x y
  |> Option.map (fun (sx, sy, scale) ->
         let size = width /. float_of_int (String.length (List.hd art)) *. scale in
         (scale, Sprite.pixels size colors art |> move sx (sy +. (float_of_int (List.length art) /. 2. *. size))))

(* a kart and its balloons, bobbing above it, its color; spinning, its
 * drawing turns *)
let view_fighter (e : eye) (view_angle : number) (b : battle) (f : fighter) : (number * shape) option =
  if f.balloons = 0 then None
  else
    Option.map
      (fun (scale, shape) ->
        match to_screen e f.kart.car.x f.kart.car.y with
        | None -> (scale, shape)
        | Some (sx, sy, sc) ->
            let r = 9. *. sc in
            let balloon j =
              let bob = 2. *. sc *. sin ((float_of_int b.clock /. 10.) +. float_of_int j) in
              group [ circle f.kart.color r; circle white (r /. 3.) |> move (-.r /. 3.) (r /. 3.) ]
              |> move (sx +. ((float_of_int j -. (float_of_int (f.balloons - 1) /. 2.)) *. 2.2 *. r)) (sy +. (48. *. sc) +. bob)
            in
            (scale, group (shape :: List.init f.balloons balloon)))
      (view_kart e view_angle f.kart)

let item_name = function Some Shell -> "SHELL" | Some Banana -> "BANANA" | None -> "--"

(* the battle as player [i] sees it, on [screen] *)
let view_battle_one (screen : screen) (b : battle) (i : int) : shape list =
  let me = List.nth b.fighters i in
  let view_angle = List.nth b.views i in
  let e = eye screen me.kart.car.x me.kart.car.y view_angle in
  let things =
    List.filter_map (view_fighter e view_angle b) b.fighters
    @ List.filter_map (fun ((x, y), t) -> if t = 0 then billboard e x y 50. [ ('O', rgb 240 140 30); ('Y', rgb 250 220 60); ('.', white) ] box_art else None) b.boxes
    @ List.filter_map (fun sh -> billboard e sh.sx sh.sy 30. [ ('W', white); ('G', rgb 40 180 60) ] shell_art) b.shells
    @ List.filter_map (fun bn -> billboard e bn.bx bn.by 30. [ ('Y', rgb 250 220 40); ('B', rgb 90 60 30) ] banana_art) b.bananas
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd
  in
  let middle = screen.top /. 2. in
  view_sky screen view_angle e
  @ [ view_ground ~floor:arena_ground ~colors:arena_palette screen e ]
  @ things
  @ [ text white 3. (Printf.sprintf "BALLOONS %d" me.balloons) |> move (screen.left +. 150.) (screen.top -. 40.);
      text yellow 3. (item_name me.item) |> move (screen.right -. 120.) (screen.top -. 40.) ]
  @
  if b.countdown > 0 then [ text yellow 8. (string_of_int ((b.countdown + 59) / 60)) |> move_y middle ]
  else if me.balloons = 0 then [ text white 5. "OUT!" |> move_y middle ]
  else if b.clock < 40 then [ text yellow 8. "GO!" |> move_y middle ]
  else []

(* the arena from above, a dot per kart *)
let view_battle_map (screen : screen) (b : battle) : shape list =
  let cell = 6. in
  let ox = screen.right -. 90. and oy = screen.bottom +. 90. in
  let dot (f : fighter) = circle f.kart.color 6. |> move (ox +. (f.kart.car.x /. tile *. cell)) (oy +. (f.kart.car.y /. tile *. cell)) in
  [ rectangle (rgb 30 30 30) 160. 160. |> move ox oy |> fade 0.7;
    Sprite.pixels cell
      [ ('#', gray); ('R', rgb 220 50 45); ('B', rgb 50 100 220); ('Y', rgb 245 205 40); ('G', rgb 50 170 60); ('?', rgb 240 140 30) ]
      (List.map (String.map (fun c -> if String.contains "#RBYG?" c then c else '.')) arena_rows)
    |> move ox oy ]
  @ List.map dot (List.filter (fun f -> f.balloons > 0) b.fighters)

let view_battle (screen : screen) (b : battle) : shape list =
  if b.players = 2 then view_split screen (fun half i -> view_battle_one half b i)
  else view_battle_one screen b 0 @ view_battle_map screen b

(* who won: the one kart with balloons left *)
let battle_result (b : battle) : shape list =
  match List.filteri (fun _ f -> f.balloons > 0) b.fighters with
  | [ w ] ->
      let i = Option.get (List.find_map (fun (i, f) -> if f == w then Some i else None) (List.mapi (fun i f -> (i, f)) b.fighters)) in
      [ text w.kart.color 6. (if i < b.players then (if b.players = 1 then "YOU WIN!" else Printf.sprintf "PLAYER %d WINS!" (i + 1)) else "THE COMPUTER WINS!") |> move_y 60. ]
  | _ -> [ text white 6. "DRAW" |> move_y 60. ]

let view_scene (computer : computer) (m : scene Scene2d.t) : shape list =
  let screen = computer.screen in
  match m.scene with
  | Title ->
      (* the track, turning slowly around the infield's middle *)
      let angle = float_of_int m.frames *. 0.3 in
      let e = eye screen 0. 0. angle in
      view_sky screen angle e
      @ [ view_ground screen e;
          text (rgb 220 30 30) 9. "TINY MARIO KART" |> move_y 330.;
          rectangle black 760. 260. |> move_y (-300.) |> fade 0.6;
          text white 3. "up: gas   down: brake   left/right: steer" |> move_y (-230.);
          text white 3. "3 laps, against the computer's karts" |> move_y (-280.);
          text white 3. "2: two players, split screen (w a s d)" |> move_y (-330.);
          text white 3. "b: battle   v: battle, two players (space, e: items)" |> move_y (-380.) ]
      @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-440.) ]
  | Racing r -> view_players screen r
  | Finished r -> view_players screen r @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y (-40.) ]
  | Battle b -> view_battle screen b
  | Battle_over b -> view_battle screen b @ battle_result b @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y (-40.) ]

(*****************************************************************************)
(* Sounds and juice (music=off, juice=off) *)
(*****************************************************************************)
(* claude: What a frame did that is heard or felt, found by comparing the
 * scene before it and after: the countdown's beeps and its GO, a lap,
 * the finish; in the battle, a box taken, a shell or a banana used, a
 * balloon lost -- with, when it is yours, the screen shaken, a red
 * flash and a hitstop, the hit felt as much as seen. The engines hum
 * all along, higher the faster they go. The sounds are Sfx's recipes
 * and the ready-made ones, a number or two changed, no recording. *)

let beep = Audio.sfx { Sfx.blip with frequency = 440.; volume = 0.3 }
let go_beep = Audio.sfx { Sfx.blip with frequency = 880.; sustain = 0.25; volume = 0.3 }
let lap_sound = Audio.sfx { Sfx.coin with volume = 0.3 }
let fanfare = Audio.sfx { Sfx.powerup with volume = 0.35 }
let pickup = Audio.sfx { Sfx.coin with frequency = 700.; volume = 0.25 }
let zap = Audio.sfx { Sfx.laser with volume = 0.25 }
let plop = Audio.sfx { Sfx.jump with frequency = 300.; slide = 150.; volume = 0.25 }
let pop = Audio.sfx { Sfx.hit with volume = 0.4 }

(* an original tune, bright and quiet under the engines: eight bars in
 * C, a bass on the beat *)
let music =
  Audio.abc
    {|X:1
T:Tiny Mario Kart (original)
L:1/8
Q:1/4=150
K:C
V:1
c2 eg c'2 ge | f2 ac' f'2 c'a | g2 bd' g'2 d'b | c'2 g2 e2 c2 |
e2 gc' e'2 c'g | f2 ac' a2 fc | d2 fa g2 bd' | c'4 c4 |
V:2
C,2 G,2 C,2 G,2 | F,2 C2 F,2 C2 | G,2 D2 G,2 D2 | C,2 G,2 C,2 G,2 |
C,2 G,2 C,2 G,2 | F,2 C2 F,2 C2 | G,2 D2 G,2 D2 | C,4 C,4 |
|}
  |> Audio.louder 0.14

(* the countdown's beeps: one a second, then GO *)
let countdown_sound (before : int) (after : int) : unit =
  if before > 0 && after < before then
    if after = 0 then Audio.play go_beep else if after mod 60 = 0 then Audio.play beep

(* where player 1 sees a point of the ground, alone on the screen *)
let seen_at (screen : screen) (angle : number) (me : Topdown.t) (x : number) (y : number) : (number * number) option =
  Option.map (fun (sx, sy, _) -> (sx, sy)) (to_screen (eye screen me.x me.y angle) x y)

let heard_and_felt (screen : screen) (before : scene) (after : scene) (fx : Juice.t) : Juice.t =
  match (before, after) with
  | Racing r, (Racing r' | Finished r') ->
      countdown_sound r.ready r'.ready;
      List.fold_left
        (fun fx i ->
          let c = (List.nth r.karts i).car and c' = (List.nth r'.karts i).car in
          if Topdown.lap track c' > Topdown.lap track c && Topdown.lap track c' < laps then Audio.play lap_sound;
          if List.nth r.places i = None && List.nth r'.places i <> None then begin
            Audio.play fanfare;
            match (r.humans, seen_at screen (List.nth r'.view_angles i) c' c'.x c'.y) with
            | 1, Some at -> fx |> Juice.burst ~at Juice.sparks |> Juice.flash white 15
            | _ -> fx
          end
          else fx)
        fx (List.init r.humans Fun.id)
  | Battle b, (Battle b' | Battle_over b') ->
      countdown_sound b.countdown b'.countdown;
      if List.length b'.shells > List.length b.shells then Audio.play zap;
      if List.length b'.bananas > List.length b.bananas then Audio.play plop;
      let me = (List.hd b'.fighters).kart.car in
      let fx =
        List.fold_left
          (fun fx (i, (f : fighter), (f' : fighter)) ->
            if i < b.players && f.item = None && f'.item <> None then Audio.play pickup;
            if f'.balloons < f.balloons then begin
              Audio.play pop;
              (* the balloon bursting, where player 1 sees it *)
              let fx =
                match (b.players, seen_at screen (List.hd b'.views) me f'.kart.car.x f'.kart.car.y) with
                | 1, Some (x, y) -> Juice.burst ~at:(x, y +. 40.) (Juice.debris f'.kart.color) fx
                | _ -> fx
              in
              if i < b.players then fx |> Juice.shake 0.6 |> Juice.flash (rgb 220 40 40) 12 |> Juice.freeze 5 else fx
            end
            else fx)
          fx
          (List.mapi (fun i (f, f') -> (i, f, f')) (List.combine b.fighters b'.fighters))
      in
      (match after with Battle_over _ -> Audio.play fanfare | _ -> ());
      fx
  | _ -> fx

(* each player's engine, its pitch its speed; two players, one on each
 * side *)
let engines (s : scene) : unit =
  let hum i (c : Topdown.t) side =
    Audio.keep_playing (Printf.sprintf "engine%d" i)
      (Audio.sawtooth (55. +. (Float.abs c.speed *. 0.12)) |> Audio.low_pass 700. |> Audio.louder 0.05 |> Audio.pan side)
  in
  let side n i = if n = 1 then 0. else if i = 0 then -0.4 else 0.4 in
  match s with
  | Racing r -> List.iteri (fun i (k : kart) -> if i < r.humans then hum i k.car (side r.humans i)) r.karts
  | Battle b -> List.iteri (fun i (f : fighter) -> if i < b.players && f.balloons > 0 then hum i f.kart.car (side b.players i)) b.fighters
  | _ -> ()

(* the rules, then what they did, heard and felt; nothing at all while
 * the juice freezes the game *)
let update (computer : computer) (m : model) : model =
  if List.assoc_opt "music" computer.flags = Some "off" then Audio.stop "music" else Audio.loop "music" music;
  let fx = Juice.step computer m.fx in
  if Juice.frozen fx then { m with fx }
  else
    let scenes = rules computer m.scenes in
    engines scenes.scene;
    { scenes; fx = heard_and_felt computer.screen m.scenes.scene scenes.scene fx }

let view (computer : computer) (m : model) : shape list = Juice.view m.fx (view_scene computer m.scenes)

let app = game view update initial_model

let main = Playground_platform.run_app app
