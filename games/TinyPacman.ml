(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Pac-Man (Toru Iwatani, Namco, 1980): eat all the dots
 * of the maze, avoid the four ghosts, and eat a power pellet ('o') to
 * turn them blue and eat them for a few seconds. Arrows to move.
 *
 * Iwatani wanted a game that wasn't about shooting, that women would
 * play too: eating, a character with a face -- the first video game
 * mascot. What makes it last is its ghosts: each has a personality,
 * made of a few lines of code choosing a target tile (see [target]),
 * and together they seem to hunt as a team. The source for all of it is
 * Jamey Pittman's "The Pac-Man Dossier" (2009), reverse-engineered from
 * the arcade's ROM, which the comments below follow.
 *
 * Two parts are the maze kit (gamekits/maze/), shared with
 * games/TinyBomberman.ml:
 *   - Grid_move: moving along the corridors of a Tilemap, one pixel at a
 *     time, turning only at the center of a tile, and remembering the
 *     turn you asked for until the corridor allows it -- what makes the
 *     controls feel good: you press up *before* the junction;
 *   - Chase: at each tile, a ghost takes the way that brings it closest
 *     to its target tile, never turning back; the four targets, here
 *     (see [target]), are the four personalities.
 *
 * The maze is ours (19x21, the original's is 28x31), as is its drawing
 * (walls as Sprite runs of blue). No randomness: the ghosts' random
 * turns when blue come from the original's own tiny generator, so every
 * game with the same inputs is the same -- which is why expert players
 * could learn "patterns", memorized routes that always work.
 *
 * Two ways of writing the ghosts' lives, chosen with the flag ai=engine
 * (?ai=engine on the web), as Asteroid and TinyMario choose their
 * physics: by default, the states change wherever the event happens --
 * a pellet eaten in [eat], a ghost caught in [collide], arrivals in
 * [move_ghost]; with ai=engine, a ghost is an ai/Fsm machine, its whole
 * life one table of rules ([mind_rules]), and the scatter/chase waves a
 * second machine ([wave_rules]). The same game either way (the tests
 * play both); the table is easier to read, and changes a state a frame
 * later than the event.
 *
 * Exercises: the fruits, the intermission cartoons, the speed-up of the
 * later levels, Pac-Man cutting corners (he may turn a few pixels before
 * the center, the ghosts may not: a reason he can outrun them), the
 * "Elroy" Blinky speeding up when few dots remain.
 *)
open Playground

(*****************************************************************************)
(* The maze *)
(*****************************************************************************)

(* '#' a wall, '.' a dot, 'o' a power pellet, '-' the ghost house's door,
 * 'P' where Pac-Man starts; the row with open ends is a tunnel, from one
 * side of the maze to the other *)
let maze_rows =
  [ "###################";
    "#o.......#.......o#";
    "#.##.###.#.###.##.#";
    "#.................#";
    "#.##.#.#####.#.##.#";
    "#....#...#...#....#";
    "####.### # ###.####";
    "   #.#       #.#   ";
    "####.# ##-## #.####";
    "    .  #   #  .    ";
    "####.# ##### #.####";
    "   #.#       #.#   ";
    "####.# ##### #.####";
    "#........#........#";
    "#.##.###.#.###.##.#";
    "#o.#.....P.....#.o#";
    "##.#.#.#####.#.#.##";
    "#....#...#...#....#";
    "#.######.#.######.#";
    "#.................#";
    "###################" ]

(* a tile's size, in pixels: the speeds below are in pixels per frame *)
let t = 40

let maze = Tilemap.of_strings (float_of_int t) maze_rows
let cols = Tilemap.cols maze
let rows = Tilemap.rows maze

(* the ghost house, below its door ('-'): the tile above the door
 * (outside), and the one inside *)
let outside = (9, 7)
let inside = (9, 9)

(*****************************************************************************)
(* Grid_move, the maze kit's *)
(*****************************************************************************)

(* moving along the corridors (gamekits/maze/Grid_move.mli), on this maze's
 * grid; the types re-exported, to write Up and m.gx here *)
type dir = Grid_move.dir = Up | Down | Left | Right | Stop
type mover = Grid_move.mover = { gx : int; gy : int; dir : dir; wanted : dir }

let grid : Grid_move.grid = { tile = t; cols; rows }
let delta = Grid_move.delta
let opposite = Grid_move.opposite
let at_center = Grid_move.at_center grid
let tile_of = Grid_move.tile_of grid
let next_tile = Grid_move.next_tile grid
let mover_at = Grid_move.mover_at grid
let slide ~choose = Grid_move.slide grid ~choose

(*****************************************************************************)
(* Ghosts *)
(*****************************************************************************)

type name = Blinky | Pinky | Inky | Clyde

(* waiting in the house for its release, leaving it, out hunting, or
 * eaten: its eyes going back to the house *)
type state = Waiting of int | Leaving | Hunting | Eyes

(* the same states as a machine for ai/Fsm (the ai=engine flag, see
 * "The ghosts as state machines" below), blue a state of its own *)
module Mind = struct
  type t = Waiting | Leaving | Hunting | Frightened | Eyes
end

type ghost = {
  name : name;
  m : mover;
  state : state;
  blue : bool;
  mind : Mind.t Fsm.run; (* only with ai=engine *)
}

(* The game alternates between "scatter", each ghost going to its own
 * corner, and "chase", each hunting Pac-Man its own way: a few seconds
 * of rest in between the attacks, the wave rhythm players feel. The
 * first level's schedule, in frames (1/60 s), the last chase forever. *)
let schedule = [ 420; 1200; 420; 1200; 300; 1200; 300 ]

let chasing (mode_frames : int) : bool =
  let rec go frames scatter = function
    | [] -> not scatter
    | d :: rest -> if frames < d then not scatter else go (frames - d) (not scatter) rest
  in
  go mode_frames true schedule

(* the corners, outside the maze, the ghosts scatter to *)
let corner (n : name) : int * int =
  match n with Blinky -> (cols - 3, -3) | Pinky -> (2, -3) | Inky -> (cols - 1, rows) | Clyde -> (0, rows)

(* [ahead pac n]: n tiles ahead of Pac-Man; when he faces up, also n to
 * the left: a bug of the original (an overflow in its arithmetic),
 * kept, as it's part of how Pinky and Inky behave *)
let ahead (pac : mover) (n : int) : int * int =
  let col, row = tile_of pac in
  let dc, dr = delta pac.dir in
  let bug = if pac.dir = Up then -n else 0 in
  (col + (dc * n) + bug, row + (dr * n))

(* The four personalities, in their chase targets (Pittman's
 * "Meet the ghosts"):
 *   - Blinky (red, "Shadow"): Pac-Man's tile, straight at him;
 *   - Pinky (pink, "Speedy"): 4 tiles ahead of him, to cut him off;
 *   - Inky (cyan, "Bashful"): the tile 2 ahead of Pac-Man, pushed away
 *     from Blinky as far again -- with Blinky, a pincer:
 *
 *        Blinky B . . . . x . . . . T   Inky's target T: twice the
 *                   2 ahead of Pac-Man x    vector from B to x
 *
 *   - Clyde (orange, "Pokey"): like Blinky while far (more than 8
 *     tiles), but back to his corner when close: shy. *)
let target ~(chase : bool) ~(pac : mover) ~(blinky : mover) (g : ghost) : int * int =
  match g.state with
  | Waiting _ | Leaving -> outside
  | Eyes -> inside
  | Hunting when not chase -> corner g.name
  | Hunting -> (
      let pc, pr = tile_of pac in
      match g.name with
      | Blinky -> (pc, pr)
      | Pinky -> ahead pac 4
      | Inky ->
          let xc, xr = ahead pac 2 and bc, br = tile_of blinky in
          ((2 * xc) - bc, (2 * xr) - br)
      | Clyde ->
          let gc, gr = tile_of g.m in
          let d2 = ((gc - pc) * (gc - pc)) + ((gr - pr) * (gr - pr)) in
          if d2 > 64 then (pc, pr) else corner g.name)

(* A ghost at a tile's center (gamekits/maze/Chase.mli): never back the way
 * it came; among the other open ways, the one whose next tile is the
 * closest to the target, or, when blue, one at random (from [rng]). *)
let ghost_choose ~(open_ : int * int -> bool) ~(goal : int * int) ~(random : int option) (m : mover) : mover =
  match random with
  | Some n -> Chase.at_random grid ~open_ n m
  | None -> Chase.toward grid ~open_ ~goal m

(* which tiles a ghost can enter: the door only to leave the house, or
 * to go back in as eyes *)
let ghost_open (g : ghost) ((col, row) : int * int) : bool =
  match Tilemap.get maze col row with
  | Some '#' | None -> false
  | Some '-' -> g.state = Leaving || g.state = Eyes
  | Some _ -> true

let next_random = Chase.next_random

(*****************************************************************************)
(* The ghosts as state machines (ai=engine) *)
(*****************************************************************************)

(* With the flag ai=engine (?ai=engine on the web), the ghosts' states
 * change by the rules below, run by ai/Fsm, instead of by the lines of
 * [eat], [collide], [move_ghost] and [update_game] that change them by
 * default. Everything else -- the targets, the moves, the scoring, the
 * drawing -- is the same code for both. What the machine version buys
 * is the whole of a ghost's life in one table, readable top to bottom,
 * in its order of priority:
 *
 *              released                  a power pellet
 *   Waiting ------------> Leaving ----> Hunting ---------> Frightened
 *                            ^      out    ^   time's up      |
 *                            |             +------------------+ eaten
 *                            +---- home ---- Eyes <-----------+
 *
 * and what it costs: a transition waits for the ghost's step, once a
 * frame, where the default code changes a state wherever the event
 * happens -- so an eaten ghost turns into eyes a frame later, and
 * [collide] has to remember it was caught in between ([caught]). *)

(* what a ghost's machine is told, each frame *)
type facts = {
  pellet : bool; (* Pac-Man ate a power pellet *)
  caught : bool; (* Pac-Man ate this ghost *)
  time_up : bool; (* the blue time is over *)
  home : bool; (* at the center of the house's tile *)
  out : bool; (* at the center of the tile outside its door *)
  wave : bool; (* scatter turned to chase, or back *)
  release : int; (* frames before it may leave the house *)
}

let mind_rules : (Mind.t, facts) Fsm.machine =
  let rule from label guard target : (Mind.t, facts) Fsm.rule = { from; label; guard; target } in
  Mind.
    [ rule Waiting "released" (fun f since -> since >= f.release) Leaving;
      rule Leaving "out" (fun f _ -> f.out) Hunting;
      rule Hunting "a power pellet" (fun f _ -> f.pellet) Frightened;
      rule Hunting "the wave turns" (fun f _ -> f.wave) Hunting;
      rule Frightened "eaten" (fun f _ -> f.caught) Eyes;
      rule Frightened "another pellet" (fun f _ -> f.pellet) Frightened;
      rule Frightened "time's up" (fun f _ -> f.time_up) Hunting;
      rule Eyes "home" (fun f _ -> f.home) Leaving ]

(* the transitions after which a ghost turns back: a sign to the player *)
let turns_back = [ "a power pellet"; "another pellet"; "the wave turns" ]

(* the default code's (state, blue) for a machine's state, which the
 * rest of the game reads *)
let of_mind (s : Mind.t) : state * bool =
  match s with
  | Mind.Waiting -> (Waiting 1, false)
  | Mind.Leaving -> (Leaving, false)
  | Mind.Hunting -> (Hunting, false)
  | Mind.Frightened -> (Hunting, true)
  | Mind.Eyes -> (Eyes, false)

(* when each ghost may leave the house: [Waiting n] counts n frames,
 * and leaves the frame after *)
let release (n : name) : int = match n with Blinky -> 0 | Pinky -> 61 | Inky -> 241 | Clyde -> 421

(* and the scatter/chase waves: [schedule] as a machine, a state per
 * wave *)
type wave = Scatter of int | Chase of int

let wave_rules : (wave, unit) Fsm.machine =
  List.mapi
    (fun i frames : (wave, unit) Fsm.rule ->
      let k = (i / 2) + 1 in
      if i mod 2 = 0 then { from = Scatter k; label = "chase"; guard = Fsm.after frames; target = Chase k }
      else { from = Chase k; label = "scatter"; guard = Fsm.after frames; target = Scatter (k + 1) })
    schedule

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type pause = Ready | Dying | Cleared

type game = {
  map : Tilemap.t; (* the maze with the dots left *)
  pac : mover;
  ghosts : ghost list;
  score : int;
  lives : int;
  level : int;
  mode_frames : int; (* time in the scatter/chase schedule, stopped while blue *)
  blue_frames : int; (* > 0: the ghosts are blue for that long *)
  eaten : int; (* ghosts eaten since the last pellet: 200, 400, 800, 1600 *)
  pause : (pause * int) option; (* a pause, and its frames left *)
  rng : int;
  frames : int;
  (* the ghosts on ai/Fsm (ai=engine) *)
  ai_engine : bool;
  waves : wave Fsm.run; (* in place of [mode_frames] *)
  pellet : bool; (* a power pellet eaten this frame *)
  caught : name list; (* ghosts eaten, not yet turned to eyes *)
}

type scene = Title | Playing of game | Game_over of int

type model = { scenes : scene Scene2d.t; hi_score : int }

let pac_start = match Tilemap.find maze 'P' with p :: _ -> p | [] -> (9, 15)

(* Blinky starts outside the house; the others wait inside, released
 * one after the other *)
let new_ghosts () : ghost list =
  let waiting = Fsm.start Mind.Waiting in
  [ { name = Blinky; m = mover_at outside; state = Hunting; blue = false; mind = Fsm.start Mind.Hunting };
    { name = Pinky; m = mover_at inside; state = Waiting 60; blue = false; mind = waiting };
    { name = Inky; m = mover_at (8, 9); state = Waiting 240; blue = false; mind = waiting };
    { name = Clyde; m = mover_at (10, 9); state = Waiting 420; blue = false; mind = waiting } ]

(* a new life: everybody back at the start, the dots as they are *)
let restart (g : game) : game =
  { g with
    pac = mover_at pac_start;
    ghosts = new_ghosts ();
    mode_frames = 0;
    blue_frames = 0;
    pause = Some (Ready, 120);
    waves = Fsm.start (Scatter 1);
    pellet = false;
    caught = [] }

let new_game ?(ai_engine = false) () : game =
  restart
    { map = maze; pac = mover_at pac_start; ghosts = []; score = 0; lives = 3; level = 1; mode_frames = 0;
      blue_frames = 0; eaten = 0; pause = None; rng = 0; frames = 0; ai_engine; waves = Fsm.start (Scatter 1);
      pellet = false; caught = [] }

let initial_model = { scenes = Scene2d.start Title; hi_score = 0 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let pac_open ((col, row) : int * int) : bool =
  match Tilemap.get maze col row with Some '#' | Some '-' | None -> false | Some _ -> true

let wanted_of (k : keyboard) (current : dir) : dir =
  if k.kup then Up else if k.kdown then Down else if k.kleft then Left else if k.kright then Right else current

(* Pac-Man eats what's on his tile *)
let eat (g : game) : game =
  let col, row = tile_of g.pac in
  match Tilemap.get g.map col row with
  | Some '.' -> { g with map = Tilemap.set g.map col row ' '; score = g.score + 10 }
  | Some 'o' when g.ai_engine ->
      (* the ghosts' machines hear of it, in [think] *)
      { g with map = Tilemap.set g.map col row ' '; score = g.score + 50; blue_frames = 360; eaten = 0; pellet = true }
  | Some 'o' ->
      { g with
        map = Tilemap.set g.map col row ' ';
        score = g.score + 50;
        blue_frames = 360;
        eaten = 0;
        (* all the ghosts out turn blue, and turn back *)
        ghosts =
          List.map
            (fun gh ->
              if gh.state = Hunting then { gh with blue = true; m = { gh.m with dir = opposite gh.m.dir } } else gh)
            g.ghosts }
  | _ -> g

(* chasing, or scattering: by the clock, or by the waves' machine *)
let chase_now (g : game) : bool =
  if g.ai_engine then (match g.waves.state with Chase _ -> true | Scatter _ -> false) else chasing g.mode_frames

(* a ghost one frame along its way to its target *)
let glide (g : game) (gh : ghost) : ghost =
  let blinky = (List.find (fun gh -> gh.name = Blinky) g.ghosts).m in
  let speed = match gh.state with Eyes -> 8 | _ when gh.blue -> 2 | Leaving -> 2 | _ -> 4 in
  let random = if gh.blue then Some g.rng else None in
  let goal = target ~chase:(chase_now g) ~pac:g.pac ~blinky gh in
  { gh with m = slide ~choose:(ghost_choose ~open_:(ghost_open gh) ~goal ~random) speed gh.m }

let move_ghost (g : game) (gh : ghost) : ghost =
  match gh.state with
  | Waiting _ when g.ai_engine -> gh (* released by its machine *)
  | _ when g.ai_engine -> glide g gh
  | Waiting 0 -> { gh with state = Leaving }
  | Waiting n -> { gh with state = Waiting (n - 1) }
  | _ ->
      let gh = glide g gh in
      (* arrived: out of the house, or back in it as eyes *)
      let state =
        match gh.state with
        | Leaving when at_center gh.m && tile_of gh.m = outside -> Hunting
        | Eyes when at_center gh.m && tile_of gh.m = inside -> Leaving
        | s -> s
      in
      { gh with state }

(* each ghost's machine told this frame's facts and stepped; turning
 * back after the transitions in [turns_back] *)
let think (g : game) : game =
  let wave = g.waves.fired <> None in
  let step gh =
    let at tile = at_center gh.m && tile_of gh.m = tile in
    let facts =
      { pellet = g.pellet; caught = List.mem gh.name g.caught; time_up = g.blue_frames = 0; home = at inside;
        out = at outside; wave; release = release gh.name }
    in
    let mind = Fsm.step mind_rules facts gh.mind in
    let m = match mind.fired with Some l when List.mem l turns_back -> { gh.m with dir = opposite gh.m.dir } | _ -> gh.m in
    let state, blue = of_mind mind.state in
    { gh with mind; m; state; blue }
  in
  { g with ghosts = List.map step g.ghosts; pellet = false; caught = [] }

(* Pac-Man and a ghost on the same tile: a blue ghost is eaten, any
 * other hunting one eats Pac-Man *)
let collide (g : game) : game =
  let pac = tile_of g.pac in
  List.fold_left
    (fun g gh ->
      if tile_of gh.m <> pac || gh.state = Eyes || (match gh.state with Waiting _ -> true | _ -> false) then g
      else if gh.blue && g.ai_engine then
        (* scored now, turned to eyes by its machine; until then, not
         * eaten twice *)
        if List.mem gh.name g.caught then g
        else { g with score = g.score + (200 * (1 lsl g.eaten)); eaten = g.eaten + 1; caught = gh.name :: g.caught }
      else if gh.blue then
        let points = 200 * (1 lsl g.eaten) in
        { g with
          score = g.score + points;
          eaten = g.eaten + 1;
          ghosts = List.map (fun o -> if o.name = gh.name then { o with state = Eyes; blue = false } else o) g.ghosts }
      else if g.pause = None then { g with pause = Some (Dying, 90) }
      else g)
    g g.ghosts

let update_game (computer : computer) (g : game) : game =
  let g = { g with frames = g.frames + 1; rng = next_random g.rng } in
  match g.pause with
  | Some (p, n) when n > 0 -> { g with pause = Some (p, n - 1) }
  | Some (Dying, _) -> restart { g with lives = g.lives - 1 }
  | Some (Cleared, _) -> restart { g with map = maze; level = g.level + 1 }
  | Some (Ready, _) | None ->
      let g = { g with pause = None } in
      let pac = { g.pac with wanted = wanted_of computer.keyboard g.pac.wanted } in
      let g = { g with pac = Grid_move.move_player grid ~open_:pac_open 5 pac } |> eat in
      let g =
        { g with
          blue_frames = max 0 (g.blue_frames - 1);
          mode_frames = (if g.blue_frames > 0 then g.mode_frames else g.mode_frames + 1);
          (* the waves stopped while blue, as the clock is *)
          waves = (if g.blue_frames > 0 then { g.waves with fired = None } else Fsm.step wave_rules () g.waves) }
      in
      if g.ai_engine then
        let g = collide g |> think in
        let g = { g with ghosts = List.map (move_ghost g) g.ghosts } in
        let g = collide g in
        if Tilemap.find g.map '.' = [] && Tilemap.find g.map 'o' = [] then { g with pause = Some (Cleared, 120) } else g
      else
      let g = if g.blue_frames = 0 then { g with ghosts = List.map (fun gh -> { gh with blue = false }) g.ghosts } else g in
      (* claude: a change of mode makes the hunting ghosts turn back, a
       * sign to the player that the wave changed *)
      let turn = chasing g.mode_frames <> chasing (g.mode_frames - 1) in
      let g =
        if turn then
          { g with ghosts = List.map (fun gh -> if gh.state = Hunting then { gh with m = { gh.m with dir = opposite gh.m.dir } } else gh) g.ghosts }
        else g
      in
      let g = collide g in
      let g = { g with ghosts = List.map (move_ghost g) g.ghosts } in
      let g = collide g in
      if Tilemap.find g.map '.' = [] && Tilemap.find g.map 'o' = [] then { g with pause = Some (Cleared, 120) } else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let space = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title ->
      let ai_engine = List.assoc_opt "ai" computer.flags = Some "engine" in
      if space then { model with scenes = Scene2d.go (Playing (new_game ~ai_engine ())) scenes } else { model with scenes }
  | Playing g ->
      let g = update_game computer g in
      let hi_score = max model.hi_score g.score in
      if g.lives = 0 then { hi_score; scenes = Scene2d.go (Game_over g.score) scenes }
      else { hi_score; scenes = { scenes with scene = Playing g } }
  | Game_over _ ->
      if space || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let bounds = Tilemap.bounds maze

(* a mover's position in the world *)
let world (m : mover) : number * number = Grid_move.to_world grid bounds m

let yellow_pac = rgb 255 230 0

(* Pac-Man: a disk with a wedge cut out, the mouth, opening and closing
 * (Sprite.cycle over three mouths), turned towards where he goes *)
let pacman_shape (mouth : number) : shape =
  if mouth = 0. then circle yellow_pac 17.
  else
    let n = 24 in
    polygon yellow_pac
      ((0., 0.)
      :: List.init (n + 1) (fun i ->
             let a = (mouth +. ((360. -. (2. *. mouth)) *. float_of_int i /. float_of_int n)) *. Float.pi /. 180. in
             (17. *. cos a, 17. *. sin a)))

let mouths = List.map pacman_shape [ 0.; 25.; 45.; 25. ]

let angle_of (d : dir) : number = match d with Right | Stop -> 0. | Up -> 90. | Left -> 180. | Down -> 270.

(* the ghosts' pixel art, two frames for their wavy skirt: '#' the body,
 * 'w' the eyes' white, 'b' the pupils *)
let ghost_top =
  [ ".....####.....";
    "...########...";
    "..##########..";
    ".##wwww##wwww#";
    ".##wwbb##wwbb#";
    "###wwbb##wwbb#";
    "####ww####ww##";
    "##############";
    "##############";
    "##############";
    "##############" ]

let ghost_frames = [ ghost_top @ [ "##.###..###.##"; "#...##..##...#" ]; ghost_top @ [ "###.##..##.###"; ".#...#..#...#." ] ]

let ghost_color (n : name) : color =
  match n with Blinky -> red | Pinky -> rgb 255 184 255 | Inky -> rgb 0 255 255 | Clyde -> rgb 255 184 82

let ghost_art (palette : (char * color) list) : shape list = List.map (Sprite.pixels 3. palette) ghost_frames

let blue_art = ghost_art [ ('#', rgb 33 33 255); ('b', rgb 255 184 174) ]
let white_art = ghost_art [ ('#', white); ('b', red) ]
let eyes_art = ghost_art [ ('w', white); ('b', rgb 33 33 255) ]
let ghost_arts = List.map (fun n -> (n, ghost_art [ ('#', ghost_color n); ('w', white); ('b', rgb 33 33 255) ])) [ Blinky; Pinky; Inky; Clyde ]

let ghost_shape (g : game) (gh : ghost) : shape =
  let art =
    match gh.state with
    | Eyes -> eyes_art
    (* blinking white in the last two seconds: hurry *)
    | _ when gh.blue -> if g.blue_frames < 120 && g.frames / 10 mod 2 = 0 then white_art else blue_art
    | _ -> List.assoc gh.name ghost_arts
  in
  let x, y = world gh.m in
  Sprite.cycle (g.frames / 8) art |> move x y

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let view_maze (g : game) : shape =
  let walls = Sprite.pixels (float_of_int t) [ ('#', rgb 33 33 200) ] (Tilemap.to_strings g.map) in
  let tile c =
    match c with
    | '.' -> square (rgb 255 184 174) 6.
    | 'o' -> if g.frames / 15 mod 2 = 0 then circle (rgb 255 184 174) 12. else group []
    | '-' -> rectangle (rgb 255 184 255) 40. 6.
    | _ -> group []
  in
  group [ walls; Tilemap.view tile g.map ]

let view_game (model : model) (g : game) : shape list =
  let px, py = world g.pac in
  let pac =
    match g.pause with
    | Some (Dying, n) -> pacman_shape (180. -. (float_of_int n *. 2.)) |> rotate 90.
    | _ -> Sprite.cycle (if g.pac.dir = Stop then 1 else g.frames / 3) mouths |> rotate (angle_of g.pac.dir)
  in
  [ view_maze g; pac |> move px py ]
  @ (match g.pause with Some (Dying, _) -> [] | _ -> List.map (ghost_shape g) g.ghosts)
  @ [ text white 3. (Printf.sprintf "SCORE %d" g.score) |> move (-300.) 460.;
      text white 3. (Printf.sprintf "HI-SCORE %d" model.hi_score) |> move 250. 460. ]
  @ List.init (max 0 (g.lives - 1)) (fun i -> pacman_shape 25. |> rotate 180. |> move (-420. +. (float_of_int i *. 45.)) (-470.))
  @ (match g.pause with
    | Some (Ready, _) -> [ text yellow_pac 3. "READY!" |> move_y (-40.) ]
    | Some (Cleared, _) -> [ text white 3. "WELL DONE!" |> move_y (-40.) ]
    | _ -> [])

(* the original's attract screen: the four ghosts, their character and
 * nickname *)
let view_title (s : scene Scene2d.t) : shape list =
  [ text yellow_pac 6. "TINY PAC-MAN" |> move_y 300. ]
  @ List.concat
      (List.mapi
         (fun i (n, character, how) ->
           let y = 150. -. (float_of_int i *. 90.) in
           [ List.hd (List.assoc n ghost_arts) |> move (-300.) y;
             text (ghost_color n) 3. character |> move (-80.) (y +. 12.);
             text gray 2. how |> move 60. (y -. 22.) ])
         [ (Blinky, "SHADOW  \"BLINKY\"", "chases you");
           (Pinky, "SPEEDY  \"PINKY\"", "aims 4 tiles ahead of you");
           (Inky, "BASHFUL  \"INKY\"", "flanks you, with Blinky");
           (Clyde, "POKEY  \"CLYDE\"", "chases you, but shy when close") ])
  @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y (-300.) ]

let view (computer : computer) (model : model) : shape list =
  rectangle black computer.screen.width computer.screen.height
  ::
  (match model.scenes.scene with
  | Title -> view_title model.scenes
  | Playing g -> view_game model g
  | Game_over score ->
      [ text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ]
      @ Scene2d.blink 1. model.scenes [ text white 3. "PRESS SPACE" |> move_y (-200.) ])

let app = game view update initial_model

let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
