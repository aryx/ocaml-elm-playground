(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Pac-Man's four ghosts, and what each is thinking. Move Pac-Man with
 * the arrows; each ghost's target tile is drawn in its colour, where
 * its rule puts it this frame, and each ghost takes, at every
 * junction, the way whose next tile is nearest that target -- nothing
 * more (Jamey Pittman, "The Pac-Man Dossier", 2009):
 *
 *   Blinky (red)    Pac-Man's own tile: straight at him
 *   Pinky (pink)    4 tiles ahead of him -- and 4 to the left too when
 *                   he faces up, the arcade's overflow bug, kept
 *   Inky (cyan)     the tile 2 ahead of Pac-Man, pushed away from
 *                   Blinky as far again: the line from Blinky through
 *                   that point, doubled -- with Blinky, a pincer
 *   Clyde (orange)  Pac-Man's tile while more than 8 tiles away (his
 *                   circle); inside it, his own corner: shy
 *
 * Four personalities from four lines of arithmetic, and together they
 * look like a team hunting: Blinky behind you, Pinky in front, Inky
 * closing the gap. No ghost knows the others are there.
 *
 * And the rhythm that makes the game breathe: the waves, a state
 * machine (ai/Fsm.mli), scatter (each ghost off to its own corner, the
 * targets there) then chase, then scatter again, on the arcade's
 * first-level clock (7 s, 20 s, 7 s, 20 s, 5 s, 20 s, 5 s, then chase
 * for good) -- the timeline at the bottom, the current wave lit.
 *
 * What it uses: the maze kit (gamekits/maze: Grid_move for moving
 * along the corridors, Chase for the choice at a junction), ai/Fsm for
 * the waves. TinyPacman.ml is the whole game, with the same rules
 * (and its ghosts' states on ai/Fsm too, with ai=engine). *)
open Playground

(*****************************************************************************)
(* The maze *)
(*****************************************************************************)

(* coupling: TinyPacman.ml's maze, without its dots *)
let maze_rows =
  [ "###################";
    "#        #        #";
    "# ## ### # ### ## #";
    "#                 #";
    "# ## # ##### # ## #";
    "#    #   #   #    #";
    "#### ### # ### ####";
    "   # #       # #   ";
    "#### # ##### # ####";
    "       #   #       ";
    "#### # ##### # ####";
    "   # #       # #   ";
    "#### # ##### # ####";
    "#        #        #";
    "# ## ### # ### ## #";
    "#  #     P     #  #";
    "## # # ##### # # ##";
    "#    #   #   #    #";
    "# ###### # ###### #";
    "#                 #";
    "###################" ]

let t = 40
let maze = Tilemap.of_strings (float_of_int t) maze_rows
let cols = Tilemap.cols maze
let rows = Tilemap.rows maze
let grid : Grid_move.grid = { tile = t; cols; rows }
let bounds = Tilemap.bounds maze
let open_ ((col, row) : int * int) : bool = match Tilemap.get maze col row with Some '#' | None -> false | Some _ -> true

(*****************************************************************************)
(* The ghosts' rules *)
(*****************************************************************************)

type name = Blinky | Pinky | Inky | Clyde

let names = [ Blinky; Pinky; Inky; Clyde ]

let color_of = function
  | Blinky -> rgb 255 0 0
  | Pinky -> rgb 255 184 255
  | Inky -> rgb 0 255 255
  | Clyde -> rgb 255 184 82

(* the corners, outside the maze, the ghosts scatter to *)
let corner (n : name) : int * int =
  match n with Blinky -> (cols - 3, -3) | Pinky -> (2, -3) | Inky -> (cols - 1, rows) | Clyde -> (0, rows)

(* n tiles ahead of Pac-Man, and n to the left too when he faces up *)
let ahead (pac : Grid_move.mover) (n : int) : int * int =
  let col, row = Grid_move.tile_of grid pac in
  let dc, dr = Grid_move.delta pac.dir in
  let bug = if pac.dir = Up then -n else 0 in
  (col + (dc * n) + bug, row + (dr * n))

let dist2 ((a, b) : int * int) ((c, d) : int * int) : int = ((a - c) * (a - c)) + ((b - d) * (b - d))

let target ~(chase : bool) ~(pac : Grid_move.mover) ~(blinky : Grid_move.mover) (n : name) (me : Grid_move.mover) : int * int =
  if not chase then corner n
  else
    let p = Grid_move.tile_of grid pac in
    match n with
    | Blinky -> p
    | Pinky -> ahead pac 4
    | Inky ->
        let xc, xr = ahead pac 2 and bc, br = Grid_move.tile_of grid blinky in
        ((2 * xc) - bc, (2 * xr) - br)
    | Clyde -> if dist2 (Grid_move.tile_of grid me) p > 64 then p else corner n

(* the waves, as ai/Fsm's machine: the arcade's first level *)
type wave = Scatter of int | Chase of int

let schedule = [ 420; 1200; 420; 1200; 300; 1200; 300 ]

let waves : (wave, unit) Fsm.machine =
  List.mapi
    (fun i frames : (wave, unit) Fsm.rule ->
      let k = (i / 2) + 1 in
      if i mod 2 = 0 then { from = Scatter k; label = "chase"; guard = Fsm.after frames; target = Chase k }
      else { from = Chase k; label = "scatter"; guard = Fsm.after frames; target = Scatter (k + 1) })
    schedule

(*****************************************************************************)
(* Model *)
(*****************************************************************************)

type model = { pac : Grid_move.mover; ghosts : (name * Grid_move.mover) list; wave : wave Fsm.run }

let initial_model =
  let pac = match Tilemap.find maze 'P' with p :: _ -> p | [] -> (9, 15) in
  { pac = Grid_move.mover_at grid pac;
    ghosts = [ (Blinky, Grid_move.mover_at grid (9, 7)); (Pinky, Grid_move.mover_at grid (8, 7)); (Inky, Grid_move.mover_at grid (4, 9));
               (Clyde, Grid_move.mover_at grid (14, 9)) ];
    wave = Fsm.start (Scatter 1) }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let chasing (m : model) : bool = match m.wave.state with Chase _ -> true | Scatter _ -> false

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let wanted =
    if k.kup then Grid_move.Up
    else if k.kdown then Grid_move.Down
    else if k.kleft then Grid_move.Left
    else if k.kright then Grid_move.Right
    else m.pac.wanted
  in
  let pac = Grid_move.move_player grid ~open_ 5 { m.pac with wanted } in
  let wave = Fsm.step waves () m.wave in
  let blinky = List.assoc Blinky m.ghosts in
  let chase = chasing m in
  let ghosts =
    List.map
      (fun (n, (g : Grid_move.mover)) ->
        (* a change of wave: every ghost turns round, the player's cue *)
        let g = if wave.fired <> None then { g with dir = Grid_move.opposite g.dir } else g in
        let goal = target ~chase ~pac ~blinky n g in
        (n, Grid_move.slide grid ~choose:(Chase.toward grid ~open_ ~goal) 4 g))
      m.ghosts
  in
  { pac; ghosts; wave }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* a tile's center, on the screen *)
let at ((col, row) : int * int) : number * number =
  (bounds.left +. (float_of_int t *. (float_of_int col +. 0.5)), bounds.top -. (float_of_int t *. (float_of_int row +. 0.5)))

let segment (color : color) (w : number) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape =
  let dx = x2 -. x1 and dy = y2 -. y1 in
  rectangle color (Float.hypot dx dy) w
  |> rotate (Float.atan2 dy dx *. 180. /. Float.pi)
  |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.)

(* a target: a square outline in the ghost's colour *)
let target_mark (color : color) ((x, y) : number * number) : shape =
  group
    [ rectangle color 34. 4. |> move_y 15.; rectangle color 34. 4. |> move_y (-15.); rectangle color 4. 34. |> move_x 15.;
      rectangle color 4. 34. |> move_x (-15.) ]
  |> move x y

let ring (color : color) (r : number) : shape =
  group (List.init 64 (fun i -> let a = float_of_int i *. Float.pi /. 32. in circle color 2. |> move (r *. cos a) (r *. sin a)))

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  let world g = Grid_move.to_world grid bounds g in
  let pac_xy = world m.pac and blinky = List.assoc Blinky m.ghosts in
  let chase = chasing m in
  let reasoning (n, g) =
    let c = color_of n in
    let goal = at (target ~chase ~pac:m.pac ~blinky n g) in
    let gx, gy = world g in
    let extra =
      if not chase then []
      else
        match n with
        | Pinky -> [ segment c 2. pac_xy goal ]
        | Inky ->
            (* the point 2 ahead of Pac-Man, and the line from Blinky
             * through it, doubled *)
            let x = at (ahead m.pac 2) in
            [ circle c 6. |> move (fst x) (snd x); segment c 2. (world blinky) goal |> fade 0.6 ]
        | Clyde -> [ ring c (8. *. float_of_int t) |> move gx gy |> fade 0.6 ]
        | Blinky -> []
    in
    extra @ [ segment c 1.5 (gx, gy) goal |> fade 0.5; target_mark c goal ]
  in
  let ghost (n, g) = let x, y = world g in group [ circle (color_of n) 16.; rectangle (color_of n) 32. 14. |> move_y (-9.) ] |> move x y in
  let timeline =
    (* each wave a bar as long as its time, the current one lit *)
    let total = float_of_int (List.fold_left ( + ) 0 schedule) in
    let width = 700. in
    let _, bars =
      List.fold_left
        (fun (x, acc) (i, frames) ->
          let w = width *. float_of_int frames /. total in
          let state = if i mod 2 = 0 then Scatter ((i / 2) + 1) else Chase ((i / 2) + 1) in
          let lit = m.wave.state = state in
          let color = if i mod 2 = 0 then rgb 90 90 200 else rgb 200 60 60 in
          (x +. w, (rectangle color (w -. 2.) 16. |> fade (if lit then 1. else 0.35) |> move (x +. (w /. 2.)) 0.) :: acc))
        (-.width /. 2., [])
        (List.mapi (fun i f -> (i, f)) schedule)
    in
    group bars |> move_y (screen.bottom +. 16.)
  in
  let wave_name = match m.wave.state with Scatter k -> Printf.sprintf "SCATTER %d: each to its corner" k | Chase k -> Printf.sprintf "CHASE %d: each by its rule" k in
  [ rectangle black screen.width screen.height;
    Sprite.pixels (float_of_int t) [ ('#', rgb 33 33 200) ] maze_rows |> move ((bounds.left +. bounds.right) /. 2.) ((bounds.top +. bounds.bottom) /. 2.) ]
  @ List.concat_map reasoning m.ghosts
  @ List.map ghost m.ghosts
  @ [ circle (rgb 255 230 0) 16. |> move (fst pac_xy) (snd pac_xy);
      timeline;
      text white 2. (Printf.sprintf "%s   (%d frames in it)" wave_name m.wave.since) |> move_y (screen.bottom +. 48.);
      text white 2. "each ghost's target, in its colour: arrows to move Pac-Man" |> move_y (screen.top -. 25.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app
