(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of X-COM: UFO Defense (Julian Gollop, Mythos Games /
 * MicroProse, 1994), its battles: four soldiers landed by a crashed
 * UFO, five aliens somewhere around it, and a fight in turns. Click a
 * soldier (or n for the next one), click a tile to walk there, click an
 * alien you can see to shoot it, right-click to turn and look that way;
 * s for a snap shot, a for an aimed one; Enter ends your turn.
 *
 * X-COM had two halves, a world map where you build bases, research
 * what you find and send interceptors after UFOs, and the battles,
 * which this toy is. It is where the squad tactics games come from --
 * Jagged Alliance, Firaxis's XCOM (2012), Into the Breach -- and what
 * it gave them is here:
 *
 *  - Time units ([step_cost], [shot_cost]): each soldier has 50 a turn,
 *    and everything spends them -- a step 4 (6 diagonally), a snap shot
 *    13, an aimed one 25, turning round 1. Moving far and shooting are a trade; a
 *    soldier who keeps some back can fire in the aliens' turn.
 *
 *  - The chance to hit, shown before you shoot ([hit_chance]): the
 *    soldier's accuracy, times the shot's (a snap shot is quick and
 *    wild, an aimed one slow and sure), less for the distance, less
 *    again if a hedge is between. A miss that the hedge was in the way
 *    of hits the hedge, which may go: cover wears away.
 *
 *  - What each soldier sees ([sees]): nine tiles ahead, in a cone of 90
 *    degrees the way it faces, walls in the way (a line of tiles,
 *    Bresenham's, looked along). An alien no one sees isn't shown; a
 *    tile no one ever saw is black. The squad knows what its soldiers
 *    see, nothing more.
 *
 *              . . . . . .
 *            . . . . . . .      a soldier facing right sees the
 *      S > . . . . . # . .      cone, not behind the wall, not
 *            . . . . . . .      behind itself
 *              . . . . . .
 *
 *  - Reaction fire ([react]): an enemy who sees you move, and has time
 *    units left, may shoot you in the middle of your move -- the more
 *    of its turn it kept back, the likelier. It is what makes every
 *    step into the open a decision, and the aliens get it too.
 *
 * The dice are a hash of the shot count, so a battle replays the same
 * (the tests, the golden frames).
 *
 * What it uses: ai/Pathfind (A*, the soldiers' and the aliens' ways,
 * with a step's time units as its cost), Tilemap (the battlefield, its
 * hedges destroyed as they are shot), Scene2d.
 *
 * Exercises: the world map (bases, research, the UFO to intercept);
 * turning costing a time unit per eighth of a turn, as in the
 * original; grenades, and walls
 * that explosions knock down; kneeling (more accurate, less time);
 * the alien terror of the Chryssalid; morale and panic; the night
 * missions, where seeing is half the battle.
 *)
open Playground

(*****************************************************************************)
(* The battlefield *)
(*****************************************************************************)

(* '#' a wall (blocks moving and seeing), 'o' a hedge (blocks moving,
 * not seeing: cover), 'S' a soldier, 'A' an alien, '.' the ground: the
 * squad's landing on the left, a farm in the middle, the UFO's hull on
 * the right *)
let map_text =
  [ "############################";
    "#..........................#";
    "#.SS.......o....#####......#";
    "#.SS.......o....#...#......#";
    "#..........o....#...#..A...#";
    "#...............##.##......#";
    "#......ooo.................#";
    "#..................#######.#";
    "#.........o........#.....#.#";
    "#.........o.....A..#..A..#.#";
    "#.......####.......#.....#.#";
    "#.......#..#.......###.###.#";
    "#.......#..#...............#";
    "#..ooo..##.#......ooo...A..#";
    "#..............A...........#";
    "#..........................#";
    "############################" ]

let tile = 34.
let field : Tilemap.t = Tilemap.of_strings tile (List.map (String.map (fun c -> if c = 'S' || c = 'A' then '.' else c)) map_text)
let cols = Tilemap.cols field
let rows = Tilemap.rows field

let starts (c : char) : (int * int) list = Tilemap.find (Tilemap.of_strings tile map_text) c

(*****************************************************************************)
(* The units *)
(*****************************************************************************)

type side = Squad | Aliens

type unit_ = {
  id : int;
  side : side;
  name : string;
  at : int * int; (* col, row *)
  facing : int * int; (* one of the 8 directions *)
  tu : int;
  max_tu : int;
  hp : int;
  accuracy : int; (* percent *)
  reactions : int;
}

let alive (u : unit_) : bool = u.hp > 0

(* a step's cost, straight or diagonal *)
let step_cost ((c1, r1) : int * int) ((c2, r2) : int * int) : int = if c1 <> c2 && r1 <> r2 then 6 else 4

type mode = Snap | Aimed

let shot_cost (u : unit_) (m : mode) : int = match m with Snap -> u.max_tu * 26 / 100 | Aimed -> u.max_tu / 2

(*****************************************************************************)
(* Seeing *)
(*****************************************************************************)

let blocks_sight (m : Tilemap.t) ((c, r) : int * int) : bool = Tilemap.get m c r = Some '#'
let blocks_move (m : Tilemap.t) ((c, r) : int * int) : bool = match Tilemap.get m c r with Some '.' -> false | _ -> true

(* the tiles from a to b, Bresenham's line, both ends included *)
let line ((c0, r0) : int * int) ((c1, r1) : int * int) : (int * int) list =
  let dc = abs (c1 - c0) and dr = -abs (r1 - r0) in
  let sc = if c0 < c1 then 1 else -1 and sr = if r0 < r1 then 1 else -1 in
  let rec go c r err acc =
    let acc = (c, r) :: acc in
    if c = c1 && r = r1 then List.rev acc
    else
      let e2 = 2 * err in
      let c, err = if e2 >= dr then (c + sc, err + dr) else (c, err) in
      let r, err = if e2 <= dc then (r + sr, err + dc) else (r, err) in
      go c r err acc
  in
  go c0 r0 (dc + dr) []

(* nothing that blocks sight between a and b (the ends themselves may
 * be anything) *)
let clear_line (m : Tilemap.t) (a : int * int) (b : int * int) : bool =
  let inner = List.filter (fun t -> t <> a && t <> b) (line a b) in
  not (List.exists (blocks_sight m) inner)

let sight = 9

(* what [u] sees of tile [t]: near enough, in its cone of 90 degrees
 * (the angle between where it faces and where t is, under 45), a clear
 * line to it; its own tile and the ones next to it always *)
let sees (m : Tilemap.t) (u : unit_) ((c, r) as t : int * int) : bool =
  let uc, ur = u.at in
  let dc = c - uc and dr = r - ur in
  let d2 = (dc * dc) + (dr * dr) in
  if d2 <= 2 then true
  else if d2 > sight * sight then false
  else
    let fc, fr = u.facing in
    let dot = float_of_int ((dc * fc) + (dr * fr)) in
    let cos_angle = dot /. (sqrt (float_of_int d2) *. sqrt (float_of_int ((fc * fc) + (fr * fr)))) in
    cos_angle >= cos (Float.pi /. 4.) -. 0.01 && clear_line m u.at t

(*****************************************************************************)
(* Shooting *)
(*****************************************************************************)

(* the hedge that stands between a target and a shooter: the tile next
 * to the target on the line from it back to the shooter *)
let cover_between (m : Tilemap.t) (shooter : int * int) (target : int * int) : (int * int) option =
  match List.rev (line shooter target) with
  | _ :: next :: _ when Tilemap.get m (fst next) (snd next) = Some 'o' -> Some next
  | _ -> None

(* The chance to hit, percent: the accuracy, times the shot's (a snap
 * 0.6, an aimed 1.1), less 2 points a tile beyond 6, less 25 behind a
 * hedge; between 5 and 95 *)
let hit_chance (m : Tilemap.t) (shooter : unit_) (target : unit_) (mode : mode) : int =
  let c1, r1 = shooter.at and c2, r2 = target.at in
  let d = sqrt (float_of_int (((c1 - c2) * (c1 - c2)) + ((r1 - r2) * (r1 - r2)))) in
  let base = float_of_int shooter.accuracy *. (match mode with Snap -> 0.6 | Aimed -> 1.1) in
  let far = 2. *. Float.max 0. (d -. 6.) in
  let hedge = if cover_between m shooter.at target.at <> None then 25. else 0. in
  max 5 (min 95 (int_of_float (base -. far -. hedge)))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type shot = { from_ : int * int; to_ : int * int; hit : bool; frames : int }

type phase =
  | Squad_turn
  | Walking of int * (int * int) list (* a soldier, the steps left *)
  | Aliens_turn of int list * int (* the aliens still to act, frames before the next action *)
  | Won
  | Lost

type game = {
  map : Tilemap.t;
  units : unit_ list;
  selected : int;
  mode : mode;
  phase : phase;
  seen : (int * int) list; (* the tiles the squad ever saw *)
  shots : shot list; (* being drawn *)
  rolls : int; (* the dice rolled so far *)
  turn : int;
  news : string;
}

type scene = Title | Battle of game
type model = scene Scene2d.t

let new_game () : game =
  let soldier i at =
    { id = i; side = Squad; name = List.nth [ "Ana"; "Bo"; "Cyd"; "Dee" ] i; at; facing = (1, 0); tu = 50; max_tu = 50; hp = 40; accuracy = 65;
      reactions = 55 }
  in
  let alien i at = { id = 10 + i; side = Aliens; name = "sectoid"; at; facing = (-1, 0); tu = 54; max_tu = 54; hp = 30; accuracy = 60; reactions = 60 } in
  let g =
    { map = field; units = List.mapi soldier (starts 'S') @ List.mapi alien (starts 'A'); selected = 0; mode = Aimed; phase = Squad_turn;
      seen = []; shots = []; rolls = 0; turn = 1; news = "click a soldier, click where it goes; click an alien you see to shoot" }
  in
  g

let unit_by (g : game) (id : int) : unit_ = List.find (fun u -> u.id = id) g.units
let set_unit (g : game) (u : unit_) : game = { g with units = List.map (fun v -> if v.id = u.id then u else v) g.units }
let enemies (g : game) (side : side) : unit_ list = List.filter (fun u -> u.side <> side && alive u) g.units
let unit_at (g : game) (t : int * int) : unit_ option = List.find_opt (fun u -> alive u && u.at = t) g.units

(* what the squad sees now, and has ever seen *)
let visible (g : game) : (int * int) list =
  let soldiers = List.filter (fun u -> u.side = Squad && alive u) g.units in
  List.concat_map
    (fun r -> List.filter_map (fun c -> if List.exists (fun s -> sees g.map s (c, r)) soldiers then Some (c, r) else None) (List.init cols Fun.id))
    (List.init rows Fun.id)

let reveal (g : game) : game = { g with seen = List.sort_uniq compare (visible g @ g.seen) }

(* a die of 100 faces, rolled: a hash of the count of rolls *)
let roll (g : game) : int * game = (Hashtbl.hash (g.rolls, 7919) mod 100, { g with rolls = g.rolls + 1 })

(* A shot: the time units spent, the die rolled against the chance; a
 * hit wounds, a miss the hedge was in the way of hits the hedge, which
 * goes one time in two *)
let shoot (g : game) (shooter : unit_) (target : unit_) (mode : mode) : game =
  let chance = hit_chance g.map shooter target mode in
  let d, g = roll g in
  let dc, dr = (fst target.at - fst shooter.at, snd target.at - snd shooter.at) in
  let facing = (compare dc 0, compare dr 0) in
  let g = set_unit g { shooter with tu = shooter.tu - shot_cost shooter mode; facing } in
  let hit = d < chance in
  let g = { g with shots = { from_ = shooter.at; to_ = target.at; hit; frames = 20 } :: g.shots } in
  if hit then begin
    Audio.play Audio.hit;
    let dmg, g = roll g in
    let target = unit_by g target.id in
    let hp = max 0 (target.hp - (20 + (dmg mod 21))) in
    let g = set_unit g { target with hp } in
    { g with news = Printf.sprintf "%s hits %s (%d%%)%s" shooter.name target.name chance (if hp = 0 then ": down" else "") }
  end
  else begin
    Audio.play Audio.laser;
    match cover_between g.map shooter.at target.at with
    | Some (c, r) ->
        let lost, g = roll g in
        let g = if lost < 50 then { g with map = Tilemap.set g.map c r '.' } else g in
        { g with news = Printf.sprintf "%s misses (%d%%): the hedge takes it%s" shooter.name chance (if lost < 50 then ", and goes" else "") }
    | None -> { g with news = Printf.sprintf "%s misses (%d%%)" shooter.name chance }
  end

(* Reaction fire: every enemy who sees [mover] now, with the time for a
 * snap shot, and more of its turn kept back than the mover has (each
 * weighted by its reactions), fires *)
let react (g : game) (mover : unit_) : game =
  List.fold_left
    (fun g (e : unit_) ->
      let e = unit_by g e.id and mover = unit_by g mover.id in
      if not (alive mover && alive e) then g
      else
        let score (u : unit_) = u.reactions * u.tu / u.max_tu in
        if e.tu >= shot_cost e Snap && sees g.map e mover.at && score e > score mover then
          let g = shoot g e mover Snap in
          { g with news = "REACTION FIRE: " ^ g.news }
        else g)
    g (enemies g mover.side)

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

let problem (g : game) (goal : int * int) : (int * int) Pathfind.problem =
  { neighbors =
      (fun (c, r) ->
        List.filter_map
          (fun (dc, dr) ->
            let t = (c + dc, r + dr) in
            (* no cutting a wall's corner diagonally *)
            let corner = dc <> 0 && dr <> 0 && (blocks_move g.map (c + dc, r) || blocks_move g.map (c, r + dr)) in
            if blocks_move g.map t || corner || (unit_at g t <> None && t <> goal) then None else Some (t, float_of_int (step_cost (c, r) t)))
          [ (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1) ]);
    goal = (fun t -> t = goal);
    estimate = (fun (c, r) -> 4. *. float_of_int (max (abs (c - fst goal)) (abs (r - snd goal)))) }

(* the steps of the way to [goal] the time units pay for *)
let affordable (g : game) (u : unit_) (goal : int * int) : (int * int) list =
  if blocks_move g.map goal || unit_at g goal <> None then []
  else
    match (Pathfind.astar (problem g goal) u.at).path with
    | [] | [ _ ] -> []
    | _ :: steps ->
        let rec take at tu = function
          | [] -> []
          | s :: rest -> let c = step_cost at s in if c > tu then [] else s :: take s (tu - c) rest
        in
        take u.at u.tu steps

(* one step: the time units, the facing, then what it sees and who sees
 * it; a new enemy in sight, or a shot, and the walk stops *)
let walk_step (g : game) (id : int) (next : int * int) : game * bool =
  let u = unit_by g id in
  let before = List.length (List.filter (fun e -> List.exists (fun s -> s.side = u.side && alive s && sees g.map s e.at) g.units) (enemies g u.side)) in
  let facing = (compare (fst next - fst u.at) 0, compare (snd next - snd u.at) 0) in
  let g = set_unit g { u with at = next; tu = u.tu - step_cost u.at next; facing } in
  let g = if u.side = Squad then reveal g else g in
  let shots = g.rolls in
  let g = react g (unit_by g id) in
  let after = List.length (List.filter (fun e -> List.exists (fun s -> s.side = u.side && alive s && sees g.map s e.at) g.units) (enemies g u.side)) in
  (g, g.rolls <> shots || after > before || not (alive (unit_by g id)))

(*****************************************************************************)
(* The aliens *)
(*****************************************************************************)

(* An alien's action: shoot the nearest soldier it sees, aimed if it can
 * afford it; else walk towards the nearest soldier, keeping a snap
 * shot's time back for its reaction fire; else nothing *)
let alien_action (g : game) (id : int) : game option =
  let a = unit_by g id in
  if not (alive a) then None
  else
    let dist (u : unit_) = abs (fst u.at - fst a.at) + abs (snd u.at - snd a.at) in
    let soldiers = List.sort (fun x y -> compare (dist x) (dist y)) (enemies g Aliens) in
    match List.find_opt (fun s -> sees g.map a s.at) soldiers with
    | Some s when a.tu >= shot_cost a Aimed -> Some (shoot g a s Aimed)
    | Some s when a.tu >= shot_cost a Snap -> Some (shoot g a s Snap)
    | Some _ -> None
    | None -> (
        match soldiers with
        | [] -> None
        | s :: _ ->
            let keep = shot_cost a Snap in
            let budget = { a with tu = a.tu - keep } in
            let near = List.filter (fun t -> not (blocks_move g.map t) && unit_at g t = None) (List.map (fun (dc, dr) -> (fst s.at + dc, snd s.at + dr)) [ (1, 0); (-1, 0); (0, 1); (0, -1) ]) in
            match List.find_map (fun t -> match affordable g budget t with [] -> None | steps -> Some steps) near with
            | Some (first :: _) when a.tu - step_cost a.at first >= keep -> Some (fst (walk_step g id first))
            | _ -> (
                (* too far for a way there this turn: one step towards it *)
                match List.find_map (fun t -> match (Pathfind.astar (problem g t) a.at).path with _ :: next :: _ -> Some next | _ -> None) near with
                | Some next when a.tu - step_cost a.at next >= keep -> Some (fst (walk_step g id next))
                | _ -> None))

(* the selected soldier, or the next one alive when it is down *)
let reselect (g : game) : game =
  if alive (unit_by g g.selected) then g
  else match List.find_opt (fun u -> u.side = Squad && alive u) g.units with Some u -> { g with selected = u.id } | None -> g

let new_turn (g : game) : game =
  let g = { g with units = List.map (fun u -> { u with tu = u.max_tu }) g.units; turn = g.turn + 1; phase = Squad_turn } in
  reselect (reveal g)

let check_end (g : game) : game =
  if enemies g Squad = [] then { g with phase = Won; news = "MISSION SUCCESSFUL" }
  else if enemies g Aliens = [] then { g with phase = Lost; news = "THE SQUAD IS LOST" }
  else g

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let cell_of (x : number) (y : number) : int * int = Tilemap.cell field x y

(* what the squad's click means: a soldier selected, an alien seen shot,
 * a tile walked to *)
let click (g : game) (t : int * int) : game =
  let s = unit_by g g.selected in
  match unit_at g t with
  | Some u when u.side = Squad -> { g with selected = u.id }
  | Some u when List.mem t (visible g) ->
      if not (alive s) then g
      else if s.tu < shot_cost s g.mode then { g with news = Printf.sprintf "%s hasn't the time units for that shot" s.name }
      else check_end (shoot g s u g.mode)
  | Some _ -> g
  | None -> (
      match affordable g s t with
      | [] -> { g with news = "no way there this turn" }
      | steps -> { g with phase = Walking (s.id, steps) })

(* a right-click: the selected soldier turns to look at [t] *)
let turn_to (g : game) ((c, r) : int * int) : game =
  let s = unit_by g g.selected in
  let facing = (compare (c - fst s.at) 0, compare (r - snd s.at) 0) in
  if not (alive s) || facing = (0, 0) || facing = s.facing || s.tu < 1 then g
  else check_end (react (reveal (set_unit g { s with facing; tu = s.tu - 1 })) s)

let step (g : game) : game =
  let g = { g with shots = List.filter_map (fun s -> if s.frames > 1 then Some { s with frames = s.frames - 1 } else None) g.shots } in
  match g.phase with
  | Walking (id, next :: rest) ->
      let g, stopped = walk_step g id next in
      check_end { g with phase = (if stopped || rest = [] then Squad_turn else Walking (id, rest)) }
  | Walking (_, []) -> { g with phase = Squad_turn }
  | Aliens_turn (_, wait) when wait > 0 -> (match g.phase with Aliens_turn (l, _) -> { g with phase = Aliens_turn (l, wait - 1) } | _ -> g)
  | Aliens_turn ([], _) -> new_turn g
  | Aliens_turn (id :: rest, _) -> (
      match alien_action g id with
      | Some g' -> check_end { g' with phase = Aliens_turn (id :: rest, 12) }
      | None -> { g with phase = Aliens_turn (rest, 6) })
  | Squad_turn | Won | Lost -> g

let end_turn (g : game) : game =
  let aliens = List.filter_map (fun u -> if u.side = Aliens && alive u then Some u.id else None) g.units in
  { g with phase = Aliens_turn (aliens, 20); news = "the aliens' turn" }

let initial_model : model = Scene2d.start Title

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let key f = Scene2d.pressed f m in
  let letter l = key (fun k -> Set_.mem l k.keys) in
  match m.scene with
  | Title -> if key (fun k -> k.kspace) then Scene2d.go (Battle (reveal (new_game ()))) m else m
  | Battle g ->
      let g =
        match g.phase with
        | Squad_turn ->
            let g = if letter "s" then { g with mode = Snap } else if letter "a" then { g with mode = Aimed } else g in
            let squad = List.filter (fun u -> u.side = Squad && alive u) g.units in
            let g =
              if key (fun k -> Set_.mem "n" k.keys) then
                match List.find_opt (fun u -> u.id > g.selected) squad with Some u -> { g with selected = u.id } | None -> (match squad with u :: _ -> { g with selected = u.id } | [] -> g)
              else g
            in
            if key (fun k -> k.kenter) then end_turn g
            else
              let t = cell_of computer.mouse.mx computer.mouse.my in
              let inside = fst t >= 0 && fst t < cols && snd t >= 0 && snd t < rows in
              if inside && computer.mouse.mclick then click g t
              else if inside && computer.mouse.mrdown then turn_to g t
              else g
        | _ -> g
      in
      let g = reselect (step g) in
      if (g.phase = Won || g.phase = Lost) && key (fun k -> k.kspace) then Scene2d.go Title m else { m with scene = Battle g }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let center ((c, r) : int * int) : number * number = Tilemap.center field c r

let view_field (g : game) (seen_now : (int * int) list) : shape list =
  List.concat
    (List.init rows (fun r ->
         List.filter_map
           (fun c ->
             let x, y = center (c, r) in
             if not (List.mem (c, r) g.seen) then Some (square black tile |> move x y)
             else
               let base =
                 match Tilemap.get g.map c r with
                 | Some '#' -> group [ square (rgb 110 105 100) tile; square (rgb 140 135 125) (tile -. 6.) ]
                 | Some 'o' -> group [ square (rgb 95 130 60) tile; circle (rgb 60 110 45) (tile *. 0.42) ]
                 | _ -> square (if (c + r) mod 2 = 0 then rgb 120 150 80 else rgb 115 145 76) tile
               in
               Some (if List.mem (c, r) seen_now then base |> move x y else group [ base; square black tile |> fade 0.45 ] |> move x y))
           (List.init cols Fun.id)))

let view_unit (g : game) (seen_now : (int * int) list) (u : unit_) : shape list =
  if not (alive u) then
    (* where it fell, if the squad saw that tile *)
    if List.mem u.at g.seen then let x, y = center u.at in [ oval (if u.side = Squad then rgb 60 40 40 else rgb 70 90 60) 26. 14. |> move x y ] else []
  else if u.side = Aliens && not (List.mem u.at seen_now) then []
  else
    let x, y = center u.at in
    let fc, fr = u.facing in
    let body =
      match u.side with
      | Squad -> group [ circle (rgb 50 80 170) 12.; circle (rgb 230 200 160) 5. |> move 0. 2. ]
      | Aliens -> group [ oval (rgb 170 170 180) 22. 26.; oval black 6. 9. |> move (-4.) 3.; oval black 6. 9. |> move 4. 3. ]
    in
    let sel = u.id = g.selected && u.side = Squad in
    (if sel then [ circle (rgb 250 220 60) 16. |> fade 0.6 |> move x y ] else [])
    @ [ body |> move x y; rectangle white 8. 2. |> rotate (atan2 (-.float_of_int fr) (float_of_int fc) *. 180. /. Float.pi) |> move (x +. (12. *. float_of_int fc)) (y -. (12. *. float_of_int fr)) ]
    @ [ rectangle (rgb 60 60 60) 26. 4. |> move x (y -. 17.); rectangle (if u.side = Squad then rgb 90 200 90 else rgb 220 80 80) (26. *. float_of_int u.hp /. float_of_int (if u.side = Squad then 40 else 30)) 4. |> move x (y -. 17.) ]

let view_battle (computer : computer) (g : game) : shape list =
  let seen_now = visible g in
  let s = unit_by g g.selected in
  let hover = cell_of computer.mouse.mx computer.mouse.my in
  (* what a click there would do: a way and its cost, or a shot's chance *)
  let preview =
    if g.phase <> Squad_turn || not (alive s) then []
    else
      match unit_at g hover with
      | Some u when u.side = Aliens && List.mem hover seen_now ->
          let x, y = center hover in
          [ text yellow 1.4 (Printf.sprintf "SNAP %d%%  AIMED %d%%" (hit_chance g.map s u Snap) (hit_chance g.map s u Aimed)) |> move x (y +. 26.) ]
      | Some _ -> []
      | None ->
          let steps = affordable g s hover in
          let cost = List.fold_left (fun (at, sum) t -> (t, sum + step_cost at t)) (s.at, 0) steps |> snd in
          List.map (fun t -> let x, y = center t in circle (rgb 250 240 120) 3. |> move x y) steps
          @ if steps = [] then [] else (let x, y = center hover in [ text white 1.3 (Printf.sprintf "%d TU" cost) |> move x (y +. 16.) ])
  in
  let shots =
    List.map
      (fun sh ->
        let x1, y1 = center sh.from_ and x2, y2 = center sh.to_ in
        let dx = x2 -. x1 and dy = y2 -. y1 in
        rectangle (if sh.hit then rgb 255 80 60 else rgb 255 240 150) (Float.hypot dx dy) 3. |> rotate (atan2 dy dx *. 180. /. Float.pi) |> fade (float_of_int sh.frames /. 20.) |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.))
      g.shots
  in
  view_field g seen_now
  @ List.concat_map (view_unit g seen_now) g.units
  @ preview @ shots
  @ [ text white 2.2 (Printf.sprintf "TURN %d" g.turn) |> move (-400.) 330.;
      text (rgb 250 230 140) 1.7 g.news |> move 60. 330.;
      text white 1.9
        (Printf.sprintf "%s   TU %d/%d   HP %d   shot: %s (s snap %d TU, a aimed %d TU)" s.name s.tu s.max_tu s.hp
           (match g.mode with Snap -> "SNAP" | Aimed -> "AIMED") (shot_cost s Snap) (shot_cost s Aimed))
      |> move 0. (-330.);
      text (rgb 180 190 210) 1.6 "click a soldier, a tile to walk to, an alien to shoot   right-click turn   n next soldier   Enter end turn" |> move 0. (-370.) ]
  @ (match g.phase with
    | Aliens_turn _ -> [ text (rgb 230 90 90) 3. "ALIENS' TURN" |> move 0. 380. ]
    | Won -> [ rectangle black 600. 120. |> fade 0.7; text (rgb 120 230 140) 4. "MISSION SUCCESSFUL" |> move_y 15.; text white 2. "space" |> move_y (-35.) ]
    | Lost -> [ rectangle black 600. 120. |> fade 0.7; text (rgb 230 90 90) 4. "THE SQUAD IS LOST" |> move_y 15.; text white 2. "space" |> move_y (-35.) ]
    | _ -> [])

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 25 28 36) screen.width screen.height
  ::
  (match m.scene with
  | Title ->
      [ text (rgb 120 170 250) 6. "TINY X-COM" |> move_y 180.;
        text white 2.2 "four soldiers, a crashed UFO, the aliens somewhere around it" |> move_y 90.;
        text white 2. "time units: every step and every shot spends them" |> move_y 40.;
        text white 2. "the chance to hit before you shoot; what you see, nothing more" |> move_y 0.;
        text white 2. "and the aliens shoot back in your turn, if you let them" |> move_y (-40.) ]
      @ Scene2d.blink 1. m [ text (rgb 250 220 60) 3. "PRESS SPACE" |> move_y (-160.) ]
  | Battle g -> view_battle computer g)

let app = game view update initial_model
let main = Playground_platform.run_app app
