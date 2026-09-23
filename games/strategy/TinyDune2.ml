(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Dune II (Westwood Studios, 1992), the game that
 * fixed what a real-time strategy game is: you harvest, you build, you
 * order units around by pointing at the ground, and the enemy does the
 * same at the other end of the map. Here: your refinery is on the left,
 * theirs on the right, the orange patches are spice.
 *
 * Click a unit of yours to select it, click the ground to send it
 * there; "b" builds a tank for 100 credits, "h" a harvester for 150.
 * The arrows and space do the same without a mouse; "p" draws the
 * selected unit's path, "f" runs faster. Your harvesters work on their
 * own. Destroy their refinery before they destroy yours.
 *
 * What's new here:
 *
 *  - Orders as paths (Pathfind.mli): an order is a click, and what
 *    the unit does with it is an A* search around the rocks ([order]).
 *    The 1992 original searched much more cheaply, and units famously
 *    got stuck on corners; ours don't, at the price of a search per
 *    order.
 *
 *  - A search whose goal is a question, not a place ([spice_run]): a
 *    harvester looks for "the nearest cell with spice in it", so the
 *    goal of the search is a predicate, and the frontier stops at
 *    whichever patch is closest through the rocks -- no list of
 *    distances to keep, and it follows the spice as it's eaten away.
 *
 *  - The loop that makes a strategy game ([harvest]): spice becomes
 *    credits, credits become units, units take more spice and shoot the
 *    other side's. Everything else here (combat, the enemy's attacks)
 *    is a few lines around that loop.
 *
 * What it uses: ai/'s Pathfind, Scene2d, Audio. Not the maze kit (its
 * Grid_move is one character on a fixed grid), not Physics: units slide
 * along their path at a constant speed, shots hit at once.
 *
 * Exercises: several units selected at once, and a flow field (one
 * Dijkstra from the goal) instead of an A* each, which is how later
 * games moved crowds; units that block each other, and what that does
 * to the paths; the fog of war (Warcraft II, 1995: draw only what a
 * unit can see); a build queue and more buildings; the worm.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The map *)
(*****************************************************************************)

type ground = Sand | Rock | Spice of int

let cols = 25
let rows = 17
let size = 38.
let left = -.(float_of_int cols * size / 2.)
let top = 330.

type terrain = ground array

let index ((x, y) : int * int) : int = (y *.. cols) +.. x
let inside ((x, y) : int * int) : bool = x >= 0 && x < cols && y >= 0 && y < rows
let at_cell (t : terrain) (c : int * int) : ground = t.(index c)
let passable (t : terrain) (c : int * int) : bool = inside c && at_cell t c <> Rock

let center ((x, y) : int * int) : number * number =
  (left + (size * (float_of_int x + 0.5)), top - (size * (float_of_int y + 0.5)))

let cell_at (mx : number) (my : number) : int * int =
  (int_of_float (Float.floor ((mx - left) / size)), int_of_float (Float.floor ((top - my) / size)))

(* the map: rocks in the middle and the corners, spice in four patches;
 * the same every game, so a player learns it (and a test can) *)
let new_terrain () : terrain =
  let t = Array.make (cols *.. rows) Sand in
  let rock (x, y) = if inside (x, y) then t.(index (x, y)) <- Rock in
  for y = 3 to 6 do rock (12, y); rock (13, y) done;
  for y = 10 to 13 do rock (11, y); rock (12, y) done;
  for x = 5 to 8 do rock (x, 2); rock (x, 14) done;
  for x = 16 to 19 do rock (x, 2); rock (x, 14) done;
  let patch (cx, cy) =
    for y = cy -.. 1 to cy +.. 1 do
      for x = cx -.. 1 to cx +.. 1 do
        if inside (x, y) && at_cell t (x, y) = Sand then t.(index (x, y)) <- Spice 6
      done
    done
  in
  List.iter patch [ (7, 5); (7, 11); (17, 5); (17, 11); (12, 8) ];
  t

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type side = Us | Them
type kind = Harvester | Tank

type unit_ = {
  id : int;
  kind : kind;
  side : side;
  x : number; (* in cells, fractional between two of them *)
  y : number;
  path : (int * int) list;
  hp : int;
  cargo : int; (* a harvester's load, 0 to 5 *)
  digging : int; (* frames left digging the cell it stands on *)
  cooldown : int; (* frames before it can fire again *)
}

type building = { bside : side; cell : int * int; bhp : int }
type shot = { from : number * number; to_ : number * number; age : int }

type game = {
  terrain : terrain;
  units : unit_ list;
  buildings : building list;
  shots : shot list;
  selected : int option; (* a unit's id *)
  next_id : int;
  cursor : int * int;
  credits : int;
  their_credits : int;
  frames : int;
  show_path : bool;
}

type scene = Title | Playing of game | Won of bool
type model = scene Scene2d.t

let our_base = (2, 8)
let their_base = (22, 8)
let tank_range = 3.2

(* The knobs, gathered so the game can be made easier or harder by
 * turning one number:
 *
 *  - how hard they push: [their_tank_every] (they buy a tank that often,
 *    if they can pay) and [their_tanks_max]. Left alone, they take an
 *    undefended refinery in about half a minute.
 *  - how long anything lasts: [refinery_hp], [tank_hp],
 *    [harvester_hp], and [shell] every [reload] frames.
 *  - the economy: [tank_cost], [harvester_cost], [full_load] (how many
 *    digs fill a harvester), [dig_frames] and [load_worth] (credits a
 *    full harvester brings).
 *)
let tank_cost = 100
let harvester_cost = 150
let full_load = 5
let dig_frames = 40
let load_worth = 25
let refinery_hp = 400
let tank_hp = 60
let harvester_hp = 80
let shell = 8
let reload = 30
let their_tank_every = 120
let their_tanks_max = 6

let cell_of (u : unit_) : int * int = Orders.cell_of (u.x, u.y)

let new_unit (id : int) (kind : kind) (side : side) ((x, y) : int * int) : unit_ =
  { id; kind; side; x = float_of_int x; y = float_of_int y; path = []; hp = (match kind with Tank -> tank_hp | Harvester -> harvester_hp);
    cargo = 0; digging = 0; cooldown = 0 }

let new_game () : game =
  { terrain = new_terrain ();
    units = [ new_unit 1 Harvester Us (4, 8); new_unit 2 Tank Us (4, 6); new_unit 3 Harvester Them (20, 8); new_unit 4 Tank Them (20, 10) ];
    buildings = [ { bside = Us; cell = our_base; bhp = refinery_hp }; { bside = Them; cell = their_base; bhp = refinery_hp } ];
    shots = []; selected = None; next_id = 5; cursor = (10, 8); credits = 150; their_credits = 150; frames = 0; show_path = true }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Orders: the paths *)
(*****************************************************************************)

(* the rocks are what a unit walks around; units don't block each other
 * (the original's did, and they jammed) *)
let walkable (t : terrain) : int * int -> bool = passable t

let order (t : terrain) (u : unit_) (target : int * int) : unit_ =
  { u with path = Orders.path ~walkable:(walkable t) ~from:(cell_of u) target }

(* the nearest spice, whichever it is: the goal is a question, so one
 * search finds both the patch and the way to it (no estimate to guide
 * it -- we don't know where we're going) *)
let spice_run (t : terrain) (u : unit_) : unit_ =
  { u with path = Orders.nearest ~walkable:(walkable t) ~from:(cell_of u) (fun c -> match at_cell t c with Spice n -> n > 0 | _ -> false) }

let base_of (side : side) : int * int = match side with Us -> our_base | Them -> their_base

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let speed (u : unit_) : number = match u.kind with Tank -> 0.055 | Harvester -> 0.04

let walk (u : unit_) : unit_ =
  let (x, y), path = Orders.advance ~speed:(speed u) (u.x, u.y) u.path in
  { u with x; y; path }

(* a harvester: dig where there's spice, carry it home, unload, go
 * again -- the loop the whole game runs on *)
let harvest (g : game) (u : unit_) : game * unit_ =
  let home = base_of u.side in
  if u.digging > 0 then
    let u = { u with digging = u.digging -.. 1 } in
    if u.digging > 0 then (g, u)
    else begin
      let c = cell_of u in
      (match at_cell g.terrain c with Spice n when n > 0 -> g.terrain.(index c) <- Spice (n -.. 1) | _ -> ());
      let u = { u with cargo = u.cargo +.. 1 } in
      if u.cargo >= full_load then (g, order g.terrain u home) else (g, spice_run g.terrain u)
    end
  else if u.path <> [] then (g, u)
  else if u.cargo >= full_load then
    if cell_of u = home then begin
      (* unloaded: 25 credits a load *)
      Audio.play Audio.coin;
      let g = if u.side = Us then { g with credits = g.credits +.. load_worth } else { g with their_credits = g.their_credits +.. load_worth } in
      (g, spice_run g.terrain { u with cargo = 0 })
    end
    else (g, order g.terrain u home)
  else
    match at_cell g.terrain (cell_of u) with
    | Spice n when n > 0 -> (g, { u with digging = dig_frames })
    | _ ->
        let u' = spice_run g.terrain u in
        (* no spice left anywhere: go home and wait *)
        (g, if u'.path = [] && cell_of u <> home then order g.terrain u home else u')

(* tanks shoot the nearest thing of the other side in range: a unit, or
 * a building *)
let fire (g : game) (u : unit_) : game * unit_ =
  if u.kind <> Tank || u.cooldown > 0 then (g, { u with cooldown = max 0 (u.cooldown -.. 1) })
  else
    let d (x, y) = Float.hypot (u.x - x) (u.y - y) in
    let unit_targets = List.filter (fun (v : unit_) -> v.side <> u.side && d (v.x, v.y) <= tank_range) g.units in
    let building_targets =
      List.filter (fun (b : building) -> b.bside <> u.side && d (float_of_int (fst b.cell), float_of_int (snd b.cell)) <= tank_range) g.buildings
    in
    match (unit_targets, building_targets) with
    | [], [] -> (g, u)
    | _ ->
        let hit_unit = unit_targets <> [] in
        let target_pos =
          if hit_unit then
            let v = List.fold_left (fun best (v : unit_) -> if d (v.x, v.y) < d (best.x, best.y) then v else best) (List.hd unit_targets) unit_targets in
            (v.x, v.y)
          else
            let b = List.hd building_targets in
            (float_of_int (fst b.cell), float_of_int (snd b.cell))
        in
        let units =
          List.map (fun (v : unit_) -> if hit_unit && (v.x, v.y) = target_pos && v.side <> u.side then { v with hp = v.hp -.. shell } else v) g.units
        in
        let buildings =
          List.map
            (fun (b : building) ->
              if (not hit_unit) && (float_of_int (fst b.cell), float_of_int (snd b.cell)) = target_pos then { b with bhp = b.bhp -.. shell } else b)
            g.buildings
        in
        Audio.play Audio.laser;
        ({ g with units; buildings; shots = { from = (u.x, u.y); to_ = target_pos; age = 0 } :: g.shots }, { u with cooldown = reload })

(* the enemy: a harvester of its own, and a tank sent at our refinery as
 * soon as it can pay for one *)
let their_turn (g : game) : game =
  let their_tanks = List.filter (fun (u : unit_) -> u.side = Them && u.kind = Tank) g.units in
  let g =
    if g.their_credits >= tank_cost && List.length their_tanks < their_tanks_max && g.frames mod their_tank_every = 0 then
      { g with their_credits = g.their_credits -.. tank_cost; next_id = g.next_id +.. 1;
        units = g.units @ [ new_unit g.next_id Tank Them their_base ] }
    else g
  in
  (* a tank with nothing to do walks at our refinery *)
  { g with
    units =
      List.map
        (fun (u : unit_) -> if u.side = Them && u.kind = Tank && u.path = [] && u.cooldown = 0 then order g.terrain u our_base else u)
        g.units }

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed key = Scene2d.pressed key scenes in
  let mouse = computer.mouse in
  let g = { g with frames = g.frames +.. 1 } in
  (* the cursor *)
  let x, y = g.cursor in
  let step key d = if pressed key then d else 0 in
  let cursor = (clamp 0 (cols -.. 1) (x +.. step (fun k -> k.kright) 1 +.. step (fun k -> k.kleft) (-1)),
                clamp 0 (rows -.. 1) (y +.. step (fun k -> k.kdown) 1 +.. step (fun k -> k.kup) (-1))) in
  let cursor = if mouse.mdx <> 0. || mouse.mdy <> 0. then (let c = cell_at mouse.mx mouse.my in if inside c then c else cursor) else cursor in
  let g = { g with cursor } in
  let g = if pressed (fun k -> Set_.mem "p" k.keys) then { g with show_path = not g.show_path } else g in
  (* building: a unit walks out of the refinery *)
  let g =
    if pressed (fun k -> Set_.mem "b" k.keys) && g.credits >= tank_cost then
      { g with credits = g.credits -.. tank_cost; next_id = g.next_id +.. 1; units = g.units @ [ new_unit g.next_id Tank Us our_base ] }
    else g
  in
  let g =
    if pressed (fun k -> Set_.mem "h" k.keys) && g.credits >= harvester_cost then
      { g with credits = g.credits -.. harvester_cost; next_id = g.next_id +.. 1;
        units = g.units @ [ new_unit g.next_id Harvester Us our_base ] }
    else g
  in
  (* selecting one of ours, or ordering the selected one to the cursor *)
  let g =
    if mouse.mclick || pressed (fun k -> k.kspace) then
      match List.find_opt (fun (u : unit_) -> u.side = Us && cell_of u = g.cursor) g.units with
      | Some u -> { g with selected = Some u.id }
      | None -> (
          match g.selected with
          | Some id when passable g.terrain g.cursor ->
              Audio.play Audio.blip;
              { g with units = List.map (fun u -> if u.id = id then order g.terrain u g.cursor else u) g.units }
          | _ -> g)
    else g
  in
  (* everyone moves, harvests, shoots *)
  let g = { g with units = List.map walk g.units } in
  let g, units =
    List.fold_left
      (fun (g, acc) (u : unit_) ->
        let g, u = if u.kind = Harvester then harvest g u else (g, u) in
        let g, u = fire g u in
        (g, u :: acc))
      (g, []) g.units
  in
  let g = { g with units = List.rev units } in
  let g = their_turn g in
  (* the dead *)
  let alive = List.filter (fun (u : unit_) -> u.hp > 0) g.units in
  let selected = match g.selected with Some id when List.exists (fun (u : unit_) -> u.id = id) alive -> Some id | _ -> None in
  { g with units = alive; selected;
    buildings = List.filter (fun b -> b.bhp > 0) g.buildings;
    shots = List.filter (fun s -> s.age < 5) (List.map (fun s -> { s with age = s.age +.. 1 }) g.shots) }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let fast = Set_.mem "f" computer.keyboard.keys in
      let g = update_game computer s g in
      let g = if fast then update_game computer s (update_game computer s g) else g in
      let ours = List.exists (fun b -> b.bside = Us) g.buildings in
      let theirs = List.exists (fun b -> b.bside = Them) g.buildings in
      if not theirs then Scene2d.go (Won true) s else if not ours then Scene2d.go (Won false) s else { s with scene = Playing g }
  | Won _ -> if space && s.elapsed > 2. then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let at (c : int * int) (shape : shape) : shape = let x, y = center c in move x y shape
let pos (x : number) (y : number) : number * number =
  let cx, cy = center (0, 0) in
  (cx + (size * x), cy - (size * y))

let view_unit (selected : bool) (u : unit_) : shape list =
  let x, y = pos u.x u.y in
  let color = match (u.side, u.kind) with
    | Us, Tank -> rgb 90 150 230
    | Us, Harvester -> rgb 120 200 150
    | Them, Tank -> rgb 230 100 90
    | Them, Harvester -> rgb 210 150 90
  in
  (if selected then [ circle white 20. |> fade 0.3 |> move x y ] else [])
  @ (match u.kind with
    | Tank -> [ rectangle color 22. 22. |> move x y; rectangle (rgb 40 40 50) 16. 5. |> move x y ]
    | Harvester -> [ rectangle color 26. 18. |> move x y; rectangle (rgb 250 200 90) (5. *. float_of_int u.cargo) 5. |> move x (y + 12.) ])
  @ [ rectangle (rgb 40 40 40) 24. 4. |> move x (y - 16.);
      rectangle (rgb 90 220 90) (24. * float_of_int u.hp / (match u.kind with Tank -> float_of_int tank_hp | Harvester -> float_of_int harvester_hp)) 4. |> move x (y - 16.) ]

let view_game (g : game) : shape list =
  [ rectangle (rgb 120 95 60) (float_of_int cols * size) (float_of_int rows * size) |> move_y (top - (float_of_int rows * size / 2.)) ]
  @ List.init (cols *.. rows) (fun i ->
        let c = (i mod cols, i /.. cols) in
        match g.terrain.(i) with
        | Sand -> at c (rectangle (rgb 190 150 90) (size - 1.) (size - 1.))
        | Rock -> at c (rectangle (rgb 95 85 75) (size - 1.) (size - 1.))
        | Spice n -> at c (rectangle (rgb (200 +.. (5 *.. n)) (120 +.. (4 *.. n)) 30) (size - 1.) (size - 1.)))
  @ List.concat_map
      (fun (b : building) ->
        let color = if b.bside = Us then rgb 60 110 200 else rgb 200 70 60 in
        [ at b.cell (rectangle color (size *. 1.8) (size *. 1.8)); at b.cell (rectangle (rgb 30 30 40) 16. 16.);
          at b.cell (rectangle (rgb 90 220 90) (size *. 1.8 * float_of_int b.bhp / float_of_int refinery_hp) 5. |> move_y (size *. 1.1)) ])
      g.buildings
  @ (match (g.selected, g.show_path) with
    | Some id, true -> (
        match List.find_opt (fun (u : unit_) -> u.id = id) g.units with
        | Some u -> List.map (fun c -> at c (circle (rgb 240 240 200) 4. |> fade 0.8)) u.path
        | None -> [])
    | _ -> [])
  @ List.concat_map (fun (u : unit_) -> view_unit (g.selected = Some u.id) u) g.units
  @ List.map
      (fun (s : shot) ->
        let x0, y0 = pos (fst s.from) (snd s.from) and x1, y1 = pos (fst s.to_) (snd s.to_) in
        rectangle (rgb 255 240 150) (Float.hypot (x1 - x0) (y1 - y0)) 3.
        |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi)
        |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.))
      g.shots
  @ [ at g.cursor (rectangle white (size - 4.) (size - 4.) |> fade 0.25) ]
  @ [ text (rgb 250 220 120) 2.5 (Printf.sprintf "credits %d" g.credits) |> move (-330.) 430.;
      text (rgb 230 120 110) 2.5 (Printf.sprintf "theirs %d" g.their_credits) |> move 0. 430.;
      text white 2.5 (Printf.sprintf "units %d" (List.length (List.filter (fun (u : unit_) -> u.side = Us) g.units))) |> move 330. 430.;
      text (rgb 190 180 160) 2. "click a unit, then the ground   b: tank 100   h: harvester 150   p: the path   f: faster" |> move_y (-420.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 60 45 30) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_game ())
      @ [ text (rgb 250 220 120) 6. "TINY DUNE II" |> move_y 100.;
          text white 2.5 "harvest the spice, build tanks, take their refinery" |> move_y 20. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-60.) ]
  | Playing g -> view_game g
  | Won ours ->
      [ text (if ours then rgb 120 220 140 else rgb 230 90 80) 6. (if ours then "THE SPICE IS YOURS" else "YOUR BASE IS GONE") ]
      @ if s.elapsed > 2. then Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-120.) ] else [])

let app = game view update initial_model
let main = Playground_platform.run_app app
