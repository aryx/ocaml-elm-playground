(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Warcraft II (Blizzard, 1995), the game that made the
 * real-time strategy genre popular: peasants mine gold and chop wood
 * for a town hall, footmen fight, and an orc camp across the map does
 * the same. (It looks three-dimensional, but it isn't: sprites drawn at
 * an angle on a flat tile map, which is why this is a 2D game and not
 * a 2.5D one.)
 *
 * Drag a box around your units to take several at once, or click one;
 * click the ground to send them all. "t" trains a peasant (40 gold),
 * "f" a footman (60 gold, 20 wood), "a" selects everyone, "p" draws the
 * flow field, "x" turns the fog off, "s" runs faster. Take their town
 * hall down before they take yours.
 *
 * What's new here (TinyDune2 has the same genre without them):
 *
 *  - One search for a whole crowd ([send]): ordering ten units to the
 *    same place is one Dijkstra from that place (Pathfind.field),
 *    which gives the cost from it to every tile. Each unit then walks
 *    downhill on that field, looking only at the tile under its feet
 *    (Pathfind.downhill). A* per unit would be ten searches, redone
 *    whenever one is pushed aside; a field is one, and it keeps working
 *    from wherever a unit ends up. It's how the RTS that came after
 *    moved crowds.
 *
 *  - The fog of war ([seen] and [visible]): two bitmaps, what your side
 *    has ever seen and what it can see right now. What was seen is
 *    drawn dim and out of date (the buildings stay where you remember
 *    them, the units don't show); what was never seen is black. The
 *    enemy's own search doesn't care about your fog -- it's a drawing
 *    rule, not a rule of the world, as it was in 1995.
 *
 *  - Two resources ([Gold] and [Wood]) and a shared purse: several
 *    peasants working at once, each on the nearest patch of what it
 *    carries, which is a search with a predicate as its goal (as in
 *    TinyDune2's spice).
 *
 * What it uses: ai/'s Pathfind (field, downhill, astar), Scene2d,
 * Audio. Not Physics: units slide along tiles, blows land at once.
 *
 * Exercises: units that push each other aside (and why the field copes
 * with that better than a path does); the field drawn as arrows; a
 * building placed by the player, blocking the field; ranged units
 * (archers) and a healer; the enemy scouting instead of walking
 * straight at you; the peasants deciding between gold and wood by what
 * the purse lacks.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The map *)
(*****************************************************************************)

type tile = Grass | Forest of int (* how much wood is left *) | Water | Mine of int

let cols = 24
let rows = 16
let size = 40.
let left = -.(float_of_int cols * size / 2.)
let top = 320.

type map = tile array

let index ((x, y) : int * int) : int = (y *.. cols) +.. x
let inside ((x, y) : int * int) : bool = x >= 0 && x < cols && y >= 0 && y < rows
let tile_at (m : map) (c : int * int) : tile = m.(index c)
let walkable (m : map) (c : int * int) : bool =
  inside c && match tile_at m c with Grass -> true | Forest _ | Water | Mine _ -> false

let center ((x, y) : int * int) : number * number =
  (left + (size * (float_of_int x + 0.5)), top - (size * (float_of_int y + 0.5)))

let cell_at (mx : number) (my : number) : int * int =
  (int_of_float (Float.floor ((mx - left) / size)), int_of_float (Float.floor ((top - my) / size)))

let new_map () : map =
  let m = Array.make (cols *.. rows) Grass in
  let put c t = if inside c then m.(index c) <- t in
  (* a lake in the middle, forests north and south, a gold mine a side *)
  for y = 6 to 9 do for x = 10 to 13 do put (x, y) Water done done;
  for y = 2 to 4 do for x = 8 to 11 do put (x, y) (Forest 8) done done;
  for y = 11 to 13 do for x = 12 to 15 do put (x, y) (Forest 8) done done;
  put (5, 4) (Mine 200);
  put (18, 11) (Mine 200);
  m

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type side = Us | Them
type job = Peasant | Footman
type carrying = Nothing | Gold of int | Wood of int

type unit_ = {
  id : int;
  job : job;
  side : side;
  x : number;
  y : number;
  goal : (int * int) option; (* where it was sent *)
  carrying : carrying;
  working : int; (* frames left mining or chopping *)
  hp : int;
  cooldown : int;
}

type building = { bside : side; cell : int * int; bhp : int }

type game = {
  map : map;
  units : unit_ list;
  buildings : building list;
  selected : int list; (* ids *)
  (* one field per place our crowd was sent to, and one for theirs *)
  our_field : ((int * int) * float) list;
  their_field : ((int * int) * float) list;
  drag : (number * number) option; (* where the mouse went down *)
  cursor : int * int;
  gold : int;
  wood : int;
  their_gold : int;
  seen : bool array; (* what we have ever seen *)
  next_id : int;
  frames : int;
  show_field : bool;
  fog : bool;
}

type scene = Title | Playing of game | Over of bool
type model = scene Scene2d.t

let our_hall = (3, 8)
let their_hall = (20, 8)
let sight = 4.5
let melee_range = 1.4

(* The knobs. Everything that makes the game easy or hard is here, so
 * it can be tried by turning one number:
 *
 *  - the orcs' pressure: [their_footman_every] (a footman every 8
 *    seconds at 480; 240 makes them twice as fast) and
 *    [their_footmen_max] (how large their army can grow). Together
 *    they are the difficulty: 480 and 5 give a player time to mine and
 *    build; 120 and 12 is a rush no toy defence survives.
 *  - how long a hall lasts: [hall_hp] against [blow] every
 *    [blow_on_building] frames. One footman takes 1200 / 7 * 60 frames,
 *    about three minutes, down alone; five take half a minute.
 *  - the economy: [peasant_cost], [footman_gold], [footman_wood],
 *    [load] (what a trip brings back) and [work_frames] (how long a
 *    trip's digging takes). Cheaper units or bigger loads mean bigger
 *    armies on both sides, since the orcs mine too.
 *)
let peasant_cost = 40
let footman_gold = 60
let footman_wood = 20
let load = 10
let work_frames = 90
let hall_hp = 1200
let blow = 7
let blow_delay = 30 (* frames between two blows, on a unit *)
let blow_on_building = 60
let their_footman_every = 480
let their_footmen_max = 5

let cell_of (u : unit_) : int * int = Orders.cell_of (u.x, u.y)
let hall_of (side : side) : int * int = match side with Us -> our_hall | Them -> their_hall

let new_unit (id : int) (job : job) (side : side) ((x, y) : int * int) : unit_ =
  { id; job; side; x = float_of_int x; y = float_of_int y; goal = None; carrying = Nothing; working = 0;
    hp = (match job with Footman -> 60 | Peasant -> 30); cooldown = 0 }

let new_game () : game =
  { map = new_map ();
    units =
      [ new_unit 1 Peasant Us (4, 7); new_unit 2 Peasant Us (4, 9); new_unit 3 Footman Us (5, 8);
        new_unit 4 Peasant Them (19, 7); new_unit 5 Peasant Them (19, 9); new_unit 6 Footman Them (18, 8) ];
    buildings = [ { bside = Us; cell = our_hall; bhp = hall_hp }; { bside = Them; cell = their_hall; bhp = hall_hp } ];
    selected = []; our_field = []; their_field = []; drag = None; cursor = (8, 8); gold = 120; wood = 60; their_gold = 120;
    seen = Array.make (cols *.. rows) false; next_id = 7; frames = 0; show_field = false; fog = true }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Searching: one field for a crowd, A* for one *)
(*****************************************************************************)

(* the crowd's order: one Dijkstra from where they're sent, and every
 * unit walks downhill on it *)
let send (m : map) (target : int * int) : ((int * int) * float) list = Orders.field ~walkable:(walkable m) target

(* a lone unit's own errand (a peasant to its mine, an orc to our hall):
 * the next step of an A*, recomputed as it goes *)
let step_toward (m : map) (u : unit_) (target : int * int) : (int * int) option =
  match Orders.path ~walkable:(walkable m) ~from:(cell_of u) target with _ :: next :: _ -> Some next | _ -> None

(* the way to the nearest tile of what we're after, stopping beside it:
 * a mine and a forest can't be walked on *)
let toward_nearest (m : map) (u : unit_) (next_to : int * int -> bool) : (int * int) option =
  match Orders.nearest ~walkable:(walkable m) ~from:(cell_of u) next_to with _ :: next :: _ -> Some next | _ -> None

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let speed (u : unit_) : number = match u.job with Footman -> 0.05 | Peasant -> 0.045

(* a unit walks toward a neighbouring cell *)
let toward (u : unit_) (cell : int * int) : unit_ =
  let x, y = Orders.toward ~speed:(speed u) (u.x, u.y) cell in
  { u with x; y }

(* the crowd: each unit takes one step downhill on its side's field *)
let follow (g : game) (u : unit_) : unit_ =
  match u.goal with
  | None -> u
  | Some goal ->
      if cell_of u = goal then { u with goal = None }
      else
        let field = if u.side = Us then g.our_field else g.their_field in
        (match Orders.downhill ~walkable:(walkable g.map) field (cell_of u) with
        | Some next -> toward u next
        | None -> { u with goal = None })

(* a peasant: to the gold or the wood, work, carry it home, unload *)
let gather (g : game) (u : unit_) : game * unit_ =
  let home = hall_of u.side in
  let is_mine = function Mine n -> n > 0 | _ -> false in
  let is_forest = function Forest n -> n > 0 | _ -> false in
  if u.working > 0 then
    let u = { u with working = u.working -.. 1 } in
    if u.working > 0 then (g, u)
    else
      (* the tile beside it gives up 10 of what it holds *)
      let take what carry =
        let neighbour =
          List.find_opt (fun (dx, dy) -> inside (fst (cell_of u) +.. dx, snd (cell_of u) +.. dy) && what (tile_at g.map (fst (cell_of u) +.. dx, snd (cell_of u) +.. dy)))
            [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
        in
        match neighbour with
        | None -> (g, { u with carrying = Nothing })
        | Some (dx, dy) ->
            let c = (fst (cell_of u) +.. dx, snd (cell_of u) +.. dy) in
            (match tile_at g.map c with
            | Mine n -> g.map.(index c) <- Mine (max 0 (n -.. load))
            | Forest n -> g.map.(index c) <- (if n <= 1 then Grass else Forest (n -.. 1))
            | _ -> ());
            (g, { u with carrying = carry })
      in
      (match u.carrying with Wood _ -> take is_forest (Wood load) | _ -> take is_mine (Gold load))
  else
    match u.carrying with
    | Gold n | Wood n when n > 0 && cell_of u = home ->
        Audio.play Audio.coin;
        let g =
          match (u.side, u.carrying) with
          | Us, Gold k -> { g with gold = g.gold +.. k }
          | Us, Wood k -> { g with wood = g.wood +.. k }
          | Them, Gold k -> { g with their_gold = g.their_gold +.. k }
          | _ -> g
        in
        (g, { u with carrying = (match u.carrying with Wood _ -> Wood 0 | _ -> Gold 0) })
    | Gold n | Wood n when n > 0 -> (
        (* carrying: home *)
        match step_toward g.map u home with Some next -> (g, toward u next) | None -> (g, u))
    | carrying -> (
        let want = match carrying with Wood _ -> is_forest | _ -> is_mine in
        let at = cell_of u in
        let touching = List.exists (fun (dx, dy) -> inside (fst at +.. dx, snd at +.. dy) && want (tile_at g.map (fst at +.. dx, snd at +.. dy))) [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
        if touching then (g, { u with working = work_frames })
        else
          let next_to c = List.exists (fun (dx, dy) -> inside (fst c +.. dx, snd c +.. dy) && want (tile_at g.map (fst c +.. dx, snd c +.. dy))) [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
          match toward_nearest g.map u next_to with
          | Some next -> (g, toward u next)
          | None ->
              (* nothing left of that kind: try the other *)
              (g, { u with carrying = (match carrying with Wood _ -> Gold 0 | _ -> Wood 0) }))

(* a footman hits whatever of the other side stands next to it *)
let fight (g : game) (u : unit_) : game * unit_ =
  if u.job <> Footman || u.cooldown > 0 then (g, { u with cooldown = max 0 (u.cooldown -.. 1) })
  else
    let d (x, y) = Float.hypot (u.x - x) (u.y - y) in
    match List.filter (fun (v : unit_) -> v.side <> u.side && d (v.x, v.y) <= melee_range) g.units with
    | v :: _ ->
        Audio.play Audio.hit;
        ({ g with units = List.map (fun (w : unit_) -> if w.id = v.id then { w with hp = w.hp -.. blow } else w) g.units }, { u with cooldown = blow_delay })
    | [] -> (
        match List.filter (fun (b : building) -> b.bside <> u.side && d (float_of_int (fst b.cell), float_of_int (snd b.cell)) <= melee_range +. 0.8) g.buildings with
        | b :: _ ->
            Audio.play Audio.hit;
            ({ g with buildings = List.map (fun (c : building) -> if c.cell = b.cell then { c with bhp = c.bhp -.. blow } else c) g.buildings }, { u with cooldown = blow_on_building })
        | [] -> (g, u))

(* the orcs: peasants of their own, a footman whenever they can pay for
 * one, and every footman sent at our hall (one field for all of them) *)
let their_turn (g : game) : game =
  let their_footmen = List.filter (fun (u : unit_) -> u.side = Them && u.job = Footman) g.units in
  let g =
    if g.their_gold >= footman_gold && List.length their_footmen < their_footmen_max && g.frames mod their_footman_every = 0 then
      { g with their_gold = g.their_gold -.. footman_gold; next_id = g.next_id +.. 1;
        units = g.units @ [ new_unit g.next_id Footman Them their_hall ] }
    else g
  in
  if g.frames mod 60 <> 0 then g
  else
    { g with their_field = send g.map our_hall;
      units = List.map (fun (u : unit_) -> if u.side = Them && u.job = Footman then { u with goal = Some our_hall } else u) g.units }

let sees (g : game) (c : int * int) : bool =
  List.exists (fun (u : unit_) -> u.side = Us && Float.hypot (u.x - float_of_int (fst c)) (u.y - float_of_int (snd c)) <= sight) g.units
  || List.exists (fun (b : building) -> b.bside = Us && Pathfind.manhattan b.cell c <= sight +. 1.) g.buildings

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed key = Scene2d.pressed key scenes in
  let key k = pressed (fun kb -> Set_.mem k kb.keys) in
  let mouse = computer.mouse in
  let g = { g with frames = g.frames +.. 1 } in
  let x, y = g.cursor in
  let step k d = if pressed k then d else 0 in
  let cursor = (clamp 0 (cols -.. 1) (x +.. step (fun k -> k.kright) 1 +.. step (fun k -> k.kleft) (-1)),
                clamp 0 (rows -.. 1) (y +.. step (fun k -> k.kdown) 1 +.. step (fun k -> k.kup) (-1))) in
  let cursor = if mouse.mdx <> 0. || mouse.mdy <> 0. then (let c = cell_at mouse.mx mouse.my in if inside c then c else cursor) else cursor in
  let g = { g with cursor } in
  let g = if key "p" then { g with show_field = not g.show_field } else g in
  let g = if key "x" then { g with fog = not g.fog } else g in
  let g = if key "a" then { g with selected = List.filter_map (fun (u : unit_) -> if u.side = Us then Some u.id else None) g.units } else g in
  (* training *)
  let g =
    if key "t" && g.gold >= peasant_cost then
      { g with gold = g.gold -.. peasant_cost; next_id = g.next_id +.. 1; units = g.units @ [ new_unit g.next_id Peasant Us our_hall ] }
    else g
  in
  let g =
    if key "f" && g.gold >= footman_gold && g.wood >= footman_wood then
      { g with gold = g.gold -.. footman_gold; wood = g.wood -.. footman_wood; next_id = g.next_id +.. 1;
        units = g.units @ [ new_unit g.next_id Footman Us our_hall ] }
    else g
  in
  (* the selection box: the mouse down, dragged, released *)
  let g = if mouse.mdown && g.drag = None then { g with drag = Some (mouse.mx, mouse.my) } else g in
  let g =
    if (not mouse.mdown) && g.drag <> None then begin
      let x0, y0 = Option.get g.drag in
      let box = Float.abs (mouse.mx - x0) > 20. || Float.abs (mouse.my - y0) > 20. in
      if box then
        let lo a b = Float.min a b and hi a b = Float.max a b in
        let inside_box (u : unit_) =
          let ux, uy = center (0, 0) in
          let ux = ux + (size * u.x) and uy = uy - (size * u.y) in
          u.side = Us && ux >= lo x0 mouse.mx && ux <= hi x0 mouse.mx && uy >= lo y0 mouse.my && uy <= hi y0 mouse.my
        in
        { g with drag = None; selected = List.filter_map (fun u -> if inside_box u then Some u.id else None) g.units }
      else
        (* a click: one unit, or an order for those already chosen *)
        match List.find_opt (fun (u : unit_) -> u.side = Us && cell_of u = g.cursor) g.units with
        | Some u -> { g with drag = None; selected = [ u.id ] }
        | None ->
            if g.selected = [] || not (walkable g.map g.cursor) then { g with drag = None }
            else begin
              Audio.play Audio.blip;
              { g with drag = None; our_field = send g.map g.cursor;
                units = List.map (fun (u : unit_) -> if List.mem u.id g.selected then { u with goal = Some g.cursor; carrying = Nothing } else u) g.units }
            end
    end
    else g
  in
  (* space does what a click does, at the cursor *)
  let g =
    if pressed (fun k -> k.kspace) then
      match List.find_opt (fun (u : unit_) -> u.side = Us && cell_of u = g.cursor) g.units with
      | Some u -> { g with selected = [ u.id ] }
      | None when g.selected <> [] && walkable g.map g.cursor ->
          { g with our_field = send g.map g.cursor;
            units = List.map (fun (u : unit_) -> if List.mem u.id g.selected then { u with goal = Some g.cursor; carrying = Nothing } else u) g.units }
      | _ -> g
    else g
  in
  (* everyone: the ones sent somewhere walk the field, the others work *)
  let g, units =
    List.fold_left
      (fun (g, acc) (u : unit_) ->
        let g, u =
          if u.goal <> None then (g, follow g u)
          else if u.job = Peasant then gather g u
          else (g, u)
        in
        let g, u = fight g u in
        (g, u :: acc))
      (g, []) g.units
  in
  let g = { g with units = List.rev units } in
  let g = their_turn g in
  (* the fog: what we see now is remembered *)
  Array.iteri (fun i s -> if not s then g.seen.(i) <- sees g (i mod cols, i /.. cols)) g.seen;
  let alive = List.filter (fun (u : unit_) -> u.hp > 0) g.units in
  { g with units = alive;
    selected = List.filter (fun id -> List.exists (fun (u : unit_) -> u.id = id) alive) g.selected;
    buildings = List.filter (fun b -> b.bhp > 0) g.buildings }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let fast = Set_.mem "s" computer.keyboard.keys in
      let g = update_game computer s g in
      let g = if fast then update_game computer s (update_game computer s g) else g in
      if not (List.exists (fun b -> b.bside = Them) g.buildings) then Scene2d.go (Over true) s
      else if not (List.exists (fun b -> b.bside = Us) g.buildings) then Scene2d.go (Over false) s
      else { s with scene = Playing g }
  | Over _ -> if space && s.elapsed > 2. then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let at (c : int * int) (shape : shape) : shape = let x, y = center c in move x y shape
let pos (x : number) (y : number) : number * number =
  let cx, cy = center (0, 0) in
  (cx + (size * x), cy - (size * y))

let view_tile (t : tile) : shape =
  match t with
  | Grass -> rectangle (rgb 70 120 60) (size - 1.) (size - 1.)
  | Water -> rectangle (rgb 50 90 170) (size - 1.) (size - 1.)
  | Forest n -> group [ rectangle (rgb 40 85 45) (size - 1.) (size - 1.); circle (rgb 30 (100 +.. (6 *.. n)) 50) 14. ]
  | Mine n -> group [ rectangle (rgb 110 95 70) (size - 1.) (size - 1.); circle (rgb 240 200 60) (6. +. (float_of_int n / 25.)) ]

let view_unit (selected : bool) (u : unit_) : shape list =
  let x, y = pos u.x u.y in
  let body = match (u.side, u.job) with
    | Us, Footman -> rgb 80 140 240
    | Us, Peasant -> rgb 150 200 240
    | Them, Footman -> rgb 220 90 70
    | Them, Peasant -> rgb 230 160 120
  in
  (if selected then [ circle (rgb 120 240 120) 17. |> fade 0.35 |> move x y ] else [])
  @ [ circle body 11. |> move x y;
      (* a head and a shoulder: the sprites that suggest a man seen from
       * above and in front, the game's whole 3D *)
      circle (rgb 240 220 190) 5. |> move x (y + 6.) ]
  @ (match u.job with Footman -> [ rectangle (rgb 200 200 210) 3. 14. |> move (x + 9.) y ] | Peasant -> [])
  @ (match u.carrying with
    | Gold n when n > 0 -> [ circle (rgb 240 210 60) 4. |> move (x - 9.) (y + 8.) ]
    | Wood n when n > 0 -> [ rectangle (rgb 150 100 50) 10. 4. |> move (x - 9.) (y + 8.) ]
    | _ -> [])
  @ [ rectangle (rgb 40 40 40) 22. 3. |> move x (y - 14.);
      rectangle (rgb 90 220 90) (22. * float_of_int u.hp / (match u.job with Footman -> 60. | Peasant -> 30.)) 3. |> move x (y - 14.) ]

let view_game (g : game) : shape list =
  let lit c = (not g.fog) || sees g c in
  let known i = (not g.fog) || g.seen.(i) in
  [ rectangle (rgb 20 25 20) (float_of_int cols * size) (float_of_int rows * size) |> move_y (top - (float_of_int rows * size / 2.)) ]
  @ List.filter_map
      (fun i ->
        let c = (i mod cols, i /.. cols) in
        if not (known i) then None
        else Some (at c (view_tile g.map.(i) |> fade (if lit c then 1. else 0.45))))
      (List.init (cols *.. rows) Fun.id)
  (* the field the crowd is following *)
  @ (if g.show_field then
       List.filter_map (fun (c, d) -> if known (index c) then Some (at c (circle (rgb 240 240 180) (Float.max 1. (8. -. (d /. 3.))) |> fade 0.5)) else None) g.our_field
     else [])
  @ List.filter_map
      (fun (b : building) ->
        if not (known (index b.cell)) then None
        else
          let color = if b.bside = Us then rgb 60 110 210 else rgb 200 70 60 in
          Some
            (group
               [ rectangle color (size *. 1.9) (size *. 1.9); rectangle (rgb 40 35 30) 14. 20. |> move_y (-6.);
                 rectangle (rgb 90 220 90) (size *. 1.9 * float_of_int b.bhp / float_of_int hall_hp) 5. |> move_y (size *. 1.2) ]
            |> fade (if lit b.cell then 1. else 0.5)
            |> at b.cell))
      g.buildings
  (* units of ours always, theirs only where we can see *)
  @ List.concat_map (fun (u : unit_) -> if u.side = Us || lit (cell_of u) then view_unit (List.mem u.id g.selected) u else []) g.units
  @ [ at g.cursor (rectangle white (size - 4.) (size - 4.) |> fade 0.2) ]
  @ [ text (rgb 250 220 120) 2.5 (Printf.sprintf "gold %d" g.gold) |> move (-360.) 430.;
      text (rgb 180 140 90) 2.5 (Printf.sprintf "wood %d" g.wood) |> move (-140.) 430.;
      text white 2.5 (Printf.sprintf "units %d" (List.length (List.filter (fun (u : unit_) -> u.side = Us) g.units))) |> move 100. 430.;
      text (rgb 230 120 110) 2.5 (Printf.sprintf "their gold %d" g.their_gold) |> move 340. 430.;
      text (rgb 190 190 180) 2.
        "drag a box or click: choose   click the ground: send them   t: peasant 40   f: footman 60+20   a: all   p: the field   x: the fog   s: faster"
      |> move_y (-420.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 15 20 15) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game { (new_game ()) with fog = false }
      @ [ text (rgb 250 220 120) 6. "TINY WARCRAFT II" |> move_y 100.;
          text white 2.5 "mine gold, chop wood, send a crowd with one search" |> move_y 20. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-60.) ]
  | Playing g -> view_game g
  | Over ours ->
      [ text (if ours then rgb 120 220 140 else rgb 230 90 80) 6. (if ours then "THEIR HALL IS DOWN" else "YOUR HALL IS DOWN") ]
      @ if s.elapsed > 2. then Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-120.) ] else [])

let app = game view update initial_model
let main = Playground_platform.run_app app
