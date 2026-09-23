(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Civilization (Sid Meier and Bruce Shelley,
 * MicroProse, 1991): from one band of settlers in 4000 BC, found cities,
 * grow them, learn your way up a tree of advances, and outlast the
 * civilization next door -- by conquest, or by being the first to
 * Philosophy (standing in for the race to Alpha Centauri).
 *
 *   arrows      move the unit that blinks (into an enemy: attack)
 *   b           settlers: build a city here
 *   space       this unit waits for the next turn
 *   p           what the city under the unit builds (or click a city)
 *   r           what to research next
 *   t           the tree of advances
 *   Enter       the end of the turn
 *
 * The genre was named after it: "4X", explore, expand, exploit,
 * exterminate. Meier had made Railroad Tycoon; Civilization borrowed
 * from Walter Bright's Empire and from the board game of the same name
 * (Francis Tresham, 1980). It is the game of "one more turn". (Names
 * and dates from memory, to check.)
 *
 * Turns are TinyRogue's (the world waits for you, and moves when you
 * say so: Enter here), and the black map you uncover is TinyWarcraft2's
 * fog of war, the "ever seen" half of it. What is new here:
 *
 *  - The tree of advances ([requires]). An advance can be learnt once
 *    all of its parents are known, so the choices open up as you go --
 *    a directed acyclic graph, which "t" draws in columns by depth
 *    ([depth]: one more than its deepest parent):
 *
 *      Alphabet ----------+--> Writing -------+--> Literacy ---+
 *                         +--> Code of Laws --+                |
 *      Ceremonial Burial -----------+-----------> Monarchy ----+--> Philosophy
 *                     Code of Laws -+                          |
 *      Bronze Working ---------------> Currency ---------------+
 *      Masonry --+--> Mathematics (catapults)
 *      Alphabet -+
 *      Warrior Code (archers)   Horseback Riding (horsemen)
 *
 *    Research is a choice of path: the military advances lead nowhere
 *    near Philosophy, and cost as much as the others.
 *
 *  - A city is a small economy ([worked], [city_turn]). It works its
 *    own tile and one more of the eight around it per citizen, the best
 *    ones: each gives food, shields and trade. Food beyond two per
 *    citizen fills a box, and a full box is one more citizen -- who
 *    works one more tile; shields build the unit in hand (settlers take
 *    a citizen with them); trade is research. Everything in the game
 *    comes out of those three numbers, and a city by the sea (trade) or
 *    by a forest (shields) plays differently.
 *
 *  - The world is made, not drawn ([noise]): a random number per tile,
 *    blurred three times (SimCity's smog, again an image filter), cut at
 *    a sea level to make the coasts and at other levels for the kinds of
 *    land. The same seed, the same world, so a game replays.
 *
 *  - The rival plays by the same rules ([ai_turn]): it researches,
 *    grows, sends settlers to the best free places, keeps one unit home
 *    in each city and, once it has an army, marches on your nearest
 *    city. It sees the whole map (no fog of its own).
 *
 *  - Combat is a coin weighted by strength ([odds]): attack against
 *    defence, times 1.5 on hills and in a city. And as in the first
 *    Civilization, when the defender of a tile loses, every unit on the
 *    tile dies with it -- stacking is a risk.
 *
 * What it uses: Scene2d. No kit (gamekits/rts is units walking paths in
 * real time; here a unit steps one tile a turn), no Pathfind (the
 * rival walks greedily towards its goal, and a coast can stop it).
 *
 * Exercises: the 21-tile "fat cross" a city really works, and the tiles
 * two cities share; buildings (a granary keeping half the food, city
 * walls tripling defence, a temple); roads, and settlers working the
 * land; boats, for the other continents; taxes against science; the
 * rival's own fog; more rivals, and diplomacy.
 *)
open Playground

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

let cols = 40
let rows = 26
let size = 20.
let left = -490.
let top = 360.

type terrain = Ocean | Grass | Plains | Forest | Hills

let index ((x, y) : int * int) : int = (y * cols) + x
let cell (i : int) : int * int = (i mod cols, i / cols)
let inside ((x, y) : int * int) : bool = x >= 0 && x < cols && y >= 0 && y < rows
let distance ((x1, y1) : int * int) ((x2, y2) : int * int) : int = max (abs (x1 - x2)) (abs (y1 - y2))

(* the tiles at most [r] steps away (diagonals count as one), [c] first *)
let around (r : int) ((x, y) as c : int * int) : (int * int) list =
  c
  :: List.filter
       (fun c' -> inside c' && c' <> c)
       (List.concat_map (fun dy -> List.init ((2 * r) + 1) (fun dx -> (x + dx - r, y + dy - r))) (List.init ((2 * r) + 1) Fun.id))

(* a random number per tile, blurred: neighbours end up alike, so the
 * land comes in continents rather than salt and pepper *)
let noise (seed : int) : float array =
  let n = cols * rows in
  let blur (a : float array) =
    Array.init n (fun i ->
        let cs = around 1 (cell i) in
        List.fold_left (fun s c -> s +. a.(index c)) 0. cs /. float_of_int (List.length cs))
  in
  blur (blur (blur (Array.init n (fun i -> float_of_int (Hashtbl.hash (seed, i) mod 1000) /. 1000.))))

(* the value below which a fraction [q] of [a] lies *)
let quantile (a : float array) (q : float) : float =
  let s = Array.copy a in
  Array.sort compare s;
  s.(int_of_float (q *. float_of_int (Array.length s - 1)))

let world : terrain array =
  let height = noise 1 and wet = noise 2 in
  let sea = quantile height 0.5 in
  let level = quantile wet in
  let q30 = level 0.3 and q60 = level 0.6 and q80 = level 0.8 in
  Array.init (cols * rows) (fun i ->
      let x, y = cell i in
      if height.(i) < sea || x = 0 || y = 0 || x = cols - 1 || y = rows - 1 then Ocean
      else if wet.(i) < q30 then Plains
      else if wet.(i) < q60 then Grass
      else if wet.(i) < q80 then Forest
      else Hills)

let on_land (c : int * int) : bool = inside c && world.(index c) <> Ocean

(* food, shields, trade *)
let yield_of = function
  | Ocean -> (1, 0, 2)
  | Grass -> (2, 0, 1)
  | Plains -> (1, 1, 1)
  | Forest -> (1, 2, 0)
  | Hills -> (1, 1, 0)

(*****************************************************************************)
(* The tree of advances *)
(*****************************************************************************)

type tech =
  | Alphabet
  | Bronze_working
  | Ceremonial_burial
  | Horseback_riding
  | Masonry
  | Warrior_code
  | Writing
  | Code_of_laws
  | Currency
  | Mathematics
  | Monarchy
  | Literacy
  | Philosophy

let techs =
  [ Alphabet; Bronze_working; Ceremonial_burial; Horseback_riding; Masonry; Warrior_code; Writing; Code_of_laws;
    Currency; Mathematics; Monarchy; Literacy; Philosophy ]

(* the tree, cut down from Civilization's own *)
let requires = function
  | Alphabet | Bronze_working | Ceremonial_burial | Horseback_riding | Masonry | Warrior_code -> []
  | Writing | Code_of_laws -> [ Alphabet ]
  | Currency -> [ Bronze_working ]
  | Mathematics -> [ Alphabet; Masonry ]
  | Monarchy -> [ Ceremonial_burial; Code_of_laws ]
  | Literacy -> [ Writing; Code_of_laws ]
  | Philosophy -> [ Literacy; Monarchy; Currency ]

let tech_name = function
  | Alphabet -> "Alphabet"
  | Bronze_working -> "Bronze Working"
  | Ceremonial_burial -> "Ceremonial Burial"
  | Horseback_riding -> "Horseback Riding"
  | Masonry -> "Masonry"
  | Warrior_code -> "Warrior Code"
  | Writing -> "Writing"
  | Code_of_laws -> "Code of Laws"
  | Currency -> "Currency"
  | Mathematics -> "Mathematics"
  | Monarchy -> "Monarchy"
  | Literacy -> "Literacy"
  | Philosophy -> "Philosophy"

(* the longest way down to an advance with no parent: its column in the
 * drawing of the tree *)
let rec depth (t : tech) : int = List.fold_left (fun d p -> max d (depth p + 1)) 0 (requires t)

(* what can be researched: not known, every parent known *)
let available (known : tech list) : tech list =
  List.filter (fun t -> (not (List.mem t known)) && List.for_all (fun p -> List.mem p known) (requires t)) techs

(* each advance costs more than the one before *)
let research_cost (known : tech list) : int = 8 * (List.length known + 2)

(*****************************************************************************)
(* Units and cities *)
(*****************************************************************************)

type side = Us | Them

type kind = Settlers | Warriors | Phalanx | Archers | Horsemen | Catapult

(* attack, defence, moves a turn *)
let stats = function
  | Settlers -> (0, 1, 1)
  | Warriors -> (1, 1, 1)
  | Phalanx -> (1, 2, 1)
  | Archers -> (3, 2, 1)
  | Horsemen -> (2, 1, 2)
  | Catapult -> (6, 1, 1)

let cost = function Settlers -> 30 | Warriors -> 10 | Phalanx | Horsemen -> 20 | Archers -> 30 | Catapult -> 40

let needs = function
  | Settlers | Warriors -> None
  | Phalanx -> Some Bronze_working
  | Archers -> Some Warrior_code
  | Horsemen -> Some Horseback_riding
  | Catapult -> Some Mathematics

let kinds = [ Warriors; Settlers; Phalanx; Archers; Horsemen; Catapult ]
let kind_name = function
  | Settlers -> "Settlers"
  | Warriors -> "Warriors"
  | Phalanx -> "Phalanx"
  | Archers -> "Archers"
  | Horsemen -> "Horsemen"
  | Catapult -> "Catapult"

let buildable (known : tech list) : kind list =
  List.filter (fun k -> match needs k with None -> true | Some t -> List.mem t known) kinds

type unit_ = { id : int; side : side; kind : kind; at : int * int; moves : int }

type city = { name : string; owner : side; at : int * int; size : int; food : int; shields : int; making : kind }

(* A city works its own tile, better than it is (Civilization's cities
 * irrigate and pave their centre), and one tile around it per citizen,
 * the best first. *)
let centre (f, s, t) = (max f 2, max s 1, max t 1)

let worked (c : city) : (int * int) list =
  let score cell = let f, s, t = yield_of world.(index cell) in (3 * f) + (2 * s) + t in
  let ring = List.tl (around 1 c.at) in
  let best = List.stable_sort (fun a b -> compare (score b) (score a)) ring in
  c.at :: List.filteri (fun i _ -> i < c.size) best

let output (c : city) : int * int * int =
  List.fold_left
    (fun (f, s, t) cell ->
      let y = yield_of world.(index cell) in
      let f', s', t' = if cell = c.at then centre y else y in
      (f + f', s + s', t + t'))
    (0, 0, 0) (worked c)

let food_box (size : int) : int = 10 * (size + 1)

(* A city's turn: food beyond two a citizen fills the box (a full box
 * is a new citizen, an empty one loses one), shields build what is in
 * hand. Settlers wait for a second citizen to take with them. *)
let city_turn (c : city) : city * kind option =
  let f, s, _ = output c in
  let food = c.food + f - (2 * c.size) in
  let size, food =
    if food >= food_box c.size then (c.size + 1, 0) else if food < 0 then (max 1 (c.size - 1), 0) else (c.size, food)
  in
  let shields = c.shields + s in
  if shields >= cost c.making && (c.making <> Settlers || size >= 2) then
    ( { c with size = (if c.making = Settlers then size - 1 else size); food; shields = shields - cost c.making },
      Some c.making )
  else ({ c with size; food; shields }, None)

let trade (c : city) : int = let _, _, t = output c in t

(*****************************************************************************)
(* The game *)
(*****************************************************************************)

type civ = {
  known : tech list;
  research : tech;
  beakers : int;
  seen : bool array; (* the tiles ever seen: the rest is black *)
  names : string list; (* for its next cities *)
}

type game = {
  units : unit_ list;
  cities : city list;
  us : civ;
  them : civ;
  turn : int;
  next_id : int;
  rolls : int; (* how many dice rolled so far: the next roll's seed *)
  news : string;
  tree : bool; (* the tree of advances on screen *)
  winner : side option;
}

let civ_of (g : game) = function Us -> g.us | Them -> g.them
let with_civ (g : game) (side : side) (c : civ) : game = match side with Us -> { g with us = c } | Them -> { g with them = c }
let enemy = function Us -> Them | Them -> Us
let people = function Us -> "Romans" | Them -> "Babylonians"
let year (turn : int) : string = let y = -4000 + (20 * turn) in if y < 0 then Printf.sprintf "%d BC" (-y) else Printf.sprintf "%d AD" (y + 1)

let units_at (g : game) (c : int * int) : unit_ list = List.filter (fun (u : unit_) -> u.at = c) g.units
let city_at (g : game) (c : int * int) : city option = List.find_opt (fun (ct : city) -> ct.at = c) g.cities

let reveal (g : game) : game =
  let seen = Array.copy g.us.seen in
  List.iter (fun (u : unit_) -> if u.side = Us then List.iter (fun c -> seen.(index c) <- true) (around 1 u.at)) g.units;
  List.iter (fun (c : city) -> if c.owner = Us then List.iter (fun c -> seen.(index c) <- true) (around 2 c.at)) g.cities;
  { g with us = { g.us with seen } }

(* The two starts: the far west and the far east of the biggest land,
 * so that the rival can walk to you. *)
let starts : (int * int) * (int * int) =
  let label = Array.make (cols * rows) (-1) in
  let rec flood l = function
    | [] -> ()
    | c :: rest ->
        if on_land c && label.(index c) < 0 then begin
          label.(index c) <- l;
          flood l (List.tl (around 1 c) @ rest)
        end
        else flood l rest
  in
  List.iter (fun i -> if on_land (cell i) && label.(i) < 0 then flood i [ cell i ]) (List.init (cols * rows) Fun.id);
  let count l = Array.fold_left (fun n l' -> if l' = l then n + 1 else n) 0 label in
  let biggest = List.fold_left (fun b i -> if label.(i) >= 0 && count label.(i) > count b then label.(i) else b) 0 (List.init (cols * rows) Fun.id) in
  let continent = List.filter (fun i -> label.(i) = biggest) (List.init (cols * rows) Fun.id) |> List.map cell in
  let roomy c = List.length (List.filter on_land (around 1 c)) in
  let pick better = List.fold_left (fun b c -> if better c b then c else b) (List.hd continent) continent in
  let west = pick (fun (x, _) (bx, _) -> x < bx) and east = pick (fun (x, _) (bx, _) -> x > bx) in
  let near c = List.filter on_land (around 2 c) in
  let best_of cs = List.fold_left (fun b c -> if roomy c > roomy b then c else b) (List.hd cs) cs in
  (best_of (near west), best_of (near east))

let new_civ (research : tech) (names : string list) : civ =
  { known = []; research; beakers = 0; seen = Array.make (cols * rows) false; names }

let new_game () : game =
  let ours, theirs = starts in
  reveal
    { units =
        [ { id = 0; side = Us; kind = Settlers; at = ours; moves = 1 };
          { id = 1; side = Us; kind = Warriors; at = ours; moves = 1 };
          { id = 2; side = Them; kind = Settlers; at = theirs; moves = 1 };
          { id = 3; side = Them; kind = Warriors; at = theirs; moves = 1 } ];
      cities = [];
      us = new_civ Alphabet [ "Rome"; "Antium"; "Cumae"; "Neapolis"; "Ravenna"; "Capua"; "Veii"; "Tarentum" ];
      them = new_civ Bronze_working [ "Babylon"; "Ur"; "Nineveh"; "Ashur"; "Uruk"; "Lagash"; "Kish"; "Nippur" ];
      turn = 0;
      next_id = 4;
      rolls = 0;
      news = "b builds a city with the settlers; Enter ends the turn";
      tree = false;
      winner = None }

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

(* a city may be built on land, three tiles or more from any other *)
let can_found (g : game) (c : int * int) : bool = on_land c && List.for_all (fun (ct : city) -> distance ct.at c >= 3) g.cities

let found (g : game) (u : unit_) : game =
  if u.kind <> Settlers || not (can_found g u.at) then g
  else
    let civ = civ_of g u.side in
    let name, names = match civ.names with n :: rest -> (n, rest) | [] -> (Printf.sprintf "City %d" g.next_id, []) in
    let g = with_civ g u.side { civ with names } in
    reveal
      { g with
        units = List.filter (fun (u' : unit_) -> u'.id <> u.id) g.units;
        cities = g.cities @ [ { name; owner = u.side; at = u.at; size = 1; food = 0; shields = 0; making = Warriors } ];
        news = Printf.sprintf "the %s found %s" (people u.side) name }

(* the defender's strength: defence, half again on hills and again in a
 * city *)
let defence (g : game) (u : unit_) : float =
  let _, d, _ = stats u.kind in
  float_of_int d
  *. (if world.(index u.at) = Hills then 1.5 else 1.)
  *. if city_at g u.at <> None then 1.5 else 1.

(* the chance that [a] wins against the best defender of [c] *)
let odds (g : game) (a : unit_) (c : int * int) : float =
  let att, _, _ = stats a.kind in
  match List.filter (fun (u : unit_) -> u.side <> a.side) (units_at g c) with
  | [] -> 1.
  | defenders ->
      let d = List.fold_left (fun d u -> Float.max d (defence g u)) 0. defenders in
      float_of_int att /. (float_of_int att +. d)

let roll (g : game) : float * game =
  (float_of_int (Hashtbl.hash (g.rolls, 7919) mod 1000) /. 1000., { g with rolls = g.rolls + 1 })

let winner_of (g : game) : side option =
  let alive side = List.exists (fun (c : city) -> c.owner = side) g.cities || List.exists (fun (u : unit_) -> u.side = side) g.units in
  if List.mem Philosophy g.us.known then Some Us
  else if List.mem Philosophy g.them.known then Some Them
  else if not (alive Them) then Some Us
  else if not (alive Us) then Some Them
  else None

(* A step of [u] towards [c], next to it: into an enemy, an attack; into
 * an enemy city left empty, its capture. *)
let advance (g : game) (u : unit_) (c : int * int) : game =
  let att, _, _ = stats u.kind in
  let others = List.filter (fun (u' : unit_) -> u'.side <> u.side) (units_at g c) in
  let hostile_city = match city_at g c with Some ct -> ct.owner <> u.side | None -> false in
  let spent g = { g with units = List.map (fun (u' : unit_) -> if u'.id = u.id then { u' with moves = 0 } else u') g.units } in
  if not (on_land c) || distance u.at c <> 1 || u.moves <= 0 then g
  else if others <> [] then
    if att = 0 then g
    else
      let r, g = roll g in
      if r < odds g u c then
        (* the defender lost, and every unit on its tile with it *)
        spent
          { g with
            units = List.filter (fun (u' : unit_) -> not (u'.at = c && u'.side <> u.side)) g.units;
            news = Printf.sprintf "%s %s beat the %s on their tile" (people u.side) (kind_name u.kind) (people (enemy u.side)) }
      else
        { g with
          units = List.filter (fun (u' : unit_) -> u'.id <> u.id) g.units;
          news = Printf.sprintf "%s %s lost their attack" (people u.side) (kind_name u.kind) }
  else if hostile_city && att = 0 then g
  else
    let g =
      if not hostile_city then g
      else
        let taken (ct : city) =
          if ct.at <> c then Some ct
          else if ct.size <= 1 then None
          else Some { ct with owner = u.side; size = ct.size - 1; food = 0; shields = 0; making = Warriors }
        in
        { g with cities = List.filter_map taken g.cities; news = Printf.sprintf "the %s take a city" (people u.side) }
    in
    let g = { g with units = List.map (fun (u' : unit_) -> if u'.id = u.id then { u' with at = c; moves = u'.moves - 1 } else u') g.units } in
    let g = reveal g in
    { g with winner = winner_of g }

let wait (g : game) (u : unit_) : game =
  { g with units = List.map (fun (u' : unit_) -> if u'.id = u.id then { u' with moves = 0 } else u') g.units }

(* the unit you move now: the first of yours with a move left *)
let active (g : game) : unit_ option = List.find_opt (fun (u : unit_) -> u.side = Us && u.moves > 0) g.units

let next_in (l : 'a list) (x : 'a) : 'a =
  let rec go = function a :: (b :: _ as rest) -> if a = x then b else go rest | _ -> List.hd l in
  go l

let set_making (g : game) (c : int * int) : game =
  { g with
    cities =
      List.map (fun (ct : city) -> if ct.at = c && ct.owner = Us then { ct with making = next_in (buildable g.us.known) ct.making } else ct) g.cities }

let set_research (g : game) : game =
  match available g.us.known with [] -> g | l -> { g with us = { g.us with research = next_in l g.us.research } }

(*****************************************************************************)
(* The rival *)
(*****************************************************************************)

let military (k : kind) : bool = k <> Settlers

(* the best free place for a city within reach of [from] *)
let site (g : game) (from : int * int) : (int * int) option =
  let score c = List.fold_left (fun s c' -> let f, sh, t = yield_of world.(index c') in s + (3 * f) + (2 * sh) + t) 0 (around 1 c) in
  List.fold_left
    (fun best c -> if can_found g c && (match best with None -> true | Some b -> score c - distance from c > score b - distance from b) then Some c else best)
    None (around 5 from)

(* one step towards [target]: the land tile next to [from] that is
 * nearest to it *)
let step_towards (from : int * int) (target : int * int) : int * int =
  List.fold_left (fun b c -> if on_land c && distance c target < distance b target then c else b) from (List.tl (around 1 from))

(* the rival's move for one unit, as long as it has moves left *)
let rec ai_unit (g : game) (id : int) : game =
  match List.find_opt (fun (u : unit_) -> u.id = id) g.units with
  | None -> g
  | Some u when u.moves <= 0 -> g
  | Some u ->
      let mine = List.filter (fun (c : city) -> c.owner = u.side) g.cities in
      let g' =
        if u.kind = Settlers then
          if can_found g u.at && (mine = [] || site g u.at = Some u.at) then found g u
          else match site g u.at with Some c -> advance g u (step_towards u.at c) | None -> wait g u
        else
          let targets = List.filter (fun c -> (units_at g c <> [] && List.exists (fun (e : unit_) -> e.side <> u.side) (units_at g c)) || match city_at g c with Some ct -> ct.owner <> u.side | None -> false) (List.tl (around 1 u.at)) in
          match List.find_opt (fun c -> odds g u c >= 0.5) targets with
          | Some c -> advance g u c
          | None ->
              let home = List.find_opt (fun (c : city) -> c.at = u.at) mine in
              let guards = List.filter (fun (u' : unit_) -> u'.at = u.at && u'.side = u.side && military u'.kind) g.units in
              let army = List.filter (fun (u' : unit_) -> u'.side = u.side && military u'.kind) g.units in
              let theirs = List.filter (fun (c : city) -> c.owner <> u.side) g.cities in
              if home <> None && List.hd guards = u then wait g u
              else if List.length army >= 4 && theirs <> [] then
                let goal = List.fold_left (fun (b : city) (c : city) -> if distance u.at c.at < distance u.at b.at then c else b) (List.hd theirs) theirs in
                let next = step_towards u.at goal.at in
                if next = u.at then wait g u else advance g u next
              else
                (* to the nearest city of its own with no guard, or stay *)
                let empty = List.filter (fun (c : city) -> not (List.exists (fun (u' : unit_) -> u'.at = c.at && u'.side = u.side && military u'.kind) g.units)) mine in
                match empty with
                | c :: _ when c.at <> u.at -> let next = step_towards u.at c.at in if next = u.at then wait g u else advance g u next
                | _ -> wait g u
      in
      if g' == g then wait g u else ai_unit g' id

let ai_research (g : game) : game =
  let civ = g.them in
  let prefer = [ Bronze_working; Warrior_code; Horseback_riding; Alphabet; Masonry; Mathematics ] in
  let open_ = available civ.known in
  match List.find_opt (fun t -> List.mem t open_) prefer, open_ with
  | Some t, _ -> { g with them = { civ with research = t } }
  | None, t :: _ -> { g with them = { civ with research = t } }
  | None, [] -> g

(* what each of its cities builds: settlers while it has few cities,
 * else its strongest attacker *)
let ai_making (g : game) : game =
  let mine = List.filter (fun (c : city) -> c.owner = Them) g.cities in
  let settling = List.exists (fun (u : unit_) -> u.side = Them && u.kind = Settlers) g.units in
  let strongest = List.fold_left (fun b k -> let a, _, _ = stats k and ab, _, _ = stats b in if a > ab then k else b) Warriors (buildable g.them.known) in
  { g with
    cities =
      List.map
        (fun (c : city) ->
          if c.owner <> Them then c
          else if List.length mine < 4 && (not settling) && c.size >= 2 then { c with making = Settlers }
          else if c.making = Settlers && (settling || List.length mine >= 4) then { c with making = strongest }
          else if c.making <> Settlers then { c with making = strongest }
          else c)
        g.cities }

let ai_turn (g : game) : game =
  let g = ai_making (ai_research g) in
  let ids = List.filter_map (fun (u : unit_) -> if u.side = Them then Some u.id else None) g.units in
  List.fold_left ai_unit g ids

(*****************************************************************************)
(* The turn *)
(*****************************************************************************)

(* research: a civilization's trade, all of it, into its next advance *)
let science (g : game) (side : side) : game =
  let civ = civ_of g side in
  let beakers = civ.beakers + List.fold_left (fun b (c : city) -> if c.owner = side then b + trade c else b) 0 g.cities in
  if beakers < research_cost civ.known || List.mem civ.research civ.known then with_civ g side { civ with beakers }
  else
    let known = civ.known @ [ civ.research ] in
    let research = match available known with t :: _ -> t | [] -> civ.research in
    let g = with_civ g side { civ with known; beakers = 0; research } in
    { g with news = Printf.sprintf "the %s learn %s" (people side) (tech_name civ.research) }

(* the end of a turn: the rival plays, every city works, both learn,
 * every unit gets its moves back *)
let end_turn (g : game) : game =
  let g = ai_turn g in
  let g, made =
    List.fold_left
      (fun (g, made) (c : city) ->
        let c', unit = city_turn c in
        ({ g with cities = List.map (fun (c0 : city) -> if c0.at = c.at then c' else c0) g.cities },
         match unit with Some k -> (c.owner, k, c.at) :: made | None -> made))
      (g, []) g.cities
  in
  let g =
    List.fold_left
      (fun g (side, kind, at) -> { g with units = g.units @ [ { id = g.next_id; side; kind; at; moves = 0 } ]; next_id = g.next_id + 1 })
      g (List.rev made)
  in
  let g = science (science g Us) Them in
  let g = { g with units = List.map (fun (u : unit_) -> let _, _, m = stats u.kind in { u with moves = m }) g.units; turn = g.turn + 1 } in
  let g = reveal g in
  { g with winner = winner_of g }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type scene = Title | Playing of game | Over of game
type model = scene Scene2d.t

let initial_model : model = Scene2d.start Title

let cell_at (mx : float) (my : float) : int * int =
  (int_of_float (Float.floor ((mx -. left) /. size)), int_of_float (Float.floor ((top -. my) /. size)))

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let key f = Scene2d.pressed f scenes in
  let letter l = key (fun k -> Set_.mem l k.keys) in
  let g = if letter "t" then { g with tree = not g.tree } else g in
  let g = if letter "r" then set_research g else g in
  let g = if computer.mouse.mclick then set_making g (cell_at computer.mouse.mx computer.mouse.my) else g in
  match active g with
  | Some u ->
      let x, y = u.at in
      let dir =
        if key (fun k -> k.kleft) then Some (x - 1, y)
        else if key (fun k -> k.kright) then Some (x + 1, y)
        else if key (fun k -> k.kup) then Some (x, y - 1)
        else if key (fun k -> k.kdown) then Some (x, y + 1)
        else None
      in
      if letter "p" then set_making g u.at
      else if letter "b" then found g u
      else if key (fun k -> k.kspace) then wait g u
      else if key (fun k -> k.kenter) then end_turn g
      else (match dir with Some c -> advance g u c | None -> g)
  | None ->
      if letter "p" then match List.find_opt (fun (c : city) -> c.owner = Us) g.cities with Some c -> set_making g c.at | None -> g
      else if key (fun k -> k.kenter) then end_turn g
      else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (Playing (new_game ())) scenes else scenes
  | Over _ -> if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go Title scenes else scenes
  | Playing g ->
      let g = update_game computer scenes g in
      if g.winner <> None then Scene2d.go (Over g) scenes else { scenes with scene = Playing g }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (sz : float) (s : string) : shape = words color s |> scale sz

let center ((x, y) : int * int) : float * float =
  (left +. ((float_of_int x +. 0.5) *. size), top -. ((float_of_int y +. 0.5) *. size))

let at (c : int * int) (shape : shape) : shape = let x, y = center c in move x y shape

let terrain_color = function
  | Ocean -> rgb 40 80 160
  | Grass -> rgb 110 170 80
  | Plains -> rgb 190 180 100
  | Forest -> rgb 40 110 50
  | Hills -> rgb 150 120 80

let side_color = function Us -> rgb 230 60 60 | Them -> rgb 240 200 60
let letter_of = function Settlers -> "S" | Warriors -> "W" | Phalanx -> "P" | Archers -> "A" | Horsemen -> "H" | Catapult -> "C"

let view_map (model : model) (g : game) : shape list =
  let seen c = g.us.seen.(index c) in
  let tiles =
    List.init (cols * rows) (fun i ->
        let c = cell i in
        at c (square (if seen c then terrain_color world.(i) else black) size))
  in
  let cities =
    List.concat_map
      (fun (ct : city) ->
        if not (seen ct.at) then []
        else
          [ at ct.at (square (side_color ct.owner) (size -. 2.));
            at ct.at (square black (size -. 7.));
            at ct.at (text white 1.1 (string_of_int ct.size));
            (* the size in the label too: a unit in the city hides the square *)
            at ct.at (text white 1.0 (Printf.sprintf "%s %d" ct.name ct.size) |> move 0. (-.size *. 0.85)) ])
      g.cities
  in
  let blink = List.length (Scene2d.blink 0.5 model [ square black 1. ]) > 0 in
  let active_id = match active g with Some u -> u.id | None -> -1 in
  let units =
    List.concat_map
      (fun (u : unit_) ->
        if not (seen u.at) || (u.id = active_id && not blink) then []
        else
          [ at u.at (circle black (size *. 0.42));
            at u.at (circle (side_color u.side) (size *. 0.36));
            at u.at (text black 1.0 (letter_of u.kind)) ])
      (List.rev g.units)
  in
  tiles @ cities @ units

let panel (g : game) : shape list =
  let x = 410. in
  let line k color s = text color 1.4 s |> move x (330. -. (float_of_int k *. 26.)) in
  let city_here = match active g with Some u -> city_at g u.at | None -> List.find_opt (fun (c : city) -> c.owner = Us) g.cities in
  [ line 0 (side_color Us) "the Romans";
    line 1 white (Printf.sprintf "turn %d, %s" g.turn (year g.turn));
    line 3 (rgb 200 200 210) "researching";
    line 4 white (tech_name g.us.research);
    line 5 white (Printf.sprintf "%d / %d  (r)" g.us.beakers (research_cost g.us.known));
    line 6 (rgb 200 200 210) (Printf.sprintf "%d advances known (t)" (List.length g.us.known));
    line 8 (rgb 200 200 210) (match active g with Some u -> "moving: " ^ kind_name u.kind | None -> "Enter: next turn") ]
  @ (match city_here with
    | Some c when c.owner = Us ->
        let f, s, t = output c in
        [ line 10 white c.name;
          line 11 (rgb 200 200 210) (Printf.sprintf "size %d, food %d/%d" c.size c.food (food_box c.size));
          line 12 (rgb 200 200 210) (Printf.sprintf "%d food %d shields %d trade" f s t);
          line 13 white (Printf.sprintf "%s %d/%d (p)" (kind_name c.making) c.shields (cost c.making)) ]
    | _ -> [])
  @ [ line 16 (side_color Them) "the Babylonians";
      line 17 (rgb 200 200 210) (Printf.sprintf "%d cities, %d advances" (List.length (List.filter (fun (c : city) -> c.owner = Them) g.cities)) (List.length g.them.known)) ]

(* the tree of advances, one column per depth *)
let view_tree (g : game) : shape list =
  let column d = List.filter (fun t -> depth t = d) techs in
  let pos t =
    let d = depth t in
    let k = let rec find i = function [] -> 0 | t' :: r -> if t' = t then i else find (i + 1) r in find 0 (column d) in
    (-380. +. (float_of_int d *. 250.), 250. -. (float_of_int k *. 80.))
  in
  let color t =
    if List.mem t g.us.known then rgb 90 180 100
    else if t = g.us.research then rgb 240 200 60
    else if List.mem t (available g.us.known) then rgb 200 200 210
    else rgb 90 90 100
  in
  let lines =
    List.concat_map
      (fun t ->
        List.map
          (fun p ->
            let x1, y1 = pos p and x2, y2 = pos t in
            let x1 = x1 +. 85. and x2 = x2 -. 85. in
            let dx = x2 -. x1 and dy = y2 -. y1 in
            rectangle (rgb 120 120 140) (Float.sqrt ((dx *. dx) +. (dy *. dy))) 2.
            |> rotate (Float.atan2 dy dx *. 180. /. Float.pi)
            |> move ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.))
          (requires t))
      techs
  in
  let boxes =
    List.concat_map
      (fun t ->
        let x, y = pos t in
        [ rectangle (color t) 170. 34. |> move x y; text black 1.4 (tech_name t) |> move x y ])
      techs
  in
  [ text white 3. "THE TREE OF ADVANCES" |> move 0. 360. ]
  @ lines @ boxes
  @ [ text (rgb 200 200 210) 1.5 "known: green   researching: yellow   open: white   (r to change, t to close)" |> move 0. (-360.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 20 22 30) screen.width screen.height
  ::
  (match model.scene with
  | Title ->
      [ text (rgb 240 200 60) 6. "TINY CIVILIZATION" |> move 0. 150.;
        text white 2. "from one band of settlers in 4000 BC:" |> move 0. 60.;
        text white 2. "found cities, learn, and outlast the Babylonians" |> move 0. 25.;
        text (rgb 200 200 210) 1.8 "by conquest, or by being the first to Philosophy" |> move 0. (-20.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move 0. (-200.) ]
  | Playing g when g.tree -> view_tree g
  | Playing g ->
      view_map model g @ panel g
      @ [ text (rgb 250 220 120) 1.6 g.news |> move (-90.) (-190.);
          text (rgb 170 170 185) 1.4 "arrows move   b build a city   space wait   p production   r research   t tree   Enter end turn"
          |> move 0. (-380.) ]
  | Over g ->
      let us_won = g.winner = Some Us in
      view_map model g
      @ [ rectangle black screen.width screen.height |> fade 0.6;
          text (if us_won then rgb 120 230 140 else rgb 230 80 80) 4. (if us_won then "ROME ENDURES" else "ROME HAS FALLEN") |> move 0. 80.;
          text white 2. (Printf.sprintf "%s, turn %d: %s" (year g.turn) g.turn (if List.mem Philosophy (civ_of g (Option.get g.winner)).known then "the first to Philosophy" else "by conquest")) |> move 0. 20. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move 0. (-200.) ])

let help =
  {|TinyCivilization
  arrows      move the unit that blinks (into an enemy: attack)
  b           settlers: build a city here
  space       this unit waits for the next turn
  p           what the city under the unit builds (or click a city)
  r           what to research next
  t           the tree of advances
  Enter       the end of the turn
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
