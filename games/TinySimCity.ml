(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of SimCity (Will Wright, Maxis, 1989): you lay out
 * roads, power lines and zones -- residential, commercial, industrial --
 * and the city builds itself on them, month after month, or doesn't.
 *
 *   1 .. 7       the tools: road, power line, R, C, I zones, power
 *                plant, bulldozer (or click the toolbar)
 *   mouse        point and click (hold to draw a road); or the arrows
 *                to move and space to build
 *   v            the view: the city, its power, its pollution
 *   - =          the tax rate
 *   f            fast
 *
 * SimCity started as the level editor of Wright's helicopter game, Raid
 * on Bungeling Bay (1984): he found building the islands more fun than
 * bombing them. Publishers would not sell a game that could not be won
 * nor lost, so he and Jeff Braun founded Maxis to do it themselves. Its
 * source was released in 2008 under the GPL, as Micropolis, for the One
 * Laptop per Child. (Names and dates from memory, to check.)
 *
 * What is new here, against every other game of this directory:
 *
 *  - The game is the simulation. No enemy, no goal, nothing to win --
 *    Wright called it a "software toy". The player only ever edits
 *    tiles; everything interesting happens in one function, the month
 *    ([month]), which the player never calls. The code worth reading
 *    is that one, not the controls.
 *
 *  - The zones need each other ([valves]). People want jobs, shops want
 *    customers, factories want workers, and each month the city
 *    measures what it lacks: SimCity's famous R C I bars, on the right
 *    of the screen. A zone grows only while its bar is up:
 *
 *        homes (R) ----- workers -----> factories (I) --> sold outside
 *            ^     \---- customers ---> shops (C)
 *            |                              |
 *            +--------- jobs <--------------+
 *
 *    More jobs than workers wants homes; more workers wants jobs. It
 *    feeds on itself, which is why a city grows at all -- and the tax
 *    rate pushes on every bar at once.
 *
 *  - The city keeps maps you do not see ([power_scan], [spread]): which
 *    tiles have power, how dirty the air is. They are the simulation's
 *    real state, and "v" shows them. Power is a flood fill from the
 *    plants, through the tiles that conduct: power lines, and zones --
 *    but not roads, the lesson every SimCity mayor learns once. The
 *    pollution is a blur: each month every tile becomes the average of
 *    itself and its four neighbours, a little less, plus what it gives
 *    off (a factory, a plant), the same filter that blurs an image.
 *    Homes will not grow in the smog, and move out of it.
 *
 *  - The dice are a hash ([chance]). SimCity rolls one per zone per
 *    month, so a neighbourhood grows in patches instead of all at once;
 *    here the roll is a hash of the place and the month, so the same
 *    city replays the same, month for month, in the tests and golden
 *    frames.
 *
 * What it uses: Scene2d. No kit, no Tilemap (the tiles change every
 * month and are drawn from the model's array, as TinyTowerDefense
 * does), no ai/Pathfind: nobody walks here -- which is the first
 * exercise.
 *
 * Exercises: traffic (SimCity's cars: from each zone a random walk
 * along the roads to a zone it needs, the roads they crowd, and the
 * pollution they add), crime and police stations (another map, spread
 * from each station as pollution is from each factory), land value
 * (water and parks raising it), a plant's capacity (too many zones, and
 * the lights go out), bridges, the 3x3 zones and their bigger
 * buildings, and the disasters.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The land *)
(*****************************************************************************)

let cols = 32
let rows = 24
let size = 26.
let left = -490.
let top = 380.

type kind = Residential | Commercial | Industrial

(* a zone's level: 0 an empty lot, up to 3, a block of flats or a
 * factory *)
type tile = Land | Water | Road | Wire | Zone of kind * int | Plant

let index ((x, y) : int * int) : int = (y *.. cols) +.. x
let cell (i : int) : int * int = (i mod cols, i /.. cols)
let inside ((x, y) : int * int) : bool = x >= 0 && x < cols && y >= 0 && y < rows
let neighbours ((x, y) : int * int) : (int * int) list =
  List.filter inside [ (x -.. 1, y); (x +.. 1, y); (x, y -.. 1); (x, y +.. 1) ]

let center ((x, y) : int * int) : number * number =
  (left + ((float_of_int x + 0.5) * size), top - ((float_of_int y + 0.5) * size))

let cell_at (mx : number) (my : number) : int * int =
  (int_of_float (Float.floor ((mx - left) / size)), int_of_float (Float.floor ((top - my) / size)))

(* a river, winding down the map *)
let river ((x, y) : int * int) : bool =
  Float.abs (float_of_int x - (23. + (2. * sin (float_of_int y / 4.)))) < 1.

(*****************************************************************************)
(* The maps: power and pollution *)
(*****************************************************************************)

(* A zone passes power on to the next, so a block of zones needs one
 * line to it; a road does not. *)
let conducts = function Wire | Plant | Zone _ -> true | Land | Water | Road -> false

(* a flood fill from every plant, through what conducts *)
let power_scan (tiles : tile array) : bool array =
  let powered = Array.make (cols *.. rows) false in
  let rec flood = function
    | [] -> ()
    | c :: rest ->
        if (not powered.(index c)) && conducts tiles.(index c) then begin
          powered.(index c) <- true;
          flood (neighbours c @ rest)
        end
        else flood rest
  in
  flood (List.filter (fun c -> tiles.(index c) = Plant) (List.init (cols *.. rows) cell));
  powered

(* what a tile gives off in a month *)
let emission = function
  | Zone (Industrial, level) -> 0.12 * float_of_int level
  | Plant -> 0.3
  | Land | Water | Road | Wire | Zone _ -> 0.

(* A month of air: each tile the average of itself and its neighbours
 * (a blur), a tenth of it gone, plus what the tile gives off. A big
 * factory alone settles at about 0.6 on its own tile, 0.2 next to it,
 * 0.07 two tiles away; a coal plant much the same; a small factory
 * hardly shows. *)
let spread (tiles : tile array) (pollution : number array) : number array =
  Array.init (cols *.. rows) (fun i ->
      let around = cell i :: neighbours (cell i) in
      let sum = List.fold_left (fun s c -> s + pollution.(index c)) 0. around in
      Float.min 1. ((0.9 * sum / float_of_int (List.length around)) + emission tiles.(i)))

let smog = 0.15 (* more, and homes won't grow: next to a big factory or a plant *)

(*****************************************************************************)
(* The valves *)
(*****************************************************************************)

(* how many live, or work, in a zone of each level *)
let per_level = function Residential -> 20 | Commercial | Industrial -> 10

type census = { residents : int; shops : int; factories : int (* the last two in jobs *) }

let census (tiles : tile array) : census =
  Array.fold_left
    (fun c t ->
      match t with
      | Zone (Residential, l) -> { c with residents = c.residents +.. (l *.. per_level Residential) }
      | Zone (Commercial, l) -> { c with shops = c.shops +.. (l *.. per_level Commercial) }
      | Zone (Industrial, l) -> { c with factories = c.factories +.. (l *.. per_level Industrial) }
      | Land | Water | Road | Wire | Plant -> c)
    { residents = 0; shops = 0; factories = 0 } tiles

type demand = { r : int; c : int; i : int }

(* What the city lacks, for each kind of zone: positive, it wants more.
 * Half the residents work. Homes follow the jobs, and some people come
 * anyway (+20: a town starts from nothing); shops follow the workers'
 * spending; factories follow the workers too, and sell outside the
 * town (+10). Every point of tax over 7% takes 4 off every bar. *)
let valves (census : census) (tax : int) : demand =
  let workers = census.residents /.. 2 and jobs = census.shops +.. census.factories in
  let taxed d = d -.. ((tax -.. 7) *.. 4) in
  { r = taxed (jobs -.. workers +.. 20);
    c = taxed ((workers /.. 2) -.. census.shops);
    i = taxed ((workers /.. 2) -.. census.factories +.. 10) }

(*****************************************************************************)
(* The city *)
(*****************************************************************************)

type tool = Road_tool | Wire_tool | Zone_tool of kind | Plant_tool | Bulldozer

let tools = [ Road_tool; Wire_tool; Zone_tool Residential; Zone_tool Commercial; Zone_tool Industrial; Plant_tool; Bulldozer ]

let cost = function Road_tool -> 10 | Wire_tool -> 5 | Zone_tool _ -> 50 | Plant_tool -> 1000 | Bulldozer -> 1

let tool_name = function
  | Road_tool -> "road"
  | Wire_tool -> "power line"
  | Zone_tool Residential -> "R zone"
  | Zone_tool Commercial -> "C zone"
  | Zone_tool Industrial -> "I zone"
  | Plant_tool -> "power plant"
  | Bulldozer -> "bulldozer"

(* what a tool makes of a tile, if it can: everything is built on bare
 * land, and the bulldozer clears anything but water *)
let result (tool : tool) (tile : tile) : tile option =
  match (tool, tile) with
  | _, Water -> None
  | Bulldozer, Land -> None
  | Bulldozer, _ -> Some Land
  | Road_tool, Land -> Some Road
  | Wire_tool, Land -> Some Wire
  | Zone_tool k, Land -> Some (Zone (k, 0))
  | Plant_tool, Land -> Some Plant
  | (Road_tool | Wire_tool | Zone_tool _ | Plant_tool), _ -> None

type view = City | Power | Pollution

type city = {
  tiles : tile array;
  powered : bool array;
  pollution : number array;
  funds : int;
  month : int; (* since January 1900 *)
  tax : int; (* percent *)
  demand : demand; (* as last measured *)
  tool : tool;
  cursor : int * int;
  view : view;
  fast : bool;
  frame : int; (* frames into this month *)
}

let new_city () : city =
  let tiles = Array.init (cols *.. rows) (fun i -> if river (cell i) then Water else Land) in
  { tiles; powered = Array.make (cols *.. rows) false; pollution = Array.make (cols *.. rows) 0.; funds = 3000;
    month = 0; tax = 7; demand = valves (census tiles) 7; tool = Road_tool; cursor = (8, 10); view = City;
    fast = false; frame = 0 }

let build (city : city) (c : int * int) : city =
  if not (inside c) then city
  else
    match result city.tool city.tiles.(index c) with
    | Some tile when cost city.tool <= city.funds ->
        let tiles = Array.copy city.tiles in
        tiles.(index c) <- tile;
        { city with tiles; funds = city.funds -.. cost city.tool; powered = power_scan tiles }
    | _ -> city

(*****************************************************************************)
(* The month *)
(*****************************************************************************)

(* one roll of a die with [n] faces, for the zone at [c] this month *)
let chance (c : int * int) (month : int) (n : int) : bool = Hashtbl.hash (c, month) mod n = 0

let road_access (tiles : tile array) (c : int * int) : bool =
  List.exists (fun n -> tiles.(index n) = Road) (neighbours c)

(* A zone's month. Cut off -- no power, no road, or homes in the smog --
 * it decays; otherwise it grows while its bar is up and shrinks when
 * the bar is far down, one level at a time, on a roll of the die. *)
let zone_month (city : city) (c : int * int) (kind : kind) (level : int) : int =
  let d = match kind with Residential -> city.demand.r | Commercial -> city.demand.c | Industrial -> city.demand.i in
  let cut_off =
    (not city.powered.(index c)) || (not (road_access city.tiles c))
    || (kind = Residential && city.pollution.(index c) > smog)
  in
  if cut_off then if level > 0 && chance c city.month 2 then level -.. 1 else level
  else if d > 0 && level < 3 && chance c city.month 3 then level +.. 1
  else if d < -30 && level > 0 && chance c city.month 3 then level -.. 1
  else level

(* what the city pays each month for what it keeps up *)
let upkeep (tiles : tile array) : int =
  let count t = Array.fold_left (fun n t' -> if t' = t then n +.. 1 else n) 0 tiles in
  (count Road /.. 4) +.. (count Wire /.. 8)

let taxes (census : census) (tax : int) : int =
  (census.residents +.. census.shops +.. census.factories) *.. tax /.. 100

(* The simulation: the maps measured, then every zone's month, the air,
 * and the budget. The player never calls it; it is the game. *)
let month (city : city) : city =
  let powered = power_scan city.tiles in
  let census = census city.tiles in
  let city = { city with powered; demand = valves census city.tax } in
  let tiles =
    Array.mapi (fun i t -> match t with Zone (k, l) -> Zone (k, zone_month city (cell i) k l) | t -> t) city.tiles
  in
  { city with
    tiles;
    pollution = spread city.tiles city.pollution;
    funds = city.funds +.. taxes census city.tax -.. upkeep city.tiles;
    month = city.month +.. 1 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type scene = Title | Playing of city
type model = scene Scene2d.t

let initial_model : model = Scene2d.start Title

let frames_a_month = 30

(* the toolbar under the map *)
let toolbar_y = -285.
let button_w = 118.
let button_x (k : int) : number = left + 55. + (float_of_int k * button_w)

let tool_clicked (m : mouse) : tool option =
  if m.mclick && Float.abs (m.my - toolbar_y) < 18. then
    List.nth_opt tools (int_of_float (Float.floor ((m.mx - left) / button_w)))
  else None

let update_city (computer : computer) (scenes : model) (city : city) : city =
  let key f = Scene2d.pressed f scenes in
  let digit d = key (fun k -> Set_.mem d k.keys) in
  let kb = computer.keyboard and mouse = computer.mouse in
  (* an arrow moves the cursor once, then every 6 frames while held *)
  let arrow f = key f || (f kb && scenes.frames mod 6 = 0) in
  let tool =
    match tool_clicked mouse with
    | Some t -> t
    | None -> (
        match List.find_opt (fun k -> digit (string_of_int (k +.. 1))) (List.init 7 Fun.id) with
        | Some k -> List.nth tools k
        | None -> city.tool)
  in
  let x, y = city.cursor in
  let cursor =
    ( x +.. (if arrow (fun k -> k.kright) then 1 else 0) -.. (if arrow (fun k -> k.kleft) then 1 else 0),
      y +.. (if arrow (fun k -> k.kdown) then 1 else 0) -.. if arrow (fun k -> k.kup) then 1 else 0 )
  in
  let under_mouse = cell_at mouse.mx mouse.my in
  let cursor =
    if (mouse.mdx <> 0. || mouse.mdy <> 0.) && inside under_mouse then under_mouse
    else if inside cursor then cursor
    else city.cursor
  in
  let view =
    if digit "v" then match city.view with City -> Power | Power -> Pollution | Pollution -> City else city.view
  in
  let tax = max 0 (min 20 (city.tax +.. (if digit "=" then 1 else 0) -.. if digit "-" then 1 else 0)) in
  let fast = if digit "f" then not city.fast else city.fast in
  let city = { city with tool; cursor; view; tax; fast } in
  let city = if kb.kspace || (mouse.mdown && inside under_mouse) then build city cursor else city in
  let frame = city.frame +.. if city.fast then 4 else 1 in
  if frame >= frames_a_month then month { city with frame = frame -.. frames_a_month } else { city with frame }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) scenes then Scene2d.go (Playing (new_city ())) scenes else scenes
  | Playing city -> { scenes with scene = Playing (update_city computer scenes city) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let at (c : int * int) (shape : shape) : shape = let x, y = center c in move x y shape

let grass = rgb 120 165 90
let kind_color = function
  | Residential -> rgb 150 215 140
  | Commercial -> rgb 140 180 240
  | Industrial -> rgb 230 205 110

let building_color = function
  | Residential -> rgb 60 140 70
  | Commercial -> rgb 50 90 190
  | Industrial -> rgb 170 120 40

let letter = function Residential -> "R" | Commercial -> "C" | Industrial -> "I"

(* a line from a tile's middle towards each neighbour [joins] *)
let joining (tiles : tile array) (c : int * int) (joins : tile -> bool) (color : color) (w : number) : shape list =
  List.filter_map
    (fun ((nx, ny) as n) ->
      if not (joins tiles.(index n)) then None
      else
        let x, y = c in
        let dx = float_of_int (nx -.. x) and dy = float_of_int (y -.. ny) in
        let len = size / 2. in
        Some
          ((if dx <> 0. then rectangle color len w else rectangle color w len)
          |> move (dx * len / 2.) (dy * len / 2.)))
    (neighbours c)

let tile_shapes (city : city) (i : int) : shape list =
  let c = cell i in
  let ground = square grass size in
  let here =
    match city.tiles.(i) with
    | Land -> [ ground ]
    | Water -> [ square (rgb 60 110 200) size ]
    | Road ->
        [ ground; square (rgb 90 90 95) (size * 0.5) ]
        @ joining city.tiles c (fun t -> t = Road) (rgb 90 90 95) (size * 0.5)
        @ [ square (rgb 230 220 120) 2. ]
    | Wire ->
        [ ground; square (rgb 60 50 40) 4. ]
        @ joining city.tiles c conducts (rgb 60 50 40) 2.
    | Plant ->
        [ square (rgb 110 110 120) size; rectangle (rgb 70 70 80) (size * 0.25) (size * 0.6) |> move (size * 0.2) 0.;
          circle (rgb 200 200 200) (size * 0.18) |> move (size * 0.2) (size * 0.35) ]
    | Zone (k, level) ->
        [ square (kind_color k) size; square (rgb 250 250 250) (size - 3.) |> fade 0.25 ]
        @ (if level = 0 then [ text (building_color k) 1.1 (letter k) ]
           else
             (* one building, bigger at each level, and its roof *)
             let w = size * (0.3 + (0.17 * float_of_int level)) in
             [ square (building_color k) w; square (rgb 255 255 255) (w * 0.45) |> fade 0.3 ])
        @ if city.powered.(i) then [] else [ circle (rgb 230 60 50) 3. |> move (size * 0.32) (size * 0.32) ]
  in
  let overlay =
    match city.view with
    | City -> []
    | Power ->
        if city.powered.(i) then [ square (rgb 255 230 60) size |> fade 0.55 ]
        else if conducts city.tiles.(i) then [ square (rgb 230 60 50) size |> fade 0.55 ]
        else [ square black size |> fade 0.35 ]
    | Pollution ->
        let p = city.pollution.(i) in
        if p < 0.02 then [] else [ square (rgb 90 60 30) size |> fade (Float.min 0.85 (p * 1.2)) ]
  in
  List.map (at c) (here @ overlay)

let month_names = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

(* the city's most pressing problem, in the words of an advisor *)
let advice (city : city) : string =
  let zones = List.filter (fun i -> match city.tiles.(i) with Zone _ -> true | _ -> false) (List.init (cols *.. rows) Fun.id) in
  if zones = [] then "zone some land (R, C and I), with roads beside it and power"
  else if not (Array.exists (fun t -> t = Plant) city.tiles) then "the zones need a power plant (6)"
  else if List.exists (fun i -> not city.powered.(i)) zones then
    "some zones have no power: zones pass it on, roads don't -- lay a line"
  else if List.exists (fun i -> not (road_access city.tiles (cell i))) zones then "some zones have no road beside them"
  else if
    List.exists
      (fun i -> city.tiles.(i) |> function Zone (Residential, _) -> city.pollution.(i) > smog | _ -> false)
      zones
  then "smog: people are moving out of the homes near industry"
  else if city.tax > 12 then "the taxes are keeping people away"
  else
    (* a bar up, and no lot of that kind left to grow on *)
    let wanted k d =
      d > 0 && not (List.exists (fun i -> match city.tiles.(i) with Zone (k', l) -> k' = k && l < 3 | _ -> false) zones)
    in
    match List.find_opt (fun (k, d, _) -> wanted k d) [ (Residential, city.demand.r, "homes (R)"); (Commercial, city.demand.c, "shops (C)"); (Industrial, city.demand.i, "industry (I)") ] with
    | Some (_, _, what) -> "the city wants more " ^ what ^ ": zone some"
    | None -> ""

(* SimCity's R C I bars: up, the city wants more of that zone *)
let valve_bars (d : demand) : shape list =
  List.concat
    (List.mapi
       (fun k (name, v, k') ->
         let x = 390. + (float_of_int k * 40.) in
         let h = Float.max (-60.) (Float.min 60. (float_of_int v)) in
         [ rectangle (building_color k') 22. (Float.abs h) |> move x (250. + (h / 2.));
           text white 1.6 name |> move x 170. ])
       [ ("R", d.r, Residential); ("C", d.c, Commercial); ("I", d.i, Industrial) ])
  @ [ rectangle white 120. 1. |> move 430. 250.; text (rgb 200 200 210) 1.4 "demand" |> move 430. 330. ]

let view_city (city : city) : shape list =
  let census = census city.tiles in
  List.concat (List.init (cols *.. rows) (tile_shapes city))
  @ [ at city.cursor (square white size |> fade 0.35) ]
  @ valve_bars city.demand
  @ [ text white 1.8 (Printf.sprintf "%s %d" month_names.(city.month mod 12) (1900 +.. (city.month /.. 12)))
      |> move 430. 110.;
      text white 1.6 (Printf.sprintf "$%d" city.funds) |> move 430. 75.;
      text white 1.6 (Printf.sprintf "pop. %d" census.residents) |> move 430. 40.;
      text white 1.6 (Printf.sprintf "tax %d%%" city.tax) |> move 430. 5.;
      text (rgb 200 200 210) 1.4 (match city.view with City -> "view: city" | Power -> "view: power" | Pollution -> "view: pollution")
      |> move 430. (-30.);
      text (rgb 200 200 210) 1.4 (if city.fast then "fast" else "") |> move 430. (-60.) ]
  @ List.concat
      (List.mapi
         (fun k t ->
           let chosen = t = city.tool in
           [ rectangle (if chosen then rgb 250 200 80 else rgb 60 60 75) (button_w - 6.) 32. |> move (button_x k) toolbar_y;
             text (if chosen then black else white) 1.3 (Printf.sprintf "%d %s" (k +.. 1) (tool_name t))
             |> move (button_x k) (toolbar_y + 5.);
             text (if chosen then black else rgb 180 180 190) 1.1 (Printf.sprintf "$%d" (cost t))
             |> move (button_x k) (toolbar_y - 9.) ])
         tools)
  @ [ text (rgb 250 220 120) 1.7 (advice city) |> move (-60.) (-340.);
      text (rgb 170 170 185) 1.4 "v the view    - = the tax    f fast    arrows and space, or the mouse" |> move (-60.) (-380.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 30 32 40) screen.width screen.height
  ::
  (match model.scene with
  | Title ->
      [ text (rgb 250 200 80) 6. "TINY SIM CITY" |> move_y 150.;
        text white 2. "lay out the roads, the power and the zones:" |> move_y 60.;
        text white 2. "the city builds itself, month after month -- or doesn't" |> move_y 25.;
        text (rgb 200 200 210) 1.8 "no enemy, nothing to win: a software toy" |> move_y (-40.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing city -> view_city city)

let help =
  {|TinySimCity
  1 .. 7      road, power line, R, C, I zones, power plant, bulldozer
  mouse       point, click, hold to draw (or the arrows and space)
  v           the view: city, power, pollution
  - =         the tax rate
  f           fast
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
