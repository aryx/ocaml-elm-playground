(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Grand Theft Auto (DMA Design, 1997): a city seen
 * from above, and any car in it is yours.
 *
 *   arrows   walk; in a car, up is the gas, down the brake and reverse,
 *            left and right the wheel
 *   space    get into the car next to you (the driver gets out), or
 *            out of yours
 *
 * GTA came from Dundee, from the studio of Lemmings, and started as a
 * racing game about police cars ("Race'n'Chase") until its testers
 * found it more fun to be chased than to chase. What it kept is the
 * first open world of its kind: a city that goes on without you --
 * traffic, people on the pavements, the police -- in which the game is
 * whatever you do, and the missions, given by ringing phone boxes, are
 * only one of the things to do. Seen from straight above, as a racing
 * game is (Micro Machines, TinyMicroMachines), with the buildings
 * leaning out of the screen. (Names and dates from memory, to check.)
 *
 * What an open world is made of, here:
 *
 *   - The city as a graph ([node], [neighbours]): the crossroads of a
 *     grid of blocks, the roads between them. Traffic ([civilian])
 *     drives it on the right, towards the next crossroads, and there
 *     picks a way on, never back; the police ([cop]) drive the same
 *     graph, but pick the way that goes towards you. People walk round
 *     their block, on its pavement ([ped_at]).
 *   - Any car yours ([enter]): what is driven is only who drives it, a
 *     field of the car. A parked car is taken; a car with someone in it
 *     is taken too, and that is a crime.
 *   - The wanted level ([crime], [cool_down]): each crime a star, and a
 *     police car more after you; out of their sight long enough, a star
 *     less. A police car touching you while you are on foot or stopped
 *     is BUSTED: a fine, and the stars gone.
 *   - The missions ([phones]): a phone box rings, somewhere; walk into
 *     it, and there is a car to get to a place, in a minute. The yellow
 *     arrow at the top of the screen points at where to go, which is
 *     all the map GTA had.
 *   - The camera ([zoom]): higher the faster you go, so that there is
 *     time to see a corner coming -- GTA's, and GTA 2's.
 *   - The buildings ([building]): the trick of this game. Seen from
 *     straight above, a building is its roof; GTA drew its side walls
 *     too, leaning away from the middle of the screen as the camera
 *     moved, which is the look people remember. Each roof corner is
 *     drawn further from the camera's point, the further it is from it
 *     and the taller the building: [p + (p - cam) * height]. The walls
 *     are the four quads between the ground corners and the roof's,
 *     drawn before the roof, which covers the two turned away:
 *
 *          camera .                 roof  +----------+
 *                  \                     /|          |
 *                   \      ground  +----+ |          |
 *                    \             |    | +----------+
 *                                  +----+/
 *
 *     A 2.5D look on the 2D playground, as games/README-2.5d.md
 *     compares them; the whole trick is a line of arithmetic per corner.
 *
 * What it uses: gamekits/racing's Topdown (the cars, the gas, the
 * wheel, the slide; [bounce] off the buildings and [push] between two
 * cars), Camera2d (following you, zoomed by speed), Scene2d. Not
 * Tilemap: the city is a grid of blocks, measured, not typed. Not
 * Physics.
 *
 * Left undone, exercises: guns, and the police's; the health and the
 * WASTED screen; the radio stations, one per kind of car, which were
 * half of GTA's fame; the city of GTA 1's three, and the traffic
 * lights; a car catching fire and blowing up; the people who run when
 * a car comes at them; a mission more than "get there".
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The city *)
(*****************************************************************************)

(* a grid of 5 x 4 blocks, the roads between and around them *)
let nx = 5
let ny = 4
let pitch = 440.
let road = 120.

type node = int * int

(* the crossroads (i, j), 0 <= i <= nx, 0 <= j <= ny *)
let road_x (i : int) : number = (float_of_int i - (float_of_int nx / 2.)) * pitch
let road_y (j : int) : number = (float_of_int j - (float_of_int ny / 2.)) * pitch
let node_at ((i, j) : node) : number * number = (road_x i, road_y j)

let neighbours ((i, j) : node) : node list =
  List.filter (fun (a, b) -> a >= 0 && a <= nx && b >= 0 && b <= ny) [ (i +.. 1, j); (i -.. 1, j); (i, j +.. 1); (i, j -.. 1) ]

let world : Camera2d.rect =
  { left = road_x 0 - (road / 2.); right = road_x nx + (road / 2.); bottom = road_y 0 - (road / 2.); top = road_y ny + (road / 2.) }

(* a block, inside its four roads, and the building on it, a pavement's
 * width in; two blocks are parks *)
let block_rect ((i, j) : node) (inset : number) : Camera2d.rect =
  { left = road_x i + (road / 2.) + inset; right = road_x (i +.. 1) - (road / 2.) - inset;
    bottom = road_y j + (road / 2.) + inset; top = road_y (j +.. 1) - (road / 2.) - inset }

let blocks : node list = List.concat (List.init nx (fun i -> List.init ny (fun j -> (i, j))))
let park (b : node) : bool = b = (1, 1) || b = (3, 2)
(* how far a roof leans, per pixel from the camera's point: its height,
 * over the camera's *)
let height ((i, j) : node) : number = 0.05 + (0.03 * float_of_int (((i *.. 7) +.. (j *.. 3)) mod 4))

let inside (r : Camera2d.rect) (x : number) (y : number) : bool = x > r.left && x < r.right && y > r.bottom && y < r.top

(* in a building, or out of the city: where cars bounce and people stop *)
let blocked (x : number) (y : number) : bool =
  (not (inside world x y)) || List.exists (fun b -> (not (park b)) && inside (block_rect b 20.) x y) blocks

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type driver = Nobody | Civilian | Cop | Player

type car = {
  body : Topdown.t;
  driver : driver;
  color : color;
  from : node; (* the crossroads it comes from, *)
  toward : node; (* and the one it drives to *)
}

(* someone walking round a block, [s] pixels along its pavement *)
type ped = { block : node; s : number; pace : number; alive : bool }

type you = { x : number; y : number; car : int option (* in which of the cars *) }

type city = {
  you : you;
  cars : car list;
  peds : ped list;
  wanted : int; (* the stars, 0 to 4 *)
  unseen : int; (* frames since a police car was near you *)
  money : int;
  phone : int; (* the phone ringing, in [phones] *)
  mission : (int * int) option; (* the place to get a car to, and the frames left *)
  rng : int;
  frames : int;
}

type scene = Title | Playing of city | Busted of city
type model = scene Scene2d.t

(* a pavement's corner, next to a crossroads: (dx, dy) the side, each 1
 * or -1 *)
let corner (n : node) (dx : number) (dy : number) : number * number =
  let x, y = node_at n in
  (x + (dx * 70.), y + (dy * 70.))

(* the phone boxes, and where each one's mission goes *)
let phones : ((number * number) * (number * number)) array =
  [| (corner (1, 0) 1. 1., corner (4, 3) (-1.) (-1.)); (corner (3, 2) 1. 1., corner (0, 4) 1. (-1.));
     (corner (2, 4) (-1.) (-1.), corner (5, 0) (-1.) 1.); (corner (1, 2) 1. (-1.), corner (4, 1) 1. 1.) |]

let random (rng : int) : int = ((rng *.. 1103515245) +.. 12345) land 0x3fffffff

let a_car (driver : driver) (color : color) (from : node) (toward : node) (lane : number) : car =
  let fx, fy = node_at from and tx, ty = node_at toward in
  let heading = Float.atan2 (ty - fy) (tx - fx) * 180. / Float.pi in
  let d = Float.hypot (tx - fx) (ty - fy) in
  (* on the right of the road, a third of the way along *)
  let x = fx + ((tx - fx) / 3.) + ((ty - fy) / d * lane) and y = fy + ((ty - fy) / 3.) - ((tx - fx) / d * lane) in
  { body = { x; y; vx = 0.; vy = 0.; heading; speed = 0.; next = 0 }; driver; color; from; toward }

let start () : city =
  let traffic =
    [ ((0, 0), (1, 0)); ((2, 1), (2, 2)); ((5, 3), (4, 3)); ((3, 4), (3, 3)); ((1, 2), (2, 2)); ((4, 0), (4, 1));
      ((0, 3), (0, 2)); ((2, 4), (3, 4)) ]
  in
  let colors = [| rgb 200 60 50; rgb 60 110 200; rgb 230 190 60; rgb 90 170 90; rgb 170 90 170 |] in
  let cars =
    List.mapi (fun k (f, t) -> a_car Civilian colors.(k mod Array.length colors) f t 30.) traffic
    @ [ a_car Nobody (rgb 240 240 240) (2, 2) (3, 2) (-45.); a_car Nobody (rgb 60 60 60) (1, 3) (1, 4) 45. ]
  in
  let peds =
    List.concat_map
      (fun b -> if park b then [] else [ { block = b; s = 0.; pace = 0.8; alive = true }; { block = b; s = 600.; pace = -0.6; alive = true } ])
      blocks
  in
  let x, y = node_at (2, 2) in
  { you = { x = x + 30.; y = y - 45.; car = None }; cars; peds; wanted = 0; unseen = 0; money = 0; phone = 0; mission = None;
    rng = 7; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The traffic and the police: driving the graph *)
(*****************************************************************************)

let angle_diff (a : number) (b : number) : number = Float.rem (a - b + 540.) 360. - 180.

(* gas and wheel to get to (ax, ay): the wheel by how far off the
 * heading it is, the gas eased in the turns *)
let steer_to (ax : number) (ay : number) (gas : number) (b : Topdown.t) : number * number =
  let want = Float.atan2 (ay - b.y) (ax - b.x) * 180. / Float.pi in
  let off = angle_diff want b.heading in
  ((if Float.abs off > 50. then gas * 0.35 else gas), Float.max (-1.) (Float.min 1. (off / 15.)))

(* the point a car aims at: the crossroads it drives to, on the right *)
let aim (c : car) (lane : number) : number * number =
  let fx, fy = node_at c.from and tx, ty = node_at c.toward in
  let d = Float.max 1. (Float.hypot (tx - fx) (ty - fy)) in
  (tx + ((ty - fy) / d * lane), ty - ((tx - fx) / d * lane))

(* at the crossroads, the way on: for traffic any but back, for the
 * police the one nearest you *)
let way_on (c : car) (you : you) (rng : int) : node =
  let ways = List.filter (fun n -> n <> c.from) (neighbours c.toward) in
  let ways = if ways = [] then [ c.from ] else ways in
  if c.driver = Cop then
    let dist n = let x, y = node_at n in Float.hypot (x - you.x) (y - you.y) in
    List.fold_left (fun a n -> if dist n < dist a then n else a) (List.hd ways) ways
  else List.nth ways (rng mod List.length ways)

let civilian (rng : int) (you : you) (c : car) : car =
  let lane = if c.driver = Cop then 0. else 30. in
  let ax, ay = aim c lane in
  let c =
    if Float.hypot (ax - c.body.x) (ay - c.body.y) < 50. then { c with from = c.toward; toward = way_on c you rng } else c
  in
  let ax, ay = aim c lane in
  let gas, wheel = steer_to ax ay 0.7 c.body in
  (* braking into a crossroads: too fast, a turn there is a wall *)
  let gas = if Float.hypot (ax - c.body.x) (ay - c.body.y) < 160. && c.body.speed > 200. then -0.4 else gas in
  let top = if c.driver = Cop then 430. else 230. in
  { c with body = Topdown.drive Topdown.toy top gas wheel c.body }

(* a police car near you drives straight at you; further, it drives the
 * roads *)
let cop (rng : int) (you : you) (c : car) : car =
  if Float.hypot (you.x - c.body.x) (you.y - c.body.y) < 220. then
    let gas, wheel = steer_to you.x you.y 1. c.body in
    { c with body = Topdown.drive Topdown.toy 430. gas wheel c.body }
  else civilian rng you c

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* what the player does this frame, which is all [step] needs: the game
 * reads it off the keyboard, the tests make it up *)
type input = { dx : number; dy : number; enter : bool }

let nothing = { dx = 0.; dy = 0.; enter = false }

(* a crime: a star more, and a police car more, coming from the
 * crossroads furthest from you *)
let crime (city : city) : city =
  if city.wanted >= 4 then { city with unseen = 0 }
  else
    let far =
      List.fold_left
        (fun a n ->
          let d n = let x, y = node_at n in Float.hypot (x - city.you.x) (y - city.you.y) in
          if d n > d a then n else a)
        (0, 0)
        (List.concat (List.init (nx +.. 1) (fun i -> List.init (ny +.. 1) (fun j -> (i, j)))))
    in
    let police = a_car Cop white far (List.hd (neighbours far)) 0. in
    { city with wanted = city.wanted +.. 1; unseen = 0; cars = city.cars @ [ police ] }

(* out of the police's sight (500 pixels) for ten seconds, a star less,
 * and a police car less *)
let cool_down (city : city) : city =
  let near = List.exists (fun c -> c.driver = Cop && Float.hypot (c.body.x - city.you.x) (c.body.y - city.you.y) < 500.) city.cars in
  let unseen = if near then 0 else city.unseen +.. 1 in
  if unseen > 600 && city.wanted > 0 then
    let rec drop_one = function [] -> [] | c :: rest when c.driver = Cop -> rest | c :: rest -> c :: drop_one rest in
    { city with wanted = city.wanted -.. 1; unseen = 0; cars = drop_one city.cars }
  else { city with unseen }

(* into the nearest car that is slow enough, or out of yours *)
let enter (city : city) : city =
  match city.you.car with
  | Some k ->
      let cars = List.mapi (fun i c -> if i = k then { c with driver = Nobody } else c) city.cars in
      let b = (List.nth city.cars k).body in
      (* out on its left, unless that is in a wall *)
      let a = (b.heading + 90.) * Float.pi / 180. in
      let x, y = (b.x + (Float.cos a * 28.), b.y + (Float.sin a * 28.)) in
      let x, y = if blocked x y then (b.x, b.y) else (x, y) in
      { city with cars; you = { x; y; car = None } }
  | None -> (
      let near =
        List.mapi (fun i c -> (i, c)) city.cars
        |> List.filter (fun (_, c) ->
               c.driver <> Cop && Float.abs c.body.speed < 120. && Float.hypot (c.body.x - city.you.x) (c.body.y - city.you.y) < 45.)
      in
      match near with
      | [] -> city
      | (k, c) :: _ ->
          let city = { city with cars = List.mapi (fun i c -> if i = k then { c with driver = Player } else c) city.cars;
                                 you = { city.you with car = Some k } } in
          (* a car with its driver in it: stolen in the street *)
          if c.driver = Civilian then crime city else city)

let walk (i : input) (you : you) : you =
  let x = you.x + (i.dx * 2.2) and y = you.y + (i.dy * 2.2) in
  let x = if blocked x you.y then you.x else x in
  let y = if blocked x y then you.y else y in
  { you with x; y }

(* where a person walking round block [b] is, [s] along the pavement,
 * 10 pixels in from the block's edge *)
let ped_at (p : ped) : number * number =
  let r = block_rect p.block 10. in
  let w = r.right - r.left and h = r.top - r.bottom in
  let s = Float.rem (Float.rem p.s (2. * (w + h)) + (2. * (w + h))) (2. * (w + h)) in
  if s < w then (r.left + s, r.bottom)
  else if s < w + h then (r.right, r.bottom + s - w)
  else if s < (2. * w) + h then (r.right - (s - w - h), r.top)
  else (r.left, r.top - (s - (2. * w) - h))

(* people knocked down by a car going fast enough: each one a crime *)
let run_over (city : city) : city =
  let fast = List.filter (fun c -> Float.abs c.body.speed > 80.) city.cars in
  List.fold_left
    (fun (city : city) (p : ped) ->
      let x, y = ped_at p in
      if p.alive && List.exists (fun c -> Float.hypot (c.body.x - x) (c.body.y - y) < 20.) fast then
        let peds = List.map (fun q -> if q == p then { q with alive = false } else q) city.peds in
        let city = { city with peds } in
        if List.exists (fun c -> c.driver = Player) fast then crime city else city
      else city)
    city city.peds

(* every car against the buildings, then every two of them against each
 * other *)
let collide (before : car list) (after : car list) : car list =
  let arr = Array.of_list (List.map2 (fun b a -> { a with body = Topdown.bounce blocked b.body a.body }) before after) in
  let n = Array.length arr in
  for i = 0 to n -.. 1 do
    for j = i +.. 1 to n -.. 1 do
      let a, b = Topdown.push 18. arr.(i).body arr.(j).body in
      arr.(i) <- { (arr.(i)) with body = a };
      arr.(j) <- { (arr.(j)) with body = b }
    done
  done;
  Array.to_list arr

let busted (city : city) : bool =
  let stopped = match city.you.car with None -> true | Some k -> Float.abs (List.nth city.cars k).body.speed < 50. in
  stopped && List.exists (fun c -> c.driver = Cop && Float.hypot (c.body.x - city.you.x) (c.body.y - city.you.y) < 34.) city.cars

(* the phone answered: a mission; its place reached in a car: paid, and
 * the next phone rings *)
let missions (city : city) : city =
  let (px, py), (tx, ty) = phones.(city.phone) in
  let next = (city.phone +.. 1) mod Array.length phones in
  match city.mission with
  | None -> if Float.hypot (px - city.you.x) (py - city.you.y) < 30. then { city with mission = Some (city.phone, 3600) } else city
  | Some (_, 0) -> { city with mission = None; phone = next }
  | Some (m, left) ->
      if city.you.car <> None && Float.hypot (tx - city.you.x) (ty - city.you.y) < 60. then
        { city with mission = None; phone = next; money = city.money +.. 1000 +.. (left /.. 6) }
      else { city with mission = Some (m, left -.. 1) }

let step (i : input) (city : city) : city =
  let city = if i.enter then enter city else city in
  let rng = random city.rng in
  let cars =
    List.map
      (fun c ->
        match c.driver with
        | Player -> { c with body = Topdown.drive Topdown.toy 480. i.dy (-.i.dx) c.body }
        | Civilian -> civilian rng city.you c
        | Cop -> cop rng city.you c
        | Nobody -> { c with body = Topdown.drive Topdown.toy 0. 0. 0. c.body })
      city.cars
  in
  let cars = collide city.cars cars in
  let you =
    match city.you.car with Some k -> let b = (List.nth cars k).body in { city.you with x = b.x; y = b.y } | None -> walk i city.you
  in
  let peds = List.map (fun p -> if p.alive then { p with s = p.s + p.pace } else p) city.peds in
  { city with you; cars; peds; rng; frames = city.frames +.. 1 } |> run_over |> cool_down |> missions

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (start ())) scenes else scenes
  | Busted city ->
      (* the fine paid, the stars gone, back on foot where you were *)
      if pressed (fun k -> k.kspace) then
        let cars = List.filter (fun c -> c.driver <> Cop) city.cars |> List.map (fun c -> if c.driver = Player then { c with driver = Nobody } else c) in
        Scene2d.go (Playing { city with cars; wanted = 0; unseen = 0; money = max 0 (city.money -.. 500); you = { city.you with car = None } }) scenes
      else scenes
  | Playing city ->
      let k = computer.keyboard in
      let city = step { dx = to_x k; dy = to_y k; enter = pressed (fun k -> k.kspace) } city in
      if busted city then Scene2d.go (Busted city) scenes else { scenes with scene = Playing city }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let asphalt = rgb 70 70 76
let pavement = rgb 150 146 138
let grass = rgb 80 140 70
let yellow = rgb 250 210 50

(* higher the faster you go *)
let zoom (city : city) : number =
  let speed = match city.you.car with Some k -> Float.abs (List.nth city.cars k).body.speed | None -> 0. in
  1.2 - (0.55 * Float.min 1. (speed / 450.))

let camera (computer : computer) (city : city) : Camera2d.t =
  { Camera2d.origin with zoom = zoom city } |> Camera2d.look_at city.you.x city.you.y |> Camera2d.clamp computer.screen world

(* The buildings -- the trick of this game, in 17 lines (see the header):
 * each roof corner pushed away from the camera's point by the
 * building's height, the walls between the ground and the roof *)
let building (cam : Camera2d.t) (b : node) : shape =
  let r = block_rect b 20. and h = height b in
  let ground = [ (r.left, r.bottom); (r.right, r.bottom); (r.right, r.top); (r.left, r.top) ] in
  let up (x, y) = (x + ((x - cam.x) * h), y + ((y - cam.y) * h)) in
  let roof = List.map up ground in
  let shade k = let (i, j) = b in rgb (90 +.. k +.. (i *.. 9)) (86 +.. k +.. (j *.. 7)) (96 +.. k) in
  let walls =
    List.mapi
      (fun n a ->
        let b' = List.nth ground ((n +.. 1) mod 4) in
        polygon (shade (20 *.. (n mod 2))) [ a; b'; up b'; up a ])
      ground
  in
  group (walls @ [ polygon (shade 70) roof ])

let car_shape (frames : int) (c : car) : shape =
  let b = c.body in
  let lights =
    if c.driver = Cop then
      let blue, red = if frames /.. 8 mod 2 = 0 then (rgb 40 80 250, rgb 120 20 20) else (rgb 20 30 90, rgb 250 40 40) in
      [ rectangle blue 5. 7. |> move_y 4.; rectangle red 5. 7. |> move_y (-4.) ]
    else []
  in
  group ([ rectangle (rgb 20 20 20) 38. 20.; rectangle c.color 34. 17.; rectangle (rgb 40 50 70) 10. 14. |> move_x 5. ] @ lights)
  |> rotate b.heading |> move b.x b.y

let view_city (computer : computer) (city : city) : shape list =
  let screen = computer.screen in
  let cam = camera computer city in
  let pulse = 1. + (0.3 * Float.sin (float_of_int city.frames * 0.2)) in
  let (px, py), (tx, ty) = phones.(city.phone) in
  let goal_x, goal_y = if city.mission = None then (px, py) else (tx, ty) in
  let ground =
    [ rectangle asphalt (world.right - world.left) (world.top - world.bottom) ]
    @ List.init (nx +.. 1) (fun i -> rectangle yellow 3. (world.top - world.bottom) |> move_x (road_x i) |> fade 0.5)
    @ List.init (ny +.. 1) (fun j -> rectangle yellow (world.right - world.left) 3. |> move_y (road_y j) |> fade 0.5)
    @ List.map
        (fun b ->
          let r = block_rect b 0. in
          rectangle (if park b then grass else pavement) (r.right - r.left) (r.top - r.bottom)
          |> move ((r.left + r.right) / 2.) ((r.bottom + r.top) / 2.))
        blocks
  in
  let people =
    List.map
      (fun p ->
        let x, y = ped_at p in
        if p.alive then circle (rgb 230 180 140) 5. |> move x y else oval (rgb 160 20 20) 16. 10. |> move x y)
      city.peds
  in
  let markers =
    [ circle yellow (16. * pulse) |> fade 0.7 |> move px py ]
    @ (match city.mission with Some _ -> [ circle (rgb 60 220 90) (40. * pulse) |> fade 0.5 |> move tx ty ] | None -> [])
  in
  let you = match city.you.car with None -> [ circle (rgb 250 230 60) 7. |> move city.you.x city.you.y ] | Some _ -> [] in
  (* the arrow at the top, pointing where to go *)
  let arrow_angle = Float.atan2 (goal_y - city.you.y) (goal_x - city.you.x) * 180. / Float.pi in
  [ Camera2d.view cam
      (ground @ people @ markers @ List.map (car_shape city.frames) city.cars @ you
      @ List.map (building cam) (List.filter (fun b -> not (park b)) blocks));
    polygon yellow [ (30., 0.); (-15., 18.); (-5., 0.); (-15., -18.) ] |> rotate arrow_angle |> move_y (screen.top - 60.);
    text white 2.2 (Printf.sprintf "$%d" city.money) |> move (screen.left + 90.) (screen.top - 40.);
    text (rgb 250 80 60) 2.5 (String.concat " " (List.init city.wanted (fun _ -> "*"))) |> move (screen.right - 120.) (screen.top - 40.);
    text white 1.7
      (match city.mission with
       | None -> "a phone is ringing: follow the arrow"
       | Some (_, left) -> Printf.sprintf "get a car to the green circle: %d s" (left /.. 60))
    |> move_y (screen.bottom + 55.);
    text (rgb 200 200 200) 1.5 "arrows walk and drive    space into a car, or out" |> move_y (screen.bottom + 25.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      [ rectangle (rgb 20 20 30) screen.width screen.height; text yellow 6. "TINY GTA" |> move_y 200.;
        text white 2. "any car in the city is yours: walk up to one, press space" |> move_y 70.;
        text white 2. "phones ring with work; the police count your crimes in stars" |> move_y 30. ]
      @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing city -> view_city computer city
  | Busted city ->
      view_city computer city
      @ [ rectangle (rgb 10 10 30) screen.width 180. |> fade 0.8; text (rgb 80 120 250) 5. "BUSTED" |> move_y 25.;
          text white 2. "a $500 fine -- space to go on" |> move_y (-40.) ]

let help =
  {|TinyGTA
  arrows   walk; in a car, up gas, down brake and reverse, left and right steer
  space    into the car next to you, or out of yours
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
