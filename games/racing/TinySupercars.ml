(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Supercars (Magnetic Fields, Gremlin Graphics, 1990,
 * on the Amiga and the Atari ST; Shaun Southern and Andrew Morris --
 * from memory, to check): a race seen from above on a track too big for
 * the screen, three laps against three computer cars, and between the
 * races, the shop. Arrows to drive, space to fire a missile.
 *
 * The arcade racers from above (TinyGranTrak10.ml, TinySuperSprint.ml)
 * fit their track on one screen; the home computers' scrolled it, and
 * Supercars added what an arcade machine, paid a coin at a time, had
 * no use for: a career. Each race pays by your place, and the money
 * buys, in the shop: repairs (every knock damages the car, and the
 * damage slows it; at 100 it's out), a better engine, better tires,
 * missiles. Finish last and you don't qualify: the game is over.
 *
 * Two pictures of the same world: the track under a camera that follows
 * the car, looking ahead of it (Camera2d, as in TinyMicroMachines.ml),
 * and the whole track in a corner, small: the minimap, the same ribbon
 * at a twentieth of the size, the cars as dots. Nothing to compute that
 * the big picture doesn't already have: a scale.
 *
 *       +--------------------------+
 *       |                  +-----+ |   the minimap: the world scaled
 *       |      \\ road     | o_  | |   down to fit its box, and moved
 *       |       \\         |/ .\ | |   to the corner; the big view:
 *       |      [car]       +-----+ |   the world through the camera
 *       |         \\               |
 *       +--------------------------+
 *
 * The missiles: shot ahead of the car, twice as fast as it goes at full
 * speed, gone on a wall; a car hit spins for most of a second, out of
 * control, and takes damage.
 *
 * What it uses: the racing kit's Topdown -- the car ([drive]), the track
 * (laps, places, the computer's driving), the walls ([distance],
 * [bounce]), the bumps ([push]), the road's drawing ([ribbon]);
 * Camera2d, following with a look ahead; Scene2d for the title, the
 * race, the shop and the end; Sprite for the cars; Audio for the
 * missiles and the knocks.
 *
 * What it doesn't: Supercars' shopkeepers, who haggled over the price
 * (answered in words); its nine tracks a season and its three seasons;
 * the computer cars firing back; buying a new car.
 *
 * Exercises: a second track for every other race; the computer cars
 * buying missiles too, and firing them when you're just ahead; rear
 * missiles, fired backwards; a nitro, bought by the shot, a second of
 * a higher top speed.
 *)
open Playground

(*****************************************************************************)
(* The track *)
(*****************************************************************************)

let road_width = 170.

(* the start halfway along the bottom straight, the grid behind it on
 * the straight too *)
let track : Topdown.track =
  { points =
      [| (-150., -650.); (500., -650.); (950., -500.); (1050., -150.); (800., 50.); (300., 50.); (150., 250.);
         (350., 500.); (900., 550.); (1000., 750.); (700., 900.); (-300., 900.); (-700., 750.); (-700., 400.);
         (-400., 250.); (-400., -50.); (-900., -150.); (-1000., -450.);
         (-600., -650.) |];
    reach = 130.;
    corner = 300. }

let in_wall (x : number) (y : number) : bool = Topdown.distance track x y > (road_width /. 2.) -. 16.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type car = {
  body : Topdown.t;
  color : color;
  human : bool;
  damage : number; (* from 0 to 100: out *)
  spin : int; (* > 0: hit by a missile, spinning, for that many frames *)
}

type missile = { x : number; y : number; vx : number; vy : number; life : int (* frames left *) }

(* what the player keeps from race to race *)
type garage = { money : int; engine : int; tires : int; missiles : int; hurt : number (* the damage, carried *) }

type race = {
  cars : car list; (* the player's first *)
  shots : missile list;
  garage : garage;
  number : int; (* the race, the first 1 *)
  ready : int; (* > 0: waiting before the start *)
  cam : Camera2d.t;
}

type scene = Title | Racing of race | Shop of race * int (* the place *) | Over of race * string
type model = scene Scene2d.t

let laps = 3
let prizes = [| 4000; 2500; 1500; 0 |]

let player (r : race) : car = List.hd r.cars

let grid (row : int) (side : number) : Topdown.t =
  let c = Topdown.start track 0 side in
  let a = c.heading *. Float.pi /. 180. and back = 40. +. (70. *. float_of_int row) in
  { c with x = c.x -. (back *. cos a); y = c.y -. (back *. sin a) }

(* the player at the back of the grid, the computer's cars ahead *)
let new_race (garage : garage) (number : int) : race =
  let car i color human = { body = grid (i / 2) (if i mod 2 = 0 then 35. else -35.); color; human; damage = 0.; spin = 0 } in
  let you = { (car 3 (rgb 230 40 40) true) with damage = garage.hurt } in
  { cars = [ you; car 0 (rgb 240 200 40) false; car 1 (rgb 60 120 240) false; car 2 (rgb 240 240 240) false ];
    shots = [];
    garage;
    number;
    ready = 90;
    cam = { Camera2d.origin with x = you.body.x; y = you.body.y } }

let first_garage = { money = 0; engine = 0; tires = 0; missiles = 5; hurt = 0. }

let initial_model = Scene2d.start Title

(*****************************************************************************)
(* The rules *)
(*****************************************************************************)

let launch = Audio.sfx { Sfx.laser with volume = 0.3 }
let boom = Audio.sfx { Sfx.explosion with decay = 0.4; volume = 0.4 }
let knock = Audio.sfx { Sfx.hit with decay = 0.08; volume = 0.3 }

(* how the car drives: the player's by the garage, the computer's
 * better every race; the damage takes up to half the top speed *)
let params (r : race) (c : car) : Topdown.params * number =
  let level = if c.human then 0 else r.number - 1 in
  let tires = if c.human then r.garage.tires else min 3 level in
  let engine = if c.human then float_of_int r.garage.engine else 0.4 *. float_of_int level in
  ( { Topdown.toy with grip = 0.1 +. (0.03 *. float_of_int tires) },
    (560. +. (70. *. engine)) *. (1. -. (c.damage /. 200.)) )

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

let drive (k : keyboard) (r : race) (c : car) : car =
  if c.spin > 0 then
    let b = c.body in
    let slid = { b with heading = b.heading +. 15.; x = b.x +. (b.vx /. 60.); y = b.y +. (b.vy /. 60.); vx = b.vx *. 0.95; vy = b.vy *. 0.95; speed = 0. } in
    { c with spin = c.spin - 1; body = Topdown.bounce in_wall b slid }
  else
    let gas, steer = if c.human then (axis k.kup k.kdown, axis k.kleft k.kright) else Topdown.computer track c.body in
    let p, top = params r c in
    let after = Topdown.drive p top gas steer c.body in
    let body = Topdown.bounce in_wall c.body after |> Topdown.follow track in
    (* a knock on the wall: damage, by how fast *)
    if body.speed <> after.speed && Float.abs after.speed > 200. then begin
      if c.human then Audio.play knock;
      { c with body; damage = c.damage +. (Float.abs after.speed /. 150.) }
    end
    else { c with body }

let bump (cars : car list) : car list =
  let a = Array.of_list cars in
  for i = 0 to Array.length a - 1 do
    for j = i + 1 to Array.length a - 1 do
      let bi, bj = Topdown.push 20. a.(i).body a.(j).body in
      a.(i) <- { (a.(i)) with body = bi };
      a.(j) <- { (a.(j)) with body = bj }
    done
  done;
  Array.to_list a

(* a missile from the player's car, ahead of it *)
let fire (r : race) : race =
  let c = (player r).body in
  if r.garage.missiles = 0 then r
  else begin
    Audio.play launch;
    let a = c.heading *. Float.pi /. 180. in
    let m = { x = c.x +. (40. *. cos a); y = c.y +. (40. *. sin a); vx = 1200. *. cos a; vy = 1200. *. sin a; life = 90 } in
    { r with shots = m :: r.shots; garage = { r.garage with missiles = r.garage.missiles - 1 } }
  end

(* the missiles fly; one hitting a car or a wall is gone, the car hit
 * spinning *)
let fly (r : race) : race =
  let hits (m : missile) (c : car) = (not c.human) && Float.hypot (c.body.x -. m.x) (c.body.y -. m.y) < 35. in
  let moved = List.map (fun m -> { m with x = m.x +. (m.vx /. 60.); y = m.y +. (m.vy /. 60.); life = m.life - 1 }) r.shots in
  let cars =
    List.map
      (fun c ->
        if List.exists (fun m -> hits m c) moved then begin
          Audio.play boom;
          { c with spin = 50; damage = c.damage +. 20. }
        end
        else c)
      r.cars
  in
  let shots = List.filter (fun m -> m.life > 0 && (not (in_wall m.x m.y)) && not (List.exists (hits m) r.cars)) moved in
  { r with cars; shots }

let update_race (k : keyboard) (firing : bool) (r : race) : race =
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else
    let r = if firing then fire r else r in
    let r = fly { r with cars = List.map (drive k r) r.cars |> bump } in
    let b = (player r).body in
    { r with cam = Camera2d.follow 0.1 (b.x +. (0.4 *. b.vx)) (b.y +. (0.4 *. b.vy)) r.cam }

let progress (c : car) : number = Topdown.progress track c.body

(* the player's place, 0 the first *)
let place (r : race) : int =
  let you = player r in
  List.length (List.filter (fun c -> c != you && progress c > progress you) r.cars)

let finished (r : race) : bool = Topdown.lap track (player r).body >= laps

(* the end of the race: paid by the place, the damage carried *)
let paid (r : race) : race =
  let g = r.garage in
  { r with garage = { g with money = g.money + prizes.(place r); hurt = (player r).damage } }

type item = { key : string; name : string; price : garage -> int; buy : garage -> garage }

(* the shop: a price of 0 is out of stock *)
let items : item list =
  [ { key = "1"; name = "repair";
      price = (fun g -> 30 * int_of_float (Float.ceil g.hurt));
      buy = (fun g -> { g with hurt = 0. }) };
    { key = "2"; name = "engine";
      price = (fun g -> if g.engine < 3 then 3000 * (g.engine + 1) else 0);
      buy = (fun g -> { g with engine = g.engine + 1 }) };
    { key = "3"; name = "tires";
      price = (fun g -> if g.tires < 3 then 2000 * (g.tires + 1) else 0);
      buy = (fun g -> { g with tires = g.tires + 1 }) };
    { key = "4"; name = "5 missiles"; price = (fun _ -> 1000); buy = (fun g -> { g with missiles = g.missiles + 5 }) } ]

let shop (pressed : string -> bool) (g : garage) : garage =
  List.fold_left
    (fun g it ->
      let p = it.price g in
      if pressed it.key && p > 0 && g.money >= p then { (it.buy g) with money = g.money - p } else g)
    g items

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed name = Scene2d.pressed (fun k -> Set_.mem name k.keys) s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Racing (new_race first_garage 1)) s else s
  | Racing r ->
      let r = update_race computer.keyboard space r in
      if (player r).damage >= 100. then Scene2d.go (Over (r, "WRECKED")) s
      else if finished r then
        let r = paid r in
        if place r = List.length r.cars - 1 then Scene2d.go (Over (r, "NOT QUALIFIED")) s
        else Scene2d.go (Shop (r, place r)) s
      else { s with scene = Racing r }
  | Shop (r, p) ->
      if space then Scene2d.go (Racing (new_race r.garage (r.number + 1))) s
      else { s with scene = Shop ({ r with garage = shop pressed r.garage }, p) }
  | Over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let car_shape (color : color) : shape =
  Sprite.pixels 4.
    [ ('B', color); ('W', rgb 30 40 60); ('K', rgb 20 20 20); ('L', rgb 255 240 150) ]
    [ ".LBBBBL."; "KBBBBBBK"; "KBBBBBBK"; ".BBBBBB."; ".BWWWWB."; ".BBBBBB."; ".BBBBBB."; ".BWWWWB.";
      "KBBBBBBK"; "KBBBBBBK"; ".BBBBBB." ]

let view_car (c : car) : shape = car_shape c.color |> rotate (c.body.heading -. 90.) |> move c.body.x c.body.y

let view_missile (m : missile) : shape =
  group [ rectangle (rgb 250 150 40) 24. 6.; rectangle (rgb 250 240 120) 8. 6. |> move_x (-14.) ]
  |> rotate (atan2 m.vy m.vx *. 180. /. Float.pi)
  |> move m.x m.y

let ground : shape list =
  let x, y = Topdown.point track 0 in
  [ rectangle (rgb 50 120 50) 3200. 2600. |> move 0. 100.;
    Topdown.ribbon (rgb 200 40 40) (road_width +. 20.) track;
    Topdown.ribbon (rgb 240 240 240) (road_width +. 6.) track;
    Topdown.ribbon (rgb 80 80 88) (road_width -. 6.) track;
    group (List.init 8 (fun i -> rectangle (if i mod 2 = 0 then white else black) 12. (road_width /. 8.) |> move_y ((float_of_int i -. 3.5) *. road_width /. 8.)))
    |> move x y ]

let text color size str = words color str |> scale size

(* the whole track, [size] across, the cars as dots *)
let minimap (size : number) (r : race) : shape =
  let k = size /. 2200. in
  let dot (c : car) = circle c.color (if c.human then 70. else 50.) |> move c.body.x c.body.y in
  group
    ([ rectangle black (size /. k +. 200.) (size /. k +. 200.) |> fade 0.5 |> move 0. 125.; Topdown.ribbon (rgb 220 220 220) 120. track ]
    @ List.map dot (List.rev r.cars))
  |> scale k

let view_race (screen : screen) (r : race) : shape list =
  let you = player r in
  let world = ground @ List.map view_missile r.shots @ List.map view_car (List.rev r.cars) in
  let hud y str = text white 2.5 str |> move (screen.left +. 130.) (screen.top -. y) in
  [ Camera2d.view r.cam world;
    minimap 200. r |> move (screen.right -. 120.) (screen.top -. 120.);
    hud 30. (Printf.sprintf "LAP %d/%d" (min laps (Topdown.lap track you.body + 1)) laps);
    hud 60. (Printf.sprintf "PLACE %d" (place r + 1));
    hud 90. (Printf.sprintf "MISSILES %d" r.garage.missiles);
    hud 120. "DAMAGE";
    rectangle (rgb 60 60 60) 100. 14. |> move (screen.left +. 150.) (screen.top -. 145.);
    rectangle (rgb 230 60 40) you.damage 14. |> move (screen.left +. 100. +. (you.damage /. 2.)) (screen.top -. 145.) ]
  @ if r.ready > 0 then [ text yellow 6. (if r.ready > 30 then "READY" else "GO!") ] else []

let place_names = [| "1ST"; "2ND"; "3RD"; "4TH" |]

let view_shop (screen : screen) (s : model) (r : race) (p : int) : shape list =
  let g = r.garage in
  let row i it =
    let price = it.price g in
    let what = if price = 0 then "--" else "$" ^ string_of_int price in
    text (if price > 0 && g.money >= price then white else rgb 130 130 130) 3.
      (Printf.sprintf "%s: %-12s %8s" it.key it.name what)
    |> move_y (60. -. (float_of_int i *. 50.))
  in
  view_race screen r
  @ [ rectangle (rgb 25 25 40) 760. 520.;
      text yellow 4. (Printf.sprintf "RACE %d: %s, $%d" r.number place_names.(p) prizes.(p)) |> move_y 210.;
      text white 3. (Printf.sprintf "THE SHOP -- you have $%d" g.money) |> move_y 150.;
      text white 2.
        (Printf.sprintf "damage %d   engine %d   tires %d   missiles %d" (int_of_float g.hurt) g.engine g.tires g.missiles)
      |> move_y 110. ]
  @ List.mapi row items
  @ Scene2d.blink 1. s [ text yellow 3. "SPACE: NEXT RACE" |> move_y (-200.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  [ rectangle black screen.width screen.height ]
  @
  match s.scene with
  | Title ->
      [ Camera2d.view { Camera2d.origin with zoom = 0.3 } ground;
        rectangle (rgb 25 25 40) 760. 330.;
        text (rgb 230 40 40) 7. "TINY SUPERCARS" |> move_y 100.;
        text white 2.5 "arrows: drive   space: fire a missile" |> move_y 20.;
        text white 2.5 "three laps; win money, spend it in the shop" |> move_y (-20.);
        text white 2.5 "finish last and you don't qualify" |> move_y (-60.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-120.) ]
  | Racing r -> view_race screen r
  | Shop (r, p) -> view_shop screen s r p
  | Over (r, why) ->
      view_race screen r
      @ [ text (rgb 230 40 40) 7. why |> move_y 80.;
          text white 3. (Printf.sprintf "after %d races, $%d won" r.number r.garage.money) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-80.) ]

let app = game view update initial_model

let main = Playground_platform.run_app app
