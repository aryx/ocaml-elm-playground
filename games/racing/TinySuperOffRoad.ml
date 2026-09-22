(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Ironman Ivan Stewart's Super Off Road (Leland, 1989):
 * four pickup trucks on a dirt track in a stadium, all of it on one
 * screen, seen from the stands; four laps, over a jump, the whoops, a
 * big hill and a mud hole. Red on the arrows and space for a nitro,
 * blue on w/a/s/d and e (or the computer); yellow, and grey, the
 * Ironman's own truck, are the computer's.
 *
 * It is Super Sprint (TinySuperSprint.ml) with one number more: the
 * ground has a height. That changes the driving, which is what whoever
 * played it remembers: a truck slows on the way up a hill and speeds
 * up on the way down, and over a crest, if it's fast enough, it takes
 * off. Nothing else about the car changes (the racing kit's Topdown
 * drives it, as in the other games); the hills are three more rules:
 *
 *   - the slope ([slope]): gravity along the ground, the part of it
 *     along the truck taken off its speed, the part across pushing it
 *     sideways (a truck on a side slope slides down it);
 *   - the takeoff: the truck's height is a second number next to the
 *     ground's, and it follows the ground only as long as the ground
 *     doesn't fall away faster than gravity would pull the truck down:
 *
 *              ___         where it would be,     .  <- the truck, flying:
 *          ___/   \        flying, one frame on:  .     its height ahead of
 *      ___/        \___    above the ground ahead:        the ground's
 *                          off it goes
 *
 *     a crest taken slowly, the ground drops a little, the truck stays
 *     on it; fast, the ground drops more than gravity can follow in a
 *     frame, and the truck flies: no wheel, no gas, until it lands (a
 *     hard landing costs speed);
 *   - the mud: the top speed cut where the ground is low and wet.
 *
 * And the view. The stadium is seen from one fixed angle, from the
 * front and above: the gamekits/isometric projection (TinyZaxxon.ml's
 * and TinyDiablo.ml's), with x across the screen, the depth compressed
 * into its upper part, and the height straight up:
 *
 *      screen x = x                    a point further away is
 *      screen y = 0.6 * depth + h      higher up the screen, a point
 *                                      higher up is too
 *
 * The ground is a grid of cells, each a quad whose four corners are
 * lifted by their heights, shaded by its slope (lighter facing the
 * light, from the left and the front); a truck is three boxes (four
 * wheels, the body, the cab), each face a quad projected the same way.
 * Then everything, the thousand quads of the ground and the trucks,
 * drawn far to near (Isometric.sorted): a hill in front of a truck
 * hides it. A truck is sorted by its nearest point, not its center: by
 * its center, the flat cell just in front of it, drawn after it, would
 * cover its front. And its shadow, on the ground under it, drawn with
 * it: in the air, the gap between the two is its height, the one
 * number the projection throws away (Zaxxon's answer again).
 *
 * The original drew all this as a painted picture, one per track, the
 * hills shaded by an artist, with the trucks drawn on top by the
 * hardware, and a map of the heights behind it for the physics; here
 * the picture is computed from the heights, once, at startup.
 *
 * What it uses: the racing kit's Topdown -- the trucks' driving
 * ([drive]), the track ([follow], [lap], [progress], [computer], the
 * walls by [distance], [bounce], the bumps by [push]); the isometric
 * kit ([make], [project], [depth], [sorted]); Camera2d's zoom, the
 * stadium fitted to the window; Scene2d; Audio for the engine, the
 * landings and the nitro. Not Heightmap (TinyComanche.ml's): its
 * heights are made up at random, a stadium's are placed, each a bump
 * written as a formula ([height]).
 *
 * What it doesn't: the speed shop between races (TinySupercars.ml
 * has one), the money and nitros lying on the track, the
 * eight tracks and their water, the third player; the trucks turning
 * in the air, pitching over the jump.
 *
 * Exercises: the shop; a nitro canister appearing on the track;
 * banked turns (the height rising towards a corner's outside, and the
 * slope then pushing the truck into the turn); a truck that tips
 * forward in the air, its nose following its velocity.
 *)
open Playground

(*****************************************************************************)
(* The stadium *)
(*****************************************************************************)

let road_width = 130.

(* the track, from the start on the near straight: up the right side
 * over the big hill, back along the far straight over the jump, down
 * the left, then through the middle and its mud, a hairpin, and home *)
let track : Topdown.track =
  { points =
      [| (-250., -370.); (250., -370.); (400., -280.); (420., 0.); (400., 280.); (250., 350.); (-250., 350.);
         (-400., 280.); (-410., 100.); (-300., 30.); (-50., 30.); (80., -80.); (-50., -180.); (-380., -180.);
         (-420., -300.) |];
    reach = 100.;
    corner = 240. }

let smoothstep (a : number) (b : number) (x : number) : number =
  let t = Float.max 0. (Float.min 1. ((x -. a) /. (b -. a))) in
  t *. t *. (3. -. (2. *. t))

let bump (cx : number) (cz : number) (h : number) (sigma : number) (x : number) (z : number) : number =
  h *. exp (-.(((x -. cx) ** 2.) +. ((z -. cz) ** 2.)) /. (2. *. sigma *. sigma))

(* the ground's height, the sum of the stadium's features *)
let height (x : number) (z : number) : number =
  (* the banks: everything off the track raised, a groove cut in the
   * ground *)
  let banks = 35. *. smoothstep ((road_width /. 2.) +. 5.) ((road_width /. 2.) +. 55.) (Topdown.distance track x z) in
  (* the big hill, up the right side *)
  let hill = bump 420. 0. 70. 100. x z in
  (* the jump, on the far straight driven leftwards: a ramp up from
   * x = 120 to 30, then the lip, straight down (on the cells' edges, so
   * the picture has the same corners) *)
  let jump =
    if Float.abs (z -. 350.) > 60. then 0.
    else if x > 30. && x < 120. then 40. *. (120. -. x) /. 90.
    else if x > 0. && x <= 30. then 40. *. x /. 30.
    else 0.
  in
  (* the whoops, five small bumps on the near straight, two cells
   * each *)
  let whoops =
    if Float.abs (z +. 370.) > 60. || x < -90. || x > 210. then 0.
    else 14. *. (1. -. cos (2. *. Float.pi *. (x +. 90.) /. 60.)) /. 2.
  in
  (* the mud hole, in the middle *)
  let hole = bump (-175.) 30. (-25.) 45. x z in
  banks +. hill +. jump +. whoops +. hole

(* the ground's slope, (dh/dx, dh/dz) *)
let slope (x : number) (z : number) : number * number =
  ((height (x +. 1.) z -. height (x -. 1.) z) /. 2., (height x (z +. 1.) -. height x (z -. 1.)) /. 2.)

let in_wall (x : number) (z : number) : bool = Topdown.distance track x z > (road_width /. 2.) -. 14.
let in_mud (x : number) (z : number) : bool = height x z < -8.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type driver = Red | Blue | Computer of number (* its skill, 1 the best *)

type truck = {
  body : Topdown.t; (* on the ground plane: its x, and its depth as y *)
  h : number; (* its height *)
  vh : number; (* and how fast it changes, per second *)
  air : bool;
  driver : driver;
  name : string;
  paint : int * int * int;
  nitros : int;
  boost : int; (* > 0: a nitro burning, for that many frames *)
}

type race = { trucks : truck list; number : int; ready : int }

type scene = Title | Racing of race | Results of race
type model = scene Scene2d.t

let laps = 4
let gravity = 600.

let new_race (two : bool) (number : int) : race =
  let truck i driver name paint =
    let c = Topdown.start track 0 (if i mod 2 = 0 then 30. else -30.) in
    let back = 30. +. (60. *. float_of_int (i / 2)) in
    let body = { c with x = c.x -. back } in
    { body; h = height body.x body.y; vh = 0.; air = false; driver; name; paint; nitros = 3; boost = 0 }
  in
  let skill = Float.min 1. (0.8 +. (0.05 *. float_of_int number)) in
  { trucks =
      [ truck 3 Red "RED" (220, 40, 40);
        truck 2 (if two then Blue else Computer (skill -. 0.05)) (if two then "BLUE" else "COMPUTER") (50, 100, 230);
        truck 1 (Computer (skill -. 0.1)) "COMPUTER" (240, 200, 40); truck 0 (Computer skill) "IRONMAN" (170, 170, 175) ];
    number;
    ready = 90 }

let initial_model = Scene2d.start Title

(*****************************************************************************)
(* The rules: the hills *)
(*****************************************************************************)

let dt = 1. /. 60.

let nitro_sound = Audio.sfx { Sfx.explosion with frequency = 300.; decay = 0.6; volume = 0.3 }
let thud = Audio.sfx { Sfx.hit with decay = 0.1; volume = 0.3 }

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

(* (gas, steer, nitro): the keys, or the computer *)
let controls (s : model) (t : truck) : number * number * bool =
  let k = s.keys in
  match t.driver with
  | Red -> (axis k.kup k.kdown, axis k.kleft k.kright, Scene2d.pressed (fun k -> k.kspace) s)
  | Blue -> (axis k.kw k.ks, axis k.ka k.kd, Scene2d.pressed (fun k -> Set_.mem "e" k.keys) s)
  | Computer _ -> let gas, steer = Topdown.computer track t.body in (gas, steer, false)

(* the truck's engine: a nitro is more pull and a higher top; the
 * computer's by its skill; the mud cuts it *)
let engine (t : truck) : Topdown.params * number =
  let skill = match t.driver with Computer k -> k | Red | Blue -> 1. in
  let boost = if t.boost > 0 then 1.7 else 1. in
  let mud = if in_mud t.body.x t.body.y then 0.5 else 1. in
  ({ Topdown.toy with accel = 650. *. skill *. boost *. mud; grip = 0.1 }, 520. *. boost *. mud)

(* on the ground: driven, then gravity along the slope; then either
 * still on the ground, or off it *)
let on_ground (gas : number) (steer : number) (t : truck) : truck =
  let b = t.body in
  let p, top = engine t in
  let after = Topdown.drive p top gas steer b in
  let gx, gz = slope b.x b.y in
  let a = after.heading *. Float.pi /. 180. in
  let along = (gx *. cos a) +. (gz *. sin a) in
  (* the slope's part along the truck slows it or speeds it up, the part
   * across pushes it sideways *)
  let after =
    { after with
      speed = after.speed -. (gravity *. along *. dt);
      vx = after.vx -. (gravity *. (gx -. (along *. cos a)) *. dt);
      vy = after.vy -. (gravity *. (gz -. (along *. sin a)) *. dt) }
  in
  let body = Topdown.bounce in_wall b after |> Topdown.follow track in
  let ground = height body.x body.y in
  (* where it would be, flying from here for a frame *)
  let flying = t.h +. (t.vh *. dt) -. (gravity *. dt *. dt /. 2.) in
  if ground < flying -. 1. then { t with body; h = flying; vh = t.vh -. (gravity *. dt); air = true }
  else { t with body; h = ground; vh = (ground -. t.h) /. dt; air = false }

(* in the air: no wheel, no gas; falling; landing when the ground comes
 * up to it, hard if falling fast *)
let in_air (t : truck) : truck =
  let b = t.body in
  let moved = { b with x = b.x +. (b.vx *. dt); y = b.y +. (b.vy *. dt) } in
  let body = Topdown.bounce in_wall b moved |> Topdown.follow track in
  let vh = t.vh -. (gravity *. dt) in
  let h = t.h +. (vh *. dt) in
  let ground = height body.x body.y in
  if h > ground then { t with body; h; vh }
  else begin
    let hard = vh < -200. in
    if hard && t.driver = Red then Audio.play thud;
    { t with body = { body with speed = (if hard then body.speed *. 0.7 else body.speed) }; h = ground; vh = 0.; air = false }
  end

let move_truck (s : model) (t : truck) : truck =
  let gas, steer, nitro = controls s t in
  let t =
    if nitro && t.nitros > 0 && t.boost = 0 then begin
      Audio.play nitro_sound;
      { t with nitros = t.nitros - 1; boost = 90 }
    end
    else { t with boost = max 0 (t.boost - 1) }
  in
  if t.air then in_air t else on_ground gas steer t

(* every two trucks near the same height, pushed apart (one flying over
 * another doesn't touch it) *)
let bump_trucks (trucks : truck list) : truck list =
  let a = Array.of_list trucks in
  for i = 0 to Array.length a - 1 do
    for j = i + 1 to Array.length a - 1 do
      if Float.abs (a.(i).h -. a.(j).h) < 15. then begin
        let bi, bj = Topdown.push 16. a.(i).body a.(j).body in
        a.(i) <- { (a.(i)) with body = bi };
        a.(j) <- { (a.(j)) with body = bj }
      end
    done
  done;
  Array.to_list a

let update_race (s : model) (r : race) : race =
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else
    let trucks = List.map (move_truck s) r.trucks |> bump_trucks in
    let red = List.hd trucks in
    Audio.keep_playing "engine"
      (Audio.sawtooth (45. +. (0.25 *. Float.abs red.body.speed)) |> Audio.low_pass 500. |> Audio.louder 0.2);
    { r with trucks }

let progress (t : truck) : number = Topdown.progress track t.body
let places (r : race) : truck list = List.stable_sort (fun a b -> compare (progress b) (progress a)) r.trucks
let finished (r : race) : bool = List.exists (fun (t : truck) -> Topdown.lap track t.body >= laps) r.trucks

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed name = Scene2d.pressed (fun k -> Set_.mem name k.keys) s in
  match s.scene with
  | Title ->
      if pressed "1" then Scene2d.go (Racing (new_race false 1)) s
      else if pressed "2" then Scene2d.go (Racing (new_race true 1)) s
      else s
  | Racing r ->
      let r = update_race s r in
      if finished r then Scene2d.go (Results r) s else { s with scene = Racing r }
  | Results r ->
      if Scene2d.pressed (fun k -> k.kspace) s then
        let two = List.exists (fun (t : truck) -> t.driver = Blue) r.trucks in
        Scene2d.go (Racing (new_race two (r.number + 1))) s
      else s

(*****************************************************************************)
(* The view -- the trick of this game, in 110 lines (see the header) *)
(*****************************************************************************)

(* x across, the depth up the screen at 0.6, the height straight up *)
let iso : Isometric.t = Isometric.make ~across:(1., 0.) ~along:(0., 0.6) ~up:1.

(* a point of the ground plane at a height, for the kit: (x, height,
 * depth) *)
let at (x : number) (z : number) (h : number) : number * number * number = (x, h, z)

let lit (c : int * int * int) (k : number) : int * int * int =
  let r, g, b = c in
  let f v = max 0 (min 255 (int_of_float (float_of_int v *. k))) in
  (f r, f g, f b)

let shade (c : int * int * int) (k : number) : color = let r, g, b = lit c k in rgb r g b

(* the ground: a quad per cell, its corners at their heights, colored
 * by what's there and lit by its slope; each with its depth *)
let cell = 30.
let cols = 34
let rows = 30

(* the cell whose corner is (xa, za): its color, and its corners'
 * heights *)
let paint (xa : number) (za : number) : (int * int * int) * number list =
  let cx = xa +. (cell /. 2.) and cz = za +. (cell /. 2.) in
  let d = Topdown.distance track cx cz in
  let start_x, start_z = Topdown.point track 0 in
  let base =
    if d > (road_width /. 2.) +. 18. then (70, 140, 60) (* the grass *)
    else if d > (road_width /. 2.) -. 10. then (215, 185, 90) (* the hay bales *)
    else if in_mud cx cz then (100, 75, 50)
    else if Float.abs (cx -. start_x) < cell /. 2. && Float.abs (cz -. start_z) < road_width /. 2. then if int_of_float ((cz +. 1000.) /. cell) mod 2 = 0 then (240, 240, 240) else (40, 40, 40)
    else (175, 125, 75) (* the dirt *)
  in
  let gx, gz = slope cx cz in
  let light = Float.max 0.6 (Float.min 1.4 (1. +. (1.2 *. gx) +. (0.8 *. gz))) in
  let xb = xa +. cell and zb = za +. cell in
  (lit base light, [ height xa za; height xb za; height xb zb; height xa zb ])

(* A row of cells, from the left. Most of the stadium is flat, and in
 * this view the depth doesn't change across the screen: a flat cell of
 * the same color and height as the one on its left only widens it, one
 * quad for the run (Sprite.pixels' runs, on the ground), a third as
 * many quads to draw. *)
let terrain : (number * shape) list =
  let x0 = -.cell *. float_of_int cols /. 2. and z0 = -.cell *. float_of_int rows /. 2. in
  let flat hs = List.for_all (fun h -> Float.abs (h -. List.hd hs) < 0.5) hs in
  let row j =
    let za = z0 +. (float_of_int j *. cell) in
    let zb = za +. cell in
    let runs =
      List.fold_left
        (fun runs i ->
          let xa = x0 +. (float_of_int i *. cell) in
          let color, hs = paint xa za in
          match runs with
          | (xa', _, color', hs') :: rest when color' = color && flat hs && flat hs' && Float.abs (List.hd hs -. List.hd hs') < 0.5 ->
              (xa', xa +. cell, color, hs') :: rest
          | _ -> (xa, xa +. cell, color, hs) :: runs)
        [] (List.init cols Fun.id)
    in
    List.map
      (fun (xa, xb, (r, g, b), hs) ->
        let h1, h2, h3, h4 = match hs with [ h1; h2; h3; h4 ] -> (h1, h2, h3, h4) | _ -> assert false in
        let h2, h3 = if flat hs then (h1, h1) else (h2, h3) in
        let ps = [ at xa za h1; at xb za h2; at xb zb h3; at xa zb h4 ] in
        (Isometric.depth iso (at ((xa +. xb) /. 2.) ((za +. zb) /. 2.) ((h1 +. h3) /. 2.)), polygon (rgb r g b) (List.map (Isometric.project iso) ps)))
      runs
  in
  List.concat_map row (List.init rows Fun.id)

(* a box on the ground plane: [length] along the heading [a] (in
 * radians), [width] across it, from height [h1] to [h2], its center
 * at (x, z); its sides far to near, then its top *)
let box (color : int * int * int) (x : number) (z : number) (a : number) (length : number) (width : number) (h1 : number)
    (h2 : number) : shape list =
  let corner (u, v) h = at (x +. (u *. cos a) -. (v *. sin a)) (z +. (u *. sin a) +. (v *. cos a)) h in
  let foot = [ (length /. 2., width /. 2.); (-.length /. 2., width /. 2.); (-.length /. 2., -.width /. 2.); (length /. 2., -.width /. 2.) ] in
  let side k =
    let p = List.nth foot k and q = List.nth foot ((k + 1) mod 4) in
    let pts = [ corner p h1; corner q h1; corner q h2; corner p h2 ] in
    let (_, _, zp) = corner p h1 and (_, _, zq) = corner q h1 in
    ((zp +. zq) /. 2., polygon (shade color 0.7) (List.map (Isometric.project iso) pts))
  in
  let sides = List.stable_sort (fun (za, _) (zb, _) -> compare zb za) (List.init 4 side) in
  List.map snd sides @ [ polygon (shade color 1.) (List.map (fun p -> Isometric.project iso (corner p h2)) foot) ]

let color (t : truck) : color = shade t.paint 1.

(* a truck: its shadow on the ground, four wheels, the body, the cab;
 * sorted by its nearest point (half its length ahead of its center, if
 * it's going up or down the screen), less half a cell: after the cell
 * its front is on *)
let view_truck (t : truck) : number * shape =
  let b = t.body and h = t.h in
  let a = b.heading *. Float.pi /. 180. in
  let ground = height b.x b.y in
  let wheel (u, v) =
    box (30, 30, 30) (b.x +. (u *. cos a) -. (v *. sin a)) (b.y +. (u *. sin a) +. (v *. cos a)) a 11. 7. h (h +. 10.)
  in
  let shadow = Isometric.at iso (at b.x b.y ground) (oval black 44. 22. |> fade 0.35) in
  let shapes =
    [ shadow ]
    @ List.concat_map wheel [ (11., 12.); (-11., 12.); (11., -12.); (-11., -12.) ]
    @ box t.paint b.x b.y a 34. 20. (h +. 6.) (h +. 16.)
    @ box t.paint (b.x -. (4. *. cos a)) (b.y -. (4. *. sin a)) a 14. 16. (h +. 16.) (h +. 25.)
    @ if t.boost > 0 then [ circle orange 7. |> Isometric.at iso (at (b.x -. (22. *. cos a)) (b.y -. (22. *. sin a)) (h +. 10.)) ] else []
  in
  (Isometric.depth iso (at b.x (b.y -. 35.) (Float.max h ground)), group shapes)

let text color size str = words color str |> scale size

let view_race (r : race) : shape list =
  let hud i (t : truck) =
    let x = -360. +. (float_of_int i *. 240.) in
    [ square (color t) 16. |> move (x -. 70.) 330.;
      text white 2. (Printf.sprintf "L%d  N%d" (min laps (Topdown.lap track t.body + 1)) t.nitros) |> move (x +. 5.) 330. ]
  in
  Isometric.sorted (terrain @ List.map view_truck r.trucks)
  @ List.concat (List.mapi hud r.trucks)
  @ if r.ready > 0 then [ text yellow 6. (if r.ready > 30 then "READY" else "GO!") |> move_y 60. ] else []

let place_names = [| "1ST"; "2ND"; "3RD"; "4TH" |]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  let zoom = Float.min (screen.width /. 1040.) (screen.height /. 720.) in
  let cam = { Camera2d.origin with zoom; y = 20. } in
  [ rectangle (rgb 30 30 45) screen.width screen.height ]
  @
  match s.scene with
  | Title ->
      [ Camera2d.view cam
          (Isometric.sorted terrain
          @ [ rectangle (rgb 25 25 40) 820. 300. |> move_y 40.;
              text (rgb 240 200 40) 6. "TINY SUPER OFF ROAD" |> move_y 140.;
              text white 3. "1: one player   2: two players" |> move_y 70.;
              text white 2.5 "red: arrows, space: nitro   blue: w/a/s/d, e" |> move_y 20.;
              text white 2.5 "slow up the hills, fast down, and fly over the tops" |> move_y (-20.) ]
          @ Scene2d.blink 1. s [ text yellow 3. "PRESS 1 OR 2" |> move_y (-70.) ]) ]
  | Racing r -> [ Camera2d.view cam (view_race r) ]
  | Results r ->
      let row i (t : truck) =
        [ square (color t) 24. |> move (-150.) (90. -. (float_of_int i *. 50.));
          text white 3. (Printf.sprintf "%s  %s" place_names.(i) t.name) |> move 30. (90. -. (float_of_int i *. 50.)) ]
      in
      [ Camera2d.view cam
          (view_race r
          @ [ rectangle (rgb 25 25 40) 600. 380. |> move_y 20.; text yellow 4. (Printf.sprintf "RACE %d" r.number) |> move_y 160. ]
          @ List.concat (List.mapi row (places r))
          @ Scene2d.blink 1. s [ text yellow 3. "SPACE: NEXT RACE" |> move_y (-130.) ]) ]

let app = game view update initial_model

let main = Playground_platform.run_app app
