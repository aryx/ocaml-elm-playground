(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Star Fox (Nintendo and Argonaut, 1993): an arwing
 * down a canyon, on rails -- it flies forward by itself and you only
 * dodge and shoot. Left/right and up/down to move across the canyon,
 * space to fire. One run to the end, and whatever is left of your
 * shield.
 *
 * Star Fox put polygons on a SNES with a chip in the cartridge (the
 * Super FX), and could afford perhaps a few hundred of them a frame.
 * What it could not afford was a world you fly around in, so it did
 * what its arcade ancestors did (Space Harrier, Sega, 1985): it put
 * the ship on rails. The stage flies past; you steer within a window
 * of it. That is not a compromise the game apologises for -- it is
 * what makes the stage a *composition*, with every arch and every wave
 * of fighters arriving where the designer put it.
 *
 * On rails, said plainly, is two numbers instead of six: the ship has
 * no heading and no velocity along the canyon, only
 *
 *     s        how far down the canyon the stage has carried it
 *     offset,  where it sits across and above the canyon floor
 *     height
 *
 * and those are exactly the coordinates the racing kit's Track3d hands
 * out (gamekits/racing/3d/Track3d.mli: a course as a closed spline with a
 * width, a height and a bank, resampled into segments of equal
 * length). It was written for TinyMarioKart64's circuit; a
 * canyon is the same ribbon with walls instead of kerbs, flown over
 * instead of driven on, which is the useful thing to learn about a kit
 * -- whether it survives a genre it was not designed for. The bank it
 * carries is worth more here than in the kart game: a banked turn
 * rolls the whole canyon, which is the Star Fox shot.
 *
 * The enemies come out of the 2D shoot 'em up kit unaltered, which is
 * the other half of the lesson. gamekits/shmup/Path is Galaga's flight
 * curves: a few points, a Catmull-Rom spline through them, measured so
 * that a ship moves along it by *distance* rather than by parameter.
 * In 2D that curve is where an enemy flies on the screen. Here it is
 * where an enemy flies across the canyon's cross-section, while the
 * stage carries it towards you: a 2D pattern in a 3D tube, and the
 * same file for both. gamekits/shmup/Shots carries the bolts the same way,
 * its x and y being across and up.
 *
 *        the canyon, from the front        a path across it
 *        |                      |             ___
 *        |    . <- an enemy     |            /   \
 *        |         following    |           |     |   the enemy's own
 *        |         its path     |            \___/    flight, in 2D
 *        |______________________|
 *
 * Uses: Track3d (the canyon, with TinyMarioKart64 and
 * TinyVirtuaRacing), the shmup kit's Path and Shots (with
 * TinyGalaga and TinyGradius), Scene2d, Camera3d. Not
 * Physics3d (a ship on rails has no forces), not Topdown or Car (there
 * is nothing to drive).
 *
 * Exercises: the all-range mode Star Fox 64 added, where the rails come
 * off and the ship really flies (a heading, and the camera problem
 * back); a boss at the end of the canyon; a barrel roll that deflects
 * bolts; rings that mend the shield; a wingman who calls out.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The canyon *)
(*****************************************************************************)

(* the canyon's floor is the ribbon, its width the room to fly across,
 * and the walls stand on its edges *)
let canyon : Track3d.control list =
  let control = Track3d.control in
  [ control ~width:16. 0. 0.;
    control ~width:15. 40. (-60.);
    control ~y:6. ~width:14. ~bank:(-12.) 110. (-95.);
    control ~y:10. ~width:13. ~bank:(-16.) 190. (-80.);
    control ~y:4. ~width:15. 250. (-20.);
    control ~y:0. ~width:16. ~bank:14. 270. 60.;
    control ~y:(-4.) ~width:14. ~bank:16. 210. 130.;
    control ~y:2. ~width:13. 120. 160.;
    control ~y:6. ~width:14. ~bank:(-10.) 30. 140.;
    control ~y:2. ~width:16. (-30.) 80. ]

let track : Track3d.t = Track3d.build ~step:6. canyon
let course_length = Track3d.length track

(* the stage: it starts a little way in and ends before it comes round
 * to the beginning, so that a run is a flight down a canyon and not a
 * lap *)
let start_s = 20.
let finish_s = course_length -. 40.

let wall_height = 26.
let ceiling = 22. (* how high above the floor the ship may fly *)
let floor_clearance = 2.2

(*****************************************************************************)
(* Drawing the canyon *)
(*****************************************************************************)

let shade ((r, g, b) : int * int * int) (light : number) : color =
  let v (c : int) = int_of_float ((float_of_int c *. light) +. 0.5) in
  rgb (v r) (v g) (v b)

let segment_shapes (i : int) : shape3d list =
  let p = Track3d.at track (float_of_int i *. Track3d.step track) in
  let w = p.width in
  let light = i mod 2 = 0 in
  let strip = Track3d.strip track in
  let ground = if light then rgb 116 96 72 else rgb 104 86 64 in
  let river = if light then rgb 60 96 132 else rgb 54 88 124 in
  [ strip ground i (-.w) (-3.5); strip river i (-3.5) 3.5; strip ground i 3.5 w;
    (* the canyon walls, in bands so that speed shows on them *)
    Track3d.wall track (if light then shade (128, 104, 78) 1. else shade (112, 90, 68) 1.) i (-.w) wall_height;
    Track3d.wall track (if light then shade (128, 104, 78) 1. else shade (112, 90, 68) 1.) i w wall_height ]

let arch_at (s : number) : shape3d list =
  let leg (offset : number) =
    let x, y, z = Track3d.across track s offset in
    box (rgb 150 126 96) 2.2 14. 2.2 |> move3d x (y +. 7.) z
  in
  let x, y, z = Track3d.across track s 0. in
  let p = Track3d.at track s in
  [ leg (-.p.width +. 2.); leg (p.width -. 2.);
    box (rgb 150 126 96) ((2. *. p.width) -. 2.) 2.4 2.4 |> rotate3d 0. (-.p.heading) 0. |> move3d x (y +. 14.) z ]

let canyon_shapes : shape3d =
  let arches = List.concat_map arch_at [ 90.; 200.; 330.; 470.; 600. ] in
  cached3d (List.concat (List.init (Track3d.segments track) segment_shapes) @ arches)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* an enemy: where it sits down the canyon, and the path it flies
 * across the canyon's cross-section -- the shmup kit's 2D curve, used
 * as the pattern of a 3D flight *)
type enemy = { es : number; path : Path.t; along : number; speed : number; alive : bool; fire : int }

(* a bolt: how far down the canyon, and where it is across it. The
 * across part is the shmup kit's [Shots.t], its x the offset and its y
 * the height, so a bolt that drifts does it in two dimensions as a 2D
 * game's would *)
type bolt = { bs : number; bvs : number; across : Shots.t; mine : bool }

type ship = { offset : number; height : number; roll : number; shield : int; hit : int }

type run = {
  ship : ship;
  s : number; (* how far the stage has carried the ship *)
  enemies : enemy list;
  bolts : bolt list;
  score : int;
  frames : int;
  cooldown : int;
}

type scene = Title | Flying of run | Ended of run * bool (* reached the end *)
type model = scene Scene2d.t

let full_shield = 12

(* The waves, each a few enemies at a place down the canyon, flying a
 * path across it. The paths are written as if on a screen -- across
 * from -10 to 10, up from 2 to 18 -- which is what they are: a shoot
 * 'em up's patterns, read here as a cross-section. *)
let wave (at_s : number) (n : int) (points : (number * number) list) (speed : number) : enemy list =
  let path = Path.make points in
  List.init n (fun k ->
      { es = at_s +. (float_of_int k *. 9.);
        path;
        along = float_of_int k *. -18.;
        speed;
        alive = true;
        fire = 40 + (k * 23) })

let waves : enemy list =
  wave 120. 3 [ (-12., 6.); (-4., 14.); (6., 6.); (12., 12.) ] 26.
  @ wave 230. 4 [ (10., 4.); (2., 16.); (-8., 8.); (-12., 14.) ] 30.
  @ wave 350. 3 [ (0., 18.); (-10., 8.); (0., 4.); (10., 8.); (0., 18.) ] 34.
  @ wave 470. 4 [ (-14., 10.); (0., 5.); (14., 10.); (0., 16.) ] 30.
  @ wave 580. 4 [ (12., 14.); (-12., 6.); (12., 6.); (-12., 14.) ] 36.

let new_run () : run =
  { ship = { offset = 0.; height = 8.; roll = 0.; shield = full_shield; hit = 0 };
    s = start_s; enemies = waves; bolts = []; score = 0; frames = 0; cooldown = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let ship_speed = 34.
let bolt_speed = 90.

(* where an enemy is now, across the canyon: its path, walked by
 * distance, which is what Path.at gives *)
let enemy_across (e : enemy) : number * number =
  let along = Float.rem (Float.max 0. e.along) (Path.length e.path) in
  let (x, y), _ = Path.at e.path along in
  (x, y)

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

let step_ship (keys : keyboard) (r : run) : ship =
  let s = r.ship in
  let width = (Track3d.at track r.s).width in
  let across = axis keys.kright keys.kleft and lift = axis keys.kup keys.kdown in
  let offset = Basics.clamp (-.width +. 2.) (width -. 2.) (s.offset +. (across *. 0.42)) in
  let height = Basics.clamp floor_clearance ceiling (s.height +. (lift *. 0.36)) in
  (* the arwing rolls into the turn, and rolls back when it stops *)
  let roll = s.roll +. (((across *. -28.) -. s.roll) *. 0.12) in
  { s with offset; height; roll; hit = max 0 (s.hit - 1) }

let fire (r : run) : run =
  if r.cooldown > 0 then r
  else
    let left = Shots.straight (r.ship.offset -. 0.8) r.ship.height 0. 0. in
    let right = Shots.straight (r.ship.offset +. 0.8) r.ship.height 0. 0. in
    { r with
      cooldown = 9;
      bolts =
        { bs = r.s +. 3.; bvs = bolt_speed; across = left; mine = true }
        :: { bs = r.s +. 3.; bvs = bolt_speed; across = right; mine = true } :: r.bolts }

let step_enemies (r : run) : run =
  let enemies =
    List.map
      (fun (e : enemy) ->
        let e = { e with along = e.along +. (e.speed /. 60.) } in
        if (not e.alive) || Float.abs (e.es -. r.s) > 150. then e
        else if e.fire <= 0 then { e with fire = 70 + (int_of_float (Float.abs e.es) mod 50) }
        else { e with fire = e.fire - 1 })
      r.enemies
  in
  (* the ones whose count reached zero this frame shoot down the canyon *)
  let shots =
    List.filter_map
      (fun (e : enemy) ->
        if e.alive && e.fire = 0 && e.es > r.s && e.es -. r.s < 120. then
          let x, y = enemy_across e in
          Some { bs = e.es; bvs = -.bolt_speed *. 0.55; across = Shots.straight x y 0. 0.; mine = false }
        else None)
      r.enemies
  in
  { r with enemies; bolts = shots @ r.bolts }

let step_bolts (r : run) : run =
  let bolts =
    List.filter_map
      (fun (b : bolt) ->
        let bs = b.bs +. (b.bvs /. 60.) in
        let across = Shots.advance b.across in
        if bs < r.s -. 20. || bs > r.s +. 200. then None else Some { b with bs; across })
      r.bolts
  in
  (* the player's bolts meeting an enemy, and the enemies' meeting the ship *)
  let hits = ref 0 and shield_hit = ref false in
  let enemies =
    List.map
      (fun (e : enemy) ->
        if not e.alive then e
        else
          let x, y = enemy_across e in
          let struck =
            List.exists
              (fun (b : bolt) -> b.mine && Float.abs (b.bs -. e.es) < 4. && Shots.near 2.2 (x, y) b.across)
              bolts
          in
          if struck then begin
            incr hits;
            { e with alive = false }
          end
          else e)
      r.enemies
  in
  let bolts =
    List.filter
      (fun (b : bolt) ->
        if b.mine then
          not
            (List.exists
               (fun (e : enemy) ->
                 e.alive && Float.abs (b.bs -. e.es) < 4.
                 &&
                 let x, y = enemy_across e in
                 Shots.near 2.2 (x, y) b.across)
               r.enemies)
        else if Float.abs (b.bs -. r.s) < 3. && Shots.near 1.6 (r.ship.offset, r.ship.height) b.across then begin
          shield_hit := true;
          false
        end
        else true)
      bolts
  in
  let ship =
    if !shield_hit && r.ship.hit = 0 then { r.ship with shield = r.ship.shield - 1; hit = 30 } else r.ship
  in
  { r with bolts; enemies; ship; score = r.score + (!hits * 100) }

(* flying into an enemy costs a shield too: on rails, the stage runs
 * you into things *)
let step_collisions (r : run) : run =
  let struck =
    List.exists
      (fun (e : enemy) ->
        e.alive && Float.abs (e.es -. r.s) < 2.2
        &&
        let x, y = enemy_across e in
        Float.hypot (x -. r.ship.offset) (y -. r.ship.height) < 2.6)
      r.enemies
  in
  if struck && r.ship.hit = 0 then { r with ship = { r.ship with shield = r.ship.shield - 2; hit = 40 } } else r

let step_run (keys : keyboard) (r : run) : run =
  let r = { r with s = r.s +. (ship_speed /. 60.); frames = r.frames + 1; cooldown = max 0 (r.cooldown - 1) } in
  let r = { r with ship = step_ship keys r } in
  let r = if keys.kspace then fire r else r in
  let r = step_enemies r in
  let r = step_bolts r in
  step_collisions r

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  match m.scene with
  | Title -> if space then Scene2d.go (Flying (new_run ())) m else m
  | Flying r ->
      let r = step_run computer.keyboard r in
      if r.ship.shield <= 0 then Scene2d.go (Ended (r, false)) m
      else if r.s >= finish_s then Scene2d.go (Ended (r, true)) m
      else { m with scene = Flying r }
  | Ended (r, won) -> if space then Scene2d.go Title m else { m with scene = Ended (r, won) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a point of the canyon's cross-section, in the world: [offset] across
 * the floor, [height] above it *)
let world_at (s : number) (offset : number) (height : number) : number * number * number =
  let x, y, z = Track3d.across track s offset in
  (x, y +. height, z)

let arwing : shape3d =
  let body = rgb 210 215 225 and wing = rgb 90 120 200 and glass = rgb 120 190 230 in
  group3d
    [ box body 1. 0.7 3.4;
      box glass 0.7 0.5 1.2 |> move3d 0. 0.35 (-0.6);
      box wing 4.4 0.24 1.2 |> move3d 0. 0. 0.4;
      box wing 0.9 1.1 0.9 |> move3d (-2.1) 0.4 0.9;
      box wing 0.9 1.1 0.9 |> move3d 2.1 0.4 0.9;
      box (rgb 250 170 60) 0.6 0.4 0.5 |> move3d 0. 0. 1.9 ]

let enemy_model : shape3d =
  let hull = rgb 190 80 70 and fin = rgb 120 50 45 in
  group3d
    [ box hull 1.2 0.9 2.2; box fin 3.2 0.2 0.9 |> move3d 0. 0.1 0.2;
      box (rgb 250 200 90) 0.5 0.4 0.4 |> move3d 0. 0. (-1.3) ]

let placed (s : number) (offset : number) (height : number) (shape : shape3d) : shape3d =
  let x, y, z = world_at s offset height in
  let p = Track3d.at track s in
  shape |> rotate3d 0. (-.p.heading) 0. |> move3d x y z

let view_run (r : run) : shape3d list =
  let ship =
    arwing
    |> rotate3d 0. 0. r.ship.roll
    |> (fun s -> placed r.s r.ship.offset r.ship.height s)
    |> fun s -> if r.ship.hit > 0 && r.ship.hit mod 8 < 4 then fade3d 0.35 s else s
  in
  let enemies =
    List.filter_map
      (fun (e : enemy) ->
        if (not e.alive) || e.es < r.s -. 20. || e.es > r.s +. 190. then None
        else
          let x, y = enemy_across e in
          Some (placed e.es x y (enemy_model |> rotate3d 0. 180. 0.)))
      r.enemies
  in
  let bolts =
    List.map
      (fun (b : bolt) ->
        let color = if b.mine then rgb 120 240 180 else rgb 250 160 80 in
        placed b.bs b.across.x b.across.y (box color 0.35 0.35 2.2))
      r.bolts
  in
  (canyon_shapes :: ship :: enemies) @ bolts

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let r = match m.scene with Title -> new_run () | Flying r | Ended (r, _) -> r in
  (* behind and above the ship, looking down the canyon: on rails, the
   * camera is on rails too. It follows most of the ship's slide across
   * but not all of it -- all of it and the canyon swings with every
   * dodge, too little and a ship near the wall slides off the screen,
   * which a first version of this did at 45% *)
  let eye = world_at (r.s -. 14.) (r.ship.offset *. 0.8) (r.ship.height +. 3.4) in
  let target = world_at (r.s +. 26.) (r.ship.offset *. 0.55) (r.ship.height +. 1.2) in
  (* the title too: an orbit round the start sees only the outside of a
   * canyon wall twenty-six high, so the title is the first frame of the
   * run, the arwing at the mouth of the canyon *)
  let cam = camera ~eye ~target ~far:2400. () in
  let sky =
    Camera3d.sky ~sky:(rgb 96 130 190) ~horizon:(rgb 92 78 60) ~ground:(-30.) cam
    @ [ Camera3d.floor ~color:(rgb 86 74 58) ~ground:(-30.) cam ]
  in
  let hud_shapes =
    match m.scene with
    | Title ->
        [ text (rgb 120 200 240) 6.5 "TINY STAR FOX" |> move_y 320.;
          rectangle black 900. 190. |> move_y (-250.) |> fade 0.55;
          text white 2.5 "left/right, up/down: fly across the canyon" |> move_y (-210.);
          text white 2.5 "space: fire -- the ship flies forward by itself" |> move_y (-255.);
          text white 2.5 "one run down the canyon, and what is left of your shield" |> move_y (-300.) ]
        @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-380.) ]
    | Flying _ | Ended _ ->
        let along = Basics.clamp 0. 1. ((r.s -. start_s) /. (finish_s -. start_s)) in
        [ text white 3. (Printf.sprintf "SHIELD %d" (max 0 r.ship.shield))
          |> move (screen.left +. 140.) (screen.top -. 40.);
          text white 3. (Printf.sprintf "%06d" r.score) |> move (screen.right -. 130.) (screen.top -. 40.);
          (* how far down the canyon: the stage, as a bar *)
          rectangle (rgb 40 40 50) 420. 14. |> move 0. (screen.top -. 40.);
          rectangle (rgb 120 200 240) (420. *. along) 10.
          |> move (-210. +. (210. *. along)) (screen.top -. 40.) ]
        @ (match m.scene with
          | Ended (_, true) ->
              [ text yellow 8. "STAGE CLEAR" |> move_y 200. ]
              @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 110. ]
          | Ended (_, false) ->
              [ text (rgb 250 120 90) 8. "SHIELD GONE" |> move_y 200. ]
              @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 110. ]
          | _ -> [])
  in
  (cam, sky @ view_run r @ List.map hud hud_shapes)

let app = game3d view update initial_model

(* flat shading: the Super FX's polygons were flat too, and there were
 * not many of them; the back faces are drawn for the sky *)
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    app
