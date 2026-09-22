(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Missile Command (Dave Theurer, Atari, 1980): missiles
 * rain down on six cities; three bases fire counter-missiles, which fly
 * to the point you aim at and explode there, and an enemy warhead caught
 * in an explosion explodes too. Move the crosshair with the mouse (or
 * the arrows); a, s, d fire from the left, center and right bases (the
 * arcade's three buttons), a click from the nearest one. When the last
 * city falls: THE END.
 *
 * Theurer's game came from the Cold War: he dreamed of nuclear war for
 * months while making it. It had a trackball, rolled with the palm, and
 * no way to win: the waves come faster and faster, and every game ends
 * with the screen's "THE END". (Names and dates from memory, to check.)
 *
 * What's new here:
 *
 *  - Aiming at a point, not at a target: a counter-missile flies to where
 *    the crosshair was, and explodes there; hitting a warhead is placing
 *    the explosion where it will be. The player does the intercept's
 *    quadratic in their head (TinyXpilot's cannons do it on paper,
 *    [intercept]).
 *
 *  - Explosions as circles that grow and shrink ([radius]), and the
 *    chain reaction: a warhead destroyed explodes in turn, its circle
 *    catching the ones near it -- the whole game's pleasure.
 *
 *  - Missiles that split (MIRVs, [split]): a warhead becoming three, at
 *    a height drawn from the seed in the model.
 *
 * What it uses: the shoot 'em up kit's Shots (gamekits/shmup/: every
 * missile a shot, aimed at its target), Scene2d, Audio. Not Physics: the
 * missiles fly straight, at a constant speed.
 *
 * Exercises: the bombers and the smart bombs that dodge explosions, a
 * bonus city every 10,000 points, the colors changing with the waves
 * (the original's), the trackball's feel with the mouse's speed
 * (Playground's mdx, mdy).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type point = number * number

(* a missile: where it started (for its trail), where it goes, and
 * where it is *)
type missile = { from : point; target : point; shot : Shots.t }

type explosion = { ex : number; ey : number; age : int }

type game = {
  cities : bool list; (* standing *)
  ammo : int list; (* each base's counter-missiles *)
  crosshair : point;
  mine : missile list; (* the counter-missiles *)
  theirs : missile list; (* the warheads *)
  explosions : explosion list;
  to_come : int; (* warheads of this wave still to fall *)
  wave : int;
  score : int;
  seed : int;
  frames : int;
  pause : int; (* frames of the pause between two waves *)
}

type scene = Title | Playing of game | The_end of int
type model = scene Scene2d.t

let ground = -400.
let city_x = [ -330.; -230.; -130.; 130.; 230.; 330. ]
let base_x = [ -440.; 0.; 440. ]
let base_top = ground + 40.

let new_wave (g : game) : game =
  { g with ammo = [ 10; 10; 10 ]; mine = []; theirs = []; explosions = []; to_come = 8 +.. (2 *.. g.wave); pause = 90 }

let new_game () : game =
  new_wave { cities = List.map (fun _ -> true) city_x; ammo = []; crosshair = (0., 0.); mine = []; theirs = []; explosions = []; to_come = 0; wave = 1; score = 0; seed = 1; frames = 0; pause = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let roll (seed : int) (n : int) : int * int =
  let s = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff in
  (s /.. 65536 mod n, s)

let speed_of (wave : int) : number = 0.8 + (0.25 * float_of_int wave)

(* a warhead from (x, top) to a target: a city or a base, standing or not *)
let launch (g : game) : game =
  let x, seed = roll g.seed 900 in
  let t, seed = roll seed 9 in
  let tx = if t < 6 then List.nth city_x t else List.nth base_x (t -.. 6) in
  let from = (float_of_int x - 450., 500.) and target = (tx, ground) in
  { g with seed; to_come = g.to_come -.. 1; theirs = { from; target; shot = Shots.aimed (speed_of g.wave) from target } :: g.theirs }

(* a warhead splitting in three, sometimes (more often wave after wave),
 * above the middle of the screen; its forks, starting below the top,
 * don't split again *)
let split (g : game) : game =
  let targets = city_x @ base_x in
  let g, split =
    List.fold_left
      (fun (g, acc) (m : missile) ->
        let r, seed = roll g.seed (max 200 (1600 /.. g.wave)) in
        let g = { g with seed } in
        if r = 0 && m.shot.y > 100. && snd m.from = 500. then
          let here = (m.shot.x, m.shot.y) in
          let fork (g : game) =
            let t, seed = roll g.seed 9 in
            let target = (List.nth targets t, ground) in
            ({ g with seed }, { from = here; target; shot = Shots.aimed (speed_of g.wave) here target })
          in
          let g, a = fork g in
          let g, b = fork g in
          (g, a :: b :: { m with from = here } :: acc)
        else (g, m :: acc))
      (g, []) g.theirs
  in
  { g with theirs = split }

(* an explosion's radius: growing to 50 in 30 frames, shrinking back in
 * the next 30 *)
let radius (e : explosion) : number = if e.age < 30 then float_of_int e.age * 50. / 30. else float_of_int (60 -.. e.age) * 50. / 30.

let arrived (m : missile) : bool =
  let tx, ty = m.target in
  (tx - m.shot.x) * m.shot.vx + (ty - m.shot.y) * m.shot.vy <= 0.

(* [fire g base]: a counter-missile from this base, if it has any left,
 * to the crosshair *)
let fire (g : game) (base : int) : game =
  if List.nth g.ammo base <= 0 || snd g.crosshair < base_top + 20. then g
  else begin
    Audio.play Audio.laser;
    let from = (List.nth base_x base, base_top) in
    { g with ammo = List.mapi (fun i a -> if i = base then a -.. 1 else a) g.ammo; mine = { from; target = g.crosshair; shot = Shots.aimed 12. from g.crosshair } :: g.mine }
  end

(* the base nearest the crosshair that has counter-missiles left *)
let nearest_base (g : game) : int option =
  List.filter (fun i -> List.nth g.ammo i > 0) [ 0; 1; 2 ]
  |> List.sort (fun a b -> compare (Float.abs (List.nth base_x a - fst g.crosshair)) (Float.abs (List.nth base_x b - fst g.crosshair)))
  |> function i :: _ -> Some i | [] -> None

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  let k = computer.keyboard and m = computer.mouse in
  let pressed key = Scene2d.pressed (fun kb -> Set_.mem key kb.keys) scenes in
  (* the crosshair: the mouse when it moves, the arrows otherwise *)
  let cx, cy = if m.mdx <> 0. || m.mdy <> 0. then (m.mx, m.my) else (fst g.crosshair + (8. * to_x k), snd g.crosshair + (8. * to_y k)) in
  let g = { g with crosshair = (clamp (-480.) 480. cx, clamp (base_top + 20.) 480. cy) } in
  let g = if pressed "a" then fire g 0 else g in
  let g = if pressed "s" then fire g 1 else g in
  let g = if pressed "d" then fire g 2 else g in
  let g = if m.mclick then (match nearest_base g with Some b -> fire g b | None -> g) else g in
  (* the warheads, after the pause: a new one now and then, some
   * splitting *)
  let g = if g.pause > 0 then { g with pause = g.pause -.. 1 } else g in
  let g = if g.pause = 0 && g.to_come > 0 && g.frames mod max 20 (90 -.. (8 *.. g.wave)) = 0 then launch g else g in
  let g = split g in
  (* the missiles fly; the counter-missiles explode at their point *)
  let mine = List.map (fun (m : missile) -> { m with shot = Shots.advance m.shot }) g.mine in
  let bursting, mine = List.partition arrived mine in
  let theirs = List.map (fun (m : missile) -> { m with shot = Shots.advance m.shot }) g.theirs in
  let explosions = List.filter (fun e -> e.age < 60) (List.map (fun e -> { e with age = e.age +.. 1 }) g.explosions) @ List.map (fun (m : missile) -> { ex = fst m.target; ey = snd m.target; age = 0 }) bursting in
  if bursting <> [] then Audio.play Audio.explosion;
  (* a warhead in an explosion explodes: the chain reaction *)
  let caught (m : missile) = List.exists (fun e -> Float.hypot (e.ex - m.shot.x) (e.ey - m.shot.y) < radius e) explosions in
  let destroyed, theirs = List.partition caught theirs in
  let explosions = explosions @ List.map (fun (m : missile) -> { ex = m.shot.x; ey = m.shot.y; age = 0 }) destroyed in
  (* a warhead on the ground: its target is gone *)
  let landed, theirs = List.partition arrived theirs in
  let hit x = List.exists (fun (m : missile) -> Float.abs (fst m.target - x) < 1.) landed in
  let cities = List.map2 (fun alive x -> alive && not (hit x)) g.cities city_x in
  let ammo = List.map2 (fun a x -> if hit x then 0 else a) g.ammo base_x in
  if landed <> [] then Audio.play Audio.hit;
  let explosions = explosions @ List.map (fun (m : missile) -> { ex = fst m.target; ey = ground + 10.; age = 0 }) landed in
  let g = { g with mine; theirs; explosions; cities; ammo; score = g.score +.. (25 *.. List.length destroyed) } in
  (* the wave over: the bonus, and the next one *)
  if g.to_come = 0 && g.theirs = [] && g.explosions = [] && g.mine = [] then
    let bonus = (5 *.. List.fold_left ( +.. ) 0 g.ammo) +.. (100 *.. List.length (List.filter Fun.id g.cities)) in
    new_wave { g with wave = g.wave +.. 1; score = g.score +.. bonus }
  else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  match s.scene with
  | Title -> if Scene2d.pressed (fun k -> k.kspace) s then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if not (List.exists Fun.id g.cities) then Scene2d.go (The_end g.score) s else { s with scene = Playing g }
  | The_end _ -> if Scene2d.pressed (fun k -> k.kspace) s && s.elapsed > 2. then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let segment (color : color) ((x0, y0) : point) ((x1, y1) : point) : shape =
  rectangle color (Float.hypot (x1 - x0) (y1 - y0)) 2. |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi) |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.)

let view_city (alive : bool) (x : number) : shape =
  if alive then group [ rectangle (rgb 60 140 230) 50. 18. |> move_y 9.; rectangle (rgb 60 140 230) 12. 34. |> move (-12.) 17.; rectangle (rgb 60 140 230) 10. 28. |> move 12. 14. ] |> move x ground
  else rectangle (rgb 120 60 30) 44. 6. |> move x (ground + 3.)

let view_base (ammo : int) (x : number) : shape list =
  (polygon (rgb 200 170 60) [ (x - 45., ground); (x + 45., ground); (x + 25., base_top); (x - 25., base_top) ])
  :: List.init ammo (fun i -> rectangle (rgb 60 60 200) 4. 10. |> move (x - 18. + (float_of_int (i mod 5) * 9.)) (ground + 10. + (float_of_int (i /.. 5) * 14.)))

let view_game (g : game) : shape list =
  let cx, cy = g.crosshair in
  [ rectangle (rgb 200 170 60) 1000. 100. |> move_y (ground - 50.) ]
  @ List.map2 view_city g.cities city_x
  @ List.concat (List.map2 view_base g.ammo base_x)
  @ List.concat_map (fun (m : missile) -> [ segment (rgb 230 60 60) m.from (m.shot.x, m.shot.y); rectangle white 4. 4. |> move m.shot.x m.shot.y ]) g.theirs
  @ List.concat_map (fun (m : missile) -> [ segment (rgb 90 130 255) m.from (m.shot.x, m.shot.y); text white 1.5 "x" |> move (fst m.target) (snd m.target) ]) g.mine
  @ List.map (fun e -> circle (if (e.age /.. 4) mod 2 = 0 then white else rgb 255 200 80) (radius e) |> move e.ex e.ey) g.explosions
  @ [ rectangle white 24. 2. |> move cx cy; rectangle white 2. 24. |> move cx cy;
      text (rgb 230 60 60) 3. (Printf.sprintf "%d" g.score) |> move (-380.) 460.; text (rgb 90 130 255) 2.5 (Printf.sprintf "WAVE %d" g.wave) |> move 380. 460. ]
  @ if g.pause > 0 then [ text (rgb 90 130 255) 4. "DEFEND CITIES" |> move_y 100. ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game { (new_game ()) with pause = 0 }
      @ [ text (rgb 230 60 60) 7. "TINY MISSILE COMMAND" |> move_y 250.; text white 2.5 "the mouse (or the arrows) aims" |> move_y 170.;
          text white 2.5 "a s d fire from the left, center, right bases (or click)" |> move_y 130. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 30. ]
  | Playing g -> view_game g
  | The_end score ->
      (* the original's ending: a flash, and the words *)
      let r = Float.min 700. (float_of_int s.frames * 8.) in
      [ circle (rgb 255 220 120) r |> fade (Float.max 0. (1. - (float_of_int s.frames / 120.))); text (rgb 230 60 60) 9. "THE END"; text white 3. (Printf.sprintf "%d" score) |> move_y (-100.) ]
      @ if s.elapsed > 2. then Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-180.) ] else [])

let app = game view update initial_model
let main = Playground_platform.run_app app
