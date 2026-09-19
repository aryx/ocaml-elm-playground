(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Donkey Kong (Shigeru Miyamoto, Nintendo, 1981), its
 * first stage: a construction site of slanted girders and ladders, an
 * ape at the top rolling barrels down, and Jumpman -- not yet called
 * Mario -- climbing to rescue Pauline. Left/right to walk, up/down to
 * climb, space to jump; jumping over a barrel is 100 points, and the
 * bonus counting down is yours if you reach the top before it runs out.
 *
 * Miyamoto's first game, and one of the first with a story told in its
 * screens, and the first where jumping was the point: the genre's name,
 * platformer, came later, but it starts here (with Space Panic's
 * ladders, 1980). Nintendo made it from the boards of a failed game,
 * Radar Scope, and it saved Nintendo of America. (Names and dates from
 * memory, to check.)
 *
 * The new idea here is state machines. The hero is always in one of a
 * few states, and each state has its own rules: walking follows the
 * girder and can start a jump or a climb; in the air nothing can be
 * changed until landing; on a ladder, no walking and no gravity. A
 * variant type says which state, with what it needs ([hero_state]),
 * and [step_hero] is a match on it, one case per state, each saying
 * how to leave it:
 *
 *          walk off an edge                 land from high
 *   +------------------------> Falling --------------------> Dying
 *   |                            | land                        ^
 *   |   <------------------------+                             | a barrel
 *  Walking --- space ---> Jumping --- land ---> Walking        | (any state)
 *   |  ^
 *   |  +--- top or bottom --+
 *   +--- up/down at a ladder ---> Climbing
 *
 * The barrels are a second, smaller machine ([barrel_state]): rolling
 * downhill on a girder, falling off its end onto the one below (which
 * slopes the other way: they zigzag down), or rolling down a ladder
 * when they pass the top of one, one time in three ([step_barrel]).
 *
 * The girders are slanted, so they aren't tiles: each is a segment, and
 * the ground under a walker is its height there ([height]), found anew
 * at each step; landing is crossing that height going down. (Not
 * TinyMario's move_by, one pixel at a time against tiles: a platformer
 * kit, plan_games.md section 8, would offer both.)
 *
 * What it uses: Sprite (Jumpman, turned to face where he walks),
 * Scene2d (title, play, game over), Audio (a jump, a barrel jumped, a
 * hit). Not Physics: the jump is a fixed arc, the same every time, the
 * way the arcade did it (its gravity is one line), and nothing bounces.
 * Not Tilemap nor Camera2d: one screen, girders as segments.
 *
 * Exercises: the hammer (smashing barrels for a while), the oil drum's
 * fireballs, the barrels choosing ladders when Jumpman is below them
 * (the original's barrels were smarter than one in three), the other
 * three stages (rivets, elevators, conveyor belts).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The site *)
(*****************************************************************************)

(* a girder: from (x0, y0) to (x1, y1), its top side *)
type girder = { x0 : number; y0 : number; x1 : number; y1 : number }

(* from the bottom: the barrels zigzag down, each girder sloping the
 * other way from the one above; the last one, Pauline's *)
let girders : girder array =
  [| { x0 = -460.; y0 = -440.; x1 = 460.; y1 = -420. };
     { x0 = -460.; y0 = -290.; x1 = 400.; y1 = -320. };
     { x0 = -400.; y0 = -190.; x1 = 460.; y1 = -160. };
     { x0 = -460.; y0 = -30.; x1 = 400.; y1 = -60. };
     { x0 = -400.; y0 = 70.; x1 = 460.; y1 = 100. };
     { x0 = -460.; y0 = 210.; x1 = 400.; y1 = 190. };
     { x0 = -120.; y0 = 320.; x1 = 120.; y1 = 320. } |]

let top = Array.length girders -.. 1

let on (g : girder) (x : number) : bool = x >= g.x0 && x <= g.x1

(* [height g x]: the girder's top at [x], by proportion: e.g. the bottom
 * one, from -440 at x = -460 to -420 at x = 460, is at -430 at x = 0 *)
let height (g : girder) (x : number) : number = g.y0 + ((g.y1 - g.y0) * (x - g.x0) / (g.x1 - g.x0))

(* the girder something at [x] falling from [y_before] to [y] crosses:
 * where it lands *)
let landing (x : number) (y_before : number) (y : number) : int option =
  let rec go i = if i < 0 then None else if on girders.(i) x && height girders.(i) x <= y_before && height girders.(i) x >= y then Some i else go (i -.. 1) in
  go top

(* a ladder: at [lx], from the girder [below] to the one [above];
 * Jumpman can't climb a broken one, barrels can roll down it *)
type ladder = { lx : number; below : int; above : int; broken : bool }

let ladders : ladder list =
  [ { lx = 300.; below = 0; above = 1; broken = false }; { lx = -200.; below = 0; above = 1; broken = true };
    { lx = -300.; below = 1; above = 2; broken = false }; { lx = 100.; below = 1; above = 2; broken = true };
    { lx = 250.; below = 2; above = 3; broken = false }; { lx = -50.; below = 2; above = 3; broken = false };
    { lx = -300.; below = 3; above = 4; broken = false }; { lx = 50.; below = 3; above = 4; broken = true };
    { lx = 300.; below = 4; above = 5; broken = false }; { lx = 80.; below = 5; above = top; broken = false } ]

let bottom_of (l : ladder) = height girders.(l.below) l.lx
let top_of (l : ladder) = height girders.(l.above) l.lx

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type hero_state =
  | Walking of int (* on this girder *)
  | Jumping of number * number (* its sideways speed, and its highest point *)
  | Falling of number (* from this height *)
  | Climbing of ladder
  | Dying of int (* frames since *)

type hero = { x : number; y : number (* its feet *); vy : number; state : hero_state; facing_left : bool; steps : int }

type barrel_state = Rolling of int | Dropping | Down of ladder

type barrel = { bx : number; by : number; bvy : number; bstate : barrel_state; spin : number; jumped : bool }

type game = {
  hero : hero;
  barrels : barrel list;
  next_barrel : int; (* frames before the next throw *)
  ladders_passed : int; (* by the barrels: every third, they go down *)
  bonus : int;
  score : int;
  lives : int;
  round : int;
  frames : int;
}

type scene = Title | Playing of game | Rescued of game | Game_over of int
type model = { scenes : scene Scene2d.t; hi_score : int }

let gravity = 0.4

(* a jump rises 6.6, 6.2, ..., 0.2 pixels in its first 17 frames: 57.8
 * pixels high, not the 7^2 / (2 x 0.4) = 61 of the continuous formula
 * v^2 / 2g (the frames' steps cut the parabola's top), and lands 35
 * frames after it left *)
let jump_speed = 7.
let walk_speed = 3.
let barrel_radius = 12.

let start_hero = { x = -380.; y = height girders.(0) (-380.); vy = 0.; state = Walking 0; facing_left = false; steps = 0 }

let new_round (round : int) (score : int) (lives : int) : game =
  { hero = start_hero; barrels = []; next_barrel = 60; ladders_passed = 0; bonus = 5000; score; lives; round; frames = 0 }

let initial_model = { scenes = Scene2d.start Title; hi_score = 0 }

(*****************************************************************************)
(* Jumpman *)
(*****************************************************************************)

let ladder_at (x : number) (pred : ladder -> bool) : ladder option = List.find_opt (fun l -> Float.abs (l.lx - x) < 10. && pred l) ladders

(* landing after a fall of more than this kills *)
let deadly_fall = 70.

(* in the air, [vx] sideways: gravity, and landing on a girder crossed
 * going down, unless from too high *)
let in_air (h : hero) (vx : number) (peak : number) : hero =
  let vy = h.vy - gravity in
  let x = clamp (-480.) 480. (h.x + vx) and y = h.y + vy in
  let peak = Float.max peak y in
  match if vy < 0. then landing x h.y y else None with
  | Some g when peak - height girders.(g) x > deadly_fall -> { h with x; y = height girders.(g) x; state = Dying 0 }
  | Some g -> { h with x; y = height girders.(g) x; vy = 0.; state = Walking g }
  | None when y < -520. -> { h with state = Dying 0 }
  | None -> { h with x; y; vy; state = (match h.state with Jumping _ -> Jumping (vx, peak) | _ -> Falling peak) }

let step_hero (keys : keyboard) (jump : bool) (h : hero) : hero =
  let dir = to_x keys in
  match h.state with
  | Walking g -> (
      let going_up = ladder_at h.x (fun l -> l.below = g && not l.broken) in
      let going_down = ladder_at h.x (fun l -> l.above = g && not l.broken) in
      match (going_up, going_down) with
      | Some l, _ when keys.kup -> { h with x = l.lx; state = Climbing l }
      | _, Some l when keys.kdown -> { h with x = l.lx; state = Climbing l }
      | _ when jump ->
          Audio.play Audio.jump;
          { h with vy = jump_speed; state = Jumping (dir * walk_speed, h.y) }
      | _ ->
          let x = clamp (-480.) 480. (h.x + (dir * walk_speed)) in
          let h = { h with x; facing_left = (if dir < 0. then true else if dir > 0. then false else h.facing_left); steps = (if dir <> 0. then h.steps +.. 1 else h.steps) } in
          if on girders.(g) x then { h with y = height girders.(g) x } else { h with vy = 0.; state = Falling h.y })
  | Jumping (vx, peak) -> in_air h vx peak
  | Falling peak -> in_air h 0. peak
  | Climbing l ->
      let y = clamp (bottom_of l) (top_of l) (h.y + (2. * to_y keys)) in
      let steps = if to_y keys <> 0. then h.steps +.. 1 else h.steps in
      if y >= top_of l && keys.kup then { h with y; steps; state = Walking l.above }
      else if y <= bottom_of l && keys.kdown then { h with y; steps; state = Walking l.below }
      else { h with y; steps }
  | Dying n -> { h with state = Dying (n +.. 1) }

(*****************************************************************************)
(* Barrels *)
(*****************************************************************************)

let barrel_speed (g : game) : number = 3. + (0.5 * float_of_int (g.round -.. 1))

(* which way a girder goes down: +1 right, -1 left *)
let downhill (g : girder) : number = if g.y1 < g.y0 then 1. else -1.

(* one barrel, one frame; None once it's rolled into the oil drum (the
 * bottom girder's left end); and whether it passed the top of a
 * ladder *)
let step_barrel (g : game) (b : barrel) : barrel option * bool =
  let speed = barrel_speed g in
  match b.bstate with
  | Rolling i ->
      let gi = girders.(i) in
      let x = b.bx + (downhill gi * speed) in
      let passing = List.find_opt (fun l -> l.above = i && Float.abs (l.lx - x) <= speed / 2.) ladders in
      (* every third ladder passed: down it *)
      (match passing with
      | Some l when (g.ladders_passed +.. 1) mod 3 = 0 -> (Some { b with bx = l.lx; bstate = Down l }, true)
      | _ ->
          let b = { b with bx = x; spin = b.spin - (downhill gi * speed * 4.) } in
          if i = 0 && x < gi.x0 + 20. then (None, false)
          else if on gi x then (Some { b with by = height gi x }, passing <> None)
          else (Some { b with bvy = 0.; bstate = Dropping }, passing <> None))
  | Dropping -> (
      let bvy = b.bvy - gravity in
      let y = b.by + bvy in
      match landing b.bx b.by y with
      | Some i -> (Some { b with by = height girders.(i) b.bx; bvy = 0.; bstate = Rolling i }, false)
      | None -> ((if y < -520. then None else Some { b with by = y; bvy }), false))
  | Down l ->
      let y = b.by - speed in
      if y <= bottom_of l then (Some { b with by = bottom_of l; bstate = Rolling l.below }, false) else (Some { b with by = y }, false)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let kong_x = -380.

let update_game (computer : computer) (scenes : scene Scene2d.t) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; bonus = (if g.frames mod 120 = 119 then max 0 (g.bonus -.. 100) else g.bonus) } in
  let hero = step_hero computer.keyboard (Scene2d.pressed (fun k -> k.kspace) scenes) g.hero in
  (* the barrels: Donkey Kong throws one every so often, faster each round *)
  let thrown = g.next_barrel <= 0 in
  let barrels = if thrown then { bx = kong_x + 40.; by = height girders.(5) (kong_x + 40.); bvy = 0.; bstate = Rolling 5; spin = 0.; jumped = false } :: g.barrels else g.barrels in
  let stepped = List.map (step_barrel g) barrels in
  let barrels = List.filter_map fst stepped in
  let passed = List.length (List.filter snd stepped) in
  (* a barrel on Jumpman; one jumped over: 100 points *)
  let alive = match hero.state with Dying _ -> false | _ -> true in
  let hit = alive && List.exists (fun b -> Float.hypot (b.bx - hero.x) (b.by + barrel_radius - (hero.y + 20.)) < 26.) barrels in
  let airborne = match hero.state with Jumping _ -> true | _ -> false in
  let over b = airborne && (not b.jumped) && Float.abs (b.bx - hero.x) < 14. && hero.y > b.by + (2. * barrel_radius) && hero.y - b.by < 70. in
  let jumped_now = List.length (List.filter over barrels) in
  if jumped_now > 0 then Audio.play Audio.coin;
  let barrels = List.map (fun b -> if over b then { b with jumped = true } else b) barrels in
  let hero = if hit || (alive && g.bonus = 0) then { hero with state = Dying 0 } else hero in
  if hit then Audio.play Audio.hit;
  { g with hero; barrels; score = g.score +.. (100 *.. jumped_now); ladders_passed = g.ladders_passed +.. passed;
    next_barrel = (if thrown then max 70 (170 -.. (20 *.. g.round)) else g.next_barrel -.. 1) }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let space = Scene2d.pressed (fun k -> k.kspace) scenes in
  let go scene = { model with scenes = Scene2d.go scene scenes } in
  match scenes.scene with
  | Title -> if space then go (Playing (new_round 1 0 3)) else { model with scenes }
  | Playing g -> (
      let g = update_game computer scenes g in
      let model = { model with hi_score = max model.hi_score g.score } in
      let go scene = { model with scenes = Scene2d.go scene scenes } in
      match g.hero.state with
      | Walking i when i = top -> go (Rescued { g with score = g.score +.. g.bonus })
      | Dying n when n > 120 -> if g.lives > 1 then go (Playing (new_round g.round g.score (g.lives -.. 1))) else go (Game_over g.score)
      | _ -> { model with scenes = { scenes with scene = Playing g } })
  | Rescued g -> if scenes.elapsed > 3. then go (Playing (new_round (g.round +.. 1) g.score g.lives)) else { model with scenes }
  | Game_over _ -> if space || scenes.elapsed > 10. then go Title else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* a line from (x0, y0) to (x1, y1) *)
let segment (color : color) (width : number) (x0 : number) (y0 : number) (x1 : number) (y1 : number) : shape =
  rectangle color (Float.hypot (x1 - x0) (y1 - y0)) width |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi) |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.)

let girder_red = rgb 230 40 90

(* a girder: a band under its top, crossed by struts *)
let view_girder (g : girder) : shape list =
  let t = 14. in
  let n = int_of_float ((g.x1 - g.x0) / 28.) in
  polygon girder_red [ (g.x0, g.y0); (g.x1, g.y1); (g.x1, g.y1 - t); (g.x0, g.y0 - t) ]
  :: List.init n (fun i ->
         let xa = g.x0 + (float_of_int i * 28.) in
         let xb = xa + 28. in
         segment black 2. xa (height g xa - 2.) (xb - 14.) (height g (xb - 14.) - t + 2.))

let view_ladder (l : ladder) : shape list =
  let cyan = rgb 90 220 230 and y0 = bottom_of l and y1 = top_of l - 14. in
  let span = y1 - y0 in
  let rail x = if l.broken then [ segment cyan 3. x y0 x (y0 + (span * 0.35)); segment cyan 3. x (y0 + (span * 0.65)) x y1 ] else [ segment cyan 3. x y0 x y1 ] in
  let rungs = List.filter (fun y -> not (l.broken && y > y0 + (span * 0.35) && y < y0 + (span * 0.65))) (List.init (int_of_float (span / 14.)) (fun i -> y0 + 7. + (float_of_int i * 14.))) in
  rail (l.lx - 10.) @ rail (l.lx + 10.) @ List.map (fun y -> rectangle cyan 20. 3. |> move l.lx y) rungs

(* Jumpman, facing right, two frames of a walk *)
let jumpman_rows =
  [ [ "...RRRR..."; "..RRRRRRR."; "..KKSSK..."; ".KSKSSSS.."; "..SSSSS..."; "..BRBBR..."; ".BBRBBRBB."; ".SSRRRRSS."; "..RRRRRR.."; ".KKK..KKK." ];
    [ "...RRRR..."; "..RRRRRRR."; "..KKSSK..."; ".KSKSSSS.."; "..SSSSS..."; "..BRBBR..."; ".BBRBBRBB."; ".SSRRRRSS."; "...RRRR..."; "...KKKK..." ] ]

let palette = [ ('R', rgb 230 50 40); ('S', rgb 250 190 140); ('B', rgb 60 90 230); ('K', rgb 110 60 20) ]
let jumpman = List.map (Sprite.pixels 4. palette) jumpman_rows
let jumpman_left = List.map (fun rows -> Sprite.pixels 4. palette (Sprite.flip rows)) jumpman_rows

let view_hero (h : hero) : shape list =
  match h.state with
  | Dying n -> [ circle (if n mod 10 < 5 then white else yellow) (10. + float_of_int (min n 30)) |> fade (1. - (float_of_int n / 120.)) |> move h.x (h.y + 20.) ]
  | _ -> [ Sprite.cycle (h.steps /.. 6) (if h.facing_left then jumpman_left else jumpman) |> move h.x (h.y + 20.) ]

let view_barrel (b : barrel) : shape =
  group [ circle (rgb 170 100 40) barrel_radius; rectangle (rgb 90 50 20) (2. * barrel_radius) 3.; rectangle (rgb 90 50 20) 3. (2. * barrel_radius) ]
  |> rotate b.spin |> move b.bx (b.by + barrel_radius)

(* the ape: a heap of brown ovals, an arm up when throwing *)
let view_kong (g : game) : shape list =
  let brown = rgb 150 80 30 and skin = rgb 230 180 120 and y = height girders.(5) kong_x in
  let throwing = g.next_barrel < 20 in
  [ oval brown 90. 80. |> move kong_x (y + 45.); circle brown 28. |> move kong_x (y + 100.); oval skin 30. 20. |> move kong_x (y + 92.);
    oval brown 26. 60. |> rotate (if throwing then -40. else 20.) |> move (kong_x + 45.) (y + (if throwing then 90. else 40.));
    oval brown 26. 60. |> rotate (-20.) |> move (kong_x - 45.) (y + 40.) ]
  @ List.init 3 (fun i -> view_barrel { bx = kong_x - 90. + (float_of_int (i mod 2) * 26.); by = y + (float_of_int (i /.. 2) * 24.); bvy = 0.; bstate = Dropping; spin = 0.; jumped = false })

let view_pauline (frames : int) : shape list =
  let y = girders.(top).y0 in
  [ polygon (rgb 240 120 200) [ (-40., y); (-60., y); (-50., y + 30.) ]; circle (rgb 250 190 140) 8. |> move (-50.) (y + 38.) ]
  @ if frames mod 60 < 40 then [ text white 2. "HELP!" |> move 20. (y + 45.) ] else []

let header (model : model) (g : game) : shape list =
  [ text white 2.5 (Printf.sprintf "SCORE %06d" g.score) |> move (-350.) 470.;
    text white 2.5 (Printf.sprintf "HIGH %06d" model.hi_score) |> move 0. 470.;
    rectangle (rgb 60 90 230) 130. 50. |> move 380. 455.;
    text white 2. "BONUS" |> move 380. 468.;
    text yellow 2.5 (Printf.sprintf "%d" g.bonus) |> move 380. 443. ]
  @ List.init (max 0 (g.lives -.. 1)) (fun i -> List.hd jumpman |> scale 0.5 |> move (-460. + (float_of_int i * 26.)) 430.)

let view_site (model : model) (g : game) : shape list =
  List.concat_map view_girder (Array.to_list girders)
  @ List.concat_map view_ladder ladders
  @ [ rectangle (rgb 40 80 200) 40. 50. |> move (-440.) (height girders.(0) (-440.) + 25.); text white 1.5 "OIL" |> move (-440.) (height girders.(0) (-440.) + 25.) ]
  @ view_kong g @ view_pauline g.frames
  @ List.map view_barrel g.barrels
  @ view_hero g.hero @ header model g

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  rectangle black screen.width screen.height
  ::
  (match scenes.scene with
  | Title ->
      view_site model (new_round 1 0 3)
      @ [ rectangle black 700. 190. |> fade 0.8 |> move_y (-60.); text girder_red 6. "TINY DONKEY KONG" |> move_y 0.;
          text white 2. "left/right walk   up/down climb   space jump" |> move_y (-60.) ]
      @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-110.) ]
  | Playing g -> view_site model g
  | Rescued g -> view_site model g @ [ text (rgb 240 120 200) 3. "MY HERO!" |> move (-50.) 400. ]
  | Game_over score ->
      [ text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ]
      @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
