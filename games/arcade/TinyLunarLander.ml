(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Lunar Lander (Howard Delman and Rich Moore, Atari,
 * 1979): a lunar module falls towards a jagged moon; left and right turn
 * it, up (or space) fires its engine, which pushes it the way it points,
 * and burns fuel. Put it down on a flat pad, upright and slowly enough,
 * and score; the fuel is never given back, and when it is gone the
 * module falls, and the game is over at its next touchdown.
 *
 * The game is older than video games. Jim Storer, a high-school student,
 * wrote it in 1969 in FOCAL on a PDP-8, as text: each turn the computer
 * printed the altitude, the speed and the fuel left, and you typed how
 * much fuel to burn for the next ten seconds. Jack Burness's Moonlander
 * (DEC, 1973) drew it, on the GT40's vector display, with a light pen
 * for the throttle and a McDonald's to land next to. Atari's cabinet
 * (1979) was its first vector game, the Asteroids hardware before
 * Asteroids, with a throttle lever instead of a button. (Names and dates
 * from memory, to check.)
 *
 * Three ideas are new to the games of this directory:
 *
 *  - A landing is a set of tolerances, not a collision. Everywhere else
 *    here, touching the ground (Flappy Bird) or a wall ends a life
 *    whatever the speed; here the touchdown is *judged*, by three
 *    numbers at the moment the feet touch: the vertical speed, the
 *    horizontal speed, and how far from upright the module is -- and
 *    whether both feet are on the same flat pad ([judge]). Small enough,
 *    a good landing; a bit more, a hard one (fewer points); more, or a
 *    foot on a slope, a crash. The narrower the pad, the more it's
 *    worth: x2, x3 and x5, written under each. So the score is not for
 *    surviving but for precision, and the player picks the risk.
 *
 *  - Fuel is both the resource and the clock. There are no lives and no
 *    timer: every second of thrust is taken from the whole game, so a
 *    careful landing now (braking early, hovering) is fewer landings
 *    later. The score is what you made of one tank.
 *
 *  - The camera zooms in near the ground, as the arcade did: high up,
 *    the whole moon is on the screen, so you can choose a pad; below
 *    [close_in] units of altitude the view jumps to a close-up following
 *    the module, three times bigger, because the last meters are the
 *    game and a pixel is too coarse a ruler for them. It is Camera2d's
 *    zoom, switched with some hysteresis (in below 100, out above 140),
 *    else hovering at the threshold would flicker between the two views
 *    ([update_camera]).
 *
 * The module is six numbers -- x, y, vx, vy, angle, and the fuel --
 * moved by semi-implicit Euler as in TinyFlappyBird.ml and TinyMario.ml
 * (the velocity first, then the position with the new velocity): gravity
 * down, and when the engine fires, [thrust] along the module's axis.
 * Not the Physics engine: the module never bounces, rolls nor rests on
 * anything -- the moment it touches, the flight is over and [judge]
 * decides -- so an engine's contacts and restitution would have nothing
 * to do, and its collision would say "touched" without the three numbers
 * that matter. Its shape against the ground is a few points (its feet,
 * the corners of its body) tested against the height of the terrain
 * under each ([ground_at]).
 *
 * The moon is a line of 41 points, a random walk from the LFSR of
 * TinyFlappyBird.ml, its state in the model (seed= flag, default 0xACE1),
 * with three stretches flattened into pads; each landing gets the next
 * moon from the same LFSR. The world is 1000 units wide and wraps round:
 * flying off one side comes back by the other, and the moon's last point
 * is its first.
 *
 * What it uses: Scene2d (title, play, game over), Camera2d (the zoom).
 * Not Physics (see above), not Tilemap (the moon is a line, not a grid).
 * The look is the cabinet's, thin white lines on black, drawn as thin
 * rectangles ([segment]).
 *
 * Left as exercises: the abort button (the arcade's: full thrust and
 * upright, at a cost in fuel), a throttle instead of an on/off engine
 * (the arcade's lever; the keyboard has no analog key, the mouse's y
 * could be one), difficulty levels (stronger gravity, a smaller tank,
 * drift: the arcade had four), a moon wider than the screen that
 * scrolls, fuel given back for a good landing, the module's mass going
 * down as it burns fuel (so the same thrust pushes harder: Storer's
 * version had it), the sounds (a rumbling noise for the engine).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The numbers of the game *)
(*****************************************************************************)

(* In units (a unit is a pixel far away) and seconds; the moon is 1000
 * units wide, from x = -500 to 500 *)

let gravity = 20. (* units per second, per second, down *)
let thrust = 50. (* the engine's push, along the module's axis: 2.5 gravities *)
let turn_speed = 90. (* degrees per second *)
let burn = 40. (* fuel per second of thrust *)
let full_tank = 1000.

let world_width = 1000.
let ceiling = 480. (* the module can't fly higher *)

(* the tolerances of [judge]: vertical speed, horizontal speed, degrees
 * from upright *)
let soft = (15., 8., 6.)
let hard = (30., 15., 12.)

(* the camera's close-up: in below this altitude, out above [close_out] *)
let close_in = 100.
let close_out = 140.
let close_zoom = 3.

(*****************************************************************************)
(* Randomness in the model: TinyFlappyBird's LFSR *)
(*****************************************************************************)

(* a 16-bit Galois LFSR (see TinyFlappyBird.ml for how it works) *)
let lfsr (state : int) : int =
  let shifted = state lsr 1 in
  if state land 1 = 1 then shifted lxor 0xB400 else shifted

let default_seed = 0xACE1

(* a number from 0 to 255, and the next state *)
let random_byte (state : int) : int * int =
  let rec steps n s = if n = 0 then s else steps (n -.. 1) (lfsr s) in
  let s = steps 8 state in
  (s land 0xFF, s)

(*****************************************************************************)
(* The moon *)
(*****************************************************************************)

(* The moon's points are [segments + 1] heights, one every [seg_w]
 * units from x = -500; a pad is [len] segments from point [first], all
 * at the same height, worth [mult] times the points.
 *
 *        /\        x2                    /\
 *   ____/  \    ________      /\  x5  __/  \
 *            \_/        \    /  \____/      \_
 *                        \__/
 *)
type pad = { first : int; len : int; mult : int }
type moon = { heights : number array; pads : pad list }

let segments = 40
let seg_w = world_width / float_of_int segments
let point_x (i : int) : number = (-.world_width / 2.) + (float_of_int i * seg_w)

(* the three pads, the widest the cheapest; the moon cut in thirds, a
 * pad in each, which pad in which third turned by the seed *)
let pad_kinds = [ (4, 2); (3, 3); (2, 5) ]

let new_moon (seed : int) : moon * int =
  (* the random walk, between -450 and -100: a step past a bound is
   * reflected back (clamped, the moon would have flat stretches looking
   * like pads) *)
  let rec walk i h seed acc =
    if i > segments then (List.rev acc, seed)
    else
      let byte, seed = random_byte seed in
      let h = h + (float_of_int (byte -.. 128) * 0.9) in
      let h = if h < -450. then -900. - h else if h > -100. then -200. - h else h in
      walk (i +.. 1) h seed (h :: acc)
  in
  let hs, seed = walk 0 (-300.) seed [] in
  let turn, seed = random_byte seed in
  let third = segments /.. 3 in
  let rec place k seed acc =
    if k = 3 then (List.rev acc, seed)
    else
      let len, mult = List.nth pad_kinds ((k +.. turn) mod 3) in
      (* from the third's start + 1 to its end - len - 1: never on the
       * last point, which is the first one's copy *)
      let lo = (k *.. third) +.. 1 and hi = ((k +.. 1) *.. third) -.. len -.. 1 in
      let byte, seed = random_byte seed in
      place (k +.. 1) seed ({ first = lo +.. (byte mod (hi -.. lo +.. 1)); len; mult } :: acc)
  in
  let pads, seed = place 0 seed [] in
  let hs = Array.of_list hs in
  let heights =
    Array.mapi
      (fun i h ->
        if i = segments then hs.(0) (* the wrap: the last point is the first *)
        else match List.find_opt (fun p -> i > p.first && i <= p.first +.. p.len) pads with Some p -> hs.(p.first) | None -> h)
      hs
  in
  ({ heights; pads }, seed)

(* x brought back into [-500, 500) *)
let wrap (x : number) : number = Float.rem (Float.rem (x + (world_width / 2.)) world_width + world_width) world_width - (world_width / 2.)

(* the height of the moon at x: between the two points around it, on
 * the line joining them *)
let ground_at (moon : moon) (x : number) : number =
  let u = (wrap x + (world_width / 2.)) / seg_w in
  let i = min (segments -.. 1) (int_of_float u) in
  let t = u - float_of_int i in
  moon.heights.(i) + (t * (moon.heights.(i +.. 1) - moon.heights.(i)))

(* the pad under x, if any *)
let pad_at (moon : moon) (x : number) : pad option =
  let x = wrap x in
  List.find_opt (fun p -> x >= point_x p.first && x <= point_x (p.first +.. p.len)) moon.pads

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type lander = {
  x : number;
  y : number; (* the module's center; its feet are [feet_y] below *)
  vx : number;
  vy : number;
  angle : number; (* degrees, counterclockwise, 0 upright *)
  firing : bool; (* the engine, this frame: for the flame *)
}

(* how a touchdown went: the points before the pad's multiplier *)
type outcome = Good | Hard | Crashed

type game = {
  lander : lander;
  moon : moon;
  fuel : number;
  score : int;
  seed : int; (* the LFSR's state: where the next moon comes from *)
  close : bool; (* the camera's close-up *)
  (* touched down: how, where (the pad's multiplier, 0 off a pad), and
   * the seconds since *)
  touched : (outcome * int * number) option;
}

type scene = Title | Playing of game | Game_over of game
type model = { scenes : scene Scene2d.t; best : int }

(* each moon, the module comes in high on the left, drifting right *)
let start_lander = { x = -420.; y = 400.; vx = 40.; vy = 0.; angle = -30.; firing = false }

let new_round (g : game) : game =
  let moon, seed = new_moon g.seed in
  { g with lander = start_lander; moon; seed; close = false; touched = None }

let new_game (seed : int) : game =
  new_round { lander = start_lander; moon = { heights = [||]; pads = [] }; fuel = full_tank; score = 0; seed; close = false; touched = None }

(* the seed= flag, e.g. seed=42; an LFSR's seed can't be 0, nor more than
 * 16 bits *)
let flag_seed (computer : computer) : int =
  match Option.bind (List.assoc_opt "seed" computer.flags) int_of_string_opt with
  | Some n when n land 0xFFFF <> 0 -> n land 0xFFFF
  | _ -> default_seed

let initial_model : model = { scenes = Scene2d.start Title; best = 0 }

(*****************************************************************************)
(* The module's shape *)
(*****************************************************************************)

(* The module upright, around its center; y up:
 *
 *            __
 *           /  \        the ascent stage (the cabin)
 *           \__/
 *         +------+      the descent stage
 *        / +----+ \     its legs,
 *       /   \__/   \    its nozzle,
 *     -+-          -+-  its feet, [feet_y] below the center
 *)
let feet_y = 18.
let feet = [ (-15., -.feet_y); (15., -.feet_y) ]

let cabin = [ (-4., 16.); (4., 16.); (9., 11.); (9., 5.); (4., 0.); (-4., 0.); (-9., 5.); (-9., 11.) ]
let stage = [ (-11., 0.); (11., 0.); (11., -9.); (-11., -9.) ]
let nozzle = [ (-4., -9.); (4., -9.); (6., -13.); (-6., -13.) ]

(* the corners that must not touch the moon: all of them but the feet *)
let body = cabin @ stage

(* a point of the module, in the world: turned by the module's angle,
 * then moved to its center *)
let to_world (l : lander) ((px, py) : number * number) : number * number =
  let a = degrees_to_radians l.angle in
  (l.x + (px * cos a) - (py * sin a), l.y + (px * sin a) + (py * cos a))

(* the altitude: from the lower foot to the moon under it *)
let altitude (moon : moon) (l : lander) : number =
  List.fold_left (fun acc p -> let x, y = to_world l p in min acc (y - ground_at moon x)) infinity feet

(*****************************************************************************)
(* The landing *)
(*****************************************************************************)

(* Whether the module touches the moon: a foot, or a corner of its body,
 * at or under the ground *)
let touches (moon : moon) (l : lander) : bool =
  List.exists (fun p -> let x, y = to_world l p in y <= ground_at moon x) (feet @ body)

(* How the touchdown went, and the pad's multiplier (0 off a pad). Good
 * or Hard only if both feet are on the same pad, no corner of the body
 * down, and the three numbers within [soft] or [hard]:
 *
 *   vy -8, vx 3, angle 2, on the x3 pad     -> Good, 3 (50 x 3 points)
 *   vy -20, vx 3, angle 2, on the x3 pad    -> Hard, 3 (15 x 3)
 *   vy -8, vx 3, angle 20                   -> Crashed (a leg first)
 *   vy -8, a foot on the pad, one on a rock -> Crashed
 *)
let judge (moon : moon) (l : lander) : outcome * int =
  let within (sv, sh, sa) = Float.abs l.vy <= sv && Float.abs l.vx <= sh && Float.abs l.angle <= sa in
  let pads = List.map (fun p -> pad_at moon (fst (to_world l p))) feet in
  let body_down = List.exists (fun p -> let x, y = to_world l p in y <= ground_at moon x) body in
  match pads with
  | [ Some p1; Some p2 ] when p1 = p2 && not body_down ->
      if within soft then (Good, p1.mult) else if within hard then (Hard, p1.mult) else (Crashed, 0)
  | _ -> (Crashed, 0)

let points (o : outcome) (mult : int) : int = match o with Good -> 50 *.. mult | Hard -> 15 *.. mult | Crashed -> 0

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let dt = 1. / 60.

(* One tick of flight, semi-implicit Euler: the velocity (gravity, and
 * the engine's push along the module's axis, turned by its angle), then
 * the position with it. The engine fires only while there is fuel. *)
let fly (k : keyboard) (fuel : number) (l : lander) : lander * number =
  let turn = (if k.kleft then 1. else 0.) - if k.kright then 1. else 0. in
  let angle = clamp (-90.) 90. (l.angle + (turn * turn_speed * dt)) in
  let firing = (k.kup || k.kspace) && fuel > 0. in
  let a = degrees_to_radians angle in
  let push = if firing then thrust else 0. in
  let vx = l.vx + (-.sin a * push * dt) and vy = l.vy + (((cos a * push) - gravity) * dt) in
  let y = l.y + (vy * dt) in
  let vy = if y > ceiling then min 0. vy else vy in
  ({ x = wrap (l.x + (vx * dt)); y = min ceiling y; vx; vy; angle; firing }, if firing then max 0. (fuel - (burn * dt)) else fuel)

(* the close-up, with hysteresis: in below [close_in], out above
 * [close_out], unchanged in between *)
let update_camera (g : game) : game =
  let alt = altitude g.moon g.lander in
  { g with close = (if alt < close_in then true else if alt > close_out then false else g.close) }

let update_game (k : keyboard) (g : game) : game =
  match g.touched with
  | Some (o, mult, since) -> { g with touched = Some (o, mult, since + dt) }
  | None ->
      let lander, fuel = fly k g.fuel g.lander in
      let g = update_camera { g with lander; fuel } in
      if touches g.moon lander then
        let o, mult = judge g.moon lander in
        { g with lander = { lander with firing = false }; touched = Some (o, mult, 0.); score = g.score +.. points o mult }
      else g

let started (scenes : scene Scene2d.t) : bool = Scene2d.pressed (fun k -> k.kspace) scenes || Scene2d.pressed (fun k -> k.kenter) scenes

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  match scenes.scene with
  | Title -> if started scenes then { model with scenes = Scene2d.go (Playing (new_game (flag_seed computer))) scenes } else { model with scenes }
  | Playing g -> (
      let g = update_game computer.keyboard g in
      let best = max model.best g.score in
      match g.touched with
      (* three seconds to read the verdict, then the next moon, or the
       * end if the tank is empty *)
      | Some (_, _, since) when since > 3. ->
          if g.fuel <= 0. then { best; scenes = Scene2d.go (Game_over g) scenes } else { best; scenes = { scenes with scene = Playing (new_round g) } }
      | _ -> { best; scenes = { scenes with scene = Playing g } })
  | Game_over g ->
      if scenes.elapsed > 1. && started scenes then { model with scenes = Scene2d.go (Playing (new_game g.seed)) scenes }
      else if scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes }
      else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a line, as a thin rectangle [w] wide *)
let segment (color : color) (w : number) ((x0, y0) : number * number) ((x1, y1) : number * number) : shape =
  rectangle color (Float.hypot (x1 - x0) (y1 - y0) + w) w |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi) |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.)

(* a closed outline *)
let outline (color : color) (w : number) (ps : (number * number) list) : shape list =
  List.map2 (segment color w) ps (List.tl ps @ [ List.hd ps ])

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the moon, three times (the one in the middle, and its copies left and
 * right, which the wrap round and the close-up at an edge show), its
 * pads brighter and their multipliers under them *)
let view_moon (w : number) (moon : moon) : shape list =
  let one =
    List.init segments (fun i -> segment white w (point_x i, moon.heights.(i)) (point_x (i +.. 1), moon.heights.(i +.. 1)))
    @ List.concat_map
        (fun p ->
          let x0 = point_x p.first and x1 = point_x (p.first +.. p.len) and y = moon.heights.(p.first) in
          [ segment yellow (2. * w) (x0, y) (x1, y); text yellow 1.5 (Printf.sprintf "x%d" p.mult) |> move ((x0 + x1) / 2.) (y - 18.) ])
        moon.pads
  in
  List.map (fun dx -> group one |> move_x dx) [ -.world_width; 0.; world_width ]

(* the module: its outlines, its legs, and the flame, flickering, while
 * the engine fires *)
let view_lander (w : number) (frames : int) (l : lander) : shape =
  let legs = [ ((-11., -5.), (-15., -.feet_y)); ((11., -5.), (15., -.feet_y)); ((-18., -.feet_y), (-12., -.feet_y)); ((12., -.feet_y), (18., -.feet_y)) ] in
  let flame = if l.firing then let len = 12. + float_of_int (frames mod 3 *.. 5) in [ segment orange w (-4., -13.) (0., -13. - len); segment orange w (4., -13.) (0., -13. - len) ] else [] in
  group (outline white w cabin @ outline white w stage @ outline white w nozzle @ List.map (fun (a, b) -> segment white w a b) legs @ flame)
  |> rotate l.angle |> move l.x l.y

(* the crash: the module's pieces flying apart from its center, each
 * outward along the line from the center to its middle *)
let view_debris (w : number) (since : number) (l : lander) : shape list =
  let pieces = List.concat_map (fun ps -> List.combine ps (List.tl ps @ [ List.hd ps ])) [ cabin; stage ] in
  List.mapi
    (fun i ((x0, y0), (x1, y1)) ->
      let mx = (x0 + x1) / 2. and my = (y0 + y1) / 2. in
      let d = since * 40. in
      segment white w (x0 - mx, y0 - my) (x1 - mx, y1 - my) |> rotate (since * float_of_int (i *.. 70)) |> move (mx + (mx * d / 10.)) (my + (my * d / 10.) + 3.))
    pieces
  |> List.map (fun s -> s |> rotate l.angle |> move l.x l.y)

(* the camera: the whole moon far away, or the module close up (a bit
 * above it, so the ground under it is on the screen) *)
let camera (screen : screen) (g : game) : Camera2d.t =
  let far = min screen.width screen.height / world_width in
  if g.close then { Camera2d.origin with zoom = far * close_zoom } |> Camera2d.look_at g.lander.x (g.lander.y - 30.)
  else { Camera2d.origin with zoom = far }

let view_world (computer : computer) (frames : int) (g : game) : shape list =
  let cam = camera computer.screen g in
  (* lines as thin on the screen near as far *)
  let w = 1.5 / cam.zoom in
  let lander = match g.touched with Some (Crashed, _, since) -> view_debris w since g.lander | _ -> [ view_lander w frames g.lander ] in
  [ Camera2d.view cam (view_moon w g.moon @ lander) ]

(* the instruments, as on the cabinet: the score and the fuel on the
 * left, the altitude and the two speeds on the right, with the way
 * they go *)
let view_hud (screen : screen) (g : game) : shape list =
  let l = g.lander in
  let left = screen.left + 40. and right = screen.right - 40. and top = screen.top - 40. in
  let line x y s = text white 2. s |> move x y in
  let way v pos neg = if Float.abs v < 0.5 then "" else if v > 0. then pos else neg in
  [ line (left + 110.) top (Printf.sprintf "SCORE %04d" g.score);
    line (left + 110.) (top - 36.) (Printf.sprintf "FUEL  %04d" (int_of_float g.fuel));
    line (right - 190.) top (Printf.sprintf "ALTITUDE %4d" (max 0 (int_of_float (altitude g.moon l))));
    line (right - 190.) (top - 36.) (Printf.sprintf "HORIZONTAL SPEED %3d %s" (int_of_float (Float.abs l.vx)) (way l.vx ">" "<"));
    line (right - 190.) (top - 72.) (Printf.sprintf "VERTICAL SPEED %3d %s" (int_of_float (Float.abs l.vy)) (way l.vy "^" "v")) ]

let verdict (o : outcome) (mult : int) : string list =
  match o with
  | Good -> [ "A GOOD LANDING"; Printf.sprintf "50 x %d = %d POINTS" mult (points o mult) ]
  | Hard -> [ "A HARD LANDING"; Printf.sprintf "15 x %d = %d POINTS" mult (points o mult) ]
  | Crashed -> [ "YOU CRASHED"; "NO POINTS" ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let scenes = model.scenes in
  rectangle black screen.width screen.height
  ::
  (match scenes.scene with
  | Title ->
      let g = new_game default_seed in
      view_world computer scenes.frames g
      @ [ text white 5. "TINY LUNAR LANDER" |> move_y 300.;
          text white 2.5 "left, right: turn    up or space: engine" |> move_y 220.;
          text white 2.5 "land upright and slowly, on a flat pad" |> move_y 180. ]
      @ Scene2d.blink 1. scenes [ text white 3. "PRESS SPACE" |> move_y 60. ]
  | Playing g ->
      let messages =
        match g.touched with
        (* high on the screen: in the close-up, the module is in the
         * middle *)
        | Some (o, mult, _) -> List.mapi (fun i s -> text yellow 3. s |> move_y (300. - (float_of_int i * 50.))) (verdict o mult)
        | None -> if g.fuel <= 0. then [ text orange 3. "OUT OF FUEL" |> move_y 300. ] else if g.fuel < 150. then Scene2d.blink 0.5 scenes [ text orange 3. "LOW ON FUEL" |> move_y 300. ] else []
      in
      view_world computer scenes.frames g @ view_hud screen g @ messages
  | Game_over g ->
      view_world computer scenes.frames g
      @ view_hud screen g
      @ [ text orange 6. "GAME OVER" |> move_y 200.;
          text white 3. (Printf.sprintf "SCORE %d    BEST %d" g.score model.best) |> move_y 120. ]
      @ if scenes.elapsed > 1. then Scene2d.blink 1. scenes [ text white 3. "PRESS SPACE" |> move_y 40. ] else [])

let app = game view update initial_model
let main = Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
