(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Shufflepuck Café (Christopher Gross, Brøderbund,
 * 1988, on the Macintosh first): air hockey in a bar at the edge of the
 * galaxy, against its regulars, one after the other. First to 7 (the
 * original played to 15), and the next one sits down.
 *
 *   the mouse        your paddle, where it points on the table
 *   arrows           your paddle, from the keyboard
 *   space            start, and on to the next opponent
 *
 * The rules are Pong's (TinyPong.ml), and the physics the
 * playground's engine's (Physics). What Shufflepuck added
 * is the *view* -- you look down the table from your end of it, as you
 * would standing at a real one -- and the *people*.
 *
 * The view is the trick of this game, and the simplest perspective in
 * the 2.5D games: everything is on one plane, the table, seen from one eye
 * that never moves. A point of the table [depth] ahead of the eye,
 * [x] across it, is drawn
 *
 *     sx = f x / depth          sy = horizon - f h / depth
 *
 * with [h] the eye's height over the table and [f] the focal length --
 * one division, and a size that shrinks by the same [f / depth]:
 *
 *       eye
 *        o-----------------------------  horizon (depth infinite)
 *        |  .   .
 *      h |      .   .       far end    ___________
 *        |          .   .             /           \
 *     ---+----------+------+----     /             \
 *        |<-- d -->| table            \_____________/ near end
 *
 * It is TinyMarioKart's Mode 7 for a table that never turns: the
 * same one division per row, where Mode 7 has to redo it for every row
 * of every frame because the floor turns under you. Here nothing turns,
 * so [project] is all of it, and [unproject] -- the same division the
 * other way -- is what turns the mouse into a point of the table.
 *
 * What the trick throws away is the one number the game is about: how
 * far away the puck is. On the screen, a puck coming at you is a puck
 * getting bigger and lower, and a fast one is on you before its size
 * has told you anything. The original's answer was sound -- a *clack*
 * at every bounce off the rails -- and the puck's size; here the size,
 * the puck drawn as the flattened oval a disc on a table is from an
 * eye above it, and the paddles standing on it to judge it against.
 *
 * The people are the other half, and each one is the same few knobs
 * ([opponent]): how late they see the puck (their reaction, in frames),
 * whether they see where it *will* cross their line or only where it
 * is -- which is what a bank shot beats -- how fast they move, how far
 * forward they dare come, how they aim --
 * straight at your goal, or banked off a rail, where you are not
 * looking -- and how sloppy that aim is. Four of them, from a nervous
 * beginner to a princess who banks everything; the robot never misses
 * a straight shot and never plays anything else, which is how you beat
 * it.
 *
 * The table is Physics's, in metres, as TinyPinball.ml's
 * physics=engine is in pixels: the rails are immovable boxes, the puck
 * a circle, and a paddle an immovable circle *given the velocity your
 * hand moved it with* -- so that Physics.bounce_off, which takes the
 * speed of what it bounces off at the point of contact, gives the puck
 * the swing of the paddle and not just a wall to bounce off. A hand is
 * not pushed back by a puck, which is what immovable says. And the puck
 * is fast and small: at 4 m/s it goes 7 cm between two frames, more
 * than its own width, and would cross a paddle without ever touching it
 * (tunnelling, as the pinball's ball does). So the game moves it itself,
 * in eight steps a frame ([substeps]), never more than a centimetre in
 * one, and asks the engine about each touch -- TinyPinball's way.
 *
 * Uses: Physics (bodies, touching, bounce_off), Scene2d. No
 * 3D twin: a flat table in a real 3D engine would teach nothing the
 * projection here does not, which is also why TinyDungeonMaster has
 * none.
 *
 * Exercises: the rest of the café (Skip, Vinnie, Lexan, Nerual, the
 * DC3 and Biff, in the original); a practice machine, a robot arm
 * serving at you; spin, a puck struck off-centre curving; the original's
 * shield, a bar of force across the goal for the last opponent; the two
 * of you on one keyboard.
 *)
open Playground

(*****************************************************************************)
(* The table *)
(*****************************************************************************)

(* In metres: the table is 1 m across and 2 m long, your end at y = 0
 * and the opponent's at y = 2; the goals are slots 36 cm wide in the
 * middle of each end. *)
let half_width = 0.5
let length = 2.
let goal_half = 0.18
let puck_r = 0.03
let paddle_r = 0.055

(*****************************************************************************)
(* The view -- the trick of this game, in 22 lines (see the header) *)
(*****************************************************************************)

(* The eye: 2 m behind your end of the table and 1.3 m above it, with a
 * focal length of 1800 pixels -- worked out from where the table should
 * be on the screen: its near end 900 pixels wide along the bottom
 * (sy = -450), the centre line at -60, the far end at 135 and 450
 * pixels wide, the far half half as tall as the near one. (A first eye,
 * low and close, 0.8 m up and 0.6 m behind, squeezed the whole far half
 * into a strip, the puck a dot in it.) The horizon, 720, is above the
 * screen: looking down at a table, the eye never sees it. *)
let behind = 2.
let eye_height = 1.3
let focal = 1800.
let horizon = 720.

(* [project (x, y, z)]: a point [z] above the table at (x, y), on the
 * screen, and how many pixels a metre is there *)
let project ((x, y, z) : number * number * number) : number * number * number =
  let depth = y +. behind in
  (focal *. x /. depth, horizon -. (focal *. (eye_height -. z) /. depth), focal /. depth)

(* [unproject (sx, sy)]: the point of the table drawn at (sx, sy) -- the
 * same division the other way *)
let unproject ((sx, sy) : number * number) : number * number =
  let depth = focal *. eye_height /. Float.max 1. (horizon -. sy) in
  (sx *. depth /. focal, depth -. behind)

(*****************************************************************************)
(* The opponents *)
(*****************************************************************************)

type aim = Straight | Bank

(* the knobs: everyone is these five numbers and a face *)
type opponent = {
  name : string;
  color : color;
  reaction : int; (* frames between the puck being somewhere and her seeing it there *)
  predicts : bool; (* where it will cross her line, or only where it is *)
  speed : number; (* m/s, the most her paddle moves *)
  reach : number; (* how far forward she comes: the least y of her paddle *)
  aim : aim;
  sloppy : number; (* metres her aim can be off *)
}

let cast : opponent list =
  [ { name = "NERVOUS NED"; color = rgb 120 170 90; reaction = 14; predicts = false; speed = 1.1; reach = 1.6; aim = Straight;
      sloppy = 0.18 };
    { name = "ROBO-9"; color = rgb 150 160 180; reaction = 0; predicts = true; speed = 2.; reach = 1.35; aim = Straight; sloppy = 0. };
    { name = "BANK BETTY"; color = rgb 220 120 160; reaction = 6; predicts = true; speed = 2.2; reach = 1.3; aim = Bank;
      sloppy = 0.05 };
    { name = "THE PRINCESS"; color = rgb 200 170 250; reaction = 2; predicts = true; speed = 3.; reach = 1.15; aim = Bank;
      sloppy = 0.02 } ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type puck = { x : number; y : number; vx : number; vy : number }
type paddle = { px : number; py : number; pvx : number; pvy : number }

type rally = {
  who : int; (* in [cast] *)
  puck : puck;
  you : paddle;
  them : paddle;
  seen : puck list; (* the puck's last places, newest first: what she sees is one of the old ones *)
  mine : int;
  theirs : int;
  frames : int;
  wait : int; (* frames until the puck is served again *)
  mouse : bool; (* the paddle follows the mouse, once it has moved *)
}

type scene = Title | Playing of rally | Beaten of rally | Lost of rally | Champion
type model = scene Scene2d.t

let to_win = 7

(* served from the side that let the last one in, at rest *)
let served (towards_you : bool) : puck =
  { x = 0.; y = (if towards_you then 0.45 else length -. 0.45); vx = 0.; vy = 0. }

let new_rally (who : int) : rally =
  { who; puck = served true; you = { px = 0.; py = 0.25; pvx = 0.; pvy = 0. };
    them = { px = 0.; py = length -. 0.25; pvx = 0.; pvy = 0. }; seen = []; mine = 0; theirs = 0; frames = 0; wait = 0;
    mouse = false }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The puck *)
(*****************************************************************************)

let substeps = 8
let max_speed = 4.

type outcome = Rolling | Your_goal | Their_goal

(* the rails, as the engine sees them: boxes 10 cm thick outside the
 * table, the ends in two pieces either side of the slot *)
let rails : Physics.body list =
  let rail x y w h = Physics.body (rectangle black w h) |> Physics.at x y |> Physics.immovable |> Physics.bouncy 0.9 in
  let side = half_width -. goal_half in
  let end_x = goal_half +. (side /. 2.) in
  [ rail (-.half_width -. 0.05) (length /. 2.) 0.1 (length +. 0.2);
    rail (half_width +. 0.05) (length /. 2.) 0.1 (length +. 0.2);
    rail (-.end_x) (-0.05) side 0.1; rail end_x (-0.05) side 0.1;
    rail (-.end_x) (length +. 0.05) side 0.1; rail end_x (length +. 0.05) side 0.1 ]

(* a paddle: immovable, the hand not pushed back, but moving -- the
 * engine hits the puck with its speed *)
let paddle_body (h : paddle) : Physics.body =
  Physics.body (circle white paddle_r) |> Physics.at h.px h.py |> Physics.moving h.pvx h.pvy |> Physics.immovable
  |> Physics.bouncy 0.9

let puck_body (p : puck) : Physics.body =
  Physics.body (circle white puck_r) |> Physics.at p.x p.y |> Physics.moving p.vx p.vy |> Physics.bouncy 0.9
  |> Physics.upright

(* one substep: the puck moved, and every touch answered by the engine;
 * or out through a slot *)
let slide (dt : number) (you : paddle) (them : paddle) (p : puck) : puck * outcome =
  let p = { p with x = p.x +. (p.vx *. dt); y = p.y +. (p.vy *. dt) } in
  if p.y < -.puck_r then (p, Their_goal)
  else if p.y > length +. puck_r then (p, Your_goal)
  else
    let body =
      List.fold_left
        (fun b o -> if Physics.touching o b then Physics.bounce_off o b else b)
        (puck_body p)
        (rails @ [ paddle_body you; paddle_body them ])
    in
    let s = Float.hypot body.vx body.vy in
    let k = if s > max_speed then max_speed /. s else 1. in
    ({ x = body.x; y = body.y; vx = body.vx *. k; vy = body.vy *. k }, Rolling)

(* a frame of the puck: [substeps] of it, and a little air friction *)
let roll (you : paddle) (them : paddle) (p : puck) : puck * outcome =
  let dt = 1. /. 60. /. float_of_int substeps in
  let rec go p n = if n = 0 then (p, Rolling) else match slide dt you them p with p, Rolling -> go p (n - 1) | done_ -> done_ in
  let p, outcome = go p substeps in
  ({ p with vx = p.vx *. 0.997; vy = p.vy *. 0.997 }, outcome)

(*****************************************************************************)
(* The hands *)
(*****************************************************************************)

(* a paddle moved towards [target], at most [speed] m/s, kept on its
 * half ([ymin] to [ymax]); its velocity is what it hits with *)
let move_paddle (speed : number) (ymin : number) (ymax : number) ((tx, ty) : number * number) (h : paddle) : paddle =
  let tx = Float.max (paddle_r -. half_width) (Float.min (half_width -. paddle_r) tx) in
  let ty = Float.max ymin (Float.min ymax ty) in
  let dx = tx -. h.px and dy = ty -. h.py in
  let d = Float.hypot dx dy and most = speed /. 60. in
  let k = if d > most then most /. d else 1. in
  { px = h.px +. (dx *. k); py = h.py +. (dy *. k); pvx = dx *. k *. 60.; pvy = dy *. k *. 60. }

(* where a puck at (x, y) moving (vx, vy) crosses the line y = [line],
 * the rails folded out as mirrors (a bank off a rail is a straight line
 * in the reflected table) *)
let crossing (p : puck) (line : number) : number option =
  if Float.abs p.vy < 1e-6 || (line -. p.y) /. p.vy < 0. then None
  else
    let x = p.x +. (p.vx *. ((line -. p.y) /. p.vy)) in
    let w = half_width -. puck_r in
    let period = 4. *. w in
    let m = Float.rem (Float.rem (x +. w) period +. period) period in
    Some (if m < 2. *. w then m -. w else (3. *. w) -. m)

(* her wobble: a number in [-1, 1] that changes from one point to the
 * next -- not Random, so that a match can be replayed *)
let wobble (k : int) : number =
  let s = Float.abs (sin (float_of_int k *. 12.9898) *. 43758.5453) in
  (2. *. (s -. Float.of_int (truncate s))) -. 1.

(* What she does: she sees the puck [reaction] frames late (and, before
 * she has watched that long, as it was when she started watching). On
 * her side, she shoots: towards your goal -- or towards the goal's
 * mirror image across a rail, which is a bank shot -- in two moves,
 * first behind the puck on that line, then through it along it (a
 * first version went straight at the puck, and pushed it wherever she
 * came from: no bank ever banked). Coming at her, she goes where it
 * will cross her line if she can see that far, where it is if not.
 * Going away, she goes home. *)
let think (o : opponent) (r : rally) : number * number =
  let p =
    match List.nth_opt r.seen o.reaction with
    | Some p -> p
    | None -> ( match List.rev r.seen with oldest :: _ -> oldest | [] -> r.puck)
  in
  let home = (0., length -. 0.2) in
  if p.y > 1. && (p.vy >= -0.5 || Float.hypot p.vx p.vy < 1.) then
    let off = o.sloppy *. wobble (r.mine + (3 * r.theirs)) in
    let gx, gy =
      match o.aim with
      | Straight -> (off, 0.)
      | Bank -> ((if p.x >= 0. then -.2. *. half_width else 2. *. half_width) +. off, 0.)
    in
    let dx = gx -. p.x and dy = gy -. p.y in
    let d = Float.hypot dx dy in
    let ux = dx /. d and uy = dy /. d in
    (* lined up behind it: the paddle on the far side of the puck from
     * the aim, and along the line -- then through it *)
    let bx = r.them.px -. p.x and by = r.them.py -. p.y in
    let bd = Float.max 1e-6 (Float.hypot bx by) in
    let lined = ((-.bx *. ux) +. (-.by *. uy)) /. bd > 0.95 in
    if lined then (p.x +. (ux *. 0.1), p.y +. (uy *. 0.1))
    else
      let gap = puck_r +. paddle_r +. 0.04 in
      (p.x -. (ux *. gap), p.y -. (uy *. gap))
  else if p.vy > 0. then
    if o.predicts then match crossing p (length -. 0.2) with Some x -> (x, length -. 0.2) | None -> home
    else (p.x, length -. 0.2)
  else home

(* yours: the mouse, once it has moved, or the arrows *)
let your_target (computer : computer) (r : rally) : number * number * bool =
  let k = computer.keyboard and m = computer.mouse in
  let arrows = k.kleft || k.kright || k.kup || k.kdown in
  let mouse = (r.mouse || m.mdx <> 0. || m.mdy <> 0.) && not arrows in
  if mouse then let x, y = unproject (m.mx, m.my) in (x, y, true)
  else
    let axis a b = (if a then 1. else 0.) -. if b then 1. else 0. in
    (r.you.px +. (0.05 *. axis k.kright k.kleft), r.you.py +. (0.05 *. axis k.kup k.kdown), false)

let step_rally (computer : computer) (r : rally) : rally =
  let o = List.nth cast r.who in
  let tx, ty, mouse = your_target computer r in
  let you = move_paddle 3.5 0.05 0.95 (tx, ty) r.you in
  let them = move_paddle o.speed (Float.max 1.05 o.reach) (length -. 0.05) (think o r) r.them in
  let r = { r with you; them; mouse; frames = r.frames + 1; seen = List.filteri (fun i _ -> i < 30) (r.puck :: r.seen) } in
  if r.wait > 0 then { r with wait = r.wait - 1 }
  else
    match roll you them r.puck with
    | puck, Rolling -> { r with puck }
    | _, Your_goal -> { r with mine = r.mine + 1; puck = served false; wait = 60; seen = [] }
    | _, Their_goal -> { r with theirs = r.theirs + 1; puck = served true; wait = 60; seen = [] }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_rally 0)) s else s
  | Playing r ->
      let r = step_rally computer r in
      if r.mine >= to_win then Scene2d.go (if r.who + 1 >= List.length cast then Champion else Beaten r) s
      else if r.theirs >= to_win then Scene2d.go (Lost r) s
      else { s with scene = Playing r }
  | Beaten r -> if space then Scene2d.go (Playing (new_rally (r.who + 1))) s else s
  | Lost r -> if space then Scene2d.go (Playing (new_rally r.who)) s else s
  | Champion -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

(* the table's outline, a rim a few centimetres wide around it *)
let quad (color : color) (x1, y1) (x2, y2) (z : number) : shape =
  let p x y = let sx, sy, _ = project (x, y, z) in (sx, sy) in
  polygon color [ p x1 y1; p x2 y1; p x2 y2; p x1 y2 ]

(* a disc lying on the table, [z] up: an oval, as wide as the scale
 * says and flattened by how steeply the eye looks down at it there *)
let disc (color : color) (x : number) (y : number) (z : number) (r : number) : shape =
  let sx, sy, scale = project (x, y, z) in
  let depth = y +. behind in
  oval color (2. *. r *. scale) (2. *. r *. scale *. (eye_height -. z) /. depth) |> move sx sy

let paddle_shapes (color : color) (h : paddle) : shape list =
  [ disc (rgb 30 30 30) h.px h.py 0.001 paddle_r; disc color h.px h.py 0.02 paddle_r;
    disc (rgb 240 240 240) h.px h.py 0.05 (paddle_r *. 0.4) ]

let puck_shapes (p : puck) : shape list =
  [ disc (rgb 20 20 20) p.x p.y 0.001 puck_r; disc (rgb 230 60 40) p.x p.y 0.012 puck_r ]

(* her, behind the far end of the table, above her paddle *)
let person (o : opponent) (r : rally) : shape list =
  let sx, sy, scale = project (r.them.px, length +. 0.3, 0.) in
  (* a person at the far end's scale is taller than the screen: drawn
   * at a little over half of it, a figure behind the table rather
   * than a giant *)
  let scale = scale *. 0.55 in
  let mood = if r.theirs > r.mine then 1. else if r.mine > r.theirs then -1. else 0. in
  [ rectangle o.color (0.5 *. scale) (0.7 *. scale) |> move sx (sy +. (0.3 *. scale));
    circle (rgb 240 210 180) (0.13 *. scale) |> move sx (sy +. (0.8 *. scale));
    circle black (0.02 *. scale) |> move (sx -. (0.05 *. scale)) (sy +. (0.83 *. scale));
    circle black (0.02 *. scale) |> move (sx +. (0.05 *. scale)) (sy +. (0.83 *. scale));
    rectangle black (0.1 *. scale) (0.015 *. scale) |> rotate (15. *. mood) |> move sx (sy +. (0.74 *. scale));
    text white 1.8 o.name |> move sx (sy +. (1.05 *. scale)) ]

let view_rally (computer : computer) (r : rally) : shape list =
  let screen = computer.screen in
  let o = List.nth cast r.who in
  (* the café, the table's rim and its top, the centre line, the slots *)
  let table =
    [ quad (rgb 70 45 30) (-.half_width -. 0.05, -0.05) (half_width +. 0.05, length +. 0.05) 0.;
      quad (rgb 30 90 110) (-.half_width, 0.) (half_width, length) 0.;
      quad (rgb 90 170 190) (-.half_width, 0.995) (half_width, 1.005) 0.;
      quad black (-.goal_half, -0.05) (goal_half, 0.) 0.001;
      quad black (-.goal_half, length) (goal_half, length +. 0.05) 0.001 ]
  in
  (* far things first: her paddle, the puck, yours -- each drawn over
   * what is behind it *)
  let things =
    List.sort (fun (a, _) (b, _) -> compare b a)
      [ (r.them.py, paddle_shapes o.color r.them); (r.puck.y, puck_shapes r.puck); (r.you.py, paddle_shapes (rgb 90 140 230) r.you) ]
    |> List.concat_map snd
  in
  [ rectangle (rgb 40 25 30) screen.width screen.height; rectangle (rgb 60 40 45) screen.width 120. |> move_y (horizon +. 60.) ]
  @ person o r @ table @ things
  @ [ text white 3. (Printf.sprintf "YOU %d" r.mine) |> move (screen.left +. 120.) (screen.top -. 40.);
      text o.color 3. (Printf.sprintf "%s %d" o.name r.theirs) |> move (screen.right -. 200.) (screen.top -. 40.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  let over (r : rally) (title : string) (color : color) =
    view_rally computer r @ [ rectangle black 900. 160. |> move_y 60. |> fade 0.7; text color 5. title |> move_y 80. ]
    @ Scene2d.blink 1. s [ text white 2.5 "PRESS SPACE" |> move_y 20. ]
  in
  match s.scene with
  | Title ->
      view_rally computer (new_rally 0)
      @ [ rectangle black 900. 250. |> move_y 60. |> fade 0.7; text (rgb 240 200 120) 6. "TINY SHUFFLEPUCK" |> move_y 130.;
          text white 2.2 "the mouse or the arrows: your paddle   first to 7" |> move_y 70. ]
      @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 10. ]
  | Playing r -> view_rally computer r
  | Beaten r -> over r (Printf.sprintf "%s IS BEATEN" (List.nth cast r.who).name) (rgb 120 230 120)
  | Lost r -> over r "YOU LOST" (rgb 240 90 90)
  | Champion ->
      [ rectangle (rgb 40 25 30) screen.width screen.height; text (rgb 240 200 120) 5. "CHAMPION OF THE CAFE";
        text white 2.2 "everyone at the bar has been beaten" |> move_y (-70.) ]

let app = game view update initial_model

let main = Playground_platform.run_app app
