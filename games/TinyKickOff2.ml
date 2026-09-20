(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Kick Off 2 (Dino Dini, Anco, 1990; Kick Off the year before),
 * the football game seen from above whose one idea changed how the
 * whole thing feels: THE BALL IS NOT GLUED TO YOUR FEET. Arrows run,
 * space kicks (tap to pass, hold to shoot), and after you kick it the
 * arrows bend the ball in the air -- the aftertouch.
 *
 * Every other football game of the time, and Sensible Soccer (1992)
 * after it, carried the ball along with whoever was nearest: you ran,
 * it came with you, and dribbling was steering. Dini's ball is a thing
 * of its own, touched ahead of you as you run into it and rolling on
 * by itself a little faster than you can run, so a dribble is a chase
 * you are only just winning, and every touch is a decision about
 * whether you will get there:
 *
 *      glued (Sensible)          free (Kick Off)
 *      p o---->                  p   o- - ->      the ball runs on;
 *      the ball is a part        you have to catch it up, and an
 *      of the player             opponent may get there first
 *
 * The flag ball=glued plays it the other way, which is the fastest way
 * to feel what the decision is worth. Measured, dribbling straight up
 * the pitch for three seconds (tests/games/Unit_games.ml): glued, the
 * ball stays 22 pixels from him the whole way, which is his feet --
 * the two radii, exactly; free, it gets 48 ahead, and he touches it 5
 * times in those 180 frames. The rest is running after it.
 *
 * The second idea is the aftertouch, the thing Kick Off players spent
 * years on: while the ball is in the air from *your* kick, holding a
 * direction bends it. It is a small sideways acceleration on a ball
 * that is already moving, and it is what turns a shot into a shot on
 * goal. Measured here: the same shot, held right for its 40 frames in
 * the air, ends up 141 pixels to the side of the one left alone.
 *
 * The third is the shape of a team. Each player has a place in the
 * formation rather than a brain: the nearest one to the ball chases
 * it, and everyone else walks towards where they belong, which is
 * their own spot pulled a third of the way towards the ball. Ten lines
 * of that looks more like football than any amount of cleverness, and
 * it is the same trick as a flock: no player knows the plan, and the
 * shape appears anyway.
 *
 * What it uses: Camera2d (the pitch is taller than the screen, and
 * Kick Off scrolls up and down it), Scene2d, Audio. Not Physics: a
 * ball on grass is a velocity and a friction, and the whole of the
 * game's physics is the touch that pushes it ahead of you. Not
 * kits/*: this is the first sports game here; if a second one comes
 * (TinySpeedball2 is the obvious one, and Speedball's arena is a
 * pinball table with players in it), the pitch, the formation and the
 * free ball are what it would want out of this file.
 *
 * Exercises: the second player, on the same keyboard; the goalkeeper
 * who comes out; throw-ins and corners taken by hand rather than
 * given; the referee and the offside rule (a line in the model, and
 * an argument in the pub); a curved pass, which is aftertouch on the
 * ground; and Kick Off's own tactics screens, which were a formation
 * as data, the very thing [formation] below is.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The pitch *)
(*****************************************************************************)

let half_w = 440. (* the pitch, from the middle *)
let half_h = 690.
let goal_half = 110. (* the posts *)
let ball_r = 9.
let player_r = 13.

let in_play (x : number) (y : number) : bool = Float.abs x < half_w && Float.abs y < half_h

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type side = North | South (* which goal a team is shooting at *)

type player = {
  side : side;
  home : number * number; (* his place in the formation, as fractions of the pitch *)
  px : number;
  py : number;
  dir : number * number; (* where he faces, which is where he kicks *)
  touch : int; (* frames before he may touch the ball again *)
}

type ball = { bx : number; by : number; vx : number; vy : number; last : side option (* who touched it last *) }

type game = {
  players : player list;
  ball : ball;
  mine : int; (* which of my players I am running, an index into [players] *)
  power : number; (* the kick being charged, 0 to 1 *)
  bent : int; (* frames of aftertouch left on my kick *)
  south : int; (* the score *)
  north : int;
  clock : int; (* frames left *)
  message : (string * int) option;
  glued : bool; (* the flag: the ball carried, the way every other game did it *)
  kickoff : int; (* frames of the whistle before play *)
}

type scene = Title | Playing of game | Full_time of int * int
type model = { scenes : scene Scene2d.t }

let run_speed = 3.4
let keeper_speed = 2.6
let touch_speed = 4.8 (* the ball, pushed ahead: faster than a man can run *)
let friction = 0.985
let aftertouch = 0.22 (* how hard the arrows bend a ball in flight *)

(* The formation, as fractions of the half: the keeper on his line,
 * two backs, a midfielder, a forward. This is the whole of the team's
 * "tactics", and Kick Off's own tactics screens were this table with a
 * picture. *)
let formation : (number * number) list = [ (0., -0.95); (-0.5, -0.55); (0.5, -0.55); (0., -0.1); (0., 0.4) ]

let keeper (p : player) : bool = snd p.home < -0.9

let place (side : side) ((fx, fy) : number * number) : number * number =
  match side with South -> (fx * half_w * 0.8, fy * half_h) | North -> (0. - (fx * half_w * 0.8), 0. - (fy * half_h))

let team (side : side) : player list =
  List.map
    (fun home ->
      let px, py = place side home in
      { side; home; px; py; dir = (0., (match side with South -> 1. | North -> -1.)); touch = 0 })
    formation

let new_ball () : ball = { bx = 0.; by = 0.; vx = 0.; vy = 0.; last = None }

let new_game (glued : bool) : game =
  { players = team South @ team North; ball = new_ball (); mine = 4; power = 0.; bent = 0; south = 0; north = 0; clock = 60 *.. 120;
    message = Some ("KICK OFF", 90); glued; kickoff = 60 }

let initial_model = { scenes = Scene2d.start Title }

(*****************************************************************************)
(* The ball *)
(*****************************************************************************)

let goal_line (side : side) : number = match side with South -> half_h | North -> 0. - half_h

(* the ball rolls, losing a little speed to the grass; the arrows bend
 * it while the aftertouch lasts *)
let roll (computer : computer) (g : game) : ball =
  let b = g.ball in
  let ax, ay = if g.bent > 0 then to_xy computer.keyboard else (0., 0.) in
  let vx = (b.vx + (ax * aftertouch)) * friction and vy = (b.vy + (ay * aftertouch)) * friction in
  { b with bx = b.bx + vx; by = b.by + vy; vx; vy }

(* A touch: a player who reaches the ball pushes it ahead of him, in
 * the direction he is running. That is all "having the ball" is here;
 * with [glued] the ball is instead kept at his feet, which is the
 * other game. *)
let touched (g : game) (p : player) : ball option =
  let b = g.ball in
  (* the reach: a little more than the two radii, and it has to be more
   * than a running player covers in a frame -- at 3.4 pixels a frame a
   * reach of exactly the radii means a glued ball comes off his feet
   * on the second one *)
  if p.touch > 0 || Float.hypot (b.bx - p.px) (b.by - p.py) > player_r + ball_r + 12. then None
  else
    let dx, dy = p.dir in
    let d = Float.max 1e-9 (Float.hypot dx dy) in
    if g.glued then Some { bx = p.px + (dx / d * (player_r + ball_r)); by = p.py + (dy / d * (player_r + ball_r)); vx = 0.; vy = 0.; last = Some p.side }
    else Some { b with vx = touch_speed * dx / d; vy = touch_speed * dy / d; last = Some p.side }

(*****************************************************************************)
(* The team: a place, not a brain *)
(*****************************************************************************)

(* Where a player belongs *now*: his own spot in the formation, pulled
 * a third of the way towards the ball. The whole team slides up the
 * pitch when the ball does, keeping its shape, and nobody had to be
 * told. The keeper stays on his line and only moves across it. *)
let belongs (g : game) (p : player) : number * number =
  let hx, hy = place p.side p.home in
  if keeper p then (clamp (0. - goal_half) goal_half (g.ball.bx / 2.), hy)
  else (hx + ((g.ball.bx - hx) * 0.33), hy + ((g.ball.by - hy) * 0.33))

let nearest_to_ball (g : game) (side : side) : int =
  let ds =
    List.mapi (fun i (p : player) -> (i, p, Float.hypot (g.ball.bx -. p.px) (g.ball.by -. p.py))) g.players
    |> List.filter (fun (_, (p : player), _) -> p.side = side && not (keeper p))
  in
  match List.sort (fun (_, _, a) (_, _, b) -> compare a b) ds with (i, _, _) :: _ -> i | [] -> 0

(* one player, one frame, told where to go *)
let run_to (p : player) ((tx, ty) : number * number) (speed : number) : player =
  let dx = tx - p.px and dy = ty - p.py in
  let d = Float.hypot dx dy in
  if d < 2. then { p with touch = max 0 (p.touch -.. 1) }
  else
    let px = p.px + (speed * dx / d) and py = p.py + (speed * dy / d) in
    { p with px = clamp (0. - half_w - 30.) (half_w + 30.) px; py = clamp (0. - half_h - 30.) (half_h + 30.) py; dir = (dx / d, dy / d);
      touch = max 0 (p.touch -.. 1) }

(* the computer's players: the nearest to the ball goes for it (and,
 * once he has it, towards the goal he is shooting at), everyone else
 * walks to where they belong *)
let step_ai (g : game) (i : int) (p : player) : player =
  let chaser = nearest_to_ball g p.side in
  if keeper p then run_to p (belongs g p) keeper_speed
  else if i <> chaser then run_to p (belongs g p) (run_speed * 0.8)
  else
    let mine = Float.hypot (g.ball.bx -. p.px) (g.ball.by -. p.py) < 40. in
    if mine then run_to p (0., goal_line p.side) run_speed else run_to p (g.ball.bx, g.ball.by) run_speed

(*****************************************************************************)
(* Me *)
(*****************************************************************************)

(* The man I am running is the one of mine nearest the ball, which is
 * how Kick Off chose too: you do not pick a player, the ball does. *)
let switch_to_nearest (g : game) : game = { g with mine = nearest_to_ball g South }

let step_me (computer : computer) (g : game) : game =
  let me = List.nth g.players g.mine in
  let dx, dy = to_xy computer.keyboard in
  let me = if dx = 0. && dy = 0. then { me with touch = max 0 (me.touch -.. 1) } else run_to me (me.px + (dx * 40.), me.py + (dy * 40.)) run_speed in
  let players = List.mapi (fun i p -> if i = g.mine then me else p) g.players in
  { g with players }

(* the kick: space charges it, letting go sends the ball, and for the
 * next second the arrows bend it -- the aftertouch *)
let kick (computer : computer) (g : game) : game =
  let me = List.nth g.players g.mine in
  let has = Float.hypot (g.ball.bx -. me.px) (g.ball.by -. me.py) < player_r +. ball_r +. 8. in
  if computer.keyboard.kspace then { g with power = Float.min 1. (g.power + 0.04) }
  else if g.power = 0. || not has then { g with power = 0. }
  else begin
    Audio.play Audio.laser;
    let dx, dy = me.dir in
    let d = Float.max 1e-9 (Float.hypot dx dy) in
    let speed = 7. + (11. * g.power) in
    { g with power = 0.; bent = 70;
      players = List.mapi (fun i (p : player) -> if i = g.mine then { p with touch = 14 } else p) g.players;
      ball = { g.ball with vx = speed * dx / d; vy = speed * dy / d; last = Some South } }
  end

(*****************************************************************************)
(* The referee *)
(*****************************************************************************)

let say (g : game) (what : string) : game = { g with message = Some (what, 110) }

let restart (g : game) (at : number * number) (_ : side) : game =
  { g with ball = { (new_ball ()) with bx = fst at; by = snd at }; bent = 0; power = 0. }

(* a goal, or the ball out of play: the two things that stop a game *)
let referee (g : game) : game =
  let b = g.ball in
  if Float.abs b.by >= half_h && Float.abs b.bx < goal_half then
    (* through the posts *)
    let south_scored = b.by >= half_h in
    let g = if south_scored then { g with south = g.south +.. 1 } else { g with north = g.north +.. 1 } in
    Audio.play Audio.explosion;
    let g = say g (if south_scored then "GOAL!" else "GOAL AGAINST") in
    { (restart g (0., 0.) South) with players = team South @ team North; kickoff = 50 }
  else if in_play b.bx b.by then g
  else begin
    Audio.play Audio.blip;
    let other = match b.last with Some South -> North | _ -> South in
    let x = clamp (0. - half_w + 20.) (half_w - 20.) b.bx and y = clamp (0. - half_h + 20.) (half_h - 20.) b.by in
    say { (restart g (x, y) other) with ball = { (new_ball ()) with bx = x; by = y; last = Some other } } "THROW IN"
  end

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (g : game) : game =
  let g = { g with clock = max 0 (g.clock -.. 1); bent = max 0 (g.bent -.. 1) } in
  let g = match g.message with Some (_, 0) -> { g with message = None } | Some (w, n) -> { g with message = Some (w, n -.. 1) } | None -> g in
  if g.kickoff > 0 then { g with kickoff = g.kickoff -.. 1 }
  else
    let g = switch_to_nearest g in
    let g = step_me computer g in
    (* everyone but the man I am running, including the rest of my own
       side: in Kick Off you run one of them and trust the other nine *)
    let g = { g with players = List.mapi (fun i p -> if i = g.mine then p else step_ai g i p) g.players } in
    let g = kick computer g in
    let g = { g with ball = roll computer g } in
    (* whoever reaches it, touches it: mine ahead of him, theirs ahead
     * of them, and the ball is gone again *)
    let g =
      List.fold_left
        (fun g i ->
          match touched g (List.nth g.players i) with
          | None -> g
          | Some b ->
              (* a free ball is touched and then left alone for a few
               * frames, which is what lets it run ahead; a glued one is
               * put back at his feet every frame, or it would lag a
               * cooldown's worth behind him and come off *)
              let cool = if g.glued then 0 else 8 in
              { g with ball = b; players = List.mapi (fun j (p : player) -> if j = i then { p with touch = cool } else p) g.players })
        g
        (List.init (List.length g.players) (fun i -> i))
    in
    referee g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let go = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title ->
      if go then
        let glued = List.assoc_opt "ball" computer.flags = Some "glued" in
        { scenes = Scene2d.go (Playing (new_game glued)) scenes }
      else { scenes }
  | Playing g ->
      let g = update_game computer g in
      if g.clock = 0 then { scenes = Scene2d.go (Full_time (g.south, g.north)) scenes } else { scenes = { scenes with scene = Playing g } }
  | Full_time _ -> if go || scenes.elapsed > 12. then { scenes = Scene2d.go Title scenes } else { scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let grass = rgb 42 120 56
let grass2 = rgb 48 132 62
let line = rgb 235 245 235

(* the pitch: the stripes a mower leaves, the lines, the circle and the
 * goals, all of it drawn from the same two numbers as the rules use *)
let view_pitch : shape list =
  List.init 14 (fun i ->
      let h = 2. * half_h / 14. in
      rectangle (if i mod 2 = 0 then grass else grass2) (2. * half_w) h |> move_y (0. - half_h + (h / 2.) + (float_of_int i * h)))
  @ [ rectangle line (2. * half_w) 4. ;
      circle line 92. |> fade 0.35; circle grass 88.;
      rectangle line 4. (2. * half_h) |> fade 0.12 ]
  @ List.concat_map
      (fun (s : number) ->
        [ rectangle line (2. * half_w) 4. |> move_y (s * half_h);
          (* the box, and the goal itself *)
          rectangle line 340. 4. |> move_y (s * (half_h - 140.));
          rectangle line 4. 140. |> move (-170.) (s * (half_h - 70.));
          rectangle line 4. 140. |> move 170. (s * (half_h - 70.));
          rectangle (rgb 250 250 255) ((2. * goal_half) + 8.) 12. |> move_y (s * (half_h + 6.)) ])
      [ 1.; -1. ]

let shirt (side : side) : color = match side with South -> rgb 230 80 70 | North -> rgb 70 130 240

let view_player (g : game) (i : int) (p : player) : shape list =
  let dx, dy = p.dir in
  [ circle (if keeper p then rgb 240 220 90 else shirt p.side) player_r |> move p.px p.py;
    circle (rgb 250 220 180) 6. |> move (p.px + (dx * 3.)) (p.py + (dy * 3.)) ]
  @ if i = g.mine then [ circle white 4. |> move p.px (p.py + 22.); circle (shirt p.side) 2. |> move p.px (p.py + 22.) ] else []

let view_ball (b : ball) : shape list =
  [ circle (rgb 20 20 24) (ball_r + 2.) |> fade 0.25 |> move (b.bx + 3.) (b.by - 3.); circle white ball_r |> move b.bx b.by;
    circle (rgb 40 40 50) 3. |> move b.bx b.by ]

let view_world (g : game) : shape list = view_pitch @ List.concat (List.mapi (view_player g) g.players) @ view_ball g.ball

let view_hud (g : game) : shape list =
  let seconds = g.clock /.. 60 in
  [ rectangle (rgb 20 24 30) 1000. 64. |> move_y 468.;
    text (shirt South) 3. (Printf.sprintf "%d" g.south) |> move (-120.) 468.;
    text white 2.4 (Printf.sprintf "%d:%02d" (seconds /.. 60) (seconds mod 60)) |> move_y 468.;
    text (shirt North) 3. (Printf.sprintf "%d" g.north) |> move 120. 468.;
    text (rgb 150 160 170) 1.6 (if g.glued then "ball=glued" else "the ball is free") |> move 400. 468. ]
  (* the kick being charged, and the aftertouch while it lasts *)
  @ (if g.power > 0. then
       [ rectangle (rgb 60 60 70) 204. 16. |> move (-330.) (-460.); rectangle (rgb 250 220 90) (Float.max 2. (200. * g.power)) 12. |> move (-430. + (100. * g.power)) (-460.) ]
     else [])
  @ (if g.bent > 0 then [ text (rgb 250 220 90) 2. "AFTERTOUCH" |> move 330. (-460.) ] else [])
  (* low on the screen, where it does not sit over the goalmouth *)
  @ match g.message with Some (what, _) -> [ text white 4. what |> move_y (-300.) ] | None -> []

let view_title (scenes : scene Scene2d.t) : shape list =
  [ text (rgb 250 240 150) 6.5 "TINY KICK OFF 2" |> move_y 320.; text white 2.4 "the ball is not glued to your feet" |> move_y 250. ]
  @ List.concat
      (List.mapi
         (fun i ((key : string), (what : string)) ->
           let y = 110. - (float_of_int i * 56.) in
           [ text (rgb 120 220 255) 2.3 key |> move (-230.) y; text white 2.3 what |> move 120. y ])
         [ ("arrows", "run, and bend the ball after you kick it"); ("space", "tap to pass, hold to shoot");
           ("the ball", "runs ahead of you: catch it up"); ("flag ball=glued", "carried, the way the other games did it") ])
  @ [ text (rgb 160 170 180) 1.9 "you run whichever of your side is nearest the ball, as the arcade did" |> move_y (-180.) ]
  @ Scene2d.blink 1. scenes [ text (rgb 250 240 150) 3. "PRESS SPACE" |> move_y (-300.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  match scenes.scene with
  | Title -> rectangle (rgb 20 60 30) screen.width screen.height :: view_title scenes
  | Playing g ->
      let cam = Camera2d.origin |> Camera2d.look_at 0. (clamp (0. - half_h + 420.) (half_h - 420.) g.ball.by) in
      (rectangle (rgb 24 70 36) screen.width screen.height :: Camera2d.view cam (view_world g) :: view_hud g)
  | Full_time (south, north) ->
      [ rectangle (rgb 20 60 30) screen.width screen.height; text white 5. "FULL TIME" |> move_y 120.;
        text (shirt South) 6. (Printf.sprintf "%d" south) |> move (-90.) 0.; text white 4. "-" |> move_y 0.;
        text (shirt North) 6. (Printf.sprintf "%d" north) |> move 90. 0. ]
      @ Scene2d.blink 1. scenes [ text (rgb 250 240 150) 3. "PRESS SPACE" |> move_y (-180.) ]

let help =
  {|TinyKickOff2
  keys:  arrows  run; after a kick, they bend the ball (the aftertouch)
         space   tap to pass, hold to shoot
  flags: ball=glued  the ball carried at your feet, the way every other
                     football game of the time did it
  e.g.   dune exec games/TinyKickOff2.exe -- ball=glued
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
