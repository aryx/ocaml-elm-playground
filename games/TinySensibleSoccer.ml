(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Sensible Soccer (Sensible Software -- Jon Hare and Chris
 * Yates -- 1992), the third answer in games/ to the question the other
 * two ask: when a player runs into the ball, what happens? Arrows run,
 * space kicks (tap along the ground, hold to loft it), and the arrows
 * bend it while it is in the air.
 *
 * THE THREE ANSWERS, which is why this game is worth writing after the
 * other two rather than instead of either. All three use the same
 * kits/sports Free_ball, and what separates them is one number -- how
 * fast a touch sends the ball, against how fast a man runs:
 *
 *   game            touch   run    the ball, dribbling straight
 *   glued (the flag  --      --     22 px away: it is his feet
 *     in TinyKickOff2)
 *   Sensible         4.0    3.6     28 px ahead: a stride, and it
 *                                   stays there -- close control
 *   Kick Off         4.8    3.4     48 px ahead: a chase you are
 *     (TinyKickOff2)                only just winning
 *   Speedball        --      --     in his hands: he carries it
 *     (TinySpeedball2)
 *
 * Sensible sits deliberately between the other two: the ball is not
 * yours, but it is never far. That is the whole feel of the game, and
 * it is one constant.
 *
 * WHAT IS ITS OWN, and is the reason it is not just a slower Kick Off:
 *
 * 1. THE BALL HAS A HEIGHT. Hold the kick and it goes up: a z, a
 *    shadow on the grass under it, and a bounce when it lands. While
 *    it is up, nobody can touch it but a head (a player under a ball
 *    below head height), which is what makes the lofted through-ball
 *    and the volley. The shadow is the whole of the 3D here -- this
 *    game belongs in games/ and not in games2.5d/, whose games fake a
 *    3D *view* (a raycaster, Mode 7, voxels); a height and a shadow
 *    are a coordinate, not a trick of rendering:
 *
 *        o          the ball, high            drawn: the ball, and
 *       ---                                   its shadow apart
 *        .          its shadow, on the grass
 *
 * 2. THE VIEW IS PULLED BACK. Sensible's players are the smallest of
 *    the three and you see the most pitch: the shape of the play
 *    rather than the man you are running. Speedball is nailed in
 *    close on the ball, Kick Off shows the width of the pitch and
 *    scrolls up and down; this one shows nearly all of it, and the
 *    camera barely moves.
 *
 * 3. AFTERTOUCH IS THE GAME. Kick Off had it first, but Sensible made
 *    it the thing everybody remembers: a lofted ball bends much more
 *    than one on the ground, which is why its shots curl into the top
 *    corner and its crosses hang. Measured here (tests/games/), over
 *    the same 45 frames: a lofted shot held right ends up 321 pixels
 *    to the side of the one left alone, a tap along the grass 163.
 *
 * What it uses: kits/sports (Free_ball for the ball on the grass and
 * the touch, Formation for the shape of a side -- its third user,
 * after TinyKickOff2 and TinySpeedball2), Camera2d, Scene2d, Audio.
 * The height is the game's own: the kit's ball is flat, and a second
 * game wanting a z is what would move it there.
 *
 * Exercises: headers that aim (Sensible's were a direction and a
 * jump); the keeper who comes for a cross; a tackle that takes the
 * ball rather than the man; the famous editor, which was the game's
 * other half -- a team as data; and a throw-in taken by hand.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The pitch *)
(*****************************************************************************)

let half_w = 520.
let half_h = 760.
let goal_half = 120.
let ball_r = 7.
let player_r = 10.

(* the numbers of this game, next to the other two (see the header) *)
let run_speed = 3.6
let touch_speed = 4.0
let reach = player_r + ball_r + 10.
let friction = 0.987
let gravity = 0.42 (* what brings a lofted ball down, per frame *)
let aftertouch_ground = 0.2
let aftertouch_air = 0.34 (* a ball in the air bends much more: the game's signature *)

let in_play (x : number) (y : number) : bool = Float.abs x < half_w && Float.abs y < half_h

type side = Home | Away

let goal_line (s : side) : number = match s with Home -> half_h | Away -> 0. - half_h

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type player = { side : side; home : Formation.spot; px : number; py : number; dir : number * number; touch : int }

type game = {
  players : player list;
  ball : Free_ball.t;
  z : number; (* how high the ball is off the grass *)
  vz : number;
  last : side option;
  mine : int;
  power : number;
  bent : int; (* frames of aftertouch left on my kick *)
  home : int;
  away : int;
  clock : int;
  message : (string * int) option;
  kickoff : int;
}

type scene = Title | Playing of game | Full_time of int * int
type model = { scenes : scene Scene2d.t }

(* four at the back is too many for five a side: a keeper, two, and two *)
let formation : Formation.spot list = [ (0., -0.95); (-0.45, -0.55); (0.45, -0.55); (-0.2, 0.05); (0.35, 0.35) ]

let keeper (p : player) : bool = snd p.home < -0.9
let place (s : side) (spot : Formation.spot) : number * number = Formation.at ~half_w:(half_w * 0.82) ~half_h ~up:(s = Home) spot

let team (s : side) : player list =
  List.map
    (fun home ->
      let px, py = place s home in
      { side = s; home; px; py; dir = (0., (match s with Home -> 1. | Away -> -1.)); touch = 0 })
    formation

let new_game () : game =
  { players = team Home @ team Away; ball = Free_ball.still 0. 0.; z = 0.; vz = 0.; last = None; mine = 4; power = 0.; bent = 0; home = 0;
    away = 0; clock = 60 *.. 120; message = Some ("KICK OFF", 80); kickoff = 50 }

let initial_model = { scenes = Scene2d.start Title }

(*****************************************************************************)
(* The ball, on the grass and above it *)
(*****************************************************************************)

let head_height = 34. (* a ball higher than this is out of everyone's reach *)

(* one frame: along the grass with the kit's ball, and up and down
 * with a gravity of its own. A ball that lands keeps half its height,
 * which is what a football does on grass. *)
let fly (computer : computer) (g : game) : game =
  let ax, ay = if g.bent > 0 then to_xy computer.keyboard else (0., 0.) in
  let bend = if g.z > 1. then aftertouch_air else aftertouch_ground in
  let ball = Free_ball.roll ~friction:(if g.z > 1. then 0.997 else friction) ~push:(ax * bend, ay * bend) g.ball in
  let z = g.z + g.vz and vz = g.vz - gravity in
  if z > 0. then { g with ball; z; vz }
  else begin
    if g.vz < -3. then Audio.play Audio.step;
    { g with ball; z = 0.; vz = (if Float.abs vz < 2. then 0. else Float.abs vz * 0.5) }
  end

(* Who may touch it: a ball on the grass, anyone near it; a ball in the
 * air, only a head, and only under head height. Above that it sails
 * over everybody, which is what a lofted pass is for. *)
let touchable (g : game) : bool = g.z < head_height

let touched (g : game) (p : player) : Free_ball.t option =
  if p.touch > 0 || not (touchable g) then None
  else Free_ball.touch ~glued:false ~speed:touch_speed ~reach ~hold:(player_r + ball_r) (p.px, p.py) p.dir g.ball

(*****************************************************************************)
(* The players *)
(*****************************************************************************)

let run_to (p : player) (target : number * number) (speed : number) : player =
  let (px, py), (dx, dy) = Formation.run_to ~speed ~bounds:(half_w + 20., half_h + 20.) (p.px, p.py) target in
  let p = { p with px; py; touch = max 0 (p.touch -.. 1) } in
  if dx = 0. && dy = 0. then p else { p with dir = (dx, dy) }

let nearest_of (g : game) (s : side) : int =
  match Formation.nearest (fun (p : player) -> (p.px, p.py)) (fun (p : player) -> p.side = s && not (keeper p)) (g.ball.x, g.ball.y) g.players with
  | Some i -> i
  | None -> 0

let belongs (g : game) (p : player) : number * number =
  let hx, hy = place p.side p.home in
  if keeper p then (clamp (0. - goal_half) goal_half (g.ball.x / 2.), hy) else Formation.belongs ~pull:0.3 ~home:(hx, hy) ~ball:(g.ball.x, g.ball.y)

let step_ai (g : game) (i : int) (p : player) : player =
  if keeper p then run_to p (belongs g p) (run_speed * 0.8)
  else if i <> nearest_of g p.side then run_to p (belongs g p) (run_speed * 0.85)
  else if Free_ball.near (reach * 0.8) (p.px, p.py) g.ball then run_to p (0., goal_line p.side) run_speed
  else run_to p (g.ball.x, g.ball.y) run_speed

let step_me (computer : computer) (g : game) : game =
  let me = List.nth g.players g.mine in
  let dx, dy = to_xy computer.keyboard in
  let me = if dx = 0. && dy = 0. then { me with touch = max 0 (me.touch -.. 1) } else run_to me (me.px + (dx * 40.), me.py + (dy * 40.)) run_speed in
  { g with players = List.mapi (fun i p -> if i = g.mine then me else p) g.players }

(* The kick: a tap goes along the grass, a held one goes up. The longer
 * it is held the higher and further it goes, and the aftertouch has
 * that much longer to work on it -- which is why Sensible's shots
 * curl and its taps do not. *)
let kick (computer : computer) (g : game) : game =
  let me = List.nth g.players g.mine in
  let has = Free_ball.near (reach + 6.) (me.px, me.py) g.ball && g.z < head_height in
  if computer.keyboard.kspace && has then { g with power = Float.min 1. (g.power + 0.045) }
  else if g.power = 0. || not has then { g with power = 0. }
  else begin
    Audio.play Audio.laser;
    let dx, dy = me.dir in
    let d = Float.max 1e-9 (Float.hypot dx dy) in
    let speed = 6.5 + (9. * g.power) in
    { g with power = 0.; bent = 80; last = Some me.side;
      z = (if g.power > 0.35 then 1. else g.z);
      vz = (if g.power > 0.35 then 3.5 + (5.5 * g.power) else g.vz);
      players = List.mapi (fun i (p : player) -> if i = g.mine then { p with touch = 12 } else p) g.players;
      ball = { g.ball with vx = speed * dx / d; vy = speed * dy / d } }
  end

(*****************************************************************************)
(* The referee *)
(*****************************************************************************)

let say (g : game) (what : string) : game = { g with message = Some (what, 100) }

let referee (g : game) : game =
  let b = g.ball in
  if Float.abs b.y >= half_h && Float.abs b.x < goal_half && g.z < 60. then begin
    Audio.play Audio.explosion;
    let home_scored = b.y >= half_h in
    let g = if home_scored then { g with home = g.home +.. 1 } else { g with away = g.away +.. 1 } in
    { (say g (if home_scored then "GOAL!" else "GOAL AGAINST")) with ball = Free_ball.still 0. 0.; z = 0.; vz = 0.; bent = 0; power = 0.;
      players = team Home @ team Away; kickoff = 45 }
  end
  else if in_play b.x b.y then g
  else begin
    Audio.play Audio.blip;
    let other = match g.last with Some Home -> Away | _ -> Home in
    let x = clamp (0. - half_w + 16.) (half_w - 16.) b.x and y = clamp (0. - half_h + 16.) (half_h - 16.) b.y in
    say { g with ball = Free_ball.still x y; z = 0.; vz = 0.; bent = 0; power = 0.; last = Some other } "THROW IN"
  end

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (g : game) : game =
  let g = { g with clock = max 0 (g.clock -.. 1); bent = max 0 (g.bent -.. 1) } in
  let g = match g.message with Some (_, 0) -> { g with message = None } | Some (w, n) -> { g with message = Some (w, n -.. 1) } | None -> g in
  if g.kickoff > 0 then { g with kickoff = g.kickoff -.. 1 }
  else
    let g = { g with mine = nearest_of g Home } in
    let g = step_me computer g in
    let g = { g with players = List.mapi (fun i p -> if i = g.mine then p else step_ai g i p) g.players } in
    let g = kick computer g in
    let g = fly computer g in
    (* one touch a frame, by whoever is nearest and can reach it *)
    let g =
      match Formation.nearest (fun (p : player) -> (p.px, p.py)) (fun (p : player) -> p.touch = 0) (g.ball.x, g.ball.y) g.players with
      | None -> g
      | Some i -> (
          let p = List.nth g.players i in
          match touched g p with
          | None -> g
          | Some ball ->
              (* a ball taken out of the air comes down where it is met:
               * that is a header *)
              let headed = g.z > 6. in
              if headed then Audio.play Audio.hit;
              { g with ball; z = (if headed then g.z else 0.); vz = (if headed then 0. - Float.abs g.vz else 0.); last = Some p.side;
                players = List.mapi (fun j (q : player) -> if j = i then { q with touch = 8 } else q) g.players })
    in
    referee g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let go = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if go then { scenes = Scene2d.go (Playing (new_game ())) scenes } else { scenes }
  | Playing g ->
      let g = update_game computer g in
      if g.clock = 0 then { scenes = Scene2d.go (Full_time (g.home, g.away)) scenes } else { scenes = { scenes with scene = Playing g } }
  | Full_time _ -> if go || scenes.elapsed > 12. then { scenes = Scene2d.go Title scenes } else { scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let grass = rgb 46 126 58
let grass2 = rgb 52 138 66
let line = rgb 236 246 236

let view_pitch : shape list =
  List.init 16 (fun i ->
      let h = 2. * half_h / 16. in
      rectangle (if i mod 2 = 0 then grass else grass2) (2. * half_w) h |> move_y (0. - half_h + (h / 2.) + (float_of_int i * h)))
  @ [ rectangle line (2. * half_w) 3.; circle line 84. |> fade 0.4; circle grass 81.;
      rectangle line 3. (2. * half_h) |> move_x (0. - half_w); rectangle line 3. (2. * half_h) |> move_x half_w ]
  @ List.concat_map
      (fun (s : number) ->
        [ rectangle line (2. * half_w) 3. |> move_y (s * half_h);
          rectangle line 400. 3. |> move_y (s * (half_h - 150.));
          rectangle line 3. 150. |> move (-200.) (s * (half_h - 75.));
          rectangle line 3. 150. |> move 200. (s * (half_h - 75.));
          rectangle (rgb 250 250 255) ((2. * goal_half) + 10.) 10. |> move_y (s * (half_h + 5.)) ])
      [ 1.; -1. ]

let shirt (s : side) : color = match s with Home -> rgb 240 240 245 | Away -> rgb 40 70 190

(* Sensible's men are tiny: a shirt, shorts and a head, four pixels
 * each, and you see half the pitch at once *)
let view_player (g : game) (i : int) (p : player) : shape list =
  let dx, dy = p.dir in
  [ rectangle (shirt p.side) 15. 15. |> move p.px p.py;
    rectangle (if p.side = Home then rgb 30 30 40 else rgb 240 240 245) 13. 6. |> move p.px (p.py - 8.);
    circle (rgb 250 220 180) 6. |> move (p.px + (dx * 3.)) (p.py + (dy * 3.)) ]
  @ if i = g.mine then [ circle (rgb 250 240 120) 4. |> move p.px (p.py + 16.) ] else []

(* the ball and, when it is up, its shadow on the grass under it: the
 * only 3D in the game, and all it needs *)
let view_ball (g : game) : shape list =
  let b = g.ball in
  [ oval (rgb 20 50 24) (ball_r * 2.) (ball_r * 1.4) |> fade (0.45 - (Float.min 0.3 (g.z / 300.))) |> move b.x b.y ]
  @ [ circle white (ball_r + 2. + (g.z / 40.)) |> move b.x (b.y + (g.z * 0.6)) ]

let view_world (g : game) : shape list = view_pitch @ List.concat (List.mapi (view_player g) g.players) @ view_ball g

let view_hud (g : game) : shape list =
  let seconds = g.clock /.. 60 in
  [ rectangle (rgb 18 24 20) 1000. 58. |> move_y 470.;
    text (shirt Home) 2.8 (Printf.sprintf "%d" g.home) |> move (-110.) 470.;
    text white 2.2 (Printf.sprintf "%d:%02d" (seconds /.. 60) (seconds mod 60)) |> move_y 470.;
    text (shirt Away) 2.8 (Printf.sprintf "%d" g.away) |> move 110. 470.;
    text (rgb 150 170 155) 1.5 "tap: pass   hold: loft" |> move 400. 470. ]
  @ (if g.power > 0. then
       [ rectangle (rgb 50 60 50) 164. 12. |> move (-360.) (-468.);
         rectangle (if g.power > 0.35 then rgb 250 230 120 else rgb 190 200 190) (Float.max 2. (160. * g.power)) 8. |> move (-440. + (80. * g.power)) (-468.) ]
     else [])
  @ (if g.bent > 0 then [ text (rgb 250 240 150) 1.8 "AFTERTOUCH" |> move 360. (-468.) ] else [])
  @ match g.message with Some (what, _) -> [ text white 3.4 what |> move_y (-330.) ] | None -> []

let view_title (scenes : scene Scene2d.t) : shape list =
  [ text (rgb 240 240 245) 5.5 "TINY SENSIBLE SOCCER" |> move_y 330.; text (rgb 190 230 190) 2.2 "the ball a stride ahead, and the sky to put it in" |> move_y 262. ]
  @ List.concat
      (List.mapi
         (fun i ((key : string), (what : string)) ->
           let y = 130. - (float_of_int i * 52.) in
           [ text (rgb 250 240 150) 2.1 key |> move (-250.) y; text white 2.1 what |> move 110. y ])
         [ ("arrows", "run; in the air, they bend the ball"); ("tap space", "a pass along the grass");
           ("hold space", "a lofted ball: it goes up, and bends twice as much");
           ("a header", "meet a ball below head height and it goes on") ])
  @ [ text (rgb 170 200 175) 1.8 "the third answer: the ball is 28 pixels ahead of you, and stays there" |> move_y (-130.);
      text (rgb 140 170 145) 1.7 "(TinyKickOff2 lets it get 48 ahead; TinySpeedball2 puts it in your hands)" |> move_y (-175.) ]
  @ Scene2d.blink 1. scenes [ text (rgb 250 240 150) 3. "PRESS SPACE" |> move_y (-290.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  match scenes.scene with
  | Title -> rectangle (rgb 24 60 32) screen.width screen.height :: view_title scenes
  | Playing g ->
      (* pulled back: nearly the whole pitch at once, and the camera
       * barely moves -- the opposite of TinySpeedball2's close-in view *)
      let cam = { Camera2d.origin with zoom = 0.62 } |> Camera2d.look_at 0. (clamp (-180.) 180. (g.ball.y / 2.)) in
      (rectangle (rgb 24 60 32) screen.width screen.height :: Camera2d.view cam (view_world g) :: view_hud g)
  | Full_time (home, away) ->
      [ rectangle (rgb 24 60 32) screen.width screen.height; text white 5. "FULL TIME" |> move_y 120.;
        text (shirt Home) 6. (Printf.sprintf "%d" home) |> move (-90.) 0.; text white 4. "-" |> move_y 0.;
        text (shirt Away) 6. (Printf.sprintf "%d" away) |> move 90. 0. ]
      @ Scene2d.blink 1. scenes [ text (rgb 250 240 150) 3. "PRESS SPACE" |> move_y (-160.) ]

let help =
  {|TinySensibleSoccer
  keys:  arrows  run; while the ball is in the air, bend it
         space   tap for a pass along the grass, hold to loft it
  the ball is never yours, but it is never far: 28 pixels ahead,
  against TinyKickOff2's 48 and TinySpeedball2's nought (carried).
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
