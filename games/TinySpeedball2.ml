(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Speedball 2: Brutal Deluxe (The Bitmap Brothers, 1990), the
 * sport that is handball, ice hockey and a fist fight played on a
 * sheet of metal. Arrows run, space throws the ball (tap to pass, hold
 * to shoot) and, when you have not got it, tackles whoever you run
 * into. The clock runs for ninety seconds and nothing stops it: there
 * is no out of play, because the arena has walls.
 *
 * It shares its bones with games/TinyKickOff2 -- both are the sports
 * kit (kits/sports/): the ball as a thing with a speed and a friction
 * and walls (Free_ball), and a side that keeps its shape because every
 * player has a place in it (Formation). Writing the second game is
 * what turned those into a kit; the first game had them inline, as it
 * should have.
 *
 * But the ball is the opposite of Kick Off's, and that is the first
 * thing to say about this game:
 *
 * 1. THE BALL IS CARRIED. This is handball, not football. Run near it
 *    and you simply *have* it -- there is no catch button, and no
 *    chasing it either -- and you keep it, running with it, until you
 *    throw it or somebody knocks it out of you:
 *
 *      Kick Off              Speedball
 *      p   o- - ->           p(o) ----->     you have it; the arena
 *      the ball runs on      it runs with you, and only what you
 *      and you chase it      throw is loose again
 *
 *    A carried ball does not roll, does not bounce off the walls and
 *    does not score off the furniture. To use the arena you have to
 *    let go of it, which is the whole of the game's tension.
 *
 * 2. THE ARENA SCORES. A goal is 10 points, and so are two hits on a
 *    bounce dome; the stars are 5 and light the ball up, the two
 *    multiplier plates double everything you score for ten seconds,
 *    and flattening an opponent is 10. So the thing on the screen is
 *    not a pitch with a goal at each end, it is a *table*, and a
 *    thrown ball rattling around the furniture is worth as much as the
 *    ball in the net. It is TinyPinball's table with players on it.
 *
 * 3. THE LOOSE BALL NEVER STOPS. No touchlines, no throw-ins, no
 *    referee: the walls give it back, keeping four fifths of its speed
 *    (Free_ball.bounce_in), and a shot that misses comes back at you
 *    off the end wall. The mouth at each end is a gap in the wall,
 *    which is what a goal is here.
 *
 * 4. VIOLENCE IS A MOVE, not a foul. Space with no ball is a tackle:
 *    whoever you catch goes down for two seconds, you score for it,
 *    and if he was carrying the ball it comes out of his hands.
 *    Speedball's subtitle was Brutal Deluxe, and its rule was that
 *    there are no rules.
 *
 * The view is the other difference: close in (625 pixels of a 760 by
 * 1320 arena) and nailed to the ball, which is in somebody's hands
 * most of the time. Kick Off's camera follows the ball up and down a
 * pitch you can see the width of; this one scrolls in both directions
 * and shows you a corner of the arena at a time.
 *
 * What it uses: kits/sports (above), Camera2d (the arena is taller
 * than the screen), Scene2d, Audio. Not Physics: a heavy ball on metal
 * is a velocity, a friction and a wall, and the domes are two lines of
 * reflection -- see games/TinyPinball.ml, which does the same thing
 * with the whole of its table.
 *
 * Exercises: the ice cream vendor (the sample everyone remembers); the
 * warp tubes that swallow the ball and spit it out at the other end
 * (games/TinyPortal2D's transform, on a ball); the management between
 * matches, which was half the game -- buying players, and spending the
 * points you just scored; the armour and the injuries; and two
 * players, since this is a game about what you do to each other.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The arena *)
(*****************************************************************************)

let half_w = 380.
let half_h = 660.
let goal_half = 90. (* the mouth at each end *)
let ball_r = 11.
let player_r = 14.
let wall_keep = 0.8 (* a heavy ball on metal loses a fifth of its speed *)

(* the view is close in: 625 pixels of a 760 by 1320 arena, so it
 * scrolls in both directions, and the ball is always in the middle of
 * it *)
let zoom = 1.6

type side = Red | Blue

let goal_line (s : side) : number = match s with Red -> half_h | Blue -> 0. - half_h

(* The furniture, which is where the points are. A dome bounces the
 * ball and scores; a star scores more and lights the ball; a plate
 * doubles what its toucher scores for a while. *)
type thing = Dome | Star | Plate

type fixture = { what : thing; fx : number; fy : number; lit : int }

let fixtures : fixture list =
  [ { what = Dome; fx = -190.; fy = 250.; lit = 0 };
    { what = Dome; fx = 190.; fy = 250.; lit = 0 };
    { what = Dome; fx = -190.; fy = -250.; lit = 0 };
    { what = Dome; fx = 190.; fy = -250.; lit = 0 };
    { what = Star; fx = 0.; fy = 420.; lit = 0 };
    { what = Star; fx = 0.; fy = -420.; lit = 0 };
    { what = Plate; fx = -250.; fy = 0.; lit = 0 };
    { what = Plate; fx = 250.; fy = 0.; lit = 0 } ]

let radius_of (t : thing) : number = match t with Dome -> 26. | Star -> 20. | Plate -> 24.
let points_of (t : thing) : int = match t with Dome -> 5 | Star -> 5 | Plate -> 2

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type player = {
  side : side;
  home : Formation.spot;
  px : number;
  py : number;
  dir : number * number;
  touch : int; (* frames before he may touch the ball again *)
  down : int; (* frames spent on the floor, tackled *)
}

type game = {
  players : player list;
  ball : Free_ball.t;
  (* who is holding it. This is handball, not football: run near the
   * ball and you have it, and you keep it until you throw it or
   * somebody takes it off you. *)
  carrier : int option;
  last : side option; (* who touched it last: who the arena pays *)
  electric : int; (* frames the ball stays lit, from a star *)
  mine : int;
  power : number;
  fixtures : fixture list;
  double : (side * int) list; (* a side, and the frames its score is doubled for *)
  red : int;
  blue : int;
  clock : int;
  message : (string * int) option;
  restarting : int;
}

type scene = Title | Playing of game | Full_time of int * int
type model = { scenes : scene Scene2d.t }

let run_speed = 3.6
let touch_speed = 5.2 (* the ball is heavy and goes further ahead than a football *)
let friction = 0.99 (* metal, not grass *)
let reach = player_r + ball_r + 12.

(* three at the back, two up: a nine-a-side game played five a side *)
let formation : Formation.spot list = [ (0., -0.95); (-0.55, -0.5); (0.55, -0.5); (-0.3, 0.2); (0.4, 0.45) ]

let keeper (p : player) : bool = snd p.home < -0.9
let place (s : side) (spot : Formation.spot) : number * number = Formation.at ~half_w:(half_w * 0.8) ~half_h ~up:(s = Red) spot

let team (s : side) : player list =
  List.map
    (fun home ->
      let px, py = place s home in
      { side = s; home; px; py; dir = (0., (match s with Red -> 1. | Blue -> -1.)); touch = 0; down = 0 })
    formation

let new_game () : game =
  { players = team Red @ team Blue; ball = Free_ball.still 0. 0.; carrier = None; last = None; electric = 0; mine = 4; power = 0.; fixtures;
    double = []; red = 0; blue = 0; clock = 60 *.. 90; message = Some ("SPEEDBALL", 90); restarting = 50 }

let initial_model = { scenes = Scene2d.start Title }

(*****************************************************************************)
(* Scoring: the arena pays *)
(*****************************************************************************)

let say (g : game) (what : string) : game = { g with message = Some (what, 100) }

(* whatever a side scores is doubled while its plate is lit: the
 * multiplier is the reason to go and touch a wall in the middle of a
 * game about scoring goals *)
let score (g : game) (s : side) (points : int) : game =
  let doubled = match List.assoc_opt s g.double with Some n when n > 0 -> 2 | _ -> 1 in
  let points = points *.. doubled in
  match s with Red -> { g with red = g.red +.. points } | Blue -> { g with blue = g.blue +.. points }

(*****************************************************************************)
(* The ball against the furniture *)
(*****************************************************************************)

(* A dome, a star or a plate is a circle the ball bounces off, exactly
 * as a pinball bumper is (games/TinyPinball.ml): push the ball out
 * along the normal, reflect its speed about it, and pay whoever
 * touched it last. *)
let hit_fixtures (g : game) : game =
  List.fold_left
    (fun g (f : fixture) ->
      let b = g.ball in
      let r = radius_of f.what + ball_r in
      let dx = b.x - f.fx and dy = b.y - f.fy in
      let d = Float.hypot dx dy in
      if d >= r then { g with fixtures = { f with lit = max 0 (f.lit -.. 1) } :: g.fixtures }
      else
        let nx = if d < 1e-6 then 0. else dx / d and ny = if d < 1e-6 then 1. else dy / d in
        let vn = (b.vx * nx) + (b.vy * ny) in
        let ball =
          { Free_ball.x = f.fx + (nx * r); y = f.fy + (ny * r);
            vx = ((b.vx - (2. * vn * nx)) * 1.05) + (nx * 1.5); vy = ((b.vy - (2. * vn * ny)) * 1.05) + (ny * 1.5) }
        in
        Audio.play Audio.blip;
        let g = { g with ball; fixtures = { f with lit = 25 } :: g.fixtures } in
        let g = match g.last with Some s -> score g s (points_of f.what) | None -> g in
        match (f.what, g.last) with
        | Star, _ -> say { g with electric = 180 } "ELECTRO STAR"
        | Plate, Some s -> say { g with double = (s, 600) :: List.remove_assoc s g.double } "SCORE DOUBLED"
        | _ -> g)
    { g with fixtures = [] } g.fixtures

(* The walls, and the one gap in them. The sides always send the ball
 * back; the ends do too, except across the mouth, where there is no
 * wall at all -- which is what a goal is. (Bouncing the ball off the
 * whole end wall, as the first version did, makes a game in which no
 * goal can ever be scored.) *)
let walls (b : Free_ball.t) : Free_ball.t =
  let b = Free_ball.bounce_in ~half_w:(half_w - ball_r) ~half_h:infinity ~keep:wall_keep b in
  if Float.abs b.x <= goal_half then b else Free_ball.bounce_in ~half_w:infinity ~half_h:(half_h - ball_r) ~keep:wall_keep b

(* the mouth at each end: through it is ten points, and the ball goes
 * back to the middle, which is the only time this game stops *)
let goals (g : game) : game =
  let b = g.ball in
  (* the mouth is a pocket, not a line: a ball that has reached the end
   * wall between the posts is in, even if it got there sideways --
   * which, in a game where players shove it along the wall, is most of
   * them *)
  if Float.abs b.y < half_h - 24. || Float.abs b.x > goal_half then g
  else
    let scorer = if b.y > 0. then Red else Blue in
    Audio.play Audio.explosion;
    let g = score g scorer 10 in
    { (say g "GOAL -- TEN POINTS") with ball = Free_ball.still 0. 0.; carrier = None; restarting = 50; players = team Red @ team Blue }

(*****************************************************************************)
(* The players *)
(*****************************************************************************)

let run_to (p : player) (target : number * number) (speed : number) : player =
  let (px, py), (dx, dy) = Formation.run_to ~speed ~bounds:(half_w - player_r, half_h - player_r) (p.px, p.py) target in
  let p = { p with px; py; touch = max 0 (p.touch -.. 1); down = max 0 (p.down -.. 1) } in
  if dx = 0. && dy = 0. then p else { p with dir = (dx, dy) }

let nearest_of (g : game) (s : side) : int =
  match
    Formation.nearest (fun (p : player) -> (p.px, p.py)) (fun (p : player) -> p.side = s && (not (keeper p)) && p.down = 0) (g.ball.x, g.ball.y) g.players
  with
  | Some i -> i
  | None -> 0

let belongs (g : game) (p : player) : number * number =
  let hx, hy = place p.side p.home in
  if keeper p then (clamp (0. - goal_half) goal_half (g.ball.x / 2.), hy) else Formation.belongs ~pull:0.4 ~home:(hx, hy) ~ball:(g.ball.x, g.ball.y)

let step_ai (g : game) (i : int) (p : player) : player =
  if p.down > 0 then { p with down = p.down -.. 1 }
  else if g.carrier = Some i then
    (* he has the ball in his hands: he runs it at the mouth *)
    run_to p (0., goal_line p.side) run_speed
  else if keeper p then run_to p (belongs g p) (run_speed * 0.7)
  else if i <> nearest_of g p.side then run_to p (belongs g p) (run_speed * 0.85)
  else
    (* after the man with it, or after the loose ball *)
    match g.carrier with
    | Some c when (List.nth g.players c).side <> p.side ->
        let o = List.nth g.players c in
        run_to p (o.px, o.py) run_speed
    | _ -> run_to p (g.ball.x, g.ball.y) run_speed

(*****************************************************************************)
(* Me: throwing, and the tackle *)
(*****************************************************************************)

let step_me (computer : computer) (g : game) : game =
  let me = List.nth g.players g.mine in
  let dx, dy = to_xy computer.keyboard in
  let me = if me.down > 0 then { me with down = me.down -.. 1 } else if dx = 0. && dy = 0. then { me with touch = max 0 (me.touch -.. 1) } else run_to me (me.px + (dx * 40.), me.py + (dy * 40.)) run_speed in
  { g with players = List.mapi (fun i p -> if i = g.mine then me else p) g.players }

(* Space does one of two things, and which one is not a choice: with
 * the ball it throws, without it it tackles. Speedball's whole
 * defence is the second one. *)
let throw_or_tackle (computer : computer) (g : game) : game =
  let me = List.nth g.players g.mine in
  let has = g.carrier = Some g.mine in
  if computer.keyboard.kspace && has then { g with power = Float.min 1. (g.power + 0.05) }
  else if computer.keyboard.kspace && not has then
    (* the tackle: whoever is within reach goes down, it pays ten, and
     * if he was the one carrying the ball it comes out of his hands *)
    match List.find_opt (fun (p : player) -> p.side <> me.side && p.down = 0 && Float.hypot (p.px -. me.px) (p.py -. me.py) < 36.) g.players with
    | None -> g
    | Some victim ->
        Audio.play Audio.hit;
        let hit = List.mapi (fun i (p : player) -> (i, p)) g.players |> List.find (fun (_, p) -> p == victim) |> fst in
        let g = score g me.side 10 in
        let g =
          if g.carrier <> Some hit then g
          else
            (* knocked loose, away from the man who hit him *)
            let dx = victim.px -. me.px and dy = victim.py -. me.py in
            let d = Float.max 1e-9 (Float.hypot dx dy) in
            { g with carrier = None; ball = { (Free_ball.still victim.px victim.py) with vx = 6. * dx / d; vy = 6. * dy / d } }
        in
        (* the man who hit him cannot simply take it out of the air: he
         * is held off for a moment, and the ball squirts loose, which
         * is what a tackle looks like *)
        say
          { g with
            players =
              List.mapi
                (fun i (p : player) -> if p == victim then { p with down = 120 } else if i = g.mine then { p with touch = 12 } else p)
                g.players }
          "TACKLED"
  else if g.power = 0. || not has then { g with power = 0. }
  else begin
    Audio.play Audio.laser;
    let dx, dy = me.dir in
    let d = Float.max 1e-9 (Float.hypot dx dy) in
    let speed = 8. + (12. * g.power) in
    { g with power = 0.; carrier = None; last = Some me.side;
      players = List.mapi (fun i (p : player) -> if i = g.mine then { p with touch = 14 } else p) g.players;
      ball = { g.ball with vx = speed * dx / d; vy = speed * dy / d } }
  end

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (g : game) : game =
  let g = { g with clock = max 0 (g.clock -.. 1); electric = max 0 (g.electric -.. 1) } in
  let g = { g with double = List.filter_map (fun (s, n) -> if n > 1 then Some (s, n -.. 1) else None) g.double } in
  let g = match g.message with Some (_, 0) -> { g with message = None } | Some (w, n) -> { g with message = Some (w, n -.. 1) } | None -> g in
  if g.restarting > 0 then { g with restarting = g.restarting -.. 1 }
  else
    let g = { g with mine = nearest_of g Red } in
    let g = step_me computer g in
    let g = { g with players = List.mapi (fun i p -> if i = g.mine then p else step_ai g i p) g.players } in
    let g = throw_or_tackle computer g in
    (* The ball is either in somebody's hands or loose on the metal,
     * and that is the whole difference with games/TinyKickOff2. There
     * the ball is never yours: a touch pushes it ahead of you and you
     * chase it. Here running near it *is* catching it -- no button,
     * you simply have it -- and you keep it, running with it, until
     * you throw it or somebody knocks it out of you. A carried ball
     * does not roll, does not bounce off the walls and does not score
     * off the furniture: to use the arena you have to let go of it. *)
    let g =
      match g.carrier with
      | Some i when (List.nth g.players i).down = 0 ->
          let p = List.nth g.players i in
          let dx, dy = p.dir in
          let d = Float.max 1e-9 (Float.hypot dx dy) in
          let hold = player_r + ball_r in
          { g with ball = Free_ball.still (p.px + (dx / d * hold)) (p.py + (dy / d * hold)); last = Some p.side }
      | _ ->
          let g = { g with carrier = None } in
          let g = { g with ball = Free_ball.roll ~friction g.ball } in
          let g = { g with ball = walls g.ball } in
          let g = hit_fixtures g in
          (* and whoever is nearest picks it up, one man a frame *)
          let can (p : player) = p.touch = 0 && p.down = 0 in
          (match Formation.nearest (fun (p : player) -> (p.px, p.py)) can (g.ball.x, g.ball.y) g.players with
          | None -> g
          | Some i ->
              let p = List.nth g.players i in
              if not (Free_ball.near reach (p.px, p.py) g.ball) then g
              else begin
                Audio.play Audio.blip;
                { g with carrier = Some i; last = Some p.side }
              end)
    in
    goals g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let go = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if go then { scenes = Scene2d.go (Playing (new_game ())) scenes } else { scenes }
  | Playing g ->
      let g = update_game computer g in
      if g.clock = 0 then { scenes = Scene2d.go (Full_time (g.red, g.blue)) scenes } else { scenes = { scenes with scene = Playing g } }
  | Full_time _ -> if go || scenes.elapsed > 12. then { scenes = Scene2d.go Title scenes } else { scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let steel = rgb 92 98 112
let steel_dark = rgb 62 68 80
let shirt (s : side) : color = match s with Red -> rgb 225 70 60 | Blue -> rgb 70 140 235

(* the arena: plate metal, the walls, and the mouth at each end *)
let view_arena : shape list =
  [ rectangle steel_dark (2. * half_w) (2. * half_h) ]
  @ List.init 12 (fun i ->
        let h = 2. * half_h / 12. in
        rectangle (if i mod 2 = 0 then steel_dark else rgb 68 74 88) (2. * half_w) (h - 3.) |> move_y (0. - half_h + (h / 2.) + (float_of_int i * h)))
  @ [ rectangle (rgb 150 158 175) (2. * half_w) 6.; circle (rgb 150 158 175) 70. |> fade 0.3; circle steel_dark 64. ]
  @ List.concat_map
      (fun (s : number) ->
        [ rectangle (rgb 150 158 175) (2. * half_w) 10. |> move_y (s * half_h);
          (* the mouth: a gap in the end wall, lit up *)
          rectangle (rgb 250 230 120) (2. * goal_half) 14. |> move_y (s * half_h);
          rectangle (rgb 120 128 145) 200. 6. |> move_y (s * (half_h - 120.)) ])
      [ 1.; -1. ]

let view_fixture (f : fixture) : shape list =
  let r = radius_of f.what in
  let glow = f.lit > 0 in
  match f.what with
  | Dome ->
      [ circle (if glow then rgb 255 240 170 else rgb 150 160 180) (r + 4.) |> move f.fx f.fy;
        circle (if glow then rgb 255 200 90 else rgb 110 120 140) r |> move f.fx f.fy; circle (rgb 60 66 80) (r / 2.) |> move f.fx f.fy ]
  | Star ->
      [ circle (if glow then rgb 180 250 255 else rgb 80 180 200) (r + 5.) |> fade 0.6 |> move f.fx f.fy;
        pentagon (if glow then white else rgb 130 220 240) r |> rotate 18. |> move f.fx f.fy ]
  | Plate ->
      [ square (if glow then rgb 255 230 120 else rgb 140 130 70) (r * 2.) |> rotate 45. |> move f.fx f.fy; text black 1.6 "x2" |> move f.fx f.fy ]

let view_player (g : game) (i : int) (p : player) : shape list =
  let dx, dy = p.dir in
  if p.down > 0 then [ oval (shirt p.side) (player_r * 2.2) (player_r * 1.1) |> fade 0.7 |> move p.px p.py ]
  else
    [ circle (shirt p.side) player_r |> move p.px p.py;
      (* the armour: a plate the way he faces *)
      rectangle (rgb 220 225 235) 16. 7. |> rotate (radians_to_degrees (atan2 dy dx)) |> move (p.px + (dx * 9.)) (p.py + (dy * 9.));
      circle (if keeper p then rgb 250 230 120 else rgb 250 220 190) 6. |> move p.px p.py ]
    @ if i = g.mine then [ circle white 4. |> move p.px (p.py + 24.) ] else []

let view_ball (g : game) : shape list =
  let b = g.ball in
  [ circle (rgb 15 16 20) (ball_r + 2.) |> fade 0.3 |> move (b.x + 3.) (b.y - 3.) ]
  @ (if g.electric > 0 then [ circle (rgb 180 250 255) (ball_r + 7.) |> fade 0.55 |> move b.x b.y ] else [])
  @ [ circle (rgb 225 230 240) ball_r |> move b.x b.y; circle (rgb 140 150 170) 4. |> move (b.x - 3.) (b.y + 3.) ]

let view_world (g : game) : shape list =
  view_arena @ List.concat_map view_fixture g.fixtures @ List.concat (List.mapi (view_player g) g.players) @ view_ball g

let view_hud (g : game) : shape list =
  let seconds = g.clock /.. 60 in
  let doubled (s : side) = match List.assoc_opt s g.double with Some n when n > 0 -> " x2" | _ -> "" in
  [ rectangle (rgb 16 18 24) 1000. 70. |> move_y 465.;
    text (shirt Red) 3.2 (Printf.sprintf "%d%s" g.red (doubled Red)) |> move (-250.) 465.;
    text white 2.4 (Printf.sprintf "0:%02d" seconds) |> move_y 465.;
    text (shirt Blue) 3.2 (Printf.sprintf "%d%s" g.blue (doubled Blue)) |> move 250. 465.;
    text (rgb 140 150 165) 1.6 "space: throw, or tackle" |> move 400. 465. ]
  @ (if g.power > 0. then
       [ rectangle (rgb 60 64 76) 204. 16. |> move (-330.) (-455.); rectangle (rgb 250 230 120) (Float.max 2. (200. * g.power)) 12. |> move (-430. + (100. * g.power)) (-455.) ]
     else [])
  @ match g.message with Some (what, _) -> [ text (rgb 250 240 170) 3.4 what |> move_y (-330.) ] | None -> []

let view_title (scenes : scene Scene2d.t) : shape list =
  [ text (rgb 230 90 70) 6.5 "TINY SPEEDBALL 2" |> move_y 330.; text (rgb 200 210 230) 2.4 "the arena scores, and the ball never stops" |> move_y 262. ]
  @ List.concat
      (List.mapi
         (fun i ((key : string), (what : string)) ->
           let y = 120. - (float_of_int i * 54.) in
           [ text (rgb 250 230 120) 2.2 key |> move (-250.) y; text white 2.2 what |> move 120. y ])
         [ ("goal", "10 points, and a restart in the middle"); ("dome", "5, and the ball comes straight back at you");
           ("star", "5, and the ball is lit"); ("x2 plate", "everything you score, doubled, for ten seconds");
           ("tackle", "10, and he is on the floor for two seconds") ])
  @ [ text (rgb 160 170 185) 1.9 "arrows run    space throws the ball, or tackles when you have not got it" |> move_y (-190.) ]
  @ Scene2d.blink 1. scenes [ text (rgb 250 240 170) 3. "PRESS SPACE" |> move_y (-290.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  match scenes.scene with
  | Title -> rectangle (rgb 24 26 32) screen.width screen.height :: view_title scenes
  | Playing g ->
      (* Zoomed in, and the ball dead centre at all times -- not the
       * player, the ball, which in this game is usually in somebody's
       * hands and so amounts to the same thing until he throws it.
       * No easing: Speedball's view is nailed to the ball. Camera2d
       * clamps it to the walls of the arena. *)
      let cam =
        { Camera2d.origin with zoom }
        |> Camera2d.look_at g.ball.x g.ball.y
        |> Camera2d.clamp screen { Camera2d.left = 0. - half_w; right = half_w; bottom = 0. - half_h; top = half_h }
      in
      (rectangle (rgb 24 26 32) screen.width screen.height :: Camera2d.view cam (view_world g) :: view_hud g)
  | Full_time (red, blue) ->
      [ rectangle (rgb 24 26 32) screen.width screen.height; text white 5. "FULL TIME" |> move_y 140.;
        text (shirt Red) 6. (Printf.sprintf "%d" red) |> move (-110.) 20.; text white 4. "-" |> move_y 20.;
        text (shirt Blue) 6. (Printf.sprintf "%d" blue) |> move 110. 20.;
        text (rgb 160 170 185) 2. "points, not goals: the arena paid for most of them" |> move_y (-70.) ]
      @ Scene2d.blink 1. scenes [ text (rgb 250 240 170) 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinySpeedball2
  keys:  arrows  run
         space   throw the ball (tap to pass, hold to shoot);
                 with no ball, tackle whoever you run into
  the arena scores: domes and stars 5, the x2 plates double what you
  score for ten seconds, a goal is 10, and so is flattening someone.
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
