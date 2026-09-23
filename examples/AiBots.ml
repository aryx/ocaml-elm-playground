(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* What makes a bot fair rather than strong (Sense.mli, Bot.mli).
 *
 * A bot that knows everything and reacts instantly is the easiest one
 * to write and the least pleasant to play against. Every knob that
 * makes one feel like an opponent is a *handicap*, and the four keys
 * here turn them off, one at a time, so you can feel the machine come
 * back:
 *
 *   1  reaction delay   it acts on what it saw 15 frames ago (a
 *                       human's quarter of a second)
 *   2  input rate       it may change its mind every 6 frames, and
 *                       repeats itself in between
 *   3  aim error        its aim starts off and settles the longer it
 *                       keeps you in sight
 *   4  senses           what it may know at all: with this off it sees
 *                       you through the walls, which is the cheat the
 *                       whole layer exists to take away
 *
 * Turn all four off and it is a machine: it knows where you are the
 * frame you move, faces you exactly, and fires the moment you step out.
 *
 * What the bot knows is drawn, which is the other half of the point:
 * a green line while it can see you, a fading marker where it last saw
 * you (it hunts there, then gives up), and nothing at all before it
 * has ever seen you -- when it patrols instead.
 *
 * Move with the arrows or WASD. The bot fires when it is aimed at you;
 * a shot that reaches you flashes the screen (nothing else happens:
 * this is a demonstration, not a game).
 *
 * What it uses: the Playground, Scene2d (the keys pressed), Sense
 * and Bot, and physics/2d's Collide for the line of sight (one
 * segment against the walls).
 *)
open Playground
open Basics (* float arithmetics *)

(* the arena: walls to hide behind, as rectangles (x, y, width, height) *)
let walls = [ (-260., 140., 300., 40.); (200., 40., 40., 320.); (-120., -200., 360., 40.); (260., -260., 220., 40.) ]

let corners_of ((x, y, w, h) : number * number * number * number) : (number * number) list =
  [ (x - (w / 2.), y - (h / 2.)); (x + (w / 2.), y - (h / 2.)); (x + (w / 2.), y + (h / 2.)); (x - (w / 2.), y + (h / 2.)) ]

(* nothing of the arena on the segment between them *)
let clear (a : number * number) (b : number * number) : bool =
  List.for_all (fun wall -> Collide.segment_polygon (a, b) (corners_of wall) = None) walls

let hits_wall ((x, y) : number * number) : bool =
  List.exists (fun (wx, wy, w, h) -> Float.abs (x - wx) < (w / 2.) + 12. && Float.abs (y - wy) < (h / 2.) + 12.) walls

(* {1 The bot} *)

(* what it may know: where it is, and you -- seen, or remembered, or
   not at all (Sense.mli) *)
type senses = { me : number * number; frame : int; you : (number * number) Sense.target }

(* what it does: where it walks, where it aims, whether it fires --
   the same three things you do with the keys *)
type intent = { walk : number * number; aim : number; fire : bool }

type knobs = { delay : bool; rate : bool; wobble : bool; senses : bool }

let degrees (dx : number) (dy : number) : number = Float.atan2 dy dx * 180. / Float.pi

(* the rounds it walks when it knows nothing: the four corners, three
   seconds each, which sweeps the arena until it sees you *)
let beat = [| (-320., -320.); (320., -320.); (320., 320.); (-320., 320.) |]

let decide (k : knobs) (s : senses) : intent =
  let (mx, my) = s.me in
  match s.you.position with
  (* never seen you, or given up: walk the rounds *)
  | None ->
      let (wx, wy) = beat.((s.frame /.. 180) mod Array.length beat) in
      let (dx, dy) = (wx - mx, wy - my) in
      let far = Float.max 1. (Float.hypot dx dy) in
      { walk = (dx / far, dy / far); aim = degrees dx dy; fire = false }
  | Some (tx, ty) ->
      let dx = tx - mx and dy = ty - my in
      let far = Float.hypot dx dy in
      let error = if k.wobble then Bot.aim_error ~spread:25. ~settle:20. ~seen_for:s.you.seen_for ~seed:1 () else 0. in
      { (* keep some distance while it can see you, else walk to where you were *)
        walk = (if s.you.visible && far < 260. then (-.dx / far, -.dy / far) else (dx / far, dy / far));
        aim = degrees dx dy + error;
        fire = s.you.visible && far < 700.
      }

let sense (k : knobs) (was : senses option) ((me, you, frame) : (number * number) * (number * number) * int) : senses =
  let target = match was with Some s -> s.you | None -> Sense.unknown in
  {
    me;
    frame;
    you =
      Sense.update ~sight:900. ~distance:(Float.hypot (fst you - fst me) (snd you - snd me))
        (* knob 4: off, and the walls stop mattering *)
        ~clear:((not k.senses) || clear me you)
        ~position:you target
      |> Sense.forget ~after:180;
  }

let mind (k : knobs) : ((number * number) * (number * number) * int, senses, intent) Bot.t =
  Bot.make ~delay:(if k.delay then 15 else 0) ~rate:(if k.rate then 6 else 1) ~sense:(sense k) ~decide:(decide k) ()

(* {1 The model} *)

type shot = { from : number * number; to_ : number * number; age : int }

type state = {
  you : number * number;
  bot : number * number;
  bot_aim : number;
  running : (senses, intent) Bot.running;
  last : senses option; (* what it knew last frame, to draw *)
  shots : shot list;
  hit : int; (* frames since a shot reached you *)
  knobs : knobs;
  frame : int;
}

type model = state Scene2d.t

let still = { walk = (0., 0.); aim = 0.; fire = false }

let initial_model : model =
  Scene2d.start
    { you = (-380., -380.);
      bot = (380., 360.);
      bot_aim = 180.;
      running = Bot.start still;
      last = None;
      shots = [];
      hit = 0;
      knobs = { delay = true; rate = true; wobble = true; senses = true };
      frame = 0 }

let speed = 3.2

(* a step; if it would end in a wall or out of the arena, the same step
   along one axis only, so that a body slides along a wall instead of
   sticking to it (which a bot walking into a corner would do forever) *)
let walk ((x, y) : number * number) ((dx, dy) : number * number) : number * number =
  let free (x, y) = (not (hits_wall (x, y))) && Float.abs x < 470. && Float.abs y < 470. in
  let step (dx, dy) = (x + (dx * speed), y + (dy * speed)) in
  List.find_opt free [ step (dx, dy); step (dx, 0.); step (0., dy) ] |> Option.value ~default:(x, y)

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let s = scenes.scene and k = computer.keyboard in
  let pressed key = Scene2d.pressed (fun k -> Set_.mem key k.keys) scenes in
  let knobs =
    { delay = (if pressed "1" then not s.knobs.delay else s.knobs.delay);
      rate = (if pressed "2" then not s.knobs.rate else s.knobs.rate);
      wobble = (if pressed "3" then not s.knobs.wobble else s.knobs.wobble);
      senses = (if pressed "4" then not s.knobs.senses else s.knobs.senses) }
  in
  let letter l = Set_.mem l k.keys in
  let dx = (if k.kright || letter "d" then 1. else 0.) - if k.kleft || letter "a" then 1. else 0. in
  let dy = (if k.kup || letter "w" then 1. else 0.) - if k.kdown || letter "s" then 1. else 0. in
  let you = walk s.you (dx, dy) in
  let (it, running) = Bot.step (mind knobs) (s.bot, you, s.frame) s.running in
  let bot = walk s.bot it.walk in
  (* a shot: a line from the bot the way it aims; it reaches you if it
     passes close and nothing is in the way *)
  let shots = List.filter_map (fun sh -> if sh.age > 8 then None else Some { sh with age = sh.age +.. 1 }) s.shots in
  let fired = it.fire && s.frame mod 12 = 0 in
  let ray =
    let a = it.aim * Float.pi / 180. in
    (fst bot + (900. * cos a), snd bot + (900. * sin a))
  in
  let (bx, by) = bot and (yx, yy) = you in
  let near_line =
    let (rx, ry) = ray in
    let len = Float.hypot (rx - bx) (ry - by) in
    let t = Basics.clamp 0. 1. ((((yx - bx) * (rx - bx)) + ((yy - by) * (ry - by))) / (len * len)) in
    Float.hypot (yx - (bx + (t * (rx - bx)))) (yy - (by + (t * (ry - by))))
  in
  let hit = if fired && near_line < 18. && clear bot you then 12 else max 0 (s.hit -.. 1) in
  { scenes with
    scene =
      { you;
        bot;
        bot_aim = it.aim;
        running;
        last = Bot.last_senses running;
        shots = (if fired then { from = bot; to_ = ray; age = 0 } :: shots else shots);
        hit;
        knobs;
        frame = s.frame +.. 1 } }

(* {1 The view} *)

let segment (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) (width : number) : shape =
  rectangle color (Float.hypot (x2 - x1) (y2 - y1)) width
  |> rotate (degrees (x2 - x1) (y2 - y1))
  |> move ((x1 + x2) / 2.) ((y1 + y2) / 2.)

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and s = model.scene in
  let text size color str = words color str |> scale size in
  let on_off b = if b then "on" else "OFF" in
  let knob i name b key =
    text 1.7 (if b then rgb 180 180 200 else rgb 250 120 90) (Printf.sprintf "%d  %-15s %s" key name (on_off b))
    |> move (-330.) (400. - (float_of_int i * 30.))
  in
  let knowledge =
    match s.last with
    | Some { you = { visible = true; position = Some p; _ }; _ } ->
        [ segment (rgb 90 230 120) s.bot p 2.; text 1.6 (rgb 90 230 120) "it sees you" |> move 300. 420. ]
    | Some { you = { position = Some (x, y); age; _ }; _ } ->
        [ circle (rgb 240 200 90) 10. |> fade (Float.max 0.15 (1. - (float_of_int age / 180.))) |> move x y;
          text 1.6 (rgb 240 200 90) (Printf.sprintf "it hunts where it saw you, %d frames ago" age) |> move 300. 420. ]
    | _ -> [ text 1.6 (rgb 150 150 170) "it has never seen you: patrolling" |> move 300. 420. ]
  in
  [ rectangle (rgb 22 24 32) screen.width screen.height ]
  @ (if s.hit > 0 then [ rectangle (rgb 250 80 70) screen.width screen.height |> fade 0.12 ] else [])
  @ List.map (fun (x, y, w, h) -> rectangle (rgb 70 74 90) w h |> move x y) walls
  @ knowledge
  @ List.map (fun sh -> segment (rgb 250 220 120) sh.from sh.to_ 2. |> fade (1. - (float_of_int sh.age / 9.))) s.shots
  @ [ circle (rgb 90 170 250) 12. |> move (fst s.you) (snd s.you);
      circle (rgb 230 90 80) 12. |> move (fst s.bot) (snd s.bot);
      segment (rgb 230 90 80) s.bot (fst s.bot + (30. * cos (s.bot_aim * Float.pi / 180.)), snd s.bot + (30. * sin (s.bot_aim * Float.pi / 180.))) 3.;
      text 2.2 white "what makes a bot fair" |> move_y 460.;
      text 1.6 (rgb 150 150 170) "arrows or WASD: move.  the keys turn its handicaps off" |> move_y (-450.) ]
  @ [ knob 0 "reaction delay" s.knobs.delay 1; knob 1 "input rate" s.knobs.rate 2; knob 2 "aim error" s.knobs.wobble 3;
      knob 3 "senses" s.knobs.senses 4 ]
  @ [ text 1.5 (rgb 150 150 170) (if s.knobs.senses then "it sees you only when nothing is in the way" else "it sees you through the walls")
      |> move (-250.) 270. ]

let help =
  {|Bots
  keys:  arrows, WASD  move
         1             the bot's reaction delay, on or off
         2             its input rate
         3             its aim error
         4             its senses (off: it sees through walls)
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app app
