(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Pang (Mitchell, 1989; Buster Bros. in America):
 * balloons bouncing round a screen, and a hunter at the bottom with a
 * harpoon gun that shoots a wire straight up. Left/right to walk, space
 * to shoot. A balloon hit bursts into two smaller ones, the smallest
 * into nothing; touch one and you lose a life. Clear the screen before
 * the time runs out, and on to the next landmark of the world tour.
 *
 * Pang went round the world, a stage in front of each of its famous
 * places (Mount Fuji first), and was played by two at once; Super Pang
 * (1990) and Pang! 3 followed, and the Amiga and Atari ST versions by
 * Ocean were loved in Europe. (Names and dates from memory, to check.)
 *
 * The new idea here is a bounce that is a rule, not physics. A ball
 * falling under gravity would, by Newton, bounce back as high as it
 * fell (and lower each time, with friction). Pang's balls don't care
 * where they fell from: each size has its own height, the big ones
 * bouncing high and slow, the small ones low and fast, forever
 * ([bounce_speed]: at the floor, the vertical speed is set, not
 * reversed). So a ball never drifts, the level designer chooses the
 * rhythm of every size, and a ball split in mid-air settles into its
 * size's rhythm at its first landing. Everything else is the
 * simplest motion there is: a constant speed sideways, gravity down.
 *
 *      big      ___           the heights of the four sizes: each
 *              /   \          ball, wherever it came from, bounces
 *     medium  / __  \         back to its own, and a burst ball's two
 *            / /  \  \        halves hop up a little, then fall into
 *     small / / __ \  \       the next size's rhythm
 *     tiny / / /  \ \  \
 *     ====================
 *
 * And splitting is recursion: a ball of size n bursts into two of size
 * n - 1, so one big ball is 1 + 2 + 4 + 8 = 15 hits, the whole screen
 * filling with small fast balls before it empties -- the difficulty
 * curve of a stage is in that doubling, no code for it.
 *
 * What it uses: Scene2d (the title, the stages, their ends), Audio (a
 * burst, a pickup, a death), Playground's random (the seed in the
 * model: which burst drops an item, and which). Not Physics: the balls
 * are the rule above; not Tilemap: the platforms are a few rectangles.
 *
 * Exercises: the second player, side by side; the original's other
 * weapons (the power wire that sticks to the ceiling for a while, the
 * machine gun); breakable glass platforms; ladders; more stages round
 * the world (the original has seventeen places, three stages each).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The arena and the stages *)
(*****************************************************************************)

(* the arena: the floor, the ceiling, the walls *)
let left = -480.
let right = 480.
let floor_y = -260.
let ceiling = 340.
let gravity = 0.25

(* the four sizes, 0 the tiny one: radius, height of the bounce (of its
 * bottom, above the floor), points *)
let radius (size : int) : number = [| 10.; 20.; 34.; 54. |].(size)
let bounce_height (size : int) : number = [| 110.; 190.; 270.; 360. |].(size)
let points (size : int) : int = [| 200; 150; 100; 50 |].(size)

(* the vertical speed leaving the floor that reaches [bounce_height]:
 * v^2 = 2 g h. The rule of this game: set, whatever the speed that came
 * down. *)
let bounce_speed (size : int) : number = sqrt (2. * gravity * bounce_height size)
let side_speed = 2.

type ball = { x : number; y : number; vx : number; vy : number; size : int }

(* a platform: its center and size *)
type platform = { px : number; py : number; pw : number; ph : number }

type stage = { place : string; balls : ball list; platforms : platform list }

let ball (size : int) (x : number) (y : number) (dir : number) : ball = { x; y; vx = dir * side_speed; vy = 0.; size }

let stages : stage list =
  [ { place = "MT. FUJI"; balls = [ ball 3 (-250.) 150. 1. ]; platforms = [] };
    { place = "GUILIN"; balls = [ ball 3 (-300.) 150. 1.; ball 2 300. 100. (-1.) ];
      platforms = [ { px = 0.; py = 20.; pw = 240.; ph = 24. } ] };
    { place = "AYERS ROCK"; balls = [ ball 3 0. 180. 1.; ball 2 (-350.) 80. 1.; ball 2 350. 80. (-1.) ];
      platforms = [ { px = -250.; py = -60.; pw = 160.; ph = 24. }; { px = 250.; py = -60.; pw = 160.; ph = 24. } ] } ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type item_kind = Double (* two wires at once *) | Freeze (* the balls stop *) | Dynamite (* every ball burst to the smallest *)

type item = { ix : number; iy : number; kind : item_kind; left_frames : int }

(* a wire, shot from the floor at [wx], its top rising *)
type wire = { wx : number; top : number }

type game = {
  stage : int;
  balls : ball list;
  hunter : number; (* his x, on the floor *)
  facing : number;
  steps : int;
  wires : wire list;
  double : bool;
  frozen : int; (* frames left of the freeze *)
  items : item list;
  time : int; (* frames left *)
  lives : int;
  score : int;
  seed : seed;
  frames : int;
}

type scene = Title | Playing of game | Dead of game * int | Cleared of game * int | Game_over of int | Won of int
type model = scene Scene2d.t

let stage_time = 90 *.. 60

let start_stage (i : int) (g : game) : game =
  { g with stage = i; balls = (List.nth stages i).balls; hunter = 0.; wires = []; frozen = 0; items = []; time = stage_time; frames = 0 }

let new_game () : game =
  start_stage 0
    { stage = 0; balls = []; hunter = 0.; facing = 1.; steps = 0; wires = []; double = false; frozen = 0; items = []; time = 0; lives = 3;
      score = 0; seed = initial_seed 1989; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The balls *)
(*****************************************************************************)

(* does a ball overlap a rectangle (center, half sizes)? the nearest
 * point of the rectangle within its radius *)
let touches (b : ball) (cx : number) (cy : number) (hw : number) (hh : number) : bool =
  let nx = Float.max (cx - hw) (Float.min b.x (cx + hw)) in
  let ny = Float.max (cy - hh) (Float.min b.y (cy + hh)) in
  Float.hypot (b.x - nx) (b.y - ny) < radius b.size

(* a ball against a platform: from above, it bounces as on the floor
 * (the rule again); from below, it's sent down; from the side, back.
 * Which one, by where the ball was the frame before. *)
let off_platform (prev : ball) (b : ball) (p : platform) : ball =
  if not (touches b p.px p.py (p.pw / 2.) (p.ph / 2.)) then b
  else
    let r = radius b.size in
    if prev.y - r >= p.py + (p.ph / 2.) then { b with y = p.py + (p.ph / 2.) + r; vy = bounce_speed b.size }
    else if prev.y + r <= p.py - (p.ph / 2.) then { b with y = p.py - (p.ph / 2.) - r; vy = -.Float.abs b.vy }
    else { b with x = prev.x; vx = -.b.vx }

let step_ball (platforms : platform list) (b : ball) : ball =
  let r = radius b.size in
  let moved = { b with x = b.x + b.vx; y = b.y + b.vy; vy = b.vy - gravity } in
  let moved = if moved.x - r < left || moved.x + r > right then { moved with x = b.x; vx = -.b.vx } else moved in
  let moved = if moved.y - r < floor_y then { moved with y = floor_y + r; vy = bounce_speed b.size } else moved in
  let moved = if moved.y + r > ceiling then { moved with y = ceiling - r; vy = -.Float.abs moved.vy } else moved in
  List.fold_left (off_platform b) moved platforms

(* a ball burst: two of the next size, one each way, hopping up a
 * little; the tiny one bursts into nothing *)
let burst (b : ball) : ball list =
  if b.size = 0 then [] else List.map (fun dir -> { b with size = b.size -.. 1; vx = dir * side_speed; vy = 5. }) [ -1.; 1. ]

(*****************************************************************************)
(* The hunter, his wires, the items *)
(*****************************************************************************)

let hunter_speed = 4.
let hunter_w = 36.
let hunter_h = 50.
let wire_speed = 12.

(* a wire rises until it reaches the ceiling or the underside of a
 * platform *)
let wire_stop (platforms : platform list) (w : wire) : number =
  List.fold_left
    (fun top p -> if Float.abs (w.wx - p.px) < p.pw / 2. then Float.min top (p.py - (p.ph / 2.)) else top)
    ceiling platforms

let wire_hits (w : wire) (b : ball) : bool = touches b w.wx ((floor_y + w.top) / 2.) 2. ((w.top - floor_y) / 2.)

(* the hunter's box, a little smaller than his picture: Pang forgives a
 * ball that only grazes his hair *)
let hunter_hit (g : game) : bool = List.exists (fun b -> touches b g.hunter (floor_y + (hunter_h / 2.) - 4.) ((hunter_w / 2.) - 6.) ((hunter_h / 2.) - 6.)) g.balls

(* one burst in six drops an item *)
let maybe_drop (b : ball) (g : game) : game =
  let n, seed = random_int 0 5 g.seed in
  if n <> 0 then { g with seed }
  else
    let kind, seed = pick [ Double; Freeze; Dynamite ] seed in
    { g with seed; items = { ix = b.x; iy = b.y; kind; left_frames = 400 } :: g.items }

let take (kind : item_kind) (g : game) : game =
  Audio.play Audio.coin;
  match kind with
  | Double -> { g with double = true }
  | Freeze -> { g with frozen = 240 }
  | Dynamite ->
      let rec down b = if b.size = 0 then [ b ] else List.concat_map down (burst b) in
      { g with balls = List.concat_map down g.balls }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let platforms = (List.nth stages g.stage).platforms in
  let dir = to_x computer.keyboard in
  let half = hunter_w / 2. in
  let g =
    { g with frames = g.frames +.. 1; time = g.time -.. 1; frozen = max 0 (g.frozen -.. 1);
      hunter = Float.max (left + half) (Float.min (right - half) (g.hunter + (dir * hunter_speed)));
      facing = (if dir <> 0. then dir else g.facing); steps = (if dir <> 0. then g.steps +.. 1 else g.steps) }
  in
  (* shooting: one wire at a time, two with the double *)
  let g =
    if Scene2d.pressed (fun k -> k.kspace) scenes && List.length g.wires < (if g.double then 2 else 1) then (
      Audio.play Audio.laser;
      { g with wires = { wx = g.hunter; top = floor_y + hunter_h } :: g.wires })
    else g
  in
  let wires = List.filter_map (fun w -> let top = w.top + wire_speed in if top >= wire_stop platforms w then None else Some { w with top }) g.wires in
  (* each wire bursts the first ball it touches, and is spent *)
  let g =
    List.fold_left
      (fun g w ->
        match List.find_opt (wire_hits w) g.balls with
        | Some b ->
            Audio.play (Audio.noise 800. |> Audio.lasting 0.08 |> Audio.fading);
            maybe_drop b { g with balls = burst b @ List.filter (fun b' -> b' != b) g.balls; score = g.score +.. points b.size }
        | None -> { g with wires = w :: g.wires })
      { g with wires = [] } wires
  in
  let balls = if g.frozen > 0 then g.balls else List.map (step_ball platforms) g.balls in
  (* the items fall to the floor, and wait there a while *)
  let items =
    List.filter_map
      (fun it -> if it.left_frames <= 0 then None else Some { it with iy = Float.max (floor_y + 15.) (it.iy - 3.); left_frames = it.left_frames -.. 1 })
      g.items
  in
  let taken, items = List.partition (fun it -> Float.abs (it.ix - g.hunter) < 35. && it.iy < floor_y + hunter_h) items in
  List.fold_left (fun g it -> take it.kind g) { g with balls; items } taken

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if g.balls = [] then Scene2d.go (Cleared (g, 0)) s
      else if hunter_hit g || g.time <= 0 then (
        Audio.play Audio.explosion;
        Scene2d.go (Dead (g, 0)) s)
      else { s with scene = Playing g }
  (* a death: the stage again from its start, the double lost *)
  | Dead (g, n) ->
      if n < 90 then { s with scene = Dead (g, n +.. 1) }
      else if g.lives <= 1 then Scene2d.go (Game_over g.score) s
      else Scene2d.go (Playing (start_stage g.stage { g with lives = g.lives -.. 1; double = false })) s
  | Cleared (g, n) ->
      if n < 120 then { s with scene = Cleared (g, n +.. 1) }
      else
        (* the time left, as points *)
        let g = { g with score = g.score +.. (g.time /.. 6) } in
        if g.stage +.. 1 >= List.length stages then Scene2d.go (Won g.score) s else Scene2d.go (Playing (start_stage (g.stage +.. 1) g)) s
  | Game_over _ | Won _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size
let arena_w = right - left
let arena_h = ceiling - floor_y
let arena_y = (ceiling + floor_y) / 2.

(* the landmarks behind each stage, in a few shapes *)
let backdrop (stage : int) : shape =
  let sky c = rectangle c arena_w arena_h |> move_y arena_y in
  match stage with
  | 0 ->
      group
        [ sky (rgb 150 200 240); polygon (rgb 90 110 150) [ (-420., floor_y); (-60., 180.); (60., 180.); (420., floor_y) ];
          polygon white [ (-60., 180.); (60., 180.); (120., 100.); (60., 120.); (0., 90.); (-50., 125.); (-120., 100.) ];
          circle (rgb 255 245 200) 40. |> move 330. 250. ]
  | 1 ->
      let peak x h c = group [ oval c 150. (h * 2.) |> move x floor_y; oval c 90. 80. |> move x (floor_y + h - 30.) ] in
      group
        [ sky (rgb 200 220 210); peak (-360.) 330. (rgb 110 150 120); peak (-170.) 250. (rgb 90 130 100); peak 60. 380. (rgb 110 150 120);
          peak 260. 280. (rgb 90 130 100); peak 420. 220. (rgb 110 150 120); rectangle (rgb 120 170 190) arena_w 40. |> move_y (floor_y + 20.) ]
  | _ ->
      group
        [ sky (rgb 240 170 110); oval (rgb 190 80 40) 700. 260. |> move 0. (floor_y + 60.); oval (rgb 160 60 30) 600. 30. |> move 0. (floor_y + 150.);
          rectangle (rgb 220 150 80) arena_w 60. |> move_y (floor_y + 30.) ]

(* a balloon: red, with a shine *)
let view_ball (b : ball) : shape =
  let r = radius b.size in
  group [ circle (rgb 120 20 20) (r + 2.); circle (rgb 220 40 40) r; circle (rgb 255 170 170) (r / 4.) |> move (-.r / 2.5) (r / 2.5) ] |> move b.x b.y

let hunter_rows =
  [ [ "..HHHH.."; ".HHHHHH."; "..SSSS.."; "..SWSS.."; "..SSSS.."; ".BBBBBB."; "SBBBBBBS"; "SBBBBBBS"; ".YYYYYY."; "..PP.PP."; "..PP.PP."; ".KK..KK." ];
    [ "..HHHH.."; ".HHHHHH."; "..SSSS.."; "..SWSS.."; "..SSSS.."; ".BBBBBB."; "SBBBBBBS"; "SBBBBBBS"; ".YYYYYY."; "...PPP.."; "...PPP.."; "..KKKK.." ] ]

let palette = [ ('H', rgb 200 150 60); ('S', rgb 240 190 140); ('W', black); ('B', rgb 40 110 200); ('Y', rgb 240 200 60); ('P', rgb 60 60 90); ('K', rgb 90 50 30) ]

let item_shape (kind : item_kind) : shape =
  match kind with
  | Double -> group [ square (rgb 60 60 200) 28.; rectangle white 3. 20. |> move_x (-5.); rectangle white 3. 20. |> move_x 5. ]
  | Freeze -> group [ circle (rgb 200 200 60) 15.; circle white 11.; rectangle black 2. 9. |> move_y 4.; rectangle black 7. 2. |> move_x 3. ]
  | Dynamite -> group [ rectangle red 12. 26.; rectangle yellow 2. 8. |> move_y 16. ]

let view_game (g : game) (blink : bool) : shape list =
  let stage = List.nth stages g.stage in
  let hunter =
    if blink && g.frames mod 8 < 4 then []
    else [ Sprite.pixels 4.2 palette (let r = Sprite.cycle (g.steps /.. 6) hunter_rows in if g.facing < 0. then Sprite.flip r else r) |> move g.hunter (floor_y + (hunter_h / 2.)) ]
  in
  [ backdrop g.stage;
    (* the frame round the arena *)
    rectangle (rgb 100 100 110) (arena_w + 30.) 15. |> move_y (ceiling + 7.); rectangle (rgb 100 100 110) (arena_w + 30.) 15. |> move_y (floor_y - 7.);
    rectangle (rgb 100 100 110) 15. (arena_h + 30.) |> move (left - 7.) arena_y; rectangle (rgb 100 100 110) 15. (arena_h + 30.) |> move (right + 7.) arena_y ]
  @ List.map (fun p -> group [ rectangle (rgb 90 70 150) p.pw p.ph; rectangle (rgb 140 120 200) (p.pw - 6.) (p.ph - 8.) ] |> move p.px p.py) stage.platforms
  @ List.map
      (fun w ->
        let h = w.top - floor_y in
        group [ rectangle (rgb 230 230 230) 3. h |> move_y (h / 2.); triangle (rgb 230 230 230) 8. |> move_y h ] |> move w.wx floor_y)
      g.wires
  @ List.map (fun it -> item_shape it.kind |> fade (if it.left_frames < 100 && it.left_frames mod 10 < 5 then 0.3 else 1.) |> move it.ix it.iy) g.items
  @ List.map (fun b -> view_ball b |> fade (if g.frozen > 0 && g.frozen < 60 && g.frozen mod 10 < 5 then 0.5 else 1.)) g.balls
  @ hunter
  @ [ text white 2.5 (Printf.sprintf "%s   STAGE %d" stage.place (g.stage +.. 1)) |> move_y (ceiling + 45.);
      text white 2.5 (Printf.sprintf "SCORE %d" g.score) |> move (-330.) (floor_y - 50.);
      text (if g.time < 10 *.. 60 then red else white) 2.5 (Printf.sprintf "TIME %d" (g.time /.. 60)) |> move_y (floor_y - 50.);
      text white 2.5 (Printf.sprintf "LIVES %d%s" g.lives (if g.double then "  DOUBLE" else "")) |> move 330. (floor_y - 50.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 20 20 30) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_game ()) false
      @ [ rectangle black 700. 240. |> fade 0.8 |> move_y 60.; text (rgb 250 80 80) 8. "TINY PANG" |> move_y 130.;
          text white 2.3 "left/right walk   space shoot the harpoon" |> move_y 65.; text white 2.3 "burst every balloon, and don't let one touch you" |> move_y 30. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-20.) ]
  | Playing g -> view_game g false
  | Dead (g, _) -> view_game g true @ [ text red 5. (if g.time <= 0 then "TIME UP" else "OUCH!") |> move_y 60. ]
  | Cleared (g, _) -> view_game g false @ [ text yellow 5. "STAGE CLEAR" |> move_y 60.; text white 2.5 (Printf.sprintf "TIME BONUS %d" (g.time /.. 6)) ]
  | Game_over score -> [ text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Won score ->
      [ text yellow 6. "ROUND THE WORLD!"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app