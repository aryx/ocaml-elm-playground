(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Journey (Jenova Chen, thatgamecompany, 2012): a
 * figure in a red robe, a desert, and a mountain on the horizon with a
 * light at its summit. Left/right to walk, up to hop, space held to fly
 * (while your scarf has charge), x to sing -- a chirp, the only word
 * there is. The glowing glyphs lengthen your scarf; the cloth banners,
 * sung to, refill it; the dunes are for sliding down; somewhere a
 * stranger in white walks the same way.
 *
 * Journey's companions were other players, met at random over the
 * network, their names shown only at the credits; they could not talk,
 * only chirp, and many players remember one of them more than the game.
 * It was thatgamecompany's third game, after flOw and Flower, and won
 * most of the awards of its year. (Names and dates from memory, to
 * check.) Here the stranger is a program.
 *
 * The new idea here is cooperation without words: one verb, the chirp,
 * and nearness. The scarf is the only resource of the game (no life,
 * no score): its length is how long you can fly, and it refills only
 * by what you sing to -- the banners, and the stranger, whose chirp
 * refills yours when you are close ([chirp]). On the mountain, the wind
 * pushes you back and drains the scarf, less when the stranger walks at
 * your side ([wind]). Nothing forces you together; everything is
 * easier together, and that is the whole design.
 *
 * And the desert is a function: the ground's height at every x
 * ([ground]), eased between a few points typed by hand (cosine
 * interpolation), so the dunes are smooth; its slope makes the
 * sliding: on the ground, gravity along the slope pushes the traveler
 * downhill, and the long descent at sunset is surfed, not walked.
 *
 *        ___                   on the ground: the slope s = dh/dx,
 *       /   \___               and each frame vx += -g * s: going
 *      /  @->   \___           down a dune (s < 0), faster and faster;
 *     /             \___       up the next one, slower, and back
 *
 * What it uses: Camera2d (following the traveler, the far dunes and
 * the mountain with parallax), Scene2d, Audio (the chirps). Not
 * Tilemap: the ground is a curve, not a grid; not Physics: the
 * traveler is a point on a curve, or in the air.
 *
 * Exercises: the stranger as another player, over the network
 * (Multiplayer.mli); the cloth creatures that swim in the air and
 * carry you; the underground's guardians, whose light tears scarves;
 * the snow's footprints; the credits, listing who walked with you.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The desert *)
(*****************************************************************************)

(* the ground's height at a few x: the dunes, the long descent at
 * sunset, the mountain's slope in the snow *)
let heights : (number * number) list =
  [ (-400., -150.); (0., -150.); (400., -120.); (800., -60.); (1200., -140.); (1600., -40.); (2000., -130.); (2400., -20.); (2800., -100.);
    (3200., 0.); (3500., 60.); (3900., -40.); (4400., -160.); (5000., -280.); (5600., -380.); (6200., -450.); (6800., -480.);
    (7000., -470.); (7400., -380.); (7800., -250.); (8200., -200.); (8600., -60.); (9000., 0.); (9400., 120.); (9800., 200.);
    (10200., 320.); (10600., 380.); (11000., 460.); (11400., 480.); (12400., 480.) ]

let summit = 11600.
let mountain_starts = 7000.

(* [ground x]: between the two points around x, eased by a cosine: flat
 * at each point, steepest halfway, so the dunes have round tops *)
let ground (x : number) : number =
  let rec go = function
    | (x0, y0) :: ((x1, y1) :: _ as rest) -> if x <= x1 then let t = (x - x0) / (x1 - x0) in let e = (1. - cos (t * Float.pi)) / 2. in y0 + ((y1 - y0) * e) else go rest
    | [ (_, y) ] -> y
    | [] -> 0.
  in
  if x <= fst (List.hd heights) then snd (List.hd heights) else go heights

let slope (x : number) : number = (ground (x + 2.) - ground (x - 2.)) / 4.

(* the banners (they refill a scarf, sung to), the glyphs (a longer
 * scarf, floating: some only reached flying), the gravestones *)
let banners : number list = [ 700.; 1900.; 2900.; 4300.; 8100.; 9500.; 10500. ]
let glyphs : (number * number) list = [ (1200., 170.); (2400., 230.); (5100., 60.); (8600., 200.); (10300., 160.) ]
let graves : number list = [ 300.; 1000.; 1500.; 2200.; 2600.; 3300.; 7600.; 8800.; 9200.; 9900.; 10800. ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type walker = {
  x : number;
  y : number;
  vx : number;
  vy : number;
  facing : number;
  steps : int;
  charge : number; (* what's left to fly *)
  length : number; (* the scarf: the most charge it holds *)
}

type stranger = { w : walker; met : bool; sings : int (* frames until he sings back, 0 not *) }

type game = {
  me : walker;
  stranger : stranger;
  glyphs_left : (number * number) list;
  lit : (number * int) list; (* banners glowing, and for how long more *)
  rings : (number * number * int * bool) list; (* chirps: where, age, mine *)
  cam : Camera2d.t;
  frames : int;
}

type scene = Title | Playing of game | Summit of bool (* together *) * int
type model = scene Scene2d.t

let new_walker (x : number) (length : number) : walker = { x; y = ground x; vx = 0.; vy = 0.; facing = 1.; steps = 0; charge = length; length }

let new_game () : game =
  { me = new_walker 0. 60.; stranger = { w = new_walker 2700. 120.; met = false; sings = 0 }; glyphs_left = glyphs; lit = []; rings = [];
    cam = Camera2d.look_at 0. 0. Camera2d.origin; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Walking, sliding, flying *)
(*****************************************************************************)

let gravity = 0.3
let walk_speed = 2.6
let on_ground (w : walker) : bool = w.y <= ground w.x + 0.5 && w.vy <= 0.

(* the wind on the mountain: against the climb, and draining the scarf
 * in the air; the stranger at your side is a shelter *)
let wind (x : number) (sheltered : bool) : number = if x < mountain_starts then 0. else (if sheltered then 0.04 else 0.11) * Float.min 1. ((x - mountain_starts) / 1500.)

(* [step_walker dir hop fly blow w]: on the ground, walking towards
 * [dir] and pushed down the slope; in the air, falling, or rising
 * while [fly] and the scarf has charge *)
let step_walker (dir : number) (hop : bool) (fly : bool) (blow : number) (w : walker) : walker =
  let facing = if dir <> 0. then dir else w.facing in
  if on_ground w then
    let s = slope w.x in
    (* walking pulls the speed towards the walking pace, but a slide
     * faster than it in the same way is only slowed by the sand *)
    let pull = if dir = 0. then 0.03 else if w.vx * dir > walk_speed then 0.008 else 0.12 in
    let vx = w.vx + ((dir * walk_speed) - w.vx) * pull - (s * 0.35) - blow in
    let vx = Float.max (-9.) (Float.min 9. vx) in
    let x = w.x + vx in
    let vy = if hop || (fly && w.charge > 0.) then 5. else 0. in
    { w with x; y = ground x + (if vy > 0. then 1. else 0.); vx; vy; facing; steps = (if dir <> 0. || Float.abs vx > 1. then w.steps +.. 1 else w.steps) }
  else
    let flying = fly && w.charge > 0. in
    let vy = if flying then Float.min 4.5 (w.vy + 0.7) else w.vy - gravity in
    let vx = w.vx + (((dir * walk_speed) - w.vx) * 0.04) - blow in
    let x = w.x + vx and y = w.y + vy in
    let charge = if flying then Float.max 0. (w.charge - 0.5 - (blow * 5.)) else w.charge in
    if y <= ground x then { w with x; y = ground x; vx; vy = 0.; facing; charge } else { w with x; y; vx; vy; facing; charge }

(*****************************************************************************)
(* Singing, and the stranger *)
(*****************************************************************************)

let reach = 170.

(* a chirp at (x, y): the banners in reach glow, and refill whoever
 * sings near them; the stranger in reach refills too, and sings back *)
let chirp (mine : bool) (g : game) : game =
  let who = if mine then g.me else g.stranger.w in
  let near_banners = List.filter (fun b -> Float.abs (b - who.x) < reach) banners in
  let refill w = { w with charge = w.length } in
  let together = Float.hypot (g.me.x - g.stranger.w.x) (g.me.y - g.stranger.w.y) < reach && g.stranger.met in
  let g = { g with lit = List.map (fun b -> (b, 300)) near_banners @ List.filter (fun (b, _) -> not (List.mem b near_banners)) g.lit; rings = (who.x, who.y, 0, mine) :: g.rings } in
  let g = if near_banners <> [] then (if mine then { g with me = refill g.me } else { g with stranger = { g.stranger with w = refill g.stranger.w } }) else g in
  if together then { g with me = refill g.me; stranger = { g.stranger with w = refill g.stranger.w; sings = (if mine then 25 else g.stranger.sings) } } else g

(* the stranger: sits until you come near, then keeps near you -- a
 * little ahead when you walk, waiting when you stop, flying when you
 * fly -- and sings back when you sing *)
let step_stranger (me : walker) (st : stranger) : stranger * bool =
  let met = st.met || Float.abs (me.x - st.w.x) < 350. in
  if not met then ({ st with met }, false)
  else
    let target = me.x + (me.facing * 70.) in
    let dx = target - st.w.x in
    let dir = if Float.abs dx < 30. then 0. else if dx > 0. then 1. else -1. in
    let fly = (not (on_ground me)) && me.y > st.w.y + 40. in
    let sheltered = Float.abs (me.x - st.w.x) < 150. in
    let w = step_walker dir false fly (wind st.w.x sheltered) st.w in
    (* he keeps up: never more than a screen behind *)
    let w = if me.x - w.x > 600. then { w with x = me.x - 600.; y = ground (me.x - 600.) } else w in
    let sings = max 0 (st.sings -.. 1) in
    ({ w; met; sings }, st.sings = 1)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let keys = computer.keyboard in
  let g = { g with frames = g.frames +.. 1; lit = List.filter_map (fun (b, n) -> if n > 0 then Some (b, n -.. 1) else None) g.lit;
                   rings = List.filter_map (fun (x, y, a, m) -> if a < 40 then Some (x, y, a +.. 1, m) else None) g.rings } in
  let sheltered = g.stranger.met && Float.abs (g.me.x - g.stranger.w.x) < 150. in
  let me = step_walker (to_x keys) (Scene2d.pressed (fun k -> k.kup) scenes) keys.kspace (wind g.me.x sheltered) g.me in
  let me = { me with x = Float.max (-300.) me.x } in
  (* the glyphs touched: a longer scarf, refilled *)
  let got, glyphs_left = List.partition (fun (gx, gy) -> Float.hypot (gx - me.x) (ground gx + gy - (me.y + 20.)) < 40.) g.glyphs_left in
  let me = if got = [] then me else (Audio.play Audio.coin; let length = me.length + (30. * float_of_int (List.length got)) in { me with length; charge = length }) in
  let stranger, sings_back = step_stranger me g.stranger in
  let g = { g with me; stranger; glyphs_left } in
  let g = if Scene2d.pressed (fun k -> Set_.mem "x" k.keys) scenes then (Audio.play (Audio.square 880. |> Audio.lasting 0.12 |> Audio.fading); chirp true g) else g in
  let g = if sings_back || (g.stranger.met && g.frames mod 400 = 0) then (Audio.play (Audio.square 740. |> Audio.lasting 0.12 |> Audio.fading); chirp false g) else g in
  { g with cam = Camera2d.follow 0.08 (g.me.x + (g.me.facing * 120.)) (g.me.y + 120.) g.cam }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if g.me.x > summit then Scene2d.go (Summit (g.stranger.met && Float.abs (g.stranger.w.x - g.me.x) < 500., 0)) s else { s with scene = Playing g }
  | Summit (together, n) -> if n > 90 && space then Scene2d.go Title s else { s with scene = Summit (together, n +.. 1) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the colours of the day, from the desert's noon to the sunset of the
 * descent and the mountain's grey snow: [palette x] blends the two
 * around x *)
type palette = { sky : number * number * number; sand : number * number * number; far : number * number * number }

let noon = { sky = (150., 200., 235.); sand = (225., 180., 110.); far = (200., 160., 110.) }
let sunset = { sky = (240., 150., 110.); sand = (230., 150., 90.); far = (190., 110., 80.) }
let snow = { sky = (120., 130., 160.); sand = (235., 240., 250.); far = (170., 180., 200.) }

let blend (t : number) ((r1, g1, b1) : number * number * number) ((r2, g2, b2) : number * number * number) : color =
  let c a b = int_of_float (a + ((b - a) * t)) in
  rgb (c r1 r2) (c g1 g2) (c b1 b2)

let colours (x : number) : color * color * color =
  let a, b, t =
    if x < 3500. then (noon, noon, 0.)
    else if x < 5000. then (noon, sunset, (x - 3500.) / 1500.)
    else if x < 7000. then (sunset, sunset, 0.)
    else if x < 8000. then (sunset, snow, (x - 7000.) / 1000.)
    else (snow, snow, 0.)
  in
  (blend t a.sky b.sky, blend t a.sand b.sand, blend t a.far b.far)

(* the ground from [left] to [right], filled down below the screen *)
let dunes (c : color) (f : number -> number) (left : number) (right : number) (floor : number) : shape =
  let xs = List.init (int_of_float ((right - left) / 20.) +.. 2) (fun i -> left + (float_of_int i * 20.)) in
  polygon c (List.map (fun x -> (x, f x)) xs @ [ (right + 20., floor); (left, floor) ])

(* the traveler: a robe, a head, and the scarf trailing behind, as long
 * as it can hold, lit as far as its charge *)
let view_walker (robe : color) (w : walker) (frames : int) : shape list =
  let n = int_of_float (w.length / 10.) in
  let lit = int_of_float (w.charge / 10.) in
  let scarf =
    List.init n (fun i ->
        let t = float_of_int i in
        let wave = 5. * sin ((float_of_int frames / 6.) + (t * 0.7)) in
        rectangle (if i < lit then rgb 250 200 80 else rgb 140 110 60) 11. 6. |> move (w.x - (w.facing * (8. + (t * 9.)))) (w.y + 40. + wave - (t * 0.8)))
  in
  let bob = if w.steps mod 16 < 8 then 0. else 1.5 in
  scarf
  @ [ polygon robe [ (w.x - 13., w.y); (w.x + 13., w.y); (w.x + 5., w.y + 38. + bob); (w.x - 5., w.y + 38. + bob) ];
      circle (rgb 70 50 40) 8. |> move w.x (w.y + 44. + bob); rectangle (rgb 250 240 200) 7. 2. |> move (w.x + (w.facing * 2.)) (w.y + 45. + bob) ]

let view_game (screen : screen) (g : game) : shape list =
  let cam : Camera2d.t = g.cam in
  let sky, sand, far = colours g.me.x in
  let seen = Camera2d.visible screen cam in
  (* the mountain on the horizon, nearer as you go, its light *)
  let near = Float.min 1. (Float.max 0. g.me.x / summit) in
  let peak_y = 140. + (near * 160.) in
  let mountain =
    group
      [ polygon (rgb 110 110 130) [ (-900. - (near * 300.), -900.); (0., peak_y); (900. + (near * 300.), -900.) ];
        polygon (rgb 240 240 250) [ (-60. - (near * 30.), peak_y - 70.); (0., peak_y); (60. + (near * 30.), peak_y - 70.) ];
        rectangle (rgb 255 250 220) 6. 600. |> move_y (peak_y + 300.) |> fade 0.7 ]
    |> move_x (250. - (near * 250.))
  in
  let world =
    (* the far dunes: a band a little below the middle of the screen,
     * following the camera, rising and falling a third as much as the
     * ground: the parallax of height *)
    [ dunes far (fun x -> cam.y - 150. + (0.3 * (ground (x * 0.7) + 150.))) (seen.left - 40.) (seen.right + 40.) (seen.bottom - 400.);
      dunes sand ground (seen.left - 40.) (seen.right + 40.) (seen.bottom - 400.) ]
    @ List.map (fun x -> group [ rectangle (rgb 90 70 60) 8. 36. |> move_y 18.; rectangle (rgb 90 70 60) 20. 6. |> move_y 26. ] |> move x (ground x)) graves
    @ List.concat_map
        (fun b ->
          let glow = List.mem_assoc b g.lit in
          [ rectangle (rgb 70 50 40) 5. 120. |> move b (ground b + 60.);
            group (List.init 4 (fun i -> rectangle (if glow then rgb 255 200 90 else rgb 170 50 40) 26. 12. |> move (13. + (3. * sin ((float_of_int g.frames / 8.) + float_of_int i))) (-.float_of_int i * 14.)))
            |> move b (ground b + 110.) ])
        banners
    @ List.map (fun (x, h) -> group [ circle (rgb 255 240 170) 16. |> fade 0.5; circle (rgb 255 250 220) 9.; rectangle (rgb 200 140 40) 3. 12. ] |> move x (ground x + h)) g.glyphs_left
    @ (if g.stranger.met || Float.abs (g.stranger.w.x - g.me.x) < 1200. then view_walker (rgb 240 240 240) g.stranger.w g.frames else [])
    @ view_walker (rgb 180 40 40) g.me g.frames
    @ List.map (fun (x, y, a, mine) -> circle (if mine then rgb 255 240 180 else white) (float_of_int a * 4.) |> fade (0.6 - (float_of_int a / 70.)) |> move x (y + 40.)) g.rings
  in
  [ rectangle sky screen.width screen.height; mountain; Camera2d.view cam world ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      view_game screen (new_game ())
      @ [ text (rgb 120 40 30) 7. "TINY JOURNEY" |> move_y 300.; text (rgb 120 40 30) 2.2 "left/right walk   up hop   space held fly   x sing" |> move_y 240. ]
      @ Scene2d.blink 1. s [ text (rgb 120 40 30) 3. "PRESS SPACE" |> move_y 190. ]
  | Playing g -> view_game screen g
  | Summit (together, n) ->
      [ rectangle (rgb 255 252 240) screen.width screen.height; circle white (float_of_int n * 8.) |> fade 0.8;
        text (rgb 150 120 80) 4. "INTO THE LIGHT";
        text (rgb 150 120 80) 2.5 (if together then "you did not walk alone" else "you walked alone") |> move_y (-70.) ]
      @ if n > 90 then Scene2d.blink 1. s [ text (rgb 150 120 80) 2.5 "PRESS SPACE" |> move_y (-150.) ] else []

let app = game view update initial_model
let main = Playground_platform.run_app app