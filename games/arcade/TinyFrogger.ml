(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Frogger (Konami, 1981, published by Sega/Gremlin in
 * the west): get a frog across a road and then a river, five times, into
 * the five bays at the top. The road has cars and trucks you must not be
 * under; the river is death, except on the logs and the turtles floating
 * by, which carry you along -- off the edge of the screen if you stay on
 * too long. Arrows to hop, one hop per press.
 *
 * Frogger was one of the first games where the goal is to cross, not to
 * shoot: no gun, no enemy aiming at you, only traffic. It was a hit in
 * the arcades and on every home machine after, and it is still the
 * model of the "crossing" games, down to Crossy Road (Hipster Whale,
 * 2014), which is Frogger made endless. (Names and dates from memory,
 * to check.)
 *
 * It is a good game to read after TinyBreakout and TinyTron, and one idea
 * in it is new to the games of this directory:
 *
 *  - The lanes need no state. A lane is a few cars (or logs, or turtles)
 *    evenly repeating, all at the same constant speed, so where each one
 *    is at time t is a formula: x = x0 + v t, wrapped around the lane's
 *    period ([spans]). Nothing about the traffic is in the model; it
 *    holds the frog, the lives, the score, the bays filled, the timer,
 *    and the lanes' clock. Twenty-odd moving things, and not one of them
 *    is updated: they are drawn, and hit, where the formula puts them.
 *    The diving turtles too: whether they are under is another function
 *    of the same clock ([depth]). A pure function of time is how the
 *    Elm Playground's own [animation] works (see [spin], [wave]): it is
 *    a game made mostly of an animation. The same trick drives the
 *    obstacles of many old games, whose hardware had no memory to spare
 *    for a list of cars.
 *
 *  - Being carried: on a river row, the frog's x moves at the lane's
 *    speed ([carry]); nothing else is needed for "standing on a log",
 *    since the log's own position is, again, the formula.
 *
 *  - Moving by hops: a key press is a whole hop from one row to the next
 *    (or one tile sideways), drawn as a short tween ([hop_time]) during
 *    which the keys do nothing, like the grid moves of Snake and
 *    TinySokoban but with the in-between shown. The road can kill the
 *    frog in mid-hop, the water only once it has landed.
 *
 * What it uses: Scene2d (title, play, game over; and [Scene2d.pressed],
 * one hop per press, not one per frame the key is held), Audio (the hop,
 * the squash, the splash, the bay). Not Physics: a hop is a tween and
 * the traffic a formula. Not Tilemap: the rows are the only grid there
 * is, and each is a list of spans. No randomness either: the traffic is
 * the same every game, as in the original, and learning it is the game.
 *
 * Every time the five bays are filled, the traffic gets faster (the
 * level's [speedup]), from the same clock and the same formula.
 *
 * Left as exercises: the snake on the median and on the logs, the
 * crocodile that swims with its mouth in a bay, the lady frog to carry
 * home and the fly for bonus points (the original's extras, each a new
 * formula of time), lanes whose cars are not evenly spaced (a list of
 * offsets instead of a count), the best score kept between runs, and an
 * endless Crossy Road (rows made as you go up, as TinyFlappyBird.ml
 * makes its pipes).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The board *)
(*****************************************************************************)

(* 13 columns by 13 rows of 60 pixels: row 0 the start, 1 to 5 the road,
 * 6 the median, 7 to 11 the river, 12 the bays.
 *
 *   12  ####[ ]###[ ]###[ ]###[ ]###[ ]####    bays in the hedge
 *   11  ~~~~=====~~~~=====~~~~=====~~~~~~~~    logs  ->
 *   10  ~~oo~~~~~oo~~~~~~oo~~~~~~~~~~~~~~~~    turtles  <-
 *    9  ~~==========~~~~~~~~==========~~~~~    long logs  ->
 *    8  ~~~====~~~~~~====~~~~~~====~~~~~~~~    short logs  ->
 *    7  ~ooo~~~~~~~ooo~~~~~~~ooo~~~~~~~~~~~    turtles  <-
 *    6  ...................................    median, safe
 *    5  ====TRUCK======        ====TRUCK===    <-
 *    4        >car                             ->  fast
 *    3    car      car      car                <-
 *    2      car      car       car             ->
 *    1   car      car      car                 <-
 *    0  .................F.................    start
 *)
let tile = 60.
let columns = 13
let width = float_of_int columns * tile (* 780 *)
let field_left = -.width / 2.
let field_right = width / 2.

let row_y (row : int) : number = -360. + (tile * float_of_int row)

let start_row = 0
let median_row = 6
let home_row = 12
let on_road (row : int) : bool = row > start_row && row < median_row
let on_river (row : int) : bool = row > median_row && row < home_row

(* the five bays' centers, at the columns 0, 3, 6, 9 and 12 *)
let bays : number list = List.map (fun col -> field_left + (tile / 2.) + (tile * float_of_int col)) [ 0; 3; 6; 9; 12 ]

let hop_time = 0.15 (* seconds a hop takes *)
let frog_half = 20. (* the frog's hitbox against the cars: narrower than a tile *)
let time_per_frog = 30. (* seconds to get one frog home *)
let dying_time = 1.2 (* seconds the squashed or drowned frog stays shown *)
let dt = 1. / 60.

(*****************************************************************************)
(* The lanes: a formula of time *)
(*****************************************************************************)

type kind = Car of color | Truck | Log | Turtles

type lane = {
  row : int;
  kind : kind;
  len : number; (* each thing's length *)
  count : int; (* how many, evenly spaced over the period *)
  period : number; (* at least [width + len]: so a thing has left the field before it comes back *)
  speed : number; (* pixels per second, < 0 going left *)
}

let lanes : lane list =
  [ { row = 1; kind = Car yellow; len = 60.; count = 3; period = 900.; speed = -60. };
    { row = 2; kind = Car green; len = 60.; count = 3; period = 900.; speed = 50. };
    { row = 3; kind = Car purple; len = 60.; count = 3; period = 900.; speed = -80. };
    { row = 4; kind = Car white; len = 60.; count = 1; period = 1000.; speed = 170. };
    { row = 5; kind = Truck; len = 120.; count = 2; period = 1000.; speed = -70. };
    { row = 7; kind = Turtles; len = 180.; count = 3; period = 1100.; speed = -70. };
    { row = 8; kind = Log; len = 150.; count = 3; period = 1000.; speed = 50. };
    { row = 9; kind = Log; len = 300.; count = 2; period = 1200.; speed = 110. };
    { row = 10; kind = Turtles; len = 120.; count = 3; period = 1000.; speed = -80. };
    { row = 11; kind = Log; len = 210.; count = 3; period = 1100.; speed = 70. } ]

let lane_of_row (row : int) : lane option = List.find_opt (fun l -> l.row = row) lanes

(* the traffic gets faster with each level: 1, 1.3, 1.6, ... *)
let speedup (level : int) : number = 1. + (0.3 * float_of_int (level -.. 1))

(* [wrap a p]: a modulo p, in [0, p) even for a negative a (OCaml's
 * Float.rem keeps a's sign) *)
let wrap (a : number) (p : number) : number = a - (p * Float.floor (a / p))

(* The whole idea of the game: where the things of a lane are at time t,
 * as (left, right) spans. Thing i starts at i * period / count, moves at
 * the lane's speed, and wraps around the period; a wrapped position of 0
 * puts the thing just off the left edge, one of period - len just off
 * the right edge (hence a period at least width + len: there is always
 * a moment when a thing is off the field entirely, and that's when it
 * jumps from one side to the other, unseen).
 *
 *      off-field           the field            off-field
 *   |<- len ->|<-------- width ---------->|
 *   +---------+---------------------------+-------------+
 *   0                                                period
 *
 * E.g. the lane of row 4 (one car of 60 at 170 pixels per second, a
 * period of 1000): at t = 0 the car is at [-450, -390], just off the
 * left edge; at t = 2 it is 340 further, at [-110, -50]; at t = 1000 /
 * 170 = 5.88 it is back where it was at 0. *)
let spans (level : int) (t : number) (l : lane) : (number * number) list =
  List.init l.count (fun i ->
      let x0 = l.period * float_of_int i / float_of_int l.count in
      let left = field_left - l.len + wrap (x0 + (l.speed * speedup level * t)) l.period in
      (left, left + l.len))

(* How far under water a group of turtles is at time t, from 0 (afloat)
 * to 1 (gone): the first group of each turtle lane dives, a cycle of 6
 * seconds -- 4 afloat, half a second going down, a second under, half a
 * second coming up. A second periodic function of the same clock. *)
let depth (t : number) : number =
  let c = wrap t 6. in
  if c < 4. then 0. else if c < 4.5 then (c - 4.) * 2. else if c < 5.5 then 1. else (6. - c) * 2.

let diving (l : lane) (i : int) : bool = l.kind = Turtles && i = 0

(* whether a frog at x is held up by something of the river lane l:
 * its center on a log, or on turtles not under water *)
let floating (level : int) (t : number) (l : lane) (x : number) : bool =
  spans level t l
  |> List.mapi (fun i s -> (i, s))
  |> List.exists (fun (i, (left, right)) -> x > left && x < right && not (diving l i && depth t > 0.5))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* The frog is on [row] at [x] (not always a column's center: the logs
 * carry it off the grid); during a hop, it comes from [from_row] and
 * [from_x], [hop] going from 0 to 1. *)
type frog = {
  x : number;
  row : int;
  from_x : number;
  from_row : int;
  hop : number; (* 1 once landed *)
  facing : number; (* degrees, for the picture: 0 is up *)
}

type death = Squashed | Drowned | Timeout

type game = {
  frog : frog;
  clock : number; (* the lanes' time, since the level started *)
  level : int;
  lives : int; (* the frogs still waiting, the one playing not counted *)
  score : int;
  homes : bool list; (* the five bays, filled or not *)
  furthest : int; (* the highest row this frog reached: 10 points a new row *)
  time_left : number;
  dying : (death * number) option; (* how it died, and for how long it has been dead *)
}

type scene = Title | Playing of game | Game_over of game

type model = { scenes : scene Scene2d.t; best : int }

let new_frog : frog = { x = 0.; row = start_row; from_x = 0.; from_row = start_row; hop = 1.; facing = 0. }

let new_game : game =
  { frog = new_frog;
    clock = 0.;
    level = 1;
    lives = 2;
    score = 0;
    homes = [ false; false; false; false; false ];
    furthest = start_row;
    time_left = time_per_frog;
    dying = None }

let initial_model : model = { scenes = Scene2d.start Title; best = 0 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let hop_sound = Audio.square 300. |> Audio.sliding 500. |> Audio.lasting 0.05 |> Audio.fading
let splash_sound = Audio.noise 3000. |> Audio.lasting 0.4 |> Audio.fading

(* where the frog is drawn, and hit by cars: between where it comes from
 * and where it goes *)
let frog_x (f : frog) : number = f.from_x + ((f.x - f.from_x) * f.hop)
let frog_row (f : frog) : int = if f.hop < 0.5 then f.from_row else f.row

(* A press of an arrow, while landed, starts a hop: a row up or down, or a
 * tile sideways, never off the board. *)
let start_hop (scenes : scene Scene2d.t) (f : frog) : frog =
  let hop dx drow facing =
    let x = f.x + (dx * tile) and row = f.row +.. drow in
    if row < start_row || row > home_row || x < field_left + (tile / 3.) || x > field_right - (tile / 3.) then f
    else begin
      Audio.play hop_sound;
      { x; row; from_x = f.x; from_row = f.row; hop = 0.; facing }
    end
  in
  if f.hop < 1. then f
  else if Scene2d.pressed (fun k -> k.kup) scenes then hop 0. 1 0.
  else if Scene2d.pressed (fun k -> k.kdown) scenes then hop 0. (-1) 180.
  else if Scene2d.pressed (fun k -> k.kleft) scenes then hop (-1.) 0 90.
  else if Scene2d.pressed (fun k -> k.kright) scenes then hop 1. 0 (-90.)
  else f

(* On a river row, landed, the frog moves with the lane, whatever carries
 * it: [floating] said it was on something. *)
let carry (level : int) (f : frog) : frog =
  match lane_of_row f.row with
  | Some l when on_river f.row && f.hop >= 1. ->
      let dx = l.speed * speedup level * dt in
      { f with x = f.x + dx; from_x = f.from_x + dx }
  | _ -> f

(* how the frog dies now, if it does *)
let death (g : game) : death option =
  let f = g.frog in
  let x = frog_x f and row = frog_row f in
  let hit_by_car =
    on_road row
    &&
    match lane_of_row row with
    | Some l -> List.exists (fun (left, right) -> x + frog_half > left && x - frog_half < right) (spans g.level g.clock l)
    | None -> false
  in
  let in_water =
    f.hop >= 1. && on_river f.row
    && (match lane_of_row f.row with Some l -> not (floating g.level g.clock l f.x) | None -> false)
  in
  if hit_by_car then Some Squashed
  else if in_water || x < field_left || x > field_right then Some Drowned
  else if g.time_left <= 0. then Some Timeout
  else None

(* The frog landed in the bays' row: in an empty bay, a frog home; against
 * the hedge, or in a bay already taken, it dies. *)
let arrive (g : game) : game =
  let near = List.map (fun bx -> Float.abs (g.frog.x - bx) < tile * 0.6) bays in
  match List.combine near g.homes |> List.mapi (fun i nh -> (i, nh)) |> List.find_opt (fun (_, (n, _)) -> n) with
  | Some (i, (_, false)) ->
      Audio.play Audio.coin;
      let homes = List.mapi (fun j h -> h || j = i) g.homes in
      let score = g.score +.. 50 +.. (10 *.. int_of_float g.time_left) in
      if List.for_all Fun.id homes then
        (* the five: a bonus, and the next level, faster *)
        { g with frog = new_frog; homes = [ false; false; false; false; false ]; score = score +.. 1000; level = g.level +.. 1;
          clock = 0.; furthest = start_row; time_left = time_per_frog }
      else { g with frog = new_frog; homes; score; furthest = start_row; time_left = time_per_frog }
  | _ ->
      Audio.play Audio.hit;
      { g with dying = Some (Squashed, 0.) }

let update_game (scenes : scene Scene2d.t) (g : game) : game =
  (* the lanes go on, whatever happens to the frog *)
  let g = { g with clock = g.clock + dt } in
  match g.dying with
  | Some (how, since) ->
      if since + dt < dying_time then { g with dying = Some (how, since + dt) }
      else { g with dying = None; frog = new_frog; furthest = start_row; time_left = time_per_frog; lives = g.lives -.. 1 }
  | None ->
      let f = start_hop scenes g.frog in
      let f = { f with hop = min 1. (f.hop + (dt / hop_time)) } in
      let f = carry g.level f in
      let g = { g with frog = f; time_left = g.time_left - dt } in
      let g =
        if f.hop >= 1. && f.row > g.furthest then { g with furthest = f.row; score = g.score +.. (if f.row = home_row then 0 else 10) }
        else g
      in
      if f.hop >= 1. && f.row = home_row then arrive g
      else begin
        match death g with
        | Some how ->
            Audio.play (if how = Drowned then splash_sound else Audio.hit);
            { g with dying = Some (how, 0.) }
        | None -> g
      end

let any_key (scenes : scene Scene2d.t) : bool =
  Scene2d.pressed (fun k -> k.kspace) scenes || Scene2d.pressed (fun k -> k.kup) scenes

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  match scenes.scene with
  | Title -> if any_key scenes then { model with scenes = Scene2d.go (Playing new_game) scenes } else { model with scenes }
  | Playing g ->
      let g = update_game scenes g in
      let best = max model.best g.score in
      if g.lives < 0 then { best; scenes = Scene2d.go (Game_over g) scenes } else { best; scenes = { scenes with scene = Playing g } }
  | Game_over g ->
      (* the traffic goes on behind the GAME OVER *)
      let g = { g with clock = g.clock + dt } in
      if scenes.elapsed > 1. && any_key scenes then { model with scenes = Scene2d.go (Playing new_game) scenes }
      else if scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes }
      else { model with scenes = { scenes with scene = Game_over g } }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let water = rgb 0 0 72
let road = rgb 20 20 20
let verge = rgb 110 60 160 (* the purple sidewalks of the original *)
let hedge = rgb 0 140 60
let log_brown = rgb 140 80 30
let frog_green = rgb 60 220 60

(* one thing of a lane, between left and right, going right if [speed]
 * > 0 (the cars' fronts, the turtles' heads) *)
let view_thing (l : lane) (depth : number) ((left, right) : number * number) : shape =
  let x = (left + right) / 2. and y = row_y l.row in
  let front = if l.speed > 0. then 1. else -1. in
  let s =
    match l.kind with
    | Car color ->
        group
          [ rectangle black 14. 50. |> move_x (-18.); rectangle black 14. 50. |> move_x 18.;
            rectangle color 54. 36.; rectangle (rgb 150 200 255) 14. 28. |> move_x (front * 10.) ]
    | Truck ->
        group
          [ rectangle white (l.len - 30.) 44. |> move_x (front * -15.);
            rectangle red 26. 40. |> move_x (front * ((l.len / 2.) - 14.)) ]
    | Log ->
        group
          [ circle log_brown 22. |> move_x ((l.len / 2.) - 22.);
            circle log_brown 22. |> move_x (22. - (l.len / 2.));
            rectangle log_brown (l.len - 44.) 44.;
            rectangle (rgb 110 60 20) (l.len - 60.) 4. |> move_y 8.;
            circle (rgb 200 140 80) 16. |> move_x ((l.len / 2.) - 22.) ]
    | Turtles ->
        let n = int_of_float (l.len / tile) in
        group
          (List.init n (fun i ->
               let tx = (tile * float_of_int i) - (l.len / 2.) + (tile / 2.) in
               group [ circle frog_green 8. |> move_x (front * 24.); circle red 24.; circle (rgb 150 30 30) 14. ] |> move_x tx))
        |> fade (1. - (0.85 * depth))
  in
  s |> move x y

let view_lanes (level : int) (t : number) : shape list =
  List.concat_map
    (fun l -> spans level t l |> List.mapi (fun i s -> view_thing l (if diving l i then depth t else 0.) s))
    lanes

let view_frog (f : frog) : shape =
  let lift = 1. + (0.25 * Float.sin (Float.pi * f.hop)) (* bigger in mid-air *) in
  group
    [ oval frog_green 22. 14. |> move (-18.) (-14.); oval frog_green 22. 14. |> move 18. (-14.);
      oval frog_green 14. 18. |> move (-16.) 14.; oval frog_green 14. 18. |> move 16. 14.;
      oval frog_green 34. 40.;
      circle white 7. |> move (-9.) 14.; circle white 7. |> move 9. 14.;
      circle black 3. |> move (-9.) 16.; circle black 3. |> move 9. 16. ]
  |> rotate f.facing |> scale lift
  |> move (frog_x f) (row_y f.from_row + ((row_y f.row - row_y f.from_row) * f.hop))

(* where the frog died: tyre tracks, or rings in the water *)
let view_death (f : frog) (how : death) (since : number) : shape =
  let at s = s |> move f.x (row_y f.row) in
  match how with
  | Drowned -> at (group [ circle (rgb 120 160 255) (10. + (40. * since)) |> fade (1. - (since / dying_time)); circle water 8. ])
  | Squashed | Timeout ->
      at (group [ rectangle white 50. 8. |> rotate 45.; rectangle white 50. 8. |> rotate (-45.); circle red 8. ])

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the board, the traffic at time t, the bays filled *)
let view_board (level : int) (t : number) (homes : bool list) : shape list =
  let band color r0 r1 = rectangle color width (tile * float_of_int (r1 -.. r0 +.. 1)) |> move_y ((row_y r0 + row_y r1) / 2.) in
  [ band road 1 5; band water 7 11; band verge 0 0; band verge 6 6; band hedge 12 12 ]
  @ List.map2
      (fun bx filled ->
        group ((rectangle water (tile * 0.9) (tile * 0.9)) :: (if filled then [ circle frog_green 20.; circle white 5. |> move 0. 8. ] else []))
        |> move bx (row_y home_row))
      bays homes
  @ view_lanes level t

(* what goes past the field's edges is hidden, as on the arcade's screen *)
let view_edges (screen : screen) : shape list =
  let side = (screen.width - width) / 2. + 1. in
  [ rectangle black side screen.height |> move_x (field_left - (side / 2.));
    rectangle black side screen.height |> move_x (field_right + (side / 2.)) ]

let view_hud (g : game) : shape list =
  let lives = List.init (max 0 g.lives) (fun i -> circle frog_green 12. |> move (field_left + 15. + (30. * float_of_int i)) 440.) in
  let bar = 500. * max 0. g.time_left / time_per_frog in
  [ text white 3. (Printf.sprintf "SCORE %d" g.score) |> move (-230.) 470.;
    text white 3. (Printf.sprintf "LEVEL %d" g.level) |> move 250. 470.;
    text yellow 2.5 "TIME" |> move (field_right - 50.) (-430.);
    rectangle (if g.time_left < 8. then red else green) bar 20. |> move ((field_right - 110.) - (bar / 2.)) (-430.) ]
  @ lives

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let scenes = model.scenes in
  rectangle black screen.width screen.height
  ::
  (match scenes.scene with
  | Title ->
      view_board 1 scenes.elapsed new_game.homes
      @ view_edges screen
      @ [ text green 6. "TINY FROGGER" |> move_y 440.;
          text white 2.5 "arrows: hop, one hop a press" |> move_y (-430.) ]
      @ Scene2d.blink 1. scenes [ text white 3. "PRESS SPACE" |> move_y (row_y median_row) ]
  | Playing g ->
      let frog = match g.dying with Some (how, since) -> view_death g.frog how since | None -> view_frog g.frog in
      view_board g.level g.clock g.homes @ [ frog ] @ view_edges screen @ view_hud g
  | Game_over g ->
      view_board g.level g.clock g.homes
      @ view_edges screen @ view_hud g
      @ [ rectangle black 500. 170. |> move_y (row_y median_row);
          text red 6. "GAME OVER" |> move_y (row_y median_row + 30.);
          text white 3. (Printf.sprintf "BEST %d" model.best) |> move_y (row_y median_row - 40.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
