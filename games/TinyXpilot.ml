(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of XPilot (Bjørn Stabell and Ken Ronny Schouten,
 * University of Tromsø, 1991): ships in a cave, with inertia and
 * gravity, cannons in the walls, fuel stations, and the ball game --
 * fly to the other team's ball, catch it on your connector, and haul it
 * back onto your treasure box, the ball swinging under the ship.
 *
 * One player: the red team's ball is down in the cave, guarded by
 * cannons; bring it home three times. Left/right to turn, up to thrust,
 * down for the shield, space to fire, b to let the ball go.
 *
 * Two players, on one keyboard: blue (a/d turn, w thrust, s shield,
 * space fire, x let go) against red (the arrows, down the shield, enter
 * fire, / let go), on a map the same for both, mirrored. Steal the
 * other's ball, shoot the one who stole yours (the ball drops, and goes
 * home when left alone); three balls wins. The camera frames both ships,
 * zooming out as they part ([frame], as Super Smash Bros. does).
 *
 * XPilot was one of the first games played over the Internet: a server,
 * and clients on the X terminals of the universities' labs, dozens of
 * players in the same cave, teams, robots, and hundreds of maps written
 * as ASCII files, walls and slopes and cannons and fuel -- like
 * [solo_map] and [duel_map] below. Its ancestors: Gravitar (Atari, 1982:
 * gravity and caves) and Thrust (Jeremy Smith, 1986, on the BBC Micro
 * and the C64), where the ship hauls a pod on a rod out of a planet: the
 * ball game is Thrust's pod, multiplayer. (Names and dates from memory,
 * to check.)
 *
 * Three ideas in it are new to the games of this directory:
 *
 *  - Newton's third law, by hand: the connector is a rope, a spring
 *    that only pulls, and only when stretched ([rope_pull]); the same
 *    force pulls the ship towards the ball and the ball towards the
 *    ship, the other way ([Physics.push] with opposite signs). The ball
 *    being twice as heavy, it moves half as much (F = m a): feel it
 *    swing the ship when you turn, and drag it down when you stop
 *    thrusting.
 *
 *  - Crashing is a jolt: a ship bouncing off a wall has its velocity
 *    changed at once, and the size of that change says how hard it hit
 *    ([hit_walls]). Landing softly or sliding along a wall changes it a
 *    little, ramming it a lot: above [crash], the ship explodes. What
 *    kills pilots in real life too: not the speed, the deceleration.
 *
 *  - Cannons aim ahead ([intercept]): a shot at speed s meets a target
 *    at d moving at v after a time t with |d + v t| = s t, a quadratic
 *    in t. Fly straight at a steady speed, and the cannons hit you; turn
 *    and thrust, and they miss.
 *
 * What it uses: Tilemap (the maps), Camera2d (follow, clamp; the radar
 * is the walls again, scaled down), Scene2d (title, play, won), and
 * Physics: the ships ([upright]: a wall never spins them), the balls and
 * the walls are bodies ([immovable] walls, [bounce_off] them, [turn],
 * [thrust], [fall], [push], [shot_from], [touching]); underneath,
 * physics/2d/'s Body, Integrate, Shape, Collide and Resolve. Not a world
 * and its solver (nothing piles up), not Physics.pulled_to (a spring to
 * a fixed point; the rope links two moving bodies), not went_through
 * (the shots, 9 pixels a frame, are slower than a wall is thick). The
 * shots feel no gravity, so that the cannons' aim is exact.
 *
 * The flag hitboxes draws what the physics sees.
 *
 * Exercises: a split screen, a camera for each player (the playground
 * can't clip a view to its half: each shape would have to be cut, e.g.
 * with Sutherland and Hodgman's polygon clipping, 1974), robots
 * (tests/games/Unit_games.ml has one, flying from waypoint to
 * waypoint), real pilots over the network (plan_networking_teaching.md),
 * XPilot's wormholes, its items (afterburners, missiles, cloaking), the
 * refueling key rather than refueling by just being near, maps read from
 * files in XPilot's own format.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The maps *)
(*****************************************************************************)

(* x walls; q w a s slopes, the letter saying which corner of the tile
 * is filled, as the keys sit on a keyboard (q the top-left, s the
 * bottom-right); c cannons, # fuel; 1 and 2 the blue and red bases,
 * * and % their treasure boxes, B and R their balls *)
let solo_map : string list =
  [ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    "x            wxxxxxxxxxxxxxxq          x";
    "x             wxxxxxxxxxxxxq           x";
    "x              wxxxxxxxxxxq            x";
    "x                                      x";
    "x    1     #                           x";
    "x**xxxxxxxxxxxxxxxxxxa       sxxxxa    x";
    "xxxxxxxxxxxxxxxxxxxxxxa     sxxxxxxa   x";
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx   x";
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxcxxxx   x";
    "x                                      x";
    "x                                      x";
    "x                                      x";
    "x         sca             sa           x";
    "x        sxxxa           sxxa          x";
    "x       sxxxxxa         sxxxxa         c";
    "x                                      x";
    "x                                      x";
    "x   R                                  x";
    "x  xxx                           #     x";
    "xxxxxxxxxxxxcxxxxxxxxxxxxxxxxxxxxxxxxxxx" ]

(* the same for both teams: the right half is the left one mirrored *)
let duel_map : string list =
  [ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    "x                       xxxxxxxx                       x";
    "x                       xxxxxxxx                       x";
    "x B   1       #         xxxxxxxx         #       2   R x";
    "x***xxxxxxxxxxxxxa      xxxxxxxx      sxxxxxxxxxxxxx%%%x";
    "xxxxxxxxxxxxxxxxxxa     wxxxxxxq     sxxxxxxxxxxxxxxxxxx";
    "xxxxxxxxxxxxxxxxxxxa     wxxxxq     sxxxxxxxxxxxxxxxxxxx";
    "x                         wccq                         x";
    "x                                                      x";
    "x                                                      x";
    "x        sca                                sca        x";
    "x       sxxxa                              sxxxa       x";
    "x      sxxxxxa                            sxxxxxa      x";
    "x                                                      x";
    "x                                                      x";
    "x                     sa        sa                     x";
    "x                    sxxa      sxxa                    x";
    "x            #      sxxxxa    sxxxxa      #            x";
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ]

let tile = 40.
let h = tile / 2.

let is_block (c : char) : bool = match c with 'x' | 'c' | '#' | '*' | '%' -> true | _ -> false
let is_slope (c : char) : bool = match c with 'q' | 'w' | 'a' | 's' -> true | _ -> false

(* a slope's triangle, around its tile's center, counterclockwise:
 *
 *    q      w      a      s
 *   +--+   +--+   +      +   (x the filled corner)
 *   |x/     \x|   |\      /|
 *   |/       \|   |x\    /x|
 *   +         +   +--+  +--+
 *)
let triangle (c : char) : (number * number) list =
  let tl = (-.h, h) and tr = (h, h) and bl = (-.h, -.h) and br = (h, -.h) in
  match c with 'q' -> [ tl; bl; tr ] | 'w' -> [ tl; br; tr ] | 'a' -> [ tl; bl; br ] | _ -> [ bl; br; tr ]

type team = Blue | Red

let team_color (t : team) : color = match t with Blue -> rgb 80 150 255 | Red -> rgb 255 80 70

type cannon = {
  mx : number; (* its muzzle, on the side of its tile facing the cave *)
  my : number;
  facing : number * number;
  reload : int; (* frames before it can fire *)
  down : int; (* frames before it's rebuilt, 0 when it's up *)
}

(* a map, and what's found in it *)
type level = {
  map : Tilemap.t;
  walls : (Physics.body * number) list; (* with their half width *)
  bases : (team * (number * number)) list;
  treasures : (team * (number * number)) list; (* each tile of them *)
  homes : (team * (number * number)) list; (* where each team's ball sits *)
  fuel_stations : (number * number) list;
  cannons : cannon list;
}

let wall_color = rgb 40 60 190

(* The walls, for the physics and to draw them: each row's runs of
 * blocks as one rectangle (drawn tile by tile, the tiles' edges would
 * show), each slope a triangle; with their half width, to find the ones
 * near a body *)
let make_walls (map : Tilemap.t) : (Physics.body * number) list =
  List.concat
    (List.mapi
       (fun row line ->
         let n = String.length line in
         let rec go col acc =
           if col >= n then acc
           else if is_block line.[col] then
             let rec last c = if c +.. 1 < n && is_block line.[c +.. 1] then last (c +.. 1) else c in
             let e = last col in
             let x0, y = Tilemap.center map col row and x1, _ = Tilemap.center map e row in
             let w = x1 - x0 + tile in
             go (e +.. 1) ((Physics.body (rectangle wall_color w tile) |> Physics.at ((x0 + x1) / 2.) y |> Physics.immovable, w / 2.) :: acc)
           else if is_slope line.[col] then
             let x, y = Tilemap.center map col row in
             go (col +.. 1) ((Physics.body (polygon wall_color (triangle line.[col])) |> Physics.at x y |> Physics.immovable, h) :: acc)
           else go (col +.. 1) acc
         in
         go 0 [])
       (Tilemap.to_strings map))

(* a cannon faces the first open side of its tile: below, above, left,
 * right *)
let make_cannons (map : Tilemap.t) : cannon list =
  List.mapi
    (fun i (col, row) ->
      let open_ (dc, dr) = match Tilemap.get map (col +.. dc) (row +.. dr) with Some ' ' -> true | _ -> false in
      let dc, dr = List.find open_ [ (0, 1); (0, -1); (-1, 0); (1, 0) ] in
      let fx = float_of_int dc and fy = -.float_of_int dr in
      let x, y = Tilemap.center map col row in
      { mx = x + (fx * h); my = y + (fy * h); facing = (fx, fy); reload = 40 *.. i; down = 0 })
    (Tilemap.find map 'c')

let load (rows : string list) : level =
  let map = Tilemap.of_strings tile rows in
  let places c = List.map (fun (col, row) -> Tilemap.center map col row) (Tilemap.find map c) in
  let teams blue red = List.map (fun p -> (Blue, p)) (places blue) @ List.map (fun p -> (Red, p)) (places red) in
  { map; walls = make_walls map; bases = teams '1' '2'; treasures = teams '*' '%'; homes = teams 'B' 'R';
    fuel_stations = places '#'; cannons = make_cannons map }

let solo = load solo_map
let duel = load duel_map

(* whether the world point (x, y) is in a wall: a block, or the filled
 * half of a slope (u and v from -1 to 1 across the tile: the diagonal
 * of q and s is v = u, the one of w and a is v = -u) *)
let solid (lv : level) (x : number) (y : number) : bool =
  match Tilemap.tile_at lv.map x y with
  | Some c when is_block c -> true
  | Some c when is_slope c -> (
      let col, row = Tilemap.cell lv.map x y in
      let cx, cy = Tilemap.center lv.map col row in
      let u = (x - cx) / h and v = (y - cy) / h in
      match c with 'q' -> v > u | 's' -> v < u | 'w' -> u + v > 0. | _ -> u + v < 0.)
  | _ -> false

(* the walls in a rectangle *)
let walls_in (lv : level) (r : Camera2d.rect) : Physics.body list =
  List.filter_map
    (fun ((w : Physics.body), half) -> if w.x + half > r.left && w.x - half < r.right && w.y + h > r.bottom && w.y - h < r.top then Some w else None)
    lv.walls

(* the walls near a body: the only ones worth testing *)
let near (lv : level) (b : Physics.body) : Physics.body list = walls_in lv { left = b.x - 50.; right = b.x + 50.; bottom = b.y - 50.; top = b.y + 50. }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type ship = {
  body : Physics.body;
  fuel : number;
  dead : int option; (* exploded, n frames ago *)
}

type shot = { shot : Physics.body; ttl : int (* frames left *) }

(* a pilot's keys *)
type controls = {
  left : keyboard -> bool;
  right : keyboard -> bool;
  thrust : keyboard -> bool;
  shield : keyboard -> bool;
  fire : keyboard -> bool;
  let_go : keyboard -> bool;
}

let key (k : string) (kb : keyboard) : bool = Set_.mem k kb.keys

(* one player: the arrows, space, b; two: w a s d, space and x on the
 * left of the keyboard, the arrows, enter and / on the right (enter is
 * "return" for SDL, "Enter" for the browsers) *)
let solo_keys = { left = (fun k -> k.kleft); right = (fun k -> k.kright); thrust = (fun k -> k.kup); shield = (fun k -> k.kdown); fire = (fun k -> k.kspace); let_go = key "b" }
let left_keys = { left = (fun k -> k.ka); right = (fun k -> k.kd); thrust = (fun k -> k.kw); shield = (fun k -> k.ks); fire = (fun k -> k.kspace); let_go = key "x" }
let right_keys = { solo_keys with fire = (fun k -> key "return" k || key "Enter" k); let_go = key "/" }

type pilot = { team : team; controls : controls; ship : ship; shots : shot list; score : int; deaths : int }

type ball = {
  owner : team; (* whose treasure it sits on *)
  ball : Physics.body;
  holder : team option; (* the pilot whose connector holds it *)
  grab_wait : int; (* frames before a connector can catch it again *)
  free : int; (* frames since it was let go *)
}

type game = {
  lv : level;
  pilots : pilot list;
  balls : ball list;
  bullets : shot list; (* the cannons' *)
  cannons : cannon list;
  frames : int;
  cam : Camera2d.t;
}

type scene = Title | Playing of game | Won of game
type model = scene Scene2d.t

let gravity = 60. (* pixels per second, per second: XPilot's caves pull gently *)
let fuel_max = 150.
let balls_to_win = 3

let ship_shape = polygon white [ (16., 0.); (-12., 11.); (-12., -11.) ]

let new_ship (lv : level) (t : team) : ship =
  let x, y = List.assoc t lv.bases in
  { body = Physics.body ship_shape |> Physics.at x (y - 4.) |> Physics.pointing 90. |> Physics.upright |> Physics.bouncy 0.4; fuel = fuel_max; dead = None }

let new_ball (lv : level) (owner : team) : ball =
  let x, y = List.assoc owner lv.homes in
  { owner; ball = Physics.body (circle (team_color owner) 10.) |> Physics.at x y |> Physics.heavy 2. |> Physics.bouncy 0.5; holder = None; grab_wait = 0; free = 0 }

let new_game (players : int) : game =
  let lv = if players = 1 then solo else duel in
  let pilot t controls = { team = t; controls; ship = new_ship lv t; shots = []; score = 0; deaths = 0 } in
  let x, y = List.assoc Blue lv.bases in
  { lv; pilots = (if players = 1 then [ pilot Blue solo_keys ] else [ pilot Blue left_keys; pilot Red right_keys ]);
    balls = List.map (fun (t, _) -> new_ball lv t) lv.homes; bullets = []; cannons = lv.cannons; frames = 0;
    cam = { (Camera2d.origin |> Camera2d.look_at x y) with zoom = 1.5 } }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The rope *)
(*****************************************************************************)

let rope_length = 110.
let stiffness = 60. (* the pull per pixel of stretch *)
let rope_damping = 8. (* the pull per pixel per second of stretching *)

(* [rope_pull ship ball]: the force pulling the ship towards the ball
 * (the ball is pulled the other way, as hard): nothing while the rope
 * is slack, then a spring, k times the stretch (Hooke's law), plus a
 * damper resisting the stretching, so that the ball doesn't bounce on
 * its rope forever; and a rope never pushes. E.g. the ball 120 pixels
 * right of the ship, both still: stretched by 10, a pull of 600 to the
 * right on the ship, 600 to the left on the ball -- which, twice as
 * heavy, accelerates at 300. Hanging still under the ship, the ball
 * stretches it by 2 (its weight, 2 x 60, over k). *)
let rope_pull (s : Physics.body) (b : Physics.body) : number * number =
  let dx = b.x - s.x and dy = b.y - s.y in
  let d = Float.hypot dx dy in
  if d <= rope_length then (0., 0.)
  else
    let nx = dx / d and ny = dy / d in
    let stretching = ((b.vx - s.vx) * nx) + ((b.vy - s.vy) * ny) in
    let f = Float.max 0. ((stiffness * (d - rope_length)) + (rope_damping * stretching)) in
    (f * nx, f * ny)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let crash = 300. (* a velocity change, in pixels per second, that breaks the ship *)
let thrust_power = 320.
let turn_speed = 250.

(* [hit_walls lv b]: [b] bounced off the walls it touches, and how hard
 * it hit: the biggest change of its velocity. With the ship's
 * bounciness of 0.4, hitting a wall head on at v changes the velocity
 * by 1.4 v: above 214 pixels per second, a crash. Landing at 60 (after
 * a fall of 30 pixels: v^2 = 2 g d), a jolt of 84, fine; falling from
 * 380 pixels, almost 10 tiles, without thrusting: a crash. *)
let hit_walls (lv : level) (b : Physics.body) : Physics.body * number =
  List.fold_left
    (fun ((b : Physics.body), jolt) w ->
      let (b' : Physics.body) = Physics.bounce_off w b in
      (b', Float.max jolt (Float.hypot (b'.vx - b.vx) (b'.vy - b.vy))))
    (b, 0.) (near lv b)

let refueling (lv : level) (s : ship) : bool =
  s.dead = None && List.exists (fun (x, y) -> Float.hypot (x - s.body.x) (y - s.body.y) < 110.) lv.fuel_stations

let shielded (keys : keyboard) (p : pilot) : bool = p.ship.dead = None && p.controls.shield keys && p.ship.fuel > 0.

let fly (lv : level) (keys : keyboard) (c : controls) (pull : number * number) (s : ship) : ship =
  let thrusting = c.thrust keys && s.fuel > 0. and shielding = c.shield keys && s.fuel > 0. in
  let turning = (if c.left keys then turn_speed else 0.) - if c.right keys then turn_speed else 0. in
  let body, jolt =
    s.body |> Physics.turn turning
    |> Physics.thrust (if thrusting then thrust_power else 0.)
    |> Physics.fall gravity
    |> Physics.push (fst pull) (snd pull)
    |> Physics.step |> hit_walls lv
  in
  let fuel = s.fuel - (if thrusting then 3. / 60. else 0.) - if shielding then 8. / 60. else 0. in
  let fuel = if refueling lv s then Float.min fuel_max (fuel + (30. / 60.)) else fuel in
  { body; fuel = Float.max 0. fuel; dead = (if jolt > crash then Some 0 else None) }

(* [intercept dx dy vx vy speed]: when a shot at [speed], fired now from
 * (0, 0), can meet a target at (dx, dy) moving at (vx, vy): the time t
 * with |d + v t| = speed t, squared:
 *   (v.v - speed^2) t^2 + 2 (d.v) t + d.d = 0
 * the smallest positive root, None if the target outruns the shot. E.g.
 * a ship 300 to the right, going up at 160, a shot at 200: t^2 (160^2 -
 * 200^2) + 300^2 = 0, t = 2.5 s, aimed at (300, 400), 500 away: 200 x
 * 2.5 (a 3-4-5 triangle). *)
let intercept (dx : number) (dy : number) (vx : number) (vy : number) (speed : number) : number option =
  let a = (vx * vx) + (vy * vy) - (speed * speed) and b = 2. * ((dx * vx) + (dy * vy)) and c = (dx * dx) + (dy * dy) in
  let roots =
    if Float.abs a < 1e-6 then if b < 0. then [ -.c / b ] else []
    else
      let disc = (b * b) - (4. * a * c) in
      if disc < 0. then [] else [ (-.b - sqrt disc) / (2. * a); (-.b + sqrt disc) / (2. * a) ]
  in
  match List.filter (fun t -> t > 0.) roots with [] -> None | ts -> Some (List.fold_left Float.min infinity ts)

(* nothing of the walls between two points: sampled every 10 pixels *)
let in_sight (lv : level) (x0 : number) (y0 : number) (x1 : number) (y1 : number) : bool =
  let n = int_of_float (Float.hypot (x1 - x0) (y1 - y0) / 10.) in
  List.for_all
    (fun k ->
      let f = float_of_int k / float_of_int n in
      not (solid lv (x0 + (f * (x1 - x0))) (y0 + (f * (y1 - y0)))))
    (List.init (max 0 (n -.. 1)) (fun k -> k +.. 1))

let bullet_speed = 280.
let bullet_shape = circle orange 4.

(* a cannon fires when it's up, loaded, and sees a ship in front of it,
 * not too far (the nearest such ship): at where the ship will be *)
let fire_cannon (lv : level) (ships : ship list) (c : cannon) : cannon * shot list =
  if c.down > 0 then ({ c with down = c.down -.. 1 }, [])
  else if c.reload > 0 then ({ c with reload = c.reload -.. 1 }, [])
  else
    let fx, fy = c.facing in
    let target (s : ship) =
      let dx = s.body.x - c.mx and dy = s.body.y - c.my in
      let d = Float.hypot dx dy in
      if s.dead <> None || d > 520. || ((dx * fx) + (dy * fy)) / d < 0.2 || not (in_sight lv c.mx c.my s.body.x s.body.y) then None
      else Some (d, s)
    in
    match List.sort (fun (d1, _) (d2, _) -> compare d1 d2) (List.filter_map target ships) with
    | [] -> (c, [])
    | (_, s) :: _ ->
        let dx = s.body.x - c.mx and dy = s.body.y - c.my in
        let tx, ty =
          match intercept dx dy s.body.vx s.body.vy bullet_speed with
          | Some t -> (dx + (s.body.vx * t), dy + (s.body.vy * t))
          | None -> (dx, dy)
        in
        let n = Float.hypot tx ty in
        let b = Physics.body bullet_shape |> Physics.at c.mx c.my |> Physics.moving (bullet_speed * tx / n) (bullet_speed * ty / n) in
        ({ c with reload = 100 }, [ { shot = b; ttl = 240 } ])

let move_shots (lv : level) (l : shot list) : shot list =
  List.filter_map
    (fun s ->
      let b = Physics.step s.shot in
      if s.ttl <= 0 || solid lv b.x b.y then None else Some { shot = b; ttl = s.ttl -.. 1 })
    l

(* the ships' shots hitting a cannon's tile bring it down for 15 s *)
let hit_cannons (shots : shot list) (cs : cannon list) : cannon list =
  List.map
    (fun c ->
      let fx, fy = c.facing in
      let cx = c.mx - (fx * h) and cy = c.my - (fy * h) in
      let hit (s : shot) = Float.abs (s.shot.x - cx) <= h + 4. && Float.abs (s.shot.y - cy) <= h + 4. in
      if c.down = 0 && List.exists hit shots then { c with down = 900 } else c)
    cs

(* the pull of the rope on the pilot of team [t]: from the ball it holds *)
let pull_on (t : team) (g : game) : number * number =
  match (List.find_opt (fun (p : pilot) -> p.team = t) g.pilots, List.find_opt (fun b -> b.holder = Some t) g.balls) with
  | Some p, Some b -> rope_pull p.ship.body b.ball
  | _ -> (0., 0.)

(* the connectors, one ball after the other: a held ball is let go when
 * its pilot dies, lets go, or stretches the rope too far; a free ball
 * is caught by a living pilot of the other team near it, holding
 * nothing yet *)
let connect (scenes : model) (pilots : pilot list) (balls : ball list) : ball list =
  List.fold_left
    (fun caught (b : ball) ->
      let b =
        match b.holder with
        | Some t ->
            let p = List.find (fun (p : pilot) -> p.team = t) pilots in
            if p.ship.dead <> None || Scene2d.pressed p.controls.let_go scenes || Physics.distance p.ship.body b.ball > rope_length + 70. then
              { b with holder = None; grab_wait = 60 }
            else b
        | None -> (
            let can (p : pilot) =
              p.ship.dead = None && p.team <> b.owner && b.grab_wait = 0 && Physics.distance p.ship.body b.ball < 60.
              && not (List.exists (fun (b' : ball) -> b'.holder = Some p.team) caught)
            in
            match List.find_opt can pilots with Some p -> { b with holder = Some p.team } | None -> { b with grab_wait = max 0 (b.grab_wait -.. 1) })
      in
      caught @ [ b ])
    [] balls

(* [frame screen ships cam]: the camera following the ships, zooming out
 * to keep them all on the screen (with 250 pixels around them), but
 * never closer than 1.5 *)
let frame (screen : screen) (lv : level) (ships : Physics.body list) (cam : Camera2d.t) : Camera2d.t =
  let xs = List.map (fun (b : Physics.body) -> b.x) ships and ys = List.map (fun (b : Physics.body) -> b.y) ships in
  let lo l = List.fold_left Float.min infinity l and hi l = List.fold_left Float.max neg_infinity l in
  let zoom = Float.min 1.5 (Float.min (screen.width / (hi xs - lo xs + 500.)) (screen.height / (hi ys - lo ys + 500.))) in
  { (Camera2d.follow 0.12 ((lo xs + hi xs) / 2.) ((lo ys + hi ys) / 2.) cam) with zoom = cam.zoom + (0.1 * (zoom - cam.zoom)) }
  |> Camera2d.clamp screen (Tilemap.bounds lv.map)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let keys = computer.keyboard and lv = g.lv in
  (* the ships, pulled by their ropes *)
  let pilots =
    List.map
      (fun (p : pilot) ->
        match p.ship.dead with
        | Some n when n >= 60 -> { p with ship = new_ship lv p.team }
        | Some n -> { p with ship = { p.ship with dead = Some (n +.. 1) } }
        | None ->
            let s = fly lv keys p.controls (pull_on p.team g) p.ship in
            { p with ship = s; deaths = (if s.dead = None then p.deaths else p.deaths +.. 1) })
      g.pilots
  in
  (* the balls, pulled the other way *)
  let balls =
    List.map
      (fun b ->
        let fx, fy = match b.holder with Some t -> pull_on t g | None -> (0., 0.) in
        let ball, _ = b.ball |> Physics.fall gravity |> Physics.push (-.fx) (-.fy) |> Physics.step |> hit_walls lv in
        { b with ball })
      g.balls
  in
  (* the ships' shots, and the cannons' *)
  let pilots =
    List.map
      (fun (p : pilot) ->
        let firing = p.ship.dead = None && Scene2d.pressed p.controls.fire scenes && List.length p.shots < 8 && p.ship.fuel >= 1.5 in
        let fired = if firing then [ { shot = Physics.body (circle (team_color p.team) 3.) |> Physics.shot_from 520. 20. p.ship.body; ttl = 90 } ] else [] in
        { p with shots = move_shots lv p.shots @ fired; ship = (if firing then { p.ship with fuel = p.ship.fuel - 1.5 } else p.ship) })
      pilots
  in
  let cannons, fired =
    List.split (List.map (fire_cannon lv (List.map (fun (p : pilot) -> p.ship) pilots)) (hit_cannons (List.concat_map (fun (p : pilot) -> p.shots) g.pilots) g.cannons))
  in
  let bullets = move_shots lv g.bullets @ List.concat fired in
  (* the shots hitting a ship, the cannons' and the other pilot's, are
   * spent; stopped by a shield, or the ship explodes *)
  let hitting (p : pilot) (s : shot) = p.ship.dead = None && Physics.touching s.shot p.ship.body in
  let enemy (p : pilot) (s : shot) = List.exists (fun (o : pilot) -> o.team <> p.team && List.memq s o.shots) pilots in
  let hit_by (p : pilot) = List.exists (hitting p) bullets || List.exists (fun s -> enemy p s && hitting p s) (List.concat_map (fun (o : pilot) -> o.shots) pilots) in
  let spent s = List.exists (fun p -> hitting p s) pilots in
  let pilots =
    List.map
      (fun (p : pilot) ->
        let p = { p with shots = List.filter (fun s -> not (spent s)) p.shots } in
        if hit_by p && not (shielded keys p) then { p with ship = { p.ship with dead = Some 0 }; deaths = p.deaths +.. 1 } else p)
      pilots
  in
  let bullets = List.filter (fun s -> not (spent s)) bullets in
  (* a ball touching the other team's treasure box: a point for them;
   * left alone 10 s, a ball goes back to its place *)
  let balls = connect scenes pilots balls in
  let on_treasure (b : ball) =
    List.exists (fun (t, (x, y)) -> t <> b.owner && Float.abs (x - b.ball.x) < h + 11. && Float.abs (y - b.ball.y) < h + 11.) lv.treasures
  in
  let pilots = List.map (fun (p : pilot) -> { p with score = p.score +.. List.length (List.filter (fun b -> b.owner <> p.team && on_treasure b) balls) }) pilots in
  let balls =
    List.map
      (fun b ->
        let free = if b.holder = None then b.free +.. 1 else 0 in
        if on_treasure b || free > 600 then new_ball lv b.owner else { b with free })
      balls
  in
  let cam = frame computer.screen lv (List.map (fun (p : pilot) -> p.ship.body) pilots) g.cam in
  { g with pilots; balls; bullets; cannons; frames = g.frames +.. 1; cam }

let winner (g : game) : pilot option = List.find_opt (fun (p : pilot) -> p.score >= balls_to_win) g.pilots

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed k = Scene2d.pressed k scenes in
  match scenes.scene with
  | Title ->
      if pressed (fun k -> k.kspace || key "1" k) then Scene2d.go (Playing (new_game 1)) scenes
      else if pressed (key "2") then Scene2d.go (Playing (new_game 2)) scenes
      else scenes
  | Playing g ->
      let g = update_game computer scenes g in
      if winner g <> None then Scene2d.go (Won g) scenes else { scenes with scene = Playing g }
  | Won _ -> if pressed (fun k -> k.kspace) then Scene2d.go Title scenes else scenes

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* a line from (x0, y0) to (x1, y1) *)
let segment (color : color) (width : number) (x0 : number) (y0 : number) (x1 : number) (y1 : number) : shape =
  rectangle color (Float.hypot (x1 - x0) (y1 - y0)) width
  |> rotate (atan2 (y1 - y0) (x1 - x0) * 180. / pi)
  |> move ((x0 + x1) / 2.) ((y0 + y1) / 2.)

(* the tiles drawn over the walls *)
let tile_shape (c : char) : shape =
  let box outer inner = group [ square outer tile; square inner (tile - 10.) ] in
  match c with
  | '#' -> group [ box (rgb 230 150 30) (rgb 120 70 10); text white 1.5 "F" ]
  | '*' -> box (team_color Blue) (rgb 30 60 130)
  | '%' -> box (team_color Red) (rgb 130 30 30)
  | '1' -> rectangle (team_color Blue) tile 5. |> move_y (-.h + 2.5)
  | '2' -> rectangle (team_color Red) tile 5. |> move_y (-.h + 2.5)
  | _ -> group []

let view_cannon (c : cannon) : shape list =
  if c.down > 0 then []
  else
    let fx, fy = c.facing in
    [ circle (rgb 200 200 210) 9. |> move c.mx c.my;
      rectangle (rgb 200 200 210) 16. 5. |> rotate (atan2 fy fx * 180. / pi) |> move (c.mx + (fx * 8.)) (c.my + (fy * 8.)) ]

let view_ship (keys : keyboard) (p : pilot) : shape list =
  let b = p.ship.body in
  match p.ship.dead with
  | Some n ->
      let r = 6. + (0.8 * float_of_int n) in
      [ circle orange r |> fade (Float.max 0. (1. - (float_of_int n / 60.))) |> move b.x b.y ]
  | None ->
      let flame = if p.controls.thrust keys && p.ship.fuel > 0. then [ polygon orange [ (-12., 6.); (-26., 0.); (-12., -6.) ] ] else [] in
      let outline = [ polygon (team_color p.team) [ (16., 0.); (-12., 11.); (-12., -11.) ]; polygon black [ (9., 0.); (-9., 6.); (-9., -6.) ] ] in
      let shield = if shielded keys p then [ circle (rgb 90 180 255) 26. |> fade 0.35 ] else [] in
      [ group (flame @ outline) |> rotate b.angle |> move b.x b.y; group shield |> move b.x b.y ]

let view_world (computer : computer) (g : game) : shape list =
  let lv = g.lv in
  let pilot t = List.find (fun (p : pilot) -> p.team = t) g.pilots in
  let ropes = List.filter_map (fun b -> Option.map (fun t -> segment (rgb 230 230 120) 2. (pilot t).ship.body.x (pilot t).ship.body.y b.ball.x b.ball.y) b.holder) g.balls in
  let beams =
    List.concat_map
      (fun (p : pilot) ->
        List.filter_map
          (fun (x, y) -> if refueling lv p.ship && Float.hypot (x - p.ship.body.x) (y - p.ship.body.y) < 110. then Some (segment (rgb 255 170 90) 2. x y p.ship.body.x p.ship.body.y) else None)
          lv.fuel_stations)
      g.pilots
  in
  let visible = Camera2d.visible computer.screen g.cam in
  List.map Physics.draw (walls_in lv visible)
  @ [ Tilemap.view_visible visible tile_shape lv.map ]
  @ List.concat_map view_cannon g.cannons
  @ beams @ ropes
  @ List.map (fun b -> Physics.draw b.ball) g.balls
  @ List.concat_map (view_ship computer.keyboard) g.pilots
  @ List.map (fun s -> Physics.draw s.shot) (g.bullets @ List.concat_map (fun (p : pilot) -> p.shots) g.pilots)
  @
  if List.mem_assoc "hitboxes" computer.flags then
    List.map Physics.debug (List.concat_map (fun (p : pilot) -> p.ship.body :: near lv p.ship.body) g.pilots @ List.map (fun b -> b.ball) g.balls)
  else []

(* the radar: the walls again, 1/12 of their size, the ships and the
 * balls on it, at the top right *)
let radar_scale = 1. / 12.

let radar (screen : screen) (g : game) : shape =
  let r = Tilemap.bounds g.lv.map in
  let dot color (b : Physics.body) = circle color (3. / radar_scale) |> move b.x b.y in
  group
    ((rectangle black (r.right - r.left) (r.top - r.bottom) :: List.map (fun (w, _) -> Physics.draw w) g.lv.walls)
    @ List.map (fun b -> dot (team_color b.owner) b.ball) g.balls
    @ List.map (fun (p : pilot) -> dot white p.ship.body) g.pilots)
  |> scale radar_scale
  |> move (screen.right - 20. - (r.right * radar_scale)) (screen.top - 20. - (r.top * radar_scale))

(* a pilot's fuel and score: blue at the top left, red below the radar *)
let view_hud (screen : screen) (g : game) (p : pilot) : shape list =
  let x = if p.team = Blue then screen.left else screen.right - 280. and y = if p.team = Blue then screen.top else screen.top - 120. in
  let fuel = p.ship.fuel / fuel_max in
  [ text white 2. "FUEL" |> move (x + 50.) (y - 30.);
    rectangle (rgb 60 60 60) 150. 14. |> move (x + 165.) (y - 30.);
    rectangle (if fuel < 0.2 then red else rgb 230 150 30) (150. * fuel) 14. |> move (x + 90. + (75. * fuel)) (y - 30.);
    text (team_color p.team) 2.5 (Printf.sprintf "BALLS %d / %d" p.score balls_to_win) |> move (x + 110.) (y - 70.);
    text white 2. (Printf.sprintf "DEATHS %d   %.0f s" p.deaths (float_of_int g.frames / 60.)) |> move (x + 110.) (y - 105.) ]

let view_game (computer : computer) (g : game) : shape list =
  (Camera2d.view g.cam (view_world computer g) :: List.concat_map (view_hud computer.screen g) g.pilots) @ [ radar computer.screen g ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match model.scene with
  | Title ->
      let g = new_game 1 in
      let g = { g with pilots = []; cam = { g.cam with x = 0.; y = 150.; zoom = 0.6 } } in
      [ Camera2d.view g.cam (view_world computer g) |> fade 0.5;
        text white 6. "TINY XPILOT" |> move_y 300.;
        text white 2. "left/right turn   up thrust   down shield   space fire   b let go" |> move_y 220.;
        text yellow 2. "catch the red ball down in the cave, set it on your blue box: three to win" |> move_y 185.;
        text white 2. "2 players: blue w a d, s shield, space fire, x let go; red the arrows, enter fire, / let go" |> move_y (-430.) ]
      @ Scene2d.blink 1. model [ text yellow 3. "SPACE: ONE PLAYER   2: TWO PLAYERS" |> move_y (-380.) ]
  | Playing g -> view_game computer g
  | Won g ->
      let over =
        match (g.pilots, winner g) with
        | [ p ], _ ->
            [ text yellow 5. "MISSION COMPLETE" |> move_y 100.;
              text white 3. (Printf.sprintf "%.0f seconds, %d deaths" (float_of_int g.frames / 60.) p.deaths) |> move_y 30. ]
        | _, Some p -> [ text (team_color p.team) 5. (if p.team = Blue then "BLUE WINS" else "RED WINS") |> move_y 100. ]
        | _ -> []
      in
      view_game computer g @ over @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ])

(* the keys and flags, printed at launch, to remember them (on the web,
 * in the browser's console) *)
let help =
  {|TinyXpilot
  1 player (space on the title):
         left/right  turn
         up          thrust
         down        shield (uses fuel)
         space       fire (start, restart)
         b           let the ball go
  2 players (2 on the title):
         blue: a/d turn, w thrust, s shield, space fire, x let go
         red:  the arrows, down shield, enter fire, / let go
  flags: hitboxes    draw what the physics sees
  e.g.   dune exec games/TinyXpilot.exe -- hitboxes
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
