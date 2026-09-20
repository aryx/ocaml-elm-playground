(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Sonic the Hedgehog (Yuji Naka, Hirokazu Yasuhara,
 * Naoto Ohshima, Sega, 1991): run right, keep your speed, take the
 * hill, go round the loop. Left and right run, up jumps, down rolls;
 * down held with jump charges a spindash, let go to fire. Rings are
 * your life: hit an enemy with none and it's over, hit it with some and
 * you drop them. Reach the sign post at the end.
 *
 * Where games/TinyMario is about jumping exactly, this is about not
 * losing speed, and the difference is in how the ground is felt.
 * TinyMario walks a box against solid tiles (kits/platformer's
 * Tile_move): the ground is flat, a tile is in or out. Sonic runs on a
 * *surface* (kits/platformer's Slope):
 *
 *  - The ground is tiles with a shape and an angle, and a few sensors
 *    find the surface under the feet rather than a box being pushed out
 *    of a wall (Slope.ground).
 *
 *  - Speed on the ground is one number *along* the surface ([gsp]), not
 *    an (x, y) velocity: gravity becomes a slope factor that adds to it
 *    downhill and eats it uphill ([slope_pull]), and it turns back into
 *    x and y only in the air. Running up a wall at a constant speed is
 *    then the ordinary case, not a special one.
 *
 *  - The loop is not a trick: it's curved tiles, and the hero's mode
 *    (floor, wall, ceiling, wall) follows their angle. Go too slowly on
 *    a steep angle and you slide off ([slip]) -- the same rule, not a
 *    second one. The whole "3D-looking" loop is a circle of tiles.
 *
 *  - Rolling and the spindash: a ball keeps its speed downhill and
 *    can't accelerate, so rolling is a commitment; the spindash
 *    ([charge]) is speed bought while standing still, the 1992 sequel's
 *    answer to a hill you can't climb.
 *
 * What it uses: kits/platformer's Slope (the new one) and Camera2d (a
 * window that looks ahead when Sonic is fast), Scene2d, Audio. Not
 * Tile_move, whose blocks are what Slope replaces here; not Physics:
 * the ground speed is no rigid body, and gravity in the air is two
 * lines.
 *
 * Exercises: the airborne arc on the Physics layer (as TinyMario's
 * physics=engine flag does); the control lock after a slip (30 frames
 * in the original, so you can't instantly climb back); the loop drawn
 * from its tiles instead of a circle; Sonic's real sensor set (two
 * feet, two sides, one head) instead of our three; a second act, badniks
 * that shoot, the shield and the invincibility stars.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The knobs *)
(*****************************************************************************)
(* The numbers below are Sonic 1's own, in pixels per frame at 60 frames
 * a second (the Sonic Physics Guide's table), and they are the feel of
 * the game:
 *
 *  - [accel] and [friction] are tiny (3/64 of a pixel per frame), which
 *    is why he takes a second to get going and glides so long;
 *    [decel] is ten times bigger: turning round is sharp.
 *  - [top_speed] 6 is what running gives; a slope can take him past it,
 *    and rolling downhill well past it.
 *  - [slope] is the pull along the ground, [roll_up] and [roll_down] the
 *    same for a ball, which keeps more of it downhill.
 *  - [slip_speed] is how slowly you may go on a wall before falling off:
 *    the loop's whole difficulty.
 *  - [gravity] and [jump] are the air; [spindash_step] is what one tap
 *    of the charge is worth.
 *)
let accel = 0.046875
let friction = 0.046875
let decel = 0.5
let top_speed = 6.
let gravity = 0.21875
let jump = 6.5
let slope = 0.125
let roll_up = 0.078125
let roll_down = 0.3125
let roll_friction = 0.0234375
let slip_speed = 2.5
let spindash_step = 2.
let spindash_max = 12.
let tile = 16
let radius = 10. (* how far the feet are from the middle *)

(* the screen shows about 20 tiles across, as the Mega Drive's 320
 * pixels did *)
let zoom = 3.

(*****************************************************************************)
(* The level *)
(*****************************************************************************)

(* A row of the level per line, a tile per character:
 *   '#' solid, ' ' sky, '/' and '\' 45 degrees, 'a'..'d' a gentle hill
 *   (two tiles up, two down), 'o' a ring, 's' a spring, 'm' a motobug,
 *   'G' the goal sign, 'S' where Sonic starts.
 * The loop isn't drawn here: it's a circle of tiles built by [loop_at],
 * because its tiles each need to know where the circle's center is. *)
let level =
  [ "                                                                                                ";
    "                                                                                                ";
    "                                                                                                ";
    "                                                                                                ";
    "                                                                                                ";
    "                                                               o o o                            ";
    "                                                                                                ";
    "                                                                                                ";
    "                                                                            o o o               ";
    "                              ooo                         o                                     ";
    "         S    o o o o       ab###cd omo o   /##\    o o o s                         m       G   ";
    "################################################################################################";
    "################################################################################################" ]
let cols = String.length (List.hd level)
let rows = List.length level
let char_at (tx : int) (ty : int) : char =
  (* the level is written top to bottom, the world counts y upwards; a
   * line shorter than the others is sky the rest of the way *)
  if tx < 0 || tx >= cols || ty < 0 || ty >= rows then ' '
  else
    let row = List.nth level (rows -.. 1 -.. ty) in
    if tx >= String.length row then ' ' else row.[tx]

(* the loop: a circle of tiles, its inside the floor Sonic runs on *)
let loop_radius = 5.2 *. float_of_int tile

(* the circle sits on the ground (two tiles of it), so its lowest point
 * is the floor Sonic is already running on: he enters the loop without
 * anything happening *)
let loop_center = (65.5 *. float_of_int tile, (2. *. float_of_int tile) +. loop_radius)

let in_loop (tx : int) (ty : int) : bool =
  let x = (float_of_int tx +. 0.5) *. float_of_int tile and y = (float_of_int ty +. 0.5) *. float_of_int tile in
  Float.hypot (x -. fst loop_center) (y -. snd loop_center) <= loop_radius +. float_of_int tile

(* the surface of a tile: the loop's ring where the circle passes, the
 * level's characters everywhere else *)
let loop_tiles (on_loop : bool) (tx, ty) : Slope.surface option =
  (* the loop is a ring of ground one tile thick, which Sonic runs round
   * the inside of; everywhere else, and under it, the level's own
   * ground *)
  let looping =
    if (not on_loop) || not (in_loop tx ty) then None
    else
      let cx = fst loop_center -. float_of_int (tx *.. tile) and cy = snd loop_center -. float_of_int (ty *.. tile) in
      let d = Float.hypot (cx -. (float_of_int tile /. 2.)) (cy -. (float_of_int tile /. 2.)) in
      if Float.abs (d -. loop_radius) > 1.5 *. float_of_int tile then None
      else Some (Slope.ring tile ~cx ~cy ~radius:loop_radius ~thickness:(float_of_int tile) ~inside:true)
  in
  match looping with
  | Some _ as s -> s
  | None ->
    match char_at tx ty with
    | '#' -> Some (Slope.block tile)
    | '/' -> Some (Slope.slope tile ~from_:0 ~to_:tile)
    | '\\' -> Some (Slope.slope tile ~from_:tile ~to_:0)
    | 'a' -> Some (Slope.slope tile ~from_:0 ~to_:(tile /.. 2))
    | 'b' -> Some (Slope.slope tile ~from_:(tile /.. 2) ~to_:tile)
    | 'c' -> Some (Slope.slope tile ~from_:tile ~to_:(tile /.. 2))
    | 'd' -> Some (Slope.slope tile ~from_:(tile /.. 2) ~to_:0)
    | _ -> None

(* the ground alone, without the loop: what the hero runs on before he
 * enters it and after he has been round *)
let tiles (c : int * int) : Slope.surface option = loop_tiles true c

let things (c : char) : (number * number) list =
  List.concat
    (List.init rows (fun ty ->
         List.filter_map
           (fun tx ->
             if char_at tx ty = c then
               Some ((float_of_int tx +. 0.5) *. float_of_int tile, (float_of_int ty +. 0.5) *. float_of_int tile)
             else None)
           (List.init cols Fun.id)))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type hero = {
  x : number;
  y : number;
  gsp : number; (* the speed along the ground *)
  vx : number; (* in the air *)
  vy : number;
  angle : number;
  mode : Slope.mode;
  grounded : bool;
  rolling : bool;
  spindash : number option; (* charged, while crouching *)
  facing : number; (* 1 right, -1 left *)
  hurt : int; (* frames of blinking after a hit *)
  (* Sonic's "path layers": the loop's tiles are solid only while he is
   * going round it, and stop being once he has, so he runs out at the
   * bottom instead of circling for ever. The original switched layers
   * at markers placed at each end of the loop. *)
  on_loop : bool;
  looped : bool; (* he has been over the top of it *)
}

type enemy = { ex : number; ey : number; edir : number; alive : bool }

type game = {
  sonic : hero;
  enemies : enemy list;
  rings : (number * number) list;
  taken : int;
  camera : Camera2d.t;
  dead : bool; (* hit without a ring to lose *)
  frames : int;
  showing_sensors : bool;
}

type scene = Title | Playing of game | Won of int | Lost
type model = scene Scene2d.t

let start_at = List.hd (things 'S')
let goal_at = List.hd (things 'G')

let new_game () : game =
  { sonic =
      { x = fst start_at; y = snd start_at; gsp = 0.; vx = 0.; vy = 0.; angle = 0.; mode = Slope.Floor; grounded = false;
        rolling = false; spindash = None; facing = 1.; hurt = 0; on_loop = false; looped = false };
    enemies = List.map (fun (ex, ey) -> { ex; ey; edir = -1.; alive = true }) (things 'm');
    rings = things 'o'; taken = 0; camera = { (Camera2d.look_at (fst start_at) (snd start_at) Camera2d.origin) with zoom }; dead = false; frames = 0;
    showing_sensors = false }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The ground under the feet *)
(*****************************************************************************)

(* Sonic's sensors: two under the feet, a little apart, so a hill is
 * felt before the middle reaches it; the lower of the two wins *)
let feet (h : hero) : (number * number) option =
  let dx, dy = Slope.down h.mode in
  let side = (-.dy, dx) in
  let sensor k =
    Slope.ground ~tiles:(loop_tiles h.on_loop) ~size:tile h.mode
      (h.x +. (fst side *. k) +. (dx *. radius), h.y +. (snd side *. k) +. (dy *. radius))
  in
  match (sensor (-6.), sensor 6.) with
  | None, None -> None
  | Some a, None | None, Some a -> Some a
  | Some (pa, aa), Some (pb, ab) ->
      (* the one the hero rests higher on, along the mode's axis *)
      let further = if dx +. dy > 0. then pa < pb else pa > pb in
      if further then Some (pa, aa) else Some (pb, ab)

(* put the hero's middle a radius away from the surface it found *)
let stand (h : hero) ((where, angle) : number * number) : hero =
  let dx, dy = Slope.down h.mode in
  let h = if dx <> 0. then { h with x = where -. (dx *. radius) } else { h with y = where -. (dy *. radius) } in
  { h with angle; mode = Slope.mode_of angle }

(*****************************************************************************)
(* Update: the hero *)
(*****************************************************************************)

let sin_deg (a : number) : number = sin (a *. pi /. 180.)
let cos_deg (a : number) : number = cos (a *. pi /. 180.)

(* gravity felt along the ground: downhill it adds, uphill it eats -- a
 * ball keeps more of it *)
let slope_pull (h : hero) : number =
  let factor =
    if not h.rolling then slope
    else if h.gsp *. sin_deg h.angle > 0. then roll_up (* going up: it bites less *)
    else roll_down
  in
  h.gsp -. (factor *. sin_deg h.angle)

(* too slow on a steep angle: the hero comes off and falls *)
let slip (h : hero) : hero =
  if h.grounded && Float.abs h.gsp < slip_speed && h.mode <> Slope.Floor then
    { h with grounded = false; vx = h.gsp *. cos_deg h.angle; vy = h.gsp *. sin_deg h.angle; gsp = 0.; angle = 0.; mode = Slope.Floor }
  else h

let on_ground (computer : computer) (pressed_jump : bool) (h : hero) : hero =
  let k = computer.keyboard in
  let left = k.kleft and right = k.kright and dn = k.kdown in
  let h = { h with gsp = slope_pull h } in
  (* the controls, unless crouching for a spindash *)
  let h =
    match h.spindash with
    | Some _ -> h
    | None ->
        if h.rolling then
          (* a ball can't speed up, only brake and rub along *)
          let gsp = if (left && h.gsp > 0.) || (right && h.gsp < 0.) then h.gsp -. (decel /. 4. *. Float.of_int (if h.gsp > 0. then 1 else -1)) else h.gsp in
          let gsp = gsp -. (Float.min (Float.abs gsp) roll_friction *. if gsp > 0. then 1. else -1.) in
          { h with gsp }
        else if left && not right then
          { h with facing = -1.; gsp = (if h.gsp > 0. then h.gsp -. decel else Float.max (-.top_speed) (h.gsp -. accel)) }
        else if right && not left then
          { h with facing = 1.; gsp = (if h.gsp < 0. then h.gsp +. decel else Float.min top_speed (h.gsp +. accel)) }
        else { h with gsp = h.gsp -. (Float.min (Float.abs h.gsp) friction *. if h.gsp > 0. then 1. else -1.) }
  in
  (* rolling starts by pressing down while moving; it ends when slow *)
  let h =
    if dn && (not h.rolling) && Float.abs h.gsp > 1. && h.spindash = None then (Audio.play Audio.blip; { h with rolling = true })
    else if h.rolling && Float.abs h.gsp < 0.2 then { h with rolling = false }
    else h
  in
  (* the spindash: crouch, tap jump to charge, let go of down to fire *)
  let h =
    match (h.spindash, dn, pressed_jump) with
    | None, true, true when Float.abs h.gsp < 0.2 -> Audio.play Audio.jump; { h with spindash = Some spindash_step; rolling = true }
    | Some c, true, true -> Audio.play Audio.jump; { h with spindash = Some (Float.min spindash_max (c +. spindash_step)) }
    | Some c, false, _ -> Audio.play Audio.laser; { h with spindash = None; gsp = c *. h.facing; rolling = true }
    | Some c, true, false -> { h with spindash = Some (Float.max 0. (c -. (c /. 64.))) }
    | _ -> h
  in
  (* jumping leaves the ground along the surface's normal *)
  let h =
    if pressed_jump && h.spindash = None then begin
      Audio.play Audio.jump;
      { h with grounded = false; rolling = h.rolling;
        vx = (h.gsp *. cos_deg h.angle) +. (jump *. sin_deg h.angle);
        vy = (h.gsp *. sin_deg h.angle) +. (jump *. cos_deg h.angle) }
    end
    else h
  in
  if not h.grounded then h
  else
    (* walk along the surface, then feel for it again *)
    let h = { h with x = h.x +. (h.gsp *. cos_deg h.angle); y = h.y +. (h.gsp *. sin_deg h.angle) } in
    match feet h with
    | Some found -> slip (stand h found)
    | None -> { h with grounded = false; vx = h.gsp *. cos_deg h.angle; vy = h.gsp *. sin_deg h.angle; gsp = 0.; angle = 0.; mode = Slope.Floor }

let in_air (computer : computer) (h : hero) : hero =
  let k = computer.keyboard in
  let vx = if k.kleft then h.vx -. (accel *. 2.) else if k.kright then h.vx +. (accel *. 2.) else h.vx in
  let vx = Float.max (-.top_speed) (Float.min top_speed vx) in
  (* a short hop: let go of jump early and the rise is cut *)
  let vy = (if (not k.kup) && h.vy > 4. then 4. else h.vy) -. gravity in
  let h = { h with vx; vy; x = h.x +. vx; y = h.y +. vy } in
  (* landing: only while falling, and the ground gives back a speed
   * along itself *)
  if h.vy > 0. then h
  else
    match feet { h with mode = Slope.Floor } with
    | Some (where, angle) when h.y -. radius <= where +. 2. ->
        let h = stand { h with mode = Slope.mode_of angle } (where, angle) in
        { h with grounded = true; gsp = (h.vx *. cos_deg angle) +. (h.vy *. sin_deg angle); vx = 0.; vy = 0. }
    | _ -> h

(* the markers at each end of the loop, as the original had them: fast
 * enough and on the ground, the loop becomes solid; back at the bottom
 * after going over the top, it stops being, and he runs out *)
let layers (h : hero) : hero =
  let cx, cy = loop_center in
  let near = Float.abs (h.x -. cx) < loop_radius +. float_of_int tile in
  let at_the_bottom = h.y < cy -. loop_radius +. (2. *. float_of_int tile) in
  if not near then { h with on_loop = false; looped = false }
  else if (not h.on_loop) && (not h.looped) && h.grounded && Float.abs h.gsp > slip_speed then { h with on_loop = true }
  else if h.on_loop && h.y > cy then { h with looped = true }
  else if h.on_loop && h.looped && at_the_bottom then { h with on_loop = false }
  else h

let update_hero (computer : computer) (scenes : model) (h : hero) : hero =
  let pressed_jump = Scene2d.pressed (fun k -> k.kup) scenes in
  let h = layers h in
  let h = if h.grounded then on_ground computer pressed_jump h else in_air computer h in
  { h with hurt = max 0 (h.hurt -.. 1) }

(*****************************************************************************)
(* Update: the game *)
(*****************************************************************************)

let hit (h : hero) ((x, y) : number * number) (r : number) : bool = Float.hypot (h.x -. x) (h.y -. y) < r

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1 } in
  let g = if Scene2d.pressed (fun k -> Set_.mem "d" k.keys) scenes then { g with showing_sensors = not g.showing_sensors } else g in
  let sonic = update_hero computer scenes g.sonic in
  (* the springs throw him up *)
  let sonic =
    if List.exists (fun p -> hit sonic p 14.) (things 's') then begin
      Audio.play Audio.coin;
      { sonic with grounded = false; rolling = false; vy = 11.; angle = 0.; mode = Slope.Floor }
    end
    else sonic
  in
  (* the rings *)
  let taken, rings = List.partition (fun p -> hit sonic p 14.) g.rings in
  if taken <> [] then Audio.play Audio.coin;
  (* the motobugs walk, turn at an edge, and die under a ball *)
  let enemies =
    List.map
      (fun (e : enemy) ->
        if not e.alive then e
        else
          let ex = e.ex +. (0.5 *. e.edir) in
          let ahead = Slope.ground ~tiles ~size:tile Slope.Floor (ex +. (8. *. e.edir), e.ey -. 8.) in
          if ahead = None then { e with edir = -.e.edir } else { e with ex })
      g.enemies
  in
  let stomped, enemies =
    List.partition (fun (e : enemy) -> e.alive && hit sonic (e.ex, e.ey) 16. && (sonic.rolling || (not sonic.grounded && sonic.vy < 0.))) enemies
  in
  if stomped <> [] then Audio.play Audio.explosion;
  let enemies = enemies @ List.map (fun e -> { e with alive = false }) stomped in
  (* a hit: the rings fly off, and without any it's over *)
  let touched = List.exists (fun (e : enemy) -> e.alive && hit sonic (e.ex, e.ey) 16.) enemies in
  let sonic =
    if touched && sonic.hurt = 0 then begin
      Audio.play Audio.hit;
      { sonic with hurt = 90; grounded = false; rolling = false; vx = -2. *. sonic.facing; vy = 4.; gsp = 0. }
    end
    else sonic
  in
  (* the rings are the life: hit with some, you drop them; hit with
   * none, that's the end *)
  let lost_them = touched && g.sonic.hurt = 0 in
  let dead = g.dead || (lost_them && g.taken = 0) in
  let rings_left = if lost_them then 0 else g.taken +.. List.length taken in
  (* the camera looks ahead when he's fast (the original's window) *)
  let camera =
    g.camera
    |> Camera2d.window 32. 64. (sonic.x +. (16. *. sonic.facing)) sonic.y
    |> Camera2d.clamp computer.screen
         { left = 0.; right = float_of_int (cols *.. tile); bottom = 0.; top = float_of_int (rows *.. tile) }
  in
  { g with sonic; rings; taken = rings_left; enemies; camera; dead }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if hit g.sonic goal_at 20. then Scene2d.go (Won g.taken) s
      else if g.dead || g.sonic.y < -32. then Scene2d.go Lost s
      else { s with scene = Playing g }
  | Won _ | Lost -> if space && s.elapsed > 1.5 then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the ground, tile by tile, each drawn as the rows of solid pixels it
 * holds: the bitmap the sensors read, seen *)
let view_tiles (visible : Camera2d.rect) : shape list =
  let t = float_of_int tile in
  let from_x = max 0 (int_of_float (visible.left /. t) -.. 1) and to_x = min (cols -.. 1) (int_of_float (visible.right /. t) +.. 1) in
  let from_y = max 0 (int_of_float (visible.bottom /. t) -.. 1) and to_y = min (rows -.. 1) (int_of_float (visible.top /. t) +.. 1) in
  List.concat_map
    (fun tx ->
      List.filter_map
        (fun ty ->
          match tiles (tx, ty) with
          | None -> None
          | Some s ->
              (* one rectangle per column of the tile: enough for a
               * slope or a curve, and cheap *)
              let x0 = float_of_int (tx *.. tile) and y0 = float_of_int (ty *.. tile) in
              let column i =
                let solid = List.filter (fun j -> s.solid.((j *.. tile) +.. i)) (List.init tile Fun.id) in
                match solid with
                | [] -> None
                | js ->
                    let lo = float_of_int (List.hd js) and hi = float_of_int (List.nth js (List.length js -.. 1)) in
                    Some (rectangle (rgb 90 70 50) 1. (hi -. lo +. 1.) |> move (x0 +. float_of_int i +. 0.5) (y0 +. ((lo +. hi) /. 2.) +. 0.5))
              in
              Some (group (List.filter_map column (List.init tile Fun.id))))
        (List.init rows Fun.id))
    (List.init cols Fun.id)
  |> List.filteri (fun i _ -> i >= 0 && from_x <= to_x && from_y <= to_y)

let view_sonic (g : game) : shape list =
  let h = g.sonic in
  let blink = h.hurt > 0 && (h.hurt /.. 4) mod 2 = 0 in
  if blink then []
  else
    let ball = h.rolling || h.spindash <> None in
    [ (if ball then group [ circle (rgb 40 80 220) 11.; circle (rgb 20 50 160) 5. |> move (3. *. h.facing) 0. ]
       else
         group
           [ circle (rgb 40 80 220) 10. |> move_y 2.;
             circle (rgb 240 200 160) 6. |> move (4. *. h.facing) 5.;
             rectangle (rgb 220 60 60) 9. 4. |> move (2. *. h.facing) (-7.) ])
      |> rotate (if h.grounded then h.angle else 0.)
      |> move h.x h.y ]

let view_game (computer : computer) (g : game) : shape list =
  let visible = Camera2d.visible computer.screen g.camera in
  let world =
    view_tiles visible
    @ List.map (fun (x, y) -> circle (rgb 250 210 60) 6. |> move x y) g.rings
    @ List.map (fun (x, y) -> rectangle (rgb 230 80 80) 16. 6. |> move x (y -. 5.)) (things 's')
    @ List.filter_map
        (fun (e : enemy) -> if e.alive then Some (group [ circle (rgb 120 60 160) 10.; circle (rgb 240 240 240) 4. |> move (4. *. e.edir) 2. ] |> move e.ex e.ey) else None)
        g.enemies
    @ [ group [ rectangle (rgb 200 200 200) 3. 40. |> move_y 20.; rectangle (rgb 240 180 60) 26. 18. |> move_y 32. ] |> move (fst goal_at) (snd goal_at -. 8.) ]
    @ view_sonic g
    @ (if g.showing_sensors then
         let dx, dy = Slope.down g.sonic.mode in
         let side = (-.dy, dx) in
         List.map
           (fun k ->
             circle (rgb 100 240 240) 2.
             |> move (g.sonic.x +. (fst side *. k) +. (dx *. radius)) (g.sonic.y +. (snd side *. k) +. (dy *. radius)))
           [ -6.; 6. ]
       else [])
  in
  [ rectangle (rgb 110 190 230) computer.screen.width computer.screen.height; Camera2d.view g.camera world ]
  @ [ text yellow 2.5 (Printf.sprintf "rings %d" g.taken) |> move (-380.) 460.;
      text white 2. (Printf.sprintf "speed %.1f" (if g.sonic.grounded then Float.abs g.sonic.gsp else Float.hypot g.sonic.vx g.sonic.vy)) |> move (-100.) 460.;
      text white 2. (match g.sonic.mode with Slope.Floor -> "floor" | Right_wall -> "right wall" | Ceiling -> "ceiling" | Left_wall -> "left wall") |> move 120. 460.;
      text (rgb 60 60 80) 2. "left right run   up jumps   down rolls   down+up spindash   d: the sensors" |> move_y (-460.) ]

let view (computer : computer) (s : model) : shape list =
  match s.scene with
  | Title ->
      view_game computer (new_game ())
      @ [ text (rgb 40 80 220) 7. "TINY SONIC" |> move_y 160.; text white 2.5 "keep your speed: the hill, the loop, the sign" |> move_y 90. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 20. ]
  | Playing g -> view_game computer g
  | Won rings ->
      [ rectangle (rgb 110 190 230) computer.screen.width computer.screen.height;
        text (rgb 240 180 60) 6. "ACT CLEAR"; text white 3. (Printf.sprintf "%d rings" rings) |> move_y (-80.) ]
      @ if s.elapsed > 1.5 then Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-180.) ] else []
  | Lost ->
      [ rectangle (rgb 40 40 60) computer.screen.width computer.screen.height; text (rgb 230 80 80) 6. "TRY AGAIN" ]
      @ if s.elapsed > 1.5 then Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-140.) ] else []

let app = game view update initial_model
let main = Playground_platform.run_app app
