(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Marble Madness (Mark Cerny, Atari Games, 1984): roll a
 * marble down a course floating in space, to the goal, against the
 * clock. Arrows to push the marble, or the mouse, moved the way you
 * would spin the arcade's trackball.
 *
 * Mark Cerny designed Marble Madness at 18, after M. C. Escher's
 * impossible drawings and miniature golf. The course is seen from a
 * corner, in isometric projection, and the controls follow the screen's
 * diagonals, the way the trackball turned under the player's palm. A
 * marble falling too far breaks, the black "steelie" marbles push yours
 * off the edges, and the time left at the end of a race is carried into
 * the next one. It was one of the first arcade games in stereo, and
 * Cerny went on to produce Crash Bandicoot and Spyro, and to design the
 * PlayStation 4. (Names and dates from memory, to check.)
 *
 * The course is an ASCII map ([course]), as in the 2D games, but each
 * character is a height: '0' to '9' flat tiles at that height, 'v' and
 * '>' ramps going down south or east, between the flat tiles at both
 * ends of the run of ramps ([ramp_edges]). The marble reads the height
 * under it ([ground]), bilinearly interpolated between a tile's four
 * corners, and the slope there ([slope]).
 *
 * What the marble does, each frame ([step]):
 *
 *   - rolls down the slopes: gravity along the slope is g sin(angle),
 *     but a rolling ball speeds up at only 5/7 of it, the rest going
 *     into its spin ([rolling]); Galileo timed balls rolling down
 *     inclined planes to slow down falling enough to measure it (Two
 *     New Sciences, 1638), and missed that 5/7:
 *
 *                o  ball
 *               /|
 *              / |       along the slope: g sin(a) for a sliding
 *             /  | h     block, (5/7) g sin(a) for a rolling ball
 *            /a__|
 *              d         slope s = h / d = tan(a)
 *
 *   - sticks to the course, down the ramps, but falls off an edge or a
 *     step down (the ground under it dropping more than [step_down] in
 *     a frame); a real marble would fly off a crest at speed, Marble
 *     Madness's hugs its slopes, and so does ours;
 *   - breaks when it lands after falling more than [max_fall]: the
 *     course's shortcut, down the cliff south of the second plateau,
 *     breaks it (the original's rule, and its best trap);
 *   - bounces off walls, and off the steelie: an elastic collision
 *     between two balls of different masses ([collide]); the steelie,
 *     twice as heavy, knocks yours away and barely slows down.
 *
 * The steelie is the same ball, with the same [step]: its whole AI is
 * the direction of its push, towards you ([steelie_push]). And steel
 * doesn't break: it falls from anywhere unharmed.
 *
 * A broken or lost marble comes back at the last checkpoint passed; the
 * time it took is the only penalty, as in the original.
 *
 * What it uses: Scene2d (title, race, finish, time's up), Camera3d
 * ([from_far]: a far eye and a narrow field of view, nearly isometric;
 * [follow], [orbit] for the title, [floor] for the void), cached3d for
 * the course. Not Physics: it's 2D, and a ball on a height map is ten
 * lines; not Minecraft_player or TinyMario64's box collisions: the
 * ground here is a function of (x, z), not boxes. The marble's roll is
 * shown by turning its axes ([roll_axes]: Rodrigues' rotation formula),
 * not by Playground3d.rotate3d's three angles, which can't add up
 * rotations around changing axes.
 *
 * Exercises: the other hazards (acid pools, the marble munchers, the
 * vacuums), a second race with the time carried over, two players side
 * by side (the original had two trackballs), the mouse captured
 * (Playground3d_platform's capture_mouse) for an endless trackball, the
 * marble flying off crests (drop [step_down] and see), a board tilted
 * rather than a ball pushed (Super Monkey Ball, 2001; see TinyCameltry
 * for the same idea in 2D).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The course *)
(*****************************************************************************)

(* north is up (-z), east right (+x); the camera looks from the
 * south-east, so the course goes down the screen. ' ' is the void, 'g'
 * the goal (flat, height 0). *)
let course =
  [| "                ";
     " 9999           ";
     " 9999           ";
     " 9999           ";
     " vvvv           ";
     " vvvv           ";
     " 7777777>>>44   ";
     " 7777777>>>44   ";
     " 7777777>>>44   ";
     " 222       44   ";
     " 222       vv   ";
     " 222       vv   ";
     " 2222222222222  ";
     " 2222222222222  ";
     " 2222222222222  ";
     "    2           ";
     "    2           ";
     "    2           ";
     "    2           ";
     "   222          ";
     "   1111111      ";
     "   1111111      ";
     "      gggg      ";
     "      gggg      ";
     "                " |]

let cell = 2. (* a tile's width *)
let level = 1. (* the height of one step, from '0' to '1' *)
let rows = Array.length course
let cols = String.length course.(0)

let char_at (c : int) (r : int) : char = if r < 0 || r >= rows || c < 0 || c >= cols then ' ' else course.(r).[c]

let flat_height (ch : char) : number option =
  match ch with
  | '0' .. '9' -> Some (float_of_int (Char.code ch - Char.code '0') *. level)
  | 'g' -> Some 0.
  | _ -> None

let is_ramp_x ch = ch = '>'
let is_ramp_z ch = ch = 'v'

(* [ramp_edges get is_ramp i]: the heights at both edges of the ramp tile
 * [i], on a line of tiles ([get j] the tile [j] of it): the run of ramps
 * around it goes evenly from the flat tile before the run to the one
 * after. E.g. "7>>>4": the edges 7, 6, 5, 4, so the second '>' goes
 * from 6 to 5. *)
let ramp_edges (get : int -> char) (is_ramp : char -> bool) (i : int) : number * number =
  let rec before j = if is_ramp (get j) then before (j - 1) else j in
  let rec after j = if is_ramp (get j) then after (j + 1) else j in
  let a = before i and b = after i in
  match (flat_height (get a), flat_height (get b)) with
  | Some ha, Some hb ->
      let edge k = ha +. ((hb -. ha) *. float_of_int k /. float_of_int (b - a - 1)) in
      (edge (i - a - 1), edge (i - a))
  | _ -> failwith "TinyMarble: a ramp not between two flat tiles"

(* a tile's heights at its four corners *)
type corners = { nw : number; ne : number; se : number; sw : number }

let corners_at (c : int) (r : int) : corners option =
  let ch = char_at c r in
  match flat_height ch with
  | Some h -> Some { nw = h; ne = h; se = h; sw = h }
  | None when is_ramp_x ch ->
      let w, e = ramp_edges (fun i -> char_at i r) is_ramp_x c in
      Some { nw = w; sw = w; ne = e; se = e }
  | None when is_ramp_z ch ->
      let n, s = ramp_edges (fun j -> char_at c j) is_ramp_z r in
      Some { nw = n; ne = n; sw = s; se = s }
  | None -> None

let tiles = Array.init rows (fun r -> Array.init cols (fun c -> corners_at c r))
let tile (c : int) (r : int) : corners option = if r < 0 || r >= rows || c < 0 || c >= cols then None else tiles.(r).(c)

(* the tile under (x, z), and where in it, from (0, 0) at its north-west
 * corner to (1, 1) at its south-east one *)
let locate (x : number) (z : number) : int * int * number * number =
  let fc = Float.floor (x /. cell) and fr = Float.floor (z /. cell) in
  (int_of_float fc, int_of_float fr, (x /. cell) -. fc, (z /. cell) -. fr)

(* [ground x z]: the course's height at (x, z), None over the void;
 * bilinear between the tile's corners (on a ramp, the same as linear) *)
let ground (x : number) (z : number) : number option =
  let c, r, fx, fz = locate x z in
  match tile c r with
  | None -> None
  | Some k ->
      let north = k.nw +. ((k.ne -. k.nw) *. fx) and south = k.sw +. ((k.se -. k.sw) *. fx) in
      Some (north +. ((south -. north) *. fz))

(* [slope x z]: how much the ground rises per unit east and per unit
 * south, at (x, z) (the derivatives of [ground]'s formula) *)
let slope (x : number) (z : number) : number * number =
  let c, r, fx, fz = locate x z in
  match tile c r with
  | None -> (0., 0.)
  | Some k ->
      ( (((k.ne -. k.nw) *. (1. -. fz)) +. ((k.se -. k.sw) *. fz)) /. cell,
        (((k.sw -. k.nw) *. (1. -. fx)) +. ((k.se -. k.ne) *. fx)) /. cell )

let on_goal (x : number) (z : number) : bool =
  let c, r, _, _ = locate x z in
  char_at c r = 'g'

(* where a tile's center is, on the ground *)
let center (c, r) : number * number = ((float_of_int c +. 0.5) *. cell, (float_of_int r +. 0.5) *. cell)

(* where a lost marble comes back: the start, then the last one passed *)
let checkpoints = [| (2, 2); (11, 8); (11, 13); (4, 19) |]
let steelie_home = (7, 13)
let race_time = 35 (* seconds *)

(*****************************************************************************)
(* The ball *)
(*****************************************************************************)

type vec = number * number * number

(* where the ball's own x, y and z axes point: it turns as it rolls *)
type axes = { ex : vec; ey : vec; ez : vec }

type ball = {
  x : number;
  y : number; (* its bottom, on the ground when it rolls *)
  z : number;
  vx : number;
  vy : number;
  vz : number;
  on_ground : bool;
  top : number; (* the highest it has been since it left the ground *)
  axes : axes;
}

let radius = 0.5
let gravity = 0.012
let rolling = 5. /. 7.
let push = 0.005 (* the arrows' acceleration *)
let drag = 0.985 (* rolling resistance: 1.5% of the speed lost each frame *)
let max_speed = 0.35
let step_up = 0.6 (* ground higher than this at the ball's front: a wall *)
let step_down = 0.3 (* ground dropping more than this in a frame: an edge *)
let bounce = 0.5 (* the speed kept bouncing off a wall *)
let max_fall = 3. (* falling from higher than this breaks a marble *)
let lost = -12. (* fallen into the void *)

let ball_at (c, r) : ball =
  let x, z = center (c, r) in
  let y = Option.value (ground x z) ~default:0. in
  { x; y; z; vx = 0.; vy = 0.; vz = 0.; on_ground = true; top = y; axes = { ex = (1., 0., 0.); ey = (0., 1., 0.); ez = (0., 0., 1.) } }

(* a few vector operations, for the axes *)
let add (a, b, c) (d, e, f) = (a +. d, b +. e, c +. f)
let times k (a, b, c) = (k *. a, k *. b, k *. c)
let dot (a, b, c) (d, e, f) = (a *. d) +. (b *. e) +. (c *. f)
let cross (a, b, c) (d, e, f) = ((b *. f) -. (c *. e), (c *. d) -. (a *. f), (a *. e) -. (b *. d))
let normalize v = times (1. /. sqrt (dot v v)) v

(* Rodrigues' rotation formula (1840): [v] turned by [angle] radians
 * around the unit vector [k] *)
let rotate (k : vec) (angle : number) (v : vec) : vec =
  add (times (cos angle) v) (add (times (sin angle) (cross k v)) (times ((1. -. cos angle) *. dot k v) k))

(* [roll_axes vx vz axes]: a ball rolling (vx, vz) without slipping turns
 * by the distance over its radius, around the horizontal axis up x
 * velocity = (vz, 0, -vx): rolling east (+x), around -z, its top going
 * east. The axes are then made perpendicular again (Gram-Schmidt): the
 * rounding errors of thousands of rotations would otherwise skew them. *)
let roll_axes (vx : number) (vz : number) (a : axes) : axes =
  let speed = Float.hypot vx vz in
  if speed < 1e-9 then a
  else
    let k = (vz /. speed, 0., -.vx /. speed) and angle = speed /. radius in
    let ex = normalize (rotate k angle a.ex) and ey = rotate k angle a.ey in
    let ey = normalize (add ey (times (-.dot ey ex) ex)) in
    { ex; ey; ez = cross ex ey }

let limit (vx : number) (vz : number) : number * number =
  let s = Float.hypot vx vz in
  if s > max_speed then (vx *. max_speed /. s, vz *. max_speed /. s) else (vx, vz)

(* the ground at the ball's front (moving by [v] along one axis) higher
 * than a step up *)
let wall (x : number) (z : number) (y : number) : bool =
  match ground x z with Some g -> g > y +. step_up | None -> false

let front (v : number) : number = if v > 0. then radius else if v < 0. then -.radius else 0.

(* [step (px, pz) b]: one frame of the ball [b], pushed towards (px, pz)
 * (a vector of length at most 1); also, when it lands, from how high it
 * fell *)
let step ((px, pz) : number * number) (b : ball) : ball * number option =
  (* the push and the slope, on the ground only: horizontally, the
   * acceleration along a slope (sx, sz) is g sin(a) cos(a), which is
   * g s / (1 + s^2) (tan(a) = s), times 5/7 for a rolling ball *)
  let ax, az =
    if not b.on_ground then (0., 0.)
    else
      let sx, sz = slope b.x b.z in
      let k = rolling *. gravity /. (1. +. (sx *. sx) +. (sz *. sz)) in
      ((push *. px) -. (k *. sx), (push *. pz) -. (k *. sz))
  in
  let vx, vz = limit ((b.vx +. ax) *. drag) ((b.vz +. az) *. drag) in
  (* moving, one axis at a time, bounced back by walls *)
  let x, vx = if wall (b.x +. vx +. front vx) b.z b.y then (b.x, -.bounce *. vx) else (b.x +. vx, vx) in
  let z, vz = if wall x (b.z +. vz +. front vz) b.y then (b.z, -.bounce *. vz) else (b.z +. vz, vz) in
  let b = { b with x; z; vx; vz; axes = roll_axes vx vz b.axes } in
  (* up and down: sticking to the ground, or falling *)
  match ground x z with
  | Some g when b.on_ground && g > b.y -. step_down -> ({ b with y = g; vy = g -. b.y; top = g }, None)
  | g -> (
      let vy = b.vy -. gravity in
      let y = b.y +. vy in
      let top = Float.max b.top y in
      match g with
      | Some g when y <= g ->
          (* landing: a little bounce if it fell fast *)
          let fall = top -. g in
          if vy < -0.1 then ({ b with y = g; vy = -0.3 *. vy; top = g; on_ground = false }, Some fall)
          else ({ b with y = g; vy = 0.; top = g; on_ground = true }, Some fall)
      | _ -> ({ b with y; vy; top; on_ground = false }, None))

(* [collide a ma b mb]: two balls of masses [ma] and [mb] touching and
 * coming closer bounce off each other, elastically: along the line
 * between their centers, their speeds va and vb become
 *   va' = ((ma - mb) va + 2 mb vb) / (ma + mb)
 *   vb' = ((mb - ma) vb + 2 ma va) / (ma + mb)
 * (the momentum and the energy kept), their speeds across it unchanged.
 * E.g. a marble (1) at rest hit by the steelie (2) at 0.1: the marble
 * goes off at 0.4 / 3 = 0.133, the steelie goes on at 0.1 / 3 = 0.033.
 * With equal masses they exchange their speeds: billiards. *)
let collide (a : ball) (ma : number) (b : ball) (mb : number) : ball * ball =
  let dx = b.x -. a.x and dz = b.z -. a.z in
  let d = Float.hypot dx dz in
  if d >= 2. *. radius || d < 1e-6 || Float.abs (a.y -. b.y) > radius then (a, b)
  else
    let nx = dx /. d and nz = dz /. d in
    let va = (a.vx *. nx) +. (a.vz *. nz) and vb = (b.vx *. nx) +. (b.vz *. nz) in
    if va <= vb then (a, b) (* moving apart already *)
    else
      let va' = (((ma -. mb) *. va) +. (2. *. mb *. vb)) /. (ma +. mb) in
      let vb' = (((mb -. ma) *. vb) +. (2. *. ma *. va)) /. (ma +. mb) in
      ( { a with vx = a.vx +. ((va' -. va) *. nx); vz = a.vz +. ((va' -. va) *. nz) },
        { b with vx = b.vx +. ((vb' -. vb) *. nx); vz = b.vz +. ((vb' -. vb) *. nz) } )

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type fate = Rolling | Broken of int (* frames since *)

type race = {
  me : ball;
  fate : fate;
  checkpoint : int; (* in [checkpoints] *)
  steelie : ball;
  time_left : int; (* frames *)
  cam : camera option; (* the camera, smoothed *)
}

type scene = Title | Racing of race | Finished of race | Time_up of race
type model = scene Scene2d.t

let new_race () =
  { me = ball_at checkpoints.(0); fate = Rolling; checkpoint = 0; steelie = ball_at steelie_home; time_left = race_time * 60; cam = None }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* The push asked for, in the world: the camera looks from the south-east,
 * so the screen's right is the world's (1, -1) and its up (-1, -1) (both
 * over sqrt 2). The mouse as a trackball: 8 pixels a frame, a full push. *)
let wanted_push (computer : computer) : number * number =
  let k = computer.keyboard and m = computer.mouse in
  let key b = if b then 1. else 0. in
  let right = key k.kright -. key k.kleft +. (m.mdx /. 8.) and up = key k.kup -. key k.kdown +. (m.mdy /. 8.) in
  let wx = (right -. up) /. sqrt 2. and wz = (-.right -. up) /. sqrt 2. in
  let n = Float.hypot wx wz in
  if n > 1. then (wx /. n, wz /. n) else (wx, wz)

(* the steelie pushes towards you, when you're near, at 60% of your push *)
let steelie_push (r : race) : number * number =
  let dx = r.me.x -. r.steelie.x and dz = r.me.z -. r.steelie.z in
  let d = Float.hypot dx dz in
  if r.fate <> Rolling || d > 12. || d < 1e-6 then (0., 0.) else (0.6 *. dx /. d, 0.6 *. dz /. d)

let camera_for (b : ball) : camera = Camera3d.from_far ~fov:30. ~offset:(22., 30., 22.) (b.x, b.y +. radius, b.z)

let respawn (r : race) : race = { r with me = ball_at checkpoints.(r.checkpoint); fate = Rolling }

(* the last checkpoint passed: the ball rolling on one further on *)
let passed (r : race) : int =
  let near i =
    let x, z = center checkpoints.(i) in
    r.me.on_ground && Float.hypot (r.me.x -. x) (r.me.z -. z) < cell
  in
  let rec go i best = if i >= Array.length checkpoints then best else go (i + 1) (if near i then i else best) in
  go (r.checkpoint + 1) r.checkpoint

let update_race (computer : computer) (r : race) : race =
  let steelie, _ = step (steelie_push r) r.steelie in
  let steelie = if steelie.y < lost then ball_at steelie_home else steelie in
  let r = { r with steelie; time_left = r.time_left - 1 } in
  let r =
    match r.fate with
    | Broken k -> if k >= 60 then respawn r else { r with fate = Broken (k + 1) }
    | Rolling -> (
        let me, fall = step (wanted_push computer) r.me in
        let me, steelie = collide me 1. r.steelie 2. in
        let r = { r with me; steelie } in
        match fall with
        | Some h when h > max_fall -> { r with fate = Broken 0 }
        | _ -> if me.y < lost then respawn r else { r with checkpoint = passed r })
  in
  let wanted = camera_for r.me in
  { r with cam = Some (match r.cam with None -> wanted | Some cam -> Camera3d.follow 0.12 wanted cam) }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Racing (new_race ())) s else s
  | Racing r ->
      let r = update_race computer r in
      if r.fate = Rolling && r.me.on_ground && on_goal r.me.x r.me.z then Scene2d.go (Finished r) s
      else if r.time_left <= 0 then Scene2d.go (Time_up r) s
      else { s with scene = Racing r }
  | Finished _ | Time_up _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let tile_color (c : int) (r : int) : color =
  let odd = (c + r) mod 2 = 1 in
  match char_at c r with
  | 'g' -> if odd then black else white
  | 'v' | '>' -> if odd then rgb 205 185 120 else rgb 190 170 105
  | _ -> if odd then rgb 150 170 235 else rgb 125 145 220

let side_color = rgb 110 80 170
let bottom = -4. (* where the sides over the void end *)

(* A tile: its top, and its sides where the next tile is lower (or the
 * void), from its edge down to that tile's edge. The corners in the
 * order of Playground3d's box faces, so their fronts face out. *)
let tile_shapes (c : int) (r : int) (k : corners) : shape3d list =
  let x0 = float_of_int c *. cell and z0 = float_of_int r *. cell in
  let x1 = x0 +. cell and z1 = z0 +. cell in
  let top = polygon3d (tile_color c r) [ (x0, k.nw, z0); (x0, k.sw, z1); (x1, k.se, z1); (x1, k.ne, z0) ] in
  let below (neighbor : corners option) (pick : corners -> number * number) (a, b) =
    match neighbor with None -> (bottom, bottom) | Some n -> let na, nb = pick n in (Float.min a na, Float.min b nb)
  in
  let side (a, b) (ba, bb) points = if a > ba || b > bb then [ polygon3d side_color (points (a, b) (ba, bb)) ] else [] in
  let east =
    side (k.ne, k.se) (below (tile (c + 1) r) (fun n -> (n.nw, n.sw)) (k.ne, k.se)) (fun (n, s) (bn, bs) ->
        [ (x1, bn, z0); (x1, n, z0); (x1, s, z1); (x1, bs, z1) ])
  in
  let west =
    side (k.nw, k.sw) (below (tile (c - 1) r) (fun n -> (n.ne, n.se)) (k.nw, k.sw)) (fun (n, s) (bn, bs) ->
        [ (x0, bs, z1); (x0, s, z1); (x0, n, z0); (x0, bn, z0) ])
  in
  let south =
    side (k.sw, k.se) (below (tile c (r + 1)) (fun n -> (n.nw, n.ne)) (k.sw, k.se)) (fun (w, e) (bw, be) ->
        [ (x0, bw, z1); (x1, be, z1); (x1, e, z1); (x0, w, z1) ])
  in
  let north =
    side (k.nw, k.ne) (below (tile c (r - 1)) (fun n -> (n.sw, n.se)) (k.nw, k.ne)) (fun (w, e) (bw, be) ->
        [ (x0, bw, z0); (x0, w, z0); (x1, e, z0); (x1, be, z0) ])
  in
  (top :: east) @ west @ south @ north

let course_shape : shape3d =
  cached3d
    (List.concat
       (List.init rows (fun r ->
            List.concat (List.init cols (fun c -> match tile c r with Some k -> tile_shapes c r k | None -> [])))))

(* A marble: a sphere (as Playground3d.sphere) whose points are turned by
 * its axes, so that its band shows it rolling *)
let marble (color : color) (band : color) (b : ball) : shape3d =
  let lats = 8 and lons = 12 in
  let point lat lon =
    let theta = Float.pi *. float_of_int lat /. float_of_int lats in
    let phi = 2. *. Float.pi *. float_of_int lon /. float_of_int lons in
    let px = sin theta *. cos phi and py = cos theta and pz = sin theta *. sin phi in
    let dx, dy, dz = add (times px b.axes.ex) (add (times py b.axes.ey) (times pz b.axes.ez)) in
    (b.x +. (radius *. dx), b.y +. radius +. (radius *. dy), b.z +. (radius *. dz))
  in
  group3d
    (List.concat
       (List.init lats (fun lat ->
            List.init lons (fun lon ->
                polygon3d
                  (if lat = 3 || lat = 4 then band else color)
                  [ point lat lon; point lat (lon + 1); point (lat + 1) (lon + 1); point (lat + 1) lon ]))))

(* its shadow on the course, when it's in the air: where it will land *)
let shadow (b : ball) : shape3d list =
  match ground b.x b.z with
  | Some g when not b.on_ground ->
      let at dx dz = (b.x +. dx, Option.value (ground (b.x +. dx) (b.z +. dz)) ~default:g +. 0.03, b.z +. dz) in
      let s = radius *. 0.8 in
      [ polygon3d (rgb 40 40 70) [ at (-.s) (-.s); at (-.s) s; at s s; at s (-.s) ] ]
  | _ -> []

let me_blue = rgb 40 90 230

(* a broken marble: its pieces flying, [k] frames after *)
let pieces (b : ball) (k : int) : shape3d list =
  let t = float_of_int k in
  List.init 10 (fun i ->
      let a = 2. *. Float.pi *. float_of_int i /. 10. in
      let x = b.x +. (cos a *. 0.06 *. t) and z = b.z +. (sin a *. 0.06 *. t) in
      let up = if i mod 3 = 0 then 0.18 else 0.12 in
      let y = b.y +. radius +. (up *. t) -. (0.5 *. gravity *. t *. t) in
      box (if i mod 2 = 0 then me_blue else white) 0.35 0.35 0.35 |> rotate3d (7. *. t) (11. *. t) 0. |> move3d x y z)

let steel = rgb 50 50 60
let void = rgb 15 10 40
let text color size str = words color str |> scale size

let view_race (screen : screen) (r : race) : camera * shape3d list =
  let cam = match r.cam with Some c -> c | None -> camera_for r.me in
  let me = match r.fate with Rolling -> marble me_blue white r.me :: shadow r.me | Broken k -> pieces r.me k in
  let seconds = (r.time_left + 59) / 60 in
  ( cam,
    [ Camera3d.floor ~color:void ~ground:(-15.) cam; course_shape ]
    @ me
    @ (marble steel (rgb 130 130 140) r.steelie :: shadow r.steelie)
    @ [ hud (text (if seconds <= 10 then red else yellow) 4. (Printf.sprintf "TIME %d" seconds) |> move_y (screen.top -. 50.)) ]
  )

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let cx = float_of_int cols *. cell /. 2. and cz = float_of_int rows *. cell /. 2. in
      let cam = Camera3d.orbit ~distance:45. ~height:35. ~look:0. (spin 12. computer.time) (cx, 2., cz) in
      ( cam,
        [ Camera3d.floor ~color:void ~ground:(-15.) cam; course_shape; marble me_blue white (ball_at checkpoints.(0));
          marble steel (rgb 130 130 140) (ball_at steelie_home) ]
        @ List.map hud
            ([ text (rgb 120 170 255) 7. "TINY MARBLE" |> move_y 320.;
               text white 2.5 "arrows (or the mouse, as a trackball): roll the marble to the goal" |> move_y 250.;
               text white 2.5 "don't fall too far, beware the steelie" |> move_y 215. ]
            @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 160. ]) )
  | Racing r -> view_race screen r
  | Finished r ->
      let cam, shapes = view_race screen r in
      ( cam,
        shapes
        @ List.map hud
            ([ text yellow 6. "GOAL!" |> move_y 200.;
               text white 3. (Printf.sprintf "%d seconds to spare" ((r.time_left + 59) / 60)) |> move_y 130. ]
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 70. ]) )
  | Time_up r ->
      let cam, shapes = view_race screen r in
      ( cam,
        shapes
        @ List.map hud
            ([ text red 6. "TIME'S UP" |> move_y 200. ] @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 130. ]) )

let app = game3d view update initial_model

(* flat shading: each tile and facet of the marbles its own shade *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
