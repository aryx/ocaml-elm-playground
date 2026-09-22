(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy Worms (Andy Davidson, Team17, 1995): two worms on one keyboard,
 * taking turns, in a landscape of hills and caves over the water.
 * Left/right walk, up/down aim, Enter jumps; 1 the bazooka, 2 the
 * grenade, 3 the ninja rope; space held charges a shot, let go fires
 * it. On the rope, left/right swing, up/down climb, space lets go.
 *
 * The genre is one of the oldest: "Artillery" games ran on mainframes
 * and the first home computers in the late 1970s (two cannons, a hill,
 * type an angle and a speed); Gorillas (1991, shipped with MS-DOS's
 * QBasic) threw exploding bananas between skyscrapers, with wind;
 * Scorched Earth (Wendell Hicks, 1991) gave tanks a terrain that
 * explosions carve away; Worms made the cannons into worms that walk,
 * and the terrain into a cartoon island with caves.
 *
 * What it teaches is physics, three ways:
 *
 *  - The projectiles are bodies of the playground's Physics layer
 *    (playground/Physics.mli), stepped with semi-implicit Euler: the
 *    bazooka's shell falls and is pushed by the wind (a parabola, bent
 *    by a constant sideways push: notes_2d_physics.md section 4), and
 *    explodes where it touches; the grenade falls too, but bounces off
 *    the ground ([bounce]: the velocity reflected off the ground's
 *    slope, read from the bitmap, and half of it lost) and waits for
 *    its fuse.
 *
 *  - The terrain is a bitmap ([terrain], TinyLemmings' way): a byte per
 *    4x4 cell, earth, girder or air. That is what gives Worms its caves
 *    and overhangs, a height map having one height per column; an
 *    explosion cuts a circle out of it anywhere, a tunnel as easily as
 *    a crater. The worms walk on it cell by cell, up a slope of a few
 *    cells, not up a wall ([walk]).
 *
 *  - The ninja rope is a joint (physics/2d/Joint2d, the engine's
 *    [Physics.rope]): fired along your aim, it hooks the first earth or
 *    girder it meets, and you hang from it, a pendulum. Swinging is a
 *    push at the right moment of each swing, as on a playground swing;
 *    climbing changes the rope's length; letting go keeps your speed,
 *    which is how a good player crosses the map in a turn:
 *
 *                 hook *
 *                      |\        a rope: pulls when taut, slack
 *                      | \       shorter -- the worm swings under the
 *                      |  \      hook, on a circle of the rope's length
 *                      |   o worm
 *
 * What it uses: Physics (bodies for the shell and the grenade, a world
 * with a rope joint for the ninja rope), Scene2d. Not TinyLemmings'
 * terrain code itself: the same idea, written again here -- a
 * destructible-terrain kit would have these two games as its users.
 *
 * Exercises: teams of worms, the turn passing from one to the next; the
 * turn timer; the air strike and the sheep; the rope's second shot (a
 * worm hanging from a rope fires it again from where it is, to go
 * further); fall damage; a computer opponent (aiming by simulating
 * shots: a search, see plan_teaching_other.md's game AI).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The terrain: a bitmap *)
(*****************************************************************************)

let cell = 4.
let cols = 250 (* the screen's width, 1000 *)
let rows = 215 (* from the top at 400 down to -460 *)
let top = 400.
let left = -500.
let water = -430. (* under this, a worm drowns *)

type terrain = Bytes.t

let air = 0
let earth = 1
let girder = 2

let col_of (x : number) : int = int_of_float (Float.floor ((x - left) / cell))
let row_of (y : number) : int = int_of_float (Float.floor ((top - y) / cell))
let x_of (c : int) : number = left + ((float_of_int c + 0.5) * cell)
let y_of (r : int) : number = top - ((float_of_int r + 0.5) * cell)

let get (t : terrain) (c : int) (r : int) : int =
  if c < 0 || c >= cols || r < 0 || r >= rows then air else Bytes.get_uint8 t ((r *.. cols) +.. c)

let solid_at (t : terrain) (x : number) (y : number) : bool = get t (col_of x) (row_of y) <> air

(* a circle cut out of the terrain, on a copy: an explosion *)
let carve (t : terrain) (cx : number) (cy : number) (radius : number) : terrain =
  let t = Bytes.copy t in
  let r0 = row_of (cy + radius) and r1 = row_of (cy - radius) and c0 = col_of (cx - radius) and c1 = col_of (cx + radius) in
  for r = max 0 r0 to min (rows -.. 1) r1 do
    for c = max 0 c0 to min (cols -.. 1) c1 do
      if Float.hypot (x_of c - cx) (y_of r - cy) <= radius then Bytes.set_uint8 t ((r *.. cols) +.. c) air
    done
  done;
  t

(* The island: rolling hills (three sines with random phases) with the
 * earth under them; caves dug in it, round rooms and the tunnels
 * between them; and three girders across the sky *)
let island () : terrain =
  let t = Bytes.make (cols *.. rows) (Char.chr air) in
  let phase () = Random.float (2. * Float.pi) in
  let p1 = phase () and p2 = phase () and p3 = phase () in
  let surface x = -120. + (90. * sin ((x / 170.) + p1)) + (45. * sin ((x / 70.) + p2)) + (15. * sin ((x / 25.) + p3)) in
  for c = 0 to cols -.. 1 do
    (* the island stops short of the edges: water on both sides *)
    let x = x_of c in
    if Float.abs x < 470. then
      for r = row_of (surface x) to rows -.. 1 do
        if y_of r > water - 20. then Bytes.set_uint8 t ((r *.. cols) +.. c) earth
      done
  done;
  let t = ref t in
  for _ = 1 to 4 do
    let x = Random.float 700. - 350. in
    let y = surface x - 90. - Random.float 100. in
    t := carve !t x y (40. + Random.float 25.);
    (* a tunnel from it, sideways *)
    let dir = if Random.bool () then 1. else -1. in
    for k = 1 to 12 do t := carve !t (x + (dir * float_of_int k * 12.)) (y + (float_of_int k * 2.)) 16. done
  done;
  let t = !t in
  List.iter
    (fun (x, y, w) ->
      for c = col_of (x - (w / 2.)) to col_of (x + (w / 2.)) do
        for r = row_of y to row_of y +.. 2 do
          if c >= 0 && c < cols then Bytes.set_uint8 t ((r *.. cols) +.. c) girder
        done
      done)
    [ (-220., 250. + Random.float 60., 160.); (60., 280. + Random.float 60., 200.); (300., 230. + Random.float 60., 150.) ];
  t

(* the highest ground under (x, from y down): where a worm dropped there
 * lands *)
let ground_below (t : terrain) (x : number) (y : number) : number =
  let rec down r = if r >= rows then water - 100. else if get t (col_of x) r <> air then y_of r + (cell / 2.) else down (r +.. 1) in
  down (row_of y)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

let radius = 8. (* a worm's *)

type worm = {
  x : number;
  y : number; (* its middle, [radius] above its feet *)
  vx : number; (* when flying: jumped, blown, let go of the rope *)
  vy : number;
  airborne : bool;
  facing : number; (* 1 right, -1 left *)
  aim : number; (* degrees above level, -80 to 80, the way it faces *)
  health : number;
  color : color;
}

type weapon = Bazooka | Grenade | Rope

type phase =
  | Moving of int option (* walking and aiming; the frames space has been held, charging *)
  | Roping of Physics.world * (number * number) (* the worm and its rope, and where it hooked *)
  | Shell of Physics.body
  | Thrown of Physics.body * int (* the grenade, and its fuse's frames left *)
  | Blast of number * number * int (* where, and frames of the explosion left *)
  | Settling (* the worms landing after a blast, before the next turn *)

type game = {
  terrain : terrain;
  worms : worm array; (* 2 *)
  turn : int;
  weapon : weapon;
  wind : number;
  phase : phase;
}

type scene = Title | Playing of game | Winner of int
type model = scene Scene2d.t

let gravity = 400.
let blast_radius = 45.
let new_wind () = Random.float 300. - 150.

let new_game () : game =
  let terrain = island () in
  let worm x color facing =
    (* on the island, under the girders *)
    { x; y = ground_below terrain x 200. + radius; vx = 0.; vy = 0.; airborne = false; facing; aim = 30.; health = 100.; color }
  in
  { terrain; worms = [| worm (-330.) red 1.; worm 330. blue (-1.) |]; turn = 0; weapon = Bazooka; wind = new_wind (); phase = Moving None }

let initial_model : model = Scene2d.start Title

(* the aim as an angle in degrees, 0 right, 90 up *)
let aim_angle (w : worm) : number = if w.facing > 0. then w.aim else 180. - w.aim

(*****************************************************************************)
(* The worms *)
(*****************************************************************************)

(* the circle of a worm meeting the terrain: its bottom, its sides,
 * its top -- and how many of those points are in it, how deep it is *)
let points = [ (0., -.radius); (-.radius *. 0.7, -.radius *. 0.7); (radius *. 0.7, -.radius *. 0.7); (-.radius, 0.); (radius, 0.); (0., radius) ]
let depth (t : terrain) (x : number) (y : number) : int = List.length (List.filter (fun (dx, dy) -> solid_at t (x + dx) (y + dy)) points)
let touches (t : terrain) (x : number) (y : number) : bool = depth t x y > 0

(* A step of a walking worm: a little sideways, and up a slope of up to
 * 3 cells, not up a wall; nothing under it, and it falls *)
let walk (t : terrain) (dir : number) (w : worm) : worm =
  let x = w.x + (1.2 * dir) in
  let rec climb k = if k > 12 then None else if touches t x (w.y + float_of_int k) then climb (k +.. 1) else Some (w.y + float_of_int k) in
  match climb 0 with
  | None -> { w with facing = dir }
  | Some y ->
      let w = { w with x; y; facing = dir } in
      if solid_at t x (y - radius - 2.) then w else { w with airborne = true; vx = 0.; vy = 0. }

(* A step of a worm in the air: gravity, and the terrain stopping it --
 * on the ground it lands, against a wall or a ceiling it stops that
 * way *)
let fly (t : terrain) (w : worm) : worm =
  if not w.airborne then w
  else
    let vy = w.vy - (gravity / 60.) in
    let x = w.x + (w.vx / 60.) and y = w.y + (vy / 60.) in
    if not (touches t x y) then { w with x; y; vy }
    else if vy < 0. && not (touches t x w.y) then
      (* the ground: it lands, and slides no further *)
      let rec up y = if touches t x y then up (y + 1.) else y in
      { w with x; y = up w.y; vx = 0.; vy = 0.; airborne = false }
    else if not (touches t w.x y) then { w with y; vx = 0.; vy }
    else { w with vx = 0.; vy = Float.min 0. vy }

let drowned (w : worm) : bool = w.y < water

(*****************************************************************************)
(* The weapons *)
(*****************************************************************************)

(* the power of a shot held [held] frames, 20% to 100% *)
let power (held : int) : number = 200. + (float_of_int (min held 60) * 13.)

let launch (w : worm) (speed : number) (shape : shape) : Physics.body =
  let a = aim_angle w in
  Physics.body shape |> Physics.at (w.x + (14. * cos (a * pi / 180.))) (w.y + (14. * sin (a * pi / 180.))) |> Physics.launched speed a

(* the ground's way out at (x, y): away from the solid cells around, the
 * sum of the directions to the empty ones *)
let normal (t : terrain) (x : number) (y : number) : number * number =
  let nx = ref 0. and ny = ref 0. in
  for dc = -3 to 3 do
    for dr = -3 to 3 do
      if not (solid_at t (x + (float_of_int dc * cell)) (y + (float_of_int dr * cell))) then begin
        nx := !nx + float_of_int dc;
        ny := !ny + float_of_int dr
      end
    done
  done;
  let d = Float.hypot !nx !ny in
  if d = 0. then (0., 1.) else (!nx / d, !ny / d)

(* A grenade meeting the ground bounces: its velocity reflected off the
 * ground's slope, v - 2 (v . n) n, and half of it lost *)
let bounce (t : terrain) (b : Physics.body) : Physics.body =
  let next = Physics.step (Physics.fall gravity b) in
  if not (solid_at t next.x next.y) then next
  else
    let nx, ny = normal t next.x next.y in
    let dot = (b.vx * nx) + (b.vy * ny) in
    if dot >= 0. then { b with vy = b.vy - (gravity / 60.) }
    else { b with vx = 0.55 * (b.vx - (2. * dot * nx)); vy = 0.55 * (b.vy - (2. * dot * ny)) }

(* The rope, fired along the aim: the first solid cell within 520
 * pixels (enough to reach the girders from the hills), if any *)
let hook (t : terrain) (w : worm) : (number * number) option =
  let a = aim_angle w * pi / 180. in
  let rec along d = if d > 520. then None else let x = w.x + (d * cos a) and y = w.y + (d * sin a) in if solid_at t x y then Some (x, y) else along (d + 2.) in
  along 12.

(* the worm on its rope: a world of two bodies, the hook (immovable) and
 * the worm, joined by a rope as long as they are apart *)
let rope_world (w : worm) ((hx, hy) : number * number) : Physics.world =
  let anchor = Physics.body (circle white 2.) |> Physics.at hx hy |> Physics.immovable in
  let body = Physics.body (circle w.color radius) |> Physics.at w.x w.y |> Physics.moving w.vx w.vy |> Physics.upright in
  Physics.world [ anchor; body ] |> Physics.rope 0 1 ~at_a:(hx, hy) ~at_b:(w.x, w.y)

(* A frame on the rope: a push to swing, the rope shortened or let out,
 * the world simulated -- and the terrain, which the rope's world knows
 * nothing of, stopping the worm where it would go deeper into it (a
 * worm roped from where it stands is in touch with the ground already:
 * it may slide along it, or be lifted off it, not sink) *)
let swing (t : terrain) (swing_dir : number) (climb : number) (w : Physics.world) : Physics.world =
  let w =
    match w.joints with
    | [ ({ kind = Joint2d.Rope { length }; _ } as j) ] ->
        { w with joints = [ { j with kind = Joint2d.Rope { length = Float.max 20. (Float.min 520. (length - (2. * climb))) } } ] }
    | _ -> w
  in
  let before = List.nth w.bodies 1 in
  let w = { w with bodies = [ List.hd w.bodies; Physics.push (220. * swing_dir) 0. before ] } in
  let w = Physics.simulate ~gravity w in
  let after = List.nth w.bodies 1 in
  if depth t after.x after.y > depth t before.x before.y then { w with bodies = [ List.hd w.bodies; { before with vx = -0.3 *. before.vx; vy = -0.3 *. before.vy } ] }
  else w

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let current (g : game) : worm = g.worms.(g.turn)
let with_current (g : game) (w : worm) : game = { g with worms = Array.mapi (fun i x -> if i = g.turn then w else x) g.worms }

(* The blast: the terrain carved, the worms near hurt, and blown away
 * from it *)
let explode (g : game) (cx : number) (cy : number) : game =
  let worms =
    Array.map
      (fun (w : worm) ->
        let d = Float.hypot (w.x - cx) (w.y - cy) in
        if d > 70. then w
        else
          let push = (70. - d) * 7. in
          let dx = (w.x - cx) / Float.max 1. d and dy = (w.y - cy) / Float.max 1. d in
          { w with health = Float.max 0. (w.health - ((70. - d) * 0.8)); airborne = true; vx = push * dx; vy = (push * dy) + 150. })
      g.worms
  in
  Audio.play Audio.explosion;
  { g with terrain = carve g.terrain cx cy blast_radius; worms; phase = Blast (cx, cy, 30) }

let next_turn (g : game) : game = { g with turn = 1 -.. g.turn; wind = new_wind (); phase = Moving None; weapon = (if g.weapon = Rope then Bazooka else g.weapon) }

(* the worms falling where the blast threw them *)
let settle (g : game) : game = { g with worms = Array.map (fly g.terrain) g.worms }

let hits_worm (g : game) (b : Physics.body) : bool =
  Array.exists (fun (w : worm) -> Float.hypot (b.x - w.x) (b.y - w.y) < radius + 4.) g.worms

(* the hands: walking, aiming, the weapon keys, space *)
type hands = { dir : number; aim_by : number; jump : bool; pick : weapon option; held : bool; pressed : bool; released : bool }

let step (h : hands) (g : game) : game =
  let t = g.terrain in
  let g = settle g in
  match g.phase with
  | Moving charge ->
      let w = current g in
      let g = match h.pick with Some weapon -> { g with weapon } | None -> g in
      let w =
        if w.airborne then w
        else
          let w = if h.dir <> 0. then walk t h.dir w else w in
          let w = { w with aim = Float.max (-80.) (Float.min 80. (w.aim + (1.5 * h.aim_by))) } in
          if h.jump then { w with airborne = true; vx = 110. * w.facing; vy = 230. } else w
      in
      let g = with_current g w in
      (match (g.weapon, charge) with
      | Rope, _ when h.pressed && not w.airborne -> (
          match hook t w with Some at -> { g with phase = Roping (rope_world w at, at) } | None -> g)
      | Rope, _ -> g
      | _, None -> if h.pressed && not w.airborne then { g with phase = Moving (Some 0) } else g
      | _, Some held ->
          if h.held then { g with phase = Moving (Some (held +.. 1)) }
          else begin
            Audio.play Audio.laser;
            match g.weapon with
            | Grenade -> { g with phase = Thrown (launch w (power held) (circle (rgb 40 90 40) 5.), 180) }
            | _ -> { g with phase = Shell (launch w (power held) (circle black 5.)) }
          end)
  | Roping (world, at) ->
      if h.pressed then
        (* let go: the worm flies on with the rope's speed *)
        let b = List.nth world.bodies 1 in
        with_current { g with phase = Moving None } { (current g) with x = b.x; y = b.y; vx = b.vx; vy = b.vy; airborne = true }
      else
        let world = swing t h.dir h.aim_by world in
        let b = List.nth world.bodies 1 in
        with_current { g with phase = Roping (world, at) } { (current g) with x = b.x; y = b.y; facing = (if h.dir <> 0. then h.dir else (current g).facing) }
  | Shell b ->
      let b = b |> Physics.fall gravity |> Physics.push g.wind 0. |> Physics.step in
      if solid_at t b.x b.y || hits_worm g b then explode g b.x b.y
      else if Float.abs b.x > 560. || b.y < water - 40. then { g with phase = Settling }
      else { g with phase = Shell b }
  | Thrown (b, fuse) ->
      if fuse <= 0 then explode g b.x b.y
      else if b.y < water - 40. || Float.abs b.x > 560. then { g with phase = Settling }
      else { g with phase = Thrown (bounce t b, fuse -.. 1) }
  | Blast (x, y, n) -> if n > 0 then { g with phase = Blast (x, y, n -.. 1) } else { g with phase = Settling }
  | Settling -> if Array.exists (fun w -> w.airborne && not (drowned w)) g.worms then g else next_turn g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let key f = Scene2d.pressed f scenes in
  let letter l = key (fun k -> Set_.mem l k.keys) in
  match scenes.scene with
  | Title -> if key (fun k -> k.kspace) then Scene2d.go (Playing (new_game ())) scenes else scenes
  | Playing g -> (
      let k = computer.keyboard in
      let h =
        { dir = to_x k; aim_by = to_y k; jump = key (fun k -> k.kenter);
          pick = (if letter "1" then Some Bazooka else if letter "2" then Some Grenade else if letter "3" then Some Rope else None);
          held = k.kspace; pressed = key (fun k -> k.kspace); released = (not k.kspace) && scenes.before.kspace }
      in
      let g = step h g in
      let dead i = g.worms.(i).health <= 0. || drowned g.worms.(i) in
      match g.phase with
      | Moving _ when dead 0 -> Scene2d.go (Winner 1) scenes
      | Moving _ when dead 1 -> Scene2d.go (Winner 0) scenes
      | _ -> { scenes with scene = Playing g })
  | Winner _ -> if key (fun k -> k.kspace) && scenes.elapsed > 1. then Scene2d.go Title scenes else scenes

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the terrain, each row's runs of the same cells one rectangle each
 * (TinyLemmings' trick: a few hundred shapes, not 50,000); grass on the
 * earth's top cells *)
let view_terrain (t : terrain) : shape list =
  List.concat
    (List.init rows (fun r ->
         let rec runs c acc =
           if c >= cols then acc
           else
             let v = get t c r in
             let grassy = v = earth && get t c (r -.. 1) = air in
             let rec stop e = if e < cols && get t e r = v && (v <> earth || (get t e (r -.. 1) = air) = grassy) then stop (e +.. 1) else e in
             let e = stop c in
             let color = if v = girder then rgb 150 150 165 else if grassy then rgb 90 170 70 else if r mod 6 < 3 then rgb 150 100 60 else rgb 135 90 55 in
             let acc = if v = air then acc else (rectangle color (float_of_int (e -.. c) * cell) cell |> move ((x_of c + x_of (e -.. 1)) / 2.) (y_of r)) :: acc in
             runs e acc
         in
         runs 0 []))

let view_worm (active : bool) (w : worm) : shape list =
  if drowned w then []
  else
    [ group
        [ oval (rgb 240 150 170) (2. * radius) (2.4 * radius); circle white 3. |> move (3. * w.facing) 4.; circle black 1.5 |> move (4. * w.facing) 4.;
          rectangle w.color (2. * radius) 3. |> move 0. 7. ]
      |> move w.x w.y;
      text (if active then yellow else white) 1.3 (Printf.sprintf "%.0f" w.health) |> move w.x (w.y + 22.) ]

let view_game (g : game) : shape list =
  let w = current g in
  let a = aim_angle w * pi / 180. in
  let crosshair = match g.phase with Moving _ when not w.airborne -> [ circle (rgb 250 60 60) 4. |> move (w.x + (60. * cos a)) (w.y + (60. * sin a)) ] | _ -> [] in
  let charging = match g.phase with Moving (Some held) -> [ rectangle (rgb 250 180 40) (float_of_int (min held 60) * 1.5) 8. |> move (w.x - 45. + (float_of_int (min held 60) * 0.75)) (w.y - 20.) ] | _ -> [] in
  view_terrain g.terrain
  @ [ rectangle (rgb 40 90 180) 1000. 80. |> fade 0.85 |> move 0. (water - 40.) ]
  @ List.concat (List.mapi (fun i x -> view_worm (i = g.turn) x) (Array.to_list g.worms))
  @ crosshair @ charging
  @ (match g.phase with
    | Roping (world, (hx, hy)) ->
        let b = List.nth world.bodies 1 in
        let dx = b.x - hx and dy = b.y - hy in
        [ rectangle (rgb 60 50 40) (Float.hypot dx dy) 2. |> rotate (atan2 dy dx * 180. / pi) |> move ((hx + b.x) / 2.) ((hy + b.y) / 2.);
          circle (rgb 60 50 40) 3. |> move hx hy ]
    | Shell b -> [ Physics.draw b ]
    | Thrown (b, fuse) -> [ Physics.draw b; text white 1.3 (string_of_int ((fuse +.. 59) /.. 60)) |> move b.x (b.y + 14.) ]
    | Blast (x, y, n) ->
        let r = blast_radius * (1. - (float_of_int n / 30.)) in
        [ circle orange r |> fade 0.8 |> move x y; circle yellow (r / 2.) |> move x y ]
    | Moving _ | Settling -> [])

let view_hud (g : game) : shape list =
  let bar i (w : worm) =
    let x = if i = 0 then -350. else 350. in
    [ rectangle (rgb 60 60 60) 204. 24. |> move x 460.; rectangle w.color (2. * w.health) 20. |> move (x - 100. + w.health) 460. ]
  in
  let arrow = group [ rectangle white (Float.abs g.wind / 2.) 6.; triangle white 8. |> rotate (if g.wind < 0. then 180. else 0.) |> move_x (g.wind / 4.) ] in
  let weapon_name = function Bazooka -> "1 BAZOOKA" | Grenade -> "2 GRENADE" | Rope -> "3 NINJA ROPE" in
  List.concat (List.mapi bar (Array.to_list g.worms))
  @ [ text white 2. (Printf.sprintf "WIND %+.0f" g.wind) |> move 0. 470.; arrow |> move 0. 440. ]
  @ List.mapi
      (fun i wp -> text (if wp = g.weapon then yellow else rgb 200 210 230) (if wp = g.weapon then 2.2 else 1.8) (weapon_name wp) |> move (-250. + (float_of_int i * 250.)) (-475.))
      [ Bazooka; Grenade; Rope ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 110 170 230) screen.width screen.height
  ::
  (match model.scene with
  | Title ->
      [ text white 6. "TINY WORMS" |> move_y 220.;
        text white 2. "two worms, taking turns, on an island of caves" |> move_y 120.;
        text white 2. "left/right walk   up/down aim   Enter jump" |> move_y 60.;
        text white 2. "1 bazooka   2 grenade   3 ninja rope" |> move_y 20.;
        text white 2. "space: hold to charge, let go to fire (the rope: fire, and let go)" |> move_y (-20.) ]
      @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y (-150.) ]
  | Playing g -> view_game g @ view_hud g
  | Winner i ->
      [ text white 5. (Printf.sprintf "PLAYER %d WINS" (i +.. 1)) |> move_y 100. ]
      @ Scene2d.blink 1. model [ text yellow 3. "PRESS SPACE" |> move_y (-100.) ])

let app = game view update initial_model

let main =
  (* seed=n (see Playground.flags): the same island and winds every run,
   * e.g. for golden frames *)
  (match List.assoc_opt "seed" (Playground_platform.flags ()) with
  | Some n -> Random.init (int_of_string n)
  | None -> Random.self_init ());
  Playground_platform.run_app app
