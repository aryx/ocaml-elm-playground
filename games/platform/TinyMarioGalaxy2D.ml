(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Mario Galaxy (Nintendo EAD Tokyo, 2007), in
 * 2D: Mario runs round planetoids too small to have a horizon, stands
 * under them upside down, and jumps from one into the pull of the next.
 *
 *   left right   walk (round and round)
 *   space        jump
 *
 * The flags: camera=fixed, the world not turned to keep Mario's up up;
 * latch=off, the arrows re-read every frame instead of kept while held
 * (see "The controls" below: try them together).
 *
 * The other games of the repository with gravity have one "down": a
 * direction for the whole level (TinyCameltry turns it, TinyVVVVVV
 * flips its sign), or Newton's pull towards a star (TinySpacewar,
 * TinyXpilot), which makes orbits. TinySonic runs round a loop, but
 * with its gravity still pointing down the screen: stop on the
 * ceiling and Sonic falls. Galaxy's down is somewhere else at every
 * place, and that one idea is the game:
 *
 *   - the gravity is not physics but level design. Galaxy doesn't add up
 *     the pulls of its planets; its levels are full of invisible
 *     volumes, each saying which way is down inside it -- towards a
 *     point, towards a segment, a fixed direction in a box, and more
 *     (GlobalPointGravity, GlobalSegmentGravity, GlobalParallelGravity,
 *     ... in its level files, as the modding community documents them;
 *     from memory, to check) -- each with a priority for where they
 *     overlap. The pull is the same everywhere, near or far, so
 *     nothing orbits: a jump lands where the designer wanted. Here,
 *     [zone], [Around] (towards a body's segment: a ball's centre, a
 *     capsule's spine) and [Parallel] (a fixed direction, in a box),
 *     and [gravity_at], the highest priority, then the nearest;
 *   - "up" is local: every frame the velocity is split along the
 *     ground and along up, of *this* place, and walking, braking and
 *     jumping are done in those two numbers ([step]); the code
 *     otherwise does not know where Mario is;
 *   - a small planet flings you off its curve if you run too fast: the
 *     pull is too weak to bend your path round it. Galaxy keeps Mario on
 *     the ground, and so does [snap_to_ground], which glues him back to the
 *     surface he stood on if it's a few pixels away (what TinySonic's
 *     sensors do on its loop, see Slope);
 *   - the camera turns so that Mario's up is the screen's (Camera2d's
 *     angle, turned smoothly after it); but then "right" on the screen
 *     is not always along the ground, and see the controls;
 *   - and where there is no zone there is no gravity: you drift, in a
 *     straight line, and if you drift too long you are lost in space.
 *     Falling off the flat platform's edge is a fall into the void.
 *
 * The controls: the arrow says a direction on the screen; Mario needs
 * one along the ground, clockwise or anticlockwise. Projecting one on
 * the other works on top of a planet, and fails on its side: with the
 * camera fixed, walk right from the top of the home planet, and on its
 * right side the ground points straight down, "right" is along neither
 * way, and Mario stops (latch=off camera=fixed shows it). Galaxy's
 * answer, and Devil May Cry's across its camera cuts: the direction
 * the arrow meant when it was pressed is kept while it is held
 * ([walk_direction]); hold right and Mario goes all the way round.
 *
 * What it uses: Camera2d (following Mario, and turned with his up:
 * [Camera2d.turn_toward]), Scene2d, Sprite (TinyMario's pixel art,
 * Mario_xpm, turned with his up; artwork=shapes draws him as a circle
 * and a cap). Not Tilemap nor gamekits/platformer's Tile_move: they are
 * made of axis-aligned tiles, and here the ground is round; the bodies
 * are capsules, the one shape whose collision is a distance, to a
 * segment. Not Physics: its gravity is one vector for the world.
 *
 * Exercises: Galaxy's other volumes (a disk, a cube's six faces, a
 * cone), and its inverted sphere, walked on from the inside; a launch
 * star, flying along a path to a far planet; the spin, which stuns;
 * more goombas, which walk round their planet; a planet with a
 * variable pull, weaker for longer jumps; and the 3D original, as
 * TinyMarioGalaxy.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* Vectors *)
(*****************************************************************************)

type vec = number * number

let add ((ax, ay) : vec) ((bx, by) : vec) : vec = (ax + bx, ay + by)
let sub ((ax, ay) : vec) ((bx, by) : vec) : vec = (ax - bx, ay - by)
let mul (k : number) ((x, y) : vec) : vec = (k * x, k * y)
let dot ((ax, ay) : vec) ((bx, by) : vec) : number = (ax * bx) + (ay * by)
let len (v : vec) : number = sqrt (dot v v)
let norm (v : vec) : vec = if len v < 1e-9 then (0., 1.) else mul (1. / len v) v

(* a quarter turn clockwise: from up, the ground's direction, "right"
 * when up is up *)
let clockwise ((x, y) : vec) : vec = (y, -.x)

(* the angle of a direction, in degrees, anticlockwise from the x axis *)
let degrees ((x, y) : vec) : number = radians_to_degrees (atan2 y x)

(* the point of the segment [a]-[b] closest to [p] *)
let closest (a : vec) (b : vec) (p : vec) : vec =
  let ab = sub b a in
  let l2 = dot ab ab in
  if l2 = 0. then a else add a (mul (clamp 0. 1. (dot (sub p a) ab / l2)) ab)

(*****************************************************************************)
(* The galaxy *)
(*****************************************************************************)

(* A body is the points within [radius] of the segment [a]-[b]: [a] =
 * [b] is a ball, a planetoid; a long segment a capsule; and a long one
 * with a small radius a platform. *)
type body = { a : vec; b : vec; radius : number; color : color }

type rect = Camera2d.rect

(* Where the gravity is, and where it pulls: [Around] a body, towards the
 * nearest point of its segment, up to [reach] from its surface (so
 * from anywhere round it, even under it); [Parallel], in a box,
 * towards a fixed [down]. *)
type zone =
  | Around of { body : body; reach : number; priority : int }
  | Parallel of { box : rect; down : vec; priority : int }

let ball (x, y) radius color : body = { a = (x, y); b = (x, y); radius; color }

(* where Mario starts, on top *)
let home = ball (0., 0.) 110. (rgb 110 190 90)

(* a capsule, star bits along its top *)
let pill = { a = (440., 170.); b = (620., 300.); radius = 55.; color = rgb 220 160 90 }

(* a flat platform under the home planet, its gravity straight down:
 * jump off the planet's underside and you fall onto it; fall off its
 * left edge and nothing pulls you back *)
let platform = { a = (-150., -470.); b = (420., -470.); radius = 14.; color = rgb 150 150 200 }

(* the tiny planet of the Power Star *)
let tiny = ball (620., -300.) 45. (rgb 230 120 160)

let bodies = [ home; pill; platform; tiny ]

let zones =
  [ Around { body = home; reach = 190.; priority = 1 };
    Around { body = pill; reach = 170.; priority = 1 };
    Parallel { box = { left = -165.; right = 435.; bottom = -460.; top = -210. }; down = (0., -1.); priority = 2 };
    Around { body = tiny; reach = 190.; priority = 1 } ]

(* the star bits: picked up, they count *)
let star_bits : vec list =
  List.init 5 (fun i ->
      let a = degrees_to_radians (60. + (float_of_int i * 15.)) in
      (150. * cos a, 150. * sin a))
  @ List.init 4 (fun i -> add (closest pill.a pill.b (add pill.a (mul (float_of_int i / 3.) (sub pill.b pill.a)))) (-40., 90.))
  @ List.init 5 (fun i -> (-100. + (float_of_int i * 110.), -420.))

let power_star : vec = (620., -230.)

(*****************************************************************************)
(* Gravity, zone by zone *)
(*****************************************************************************)

let inside (b : rect) ((x, y) : vec) : bool = x >= b.left && x <= b.right && y >= b.bottom && y <= b.top

(* [zone] at [p]: which way it pulls, and how far [p] is from its body
 * (0 in a box), or None if [p] is out of it *)
let pull (z : zone) (p : vec) : (vec * number) option =
  match z with
  | Around { body; reach; _ } ->
      let q = closest body.a body.b p in
      let d = len (sub p q) - body.radius in
      if d <= reach then Some (norm (sub q p), d) else None
  | Parallel { box; down; _ } -> if inside box p then Some (down, 0.) else None

let priority (z : zone) : int = match z with Around { priority; _ } | Parallel { priority; _ } -> priority

(* which way is down at [p]: the zone of highest priority there, and of
 * those the nearest; None in the void between them *)
let gravity_at (p : vec) : vec option =
  List.fold_left
    (fun best z ->
      match (pull z p, best) with
      | None, _ -> best
      | Some (down, d), None -> Some (down, d, priority z)
      | Some (down, d), Some (_, bd, bp) ->
          if priority z > bp || (priority z = bp && d < bd) then Some (down, d, priority z) else best)
    None zones
  |> Option.map (fun (down, _, _) -> down)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  pos : vec;
  vel : vec;
  up : vec; (* the last down turned round, kept while adrift *)
  standing : bool;
  walk : number; (* -1, 0 or 1: along the ground, anticlockwise or clockwise *)
  held : number; (* the arrow that chose [walk], -1, 0 or 1 *)
  cam : Camera2d.t;
  goomba : number; (* its angle round the home planet *)
  squashed : int; (* frames since stomped, 0 while it walks *)
  bits : vec list; (* the star bits still there *)
  collected : int;
  adrift : int; (* frames in no zone *)
  dying : int; (* frames of dying left *)
  deaths : int;
  frames : int;
}

type scene = Title | Galaxy of play | Star of play
type model = scene Scene2d.t

(* Mario's radius: he is a ball too, for the collisions *)
let size = 15.
let zoom = 0.8

let start : play =
  let pos = (0., home.radius + size) in
  { pos; vel = (0., 0.); up = (0., 1.); standing = true; walk = 0.; held = 0.;
    cam = { Camera2d.origin with x = fst pos; y = snd pos; zoom }; goomba = 200.; squashed = 0;
    bits = star_bits; collected = 0; adrift = 0; dying = 0; deaths = 0; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The controls *)
(*****************************************************************************)

type config = { turning : bool; latch : bool }

let config_of (flags : flags) : config =
  { turning = List.assoc_opt "camera" flags <> Some "fixed"; latch = List.assoc_opt "latch" flags <> Some "off" }

(* what the player does this frame: the game reads it off the keyboard,
 * the tests make it up *)
type input = { dx : number; jump : bool }

let nothing = { dx = 0.; jump = false }

(* Which way along the ground the arrow [dx] means: its direction on
 * the screen, turned by the camera into the world, projected on the
 * ground's [right]. Kept while the same arrow is held (with the
 * latch), or re-read every frame, and then nothing when the arrow is
 * across the ground. *)
let walk_direction (c : config) (i : input) (p : play) (right : vec) : number =
  let a = degrees_to_radians p.cam.angle in
  let along = dot (mul i.dx (cos a, sin a)) right in
  if i.dx = 0. then 0.
  else if c.latch && i.dx = p.held && p.walk <> 0. then p.walk
  else if Float.abs along > 0.2 then Float.copy_sign 1. along
  else if c.latch then i.dx (* pressed across the ground: any way, and then kept *)
  else 0.

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let gravity = 0.35
let jump_speed = 9.
let run_top = 4.5
let run_accel = 0.5
let run_brake = 0.6
let air_accel = 0.15
let dying_frames = 40
let lost_frames = 150
let snap = 8.

(* approach [target] from [v] by at most [rate] *)
let approach (rate : number) (target : number) (v : number) : number =
  if v < target then Float.min target (v + rate) else Float.max target (v - rate)

(* Mario pushed out of [body], and his velocity into it removed; and
 * whether he stands on it: pushed from below, where below is [up]'s
 * opposite. *)
let collide (up : vec) ((pos, vel, standing) : vec * vec * bool) (body : body) : vec * vec * bool =
  let q = closest body.a body.b pos in
  let d = len (sub pos q) in
  if d >= body.radius + size then (pos, vel, standing)
  else
    let n = norm (sub pos q) in
    let into = dot vel n in
    let vel = if into < 0. then sub vel (mul into n) else vel in
    (add q (mul (body.radius + size) n), vel, standing || dot n up > 0.5)

(* Glued back on the ground he stood on last frame, if it is a few
 * pixels away: without it, running round the tiny planet flings him
 * off, the pull too weak to bend his path (v^2 / r > gravity). *)
let snap_to_ground (pos, vel) : (vec * vec * bool) =
  match
    List.find_opt
      (fun body ->
        let d = len (sub pos (closest body.a body.b pos)) - body.radius - size in
        d >= 0. && d < snap)
      bodies
  with
  | None -> (pos, vel, false)
  | Some body ->
      let q = closest body.a body.b pos in
      let n = norm (sub pos q) in
      let out = dot vel n in
      (add q (mul (body.radius + size) n), (if out > 0. then sub vel (mul out n) else vel), true)

let goomba_speed = 0.5

(* the goomba's place and its up, walking round the home planet *)
let goomba_at (angle : number) : vec * vec =
  let a = degrees_to_radians angle in
  let up = (cos a, sin a) in
  (add home.a (mul (home.radius + 12.) up), up)

let respawn (p : play) : play =
  { start with bits = p.bits; collected = p.collected; deaths = p.deaths; frames = p.frames; cam = p.cam }

let step (c : config) (i : input) (p : play) : play =
  if p.dying > 0 then if p.dying = 1 then respawn p else { p with dying = p.dying -.. 1 }
  else
    let p = { p with frames = p.frames +.. 1 } in
    let down = gravity_at p.pos in
    let up = match down with Some d -> mul (-1.) d | None -> p.up in
    let right = clockwise up in
    (* the velocity in this place's terms: along the ground, and up *)
    let walk = walk_direction c i p right in
    let vt = dot p.vel right and vn = dot p.vel up in
    let rate = if not p.standing then air_accel else if walk = 0. then run_brake else run_accel in
    let vt = approach rate (walk * run_top) vt in
    let jumped = i.jump && p.standing in
    let vn = if jumped then jump_speed else vn in
    let vel = add (mul vt right) (mul vn up) in
    let vel = match down with Some d -> add vel (mul gravity d) | None -> vel in
    let pos, vel, standing = List.fold_left (collide up) (add p.pos vel, vel, false) bodies in
    let pos, vel, standing =
      if p.standing && (not jumped) && not standing then snap_to_ground (pos, vel) else (pos, vel, standing)
    in
    (* the camera: after Mario, and turned so that his up is up *)
    let cam = Camera2d.follow 0.2 (fst pos) (snd pos) p.cam in
    let cam = if c.turning then Camera2d.turn_toward 0.12 (degrees up - 90.) cam else cam in
    let p =
      { p with pos; vel; up; standing; walk; held = i.dx; cam;
               adrift = (if down = None then p.adrift +.. 1 else 0) }
    in
    (* the star bits within reach *)
    let near q = len (sub q p.pos) < size + 14. in
    let got, bits = List.partition near p.bits in
    let p = { p with bits; collected = p.collected +.. List.length got } in
    (* the goomba: stomped if Mario comes down on it, down being ITS
     * down; otherwise it hurts *)
    let g, gup = goomba_at p.goomba in
    let p = if p.squashed > 0 then { p with squashed = p.squashed +.. 1 } else { p with goomba = p.goomba - goomba_speed } in
    if p.squashed = 0 && len (sub g p.pos) < size + 14. then
      if dot p.vel gup < -1. && dot (sub p.pos g) gup > 0. then
        { p with squashed = 1; vel = add (sub p.vel (mul (dot p.vel gup) gup)) (mul 6. gup); standing = false }
      else { p with dying = dying_frames; deaths = p.deaths +.. 1 }
    else if p.adrift > lost_frames then { p with dying = dying_frames; deaths = p.deaths +.. 1 }
    else p

let got_star (p : play) : bool = p.dying = 0 && len (sub power_star p.pos) < size + 20.

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title | Star _ -> if pressed (fun k -> k.kspace) then Scene2d.go (Galaxy start) scenes else scenes
  | Galaxy p ->
      let i = { dx = to_x computer.keyboard; jump = pressed (fun k -> k.kspace || k.kup) } in
      let p = step (config_of computer.flags) i p in
      if got_star p then Scene2d.go (Star p) scenes else { scenes with scene = Galaxy p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let space = rgb 10 12 40
let dark = rgb 20 20 30
let gold = rgb 250 220 60

(* far stars, part of the world: they turn with it *)
let sky : shape list =
  List.init 160 (fun i ->
      let x = float_of_int ((i *.. 7919) mod 2600) - 900. and y = float_of_int ((i *.. 104729) mod 2000) - 1000. in
      square (rgb 150 150 200) (float_of_int ((i mod 3) +.. 1) * 1.5) |> move x y)

(* a capsule: two balls and the box between them *)
let capsule (color : color) (a : vec) (b : vec) (r : number) : shape =
  let mid = mul 0.5 (add a b) in
  group
    [ circle color r |> move (fst a) (snd a);
      circle color r |> move (fst b) (snd b);
      rectangle color (len (sub b a)) (2. * r) |> rotate (degrees (sub b a)) |> move (fst mid) (snd mid) ]

(* the zones, a shade lighter than space: invisible in Galaxy, shown
 * here (opaque, not faded: a capsule is three shapes, which would show
 * where they overlap) *)
let zone_shape (z : zone) : shape =
  match z with
  | Around { body; reach; _ } -> capsule (rgb 20 24 60) body.a body.b (body.radius + reach)
  | Parallel { box; _ } ->
      rectangle (rgb 26 30 66) (box.right - box.left) (box.top - box.bottom)
      |> move ((box.left + box.right) / 2.) ((box.bottom + box.top) / 2.)

let body_shape (b : body) : shape =
  group [ capsule (rgb 40 40 60) b.a b.b (b.radius + 3.); capsule b.color b.a b.b b.radius ]

let star_shape (color : color) (r : number) : shape =
  polygon color
    (List.init 10 (fun i ->
         let a = degrees_to_radians (90. + (float_of_int i * 36.)) in
         let r = if i mod 2 = 0 then r else r * 0.45 in
         (r * cos a, r * sin a)))

let goomba_shape (p : play) : shape =
  let g, up = goomba_at p.goomba in
  let brown = rgb 150 90 40 in
  (if p.squashed > 0 then group [ oval brown 30. 8. |> move_y (-8.) ]
   else
     group
       [ oval (rgb 60 40 20) 10. 6. |> move (-7.) (-11.);
         oval (rgb 60 40 20) 10. 6. |> move 7. (-11.);
         oval brown 28. 22.;
         circle white 3. |> move (-5.) 3.;
         circle white 3. |> move 5. 3. ])
  |> rotate (degrees up - 90.)
  |> move (fst g) (snd g)

let palette, stand = Sprite.of_xpm Mario_xpm.stand
let walk1 = snd (Sprite.of_xpm Mario_xpm.walk1)
let walk2 = snd (Sprite.of_xpm Mario_xpm.walk2)
let jump = snd (Sprite.of_xpm Mario_xpm.jump)

let mario (computer : computer) (p : play) : shape =
  let color = if p.dying > 0 then rgb 230 150 150 else red in
  let facing = if p.walk <> 0. then p.walk else 1. in
  (if not (Sprite.artwork ~default:true computer.flags) then
     group [ circle color size; rectangle (rgb 180 20 20) 22. 6. |> move (4. * facing) 11.; circle dark 2.5 |> move (6. * facing) 3. ]
   else
     let rows =
       if not p.standing then jump
       else if p.walk = 0. then stand
       else if p.frames /.. 6 mod 2 = 0 then walk1
       else walk2
     in
     Sprite.pixels 3.5 palette (if facing < 0. then Sprite.flip rows else rows))
  |> rotate (degrees p.up - 90.)
  |> move (fst p.pos) (snd p.pos)

let world (computer : computer) (p : play) : shape list =
  sky
  @ List.map zone_shape zones
  @ List.map body_shape bodies
  @ List.map (fun (x, y) -> star_shape (rgb 120 220 250) 9. |> rotate (float_of_int p.frames * 3.) |> move x y) p.bits
  @ [ star_shape gold 22. |> rotate (float_of_int p.frames) |> move (fst power_star) (snd power_star);
      goomba_shape p;
      mario computer p ]

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  [ rectangle space screen.width screen.height;
    Camera2d.view p.cam (world computer p);
    text white 1.8 (Printf.sprintf "star bits %d   deaths %d" p.collected p.deaths)
    |> move (screen.left + 150.) (screen.top - 20.);
    text (rgb 150 150 200) 1.6 "arrows walk    space jumps    the faint areas are where gravity pulls"
    |> move_y (screen.bottom + 20.) ]
  @ if p.adrift > 30 && p.dying = 0 then [ text white 3. "ADRIFT" |> move_y 150. ] else []

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let backdrop = [ rectangle space screen.width screen.height ] in
  match model.scene with
  | Title ->
      backdrop
      @ [ text gold 6. "MARIO GALAXY 2D" |> move_y 220.;
          text white 2. "every planet has its own down" |> move_y 120.;
          text white 2. "walk all the way round, stand underneath, jump to the next" |> move_y 80.;
          text white 2. "and don't drift off into the void" |> move_y 40.;
          (* one small planet, stood on from above and from below *)
          circle home.color 60. |> move_y (-110.);
          mario computer { start with pos = (0., -32.) };
          mario computer { start with pos = (0., -188.); up = (0., -1.) } ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-250.) ]
  | Galaxy p -> view_play computer p
  | Star p ->
      backdrop
      @ [ star_shape gold 60. |> rotate (float_of_int model.frames) |> move_y 180.;
          text gold 5. "YOU GOT A POWER STAR" |> move_y 60.;
          text white 2.5
            (Printf.sprintf "%d star bits, %d deaths, %.1f seconds" p.collected p.deaths (float_of_int p.frames / 60.))
          |> move_y (-20.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinyMarioGalaxy2D
  left right   walk, round and round
  space (up)   jump
  flags: camera=fixed (the world not turned), latch=off (the arrows re-read every frame)
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
