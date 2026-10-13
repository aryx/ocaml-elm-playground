(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Metal Slug (Nazca, SNK, Neo Geo, 1996): one soldier
 * against an army, on foot and then in a small tank that jumps, the
 * SV-001 "Metal Slug". Left/right to run, up to aim up, down to crouch
 * (in the air: to aim down), x to jump, space to fire (held: it keeps
 * firing; next to a soldier it is the knife), z to throw a grenade.
 * Shoot the prisoners' ropes: they give the heavy machine gun, or
 * grenades. Jump onto the tank to drive it: the arrows then turn its
 * gun, z fires its cannon, and it takes three hits for you. A
 * helicopter waits at the end.
 *
 * Nazca were the people of Irem's In the Hunt and GunForce II, and
 * Metal Slug is the end of a line that began with Contra (Konami,
 * 1987): the run and gun, a shoot 'em up on foot. What made it famous
 * is the drawing, every frame by hand -- soldiers who chat, scream and
 * run, a tank that squashes and stretches -- on a console, the Neo
 * Geo, that could only do sprites. (Names and dates from memory, to
 * check.)
 *
 * The new ideas here are three:
 *
 *   - a body in parts. The original's heroes are two sprites, the legs
 *     and the torso, animated apart: the legs follow the ground (run,
 *     jump, crouch), the torso follows the gun. Here three parts: the
 *     legs, the torso, and the gun, a rectangle turned by [aim]:
 *
 *           gun: aim         the aim is an angle, and *takes time*:
 *          /                 [turn_toward] moves it a few degrees a
 *      [torso]               frame to where the arrows ask. So the
 *      [legs ]: steps        heavy machine gun, held while the aim
 *                            goes from ahead to up, sprays the arc
 *                            between -- the original's best-known
 *                            accident, and here nobody wrote it;
 *
 *   - a vehicle is the same hero with other numbers ([riding]): the
 *     same [step_hero], a wider and slower body, a gun that turns three
 *     times slower but all the way round (the arrows are its eight
 *     directions), and an armour that takes the hits. When it is gone
 *     the tank blows up and throws you out, on foot again;
 *
 *   - a level is a timeline, as in a shoot 'em up ([waves]): the
 *     camera only goes forward, and what it reaches comes in from the
 *     edges of the screen. A wave can lock it (the ambushes, from both
 *     sides; the helicopter): no going on until the screen is clear.
 *
 * And one old one: the soldiers and the prisoners are the hero's
 * sprite with another palette ([army], [rags]), the palette swap that
 * gave arcade games their second player and half their enemies.
 *
 * The ground is not tiles but a profile of heights ([terrain],
 * [ground]): a body walks up a step lower than [step_up], and must
 * jump the others. One hit kills, as in the original; three lives.
 *
 * What it uses: the shoot 'em up kit's Shots (gamekits/shmup/: every
 * bullet, grenade and bomb, the arcs being shots with a gravity of
 * their own), Camera2d ([look_at], [parallax] for the far ruins),
 * Sprite (the pixel art, [flip]), Scene2d, Audio's ready-made sounds.
 * Not the platformer kit (Tile_move): no tiles. Not Physics: nothing
 * bounces. Not Juice: the explosions are circles, drawn here.
 *
 * Exercises: getting out of the tank (down and jump, in the original)
 * and its suicide attack; the other weapons (the shotgun, the rocket
 * launcher, the flame shot) as other numbers for [fire]; soldiers who
 * panic at a grenade; a second player; the score for prisoners kept
 * alive to the end of the mission; Juice for the explosions (a shake,
 * debris); the helicopter's rotor wash; a second mission.
 *)
open Playground

let rad (deg : number) : number = deg *. Float.pi /. 180.
let deg (rad : number) : number = rad *. 180. /. Float.pi

(*****************************************************************************)
(* The ground *)
(*****************************************************************************)

(* from which x, how high: steps of a hill, a wall to jump, a trench *)
let terrain : (number * number) list =
  [ (0., 0.); (900., 12.); (930., 24.); (960., 36.); (1200., 0.); (1600., 60.); (1850., 0.); (2700., -40.); (2950., 0.);
    (3300., 12.); (3330., 24.); (3650., 0.) ]

let base = 0. (* the height 0 of [terrain], in the world *)

(* the camera shows the world 1.6 times its size, the sprites' pixels
 * 6 of the screen's: half the screen is [half_screen] of the world *)
let zoom = 1.6
let half_screen = 500. /. zoom
let level_end = 4300.
let step_up = 14. (* a step no higher is walked up *)

(* [ground x]: the height of the ground at x. E.g. ground 940 is
 * base + 24, the second step of the first hill. *)
let ground (x : number) : number =
  base +. List.fold_left (fun h (from, height) -> if x >= from then height else h) 0. terrain

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type hero = {
  x : number;
  y : number; (* his feet *)
  vy : number;
  facing : number; (* 1 right, -1 left *)
  aim : number; (* the gun's angle, degrees: 0 right, 90 up *)
  steps : int; (* frames spent running, for the legs *)
  running : bool;
  crouch : bool;
  cooldown : int;
  heavy : int; (* rounds left in the heavy machine gun; 0: the pistol *)
  bombs : int; (* grenades, and the tank's shells *)
  safe : int; (* frames left of being untouchable *)
  dead : int; (* frames since he died; 0 alive *)
}

(* the tank: waiting, driven with so much armour left, or blown up *)
type slug = Parked | Riding of int | Wrecked

let slug_x = 2050.

(* a bullet, or with [arc] a grenade, a shell, a bomb: it falls, and
 * blows up where it lands *)
type shot = { at : Shots.t; power : int; arc : bool }

type kind = Rifle | Grenadier | Chopper
type enemy = { kind : kind; ex : number; ey : number; hp : int; clock : int }

(* a prisoner: tied (0), or freed for so many frames; what he gives *)
type pow = { px : number; gift : char; freed : int }

type game = {
  hero : hero;
  slug : slug;
  enemies : enemy list;
  mine : shot list;
  theirs : shot list;
  pows : pow list;
  gifts : (number * char) list; (* on the ground: 'H' the heavy machine gun, 'B' grenades *)
  booms : (number * number * int) list; (* where, and for how many frames *)
  wave : int; (* the next of [waves] *)
  lock : number option; (* the camera held there until the screen is clear *)
  cam : number; (* the middle of the screen, in the world: it never goes back *)
  frames : int;
  score : int;
  lives : int;
}

type scene = Title | Playing of game | Complete of int | Game_over of int
type model = scene Scene2d.t

(* The level's timeline: when the camera reaches [at], these come in
 * from the right edge -- and, in a wave that locks, every third from
 * the left. *)
let waves : (number * bool * kind list) list =
  [ (350., false, [ Rifle; Rifle ]);
    (750., false, [ Rifle; Grenadier ]);
    (1250., true, [ Rifle; Rifle; Rifle; Grenadier ]);
    (1750., false, [ Rifle; Grenadier; Rifle ]);
    (2300., false, [ Rifle; Rifle; Grenadier; Rifle; Rifle ]);
    (2800., true, [ Rifle; Grenadier; Rifle; Rifle; Grenadier; Rifle ]);
    (3400., false, [ Grenadier; Rifle; Grenadier ]);
    (3900., true, [ Chopper; Rifle; Rifle ]) ]

let new_hero (x : number) : hero =
  { x; y = ground x; vy = 0.; facing = 1.; aim = 0.; steps = 0; running = false; crouch = false; cooldown = 0; heavy = 0; bombs = 10; safe = 120; dead = 0 }

let new_game () : game =
  { hero = { (new_hero 100.) with safe = 0 }; slug = Parked; enemies = []; mine = []; theirs = [];
    pows = [ { px = 550.; gift = 'H'; freed = 0 }; { px = 1750.; gift = 'B'; freed = 0 }; { px = 3100.; gift = 'H'; freed = 0 } ];
    gifts = []; booms = []; wave = 0; lock = None; cam = 0.; frames = 0; score = 0; lives = 3 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The hero, on foot or in the tank *)
(*****************************************************************************)

let gravity = 0.7
let riding (g : game) : bool = match g.slug with Riding _ -> true | _ -> false

(* half his width, and his height: what a bullet must cross *)
let half (riding : bool) : number = if riding then 40. else 12.
let height (riding : bool) (h : hero) : number = if riding then 64. else if h.crouch then 30. else 56.

(* [turn_toward rate wanted a]: the angle [a] turned towards [wanted],
 * the short way round, by at most [rate] degrees. E.g. turn_toward 15
 * 90 0 = 15, and turn_toward 15 0 340 = 355: on through 360, not back
 * through 180. *)
let turn_toward (rate : number) (wanted : number) (a : number) : number =
  let d = Float.rem (wanted -. a) 360. in
  let d = if d > 180. then d -. 360. else if d < -180. then d +. 360. else d in
  if Float.abs d <= rate then wanted else a +. Float.copy_sign rate d

(* [step_hero ~riding ~left ~right keys jump h]: one frame of running,
 * jumping, crouching and aiming, between the screen's [left] and
 * [right] edges. On foot the gun points where he faces, up with the
 * up arrow, down with the down arrow in the air; turning round mirrors
 * it at once. The tank's turns to the arrows' direction, and stays. *)
let step_hero ~(riding : bool) ~(left : number) ~(right : number) (keys : keyboard) (jump : bool) (h : hero) : hero =
  let on = h.y <= ground h.x in
  let crouch = (not riding) && on && keys.kdown in
  let dir = if crouch then 0. else to_x keys in
  let facing = if dir <> 0. then dir else h.facing in
  let aim = if (not riding) && facing <> h.facing then 180. -. h.aim else h.aim in
  let wanted =
    if riding then if to_x keys = 0. && to_y keys = 0. then aim else deg (atan2 (to_y keys) (to_x keys))
    else if keys.kup then 90.
    else if keys.kdown && not on then -90.
    else if facing > 0. then 0.
    else 180.
  in
  let aim = turn_toward (if riding then 5. else 15.) wanted aim in
  let x' = h.x +. (dir *. if riding then 3. else 4.5) in
  let blocked = ground (x' +. (dir *. half riding)) > h.y +. step_up || x' < left || x' > right in
  let x = if blocked then h.x else x' in
  let vy = if jump && on && not crouch then if riding then 11. else 13. else h.vy -. gravity in
  let y = h.y +. vy in
  let y, vy = if y <= ground x then (ground x, 0.) else (y, vy) in
  { h with x; y; vy; facing; aim; crouch; running = x <> h.x; steps = (if x <> h.x then h.steps + 1 else h.steps) }

(* where the gun's mouth is *)
let muzzle (riding : bool) (h : hero) : number * number =
  let r = if riding then 46. else 28. and up = if riding then 46. else if h.crouch then 18. else 36. in
  (h.x +. (r *. cos (rad h.aim)), h.y +. up +. (r *. sin (rad h.aim)))

(* a bullet along the gun, [spread] degrees off *)
let bullet (riding : bool) (h : hero) (spread : number) : shot =
  let x, y = muzzle riding h and a = rad (h.aim +. spread) in
  { at = Shots.straight x y (14. *. cos a) (14. *. sin a); power = 1; arc = false }

(* a grenade thrown ahead, or the tank's shell along its gun *)
let bomb (riding : bool) (h : hero) : shot =
  let x, y = muzzle riding h in
  let vx, vy = if riding then (12. *. cos (rad h.aim), (12. *. sin (rad h.aim)) +. 3.) else (h.facing *. 6., 9.) in
  { at = Shots.straight x y vx vy; power = 10; arc = true }

(*****************************************************************************)
(* The enemies *)
(*****************************************************************************)

let soldier (e : enemy) : bool = e.kind <> Chopper

(* his middle: a soldier's chest, the helicopter's cabin *)
let target (e : enemy) : number * number = if soldier e then (e.ex, e.ey +. 28.) else (e.ex, e.ey)

(* [struck (x, y) e]: a shot at (x, y) is in him: a soldier's box, from
 * his feet to his head; 60 round the helicopter *)
let struck ((x, y) : number * number) (e : enemy) : bool =
  if soldier e then Float.abs (x -. e.ex) < 16. && y > e.ey && y < e.ey +. 58. else Float.hypot (x -. e.ex) (y -. e.ey) < 60.

let period (k : kind) : int = match k with Rifle -> 120 | Grenadier -> 150 | Chopper -> 50

(* about to fire: the rifle levelled, the arm back *)
let ready (e : enemy) : bool = e.clock mod period e.kind > period e.kind - 30

(* A rifleman comes to 230 from the hero and fires level, at chest
 * height: a crouch ducks it, a jump clears it. A grenadier stays
 * further and lobs, to land where the hero stands: keep moving. The
 * helicopter swings over the screen, from edge to edge (no corner is
 * safe), and drops bombs. *)
let step_enemy (cam : number) (h : hero) (e : enemy) : enemy * shot list =
  let e = { e with clock = e.clock + 1 } in
  let dx = h.x -. e.ex in
  let dir = if dx >= 0. then 1. else -1. in
  let seen = Float.abs (e.ex -. cam) < half_screen in
  let fires = seen && e.clock mod period e.kind = 0 in
  (* walking: onto the screen first, then up to [keep] from the hero *)
  let walk keep =
    let ex =
      if Float.abs (e.ex -. cam) > half_screen -. 30. then e.ex +. Float.copy_sign 2. (cam -. e.ex)
      else if Float.abs dx > keep then e.ex +. (dir *. 2.)
      else e.ex
    in
    { e with ex; ey = ground ex }
  in
  match e.kind with
  | Rifle -> (walk 230., if fires then [ { at = Shots.straight (e.ex +. (dir *. 20.)) (e.ey +. 34.) (dir *. 4.5) 0.; power = 1; arc = false } ] else [])
  | Grenadier ->
      (* to land at the hero in 60 frames: vx = dx / 60, and up at
       * half of what its gravity takes back in that time *)
      (walk 280., if fires then [ { at = Shots.straight e.ex (e.ey +. 50.) (dx /. 60.) 12.; power = 1; arc = true } ] else [])
  | Chopper ->
      let t = float_of_int e.clock in
      let e = { e with ex = cam +. ((half_screen -. 40.) *. sin (t /. 60.)); ey = base +. 400. +. (20. *. sin (t /. 25.)) } in
      (e, if fires then [ { at = Shots.straight e.ex (e.ey -. 40.) 0. (-2.); power = 1; arc = true } ] else [])

let points (k : kind) : int = match k with Rifle -> 100 | Grenadier -> 200 | Chopper -> 5000

(* the wave [who] coming in round a screen whose middle is [cam] *)
let spawn (cam : number) (lock : bool) (who : kind list) : enemy list =
  List.mapi
    (fun i kind ->
      let out = half_screen +. 40. +. (60. *. float_of_int i) in
      let ex = if lock && i mod 3 = 2 then cam -. out else cam +. out in
      { kind; ex; ey = (if kind = Chopper then base +. 400. else ground ex); hp = (if kind = Chopper then 60 else 1); clock = 37 * i })
    who

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let arc_gravity = 0.4
let blast = 85. (* a grenade's reach *)

let fly (s : shot) : shot =
  let at = Shots.advance s.at in
  { s with at = (if s.arc then { at with vy = at.vy -. arc_gravity } else at) }

let landed (s : shot) : bool = s.at.y <= ground s.at.x
let dist (x, y) (x', y') : number = Float.hypot (x -. x') (y -. y')

(* [fire space g]: what the fire button does this frame: the knife, if
 * a soldier is within reach ahead, or a shot, when the gun is ready --
 * the pistol every 12 frames, the heavy machine gun and the tank's
 * every 4, each round a little off the last one's line *)
let fire (g : game) : game =
  let h = g.hero and riding = riding g in
  let reach (e : enemy) = soldier e && (e.ex -. h.x) *. h.facing > 0. && Float.abs (e.ex -. h.x) < 46. && Float.abs (e.ey -. h.y) < 40. in
  if h.cooldown > 0 then g
  else if (not riding) && List.exists reach g.enemies then (
    Audio.play Audio.hit;
    { g with hero = { h with cooldown = 15 }; enemies = List.map (fun e -> if reach e then { e with hp = 0 } else e) g.enemies })
  else (
    let fast = riding || h.heavy > 0 in
    let spread = if not fast then 0. else if g.frames mod 8 < 4 then 2.5 else -2.5 in
    if spread >= 0. then Audio.play Audio.blip;
    { g with mine = bullet riding h spread :: g.mine; hero = { h with cooldown = (if fast then 4 else 12); heavy = (if riding then h.heavy else max 0 (h.heavy - 1)) } })

(* the hero's shots against the enemies and the prisoners' ropes: a
 * bullet is spent on the first it meets; a grenade blows up on what it
 * meets or where it lands, and hurts all within its [blast] *)
let my_shots (g : game) : game =
  let flying = List.map fly g.mine in
  let meets (s : shot) (e : enemy) = struck (s.at.x, s.at.y) e in
  let rope (s : shot) (p : pow) = p.freed = 0 && (not s.arc) && dist (s.at.x, s.at.y) (p.px, ground p.px +. 28.) < 28. in
  let bursts = List.filter (fun s -> s.arc && (landed s || List.exists (meets s) g.enemies)) flying in
  let hurt (e : enemy) =
    let bullets = List.length (List.filter (fun s -> (not s.arc) && meets s e) flying) in
    let blasts = List.length (List.filter (fun (s : shot) -> dist (s.at.x, s.at.y) (target e) < blast) bursts) in
    { e with hp = e.hp - bullets - (10 * blasts) }
  in
  let spent (s : shot) =
    landed s || Float.abs (s.at.x -. g.cam) > half_screen +. 20. || s.at.y > 520. || List.exists (meets s) g.enemies || List.exists (rope s) g.pows
  in
  let cut, pows = List.partition (fun p -> List.exists (fun s -> rope s p) flying) g.pows in
  if bursts <> [] then Audio.play Audio.explosion;
  if cut <> [] then Audio.play Audio.coin;
  { g with mine = List.filter (fun s -> not (spent s)) flying; enemies = List.map hurt g.enemies;
    pows = List.map (fun p -> { p with freed = 1 }) cut @ pows; gifts = List.map (fun p -> (p.px +. 40., p.gift)) cut @ g.gifts;
    score = g.score + (500 * List.length cut); booms = List.map (fun (s : shot) -> (s.at.x, s.at.y, 0)) bursts @ g.booms }

(* [hit g]: the hero touched by a bullet or a blast. In the tank its
 * armour takes it, and the last of it throws him out; on foot he dies. *)
let hit (g : game) : game =
  let h = g.hero in
  if h.safe > 0 || h.dead > 0 then g
  else
    match g.slug with
    | Riding n when n > 1 -> Audio.play Audio.hit; { g with slug = Riding (n - 1); hero = { h with safe = 40 } }
    | Riding _ ->
        Audio.play Audio.explosion;
        { g with slug = Wrecked; hero = { h with vy = 12.; safe = 120; aim = (if h.facing > 0. then 0. else 180.) }; booms = (h.x, h.y +. 30., -20) :: g.booms }
    | Parked | Wrecked -> Audio.play Audio.laser; { g with hero = { h with dead = 1 } }

(* the enemies' shots: a bullet through the hero's box, a bomb landing
 * within its blast of him *)
let their_shots (g : game) : game =
  let h = g.hero and riding = riding g in
  let flying = List.map fly g.theirs in
  let inside (s : shot) = Float.abs (s.at.x -. h.x) < half riding +. 5. && s.at.y > h.y && s.at.y < h.y +. height riding h in
  let bursts = List.filter (fun s -> s.arc && (landed s || inside s)) flying in
  let wounded = List.exists (fun s -> (not s.arc) && inside s) flying || List.exists (fun (s : shot) -> dist (s.at.x, s.at.y) (h.x, h.y +. 20.) < 70.) bursts in
  if bursts <> [] then Audio.play Audio.explosion;
  let g =
    { g with theirs = List.filter (fun s -> not (landed s || inside s || Float.abs (s.at.x -. g.cam) > half_screen +. 60.)) flying;
      booms = List.map (fun (s : shot) -> (s.at.x, s.at.y, 0)) bursts @ g.booms }
  in
  if wounded then hit g else g

(* what he walks into: a gift, a prisoner's rope (the knife cuts it),
 * the tank, when he comes down on it *)
let touch (g : game) : game =
  let h = g.hero in
  let near x = Float.abs (x -. h.x) < 30. && h.y < ground x +. 40. in
  let taken, gifts = List.partition (fun (x, _) -> near x) g.gifts in
  let h =
    List.fold_left
      (fun (h : hero) (_, gift) -> Audio.play Audio.powerup; if gift = 'H' then { h with heavy = 200 } else { h with bombs = h.bombs + 10 })
      h taken
  in
  let slug = if g.slug = Parked && Float.abs (slug_x -. h.x) < 40. && h.vy < 0. && h.y > ground slug_x +. 10. then (Audio.play Audio.powerup; Riding 3) else g.slug in
  let h = if slug <> g.slug then { h with x = slug_x; safe = 30 } else h in
  { g with hero = h; gifts; slug;
    pows = List.filter_map (fun p -> if p.freed > 200 then None else Some { p with freed = (if p.freed > 0 then p.freed + 1 else 0) }) g.pows }

(* the timeline: the next wave when the camera reaches it; a lock taken
 * off when the screen is clear *)
let timeline (g : game) : game =
  let g = if g.enemies = [] then { g with lock = None } else g in
  match List.nth_opt waves g.wave with
  | Some (at, lock, who) when g.cam >= at ->
      { g with wave = g.wave + 1; enemies = spawn g.cam lock who @ g.enemies; lock = (if lock then Some at else g.lock) }
  | _ -> g

let update_game (keys : keyboard) ~(jump : bool) ~(throw : bool) (g : game) : game =
  let g = { g with frames = g.frames + 1; booms = List.filter_map (fun (x, y, n) -> if n < 24 then Some (x, y, n + 1) else None) g.booms } in
  let h = g.hero and ride = riding g in
  (* dead: a second on the ground, then back, with the pistol *)
  if h.dead > 0 then
    let g = my_shots g in
    { g with theirs = List.filter (fun s -> not (landed s)) (List.map fly g.theirs);
      hero = (if h.dead >= 60 then { (new_hero h.x) with y = h.y } else { h with dead = h.dead + 1; y = Float.max (ground h.x) (h.y -. 6.) });
      lives = (if h.dead >= 60 then g.lives - 1 else g.lives) }
  else
    let h = step_hero ~riding:ride ~left:(g.cam -. half_screen +. 20.) ~right:(g.cam +. half_screen -. 20.) keys jump h in
    let h = { h with cooldown = max 0 (h.cooldown - 1); safe = max 0 (h.safe - 1) } in
    let g = { g with hero = h } in
    let g = if keys.kspace then fire g else g in
    let g = if throw && g.hero.bombs > 0 then { g with mine = bomb ride g.hero :: g.mine; hero = { g.hero with bombs = g.hero.bombs - 1 } } else g in
    (* the camera, ahead of him, never back, held by a lock *)
    let limit = match g.lock with Some at -> at | None -> level_end -. half_screen in
    let g = timeline { g with cam = Float.min limit (Float.max g.cam (h.x +. 80.)) } in
    (* the enemies: the tank runs the soldiers over *)
    let over (e : enemy) = ride && soldier e && Float.abs (e.ex -. h.x) < 50. && Float.abs (e.ey -. h.y) < 50. in
    let moved = List.map (fun e -> step_enemy g.cam h (if over e then { e with hp = 0 } else e)) g.enemies in
    let g = my_shots { g with enemies = List.map fst moved; theirs = List.concat_map snd moved @ g.theirs } in
    let dead, enemies = List.partition (fun e -> e.hp <= 0) g.enemies in
    if List.exists (fun e -> e.kind = Chopper) dead then Audio.play Audio.explosion;
    let g =
      { g with enemies; score = g.score + List.fold_left (fun s e -> s + points e.kind) 0 dead;
        booms = List.map (fun e -> let x, y = target e in (x, y, if soldier e then 8 else -30)) dead @ g.booms }
    in
    touch (their_shots g)

(* every wave come and gone *)
let won (g : game) : bool = g.wave >= List.length waves && g.enemies = []

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed key = Scene2d.pressed (fun k -> Set_.mem key k.keys) s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer.keyboard ~jump:(pressed "x") ~throw:(pressed "z") g in
      if won g then Scene2d.go (Complete g.score) s
      else if g.lives <= 0 then Scene2d.go (Game_over g.score) s
      else { s with scene = Playing g }
  | Complete _ | Game_over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size
let px = 4. (* a pixel of the sprites *)

(* the torso, facing right, without its arms: the gun is drawn apart *)
let torso = [ "..HHHH.."; ".RRRRRR."; "..SSSSK."; "..SSSS.."; ".VVVVVV."; ".VVVVVV."; ".VVVVVV."; ".VVVVVV." ]

(* the legs: standing, two frames of a run, in the air, crouched *)
let stand = [ ".PPPPPP."; ".PP..PP."; ".PP..PP."; ".PP..PP."; ".BB..BBB" ]
let run = [ [ ".PPPPPP."; ".PPP.PP."; "PP....PP"; "PP....PP"; "BBB...BB" ]; [ ".PPPPPP."; "..PPPP.."; "..PPPP.."; "..PPP..."; "..BBBB.." ] ]
let jumping = [ ".PPPPPP."; "PPP..PPP"; "BB....PP"; "......BB" ]
let crouched = [ "PPPPPPPP"; "BBB..BBB" ]

(* three palettes for one drawing: the hero, the army, the prisoners *)
let marco = [ ('H', rgb 230 200 90); ('R', rgb 210 40 40); ('S', rgb 240 190 150); ('K', black); ('V', rgb 70 90 160); ('P', rgb 200 180 130); ('B', rgb 70 50 40) ]
let army = [ ('H', rgb 70 100 60); ('R', rgb 70 100 60); ('S', rgb 240 190 150); ('K', black); ('V', rgb 90 125 75); ('P', rgb 90 125 75); ('B', rgb 40 40 40) ]
let rags = [ ('H', rgb 235 235 235); ('R', rgb 240 190 150); ('S', rgb 240 190 150); ('K', black); ('V', rgb 235 235 235); ('P', rgb 240 190 150); ('B', rgb 240 190 150) ]

(* a man of [palette] at (x, y), his feet there: the legs, the torso on
 * them, and a gun of [color] turned by [aim] from the shoulder *)
let man palette ~(facing : number) ~(legs : string list) ?(gun : (color * number) option) (x : number) (y : number) : shape =
  let side rows = if facing < 0. then Sprite.flip rows else rows in
  let legs_h = px *. float_of_int (List.length legs) in
  let shoulder = legs_h +. 14. in
  group
    ([ Sprite.pixels px palette (side legs) |> move_y (legs_h /. 2.); Sprite.pixels px palette (side torso) |> move_y (legs_h +. 16.) ]
    @ match gun with
      | None -> []
      | Some (color, aim) ->
          [ group [ rectangle (rgb 240 190 150) 16. 6. |> move_x 8.; rectangle color 20. 6. |> move_x 22. ] |> rotate aim |> move_y shoulder ])
  |> move x y

let legs_of (steps : int) (on : bool) (crouch : bool) (moving : bool) : string list =
  if crouch then crouched else if not on then jumping else if moving then Sprite.cycle (steps / 5) run else stand

(* the SV-001: tracks, a hull, a dome, and the gun turned by the aim *)
let view_slug (x : number) (y : number) (aim : number) (steps : int) : shape =
  let grey = rgb 150 155 165 and dark = rgb 70 72 80 in
  let wheel i = circle (rgb 110 112 120) 8. |> move ((float_of_int i *. 22.) -. 33.) 10. in
  let tread i = rectangle (rgb 40 40 44) 4. 4. |> move (Float.rem (float_of_int ((i * 14) + (steps * 3))) 84. -. 42.) 1. in
  group
    ([ rectangle dark 92. 20. |> move_y 10.; polygon grey [ (-40., 20.); (44., 20.); (36., 40.); (-30., 44.) ]; circle grey 18. |> move_y 44.;
       rectangle (rgb 200 60 50) 12. 6. |> move (-20.) 32.;
       group [ rectangle dark 40. 8. |> move_x 26. ] |> rotate aim |> move_y 46. ]
    @ List.init 4 wheel @ List.init 6 tread)
  |> move x y

let view_enemy (h : hero) (e : enemy) : shape =
  match e.kind with
  | Chopper ->
      let x, y = (e.ex, e.ey) in
      group
        [ rectangle (rgb 60 80 60) 110. 10. |> move (-70.) 4.; oval (rgb 80 105 75) 120. 60.; oval (rgb 150 200 220) 50. 30. |> move 26. 6.;
          rectangle (rgb 40 40 40) (if e.clock mod 4 < 2 then 200. else 60.) 5. |> move_y 38.; rectangle (rgb 60 80 60) 8. 24. |> move (-122.) 14. ]
      |> move x y
  | Rifle | Grenadier ->
      let facing = if h.x >= e.ex then 1. else -1. in
      let level = if facing > 0. then 0. else 180. in
      (* the rifle hangs until he is about to fire; a grenadier's arm goes back *)
      let aim = if e.kind = Rifle then if ready e then level else level -. (facing *. 60.) else if ready e then level +. (facing *. 120.) else level -. (facing *. 80.) in
      let color = if e.kind = Rifle then rgb 60 50 40 else rgb 40 90 40 in
      man army ~facing ~legs:(if ready e then stand else Sprite.cycle (e.clock / 6) run) ~gun:(color, aim) e.ex e.ey

let view_shot (mine : bool) (s : shot) : shape =
  (if s.arc then circle (if mine then rgb 60 90 50 else rgb 40 40 40) 7.
   else rectangle (if mine then rgb 255 240 140 else rgb 255 120 80) 16. 5. |> rotate (Shots.angle s.at))
  |> move s.at.x s.at.y

let view_boom ((x, y, n) : number * number * int) : shape =
  let n = float_of_int (max 0 n) in
  group [ circle orange (10. +. (2. *. n)) |> fade (1. -. (n /. 24.)); circle yellow (5. +. (1.2 *. n)) |> fade (1. -. (n /. 24.)) ] |> move x y

(* the ground: each stretch of [terrain] down to the bottom of the screen *)
let view_terrain : shape =
  let rec stretches = function
    | (from, h) :: (((next, _) :: _) as rest) -> (from, next, h) :: stretches rest
    | [ (from, h) ] -> [ (from, level_end +. 400., h) ]
    | [] -> []
  in
  group
    (List.concat_map
       (fun (from, next, h) ->
         let w = next -. from and top = base +. h in
         [ rectangle (rgb 120 90 60) w 300. |> move (from +. (w /. 2.)) (top -. 150.); rectangle (rgb 110 150 70) w 10. |> move (from +. (w /. 2.)) (top -. 5.) ])
       (stretches ((-400., 0.) :: List.tl terrain)))

(* far ruins, moving a third as fast as the ground: depth for free;
 * lowered by what the slower camera does not lift them *)
let ruins : shape =
  group
    (List.init 12 (fun i ->
         let w = 60. +. float_of_int (i * 37 mod 70) and h = 90. +. float_of_int (i * 53 mod 160) in
         rectangle (rgb 150 140 150) w h |> move (float_of_int i *. 190.) (base +. (h /. 2.) -. (0.7 *. 300. /. zoom))))

let view_game (screen : screen) (g : game) : shape list =
  let h = g.hero and ride = riding g in
  (* the ground's height 0 at 300 under the middle of the screen *)
  let cam = { (Camera2d.look_at g.cam (base +. (300. /. zoom)) Camera2d.origin) with zoom } in
  let on = h.y <= ground h.x in
  let blink = h.safe > 0 && h.safe / 3 mod 2 = 0 in
  let hero =
    if blink then []
    else if h.dead > 0 then [ man marco ~facing:h.facing ~legs:crouched h.x h.y |> fade (1. -. (float_of_int h.dead /. 60.)) ]
    else if ride then [ man marco ~facing:h.facing ~legs:crouched h.x (h.y +. 34.); view_slug h.x h.y h.aim h.steps ]
    else [ man marco ~facing:h.facing ~legs:(legs_of h.steps on h.crouch h.running) ~gun:((if h.heavy > 0 then rgb 50 50 60 else rgb 120 120 130), h.aim) h.x h.y ]
  in
  let view_pow (p : pow) =
    let y = ground p.px in
    if p.freed = 0 then group [ rectangle (rgb 110 80 50) 8. 70. |> move (p.px -. 14.) (y +. 35.); man rags ~facing:1. ~legs:stand p.px y ]
    else if p.freed < 40 then man rags ~facing:1. ~legs:stand ~gun:(rgb 240 190 150, 70.) p.px y
    else man rags ~facing:(-1.) ~legs:(Sprite.cycle (p.freed / 5) run) (p.px -. (5. *. float_of_int (p.freed - 40))) y
  in
  let view_gift (x, gift) = group [ square (if gift = 'H' then rgb 200 60 50 else rgb 60 110 60) 28.; text white 1.6 (String.make 1 gift) ] |> move x (ground x +. 16.) in
  let world =
    [ view_terrain ]
    @ (match g.slug with Parked -> [ view_slug slug_x (ground slug_x) 0. 0 ] | Wrecked | Riding _ -> [])
    @ List.map view_pow g.pows @ List.map view_gift g.gifts
    @ List.map (view_enemy h) g.enemies
    @ hero
    @ List.map (view_shot true) g.mine @ List.map (view_shot false) g.theirs
    @ List.map view_boom g.booms
  in
  let arms = if ride then "ARMS oo" else if h.heavy > 0 then Printf.sprintf "ARMS H %d" h.heavy else "ARMS oo" in
  [ rectangle (rgb 150 200 235) screen.width screen.height; circle (rgb 255 240 200) 60. |> move 300. 300.;
    Camera2d.view (Camera2d.parallax 0.3 cam) [ ruins ]; Camera2d.view cam world;
    text white 2.2 (Printf.sprintf "1UP %d   %s   BOMB %d   %d" g.lives arms h.bombs g.score) |> move_y (screen.top -. 30.) ]
  @ (match g.slug with
    | Riding n -> [ text white 2. "SLUG" |> move (screen.left +. 60.) (screen.top -. 70.); rectangle (rgb 80 220 100) (40. *. float_of_int n) 12. |> move (screen.left +. 110. +. (20. *. float_of_int n)) (screen.top -. 70.) ]
    | _ -> [])
  @ if g.frames > 0 && g.enemies = [] && g.frames / 30 mod 2 = 0 then [ text yellow 3. "GO! >" |> move (screen.right -. 100.) 250. ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  let press y = Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y y ] in
  let ended color title score = [ rectangle black screen.width screen.height; text color 6. title; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ press (-160.) in
  match s.scene with
  | Title ->
      (* the hero beside the tank he will find *)
      let g = new_game () in
      view_game screen { g with hero = { (new_hero (slug_x -. 110.)) with safe = 0 }; cam = slug_x -. 40. }
      @ [ rectangle black 880. 290. |> fade 0.8 |> move_y 170.; text (rgb 255 200 60) 7. "TINY METAL SLUG" |> move_y 260.;
          text white 2.2 "left/right run   up aim up   down crouch   x jump" |> move_y 195.;
          text white 2.2 "space fire (close: the knife)   z grenade" |> move_y 160.;
          text white 2.2 "free the prisoners, jump onto the tank" |> move_y 125. ]
      @ press 65.
  | Playing g -> view_game screen g
  | Complete score -> ended (rgb 255 200 60) "MISSION COMPLETE" score
  | Game_over score -> ended red "GAME OVER" score

let app = game view update initial_model

let main = Program.main __MODULE__ (fun () -> Playground_platform.run_app app)
