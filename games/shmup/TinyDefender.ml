(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Defender (Eugene Jarvis and Larry DeMar, Williams
 * Electronics, 1981): a planet six screens wide, ten humanoids on its
 * mountains, and landers coming down to carry them off. Shoot the
 * lander that is lifting one, catch the falling human, and put him
 * back on the ground. Let all ten go and the planet blows up.
 *
 *   left/right   thrust, and the way you are pointing (press the
 *                other way to flip: the view flips with you)
 *   up/down      climb and dive
 *   space        the laser
 *   b            a smart bomb: everything on screen, gone (3 of them)
 *
 * Defender was the hardest game of the arcade and the highest-grossing
 * of its year, and the two facts are the same fact. What it invented
 * is not the shooting but the *place*: a world that does not fit on
 * the screen and goes on when you are not looking, with people living
 * in it who can be taken while your back is turned. Jarvis went on to
 * Robotron: 2084 (TinyRobotron.ml).
 *
 * Three ideas in it are new to the games of this directory:
 *
 *  - The world is a cylinder, and the only place that knows is [near],
 *    which slides a thing to whichever of its copies is closest to the
 *    camera. Everything else -- drawing, aiming, the distance between
 *    two things -- goes through it and never thinks about the seam
 *    again. Asteroids' wrap (Asteroid.ml) is the same idea on a
 *    world the size of the screen, where it is invisible; here the
 *    world is six screens, so the seam is real and you can chase a
 *    lander all the way round to where you started.
 *
 *  - The scanner, and it is why this game is worth writing. The whole
 *    planet is squashed into the strip at the top ([scanner]), which
 *    is nothing but a second view of the same model at another scale:
 *    a minimap is a camera, not a picture. Defender invented it, and
 *    Camera2d.mli says so in its history. Playing it is
 *    reading the scanner: the game is at the top of the screen, and
 *    the picture below is where you are only now arriving.
 *
 *  - The mountains are a function, not data ([ground]): three sines
 *    whose wavelengths divide the world exactly, so the range meets
 *    itself where the planet comes round. A height map (as in
 *    TinyWorms.ml) would be 6000 numbers and a seam to get
 *    right.
 *
 * And one rule, which is the game: the humans are the state of the
 * world, not the score. A lander that reaches the top with one
 * *becomes* a mutant -- faster, and interested only in you -- so every
 * abduction you miss is an enemy you will have to fight. Lose all ten
 * and the planet goes ([planet]): the ground is gone, the sky is empty
 * space, and every lander left turns mutant at once. You keep playing,
 * and you have lost the thing you were playing for.
 *
 * What it uses: the shoot 'em up kit's [Shots] (its fourth game, after
 * TinyInvaders, TinyGalaga and TinyRobotron: the laser and the
 * landers' bullets), Camera2d (the scroll that leads the ship, and the
 * scanner), Scene2d, and Physics for the ship alone -- Defender's ship
 * is remembered for its inertia, which is [push], [slow] and [step]
 * and nothing else. Not Physics for the rest: a lander is five
 * numbers, and the arcade's enemies move at their speed or not at all.
 * Not Tilemap: the ground is [ground]. Not Random: the waves are laid
 * out from the wave number, so a run replays exactly. Juice, with the
 * flag juice=engine: the smart bomb's flash is written by hand here (a
 * counter, a white rectangle: the default), and juice=engine does it
 * with Juice.flash instead, and knocks the screen too; juice=off, no
 * flash (see the juice section).
 *
 * Left as exercises: the rest of the zoo (baiters when you dawdle,
 * bombers and their mines, pods that burst into swarmers);
 * hyperspace (the panic button that teleports you, and sometimes
 * kills you); the mountains being fatal to fly into, as they are in
 * the arcade; the humans you carry being worth more the higher you
 * catch them; and a second player, taking turns as the arcade did.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The planet *)
(*****************************************************************************)

let world_w = 6000. (* six screens round *)
let sky_color = rgb 8 8 22
let rock = rgb 140 95 55
let human_color = rgb 90 200 230
let lander_color = rgb 80 220 120
let mutant_color = rgb 235 70 60
let ship_color = rgb 240 240 250

(* The mountains: three sines whose wavelengths divide the world
 * exactly, so the range meets itself where the planet comes round. *)
let ground (x : number) : number =
  let turn (k : number) = 2. * Float.pi * k * x / world_w in
  -300. + (70. * sin (turn 3.)) + (40. * sin (turn 7. + 1.)) + (22. * sin (turn 13. + 2.))

(* [wrap x]: where [x] really is, in [0, world_w) *)
let wrap (x : number) : number = Float.rem (Float.rem x world_w + world_w) world_w

(* [near around x]: the copy of [x] closest to [around] -- the only
 * place in the game that knows the world is a cylinder. Everything
 * else (drawing, aiming, the distance between two things) goes
 * through it. *)
let near (around : number) (x : number) : number =
  around + Float.rem (x - around + (1.5 * world_w)) world_w - (world_w / 2.)

(* how far apart two things are, the short way round *)
let apart (x1, y1) (x2, y2) : number = Float.hypot (near x1 x2 - x1) (y2 - y1)

(*****************************************************************************)
(* What lives on it *)
(*****************************************************************************)

(* A human is never removed from the list: his place in it is his name,
 * which is how a lander says which one it is carrying. *)
type state = Standing | Grabbed | Falling | Held | Dead
type human = { hx : number; hy : number; hvy : number; hstate : state }

let alive (h : human) : bool = h.hstate <> Dead

let humans_start : human list =
  List.init 10 (fun i ->
      let x = (float_of_int i + 0.5) * world_w / 10. in
      { hx = x; hy = ground x + 14.; hvy = 0.; hstate = Standing })

type kind = Lander | Mutant

type enemy = {
  ex : number;
  ey : number;
  kind : kind;
  (* the human it is carrying off, by his place in the list *)
  holds : int option;
  cool : int; (* frames before it may fire again *)
}

(* the ship, the one thing in the game with a body *)
type ship = { b : Physics.body; facing : number }

let new_ship (x : number) : ship =
  { b = Physics.body (circle ship_color 12.) |> Physics.at x 150.; facing = 1. }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  ship : ship;
  (* where the camera is looking, which leads the ship *)
  cam : number;
  humans : human list;
  enemies : enemy list;
  lasers : Shots.t list;
  bullets : Shots.t list; (* the landers' *)
  carried : int option; (* the human the ship is bringing down *)
  wave : int;
  score : int;
  lives : int;
  bombs : int;
  (* frames before the ship comes back, 0 while it flies *)
  dead : int;
  (* false once every human is gone: no ground, and every lander turns *)
  planet : bool;
  (* frames of the smart bomb's flash *)
  flash : int;
  juice : Juice.t; (* the effects of juice=engine *)
}

type scene = Title | Playing of play | Over of int (* score *)
type model = scene Scene2d.t

let thrust = 1100. (* the push, per second per second *)
let climb = 900.
let drag = 1.7 (* with the push, a top speed of 1100/1.7 *)
let lead = 260. (* how far ahead of the ship the camera looks *)
let ceiling = 270. (* the playfield stops under the scanner *)
let laser_speed = 26. (* pixels per frame, like every Shots shot *)
let lander_points = 150
let rescue_points = 500

let lander_speed (wave : int) : number = 1. + (0.15 * float_of_int wave)

(* a wave is laid out from its number: no Random, so a run replays *)
let spawn_wave (wave : int) (cam : number) : enemy list =
  let n = 4 +.. wave in
  List.init n (fun i ->
      let x = wrap (cam + 700. + (float_of_int i * world_w / float_of_int n)) in
      { ex = x; ey = 120. + (float_of_int (i mod 3) * 60.); kind = Lander; holds = None; cool = 60 +.. (i *.. 17) })

let start () : play =
  {
    ship = new_ship 0.;
    cam = 0.;
    humans = humans_start;
    enemies = spawn_wave 1 0.;
    lasers = [];
    bullets = [];
    carried = None;
    wave = 1;
    score = 0;
    lives = 3;
    bombs = 3;
    dead = 0;
    planet = true;
    flash = 0;
    juice = Juice.none ~seed:1;
  }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The ship *)
(*****************************************************************************)

(* Thrust, drag, and one step: Defender's inertia is this and nothing
 * else. Pressing the other way flips it, and the camera goes with the
 * flip, which is the game's other famous feel. *)
let drive (k : keyboard) (s : ship) : ship =
  let dx = to_x k and dy = to_y k in
  let b : Physics.body = s.b |> Physics.push (dx * thrust) (dy * climb) |> Physics.slow drag |> Physics.step in
  let low = ground b.x + 18. and high = ceiling in
  let y, vy =
    if b.y < low then (low, Float.max 0. b.vy) else if b.y > high then (high, Float.min 0. b.vy) else (b.y, b.vy)
  in
  { b = { b with x = wrap b.x; y; vy }; facing = (if dx = 0. then s.facing else dx) }

(* the camera looks where the ship is going, and takes the short way
 * round the planet to get there *)
let follow (s : ship) (cam : number) : number =
  wrap (cam + (0.08 * (near cam (s.b.x + (lead * s.facing)) - cam)))

(*****************************************************************************)
(* The enemies *)
(*****************************************************************************)

let nearest_standing (humans : human list) (x : number) : int option =
  let best = ref None in
  List.iteri
    (fun i (h : human) ->
      if h.hstate = Standing then
        let d = Float.abs (near x h.hx - x) in
        match !best with Some (bd, _) when bd <= d -> () | _ -> best := Some (d, i))
    humans;
  Option.map snd !best

(* A lander looks for the nearest human still standing, comes down on
 * him, and carries him up. A mutant has no interest in the planet any
 * more: it comes at you, fast and never quite straight. *)
let step_enemy (p : play) (e : enemy) : enemy =
  let sp = lander_speed p.wave in
  match (e.kind, e.holds) with
  | Mutant, _ ->
      let dx = near e.ex p.ship.b.x - e.ex and dy = p.ship.b.y - e.ey in
      let d = Float.max 1. (Float.hypot dx dy) in
      let fast = 2.2 + sp in
      { e with ex = wrap (e.ex + (dx / d * fast)); ey = e.ey + (dy / d * fast) + (2.5 * sin (e.ex / 30.)) }
  | Lander, Some _ -> { e with ey = e.ey + 1.8 }
  | Lander, None -> (
      match nearest_standing p.humans e.ex with
      | None ->
          (* nothing left to take: it drifts towards you instead *)
          let dx = near e.ex p.ship.b.x - e.ex in
          { e with ex = wrap (e.ex + Float.max (-.sp) (Float.min sp (dx / 30.))) }
      | Some i ->
          let h = List.nth p.humans i in
          let dx = near e.ex h.hx - e.ex in
          let vx = Float.max (-.sp) (Float.min sp (dx / 25.)) in
          let vy = if Float.abs dx < 50. then -2. else Float.max (-1.) (Float.min 1. ((h.hy + 120. - e.ey) / 90.)) in
          { e with ex = wrap (e.ex + vx); ey = e.ey + vy })

(* a lander that carries him this high has him: the human is gone for
 * good, and the lander is a mutant from now on *)
let top = ceiling

(*****************************************************************************)
(* The juice (juice=hand, juice=engine, juice=off) *)
(*****************************************************************************)

(* The smart bomb's flash, a counter in [update_play] and a white
 * rectangle in [view_play], is the juice written by hand: juice=hand,
 * the default. juice=engine says the same moment to the Juice module
 * -- the flash, and the screen knocked hard -- and [view_play] draws it
 * around the world, under the scanner and the score. juice=off:
 * neither. *)
let mode (computer : computer) : Juice.mode = Juice.mode ~default:Juice.Hand computer.flags

let engine_bomb (juice : Juice.t) : Juice.t = juice |> Juice.flash white 10 |> Juice.shake 0.8

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let set (i : int) (f : human -> human) (humans : human list) : human list =
  List.mapi (fun j h -> if j = i then f h else h) humans

(* the first human [f] holds for, by his place in the list *)
let find_human (f : human -> bool) (humans : human list) : int option =
  let found = ref None in
  List.iteri (fun i h -> if !found = None && f h then found := Some i) humans;
  !found

(* where a carried human hangs: under his lander, or under the ship *)
let step_human (p : play) (enemies : enemy list) (i : int) (h : human) : human =
  match h.hstate with
  | Dead | Standing -> h
  | Held -> { h with hx = p.ship.b.x; hy = p.ship.b.y - 24. }
  | Grabbed -> (
      match List.find_opt (fun (e : enemy) -> e.holds = Some i) enemies with
      | Some e -> { h with hx = e.ex; hy = e.ey - 26. }
      (* his lander was shot from under him *)
      | None -> { h with hstate = Falling; hvy = 0. })
  | Falling ->
      let hvy = h.hvy - 0.25 in
      let hy = h.hy + hvy in
      let floor = ground h.hx + 14. in
      if hy > floor then { h with hy; hvy }
      else if Float.abs hvy < 6. then { h with hy = floor; hvy = 0.; hstate = Standing }
      else (* dropped from too high *) { h with hy = floor; hstate = Dead }

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let k = computer.keyboard in
  let pressed f = Scene2d.pressed f scenes in
  let flying = p.dead = 0 in
  let ship = if flying then drive k p.ship else p.ship in
  let cam = follow ship p.cam in
  (* the laser, and the smart bomb *)
  let lasers =
    (if flying && pressed (fun k -> k.kspace) then
       [ Shots.straight ship.b.x ship.b.y (laser_speed * ship.facing) 0. ]
     else [])
    @ List.map Shots.advance p.lasers
  in
  (* a laser is gone once it is a screen away from the ship *)
  let lasers = List.filter (fun (s : Shots.t) -> Float.abs (near ship.b.x s.x - ship.b.x) < 700.) lasers in
  let bombing = flying && p.bombs > 0 && pressed (fun k -> Set_.mem "b" k.keys) in
  let on_screen (e : enemy) : bool = Float.abs (near cam e.ex - cam) < 520. in
  let bombed, enemies = if bombing then List.partition on_screen p.enemies else ([], p.enemies) in
  (* every enemy moves, then the landers that arrived take their human
   * and the ones that reached the top turn mutant *)
  let enemies = List.map (step_enemy { p with ship; enemies }) enemies in
  let humans = ref p.humans in
  let enemies =
    List.map
      (fun (e : enemy) ->
        match (e.kind, e.holds) with
        | Lander, None -> (
            match nearest_standing !humans e.ex with
            | Some i when apart (e.ex, e.ey) ((List.nth !humans i).hx, (List.nth !humans i).hy) < 30. ->
                humans := set i (fun h -> { h with hstate = Grabbed }) !humans;
                { e with holds = Some i }
            | _ -> e)
        | Lander, Some i when e.ey > top ->
            humans := set i (fun h -> { h with hstate = Dead }) !humans;
            { e with kind = Mutant; holds = None }
        | _ -> e)
      enemies
  in
  (* the lasers that hit: the enemy goes, and what it was carrying falls *)
  let hit_by_laser (e : enemy) : bool =
    List.exists (fun (s : Shots.t) -> Shots.near 26. (near s.x e.ex, e.ey) s) lasers
  in
  let shot_down, enemies = List.partition hit_by_laser enemies in
  List.iter (fun (e : enemy) -> match e.holds with Some i -> humans := set i (fun h -> { h with hstate = Falling; hvy = 0. }) !humans | None -> ()) shot_down;
  let lasers = List.filter (fun (s : Shots.t) -> not (List.exists (fun (e : enemy) -> Shots.near 26. (near s.x e.ex, e.ey) s) shot_down)) lasers in
  (* the landers shoot back, but only what they can see of you *)
  let bullets = ref (List.map Shots.advance p.bullets) in
  let enemies =
    List.map
      (fun (e : enemy) ->
        if (not flying) || e.cool > 0 || not (on_screen e) then { e with cool = max 0 (e.cool -.. 1) }
        else begin
          bullets := Shots.aimed 6. (near ship.b.x e.ex, e.ey) (ship.b.x, ship.b.y) :: !bullets;
          { e with cool = 90 }
        end)
      enemies
  in
  let bullets = List.filter (fun (s : Shots.t) -> Float.abs (near cam s.x - cam) < 700. && Float.abs s.y < 600.) !bullets in
  (* the humans: carried, falling, or put down *)
  let humans = List.mapi (step_human { p with ship } enemies) !humans in
  (* the ship catches a falling human, and gives him back to the ground *)
  let carried, humans, rescued =
    match p.carried with
    | Some i when not flying -> (None, set i (fun h -> { h with hstate = Falling; hvy = 0. }) humans, 0)
    | Some i ->
        if ship.b.y < ground ship.b.x + 30. then
          (None, set i (fun h -> { h with hy = ground h.hx + 14.; hvy = 0.; hstate = Standing }) humans, rescue_points)
        else (Some i, humans, 0)
    | None ->
        if not flying then (None, humans, 0)
        else (
          match find_human (fun (h : human) -> h.hstate = Falling && apart (ship.b.x, ship.b.y) (h.hx, h.hy) < 30.) humans with
          | Some i -> (Some i, set i (fun h -> { h with hstate = Held }) humans, 0)
          | None -> (None, humans, 0))
  in
  (* what kills you: a bullet, or flying into anything *)
  let shot = List.exists (fun (s : Shots.t) -> Shots.near 16. (near s.x ship.b.x, ship.b.y) s) bullets in
  let rammed = List.exists (fun (e : enemy) -> apart (ship.b.x, ship.b.y) (e.ex, e.ey) < 26.) enemies in
  let lost = flying && (shot || rammed) in
  let enemies = if lost then List.filter (fun (e : enemy) -> apart (ship.b.x, ship.b.y) (e.ex, e.ey) >= 26.) enemies else enemies in
  (* the planet goes when the last human does *)
  let planet = p.planet && List.exists alive humans in
  let enemies = if p.planet && not planet then List.map (fun e -> { e with kind = Mutant; holds = None }) enemies else enemies in
  let wave = if enemies = [] then p.wave +.. 1 else p.wave in
  {
    ship = (if lost then new_ship ship.b.x else ship);
    cam;
    humans;
    enemies = (if enemies = [] then spawn_wave wave cam else enemies);
    lasers;
    bullets = (if lost then [] else bullets);
    carried = (if lost then None else carried);
    wave;
    score = p.score +.. rescued +.. ((List.length shot_down +.. List.length bombed) *.. lander_points);
    lives = (if lost then p.lives -.. 1 else p.lives);
    bombs = (if bombing then p.bombs -.. 1 else p.bombs);
    dead = (if lost then 100 else max 0 (p.dead -.. 1));
    planet;
    flash = (if bombing && mode computer = Juice.Hand then 10 else max 0 (p.flash -.. 1));
    juice = (let juice = Juice.step computer p.juice in if bombing && mode computer = Juice.Engine then engine_bomb juice else juice);
  }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title | Over _ -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (start ())) scenes else scenes
  | Playing p ->
      let p = update_play computer scenes p in
      if p.lives <= 0 then Scene2d.go (Over p.score) scenes else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the range, sampled across the window the camera can see *)
let mountains (cam : number) : shape =
  let steps = 56 in
  let w = 1200. in
  let at i = cam - (w / 2.) + (float_of_int i * w / float_of_int steps) in
  polygon rock
    (((cam + (w / 2.), -520.) :: (cam - (w / 2.), -520.) :: List.init (steps +.. 1) (fun i -> (at i, ground (at i))))
    |> List.rev)

(* space, once the planet is gone: the stars are the same function, a
 * lot further up *)
let stars (cam : number) : shape list =
  List.init 40 (fun i ->
      let x = cam - 600. + (float_of_int i * 30.) in
      circle (rgb 120 120 150) 2. |> move x (ground (x * 4.) + (420. * sin (x / 91.))))

let human_shape : shape =
  group [ rectangle human_color 6. 12.; circle human_color 4. |> move_y 10.; rectangle human_color 10. 3. |> move_y 2. ]

let enemy_shape (e : enemy) : shape =
  let c = if e.kind = Lander then lander_color else mutant_color in
  group
    [ rectangle c 26. 8.; triangle c 12. |> rotate 180. |> move_y (-8.); rectangle c 4. 10. |> move (-9.) 8.;
      rectangle c 4. 10. |> move 9. 8. ]

let ship_shape (s : ship) : shape =
  group
    [ rectangle ship_color 34. 8.; rectangle ship_color 14. 14. |> move (-6. * s.facing) 4.;
      triangle ship_color 9. |> rotate (-90. * s.facing) |> move (20. * s.facing) 0. ]

(* The scanner: the whole planet in a strip, which is nothing but the
 * same model drawn at another scale. A minimap is a camera. *)
let scan_w = 620.
let scan_h = 110.
let scan_y = 425.

let scanner (p : play) : shape list =
  let dot (color : color) (r : number) (x : number) (y : number) : shape =
    circle color r |> move ((near p.cam x - p.cam) / world_w * scan_w) (scan_y + ((y + 90.) / 360. * (scan_h / 2.)))
  in
  [ rectangle (rgb 25 25 50) scan_w scan_h |> move_y scan_y;
    rectangle (rgb 70 70 120) scan_w 2. |> move_y (scan_y + (scan_h / 2.));
    rectangle (rgb 70 70 120) scan_w 2. |> move_y (scan_y - (scan_h / 2.)) ]
  @ List.filter_map (fun (h : human) -> if alive h then Some (dot human_color 2.5 h.hx h.hy) else None) p.humans
  @ List.map (fun (e : enemy) -> dot (if e.kind = Lander then lander_color else mutant_color) 3. e.ex e.ey) p.enemies
  @ [ dot ship_color 3.5 p.ship.b.x p.ship.b.y ]

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let cam = Camera2d.origin |> Camera2d.look_at p.cam 0. in
  let at (x : number) (y : number) (s : shape) : shape = s |> move (near p.cam x) y in
  let world =
    (if p.planet then [ mountains p.cam ] else stars p.cam)
    @ List.filter_map (fun (h : human) -> if alive h then Some (at h.hx h.hy human_shape) else None) p.humans
    @ List.map (fun (e : enemy) -> at e.ex e.ey (enemy_shape e)) p.enemies
    @ List.map (fun (s : Shots.t) -> at s.x s.y (rectangle (rgb 255 240 120) 60. 3.)) p.lasers
    @ List.map (fun (s : Shots.t) -> at s.x s.y (circle (rgb 255 150 60) 4.)) p.bullets
    @ (if p.dead > 0 then [] else [ at p.ship.b.x p.ship.b.y (ship_shape p.ship) ])
  in
  (rectangle sky_color screen.width screen.height :: Juice.view p.juice [ Camera2d.view cam world ])
  @ (if p.flash > 0 then [ rectangle white screen.width screen.height |> fade (float_of_int p.flash / 20.) ] else [])
  @ scanner p
  @ [ text white 2.2
        (Printf.sprintf "score %d    wave %d    humans %d    bombs %d" p.score p.wave
           (List.length (List.filter alive p.humans)) p.bombs)
      |> move_y 330.;
      text ship_color 2.2 (String.concat " " (List.init (max 0 p.lives) (fun _ -> "^"))) |> move_y 300. ]
  @ if p.dead > 0 then [ text mutant_color 3. "AGAIN" |> move_y (-40.) ] else []

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      [ rectangle sky_color screen.width screen.height;
        text white 6. "TINY DEFENDER" |> move_y 230.;
        text white 2. "left/right: thrust, and the way you point (press the other way to flip)" |> move_y 100.;
        text white 2. "up/down: climb and dive    space: laser    b: smart bomb" |> move_y 60.;
        text lander_color 2. "the landers carry the humans up. shoot one, catch what falls," |> move_y 0.;
        text lander_color 2. "and fly it down to the ground" |> move_y (-40.);
        text mutant_color 2. "a lander that reaches the top becomes a mutant, and wants only you" |> move_y (-90.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-230.) ]
  | Playing p -> view_play computer p
  | Over score ->
      [ rectangle sky_color screen.width screen.height;
        text white 5. "GAME OVER" |> move_y 120.;
        text white 3. (Printf.sprintf "score %d" score) |> move_y 20. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-230.) ]

let help =
  {|TinyDefender
  left/right  thrust, and the way you point (press the other way to flip)
  up/down     climb and dive
  space       the laser (space also starts and restarts)
  b           a smart bomb: everything on screen, gone
  the planet is six screens wide: the scanner at the top is all of it
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
