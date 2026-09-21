(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Robotron: 2084 (Williams, 1982, Eugene Jarvis and
 * Larry DeMar), the game with two joysticks: one to run, one to shoot.
 * One screen, no scrolling, a crowd of robots walking straight at you,
 * a human family to carry out of it, and one touch kills. Arrows to
 * move, w/a/s/d to shoot.
 *
 * Jarvis had written Defender (1981); the two sticks came, the story
 * goes, from Berzerk (Stern, 1980) played with one hand on the stick
 * and one on the fire button, and from a broken cabinet whose second
 * stick was wired up for fun. Everything since that aims with the
 * right thumb -- Smash TV (1990, Jarvis again), Geometry Wars (2003),
 * every twin-stick shooter -- is its descendant. The premise is on the
 * attract screen: in 2084 the robots have concluded that humans are
 * inefficient, and you, a genetic error, save the last human family.
 * (Names and dates from memory, to check.)
 *
 * The new idea here is two directions at once. Every other shooter in
 * games/ fires where it points: TinyInvaders' cannon up, TinyGalaga's
 * fighter up, Asteroid's ship along its nose. Here the arrows give one
 * vector and w/a/s/d another ([to_xy] and [to_x2]/[to_y2], whose first
 * user this is), and the whole game lives in the gap between them:
 * running left while shooting right is not a trick, it is how you are
 * meant to play.
 *
 *     arrows: where you run          w/a/s/d: where you shoot
 *            (0, 1)                          (0, 1)
 *       (-1, 0) + (1, 0)      *          (-1, 0) + (1, 0)
 *            (0, -1)         man                (0, -1)
 *
 * The second idea is that the crowd needs no intelligence. Every robot
 * here is one line -- a step towards the man, or towards the nearest
 * human -- and thirty of them on the screen read as a swarm. There is
 * no pathfinding (ai/Pathfind, which TinyTowerDefense uses) on
 * purpose: grunts walk into electrodes and die there, as they do in
 * the arcade, and that is the design rather than a bug.
 *
 * What it uses: the shoot 'em up kit (gamekits/shmup/Shots, its third user
 * after TinyInvaders and TinyGalaga: the man's shots, and the
 * enforcers' aimed sparks), Sprite (the pixel art), Scene2d (title,
 * play, game over), Audio. Not Physics: nothing here has inertia, the
 * man stops the frame you let the arrows go, which is the arcade's
 * feel (inertia lives in Asteroid.ml); not Camera2d or Tilemap: one
 * screen, no scrolling, and nothing to walk on; not Random: the waves
 * come from a seed carried in the model, so a run is reproducible (the
 * golden frames and the robot test rest on it).
 *
 * Exercises: the brain's cruise missiles (a shot that steers -- Shots
 * with a turn rate); the quarks and their tanks, whose shells bounce
 * off the walls; the rescue bonus shown rising, 1000 to 5000, where it
 * is scored; grunts entering in a pattern instead of at random; two
 * players (a second man on i/j/k/l, and the sticks swapped); an
 * attract mode replaying a recorded -script.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The arena *)
(*****************************************************************************)

(* the screen is 1000 x 1000, (0, 0) at its center; the strip above the
 * arena is the score *)
let wall_x = 450.
let wall_y = 400.
let margin = 24. (* how close to a wall anything can stand *)

let inside (x : number) (y : number) : bool =
  Float.abs x < wall_x - margin && Float.abs y < wall_y - margin

let hold (x : number) (y : number) : number * number = (clamp (0. - wall_x + margin) (wall_x - margin) x, clamp (0. - wall_y + margin) (wall_y - margin) y)

(*****************************************************************************)
(* The sprites *)
(*****************************************************************************)

let palette =
  [ ('w', white); ('c', rgb 120 220 255); ('r', rgb 255 70 70); ('o', orange); ('g', rgb 60 220 90); ('p', rgb 230 90 230); ('y', yellow);
    ('b', rgb 80 120 255) ]

let sprite (size : number) (rows : string list) : shape = Sprite.pixels size palette rows

(* the man, and the family he carries out: the arcade's Mommy, Daddy
 * and Mikey, three colors of the same little figure *)
let man_rows = [ "..cccc.."; "..cccc.."; "...ww..."; ".wwwwww."; "w..ww..w"; "...ww..."; "..w..w.."; ".ww..ww." ]
let human_rows = [ "..yyyy.."; "..yyyy.."; "...bb..."; "..bbbb.."; ".b.bb.b."; "...bb..."; "..b..b.."; ".b....b." ]

(* the robots: a grunt walks, a hulk is twice its size, a brain carries
 * its own on top, a prog is a human that has been rebuilt *)
(* two frames, the legs together and apart: a grunt does not walk, it
 * hops, and the hop is the whole animation (Sprite.cycle) *)
let grunt_rows =
  [ [ ".rrrrrr."; "r.rrrr.r"; "rr.rr.rr"; ".rrrrrr."; "..rrrr.."; ".r.rr.r."; ".r.rr.r."; "rr....rr" ];
    [ ".rrrrrr."; "..rrrr.."; "rr.rr.rr"; ".rrrrrr."; "..rrrr.."; "..rrrr.."; ".r.rr.r."; ".r....r." ] ]
let hulk_rows = [ ".gggggg."; "gg.gg.gg"; "gggggggg"; "g.gggg.g"; "gg.gg.gg"; ".gg..gg."; ".gg..gg."; "gg....gg" ]
let brain_rows = [ "..pppp.."; ".pyppyp."; "pppppppp"; "pyppppyp"; ".pppppp."; "...gg..."; "..g..g.."; ".gg..gg." ]
let prog_rows = [ "..gggg.."; "..gggg.."; "...cc..."; "..cccc.."; ".c.cc.c."; "...cc..."; "..c..c.."; ".c....c." ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kind =
  | Grunt (* walks at the man, faster the longer the wave lasts *)
  | Electrode (* stands still; deadly to touch, to robots too *)
  | Hulk (* indestructible: kills the humans, shots only shove it *)
  | Spheroid (* drifts, and opens to let enforcers out *)
  | Enforcer (* flies at the man and fires sparks *)
  | Brain (* walks to a human and rebuilds it into a prog *)
  | Prog (* what is left of that human: hunts the man *)

type enemy = { kind : kind; x : number; y : number; vx : number; vy : number; timer : int (* to its next spawn or shot *) }
type human = { hx : number; hy : number; hvx : number; hvy : number }

type game = {
  mx : number; (* the man *)
  my : number;
  aim : number * number; (* where he last shot, for the muzzle *)
  cool : int; (* frames until he can fire again *)
  shots : Shots.t list;
  sparks : Shots.t list; (* the enforcers' *)
  enemies : enemy list;
  humans : human list;
  rescued : int; (* this wave: the bonus rises with each one *)
  blasts : (number * number * int) list; (* where something died, and frames since *)
  score : int;
  lives : int;
  dead : int; (* frames since the man was caught, 0 while he runs *)
  wave : int;
  frames : int; (* in this wave: what the grunts speed up with *)
  seed : int;
}

type scene = Title | Playing of game | Game_over of int
type model = { scenes : scene Scene2d.t; hi_score : int }

(*****************************************************************************)
(* The waves *)
(*****************************************************************************)

(* The random of this game: a linear congruential generator (Lehmer
 * 1949, these constants from the C standard's example) carried in the
 * model. Not Random: its state is global, and a wave laid out from a
 * seed is the same in every run and on every backend. *)
let next (seed : int) : int = ((seed *.. 1103515245) +.. 12345) land 0x7fffffff
let between (lo : number) (hi : number) (seed : int) : number = lo + ((hi - lo) * float_of_int (seed mod 1000) / 1000.)

(* a place in the arena, at least 170 pixels from the center, where the
 * man appears: nothing is on top of him at the start of a wave *)
let rec spot (seed : int) : number * number * int =
  let s1 = next seed in
  let s2 = next s1 in
  let x = between (-420.) 420. s1 and y = between (-370.) 370. s2 in
  if Float.hypot x y < 170. then spot s2 else (x, y, s2)

let spawn (kind : kind) (seed : int) : enemy * int =
  let x, y, seed = spot seed in
  let s = next seed in
  let a = between 0. 360. s in
  let speed = match kind with Hulk -> 1.7 | Spheroid -> 1.5 | _ -> 0. in
  ({ kind; x; y; vx = speed * cos (degrees_to_radians a); vy = speed * sin (degrees_to_radians a); timer = 60 +.. (s mod 90) }, s)

let human (seed : int) : human * int =
  let x, y, seed = spot seed in
  let s = next seed in
  let a = between 0. 360. s in
  ({ hx = x; hy = y; hvx = 1.1 * cos (degrees_to_radians a); hvy = 1.1 * sin (degrees_to_radians a) }, s)

(* How many of each, wave by wave: grunts from the start, then the
 * standing electrodes, the hulks, the spheroids, the brains -- the
 * arcade introduces its robots one kind at a time, and a wave is a
 * number of each rather than a level to design. *)
let how_many (kind : kind) (wave : int) : int =
  match kind with
  | Grunt -> min 26 (5 +.. (3 *.. wave))
  | Electrode -> if wave < 2 then 0 else min 9 (wave +.. 1)
  | Hulk -> if wave < 3 then 0 else min 4 ((wave -.. 1) /.. 2)
  | Spheroid -> if wave < 4 then 0 else min 3 (1 +.. (wave /.. 4))
  | Brain -> if wave < 5 then 0 else min 3 (1 +.. (wave /.. 5))
  | Enforcer | Prog -> 0 (* let out by a spheroid, made out of a human *)

let start_wave (wave : int) (g : game) : game =
  let add kind (es, seed) = List.fold_left (fun (es, seed) _ -> let e, seed = spawn kind seed in (e :: es, seed)) (es, seed) (List.init (how_many kind wave) (fun i -> i)) in
  let enemies, seed = ([], next (wave *.. 7919)) |> add Grunt |> add Electrode |> add Hulk |> add Spheroid |> add Brain in
  let humans, seed = List.fold_left (fun (hs, seed) _ -> let h, seed = human seed in (h :: hs, seed)) ([], seed) (List.init (2 +.. (wave mod 3)) (fun i -> i)) in
  { g with mx = 0.; my = 0.; aim = (0., 1.); cool = 0; shots = []; sparks = []; enemies; humans; rescued = 0; dead = 0; wave; frames = 0; seed }

let new_game () : game =
  start_wave 1
    { mx = 0.; my = 0.; aim = (0., 1.); cool = 0; shots = []; sparks = []; enemies = []; humans = []; rescued = 0; blasts = []; score = 0; lives = 3;
      dead = 0; wave = 1; frames = 0; seed = 1 }

let initial_model = { scenes = Scene2d.start Title; hi_score = 0 }

(*****************************************************************************)
(* Update: the man *)
(*****************************************************************************)

let man_speed = 5.
let shot_speed = 19.

(* the arrows run him, w/a/s/d shoot: the two sticks. He fires every 7
 * frames while a shooting key is held (the arcade's stick has no
 * button: holding it is firing). *)
let run (computer : computer) (g : game) : game =
  let dx, dy = to_xy computer.keyboard in
  let mx, my = hold (g.mx + (man_speed * dx)) (g.my + (man_speed * dy)) in
  { g with mx; my }

let shoot (computer : computer) (g : game) : game =
  let k = computer.keyboard in
  let ax = to_x2 k and ay = to_y2 k in
  let g = { g with cool = max 0 (g.cool -.. 1) } in
  if (ax = 0. && ay = 0.) || g.cool > 0 then g
  else begin
    Audio.play Audio.laser;
    let d = Float.hypot ax ay in
    let ax = ax / d and ay = ay / d in
    { g with aim = (ax, ay); cool = 7; shots = Shots.straight g.mx g.my (shot_speed * ax) (shot_speed * ay) :: g.shots }
  end

(*****************************************************************************)
(* Update: the robots *)
(*****************************************************************************)

(* [toward speed] from (x, y) to (tx, ty): the step of everything that
 * hunts, the one bit of arithmetic the crowd needs *)
let toward (speed : number) ((x, y) : number * number) ((tx, ty) : number * number) : number * number =
  let dx = tx - x and dy = ty - y in
  let d = Float.max 1e-9 (Float.hypot dx dy) in
  (speed * dx / d, speed * dy / d)

(* A grunt speeds up while the wave lasts, from 1.6 to 4 pixels a
 * frame: the pressure comes from the clock, not from their number, and
 * it is why a wave has to be cleared quickly. *)
let grunt_speed (g : game) : number = Float.min 4. (1.6 + (0.1 * float_of_int g.wave) + (float_of_int g.frames / 700.))

let bounce (e : enemy) : enemy =
  let vx = if Float.abs e.x > wall_x - margin then 0. - e.vx else e.vx and vy = if Float.abs e.y > wall_y - margin then 0. - e.vy else e.vy in
  let x, y = hold (e.x + vx) (e.y + vy) in
  { e with x; y; vx; vy }

let near (r : number) ((x, y) : number * number) ((tx, ty) : number * number) : bool = Float.hypot (tx - x) (ty - y) < r

(* One robot, one frame. It can fire a spark, let out an enforcer or
 * rebuild a human, so it works on the whole game, like TinyGalaga's
 * fly; the enemies come back in the reverse order, which nothing here
 * depends on. *)
let step_enemy (g : game) (e : enemy) : game =
  let keep e = { g with enemies = e :: g.enemies } in
  match e.kind with
  | Electrode -> keep e
  | Grunt ->
      let vx, vy = toward (grunt_speed g) (e.x, e.y) (g.mx, g.my) in
      keep { e with x = e.x + vx; y = e.y + vy }
  | Prog ->
      let vx, vy = toward 3.4 (e.x, e.y) (g.mx, g.my) in
      keep { e with x = e.x + vx; y = e.y + vy }
  | Enforcer ->
      let vx, vy = toward 2.6 (e.x, e.y) (g.mx, g.my) in
      let e = { e with x = e.x + vx; y = e.y + vy; timer = e.timer -.. 1 } in
      if e.timer > 0 then keep e
      else { (keep { e with timer = 80 }) with sparks = Shots.aimed 6. (e.x, e.y) (g.mx, g.my) :: g.sparks }
  | Spheroid ->
      let e = { (bounce e) with timer = e.timer -.. 1 } in
      if e.timer > 0 then keep e
      else begin
        Audio.play Audio.blip;
        { (keep { e with timer = 150 }) with enemies = { kind = Enforcer; x = e.x; y = e.y; vx = 0.; vy = 0.; timer = 60 } :: e :: g.enemies }
      end
  | Hulk ->
      (* it walks its way, turning at the walls and now and then, and
       * what it walks into is the family: a human it touches is gone *)
      let e = bounce e in
      let e = if e.timer > 0 then { e with timer = e.timer -.. 1 } else { e with vx = 0. - e.vy; vy = e.vx; timer = 120 } in
      let caught, spared = List.partition (fun (h : human) -> near 26. (e.x, e.y) (h.hx, h.hy)) g.humans in
      if caught <> [] then Audio.play Audio.hit;
      { (keep e) with humans = spared; blasts = List.map (fun (h : human) -> (h.hx, h.hy, 0)) caught @ g.blasts }
  | Brain -> (
      (* to the nearest human, and what it reaches walks back at you *)
      match List.sort (fun (a : human) (b : human) -> compare (Float.hypot (a.hx - e.x) (a.hy - e.y)) (Float.hypot (b.hx - e.x) (b.hy - e.y))) g.humans with
      | [] ->
          let vx, vy = toward 1.2 (e.x, e.y) (g.mx, g.my) in
          keep { e with x = e.x + vx; y = e.y + vy }
      | h :: rest ->
          let vx, vy = toward 1.4 (e.x, e.y) (h.hx, h.hy) in
          let e = { e with x = e.x + vx; y = e.y + vy } in
          if not (near 22. (e.x, e.y) (h.hx, h.hy)) then keep e
          else begin
            Audio.play Audio.hit;
            { (keep e) with humans = rest; enemies = { kind = Prog; x = h.hx; y = h.hy; vx = 0.; vy = 0.; timer = 0 } :: e :: g.enemies }
          end)

let step_enemies (g : game) : game = List.fold_left step_enemy { g with enemies = [] } g.enemies

(* the family wanders, turning at the walls: they are not running away
 * from anything, which is what makes losing one your fault *)
let step_humans (g : game) : game =
  let step (h : human) : human =
    let hvx = if Float.abs h.hx > wall_x - margin then 0. - h.hvx else h.hvx and hvy = if Float.abs h.hy > wall_y - margin then 0. - h.hvy else h.hvy in
    let hx, hy = hold (h.hx + hvx) (h.hy + hvy) in
    { hx; hy; hvx; hvy }
  in
  { g with humans = List.map step g.humans }

(*****************************************************************************)
(* Update: what touches what *)
(*****************************************************************************)

let points (k : kind) : int = match k with Grunt -> 100 | Enforcer -> 150 | Brain | Prog -> 500 | Spheroid -> 1000 | Electrode | Hulk -> 0

let indestructible (k : kind) : bool = k = Hulk

(* the man's shots: what they hit dies, except a hulk, which they shove
 * -- the only thing in the game you cannot solve by shooting *)
let shoot_down (g : game) : game =
  List.fold_left
    (fun g (s : Shots.t) ->
      match List.find_opt (fun (e : enemy) -> Shots.near 24. (e.x, e.y) s) g.enemies with
      | None -> { g with shots = s :: g.shots }
      | Some e when indestructible e.kind ->
          let push = 6. in
          let d = Float.max 1e-9 (Float.hypot s.vx s.vy) in
          { g with enemies = List.map (fun (e' : enemy) -> if e' == e then { e with x = e.x + (push * s.vx / d); y = e.y + (push * s.vy / d) } else e') g.enemies }
      | Some e ->
          Audio.play Audio.explosion;
          { g with enemies = List.filter (fun (e' : enemy) -> e' != e) g.enemies; score = g.score +.. points e.kind; blasts = (e.x, e.y, 0) :: g.blasts })
    { g with shots = [] } g.shots

(* a robot that walks into an electrode dies there: nothing is checked
 * for it, they simply don't look where they are going *)
let walk_into_electrodes (g : game) : game =
  let electrodes = List.filter (fun (e : enemy) -> e.kind = Electrode) g.enemies in
  let hits (e : enemy) = (not (e.kind = Electrode || indestructible e.kind)) && List.exists (fun (o : enemy) -> near 22. (e.x, e.y) (o.x, o.y)) electrodes in
  let dead, alive = List.partition hits g.enemies in
  if dead = [] then g else { g with enemies = alive; blasts = List.map (fun (e : enemy) -> (e.x, e.y, 0)) dead @ g.blasts }

(* the bonus rises with each rescue of the wave, 1000 to 5000, and
 * starts again at the next one *)
let rescue (g : game) : game =
  let saved, waiting = List.partition (fun (h : human) -> near 26. (g.mx, g.my) (h.hx, h.hy)) g.humans in
  List.fold_left
    (fun g (h : human) ->
      Audio.play Audio.coin;
      { g with rescued = g.rescued +.. 1; score = g.score +.. (1000 *.. min 5 (g.rescued +.. 1)); blasts = (h.hx, h.hy, 0) :: g.blasts })
    { g with humans = waiting } saved

(* one touch kills: every robot, the standing electrodes, the sparks *)
let man_caught (g : game) : game =
  let touched = List.exists (fun (e : enemy) -> near (if e.kind = Hulk then 30. else 24.) (g.mx, g.my) (e.x, e.y)) g.enemies in
  let shot = List.exists (Shots.near 16. (g.mx, g.my)) g.sparks in
  if g.dead > 0 || not (touched || shot) then g
  else begin
    Audio.play Audio.explosion;
    { g with dead = 1; lives = g.lives -.. 1; blasts = (g.mx, g.my, 0) :: g.blasts }
  end

(* a wave is over when the robots that can be shot are gone: the hulks
 * and the electrodes stay, and you leave them behind *)
let cleared (g : game) : bool = not (List.exists (fun (e : enemy) -> not (e.kind = Electrode || indestructible e.kind)) g.enemies)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; seed = next g.seed; blasts = List.filter_map (fun (x, y, n) -> if n < 24 then Some (x, y, n +.. 1) else None) g.blasts } in
  if g.dead > 0 then
    (* two seconds, then the same wave again, with what is left of the
     * family: the arcade's way, and it is why losing a human hurts *)
    if g.dead > 110 && g.lives > 0 then { (start_wave g.wave g) with score = g.score; lives = g.lives; blasts = g.blasts; humans = g.humans }
    else { g with dead = g.dead +.. 1 }
  else
    let g = g |> run computer |> shoot computer |> step_humans |> step_enemies |> walk_into_electrodes in
    let g =
      { g with shots = List.filter (fun (s : Shots.t) -> inside s.x s.y) (List.map Shots.advance g.shots);
        sparks = List.filter (fun (s : Shots.t) -> inside s.x s.y) (List.map Shots.advance g.sparks) }
    in
    let g = g |> shoot_down |> rescue |> man_caught in
    if cleared g then { (start_wave (g.wave +.. 1) g) with score = g.score; lives = g.lives; blasts = g.blasts } else g

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model.scenes in
  let fire = Scene2d.pressed (fun k -> k.kspace) scenes in
  match scenes.scene with
  | Title -> if fire then { model with scenes = Scene2d.go (Playing (new_game ())) scenes } else { model with scenes }
  | Playing g ->
      let g = update_game computer g in
      let hi_score = max model.hi_score g.score in
      if g.lives = 0 && g.dead > 110 then { hi_score; scenes = Scene2d.go (Game_over g.score) scenes } else { hi_score; scenes = { scenes with scene = Playing g } }
  | Game_over _ -> if fire || scenes.elapsed > 10. then { model with scenes = Scene2d.go Title scenes } else { model with scenes }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the playground draws filled shapes, so the arena's frame is a
 * colored rectangle with a black one on top of it *)
let arena (n : int) : shape list =
  let color = [| rgb 60 90 230; rgb 120 60 230; rgb 60 200 200 |].(n /.. 20 mod 3) in
  [ rectangle color ((wall_x * 2.) + 12.) ((wall_y * 2.) + 12.); rectangle black (wall_x * 2.) (wall_y * 2.) ]

let look (frames : int) (k : kind) : shape =
  match k with
  | Grunt -> sprite 4. (Sprite.cycle (frames /.. 8) grunt_rows)
  | Prog -> sprite 4. prog_rows
  | Brain -> sprite 4. brain_rows
  | Hulk -> sprite 5. hulk_rows
  | Electrode ->
      (* it turns, and that is all it ever does *)
      group [ square (rgb 230 90 230) 22. |> rotate (float_of_int frames * 2.); square (rgb 255 220 255) 8. |> rotate (float_of_int frames * 2.) ]
  | Spheroid -> group [ circle (rgb 80 160 255) (16. + (3. * sin (float_of_int frames / 6.))); circle black 9.; circle (rgb 200 230 255) 4. ]
  | Enforcer -> group [ square (rgb 60 220 220) 18. |> rotate 45.; circle white 5. ]

let view_game (g : game) : shape list =
  arena g.frames
  @ List.map (fun (e : enemy) -> look g.frames e.kind |> move e.x e.y) g.enemies
  @ List.map (fun (h : human) -> sprite 4. human_rows |> move h.hx h.hy) g.humans
  (* the shots are bars along their way, the arcade's bright dashes *)
  @ List.map (fun (s : Shots.t) -> rectangle (rgb 255 255 150) 20. 5. |> rotate (Shots.angle s) |> move s.x s.y) g.shots
  @ List.map (fun (s : Shots.t) -> circle (rgb 255 140 60) 5. |> move s.x s.y) g.sparks
  @ List.map (fun (x, y, n) -> circle (if n mod 6 < 3 then orange else yellow) (6. + (float_of_int n * 1.4)) |> fade (1. - (float_of_int n / 24.)) |> move x y) g.blasts
  @ (if g.dead = 0 then [ sprite 4. man_rows |> move g.mx g.my ] else [])
  (* the flash where he is shooting (between two shots, i.e. while he
   * is firing at all), so that a still frame shows the second stick
   * too, pointing where the first one is not *)
  @ (if g.dead = 0 && g.cool > 0 then [ (let ax, ay = g.aim in circle (rgb 255 255 150) 7. |> move (g.mx + (ax * 26.)) (g.my + (ay * 26.))) ] else [])
  @ if g.frames < 90 then [ text (rgb 90 200 255) 3.5 (Printf.sprintf "WAVE %d" g.wave) |> move_y 200. ] else []

let header (model : model) (g : game) : shape list =
  [ text (rgb 255 90 90) 2.5 "SCORE" |> move (-400.) 470.; text white 2.5 (Printf.sprintf "%d" g.score) |> move (-400.) 440.;
    text (rgb 255 90 90) 2.5 "HIGH" |> move_y 470.; text white 2.5 (Printf.sprintf "%d" model.hi_score) |> move_y 440.;
    text (rgb 255 90 90) 2.5 (Printf.sprintf "WAVE %d" g.wave) |> move 380. 470. ]
  @ List.init (max 0 (g.lives -.. 1)) (fun i -> sprite 2.5 man_rows |> move (330. + (float_of_int i * 30.)) 440.)

let view_title (scenes : scene Scene2d.t) (model : model) : shape list =
  [ text (rgb 90 200 255) 6. "TINY ROBOTRON" |> move_y 380.; text white 2. "2084: the robots have concluded that humans are inefficient" |> move_y 320. ]
  @ List.concat
      (List.mapi
         (fun i (k, name, what, pts) ->
           let y = 210. - (float_of_int i * 62.) in
           [ look (i *.. 30) k |> move (-390.) y; text (rgb 120 220 255) 2.2 name |> move (-290.) y; text white 2.2 what |> move 90. y ]
           @ if pts = "" then [] else [ text yellow 2.2 pts |> move 400. y ])
         [ (Grunt, "grunt", "walks at you, faster and faster", "100"); (Electrode, "electrode", "stands there; robots die on it too", "");
           (Hulk, "hulk", "only shoved by shots; eats the family", ""); (Spheroid, "spheroid", "opens, and lets enforcers out", "1000");
           (Enforcer, "enforcer", "flies at you and fires", "150"); (Brain, "brain", "rebuilds a human into a prog", "500") ])
  @ [ text (rgb 255 220 120) 2.4 "arrows RUN     w a s d SHOOT     the two are independent" |> move_y (-260.);
      text white 2.2 (Printf.sprintf "rescue the family: 1000, 2000 ... 5000      high %d" model.hi_score) |> move_y (-310.) ]
  @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-390.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen and scenes = model.scenes in
  rectangle black screen.width screen.height
  ::
  (match scenes.scene with
  | Title -> view_title scenes model
  | Playing g -> view_game g @ header model g
  | Game_over score ->
      [ text (rgb 255 90 90) 6. "GAME OVER" |> move_y 60.; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-40.) ]
      @ Scene2d.blink 1. scenes [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
