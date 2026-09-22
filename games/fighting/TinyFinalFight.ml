(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Final Fight (Capcom, 1989): walk down a street, beat
 * up the thugs who come at you, wave after wave, and their boss at the
 * end. Arrows to walk (up and down: into the street's depth), space to
 * punch (three in a row: a combo, the last knocks down), x to jump (and
 * space in the air: a jump kick), z for the spinning attack that hits
 * everyone around, at the cost of a little of your health.
 *
 * Kung-Fu Master (Irem, 1984) had a man fighting down a corridor;
 * Renegade and Double Dragon (Technōs, 1986 and 1987) gave the fight a
 * street, and depth, and two players; Final Fight made it big: large
 * characters, a mayor punching the gang that kidnapped his daughter,
 * food in the barrels. Streets of Rage (Sega, 1991) answered. (Names and
 * dates from memory, to check.)
 *
 * What's new here, with the brawler kit (gamekits/brawler/), shared with
 * TinyStreetFighter:
 *
 *  - The belt: characters walk left and right, and in depth, up and down
 *    the street ([z]); a punch only lands on someone on your line (their
 *    depth close to yours, [same_line]), and everyone is drawn from the
 *    back of the street to the front ([view_game]: sorted by depth, the
 *    painter's algorithm again, see graphics/3d's Painter). A shadow on
 *    the ground says where a jumping fighter is.
 *
 *  - Combos by chaining: a punch that hit, punch again during its
 *    recovery, and the next one of the three starts at once ([chain]);
 *    the third knocks down. The rhythm of the whole genre.
 *
 *  - The screen that locks ([waves]): at each wave, the camera stops,
 *    the thugs come in from both sides, and only when they're down does
 *    "GO ->" let you walk on.
 *
 *  - Thugs as state machines too (the kit's Frame_data for their slow
 *    punch, readable: 12 frames of wind-up), and a simple plan: get on
 *    the player's line, at arm's length, on the side you're on; punch;
 *    wait a while.
 *
 * What it uses: the brawler kit (Hitbox, Frame_data, Stickman), Camera2d
 * (the scrolling street), Scene2d, Audio. Not Physics: knocked down,
 * a fighter flies a fixed arc.
 *
 * Exercises: grabs and throws (walk into a thug: hold him, knee him,
 * throw him), weapons (a pipe, a knife), a second player, more kinds of
 * thugs (the knife thrower, the big one who charges), the enemy's name
 * over its health bar.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* Moves *)
(*****************************************************************************)

type attack = Jab1 | Jab2 | Hook | Jump_kick | Spin | Thug_punch | Boss_punch

let move_of (a : attack) : Frame_data.move =
  match a with
  | Jab1 -> { startup = 3; active = 2; recovery = 9; damage = 4; hitstun = 18; blockstun = 0; hitbox = { x = 60.; y = 125.; w = 50.; h = 30. } }
  | Jab2 -> { startup = 3; active = 2; recovery = 9; damage = 5; hitstun = 18; blockstun = 0; hitbox = { x = 60.; y = 125.; w = 50.; h = 30. } }
  | Hook -> { startup = 5; active = 3; recovery = 14; damage = 8; hitstun = 0; blockstun = 0; hitbox = { x = 65.; y = 120.; w = 60.; h = 40. } }
  | Jump_kick -> { startup = 3; active = 20; recovery = 2; damage = 9; hitstun = 0; blockstun = 0; hitbox = { x = 55.; y = 40.; w = 60.; h = 50. } }
  | Spin -> { startup = 2; active = 16; recovery = 10; damage = 7; hitstun = 0; blockstun = 0; hitbox = { x = 0.; y = 90.; w = 190.; h = 60. } }
  | Thug_punch -> { startup = 12; active = 3; recovery = 22; damage = 6; hitstun = 22; blockstun = 0; hitbox = { x = 60.; y = 120.; w = 50.; h = 30. } }
  | Boss_punch -> { startup = 16; active = 4; recovery = 28; damage = 12; hitstun = 0; blockstun = 0; hitbox = { x = 80.; y = 150.; w = 60.; h = 40. } }

(* the moves that knock down: a hitstun of 0 in the table above *)
let knocks_down (a : attack) : bool = (move_of a).hitstun = 0

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type state =
  | Idle
  | Walk
  | Jump of number (* sideways speed *)
  | Attacking of attack * int * bool (* the move, frames since it started, whether it hit *)
  | Hit of int (* frames of hitstun left *)
  | Knocked of int (* frames since, flying back, lying, getting up *)

type fighter = {
  x : number;
  z : number; (* depth: 0 the front of the street, 200 its back *)
  y : number; (* height, jumping or flying *)
  vy : number;
  facing : number;
  hp : int;
  max_hp : int;
  state : state;
  big : bool; (* the boss *)
  wait : int; (* a thug's frames before it may punch again *)
  color : color;
}

type game = {
  player : fighter;
  thugs : fighter list;
  cam : number; (* the camera's x *)
  wave : int; (* the waves already come *)
  barrel : (number * number) option; (* where, until broken *)
  food : (number * number) option; (* a roast chicken, until eaten *)
  lives : int;
  score : int;
  last_hit : fighter option; (* the thug whose health bar shows *)
  sparks : (number * number * int) list;
  seed : int;
  frames : int;
}

type scene = Title | Street of game | Cleared of int | Game_over of int
type model = scene Scene2d.t

let floor_y = -430.
let depth_max = 200.
let street_end = 2800.

let new_player () : fighter =
  { x = 150.; z = 100.; y = 0.; vy = 0.; facing = 1.; hp = 100; max_hp = 100; state = Idle; big = false; wait = 0; color = rgb 240 240 240 }

let thug (x : number) (z : number) (big : bool) (color : color) : fighter =
  { x; z; y = 0.; vy = 0.; facing = -1.; hp = (if big then 90 else 24); max_hp = (if big then 90 else 24); state = Idle; big; wait = 30; color }

(* the waves: where the camera stops, and who comes (from off the
 * screen's sides) *)
let waves : (number * (number -> fighter list)) list =
  let purple = rgb 150 80 200 and green = rgb 80 170 90 and orange = rgb 240 140 40 in
  [ (500., fun c -> [ thug (c + 600.) 60. false purple; thug (c - 600.) 150. false green ]);
    (1200., fun c -> [ thug (c + 600.) 40. false green; thug (c + 650.) 160. false purple; thug (c - 600.) 100. false purple ]);
    (1900., fun c -> [ thug (c + 600.) 50. false purple; thug (c - 600.) 170. false green; thug (c + 700.) 120. false green ]);
    (2300., fun c -> [ thug (c + 600.) 100. true orange ]) ]

let new_game () : game =
  { player = new_player (); thugs = []; cam = 500.; wave = 0; barrel = Some (1500., 170.); food = None; lives = 3; score = 0; last_hit = None; sparks = []; seed = 3; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Fighting *)
(*****************************************************************************)

let height_of (f : fighter) : number = if f.big then 220. else 170.
let same_line (a : fighter) (b : fighter) : bool = Float.abs (a.z - b.z) < 18.

let hurtbox (f : fighter) : Hitbox.box = { x = f.x; y = f.y + (height_of f / 2.); w = 60.; h = height_of f }

(* invincible: knocked down (flying, lying, getting up), spinning *)
let invincible (f : fighter) : bool = match f.state with Knocked _ | Attacking (Spin, _, _) -> true | _ -> false

(* [blow a d]: [a]'s move, active and not yet landed, on [d] on its
 * line: hit, stunned or knocked down; the spin hits both sides (and
 * everyone around in the frame it lands) *)
let blow (a : fighter) (d : fighter) : fighter option =
  match a.state with
  | Attacking (att, n, false) when Frame_data.phase (move_of att) n = Frame_data.Active && same_line a d && not (invincible d) ->
      let m = move_of att in
      let hb = Hitbox.place a.facing (a.x, a.y) m.hitbox in
      if Hitbox.overlap hb (hurtbox d) then
        let hp = max 0 (d.hp -.. m.damage) in
        let side = if d.x > a.x then 1. else -1. in
        Some { d with hp; state = (if knocks_down att || hp = 0 then Knocked 0 else Hit m.hitstun); facing = -.side; vy = (if knocks_down att || hp = 0 then 8. else d.vy) }
      else None
  | _ -> None

(* one frame of a fighter's own state: a move's frames, the stun, the
 * flight back and the getting up *)
let step_state (f : fighter) : fighter =
  match f.state with
  | Attacking (Jump_kick, n, h) ->
      let vy = f.vy - 0.9 in
      let y = f.y + vy in
      if y <= 0. then { f with y = 0.; vy = 0.; state = Idle } else { f with x = f.x + (4. * f.facing); y; vy; state = Attacking (Jump_kick, n +.. 1, h) }
  | Attacking (a, n, h) -> if n >= Frame_data.length (move_of a) then { f with state = Idle } else { f with state = Attacking (a, n +.. 1, h) }
  | Jump vx ->
      let vy = f.vy - 0.9 in
      let y = f.y + vy in
      if y <= 0. then { f with y = 0.; vy = 0.; state = Idle } else { f with x = f.x + vx; y; vy }
  | Hit n -> { f with state = (if n <= 1 then Idle else Hit (n -.. 1)) }
  | Knocked n ->
      (* flying back 20 frames, lying 50, getting up 20 *)
      let y, vy = if n < 20 then (Float.max 0. (f.y + f.vy), f.vy - 0.8) else (0., 0.) in
      let x = if n < 20 then f.x - (5. * f.facing) else f.x in
      if n >= 90 && f.hp > 0 then { f with state = Idle; y = 0. } else { f with x; y; vy; state = Knocked (n +.. 1) }
  | Idle | Walk -> f

(* [chain p punch]: punching again during a punch's recovery, after it
 * hit: the next of the combo, jab, jab, hook *)
let chain (p : fighter) : fighter option =
  match p.state with
  | Attacking (Jab1, n, true) when Frame_data.phase (move_of Jab1) n = Frame_data.Recovery -> Some { p with state = Attacking (Jab2, 1, false) }
  | Attacking (Jab2, n, true) when Frame_data.phase (move_of Jab2) n = Frame_data.Recovery -> Some { p with state = Attacking (Hook, 1, false) }
  | _ -> None

let step_player (keys : keyboard) (punch : bool) (jump : bool) (spin : bool) (lo : number) (hi : number) (p : fighter) : fighter =
  let p = step_state p in
  let p = match chain p with Some p' when punch -> p' | _ -> p in
  match p.state with
  | Idle | Walk ->
      if spin && p.hp > 6 then { p with state = Attacking (Spin, 1, false); hp = p.hp -.. 6 }
      else if jump then { p with state = Jump (4. * to_x keys); vy = 14. }
      else if punch then { p with state = Attacking (Jab1, 1, false) }
      else
        let dx = to_x keys and dz = to_y keys in
        let facing = if dx <> 0. then dx else p.facing in
        { p with x = clamp lo hi (p.x + (4. * dx)); z = clamp 0. depth_max (p.z + (3. * dz)); facing; state = (if dx <> 0. || dz <> 0. then Walk else Idle) }
  | Jump vx when punch -> { p with state = Attacking (Jump_kick, 1, false); facing = (if vx > 0. then 1. else if vx < 0. then -1. else p.facing) }
  | _ -> p

(* a thug: to the player's line, at arm's length on its own side, and a
 * punch; then it waits a while (drawn from the seed) *)
let step_thug (p : fighter) (seed : int) (t : fighter) : fighter =
  let t = step_state t in
  match t.state with
  | Idle | Walk ->
      let side = if t.x < p.x then -1. else 1. in
      let reach = if t.big then 110. else 80. in
      let tx = p.x + (side * reach) and tz = p.z in
      let t = { t with facing = -.side; wait = max 0 (t.wait -.. 1) } in
      if Float.abs (t.x - tx) < 8. && Float.abs (t.z - tz) < 6. then
        if t.wait = 0 && not (invincible p) then { t with state = Attacking ((if t.big then Boss_punch else Thug_punch), 1, false); wait = 40 +.. (seed mod 50) } else { t with state = Idle }
      else
        let speed = if t.big then 1.6 else 2.2 in
        let dx = clamp (-.speed) speed (tx - t.x) and dz = clamp (-1.5) 1.5 (tz - t.z) in
        { t with x = t.x + dx; z = t.z + dz; state = Walk }
  | _ -> t

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_street (computer : computer) (scenes : model) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; sparks = List.filter_map (fun (x, y, n) -> if n < 10 then Some (x, y, n +.. 1) else None) g.sparks } in
  let seed = ((g.seed *.. 1103515245) +.. 12345) land 0x7fffffff in
  let g = { g with seed } in
  let pressed key = Scene2d.pressed key scenes in
  (* a wave, when the camera reaches it; the screen locked while its
   * thugs stand *)
  let g =
    match List.nth_opt waves g.wave with
    | Some (at, make) when g.cam >= at && g.thugs = [] -> { g with thugs = make g.cam; wave = g.wave +.. 1 }
    | _ -> g
  in
  let locked = g.thugs <> [] in
  let lo = g.cam - 470. and hi = if locked then g.cam + 470. else street_end in
  let p = step_player computer.keyboard (pressed (fun k -> k.kspace)) (pressed (fun k -> Set_.mem "x" k.keys)) (pressed (fun k -> Set_.mem "z" k.keys)) lo hi g.player in
  let thugs = List.map (step_thug p (seed /.. 65536)) g.thugs in
  (* the player's blows on the thugs, and theirs on the player *)
  let hit_any = ref false in
  let thugs =
    List.map
      (fun t ->
        match blow p t with
        | Some t' -> hit_any := true; t'
        | None -> t)
      thugs
  in
  let p = if !hit_any then (match p.state with Attacking (a, n, _) -> { p with state = Attacking (a, n, true) } | _ -> p) else p in
  let p = List.fold_left (fun p t -> match blow t p with Some p' -> p' | None -> p) p thugs in
  let last_hit = if !hit_any then List.find_opt (fun t -> match t.state with Hit _ | Knocked 0 -> true | _ -> false) thugs else g.last_hit in
  if !hit_any then Audio.play Audio.hit;
  let sparks = if !hit_any then (p.x + (p.facing * 60.), floor_y + p.z + 120., 0) :: g.sparks else g.sparks in
  (* the barrel: a blow breaks it, a roast chicken inside *)
  let barrel, food =
    match g.barrel with
    | Some (bx, bz) when (match p.state with Attacking (_, _, _) -> true | _ -> false) && Float.abs (bx - (p.x + (p.facing * 50.))) < 40. && Float.abs (bz - p.z) < 20. ->
        Audio.play Audio.hit; (None, Some (bx, bz))
    | b -> (b, g.food)
  in
  let p, food = match food with Some (fx, fz) when Float.abs (fx - p.x) < 30. && Float.abs (fz - p.z) < 20. -> (Audio.play Audio.coin; ({ p with hp = min p.max_hp (p.hp +.. 40) }, None)) | f -> (p, f) in
  (* the knocked out thugs vanish; points *)
  let gone = List.filter (fun t -> t.hp = 0 && (match t.state with Knocked n -> n > 110 | _ -> false)) thugs in
  let thugs = List.filter (fun t -> not (List.memq t gone)) thugs in
  let score = g.score +.. List.fold_left (fun s t -> s +.. if t.big then 5000 else 500) 0 gone in
  (* the player down for good: a life *)
  let p, lives = if p.hp = 0 && (match p.state with Knocked n -> n > 100 | _ -> false) then ({ (new_player ()) with x = p.x; z = p.z }, g.lives -.. 1) else (p, g.lives) in
  (* the camera: following the player, to the right only, not past a wave
   * still standing *)
  let cam = if locked then g.cam else Float.min (street_end - 500.) (Float.max g.cam (p.x - 100.)) in
  { g with player = p; thugs; barrel; food; score; lives; cam; last_hit; sparks }

let cleared (g : game) : bool = g.wave = List.length waves && g.thugs = []

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Street (new_game ())) s else s
  | Street g ->
      let g = update_street computer s g in
      if cleared g then Scene2d.go (Cleared g.score) s else if g.lives = 0 then Scene2d.go (Game_over g.score) s else { s with scene = Street g }
  | Cleared _ | Game_over _ -> if space && s.elapsed > 1. then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let guard : Stickman.pose = { lean = 5.; front_arm = (40., 150.); back_arm = (25., 150.); front_leg = (18., 5.); back_leg = (-18., -5.) }
let walk2 : Stickman.pose = { guard with front_leg = (-5., -12.); back_leg = (8., 15.) }
let jab : Stickman.pose = { guard with front_arm = (90., 90.); lean = 12. }
let hook : Stickman.pose = { guard with front_arm = (100., 130.); lean = 25. }
let jump : Stickman.pose = { lean = 10.; front_arm = (60., 160.); back_arm = (30., 160.); front_leg = (80., -20.); back_leg = (20., -60.) }
let jump_kick : Stickman.pose = { jump with front_leg = (75., 95.) }
let spin : Stickman.pose = { Stickman.stand with front_arm = (90., 90.); back_arm = (-90., -90.); front_leg = (10., 0.); back_leg = (-10., 0.) }
let hurt : Stickman.pose = { guard with lean = -25.; front_arm = (-30., -50.); back_arm = (-40., -60.) }

let pose_of (f : fighter) (frames : int) : Stickman.pose =
  let attack a n target base =
    let m = move_of a in
    Stickman.at [ (0, base); (m.startup, target); (m.startup +.. m.active, target); (Frame_data.length m, base) ] n
  in
  match f.state with
  | Idle -> guard
  | Walk -> if frames /.. 8 mod 2 = 0 then guard else walk2
  | Jump _ -> jump
  | Attacking (Jump_kick, n, _) -> attack Jump_kick n jump_kick jump
  | Attacking (((Jab1 | Jab2 | Thug_punch) as a), n, _) -> attack a n jab guard
  | Attacking (Spin, _, _) -> spin
  | Attacking (a, n, _) -> attack a n hook guard
  | Hit _ -> hurt
  | Knocked _ -> hurt

let view_fighter (frames : int) (f : fighter) : shape list =
  let h = height_of f in
  let back = rgb 90 90 90 in
  (* spinning: turned around every 3 frames *)
  let facing = match f.state with Attacking (Spin, n, _) when n /.. 3 mod 2 = 1 -> -.f.facing | _ -> f.facing in
  let figure = Stickman.draw f.color back h facing (pose_of f frames) in
  let figure =
    match f.state with
    | Knocked n when n >= 20 && n < 70 -> figure |> rotate (90. * f.facing) |> move (-.f.facing * 30.) (-50.)
    | _ -> figure
  in
  let blink = f.hp = 0 && (match f.state with Knocked n -> n > 70 && n mod 8 < 4 | _ -> false) in
  let feet_y = floor_y + f.z in
  [ oval (rgb 40 40 50) 70. 16. |> fade 0.5 |> move f.x feet_y ] @ if blink then [] else [ figure |> move f.x (feet_y + f.y) ]

let bar (x : number) (y : number) (w : number) (f : fighter) (label : string) : shape list =
  let fw = w * float_of_int f.hp / float_of_int f.max_hp in
  [ text white 2.2 label |> move (x + 30.) (y + 26.); rectangle (rgb 120 20 20) w 18. |> move (x + (w / 2.)) y; rectangle (rgb 240 200 40) fw 18. |> move (x + (fw / 2.)) y ]

let view_game (g : game) : shape list =
  let p = g.player in
  (* the street: buildings far, the sidewalk, the road's depth lines *)
  let buildings =
    List.init 14 (fun i ->
        let x = (float_of_int i * 260.) - ((g.cam - 500.) * 0.5) - 500. in
        let h = 300. + float_of_int ((i *.. 97) mod 200) in
        group [ rectangle (rgb (70 +.. (i *.. 13 mod 40)) 60 90) 240. h; rectangle (rgb 230 210 120) 30. 40. |> move (-60.) 40.; rectangle (rgb 230 210 120) 30. 40. |> move 50. (-40.) ]
        |> move x (floor_y + depth_max + (h / 2.)))
  in
  let characters = List.sort (fun (a : fighter) b -> compare b.z a.z) (p :: g.thugs) in
  let things =
    (match g.barrel with Some (bx, bz) -> [ (bz, group [ rectangle (rgb 150 90 40) 50. 70.; rectangle (rgb 90 50 20) 50. 6. |> move_y 20.; rectangle (rgb 90 50 20) 50. 6. |> move_y (-20.) ] |> move bx (floor_y + bz + 35.)) ] | None -> [])
    @ match g.food with Some (fx, fz) -> [ (fz, group [ oval (rgb 200 120 40) 50. 30.; rectangle (rgb 250 240 220) 16. 8. |> move 26. 4. ] |> move fx (floor_y + fz + 15.)) ] | None -> []
  in
  (* everyone and everything by depth, the back of the street first *)
  let scene =
    List.map (fun (z, s) -> (z, [ s ])) things @ List.map (fun f -> (f.z, view_fighter g.frames f)) characters
    |> List.sort (fun (a, _) (b, _) -> compare b a)
    |> List.concat_map snd
  in
  let world =
    [ rectangle (rgb 110 110 120) 6000. (depth_max + 40.) |> move 2500. (floor_y + (depth_max / 2.)); rectangle (rgb 150 150 160) 6000. 40. |> move 2500. (floor_y + depth_max + 20.) ]
    @ List.init 30 (fun i -> rectangle (rgb 90 90 100) 60. 4. |> move (float_of_int i * 200.) (floor_y + 60.))
    @ scene
    @ List.map (fun (x, y, n) -> group (List.init 6 (fun i -> rectangle (if n mod 4 < 2 then yellow else white) (12. + (float_of_int n * 3.)) 4. |> rotate (float_of_int i * 30.))) |> move x y) g.sparks
  in
  let go = g.thugs = [] && g.wave < List.length waves && g.frames /.. 20 mod 2 = 0 in
  buildings
  @ [ Camera2d.view (Camera2d.origin |> Camera2d.look_at g.cam 0.) world ]
  @ bar (-460.) 430. 300. p "PLAYER"
  @ [ text white 2.2 (Printf.sprintf "LIVES %d   SCORE %d" g.lives g.score) |> move (-320.) 380. ]
  @ (match g.last_hit with Some t when List.exists (fun t' -> t'.x = t.x && t'.z = t.z) g.thugs -> bar 140. 430. 300. t (if t.big then "BOSS" else "THUG") | _ -> [])
  @ if go then [ text (rgb 240 200 40) 5. "GO ->" |> move 330. 200. ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 40 30 60) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_game ())
      @ [ rectangle black 820. 300. |> fade 0.85 |> move_y 60.; text (rgb 240 200 40) 7. "TINY FINAL FIGHT" |> move_y 150.;
          text white 2.3 "arrows walk (up and down: the street's depth)" |> move_y 80.;
          text white 2.3 "space punch (three: a combo)   x jump   z spin (costs health)" |> move_y 45. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-20.) ]
  | Street g -> view_game g
  | Cleared score -> [ text (rgb 240 200 40) 6. "STREET CLEARED"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over score -> [ text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
