(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of R-Type (Irem, 1987): a horizontal shoot 'em up
 * against the Bydo, with the Force at the front of your ship, a beam to
 * charge, and at the end of the stage a battleship longer than the
 * screen, to fly along and take apart. Arrows to fly; space to fire,
 * held to charge the beam and let go; f to launch the Force, and f
 * again to call it back.
 *
 * games/TinyGradius is its rival, two years older: the power-up bar,
 * the options on the ship's trail. What R-Type added, and what is new
 * here:
 *
 *  - The Force ([step_force]): an orange pod that nothing can destroy,
 *    docked to the front of the ship, or to its back, or sent away and
 *    called back -- a part of yourself you place. Docked in front, it
 *    is a shield, taking the bullets that come at you; sent out, it
 *    flies ahead, then hangs there following your height, grinding
 *    whatever it touches and shooting with you; called back (or simply
 *    flown into), it docks to the side it comes in from, which is how
 *    you get it behind you when the enemies come from behind. A state machine of five states:
 *
 *            f                       far enough
 *     Front ----> Flying (away) -------------> Loose
 *     Back  ----/        |                      |  f
 *       ^                | f                    v
 *       +-- docks, on the side it comes from -- Returning
 *
 *    Park it on a turret and walk away: R-Type's own trick, and the
 *    reason the Force is a strategy and not a power-up.
 *
 *  - The beam ([release]): a tap fires; hold instead, and the Wave
 *    Cannon charges, a bar filling under the screen; let go, and a beam
 *    as thick as the charge goes through everything in a row. While it
 *    charges, you are not shooting: every charge is a bet.
 *
 *  - The level as the enemy: the stage ends with a battleship typed as
 *    text ([battleship]), longer than the screen, its hull a Tilemap
 *    like TinyGradius's cave -- but this cave shoots back. Its turrets
 *    ('t') are tiles that a hit turns to empty space, its core ('C')
 *    takes 30 hits; the camera stops over its stern, and the stage is
 *    won when the core goes.
 *
 * What it uses: the shoot 'em up kit (gamekits/shmup/: Path for the waves'
 * flights, Shots for every shot and bullet -- its fourth game after
 * games/TinyInvaders, games/TinyGalaga and games/TinyGradius), Tilemap
 * (the battleship), Camera2d (the scrolling), Sprite (the ship),
 * Scene2d, Audio. Not TinyGradius's power-up bar: R-Type's power-ups
 * are the Force and the lasers it fires, and a toy has room for the
 * Force alone.
 *
 * Exercises: the Force's three lasers (red, blue, yellow crystals, each
 * changing what it fires); the Bits, the two small orbs above and below
 * the ship; R-Type Delta's Force absorbing bullets to charge a bomb;
 * the battleship's own movement, rising and falling as it passes; the
 * other stages' Bydo, Dobkeratops first.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The stage, and its battleship *)
(*****************************************************************************)

(* The battleship, a row per line: '#' and '=' its hull (the pipes are
 * only drawn differently), 't' its turrets, 'C' its core, sitting on
 * the deck at the stern where a ship flying level with it can shoot
 * it. *)
let battleship =
  [ "                                    CC     ";
    "        t    ####       t        t  CC     ";
    "  ###########################################";
    " ##==####==####==####==####==####==#########";
    "#############################################";
    " ##==####==####==####==####==####==#########";
    "  ###########################################";
    "        t          t          t        t    " ]

let tile = 40.
let rows = 18
let empty_cols = 70 (* the open space before it, where the waves come *)
let after_cols = 12

(* the stage: open space, then the battleship across rows 5 to 12, then
 * a little more space to stop the camera in *)
let stage : Tilemap.t =
  let width = empty_cols +.. String.length (List.nth battleship 2) +.. after_cols in
  let pad s = String.make empty_cols ' ' ^ s ^ String.make (width -.. empty_cols -.. String.length s) ' ' in
  Tilemap.of_strings tile
    (List.init rows (fun r ->
         if r >= 5 && r < 5 +.. List.length battleship then pad (List.nth battleship (r -.. 5)) else String.make width ' '))

let bounds = Tilemap.bounds stage
let solid (c : char) : bool = c = '#' || c = '=' || c = 't' || c = 'C'
let start_cam = bounds.left + 500.
let last_cam = bounds.right - 500.
let scroll = 2.
let core_hits = 30

(*****************************************************************************)
(* The waves *)
(*****************************************************************************)

(* the Bydo's flights, in the screen's coordinates as in TinyGradius:
 * the pata-patas' sine, from the right; the chasers come from behind,
 * where only a Force docked at the back defends you; the brutes, slow,
 * armoured, for the beam *)
type kind = Pata | Chaser | Brute

let sine (y : number) : Path.t = Path.make (List.init 12 (fun i -> (560. - (float_of_int i * 100.), y + if i mod 2 = 0 then 70. else -70.)))
let from_behind (y : number) : Path.t = Path.make [ (-560., y); (-200., y + 40.); (200., y - 40.); (560., y) ]
let straight (y : number) : Path.t = Path.make [ (560., y); (-560., y) ]

let hp_of = function Pata -> 1 | Chaser -> 2 | Brute -> 12

(* the timeline: at this frame, these enemies (path, kind, how many,
 * frames between two) -- all of it before the battleship comes into
 * view, a little after frame 900 *)
let waves : (int * (Path.t * kind * int * int)) list =
  List.concat
    (List.init 6 (fun i ->
         let t = 100 +.. (i *.. 130) in
         match i mod 3 with
         | 0 -> [ (t, (sine 180., Pata, 5, 14)); (t +.. 40, (sine (-180.), Pata, 5, 14)) ]
         | 1 -> [ (t, (from_behind (-60.), Chaser, 3, 30)) ]
         | _ -> [ (t, (straight 40., Brute, 1, 0)) ]))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type enemy = { id : int; path : Path.t; s : number; wait : int; kind : kind; x : number; y : number; hp : int }

(* a pellet, or a beam of a charge level (1 to 3) remembering who it has
 * already gone through *)
type shot_kind = Pellet | Beam of int * int list
type shot = { shot : Shots.t; kind : shot_kind }

type force = Front | Back | Flying of number (* its speed along x *) | Loose | Returning

type game = {
  cam : number;
  sx : number; (* the ship *)
  sy : number;
  dead : int; (* frames since it blew up, 0 if flying *)
  force : force;
  fx : number; (* the Force, wherever it is *)
  fy : number;
  charge : int; (* frames the fire button has been held *)
  shots : shot list;
  enemies : enemy list;
  next_id : int;
  bullets : Shots.t list;
  ship : Tilemap.t; (* the stage, its turrets gone as they are shot *)
  core : int; (* hits left *)
  explosions : (number * number * int) list;
  score : int;
  lives : int;
  frames : int;
  won : int; (* frames since the core blew, 0 before *)
}

type scene = Title | Playing of game | Clear of int | Game_over of int
type model = scene Scene2d.t

let new_game () : game =
  let sx = start_cam - 300. in
  { cam = start_cam; sx; sy = 0.; dead = 0; force = Front; fx = sx + 30.; fy = 0.; charge = 0; shots = []; enemies = []; next_id = 0;
    bullets = []; ship = stage; core = core_hits; explosions = []; score = 0; lives = 3; frames = 0; won = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The Force *)
(*****************************************************************************)

(* the hands on a frame, apart from the keyboard so the tests can fly *)
type hands = { dx : number; dy : number; fire : bool (* pressed now *); held : bool; released : bool; send : bool }

let no_hands = { dx = 0.; dy = 0.; fire = false; held = false; released = false; send = false }

let docked (f : force) : bool = f = Front || f = Back

(* One frame of the Force. Docked, it sits against the ship; sent, it
 * flies away and slows into hanging there, following the ship's height;
 * called back, it comes home and docks on the side it arrives from. *)
let step_force (h : hands) (g : game) : game =
  let away = 380. in
  match g.force with
  | Front | Back ->
      if h.send then { g with force = Flying (if g.force = Front then 14. else -14.) }
      else { g with fx = (g.sx + if g.force = Front then 30. else -30.); fy = g.sy }
  | Flying vx ->
      let fx = g.fx + vx + scroll in
      let far = Float.abs (fx - g.sx) > away || Float.abs (fx - g.cam) > 450. in
      if h.send then { g with fx; force = Returning } else { g with fx; force = (if far then Loose else Flying vx) }
  | Loose ->
      (* it keeps its place on the screen, and drifts to the ship's height *)
      let fy = g.fy + clamp (-3.) 3. (g.sy - g.fy) in
      let g = { g with fx = g.fx + scroll; fy } in
      if h.send then { g with force = Returning }
      else if Float.hypot (g.fx - g.sx) (g.fy - g.sy) < 30. then { g with force = (if g.fx >= g.sx then Front else Back) }
      else g
  | Returning ->
      let dx = g.sx - g.fx and dy = g.sy - g.fy in
      let d = Float.hypot dx dy in
      if d < 24. then { g with force = (if g.fx >= g.sx then Front else Back) }
      else { g with fx = g.fx + scroll + (10. * dx / d); fy = g.fy + (10. * dy / d) }

(* what the Force fires when the ship does: docked in front, two shots
 * slanting forward; behind, one backward; loose, one ahead *)
let force_volley (g : game) : shot list =
  let pellet x y vx vy = { shot = Shots.straight x y vx vy; kind = Pellet } in
  match g.force with
  | Front -> [ pellet g.fx (g.fy + 6.) 13. 3.; pellet g.fx (g.fy - 6.) 13. (-3.) ]
  | Back -> [ pellet g.fx g.fy (-13.) 0. ]
  | Loose | Flying _ -> [ pellet (g.fx + 16.) g.fy 13. 0. ]
  | Returning -> []

(*****************************************************************************)
(* The ship and its beam *)
(*****************************************************************************)

(* The fire button: a press fires a pellet (and the Force with it);
 * held, it charges; let go after a third of a second, a beam, level 1
 * to 3 by how long it was held *)
let beam_after = 20
let charge_max = 90

let beam_level (charge : int) : int = if charge < beam_after then 0 else min 3 (1 +.. ((charge -.. beam_after) /.. 30))

let release (h : hands) (g : game) : game =
  let g = if h.fire then (Audio.play Audio.laser; { g with shots = ({ shot = Shots.straight (g.sx + 30.) g.sy 16. 0.; kind = Pellet } :: force_volley g) @ g.shots }) else g in
  if h.held then { g with charge = min charge_max (g.charge +.. 1) }
  else if h.released && beam_level g.charge > 0 then begin
    Audio.play Audio.explosion;
    { g with charge = 0; shots = { shot = Shots.straight (g.sx + 40.) g.sy 18. 0.; kind = Beam (beam_level g.charge, []) } :: g.shots }
  end
  else { g with charge = 0 }

let clamp_ship (g : game) (x : number) (y : number) : number * number =
  (clamp (g.cam - 470.) (g.cam + 470.) x, clamp (bounds.bottom + 20.) (bounds.top - 20.) y)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let blow (x : number) (y : number) (g : game) : game = { g with explosions = (x, y, 0) :: g.explosions }

let spawn (g : game) : game =
  List.fold_left
    (fun g (t, (path, kind, n, gap)) ->
      if t <> g.frames then g
      else
        { g with
          enemies = g.enemies @ List.init n (fun i -> { id = g.next_id +.. i; path; s = 0.; wait = i *.. gap; kind; x = 0.; y = 0.; hp = hp_of kind });
          next_id = g.next_id +.. n })
    g waves

let fly (g : game) (e : enemy) : enemy option =
  if e.wait > 0 then Some { e with wait = e.wait -.. 1 }
  else if e.s > Path.length e.path then None
  else
    let (px, py), _ = Path.at e.path e.s in
    Some { e with s = e.s + (match e.kind with Brute -> 2. | Chaser -> 6. | Pata -> 4.); x = g.cam + px; y = py }

let visible (e : enemy) : bool = e.wait = 0 && e.s > 0.

let damage (s : shot) : int = match s.kind with Pellet -> 1 | Beam (n, _) -> 4 *.. n
let beam_height (s : shot) : number = match s.kind with Pellet -> 4. | Beam (n, _) -> 14. * float_of_int n

(* a shot reaching (x, y): a beam by its whole height *)
let reaches (s : shot) ((x, y) : number * number) (r : number) : bool =
  Float.abs (s.shot.x - x) < r && Float.abs (s.shot.y - y) < r + (beam_height s / 2.)

(* The shots: into the enemies (a pellet stops, a beam goes through,
 * each enemy once), into the battleship (a turret goes, the core takes
 * the damage, the hull stops everything) *)
let shoot (g : game) : game =
  List.fold_left
    (fun g (s : shot) ->
      let pierced = match s.kind with Beam (_, ids) -> ids | Pellet -> [] in
      match List.find_opt (fun (e : enemy) -> visible e && (not (List.mem e.id pierced)) && reaches s (e.x, e.y) 24.) g.enemies with
      | Some e ->
          Audio.play Audio.hit;
          let hp = e.hp -.. damage s in
          let g =
            if hp <= 0 then blow e.x e.y { g with enemies = List.filter (fun (e' : enemy) -> e'.id <> e.id) g.enemies; score = g.score +.. (100 *.. hp_of e.kind) }
            else { g with enemies = List.map (fun (e' : enemy) -> if e'.id = e.id then { e' with hp } else e') g.enemies }
          in
          (match s.kind with Beam (n, ids) -> { g with shots = { s with kind = Beam (n, e.id :: ids) } :: g.shots } | Pellet -> g)
      | None -> (
          let col, row = Tilemap.cell g.ship s.shot.x s.shot.y in
          match Tilemap.get g.ship col row with
          | Some 't' ->
              Audio.play Audio.hit;
              let x, y = Tilemap.center g.ship col row in
              blow x y { g with ship = Tilemap.set g.ship col row ' '; score = g.score +.. 300 }
          | Some 'C' ->
              Audio.play Audio.hit;
              { g with core = max 0 (g.core -.. damage s) }
          | Some c when solid c -> g
          | _ -> { g with shots = s :: g.shots }))
    { g with shots = [] } g.shots

(* the Force grinding what it touches, every 8 frames: the enemies, a
 * turret, the core *)
let grind (g : game) : game =
  if g.frames mod 8 <> 0 || g.force = Returning then g
  else
    let touched (e : enemy) = visible e && Float.hypot (e.x - g.fx) (e.y - g.fy) < 34. in
    let hit, rest = List.partition touched g.enemies in
    let hurt = List.map (fun (e : enemy) -> { e with hp = e.hp -.. 1 }) hit in
    let dead = List.filter (fun (e : enemy) -> e.hp <= 0) hurt in
    let g = List.fold_left (fun g (e : enemy) -> blow e.x e.y { g with score = g.score +.. (100 *.. hp_of e.kind) }) g dead in
    let g = { g with enemies = rest @ List.filter (fun (e : enemy) -> e.hp > 0) hurt } in
    let col, row = Tilemap.cell g.ship g.fx g.fy in
    match Tilemap.get g.ship col row with
    | Some 't' -> let x, y = Tilemap.center g.ship col row in blow x y { g with ship = Tilemap.set g.ship col row ' '; score = g.score +.. 300 }
    | Some 'C' -> { g with core = max 0 (g.core -.. 1) }
    | _ -> g

(* The battleship's turrets fire at the ship when they are on the
 * screen, each on its own beat; the brutes too *)
let enemy_fire (g : game) : game =
  let turrets =
    Tilemap.find g.ship 't'
    |> List.filter_map (fun (col, row) ->
           let x, y = Tilemap.center g.ship col row in
           if Float.abs (x - g.cam) < 480. && (g.frames +.. (col *.. 13)) mod 100 = 0 then Some (Shots.aimed 5. (x, y) (g.sx, g.sy)) else None)
  in
  let brutes =
    List.filter_map (fun (e : enemy) -> if e.kind = Brute && visible e && (g.frames +.. e.id) mod 70 = 0 then Some (Shots.aimed 4. (e.x, e.y) (g.sx, g.sy)) else None) g.enemies
  in
  { g with bullets = turrets @ brutes @ g.bullets }

let step (h : hands) (g : game) : game =
  let g = { g with frames = g.frames +.. 1; cam = Float.min last_cam (g.cam + scroll) } in
  let g = { g with explosions = List.filter_map (fun (x, y, n) -> if n < 25 then Some (x, y, n +.. 1) else None) g.explosions } in
  let g =
    if g.dead > 0 then
      if g.dead > 90 && g.lives > 0 then { g with dead = 0; sx = g.cam - 300.; sy = 0.; force = Front; charge = 0 } else { g with dead = g.dead +.. 1 }
    else
      let sx, sy = clamp_ship g (g.sx + scroll + (5. * h.dx)) (g.sy + (5. * h.dy)) in
      release h (step_force h { g with sx; sy })
  in
  let on_screen x y = Float.abs (x - g.cam) < 560. && Float.abs y < 500. in
  let g = { g with shots = List.filter (fun s -> on_screen s.shot.x s.shot.y) (List.map (fun s -> { s with shot = Shots.advance s.shot }) g.shots) } in
  let g = spawn g in
  let g = { g with enemies = List.filter_map (fly g) g.enemies } in
  let g = enemy_fire g in
  let blocked (b : Shots.t) =
    (* the Force takes the bullets that meet it, wherever it is *)
    Float.hypot (b.x - g.fx) (b.y - g.fy) < 22. || (match Tilemap.tile_at g.ship b.x b.y with Some c -> solid c | None -> false)
  in
  let g = { g with bullets = List.filter (fun b -> on_screen b.Shots.x b.y && not (blocked b)) (List.map Shots.advance g.bullets) } in
  let g = g |> shoot |> grind in
  let g = if g.core = 0 && g.won = 0 then (Audio.play Audio.explosion; blow (g.cam + 200.) 0. { g with won = 1; score = g.score +.. 10000 }) else g in
  let g = if g.won > 0 then { g with won = g.won +.. 1 } else g in
  (* the ship hit: a bullet, an enemy, the battleship's hull *)
  if g.dead > 0 || g.won > 0 then g
  else
    let bullet = List.exists (Shots.near 14. (g.sx, g.sy)) g.bullets in
    let rammed = List.exists (fun (e : enemy) -> visible e && Float.hypot (e.x - g.sx) (e.y - g.sy) < 30.) g.enemies in
    let crashed = Tilemap.hits solid g.ship g.sx g.sy 44. 16. in
    if bullet || rammed || crashed then (Audio.play Audio.explosion; blow g.sx g.sy { g with dead = 1; lives = g.lives -.. 1; bullets = []; charge = 0 })
    else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let pressed k = Scene2d.pressed k s in
  match s.scene with
  | Title -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let k = computer.keyboard in
      let h =
        { dx = to_x k; dy = to_y k; fire = pressed (fun k -> k.kspace); held = k.kspace;
          released = (not k.kspace) && s.before.kspace; send = pressed (fun k -> Set_.mem "f" k.keys) }
      in
      let g = step h g in
      if g.won > 150 then Scene2d.go (Clear g.score) s
      else if g.lives = 0 && g.dead > 90 then Scene2d.go (Game_over g.score) s
      else { s with scene = Playing g }
  | Clear _ | Game_over _ -> if pressed (fun k -> k.kspace) then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let ship_rows = [ ".##........"; "####BB....."; "BBBBBBBBWW."; "WWWWWWWWWWW"; "BBBBBBBBWW."; "####BB....."; ".##........" ]
let ship_shape = Sprite.pixels 4. [ ('#', rgb 150 150 170); ('B', rgb 70 110 230); ('W', white) ] ship_rows

(* the Force: an orange orb in a cage, its opening turned the way it fires *)
let force_shape (f : force) : shape =
  let open_to = match f with Back -> 180. | _ -> 0. in
  group
    [ circle (rgb 255 140 30) 16.; circle (rgb 255 220 120) 8.;
      rectangle (rgb 120 80 40) 6. 34. |> move (-12.) 0.; rectangle (rgb 120 80 40) 30. 5. |> move 0. 15.; rectangle (rgb 120 80 40) 30. 5. |> move 0. (-15.) ]
  |> rotate open_to

let stars (g : game) : shape list =
  List.init 90 (fun i ->
      let depth = float_of_int (1 +.. (i mod 3)) in
      let x = Float.rem (float_of_int ((i *.. 137) mod 1000) - ((g.cam - start_cam) * depth / 4.)) 1000. in
      let x = (if x < 0. then x + 1000. else x) - 500. in
      rectangle (rgb 150 150 200) depth depth |> move x (float_of_int ((i *.. 311) mod 900) - 450.))

(* the battleship, its visible tiles *)
let view_ship (g : game) : shape =
  let visible = { Camera2d.left = g.cam - 520.; right = g.cam + 520.; bottom = bounds.bottom; top = bounds.top } in
  Tilemap.view_visible visible
    (function
      | '#' -> square (rgb 110 115 130) tile
      | '=' -> group [ square (rgb 110 115 130) tile; rectangle (rgb 70 75 90) tile 10. ]
      | 't' -> group [ circle (rgb 170 80 60) 14.; rectangle (rgb 170 80 60) 22. 6. |> move (-12.) 0. ]
      | 'C' -> group [ square (rgb 60 30 70) tile; circle (if g.core > 0 then rgb 230 60 200 else rgb 80 60 80) 14. ]
      | _ -> group [])
    g.ship

let view_enemy (g : game) (e : enemy) : shape list =
  if not (visible e) then []
  else
    match e.kind with
    | Pata -> [ group [ oval (rgb 230 120 60) 34. 20.; circle yellow 5. |> move (-8.) 0. ] |> rotate (float_of_int g.frames * 6.) |> move e.x e.y ]
    | Chaser -> [ group [ polygon (rgb 200 80 200) [ (-18., -12.); (18., 0.); (-18., 12.) ]; circle white 4. |> move 6. 0. ] |> move e.x e.y ]
    | Brute ->
        [ group [ oval (rgb 90 150 90) 70. 50.; oval (rgb 60 110 60) 40. 26.; circle (rgb 230 60 60) 7. |> move (-18.) 0. ] |> move e.x e.y ]

let view_shot (s : shot) : shape =
  match s.kind with
  | Pellet -> rectangle white 14. 4. |> rotate (Shots.angle s.shot) |> move s.shot.x s.shot.y
  | Beam (n, _) ->
      group [ oval (rgb 120 220 255) (70. + (20. * float_of_int n)) (beam_height s); oval white 50. (beam_height s / 2.) ] |> move s.shot.x s.shot.y

(* the beam's bar, as in the arcade: BEAM, filling while you hold *)
let view_charge (g : game) : shape list =
  let w = 300. * float_of_int g.charge / float_of_int charge_max in
  [ text white 2.2 "BEAM" |> move (-230.) (-440.); rectangle (rgb 40 50 90) 300. 18. |> move 0. (-440.);
    rectangle (if beam_level g.charge >= 3 then rgb 255 200 60 else rgb 90 200 255) w 18. |> move (-150. + (w / 2.)) (-440.) ]

let view_game (g : game) : shape list =
  let world =
    [ view_ship g ]
    @ List.concat_map (view_enemy g) g.enemies
    @ List.map view_shot g.shots
    @ List.map (fun (b : Shots.t) -> circle (rgb 255 90 160) 5. |> move b.x b.y) g.bullets
    @ (if g.dead = 0 then [ ship_shape |> move g.sx g.sy; force_shape g.force |> move g.fx g.fy ] else [])
    @ List.map (fun (x, y, n) -> circle (if n mod 6 < 3 then orange else yellow) (10. + (float_of_int n * 2.)) |> fade (1. - (float_of_int n / 25.)) |> move x y) g.explosions
  in
  stars g
  @ [ Camera2d.view (Camera2d.origin |> Camera2d.look_at g.cam 0.) world ]
  @ view_charge g
  @ [ text white 2.5 (Printf.sprintf "SCORE %d   SHIPS %d" g.score g.lives) |> move (-300.) 440.;
      text (rgb 160 160 190) 1.8
        (match g.force with
        | Front -> "FORCE: front (f to send it)"
        | Back -> "FORCE: back (f to send it)"
        | Flying _ | Loose -> "FORCE: out (f to call it back)"
        | Returning -> "FORCE: coming back")
      |> move 250. 440. ]
  @ if g.cam >= last_cam && g.core > 0 then [ text (rgb 230 60 200) 2.2 (Printf.sprintf "CORE %d" g.core) |> move 330. (-440.) ] else []

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game (new_game ())
      @ [ rectangle black 780. 280. |> fade 0.85 |> move_y 60.; text (rgb 255 140 30) 7. "TINY R-TYPE" |> move_y 150.;
          text white 2.3 "arrows fly   space fire, hold it to charge the beam" |> move_y 80.;
          text white 2.3 "f sends the Force away, f calls it back" |> move_y 45.;
          text white 2.3 "it docks on the side it comes back to" |> move_y 10. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-40.) ]
  | Playing g -> view_game g
  | Clear score ->
      [ text (rgb 255 140 30) 6. "THE BATTLESHIP IS DOWN"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ]
  | Game_over score ->
      [ text red 6. "GAME OVER"; text white 3. (Printf.sprintf "SCORE %d" score) |> move_y (-80.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
