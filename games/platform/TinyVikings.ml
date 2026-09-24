(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of The Lost Vikings (Silicon & Synapse, Interplay,
 * 1992): three vikings kidnapped by Tomator, an alien collecting
 * specimens for his zoo, and trying to get home through his spaceship.
 * Tab to switch viking, left/right to walk, up/down to climb ladders,
 * and each his own:
 *   - Erik the Swift runs and jumps (up), and breaks walls with his
 *     head (space);
 *   - Baleog the Fierce swings his sword (space) and shoots arrows (x),
 *     which kill guards and press buttons out of reach;
 *   - Olaf the Stout holds his shield in front (guards bounce off it),
 *     or over his head (space): he then falls slowly, and the others can
 *     stand on it.
 * Each has three hearts. All three must reach the exit; lose one, and
 * the level starts again.
 *
 * Silicon & Synapse made it for the Super Nintendo, then the Amiga,
 * DOS and the Mega Drive; the studio became Blizzard Entertainment a
 * couple of years later, and the vikings came back in their sequel
 * (1997) and as cameos in Blizzard's games since. (Names and dates from
 * memory, to check.) Gobliiins (Coktel Vision, 1991) had the idea first
 * -- three goblins, one who fights, one who takes things, one who
 * casts spells -- as a point-and-click adventure; Lemmings (DMA Design,
 * 1991) gave skills to a crowd; Trine (Frozenbyte, 2009) is The Lost
 * Vikings' heir, a wizard, a thief and a knight.
 *
 * The new idea here is the party as the puzzle: three heroes, each with
 * one ability the others lack, and one keyboard. Only the active one
 * moves; the others wait where they were left, and that is the point
 * -- Olaf left with his shield up is a step for Erik, a wall only
 * Erik can open is the way for all three. The level is a chain of
 * obstacles each needing one of them (the headbutt, the sword, an arrow
 * at a button across a chasm, the shield as a step up to a ledge, the
 * shield as a glider down a shaft), in [spaceship]. In the code, the
 * three are one record and a [kind], and what differs is a match on
 * the kind ([speed], [act]): the rest -- walking, climbing, falling,
 * getting hurt -- they share.
 *
 * And the heroes are platforms: Olaf's shield raised is a floor, but
 * one that moves with him and isn't in the map, so [descend] checks it
 * beside the tiles, one pixel at a time like them, and only from above
 * (a viking jumping from under the shield goes through it: a
 * "one-way" platform, like the clouds of Super Mario Bros.).
 *
 * What it uses: the platformer kit (gamekits/platformer/: Tile_move,
 * the vikings and the guards against the hull, one pixel at a time;
 * Ladder, climbing, the top of a ladder a floor), the shoot 'em up
 * kit's Shots (gamekits/shmup/: Baleog's arrows), Tilemap (the
 * spaceship, changed by walls broken, the bridge and the ladder
 * appearing), Camera2d (following the active viking, gliding to the
 * next one at a switch), Sprite (the vikings and the guards), Scene2d.
 * Not Physics: the jump is TinyRick's, and nothing else flies but
 * arrows.
 *
 * Exercises: the vikings' inventories (the original's keys, bombs and
 * food, passed from one to the next when they stand together); guards
 * who shoot, their bolts stopped by Olaf's shield; Erik's run, faster
 * the longer he runs, and the headbutt stunning a guard; more levels,
 * each with the original's four-letter password.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The spaceship *)
(*****************************************************************************)

(* 48 x 16 tiles: # hull, b a wall Erik's head breaks, ^ spikes, _ a
 * bridge not there yet, o the button that brings it (an arrow must hit
 * it: the chasm is in between), h a ladder not there yet, p the lever
 * that brings it, E the exit; 1 2 3 Erik, Baleog, Olaf; G guards.
 * From left to right: the wall (Erik), a guard (Baleog), the chasm (an
 * arrow at the button), the ledge (Erik on Olaf's shield, then the
 * lever: a ladder for the others), another guard, and the shaft down
 * to the exit (Olaf glides; the others take a heart's damage). *)
let spaceship =
  [ "################################################";
    "#       #                                      #";
    "#       #                                      #";
    "#       #                                      #";
    "#       #                                      #";
    "#       #                                      #";
    "#       #                                      #";
    "#       #                                      #";
    "#       #                                      #";
    "#       #                        p    G        #";
    "#       b                     h##########      #";
    "#       b                     h##########      #";
    "#  123  b    G                ho#########      #";
    "##################_____##################      #";
    "##################     ##################   EE #";
    "##################^^^^^#########################" ]

let tile = 50.
let level = Tilemap.of_strings tile spaceship
let solid (c : char) : bool = c = '#' || c = 'b' || c = '=' || c = 'o' || c = 'O'
let is_ladder (c : char) : bool = c = 'H'
let viking_size = (32., 44.)
let guard_size = (32., 44.)
let half_h = snd viking_size / 2.

let places (c : char) : (number * number) list = List.map (fun (col, row) -> Tilemap.center level col row) (Tilemap.find level c)

(* the spaceship to play: the vikings and the guards are not tiles *)
let start_map : Tilemap.t =
  List.fold_left (fun m (col, row) -> Tilemap.set m col row ' ') level (List.concat_map (Tilemap.find level) [ '1'; '2'; '3'; 'G' ])

(* standing on the floor of the tile at (x, y), a body [h] high *)
let on_floor (h : number) ((x, y) : number * number) : number * number = (x, y - (tile / 2.) + (h / 2.))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kind = Erik | Baleog | Olaf

type viking = {
  kind : kind;
  x : number;
  y : number;
  vy : number;
  climbing : bool;
  facing : number; (* 1. right, -1. left *)
  steps : int;
  hearts : int;
  hurt : int; (* frames left not to be hurt again, blinking *)
  acting : int; (* frames left of a headbutt, a swing *)
  shield_up : bool; (* Olaf's *)
  saved : bool; (* through the exit *)
}

type guard = { gx : number; gy : number; dir : number }

type game = {
  map : Tilemap.t;
  vikings : viking list; (* Erik, Baleog, Olaf *)
  active : int;
  guards : guard list;
  arrows : Shots.t list;
  cam : Camera2d.t;
  frames : int;
}

type scene = Title | Playing of game | Lost of game * int | Home
type model = scene Scene2d.t

let name (k : kind) : string = match k with Erik -> "ERIK" | Baleog -> "BALEOG" | Olaf -> "OLAF"

let new_viking (kind : kind) (c : char) : viking =
  let x, y = on_floor (snd viking_size) (List.hd (places c)) in
  { kind; x; y; vy = 0.; climbing = false; facing = 1.; steps = 0; hearts = 3; hurt = 0; acting = 0; shield_up = false; saved = false }

let new_game () : game =
  let vikings = [ new_viking Erik '1'; new_viking Baleog '2'; new_viking Olaf '3' ] in
  let erik = List.hd vikings in
  { map = start_map; vikings; active = 0;
    guards = List.map (fun p -> let gx, gy = on_floor (snd guard_size) p in { gx; gy; dir = 1. }) (places 'G');
    arrows = []; cam = Camera2d.look_at erik.x erik.y Camera2d.origin; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The vikings: what they share *)
(*****************************************************************************)

let jump_speed = 13.5 (* up to about 2 tiles, from the floor; 3 from Olaf's shield *)
let gravity = 0.8
let glide_speed = 2.5 (* Olaf's fall, shield up *)
let fall_hurts = 18. (* landing faster: a heart lost; 4 tiles of fall and more *)
let speed (k : kind) : number = match k with Erik -> 5. | Baleog | Olaf -> 3.5

(* the keys, for the active viking; the others get [idle] *)
type input = { dir : number; climb : number; up : bool; space : bool; fire : bool }

let idle = { dir = 0.; climb = 0.; up = false; space = false; fire = false }

(* Olaf's shield raised: a floor [top] high, from [left] to [right] *)
type shield = { left : number; right : number; top : number }

let shield_of (vikings : viking list) : shield option =
  List.find_map
    (fun v -> if v.kind = Olaf && v.shield_up && not v.saved then Some { left = v.x - 26.; right = v.x + 26.; top = v.y + half_h + 8. } else None)
    vikings

let over (s : shield) (x : number) : bool = x + (fst viking_size / 2.) - 4. > s.left && x - (fst viking_size / 2.) + 4. < s.right

let on_shield (shield : shield option) (v : viking) : bool =
  match shield with Some s -> v.kind <> Olaf && over s v.x && Float.abs (v.y - half_h - s.top) < 0.5 | None -> false

let supported (map : Tilemap.t) (shield : shield option) (v : viking) : bool =
  Tile_move.on_ground solid map viking_size v.x v.y || Ladder.on_top is_ladder map viking_size v.x v.y || on_shield shield v

(* [descend map shield v y vy]: falling by [vy] from [y], one pixel at a
 * time, stopped by the hull, the top of a ladder, and Olaf's shield --
 * the last only when crossed from above: feet above its top before the
 * step, at or below it after (TinyRick's fall, with one more floor) *)
let descend (map : Tilemap.t) (shield : shield option) (v : viking) (vy : number) : number * bool =
  let n = int_of_float (ceil (Float.abs vy)) in
  let rec go i y =
    if i >= n then (y, false)
    else
      let y' = y + (vy / float_of_int n) in
      match shield with
      | Some s when v.kind <> Olaf && over s v.x && y - half_h >= s.top && y' - half_h <= s.top -> (s.top + half_h, true)
      | _ ->
          if Tile_move.hits solid map viking_size v.x y' then (y, true)
          else if Ladder.on_top is_ladder map viking_size v.x y' then (y', true)
          else go (succ i) y'
  in
  go 0 v.y

(* [step_viking map shield input v]: on a ladder, up and down, and off it
 * sideways; otherwise walking, Erik's jump, falling (gliding, Olaf with
 * his shield up), and onto a ladder with up (at one) or down (on its
 * top). A landing too fast costs a heart. *)
let step_viking (map : Tilemap.t) (shield : shield option) (input : input) (v : viking) : viking =
  let facing = if input.dir <> 0. then input.dir else v.facing in
  let steps = if input.dir <> 0. then v.steps +.. 1 else v.steps in
  let reach = Ladder.reach is_ladder map viking_size v.x v.y in
  let top = Ladder.on_top is_ladder map viking_size v.x v.y in
  if v.climbing && input.dir = 0. && reach <> None then
    let x, y = Ladder.climb solid is_ladder map viking_size (v.x, v.y) (3. * input.climb) in
    { v with x; y; vy = 0.; steps = (if input.climb <> 0. then v.steps +.. 1 else v.steps) }
  else if (input.climb > 0. && reach <> None && not top) || (input.climb < 0. && top) then
    { v with climbing = true; x = Option.get reach; vy = 0. }
  else
    let (x, _), _ = Tile_move.move_by solid map viking_size (v.x, v.y) (input.dir * speed v.kind, 0.) in
    let v = { v with x; facing; steps; climbing = false } in
    let on = supported map shield v in
    let vy =
      if v.kind = Erik && input.up && on then jump_speed
      else if on && v.vy <= 0. then 0.
      else
        let vy = Float.max (-20.) (v.vy - gravity) in
        if v.kind = Olaf && v.shield_up then Float.max (-.glide_speed) vy else vy
    in
    if vy > 0. then
      let (_, y), hit = Tile_move.move_by solid map viking_size (x, v.y) (0., vy) in
      { v with y; vy = (if hit then 0. else vy) }
    else
      let y, landed = descend map shield v vy in
      let hard = landed && vy < -.fall_hurts in
      { v with y; vy = (if landed then 0. else vy); hearts = (if hard then v.hearts -.. 1 else v.hearts); hurt = (if hard then 60 else v.hurt) }

(*****************************************************************************)
(* The vikings: what each does *)
(*****************************************************************************)

(* the tile in front of a viking's face *)
let in_front (map : Tilemap.t) (v : viking) : int * int = Tilemap.cell map (v.x + (v.facing * ((fst viking_size / 2.) + 10.))) v.y

(* [act g i input]: space (and x) for viking [i]: Erik's headbutt breaks
 * the wall in front, the whole column of it; Baleog's sword kills the
 * guards in front, his arrow flies; Olaf's shield goes up or down *)
let act (g : game) (i : int) (input : input) : game =
  let v = List.nth g.vikings i in
  let set v' = { g with vikings = List.mapi (fun j w -> if j = i then v' else w) g.vikings } in
  if v.acting > 0 || v.climbing then g
  else
    match v.kind with
    | Erik when input.space ->
        let col, _ = in_front g.map v in
        let wall = List.filter (fun (c, _) -> c = col) (Tilemap.find g.map 'b') in
        let g = set { v with acting = 15 } in
        { g with map = List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') g.map wall }
    | Baleog when input.space ->
        let hit gd = (gd.gx - v.x) * v.facing > 0. && (gd.gx - v.x) * v.facing < 60. && Float.abs (gd.gy - v.y) < 30. in
        let g = set { v with acting = 15 } in
        { g with guards = List.filter (fun gd -> not (hit gd)) g.guards }
    | Baleog when input.fire ->
        let g = set { v with acting = 20 } in
        { g with arrows = Shots.straight (v.x + (v.facing * 20.)) (v.y + 8.) (v.facing * 12.) 0. :: g.arrows }
    | Olaf when input.space -> set { v with shield_up = not v.shield_up }
    | _ -> g

(* an arrow stops in the hull; in the button, it brings the bridge *)
let step_arrows (g : game) : game =
  let arrows = List.map Shots.advance g.arrows in
  let blocked (a : Shots.t) = match Tilemap.tile_at g.map a.x a.y with Some c -> solid c | None -> true in
  let hits_button (a : Shots.t) = Tilemap.tile_at g.map a.x a.y = Some 'o' in
  let map =
    if List.exists hits_button arrows then
      let map = List.fold_left (fun m (c, r) -> Tilemap.set m c r 'O') g.map (Tilemap.find g.map 'o') in
      List.fold_left (fun m (c, r) -> Tilemap.set m c r '=') map (Tilemap.find map '_')
    else g.map
  in
  let shot gd = List.exists (Shots.near 25. (gd.gx, gd.gy)) arrows in
  { g with map; guards = List.filter (fun gd -> not (shot gd)) g.guards;
    arrows = List.filter (fun a -> not (blocked a) && not (List.exists (fun gd -> Shots.near 25. (gd.gx, gd.gy) a) g.guards)) arrows }

(* a viking at the lever brings the ladder; at the exit, he's home *)
let pull_lever (g : game) : game =
  if List.exists (fun v -> (not v.saved) && Tilemap.tile_at g.map v.x v.y = Some 'p') g.vikings then
    { g with map = List.fold_left (fun m (c, r) -> Tilemap.set m c r 'H') g.map (Tilemap.find g.map 'h') }
  else g

let reach_exit (g : game) : game =
  { g with vikings = List.map (fun v -> if v.hearts > 0 && Tilemap.tile_at g.map v.x v.y = Some 'E' then { v with saved = true } else v) g.vikings }

(* the next viking still in the spaceship, after [i] (or [i] itself) *)
let next_viking (g : game) (i : int) : int =
  let n = List.length g.vikings in
  let rec go k = if k > n then i else let j = (i +.. k) mod n in if (List.nth g.vikings j).saved then go (k +.. 1) else j in
  go 1

(*****************************************************************************)
(* The guards *)
(*****************************************************************************)

(* a guard walks, and turns at a wall, at the edge of its floor, and at
 * Olaf's shield held in front of him *)
let step_guard (map : Tilemap.t) (vikings : viking list) (gd : guard) : guard =
  let (x, _), hit = Tile_move.move_by solid map guard_size (gd.gx, gd.gy) (gd.dir * 2., 0.) in
  let edge = not (Tile_move.on_ground solid map guard_size (x + (gd.dir * 16.)) gd.gy) in
  let shield v = v.kind = Olaf && (not v.shield_up) && (not v.saved) && Float.abs (v.y - gd.gy) < 30. && (gd.gx - v.x) * v.facing > 0. && Float.abs (x - v.x) < 38. in
  if hit || edge || List.exists shield vikings then { gd with dir = -.gd.dir } else { gd with gx = x }

(* a guard touching a viking takes a heart, and pushes him back *)
let touched (map : Tilemap.t) (guards : guard list) (v : viking) : viking =
  match List.find_opt (fun gd -> Float.abs (gd.gx - v.x) < 30. && Float.abs (gd.gy - v.y) < 40.) guards with
  | Some gd when v.hurt = 0 && not v.saved ->
      let away = if v.x < gd.gx then -1. else 1. in
      let (x, _), _ = Tile_move.move_by solid map viking_size (v.x, v.y) (away * 40., 0.) in
      { v with x; hearts = v.hearts -.. 1; hurt = 60 }
  | _ -> v

(* the feet in the spikes *)
let on_spikes (map : Tilemap.t) (v : viking) : bool = Tilemap.tile_at map v.x (v.y - half_h + 2.) = Some '^' || v.y < -400.

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed k = Scene2d.pressed k scenes in
  let keys = computer.keyboard in
  let input =
    { dir = to_x keys; climb = to_y keys; up = keys.kup; space = pressed (fun k -> k.kspace); fire = pressed (fun k -> Set_.mem "x" k.keys) }
  in
  let g = { g with frames = g.frames +.. 1 } in
  let g = if pressed (fun k -> Set_.mem "Tab" k.keys) then { g with active = next_viking g g.active } else g in
  let g = act g g.active input in
  (* every viking moves, the ones waiting with no keys: they still fall *)
  let shield = shield_of g.vikings in
  let vikings =
    List.mapi
      (fun i v ->
        if v.saved then v
        else
          let v = step_viking g.map shield (if i = g.active then input else idle) v in
          let v = touched g.map g.guards v in
          let v = if on_spikes g.map v then { v with hearts = 0 } else v in
          { v with hurt = max 0 (v.hurt -.. 1); acting = max 0 (v.acting -.. 1) })
      g.vikings
  in
  let g = { g with vikings; guards = List.map (step_guard g.map vikings) g.guards } in
  let g = g |> step_arrows |> pull_lever |> reach_exit in
  (* the active one home: the next one takes the keys *)
  let g = if (List.nth g.vikings g.active).saved then { g with active = next_viking g g.active } else g in
  let a = List.nth g.vikings g.active in
  { g with cam = Camera2d.follow 0.12 a.x a.y g.cam }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if List.for_all (fun v -> v.saved) g.vikings then Scene2d.go Home s
      else if List.exists (fun v -> v.hearts <= 0) g.vikings then Scene2d.go (Lost (g, 0)) s
      else { s with scene = Playing g }
  | Lost (g, n) -> if n > 60 && space then Scene2d.go (Playing (new_game ())) s else { s with scene = Lost (g, n +.. 1) }
  | Home -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let tile_shape (c : char) : shape =
  let hull = rgb 70 80 100 in
  match c with
  | '#' -> group [ square hull tile; square (rgb 85 96 118) (tile - 6.); circle (rgb 55 62 80) 3. |> move (-17.) 17.; circle (rgb 55 62 80) 3. |> move 17. (-17.) ]
  | 'b' -> group [ square (rgb 150 110 70) tile; rectangle (rgb 90 60 40) tile 3.; rectangle (rgb 90 60 40) 3. (tile / 2.) |> move 0. 12.; rectangle (rgb 90 60 40) 3. (tile / 2.) |> move 12. (-12.) ]
  | 'H' -> group [ rectangle (rgb 200 200 90) 4. tile |> move_x (-14.); rectangle (rgb 200 200 90) 4. tile |> move_x 14.; rectangle (rgb 200 200 90) 28. 4. |> move_y (-12.); rectangle (rgb 200 200 90) 28. 4. |> move_y 12. ]
  | '^' -> group (List.init 3 (fun i -> triangle (rgb 200 200 210) 9. |> move ((float_of_int i * 16.) - 16.) (-18.)))
  | '=' -> group [ rectangle (rgb 120 200 230) tile 12. |> move_y 19.; rectangle (rgb 60 120 160) tile 3. |> move_y 12. ]
  | '_' -> rectangle (rgb 120 200 230) tile 2. |> move_y 19. |> fade 0.25
  | 'o' -> group [ square hull tile; circle (rgb 90 90 90) 14.; circle red 10. ]
  | 'O' -> group [ square hull tile; circle (rgb 90 90 90) 14.; circle green 10. ]
  | 'p' -> group [ rectangle (rgb 90 90 90) 20. 8. |> move_y (-21.); rectangle (rgb 180 180 180) 4. 26. |> rotate 30. |> move (-5.) (-10.); circle red 5. |> move (-11.) 1. ]
  | 'E' -> group [ rectangle (rgb 30 30 50) tile tile; rectangle (rgb 80 255 160) tile 4. |> move_y 23.; words (rgb 80 255 160) "EXIT" |> move_y 12. ]
  | _ -> group []

(* the three vikings, facing right, two frames of a walk; a guard *)
let legs = [ [ "..BB..BB.."; "..BB..BB.."; "..KK..KK.."; ".KKK..KKK." ]; [ "...BBBB..."; "...BBBB..."; "...KKKK..."; "..KKKKK..." ] ]

let viking_rows (k : kind) : string list list =
  let top =
    match k with
    | Erik -> [ "W.HHHHH..W"; "WHHHHHHH.W"; "..SSSS...."; "..SWSSS..."; ".RRSSRR..."; ".RRRRRRR.."; "SCCCCCCCS."; "SCCCCCCCS."; ".CCCCCC..."; ".BBBBBB..." ]
    | Baleog -> [ "W.HHHHH..W"; "WHHHHHHH.W"; "..SSSS...."; "..SWSSS..."; ".YYSSYY..."; ".YYYYYY..."; "SGGGGGGGS."; "SGGGGGGGS."; ".GGGGGG..."; ".BBBBBB..." ]
    | Olaf -> [ "W.HHHHH..W"; "WHHHHHHH.W"; ".SSSSSS..."; ".SSWSSSS.."; "YYYSSYYY.."; "PPPPPPPPP."; "PPPPPPPPPS"; "PPPPPPPPPS"; ".PPPPPPPP."; ".BBBBBBB.." ]
  in
  List.map (fun l -> top @ l) legs

let guard_rows =
  let top = [ "...LLLL..."; "..LLLLLL.."; "..LVLLVL.."; "..LLLLLL.."; "...LLLL..."; "..MMMMMM.."; ".LMMMMMML."; ".LMMMMMML."; "..MMMMMM.." ] in
  [ top @ [ "...M..M..."; "...M..M..."; "..M....M.."; ".MM....MM." ]; top @ [ "...M..M..."; "...M..M..."; "...M..M..."; "..MM..MM.." ] ]

let palette =
  [ ('K', rgb 60 40 20); ('S', rgb 240 190 140); ('W', white); ('H', rgb 170 170 180); ('R', rgb 210 70 30); ('C', rgb 200 50 50);
    ('Y', rgb 240 200 60); ('G', rgb 60 140 60); ('P', rgb 60 90 200); ('B', rgb 110 70 40); ('L', rgb 120 220 90); ('V', black); ('M', rgb 150 60 170) ]

let sprites (rows : string list list) (left : bool) : shape list = List.map (fun r -> Sprite.pixels 3.2 palette (if left then Sprite.flip r else r)) rows

(* what a viking holds: Erik's head down in a headbutt, Baleog's sword
 * (swung) and bow, Olaf's shield *)
let held (v : viking) : shape list =
  match v.kind with
  | Erik -> []
  | Baleog ->
      let swing = if v.acting > 5 then -60. else 30. in
      [ rectangle (rgb 220 220 230) 5. 34. |> move_y 17. |> rotate (swing * v.facing) |> move (v.facing * 16.) 0. ]
  | Olaf ->
      if v.shield_up then [ oval (rgb 200 60 40) 56. 14. |> move_y (half_h + 4.); oval (rgb 240 200 60) 14. 6. |> move_y (half_h + 5.) ]
      else [ oval (rgb 200 60 40) 12. 42. |> move_x (v.facing * 18.); circle (rgb 240 200 60) 4. |> move_x (v.facing * 19.) ]

let view_viking (active : bool) (frames : int) (v : viking) : shape list =
  if v.saved || (v.hurt > 0 && (v.hurt /.. 4) mod 2 = 0) then []
  else
    let body = Sprite.cycle (v.steps /.. 6) (sprites (viking_rows v.kind) (v.facing < 0.)) in
    let body = if v.kind = Erik && v.acting > 0 then body |> rotate (-20. * v.facing) else body in
    let marker = if active then [ triangle yellow 8. |> rotate 180. |> move_y (half_h + 22. + (3. * sin (float_of_int frames / 5.))) ] else [] in
    [ group ((body :: held v) @ marker) |> move v.x v.y ]

(* the portraits, the original's bar at the bottom: a viking each, his
 * hearts, the active one framed *)
let hud (g : game) : shape =
  group
    (List.mapi
       (fun i v ->
         let x = (float_of_int i - 1.) * 260. in
         group
           ([ rectangle (if i = g.active then yellow else rgb 60 60 80) 240. 70.; rectangle (rgb 20 20 40) 232. 62.;
              Sprite.pixels 2.2 palette (List.hd (viking_rows v.kind)) |> move (-85.) 0.; text white 1.8 (name v.kind) |> move (20.) 14. ]
           @ if v.saved then [ text (rgb 80 255 160) 1.8 "HOME" |> move 20. (-14.) ]
             else List.init 3 (fun h -> circle (if h < v.hearts then red else rgb 70 40 40) 8. |> move ((float_of_int h * 24.) - 4.) (-14.)))
         |> move_x x)
       g.vikings)

let help (k : kind) : string =
  match k with
  | Erik -> "ERIK: up jump   space headbutt"
  | Baleog -> "BALEOG: space sword   x arrow"
  | Olaf -> "OLAF: space shield up/front"

let view_game (screen : screen) (g : game) : shape list =
  let world =
    [ Tilemap.view tile_shape g.map ]
    @ List.map (fun gd -> Sprite.cycle (g.frames /.. 10) (sprites guard_rows (gd.dir < 0.)) |> move gd.gx gd.gy) g.guards
    @ List.map (fun (a : Shots.t) -> group [ rectangle (rgb 200 170 110) 24. 3.; triangle (rgb 220 220 230) 5. |> rotate (if a.vx > 0. then -90. else 90.) |> move_x (if a.vx > 0. then 12. else -12.) ] |> move a.x a.y) g.arrows
    @ List.concat (List.mapi (fun i v -> view_viking (i = g.active) g.frames v) g.vikings)
  in
  let cam = Camera2d.clamp screen (Tilemap.bounds level) g.cam in
  [ Camera2d.view cam world; hud g |> move_y (screen.bottom + 50.); text white 2. "Tab switch viking" |> move (screen.left + 120.) (screen.top - 25.);
    text yellow 2. (help (List.nth g.vikings g.active).kind) |> move_y (screen.top - 25.) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 15 15 30) screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      view_game screen (new_game ())
      @ [ rectangle black 820. 260. |> fade 0.85 |> move_y 60.; text (rgb 240 200 60) 7. "TINY VIKINGS" |> move_y 140.;
          text white 2.3 "three vikings, one keyboard: Tab to switch" |> move_y 80.;
          text white 2.3 "all three to the exit!" |> move_y 45. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-10.) ]
  | Playing g -> view_game screen g
  | Lost (g, n) ->
      let v = List.find (fun v -> v.hearts <= 0) g.vikings in
      view_game screen g
      @ [ rectangle black 700. 160. |> fade 0.85; text red 5. (name v.kind ^ " IS LOST") |> move_y 30. ]
      @ if n > 60 then Scene2d.blink 1. s [ text yellow 2.5 "PRESS SPACE TO TRY AGAIN" |> move_y (-40.) ] else []
  | Home -> [ text (rgb 240 200 60) 6. "HOME AGAIN!"; text white 2.5 "Erik, Baleog and Olaf made it out of the spaceship" |> move_y (-80.) ] @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-160.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app