(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Lemmings (DMA Design, Psygnosis, 1991): little
 * green-haired creatures drop from a hatch and walk, mindlessly, turning
 * at walls, falling into pits, splatting when they fall too far. You
 * can't move them; you give them jobs: a Blocker stands still, arms out,
 * turning the others back; a Builder lays a stair of 12 bricks; a
 * Basher digs through a wall; a Digger digs down. Save enough of them
 * (lead them to the exit) before the time runs out. Pick a job with 1-4
 * (or click its button), then click a lemming; hold f to go faster.
 *
 * Lemmings started as a 8x8-pixels animation that Mike Dailly drew in
 * 1989 to test a sprite editor; David Jones's DMA Design made it the
 * game, and it came out on the Amiga, then on everything. (Names and
 * dates from memory, to check.)
 *
 * What's new here:
 *
 *  - The terrain as a bitmap in the model ([terrain]): 240x150 cells of
 *    4 pixels, dirt, steel or brick, that the jobs modify cell by cell:
 *    digging clears cells, building sets them. Not tiles, not shapes:
 *    the world is pixels (the original's was: the bitmap it drew was
 *    the one the lemmings walked on). One copy per tick ([tick]) keeps
 *    the model a value: a tick's changes don't touch the last frame's.
 *    Drawn as runs, one rectangle per row's stretch of the same cells
 *    ([view_terrain]): a few hundred, not 36,000.
 *
 *  - Characters as tiny state machines, reading the pixels around them
 *    ([walk]: the step up a slope of 3 cells, the wall, the pit): the
 *    whole behavior is in a few cell tests, and the puzzles come from
 *    how they meet the terrain.
 *
 *  - Indirect control: you don't play a character, you change the
 *    rules of one ([assign]); the game is timing and placing those
 *    changes (games/TinyBabaIsYou changes the rules of all of them).
 *
 *  - Levels drawn as ASCII, each character a 5x5 block of cells
 *    ([load]): the levels readable in the source, the digging finer
 *    than the drawing.
 *
 * What it uses: Scene2d, Audio. Not Tilemap (the cells are the world,
 * not a drawing of it), not Physics (the falls are one cell at a time,
 * the splat a count of them). A destructible terrain would be a kit the
 * day a second game needs one: games/TinyWorms' is a height map, no
 * caves; on this bitmap, its explosions could carve them.
 *
 * Exercises: the Climber and the Floater (who don't need the terrain to
 * change, but change how a lemming reads it), the Miner (digging
 * diagonally), the nuke (every lemming exploding, taking the terrain
 * with it: the "Oh no!"), one-way walls, the release rate (+ and -),
 * more levels (the original's 120).
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The terrain *)
(*****************************************************************************)

let cols = 240
let rows = 150
let cell = 4.

(* row by row, one byte per cell: air, dirt, steel, brick *)
type terrain = Bytes.t

let air = 0
let dirt = 1
let steel = 2
let brick = 3

(* beyond the sides, walls; beyond the top and the bottom, air: a lemming
 * falling off the bottom is lost *)
let get (t : terrain) (c : int) (r : int) : int =
  if c < 0 || c >= cols then steel else if r < 0 || r >= rows then air else Bytes.get_uint8 t ((r *.. cols) +.. c)

let solid (t : terrain) (c : int) (r : int) : bool = get t c r <> air

(* digging clears dirt and bricks, not steel; building fills only air *)
let set (t : terrain) (c : int) (r : int) (v : int) : unit =
  let now = get t c r in
  if c >= 0 && c < cols && r >= 0 && r < rows && ((v = air && now <> steel) || (v <> air && now = air)) then Bytes.set_uint8 t ((r *.. cols) +.. c) v

(* the cells' centers on the screen: (0, 0) the terrain's top-left *)
let to_x (c : int) : number = (float_of_int c * cell) - 480. + (cell / 2.)
let to_y (r : int) : number = 300. - (float_of_int r * cell) - (cell / 2.)

(*****************************************************************************)
(* The levels *)
(*****************************************************************************)

(* 48x30 characters, each a 5x5 block of cells: # dirt, = steel, H the
 * hatch, E the exit (standing on the block under it) *)
type level = { name : string; map : string list; out : int; need : int; skills : int list (* block, build, bash, dig *) }

let levels : level list =
  [ { name = "Just dig!"; out = 10; need = 8; skills = [ 1; 0; 0; 2 ];
      map =
        [ "................................................";
          "................................................";
          ".........H......................................";
          "................................................";
          "................................................";
          "................................................";
          "....##......................................##..";
          "....##......................................##..";
          "....##########################################..";
          "....##########################################..";
          "....##########################################..";
          "....##########################################..";
          "....##########################################..";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          ".....................................E..........";
          "################################################";
          "################################################";
          "################################################";
          "################################################";
          "################################################";
          "################################################";
          "################################################";
          "################################################";
          "################################################";
          "################################################" ] };
    { name = "Steel on top"; out = 10; need = 8; skills = [ 1; 0; 2; 1 ];
      map =
        [ "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "......................====......................";
          "......................====......................";
          "......................====......................";
          "......................====......................";
          "......................====......................";
          "......................====......................";
          "....H.................====......................";
          "......................====......................";
          "......................====......................";
          "......................####......................";
          "......................####......................";
          "......................####...............E......";
          "################################################";
          "################################################";
          "################################################";
          "################################################";
          "================================================";
          "================================================";
          "================================================";
          "================================================";
          "================================================";
          "================================================" ] };
    { name = "Mind the gap"; out = 10; need = 6; skills = [ 2; 3; 0; 0 ];
      map =
        [ "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          ".....H..........................................";
          "................................................";
          "................................................";
          "................................................";
          "................................................";
          "........................................E.......";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################";
          "####################....########################" ] } ]

(* the terrain, the hatch and the exit (in cells) of a level *)
let load (l : level) : terrain * (int * int) * (int * int) =
  let t = Bytes.make (cols *.. rows) '\000' in
  let hatch = ref (0, 0) and exit = ref (0, 0) in
  List.iteri
    (fun r line ->
      String.iteri
        (fun c ch ->
          let v = match ch with '#' -> dirt | '=' -> steel | _ -> air in
          for dr = 0 to 4 do
            for dc = 0 to 4 do
              set t ((c *.. 5) +.. dc) ((r *.. 5) +.. dr) v
            done
          done;
          if ch = 'H' then hatch := ((c *.. 5) +.. 2, (r *.. 5) +.. 2);
          if ch = 'E' then exit := ((c *.. 5) +.. 2, (r *.. 5) +.. 4))
        line)
    l.map;
  (t, !hatch, !exit)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type job = Walker | Faller of int (* cells fallen *) | Blocker | Builder of int (* bricks left *) | Basher | Digger | Saved | Dead

(* (x, y): the cell of its feet, the ground being the cell under *)
type lemming = { x : int; y : int; dir : int; job : job }

type game = {
  level : int;
  terrain : terrain;
  hatch : int * int;
  exit : int * int;
  lemmings : lemming list; (* in release order, the saved and the dead included *)
  skills : int list; (* what's left of each job *)
  selected : int; (* the job a click gives *)
  frames : int;
  ticks : int;
}

type scene = Title | Playing of game | Result of game
type model = scene Scene2d.t

let jobs = [ ("BLOCK", Blocker); ("BUILD", Builder 12); ("BASH", Basher); ("DIG", Digger) ]
let splat = 64 (* cells: a fall of 16 lemmings *)
let every = 40 (* frames between two lemmings out of the hatch *)
let time_limit = 60 *.. 150 (* frames: 2 minutes 30 *)

let new_game (level : int) : game =
  let l = List.nth levels level in
  let terrain, hatch, exit = load l in
  { level; terrain; hatch; exit; lemmings = []; skills = l.skills; selected = 0; frames = 0; ticks = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The lemmings *)
(*****************************************************************************)

(* one tick of a walker: down a pit, back from a wall or a blocker, up or
 * down a step of up to 3 cells *)
let walk (t : terrain) (blockers : lemming list) (l : lemming) : lemming =
  if not (solid t l.x (l.y +.. 1)) then { l with job = Faller 0 }
  else
    let nx = l.x +.. l.dir in
    let blocked = List.exists (fun (b : lemming) -> abs (b.x -.. nx) <= 1 && abs (b.y -.. l.y) <= 4 && b != l) blockers in
    (* the lowest free cell of the next column, 3 at most above the feet *)
    let up = List.find_opt (fun k -> not (solid t nx (l.y -.. k))) [ 0; 1; 2; 3 ] in
    match up with
    | _ when blocked -> { l with dir = -l.dir }
    | None -> { l with dir = -l.dir }
    | Some k when k > 0 -> { l with x = nx; y = l.y -.. k }
    | Some _ ->
        (* going down: to the ground, if 3 cells at most below; beyond, the
         * next tick falls *)
        let down = List.find_opt (fun d -> solid t nx (l.y +.. d +.. 1)) [ 0; 1; 2; 3 ] in
        { l with x = nx; y = l.y +.. Option.value down ~default:0 }

(* one tick of a lemming, on a terrain being modified *)
let step (t : terrain) (blockers : lemming list) (ticks : int) (l : lemming) : lemming =
  match l.job with
  | Walker -> walk t blockers l
  | Faller n ->
      (* 2 cells a tick; a fall too long splats *)
      let rec fall l n k =
        if l.y >= rows then { l with job = Dead }
        else if solid t l.x (l.y +.. 1) then { l with job = (if n > splat then Dead else Walker) }
        else if k = 0 then { l with job = Faller n }
        else fall { l with y = l.y +.. 1 } (n +.. 1) (k -.. 1)
      in
      fall l n 2
  | Blocker | Saved | Dead -> l
  | Digger ->
      (* a row of 5 cells every other tick; nothing left under: falling; steel:
       * giving up *)
      let below = List.map (fun dx -> get t (l.x +.. dx) (l.y +.. 1)) [ -2; -1; 0; 1; 2 ] in
      if List.mem steel below then { l with job = Walker }
      else if List.for_all (fun v -> v = air) below then { l with job = Faller 0 }
      else if ticks mod 2 = 1 then l
      else begin
        List.iter (fun dx -> set t (l.x +.. dx) (l.y +.. 1) air) [ -2; -1; 0; 1; 2 ];
        { l with y = l.y +.. 1 }
      end
  | Basher ->
      (* the 5 cells high in front cleared, a column a tick; nothing left in
       * front: walking; steel: turning back *)
      let front ds = List.concat_map (fun d -> List.map (fun dy -> get t (l.x +.. (d *.. l.dir)) (l.y -.. dy)) [ 0; 1; 2; 3; 4 ]) ds in
      if not (solid t l.x (l.y +.. 1)) then { l with job = Faller 0 }
      else if List.mem steel (front [ 1; 2; 3 ]) then { l with job = Walker; dir = -l.dir }
      else if List.for_all (fun v -> v = air) (front [ 1; 2; 3; 4; 5 ]) then { l with job = Walker }
      else begin
        List.iter (fun dy -> set t (l.x +.. l.dir) (l.y -.. dy) air) [ 0; 1; 2; 3; 4 ];
        { l with x = l.x +.. l.dir }
      end
  | Builder n ->
      (* a brick of 4 cells every other tick, one cell up and two ahead of
       * the last: a stair *)
      if ticks mod 2 = 1 then l
      else if solid t (l.x +.. l.dir) (l.y -.. 1) || solid t (l.x +.. (2 *.. l.dir)) (l.y -.. 2) then { l with job = Walker; dir = -l.dir }
      else begin
        List.iter (fun d -> set t (l.x +.. (d *.. l.dir)) l.y brick) [ 1; 2; 3; 4 ];
        let l = { l with x = l.x +.. (2 *.. l.dir); y = l.y -.. 1 } in
        if n = 1 then { l with job = Walker } else { l with job = Builder (n -.. 1) }
      end

let at_exit (g : game) (l : lemming) : bool =
  let ex, ey = g.exit in
  abs (l.x -.. ex) <= 2 && abs (l.y -.. ey) <= 2

let alive (l : lemming) : bool = match l.job with Saved | Dead -> false | _ -> true

(* [tick g]: every lemming one step further, on a copy of the terrain *)
let tick (g : game) : game =
  let t = Bytes.copy g.terrain in
  let blockers = List.filter (fun l -> l.job = Blocker) g.lemmings in
  let lemmings =
    List.map
      (fun l ->
        let l' = step t blockers g.ticks l in
        match l'.job with
        | (Walker | Basher | Digger | Builder _) when at_exit g l' -> Audio.play Audio.coin; { l' with job = Saved }
        | Dead when alive l -> Audio.play Audio.hit; l'
        | _ -> l')
      g.lemmings
  in
  { g with terrain = t; lemmings; ticks = g.ticks +.. 1 }

(* [assign g i k]: the i-th lemming given the k-th job, if one is left
 * and it can take it (not in the air, not a blocker) *)
let assign (g : game) (i : int) (k : int) : game =
  let l = List.nth g.lemmings i and _, job = List.nth jobs k in
  let can = match l.job with Walker | Builder _ | Basher | Digger -> l.job <> job | _ -> false in
  if List.nth g.skills k <= 0 || not can then g
  else
    { g with lemmings = List.mapi (fun j l -> if j = i then { l with job } else l) g.lemmings; skills = List.mapi (fun j n -> if j = k then n -.. 1 else n) g.skills }

(* the lemming under the mouse: the nearest to it among those close enough *)
let under (g : game) (mx : number) (my : number) : int option =
  let d (l : lemming) = Float.hypot (to_x l.x - mx) (to_y (l.y -.. 2) - my) in
  let near = List.filter (fun (_, l) -> alive l && d l < 16.) (List.mapi (fun i l -> (i, l)) g.lemmings) in
  match List.sort (fun (_, a) (_, b) -> compare (d a) (d b)) near with (i, _) :: _ -> Some i | [] -> None

let released (g : game) : int = List.length g.lemmings
let saved (g : game) : int = List.length (List.filter (fun l -> l.job = Saved) g.lemmings)

let over (g : game) : bool =
  let l = List.nth levels g.level in
  g.frames >= time_limit || (released g = l.out && not (List.exists alive g.lemmings))

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let button_x (k : int) : number = -330. + (float_of_int k * 110.)
let button_y = -380.

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let m = computer.mouse in
  let pressed key = Scene2d.pressed (fun kb -> Set_.mem key kb.keys) scenes in
  let g = List.fold_left (fun g k -> if pressed (string_of_int (k +.. 1)) then { g with selected = k } else g) g [ 0; 1; 2; 3 ] in
  let g =
    if not m.mclick then g
    else
      match List.find_opt (fun k -> Float.abs (m.mx - button_x k) < 50. && Float.abs (m.my - button_y) < 40.) [ 0; 1; 2; 3 ] with
      | Some k -> { g with selected = k }
      | None -> ( match under g m.mx m.my with Some i -> assign g i g.selected | None -> g)
  in
  let g = { g with frames = g.frames +.. 1 } in
  (* the hatch: a lemming every [every] frames, after a second *)
  let hx, hy = g.hatch in
  let g =
    if g.frames >= 60 && (g.frames -.. 60) mod every = 0 && released g < (List.nth levels g.level).out then
      { g with lemmings = g.lemmings @ [ { x = hx; y = hy; dir = 1; job = Faller 0 } ] }
    else g
  in
  (* a tick every 3 frames, every frame with f held *)
  if g.frames mod 3 = 0 || Set_.mem "f" computer.keyboard.keys then tick g else g

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game 0)) s else s
  | Playing g ->
      let g = update_game computer s g in
      if over g then Scene2d.go (Result g) s else { s with scene = Playing g }
  | Result g ->
      if not space then s
      else if saved g < (List.nth levels g.level).need then Scene2d.go (Playing (new_game g.level)) s
      else if g.level +.. 1 < List.length levels then Scene2d.go (Playing (new_game (g.level +.. 1))) s
      else Scene2d.go Title s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let color_of (v : int) (r : int) : color =
  if v = steel then rgb 150 150 160 else if v = brick then rgb 230 210 130 else if r mod 5 = 0 then rgb 170 110 50 else rgb 140 85 40

(* each row's runs of the same cells, one rectangle each *)
let view_terrain (t : terrain) : shape list =
  List.concat
    (List.init rows (fun r ->
         let rec runs c acc =
           if c >= cols then acc
           else
             let v = get t c r in
             let rec stop e = if e < cols && get t e r = v then stop (e +.. 1) else e in
             let e = stop c in
             let acc = if v = air then acc else (rectangle (color_of v r) (float_of_int (e -.. c) * cell) cell |> move ((to_x c + to_x (e -.. 1)) / 2.) (to_y r)) :: acc in
             runs e acc
         in
         runs 0 []))

(* a lemming: green hair, blue body, its job in its pose *)
let view_lemming (frames : int) (l : lemming) : shape list =
  let x = to_x l.x and y = to_y l.y - (cell / 2.) (* its feet *) in
  let green = rgb 60 220 60 and blue = rgb 70 90 240 and skin = rgb 250 200 170 in
  let step = if (l.x +.. l.y) mod 2 = 0 then 2. else -2. in
  let body =
    [ rectangle blue 6. 8. |> move x (y + 7.); rectangle skin 5. 4. |> move (x + float_of_int l.dir) (y + 13.); rectangle green 6. 3. |> move (x - float_of_int l.dir) (y + 15.) ]
  in
  match l.job with
  | Saved | Dead -> []
  | Blocker -> body @ [ rectangle skin 18. 2. |> move x (y + 9.); rectangle blue 6. 3. |> move (x - 2.) (y + 1.); rectangle blue 6. 3. |> move (x + 2.) (y + 1.) ]
  | Faller _ -> body @ [ rectangle skin 2. 5. |> move (x - 4.) (y + 13.); rectangle skin 2. 5. |> move (x + 4.) (y + 13.) ]
  | Builder _ -> body @ [ rectangle (rgb 230 210 130) 6. 2. |> move (x + (5. * float_of_int l.dir)) (y + 6.); rectangle blue 2. 3. |> move x (y + 1.) ]
  | Digger -> body @ [ rectangle skin 8. 2. |> move x (y + (if frames mod 10 < 5 then 2. else 5.)) ]
  | Basher -> body @ [ rectangle skin 6. 2. |> move (x + (float_of_int l.dir * 5.)) (y + (if frames mod 10 < 5 then 6. else 10.)) ]
  | Walker -> body @ [ rectangle blue 2. 3. |> move (x - step) (y + 1.); rectangle blue 2. 3. |> move (x + step) (y + 1.) ]

let view_game (computer : computer) (g : game) : shape list =
  let l = List.nth levels g.level in
  let hx, hy = g.hatch and ex, ey = g.exit in
  let left = (time_limit -.. g.frames) /.. 60 in
  let m = computer.mouse in
  [ rectangle (rgb 10 10 30) 960. 600. ]
  @ view_terrain g.terrain
  (* the hatch and the exit *)
  @ [ rectangle (rgb 120 80 40) 40. 14. |> move (to_x hx) (to_y (hy -.. 4)); rectangle black 24. 4. |> move (to_x hx) (to_y (hy -.. 2)) ]
  @ [ polygon (rgb 120 80 40) [ (to_x ex - 18., to_y ey - 2.); (to_x ex + 18., to_y ey - 2.); (to_x ex + 12., to_y ey + 34.); (to_x ex - 12., to_y ey + 34.) ];
      rectangle black 12. 20. |> move (to_x ex) (to_y ey + 8.);
      circle (if g.frames mod 20 < 10 then yellow else orange) 4. |> move (to_x ex - 14.) (to_y ey + 38.);
      circle (if g.frames mod 20 < 10 then orange else yellow) 4. |> move (to_x ex + 14.) (to_y ey + 38.) ]
  @ List.concat_map (view_lemming g.frames) g.lemmings
  (* the lemming a click would pick, framed *)
  @ (match under g m.mx m.my with
    | Some i ->
        let l = List.nth g.lemmings i in
        let x = to_x l.x and y = to_y l.y + 6. in
        [ rectangle white 22. 2. |> move x (y + 11.); rectangle white 22. 2. |> move x (y - 11.); rectangle white 2. 22. |> move (x - 11.) y; rectangle white 2. 22. |> move (x + 11.) y ]
    | None -> [])
  (* the jobs' buttons, the counts *)
  @ List.concat
      (List.mapi
         (fun k (name, _) ->
           let x = button_x k in
           [ rectangle (if k = g.selected then rgb 200 60 60 else rgb 60 60 90) 100. 70. |> move x button_y;
             text white 2. (string_of_int (k +.. 1) ^ " " ^ name) |> move x (button_y + 12.);
             text yellow 2.5 (string_of_int (List.nth g.skills k)) |> move x (button_y - 14.) ])
         jobs)
  @ [ text (rgb 60 220 60) 2.5 (Printf.sprintf "OUT %d  IN %d/%d" (List.length (List.filter alive g.lemmings)) (saved g) l.need) |> move 250. (button_y + 14.);
      text (rgb 60 220 60) 2.5 (Printf.sprintf "TIME %d-%02d" (left /.. 60) (left mod 60)) |> move 250. (button_y - 16.);
      text white 2.5 (Printf.sprintf "LEVEL %d  %s" (g.level +.. 1) l.name) |> move_y 330. ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  rectangle black screen.width screen.height
  ::
  (match s.scene with
  | Title ->
      [ text (rgb 60 220 60) 8. "TINY LEMMINGS" |> move_y 200.;
        text white 2.5 "1-4 pick a job (or click its button), then click a lemming" |> move_y 80.;
        text white 2.5 "save enough of them before the time runs out; f: faster" |> move_y 40. ]
      @ [ group (view_lemming s.frames { x = 120; y = 77; dir = 1; job = Blocker }) |> scale 4. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-250.) ]
  | Playing g -> view_game computer g
  | Result g ->
      let l = List.nth levels g.level in
      let ok = saved g >= l.need in
      [ text (rgb 60 220 60) 5. (Printf.sprintf "YOU SAVED %d OF %d" (saved g) l.out) |> move_y 100.;
        text white 3. (Printf.sprintf "you needed %d" l.need) |> move_y 20.;
        text (if ok then yellow else red) 3.5 (if not ok then "TRY AGAIN" else if g.level +.. 1 < List.length levels then "ON TO THE NEXT LEVEL" else "ALL LEVELS DONE!") |> move_y (-60.) ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-180.) ])

let app = game view update initial_model
let main = Playground_platform.run_app app
