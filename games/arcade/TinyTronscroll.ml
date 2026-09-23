(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of tronscroll -- "tron v0.1", the author's own first
 * network game, written in C with svgalib at INSA Rennes in 1997 (its
 * README opens "excuse me but i am french so i don t speak english very
 * well"). Tron's light cycles (TinyTron.ml), but each player a pixel on
 * a map much bigger than the screen -- 800 x 600 cells here, 1600 x
 * 1200 in the original (map=1600x1200) -- seen through a 320 x 192
 * window that scrolls with it: the scroll the name is about. Each
 * player sees their own window (a split screen on one computer, a
 * screen each on two), and the last one alive scores.
 *
 *   player 1 (blue):   the arrows turn, space uses an option
 *   player 2 (orange): w/a/s/d turn,   q uses an option
 *
 * The options, boxes appearing now and then (6 chances in 1,000 a
 * tick), taken by riding into them, used when you want:
 *
 *   light blue  freeze  the others stop for 200 ticks (and can't die)
 *   dark blue   speed   twice as fast for 200 ticks
 *   red         teleport  somewhere with nobody around
 *   white       invulnerable  through the trails for 400 ticks (not
 *                       through the border)
 *   brown       clear   all your walls gone
 *   green       swap    you become your tail: back to where your trail
 *                       began, going away from it
 *
 * a star under the players' names when yours is about to end. After
 * 2,000 ticks the arena shrinks, a cell every 50 ticks.
 *
 * What it teaches is networking (plan_networking_teaching.md, the
 * milestone of its phase 4), and the three netcodes of
 * Multiplayer.mli, by the flag netcode= or the key n in
 * net=simulate (two computers side by side, latency= and loss=):
 *
 * - netcode=1997: the original's. Each frame, motor.c sent its state
 *   (send_coord, a raw C struct over TCP) then *blocked* until the
 *   answer came (recv_coord): a round trip per frame, invisible on the
 *   school's LAN (under a millisecond), 16 frames a second at 60 ms --
 *   a slide show over the Internet. Here the inputs travel instead of
 *   the state, but the wait is the same: try latency=100;
 * - netcode=lockstep: the same, paid once -- each key applied 3 ticks
 *   after it is pressed, and full speed as long as the network is
 *   faster than that;
 * - netcode=rollback: your keys at once, the other's guessed, the game
 *   replayed when a guess was wrong: full speed, the other rider
 *   snapping now and then.
 *
 * The trick of an immutable world: the trails are a Tilemap, one
 * character a cell (the rider and its generation), which rollback can
 * keep old versions of for free -- a new tick shares all the rows but
 * the ones it wrote. The original erased a player's walls pixel by pixel
 * ("it takes a long time but it is normal"); here clear starts a new
 * generation of its cells, and the old ones stop counting as walls, at
 * once. And each tick is computed in three passes -- every move checked
 * against the map of the tick before, then the cells written, then two
 * riders on one cell both dead -- so the result doesn't depend on the
 * order of the riders (in the original, two heads meeting in a cell
 * passed through each other). The randomness (the boxes, the teleports)
 * comes from a seed in the model (Playground.random), the same on every
 * computer: the original's server drew it alone and sent it.
 *
 * Uses: Multiplayer (the players, the netcodes), Tilemap (the trails),
 * Lightcycles' directions; not the lightcycles kit's rules (two players
 * on a 90-cell arena), nor Scene2d.
 *
 * Left as exercises: up to 8 players, as the original (Multiplayer
 * shares one keyboard between two; more wait for a relay, phase 5); a
 * name and a color chosen by each player; the original's scroll_freq
 * (the window moved every n frames, to spare a 486); a map of the whole
 * arena, which the original didn't have.
 *)
open Playground

(*****************************************************************************)
(* The map *)
(*****************************************************************************)

type dir = Lightcycles.dir = Up | Down | Left | Right

let delta = Lightcycles.delta
let opposite = Lightcycles.opposite

(* the window on the map, in cells, and the size of a cell on the screen *)
let view_cols = 320
let view_rows = 192
let cell = 1000. /. float_of_int view_cols

(* a trail's cell: its rider and its generation ([clear] starts a new
 * one), in one character; ' ' is free *)
let generations = 27
let mark (rider : int) (gen : int) : char = Char.chr (33 + (rider * generations) + (gen mod generations))
let owner (c : char) : int * int = ((Char.code c - 33) / generations, (Char.code c - 33) mod generations)

type kind = Invu | Teleport | Clear | Swap | Freeze | Speed

(* the original's order: the box drawn is its number *)
let kinds = [ Invu; Teleport; Clear; Swap; Freeze; Speed ]

(* how long an option lasts, in ticks, and the warning before its end *)
let duration (k : kind) : int = match k with Invu -> 400 | Freeze | Speed -> 200 | _ -> 0
let warning = 100

type box = { bx : int; by : int; kind : kind } (* its top-left cell; 5 x 5 *)

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type rider = {
  x : int;
  y : int;
  dir : dir;
  heading : dir; (* the direction of its trail's last line *)
  alive : bool;
  cam : int * int; (* where its window looks: itself, or drifting on once dead *)
  corners : (int * int) list; (* where the trail's current piece turned, the newest first *)
  pieces : (int * int) list list; (* the pieces before a teleport or a swap *)
  gen : int;
  tail : int * int * dir; (* the first cell of its trail, and away from it: where swap goes *)
  holding : kind option;
  using : (kind * int) option; (* an option in use, and its ticks left *)
  score : int;
}

type round = {
  w : int;
  h : int;
  map : Tilemap.t;
  riders : rider list;
  boxes : box list;
  ticks : int;
  border : int; (* the cells the arena has shrunk by *)
  over : int option; (* the frames to wait before the next round *)
}

type scene = Title | Playing of round
type model = { scene : scene; seed : seed }

let initial_model : model = { scene = Title; seed = initial_seed 1997 }

(* the original's 8 starting places, on a w x h map *)
let places (w : int) (h : int) : (int * int * dir) list =
  [ (160, 100, Right); (w - 160, h - 100, Left); (160, h - 100, Right); (w - 160, 100, Left); (w / 2, 100, Down);
    (w / 2, h - 100, Up); (160, h / 2, Right); (w - 160, h / 2, Left) ]

(* map=WxH, one of the original's sizes (800x600 by default) *)
let map_size (flags : flags) : int * int =
  match List.assoc_opt "map" flags with
  | Some ("640x400" | "640x480" | "800x600" | "1024x768" | "1280x1024" | "1600x1200" as m) ->
      Scanf.sscanf m "%dx%d" (fun w h -> (w, h))
  | _ -> (800, 600)

let new_rider (score : int) ((x, y, dir) : int * int * dir) : rider =
  let dx, dy = delta dir in
  { x; y; dir; heading = dir; alive = true; cam = (x, y); corners = []; pieces = []; gen = 0;
    tail = (x + dx, y + dy, opposite dir); holding = None; using = None; score }

(* the places shuffled: each rider a place none of the others has *)
let new_round (flags : flags) (scores : int list) (seed : seed) : round * seed =
  let w, h = map_size flags in
  let rec shuffle places seed acc =
    match places with
    | [] -> (List.rev acc, seed)
    | _ ->
        let p, seed = pick places seed in
        shuffle (List.filter (( <> ) p) places) seed (p :: acc)
  in
  let places, seed = shuffle (places w h) seed [] in
  let riders = List.mapi (fun i score -> new_rider score (List.nth places i)) scores in
  let map = Tilemap.of_strings 1. (List.init h (fun _ -> String.make w ' ')) in
  ({ w; h; map; riders; boxes = []; ticks = 0; border = 0; over = None }, seed)

(*****************************************************************************)
(* Walls *)
(*****************************************************************************)

(* inside the arena, as it has shrunk *)
let inside (r : round) (x : int) (y : int) : bool =
  x >= r.border && x < r.w - r.border && y >= r.border && y < r.h - r.border

(* a wall of the current generation of its rider *)
let wall (r : round) (x : int) (y : int) : bool =
  match Tilemap.get r.map x y with
  | Some ' ' | None -> false
  | Some c ->
      let who, gen = owner c in
      gen = (List.nth r.riders who).gen mod generations

(* a cell with nobody around, [dist] cells on its row and its column *)
let nobody_around (r : round) (x : int) (y : int) (dist : int) : bool =
  List.for_all (fun i -> not (wall r (x + i) y || wall r x (y + i))) (List.init (2 * dist) (fun i -> i - dist))

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(* teleport: a random place with nobody around, closer and closer if
 * the arena is crowded *)
let teleport (r : round) (seed : seed) : int * int * seed =
  let rec go dist tries seed =
    let x, seed = random_int (r.border + 20) (r.w - r.border - 21) seed in
    let y, seed = random_int (r.border + 20) (r.h - r.border - 21) seed in
    if nobody_around r x y dist || tries = 0 then (x, y, seed) else go (max 1 (dist - 1)) (tries - 1) seed
  in
  go 50 200 seed

(* a new piece of trail from the rider's head: its tail starts there *)
let fresh_piece (rd : rider) : rider =
  let dx, dy = delta rd.dir in
  { rd with pieces = ((rd.x, rd.y) :: rd.corners) :: rd.pieces; corners = []; tail = (rd.x + dx, rd.y + dy, opposite rd.dir) }

let use (r : round) (seed : seed) (rd : rider) : rider * seed =
  match rd.holding with
  | None -> (rd, seed)
  | Some k -> (
      let rd = { rd with holding = None } in
      match k with
      | Teleport ->
          let x, y, seed = teleport r seed in
          let dx, dy = delta rd.dir in
          ({ (fresh_piece rd) with x; y; heading = rd.dir; tail = (x + dx, y + dy, opposite rd.dir) }, seed)
      | Clear ->
          let dx, dy = delta rd.dir in
          ({ rd with gen = rd.gen + 1; corners = []; pieces = []; tail = (rd.x + dx, rd.y + dy, opposite rd.dir) }, seed)
      | Swap ->
          let tx, ty, tdir = rd.tail in
          let rd = fresh_piece rd in
          ({ rd with x = tx; y = ty; dir = tdir; heading = tdir; tail = (rd.x, rd.y, rd.dir) }, seed)
      | Invu | Freeze | Speed -> ({ rd with using = Some (k, duration k) }, seed))

(* 6 chances in 1,000 a tick of a box, away from the border and the
 * other boxes *)
let spawn (r : round) (seed : seed) : round * seed =
  let draw, seed = random_int 0 999 seed in
  if draw >= List.length kinds then (r, seed)
  else
    let rec place tries seed =
      let x, seed = random_int (r.border + 20) (r.w - r.border - 21) seed in
      let y, seed = random_int (r.border + 20) (r.h - r.border - 21) seed in
      let free = List.for_all (fun b -> abs (b.bx - x) >= 15 || abs (b.by - y) >= 15) r.boxes in
      if free then ({ r with boxes = { bx = x; by = y; kind = List.nth kinds draw } :: r.boxes }, seed)
      else if tries = 0 then (r, seed)
      else place (tries - 1) seed
    in
    place 30 seed

(*****************************************************************************)
(* A tick *)
(*****************************************************************************)

(* the arrow pressed this tick, if any; no turning back, unless
 * invulnerable or dead (its window drifting where it steers), as in the
 * original *)
let steer (p : Multiplayer.player) (rd : rider) : rider =
  let k = p.pressed in
  let wanted = if k.kup then Some Up else if k.kdown then Some Down else if k.kleft then Some Left else if k.kright then Some Right else None in
  let invulnerable = match rd.using with Some (Invu, _) -> true | _ -> false in
  match wanted with
  | Some d when d <> opposite rd.dir || invulnerable || not rd.alive -> { rd with dir = d }
  | _ -> rd

(* the cells a rider enters this tick: one, two with speed; none when
 * frozen or dead *)
let path (frozen : bool) (rd : rider) : (int * int) list =
  if (not rd.alive) || frozen then []
  else
    let dx, dy = delta rd.dir in
    let steps = match rd.using with Some (Speed, _) -> 2 | _ -> 1 in
    List.init steps (fun i -> (rd.x + (dx * (i + 1)), rd.y + (dy * (i + 1))))

(* a rider moved along its path, checked against the map of the tick
 * before: a box taken, or a wall or the border hit *)
let advance (r : round) (frozen : bool) (rd : rider) : rider * box list =
  let invulnerable = match rd.using with Some (Invu, _) -> true | _ -> false in
  List.fold_left
    (fun (rd, taken) (x, y) ->
      if not rd.alive then (rd, taken)
      else if not (inside r x y) then ({ rd with alive = false; using = None; holding = None }, taken)
      else
        (* a box is taken (its cells aren't walls); a wall kills *)
        let box = List.find_opt (fun b -> x >= b.bx && x < b.bx + 5 && y >= b.by && y < b.by + 5) r.boxes in
        let rd, taken =
          match box with
          | Some b -> ({ rd with holding = Some b.kind }, b :: taken)
          | None when wall r x y && not invulnerable -> ({ rd with alive = false; using = None; holding = None }, taken)
          | None -> (rd, taken)
        in
        (* a turn leaves a corner in the trail *)
        let corners = if rd.heading <> rd.dir || rd.corners = [] then (rd.x, rd.y) :: rd.corners else rd.corners in
        ({ rd with x; y; corners; heading = rd.dir }, taken))
    (rd, []) (path frozen rd)

let play (players : Multiplayer.player list) (r : round) (seed : seed) : round * seed =
  let freezer rd = match rd.using with Some (Freeze, _) -> true | _ -> false in
  let frozen i = (not (freezer (List.nth r.riders i))) && List.exists freezer r.riders in
  (* the keys: turn, then use *)
  let riders, seed =
    List.fold_left
      (fun (acc, seed) ((p : Multiplayer.player), rd) ->
        let rd = steer p rd in
        let rd, seed = if rd.alive && p.pressed.kspace then use r seed rd else (rd, seed) in
        (rd :: acc, seed))
      ([], seed)
      (List.combine players r.riders)
  in
  let steered = List.rev riders in
  (* pass 1: every move against the map of the tick before *)
  let moved = List.mapi (fun i rd -> advance { r with riders = steered } (frozen i) rd) steered in
  let taken = List.concat_map snd moved in
  let riders = List.map fst moved in
  (* pass 2: the new cells written *)
  let map =
    List.fold_left
      (fun map (i, (rd, _)) ->
        List.fold_left (fun map (x, y) -> if rd.alive then Tilemap.set map x y (mark i rd.gen) else map) map
          (path (frozen i) (List.nth steered i)))
      r.map
      (List.mapi (fun i m -> (i, m)) moved)
  in
  (* pass 3: two riders on one cell, both dead *)
  let riders =
    List.map
      (fun rd ->
        if rd.alive && List.length (List.filter (fun o -> o.alive && o.x = rd.x && o.y = rd.y) riders) > 1 then
          { rd with alive = false; using = None; holding = None }
        else rd)
      riders
  in
  (* the windows, the options' time *)
  let riders =
    List.map
      (fun rd ->
        let cam =
          if rd.alive then (rd.x, rd.y)
          else
            let dx, dy = delta rd.dir and cx, cy = rd.cam in
            (max 0 (min r.w (cx + dx)), max 0 (min r.h (cy + dy)))
        in
        let using = match rd.using with Some (k, n) when n > 1 -> Some (k, n - 1) | _ -> None in
        { rd with cam; using })
      riders
  in
  let r = { r with map; riders; boxes = List.filter (fun b -> not (List.memq b taken)) r.boxes; ticks = r.ticks + 1 } in
  let r, seed = spawn r seed in
  (* after 2,000 ticks, the arena a cell smaller every 50 *)
  let r = if r.ticks >= 2000 && (r.ticks - 2000) mod 50 = 0 then { r with border = r.border + 1 } else r in
  (* the last one alive scores *)
  let alive = List.filter (fun rd -> rd.alive) r.riders in
  if List.length alive <= 1 && List.length r.riders > 1 then
    ({ r with riders = List.map (fun rd -> if rd.alive then { rd with score = rd.score + 1 } else rd) r.riders; over = Some 120 }, seed)
  else (r, seed)

let update (computer : computer) (players : Multiplayer.player list) (model : model) : model =
  let anyone_pressed_space = List.exists (fun (p : Multiplayer.player) -> p.pressed.kspace) players in
  match model.scene with
  | Title ->
      if anyone_pressed_space then
        let round, seed = new_round computer.flags (List.map (fun _ -> 0) players) model.seed in
        { scene = Playing round; seed }
      else model
  | Playing r -> (
      match r.over with
      | Some 0 ->
          let round, seed = new_round computer.flags (List.map (fun rd -> rd.score) r.riders) model.seed in
          { scene = Playing round; seed }
      | Some n -> { model with scene = Playing { r with over = Some (n - 1) } }
      | None ->
          let r, seed = play players r model.seed in
          { scene = Playing r; seed })

(*****************************************************************************)
(* The view *)
(*****************************************************************************)

let rider_colors = [| rgb 90 150 255; orange; rgb 120 220 80; purple; yellow; red; rgb 0 200 200; rgb 255 120 200 |]

let kind_color (k : kind) : color =
  match k with
  | Freeze -> rgb 150 220 255
  | Speed -> rgb 40 60 200
  | Teleport -> red
  | Invu -> white
  | Clear -> brown
  | Swap -> green

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* the cells from (ox, oy) on, on the screen's top 600 pixels *)
let at (ox : int) (oy : int) (x : number) (y : number) : number * number =
  ((-500.) +. ((x -. float_of_int ox) *. cell), 500. -. ((y -. float_of_int oy) *. cell))

(* the cells from (x1, y1) to (x2, y2) (a line of a trail, a box), as
 * one rectangle, cut to the window; None outside it (shapes aren't
 * clipped: what isn't in the window mustn't be drawn) *)
let line (ox : int) (oy : int) color ((x1, y1) : int * int) ((x2, y2) : int * int) : shape option =
  let lx = max ox (min x1 x2) and hx = min (ox + view_cols - 1) (max x1 x2) in
  let ly = max oy (min y1 y2) and hy = min (oy + view_rows - 1) (max y1 y2) in
  if lx > hx || ly > hy then None
  else
    let cx, cy = at ox oy (float_of_int (lx + hx + 1) /. 2.) (float_of_int (ly + hy + 1) /. 2.) in
    Some (rectangle color (float_of_int (hx - lx + 1) *. cell) (float_of_int (hy - ly + 1) *. cell) |> move cx cy)

let polyline ox oy color (points : (int * int) list) : shape list =
  match points with
  | [] | [ _ ] -> []
  | _ :: rest -> List.filter_map Fun.id (List.map2 (line ox oy color) (List.filteri (fun i _ -> i < List.length rest) points) rest)

let view_round (n : int) (r : round) : shape list =
  let me = List.nth r.riders n in
  let cx, cy = me.cam in
  let ox = max 0 (min (r.w - view_cols) (cx - (view_cols / 2))) in
  let oy = max 0 (min (r.h - view_rows) (cy - (view_rows / 2))) in
  let trails =
    List.concat
      (List.mapi
         (fun i rd ->
           let color = rider_colors.(i mod 8) in
           List.concat_map (polyline ox oy color) (((rd.x, rd.y) :: rd.corners) :: rd.pieces))
         r.riders)
  in
  let heads = List.filter_map (fun rd -> if rd.alive then line ox oy white (rd.x, rd.y) (rd.x, rd.y) else None) r.riders in
  let boxes = List.filter_map (fun b -> line ox oy (kind_color b.kind) (b.bx, b.by) (b.bx + 4, b.by + 4)) r.boxes in
  (* the border, where the arena has shrunk to *)
  let b = r.border in
  let border =
    List.filter_map Fun.id
      [ line ox oy green (b, b) (r.w - 1 - b, b); line ox oy green (b, r.h - 1 - b) (r.w - 1 - b, r.h - 1 - b);
        line ox oy green (b, b) (b, r.h - 1 - b); line ox oy green (r.w - 1 - b, b) (r.w - 1 - b, r.h - 1 - b) ]
  in
  rectangle black 1000. 1000. :: (trails @ boxes @ border @ heads)

(* under the window, as the original's bottom line: every player's name
 * and score, in the color of the option in use; the option held; a star
 * when it's about to end *)
let hud (n : int) (r : round) : shape list =
  let me = List.nth r.riders n in
  let players =
    List.mapi
      (fun i rd ->
        let color = match (rd.alive, rd.using) with false, _ -> darkGray | true, Some (k, _) -> kind_color k | true, None -> rider_colors.(i mod 8) in
        text color 3. (Printf.sprintf "P%d %02d" (i + 1) rd.score) |> move ((-350.) +. (float_of_int i *. 200.)) (-150.))
      r.riders
  in
  let held = match me.holding with Some k -> [ square (kind_color k) 40. |> move 380. (-150.) ] | None -> [ square darkGray 40. |> move 380. (-150.) ] in
  let star = match me.using with Some (_, left) when left <= warning -> [ text yellow 4. "**" |> move 440. (-150.) ] | _ -> [] in
  let over =
    match r.over with
    | Some _ ->
        let winners = List.concat (List.mapi (fun i rd -> if rd.alive then [ i + 1 ] else []) r.riders) in
        [ text white 4. (match winners with [ w ] -> Printf.sprintf "P%d wins the round" w | _ -> "draw") |> move_y (-280.) ]
    | None -> []
  in
  players @ held @ star @ over

let view (_computer : computer) (n : int) (model : model) : shape list =
  match model.scene with
  | Title ->
      [ rectangle black 1000. 1000.;
        text (rgb 90 150 255) 7. "TINY TRONSCROLL" |> move_y 330.;
        text gray 2. "after tron v0.1 (Yoann Padioleau, INSA Rennes, 1997)" |> move_y 250.;
        text (rgb 90 150 255) 2.5 "player 1: the arrows, space uses an option" |> move_y 150.;
        text orange 2.5 "player 2: w/a/s/d, q uses an option" |> move_y 100. ]
      @ List.concat
          (List.mapi
             (fun i (k, what) ->
               let y = -10. -. (40. *. float_of_int i) in
               [ square (kind_color k) 24. |> move (-260.) y; text white 2. what |> move 20. y ])
             [ (Freeze, "freeze: the others stop"); (Speed, "speed: twice as fast"); (Teleport, "teleport: somewhere else");
               (Invu, "invulnerable: through the trails"); (Clear, "clear: your walls gone"); (Swap, "swap: become your tail") ])
      @ [ text yellow 3. "PRESS SPACE" |> move_y (-330.) ]
  | Playing r -> view_round n r @ hud n r

(* the network granted, for net=host and net=join only (plan_caps.md) *)
let app (network : < Cap.network ; .. >) = Multiplayer.game ~network ~split:true ~players:2 view update initial_model
let main = Cap.main (fun caps -> Playground_platform.run_app ~flags:(Playground_platform.flags ()) (app caps))
