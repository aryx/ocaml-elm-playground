(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Ico (Fumito Ueda, Team Ico, Sony, 2001): a boy
 * born with horns, left in a castle to die, and Yorda, a girl of light
 * he finds there and leads out by the hand. Left/right to run, up to
 * jump, space to swing your stick, x to call Yorda, or, next to her,
 * to take her hand (and let go). Holding it, she follows, and you pull
 * her up the ledges she can't climb; across a gap she can't walk, call
 * her from the other side and she leaps to you. Shadows rise from the
 * black pools to take her back: beat them off, and if one carries her
 * into its pool, she is lost. Only she opens the idol doors; the gate
 * at the end needs you both.
 *
 * Ico was made by a small team inside Sony, its designer an artist,
 * with as little as possible on the screen (no life bar, no score, a
 * dozen words in the game); it sold modestly and became one of the
 * most cited games of its decade, by designers more than players, and
 * led to Shadow of the Colossus (2005) and The Last Guardian (2016).
 * (Names and dates from memory, to check.)
 *
 * The new idea here is the companion: an AI you look after instead of
 * one that looks after you. Yorda has her own body and her own limits
 * ([step_yorda]): she walks slower, never jumps up, stops at an edge;
 * what the player gives her is a hand. Holding it is a rope between two
 * bodies, a distance kept ([leash]): too far, she's dragged along; much
 * too far, the hand slips. The two verbs of the original (call, hold)
 * are all the orders there are, and every obstacle is one for her, not
 * for him: the ledge he jumps and she must be pulled up, the gap he
 * jumps and she must be called over, the door only she opens.
 *
 *     I  >--<  Y      holding hands: she keeps about 30 pixels behind
 *     ===========     him, running when he runs; up a ledge he's
 *                     standing on, she's pulled up; over 150 pixels
 *                     apart, the hand lets go
 *
 * And the enemies want her, not him: a shadow flies to Yorda and
 * carries her to its pool ([step_shadow]); it only knocks the boy down
 * when he's in the way. The escort mission, the most hated kind in
 * other games, made the whole game, by making her worth the trouble.
 *
 * What it uses: the platformer kit (gamekits/platformer/: Tile_move,
 * both of them against the stone, one pixel at a time), Tilemap (the
 * castle, changed by the idol doors opened), Camera2d (following the
 * boy, clamped to the castle), Sprite (the boy), Scene2d. Not Physics:
 * the leash is a rule about a distance, not a spring, and the shadows
 * fly through everything.
 *
 * Exercises: the stone benches where the two of them sit to save;
 * Yorda climbing ladders, slowly, as in the original; levers and chains
 * only the boy can reach; the shadows' portals that open only when she
 * is left alone too long; the sword of the last castle.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The castle *)
(*****************************************************************************)

(* 70 x 18 tiles: # stone, D an idol door (opened by Yorda near it), O
 * a shadows' pool, X the gate out; I the boy, Y Yorda. From left to
 * right: a ledge (pull her up), a gap (call her over), the courtyard of
 * the first pool, the idol door, the second pool, another ledge, and
 * the gate. *)
let castle =
  [ "######################################################################";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                         #";
    "#                                          #                     XX  #";
    "#                                          D                     XX  #";
    "#       #######  #########                 D              ############";
    "# I Y   #######  #########        O        D        O     ############";
    "###############  #####################################################";
    "###############  #####################################################" ]

let tile = 40.
let level = Tilemap.of_strings tile castle
let solid (c : char) : bool = c = '#' || c = 'D'
let boy_size = (26., 46.)
let yorda_size = (22., 54.)
let bottom = (Tilemap.bounds level : Camera2d.rect).bottom

let place (c : char) (h : number) : number * number =
  let x, y = Tilemap.center level (fst (List.hd (Tilemap.find level c))) (snd (List.hd (Tilemap.find level c))) in
  (x, y - (tile / 2.) + (h / 2.))

let pools : (number * number) list = List.map (fun (c, r) -> Tilemap.center level c r) (Tilemap.find level 'O')

(* the castle to walk: the two of them are not tiles *)
let start_map : Tilemap.t = List.fold_left (fun m (c, r) -> Tilemap.set m c r ' ') level (Tilemap.find level 'I' @ Tilemap.find level 'Y')

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type boy = { x : number; y : number; vy : number; facing : number; steps : int; swing : int; stunned : int }

type yorda = {
  yx : number;
  yy : number;
  yvx : number; (* only in a leap *)
  yvy : number;
  held : bool;
  called : bool;
  lift : ((number * number) * (number * number) * int) option; (* pulled up a ledge: from, to, frames done *)
  ysteps : int;
}

type shadow = { sx : number; sy : number; hp : int; carrying : bool; knocked : int }

type game = { map : Tilemap.t; boy : boy; yorda : yorda; shadows : shadow list; cam : Camera2d.t; frames : int }
type scene = Title | Playing of game | Lost of game * string * int | Out of int
type model = scene Scene2d.t

let new_game () : game =
  let x, y = place 'I' (snd boy_size) in
  let yx, yy = place 'Y' (snd yorda_size) in
  { map = start_map; boy = { x; y; vy = 0.; facing = 1.; steps = 0; swing = 0; stunned = 0 };
    yorda = { yx; yy; yvx = 0.; yvy = 0.; held = false; called = false; lift = None; ysteps = 0 }; shadows = [];
    cam = Camera2d.look_at x y Camera2d.origin; frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The boy *)
(*****************************************************************************)

let run_speed = 4.
let jump_speed = 13. (* up to about 2.5 tiles *)
let gravity = 0.8

(* [fall size (x, y) vy]: gravity and the stone, for either of them *)
let fall (map : Tilemap.t) (size : number * number) ((x, y) : number * number) (vy : number) : number * number =
  let on = Tile_move.on_ground solid map size x y in
  let vy = if on && vy <= 0. then 0. else Float.max (-16.) (vy - gravity) in
  let (_, y), hit = Tile_move.move_by solid map size (x, y) (0., vy) in
  (y, if hit then 0. else vy)

let step_boy (map : Tilemap.t) (keys : keyboard) (jump : bool) (b : boy) : boy =
  let dir = if b.stunned > 0 then 0. else to_x keys in
  let (x, _), _ = Tile_move.move_by solid map boy_size (b.x, b.y) (dir * run_speed, 0.) in
  let on = Tile_move.on_ground solid map boy_size x b.y in
  let vy = if jump && on && b.stunned = 0 then jump_speed else b.vy in
  let y, vy = fall map boy_size (x, b.y) vy in
  { x; y; vy; facing = (if dir <> 0. then dir else b.facing); steps = (if dir <> 0. then b.steps +.. 1 else b.steps);
    swing = max 0 (b.swing -.. 1); stunned = max 0 (b.stunned -.. 1) }

(*****************************************************************************)
(* Yorda *)
(*****************************************************************************)

let walk_speed = 2.2 (* alone, called *)
let feet (y : number) (h : number) : number = y - (h / 2.)

(* the hand: where she keeps, holding it, and how far it holds *)
let leash (b : boy) : number = b.x - (b.facing * 30.)
let slips = 150.

(* [step_yorda map b y]: pulled up a ledge; in a leap; held, following
 * the hand, pulled up when he's on a ledge just above her; called,
 * walking to him; always stopping at an edge, unless he's across it and
 * calls her (she leaps) or holds her hand down a drop *)
let step_yorda (map : Tilemap.t) (b : boy) (yo : yorda) : yorda =
  let w, h = yorda_size in
  match yo.lift with
  | Some ((x0, y0), (x1, y1), n) ->
      let t = float_of_int n / 20. in
      if n >= 20 then { yo with yx = x1; yy = y1; lift = None; yvy = 0. } else { yo with yx = x0 + ((x1 - x0) * t); yy = y0 + ((y1 - y0) * t); lift = Some ((x0, y0), (x1, y1), n +.. 1) }
  | None ->
      let on = Tile_move.on_ground solid map yorda_size yo.yx yo.yy in
      if not on && yo.yvx <> 0. then
        (* a leap: on until she lands *)
        let (x, _), _ = Tile_move.move_by solid map yorda_size (yo.yx, yo.yy) (yo.yvx, 0.) in
        let y, vy = fall map yorda_size (x, yo.yy) yo.yvy in
        let landed = Tile_move.on_ground solid map yorda_size x y in
        { yo with yx = x; yy = y; yvy = vy; yvx = (if landed then 0. else yo.yvx) }
      else
        let target = if yo.held then Some (leash b) else if yo.called then Some b.x else None in
        let yo =
          match target with
          | None -> yo
          | Some tx ->
              let dx = tx - yo.yx in
              let speed = if yo.held then Float.min 4.5 (Float.abs dx / 6.) else walk_speed in
              let dir = if dx > 0. then 1. else -1. in
              let step = if Float.abs dx < 4. then 0. else dir * Float.min speed (Float.abs dx) in
              let ahead = yo.yx + (dir * ((w / 2.) + 6.)) in
              let edge = on && step <> 0. && not (Tile_move.on_ground solid map yorda_size ahead yo.yy) in
              (* ground below the edge, near enough to step down to *)
              let drop_ok = Tile_move.hits solid map yorda_size ahead (yo.yy - (3. * tile)) in
              let boy_across = Float.abs (b.x - yo.yx) < 150. && (b.x - yo.yx) * dir > 30. && feet b.y (snd boy_size) >= feet yo.yy h - 10. in
              if edge && (not drop_ok) && boy_across && on then (* the leap *) { yo with yvx = dir * 4.5; yvy = 9.; called = false }
              else if edge && not (drop_ok && (yo.held || feet b.y (snd boy_size) < feet yo.yy h)) then { yo with called = false }
              else
                let (x, _), blocked = Tile_move.move_by solid map yorda_size (yo.yx, yo.yy) (step, 0.) in
                let up = feet b.y (snd boy_size) - feet yo.yy h in
                if blocked && yo.held && up > 20. && up < 100. && Float.abs (b.x - yo.yx) < 70. then
                  (* pulled up: to his feet's level, a little past the edge *)
                  let x1 = x + (dir * 18.) and y1 = feet b.y (snd boy_size) + (h / 2.) in
                  if Tile_move.hits solid map yorda_size x1 y1 then { yo with yx = x } else { yo with yx = x; lift = Some ((x, yo.yy), (x1, y1), 0) }
                else { yo with yx = x; ysteps = (if step <> 0. then yo.ysteps +.. 1 else yo.ysteps); called = yo.called && Float.abs (b.x - x) > 40. }
        in
        let y, vy = fall map yorda_size (yo.yx, yo.yy) yo.yvy in
        { yo with yy = y; yvy = vy; held = yo.held && Float.hypot (b.x - yo.yx) (b.y - yo.yy) < slips }

(*****************************************************************************)
(* The shadows *)
(*****************************************************************************)

let nearest_pool (x : number) : number * number = List.fold_left (fun (bx, by) (px, py) -> if Float.abs (px - x) < Float.abs (bx - x) then (px, py) else (bx, by)) (List.hd pools) pools

let toward ((x, y) : number * number) ((tx, ty) : number * number) (speed : number) : number * number =
  let d = Float.hypot (tx - x) (ty - y) in
  if d <= speed then (tx, ty) else (x + ((tx - x) * speed / d), y + ((ty - y) * speed / d))

(* a shadow flies to Yorda, and, when her hand isn't held, takes her to
 * its pool; knocked back by the stick *)
let step_shadow (yo : yorda) (s : shadow) : shadow =
  if s.knocked > 0 then { s with knocked = s.knocked -.. 1 }
  else if s.carrying then let sx, sy = toward (s.sx, s.sy) (nearest_pool s.sx) 0.9 in { s with sx; sy }
  else let sx, sy = toward (s.sx, s.sy) (yo.yx, yo.yy + 10.) 1.8 in { s with sx; sy }

(* every 4 seconds a pool near her lets a shadow out, two at most *)
let spawn (g : game) : shadow list =
  if g.frames mod 240 <> 120 || List.length g.shadows >= 2 then []
  else
    List.filter_map
      (fun (px, py) -> if Float.abs (px - g.yorda.yx) < 500. then Some { sx = px; sy = py + 10.; hp = 3; carrying = false; knocked = 0 } else None)
      pools
    |> List.filteri (fun i _ -> i = 0)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

(* the idol doors: Yorda's light opens the one she stands near *)
let open_doors (g : game) : game =
  let near (c, r) = let x, y = Tilemap.center g.map c r in Float.abs (x - g.yorda.yx) < 70. && Float.abs (y - g.yorda.yy) < 100. in
  match List.find_opt near (Tilemap.find g.map 'D') with
  | Some (col, _) -> { g with map = List.fold_left (fun m (c, r) -> if c = col then Tilemap.set m c r ' ' else m) g.map (Tilemap.find g.map 'D') }
  | None -> g

let update_game (computer : computer) (scenes : model) (g : game) : game =
  let pressed k = Scene2d.pressed k scenes in
  let g = { g with frames = g.frames +.. 1 } in
  let b = step_boy g.map computer.keyboard (pressed (fun k -> k.kup)) g.boy in
  let b = if pressed (fun k -> k.kspace) && b.swing = 0 && b.stunned = 0 then { b with swing = 18 } else b in
  let carried = List.exists (fun s -> s.carrying) g.shadows in
  (* x: next to her, take her hand or let go; farther, call her *)
  let yo = g.yorda in
  let yo =
    if pressed (fun k -> Set_.mem "x" k.keys) && not carried then
      if Float.hypot (b.x - yo.yx) (b.y - yo.yy) < 70. then { yo with held = not yo.held; called = false } else { yo with called = true }
    else yo
  in
  let yo = if carried then yo else step_yorda g.map b yo in
  (* the stick, at its swing's middle, in front of him *)
  let hits s = b.swing >= 6 && b.swing <= 12 && (s.sx - b.x) * b.facing > -10. && Float.abs (s.sx - b.x) < 60. && Float.abs (s.sy - b.y) < 50. in
  let shadows =
    List.filter_map
      (fun s ->
        if s.knocked = 0 && hits s then
          if s.hp <= 1 then None else Some { s with hp = s.hp -.. 1; knocked = 20; carrying = false; sx = s.sx + (b.facing * 40.) }
        else Some (step_shadow yo s))
      g.shadows
    @ spawn g
  in
  (* a shadow touching her, her hand free, takes her; touching him, it
   * knocks him down, and the hand lets go *)
  let touching_her s = Float.hypot (s.sx - yo.yx) (s.sy - yo.yy) < 30. in
  let shadows = if yo.held || List.exists (fun s -> s.carrying) shadows then shadows else match List.find_opt touching_her shadows with Some t -> List.map (fun s -> if s == t then { s with carrying = true } else s) shadows | None -> shadows in
  let b, yo =
    if b.stunned = 0 && List.exists (fun s -> s.knocked = 0 && Float.hypot (s.sx - b.x) (s.sy - b.y) < 30.) shadows then ({ b with stunned = 30; vy = 5. }, { yo with held = false }) else (b, yo)
  in
  let yo = match List.find_opt (fun s -> s.carrying) shadows with Some s -> { yo with yx = s.sx; yy = s.sy + 20.; held = false; called = false; lift = None; yvx = 0.; yvy = 0. } | None -> yo in
  let g = open_doors { g with boy = b; yorda = yo; shadows } in
  { g with cam = Camera2d.follow 0.1 b.x b.y g.cam |> Camera2d.clamp computer.screen (Tilemap.bounds level) }

let in_gate (map : Tilemap.t) (x : number) (y : number) : bool = Tilemap.tile_at map x y = Some 'X'

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      let lost_in_pool = List.exists (fun sh -> sh.carrying && List.exists (fun (px, py) -> Float.hypot (sh.sx - px) (sh.sy - py) < 12.) pools) g.shadows in
      if in_gate g.map g.boy.x g.boy.y && in_gate g.map g.yorda.yx g.yorda.yy then Scene2d.go (Out g.frames) s
      else if lost_in_pool then Scene2d.go (Lost (g, "the shadows took her back", 0)) s
      else if g.yorda.yy < bottom then Scene2d.go (Lost (g, "she fell", 0)) s
      else if g.boy.y < bottom then Scene2d.go (Lost (g, "you fell", 0)) s
      else { s with scene = Playing g }
  | Lost (g, why, n) -> if n > 60 && space then Scene2d.go (Playing (new_game ())) s else { s with scene = Lost (g, why, n +.. 1) }
  | Out _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let tile_shape (c : char) : shape =
  match c with
  | '#' -> group [ square (rgb 190 180 160) tile; rectangle (rgb 165 155 135) tile 3. |> move_y 6.; rectangle (rgb 165 155 135) 3. 17. |> move (-8.) 14.; rectangle (rgb 165 155 135) 3. 17. |> move 12. (-4.) ]
  | 'D' -> group [ square (rgb 110 105 95) tile; rectangle (rgb 200 230 255) 6. 26. |> fade 0.6; circle (rgb 200 230 255) 6. |> move_y 14. |> fade 0.6 ]
  | 'X' -> square (rgb 255 250 220) tile |> fade 0.8
  | 'O' -> group [ oval black 90. 18. |> move_y (-16.); oval (rgb 40 20 50) 60. 10. |> move_y (-16.) ]
  | _ -> group []

(* the castle's halls behind: arches, and the light from high windows *)
let backdrop (cam : Camera2d.t) : shape list =
  List.init 20 (fun i ->
      let x = (float_of_int i * 300.) - 1500. in
      group [ rectangle (rgb 150 150 150) 40. 500. |> move x 20.; oval (rgb 215 225 230) 220. 300. |> move (x + 150.) 60.; rectangle (rgb 215 225 230) 220. 200. |> move (x + 150.) (-90.);
              oval (rgb 120 160 110) 200. 60. |> move (x + 150.) (-170.);
              rectangle (rgb 255 250 220) 20. 60. |> move (x + 150.) 160. |> fade 0.5 ])
  |> List.map (fun s -> s |> move ((cam.x * 0.5)) 0.)

let boy_rows =
  let top = [ "W......W"; "WW....WW"; ".KKKKKK."; ".KSSSSK."; "..SKSS.."; "..SSSS.."; ".TTTTTT."; "STTTTTTS"; "STTTTTTS"; ".TTTTTT." ] in
  [ top @ [ ".SS..SS."; ".SS..SS."; ".BB..BB." ]; top @ [ "..SSSS.."; "..SSSS.."; "..BBBB.." ] ]

let palette = [ ('W', rgb 230 230 210); ('K', rgb 60 40 30); ('S', rgb 210 160 120); ('T', rgb 200 190 160); ('B', rgb 90 70 50) ]

(* Yorda: tall, white, lit from inside *)
let view_yorda (yo : yorda) (frames : int) (carried : bool) : shape =
  let sway = if yo.ysteps mod 20 < 10 then 2. else -2. in
  let body =
    group
      [ circle (rgb 255 255 240) 34. |> fade 0.12; polygon (rgb 245 245 240) [ (-12., 10.); (12., 10.); (16., -27.); (-16., -27.) ]; circle (rgb 250 240 230) 8. |> move_y 18.;
        rectangle (rgb 230 230 225) 16. 6. |> move_y 25.; rectangle (rgb 250 240 230) 3. 10. |> move (-4. + sway) (-30.); rectangle (rgb 250 240 230) 3. 10. |> move (4. - sway) (-30.) ]
  in
  let body = if carried then body |> rotate (80. + (5. * sin (float_of_int frames / 6.))) else body in
  body |> move yo.yx yo.yy

let view_shadow (frames : int) (s : shadow) : shape =
  let wob = sin (float_of_int (frames +.. int_of_float s.sx) / 7.) * 3. in
  group
    [ oval black 44. (60. + wob) |> fade (if s.knocked > 0 then 0.5 else 0.9); oval black 30. 20. |> move (-10.) (-30.) |> fade 0.6; oval black 30. 20. |> move 10. (-30.) |> fade 0.6;
      circle white 3.5 |> move (-7.) 12.; circle white 3.5 |> move 7. 12. ]
  |> move s.sx s.sy

let view_game (screen : screen) (g : game) : shape list =
  let b = g.boy and yo = g.yorda in
  let carried = List.exists (fun s -> s.carrying) g.shadows in
  let boy = Sprite.pixels 3.5 palette (let r = Sprite.cycle (b.steps /.. 6) boy_rows in if b.facing < 0. then Sprite.flip r else r) in
  let boy = if b.stunned > 0 then boy |> rotate (90. * b.facing) |> move_y (-10.) else boy in
  let stick = if b.swing > 0 then [ rectangle (rgb 120 90 60) 36. 4. |> rotate (b.facing * (float_of_int b.swing * 8. - 60.)) |> move (b.facing * 22.) 4. |> move b.x b.y ] else [] in
  let hands = if yo.held then [ rectangle (rgb 250 240 230) (Float.abs (b.x - yo.yx)) 3. |> move ((b.x + yo.yx) / 2.) ((b.y + yo.yy) / 2.) ] else [] in
  let cam = Camera2d.clamp screen (Tilemap.bounds level) g.cam in
  [ rectangle (rgb 165 170 172) screen.width screen.height; group (backdrop cam) |> fade 0.9;
    Camera2d.view cam
      ([ Tilemap.view_visible (Camera2d.visible screen cam) tile_shape g.map ]
      @ hands
      @ [ view_yorda yo g.frames carried; boy |> move b.x b.y ]
      @ stick
      @ List.map (view_shadow g.frames) g.shadows) ]

let view (computer : computer) (s : model) : shape list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      view_game screen (new_game ())
      @ [ rectangle black 760. 260. |> fade 0.8 |> move_y 60.; text (rgb 250 245 230) 7. "TINY ICO" |> move_y 140.;
          text white 2.2 "left/right run   up jump   space swing the stick" |> move_y 75.;
          text white 2.2 "x call Yorda, or take her hand (and let go)" |> move_y 40. ]
      @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y (-20.) ]
  | Playing g -> view_game screen g
  | Lost (g, why, _) -> view_game screen g @ [ rectangle black screen.width screen.height |> fade 0.6; text white 4. why ] @ Scene2d.blink 1. s [ text yellow 2.5 "PRESS SPACE" |> move_y (-80.) ]
  | Out _ -> [ rectangle (rgb 255 250 225) screen.width screen.height; text (rgb 90 80 60) 5. "OUT OF THE CASTLE"; text (rgb 90 80 60) 2.5 "the two of them, into the light" |> move_y (-70.) ] @ Scene2d.blink 1. s [ text (rgb 90 80 60) 3. "PRESS SPACE" |> move_y (-150.) ]

let app = game view update initial_model
let main = Playground_platform.run_app app