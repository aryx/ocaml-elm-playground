(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Zaxxon (Sega, 1982), the first game to be seen from
 * an angle: you fly a fighter along an isometric fortress, over its
 * walls and between its towers, shooting what stands on it before the
 * fuel runs out.
 *
 *   left/right  across the fortress
 *   up/down     higher and lower (and the shadow says how high)
 *               -- the gauge's red mark is the wall ahead of you
 *   space       fire
 *
 * The trick of this game is the oldest in this directory, and the
 * smallest: one projection, and an order to draw in.
 *
 *   sx = 0.85 x + 0.34 z
 *   sy = -0.30 x + 0.42 z + y
 *
 * Three world axes, two screen ones: [x] across the fortress, [z]
 * along it (into the picture, up and to the right), [y] straight up.
 * There is no camera, no perspective and no depth per pixel: a thing
 * twice as far away is drawn exactly as big, which is what makes the
 * view isometric rather than perspective, and why the shapes of the
 * fortress never distort. The scrolling is [z] taken away from
 * everything at once.
 *
 * ("Isometric" is the word everyone uses, and the one that links this
 * view to Q*bert, Knight Lore, Populous and Diablo. The pedantic word,
 * and Sega's own in 1982, is *axonometric*: isometric is the special
 * case where the three axes are equally foreshortened, which the two
 * lines above are not -- x is drawn longer than z on purpose, so that
 * the fortress reads as a long corridor rather than a square grid.)
 *
 *          y                      far
 *          |                       .
 *          |                    .     .        z (along the fortress)
 *          |                 .     +     .
 *          +----- x       .     .     .
 *         /                  +     .
 *        z                      .
 *                              near
 *
 * And then the problem that projection creates, which is the game:
 * once you can be *above* something, the screen cannot tell you how
 * high you are. Two pictures a hundred pixels apart on the screen may
 * be a plane at altitude 100 and a plane on the ground, or two planes
 * at the same height a hundred units apart along the fortress -- the
 * projection has thrown away exactly the number you need to fly
 * through a hole. Sega's answer, and every isometric game's since, is
 * the **shadow**: every flying thing is drawn twice, once where it is
 * and once at y = 0 directly below ([at] and [shadow_at]), and the
 * vertical gap between the two *is* the altitude, in pixels, to be
 * read off the screen. Fly at a wall and watch the shadow, not the
 * plane. The altimeter up the left side ([altimeter]) is the arcade's
 * second answer, for when the ground below you is too busy to read.
 *
 * Nothing in the fortress is ever above the fighter: a wall is blocks
 * standing on the floor, and you clear them or you go between them.
 * That is not a simplification, it is the shape of the thing -- the
 * arcade had no windows to fly into either -- and it is what keeps
 * this world inside the rule the rest of games2.5d/ lives by, one
 * height per point of the map. What it does not spare you is the
 * tower you have *flown past*, which stands between you and the eye
 * until its picture slides off you; the fighter is then drawn again
 * over it, faintly ([hidden_by], which asks whether a block is in the
 * way -- a question this projection answers with one direction and a
 * multiplication, there being no perspective in it). Knight Lore did
 * that, and so does every game since that lets you walk behind a
 * wall.
 *
 * Depth is settled the way every game here settles it, by the order of
 * the drawing: the shapes are sorted by their z, far ones first
 * ([painted]). With no overlapping allowed to be ambiguous -- one
 * thing per place on the fortress -- the painter's algorithm is exact,
 * and costs a sort. (examples3d/PaintersAlgorithmFail3d.ml is what it
 * cannot do, and why a z-buffer was worth inventing.)
 *
 * The fourth family of this directory, then, next to cell by cell
 * (TinyDungeonMaster), row by row (TinyKart, TinyOutRun) and column by
 * column (TinyWolfenstein, TinyDoom, TinyComanche): **object by
 * object**. Its world obeys the same restriction as theirs, one height
 * per point; what is new is that the *player* has a height and must
 * judge it, where the others put the eye at a fixed level and let the
 * world have all the geometry. That one extra number is what the
 * projection throws away, and the shadow is what gives it back.
 *
 * Zaxxon was a landmark in 1982 for its look alone, and the view
 * became a genre of its own: Q*bert (1982), Marble Madness (1984),
 * Knight Lore (1984) and its Filmation engine, Populous (1989),
 * Syndicate (1993), Diablo (1996). Every one of them inherits the
 * shadow problem, and most of them solve it the same way. (Names and
 * dates from memory, to check.)
 *
 * What it uses: no kit and no layer but Scene2d. Not Camera2d: the
 * scroll is a subtraction inside [project], and a camera that moved
 * the finished shapes would move the shadows with them, which is the
 * one thing that must not happen. Not the shmup kit's Shots either,
 * and for a reason worth saying: a Shots.t is two numbers and two
 * speeds on the *screen*, and these shots fly along the world's third
 * axis, which the screen does not have -- a kit is for the games whose
 * shape it is.
 *
 * Exercises: the second half of the arcade (open space, no fortress
 * floor at all -- which is the same game with the ground taken away,
 * and much harder to judge), the enemy fighters that fly at you, the
 * homing missiles, the Zaxxon robot at the end with its lock to shoot
 * out; and the projection turned into a layer once a second game (a
 * TinyQbert, a TinyKnightLore) wants it.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The fortress *)
(*****************************************************************************)

let half_w = 200. (* the fortress is 400 wide *)
let ceiling = 170. (* as high as the fighter may fly *)
let run_length = 3800. (* and as long as it is, before the end wall *)

let sky = rgb 12 12 30
let floor_a = rgb 60 70 95
let floor_b = rgb 52 60 84
let wall_color = rgb 150 120 90
let plane_color = rgb 230 230 240
let shadow_color = rgb 20 22 40

(* A wall is blocks standing on the floor: fly over the low ones, and
 * between the tall ones. There are no windows in the fortress and
 * nothing to fly *under* -- the arcade had none either -- which is
 * also what keeps this world inside the rule the rest of games2.5d/
 * lives by: one height per point of the map, and the drawing order
 * settles everything. *)
type block = { bx0 : number; bx1 : number; bh : number }
type wall = { wz : number; blocks : block list }

let wall (wz : number) (blocks : (number * number * number) list) : wall =
  { wz; blocks = List.map (fun (bx0, bx1, bh) -> { bx0; bx1; bh }) blocks }

let walls : wall list =
  [ (* low enough to clear without touching the stick: the fortress
       lets you read your shadow once before it asks you to use it *)
    wall 700. [ (-.half_w, half_w, 30.) ];
    (* the first climb *)
    wall 1300. [ (-.half_w, half_w, 100.) ];
    (* a doorway: two towers to the ceiling, and the way is between
       them -- here it is your x that matters and your height not at all *)
    wall 1900. [ (-.half_w, -40., ceiling); (60., half_w, ceiling) ];
    wall 2500. [ (-.half_w, half_w, 140.) ];
    (* a doorway on the right, with a low step to clear beside it *)
    wall 3100. [ (-.half_w, 40., ceiling); (120., half_w, 60.) ] ]

(* what the wall does to a fighter that reaches it at (x, y) *)
let blocked (x : number) (y : number) (w : wall) : bool =
  List.exists (fun b -> x > b.bx0 && x < b.bx1 && y < b.bh) w.blocks

type kind = Fuel | Turret | Tower

(* what stands on the fortress: worth points, and the fuel tanks worth
 * the fuel that keeps you flying *)
type thing = { tx : number; tz : number; kind : kind; alive : bool }

let things : thing list =
  let at kind (tx, tz) = { tx; tz; kind; alive = true } in
  List.map (at Fuel) [ (-120., 400.); (90., 950.); (-40., 1600.); (150., 2200.); (-150., 2800.); (30., 3400.) ]
  @ List.map (at Turret) [ (60., 500.); (-90., 1100.); (120., 1750.); (-120., 2350.); (0., 2900.); (90., 3500.) ]
  @ List.map (at Tower) [ (-180., 800.); (180., 1450.); (-60., 2050.); (60., 2650.); (-180., 3250.) ]

(*****************************************************************************)
(* The projection -- the trick of this game, in 54 lines (see the header) *)
(*****************************************************************************)

(* the screen point of a world point, the camera's z taken off first:
 * no perspective, no camera, no depth -- the whole of it is these two
 * lines *)
let project (camz : number) (x : number) (y : number) (z : number) : number * number =
  let z = z - camz in
  ((0.85 * x) + (0.34 * z) - 120., (-0.30 * x) + (0.42 * z) + y - 330.)

(* a shape put where a world point is *)
let at (camz : number) ((x, y, z) : number * number * number) (s : shape) : shape =
  let sx, sy = project camz x y z in
  s |> move sx sy

(* and its shadow, the same shape flattened on the ground directly
 * below it: the gap between the two on the screen is the altitude, and
 * is the only thing that tells you what it is *)
let shadow_at (camz : number) ((x, _y, z) : number * number * number) (s : shape) : shape =
  at camz (x, 0., z) (s |> fade 0.55)

(* Everything is drawn back to front, and that is the whole of the
 * depth test: with one thing per place on the fortress, nothing can
 * cut through anything else, so the order *is* the answer. *)
let painted (l : (number * shape) list) : shape list =
  List.map snd (List.sort (fun (a, _) (b, _) -> compare b a) l)

(* One thing is outside that order, and only one: the floor, which
 * everything stands on and nothing is ever under, is painted first.
 * The fighter is *in* the sort like everything else, and has to be --
 * it is the whole point of the view. A wall is drawn over you once you
 * have passed it (it is then between you and the eye) and under you
 * while you are still coming at it, and the frame of a hole you are
 * entering cuts across your nose. Taking the fighter out of the sort
 * and always drawing it last, as sprite hardware would, is a lie the
 * player can see: you slide over the frame instead of into it. *)

(* Which way the eye is, in world coordinates: the direction the
 * projection sends to (0, 0), since walking along it keeps you on the
 * same pixel. Solving the two lines of [project] for it gives
 * (0.4, 0.54, -1) per unit of z towards the eye -- and it is a
 * direction, the same everywhere, because there is no perspective:
 * that is the one convenience the trick buys back.
 *
 * So: walk from the fighter towards the eye until z reaches a wall's,
 * see where on the wall you come out, and if that point is on the wall
 * and not in its hole, the wall is between you and the eye. *)
let hidden_by ((x, y, z) : number * number * number) (w : wall) : bool =
  let t = z - w.wz in
  t > 0.
  &&
  let hx = x + (0.4 * t) and hy = y + (0.54 * t) in
  List.exists (fun b -> hx > b.bx0 && hx < b.bx1 && hy >= 0. && hy < b.bh) w.blocks

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* a shot flies along the fortress at the height it was fired from *)
type shot = { sx : number; sy : number; sz : number; mine : bool }

type play = {
  px : number; (* the fighter, across *)
  py : number; (* and how high *)
  camz : number;
  things : thing list;
  shots : shot list;
  fuel : number;
  score : int;
  lives : int;
  run : int; (* how many times the fortress has been flown *)
  dead : int; (* frames of the explosion, 0 while flying *)
  cool : int; (* frames before the next shot *)
}

type scene = Title | Playing of play | Over of int
type model = scene Scene2d.t

let plane_z = 260. (* how far ahead of the camera the fighter flies *)
let speed (run : int) : number = 3.2 + (0.4 * float_of_int (run -.. 1))
let shot_speed = 14.
let fuel_burn = 0.035

let start_play (run : int) (score : int) (lives : int) : play =
  { px = 0.; py = 40.; camz = 0.; things; shots = []; fuel = 100.; score; lives; run; dead = 0; cool = 0 }

let start () : play = start_play 1 0 3
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let plane_z_of (p : play) : number = p.camz + plane_z

(* a thing is hit by a shot of yours that reaches it, at a height it
 * can be hit at: the towers stand tall, the tanks and turrets are low *)
let tall (k : kind) : number = match k with Tower -> 120. | Turret -> 40. | Fuel -> 40.
let points (k : kind) : int = match k with Tower -> 300 | Turret -> 200 | Fuel -> 150

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let k = computer.keyboard in
  let flying = p.dead = 0 in
  let camz = p.camz + if flying then speed p.run else 0. in
  let px = Float.max (-.half_w) (Float.min half_w (p.px + (to_x k * 3.4))) in
  let py = Float.max 0. (Float.min ceiling (p.py + (to_y k * 2.6))) in
  let px, py = if flying then (px, py) else (p.px, p.py) in
  let pz = camz + plane_z in
  (* the fighter's own shot, and everything in flight moved *)
  let firing = flying && p.cool = 0 && Scene2d.pressed (fun k -> k.kspace) scenes in
  let shots =
    (if firing then [ { sx = px; sy = py; sz = pz; mine = true } ] else [])
    @ List.map (fun s -> { s with sz = (if s.mine then s.sz + shot_speed else s.sz - shot_speed) }) p.shots
  in
  let shots = List.filter (fun s -> s.sz > camz - 200. && s.sz < camz + 1400.) shots in
  (* what the shots hit *)
  let hit (t : thing) (s : shot) : bool =
    s.mine && t.alive && Float.abs (s.sx - t.tx) < 26. && Float.abs (s.sz - t.tz) < 26. && s.sy < tall t.kind
  in
  let struck = List.filter (fun t -> t.alive && List.exists (hit t) shots) p.things in
  let things = List.map (fun t -> if List.exists (fun (u : thing) -> u == t) struck then { t with alive = false } else t) p.things in
  let shots = List.filter (fun s -> not (List.exists (fun t -> hit t s) struck)) shots in
  (* the turrets shoot back when they are in front of you *)
  let shots =
    shots
    @ List.filter_map
        (fun (t : thing) ->
          if t.kind = Turret && t.alive && t.tz - pz > 300. && t.tz - pz < 330. then
            Some { sx = t.tx; sy = 30.; sz = t.tz; mine = false }
          else None)
        things
  in
  (* what can end the flight: a wall you are not in the hole of, a
   * tower you fly into, a shot, or the fuel *)
  let crossed (w : wall) : bool = p.camz + plane_z < w.wz && pz >= w.wz in
  let into_wall = List.exists (fun w -> crossed w && blocked px py w) walls in
  let into_tower =
    List.exists (fun (t : thing) -> t.alive && t.kind = Tower && Float.abs (px - t.tx) < 26. && Float.abs (pz - t.tz) < 26. && py < tall t.kind) things
  in
  let shot_down = List.exists (fun s -> (not s.mine) && Float.abs (s.sx - px) < 22. && Float.abs (s.sz - pz) < 22. && Float.abs (s.sy - py) < 26.) shots in
  (* a fuel tank shot is fuel, which is the only way to keep flying *)
  let refuel = float_of_int (List.length (List.filter (fun (t : thing) -> t.kind = Fuel) struck)) * 25. in
  let fuel = Float.min 100. (p.fuel - (if flying then fuel_burn else 0.) + refuel) in
  let lost = flying && (into_wall || into_tower || shot_down || fuel <= 0.) in
  let scored = List.fold_left (fun n (t : thing) -> n +.. points t.kind) 0 struck in
  (* the end of the fortress: round again, faster *)
  let finished = flying && pz > run_length in
  if finished then start_play (p.run +.. 1) (p.score +.. scored +.. 1000) p.lives
  else if lost then
    { (start_play p.run (p.score +.. scored) (p.lives -.. 1)) with dead = 60; px; py; camz; things; fuel = Float.max 25. fuel }
  else if p.dead > 1 then { p with dead = p.dead -.. 1; shots = []; score = p.score +.. scored }
  else if p.dead = 1 then start_play p.run p.score p.lives
  else
    { p with px; py; camz; things; shots; fuel; score = p.score +.. scored;
      cool = (if firing then 12 else max 0 (p.cool -.. 1)) }

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

(* the fortress floor, in slices: they are the only thing that says you
 * are moving, as Out Run's stripes are *)
let floor_slices (camz : number) : (number * shape) list =
  List.init 14 (fun i ->
      let z = (Float.round ((camz - 200.) / 100.) * 100.) + (float_of_int i * 100.) in
      let corner x zz = project camz x 0. zz in
      let quad = polygon (if Float.rem (z / 100.) 2. = 0. then floor_a else floor_b)
          [ corner (-.half_w) z; corner half_w z; corner half_w (z + 100.); corner (-.half_w) (z + 100.) ]
      in
      (z, quad))

(* a wall is one quad per block, each standing on the floor *)
let wall_shapes (camz : number) (w : wall) : (number * shape) list =
  List.map
    (fun b ->
      let p0 = project camz b.bx0 0. w.wz
      and p1 = project camz b.bx1 0. w.wz
      and p2 = project camz b.bx1 b.bh w.wz
      and p3 = project camz b.bx0 b.bh w.wz in
      (w.wz, polygon wall_color [ p0; p1; p2; p3 ]))
    w.blocks

let thing_shape (t : thing) : shape =
  match t.kind with
  | Fuel -> group [ rectangle (rgb 220 140 60) 26. 30.; rectangle (rgb 250 200 90) 26. 6. |> move_y 14. ]
  | Turret -> group [ rectangle (rgb 170 170 185) 28. 14.; rectangle (rgb 120 120 140) 6. 16. |> move_y 12. ]
  | Tower -> group [ rectangle (rgb 150 155 175) 16. 90. |> move_y 45.; circle (rgb 230 90 80) 9. |> move_y 92. ]

let plane_body (color : color) : shape =
  group [ polygon color [ (-16., 0.); (16., 0.); (10., 8.); (-10., 8.) ]; rectangle color 10. 26. ]

let plane_shape : shape = group [ plane_body plane_color; rectangle (rgb 90 150 230) 8. 8. |> move_y 6. ]

(* the shadow is a silhouette, not a paler fighter: two of the same
 * picture on the screen is exactly the confusion it is there to end *)
let plane_shadow : shape = plane_body shadow_color |> fade 0.7

(* the arcade's other answer to "how high am I": the gauge up the side,
 * the fighter's altitude against the wall's hole *)
let altimeter (p : play) : shape list =
  let x = -430. and h = 300. in
  let mark (y : number) = (y / ceiling * h) - (h / 2.) in
  (* how high the next wall is *where you are*: the block your x runs
   * into, or nothing at all if you are lined up with a gap. The gauge
   * therefore answers both questions the fortress asks, "climb" and
   * "move across", with one line *)
  let need =
    match List.find_opt (fun (w : wall) -> w.wz > p.camz + plane_z) walls with
    | Some w -> List.fold_left (fun m b -> if p.px > b.bx0 && p.px < b.bx1 then Float.max m b.bh else m) 0. w.blocks
    | None -> 0.
  in
  [ rectangle (rgb 40 45 70) 26. h |> move x 0.;
    rectangle (rgb 90 200 120) 26. 4. |> move x (mark 0.) ]
  @ (if need > 0. then [ rectangle (rgb 230 90 80) 26. 4. |> move x (mark need) ] else [])
  @ [ triangle (rgb 240 240 250) 10. |> rotate (-90.) |> move (x + 22.) (mark p.py) ]

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let camz = p.camz in
  let pz = camz + plane_z in
  let visible (z : number) = z > camz - 200. && z < camz + 1300. in
  let world =
    List.concat_map (fun (w : wall) -> if visible w.wz then wall_shapes camz w else []) walls
    @ List.concat_map
        (fun (t : thing) ->
          if (not t.alive) || not (visible t.tz) then []
          else [ (t.tz, at camz (t.tx, 0., t.tz) (thing_shape t)) ])
        p.things
    @ List.concat_map
        (fun (s : shot) ->
          if not (visible s.sz) then []
          else
            [ (s.sz, at camz (s.sx, s.sy, s.sz) (circle (if s.mine then rgb 250 240 120 else rgb 250 110 90) 5.));
              (s.sz, shadow_at camz (s.sx, s.sy, s.sz) (circle shadow_color 4.)) ])
        p.shots
  in
  (* the fighter and its shadow take their places in the sort, the
   * fighter a hair nearer than the shadow it casts *)
  let fighter =
    if p.dead > 0 then [ (pz, at camz (p.px, p.py, pz) (circle (rgb 250 180 60) (float_of_int (60 -.. p.dead) + 6.) |> fade 0.8)) ]
    else [ (pz, shadow_at camz (p.px, p.py, pz) plane_shadow); (pz -. 0.5, at camz (p.px, p.py, pz) plane_shape) ]
  in
  (* and when a wall you have flown past is over you, the fighter is
   * drawn again, faintly, on top of it: correct is not the same as
   * playable, and every isometric game from Knight Lore to Diablo
   * shows you the thing the wall is hiding *)
  let ghost =
    if p.dead > 0 || not (List.exists (hidden_by (p.px, p.py, pz)) walls) then []
    else [ at camz (p.px, p.py, pz) (plane_shape |> fade 0.3) ]
  in
  (rectangle sky screen.width screen.height :: painted (floor_slices camz))
  @ painted (world @ fighter)
  @ ghost
  @ altimeter p
  @ [ text white 2.2 (Printf.sprintf "score %d    run %d" p.score p.run) |> move_y (screen.top - 40.);
      text (if p.fuel < 25. then rgb 250 110 90 else rgb 120 220 140) 2.2
        (Printf.sprintf "fuel %3.0f" p.fuel)
      |> move_y (screen.top - 75.);
      text plane_color 2.2 (String.concat " " (List.init (max 0 p.lives) (fun _ -> "^"))) |> move_y (screen.top - 110.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      [ rectangle sky screen.width screen.height;
        text white 6. "TINY ZAXXON" |> move_y 230.;
        text white 2. "left/right: across    up/down: higher and lower    space: fire" |> move_y 90.;
        text white 2. "the walls have one hole each: fly through it" |> move_y 40.;
        text (rgb 120 220 140) 2. "your shadow on the fortress is how high you are" |> move_y (-20.);
        text (rgb 250 200 90) 2. "shoot the fuel tanks, or you come down anyway" |> move_y (-70.) ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-230.) ]
  | Playing p -> view_play computer p
  | Over score ->
      [ rectangle sky screen.width screen.height;
        text white 5. "GAME OVER" |> move_y 120.;
        text white 3. (Printf.sprintf "score %d" score) |> move_y 20. ]
      @ Scene2d.blink 1. model [ text white 3. "PRESS SPACE" |> move_y (-230.) ]

let help =
  {|TinyZaxxon
  left/right  across the fortress
  up/down     higher and lower -- watch the shadow, not the plane
  space       fire (space also starts and restarts)
  the walls have one hole each; the fuel tanks are the fuel
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
