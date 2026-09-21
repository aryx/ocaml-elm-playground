(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Monument Valley (ustwo, 2014): walk a silent figure
 * over architecture that cannot exist, by believing the picture rather
 * than the world.
 *
 *   click     walk there, if there is a way
 *   left/right   turn the piece that turns
 *
 * The whole game is one rule, and the rule is a lie the projection
 * tells. This world is drawn with an **orthographic** camera
 * ([Camera3d.orthographic]): no perspective, nothing shrinking with
 * distance, the view direction exactly (1, 1, 1). A camera like that
 * cannot tell two points apart when one is the other plus (k, k, k) --
 * they land on the same pixel, for every k. So a block on the ground
 * and a block three along, three up and three away are, on the screen,
 * the same place:
 *
 *        the picture                       the world
 *                                             y
 *         +--+                                |   o  (3, 3, 3)
 *        /  /|                                |  /
 *       +--+ |    <- one block, or two?       | /
 *       |  |/                                 |/
 *       +--+                                  o------ x   (0, 0, 0)
 *                                            /
 *                                           z    three apart, drawn
 *                                                on one pixel
 *
 * And the game takes the picture's side: two blocks are walkable
 * neighbours if they *look* adjacent ([connected]) -- either they
 * really are, one step along an axis, or they are drawn on the same
 * pixel, however far apart they are in the world. Then walking is an
 * ordinary breadth-first search over that graph ([route]), and the
 * figure steps off a low path onto a high one with nothing in
 * between, because from here there is nothing in between.
 *
 * That is Escher's trick (the Penrose stairs of "Ascending and
 * Descending", 1960, and the waterfall that feeds itself, 1961), which
 * ustwo turned into a game by adding the second half: a piece you can
 * *turn*, which decides which of the impossible connections exists at
 * the moment. Turn the bridge and one gap closes while another opens;
 * nothing about the world changed except which lie the camera is
 * telling ([rotate_part]).
 *
 * It is the opposite lesson to games2.5d/TinyZaxxon.ml, and the two are
 * worth reading together. Zaxxon has the same projection with no depth
 * in it and treats the ambiguity as the problem to be solved -- the
 * shadow under the fighter exists only to tell you which of the
 * possible heights you are at. Monument Valley treats the identical
 * ambiguity as the material: it never resolves it, it builds with it.
 * One projection, two games, opposite conclusions.
 *
 * What it uses: playground3d and the orthographic camera this game is
 * what got added (see Camera3d.orthographic and
 * graphics/3d/geometry/Camera.mli's [ortho]); [Playground3d.project],
 * which the game calls itself, once per block per frame, because the
 * screen is where the rules live; and Scene2d. No kit: the isometric
 * kit (kits/isometric) is the same projection done by hand on the 2D
 * playground, and this game deliberately has the engine do it instead,
 * so that the z-buffer sorts the blocks and the game can think about
 * nothing but the graph.
 *
 * Exercises: pieces that slide as well as turn; a second figure (the
 * crow people, who walk their own paths and block yours); doors that
 * open only from one side of the illusion; a level where the goal is
 * reachable two ways and only one of them is the "impossible" one; and
 * the water, which in the real game reflects a world that is not
 * there.
 *)
open Playground
open Playground3d
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The monument *)
(*****************************************************************************)

type block = {
  bx : number;
  by : number;
  bz : number;
  (* 0 is masonry that never moves; 1 is the piece that turns *)
  part : int;
}

type level = { blocks : block list; start : int; goal : int; pivot : number * number * number }

let at (x, y, z) (part : int) : block = { bx = float_of_int x; by = float_of_int y; bz = float_of_int z; part }

(* a straight run of blocks from a corner, [n] of them *)
let run (x, y, z) (dx, dy, dz) (n : int) (part : int) : block list =
  List.init n (fun i -> at (x +.. (dx *.. i), y +.. (dy *.. i), z +.. (dz *.. i)) part)

(* The first monument: a path along the ground, and a terrace three
 * blocks up and three blocks away, which is to say *on the same
 * pixels* as the end of the path. Nothing turns here; the level exists
 * to show the rule once. *)
let level1 : level =
  let ground = run (0, 0, 0) (1, 0, 0) 5 0 in
  let terrace = run (7, 3, 3) (1, 0, 0) 3 0 in
  { blocks = ground @ terrace; start = 0; goal = List.length ground +.. 2; pivot = (0., 0., 0.) }

(* The second: the same trick, with the bridge that turns deciding
 * which terrace it is told about. Pointing along x its far end lines
 * up with the east terrace; turned a quarter, with the north one --
 * and the goal is on the north. *)
let level2 : level =
  let ground = run (0, 0, 0) (1, 0, 0) 4 0 in
  let bridge = run (4, 0, 0) (1, 0, 0) 3 1 in
  let east = run (9, 3, 3) (1, 0, 0) 3 0 in
  let north = run (7, 3, 5) (0, 0, 1) 3 0 in
  { blocks = ground @ bridge @ east @ north;
    start = 0;
    goal = List.length ground +.. List.length bridge +.. List.length east +.. 2;
    pivot = (4., 0., 0.) }

let levels = [| level1; level2 |]

(* a part turned a quarter at a time about its pivot: the only thing in
 * the game that changes the world rather than the picture of it *)
let rotate_part (l : level) (turns : int) (b : block) : block =
  if b.part = 0 then b
  else
    let px, _, pz = l.pivot in
    let dx = b.bx - px and dz = b.bz - pz in
    let dx, dz =
      match turns mod 4 with 1 -> (-.dz, dx) | 2 -> (-.dx, -.dz) | 3 -> (dz, -.dx) | _ -> (dx, dz)
    in
    { b with bx = px + dx; bz = pz + dz }

let placed (l : level) (turns : int) : block list = List.map (rotate_part l turns) l.blocks

(*****************************************************************************)
(* The camera, and the rule it makes possible *)
(*****************************************************************************)

(* The view direction is (1, 1, 1) exactly, and there is no perspective
 * in it: that pair of facts is the game. *)
let monument_camera (l : level) (turns : int) : camera =
  let blocks = placed l turns in
  let n = float_of_int (max 1 (List.length blocks)) in
  let cx = List.fold_left (fun a b -> a + b.bx) 0. blocks / n in
  let cy = List.fold_left (fun a b -> a + b.by) 0. blocks / n in
  let cz = List.fold_left (fun a b -> a + b.bz) 0. blocks / n in
  Camera3d.from_far ~offset:(22., 22., 22.) (cx, cy, cz) |> Camera3d.orthographic ~height:15.

(* where a block is drawn, in pixels *)
let on_screen (cam : camera) (screen : screen) (b : block) : (number * number) option =
  project cam screen (b.bx, b.by + 0.5, b.bz)

(* One step along an axis is a step. And so -- this is the whole game --
 * is a pair of blocks drawn on the same pixel, however far apart they
 * are in the world: the picture says they touch, and the picture is
 * what the figure believes. *)
let connected (cam : camera) (screen : screen) (a : block) (b : block) : bool =
  let step = Float.abs (a.bx - b.bx) + Float.abs (a.by - b.by) + Float.abs (a.bz - b.bz) in
  if step = 1. then true
  else
    match (on_screen cam screen a, on_screen cam screen b) with
    | Some (ax, ay), Some (bx, by) -> Float.hypot (ax - bx) (ay - by) < 6. && step > 1.
    | _ -> false

(* the way from one block to another over that graph: a plain
 * breadth-first search, which is all the pathfinding an illusion needs *)
let route (cam : camera) (screen : screen) (blocks : block list) (from : int) (goal : int) : int list =
  let n = List.length blocks in
  let arr = Array.of_list blocks in
  let came = Array.make n (-1) in
  let seen = Array.make n false in
  let queue = Queue.create () in
  Queue.add from queue;
  seen.(from) <- true;
  let found = ref false in
  while (not !found) && not (Queue.is_empty queue) do
    let i = Queue.pop queue in
    if i = goal then found := true
    else
      Array.iteri
        (fun j b ->
          if (not seen.(j)) && connected cam screen arr.(i) b then begin
            seen.(j) <- true;
            came.(j) <- i;
            Queue.add j queue
          end)
        arr
  done;
  if not seen.(goal) then []
  else
    let rec back (i : int) (acc : int list) = if i = from then acc else back came.(i) (i :: acc) in
    back goal []

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  level : int;
  turns : int; (* quarters the turning piece has been turned *)
  (* where the figure stands, and where it is going *)
  here : int;
  path : int list;
  step : number; (* 0 to 1 between two blocks *)
  turning : int; (* frames of the piece still swinging, for the drawing *)
  arrived : int; (* frames since the goal was reached *)
}

type scene = Title | Playing of play | Done
type model = scene Scene2d.t

let frames_per_step = 16.

let enter (level : int) : play =
  { level; turns = 0; here = levels.(level).start; path = []; step = 0.; turning = 0; arrived = 0 }

let start () : play = enter 0
let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let update_play (computer : computer) (scenes : model) (p : play) : play =
  let l = levels.(p.level) in
  let screen = computer.screen in
  let cam = monument_camera l p.turns in
  let blocks = placed l p.turns in
  let turn =
    (if Scene2d.pressed (fun k -> k.kright) scenes then 1 else 0)
    -.. if Scene2d.pressed (fun k -> k.kleft) scenes then 1 else 0
  in
  (* turning the piece under your feet would be unkind, and turning it
   * while walking would be worse: it waits until the figure stands still *)
  let turning_allowed = p.path = [] && p.step = 0. in
  let turns = if turn <> 0 && turning_allowed then p.turns +.. turn +.. 4 else p.turns in
  let p = { p with turns; turning = (if turns <> p.turns then 14 else max 0 (p.turning -.. 1)) } in
  (* a click picks the block drawn nearest the mouse, and the search
   * says whether the picture allows it *)
  let p =
    if (not computer.mouse.mclick) || p.path <> [] then p
    else
      let best = ref None in
      List.iteri
        (fun i b ->
          match on_screen cam screen b with
          | Some (x, y) ->
              let d = Float.hypot (x - computer.mouse.mx) (y - computer.mouse.my) in
              if d < 26. then (match !best with Some (bd, _) when bd <= d -> () | _ -> best := Some (d, i))
          | None -> ())
        blocks;
      match !best with
      | Some (_, i) when i <> p.here -> { p with path = route cam screen blocks p.here i }
      | _ -> p
  in
  (* the figure steps from block to block, a fixed number of frames a
   * step, so that a step across the impossible gap takes exactly as
   * long as an ordinary one -- which is what sells it *)
  let p =
    match p.path with
    | [] -> p
    | next :: rest ->
        let step = p.step + (1. / frames_per_step) in
        if step >= 1. then { p with here = next; path = rest; step = 0. } else { p with step }
  in
  if p.here = l.goal && p.path = [] then { p with arrived = p.arrived +.. 1 } else { p with arrived = 0 }

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  match scenes.scene with
  | Title ->
      if Scene2d.pressed (fun k -> k.kspace) scenes || computer.mouse.mclick then Scene2d.go (Playing (start ())) scenes
      else scenes
  | Done ->
      if Scene2d.pressed (fun k -> k.kspace) scenes || computer.mouse.mclick then Scene2d.go (Playing (start ())) scenes
      else scenes
  | Playing p ->
      let p = update_play computer scenes p in
      if p.arrived > 50 then
        if p.level +.. 1 < Array.length levels then Scene2d.go (Playing (enter (p.level +.. 1))) scenes
        else Scene2d.go Done scenes
      else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let sand = rgb 236 222 200
let sand_top = rgb 250 242 226
let stone = rgb 196 178 196
let stone_top = rgb 218 204 220
let goal_color = rgb 240 190 90
let figure = rgb 250 250 252

let block_shape (b : block) (is_goal : bool) : shape3d =
  let side, top = if b.part = 0 then (sand, sand_top) else (stone, stone_top) in
  group3d
    [ box side 1. 0.9 1. |> move3d 0. (-0.05) 0.;
      box (if is_goal then goal_color else top) 1. 0.12 1. |> move3d 0. 0.45 0. ]
  |> move3d b.bx b.by b.bz

(* the figure: where it stands, or part way between two blocks -- and
 * between two blocks that only the picture joins, it walks through
 * whatever is in the way, because in the world there is nothing there *)
let figure_shape (blocks : block list) (p : play) : shape3d =
  let arr = Array.of_list blocks in
  let a = arr.(p.here) in
  let bx, by, bz =
    match p.path with
    | next :: _ ->
        let b = arr.(next) in
        (a.bx + ((b.bx - a.bx) * p.step), a.by + ((b.by - a.by) * p.step), a.bz + ((b.bz - a.bz) * p.step))
    | [] -> (a.bx, a.by, a.bz)
  in
  group3d [ box figure 0.3 0.55 0.3 |> move3d 0. 0.78 0.; sphere figure 0.19 |> move3d 0. 1.18 0. ]
  |> move3d bx by bz

let view_play (computer : computer) (p : play) : camera * shape3d list =
  let l = levels.(p.level) in
  let screen = computer.screen in
  let cam = monument_camera l p.turns in
  let blocks = placed l p.turns in
  ( cam,
    List.mapi (fun i b -> block_shape b (i = l.goal)) blocks
    @ [ figure_shape blocks p ]
    @ List.map hud
        [ text (rgb 90 80 100) 2.2 (Printf.sprintf "monument %d of %d" (p.level +.. 1) (Array.length levels))
          |> move_y (screen.top - 45.);
          text (rgb 140 130 150) 1.7
            (if l.pivot = (0., 0., 0.) then "click a stone to walk there -- believe the picture"
             else "click to walk    left/right: turn the pale piece")
          |> move_y (screen.bottom + 30.) ] )

let view (computer : computer) (model : model) : camera * shape3d list =
  let screen = computer.screen in
  match model.scene with
  | Title ->
      let cam = monument_camera levels.(0) 0 in
      ( cam,
        List.mapi (fun i b -> block_shape b (i = levels.(0).goal)) (placed levels.(0) 0)
        @ List.map hud
            ([ text (rgb 70 60 80) 5. "TINY MONUMENT VALLEY" |> move_y 220.;
               text (rgb 100 90 115) 2. "the camera has no perspective, so it cannot tell" |> move_y 80.;
               text (rgb 100 90 115) 2. "a near block from a far one drawn on the same pixel" |> move_y 40.;
               text (rgb 150 110 60) 2. "and neither can the figure: she walks where it looks like she can" |> move_y (-20.) ]
            @ Scene2d.blink 1. model [ text (rgb 70 60 80) 3. "CLICK TO BEGIN" |> move_y (-200.) ]) )
  | Playing p -> view_play computer p
  | Done ->
      let cam = monument_camera levels.(Array.length levels -.. 1) 0 in
      ( cam,
        List.map hud
          ([ text (rgb 70 60 80) 4. "THE MONUMENTS ARE WALKED" |> move_y 60.;
             text (rgb 120 110 130) 2. "nothing impossible happened; the camera only left something out"
             |> move_y (-10.) ]
          @ Scene2d.blink 1. model [ text (rgb 70 60 80) 3. "CLICK TO WALK THEM AGAIN" |> move_y (-180.) ])
        @ [ hud (rectangle (rgb 250 246 240) screen.width screen.height |> fade 0.6) ] )

let help =
  {|TinyMonumentValley
  click        walk there, if the picture says there is a way
  left/right   turn the pale piece (the figure has to be standing still)
  the camera has no perspective: blocks three apart along (1,1,1) are
  drawn on one pixel, and the game lets you walk between them
|}

let app = game3d view update initial_model

let main =
  print_string help;
  Playground3d_platform.run_app3d app
