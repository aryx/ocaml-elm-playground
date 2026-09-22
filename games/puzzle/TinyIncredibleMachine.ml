(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of The Incredible Machine (Kevin Ryan, Jeff Tunnell,
 * Dynamix / Sierra, 1993): a machine to finish, from a bin of parts,
 * that must do one thing -- put a ball in a basket -- and then runs on
 * its own. Click a part in the bin, click where it goes (r tilts a
 * ramp, a click on a placed part takes it back); space starts the
 * machine, and stops it; n the next puzzle.
 *
 * A game of Rube Goldberg's cartoons, where a bowling ball rolls down a
 * ramp onto a seesaw that throws a ball into a bucket that pulls a
 * rope... What is new here, against every other game of this
 * directory:
 *
 *  - Building, then watching. There are two modes ([scene]): in the
 *    first you place the parts and nothing moves; in the second the
 *    machine runs and you can touch nothing. The whole game is design,
 *    none of it reflexes, and so the physics must be *deterministic*:
 *    the machine that worked must work again the same, frame for frame
 *    (a test runs every puzzle's solution twice). Stopping puts
 *    everything back where it was, as in the original.
 *
 *  - Joints, the engine's new piece (physics/2d/Joint2d, through
 *    Physics.pin, rope, pulley): a seesaw is a plank on a *pin*, free
 *    to turn about it; a pulley is two ropes over two fixed wheels,
 *    what one side gains the other giving up -- a bowling ball in the
 *    bucket, and the tray on the other side goes up. Solved with the
 *    contacts, in the solver's loop:
 *
 *         pulley                       seesaw
 *      ( )-------( )
 *       |         |                 ===========
 *     [bucket]  [tray]  <- a ball         ^ pin
 *
 *  - Parts that act on each other beyond touching: a switch, pressed by
 *    whatever lands on it, turns a fan on, and the fan pushes whatever
 *    is in its wind ([machine]'s step). Events on top of the physics:
 *    the Rube Goldberg part.
 *
 * What it uses: Physics (its world, simulate, and the joints: pin,
 * pulley), Scene2d. No kit: a puzzle is data ([puzzles]), its parts
 * bodies.
 *
 * Exercises: more parts (the conveyor belt, a pin with a motor that a
 * switch turns on; the candle burning a rope, a rope that stops being
 * one; the mouse in its cage, a motor that runs when a cheese falls);
 * a puzzle editor (the free-form mode of the original); a score for
 * using few parts; saving your machines.
 *)
open Playground

(*****************************************************************************)
(* The parts *)
(*****************************************************************************)

(* what the bin holds: the parts you place *)
type part = Ramp | Trampoline | Seesaw

type placed = { part : part; x : number; y : number; angle : number }

let part_name = function Ramp -> "ramp" | Trampoline -> "trampoline" | Seesaw -> "seesaw"

(* the moving things a puzzle starts with *)
type ball = Bowling | Basketball | Balloon

(* a machine a puzzle comes with *)
type contraption =
  | Pulley of { left : number * number; right : number * number; top : number } (* the bucket, the tray, the wheels' height *)
  | Fan of { switch : number * number; fan : number * number } (* the switch plate on the floor, the fan blowing right *)
  | Fixed_seesaw of number * number

type puzzle = {
  title : string;
  hint : string;
  walls : (number * number * number * number * number) list; (* x, y, width, height, angle: the scenery *)
  balls : (ball * number * number) list;
  target : int; (* which of [balls] must reach the goal *)
  goal : number * number * number * number; (* x, y, width, height *)
  machines : contraption list;
  bin : (part * int) list;
  solution : placed list;
}

let floor_y = -330.
let field = (-460., 460., floor_y, 400.)

(* the four walls round the field, and the puzzle's own *)
let frame = [ (0., floor_y -. 10., 940., 20., 0.); (-470., 35., 20., 750., 0.); (470., 35., 20., 750., 0.); (0., 410., 940., 20., 0.) ]

(*****************************************************************************)
(* The puzzles *)
(*****************************************************************************)

let puzzles : puzzle list =
  [ { title = "1. DOWN AND OVER";
      hint = "the ball must clear the wall: give it a ramp";
      walls = [ (-80., -240., 40., 180., 0.) ];
      balls = [ (Bowling, -380., 300.) ];
      target = 0;
      goal = (395., -280., 130., 80.);
      machines = [];
      bin = [ (Ramp, 2) ];
      solution = [ { part = Ramp; x = -350.; y = 150.; angle = -20. } ] };
    { title = "2. THE SEESAW";
      hint = "drop the bowling ball on the seesaw's end";
      (* a basket in the corner: a shelf, a lip at its front, the wall
       * behind; the shelf runs into the wall, as a shelf's corner left
       * against a wall is a seam a ball can be thrown out of (the
       * contacts are found from the polygons' corners, Collide.manifold) *)
      walls = [ (-400., 0., 160., 16., 0.); (-315., 20., 10., 30., 0.) ];
      balls = [ (Bowling, 100., 330.); (Basketball, 60., -250.) ];
      target = 1;
      goal = (-390., 50., 140., 90.);
      machines = [ Fixed_seesaw (-40., -290.) ];
      bin = [ (Ramp, 2) ];
      solution = [ { part = Ramp; x = 130.; y = 200.; angle = 15. } ] };
    { title = "3. THE PULLEY";
      hint = "a heavy ball in the bucket, and up goes the tray";
      walls = [];
      balls = [ (Bowling, -420., 330.); (Basketball, 250., -290.) ];
      target = 1;
      goal = (250., 170., 120., 120.);
      machines = [ Pulley { left = (-250., 150.); right = (250., -300.); top = 340. } ];
      bin = [ (Ramp, 1) ];
      solution = [ { part = Ramp; x = -380.; y = 240.; angle = -10. } ] };
    { title = "4. THE FAN";
      hint = "press the switch: the fan blows the balloon out from under the ledge";
      (* the ledge the balloon is caught under, and a slanted ceiling
       * leading it up to the goal once it is out *)
      walls = [ (-240., 180., 400., 16., 0.); (210., 320., 540., 12., 12.) ];
      balls = [ (Bowling, 400., 250.); (Balloon, -250., 60.) ];
      target = 1;
      goal = (350., 340., 200., 100.);
      (* the fan at the height the balloon floats at, under the ledge *)
      machines = [ Fan { switch = (150., floor_y +. 6.); fan = (-440., 150.) } ];
      bin = [ (Ramp, 1) ];
      solution = [ { part = Ramp; x = 400.; y = 50.; angle = 25. } ] } ]

(*****************************************************************************)
(* The machine, as a world *)
(*****************************************************************************)

(* what each body of the world is, by its place in the list *)
type role = Scenery | A_ball of ball | Plank | Bucket | Tray | Switch

type machine = {
  world : Physics.world;
  roles : role list;
  fan_on : bool;
  frames : int;
  in_goal : int; (* frames the target has been in the goal *)
}

let wall_color = rgb 150 110 80
let solid (color : color) ((x, y, w, h, angle) : number * number * number * number * number) : Physics.body =
  Physics.body (rectangle color w h) |> Physics.at x y |> Physics.pointing angle |> Physics.immovable |> Physics.rough 0.6

(* A ball's body is its circle alone: its shape is its hitbox too, and
 * the seams of a basketball as rectangles poke out of the circle by a
 * hair -- a sliver 2 wide that a wall's contact threw through the wall
 * at 2600 pixels a second. The marks are drawn on top ([marks]). *)
let ball_body (kind : ball) (x : number) (y : number) : Physics.body =
  match kind with
  | Bowling -> Physics.body (circle (rgb 40 40 60) 18.) |> Physics.heavy 5. |> Physics.rough 0.5 |> Physics.at x y
  | Basketball -> Physics.body (circle (rgb 230 120 40) 14.) |> Physics.heavy 0.6 |> Physics.bouncy 0.4 |> Physics.rough 0.6 |> Physics.at x y
  | Balloon -> Physics.body (oval (rgb 230 60 90) 30. 36.) |> Physics.heavy 0.2 |> Physics.at x y |> Physics.upright

(* a cup: a floor and two sides, open at the top *)
let cup (color : color) (w : number) (h : number) : shape =
  group [ rectangle color w 8. |> move 0. (-.h /. 2.); rectangle color 8. h |> move (-.w /. 2.) 0.; rectangle color 8. h |> move (w /. 2.) 0. ]

(* The world for a puzzle and the parts placed: the scenery, the parts,
 * the balls, the machines' bodies and their joints *)
let build (p : puzzle) (parts : placed list) : machine =
  let bodies = ref [] and roles = ref [] and joints = ref [] in
  let add role b = bodies := !bodies @ [ b ]; roles := !roles @ [ role ]; List.length !bodies - 1 in
  List.iter (fun w -> ignore (add Scenery (solid wall_color w))) (frame @ p.walls);
  let seesaw x y =
    let pivot = add Scenery (Physics.body (triangle (rgb 90 90 100) 16.) |> Physics.at x (y -. 6.) |> Physics.immovable) in
    (* a lip at its right end, so that what rests there stays *)
    let board = group [ rectangle (rgb 200 170 90) 240. 10.; rectangle (rgb 200 170 90) 8. 24. |> move 116. 12. ] in
    let plank = add Plank (Physics.body board |> Physics.heavy 1.5 |> Physics.rough 0.6 |> Physics.at x (y +. 12.)) in
    joints := !joints @ [ `Pin (pivot, plank, (x, y +. 12.)) ]
  in
  List.iter
    (fun (q : placed) ->
      match q.part with
      | Ramp -> ignore (add Scenery (solid (rgb 120 160 200) (q.x, q.y, 170., 10., q.angle)))
      | Trampoline ->
          ignore
            (add Scenery
               (Physics.body (group [ rectangle (rgb 60 60 70) 90. 8.; rectangle (rgb 60 200 90) 80. 4. |> move 0. 5. ])
               |> Physics.at q.x q.y |> Physics.immovable |> Physics.bouncy 1.1))
      | Seesaw -> seesaw q.x q.y)
    parts;
  List.iter (fun (kind, x, y) -> ignore (add (A_ball kind) (ball_body kind x y))) p.balls;
  List.iter
    (function
      | Fixed_seesaw (x, y) -> seesaw x y
      | Pulley { left = (lx, ly); right = (rx, ry); top } ->
          let bucket = add Bucket (Physics.body (cup (rgb 110 80 60) 60. 40.) |> Physics.heavy 1. |> Physics.rough 0.6 |> Physics.upright |> Physics.at lx ly) in
          let tray = add Tray (Physics.body (cup (rgb 90 110 140) 70. 26.) |> Physics.heavy 1.2 |> Physics.rough 0.8 |> Physics.upright |> Physics.at rx ry) in
          joints := !joints @ [ `Pulley (bucket, tray, (lx, ly +. 20.), (rx, ry +. 13.), (lx, top), (rx, top)) ]
      | Fan { switch = (sx, sy); _ } ->
          ignore (add Switch (Physics.body (rectangle (rgb 200 60 60) 60. 8.) |> Physics.at sx sy |> Physics.immovable)))
    p.machines;
  let world =
    List.fold_left
      (fun w j ->
        match j with
        | `Pin (a, b, at) -> Physics.pin a b ~at w
        | `Pulley (a, b, at_a, at_b, ground_a, ground_b) -> Physics.pulley a b ~at_a ~at_b ~ground_a ~ground_b w)
      (Physics.world !bodies) !joints
  in
  { world; roles = !roles; fan_on = false; frames = 0; in_goal = 0 }

let gravity = 800.

(* the balls' own indices, in [p.balls]' order *)
let ball_indices (m : machine) : int list =
  List.concat (List.mapi (fun i r -> match r with A_ball _ -> [ i ] | _ -> []) m.roles)

let inside ((gx, gy, gw, gh) : number * number * number * number) (b : Physics.body) : bool =
  Float.abs (b.x -. gx) < gw /. 2. && Float.abs (b.y -. gy) < gh /. 2.

(* One frame of the machine: the balloon's lift and the fan's wind as
 * pushes, the world simulated, the switch looked at, the goal too *)
let step (p : puzzle) (m : machine) : machine =
  let fan = List.find_map (function Fan { fan; switch } -> Some (fan, switch) | _ -> None) p.machines in
  let blow (b : Physics.body) =
    match fan with
    | Some ((fx, fy), _) when m.fan_on && b.mass <> infinity && b.x > fx && b.x < fx +. 520. && Float.abs (b.y -. fy) < 70. ->
        Physics.push (260. *. b.mass) 0. b
    | _ -> b
  in
  let bodies =
    List.map2
      (fun r (b : Physics.body) ->
        let b = match r with A_ball Balloon -> Physics.push 0. (1300. *. b.mass) b | _ -> b in
        blow b)
      m.roles m.world.bodies
  in
  let world = Physics.simulate ~gravity { m.world with bodies } in
  (* the switch: whatever moving thing rests on it *)
  let pressed =
    match fan with
    | None -> false
    | Some (_, (sx, sy)) ->
        List.exists2 (fun r (b : Physics.body) -> (match r with A_ball _ -> true | _ -> false) && Float.abs (b.x -. sx) < 36. && b.y -. sy < 40. && b.y > sy) m.roles world.bodies
  in
  let target = List.nth world.bodies (List.nth (ball_indices m) p.target) in
  { world; roles = m.roles; fan_on = m.fan_on || pressed; frames = m.frames + 1; in_goal = (if inside p.goal target then m.in_goal + 1 else 0) }

let solved (m : machine) : bool = m.in_goal >= 30

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type desk = { level : int; parts : placed list; picked : part option; tilt : number; running : machine option; won : bool }
type scene = Title | Desk of desk
type model = scene Scene2d.t

let puzzle (d : desk) : puzzle = List.nth puzzles d.level
let new_desk (level : int) : desk = { level; parts = []; picked = None; tilt = -20.; running = None; won = false }
let initial_model : model = Scene2d.start Title

let left_in_bin (d : desk) (part : part) : int =
  let total = Option.value ~default:0 (List.assoc_opt part (puzzle d).bin) in
  total - List.length (List.filter (fun (q : placed) -> q.part = part) d.parts)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let bin_y = -410.
let slot_x (i : int) : number = -330. +. (float_of_int i *. 180.)
let snap (v : number) : number = Float.round (v /. 10.) *. 10.

let in_field (x : number) (y : number) : bool =
  let l, r, b, t = field in
  x > l && x < r && y > b && y < t

(* a click, in the edit mode: on the bin, a part picked; on a placed
 * part, taken back; elsewhere in the field, the picked part put there *)
let click (d : desk) (x : number) (y : number) : desk =
  let bin = (puzzle d).bin in
  match List.find_opt (fun (i, _) -> Float.abs (x -. slot_x i) < 80. && Float.abs (y -. bin_y) < 30.) (List.mapi (fun i b -> (i, b)) bin) with
  | Some (_, (part, _)) -> { d with picked = Some part }
  | None -> (
      match List.find_opt (fun (q : placed) -> Float.hypot (q.x -. x) (q.y -. y) < 40.) d.parts with
      | Some q -> { d with parts = List.filter (fun q' -> q' != q) d.parts }
      | None -> (
          match d.picked with
          | Some part when in_field x y && left_in_bin d part > 0 ->
              { d with parts = d.parts @ [ { part; x = snap x; y = snap y; angle = (if part = Ramp then d.tilt else 0.) } ] }
          | _ -> d))

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let key f = Scene2d.pressed f m in
  let letter l = key (fun k -> Set_.mem l k.keys) in
  match m.scene with
  | Title -> if key (fun k -> k.kspace) then Scene2d.go (Desk (new_desk 0)) m else m
  | Desk d ->
      let d =
        if letter "n" then new_desk ((d.level + 1) mod List.length puzzles)
        else if key (fun k -> k.kspace) then
          match d.running with None -> { d with running = Some (build (puzzle d) d.parts) } | Some _ -> { d with running = None; won = false }
        else d
      in
      let d =
        match d.running with
        | Some run ->
            let run = step (puzzle d) run in
            { d with running = Some run; won = d.won || solved run }
        | None ->
            let d = if letter "r" then { d with tilt = (if d.tilt >= 30. then -30. else d.tilt +. 10.) } else d in
            if computer.mouse.mclick then click d computer.mouse.mx computer.mouse.my else d
      in
      { m with scene = Desk d }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

(* what a ball's circle doesn't show, turning with it *)
let marks (r : role) (b : Physics.body) : shape list =
  match r with
  | A_ball Bowling -> [ circle (rgb 90 90 120) 4. |> move 6. 6. |> rotate b.angle |> move b.x b.y ]
  | A_ball Basketball -> [ group [ rectangle black 26. 2.; rectangle black 2. 26. ] |> rotate b.angle |> move b.x b.y ]
  | _ -> []

let view_machine (p : puzzle) (m : machine) : shape list =
  let gx, gy, gw, gh = p.goal in
  let fan =
    List.concat_map
      (function
        | Fan { fan = (fx, fy); switch = (sx, sy) } ->
            [ group [ rectangle (rgb 70 70 80) 30. 60.; circle (if m.fan_on then rgb 120 200 250 else rgb 110 110 120) 22. |> move 18. 0. ] |> move fx fy;
              text (if m.fan_on then rgb 60 170 80 else rgb 200 70 70) 1.3 (if m.fan_on then "ON" else "OFF") |> move sx (sy +. 22.) ]
            @ if m.fan_on then List.init 5 (fun k -> rectangle (rgb 160 210 250) 60. 2. |> fade 0.6 |> move (fx +. 70. +. (float_of_int ((m.frames * 6 + (k * 97)) mod 440))) (fy -. 40. +. (float_of_int k *. 20.))) else []
        | Pulley { top; left = (lx, _); right = (rx, _) } ->
            [ circle (rgb 90 90 100) 14. |> move lx top; circle (rgb 90 90 100) 14. |> move rx top;
              rectangle (rgb 90 90 100) (rx -. lx) 3. |> move ((lx +. rx) /. 2.) (top +. 14.) ]
        | Fixed_seesaw _ -> [])
      p.machines
  in
  [ rectangle (rgb 250 240 170) gw gh |> fade 0.5 |> move gx gy; text (rgb 160 130 40) 1.4 "GOAL" |> move gx (gy +. (gh /. 2.) -. 14.) ]
  @ fan
  @ Physics.debug_joints m.world
  @ List.map Physics.draw m.world.bodies
  @ List.concat (List.map2 marks m.roles m.world.bodies)

let view_bin (d : desk) : shape list =
  List.concat
    (List.mapi
       (fun i (part, _) ->
         let chosen = d.picked = Some part in
         [ rectangle (if chosen then rgb 250 200 80 else rgb 70 70 90) 160. 50. |> move (slot_x i) bin_y;
           text (if chosen then black else white) 1.6 (Printf.sprintf "%s x%d" (part_name part) (left_in_bin d part)) |> move (slot_x i) bin_y ])
       (puzzle d).bin)
  @ [ text (rgb 90 90 110) 1.4 (Printf.sprintf "ramp tilt %+.0f (r)" d.tilt) |> move 300. bin_y ]

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  rectangle (rgb 225 235 245) screen.width screen.height
  ::
  (match m.scene with
  | Title ->
      [ text (rgb 60 90 160) 6. "TINY INCREDIBLE MACHINE" |> move_y 180.;
        text (rgb 60 60 80) 2.2 "finish the machine from the parts in the bin, then let it run:" |> move_y 80.;
        text (rgb 60 60 80) 2.2 "click a part, click where it goes (r tilts a ramp)" |> move_y 40.;
        text (rgb 60 60 80) 2.2 "space starts the machine, and stops it; n the next puzzle" |> move_y 0. ]
      @ Scene2d.blink 1. m [ text (rgb 200 100 40) 3. "PRESS SPACE" |> move_y (-140.) ]
  | Desk d ->
      let p = puzzle d in
      let shown = match d.running with Some run -> run | None -> build p d.parts in
      view_machine p shown
      @ view_bin d
      @ [ text (rgb 40 60 120) 2.6 p.title |> move (-250.) 450.; text (rgb 80 80 100) 1.6 p.hint |> move 150. 450. ]
      @ (match d.running with
        | None -> [ text (rgb 60 120 60) 2. "EDIT -- space: start" |> move 280. (-465.) ]
        | Some _ -> [ text (rgb 180 60 60) 2. "RUNNING -- space: stop" |> move 280. (-465.) ])
      @
      if d.won then [ rectangle white 520. 90. |> fade 0.85 |> move_y 60.; text (rgb 40 150 60) 4. "PUZZLE SOLVED!" |> move_y 70.; text (rgb 60 60 80) 1.8 "n: the next puzzle" |> move_y 30. ]
      else [])

let app = game view update initial_model
let main = Playground_platform.run_app app
