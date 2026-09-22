(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Alone in the Dark (Frédérick Raynal, Infogrames,
 * 1992): a house at night, four rooms, a key, a locked study, and
 * something in the corridor. Up to walk forward, down to step back,
 * left/right to turn. Find the key, open the study, reach the desk.
 *
 * Alone in the Dark drew its characters in polygons over rooms that
 * were *painted*: every room a picture, taken from one place, and the
 * polygons standing in it. A PC of 1992 could not draw a room in
 * polygons fast enough, but it could draw a few characters, and a
 * painted room costs nothing to show if the camera never moves. So the
 * camera never moves. It is bolted to a corner of each room, and when
 * you walk through a doorway it does not follow you -- it *cuts* to the
 * next room's corner, like a film.
 *
 * That one choice is two lessons, and they are this file:
 *
 *  - Cinematography ([rooms]): a camera that does not have to follow
 *    anyone can be put where the shot is best -- high in a corner
 *    looking down the length of the hall, low behind the shelves of the
 *    library so that you walk into your own view. Every other camera in
 *    this repository chases the player (Camera3d.behind, .orbit,
 *    .follow); this one chooses a view and keeps it, and the change is
 *    instantaneous, with no smoothing at all -- smoothing a cut is what
 *    makes it look like an error rather than an edit. A room here is
 *    drawn once and cached, which is the modern equivalent of painting
 *    it: with the camera bolted down, its picture really never changes.
 *
 *  - Tank controls ([step_player]). Up walks forward *the way Carnby
 *    faces*, not up the screen; left and right turn him. Players have
 *    complained about it since 1992, and it is the only control that
 *    works with cuts: move "up the screen" and every cut changes what
 *    up means, so a player walking through a doorway is suddenly
 *    walking back through it. Tank controls ignore the camera, so a
 *    cut cannot turn you round. Resident Evil (Capcom, 1996) kept them
 *    for the same reason, and dropped them when its cameras stopped
 *    cutting.
 *
 * The doorways are where the two meet. A camera change exactly on the
 * threshold flickers back and forth as you stand in it, so a doorway
 * belongs to no room ([zone_of]): standing in one, you keep the room
 * you came from, and the cut happens on the far side -- a hysteresis,
 * which is also what a thermostat does, and for the same reason.
 *
 * Carnby and the thing in the corridor are the brawler kit's Skeleton
 * (gamekits/brawler/3d/Skeleton.mli), the one TinyVirtuaFighter
 * fights with: a walk is two key poses swapped, and the creature is a
 * pose with its arms held out, which is all a 1992 zombie ever was.
 *
 * Uses: Tilemap (the house: its walls, its furniture, which room each
 * floor tile belongs to), the brawler kit's Skeleton, Scene2d. Not
 * Camera3d's cameras (it is the one game here whose camera follows
 * nobody), not Physics3d (a wall is a tile you cannot step into).
 *
 * Exercises: pre-rendered backgrounds for real (each room drawn once to
 * an image, the character drawn over it, which needs a depth image
 * too, as Alone in the Dark's rooms had); camera-relative controls, to
 * feel the cut turn you round; a lantern that lights only what is near
 * (the "dark"); a second creature that waits behind a door.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The house *)
(*****************************************************************************)

let tile = 2.
let wall_height = 3.4

(* The floor plan, a character per tile: '#' a wall, a lower-case
 * letter the room the floor belongs to (a the hall, b the library, c
 * the corridor, d the study), '.' a doorway, which belongs to no room,
 * 'L' the study's locked door, 'k' the key, 'Z' where the creature
 * waits, 'X' the desk you are looking for, '@' where you start, and
 * 'T', 'S', 'C' furniture: tables, shelves, a cabinet. *)
let map =
  Tilemap.of_strings tile
    [ "##################";
      "#aaaaaaa#bbbbbbbS#";
      "#aaTTaaa#bbbbbbbS#";
      "#aaTTaaa.bbbbbbbS#";
      "#aaaaaaa#bbbbbbkS#";
      "#a@aaaaa#bbbSSbbb#";
      "#####.########.###";
      "#ccccccccccccccZc#";
      "#cccccccccccccccc#";
      "######L###########";
      "#dddddddd#########";
      "#dddCdddd#########";
      "#dddddXdd#########";
      "##################" ]

let cell_at (x : number) (y : number) : char = match Tilemap.tile_at map x y with Some c -> c | None -> '#'

(* the tiles a body cannot be in: walls, furniture, and the study door
 * until it is unlocked *)
let blocks (unlocked : bool) (c : char) : bool =
  match c with '#' | 'T' | 'S' | 'C' | 'X' -> true | 'L' -> not unlocked | _ -> false

(* the room a tile is part of: its letter, or None for a doorway, the
 * start, the key, the creature's corner -- every marked floor tile
 * belongs to the room its letter would have been *)
let zone_of (c : char) : char option =
  match c with
  | 'a' | '@' -> Some 'a'
  | 'b' | 'k' -> Some 'b'
  | 'c' | 'Z' -> Some 'c'
  | 'd' -> Some 'd'
  | _ -> None

(*****************************************************************************)
(* The rooms, and where each one's camera is bolted *)
(*****************************************************************************)

(* The world is the plan's plane, y up the page in the plan and -z into
 * the screen in the world, so a plan point (x, y) is (x, h, -y). *)
let world (x : number) (y : number) (h : number) : number * number * number = (x, h, -.y)

(* The shot of each room: an eye and a target, in plan coordinates and
 * heights, placed as a cinematographer would rather than computed --
 * which is the whole point. The hall is seen from high in its corner,
 * the length of it below; the library from low, behind the shelves;
 * the corridor from its end, so that the thing in it walks at you; the
 * study from above the desk, looking back at the door you came in by. *)
let shot (room : char) : camera =
  let at (col : number) (row : number) (h : number) =
    let x, y = Tilemap.center map 0 0 in
    world (x +. (col *. tile)) (y -. (row *. tile)) h
  in
  (* [far] must reach the black void's plane (Camera3d.sky's, 2400
   * across), or the void is cut away and the night comes out white *)
  (* A wide lens: a camera in a corner looks along two walls at once, a
   * right angle, and a normal field of view of 60 degrees leaves a
   * third of its own room out of the shot -- Carnby walks out of the
   * picture without leaving the room. Alone in the Dark's shots were
   * wide for the same reason. *)
  let make eye target = camera ~eye ~target ~fov:84. ~far:2400. () in
  (* every eye is inside its own room, below the tops of the walls
   * (3.4): above them, a roofless house shows its neighbours, and one
   * room one shot is the whole idea. Inside by less than half a tile
   * (a tile is 2): put an eye 1.2 past the last floor tile's centre
   * and it is inside the wall, and the room is a brown screen. *)
  match room with
  | 'a' -> make (at 7.3 1.2 3.1) (at 3. 4. 0.4)
  | 'b' -> make (at 9.3 5.3 1.5) (at 14. 2. 1.3)
  | 'c' -> make (at 16.25 7.5 2.9) (at 2. 7.5 0.7)
  | _ -> make (at 8.3 12.3 3.1) (at 4. 10.5 0.8)

let room_name (room : char) : string =
  match room with 'a' -> "THE HALL" | 'b' -> "THE LIBRARY" | 'c' -> "THE CORRIDOR" | _ -> "THE STUDY"

(* Each room is drawn once and kept: with its camera bolted down, its
 * picture never changes, which is the modern way of painting it. The
 * floor in the room's own colour, the walls as boxes, the furniture;
 * a tile draws what its character says. *)
let floor_color (room : char) : color =
  match room with
  | 'a' -> rgb 96 72 58
  | 'b' -> rgb 70 58 50
  | 'c' -> rgb 60 60 66
  | _ -> rgb 84 64 70

let house : shape3d =
  let cols = Tilemap.cols map and rows = Tilemap.rows map in
  let shapes = ref [] in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let x, y = Tilemap.center map col row in
      let c = match Tilemap.get map col row with Some c -> c | None -> '#' in
      let wx, _, wz = world x y 0. in
      let floor room = box (floor_color room) tile 0.1 tile |> move3d wx (-0.05) wz in
      let piece color w d h = box color w h d |> move3d wx (h /. 2.) wz in
      let add s = shapes := s :: !shapes in
      match c with
      | '#' -> add (piece (rgb 120 104 92) tile tile wall_height)
      | 'T' -> add (floor 'a'); add (piece (rgb 110 70 40) tile tile 0.9)
      | 'S' -> add (floor 'b'); add (piece (rgb 80 52 34) tile (tile *. 0.8) 2.6)
      | 'C' -> add (floor 'd'); add (piece (rgb 90 60 40) (tile *. 0.8) tile 1.3)
      | 'X' -> add (floor 'd'); add (piece (rgb 130 90 50) tile tile 0.95)
      | 'L' -> ()
      | '.' -> add (box (rgb 80 70 64) tile 0.1 tile |> move3d wx (-0.05) wz)
      | _ -> (match zone_of c with Some room -> add (floor room) | None -> ())
    done
  done;
  cached3d !shapes

(* the study's door: drawn apart, since it opens *)
let door_shape (unlocked : bool) : shape3d list =
  match Tilemap.find map 'L' with
  | [ (col, row) ] ->
      let x, y = Tilemap.center map col row in
      let wx, _, wz = world x y 0. in
      if unlocked then [ box (rgb 90 60 40) 0.2 (wall_height *. 0.8) tile |> move3d (wx -. (tile /. 2.)) (wall_height *. 0.4) (wz +. (tile /. 2.)) ]
      else [ box (rgb 110 74 46) tile (wall_height *. 0.85) 0.3 |> move3d wx (wall_height *. 0.425) wz ]
  | _ -> []

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type body = { x : number; y : number; heading : number; step : number (* the walk's phase *) }

type house = {
  carnby : body;
  creature : body;
  room : char; (* whose camera is on: changes on the far side of a doorway *)
  has_key : bool;
  unlocked : bool;
  health : int;
  hurt : int; (* frames of flinching left *)
  caption : int; (* frames left of the room's name on screen *)
  frames : int;
}

type scene = Title | Exploring of house | Over of house * bool (* reached the desk *)
type model = scene Scene2d.t

let start_at (c : char) : number * number =
  match Tilemap.find map c with [ (col, row) ] -> Tilemap.center map col row | _ -> (0., 0.)

let new_house () : house =
  let x, y = start_at '@' and zx, zy = start_at 'Z' in
  { carnby = { x; y; heading = 90.; step = 0. };
    creature = { x = zx; y = zy; heading = 180.; step = 0. };
    room = 'a';
    has_key = false;
    unlocked = false;
    health = 3;
    hurt = 0;
    caption = 90;
    frames = 0 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let radians (d : number) : number = d *. Float.pi /. 180.

(* A body moves if every corner of its footprint lands on floor: the
 * house's walls are tiles, and a tile you cannot stand in is a wall.
 * One axis at a time, so that walking into a wall at an angle slides
 * along it. *)
let free (unlocked : bool) (x : number) (y : number) : bool =
  let r = 0.4 in
  List.for_all (fun (dx, dy) -> not (blocks unlocked (cell_at (x +. dx) (y +. dy)))) [ (-.r, -.r); (r, -.r); (-.r, r); (r, r) ]

let walk_by (unlocked : bool) (b : body) (dx : number) (dy : number) : body =
  let b = if free unlocked (b.x +. dx) b.y then { b with x = b.x +. dx } else b in
  if free unlocked b.x (b.y +. dy) then { b with y = b.y +. dy } else b

(* Tank controls: up and down along the way he faces, left and right
 * turn him. Nothing here reads the camera, and that is the point --
 * see the header. *)
let step_player (keys : keyboard) (h : house) : body =
  let b = h.carnby in
  let turn = (if keys.kleft then 3.2 else 0.) -. if keys.kright then 3.2 else 0. in
  let pace = (if keys.kup then 0.075 else 0.) -. if keys.kdown then 0.045 else 0. in
  let heading = b.heading +. turn in
  let a = radians heading in
  let b = walk_by h.unlocked { b with heading } (pace *. cos a) (pace *. sin a) in
  { b with step = (if pace = 0. then 0. else b.step +. Float.abs pace) }

(* the thing in the corridor comes at you, slower than you walk, and
 * only while you are in its corridor: it does not open doors *)
let step_creature (h : house) : body =
  let c = h.creature in
  if h.room <> 'c' then c
  else
    let dx = h.carnby.x -. c.x and dy = h.carnby.y -. c.y in
    let heading = atan2 dy dx *. 180. /. Float.pi in
    let a = radians heading in
    let c = walk_by true { c with heading } (0.032 *. cos a) (0.032 *. sin a) in
    { c with step = c.step +. 0.032 }

let step_house (keys : keyboard) (h : house) : house =
  let carnby = step_player keys h in
  (* the room changes on the far side of a doorway, never in it: in a
   * doorway, the camera you came in with stays *)
  let room = match zone_of (cell_at carnby.x carnby.y) with Some r -> r | None -> h.room in
  let caption = if room <> h.room then 90 else max 0 (h.caption - 1) in
  let has_key = h.has_key || cell_at carnby.x carnby.y = 'k' in
  (* the key opens the study as soon as you stand at its door *)
  let near_door =
    match Tilemap.find map 'L' with
    | [ (col, row) ] ->
        let dx, dy = Tilemap.center map col row in
        Float.hypot (carnby.x -. dx) (carnby.y -. dy) < 2.6
    | _ -> false
  in
  let unlocked = h.unlocked || (has_key && near_door) in
  let h = { h with carnby; room; caption; has_key; unlocked; frames = h.frames + 1 } in
  let creature = step_creature h in
  let touched = Float.hypot (carnby.x -. creature.x) (carnby.y -. creature.y) < 1.1 in
  if touched && h.hurt = 0 then
    (* it hits, and pushes you back the way you came *)
    let a = radians carnby.heading in
    { h with creature; health = h.health - 1; hurt = 60; carnby = walk_by h.unlocked carnby (-1.2 *. cos a) (-1.2 *. sin a) }
  else { h with creature; hurt = max 0 (h.hurt - 1) }

let at_desk (h : house) : bool =
  match Tilemap.find map 'X' with
  | [ (col, row) ] ->
      let x, y = Tilemap.center map col row in
      Float.hypot (h.carnby.x -. x) (h.carnby.y -. y) < 1.8
  | _ -> false

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  match m.scene with
  | Title -> if space then Scene2d.go (Exploring (new_house ())) m else m
  | Exploring h ->
      let h = step_house computer.keyboard h in
      if h.health <= 0 then Scene2d.go (Over (h, false)) m
      else if at_desk h then Scene2d.go (Over (h, true)) m
      else { m with scene = Exploring h }
  | Over (h, won) -> if space then Scene2d.go Title m else { m with scene = Over (h, won) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* a walk is two key poses, one leg forward and then the other, swung
 * between by the body's own phase *)
let walking (b : body) : Skeleton.pose =
  let left_forward =
    { Skeleton.stand with
      front_leg = Skeleton.limb ~bend:10. 24.;
      back_leg = Skeleton.limb ~bend:18. (-20.);
      front_arm = Skeleton.limb ~yaw:6. ~bend:12. (-16.);
      back_arm = Skeleton.limb ~yaw:(-6.) ~bend:12. 18. }
  in
  let right_forward =
    { left_forward with
      front_leg = left_forward.back_leg;
      back_leg = left_forward.front_leg;
      front_arm = left_forward.back_arm;
      back_arm = left_forward.front_arm }
  in
  if b.step = 0. then Skeleton.stand
  else Skeleton.lerp left_forward right_forward ((sin (b.step *. 5.) +. 1.) /. 2.)

(* the creature: arms held out in front, head down, the 1992 zombie *)
let shambling (b : body) : Skeleton.pose =
  let w = walking b in
  { w with lean = 14.; front_arm = Skeleton.limb ~yaw:6. 86.; back_arm = Skeleton.limb ~yaw:(-6.) 80. }

(* Plan headings are counterclockwise from +x; Skeleton wants the
 * playground's own (0 facing -z, 90 facing +x). The plan's +x is the
 * world's +x and the plan's +y is the world's -z, so a plan heading of
 * 0 is 90 in the world, and 90 is 0: world = 90 - plan. *)
let figure (body : color) (back : color) (skin : color) (pose : Skeleton.pose) (b : body) : shape3d =
  let x, _, z = world b.x b.y 0. in
  Skeleton.draw ~body ~back ~skin 1.8 (90. -. b.heading) pose |> move3d x 0. z

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let h = match m.scene with Title -> new_house () | Exploring h | Over (h, _) -> h in
  (* no smoothing: whose camera it is decides the picture, and the
   * change is a cut *)
  let cam =
    match m.scene with
    | Title -> shot 'a'
    | _ -> shot h.room
  in
  let carnby =
    figure (rgb 60 70 96) (rgb 40 46 64) (rgb 222 186 150) (walking h.carnby) h.carnby
    |> fun s -> if h.hurt > 0 && h.hurt mod 10 < 5 then fade3d 0.4 s else s
  in
  let creature = figure (rgb 90 110 70) (rgb 60 76 48) (rgb 150 170 120) (shambling h.creature) h.creature in
  let key =
    if h.has_key then []
    else
      match Tilemap.find map 'k' with
      | [ (col, row) ] ->
          let x, y = Tilemap.center map col row in
          let wx, _, wz = world x y 0. in
          [ box (rgb 230 200 70) 0.5 0.12 0.2 |> rotate3d 0. (spin 3. computer.time) 0. |> move3d wx 1.1 wz ]
      | _ -> []
  in
  (* the night outside: black above the walls, black below *)
  let void =
    Camera3d.floor ~color:(rgb 8 8 12) ~ground:(-0.2) cam
    :: Camera3d.sky ~sky:(rgb 8 8 12) ~horizon:(rgb 8 8 12) ~ground:(-0.2) cam
  in
  let world_shapes = void @ (house :: carnby :: creature :: key) @ door_shape h.unlocked in
  let hud =
    match m.scene with
    | Title ->
        [ rectangle black 1000. 1000. |> fade 0.45;
          text (rgb 200 60 50) 6. "TINY ALONE IN THE DARK" |> move_y 300.;
          text white 2.5 "up: walk forward   down: step back   left/right: turn" |> move_y (-230.);
          text white 2.5 "find the key, open the study, reach the desk" |> move_y (-275.) ]
        @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-360.) ]
    | Exploring _ ->
        [ text white 2.6 (String.make h.health '*' ^ String.make (3 - h.health) '.')
          |> move (screen.left +. 80.) (screen.top -. 40.) ]
        @ (if h.has_key then [ text (rgb 230 200 70) 2.6 "KEY" |> move (screen.right -. 70.) (screen.top -. 40.) ] else [])
        @ if h.caption > 0 then [ text (rgb 220 210 190) 4. (room_name h.room) |> move_y (screen.bottom +. 70.) ] else []
    | Over (_, won) ->
        [ rectangle black 1000. 220. |> move_y 180. |> fade 0.6;
          text (if won then rgb 220 210 190 else rgb 200 60 50) 6.
            (if won then "THE DIARY IS HERE" else "THE HOUSE KEEPS YOU")
          |> move_y 200. ]
        @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 110. ]
  in
  (cam, world_shapes @ List.map Playground3d.hud hud)

let app = game3d view update initial_model

(* flat shading, dark: a house at night; the back faces drawn too, for
 * the void's plane, seen from below *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
