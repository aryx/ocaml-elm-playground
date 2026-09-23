(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Braid (Jonathan Blow, 2008): a platformer in which
 * dying is not the end, since time can be run backwards.
 *
 *   arrows   run
 *   space    jump
 *   shift    hold it to rewind time, as far back as you like
 *
 * Braid came out on the Xbox 360's download service in 2008, the year
 * of World of Goo and Spelunky, and is the game people name when they
 * say the indie decade started: one person's game (with David
 * Hellman's paintings), sold at a small price, and about an idea
 * rather than about content. The idea is time. Its worlds are Mario's
 * (a castle at the end, a princess in another castle, enemies to
 * stomp), but every world changes what time does, and each change is
 * a new set of puzzles. In Indie Game: The Movie (Lisanne Pajot and
 * James Swirsky, 2012), Blow is the one who has already shipped,
 * looking back, while Team Meat finishes Super Meat Boy and Phil Fish
 * Fez (TinySuperMeatBoy.ml and TinyFez.ml). (Names and dates from
 * memory, to check.)
 *
 * This is the game this project was made for. With an immutable
 * model, rewinding is keeping the past models in a list and going
 * back down it: gamekits/puzzle's Undo, which TinySokoban calls once a
 * move, called here sixty times a second. Elm's time-travelling
 * debugger (Laszlo Pandy, 2013) was the same observation about Elm
 * programs, and it is the point of the Elm architecture that it costs
 * nothing: [record] is a cons, and a frame shares everything it did not
 * change with the frame before (the map is not even in the list: it
 * never changes). Braid itself, written in C++ with its state changed
 * in place, had to copy what each frame changed into a buffer, and
 * compress it, to go back an hour (Blow's talk on its implementation).
 *
 * Braid's worlds are variations on that list, and so are this game's
 * four rooms, each a room of one of them, each a rule over the list
 * (the flags of [room]):
 *
 *   - Time and Forgiveness: just the list. Die, and hold shift: the
 *     monster you walked into walks backwards, and so do you. A
 *     platformer's lives, checkpoints and "game over" all go away;
 *   - Time and Mystery ([green]): an object that glows green is not in
 *     time. Here the key: rewinding takes Tim back but leaves the key
 *     in his hand. In the list, it is a field copied from the frame you
 *     leave to the frame you go back to;
 *   - Time and Place ([place]): time goes forward when Tim walks right,
 *     backward when he walks left, and stops when he stops. The clock
 *     is no longer counted: it is read off his x. The lift runs on its
 *     timetable, and the timetable's hour is where Tim stands;
 *   - Time and Decision ([shadow]): what you rewind is not lost but
 *     done again, by a shadow of you, while you do something else. The
 *     frames rewound are the list's tail, kept instead of dropped, and
 *     played forward: two of you, to hold a plate down and go through
 *     the door it opens.
 *
 *              record                rewind (shift)
 *   now -> [w5; w4; w3; w2; w1]    [w3; w2; w1]    undone: [w4; w5]
 *                                                   (a shadow's frames,
 *                                                    in the order played)
 *
 * What it uses: gamekits/puzzle's Undo (the history: a whole game's
 * rewind is a list of worlds), gamekits/platformer's Tile_move (running
 * and jumping one pixel at a time), Tilemap (the rooms, as strings),
 * Scene2d. Not Physics: nothing is physical; not Camera2d: a room is a
 * screen.
 *
 * Left undone, exercises: the ring of World 6 (Hesitance), which slows
 * time around it -- time as a field over space, each object stepped by
 * its own clock; rewinding faster the longer shift is held (Braid had
 * four speeds); the last world, in which time runs backwards from the
 * start and the ending turns out to be something else; the puzzle
 * pieces, the story books, the cannons and ladders; and, in [place],
 * monsters on a timetable too.
 *)
open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The rooms *)
(*****************************************************************************)

let tile = 40.

(* A lift runs on a timetable: at the bottom until the clock says [c0],
 * at the top from [c1], rising in between. *)
type lift = { lx : number; lw : number; low : number; high : number; c0 : number; c1 : number }

type room = {
  name : string;
  hint : string;
  map : Tilemap.t;
  green : bool; (* the key is out of time: not rewound *)
  place : bool; (* time is where Tim stands *)
  shadow : bool; (* what is rewound is done again, by a shadow *)
  lift : lift option;
}

let plain = { name = ""; hint = ""; map = Tilemap.of_strings tile []; green = false; place = false; shadow = false; lift = None }

(* '#' '=' walls and ledges, '^' spikes, 'M' a monster, 'K' the key,
 * 'D' a door (a column of it), '_' a plate, 'P' the start, 'E' the exit *)
let forgiveness =
  { plain with
    name = "Time and Forgiveness";
    hint = "die, then hold shift: nothing you did cannot be undone";
    map =
      Tilemap.of_strings tile
        [ "########################";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#P.........M.......M..E#";
          "#######^^^#####^^^######";
          "########################" ] }

(* the key is at the bottom of a drop too deep to jump out of, and the
 * door it opens at the top: only a key out of time gets there *)
let mystery =
  { plain with
    name = "Time and Mystery";
    hint = "green things are out of time: rewind, and the key stays in your hand";
    green = true;
    map =
      Tilemap.of_strings tile
        [ "########################";
          "#...........D..........#";
          "#...........D..........#";
          "#...........D..........#";
          "#...........D..........#";
          "#P..........D......E...#";
          "########.###############";
          "####..........##########";
          "####..........##########";
          "####..........##########";
          "####..........##########";
          "####.......K..##########";
          "########################";
          "########################" ] }

(* the lift rises as Tim crosses it (it spans x = 0 to 160): its
 * timetable is in pixels *)
let place =
  { plain with
    name = "Time and Place";
    hint = "time moves forward when you walk right, back when you walk left";
    place = true;
    lift = Some { lx = 80.; lw = 160.; low = -180.; high = 120.; c0 = 12.; c1 = 148. };
    map =
      Tilemap.of_strings tile
        [ "########################";
          "#......................#";
          "#......................#";
          "#....................E.#";
          "#................======#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#......................#";
          "#P.....................#";
          "########################";
          "########################" ] }

(* the plate opens the door only while it is pressed, and the door is
 * too far to reach before it closes *)
let decision =
  { plain with
    name = "Time and Decision";
    hint = "stand on the plate, rewind, and let your shadow stand there for you";
    shadow = true;
    map =
      Tilemap.of_strings tile
        [ "########################";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#...............D......#";
          "#P.._...........D...E..#";
          "########################";
          "########################" ] }

let rooms = [| forgiveness; mystery; place; decision |]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type tim = { x : number; y : number; vx : number; vy : number; facing : number; ground : bool; dead : bool }
type monster = { mx : number; my : number; dir : number; alive : bool }
type key = Lying | Held | Used

(* what is in time: everything that rewinds, one of these a frame *)
type world = {
  tim : tim;
  monsters : monster list;
  key : key;
  (* frames since the room started, or, in [place], Tim's x *)
  clock : number;
}

type play = {
  room : int;
  history : world Undo.t;
  rewinding : bool;
  (* the frames rewound so far, the first to play first *)
  undone : tim list;
  (* the shadow's frames still to play *)
  shadow : tim list;
}

type scene = Title | Playing of play | Ending
type model = scene Scene2d.t

let tim_size = (24., 34.)
let monster_size = (30., 24.)

let start (r : room) : world =
  let x, y = match Tilemap.find r.map 'P' with (c, row) :: _ -> Tilemap.center r.map c row | [] -> (0., 0.) in
  let monsters =
    Tilemap.find r.map 'M'
    |> List.map (fun (c, row) ->
           let mx, my = Tilemap.center r.map c row in
           { mx; my = my - (tile / 2.) + (snd monster_size / 2.); dir = -1.; alive = true })
  in
  { tim = { x; y; vx = 0.; vy = 0.; facing = 1.; ground = false; dead = false }; monsters; key = Lying;
    clock = (if r.place then x else 0.) }

let enter (room : int) : play =
  { room; history = Undo.start (start rooms.(room)); rewinding = false; undone = []; shadow = [] }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* The rules of a frame *)
(*****************************************************************************)

let gravity = 0.9
let max_fall = 13.
let run_top = 6.
let run_accel = 1.2
let jump_speed = 15.

let spiky (c : char) : bool = c = '^'
let is_exit (c : char) : bool = c = 'E'
let is_plate (c : char) : bool = c = '_'

let lift_top (l : lift) (clock : number) : number =
  let t = Float.min 1. (Float.max 0. ((clock - l.c0) / (l.c1 - l.c0))) in
  l.low + ((l.high - l.low) * t)

let key_at (r : room) : (number * number) option =
  match Tilemap.find r.map 'K' with (c, row) :: _ -> Some (Tilemap.center r.map c row) | [] -> None

(* the door opens for the key, for good, or while someone, Tim or his
 * shadow, stands on the plate *)
let door_open (r : room) (shadow : tim option) (w : world) : bool =
  let on_plate (t : tim) = Tile_move.hits is_plate r.map tim_size t.x t.y in
  w.key = Used || on_plate w.tim || match shadow with Some s -> on_plate s | None -> false

let solid (door_open : bool) (c : char) : bool = c = '#' || c = '=' || (c = 'D' && not door_open)

(* what the player does this frame, which is all [step] needs: the game
 * reads it off the keyboard, the tests make it up *)
type input = { dx : number; jump : bool; rewind : bool }

let nothing = { dx = 0.; jump = false; rewind = false }

(* running and jumping, then moving one pixel at a time, x first *)
let run (solid : char -> bool) (map : Tilemap.t) (i : input) (t : tim) : tim =
  let target = i.dx * run_top in
  let vx = if t.vx < target then Float.min target (t.vx + run_accel) else Float.max target (t.vx - run_accel) in
  let vy = if i.jump && t.ground then jump_speed else Float.max (-.max_fall) (t.vy - gravity) in
  let (x, y), hit_x = Tile_move.move_by solid map tim_size (t.x, t.y) (vx, 0.) in
  let (x, y), hit_y = Tile_move.move_by solid map tim_size (x, y) (0., vy) in
  { t with x; y; vx = (if hit_x then 0. else vx); vy = (if hit_y then 0. else vy);
           facing = (if i.dx <> 0. then i.dx else t.facing); ground = Tile_move.on_ground solid map tim_size x y }

(* the lift is a ledge that moves: feet near its top, not going up,
 * are put on it -- which also carries Tim up when it rises *)
let ride (l : lift) (top : number) (t : tim) : tim =
  let feet = t.y - (snd tim_size / 2.) in
  if t.vy <= 0. && Float.abs (t.x - l.lx) < (l.lw / 2.) + 8. && feet > top - 24. && feet < top + 16. then
    { t with y = top + (snd tim_size / 2.); vy = 0.; ground = true }
  else t

(* a monster walks until a wall or the edge of its floor, and turns *)
let walk (solid : char -> bool) (map : Tilemap.t) (m : monster) : monster =
  if not m.alive then m
  else
    let ahead = m.mx + (m.dir * ((fst monster_size / 2.) + 2.)) in
    let wall = Tile_move.hits solid map monster_size (m.mx + (m.dir * 2.)) m.my in
    let floor = match Tilemap.tile_at map ahead (m.my - (snd monster_size / 2.) - 2.) with Some c -> solid c | None -> false in
    if wall || not floor then { m with dir = -.m.dir } else { m with mx = m.mx + (m.dir * 1.5) }

(* landing on a monster from above kills it, and bounces; any other
 * touch kills Tim *)
let meet (t : tim) (monsters : monster list) : tim * monster list =
  List.fold_left_map
    (fun (t : tim) (m : monster) ->
      let touch =
        m.alive
        && Float.abs (t.x - m.mx) < (fst tim_size + fst monster_size) / 2.
        && Float.abs (t.y - m.my) < (snd tim_size + snd monster_size) / 2.
      in
      if not touch then (t, m)
      else if t.vy < 0. && t.y > m.my + 8. then ({ t with vy = 10. }, { m with alive = false })
      else ({ t with dead = true }, m))
    t monsters

(* the key taken by walking into it, and used by walking into a door *)
let use_key (r : room) (t : tim) (key : key) : key =
  match (key, key_at r) with
  | Lying, Some (kx, ky) when Float.abs (t.x - kx) < 26. && Float.abs (t.y - ky) < 30. -> Held
  | Held, _ when Tile_move.hits (fun c -> c = 'D') r.map (fst tim_size + 4., snd tim_size) t.x t.y -> Used
  | _ -> key

(* one frame of the world, in time *)
let advance (r : room) (shadow : tim option) (i : input) (w : world) : world =
  let solid = solid (door_open r shadow w) in
  let t = run solid r.map i w.tim in
  (* the clock: counted, or, in [place], read off Tim *)
  let clock = if r.place then t.x else w.clock + 1. in
  let t = match r.lift with Some l -> ride l (lift_top l clock) t | None -> t in
  let t, monsters = meet t (List.map (walk solid r.map) w.monsters) in
  let t = if Tile_move.hits spiky r.map tim_size t.x t.y then { t with dead = true } else t in
  { tim = t; monsters; key = use_key r t w.key; clock }

(*****************************************************************************)
(* Update: time, forward and back *)
(*****************************************************************************)

(* Shift held: one frame back down the list. What is [green] is not
 * taken back: it is copied from the frame left to the frame gone back
 * to. And the frame left is kept, in [undone], for a [shadow]. *)
let rewind (r : room) (p : play) : play =
  match p.history.past with
  | [] -> p
  | _ ->
      let back = Undo.undo p.history in
      let back = if r.green then { back with now = { back.now with key = p.history.now.key } } else back in
      { p with history = back; rewinding = true; undone = p.history.now.tim :: p.undone }

let step (i : input) (p : play) : play =
  let r = rooms.(p.room) in
  if i.rewind then rewind r p
  else
    (* shift let go: in [shadow], what was rewound is done again *)
    let p =
      if p.rewinding then { p with rewinding = false; undone = []; shadow = (if r.shadow then p.undone else p.shadow) }
      else p
    in
    (* dead, time stops, and waits to be rewound *)
    if p.history.now.tim.dead then p
    else
      let shadow, rest = match p.shadow with s :: rest -> (Some s, rest) | [] -> (None, []) in
      { p with history = Undo.record (advance r shadow i p.history.now) p.history; shadow = rest }

let reached_exit (p : play) : bool =
  let t = p.history.now.tim in
  (not t.dead) && Tile_move.hits is_exit rooms.(p.room).map tim_size t.x t.y

let update (computer : computer) (model : model) : model =
  let scenes = Scene2d.update computer model in
  let pressed f = Scene2d.pressed f scenes in
  match scenes.scene with
  | Title | Ending -> if pressed (fun k -> k.kspace) then Scene2d.go (Playing (enter 0)) scenes else scenes
  | Playing p ->
      let k = computer.keyboard in
      let p = step { dx = to_x k; jump = pressed (fun k -> k.kspace); rewind = k.kshift } p in
      if reached_exit p then
        if p.room +.. 1 < Array.length rooms then Scene2d.go (Playing (enter (p.room +.. 1))) scenes
        else Scene2d.go Ending scenes
      else { scenes with scene = Playing p }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text (color : color) (size : number) (s : string) : shape = words color s |> scale size

let skies = [| rgb 238 206 150; rgb 150 180 200; rgb 200 170 200; rgb 150 190 160 |]
let stone = rgb 110 90 80
let grass = rgb 110 150 70
let ink = rgb 60 40 40
let glow = rgb 60 220 90
let gold = rgb 240 200 60

let tile_shape (door_open : bool) (c : char) : shape =
  match c with
  | '#' -> square stone tile
  | '=' -> group [ square stone tile; rectangle grass tile 8. |> move_y ((tile / 2.) - 4.) ]
  | '^' -> group (List.init 3 (fun i -> triangle (rgb 230 230 230) 8. |> move ((float_of_int i * 13.) - 13.) (-.tile / 2. + 7.)))
  | 'D' when not door_open -> group [ rectangle (rgb 90 60 40) (tile - 6.) tile; circle gold 4. ]
  | '_' -> rectangle (rgb 200 70 60) (tile - 8.) 8. |> move_y ((-.tile / 2.) + 4.)
  | 'E' -> group [ rectangle ink 30. 40.; rectangle gold 22. 32. |> fade 0.6 ]
  | _ -> group []

(* Tim: a blue coat, a red tie, orange hair *)
let tim_shape (color : color) (t : tim) : shape =
  group
    [ rectangle color 20. 20. |> move_y (-7.);
      rectangle (rgb 200 40 40) 4. 12. |> move (3. * t.facing) (-5.);
      circle (rgb 240 210 180) 9. |> move_y 9.;
      circle (rgb 220 120 40) 6. |> move (-3. * t.facing) 14. ]
  |> (fun s -> if t.dead then rotate 90. s |> move_y (-6.) else s)
  |> move t.x t.y

(* a monstar: a round brown head on two feet *)
let monster_shape (m : monster) : shape =
  group
    [ oval ink 8. 6. |> move (-8.) (-10.); oval ink 8. 6. |> move 8. (-10.);
      circle (rgb 150 90 40) 12. |> move_y 2.;
      circle white 3. |> move (5. * m.dir) 5. ]
  |> move m.mx m.my

let key_shape (r : room) : shape =
  let color = if r.green then glow else gold in
  group
    ((if r.green then [ circle glow 14. |> fade 0.3 ] else [])
    @ [ circle color 6. |> move_x (-6.); rectangle color 14. 4. |> move_x 5.; rectangle color 3. 6. |> move (10.) (-3.) ])

let view_play (computer : computer) (p : play) : shape list =
  let screen = computer.screen in
  let r = rooms.(p.room) in
  let w = p.history.now in
  let shadow = match p.shadow with s :: _ -> Some s | [] -> None in
  [ rectangle skies.(p.room) screen.width screen.height; Tilemap.view (tile_shape (door_open r shadow w)) r.map ]
  @ (match r.lift with
     | Some l -> [ rectangle (rgb 90 70 60) l.lw 20. |> move l.lx (lift_top l w.clock - 10.) ]
     | None -> [])
  @ List.map monster_shape (List.filter (fun m -> m.alive) w.monsters)
  @ (match shadow with Some s -> [ tim_shape (rgb 60 50 90) s |> fade 0.5 ] | None -> [])
  @ [ tim_shape (rgb 60 80 170) w.tim ]
  @ (match (w.key, key_at r) with
     | Lying, Some (kx, ky) -> [ key_shape r |> move kx ky ]
     | Held, _ -> [ key_shape r |> move w.tim.x (w.tim.y + 32.) ]
     | _ -> [])
  (* rewinding, the picture goes sepia, as in Braid *)
  @ (if p.rewinding then [ rectangle (rgb 120 90 40) screen.width screen.height |> fade 0.25; text ink 4. "<<" |> move_y 200. ]
     else [])
  @ (if w.tim.dead && not p.rewinding then [ text ink 2.5 "hold SHIFT to rewind" |> move_y 120. ] else [])
  @ [ text ink 2.5 r.name |> move_y (screen.top - 40.);
      text ink 1.7 r.hint |> move_y (screen.bottom + 55.);
      text ink 1.7
        (if r.place then Printf.sprintf "time is where you stand: %.0f" w.clock
         else Printf.sprintf "time %.1f s" (w.clock / 60.))
      |> move_y (screen.bottom + 25.) ]

let view (computer : computer) (model : model) : shape list =
  let screen = computer.screen in
  let sky = rectangle skies.(0) screen.width screen.height in
  match model.scene with
  | Title ->
      [ sky; text ink 6. "TINY BRAID" |> move_y 200.;
        text ink 2. "arrows run    space jumps" |> move_y 60.;
        text ink 2. "hold SHIFT to rewind time, as far back as you like" |> move_y 20.;
        text ink 2. "four rooms, four ways for time to go" |> move_y (-20.) ]
      @ Scene2d.blink 1. model [ text ink 3. "PRESS SPACE" |> move_y (-200.) ]
  | Playing p -> view_play computer p
  | Ending ->
      [ sky; text ink 3. "THE PRINCESS IS IN ANOTHER CASTLE" |> move_y 100.;
        text ink 2.2 "and every mistake on the way here was undone" |> move_y 20. ]
      @ Scene2d.blink 1. model [ text ink 3. "PRESS SPACE" |> move_y (-200.) ]

let help =
  {|TinyBraid
  arrows   run
  space    jump
  shift    hold it to rewind time
|}

let app = game view update initial_model

let main =
  print_string help;
  Playground_platform.run_app ~flags:(Playground_platform.flags ()) app
