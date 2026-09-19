(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Super Mario Kart (Nintendo, 1992), in "Mode 7": a
 * kart race seen from just behind your kart, the track a flat floor
 * stretching to the horizon, turning around you as you steer. Up to
 * accelerate, down to brake, left/right to steer; three laps against
 * three computer karts; the grass slows you down.
 *
 * The SNES had no 3D at all. Its "Mode 7" drew one background, a map of
 * tiles, turned and scaled; the trick (F-Zero, 1990, then Pilotwings and
 * Super Mario Kart) was to change the scale on every line of the
 * screen, in the time the beam goes back to the left edge: small at the
 * top, big at the bottom, and the flat map looks like a floor. Here, the
 * same, a row at a time ([to_ground], [view_ground]):
 *
 *      the eye, from the side              one screen row, far below
 *                                          the horizon: the ground
 *     eye o---___ horizon                  near; just below it: far
 *         |\     ---___
 *  height | \          ---___   a row [below] pixels under the horizon
 *         |  \               sees the ground at the distance
 *     ----+---*-----------------  height * focal / below
 *            near     far
 *
 * and across the row, the ground at that distance, from its left to its
 * right: a straight line through the map, sampled at every pixel, which
 * is what the SNES computed ([to_ground]'s across). Here a "pixel" is 5x5
 * real ones: the ground is a 200x130 picture of characters, a tile's
 * color each, drawn by Sprite.pixels (each row merged in runs of the
 * same color, one rectangle per run). Far away, a pixel is wider than
 * the tiles' patterns: the SNES's floors shimmered there; here the
 * patterns fade to their average color, a mipmap ([ground]).
 *
 * Compare games2.5d/TinyWolf.ml, the same idea turned sideways: the
 * raycaster casts a ray per column into the walls, Mode 7 a line per row
 * across the floor. Each draws what the other can't: Wolfenstein has no
 * floor texture, Mode 7 no walls. The karts are TinyWolf's billboards:
 * flat pictures sized by their distance ([to_screen]), the farthest
 * drawn first, and one of four drawings by the angle you see them from
 * (the back, three-quarters, the side, the front), like the SNES's
 * sprites of every kart at every angle.
 *
 * Underneath, it's Micro Machines: the model is a top-down car on a
 * plane (the racing kit's Topdown, shared with games/TinyMicroMachines),
 * the track a Tilemap with its waypoints in its characters ('a', 'b',
 * ...), which give the laps, the places and the computer's driving. Only
 * the picture differs; the minimap shows the same race from above.
 *
 * Uses: the racing kit's Topdown, Tilemap, Sprite, Scene2d. Not Road and
 * Car (the pseudo-3D road of TinyOutRun: a track is a list of segments
 * there, a map here), not Camera2d (a camera turned and zoomed, the whole
 * map the same: Mode 7 without the per-row scale).
 *
 * Exercises: items (bananas, shells: TinyWolf's billboards that move),
 * coins, jumps (a kart's height, its sprite lifted), a camera looking
 * straight down (Camera2d, turned: Mode 7 with the same scale on every
 * row), two players split screen (games/TinyXpilot.ml), F-Zero's walls.
 *)
open Playground

(*****************************************************************************)
(* The track *)
(*****************************************************************************)

let tile = 100.

(* the grass '.', the road '#', the kerbs '*' (on the corners' outsides),
 * the start line '=' (and 'a'), the waypoints 'a', 'b', ... on the
 * road's middle, in the order they're driven *)
let map =
  Tilemap.of_strings tile
    [ "..................................";
      ".......*****..............*****...";
      "......*#########=##############*..";
      "......*#k#######a############b#*..";
      "......*#########=##############*..";
      "......*####*..............**###*..";
      ".......*###*...............*###*..";
      "...*****####*..............*###*..";
      "..*#########*....*****....**###*..";
      "..*#i#####j#*...*##############*..";
      "..*#########*...*#d##########c#*..";
      "..*###******....*##############*..";
      "..*###*.........*###**....*****...";
      "...###..........*###**....*****...";
      "...###..........*##############*..";
      "...###..........*#e##########f#*..";
      "...###..........*##############*..";
      "...###...........*****....**###*..";
      "...###.....................*###*..";
      "..*###*....................*###*..";
      "..*###**..................**###*..";
      "..*############################*..";
      "..*#h########################g#*..";
      "..*############################*..";
      "...*****..................*****...";
      ".................................." ]

(* the waypoints, 'a' then 'b', ..., until a letter isn't in the map;
 * passed within 200 (the road is 300 wide) *)
let track : Topdown.track =
  let rec letters (c : char) =
    match Tilemap.find map c with
    | [ (col, row) ] -> Tilemap.center map col row :: letters (Char.chr (Char.code c + 1))
    | _ -> []
  in
  { points = Array.of_list (letters 'a'); reach = 200.; corner = 350. }

let laps = 3

(* the fastest a kart can go where it is: slow on the grass, and off the
 * map *)
let top_speed (x : number) (y : number) : number =
  match Tilemap.tile_at map x y with Some '.' | None -> 250. | Some '*' -> 600. | Some _ -> 700.

(* The ground's picture, as a character per color (see [palette]): what
 * a sample of the ground at (x, y) shows, a sample being [unit] world
 * units wide there. Patterns in the tiles, not only their colors: the
 * grass mowed in squares (all around the map too), the road in faint
 * ones, the kerbs red and white; without them, a floor of plain colors
 * doesn't seem to move.
 *
 * But far away, a pattern's squares get smaller than a sample: the
 * samples fall on its colors at random, the far rows shimmer, and cut in
 * a run per sample they cost a rectangle each. So a pattern whose
 * squares are less than 2 samples wide is drawn in the average of its
 * two colors ('.' checkers of 'r' and 'w' become 'p', pink): mipmapping
 * (Lance Williams, "Pyramidal Parametrics", SIGGRAPH 1983), in its
 * simplest form, a texture and one smaller version of it; the SNES had
 * no such thing, and shimmered. *)
let ground (unit : number) (x : number) (y : number) : char =
  let checker size (c1 : char) (c2 : char) (average : char) : char =
    if size < 2. *. unit then average
    else if (int_of_float (floor (x /. size)) + int_of_float (floor (y /. size))) land 1 = 0 then c1
    else c2
  in
  match Tilemap.tile_at map x y with
  | None | Some '.' -> checker 200. 'g' 'G' 'h'
  | Some '*' -> checker 25. 'r' 'w' 'p'
  | Some ('=' | 'a') -> checker 25. 'b' 'w' 'm'
  | Some _ -> checker 50. 'd' 'D' 'e'

let palette : (char * color) list =
  [ ('g', rgb 70 160 60); ('G', rgb 60 145 55); ('h', rgb 65 152 57);
    ('r', rgb 210 40 40); ('w', rgb 240 240 240); ('p', rgb 225 140 140);
    ('b', rgb 20 20 20); ('m', rgb 130 130 130);
    ('d', rgb 120 120 125); ('D', rgb 110 110 115); ('e', rgb 115 115 120) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kart = { car : Topdown.t; top : number (* its top speed, whatever the ground *); color : color }

type race = {
  karts : kart list; (* the player's first *)
  view_angle : number; (* the camera's heading: the kart's, a little late *)
  frames : int; (* since the start *)
  ready : int; (* > 0: the countdown *)
}

type scene = Title | Racing of race | Finished of race * int (* the player's place *)
type model = scene Scene2d.t

(* the grid, two by two behind the start line, the player last, as in
 * Super Mario Kart's first race; the computer's karts can't go as fast
 * as the player's (full gas, the friction keeps a kart under 585, see
 * Topdown.drive; the computer's 0.9 gas under 526), else no one would
 * pass them *)
let new_race () : race =
  let place slot (top, color) =
    let c = Topdown.start track 0 (if slot mod 2 = 0 then 60. else -60.) in
    let back = 60. +. (80. *. float_of_int slot) in
    let a = c.heading *. Float.pi /. 180. in
    { car = { c with x = c.x -. (back *. cos a); y = c.y -. (back *. sin a) }; top; color }
  in
  let grid = [ (480., rgb 40 170 60); (500., rgb 240 200 30); (520., rgb 40 90 220); (700., rgb 220 30 30) ] in
  let karts = List.mapi place grid in
  { karts = List.nth karts 3 :: List.filteri (fun i _ -> i < 3) karts; view_angle = 0.; frames = 0; ready = 180 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let axis a b = (if a then 1. else 0.) -. if b then 1. else 0.

(* degrees from [a] to [b], the short way, between -180 and 180 *)
let angle_diff (a : number) (b : number) : number = Float.rem (Float.rem (b -. a +. 180.) 360. +. 360.) 360. -. 180.

(* karts closer than 36 pushed apart, half the overlap each: bumping,
 * not crashing *)
let bump (karts : kart list) : kart list =
  let push (k : kart) (o : kart) =
    let dx = k.car.x -. o.car.x and dy = k.car.y -. o.car.y in
    let d = Float.hypot dx dy in
    if d >= 36. || d = 0. || k == o then k
    else
      let p = (36. -. d) /. 2. /. d in
      { k with car = { k.car with x = k.car.x +. (dx *. p); y = k.car.y +. (dy *. p) } }
  in
  List.map (fun k -> List.fold_left push k karts) karts

(* one frame of the race; [autopilot]: the computer drives the player's
 * kart too (after the finish, as in Super Mario Kart) *)
let update_race (k : keyboard) (autopilot : bool) (r : race) : race =
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else
    let drive i (kart : kart) =
      let gas, steer =
        if i = 0 && not autopilot then (axis k.kup k.kdown, axis k.kleft k.kright) else Topdown.computer track kart.car
      in
      let top = Float.min kart.top (top_speed kart.car.x kart.car.y) in
      { kart with car = Topdown.drive Topdown.toy top gas steer kart.car |> Topdown.follow track }
    in
    let karts = bump (List.mapi drive r.karts) in
    (* the camera turns after the kart, a fifth of the way a frame: the
     * kart seems to turn in front of you, and a drift shows *)
    let player = (List.hd karts).car in
    let view_angle = r.view_angle +. (0.2 *. angle_diff r.view_angle player.heading) in
    { r with karts; view_angle; frames = r.frames + 1 }

(* the player's place, 1 for the first *)
let place (r : race) : int =
  let p = Topdown.progress track (List.hd r.karts).car in
  1 + List.length (List.filter (fun k -> Topdown.progress track k.car > p) r.karts)

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  match m.scene with
  | Title -> if space then Scene2d.go (Racing (new_race ())) m else m
  | Racing r ->
      let r = update_race computer.keyboard false r in
      if Topdown.lap track (List.hd r.karts).car >= laps then Scene2d.go (Finished (r, place r)) m
      else { m with scene = Racing r }
  | Finished (r, n) -> if space then Scene2d.go Title m else { m with scene = Finished (update_race computer.keyboard true r, n) }

(*****************************************************************************)
(* Mode 7 *)
(*****************************************************************************)

(* the camera: [height] above the ground, [back] behind the kart, the
 * horizon [horizon] pixels above the screen's center, and a field of
 * view of 60 degrees *)
let height = 90.
let back = 250.
let horizon = 150.

(* the eye: where it is, the way it looks (a unit vector), and its focal
 * length, in pixels: how many pixels 1 unit at distance 1 takes *)
type eye = { ex : number; ey : number; dx : number; dy : number; focal : number }

let eye (screen : screen) (x : number) (y : number) (angle : number) : eye =
  let a = angle *. Float.pi /. 180. in
  let dx = cos a and dy = sin a in
  { ex = x -. (back *. dx); ey = y -. (back *. dy); dx; dy; focal = screen.width /. 2. /. tan (Float.pi /. 6.) }

(* [to_ground e sx sy]: the point of the ground the screen pixel (sx, sy)
 * shows, if it's below the horizon. The row [below] pixels under the
 * horizon sees the ground at the distance [d] (similar triangles: height
 * / d = below / focal), and at that distance a pixel across is d / focal
 * units: along the way we look [d], to the right (dy, -dx) sx * d /
 * focal. E.g. 90 units high with a focal of 866, the row 90 pixels
 * under the horizon sees 866 ahead, where a pixel is 1 unit. *)
let to_ground (e : eye) (sx : number) (sy : number) : (number * number) option =
  let below = horizon -. sy in
  if below <= 0. then None
  else
    let d = height *. e.focal /. below in
    let across = sx *. d /. e.focal in
    Some (e.ex +. (d *. e.dx) +. (across *. e.dy), e.ey +. (d *. e.dy) -. (across *. e.dx))

(* [to_screen e x y]: the other way, a point of the ground on the screen,
 * if it's in front: (sx, sy, pixels per unit there). Its distance
 * along the way we look ([depth]) and to the right ([across]) are dot
 * products; then the same similar triangles. *)
let to_screen (e : eye) (x : number) (y : number) : (number * number * number) option =
  let rx = x -. e.ex and ry = y -. e.ey in
  let depth = (rx *. e.dx) +. (ry *. e.dy) and across = (rx *. e.dy) -. (ry *. e.dx) in
  if depth < 20. then None else Some (across *. e.focal /. depth, horizon -. (height *. e.focal /. depth), e.focal /. depth)

(* the ground's "pixels", 5 real ones wide and high, and how far we see:
 * further, the rows are left to the horizon's color (a pixel there would
 * be tens of tiles) *)
let pixel = 5.
let far = 4500.

(* The ground, a row at a time: each row's samples across the screen,
 * one per pixel, the ground's character there ([ground]); all the rows
 * a picture of characters, drawn by Sprite.pixels. *)
let view_ground (screen : screen) (e : eye) : shape =
  let cols = int_of_float (screen.width /. pixel) in
  let rows = int_of_float ((horizon -. screen.bottom) /. pixel) in
  let row i =
    let sy = horizon -. ((float_of_int i +. 0.5) *. pixel) in
    (* the distance this row sees, and a sample's width there *)
    let d = height *. e.focal /. (horizon -. sy) in
    if d > far then String.make cols ' '
    else
      String.init cols (fun j ->
          match to_ground e (screen.left +. ((float_of_int j +. 0.5) *. pixel)) sy with
          | Some (x, y) -> ground (d /. e.focal *. pixel) x y
          | None -> ' ')
  in
  Sprite.pixels pixel palette (List.init rows row)
  |> move ((screen.left +. screen.right) /. 2.) (horizon -. (float_of_int rows *. pixel /. 2.))

(* the sky, and hills on the horizon, going by as you turn: at their
 * bearing, as many pixels a degree as the focal length gives *)
let view_sky (screen : screen) (angle : number) (focal : number) : shape list =
  let per_degree = focal *. Float.pi /. 180. in
  let hill (bearing, r, color) =
    let sx = -.angle_diff angle bearing *. per_degree in
    if Float.abs sx > (screen.width /. 2.) +. r then [] else [ circle color r |> move sx horizon ]
  in
  [ rectangle (rgb 110 170 240) screen.width (screen.top -. horizon) |> move_y ((screen.top +. horizon) /. 2.) ]
  @ List.concat_map hill
      [ (10., 120., rgb 60 130 90); (40., 200., rgb 80 150 100); (95., 90., rgb 60 130 90); (150., 160., rgb 90 160 110);
        (200., 110., rgb 60 130 90); (250., 220., rgb 80 150 100); (300., 140., rgb 90 160 110); (340., 90., rgb 60 130 90) ]
  @ [ rectangle (rgb 40 110 50) screen.width (horizon -. screen.bottom) |> move_y ((horizon +. screen.bottom) /. 2.) ]

(*****************************************************************************)
(* The karts *)
(*****************************************************************************)

(* A kart, seen from four angles, 16 x 10: the driver's helmet 'H' and
 * the body 'B' in its color, the skin 'S', the tires 'K', the engine
 * 'E'. The side and three-quarter drawings look right; [Sprite.flip]
 * for the left. *)
let from_back =
  [ "......HHHH......"; ".....HHHHHH....."; ".....HHHHHH....."; "......SSSS......"; "....BBBBBBBB....";
    "...BBBBBBBBBB..."; "KKKBBEEEEEEBBKKK"; "KKKBBEEEEEEBBKKK"; "KKKBBBBBBBBBBKKK"; "KKK.EE....EE.KKK" ]

let from_three_quarters =
  [ ".....HHHH......."; "....HHHHHH......"; "....HHHHHWW....."; ".....SSSS......."; "...BBBBBBBBBB...";
    "..BBBBBBBBBBBBB."; "KKKBEEEEBBBBKKK."; "KKKBEEEEBBBBKKKK"; "KKKBBBBBBBBBKKKK"; "KKK.EE......KKK." ]

let from_side =
  [ "......HHH......."; ".....HHHHH......"; ".....HHHWWW....."; "......SSS......."; "...BBBBBBBBBB...";
    "..BBBBBBBBBBBBBB"; ".KKKBBBBBBBKKKB."; "KKKKKEEEEEKKKKK."; "KKKKK.....KKKKK."; ".KKK.......KKK.." ]

let from_front =
  [ "......HHHH......"; ".....HWWWWH....."; ".....HSKKSH....."; "......SSSS......"; "....BBBBBBBB....";
    "...BBBBBBBBBB..."; "KKKBBBBBBBBBBKKK"; "KKKBEEEEEEEEBKKK"; "KKKBBBBBBBBBBKKK"; "KKK..........KKK" ]

let kart_palette (color : color) : (char * color) list =
  [ ('H', color); ('B', color); ('S', rgb 250 200 160); ('K', rgb 25 25 25); ('E', rgb 150 150 160); ('W', white) ]

(* The drawing for the kart's heading seen from the camera's: its
 * back when it goes our way, its side when it crosses, flipped when it
 * turns left (its nose to the left of the screen). *)
let drawing (view_angle : number) (heading : number) : string list =
  let rel = angle_diff view_angle heading in
  let rows =
    match Float.abs rel with
    | a when a < 20. -> from_back
    | a when a < 65. -> from_three_quarters
    | a when a < 140. -> from_side
    | _ -> from_front
  in
  if rel > 0. && Float.abs rel < 140. then Sprite.flip rows else rows

(* a kart 48 units wide, standing on the ground where it is *)
let view_kart (e : eye) (view_angle : number) (k : kart) : (number * shape) option =
  to_screen e k.car.x k.car.y
  |> Option.map (fun (sx, sy, scale) ->
         let size = 48. /. 16. *. scale in
         (scale, Sprite.pixels size (kart_palette k.color) (drawing view_angle k.car.heading) |> move sx (sy +. (5. *. size))))

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* the whole race from above, north up, 5 pixels a tile: the road, the
 * start line, and a dot per kart *)
let road_or_line (c : char) : char = match c with '.' -> '.' | '=' | 'a' -> '=' | _ -> '#'

let view_minimap (screen : screen) (r : race) : shape list =
  let cell = 5. in
  let ox = screen.right -. 105. and oy = screen.bottom +. 85. in
  let dot (k : kart) = circle k.color 5. |> move (ox +. (k.car.x /. tile *. cell)) (oy +. (k.car.y /. tile *. cell)) in
  [ rectangle (rgb 30 30 30) 180. 140. |> move ox oy |> fade 0.7;
    Sprite.pixels cell [ ('#', gray); ('=', white) ] (List.map (String.map road_or_line) (Tilemap.to_strings map))
    |> move ox oy ]
  @ List.map dot (List.rev r.karts)

let text color size str = words color str |> scale size

let ordinal (n : int) : string = match n with 1 -> "1ST" | 2 -> "2ND" | 3 -> "3RD" | n -> string_of_int n ^ "TH"

let view_race (screen : screen) (r : race) : shape list =
  let player = (List.hd r.karts).car in
  let e = eye screen player.x player.y r.view_angle in
  (* the karts, the farthest first (the smallest scale) *)
  let karts = List.filter_map (view_kart e r.view_angle) r.karts |> List.sort (fun (a, _) (b, _) -> compare a b) |> List.map snd in
  let lap = min laps (Topdown.lap track player + 1) in
  let time = float_of_int r.frames /. 60. in
  view_sky screen r.view_angle e.focal
  @ [ view_ground screen e ]
  @ karts
  @ view_minimap screen r
  @ [ text white 3. (Printf.sprintf "LAP %d/%d" lap laps) |> move (screen.left +. 110.) (screen.top -. 40.);
      text white 3. (Printf.sprintf "%d:%04.1f" (int_of_float time / 60) (Float.rem time 60.)) |> move (screen.right -. 120.) (screen.top -. 40.);
      text yellow 5. (ordinal (place r)) |> move (screen.left +. 80.) (screen.top -. 110.) ]
  @
  if r.ready > 0 then [ text yellow 8. (string_of_int ((r.ready + 59) / 60)) |> move_y 250. ]
  else if r.frames < 40 then [ text yellow 8. "GO!" |> move_y 250. ]
  else []

let view (computer : computer) (m : model) : shape list =
  let screen = computer.screen in
  match m.scene with
  | Title ->
      (* the track, turning slowly around the infield's middle *)
      let angle = float_of_int m.frames *. 0.3 in
      let e = eye screen 0. 0. angle in
      view_sky screen angle e.focal
      @ [ view_ground screen e;
          text (rgb 220 30 30) 9. "TINY KART" |> move_y 330.;
          rectangle black 700. 220. |> move_y (-300.) |> fade 0.6;
          text white 3. "up: gas   down: brake   left/right: steer" |> move_y (-250.);
          text white 3. "3 laps, against 3 karts" |> move_y (-300.) ]
      @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-380.) ]
  | Racing r -> view_race screen r
  | Finished (r, n) ->
      view_race screen r
      @ [ text yellow 8. (ordinal n ^ " PLACE!") |> move_y 250. ]
      @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 150. ]

let app = game view update initial_model

let main = Playground_platform.run_app app
