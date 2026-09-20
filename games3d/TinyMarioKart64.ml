(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Mario Kart 64 (Nintendo, 1996): three laps of a
 * circuit against seven computer karts, through traffic, with item
 * boxes to drive into and a powerslide that pays you a boost. Up to
 * accelerate, down to brake, left/right to steer, shift to hop and
 * slide, space to use what you hold.
 *
 * The trick of this game is that it draws with two things at once, and
 * which is which is the whole lesson:
 *
 *   - polygons for the world and for what a box can be: the road, the
 *     rails, the item boxes, and the lorries and cars of the traffic;
 *   - sprites for the karts, the trees, the bananas and the shells:
 *     flat pictures, turned to face the eye, standing on the ground.
 *
 *              polygons                     sprites
 *        +----------------+                   _|_
 *       /                /|                  (o o)     a drawing,
 *      +----------------+ |       vs.        /|_|\     always turned
 *      |     lorry      | +                   | |      towards you
 *      +----------------+                    _|_|_
 *
 * The N64 could have modelled a kart. It could not have modelled a
 * kart *with its driver on it*, recognisable from any angle, eight of
 * them on screen, at the frame rate: so Nintendo kept the SNES's
 * answer, drew each kart once per viewing angle at a quality no
 * console could render live, and pasted the drawings into a polygon
 * world. A lorry needs none of that -- it is a box with wheels, it has
 * no face, and it looks right from every side for free. Trees are the
 * far end of the same argument: foliage is what polygons are worst at.
 *
 * The tell is when the illusion breaks: a sprite seen from above, or
 * from very close, goes flat. Mario Kart hid that by keeping the
 * camera low and behind you -- and by the time the GameCube could
 * afford characters in polygons, Double Dash (2003) dropped sprites
 * altogether.
 *
 * Two things follow from the mix, and they are both in here:
 *
 *   - a sprite is drawn by [billboard]: pixel art, a quad per run of
 *     same-coloured pixels (Sprite.pixels' trick, in 3D), on a plane
 *     turned to face the camera. The transparent runs are simply not
 *     drawn, so the game needs neither textures nor transparency.
 *   - nothing is lit by the backend (No_lighting): a drawing must keep
 *     its colours whatever way the camera looks at it, and flat
 *     shading would darken a kart by a third as you go round a bend.
 *     So the boxes shade their own faces instead ([solid], [shade]),
 *     which is what the N64 did with vertex colours.
 *
 * Underneath the picture there is no new game: the kart is
 * games/TinyMicroMachines' car on a plane (the racing kit's Topdown),
 * the circuit a Tilemap with its waypoints written into it as letters,
 * which give the laps, the places and the computer's driving. That is
 * the same model as games2.5d/TinyKart, drawn a third way:
 *
 *     TinyMicroMachines        TinyKart           TinyMarioKart64
 *     from straight above      Mode 7             polygons + sprites
 *            same Topdown.t, same waypoints, three pictures
 *
 * What the picture cannot give, this game adds, and they are Mario
 * Kart's own three:
 *
 *   - the powerslide ([step_kart]): hop, slide, charge, let go, boost.
 *     It is why a good player never takes a corner straight -- and it
 *     is where the two halves of the game meet: the camera follows
 *     where the kart is *going* rather than where it points, so a
 *     sliding kart is seen from the side, and the drawing chosen by
 *     the viewing angle shows it crossed up for nothing ([drawing]);
 *   - the items ([roll]), handed out *by place*: the leader gets what
 *     only defends (a banana, a green shell), the back of the field
 *     what catches up (a mushroom, a red shell). The item box is the
 *     same for everyone; the rule inside it is not;
 *   - the rubber band ([rubber]): the computer's karts drive faster
 *     when you are ahead of them and slower when you are behind. Mario
 *     Kart is the famous case, and the reason its races stay close.
 *
 * Uses: the racing kit's Topdown (with games/TinyMicroMachines and
 * games2.5d/TinyKart), Tilemap, Sprite (its [runs], for the
 * billboards), Scene2d, Camera3d. Not Road and Car (a track is a list
 * of segments there, a map here: see kits/racing/Road.mli), not
 * Physics3d (the arcade's few rules, like Topdown's: no tyre forces),
 * not cached3d for anything that moves (only the circuit is static).
 *
 * Exercises: split screen for two players (four in the original: the
 * camera and the viewport twice, games/TinyXpilot.ml does it in 2D);
 * hills and a jump (a height per waypoint, used only to draw and to
 * place the camera -- the model stays flat on its plane); a shell that
 * bounces off the rails instead of dying on them; a blue shell; the
 * "lakitu" who fishes you out when you fall; fog and a draw distance,
 * the N64's other two tricks (see plan_3d_remaining.md).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The circuit *)
(*****************************************************************************)

let tile = 6.

(* The grass '.', the road '#', the kerb '*', the start line '=', an
 * item box '?', and the waypoints 'a', 'b', ... on the road's middle,
 * in the order they are driven (all of them road to drive on).
 *
 * Drawn from a closed spline through eleven points, every cell within
 * 1.75 tiles of it road and within 2.35 kerb, which is why the road
 * widens a little where it turns hard. *)
let map =
  Tilemap.of_strings tile
    [ "........................................";
      "........................................";
      "........***##=######?##*****............";
      "......*######=######?########**.........";
      ".....*####r##=a###b#?#c####d####*.......";
      "....*########=######?############*......";
      "...*##q##****.........*****####e###.....";
      "...*####....................**#####*....";
      "...####*.......................*####....";
      "...####.........................#f##....";
      "..*#p##.........................####*...";
      "..*###*.........................####....";
      "..*###*..............****.....*#####....";
      "..*####............*##?###***####g#*....";
      "...#o##...........*##j?############.....";
      "...####*.........#####?##i###h###*......";
      "...*####*......*######**#######**.......";
      "....##n##*...*####k#*....*****..........";
      "....*######**######*....................";
      ".....*#####?##l###*.....................";
      "......*###m?####*.......................";
      "........*##?##**........................";
      ".........*****..........................";
      "........................................";
      "........................................" ]

let cols = Tilemap.cols map
let rows = Tilemap.rows map

(* the waypoints, 'a' then 'b', ..., until a letter isn't in the map;
 * passed within 13 of their middle (the road is about 20 wide) *)
let track : Topdown.track =
  let rec letters (c : char) =
    match Tilemap.find map c with
    | [ (col, row) ] -> Tilemap.center map col row :: letters (Char.chr (Char.code c + 1))
    | _ -> []
  in
  { points = Array.of_list (letters 'a'); reach = 13.; corner = 24. }

let waypoints = Array.length track.points
let laps = 3

(* TinyMicroMachines' toy car (Topdown.toy) at this game's scale, where
 * a tile is 6 world units instead of 100: a lap is about 460 units,
 * eleven seconds of it *)
let params : Topdown.params = { accel = 54.; friction = 1.5; grip = 0.12; steering = 3.5; steering_speed = 15. }

(* the same, with the grip halved: a sliding kart keeps going the way
 * it was going a moment longer (see [step_kart]) *)
let sliding_params : Topdown.params = { params with grip = 0.05 }

let road_speed = 42.

(* how fast a kart can go where it stands: the grass is a crawl, the
 * kerb costs a little, and off the map is worse than grass *)
let top_speed_at (x : number) (y : number) : number =
  match Tilemap.tile_at map x y with None -> 12. | Some '.' -> 17. | Some '*' -> 34. | Some _ -> road_speed

(* Which grass is outside the circuit and which is the infield: a flood
 * fill of the grass from the map's border. Everything it reaches is
 * outside, and the rails stand where the circuit meets it; what it
 * does not reach is the infield, open and merely slow. So cutting a
 * corner over the grass costs you speed, and running wide costs you
 * the rail.
 *
 *     ===============  rails
 *    | #############  the circuit
 *    | ####       ###
 *    | ###   .     ##   the infield: grass, no rail, drive on it
 *    | ####       ###
 *     ===============
 *   the outside: everything the fill reached from the border *)
let is_grass (col : int) (row : int) : bool =
  match Tilemap.get map col row with Some '.' -> true | _ -> false

let outside : bool array array =
  let out = Array.make_matrix rows cols false in
  let todo = Queue.create () in
  let push (col : int) (row : int) =
    if col >= 0 && col < cols && row >= 0 && row < rows && is_grass col row && not out.(row).(col) then begin
      out.(row).(col) <- true;
      Queue.add (col, row) todo
    end
  in
  for col = 0 to cols - 1 do
    push col 0;
    push col (rows - 1)
  done;
  for row = 0 to rows - 1 do
    push 0 row;
    push (cols - 1) row
  done;
  while not (Queue.is_empty todo) do
    let col, row = Queue.pop todo in
    push (col - 1) row;
    push (col + 1) row;
    push col (row - 1);
    push col (row + 1)
  done;
  out

let is_outside (col : int) (row : int) : bool =
  col < 0 || col >= cols || row < 0 || row >= rows || outside.(row).(col)

(* a rail is between here and the outside: not a place a kart can be *)
let walled (x : number) (y : number) : bool =
  let col, row = Tilemap.cell map x y in
  is_outside col row

(* [unwall before car]: a kart that has just driven into a rail, put
 * back. One axis at a time, so that a kart meeting the rail at an
 * angle scrapes along it instead of stopping dead -- the oldest
 * collision trick there is, and the reason a wall feels smooth. *)
let unwall (before : Topdown.t) (car : Topdown.t) : Topdown.t =
  if not (walled car.x car.y) then car
  else if not (walled car.x before.y) then { car with y = before.y; vy = 0. }
  else if not (walled before.x car.y) then { car with x = before.x; vx = 0. }
  else { car with x = before.x; y = before.y; vx = 0.; vy = 0.; speed = car.speed *. 0.5 }

(*****************************************************************************)
(* The map's plane, in space *)
(*****************************************************************************)

(* The map is drawn on paper, where y goes up the page; the world's z
 * goes into the screen. So z = -y, and a heading of 0 on the map
 * (towards +x) is a heading of 90 in space (Camera3d's headings: 0
 * towards -z, 90 towards +x). Everything below goes through these two
 * lines, and they are the only place the two frames meet. *)
let world (x : number) (y : number) (h : number) : number * number * number = (x, h, -.y)
let heading3d (a : number) : number = 90. -. a

(* the flat shading this game does by hand, since the backend does none
 * (see the header): a colour at a fraction of its light *)
let shade ((r, g, b) : int * int * int) (light : number) : color =
  let v (c : int) = int_of_float ((float_of_int c *. light) +. 0.5) in
  rgb (v r) (v g) (v b)

(* [solid c w h d]: a box of [c], centred on the origin like Playground3d's
 * [box], but with each face at its own light: the top full, the front
 * and back a little less, the sides less again, the bottom least. The
 * six faces wind counterclockwise seen from outside. *)
let solid ((r, g, b) : int * int * int) (w : number) (h : number) (d : number) : shape3d =
  let x = w /. 2. and y = h /. 2. and z = d /. 2. in
  let top = shade (r, g, b) 1. and bottom = shade (r, g, b) 0.45 in
  let front = shade (r, g, b) 0.82 and side = shade (r, g, b) 0.64 in
  group3d
    [ polygon3d top [ (-.x, y, -.z); (-.x, y, z); (x, y, z); (x, y, -.z) ];
      polygon3d bottom [ (-.x, -.y, -.z); (x, -.y, -.z); (x, -.y, z); (-.x, -.y, z) ];
      polygon3d front [ (-.x, -.y, z); (x, -.y, z); (x, y, z); (-.x, y, z) ];
      polygon3d front [ (x, -.y, -.z); (-.x, -.y, -.z); (-.x, y, -.z); (x, y, -.z) ];
      polygon3d side [ (x, -.y, z); (x, -.y, -.z); (x, y, -.z); (x, y, z) ];
      polygon3d side [ (-.x, -.y, -.z); (-.x, -.y, z); (-.x, y, z); (-.x, y, -.z) ] ]

(*****************************************************************************)
(* The circuit, in polygons *)
(*****************************************************************************)

(* one colour per cell: the grass and the road in squares of two cells
 * (a flat floor of one colour does not seem to move under you), the
 * kerb in red and white stripes, the start line chequered *)
let tile_color (c : char) (col : int) (row : int) : color =
  let big = ((col / 2) + (row / 2)) land 1 = 0 in
  let small = (col + row) land 1 = 0 in
  match c with
  | '.' -> if big then rgb 74 152 64 else rgb 82 164 70
  | '*' -> if small then rgb 220 55 50 else rgb 236 236 236
  | '=' | 'a' -> if small then rgb 235 235 235 else rgb 45 45 45
  | _ -> if big then rgb 104 104 112 else rgb 112 112 120

(* the ground: a quad per run of cells of the same colour in a row,
 * which is Sprite.pixels' trick (kept here because it turns 1000 cells
 * into some 400 quads), the quads wound so that their faces point up *)
let ground_shapes : shape3d list =
  let color_at (col : int) (row : int) : color =
    tile_color (match Tilemap.get map col row with Some c -> c | None -> '.') col row
  in
  let quad (color : color) (col : int) (len : int) (row : int) : shape3d =
    let x, y = Tilemap.center map col row in
    let x1 = x -. (tile /. 2.) and x2 = x +. ((float_of_int len -. 0.5) *. tile) in
    let z1 = -.(y +. (tile /. 2.)) and z2 = -.(y -. (tile /. 2.)) in
    polygon3d color [ (x1, 0., z1); (x1, 0., z2); (x2, 0., z2); (x2, 0., z1) ]
  in
  let row_shapes (row : int) : shape3d list =
    let shapes = ref [] and start = ref 0 in
    for col = 1 to cols do
      if col = cols || color_at col row <> color_at !start row then begin
        shapes := quad (color_at !start row) !start (col - !start) row :: !shapes;
        start := col
      end
    done;
    !shapes
  in
  List.concat_map row_shapes (List.init rows Fun.id)

(* the rails, one quad along every edge where the circuit meets the
 * outside: red and white, an arcade barrier *)
let rail_height = 1.7

let rail_shapes : shape3d list =
  let wall (color : color) ((x1, y1) : number * number) ((x2, y2) : number * number) : shape3d =
    polygon3d color [ (x1, 0., -.y1); (x2, 0., -.y2); (x2, rail_height, -.y2); (x1, rail_height, -.y1) ]
  in
  let cell_walls (col : int) (row : int) : shape3d list =
    if is_grass col row || is_outside col row then []
    else
      let x, y = Tilemap.center map col row in
      let h = tile /. 2. in
      let color = if (col + row) land 1 = 0 then rgb 215 60 55 else rgb 240 240 240 in
      List.filter_map Fun.id
        [ (if is_outside (col - 1) row then Some (wall color (x -. h, y -. h) (x -. h, y +. h)) else None);
          (if is_outside (col + 1) row then Some (wall color (x +. h, y +. h) (x +. h, y -. h)) else None);
          (if is_outside col (row - 1) then Some (wall color (x +. h, y +. h) (x -. h, y +. h)) else None);
          (if is_outside col (row + 1) then Some (wall color (x -. h, y -. h) (x +. h, y -. h)) else None) ]
  in
  List.concat_map (fun row -> List.concat_map (fun col -> cell_walls col row) (List.init cols Fun.id)) (List.init rows Fun.id)

(* the circuit never changes, so its 600-odd quads are turned into
 * faces once and kept (on the GPU backends, into a buffer) *)
let circuit : shape3d = cached3d (ground_shapes @ rail_shapes)

(* the trees stand on the grass, well away from the road: one grass
 * cell in seventeen, spread by a pattern rather than at random so that
 * the same circuit comes up every run *)
let tree_places : (number * number) list =
  List.concat_map
    (fun row ->
      List.filter_map
        (fun col ->
          let around_is_grass =
            List.for_all (fun (dc, dr) -> is_grass (col + dc) (row + dr)) [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
          in
          if is_grass col row && around_is_grass && ((col * 7) + (row * 11)) mod 17 = 0 then
            Some (Tilemap.center map col row)
          else None)
        (List.init cols Fun.id))
    (List.init rows Fun.id)

(* where the item boxes are, from the map's '?' *)
let item_boxes : (number * number) array =
  Tilemap.find map '?' |> List.map (fun (col, row) -> Tilemap.center map col row) |> Array.of_list

(*****************************************************************************)
(* Sprites -- the trick of this game (see the header) *)
(*****************************************************************************)

(* [billboard right size palette rows p]: pixel art standing on the
 * ground at [p], in a plane turned to face the camera. [right] is the
 * camera's right on the ground, (x, z), and the plane is that vector
 * and straight up -- so the drawing never leans, however the camera
 * moves, and never turns with the thing it draws.
 *
 *      camera                 the plane of the sprite
 *        o - - - - - - - - -> |
 *                      right  |  up
 *                       <-----+---->
 *
 * A quad per run of pixels of the same colour (Sprite.runs, which
 * Sprite.pixels uses to draw in 2D); a character the palette does not
 * name is simply not drawn, which is how a sprite has a hole in it
 * here without a texture and without transparency.
 *
 * The quads wind so their faces point back towards the camera: with
 * [right] to its right and up above it, right x up is -(the way the
 * camera looks). *)
let billboard ((rx, rz) : number * number) (size : number) (palette : (char * color) list) (rows : string list)
    ((x, y, z) : number * number * number) : shape3d list =
  let nrows = List.length rows in
  let width = List.fold_left (fun w row -> max w (String.length row)) 0 rows in
  let half = float_of_int width /. 2. in
  let corner (u : number) (v : number) : number * number * number = (x +. (u *. rx), y +. v, z +. (u *. rz)) in
  List.concat
    (List.mapi
       (fun r row ->
         let low = float_of_int (nrows - r - 1) *. size and high = float_of_int (nrows - r) *. size in
         Sprite.runs row
         |> List.filter_map (fun (col, len, c) ->
                match List.assoc_opt c palette with
                | None -> None
                | Some color ->
                    let a = (float_of_int col -. half) *. size and b = (float_of_int (col + len) -. half) *. size in
                    Some (polygon3d color [ corner a low; corner b low; corner b high; corner a high ])))
       rows)

(* the camera's right on the ground, from its heading on the map *)
let right_of (view_angle : number) : number * number =
  let a = view_angle *. Float.pi /. 180. in
  (sin a, cos a)

(* A kart and its driver, seen from four angles, 16 x 10 pixels: the
 * cap 'C' and the body 'B' in the kart's colour, the skin 'S', the
 * tyres 'K', the engine 'E', white 'W' and the visor 'D'. The side and
 * three-quarter drawings face right; [Sprite.flip] gives the left. *)
let from_back =
  [ "......CCCC......"; ".....CCCCCC....."; "......SSSS......"; "....BBBBBBBB...."; "...BBBBBBBBBB...";
    "KKKBBEEEEEEBBKKK"; "KKKBBEEEEEEBBKKK"; "KKKBBBBBBBBBBKKK"; "KKK.BB....BB.KKK"; "KKK..........KKK" ]

let from_three_quarters =
  [ ".....CCCC......."; "....CCCCCCW....."; ".....SSSS......."; "...BBBBBBBBB...."; "..BBBBBBBBBBBB..";
    "KKKBEEEEBBBBKKK."; "KKKBEEEEBBBBKKKK"; "KKKBBBBBBBBBKKKK"; "KKK.BB.....BBKK."; "KKK.........KKK." ]

let from_side =
  [ "......CCC......."; ".....CCCCC......"; ".....CSSWW......"; "...BBBBBBBBB...."; "..BBBBBBBBBBBBB.";
    ".KKKBBBBBBBKKKB."; "KKKKKEEEEEKKKKK."; "KKKKK.....KKKKK."; ".KKK.......KKK.."; "................" ]

let from_front =
  [ "......CCCC......"; ".....CWWWWC....."; ".....CSDDSC....."; "......SSSS......"; "....BBBBBBBB....";
    "...BBBBBBBBBB..."; "KKKBBBBBBBBBBKKK"; "KKKBEEEEEEEEBKKK"; "KKKBBBBBBBBBBKKK"; "KKK..........KKK" ]

let kart_palette (color : color) : (char * color) list =
  [ ('C', color); ('B', color); ('S', rgb 250 200 160); ('K', rgb 28 28 28); ('E', rgb 150 150 160);
    ('W', white); ('D', rgb 40 60 110) ]

(* Which of the four drawings, for a kart whose heading is [heading]
 * seen by a camera whose heading is [view_angle]: its back when it
 * goes our way, its side when it crosses us, its front when it comes
 * at us. The SNES and the N64 both stored the drawings this way; a
 * kart sliding shows you its side, which is exactly what a powerslide
 * should look like. *)
let drawing (view_angle : number) (heading : number) : string list =
  let rel = Float.rem (Float.rem (heading -. view_angle +. 180.) 360. +. 360.) 360. -. 180. in
  let rows =
    match Float.abs rel with
    | a when a < 22. -> from_back
    | a when a < 68. -> from_three_quarters
    | a when a < 142. -> from_side
    | _ -> from_front
  in
  if rel > 0. && Float.abs rel < 142. then Sprite.flip rows else rows

let tree_rows =
  [ "...LLL..."; "..LLTLL.."; ".LLTTTLL."; "LLTTTTTLL"; ".LTTTTTL."; "..LTTTL.."; "...LTL...";
    "....t...."; "....t...."; "....t...."; "....t...."; "....t...." ]

let tree_palette : (char * color) list =
  [ ('T', rgb 34 106 44); ('L', rgb 52 140 58); ('t', rgb 96 66 38) ]

(* the items, 8 x 8, drawn both in the world (a banana on the road, a
 * shell on its way) and in the corner of the screen *)
let banana_rows =
  [ "......Y."; ".....YYW"; "....YYY."; "..YYYY.."; ".YYYY..."; ".YYY...."; "..YYYY.."; "....YYY." ]

let shell_rows =
  [ "..GGGG.."; ".GWWGGG."; "GWWGGGGG"; "GWGGGGGG"; "GGGGGGGG"; ".GGGGGG."; "..SSSS.."; "........" ]

let mushroom_rows =
  [ "..GGGG.."; ".GWWGGG."; "GWWGGGGG"; "GGGGWWGG"; "GGGGGGGG"; ".SSSSSS."; "..SSSS.."; "...SS..." ]

let banana_palette : (char * color) list = [ ('Y', rgb 245 215 55); ('W', white) ]

let shell_palette (c : color) : (char * color) list = [ ('G', c); ('W', white); ('S', rgb 245 230 190) ]

let mushroom_palette : (char * color) list =
  [ ('G', rgb 225 50 50); ('W', white); ('S', rgb 245 230 200) ]

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type item = Banana | Green_shell | Red_shell | Mushroom

(* [Sliding (side, charge)]: which way the kart is sliding, and how
 * long it has been held (see [step_kart]) *)
type drift = Straight | Sliding of number * int

type kart = {
  car : Topdown.t;
  color : color;
  base_top : number; (* how fast it goes on the road, before the boost *)
  boost : int; (* frames of boost left *)
  spin : int; (* frames of spinning out left *)
  hop : int; (* frames of the hop that starts a slide *)
  drift : drift;
  item : item option;
  roulette : int; (* frames left of the item still spinning *)
  wait : int; (* the computer's wait before it uses what it holds *)
}

type shell = { sx : number; sy : number; shead : number; homing : bool; life : int; owner : int }
type banana = { bx : number; by : number }
(* a lorry or a car of the traffic, and the lane it drives:
 * [with_traffic] our way, [oncoming] against us *)
type vehicle = { vcar : Topdown.t; lorry : bool; lane : Topdown.track }

type race = {
  karts : kart array; (* the player's first *)
  traffic : vehicle array;
  shells : shell list;
  bananas : banana list;
  boxes : int array; (* per item box: 0 if it is there, else the frames until it is back *)
  view_angle : number; (* the camera's heading: the kart's, a little late *)
  frames : int;
  ready : int; (* > 0: the countdown *)
}

type scene = Title | Racing of race | Finished of race * int (* the player's place *)
type model = scene Scene2d.t

(* [offset_track side reverse]: the same loop of waypoints, moved
 * [side] to the left of the way it is driven, and driven the other way
 * round if [reverse]. The traffic drives on these: the lorries going
 * our way keep right, the oncoming cars keep right too -- which is
 * their left, and ours to meet. *)
let offset_track (side : number) (reverse : bool) : Topdown.track =
  let n = waypoints in
  let pt (i : int) : number * number = Topdown.point track (if reverse then n - i else i) in
  let points =
    Array.init n (fun i ->
        let x1, y1 = pt i and x2, y2 = pt (i + 1) in
        let a = atan2 (y2 -. y1) (x2 -. x1) +. (Float.pi /. 2.) in
        (x1 +. (side *. cos a), y1 +. (side *. sin a)))
  in
  { track with points }

(* The two lanes, 7.2 either side of the middle of a road 20 wide.
 * That number is the whole of the traffic as a thing to drive through:
 * a kart is spun by a lorry within 3.6 of it, so passing one on the
 * outside is impossible (7.2 + 3.6 is the grass) and passing it on the
 * inside leaves a corridor 7 wide up the middle of the road. Put the
 * lanes at 5.5 instead and that corridor is 4 wide, which is to say
 * the traffic simply ends the race of whoever meets it. *)
let with_traffic : Topdown.track = offset_track (-7.2) false
let oncoming : Topdown.track = offset_track (-7.2) true

let kart_colors =
  [| rgb 220 40 40; rgb 60 170 70; rgb 240 200 40; rgb 60 100 220; rgb 230 120 200; rgb 80 200 210;
     rgb 240 140 40; rgb 150 90 210 |]

let karts_in_race = 8

(* A line of its own for every kart, up to 3.8 either side of the
 * middle of the road -- inside the traffic's lanes. Topdown.computer drives at the waypoints, so without
 * this the seven computers all drive the one racing line: they queue
 * up in single file, bump along it, and reach for the same item box.
 * Only the steering uses these; the laps and the places stay on the
 * track itself. *)
let lines : Topdown.track array =
  Array.init karts_in_race (fun i -> offset_track ((float_of_int i -. 3.5) *. 1.1) false)

(* the grid behind the start line, the player last as in every Mario
 * Kart; the computer's karts are a shade slower flat out than the
 * player's, and make it up with the rubber band ([rubber]) *)
let new_race () : race =
  (* four across and two rows deep, rather than two and four: a road 20
   * wide holds them, and the four go through four different item boxes
   * instead of queueing for the same one *)
  let grid_sides = [| 8.; 2.7; -2.7; -8. |] in
  let on_grid (slot : int) : kart =
    let c = Topdown.start track 0 grid_sides.(slot mod 4) in
    let back = 6. +. (9. *. float_of_int (slot / 4)) in
    let a = c.heading *. Float.pi /. 180. in
    { car = { c with x = c.x -. (back *. cos a); y = c.y -. (back *. sin a) };
      color = kart_colors.(if slot = karts_in_race - 1 then 0 else slot + 1);
      base_top = (if slot = karts_in_race - 1 then road_speed else road_speed -. 3. +. (float_of_int slot *. 0.25));
      boost = 0; spin = 0; hop = 0; drift = Straight; item = None; roulette = 0; wait = 0 }
  in
  let grid = Array.init karts_in_race on_grid in
  let vehicle (lane : Topdown.track) (lorry : bool) (at_waypoint : int) : vehicle =
    { vcar = Topdown.start lane at_waypoint 0.; lorry; lane }
  in
  { karts = Array.append [| grid.(karts_in_race - 1) |] (Array.sub grid 0 (karts_in_race - 1));
    traffic =
      Array.of_list
        (* Spread round the circuit, and none of them near the grid:
         * the oncoming ones drive *towards* the start line, so one
         * placed a few waypoints from it arrives exactly as the lights
         * go out, and the race is over for whoever it meets. *)
        [ vehicle with_traffic true 3; vehicle with_traffic true 6; vehicle with_traffic false 9;
          vehicle with_traffic true 12; vehicle oncoming false 4; vehicle oncoming true 8;
          vehicle oncoming false 12 ];
    shells = []; bananas = []; boxes = Array.make (Array.length item_boxes) 0; view_angle = 0.; frames = 0;
    ready = 180 }

let initial_model : model = Scene2d.start Title

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

(* degrees from [a] to [b], the short way, between -180 and 180 *)
let angle_diff (a : number) (b : number) : number = Float.rem (Float.rem (b -. a +. 180.) 360. +. 360.) 360. -. 180.

(* How far round the lap, counted in waypoints (2.5: half way from the
 * second to the third), laps included. Topdown.progress orders the
 * field just as well, but this one is a distance as well as an order,
 * which is what the rubber band needs. *)
let along (c : Topdown.t) : number =
  let x1, y1 = Topdown.point track (c.next - 1) and x2, y2 = Topdown.point track c.next in
  let leg = Float.max 1. (Float.hypot (x2 -. x1) (y2 -. y1)) in
  float_of_int c.next -. (Float.hypot (x2 -. c.x) (y2 -. c.y) /. leg)

(* the place of kart [i], 1 for the first *)
let place_of (r : race) (i : int) : int =
  let mine = along r.karts.(i).car in
  Array.fold_left (fun n (k : kart) -> if along k.car > mine then n + 1 else n) 1 r.karts

(* Mario Kart's rule, and the reason its races stay close: what an item
 * box holds depends on where you are. The leader gets what only
 * defends -- a banana to drop behind, a green shell that goes straight;
 * the back of the field gets what catches up -- a mushroom, a red
 * shell that finds the kart ahead. The box is the same for everyone;
 * the rule inside it is not. *)
let roll (place : int) (seed : int) : item =
  if place = 1 then if seed mod 3 = 0 then Green_shell else Banana
  else if place * 2 <= karts_in_race then
    match seed mod 3 with 0 -> Banana | 1 -> Green_shell | _ -> Red_shell
  else match seed mod 3 with 0 -> Red_shell | _ -> Mushroom

(* hit by a shell, a banana or a lorry: the kart spins on itself and
 * loses most of its speed, and there is nothing to do about it *)
let spin_out (frames : int) (k : kart) : kart =
  if k.spin > 0 then k
  else { k with spin = frames; drift = Straight; boost = 0; car = { k.car with speed = k.car.speed *. 0.3 } }

(* One frame of one kart.
 *
 * The powerslide is this game's own control, and the whole of Mario
 * Kart's driving: hold shift into a corner and the kart hops, then
 * slides -- it points further into the corner than it actually goes
 * (Topdown's grip, halved), and a charge builds while you hold it,
 * faster if you keep steering into the slide. Let go and the charge
 * becomes a boost, the "mini-turbo":
 *
 *   charge    0 ---------- 40 ---------- 85 --->    sparks: none, then
 *   let go:   nothing      25 frames     45 frames   yellow, then orange
 *
 * The two numbers are what makes it usable: a kart at full lock turns
 * 3.5 degrees a frame, so a 90-degree corner is 26 frames of wheel,
 * and a charge that wanted longer than that could only ever be earned
 * by driving off the road.
 *
 * which is why a good player never takes a corner straight, and why
 * the sprite of a kart in a corner shows you its side. *)
let step_kart (holding : bool) (gas : number) (steer : number) (road_top : number) (k : kart) : kart =
  if k.spin > 0 then
    let car = Topdown.drive params (top_speed_at k.car.x k.car.y) 0. 0. k.car in
    let car = { car with heading = car.heading +. 26. } in
    { k with car = unwall k.car car |> Topdown.follow track; spin = k.spin - 1; hop = 0 }
  else
    let fast = k.car.speed > 18. in
    let drift =
      match k.drift with
      | Straight -> if holding && Float.abs steer > 0.4 && fast then Sliding ((if steer > 0. then 1. else -1.), 0) else Straight
      | Sliding (side, charge) ->
          if (not holding) || not fast then Straight
          else Sliding (side, charge + if steer *. side > 0.2 then 2 else 1)
    in
    (* the hop that starts the slide, and the boost that ends it *)
    let hop = match (k.drift, drift) with Straight, Sliding _ -> 12 | _ -> max 0 (k.hop - 1) in
    let earned =
      match (k.drift, drift) with
      | Sliding (_, charge), Straight -> if charge > 85 then 45 else if charge > 40 then 25 else 0
      | _ -> 0
    in
    let boost = max k.boost earned in
    (* a slide turns the kart at least [side], whatever the wheel does *)
    let steer = match drift with Sliding (side, _) -> (0.6 *. side) +. (0.4 *. steer) | Straight -> steer in
    let p = match drift with Sliding _ -> sliding_params | Straight -> params in
    let p = if boost > 0 then { p with accel = p.accel *. 3. } else p in
    let top = Float.min road_top (top_speed_at k.car.x k.car.y) in
    (* a boost is half again as fast wherever it is used -- which is
     * what makes a mushroom over the grass a short cut *)
    let top = if boost > 0 then top *. 1.5 else top in
    let gas = if boost > 0 then 1. else gas in
    let car = Topdown.drive p top gas steer k.car in
    { k with car = unwall k.car car |> Topdown.follow track; drift; hop; boost = max 0 (boost - 1) }

(* karts closer than 2.6 pushed apart, half the overlap each: bumping,
 * not crashing (games2.5d/TinyKart does the same) *)
let bump (karts : kart array) : kart array =
  let push (k : kart) (o : kart) : kart =
    let dx = k.car.x -. o.car.x and dy = k.car.y -. o.car.y in
    let d = Float.hypot dx dy in
    if d >= 2.6 || d = 0. then k
    else
      let p = (2.6 -. d) /. 2. /. d in
      { k with car = { k.car with x = k.car.x +. (dx *. p); y = k.car.y +. (dy *. p) } }
  in
  Array.map (fun k -> Array.fold_left push k karts) karts

(* The rubber band: a computer's kart goes up to 14% faster when the
 * player is a waypoint ahead of it, and a little slower when the
 * player is behind. It is Mario Kart's most famous piece of cheating,
 * and the reason the last lap is always worth driving; it is also why
 * a big lead never feels safe, which players hate and keep playing. *)
let rubber (player : number) (k : kart) : number = 1. +. Basics.clamp (-0.05) 0.14 (0.12 *. (player -. along k.car))

(* The computer's karts are the only ones that see the traffic coming:
 * a vehicle in the 20 ahead of one, and it steers away from the side
 * the vehicle is on, harder the closer it is (Craig Reynolds' obstacle
 * avoidance, in four lines). The player gets no such help, which is
 * the whole point of the traffic. *)
let avoid (traffic : vehicle array) (c : Topdown.t) : number =
  let a = c.heading *. Float.pi /. 180. in
  Array.fold_left
    (fun steer (v : vehicle) ->
      let dx = v.vcar.x -. c.x and dy = v.vcar.y -. c.y in
      (* how far in front of the kart the vehicle is, and how far to its left *)
      let ahead = (dx *. cos a) +. (dy *. sin a) and side = (dy *. cos a) -. (dx *. sin a) in
      if ahead < 1. || ahead > 20. || Float.abs side > 7. then steer
      else steer -. Float.copy_sign (1. -. (ahead /. 20.)) side)
    0. traffic

let step_karts (keys : keyboard) (holding : bool) (autopilot : bool) (r : race) : race =
  let leader = along r.karts.(0).car in
  let one (i : int) (k : kart) : kart =
    let mine = i = 0 && not autopilot in
    let gas, steer =
      if mine then (axis keys.kup keys.kdown, axis keys.kleft keys.kright)
      else
        let gas, steer = Topdown.computer lines.(i) k.car in
        (gas, Basics.clamp (-1.) 1. (steer +. avoid r.traffic k.car))
    in
    (* the computer's karts powerslide too, through whatever corner
     * needs the wheel all the way over *)
    let holding = if mine then holding else Float.abs steer > 0.75 && k.car.speed > 25. in
    let top = if i = 0 then k.base_top else k.base_top *. rubber leader k in
    step_kart holding gas steer top k
  in
  { r with karts = bump (Array.mapi one r.karts) }

let step_traffic (r : race) : race =
  let one (v : vehicle) : vehicle =
    let gas, steer = Topdown.computer v.lane v.vcar in
    let top = if v.lorry then 15. else 21. in
    { v with vcar = Topdown.drive params top gas steer v.vcar |> Topdown.follow v.lane }
  in
  { r with traffic = Array.map one r.traffic }

(* Driving into an item box: it takes a moment to see what you got (the
 * roulette), and the box is back a quarter of a second later. It has
 * to be: the two rows of the grid go through a set of boxes a fifth of
 * a second apart, so a box that takes longer than that to come back is
 * a box only the front row ever sees -- and a kart at the back with
 * nothing to throw is a kart that cannot race. *)
let step_boxes (r : race) : race =
  let boxes = Array.copy r.boxes and karts = Array.copy r.karts in
  Array.iteri
    (fun b (bx, by) ->
      if boxes.(b) = 0 then
        Array.iteri
          (fun i (k : kart) ->
            if boxes.(b) = 0 && k.item = None && k.roulette = 0 && Float.hypot (k.car.x -. bx) (k.car.y -. by) < 3.6
            then begin
              karts.(i) <- { k with roulette = 42 };
              boxes.(b) <- 15
            end)
          karts)
    item_boxes;
  { r with boxes = Array.map (fun n -> max 0 (n - 1)) boxes; karts }

let set_kart (i : int) (k : kart) (r : race) : race =
  { r with karts = Array.mapi (fun j x -> if j = i then k else x) r.karts }

let use_item (i : int) (r : race) : race =
  let k = r.karts.(i) in
  match k.item with
  | None -> r
  | Some item -> (
      let k = { k with item = None; wait = 0 } in
      let a = k.car.heading *. Float.pi /. 180. in
      let ahead (d : number) : number * number = (k.car.x +. (d *. cos a), k.car.y +. (d *. sin a)) in
      let r = set_kart i k r in
      match item with
      | Mushroom -> set_kart i { k with boost = max k.boost 75 } r
      | Banana ->
          let bx, by = ahead (-3.4) in
          { r with bananas = { bx; by } :: r.bananas }
      | Green_shell | Red_shell ->
          let sx, sy = ahead 3.4 in
          { r with
            shells =
              { sx; sy; shead = k.car.heading; homing = item = Red_shell; life = 300; owner = i } :: r.shells })

(* the roulette settling on an item, and the karts using what they
 * hold: the player on space, the computers after a wait of their own *)
let step_items (use : bool) (r : race) : race =
  let settle (i : int) (k : kart) : kart =
    if k.roulette > 1 then { k with roulette = k.roulette - 1 }
    else if k.roulette = 1 then
      { k with roulette = 0; item = Some (roll (place_of r i) ((r.frames / 7) + (i * 5))); wait = 40 + (i * 37 mod 110) }
    else if k.item <> None && i > 0 then { k with wait = k.wait - 1 }
    else k
  in
  let r = { r with karts = Array.mapi settle r.karts } in
  let r = if use then use_item 0 r else r in
  let rec computers (i : int) (r : race) : race =
    if i >= karts_in_race then r
    else computers (i + 1) (if r.karts.(i).item <> None && r.karts.(i).wait <= 0 then use_item i r else r)
  in
  computers 1 r

let shell_speed = 62.

(* The shells fly on their own, and a red one turns towards the kart
 * just ahead of the one who sent it -- by at most 7 degrees a frame,
 * so that it can still miss, and so that it takes the corner wide. A
 * shell dies on a rail; in the original it would bounce (an
 * exercise). *)
let step_shells (r : race) : race =
  let hit = Array.make karts_in_race false in
  let target (owner : int) : (number * number) option =
    let mine = along r.karts.(owner).car in
    Array.fold_left
      (fun best (k : kart) ->
        let p = along k.car in
        match best with
        | _ when p <= mine -> best
        | Some (bp, _, _) when bp <= p -> best
        | _ -> Some (p, k.car.x, k.car.y))
      None r.karts
    |> Option.map (fun (_, x, y) -> (x, y))
  in
  let step_one (s : shell) : shell option =
    let shead =
      if not s.homing then s.shead
      else
        match target s.owner with
        | None -> s.shead
        | Some (tx, ty) ->
            let wanted = atan2 (ty -. s.sy) (tx -. s.sx) *. 180. /. Float.pi in
            s.shead +. Basics.clamp (-7.) 7. (angle_diff s.shead wanted)
    in
    let a = shead *. Float.pi /. 180. in
    let sx = s.sx +. (shell_speed *. cos a /. 60.) and sy = s.sy +. (shell_speed *. sin a /. 60.) in
    let struck = ref false in
    Array.iteri
      (fun j (k : kart) ->
        if (j <> s.owner || s.life < 280) && k.spin = 0 && Float.hypot (k.car.x -. sx) (k.car.y -. sy) < 2.6 then begin
          hit.(j) <- true;
          struck := true
        end)
      r.karts;
    if !struck || s.life <= 1 || walled sx sy then None else Some { s with sx; sy; shead; life = s.life - 1 }
  in
  let shells = List.filter_map step_one r.shells in
  { r with shells; karts = Array.mapi (fun i k -> if hit.(i) then spin_out 50 k else k) r.karts }

(* a banana lies where it was dropped until someone finds it *)
let step_bananas (r : race) : race =
  let hit = Array.make karts_in_race false in
  let keep (b : banana) : bool =
    let struck = ref false in
    Array.iteri
      (fun j (k : kart) ->
        if k.spin = 0 && Float.hypot (k.car.x -. b.bx) (k.car.y -. b.by) < 2.2 then begin
          hit.(j) <- true;
          struck := true
        end)
      r.karts;
    not !struck
  in
  let bananas = List.filter keep r.bananas in
  { r with bananas; karts = Array.mapi (fun i k -> if hit.(i) then spin_out 45 k else k) r.karts }

(* meeting the traffic, which costs more than a shell does: a lane is
 * 5.5 from the middle of a road 20 wide, so there is always a way
 * past, and taking it is the driving the traffic asks for *)
let step_crashes (r : race) : race =
  let crashed (k : kart) : bool =
    Array.exists
      (fun (v : vehicle) ->
        Float.hypot (k.car.x -. v.vcar.x) (k.car.y -. v.vcar.y) < if v.lorry then 3.6 else 3.)
      r.traffic
  in
  { r with karts = Array.map (fun k -> if crashed k then spin_out 55 k else k) r.karts }

(* one frame of the race; [autopilot]: the computer drives the player's
 * kart too, as it does after the finish *)
let step_race (keys : keyboard) (holding : bool) (use : bool) (autopilot : bool) (r : race) : race =
  let r = step_traffic r in
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else
    let r = step_karts keys holding autopilot r in
    let r = step_boxes r in
    let r = step_items use r in
    let r = step_shells r in
    let r = step_bananas r in
    let r = step_crashes r in
    (* The camera turns after the kart, a fifth of the way a frame, so
     * that the kart seems to turn in front of you. What it follows is
     * where the kart is *going* (its velocity), not where it points:
     * those two are the same thing everywhere except in a slide, and
     * their difference is precisely what a slide is. Follow the
     * heading instead and the camera sits behind the kart's nose the
     * whole way round the corner, which hides the one thing worth
     * seeing -- a kart crossed up, showing you its side. *)
    let player = r.karts.(0).car in
    let travel =
      if Float.hypot player.vx player.vy > 5. then atan2 player.vy player.vx *. 180. /. Float.pi
      else player.heading
    in
    { r with view_angle = r.view_angle +. (0.18 *. angle_diff r.view_angle travel); frames = r.frames + 1 }

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  let holding = computer.keyboard.kshift in
  match m.scene with
  | Title -> if space then Scene2d.go (Racing (new_race ())) m else m
  | Racing r ->
      let r = step_race computer.keyboard holding space false r in
      if Topdown.lap track r.karts.(0).car >= laps then Scene2d.go (Finished (r, place_of r 0)) m
      else { m with scene = Racing r }
  | Finished (r, n) ->
      if space then Scene2d.go Title m
      else { m with scene = Finished (step_race computer.keyboard false false true r, n) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* The camera's right on the ground, and its heading on the map: the
 * sprites need both (which way to lay their plane, and which of the
 * four drawings of a kart to use), and taking them from the camera
 * itself means the title screen, whose camera turns around the grid,
 * gets them right for free. *)
let camera_right (cam : camera) : number * number =
  let ex, _, ez = cam.eye and tx, _, tz = cam.target in
  let dx = tx -. ex and dz = tz -. ez in
  let d = Float.max 0.001 (Float.hypot dx dz) in
  (-.dz /. d, dx /. d)

let camera_angle (cam : camera) : number =
  let ex, _, ez = cam.eye and tx, _, tz = cam.target in
  atan2 (-.(tz -. ez)) (tx -. ex) *. 180. /. Float.pi

(* the lorries and cars of the traffic, in polygons, facing -z *)
let lorry_model : shape3d =
  let wheel = solid (28, 28, 28) 2.9 1.1 1.1 in
  group3d
    [ solid (205, 55, 50) 2.6 2.2 2.6 |> move3d 0. 1.5 (-2.2);
      solid (150, 200, 235) 2.2 0.9 0.2 |> move3d 0. 1.9 (-3.5);
      solid (225, 225, 230) 2.8 2.8 5. |> move3d 0. 1.9 1.2;
      wheel |> move3d 0. 0.55 (-2.);
      wheel |> move3d 0. 0.55 1.8 ]

let car_model : shape3d =
  let wheel = solid (28, 28, 28) 2.3 0.9 0.9 in
  group3d
    [ solid (70, 120, 210) 2.2 0.9 4.2 |> move3d 0. 0.9 0.;
      solid (150, 200, 235) 1.9 0.8 1.8 |> move3d 0. 1.7 (-0.2);
      wheel |> move3d 0. 0.45 (-1.3);
      wheel |> move3d 0. 0.45 1.3 ]

let traffic_shapes (r : race) : shape3d list =
  Array.to_list r.traffic
  |> List.map (fun (v : vehicle) ->
         let x, y, z = world v.vcar.x v.vcar.y 0. in
         (if v.lorry then lorry_model else car_model) |> rotate3d 0. (-.heading3d v.vcar.heading) 0. |> move3d x y z)

(* an item box: a cube turning over the road, and the road showing
 * through the gap under it *)
let box_shapes (time : time) (r : race) : shape3d list =
  Array.to_list item_boxes
  |> List.mapi (fun b (bx, by) ->
         if r.boxes.(b) > 0 then []
         else
           let x, y, z = world bx by 1.8 in
           [ solid (245, 205, 55) 2.2 2.2 2.2 |> rotate3d 0. (spin 2.5 time) 0. |> move3d x y z ])
  |> List.concat

(* a kart: its shadow on the road, the drawing of it, and, while it
 * slides, the sparks under its wheels *)
let kart_shapes (right : number * number) (angle : number) (k : kart) : shape3d list =
  let x, _, z = world k.car.x k.car.y 0. in
  let hop = if k.hop > 0 then 0.9 *. sin (Float.pi *. float_of_int k.hop /. 12.) else 0. in
  (* a dark patch rather than a faded black one: the software
   * rasterizer draws no alpha (see playground3d/software), so a shadow
   * that counted on [fade3d] would be a hole in the road there *)
  let shadow =
    polygon3d (rgb 52 62 52)
      [ (x -. 1.2, 0.04, z -. 0.85); (x -. 1.2, 0.04, z +. 0.85); (x +. 1.2, 0.04, z +. 0.85); (x +. 1.2, 0.04, z -. 0.85) ]
  in
  let sparks =
    match k.drift with
    | Sliding (side, charge) when charge > 40 ->
        let color = if charge > 85 then rgb 255 140 30 else rgb 255 230 80 in
        let a = k.car.heading *. Float.pi /. 180. in
        (* under the outside rear wheel, the one doing the sliding *)
        let back = -1.1 and out = 1.2 *. -.side in
        let sx = k.car.x +. (back *. cos a) -. (out *. sin a) and sy = k.car.y +. (back *. sin a) +. (out *. cos a) in
        billboard right 0.32 [ ('S', color) ] [ ".S."; "SSS"; ".S." ] (world sx sy 0.1)
    | _ -> []
  in
  (shadow :: billboard right (2.6 /. 16.) (kart_palette k.color) (drawing angle k.car.heading) (x, hop, z)) @ sparks

let item_art (i : item) : string list * (char * color) list =
  match i with
  | Banana -> (banana_rows, banana_palette)
  | Green_shell -> (shell_rows, shell_palette (rgb 60 190 70))
  | Red_shell -> (shell_rows, shell_palette (rgb 225 60 55))
  | Mushroom -> (mushroom_rows, mushroom_palette)

let thing_shapes (right : number * number) (r : race) : shape3d list =
  List.concat_map
    (fun (b : banana) -> billboard right 0.16 banana_palette banana_rows (world b.bx b.by 0.1))
    r.bananas
  @ List.concat_map
      (fun (s : shell) ->
        let rows, palette = item_art (if s.homing then Red_shell else Green_shell) in
        billboard right 0.18 palette rows (world s.sx s.sy 0.1))
      r.shells

(* the trees, only the ones near enough to be worth drawing: a sprite
 * costs a quad per run of pixels, and the far ones are a few pixels *)
let tree_shapes (right : number * number) ((px, py) : number * number) : shape3d list =
  List.concat_map
    (fun (x, y) ->
      if Float.hypot (x -. px) (y -. py) > 95. then []
      else billboard right 0.55 tree_palette tree_rows (world x y 0.))
    tree_places

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let ordinal (n : int) : string =
  match n with 1 -> "1ST" | 2 -> "2ND" | 3 -> "3RD" | n -> string_of_int n ^ "TH"

(* the whole circuit from above, north up, and a dot per kart: the same
 * race as the screen shows, seen the way games/TinyMicroMachines draws
 * it for real *)
let minimap (screen : screen) (r : race) : shape list =
  let cell = 4. in
  let ox = screen.right -. 95. and oy = screen.bottom +. 60. in
  let road_or_line (c : char) : char = match c with '.' -> '.' | '=' | 'a' -> '=' | _ -> '#' in
  let dot (k : kart) : shape =
    circle k.color 4. |> move (ox +. (k.car.x /. tile *. cell)) (oy +. (k.car.y /. tile *. cell))
  in
  [ rectangle (rgb 25 25 30) ((float_of_int cols +. 1.) *. cell) ((float_of_int rows +. 1.) *. cell)
    |> move ox oy |> fade 0.6;
    Sprite.pixels cell [ ('#', rgb 120 120 128); ('=', white) ] (List.map (String.map road_or_line) (Tilemap.to_strings map))
    |> move ox oy ]
  @ (Array.to_list r.karts |> List.rev |> List.map dot)

(* what you hold, in its box in the corner; while the roulette spins it
 * shows every item in turn, as the original's does *)
let item_slot (screen : screen) (k : kart) : shape list =
  let x = screen.left +. 90. and y = screen.top -. 105. in
  let shown =
    if k.roulette > 0 then Some (Sprite.cycle (k.roulette / 4) [ Banana; Green_shell; Red_shell; Mushroom ])
    else k.item
  in
  [ rectangle (rgb 20 20 25) 80. 80. |> move x y |> fade 0.55 ]
  @
  match shown with
  | None -> []
  | Some item ->
      let rows, palette = item_art item in
      [ Sprite.pixels 8. palette rows |> move x y ]

let view_hud (screen : screen) (r : race) : shape list =
  let player = r.karts.(0) in
  let lap = min laps (Topdown.lap track player.car + 1) in
  let time = float_of_int r.frames /. 60. in
  [ text white 3. (Printf.sprintf "LAP %d/%d" lap laps) |> move (screen.left +. 110.) (screen.top -. 40.);
    text yellow 5. (ordinal (place_of r 0)) |> move (screen.right -. 90.) (screen.top -. 45.);
    text white 3. (Printf.sprintf "%d:%04.1f" (int_of_float time / 60) (Float.rem time 60.))
    |> move (screen.right -. 250.) (screen.top -. 40.) ]
  @ item_slot screen player @ minimap screen r
  @
  if r.ready > 0 then
    [ text yellow 9. (string_of_int ((r.ready + 59) / 60)) |> move_y 200. ]
  else if r.frames < 40 then [ text yellow 9. "GO!" |> move_y 200. ]
  else []

(* the land beyond the map, a blue sky and a hazy horizon; the sky is
 * seen from below, which is why the game draws back faces too *)
let sky_and_land (cam : camera) : shape3d list =
  Camera3d.floor ~color:(rgb 78 158 66) ~ground:(-0.05) cam
  :: Camera3d.sky ~sky:(rgb 150 200 250) ~horizon:(rgb 96 166 82) ~ground:(-0.05) cam

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let r = match m.scene with Title -> new_race () | Racing r | Finished (r, _) -> r in
  let player = r.karts.(0).car in
  let px, _, pz = world player.x player.y 0. in
  let cam =
    match m.scene with
    | Title -> Camera3d.orbit ~distance:22. ~height:8. ~look:1.5 (spin 14. computer.time) (px, 1., pz)
    | _ -> Camera3d.behind ~back:13. ~height:5.5 ~ahead:9. ~look:2.2 { x = px; y = 0.; z = pz; heading = heading3d r.view_angle }
  in
  let right = camera_right cam and angle = camera_angle cam in
  let world_shapes =
    sky_and_land cam @ [ circuit ] @ tree_shapes right (player.x, player.y) @ traffic_shapes r
    @ box_shapes computer.time r @ thing_shapes right r
    @ List.concat_map (kart_shapes right angle) (Array.to_list r.karts)
  in
  let hud_shapes =
    match m.scene with
    | Title ->
        [ text (rgb 235 45 40) 7. "TINY MARIO KART 64" |> move_y 320.;
          rectangle black 900. 210. |> move_y (-260.) |> fade 0.55;
          text white 2.5 "up: gas   down: brake   left/right: steer" |> move_y (-210.);
          text white 2.5 "shift: hop and slide (hold it round a bend)   space: use your item" |> move_y (-260.);
          text white 2.5 "3 laps, 7 karts, and the traffic" |> move_y (-310.) ]
        @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-390.) ]
    | Racing _ -> view_hud screen r
    | Finished (_, n) ->
        view_hud screen r
        @ [ rectangle black 700. 160. |> move_y 230. |> fade 0.55;
            text yellow 9. (ordinal n ^ " PLACE!") |> move_y 250. ]
        @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 160. ]
  in
  (cam, world_shapes @ List.map hud hud_shapes)

let app = game3d view update initial_model

(* No_lighting: the sprites must keep the colours they are drawn with
 * whatever way the camera looks at them, so this game lights its
 * polygons itself ([solid], [shade]) -- see the header. The back faces
 * are drawn too, for the sky (Camera3d.sky). *)
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = No_lighting; backface_culling = false }
    app
