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
 * circuit against seven computer karts, over a hill and through
 * traffic, with item boxes to drive into, a ramp to fly off and a
 * powerslide that pays you a boost. Up to accelerate, down to brake,
 * left/right to steer, shift to hop and slide, space to use what you
 * hold.
 *
 * And what the N64 was bought for: four friends on one television.
 * On the title, 1 2 3 4 say how many players, space starts the race,
 * b the battle. Together the karts accelerate by themselves, and the
 * keyboard is shared out ([pads]):
 *
 *     player 1   arrows: left right, down brakes, up the item; shift hops
 *     player 2   a d, s brakes, w the item, q hops
 *     player 3   j l, k brakes, i the item, u hops
 *     player 4   f h, g brakes, t the item, r hops
 *
 * The split screen is the playground's (Playground3d.split3d): a view
 * per player -- a camera, its quarter or half of the window, and the
 * world drawn again for it, the sprites turned to face that camera
 * ([race_world]). The cost is plain: the whole race is drawn once per
 * player, which is why the original's three- and four-player races
 * have no computer karts, and neither do these ([new_race]).
 *
 * The battle ([step_battle], after Mario Kart 64's Block Fort): four
 * forts, bridges between their tops, the floor running under the
 * bridges, three balloons each; a hit pops one, and the last kart with
 * a balloon wins. Its ground is the one thing new, and the header of
 * its section draws it: where a bridge crosses the floor there are two
 * heights, and a kart stands on the one its own height reaches
 * ([level]); a fort is solid below its top, and shells bounce off it
 * and off the walls instead of dying on them.
 *
 * The trick of this game is that it draws with two things at once, and
 * which is which is the whole lesson:
 *
 *   - polygons for the world and for what a box can be: the road, the
 *     rails, the ramp, the item boxes, and the lorries and cars of the
 *     traffic;
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
 * The circuit is the racing kit's Track3d: a closed spline with a
 * width, a height and a lean at every point, resampled into segments
 * and drawn as a ribbon of quads. Everything in the game is then said
 * in the two numbers that ribbon gives -- how far along the lap [s] is,
 * and how far across [offset]:
 *
 *     the road         |offset| < width           the lap     s / length
 *     the kerb         |offset| < width + 1.4     the places  compare s
 *     the grass        beyond that                the rails   at the edge
 *     the traffic      a lane at a fixed offset   an item box (s, offset)
 *
 * which is why there is no map in this file at all, and why the road
 * can climb and lean: the ribbon carries the height and the bank, and
 * a kart drawn at its offset stands on the slope ([ground]). The car
 * underneath is still games/TinyMicroMachines' (the racing kit's
 * Topdown, driven on the plane, the height only drawn) -- that much is
 * shared with games2.5d/TinyMarioKart, where the same car is drawn in Mode
 * 7, which cannot show a hill at all.
 *
 * What the picture cannot give, this game adds, and they are Mario
 * Kart's own:
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
 *     Kart is the famous case, and the reason its races stay close;
 *   - the hill and the ramp: the slope of the road under a kart is
 *     taken off its gas ([step_kart] again), so the climb out of the
 *     start is slow and the drop into the banked right-hander is fast,
 *     and the ramp on the back straight throws you in the air, where
 *     nothing steers and the shadow stays on the road below -- which
 *     is games3d/TinyMario64's lesson, and free here since every kart
 *     already drags one.
 *
 * Uses: the racing kit's Track3d (the circuit) and Topdown (the car,
 * with games/TinyMicroMachines and games2.5d/TinyMarioKart), Sprite (its
 * [runs], for the billboards), Scene2d, Camera3d. Not Road and Car
 * (a course is a list of segments there, a ribbon here: see
 * gamekits/racing/Road.mli and 3d/Track3d.mli), not Tilemap (the circuit
 * was a map until the ribbon made the hill possible), not Physics3d
 * (the arcade's few rules, like Topdown's: no tyre forces, and the
 * bank does not pull the kart down the camber).
 *
 * Exercises: a shell that bounces off the race's rails as the battle's
 * do off the forts; the other battle arenas (Big Donut, Double Deck,
 * Skyscraper); gamepads, for four players who aren't sharing one
 * keyboard; a blue shell; the "lakitu" who fishes you out when you fall; a bank that
 * pulls the kart, which would make the banked corner worth driving
 * high; fog and a draw distance, the N64's other two tricks (see
 * plan_3d_remaining.md).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The circuit *)
(*****************************************************************************)

(* The course, as the handful of points the road passes through (see
 * gamekits/racing/3d/Track3d.mli): where, how high, how wide, and how much
 * it leans. Read it as a lap: the start and finish straight, the long
 * climb, the banked right-hander taken downhill, the dip at the
 * bottom, a left-hander and the drag back up.
 *
 *          start/finish          climb
 *        +---------------------------+
 *        |                            \      banked
 *        |                             |     right
 *        \                            /
 *         \          dip            /
 *          +-----+          +------+
 *                 \        /
 *                  +------+   the ramp is on the way back up
 *)
let course : Track3d.control list =
  let control = Track3d.control in
  (* the first one is in the middle of the start straight, so that the
   * lap turns over where the chequered line is and the grid lines up
   * on a straight rather than in the last corner *)
  [ control ~width:11. (-40.) (-45.);
    control ~width:11. (-9.) (-48.);
    control ~y:9. ~width:10. 51. (-42.);
    control ~y:5.5 ~width:9.5 ~bank:(-15.) 84. (-24.);
    control ~y:1.5 ~width:9.5 ~bank:(-17.) 84. 6.;
    control ~y:(-1.) ~width:10. 51. 21.;
    control ~y:(-3.5) ~width:10.5 15. 15.;
    control ~y:(-1.) ~width:9.5 ~bank:12. (-15.) 36.;
    control ~y:3. ~width:10. (-51.) 51.;
    control ~y:5. ~width:10. (-84.) 24.;
    control ~y:2. ~width:10.5 (-87.) (-15.);
    control ~y:0.5 ~width:11. (-69.) (-42.) ]

let track : Track3d.t = Track3d.build ~step:3. course
let lap_length = Track3d.length track
let laps = 3

(* TinyMicroMachines' toy car (Topdown.toy) at this game's scale, where
 * the road is 20 wide instead of 300: a lap is about 470 units, some
 * fifteen seconds of it *)
let params : Topdown.params = { accel = 54.; friction = 1.5; grip = 0.12; steering = 3.5; steering_speed = 15. }

(* the same, with the grip halved: a sliding kart keeps going the way
 * it was going a moment longer (see [step_kart]) *)
let sliding_params : Topdown.params = { params with grip = 0.05 }

let road_speed = 42.

(* the kerb is the strip just outside the road, and the grass just
 * outside that; beyond the rails there is nowhere to be *)
let kerb = 1.4
let verge = 5.

(* how fast a kart can go, by how far across the road it is: the grass
 * is a crawl and the kerb costs a little *)
let top_speed_at (width : number) (offset : number) : number =
  let out = Float.abs offset in
  if out < width then road_speed else if out < width +. kerb then 34. else 17.

(* the rails stand at the edge of the grass, both sides, all the way
 * round: this circuit is Toad's Turnpike, not a field *)
let wall_offset (width : number) : number = width +. kerb +. verge

(*****************************************************************************)
(* The ribbon, in space *)
(*****************************************************************************)

(* The car is driven on a plane and drawn in space, and these two lines
 * are the only place the two meet: the plane's y goes up the page, the
 * world's z into the screen, so z = -y; and a heading of 0 on the
 * plane (towards +x) is a heading of 90 in space (Camera3d's headings:
 * 0 towards -z, 90 towards +x). *)
let world (x : number) (y : number) (h : number) : number * number * number = (x, h, -.y)
let heading3d (a : number) : number = 90. -. a

(* [locate car]: where a car is on the circuit -- how far along the lap
 * and how far to the right of the middle. [near] is its own last
 * answer, since a circuit passes near itself and the nearest bit of
 * road is not always the one you are on. *)
let locate ?(near = -1.) (x : number) (y : number) : number * number = Track3d.locate ~near track x (-.y)

(* the road's surface at a place on the ribbon: its height, and the
 * point of the plane under it *)
let ground (s : number) (offset : number) : number =
  let _, y, _ = Track3d.across track s offset in
  y

(* [plane_at s offset]: the same point, back on the car's plane *)
let plane_at (s : number) (offset : number) : number * number =
  let x, _, z = Track3d.across track s offset in
  (x, -.z)

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

(* Where the start line is, and where the ramp is: both are stretches
 * of the lap, said in the one coordinate the ribbon has. *)
let start_line = 6.

(* the ramp is near the top of the climb, so that a kart takes off over
 * the crest and lands on the way down it; it spans the whole road, so
 * that it is a thing that happens to you rather than a thing to aim
 * for *)
let ramp_at = 66.
let ramp_length = 9.
let ramp_half = 11.5

(* one segment of circuit: the road, its kerbs (red and white, in
 * stripes along it), the grass either side, the line down the middle,
 * and the rails at the edge *)
let segment_shapes (i : int) : shape3d list =
  let p = Track3d.at track (float_of_int i *. Track3d.step track) in
  let w = p.width in
  let light = i / 2 mod 2 = 0 in
  let strip = Track3d.strip track in
  let grass = if light then rgb 74 152 64 else rgb 82 164 70 in
  let kerb_color = if i mod 2 = 0 then rgb 220 55 50 else rgb 236 236 236 in
  let road = if light then rgb 104 104 112 else rgb 112 112 120 in
  let edge = wall_offset w in
  let on_line = float_of_int i *. Track3d.step track -. start_line in
  let on_line = on_line >= 0. && on_line < Track3d.step track *. 2. in
  (* the start line is chequered across the road, and a strip is one
   * colour, so it takes six of them where the road takes one *)
  let line_strips =
    List.init 6 (fun k ->
        let a = w *. ((float_of_int k /. 3.) -. 1.) and b = w *. ((float_of_int (k + 1) /. 3.) -. 1.) in
        strip (if (i + k) mod 2 = 0 then white else rgb 45 45 45) i a b)
  in
  [ strip grass i (-.edge) (-.w -. kerb);
    strip kerb_color i (-.w -. kerb) (-.w) ]
  @ (if on_line then line_strips else [ strip road i (-.w) w ])
  @ [
    strip kerb_color i w (w +. kerb);
    strip grass i (w +. kerb) edge;
    Track3d.wall track (if i mod 2 = 0 then rgb 215 60 55 else rgb 240 240 240) i (-.edge) 1.7;
    Track3d.wall track (if i mod 2 = 0 then rgb 215 60 55 else rgb 240 240 240) i edge 1.7 ]
  @ if light && not on_line then [ strip white i (-0.25) 0.25 ] else []

(* The ramp, a wedge of boards across the road: the one place the
 * circuit leaves the ribbon, since it has to rise above it. Drawn from
 * the ribbon all the same -- its corners are (s, offset) points like
 * everything else. *)
let ramp_height = 1.7

let ramp_shapes : shape3d list =
  let up (s : number) (o : number) (h : number) : number * number * number =
    let x, y, z = Track3d.across track s o in
    (x, y +. h, z)
  in
  let steps = 6 in
  let piece (k : int) : shape3d list =
    let f0 = float_of_int k /. float_of_int steps and f1 = float_of_int (k + 1) /. float_of_int steps in
    let s0 = ramp_at +. (ramp_length *. f0) and s1 = ramp_at +. (ramp_length *. f1) in
    let h0 = ramp_height *. f0 *. f0 and h1 = ramp_height *. f1 *. f1 in
    let color = if k mod 2 = 0 then shade (190, 140, 70) 1. else shade (170, 120, 60) 1. in
    let w = ramp_half in
    [ polygon3d color [ up s0 (-.w) h0; up s1 (-.w) h1; up s1 w h1; up s0 w h0 ];
      (* the sides, so the wedge is a solid thing and not a sheet *)
      polygon3d (shade (150, 105, 55) 1.) [ up s0 (-.w) 0.; up s0 (-.w) h0; up s1 (-.w) h1; up s1 (-.w) 0. ];
      polygon3d (shade (150, 105, 55) 1.) [ up s0 w 0.; up s1 w 0.; up s1 w h1; up s0 w h0 ] ]
  in
  List.concat (List.init steps piece)
  @ [ polygon3d (shade (120, 85, 45) 1.)
        [ up (ramp_at +. ramp_length) (-.ramp_half) 0.; up (ramp_at +. ramp_length) (-.ramp_half) ramp_height;
          up (ramp_at +. ramp_length) ramp_half ramp_height; up (ramp_at +. ramp_length) ramp_half 0. ] ]

(* the circuit never changes, so its quads are turned into faces once
 * and kept (on the GPU backends, into a buffer) *)
let circuit : shape3d =
  cached3d (List.concat (List.init (Track3d.segments track) segment_shapes) @ ramp_shapes)

(* the trees stand on the grass beyond the rails, on both sides, one
 * every few segments: spread by a pattern rather than at random, so
 * that the same circuit comes up every run *)
let tree_places : (number * number) list =
  List.concat
    (List.init (Track3d.segments track) (fun i ->
         if i mod 5 <> 0 then []
         else
           let s = float_of_int i *. Track3d.step track in
           let p = Track3d.at track s in
           let out = wall_offset p.width +. 4. +. (float_of_int (i * 7 mod 5) *. 2.5) in
           if i mod 10 = 0 then [ (s, -.out) ] else [ (s, out) ]))

(* the item boxes: three sets of four across the road, spread round the
 * lap *)
let item_boxes : (number * number) array =
  [| 0.18; 0.52; 0.81 |]
  |> Array.map (fun (f : number) -> lap_length *. f)
  |> Array.to_list
  |> List.concat_map (fun s -> List.map (fun o -> (s, o)) [ -6.; -2.; 2.; 6. ])
  |> Array.of_list
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
  (* where it is on the ribbon: how far along the lap, how far to the
   * right of the middle, and which lap. [s] is also the hint the next
   * [locate] starts from, which is what keeps a circuit that passes
   * near itself from teleporting a kart across the infield. *)
  s : number;
  offset : number;
  lap : int;
  (* in the air, off the ramp: how high above the road, and how fast
   * that is changing *)
  air : number;
  rise : number;
  (* the offset this one likes to drive: the computer's racing line *)
  line : number;
}

type shell = { sx : number; sy : number; shead : number; homing : bool; life : int; owner : int }
type banana = { bx : number; by : number; bh : number }

(* A lorry or a car of the traffic. It does not drive: it is carried
 * along the ribbon at a fixed offset, which is all a lane is, and
 * [against] turns it round to come at you. The karts have to avoid it;
 * it never avoids them. *)
type vehicle = { vs : number; lane : number; lorry : bool; against : bool }

type race = {
  karts : kart array; (* the players' first *)
  humans : int; (* 1 to 4 players *)
  traffic : vehicle array;
  shells : shell list;
  bananas : banana list;
  boxes : int array; (* per item box: 0 if it is there, else the frames until it is back *)
  view_angles : number array; (* each player's camera heading: the kart's, a little late *)
  places : int option array; (* each player's place, once finished *)
  frames : int;
  ready : int; (* > 0: the countdown *)
}
let kart_colors =
  [| rgb 220 40 40; rgb 60 170 70; rgb 240 200 40; rgb 60 100 220; rgb 230 120 200; rgb 80 200 210;
     rgb 240 140 40; rgb 150 90 210 |]

let karts_in_race = 8

(* The two lanes, 7.2 either side of the middle of a road 20 wide. That
 * number is the whole of the traffic as a thing to drive through: a
 * kart is spun by a lorry within 3.6 of it, so passing one on the
 * outside is impossible (7.2 + 3.6 is the kerb) and passing it on the
 * inside leaves a corridor 7 wide up the middle. Put the lanes at 5.5
 * instead and that corridor is 4 wide, which is to say the traffic
 * simply ends the race of whoever meets it. *)
let lane = 7.2
let lorry_speed = 15.
let car_speed = 21.

(* the grid behind the start line, the players last as in every Mario
 * Kart; the computer's karts are a shade slower flat out than the
 * players', and make it up with the rubber band ([rubber]). With three
 * or four players there are no computer karts at all, as on the N64:
 * every view draws the whole race again, and eight karts four times
 * over was more than the machine had.
 *
 * Four across and two rows deep, rather than two and four: a road 20
 * wide holds them, and the four go through four different item boxes
 * instead of queueing for the same one. *)
let grid_sides = [| 8.; 2.7; -2.7; -8. |]

let new_race (humans : int) : race =
  let count = if humans <= 2 then karts_in_race else humans in
  let on_grid (slot : int) : kart =
    (* the players at the back, the first last *)
    let player = count - 1 - slot in
    let offset = grid_sides.(slot mod 4) in
    (* staggered, as a grid is: without the 0.9 the four of a row are
     * exactly level, and four karts tie for fifth place *)
    let s = start_line -. 8. -. (9. *. float_of_int (slot / 4)) -. (0.9 *. float_of_int (slot mod 4)) in
    let s = Float.rem (s +. lap_length) lap_length in
    let p = Track3d.at track s in
    let x, y = plane_at s offset in
    { car = { x; y; vx = 0.; vy = 0.; heading = 90. -. p.heading; speed = 0.; next = 1 };
      color = kart_colors.(if player < humans then player else (slot + humans) mod karts_in_race);
      base_top = (if player < humans then road_speed else road_speed -. 3. +. (float_of_int slot *. 0.25));
      boost = 0; spin = 0; hop = 0; drift = Straight; item = None; roulette = 0; wait = 0;
      s; offset;
      (* the grid is behind the start line, so the first crossing is
       * what makes it lap 0: start the race one lap in hand *)
      lap = -1;
      air = 0.; rise = 0.;
      (* a line of its own for each one, up to 3.8 either side of the
       * middle: without it the seven computers drive the one racing
       * line, queue up in single file and reach for the same item box *)
      line = (float_of_int slot -. 3.5) *. 1.1 }
  in
  let grid = Array.init count on_grid in
  let vehicle (lorry : bool) (against : bool) (f : number) : vehicle =
    { vs = lap_length *. f; lane = (if against then lane else -.lane); lorry; against }
  in
  { karts = Array.append (Array.init humans (fun j -> grid.(count - 1 - j))) (Array.sub grid 0 (count - humans));
    humans;
    (* spread round the circuit, and none of them near the grid: the
     * oncoming ones drive *towards* the start line, so one placed just
     * ahead of it arrives exactly as the lights go out *)
    traffic =
      [| vehicle true false 0.2; vehicle true false 0.42; vehicle false false 0.6; vehicle true false 0.85;
         vehicle false true 0.3; vehicle true true 0.55; vehicle false true 0.75 |];
    shells = []; bananas = []; boxes = Array.make (Array.length item_boxes) 0;
    view_angles = Array.make humans (90. -. (Track3d.at track start_line).heading);
    places = Array.make humans None;
    frames = 0; ready = 180 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let axis (a : bool) (b : bool) : number = (if a then 1. else 0.) -. if b then 1. else 0.

(* degrees from [a] to [b], the short way, between -180 and 180 *)
let angle_diff (a : number) (b : number) : number = Float.rem (Float.rem (b -. a +. 180.) 360. +. 360.) 360. -. 180.

(* the same thing for distances round a lap: from [a] to [b] the short
 * way, between -length/2 and length/2 *)
let lap_diff (a : number) (b : number) : number =
  Float.rem (Float.rem (b -. a +. (lap_length /. 2.)) lap_length +. lap_length) lap_length -. (lap_length /. 2.)

(* how far a kart has driven, laps included: what the places and the
 * rubber band compare *)
let along (k : kart) : number = (float_of_int k.lap *. lap_length) +. k.s

(* the place of kart [i], 1 for the first *)
let place_of (r : race) (i : int) : int =
  let mine = along r.karts.(i) in
  Array.fold_left (fun n (k : kart) -> if along k > mine then n + 1 else n) 1 r.karts

(* Mario Kart's rule, and the reason its races stay close: what an item
 * box holds depends on where you are. The leader gets what only
 * defends -- a banana to drop behind, a green shell that goes
 * straight; the back of the field gets what catches up -- a mushroom,
 * a red shell that finds the kart ahead. The box is the same for
 * everyone; the rule inside it is not. *)
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

(* The ramp's own height, above the road, on the stretch it covers: it
 * is the one thing in the circuit that leaves the ribbon, and a kart
 * on it rides up the same curve the boards are drawn on. *)
let on_ramp (s : number) (offset : number) : number =
  if Float.abs offset > ramp_half then 0.
  else
    let f = lap_diff ramp_at s /. ramp_length in
    if f < 0. || f > 1. then 0. else ramp_height *. f *. f

(* gravity, for the flight off it: the kart leaves at the boards'
 * slope, which at the top is twice the height over the length *)
let gravity = 45.
let ramp_slope = 2. *. ramp_height /. ramp_length

(* [put_on_track k car]: where the car ended up, said in the ribbon's
 * two numbers -- and, if that is past the rails, put back on the road.
 * Only the offset is clamped, so a kart meeting a rail at an angle
 * scrapes along it and keeps driving, which is the oldest collision
 * trick there is and what makes a wall feel smooth. *)
let put_on_track (k : kart) (car : Topdown.t) : kart =
  let s, offset = locate ~near:k.s car.x car.y in
  let p = Track3d.at track s in
  let edge = wall_offset p.width in
  let car, offset =
    if Float.abs offset <= edge then (car, offset)
    else
      (* Pushed back in *across* the road, and only across it. Rebuild
       * the whole position from (s, offset) instead -- the obvious
       * thing -- and a kart scraping a rail never advances at all:
       * [locate] measures the offset square across the segment while
       * Track3d.across lays it along the way the ribbon is drawn, and
       * seventeen out those two differ by about a quarter of a unit,
       * which is exactly how far a kart travels in a frame. The
       * rebuild then puts it back where it started, every frame.
       *
       * Scraping, not crashing: it keeps most of its speed, and the
       * rail turns it a little back along the road. Take the speed
       * away instead and a kart leaning on a wall is pinned there at
       * walking pace, since it goes on steering into it -- which is
       * every racing game's oldest complaint. *)
      let over = Float.abs offset -. edge in
      let along_track = 90. -. p.heading in
      let a = along_track *. Float.pi /. 180. in
      let inward = if offset > 0. then -.over else over in
      let x = car.x +. (inward *. sin a) and y = car.y -. (inward *. cos a) in
      let heading = car.heading +. Basics.clamp (-3.) 3. (angle_diff car.heading along_track) in
      ({ car with x; y; heading; speed = car.speed *. 0.97 }, Float.copy_sign edge offset)
  in
  (* the lap turns over when [s] jumps back across the start line *)
  let lap =
    if k.s > lap_length *. 0.75 && s < lap_length *. 0.25 then k.lap + 1
    else if k.s < lap_length *. 0.25 && s > lap_length *. 0.75 then k.lap - 1
    else k.lap
  in
  { k with car; s; offset; lap }

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
 * The hill is the other half of the driving: the slope of the road
 * under the kart is taken off its gas, so the long climb is slow and
 * the drop into the banked corner is fast, without anything as
 * expensive as gravity along a surface. *)
let step_kart (holding : bool) (gas : number) (steer : number) (road_top : number) (k : kart) : kart =
  let width = (Track3d.at track k.s).width in
  let top_here = Float.min road_top (top_speed_at width k.offset) in
  if k.spin > 0 then
    let car = Topdown.drive params top_here 0. 0. k.car in
    let car = { car with heading = car.heading +. 26. } in
    { (put_on_track k car) with spin = k.spin - 1; hop = 0 }
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
    (* a boost is half again as fast wherever it is used -- which is
     * what makes a mushroom over the grass a short cut *)
    let top = if boost > 0 then top_here *. 1.5 else top_here in
    let gas = if boost > 0 then 1. else gas in
    (* the slope the kart is about to drive up, or down *)
    let ahead = Float.max 0.5 (k.car.speed /. 60.) in
    let slope = (ground (k.s +. ahead) k.offset -. ground k.s k.offset) /. ahead in
    let gas = Basics.clamp (-1.) 1. (gas -. (slope *. 1.6)) in
    let car = Topdown.drive p top gas steer k.car in
    let k = { (put_on_track k car) with drift; hop; boost = max 0 (boost - 1) } in
    (* off the ramp, and back down: nothing steers what is in the air,
     * but nothing stops it either *)
    let was_on_ramp = on_ramp k.s k.offset in
    if k.air > 0. || k.rise > 0. then
      let air = k.air +. (k.rise /. 60.) and rise = k.rise -. (gravity /. 60.) in
      if air <= 0. then { k with air = 0.; rise = 0. } else { k with air; rise }
    else if was_on_ramp > 0. && on_ramp (k.s +. ahead) k.offset = 0. && k.car.speed > 18. then
      { k with air = was_on_ramp; rise = k.car.speed *. ramp_slope }
    else { k with air = 0.; rise = 0. }

(* karts closer than 2.6 pushed apart, half the overlap each: bumping,
 * not crashing (games2.5d/TinyMarioKart does the same) *)
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
 * player is a hundred ahead of it, and a little slower when the player
 * is behind. It is Mario Kart's most famous piece of cheating, and the
 * reason the last lap is always worth driving; it is also why a big
 * lead never feels safe, which players hate and keep playing. *)
let rubber (player : number) (k : kart) : number = 1. +. Basics.clamp (-0.05) 0.14 (0.0014 *. (player -. along k))

(* The computer's driving, now that the road is a ribbon: aim at the
 * middle of it some way ahead, shifted onto this kart's own line, and
 * ease off the gas if the road turns hard between here and there.
 * There are no waypoints left to follow -- [s] is a distance, so
 * "fourteen further on" is all a corner needs. *)
let computer_drive (k : kart) : number * number =
  let target_s = k.s +. 14. +. (k.car.speed *. 0.12) in
  let tx, ty = plane_at target_s k.line in
  let wanted = atan2 (ty -. k.car.y) (tx -. k.car.x) *. 180. /. Float.pi in
  let diff = angle_diff k.car.heading wanted in
  let steer = Basics.clamp (-1.) 1. (diff /. 20.) in
  let turn = Float.abs (angle_diff (Track3d.at track k.s).heading (Track3d.at track (k.s +. 26.)).heading) in
  let gas = if turn > 28. && k.car.speed > 30. then -0.2 else 0.95 in
  (gas, steer)

(* The computer's karts are the only ones that see the traffic coming:
 * a vehicle in the 20 ahead of one, and it steers away from the side
 * the vehicle is on, harder the closer it is (Craig Reynolds' obstacle
 * avoidance, in four lines). The player gets no such help, which is
 * the whole point of the traffic. *)
let avoid (traffic : vehicle array) (c : Topdown.t) : number =
  let a = c.heading *. Float.pi /. 180. in
  Array.fold_left
    (fun steer (v : vehicle) ->
      let vx, vy = plane_at v.vs v.lane in
      let dx = vx -. c.x and dy = vy -. c.y in
      (* how far in front of the kart the vehicle is, and how far to its left *)
      let ahead = (dx *. cos a) +. (dy *. sin a) and side = (dy *. cos a) -. (dx *. sin a) in
      if ahead < 1. || ahead > 20. || Float.abs side > 7. then steer
      else steer -. Float.copy_sign (1. -. (ahead /. 20.)) side)
    0. traffic

(* A player's hands on a frame: one player has the whole keyboard (up
 * for the gas), more share it and their karts accelerate by
 * themselves, each with a brake, a hop and an item key of its own. *)
type pad = { gas : number; steer : number; holding : bool; use : bool }

let step_karts (pads : pad array) (r : race) : race =
  (* the rubber band pulls towards the leading player *)
  let leader = Array.fold_left (fun m i -> Float.max m (along r.karts.(i))) neg_infinity (Array.init r.humans Fun.id) in
  let one (i : int) (k : kart) : kart =
    let mine = i < r.humans && r.places.(i) = None in
    let gas, steer =
      if mine then (pads.(i).gas, pads.(i).steer)
      else
        let gas, steer = computer_drive k in
        (gas, Basics.clamp (-1.) 1. (steer +. avoid r.traffic k.car))
    in
    (* the computer's karts powerslide too, through whatever corner
     * needs the wheel all the way over *)
    let holding = if mine then pads.(i).holding else Float.abs steer > 0.75 && k.car.speed > 25. in
    let top = if i < r.humans then k.base_top else k.base_top *. rubber leader k in
    step_kart holding gas steer top k
  in
  { r with karts = bump (Array.mapi one r.karts) }

(* the traffic is carried along the ribbon rather than driven: a lane
 * is an offset, and a lorry coming the other way is the same thing
 * with the sign of its speed turned round *)
let step_traffic (r : race) : race =
  let one (v : vehicle) : vehicle =
    let speed = (if v.lorry then lorry_speed else car_speed) /. 60. in
    let vs = v.vs +. if v.against then -.speed else speed in
    { v with vs = Float.rem (vs +. lap_length) lap_length }
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
    (fun b ((bs, bo) : number * number) ->
      if boxes.(b) = 0 then
        Array.iteri
          (fun i (k : kart) ->
            if boxes.(b) = 0 && k.item = None && k.roulette = 0 && k.air = 0.
               && Float.abs (lap_diff bs k.s) < 2.5 && Float.abs (k.offset -. bo) < 2.5
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
          let bs, bo = locate ~near:k.s bx by in
          { r with bananas = { bx; by; bh = ground bs bo } :: r.bananas }
      | Green_shell | Red_shell ->
          let sx, sy = ahead 3.4 in
          { r with
            shells =
              { sx; sy; shead = k.car.heading; homing = item = Red_shell; life = 300; owner = i } :: r.shells })

(* the roulette settling on an item, and the karts using what they
 * hold: the players on their key, the computers after a wait of their
 * own *)
let step_items (pads : pad array) (r : race) : race =
  let settle (i : int) (k : kart) : kart =
    if k.roulette > 1 then { k with roulette = k.roulette - 1 }
    else if k.roulette = 1 then
      { k with roulette = 0; item = Some (roll (place_of r i) ((r.frames / 7) + (i * 5))); wait = 40 + (i * 37 mod 110) }
    else if k.item <> None && i >= r.humans then { k with wait = k.wait - 1 }
    else k
  in
  let r = { r with karts = Array.mapi settle r.karts } in
  let r = Array.fold_left (fun r i -> if pads.(i).use && r.places.(i) = None then use_item i r else r) r (Array.init r.humans Fun.id) in
  let rec computers (i : int) (r : race) : race =
    if i >= Array.length r.karts then r
    else computers (i + 1) (if r.karts.(i).item <> None && r.karts.(i).wait <= 0 then use_item i r else r)
  in
  computers r.humans r

let shell_speed = 62.

(* The shells fly on their own, and a red one turns towards the kart
 * just ahead of the one who sent it -- by at most 7 degrees a frame,
 * so that it can still miss, and so that it takes the corner wide. A
 * shell dies against a rail; in the original it would bounce (an
 * exercise). *)
let step_shells (r : race) : race =
  let hit = Array.make (Array.length r.karts) false in
  let target (owner : int) : (number * number) option =
    let mine = along r.karts.(owner) in
    Array.fold_left
      (fun best (k : kart) ->
        let p = along k in
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
    let _, offset = locate sx sy in
    let gone = Float.abs offset > wall_offset 11. in
    if !struck || s.life <= 1 || gone then None else Some { s with sx; sy; shead; life = s.life - 1 }
  in
  let shells = List.filter_map step_one r.shells in
  { r with shells; karts = Array.mapi (fun i k -> if hit.(i) then spin_out 50 k else k) r.karts }

(* a banana lies where it was dropped until someone finds it *)
let step_bananas (r : race) : race =
  let hit = Array.make (Array.length r.karts) false in
  let keep (b : banana) : bool =
    let struck = ref false in
    Array.iteri
      (fun j (k : kart) ->
        if k.spin = 0 && k.air = 0. && Float.hypot (k.car.x -. b.bx) (k.car.y -. b.by) < 2.2 then begin
          hit.(j) <- true;
          struck := true
        end)
      r.karts;
    not !struck
  in
  let bananas = List.filter keep r.bananas in
  { r with bananas; karts = Array.mapi (fun i k -> if hit.(i) then spin_out 45 k else k) r.karts }

(* meeting the traffic, which costs more than a shell does; a kart in
 * the air goes over it *)
let step_crashes (r : race) : race =
  let crashed (k : kart) : bool =
    k.air < 1.5
    && Array.exists
         (fun (v : vehicle) ->
           let vx, vy = plane_at v.vs v.lane in
           Float.hypot (k.car.x -. vx) (k.car.y -. vy) < if v.lorry then 3.6 else 3.)
         r.traffic
  in
  { r with karts = Array.map (fun k -> if crashed k then spin_out 55 k else k) r.karts }

(* one frame of the race; a player who has finished is driven by the
 * computer, as in every Mario Kart *)
let step_race (pads : pad array) (r : race) : race =
  let r = step_traffic r in
  if r.ready > 0 then { r with ready = r.ready - 1 }
  else
    let r = step_karts pads r in
    let r = step_boxes r in
    let r = step_items pads r in
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
    let turn (i : int) (angle : number) : number =
      let player = r.karts.(i).car in
      let travel =
        if Float.hypot player.vx player.vy > 5. then atan2 player.vy player.vx *. 180. /. Float.pi
        else player.heading
      in
      angle +. (0.18 *. angle_diff angle travel)
    in
    let r = { r with view_angles = Array.mapi turn r.view_angles; frames = r.frames + 1 } in
    { r with
      places =
        Array.mapi (fun i p -> match p with None when r.karts.(i).lap >= laps -> Some (place_of r i) | p -> p) r.places }

(*****************************************************************************)
(* The battle: Block Fort *)
(*****************************************************************************)

(* Mario Kart 64's other game, for two to four players: no laps, an
 * arena, three balloons each, and a hit from a shell or a banana pops
 * one. The last kart with a balloon wins. Block Fort is the arena
 * everyone remembers: four square forts, one per colour, each with a
 * ramp up, their tops joined by bridges -- and the open floor running
 * under the bridges.
 *
 *        seen from above                      from the side
 *   +---------------------------+
 *   |  +-----+  bridge  +-----+ |        ramp  fort     bridge    fort
 *   |=>|  B  |==========|  R  |<=|          ___+----+=============+----+
 *   |  +-----+          +-----+ |        _/   |    |   floor     |    |
 *   |     ||    floor     ||    |   ____/     |    |  (under)    |    |
 *   |  +-----+          +-----+ |
 *   |=>|  G  |==========|  Y  |<=|   => the ramps, on the outside
 *   |  +-----+          +-----+ |
 *   +---------------------------+
 *
 * So the ground is not one height per place any more: under a bridge
 * there are two, the floor and the bridge. A kart finds its level by
 * its own height ([level]): the highest surface under it that is no
 * more than a step above it -- on the floor under a bridge, the bridge
 * is far over its head; on the bridge, the floor is far below. A fort
 * is solid from the floor to its top ([solid]): a kart meeting its side
 * bounces off, and so does a shell -- in battle a green shell does not
 * die against a wall, it comes back. *)

let arena = 60. (* the walls, at +-60 *)
let fort_height = 5.
let fort_half = 13. (* each fort a square of 26 *)
let fort_center = 28.

(* a fort's square: its centre's signs, (+1, +1) the north-east one *)
let forts = [ (1., 1.); (-1., 1.); (-1., -1.); (1., -1.) ]
let fort_colors = [| (220, 60, 50); (60, 100, 220); (60, 170, 70); (240, 200, 40) |]

let in_fort (x : number) (y : number) : bool =
  List.exists
    (fun (sx, sy) -> Float.abs (x -. (sx *. fort_center)) < fort_half && Float.abs (y -. (sy *. fort_center)) < fort_half)
    forts

(* the bridges, joining the forts round a ring at their tops: along x
 * between the north ones and the south ones, along y between the east
 * ones and the west ones; 7 wide *)
let bridge_half = 3.5

let on_bridge (x : number) (y : number) : bool =
  let gap = fort_center -. fort_half in
  (Float.abs x < gap && Float.abs (Float.abs y -. fort_center) < bridge_half)
  || (Float.abs y < gap && Float.abs (Float.abs x -. fort_center) < bridge_half)

(* the ramps, one on the outside of each fort, 14 long: the height on
 * one if (x, y) is on it *)
let ramp_length_bf = 14.
let ramp_half_bf = 5.

let ramp_height (x : number) (y : number) : number option =
  let outer = fort_center +. fort_half in
  let d = Float.abs x -. outer in
  if d >= 0. && d < ramp_length_bf && Float.abs (Float.abs y -. fort_center) < ramp_half_bf then
    Some (fort_height *. (1. -. (d /. ramp_length_bf)))
  else None

(* where a kart at height [h] stands, at (x, y): the highest surface no
 * more than a step (1) above it *)
let level (x : number) (y : number) (h : number) : number =
  let tops = (if in_fort x y || on_bridge x y then [ fort_height ] else []) @ Option.to_list (ramp_height x y) in
  List.fold_left (fun best t -> if t <= h +. 1. && t > best then t else best) 0. tops

(* a fort's side, for whatever is lower than its top *)
let solid_at (x : number) (y : number) (h : number) : bool =
  in_fort x y && h < fort_height -. 0.5 && ramp_height x y = None

let outside (x : number) (y : number) : bool = Float.abs x > arena || Float.abs y > arena

type fighter = {
  kart : kart; (* the car, its colour, spin, boost, item and roulette; the rest unused *)
  h : number; (* its height *)
  fall : number; (* how fast it is falling *)
  balloons : int;
  safe : int; (* frames it can't be hit, after a hit *)
}

type bshell = { bsx : number; bsy : number; bsh : number; vx : number; vy : number; seeking : bool; blife : int; by_ : int }

type battle = {
  fighters : fighter array;
  bshells : bshell list;
  peels : (number * number * number) list; (* the bananas, (x, y, height) *)
  crates : int array; (* per item box, as in the race *)
  angles : number array; (* each player's camera heading *)
  bframes : int;
  bready : int;
}

(* the item boxes: one on each fort, one on the floor between each two
 * forts, one in the middle *)
let crate_places : (number * number) list =
  List.map (fun (sx, sy) -> (sx *. fort_center, sy *. fort_center)) forts
  @ [ (0., 45.); (0., -45.); (45., 0.); (-45., 0.); (0., 0.) ]

let crate_height (x : number) (y : number) : number = if in_fort x y then fort_height else 0.

let new_battle (humans : int) : battle =
  let fighter (i : int) : fighter =
    (* each starts on the floor at an end of the cross the forts leave
     * open, facing the middle: they meet under the bridges *)
    let x, y = List.nth [ (0., -48.); (0., 48.); (-48., 0.); (48., 0.) ] i in
    let heading = atan2 (-.y) (-.x) *. 180. /. Float.pi in
    let k = (new_race 1).karts.(0) in
    { kart = { k with car = { k.car with x; y; heading; vx = 0.; vy = 0.; speed = 0. }; color = kart_colors.(i) };
      h = 0.; fall = 0.; balloons = 3; safe = 0 }
  in
  { fighters = Array.init humans fighter; bshells = []; peels = []; crates = Array.make (List.length crate_places) 0;
    angles = Array.init humans (fun i -> List.nth [ 90.; -90.; 0.; 180. ] i);
    bframes = 0; bready = 120 }

let alive (f : fighter) : bool = f.balloons > 0

(* one frame of one fighter: driven on the plane as in the race, then
 * its level found, a fort's side bounced off, the arena's wall too, and
 * the fall off an edge *)
let step_fighter (pad : pad) (f : fighter) : fighter =
  if not (alive f) then f
  else
    let k = f.kart in
    let car =
      if k.spin > 0 then
        let car = Topdown.drive params road_speed 0. 0. k.car in
        { car with heading = car.heading +. 26. }
      else
        let boosted = k.boost > 0 in
        Topdown.drive (if boosted then { params with accel = params.accel *. 3. } else params)
          (if boosted then road_speed *. 1.5 else road_speed)
          (if boosted then 1. else pad.gas) pad.steer k.car
    in
    let blocked = outside car.x car.y || solid_at car.x car.y f.h in
    let car = if blocked then { k.car with vx = -0.4 *. k.car.vx; vy = -0.4 *. k.car.vy; speed = -0.4 *. k.car.speed } else car in
    let under = level car.x car.y f.h in
    let h, fall =
      if f.h > under +. 0.01 || f.fall > 0. then
        let h = f.h -. (f.fall /. 60.) and fall = f.fall +. (gravity /. 60.) in
        if h <= under then (under, 0.) else (h, fall)
      else (under, 0.)
    in
    { f with
      kart = { k with car; spin = max 0 (k.spin - 1); boost = max 0 (k.boost - 1) };
      h; fall; safe = max 0 (f.safe - 1) }

(* a hit: a balloon less, a spin, and a moment when nothing else can hit *)
let pop (f : fighter) : fighter =
  if f.safe > 0 || not (alive f) then f
  else { f with balloons = f.balloons - 1; safe = 90; kart = spin_out 50 f.kart }

let near3 ((x, y, h) : number * number * number) (f : fighter) (r : number) : bool =
  alive f && Float.hypot (f.kart.car.x -. x) (f.kart.car.y -. y) < r && Float.abs (f.h -. h) < 2.

(* the shells fly at their height and bounce off everything: the
 * arena's walls and the forts' sides, the velocity's x or y turned
 * round, whichever took it in; a red one turns towards the nearest
 * other kart *)
let step_bshell (fighters : fighter array) (b : bshell) : bshell option * int option =
  let vx, vy =
    if not b.seeking then (b.vx, b.vy)
    else
      let target =
        Array.to_list fighters
        |> List.mapi (fun i f -> (i, f))
        |> List.filter (fun (i, f) -> i <> b.by_ && alive f)
        |> List.fold_left
             (fun best (_, f) ->
               let d = Float.hypot (f.kart.car.x -. b.bsx) (f.kart.car.y -. b.bsy) in
               match best with Some (bd, _) when bd <= d -> best | _ -> Some (d, f))
             None
      in
      match target with
      | None -> (b.vx, b.vy)
      | Some (_, f) ->
          let speed = Float.hypot b.vx b.vy in
          let want = atan2 (f.kart.car.y -. b.bsy) (f.kart.car.x -. b.bsx) and now = atan2 b.vy b.vx in
          let turn = Basics.clamp (-0.12) 0.12 (angle_diff (now *. 180. /. Float.pi) (want *. 180. /. Float.pi) *. Float.pi /. 180.) in
          (speed *. cos (now +. turn), speed *. sin (now +. turn))
  in
  let x = b.bsx +. (vx /. 60.) and y = b.bsy +. (vy /. 60.) in
  let wall_x = outside x b.bsy || solid_at x b.bsy b.bsh and wall_y = outside b.bsx y || solid_at b.bsx y b.bsh in
  let vx = if wall_x then -.vx else vx and vy = if wall_y then -.vy else vy in
  let x = if wall_x then b.bsx else x and y = if wall_y then b.bsy else y in
  let h = level x y b.bsh in
  let struck = ref None in
  Array.iteri (fun i f -> if !struck = None && (i <> b.by_ || b.blife < 280) && f.safe = 0 && near3 (x, y, h) f 2.4 then struck := Some i) fighters;
  if !struck <> None || b.blife <= 1 then (None, !struck)
  else (Some { b with bsx = x; bsy = y; bsh = h; vx; vy; blife = b.blife - 1 }, None)

let use_battle_item (i : int) (bt : battle) : battle =
  let f = bt.fighters.(i) in
  match f.kart.item with
  | None -> bt
  | Some item ->
      let f = { f with kart = { f.kart with item = None } } in
      let a = f.kart.car.heading *. Float.pi /. 180. in
      let ahead d = (f.kart.car.x +. (d *. cos a), f.kart.car.y +. (d *. sin a)) in
      let fighters = Array.mapi (fun j g -> if j = i then f else g) bt.fighters in
      let bt = { bt with fighters } in
      match item with
      | Mushroom -> { bt with fighters = Array.mapi (fun j g -> if j = i then { g with kart = { g.kart with boost = 75 } } else g) fighters }
      | Banana -> let x, y = ahead (-3.4) in { bt with peels = (x, y, f.h) :: bt.peels }
      | Green_shell | Red_shell ->
          let x, y = ahead 3.4 in
          { bt with
            bshells =
              { bsx = x; bsy = y; bsh = f.h; vx = shell_speed *. cos a; vy = shell_speed *. sin a; seeking = item = Red_shell;
                blife = 360; by_ = i }
              :: bt.bshells }

(* one frame of the battle *)
let step_battle (pads : pad array) (bt : battle) : battle =
  if bt.bready > 0 then { bt with bready = bt.bready - 1 }
  else
    let fighters = Array.mapi (fun i f -> step_fighter pads.(i) f) bt.fighters in
    (* the karts bump as in the race *)
    let bumped = bump (Array.map (fun f -> f.kart) fighters) in
    let fighters = Array.mapi (fun i f -> { f with kart = bumped.(i) }) fighters in
    (* the item boxes: the roulette as in the race, an item from all
     * four whatever the place *)
    let crates = Array.copy bt.crates in
    let fighters =
      Array.mapi
        (fun i f ->
          let k = f.kart in
          let k =
            if k.roulette > 1 then { k with roulette = k.roulette - 1 }
            else if k.roulette = 1 then
              { k with roulette = 0; item = Some (List.nth [ Green_shell; Red_shell; Banana; Mushroom ] ((bt.bframes / 7 + (i * 3)) mod 4)) }
            else k
          in
          let k = ref k in
          List.iteri
            (fun c (x, y) ->
              if crates.(c) = 0 && !k.item = None && !k.roulette = 0 && alive f && near3 (x, y, crate_height x y) f 2.6 then begin
                k := { !k with roulette = 42 };
                crates.(c) <- 240
              end)
            crate_places;
          { f with kart = !k })
        fighters
    in
    let bt = { bt with fighters; crates = Array.map (fun n -> max 0 (n - 1)) crates } in
    let bt = Array.fold_left (fun bt i -> if pads.(i).use && alive bt.fighters.(i) then use_battle_item i bt else bt) bt (Array.init (Array.length bt.fighters) Fun.id) in
    let moved = List.map (step_bshell bt.fighters) bt.bshells in
    let hits = List.filter_map snd moved in
    let peels_hit = List.filter_map (fun (x, y, h) -> let hit = ref None in Array.iteri (fun i f -> if !hit = None && f.safe = 0 && near3 (x, y, h) f 2.2 then hit := Some i) bt.fighters; !hit) bt.peels in
    let peels = List.filter (fun (x, y, h) -> not (Array.exists (fun f -> f.safe = 0 && near3 (x, y, h) f 2.2) bt.fighters)) bt.peels in
    let fighters = Array.mapi (fun i f -> if List.mem i hits || List.mem i peels_hit then pop f else f) bt.fighters in
    let turn i angle =
      let car = fighters.(i).kart.car in
      let travel = if Float.hypot car.vx car.vy > 5. then atan2 car.vy car.vx *. 180. /. Float.pi else car.heading in
      angle +. (0.18 *. angle_diff angle travel)
    in
    { bt with fighters; bshells = List.filter_map fst moved; peels; angles = Array.mapi turn bt.angles; bframes = bt.bframes + 1 }

(* the winner, once only one kart has a balloon left *)
let battle_winner (bt : battle) : int option =
  match List.filter (fun i -> alive bt.fighters.(i)) (List.init (Array.length bt.fighters) Fun.id) with
  | [ i ] -> Some i
  | _ -> None

(* the title remembers how many players *)
type scene = Title of int | Racing of race | Finished of race | Battling of battle | Battle_over of battle * int
type model = scene Scene2d.t

let initial_model : model = Scene2d.start (Title 1)

let no_pad = { gas = 0.; steer = 0.; holding = false; use = false }

(* The hands of [humans] players on this frame. Alone, the arrows (up
 * the gas), shift and space; together, each a left, a right, a brake,
 * a hop and an item key (the gas is theirs by default):
 *
 *     player 1   arrows: left right, down brakes, up the item; shift hops
 *     player 2   a d, s brakes, w the item, q hops
 *     player 3   j l, k brakes, i the item, u hops
 *     player 4   f h, g brakes, t the item, r hops
 *)
let pads (computer : computer) (m : model) (humans : int) : pad array =
  let k = computer.keyboard in
  let down key = Set_.mem key k.keys and pressed key = Scene2d.pressed (fun k -> Set_.mem key k.keys) m in
  if humans = 1 then
    [| { gas = axis k.kup k.kdown; steer = axis k.kleft k.kright; holding = k.kshift; use = Scene2d.pressed (fun k -> k.kspace) m } |]
  else
    let first = { gas = (if k.kdown then -1. else 1.); steer = axis k.kleft k.kright; holding = k.kshift; use = Scene2d.pressed (fun k -> k.kup) m } in
    let other (left, right, brake, hop, item) =
      { gas = (if down brake then -1. else 1.); steer = axis (down left) (down right); holding = down hop; use = pressed item }
    in
    Array.init humans (fun i ->
        if i = 0 then first else other (List.nth [ ("a", "d", "s", "q", "w"); ("j", "l", "k", "u", "i"); ("f", "h", "g", "r", "t") ] (i - 1)))

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  let digit d = Scene2d.pressed (fun k -> Set_.mem d k.keys) m in
  match m.scene with
  | Title n ->
      let n = List.fold_left (fun n d -> if digit (string_of_int d) then d else n) n [ 1; 2; 3; 4 ] in
      if space then Scene2d.go (Racing (new_race n)) m
      else if Scene2d.pressed (fun k -> Set_.mem "b" k.keys) m then Scene2d.go (Battling (new_battle (max 2 n))) m
      else { m with scene = Title n }
  | Racing r ->
      let r = step_race (pads computer m r.humans) r in
      if Array.for_all Option.is_some r.places then Scene2d.go (Finished r) m else { m with scene = Racing r }
  | Finished r ->
      if space then Scene2d.go (Title r.humans) m else { m with scene = Finished (step_race (Array.make r.humans no_pad) r) }
  | Battling bt -> (
      let bt = step_battle (pads computer m (Array.length bt.fighters)) bt in
      match battle_winner bt with Some i -> Scene2d.go (Battle_over (bt, i)) m | None -> { m with scene = Battling bt })
  | Battle_over (bt, i) ->
      if space then Scene2d.go (Title (Array.length bt.fighters)) m
      else { m with scene = Battle_over (step_battle (Array.make (Array.length bt.fighters) no_pad) bt, i) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* The camera's right on the ground, and its heading on the plane: the
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
         let x, y, z = Track3d.across track v.vs v.lane in
         let p = Track3d.at track v.vs in
         let heading = if v.against then p.heading +. 180. else p.heading in
         (if v.lorry then lorry_model else car_model) |> rotate3d 0. (-.heading) 0. |> move3d x y z)

(* an item box: a cube turning over the road, and the road showing
 * through the gap under it *)
let box_shapes (time : time) (r : race) : shape3d list =
  Array.to_list item_boxes
  |> List.mapi (fun b ((bs, bo) : number * number) ->
         if r.boxes.(b) > 0 then []
         else
           let x, y, z = Track3d.across track bs bo in
           [ solid (245, 205, 55) 2.2 2.2 2.2 |> rotate3d 0. (spin 2.5 time) 0. |> move3d x (y +. 1.8) z ])
  |> List.concat

(* a kart: its shadow on the road, the drawing of it, and, while it
 * slides, the sparks under its wheels. In the air the two come apart:
 * the shadow stays on the road, which is the only thing that says how
 * high the kart is (games3d/TinyMario64's lesson). *)
let kart_shapes (right : number * number) (angle : number) (k : kart) : shape3d list =
  let floor = ground k.s k.offset +. on_ramp k.s k.offset in
  let x, _, z = world k.car.x k.car.y 0. in
  let hop = if k.hop > 0 then 0.9 *. sin (Float.pi *. float_of_int k.hop /. 12.) else 0. in
  let y = floor +. k.air +. hop in
  (* a dark patch rather than a faded black one: the software
   * rasterizer draws no alpha (see playground3d/software), so a shadow
   * that counted on [fade3d] would be a hole in the road there *)
  let shadow =
    polygon3d (rgb 52 62 52)
      [ (x -. 1.2, floor +. 0.04, z -. 0.85); (x -. 1.2, floor +. 0.04, z +. 0.85);
        (x +. 1.2, floor +. 0.04, z +. 0.85); (x +. 1.2, floor +. 0.04, z -. 0.85) ]
  in
  let sparks =
    match k.drift with
    | Sliding (side, charge) when charge > 40 ->
        let color = if charge > 85 then rgb 255 140 30 else rgb 255 230 80 in
        let a = k.car.heading *. Float.pi /. 180. in
        (* under the outside rear wheel, the one doing the sliding *)
        let back = -1.1 and out = 1.2 *. -.side in
        let sx = k.car.x +. (back *. cos a) -. (out *. sin a) and sy = k.car.y +. (back *. sin a) +. (out *. cos a) in
        let px, _, pz = world sx sy 0. in
        billboard right 0.32 [ ('S', color) ] [ ".S."; "SSS"; ".S." ] (px, floor +. 0.1, pz)
    | _ -> []
  in
  (shadow :: billboard right (2.6 /. 16.) (kart_palette k.color) (drawing angle k.car.heading) (x, y, z)) @ sparks

let item_art (i : item) : string list * (char * color) list =
  match i with
  | Banana -> (banana_rows, banana_palette)
  | Green_shell -> (shell_rows, shell_palette (rgb 60 190 70))
  | Red_shell -> (shell_rows, shell_palette (rgb 225 60 55))
  | Mushroom -> (mushroom_rows, mushroom_palette)

let thing_shapes (right : number * number) (r : race) : shape3d list =
  List.concat_map
    (fun (b : banana) ->
      let x, _, z = world b.bx b.by 0. in
      billboard right 0.16 banana_palette banana_rows (x, b.bh +. 0.1, z))
    r.bananas
  @ List.concat_map
      (fun (s : shell) ->
        let rows, palette = item_art (if s.homing then Red_shell else Green_shell) in
        let ss, so = locate s.sx s.sy in
        let x, _, z = world s.sx s.sy 0. in
        billboard right 0.18 palette rows (x, ground ss so +. 0.1, z))
      r.shells

(* the trees, only the ones near enough along the lap to be worth
 * drawing: a sprite costs a quad per run of pixels, and the far ones
 * are a few pixels *)
let tree_shapes (right : number * number) (s : number) : shape3d list =
  List.concat_map
    (fun ((ts, to_) : number * number) ->
      if Float.abs (lap_diff ts s) > 95. then []
      else
        let x, y, z = Track3d.across track ts to_ in
        billboard right 0.55 tree_palette tree_rows (x, y, z))
    tree_places

(*****************************************************************************)
(* The screen *)
(*****************************************************************************)

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let ordinal (n : int) : string =
  match n with 1 -> "1ST" | 2 -> "2ND" | 3 -> "3RD" | n -> string_of_int n ^ "TH"

(* the whole circuit from above, a dot per kart: the same race the
 * screen shows, seen the way games/TinyMicroMachines draws it for real
 * -- and, now that the course is a ribbon rather than a map, drawn by
 * walking the middle of it *)
let minimap (screen : screen) (r : race) : shape list =
  let scale = 0.52 in
  let ox = screen.right -. 95. and oy = screen.bottom +. 70. in
  let at_screen (x : number) (z : number) : number * number = (ox +. (x *. scale), oy -. (z *. scale)) in
  let road =
    List.init (Track3d.segments track / 3) (fun i ->
        let p = Track3d.at track (float_of_int (i * 3) *. Track3d.step track) in
        let x, y = at_screen p.px p.pz in
        circle (rgb 120 120 128) 3.5 |> move x y)
  in
  let dot (k : kart) : shape =
    let x, _, z = world k.car.x k.car.y 0. in
    let sx, sy = at_screen x z in
    circle k.color 4. |> move sx sy
  in
  let sx, sy = at_screen (Track3d.at track start_line).px (Track3d.at track start_line).pz in
  [ rectangle (rgb 25 25 30) 185. 140. |> move ox oy |> fade 0.6 ]
  @ road
  @ [ rectangle white 8. 8. |> move sx sy ]
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

(* player [i]'s HUD, on its view's screen *)
let view_hud (screen : screen) (r : race) (i : int) : shape list =
  let player = r.karts.(i) in
  let lap = min laps (max 1 (player.lap + 1)) in
  let time = float_of_int r.frames /. 60. in
  let place = match r.places.(i) with Some n -> n | None -> place_of r i in
  [ text white 3. (Printf.sprintf "LAP %d/%d" lap laps) |> move (screen.left +. 110.) (screen.top -. 40.);
    text yellow 5. (ordinal place) |> move (screen.right -. 90.) (screen.top -. 45.);
    text white 3. (Printf.sprintf "%d:%04.1f" (int_of_float time / 60) (Float.rem time 60.))
    |> move (screen.right -. 250.) (screen.top -. 40.) ]
  @ item_slot screen player
  @ (if r.humans <= 2 then minimap screen r else [])
  @ (match r.places.(i) with
    | Some n ->
        [ rectangle black (Float.min 700. screen.width) 160. |> move_y (screen.top *. 0.46) |> fade 0.55;
          text yellow (if r.humans > 2 then 6. else 9.) (ordinal n ^ " PLACE!") |> move_y (screen.top *. 0.5) ]
    | None ->
        if r.ready > 0 then [ text yellow 9. (string_of_int ((r.ready + 59) / 60)) |> move_y (screen.top *. 0.4) ]
        else if r.frames < 40 then [ text yellow 9. "GO!" |> move_y (screen.top *. 0.4) ]
        else [])

(* the land beyond the circuit, a blue sky and a hazy horizon; the sky
 * is seen from below, which is why the game draws back faces too *)
let sky_and_land (cam : camera) : shape3d list =
  Camera3d.floor ~color:(rgb 78 158 66) ~ground:(-6.) cam
  :: Camera3d.sky ~sky:(rgb 150 200 250) ~horizon:(rgb 96 166 82) ~ground:(-6.) cam

(* the race as player [i] sees it: the camera behind their kart, and
 * the world drawn for that camera -- the sprites turned towards it,
 * the trees near that kart *)
let race_world (time : time) (r : race) (cam : camera) (near : kart) : shape3d list =
  let right = camera_right cam and angle = camera_angle cam in
  sky_and_land cam @ [ circuit ] @ tree_shapes right near.s @ traffic_shapes r
  @ box_shapes time r @ thing_shapes right r
  @ List.concat_map (kart_shapes right angle) (Array.to_list r.karts)

let behind (r : race) (i : int) : camera =
  let player = r.karts.(i) in
  let px, _, pz = world player.car.x player.car.y 0. in
  let py = ground player.s player.offset +. on_ramp player.s player.offset +. player.air in
  Camera3d.behind ~back:13. ~height:5.5 ~ahead:9. ~look:2.2 { x = px; y = py; z = pz; heading = heading3d r.view_angles.(i) }

(* Block Fort in polygons: the floor, the walls round it, the four forts
 * in their colours, the bridges between their tops, the ramps up *)
let block_fort : shape3d =
  let floor =
    polygon3d (rgb 120 120 128)
      (List.map (fun (x, y) -> world x y 0.) [ (-.arena, -.arena); (arena, -.arena); (arena, arena); (-.arena, arena) ])
  in
  let wall (x, y, w, d) =
    let wx, _, wz = world x y 0. in
    solid (190, 190, 200) w 2.5 d |> move3d wx 1.25 wz
  in
  let fort i (sx, sy) =
    let wx, _, wz = world (sx *. fort_center) (sy *. fort_center) 0. in
    solid fort_colors.(i) (2. *. fort_half) fort_height (2. *. fort_half) |> move3d wx (fort_height /. 2.) wz
  in
  let gap = fort_center -. fort_half in
  let bridge (x, y, w, d) =
    let wx, _, wz = world x y 0. in
    solid (230, 230, 235) w 0.6 d |> move3d wx (fort_height -. 0.3) wz
  in
  let ramp (sx, sy) =
    let outer = fort_center +. fort_half in
    let x0 = sx *. outer and x1 = sx *. (outer +. ramp_length_bf) in
    let y0 = (sy *. fort_center) -. ramp_half_bf and y1 = (sy *. fort_center) +. ramp_half_bf in
    polygon3d (rgb 170 130 80)
      [ world x0 y0 fort_height; world x1 y0 0.05; world x1 y1 0.05; world x0 y1 fort_height ]
  in
  group3d
    ([ floor ]
    @ List.map wall [ (0., arena +. 0.5, 2. *. arena, 1.); (0., -.arena -. 0.5, 2. *. arena, 1.); (arena +. 0.5, 0., 1., 2. *. arena); (-.arena -. 0.5, 0., 1., 2. *. arena) ]
    @ List.mapi fort forts
    @ List.map bridge [ (0., fort_center, 2. *. gap, 2. *. bridge_half); (0., -.fort_center, 2. *. gap, 2. *. bridge_half);
                        (fort_center, 0., 2. *. bridge_half, 2. *. gap); (-.fort_center, 0., 2. *. bridge_half, 2. *. gap) ]
    @ List.map ramp forts)

(* a fighter: its kart as in the race, its shadow on the level below
 * it, and its balloons over it, one colour each *)
let fighter_shapes (right : number * number) (angle : number) (f : fighter) : shape3d list =
  if not (alive f) then []
  else
    let k = f.kart in
    let x, _, z = world k.car.x k.car.y 0. in
    let floor = level k.car.x k.car.y f.h in
    let shadow =
      polygon3d (rgb 60 60 66)
        [ (x -. 1.2, floor +. 0.04, z -. 0.85); (x -. 1.2, floor +. 0.04, z +. 0.85);
          (x +. 1.2, floor +. 0.04, z +. 0.85); (x +. 1.2, floor +. 0.04, z -. 0.85) ]
    in
    let flicker = f.safe > 0 && f.safe / 6 mod 2 = 0 in
    let balloons =
      List.concat
        (List.init f.balloons (fun b ->
             let bx = (float_of_int b -. (float_of_int (f.balloons - 1) /. 2.)) *. 0.8 in
             billboard right 0.13 [ ('O', k.color); ('s', white) ] [ ".OOO."; "OOOOO"; "OOOOO"; ".OOO."; "..s.."; "..s.." ]
               (x +. (bx *. fst right), f.h +. 2.6, z +. (bx *. snd right))))
    in
    (shadow :: (if flicker then [] else billboard right (2.6 /. 16.) (kart_palette k.color) (drawing angle k.car.heading) (x, f.h, z)))
    @ balloons

let battle_world (time : time) (bt : battle) (cam : camera) : shape3d list =
  let right = camera_right cam and angle = camera_angle cam in
  sky_and_land cam @ [ block_fort ]
  @ List.concat
      (List.mapi
         (fun c (x, y) ->
           if bt.crates.(c) > 0 then []
           else
             let wx, _, wz = world x y 0. in
             [ solid (245, 205, 55) 2.2 2.2 2.2 |> rotate3d 0. (spin 2.5 time) 0. |> move3d wx (crate_height x y +. 1.8) wz ])
         crate_places)
  @ List.concat_map
      (fun (x, y, h) -> let wx, _, wz = world x y 0. in billboard right 0.16 banana_palette banana_rows (wx, h +. 0.1, wz))
      bt.peels
  @ List.concat_map
      (fun b ->
        let rows, palette = item_art (if b.seeking then Red_shell else Green_shell) in
        let wx, _, wz = world b.bsx b.bsy 0. in
        billboard right 0.18 palette rows (wx, b.bsh +. 0.1, wz))
      bt.bshells
  @ List.concat_map (fighter_shapes right angle) (Array.to_list bt.fighters)

let battle_hud (screen : screen) (bt : battle) (i : int) (winner : int option) : shape list =
  let f = bt.fighters.(i) in
  [ text white 3. "BALLOONS" |> move (screen.left +. 150.) (screen.top -. 40.) ]
  @ List.init f.balloons (fun b -> circle f.kart.color 14. |> move (screen.left +. 260. +. (float_of_int b *. 34.)) (screen.top -. 40.))
  @ item_slot screen f.kart
  @ (match winner with
    | Some w when w = i -> [ text yellow 7. "WINNER!" |> move_y (screen.top *. 0.4) ]
    | _ ->
        if not (alive f) then [ text (rgb 230 80 80) 7. "OUT" |> move_y (screen.top *. 0.4) ]
        else if bt.bready > 0 then [ text yellow 9. (string_of_int ((bt.bready + 59) / 60)) |> move_y (screen.top *. 0.4) ]
        else if bt.bframes < 40 then [ text yellow 9. "GO!" |> move_y (screen.top *. 0.4) ]
        else [])

(* in a split screen, a black frame round each view, as the N64 drew
 * them: without it a fort at the edge of one view runs on into the
 * next *)
let frame (screen : screen) (n : int) : shape list =
  if n = 1 then []
  else
    [ rectangle black screen.width 4. |> move_y (screen.top -. 2.); rectangle black screen.width 4. |> move_y (screen.bottom +. 2.);
      rectangle black 4. screen.height |> move_x (screen.left +. 2.); rectangle black 4. screen.height |> move_x (screen.right -. 2.) ]

(* one view per player, split as Playground3d.split lays them out *)
let view (computer : computer) (m : model) : view list =
  let screen = computer.screen in
  match m.scene with
  | Title n ->
      let r = new_race 1 in
      let player = r.karts.(0) in
      let px, _, pz = world player.car.x player.car.y 0. in
      let py = ground player.s player.offset in
      let cam = Camera3d.orbit ~distance:22. ~height:8. ~look:1.5 (spin 14. computer.time) (px, py +. 1., pz) in
      let hud_shapes =
        [ text (rgb 235 45 40) 7. "TINY MARIO KART 64" |> move_y 320.;
          rectangle black 900. 260. |> move_y (-270.) |> fade 0.55;
          text white 2.5 "up: gas   down: brake   left/right: steer" |> move_y (-190.);
          text white 2.5 "shift: hop and slide (hold it round a bend)   space: use your item" |> move_y (-240.);
          text white 2.5 "3 laps, 7 karts, a hill, a ramp and the traffic" |> move_y (-290.);
          text (rgb 250 220 120) 2.5 (Printf.sprintf "1 2 3 4: players (%d), the screen split   b: battle" n) |> move_y (-340.) ]
        @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-410.) ]
      in
      [ { camera = cam; area = whole; shapes = race_world computer.time r cam player @ List.map hud hud_shapes } ]
  | Racing r | Finished r ->
      List.mapi
        (fun i area ->
          let cam = behind r i in
          let hud_shapes =
            view_hud (area_screen screen area) r i @ frame (area_screen screen area) r.humans
            @ (match m.scene with
              | Finished _ -> Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y (if r.humans = 1 then 160. else -40.) ]
              | _ -> [])
          in
          { camera = cam; area; shapes = race_world computer.time r cam r.karts.(i) @ List.map hud hud_shapes })
        (split r.humans)
  | Battling bt | Battle_over (bt, _) ->
      let winner = match m.scene with Battle_over (_, w) -> Some w | _ -> None in
      List.mapi
        (fun i area ->
          let f = bt.fighters.(i) in
          let px, _, pz = world f.kart.car.x f.kart.car.y 0. in
          let cam =
            Camera3d.behind ~back:13. ~height:5.5 ~ahead:9. ~look:2.2 { x = px; y = f.h; z = pz; heading = heading3d bt.angles.(i) }
          in
          let hud_shapes =
            battle_hud (area_screen screen area) bt i winner @ frame (area_screen screen area) (Array.length bt.fighters)
            @ (match winner with Some _ -> Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y (-40.) ] | None -> [])
          in
          { camera = cam; area; shapes = battle_world computer.time bt cam @ List.map hud hud_shapes })
        (split (Array.length bt.fighters))

let app = split3d view update initial_model

(* No_lighting: the sprites must keep the colours they are drawn with
 * whatever way the camera looks at them, so this game lights its
 * polygons itself ([solid], [shade]) -- see the header. The back faces
 * are drawn too, for the sky (Camera3d.sky). *)
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = No_lighting; backface_culling = false }
    app
