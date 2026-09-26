(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Virtua Racing (Yu Suzuki, Sega AM2, 1992): five
 * laps of one of its three courses -- Big Forest, Bay Bridge,
 * Acropolis -- in a formula car, against fifteen others and the clock;
 * or TinyOutRun's coast, a stage, alone. Up to accelerate, down to
 * brake, left/right to steer, v to change the view; left/right on the
 * course select to choose, space to race.
 *
 * Virtua Racing was the arcade's first great polygon racer: flat-shaded,
 * no textures, 60 frames per second, on a board (Model 1) built for it.
 * Its look is exactly playground3d's: every surface one flat color, lit
 * by the sun -- the hills a patchwork of facets, no two lit alike. Its
 * courses were famous for their landmarks, each a heap of polygons no
 * sprite game could have turned around: Big Forest's amusement park,
 * Bay Bridge's suspension bridge and windmills, Acropolis' ruins above
 * the sea. Its four views were the other novelty, a button each (the
 * "V.R." buttons; here the v key goes round them): in the cockpit, close
 * behind, behind and above, high overhead.
 *
 * What is the arcade's here:
 *   - the courses' shapes, traced from the course select's maps in the
 *     Mega Drive version's manual, and their corners in the order the
 *     players' guides give them, with their landmarks where the guides
 *     put them (their heights and the scenery between are made up);
 *   - the rules: a clock counting down, checkpoints giving more time
 *     ("TIME BONUS"), five laps, sixteen cars from a grid, your place
 *     ("9TH/16"), GAME OVER when the time runs out;
 *   - the screen: the time at the top, the lap time and the best one on
 *     the right, the tachometer's arc on the left, the speed and the
 *     laps under it, the course's map at the bottom right;
 *   - the car: a formula car with a seven-speed box (the automatic one),
 *     slippery, that spins when it slides too far or hits too hard, and
 *     is set back facing the road after;
 *   - no music in the race: the engine, the tyres, the jingles at the
 *     checkpoints, the engine booming in Bay Bridge's tunnel.
 * Not here (exercises): the manual box, the pit stops, the crashes'
 * flips, the other players, the rivals' own mistakes.
 *
 * Next to TinyOutRun, the lesson is what polygons changed:
 *   - the road really turns: the racing kit's Track3d walks a course
 *     into a ribbon in space, each segment of road a quad between two
 *     edges of the center line; in TinyOutRun a curve is only a
 *     sideways shift of the picture;
 *   - and it leans: a table of segments cannot say which way the road
 *     is tilted, a shape in space can, so every curve here lifts its
 *     outside edge;
 *   - so the camera can be anywhere: behind the car, above it, in it --
 *     TinyOutRun's camera can only be where its trick works;
 *   - hills hide what's behind them because of the z-buffer, not because
 *     of a special case (TinyOutRun's [visible]);
 *   - and the world is solid: land that rises into hills around the
 *     road, a bridge you drive under the cables of, a road that crosses
 *     over itself, a tunnel through a hill.
 * The coast is still TinyOutRun's own Road.t, walked into space by
 * Track3d.of_road; the three circuits are points on their maps, walked
 * into a closed ribbon by Track3d.build.
 *
 * The road is static: it's cut in chunks of 40 segments, each a cached3d
 * group (kept in GPU buffers on the GPU backends), and only the chunks
 * around the car are drawn; the land is cut in tiles the same way, and
 * the landmarks are one more cached group.
 *
 * Uses: the racing kit's Track3d (gamekits/racing/3d/), Topdown (the
 * player's car) and Road (the coast), the heightmap kit's generator
 * (the land's rolling), Camera3d (the views), Scene2d (title, course
 * select, race, goal), Audio. Not physics: an arcade car is a few rules
 * (Topdown.mli), and the rivals fewer (see [drive_rivals]).
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* Shapes made of polygons *)
(*****************************************************************************)

(* The polygons here must be wound the right way: counterclockwise as
 * seen from outside. Flat shading lights a face from its normal, and a
 * face wound backwards is lit from inside: black in the sun. *)

(* [loft color a b z0 z1]: a solid from the section [a] at [z0] to the
 * section [b] at [z1] (z1 > z0), both lists of (x, y), the same length,
 * counterclockwise seen from +z: a car's nose, a roof, a wheel. *)
let loft (color : color) (a : (number * number) list) (b : (number * number) list) (z0 : number) (z1 : number) : shape3d =
  let a = Array.of_list a and b = Array.of_list b in
  let n = Array.length a in
  let at ((x, y) : number * number) (z : number) = (x, y, z) in
  let side i =
    let j = (i + 1) mod n in
    polygon3d color [ at a.(i) z0; at a.(j) z0; at b.(j) z1; at b.(i) z1 ]
  in
  group3d
    (polygon3d color (List.map (fun p -> at p z1) (Array.to_list b))
    :: polygon3d color (List.rev_map (fun p -> at p z0) (Array.to_list a))
    :: List.init n side)

(* a regular polygon of [sides], counterclockwise *)
let ring (sides : int) (r : number) : (number * number) list =
  List.init sides (fun k ->
      let a = 2. *. Float.pi *. float_of_int k /. float_of_int sides in
      (r *. cos a, r *. sin a))

(* a wheel: [sides] around, [w] wide, its axle along x *)
let wheel (color : color) (sides : int) (r : number) (w : number) : shape3d =
  loft color (ring sides r) (ring sides r) (-.w /. 2.) (w /. 2.) |> rotate3d 0. 90. 0.

(* a pseudo-random number in [0, 1) for the k-th thing at i: the
 * scenery is the same at every run *)
let jitter (i : int) (k : int) : number = float_of_int (Hashtbl.hash (i, k) mod 1000) /. 1000.

(* [cone ~seed ~snow color sides r h]: a peak on the ground, its [sides]
 * facets each one flat color in the sun; the top [snow] of its height
 * (0: none) white. The base points go round counterclockwise seen from
 * above, so [b_k; apex; b_k+1] faces out. *)
let cone ?(seed = 0) ?(snow = 0.) (color : color) (sides : int) (r : number) (h : number) : shape3d =
  let base =
    Array.init sides (fun k ->
        let a = 2. *. Float.pi *. (float_of_int k +. (0.4 *. jitter seed k)) /. float_of_int sides in
        let r = r *. (0.75 +. (0.4 *. jitter seed (k + 50))) in
        (r *. cos a, 0., r *. sin a))
  in
  let apex = ((jitter seed 99 -. 0.5) *. r *. 0.3, h, (jitter seed 98 -. 0.5) *. r *. 0.3) in
  let mix (x1, y1, z1) (x2, y2, z2) f = (x1 +. ((x2 -. x1) *. f), y1 +. ((y2 -. y1) *. f), z1 +. ((z2 -. z1) *. f)) in
  group3d
    (List.concat
       (List.init sides (fun k ->
            let b0 = base.(k) and b1 = base.((k + 1) mod sides) in
            if snow = 0. then [ polygon3d color [ b0; apex; b1 ] ]
            else
              let m0 = mix b0 apex (1. -. snow) and m1 = mix b1 apex (1. -. snow) in
              [ polygon3d color [ b0; m0; m1; b1 ]; polygon3d (rgb 245 245 250) [ m0; apex; m1 ] ])))

(* [column color sides r h]: a standing prism, with its top *)
let column (color : color) (sides : int) (r : number) (h : number) : shape3d =
  let base = List.map (fun (x, z) -> (x, 0., z)) (ring sides r) |> Array.of_list in
  group3d
    (polygon3d color (List.rev_map (fun (x, _, z) -> (x, h, z)) (Array.to_list base))
    :: List.init sides (fun k ->
           let x0, _, z0 = base.(k) and x1, _, z1 = base.((k + 1) mod sides) in
           polygon3d color [ (x0, 0., z0); (x0, h, z0); (x1, h, z1); (x1, 0., z1) ]))

(* [mesa color sides r top h]: a hill with a flat top of radius [top] *)
let mesa (color : color) (sides : int) (r : number) (top : number) (h : number) : shape3d =
  let at k rr y =
    let a = 2. *. Float.pi *. float_of_int k /. float_of_int sides in
    (rr *. cos a, y, rr *. sin a)
  in
  group3d
    (polygon3d color (List.init sides (fun k -> at (sides - k) top h))
    :: List.init sides (fun k -> polygon3d color [ at k r 0.; at k top h; at (k + 1) top h; at (k + 1) r 0. ]))

(* a flat shape on the ground at height [y], through (x, z) points
 * given counterclockwise on a map seen from above, -z up (the way a
 * heading of 0 points): its face up *)
let flat (color : color) (y : number) (points : (number * number) list) : shape3d =
  polygon3d color (List.map (fun (x, z) -> (x, y, z)) points)

(* a roof over a house, the triangle of its gable lofted along its depth *)
let roof (color : color) (w : number) (h : number) (d : number) : shape3d =
  loft color [ (-.w /. 2., 0.); (w /. 2., 0.); (0., h) ] [ (-.w /. 2., 0.); (w /. 2., 0.); (0., h) ] (-.d /. 2.) (d /. 2.)

(*****************************************************************************)
(* The courses *)
(*****************************************************************************)

let road_width = 6. (* half of it *)

(* The land next to the road is this far below it, the road standing on
 * the sides of its embankment ([segment_shapes]): an eye above a road
 * only 0.1 over the land sees the land win the depth test near the
 * bottom of the screen, where a quad of land straddles the eye *)
let ground = -1.5
let verge = 2.5 *. road_width (* the grass along it, up to the rail *)
let water_level = ground +. 0.2
let rumble_length = 3
let chunk_size = 40

(* The courses are written on the squares of their maps -- the course
 * select's maps of the Mega Drive version's manual, traced -- one
 * square 40 meters, x to the right and y down the map (+z: a heading
 * of 0 is up the map). *)
let map_unit = 40.

(* degrees from [a] to [b], the short way round *)
let angle_diff (a : number) (b : number) : number = Float.rem (Float.rem (b -. a +. 180.) 360. +. 360.) 360. -. 180.

let smoothstep (a : number) (b : number) (x : number) : number =
  let t = Float.max 0. (Float.min 1. ((x -. a) /. (b -. a))) in
  t *. t *. (3. -. (2. *. t))

(* A circuit through points (x, y on the map, height in meters), each
 * corner leaning into itself: the bank is read from how much the
 * course turns at each point, so a course is written as where the road
 * goes and nothing else. *)
let circuit (points : (number * number * number) list) : Track3d.t =
  let a = Array.of_list (List.map (fun (x, y, h) -> (x *. map_unit, y *. map_unit, h)) points) in
  let n = Array.length a in
  let heading i j =
    let x1, z1, _ = a.(i mod n) and x2, z2, _ = a.(j mod n) in
    atan2 (x2 -. x1) (-.(z2 -. z1)) *. 180. /. Float.pi
  in
  Track3d.build ~step:2.
    (List.init n (fun i ->
         let turn = angle_diff (heading (i + n - 1) i) (heading i (i + 1)) in
         let x, z, y = a.(i) in
         Track3d.control ~y ~width:road_width ~bank:(Float.max (-10.) (Float.min 10. (-.turn *. 0.2))) x z))

(* how much the road turns at segment [i], in degrees per segment,
 * averaged over a few (the resampled spline wobbles by a hair from one
 * sample to the next) *)
let turn_at (ribbon : Track3d.t) (i : int) : number =
  let at k = Track3d.at ribbon (float_of_int k *. Track3d.step ribbon) in
  angle_diff (at (i - 3)).heading (at (i + 4)).heading /. 7.

(* a point in a polygon (crossing number): the sea *)
let inside (poly : (number * number) list) ((x, z) : number * number) : bool =
  let a = Array.of_list poly in
  let n = Array.length a in
  let c = ref false in
  for i = 0 to n - 1 do
    let x1, z1 = a.(i) and x2, z2 = a.((i + 1) mod n) in
    if z1 > z <> (z2 > z) && x < x1 +. ((z -. z1) /. (z2 -. z1) *. (x2 -. x1)) then c := not !c
  done;
  !c

(* Stretches of road that are more than a road: a suspension bridge, an
 * overpass (the road crossing over itself), a tunnel, and the grey
 * concrete walls that stand instead of the rails in places. *)
type span_kind = Suspension | Overpass | Tunnel | Wall

type span = { kind : span_kind; s0 : number; s1 : number }

let in_span (spans : span list) (kinds : span_kind list) (s : number) : span option =
  List.find_opt (fun sp -> List.mem sp.kind kinds && s >= sp.s0 && s <= sp.s1) spans

(* the road off the land: the land under it is left as it is *)
let lifted = [ Suspension; Overpass; Tunnel ]

(*---------------------------------------------------------------------------*)
(* The land *)
(*---------------------------------------------------------------------------*)

(* The land is a grid of heights, drawn as two triangles a cell, each
 * one flat color picked from the course's palette by its height and a
 * hash, then lit by the sun by its slope: hundreds of facets, no two
 * alike, which is what Virtua Racing's hills looked like (and
 * TinyZeldaOcarina's field, the same way).
 *
 * A height is the course's own hills and mountains (smooth bumps, some
 * with a flat top), the rolling of the kit's fractal generator
 * (gamekits/heightmap/, diamond-square) on top, the sea sunk -- and,
 * near the road, the road's own height, the land blending from it to
 * its own over 50 meters: the road runs along embankments and through
 * cuttings, never over a hole or under a hill (but where it is meant
 * to: a bridge, a tunnel).
 *
 *        cutting                              embankment
 *   \                  road          _____________________
 *    \___ ___________==========_____/       road            \
 *        (the band: the land takes the road's height, then its own)
 *)

type hill = { hx : number; hz : number; radius : number; height : number; top : number }

(* a hill at (x, y) on the map, [radius] and [top] (the radius of a flat
 * top, a plateau) in squares, [height] in meters *)
let hill ?(top = 0.) (x : number) (y : number) (radius : number) (height : number) : hill =
  { hx = x *. map_unit; hz = y *. map_unit; radius = radius *. map_unit; height; top = top *. map_unit }

(* the land's colors: the lowlands', the higher slopes', the rock's; and
 * snow on the tops or not *)
type palette = { low : color list; mid : color list; rock : color list; snow : bool }

let green_palette =
  { low = [ rgb 78 150 62; rgb 70 140 58; rgb 88 160 68; rgb 96 152 60; rgb 66 132 60 ];
    mid = [ rgb 50 115 52; rgb 58 124 50; rgb 44 104 50; rgb 70 120 56 ];
    rock = [ rgb 130 124 112; rgb 118 112 104; rgb 142 134 118; rgb 110 106 100 ];
    snow = true }

let dry_palette =
  { low = [ rgb 160 150 88; rgb 150 140 80; rgb 170 158 96; rgb 144 138 86; rgb 164 146 84 ];
    mid = [ rgb 150 126 80; rgb 138 118 76; rgb 160 134 86; rgb 128 120 80 ];
    rock = [ rgb 176 150 116; rgb 160 138 108; rgb 186 162 124; rgb 150 130 104 ];
    snow = false }

type terrain = {
  x0 : number;
  z0 : number;
  cell : number;
  nx : int;
  nz : int;
  heights : number array;
  wet : bool array; (* under the sea *)
  palette : palette;
}

let rolling = Heightmap.generate ~seed:7 ~size:32 ~top:1. ~roughness:0.55

let natural (hills : hill list) (x : number) (z : number) : number =
  let bump h =
    let d = Float.hypot (x -. h.hx) (z -. h.hz) in
    if d <= h.top then h.height
    else if d >= h.radius then 0.
    else h.height *. (0.5 +. (0.5 *. cos (Float.pi *. (d -. h.top) /. (h.radius -. h.top))))
  in
  let hills = List.fold_left (fun m h -> Float.max m (bump h)) 0. hills in
  let rx = Float.rem (Float.abs (x /. 50.)) 31. and rz = Float.rem (Float.abs (z /. 50.)) 31. in
  hills +. (14. *. Float.max 0. (Heightmap.height rolling rx rz -. 0.2))

(* [make_land ...]: the grid over the course and a margin around it.
 * The road's samples (every 4 meters: where, how high, lifted off the
 * land or not) are searched for the nearest one to each point of the
 * grid. *)
let make_land (ribbon : Track3d.t) (spans : span list) (hills : hill list) (sea : (number * number) list list) (palette : palette) :
    terrain =
  let step = Track3d.step ribbon in
  let samples =
    Array.init
      (Track3d.segments ribbon / 2)
      (fun k ->
        let s = float_of_int (2 * k) *. step in
        let p = Track3d.at ribbon s in
        (p.px, p.pz, p.py, in_span spans lifted s <> None))
  in
  let x0, x1, z0, z1 =
    Array.fold_left
      (fun (a, b, c, d) (x, z, _, _) -> (Float.min a x, Float.max b x, Float.min c z, Float.max d z))
      (Float.infinity, Float.neg_infinity, Float.infinity, Float.neg_infinity)
      samples
  in
  let margin = 600. and cell = 32. in
  let x0 = x0 -. margin and z0 = z0 -. margin in
  let nx = int_of_float ((x1 -. x0 +. margin) /. cell) + 1 and nz = int_of_float ((z1 -. z0 +. margin) /. cell) + 1 in
  let point k = (x0 +. (float_of_int (k mod (nx + 1)) *. cell), z0 +. (float_of_int (k / (nx + 1)) *. cell)) in
  let wet = Array.init ((nx + 1) * (nz + 1)) (fun k -> List.exists (fun w -> inside w (point k)) sea) in
  let height k =
    let x, z = point k in
    let best = ref Float.infinity and road_y = ref 0. and off = ref false in
    Array.iter
      (fun (px, pz, py, b) ->
        let d = Float.hypot (x -. px) (z -. pz) in
        if d < !best then (
          best := d;
          road_y := py;
          off := b))
      samples;
    let own = if wet.(k) then -8. else natural hills x z in
    if !off then own
    else
      let t = smoothstep (verge +. 10.) (verge +. 60.) !best in
      let level = !road_y +. ground in
      level +. ((own -. level) *. t)
  in
  let heights =
    Array.init ((nx + 1) * (nz + 1)) (fun k ->
        (* a hair of noise, so that no two facets of a slope are lit alike *)
        height k +. (1.2 *. (jitter (k mod (nx + 1)) ((k / (nx + 1)) + 7) -. 0.5)))
  in
  { x0; z0; cell; nx; nz; heights; wet; palette }

let clamp_index (l : terrain) (i : int) (j : int) : int = (max 0 (min l.nz j) * (l.nx + 1)) + max 0 (min l.nx i)
let land_at (l : terrain) (i : int) (j : int) : number = l.heights.(clamp_index l i j)

(* the height of the land at (x, z), the four corners of its cell mixed *)
let land_height (l : terrain) (x : number) (z : number) : number =
  let fx = (x -. l.x0) /. l.cell and fz = (z -. l.z0) /. l.cell in
  let i = int_of_float (Float.floor fx) and j = int_of_float (Float.floor fz) in
  let u = fx -. Float.floor fx and v = fz -. Float.floor fz in
  let mix a b f = a +. ((b -. a) *. f) in
  mix (mix (land_at l i j) (land_at l (i + 1) j) u) (mix (land_at l i (j + 1)) (land_at l (i + 1) (j + 1)) u) v

let facet_color (l : terrain) ~(wet : bool) (h : number) (k : int) : color =
  let pick list = List.nth list (k mod List.length list) in
  if wet && h < water_level +. 1.5 then rgb 200 190 140 (* the beach, and the sand under the sea *)
  else if h < 22. then pick l.palette.low
  else if h < 55. then pick l.palette.mid
  else if h < 95. || not l.palette.snow then pick l.palette.rock
  else if k mod 3 = 0 then rgb 225 228 235
  else rgb 244 244 248

(* [land_tiles l stride]: the land in tiles of 8 x 8 facet pairs, each a
 * cached group, [stride] cells a facet (2 for the course select's model,
 * coarser); the sea over the wet cells, a quad each, flat (its shore is
 * where the land's facets come out of it); each tile with its middle,
 * to leave out the far ones *)
let land_tiles (l : terrain) (stride : int) : ((number * number) * shape3d) list =
  let p i j = (l.x0 +. (float_of_int i *. l.cell), land_at l i j, l.z0 +. (float_of_int j *. l.cell)) in
  let quad i j =
    let i' = i + stride and j' = j + stride in
    let wet = List.exists (fun (i, j) -> l.wet.(clamp_index l i j)) [ (i, j); (i', j); (i, j'); (i', j') ] in
    let color k (_, y1, _) (_, y2, _) (_, y3, _) = facet_color l ~wet ((y1 +. y2 +. y3) /. 3.) (Hashtbl.hash (i, j, k)) in
    let a = p i j and b = p i' j' and c = p i' j and d = p i j' in
    let at (x, _, z) = (x, water_level, z) in
    (* counterclockwise seen from above: faces up *)
    [ polygon3d (color 0 a b c) [ a; b; c ]; polygon3d (color 1 a d b) [ a; d; b ] ]
    @ if wet then [ polygon3d (rgb 50 110 190) [ at a; at d; at b; at c ] ] else []
  in
  let tile = 8 * stride in
  List.concat
    (List.init ((l.nz / tile) + 1) (fun tj ->
         List.filter_map
           (fun ti ->
             let quads =
               List.concat
                 (List.init 8 (fun a ->
                      List.concat
                        (List.init 8 (fun b ->
                             let i = (ti * tile) + (b * stride) and j = (tj * tile) + (a * stride) in
                             if i + stride <= l.nx && j + stride <= l.nz then quad i j else []))))
             in
             if quads = [] then None
             else
               let cx = l.x0 +. ((float_of_int (ti * tile) +. (float_of_int tile /. 2.)) *. l.cell)
               and cz = l.z0 +. ((float_of_int (tj * tile) +. (float_of_int tile /. 2.)) *. l.cell) in
               Some ((cx, cz), cached3d quads))
           (List.init ((l.nx / tile) + 1) Fun.id)))

(*---------------------------------------------------------------------------*)
(* A course *)
(*---------------------------------------------------------------------------*)

(* what the scenery of a course is placed from: the road, its spans,
 * the land's height, and where nothing stands (the sea) *)
type site = {
  ribbon : Track3d.t;
  spans : span list;
  height : number -> number -> number;
  taken : number * number -> bool;
}

type course = {
  name : string;
  level : string; (* BEGINNER, MEDIUM, EXPERT *)
  ribbon : Track3d.t;
  laps : int; (* 1: a stage, to the GOAL arch *)
  lap_length : number;
  checkpoints : int; (* a lap's, the line one of them, evenly spaced *)
  start_time : int; (* seconds on the clock at the start *)
  bonus : int; (* the seconds each checkpoint adds *)
  rivals : int;
  spans : span list;
  terrain : terrain;
  grass : color * color; (* the verges, light and dark *)
  scenery : int -> shape3d list; (* along segment i *)
  landscape : shape3d list; (* the landmarks *)
  animated : time -> shape3d list; (* what moves: the Ferris wheel, the windmills, the boats *)
}

let segment_of (c : course) (i : int) : number = float_of_int i *. Track3d.step c.ribbon

let place (ribbon : Track3d.t) (s : number) (offset : number) (shape : shape3d) : shape3d =
  let x, y, z = Track3d.across ribbon s offset in
  let p = Track3d.at ribbon s in
  shape |> rotate3d 0. (-.p.heading) 0. |> move3d x y z

(* [on_land site x y shape]: standing on the land at (x, y) on the map,
 * a little sunk (the land between the corners of a cell is not quite
 * flat) *)
let on_land (site : site) (x : number) (y : number) (shape : shape3d) : shape3d =
  let x = x *. map_unit and z = y *. map_unit in
  shape |> move3d x (site.height x z -. 0.4) z

(*---------------------------------------------------------------------------*)
(* The scenery *)
(*---------------------------------------------------------------------------*)

let tree : shape3d =
  group3d [ box (rgb 110 70 30) 0.5 2.4 0.5 |> move_y3d 1.2; box (rgb 40 120 40) 2.4 2. 2.4 |> rotate3d 0. 45. 0. |> move_y3d 3. ]

let pine (k : int) : shape3d =
  let green = if k mod 2 = 0 then rgb 30 100 45 else rgb 40 115 50 in
  group3d
    [ box (rgb 100 65 30) 0.5 1.6 0.5 |> move_y3d 0.8; cone ~seed:k green 6 2.2 4. |> move_y3d 1.4;
      cone ~seed:(k + 7) green 5 1.6 3.4 |> move_y3d 3.6 ]

(* a palm: its trunk leaning, six fronds drooping from the top *)
let palm (k : int) : shape3d =
  let lean = 8. +. (10. *. jitter k 3) in
  let frond a =
    let r = a *. Float.pi /. 180. in
    let c = cos r and s = sin r in
    polygon3d (rgb 60 150 60) [ (0., 7., 0.); (3.2 *. c -. (0.6 *. s), 6.2, 3.2 *. s +. (0.6 *. c)); (4. *. c, 5.2, 4. *. s) ]
  in
  group3d (column (rgb 140 100 60) 5 0.3 7. :: List.init 6 (fun i -> frond ((60. *. float_of_int i) +. (30. *. jitter k i))))
  |> rotate3d lean (360. *. jitter k 4) 0.

let cypress (k : int) : shape3d = group3d [ cone ~seed:k (rgb 55 115 55) 5 1.1 7. ]
let rock (k : int) : shape3d = cone ~seed:k (rgb 150 130 105) 5 2.2 1.8

let sign : shape3d =
  group3d
    [ box (rgb 90 90 90) 0.3 2. 0.3 |> move3d (-1.5) 1. 0.; box (rgb 90 90 90) 0.3 2. 0.3 |> move3d 1.5 1. 0.;
      box white 4. 1.4 0.3 |> move_y3d 2.6; box red 3.4 0.5 0.35 |> move_y3d 2.6 ]

(* a chevron board on the outside of a sharp curve *)
let chevron : shape3d =
  group3d [ box (rgb 60 60 60) 0.2 1.2 0.2 |> move_y3d 0.6; box (rgb 250 210 0) 1.6 1. 0.2 |> move_y3d 1.6; box black 0.5 1.02 0.22 |> move_y3d 1.6 ]

(* [trees kind every rows site i]: a tree each side of segment [i],
 * beyond the rail, every [every] segments: on the land, clear of the
 * road (of *another* part of it too, since a circuit comes back near
 * itself), not in the sea, not on the rock *)
let trees (kind : int -> shape3d) (every : int) (rows : int) (site : site) (i : int) : shape3d list =
  if i mod every <> 0 then []
  else
    let s = float_of_int i *. Track3d.step site.ribbon in
    let _, road_y, _ = Track3d.across site.ribbon s 0. in
    List.concat_map
      (fun row ->
        List.filter_map
          (fun side ->
            let off = side *. (verge +. 4. +. (float_of_int row *. 9.) +. (5. *. jitter i (row + int_of_float side))) in
            let x, _, z = Track3d.across site.ribbon s off in
            let _, nearest = Track3d.locate site.ribbon x z in
            if Float.abs nearest >= verge +. 2. && (not (site.taken (x, z))) && site.height x z < road_y +. 30. then
              Some (kind (i + row) |> move3d x (site.height x z -. 0.4) z)
            else None)
          [ -1.; 1. ])
      (List.init rows Fun.id)

(* signs every 150 segments, chevrons outside the sharp curves *)
let signs (site : site) (i : int) : shape3d list =
  let s = float_of_int i *. Track3d.step site.ribbon in
  let turn = turn_at site.ribbon i in
  if in_span site.spans [ Suspension; Overpass; Tunnel ] s <> None then []
  else if i mod 150 = 75 then [ place site.ribbon s (1.3 *. road_width) sign ]
  else if Float.abs turn > 1.8 && i mod 5 = 0 then [ place site.ribbon s ((if turn > 0. then -1.4 else 1.4) *. road_width) chevron ]
  else []

(*---------------------------------------------------------------------------*)
(* The landmarks *)
(*---------------------------------------------------------------------------*)

(* The grandstand at the start, on the left of the line: five steps of
 * spectators (a stripe of colors each) under a roof. Built along z
 * (the road's way) with the road on its +x side. *)
let grandstand : shape3d =
  let len = 36. in
  let steps =
    List.concat
      (List.init 5 (fun k ->
           let y = 1. +. (1.2 *. float_of_int k) and x = -2. -. (2.2 *. float_of_int k) in
           [ box (rgb 170 170 170) 2.2 (y -. ground) len |> move3d x ((y +. ground) /. 2.) 0.;
             box (List.nth [ rgb 220 60 60; rgb 240 200 40; rgb 60 110 220; rgb 240 240 240; rgb 60 170 90 ] k) 1.2 0.5 (len -. 1.)
             |> move3d (x +. 0.4) (y +. 0.25) 0. ]))
  in
  group3d
    (steps
    @ [ box (rgb 120 120 130) 0.4 8. 0.4 |> move3d (-12.) 4. (-.len /. 2.); box (rgb 120 120 130) 0.4 8. 0.4 |> move3d (-12.) 4. (len /. 2.);
        box (rgb 40 60 140) 13. 0.4 (len +. 2.) |> rotate3d 0. 0. (-8.) |> move3d (-6.) 8.4 0. ])

(* a gantry over the road: the start's (blue), a checkpoint's (yellow) *)
let gantry (color : color) : shape3d =
  let w = road_width in
  group3d
    [ box (rgb 230 230 230) 1. 7. 1. |> move3d (-.w -. 1.) 3.5 0.; box (rgb 230 230 230) 1. 7. 1. |> move3d (w +. 1.) 3.5 0.;
      box color ((2. *. w) +. 3.) 1.6 0.6 |> move_y3d 7.; box (rgb 250 60 40) 1. 0.6 0.7 |> move3d (-2.) 7. 0.;
      box (rgb 250 60 40) 1. 0.6 0.7 |> move3d 2. 7. 0. ]

(* the start, on a circuit: the line chequered, the gantry over it,
 * the grandstand on the left, over the rail, and the checkpoints'
 * gantries round the lap *)
let start (ribbon : Track3d.t) (checkpoints : int) : shape3d list =
  let w = road_width in
  let chequer =
    List.concat
      (List.init 2 (fun row ->
           List.init 8 (fun k ->
               let a = -.w +. (float_of_int k *. w /. 4.) in
               Track3d.strip ribbon (if (k + row) mod 2 = 0 then white else black) row a (a +. (w /. 4.)))))
  in
  chequer
  @ [ place ribbon 0. 0. (gantry (rgb 30 60 200)); place ribbon 30. (-.verge -. 1.) grandstand ]
  @ List.init (checkpoints - 1) (fun k ->
        place ribbon (float_of_int (k + 1) *. Track3d.length ribbon /. float_of_int checkpoints) 0. (gantry (rgb 240 200 30)))

(* The Ferris wheel, turning: its rim, spokes and cars are polygons in
 * the wheel's plane (x, y), its cars hanging level whatever the angle,
 * on two A-frames. *)
let ferris_wheel (angle : number) : shape3d =
  let r = 20. and hub = 23. and n = 16 in
  let point k rr =
    let a = (angle +. (360. *. float_of_int k /. float_of_int n)) *. Float.pi /. 180. in
    (rr *. cos a, hub +. (rr *. sin a))
  in
  let quad color (x1, y1) (x2, y2) (x3, y3) (x4, y4) z = polygon3d color [ (x1, y1, z); (x2, y2, z); (x3, y3, z); (x4, y4, z) ] in
  let rim =
    List.concat
      (List.init n (fun k ->
           List.map
             (fun z -> quad (rgb 240 240 240) (point k r) (point (k + 1) r) (point (k + 1) (r -. 0.8)) (point k (r -. 0.8)) z)
             [ -1.2; 1.2 ]))
  in
  let spokes =
    List.init n (fun k ->
        let x, y = point k r and a = (angle +. (360. *. float_of_int k /. float_of_int n)) *. Float.pi /. 180. in
        let dx = -0.2 *. sin a and dy = 0.2 *. cos a in
        quad (rgb 200 200 210) (-.dx, hub -. dy) (dx, hub +. dy) (x +. dx, y +. dy) (x -. dx, y -. dy) 0.)
  in
  let cars =
    List.init n (fun k ->
        let x, y = point k r in
        box (List.nth rainbow (k mod List.length rainbow)) 1.8 1.6 1.6 |> move3d x (y -. 1.4) 0.)
  in
  let leg x z = polygon3d (rgb 150 150 160) [ (x -. 0.6, 0., z); (x +. 0.6, 0., z); (0.3, hub, z); (-0.3, hub, z) ] in
  group3d
    (rim @ spokes @ cars
    @ [ leg (-9.) (-2.); leg 9. (-2.); leg (-9.) 2.; leg 9. 2.; box (rgb 90 90 100) 1.2 1.2 5. |> move_y3d hub ])

(* The roller coaster of the amusement park: a track on stilts, a loop
 * the loop in the middle (a ring of short boxes), the cars resting at
 * the station. *)
let roller_coaster : shape3d =
  let rail = rgb 240 220 40 and stilt = rgb 200 200 200 in
  let loop =
    List.init 20 (fun k ->
        let a = 18. *. float_of_int k in
        let r = a *. Float.pi /. 180. in
        box rail 1.2 0.4 2. |> rotate3d a 0. 0. |> move3d 0. (9. +. (8. *. cos r)) (8. *. sin r))
  in
  let run =
    List.init 12 (fun k ->
        let z = -30. +. (5. *. float_of_int k) and y = 4. +. (3. *. sin (float_of_int k *. 0.9)) in
        [ box rail 1.2 0.4 5. |> move3d 0. y z; box stilt 0.3 y 0.3 |> move3d 0. (y /. 2.) z ])
  in
  group3d (loop @ List.concat run @ List.init 3 (fun k -> box (rgb 220 40 40) 1.4 1. 1.8 |> move3d 0. 4.9 (-20. +. (2. *. float_of_int k))))

(* The suspension bridge, along the ribbon from [s0] to [s1]: two
 * towers standing in the sea, the two main cables hanging between
 * them (a parabola, the shape a cable takes under an evenly spread
 * deck) and running down to the ends, a hanger every few meters from
 * cable to deck, piers under the deck. The deck itself is in the road's
 * chunks ([segment_shapes]). Its hundreds of polygons are what made
 * Model 1 famous. *)
let bridge_shapes (ribbon : Track3d.t) (s0 : number) (s1 : number) : shape3d list =
  let w = road_width in
  let side_off = 1.55 *. w in
  let len = s1 -. s0 in
  let t1 = s0 +. (0.22 *. len) and t2 = s1 -. (0.22 *. len) in
  let top = 26. and bed = -8. in
  let deck s side =
    let _, y, _ = Track3d.across ribbon s (side *. side_off) in
    y
  in
  let cable s side =
    let d = deck s side in
    if s < t1 then d +. 1.5 +. ((top -. 1.5) *. (s -. s0) /. (t1 -. s0))
    else if s > t2 then d +. 1.5 +. ((top -. 1.5) *. (s1 -. s) /. (s1 -. t2))
    else
      let u = (2. *. (s -. t1) /. (t2 -. t1)) -. 1. in
      d +. 2. +. ((top -. 2.) *. u *. u)
  in
  let red = rgb 235 85 55 and steel = rgb 190 60 40 in
  let tower s =
    let _, y, _ = Track3d.across ribbon s 0. in
    let h = y -. bed +. top +. 1. in
    let leg side = box red 1.6 h 1.6 |> move3d (side *. side_off) (h /. 2.) 0. in
    let beam y = box red ((2. *. side_off) +. 1.6) 1.2 1.2 |> move_y3d y in
    let at = Track3d.at ribbon s in
    group3d [ leg (-1.); leg 1.; beam (h -. 0.6); beam (y -. bed +. (top *. 0.55)); beam (y -. bed -. 1.8) ]
    |> rotate3d 0. (-.at.heading) 0.
    |> move3d at.px bed at.pz
  in
  let ds = 2. in
  let pieces = int_of_float (len /. ds) in
  let cables =
    List.concat_map
      (fun side ->
        List.init pieces (fun k ->
            let sa = s0 +. (float_of_int k *. ds) and sb = s0 +. (float_of_int (k + 1) *. ds) in
            let xa, _, za = Track3d.across ribbon sa (side *. side_off) and xb, _, zb = Track3d.across ribbon sb (side *. side_off) in
            let ya = cable sa side and yb = cable sb side in
            polygon3d steel [ (xa, ya, za); (xb, yb, zb); (xb, yb +. 0.5, zb); (xa, ya +. 0.5, za) ]))
      [ -1.; 1. ]
  in
  let hangers =
    List.concat_map
      (fun side ->
        List.init (pieces / 2) (fun k ->
            let s = s0 +. (float_of_int (2 * k) *. ds) in
            let x, y, z = Track3d.across ribbon s (side *. side_off) and fx, fz = Track3d.forward ribbon s in
            let yc = cable s side in
            polygon3d (rgb 220 220 220)
              [ (x, y, z); (x +. (fx *. 0.25), y, z +. (fz *. 0.25)); (x +. (fx *. 0.25), yc, z +. (fz *. 0.25)); (x, yc, z) ]))
      [ -1.; 1. ]
  in
  [ tower t1; tower t2 ] @ cables @ hangers

(* piers under a raised road, every 30 meters, down to [bed] *)
let piers (ribbon : Track3d.t) (bed : number) (s0 : number) (s1 : number) : shape3d list =
  List.init
    (int_of_float ((s1 -. s0) /. 30.) + 1)
    (fun k ->
      let s = s0 +. (float_of_int k *. 30.) in
      let at = Track3d.at ribbon s in
      let h = at.py -. 1.8 -. bed in
      box (rgb 170 170 165) (2.6 *. road_width) h 2.5
      |> move_y3d (bed +. (h /. 2.))
      |> rotate3d 0. (-.at.heading) 0.
      |> move3d at.px 0. at.pz)

(* the mouths of a tunnel: a concrete face around the road at each end *)
let portals (ribbon : Track3d.t) (s0 : number) (s1 : number) : shape3d list =
  let face = group3d [ box (rgb 150 150 145) (2. *. verge) 4. 1.5 |> move_y3d 8.; box (rgb 150 150 145) 4. 8. 1.5 |> move3d (-.verge +. 2.) 4. 0.;
                       box (rgb 150 150 145) 4. 8. 1.5 |> move3d (verge -. 2.) 4. 0. ] in
  [ place ribbon s0 0. face; place ribbon s1 0. face ]

(* a windmill (Bay Bridge's pair by the big right): a white tower, its
 * four sails turning *)
let windmill (angle : number) : shape3d =
  let sail k =
    let a = angle +. (90. *. float_of_int k) in
    box (rgb 245 245 240) 1.4 9. 0.3 |> move_y3d 4.8 |> rotate3d 0. 0. a
  in
  group3d
    ([ column (rgb 240 240 235) 8 3. 14.; cone (rgb 170 60 50) 8 3.4 3. |> move_y3d 14. ]
    @ List.map (fun sh -> sh |> move3d 0. 13. 3.4) (List.init 4 sail))

(* a sailing boat, for Acropolis' sea *)
let sailboat : shape3d =
  group3d
    [ loft white [ (-1.2, 0.); (1.2, 0.); (1.6, 1.2); (-1.6, 1.2) ] [ (-0.2, 0.3); (0.2, 0.3); (0.4, 1.2); (-0.4, 1.2) ] (-4.) 4.;
      polygon3d white [ (0., 1.2, 2.); (0., 10., 0.5); (0., 1.2, -2.5) ] ]

let lighthouse : shape3d =
  group3d
    ([ column white 8 2.4 16.; column (rgb 60 60 60) 8 1.8 3. |> move_y3d 16.; cone (rgb 200 40 40) 8 2.4 2. |> move_y3d 19. ]
    @ List.init 3 (fun k -> column (rgb 210 40 40) 8 2.45 1.6 |> move_y3d (2. +. (5. *. float_of_int k))))

(* The ruins of Acropolis: three steps, a peristyle of columns (some
 * fallen short), the beam over part of it, the pediment's triangle,
 * built along x. *)
let temple : shape3d =
  let marble = rgb 235 230 210 in
  let steps = List.init 3 (fun k -> box marble (20. -. float_of_int k) 0.6 (12. -. float_of_int k) |> move_y3d (0.3 +. (0.6 *. float_of_int k))) in
  let columns =
    List.concat
      (List.init 7 (fun k ->
           let x = -8. +. (float_of_int k *. 8. /. 3.) in
           let h = if k = 5 then 2.5 else 6. in
           [ column marble 8 0.6 h |> move3d x 1.8 (-4.); column marble 8 0.6 (if k = 1 then 3.5 else 6.) |> move3d x 1.8 4. ]))
  in
  group3d (steps @ columns @ [ box marble 11. 1. 10. |> move3d (-3.3) 8.3 0.; roof marble 10.5 2.2 11. |> rotate3d 0. 90. 0. |> move3d (-3.3) 8.8 0. ])

let house (k : int) : shape3d =
  let w = 5. +. (2. *. jitter k 1) and d = 5. +. (2. *. jitter k 2) and h = 3. +. (2. *. jitter k 3) in
  group3d [ box (rgb 240 235 225) w (h +. 2.) d |> move_y3d ((h /. 2.) -. 1.); roof (rgb 200 90 50) (w +. 0.6) 1.8 (d +. 0.4) |> move_y3d h ]

(*---------------------------------------------------------------------------*)
(* The four courses *)
(*---------------------------------------------------------------------------*)

(* [make ...]: a course from its road and what is round it; its spans
 * given as two points on the map each, where they start and end *)
let make ~name ~level ~ribbon ?(laps = 5) ?(checkpoints = 2) ~start_time ~bonus ?(rivals = 15) ?(spans = []) ?(palette = green_palette)
    ?(grass = (rgb 70 160 60, rgb 60 140 50)) ?(hills = []) ?(sea = []) ~scenery ~landscape ?(animated = fun _ _ -> []) () : course =
  let lap_length = if laps = 1 then Track3d.length ribbon -. (2. *. Track3d.step ribbon) else Track3d.length ribbon in
  let spans =
    List.map
      (fun (kind, (x0, y0), (x1, y1)) ->
        let s0, _ = Track3d.locate ribbon (x0 *. map_unit) (y0 *. map_unit) and s1, _ = Track3d.locate ribbon (x1 *. map_unit) (y1 *. map_unit) in
        { kind; s0; s1 })
      spans
  in
  let sea = List.map (List.map (fun (x, y) -> (x *. map_unit, y *. map_unit))) sea in
  let terrain = make_land ribbon spans hills sea palette in
  let site = { ribbon; spans; height = land_height terrain; taken = (fun p -> List.exists (fun w -> inside w p) sea) } in
  { name;
    level;
    ribbon;
    laps;
    lap_length;
    checkpoints;
    start_time;
    bonus;
    rivals;
    spans;
    terrain;
    grass;
    scenery = scenery site;
    landscape = landscape site;
    animated = animated site }

(* Big Forest, the beginner's: a long pit straight up the diagonal, the
 * right-handers round the top, back along the middle, the U hanging
 * at the bottom, and the hard right by the grey wall and the amusement
 * park -- Ferris wheel, roller coaster -- onto the straight again.
 * Nearly flat, in a forest, mountains all round. *)
let big_forest : course =
  let ribbon =
    circuit
      [ (6., 12.3, 0.); (10., 9.3, 0.); (14., 6.6, 1.); (17., 4.7, 2.); (19.5, 3., 3.); (21.2, 3.2, 4.); (24., 5., 5.); (25.8, 6.5, 5.);
        (25., 8.8, 4.); (23.5, 10., 3.); (20., 10.6, 2.); (17.8, 11.2, 2.); (16.5, 13., 3.); (16.2, 16., 4.); (15., 17.8, 3.);
        (12.5, 18., 2.); (11., 16.5, 2.); (10.8, 14.2, 1.); (9.8, 12.8, 1.); (8., 13., 0.); (6.2, 14.8, 0.); (4.2, 16.4, 0.);
        (2.7, 16.1, 0.); (2.4, 14.8, 0.); (3.6, 13.7, 0.) ]
  in
  make ~name:"BIG FOREST" ~level:"BEGINNER" ~ribbon ~start_time:45 ~bonus:24
    ~spans:[ (Wall, (4.8, 16.), (2.5, 14.3)) ]
    ~hills:
      [ hill 6. 4. 5. 40.; hill 20. 15. 2.5 18.; hill (-8.) 8. 9. 160.; hill 12. (-8.) 10. 180.; hill 32. (-3.) 9. 150.;
        hill 36. 13. 8. 120.; hill 22. 28. 9. 110.; hill 6. 27. 8. 100.; hill (-7.) 21. 7. 90. ]
    ~scenery:(fun site i -> trees pine 3 2 site i @ signs site i)
    ~landscape:(fun site -> start site.ribbon 2 @ [ roller_coaster |> rotate3d 0. 60. 0. |> on_land site 7. 18.2 ])
    ~animated:(fun site time -> [ ferris_wheel (spin 3. time) |> rotate3d 0. 30. 0. |> on_land site 3.2 18.8 ])
    ()

(* Bay Bridge, medium: the straight, a right and its right-left-right
 * kink, the suspension bridge across the inlet, a left that loops over
 * the road it will come back along (the overpass), the tunnel through
 * the hill, the big right by the windmills, the climb beside the grey
 * retaining wall, under the overpass, and the final long right at the
 * tail onto the straight. Narrow, walled, palms by the sea. *)
let bay_bridge : course =
  let ribbon =
    circuit
      [ (6., 11., 0.); (10., 8.9, 1.); (14., 6.8, 2.); (18., 4.6, 3.); (21., 2.8, 4.); (23.5, 2.2, 5.); (25.3, 3.4, 6.); (24.8, 5.3, 8.);
        (22.8, 6.8, 10.); (21.2, 8.5, 12.); (21., 10.4, 14.); (21.6, 12.2, 16.); (19.4, 13.7, 16.); (17.1, 15.3, 16.);
        (14.8, 16.9, 15.); (13.6, 18.5, 13.); (13.2, 20.3, 11.); (14., 21.6, 10.); (15.8, 22., 9.); (17.8, 21.4, 8.);
        (19.6, 20.2, 7.); (21.2, 19., 6.); (22.8, 18.4, 5.); (24.5, 19., 4.); (25.2, 20.9, 3.); (24., 22.7, 2.);
        (21.5, 23.6, 1.); (18.5, 24., 0.); (16., 23.6, 0.); (13.6, 21., 0.); (12.3, 19., 2.); (11.9, 16.8, 5.); (11.8, 14.5, 6.);
        (11., 12.6, 4.); (9., 12.2, 2.); (6.5, 13.3, 1.); (3.8, 14.2, 0.); (2.2, 13.4, 0.); (3., 12.1, 0.) ]
  in
  make ~name:"BAY BRIDGE" ~level:"MEDIUM" ~ribbon ~start_time:45 ~bonus:24
    ~spans:
      [ (Suspension, (21.2, 12.5), (15.3, 16.5)); (Overpass, (13.3, 19.5), (15.2, 22.)); (Tunnel, (20.2, 19.8), (23.6, 18.5));
        (Wall, (18.5, 24.), (13.9, 21.4)); (Wall, (24.8, 5.3), (21.1, 9.4)) ]
    ~sea:[ [ (16.2, 14.4); (19.4, 12.4); (23., 13.3); (29., 13.4); (40., 13.); (40., 17.5); (25., 16.8); (21.5, 16.8); (18.5, 17.6); (16.8, 17.4) ] ]
    ~hills:
      [ hill 22.2 18.8 3.5 30.; hill 31. 27. 8. 150.; hill 8. 29. 8. 130.; hill (-6.) 20. 7. 120.; hill (-5.) 3. 7. 110.;
        hill 14. (-6.) 8. 140.; hill 33. 2. 7. 120. ]
    ~scenery:(fun site i -> trees palm 7 1 site i @ signs site i)
    ~landscape:(fun site ->
      start site.ribbon 2
      @ List.concat_map
          (fun sp ->
            match sp.kind with
            | Suspension -> bridge_shapes site.ribbon sp.s0 sp.s1 @ piers site.ribbon (-8.) sp.s0 sp.s1
            | Overpass -> piers site.ribbon ground sp.s0 sp.s1
            | Tunnel -> portals site.ribbon sp.s0 sp.s1
            | Wall -> [])
          site.spans
      @ [ lighthouse |> on_land site 30. 12.4 ])
    ~animated:(fun site time -> [ windmill (spin 20. time) |> rotate3d 0. 200. 0. |> on_land site 27.2 20.4; windmill (spin 17. time +. 40.) |> rotate3d 0. 200. 0. |> on_land site 26.8 23.2 ])
    ()

(* Acropolis, the expert's: the blob of the map -- the long straight up
 * the left, round the top, the bends down the right with the sea
 * beyond, back along the bottom -- and the finger up its middle: a
 * long straight to the keyhole hairpin, a hard long left, and a long
 * straight back; the zigzag, the canyon's twisty bends to the line.
 * The ruins on their hill, houses on the mountainside. *)
let acropolis : course =
  let ribbon =
    circuit
      [ (1.6, 13., 0.); (2., 10.8, 2.); (3.3, 8.6, 5.); (4.6, 6.8, 8.); (7., 5.1, 11.); (10., 3.6, 14.); (13., 2.7, 16.); (15.5, 2.4, 17.);
        (17.3, 2.9, 17.); (18.2, 4.3, 16.); (19., 5.8, 15.); (20.6, 6.6, 14.); (22.2, 8.4, 12.); (24., 10., 9.); (25., 11.8, 7.);
        (24.3, 13.2, 6.); (21.5, 13.9, 5.); (19.5, 14.8, 4.); (17., 15.4, 3.); (15.2, 15.3, 3.); (14.3, 14.2, 3.); (14.4, 12., 5.);
        (14.7, 9.2, 8.); (15., 6.9, 10.); (15.7, 5.6, 11.); (15.1, 4.5, 12.); (13.9, 4.7, 12.); (13.4, 6.1, 11.); (13.2, 9., 8.);
        (12.9, 12., 5.); (12.6, 14.3, 3.); (11.7, 15.6, 2.); (10., 15.2, 2.); (8.3, 15.9, 1.); (6.8, 17.2, 1.); (5., 17.8, 0.);
        (3.2, 17.1, 0.); (2., 15.3, 0.) ]
  in
  make ~name:"ACROPOLIS" ~level:"EXPERT" ~ribbon ~start_time:45 ~bonus:25 ~palette:dry_palette
    ~grass:(rgb 165 160 90, rgb 150 145 80)
    ~spans:[ (Wall, (15.6, 5.8), (13.5, 5.8)) ]
    ~sea:[ [ (27., -10.); (45., -10.); (45., 30.); (27., 30.); (27.5, 12.) ] ]
    ~hills:
      [ hill ~top:0.6 8. 10. 3.2 34.; hill 9. 3.5 4. 30.; hill 4. 20.5 3. 45.; hill 8.5 19.5 2.5 38.; hill (-7.) 6. 8. 140.;
        hill 6. (-7.) 8. 130.; hill 20. (-6.) 8. 150.; hill 9. 28. 8. 110.; hill (-6.) 24. 7. 100. ]
    ~scenery:(fun site i -> trees cypress 7 1 site i @ (if i mod 11 = 5 then trees rock 1 1 site i else []) @ signs site i)
    ~landscape:(fun site ->
      start site.ribbon 2
      @ [ temple |> rotate3d 0. 20. 0. |> on_land site 8. 10. ]
      @ List.init 12 (fun k ->
            house k |> rotate3d 0. (40. *. jitter k 4) 0. |> on_land site (7.5 +. (1.1 *. float_of_int (k mod 4)) +. jitter k 5) (6.2 +. (0.9 *. float_of_int (k / 4)))))
    ~animated:(fun _ time ->
      List.init 3 (fun k ->
          sailboat |> rotate3d 0. (20. +. (70. *. float_of_int k)) 0.
          |> move3d (((30. +. (3. *. float_of_int k)) *. map_unit) +. wave (-20.) 20. (14. +. float_of_int k) time) water_level
               ((2. +. (8. *. float_of_int k)) *. map_unit)))
    ()

(* TinyOutRun's course, walked into space: the same Road.t, read there
 * as a table of segments and here as a shape, each curve lifting its
 * outside edge (Track3d.of_road's bank). A stage, alone against the
 * clock, as in TinyOutRun. *)
let coast : course =
  let ribbon = Track3d.of_road ~width:road_width ~degrees_per_curve:0.8 ~bank_per_curve:2.2 (Road.build 2. Road.coast) in
  let segments = Track3d.segments ribbon in
  make ~name:"OUT RUN COAST" ~level:"STAGE" ~ribbon ~laps:1 ~checkpoints:3 ~start_time:35 ~bonus:12 ~rivals:0
    ~scenery:(fun site i ->
      let w = road_width and s = float_of_int i *. Track3d.step site.ribbon in
      (if i mod 8 = 0 then [ place site.ribbon s (-1.4 *. w) tree; place site.ribbon s (1.4 *. w) tree ]
       else if i mod 8 = 4 then
         [ place site.ribbon s ((if i mod 16 = 4 then -1.8 else 1.8) *. w) (box (rgb 50 130 50) 1.6 1. 1.6 |> move_y3d 0.5) ]
       else [])
      @ signs site i)
    ~landscape:(fun site ->
      let w = road_width and s = float_of_int (segments - 2) *. Track3d.step site.ribbon in
      [ Track3d.strip site.ribbon white 0 (-.w) w; Track3d.strip site.ribbon white (segments - 2) (-.w) w; place site.ribbon s 0. (gantry (rgb 30 60 200)) ]
      @ List.init 2 (fun k -> place site.ribbon (float_of_int (k + 1) *. Track3d.length site.ribbon /. 3.) 0. (gantry (rgb 240 200 30))))
    ()

let courses = [| big_forest; bay_bridge; acropolis; coast |]

(*---------------------------------------------------------------------------*)
(* The road, cut in chunks *)
(*---------------------------------------------------------------------------*)

(* A segment of road: the kerbs and the tarmac; then, on land, the
 * grass verges, the rail at their edge (red and white, what the car
 * bounces off) and the sides of the embankment down to the land; grey
 * concrete walls instead of the rails where the course has them; on a
 * bridge or an overpass, the deck's edges, its rails and the girders
 * under it; in a tunnel, its walls and its roof. *)
let segment_shapes (c : course) (i : int) : shape3d list =
  let strip = Track3d.strip c.ribbon and wall = Track3d.wall c.ribbon in
  let light = i / rumble_length mod 2 = 0 in
  let rumble = if light then rgb 240 240 240 else rgb 200 40 40 in
  let tarmac = if light then rgb 110 110 110 else rgb 100 100 100 in
  let w = road_width in
  let road =
    [ strip rumble i (-1.15 *. w) (-.w); strip tarmac i (-.w) w; strip rumble i w (1.15 *. w) ]
    @ if light then [ strip white i (-0.03 *. w) (0.03 *. w) ] else []
  in
  let grass = if light then fst c.grass else snd c.grass in
  let s = segment_of c i in
  match in_span c.spans [ Suspension; Overpass; Tunnel; Wall ] s with
  | Some { kind = Suspension | Overpass; _ } ->
      road
      @ [ strip (rgb 150 150 150) i (-.verge) (-1.15 *. w); strip (rgb 150 150 150) i (1.15 *. w) verge;
          wall (rgb 230 230 230) i (-.verge) 1.; wall (rgb 230 230 230) i verge 1.;
          wall (rgb 180 70 40) i (-.verge) (-1.8); wall (rgb 180 70 40) i verge (-1.8) ]
  | Some { kind = Tunnel; _ } ->
      (* the roof: the ribbon's quad lifted 8 meters, facing down *)
      let up (x, y, z) = (x, y +. 8., z) in
      let near o = up (Track3d.across c.ribbon s o) and far o = up (Track3d.across c.ribbon (s +. Track3d.step c.ribbon) o) in
      road
      @ [ strip (rgb 90 90 90) i (-.verge) (-1.15 *. w); strip (rgb 90 90 90) i (1.15 *. w) verge;
          wall (rgb 120 120 115) i (-.verge) 8.; wall (rgb 120 120 115) i verge 8.;
          polygon3d (rgb 80 80 80) [ near verge; near (-.verge); far (-.verge); far verge ] ]
  | Some { kind = Wall; _ } ->
      road
      @ [ strip grass i (-.verge) (-1.15 *. w); strip grass i (1.15 *. w) verge; wall (rgb 175 175 170) i (-.verge) 2.5;
          wall (rgb 175 175 170) i verge 2.5; wall (snd c.grass) i (-.verge) (-3.); wall (snd c.grass) i verge (-3.) ]
  | None ->
      let rail = if i / 2 mod 2 = 0 then rgb 230 230 230 else rgb 210 50 40 in
      road
      @ [ strip grass i (-.verge) (-1.15 *. w); strip grass i (1.15 *. w) verge; wall rail i (-.verge) 0.8; wall rail i verge 0.8;
          wall (snd c.grass) i (-.verge) (-3.); wall (snd c.grass) i verge (-3.) ]

(* the chunks, each with its middle: the road near the car is drawn by
 * its place on the lap, the road near the eye by its place in the
 * world (an overpass, the other side of a hairpin) *)
let chunks : (shape3d * (number * number)) array array =
  Array.map
    (fun c ->
      let segments = Track3d.segments c.ribbon in
      Array.init ((segments + chunk_size - 1) / chunk_size) (fun k ->
          let first = k * chunk_size in
          let last = min segments (first + chunk_size) - 1 in
          let mid = Track3d.at c.ribbon (segment_of c ((first + last) / 2)) in
          ( cached3d (List.concat (List.init (last - first + 1) (fun j -> segment_shapes c (first + j) @ c.scenery (first + j)))),
            (mid.px, mid.pz) )))
    courses

let landscapes : shape3d array = Array.map (fun c -> cached3d c.landscape) courses
let lands : ((number * number) * shape3d) list array = Array.map (fun c -> land_tiles c.terrain 1) courses

(* The course select's model of the course -- the arcade showed each
 * course in 3D: the land (a facet every two cells), the road as a band
 * (every third segment, the start in white), the landmarks, all scaled
 * down to a table top. *)
let miniature_scale = 0.1

let bounds (c : course) : number * number * number * number =
  let n = Track3d.segments c.ribbon in
  List.fold_left
    (fun (x0, x1, z0, z1) k ->
      let p = Track3d.at c.ribbon (segment_of c k) in
      (Float.min x0 p.px, Float.max x1 p.px, Float.min z0 p.pz, Float.max z1 p.pz))
    (Float.infinity, Float.neg_infinity, Float.infinity, Float.neg_infinity)
    (List.init n Fun.id)

let miniature_center (c : course) : number * number =
  let x0, x1, z0, z1 = bounds c in
  ((x0 +. x1) /. 2., (z0 +. z1) /. 2.)

let miniatures : shape3d array =
  Array.map
    (fun c ->
      let cx, cz = miniature_center c in
      let n = Track3d.segments c.ribbon in
      let band =
        List.init (n / 3) (fun k ->
            let sa = segment_of c (3 * k) and sb = segment_of c (min n ((3 * k) + 3)) in
            let a o = Track3d.across c.ribbon sa o and b o = Track3d.across c.ribbon sb o in
            let lift (x, y, z) = (x, y +. 1.5, z) in
            polygon3d (if k = 0 then white else rgb 90 90 95)
              (List.map lift [ a (-2. *. road_width); a (2. *. road_width); b (2. *. road_width); b (-2. *. road_width) ]))
      in
      let tiles = List.map snd (land_tiles c.terrain 2) in
      cached3d [ group3d (tiles @ band @ c.landscape) |> move3d (-.cx) 0. (-.cz) |> scale3d miniature_scale ])
    courses

(*****************************************************************************)
(* The car *)
(*****************************************************************************)

(* The car is the racing kit's Topdown: a heading of its own, a speed,
 * and a velocity that only turns towards the heading by the grip's
 * fraction each frame -- so it goes where it points, and in a fast
 * turn slides wide before it does (see Topdown.mli). Not TinyOutRun's
 * Car, which is only *across* a road, the road turning under it: here
 * the road is a shape in space, and the car drives on it.
 *
 * What this game adds is the road's own rules: the wheel turned by the
 * keys a little at a time (a keyboard has no wheel, and a full lock at
 * once is a spin), and less of it at speed; the grass slowing the car
 * and taking its grip; the rail at the edge of the grass, which it
 * scrapes along (Topdown.bounce); and Virtua Racing's spins: too much
 * slide, or the rail hit too hard, and the car spins round, stops
 * sliding, and is set back facing the road. The Topdown plane is
 * (x, -z), its headings counterclockwise from +x ([plane_heading]). *)

let top_speed = 124. (* 330 km/h, at the road's scale *)
let grass_speed = 45.
let kmh (speed : number) : number = Float.abs speed /. top_speed *. 330.

let params : Topdown.params = { accel = 78.; friction = 0.55; grip = 0.2; steering = 2.2; steering_speed = 25. }
let grass_params : Topdown.params = { params with grip = 0.07 }

let plane_heading (world : number) : number = 90. -. world

type driving = {
  car : Topdown.t;
  s : number; (* how far along the course, laps included *)
  offset : number; (* how far to the right of the middle *)
  wheel : number; (* -1 full right, 1 full left *)
  bump : int; (* frames since the car last hit the rail *)
  spin : int; (* frames of a spin left; 0: driving *)
}

let spin_frames = 70

let driving_at (c : course) (s : number) (offset : number) : driving =
  let x, _, z = Track3d.across c.ribbon s offset in
  let p = Track3d.at c.ribbon s in
  { car = { x; y = -.z; vx = 0.; vy = 0.; heading = plane_heading p.heading; speed = 0.; next = 0 }; s; offset; wheel = 0.; bump = 99; spin = 0 }

let length (c : course) : number = Track3d.length c.ribbon
let wrap (c : course) (s : number) : number = Float.rem (Float.rem s (length c) +. length c) (length c)

(* where the car is in the world, and which way its body points (in a
 * spin, round and round) *)
let world (c : course) (d : driving) : (number * number * number) * number =
  let _, y, _ = Track3d.across c.ribbon d.s d.offset in
  let spun = if d.spin > 0 then 720. *. (1. -. ((float_of_int d.spin /. float_of_int spin_frames) ** 2.)) else 0. in
  ((d.car.x, y, -.d.car.y), 90. -. d.car.heading +. spun)

(* the angle between where the car points and where it goes: a slide *)
let slip (d : driving) : number =
  if Float.hypot d.car.vx d.car.vy < 5. then 0. else angle_diff d.car.heading (atan2 d.car.vy d.car.vx *. 180. /. Float.pi)

(* [steer_car c ~gas ~wanted d]: one frame, [gas] -1 to 1, the wheel
 * wanted -1 to 1 *)
let steer_car (c : course) ~(gas : number) ~(wanted : number) (d : driving) : driving =
  let near = if c.laps > 1 then wrap c d.s else d.s in
  let road = plane_heading (Track3d.at c.ribbon near).heading in
  let spinning = d.spin > 0 in
  let gas, wanted = if spinning then (0., 0.) else (gas, wanted) in
  let wheel = d.wheel +. Float.max (-0.1) (Float.min 0.1 (wanted -. d.wheel)) in
  let on_road = Float.abs d.offset <= 1.15 *. road_width in
  let car = if (not on_road) && d.car.speed > grass_speed then { d.car with speed = d.car.speed -. 1.2 } else d.car in
  let car = if spinning then { car with speed = car.speed *. 0.96; vx = car.vx *. 0.96; vy = car.vy *. 0.96 } else car in
  let lock = wheel *. (1. -. (0.5 *. Float.abs car.speed /. top_speed)) in
  let moved = Topdown.drive (if on_road then params else grass_params) top_speed gas lock car in
  let rail x y =
    let _, off = Track3d.locate ~near c.ribbon x (-.y) in
    Float.abs off > verge -. 0.8
  in
  let after = Topdown.bounce rail car moved in
  let hit = after.x <> moved.x || after.y <> moved.y in
  (* against the rail the car scrapes along it: some of its speed lost
   * (Topdown.bounce would halve it every frame the gas pushes it back
   * in, a stop), and its nose turned back along the road *)
  let turn = angle_diff after.heading road in
  let after =
    if not hit then after
    else { after with speed = moved.speed *. 0.85; heading = (if Float.abs turn < 90. then after.heading +. (0.25 *. turn) else after.heading) }
  in
  let s, offset = Track3d.locate ~near c.ribbon after.x (-.after.y) in
  let ds = if c.laps > 1 then angle_diff (near *. 360. /. length c) (s *. 360. /. length c) *. length c /. 360. else s -. near in
  let d' = { car = after; s = d.s +. ds; offset; wheel; bump = (if hit then 0 else d.bump + 1); spin = max 0 (d.spin - 1) } in
  (* a spin: the rail hit hard and not along it, or the car sliding too
   * far sideways; when it ends, the car is set facing the road, as the
   * arcade did *)
  if (not spinning) && ((hit && car.speed > 55. && Float.abs turn > 14.) || (Float.abs (slip d') > 32. && car.speed > 45.)) then
    { d' with spin = spin_frames }
  else if spinning && d'.spin = 0 then { d' with car = { d'.car with heading = road; vx = 0.; vy = 0.; speed = Float.min 10. d'.car.speed }; wheel = 0. }
  else d'

(*****************************************************************************)
(* The rivals *)
(*****************************************************************************)

(* Fifteen other cars, as in the arcade. Not Topdown cars: they drive
 * *on* the road, a distance along it and a lane across it, as TinyOutRun's
 * traffic does -- which is what makes them good drivers cheaply. Each
 * one:
 *   - wants its top speed (a little under the player's), and less before
 *     a corner: it looks 20 to 100 meters ahead for the sharpest turn,
 *     whose radius r allows sqrt(grip * r) (the speed at which a car
 *     going round a circle needs all its tyres' grip, v^2 / r = grip);
 *   - brakes hard to it, accelerates from it as the player does;
 *   - keeps to its lane, and moves over to pass a slower car ahead in
 *     it (the player's too), back when the lane is clear.
 * The player bumps them as a disc bumps a disc (Topdown.push); the
 * rival slows down a little, the player is pushed aside, or spins if
 * the hit was hard. *)

type rival = { rs : number; lane : number; rspeed : number; home : number; top : number; livery : color }

let liveries =
  [| rgb 30 90 220; rgb 250 250 250; rgb 250 200 20; rgb 30 160 70; rgb 240 120 20; rgb 150 50 190; rgb 20 20 20; rgb 0 170 200;
     rgb 230 90 150; rgb 120 120 130; rgb 120 70 30; rgb 60 60 180; rgb 200 200 60; rgb 220 30 30; rgb 90 200 150 |]

(* the grid: two columns, 8 meters apart in each, behind the line; the
 * player in the 9th place, as the arcade put you mid-field *)
let player_slot = 8
let slot_s (k : int) : number = -12. -. (8. *. float_of_int k)
let slot_lane (k : int) : number = if k mod 2 = 0 then -2.8 else 2.8

let grid (c : course) : rival list =
  List.init c.rivals (fun k ->
      let slot = if k < player_slot then k else k + 1 in
      { rs = slot_s slot; lane = slot_lane slot; rspeed = 0.; home = slot_lane slot; top = top_speed *. (0.9 -. (0.012 *. float_of_int k)) +. (4. *. jitter k 9);
        livery = liveries.(k mod Array.length liveries) })

let corner_speed (c : course) (s : number) : number =
  let step = Track3d.step c.ribbon in
  let k0 = int_of_float (wrap c s /. step) in
  let sharpest = ref 0. in
  for k = 10 to 50 do
    sharpest := Float.max !sharpest (Float.abs (turn_at c.ribbon (k0 + k)))
  done;
  if !sharpest < 0.05 then Float.infinity
  else
    let radius = step /. (!sharpest *. Float.pi /. 180.) in
    Float.sqrt (32. *. radius)

(* one frame of every rival; [others] where the player is, to pass it *)
let drive_rivals (c : course) (player : driving) (rivals : rival list) : rival list =
  let dt = 1. /. 60. in
  let arr = Array.of_list rivals in
  Array.to_list
    (Array.map
       (fun r ->
         let wanted = Float.min r.top (corner_speed c r.rs) in
         (* accelerating as the player's car does, harder from low speed *)
         let accel = 70. *. (1. -. (0.6 *. r.rspeed /. top_speed)) in
         let rspeed = if r.rspeed < wanted then r.rspeed +. (accel *. dt) else Float.max wanted (r.rspeed -. (45. *. dt)) in
         (* a slower car just ahead in my lane: move over *)
         let blocked lane =
           Array.exists (fun (o : rival) -> o != r && o.rs > r.rs && o.rs -. r.rs < 25. && o.rspeed < rspeed && Float.abs (o.lane -. lane) < 3.) arr
           || (player.s > r.rs && player.s -. r.rs < 25. && player.car.speed < rspeed && Float.abs (player.offset -. lane) < 3.)
         in
         let target = if blocked r.lane then if blocked (-.r.home) then r.lane else -.r.home else if blocked r.home then r.lane else r.home in
         let lane = r.lane +. Float.max (-0.08) (Float.min 0.08 (target -. r.lane)) in
         { r with rs = r.rs +. (rspeed *. dt); rspeed; lane })
       arr)

let rival_place (c : course) (r : rival) : (number * number * number) * number =
  (Track3d.across c.ribbon r.rs r.lane, (Track3d.at c.ribbon r.rs).heading)

(* the player against the rivals, each a disc of 2.2 meters *)
let bump_rivals (c : course) (d : driving) (rivals : rival list) : driving * rival list * bool =
  List.fold_left
    (fun (d, acc, crashed) r ->
      let (x, _, z), h = rival_place c r in
      if Float.hypot (x -. d.car.x) (z +. d.car.y) > 4.4 then (d, r :: acc, crashed)
      else
        let a = (plane_heading h) *. Float.pi /. 180. in
        let other : Topdown.t = { x; y = -.z; vx = r.rspeed *. cos a; vy = r.rspeed *. sin a; heading = plane_heading h; speed = r.rspeed; next = 0 } in
        let hard = Float.abs (d.car.speed -. r.rspeed) > 30. in
        let car, _ = Topdown.push 2.2 d.car other in
        let d = { d with car = { car with speed = Float.min car.speed (Float.hypot car.vx car.vy) }; bump = 0 } in
        let d = if hard && d.spin = 0 then { d with spin = spin_frames } else d in
        (d, { r with rspeed = r.rspeed *. 0.9 } :: acc, true))
    (d, [], false) rivals
  |> fun (d, acc, crashed) -> (d, List.rev acc, crashed)

(* the player's place: 1 + the rivals ahead *)
let position (d : driving) (rivals : rival list) : int = 1 + List.length (List.filter (fun r -> r.rs > d.s) rivals)

let ordinal (n : int) : string =
  let suffix = if n mod 100 >= 11 && n mod 100 <= 13 then "TH" else match n mod 10 with 1 -> "ST" | 2 -> "ND" | 3 -> "RD" | _ -> "TH" in
  string_of_int n ^ suffix

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the four views, V.R.'s buttons: in the cockpit, close behind,
 * behind and above, high overhead *)
type view = Cockpit | Close | Behind | Overhead

type race = {
  course : int;
  drive : driving;
  rivals : rival list;
  time : int; (* frames since the start; negative: the countdown *)
  clock : int; (* frames left on the clock *)
  passed : int; (* checkpoints passed *)
  message : string * int; (* in the middle of the screen, and until when *)
  lap_times : int list; (* in frames, the last one first *)
  view : view;
  over : bool; (* the time ran out *)
}

type scene = Title | Select of int | Racing of race | Finished of race

type model = scene Scene2d.t

let countdown = 180

let new_race (course : int) : race =
  let c = courses.(course) in
  let slot = if c.rivals > 0 then player_slot else 0 in
  let d = driving_at c (if c.rivals > 0 then slot_s slot else 0.) (if c.rivals > 0 then slot_lane slot else 0.) in
  { course; drive = d; rivals = grid c; time = -countdown - 1; clock = c.start_time * 60; passed = 0; message = ("", 0); lap_times = [];
    view = Close; over = false }

let initial_model : model = Scene2d.start Title

let next_view = function Cockpit -> Close | Close -> Behind | Behind -> Overhead | Overhead -> Cockpit

(* laps and checkpoints are counted from the line; the grid is behind it *)
let lap (c : course) (d : driving) : int = int_of_float (Float.floor (d.s /. c.lap_length))
let checkpoint (c : course) (d : driving) : int = int_of_float (Float.floor (d.s /. (c.lap_length /. float_of_int c.checkpoints)))

let camera_for (view : view) (speed : number) ((x, y, z) : number * number * number) (heading : number) : camera =
  (* a wider angle at speed: the road rushes past the edges *)
  let fov = 60. +. (10. *. speed) in
  let at back height ahead look = Camera3d.behind ~fov ~back ~height ~ahead ~look { x; y; z; heading } in
  match view with
  | Cockpit -> at 0.1 1.3 20. 0.4
  | Close -> at 7.5 2.6 8. 1.
  | Behind -> at 14. 6.5 12. 1.
  | Overhead -> at 22. 34. 16. 0.

(*****************************************************************************)
(* Sound *)
(*****************************************************************************)

(* A formula car's V12: seven gears (the automatic box), each taking the
 * revs from low to high over a seventh of the top speed, so the pitch
 * climbs, drops at each change, and climbs again -- the sound of speed.
 * Two voices, a bright sawtooth opened up by the throttle and a square
 * an octave down, the body. In a tunnel, the walls give it back:
 * louder, lower, boomier. *)
let gears = 7
let gear (d : driving) : int = max 1 (min gears (1 + int_of_float (Float.abs d.car.speed /. top_speed *. float_of_int gears)))

let revs (d : driving) : number =
  let g = Float.max 0. (Float.abs d.car.speed /. top_speed *. float_of_int gears) in
  let f = g -. Float.of_int (int_of_float g) in
  if g >= float_of_int gears then 1. else if g < 1. then 0.2 +. (0.8 *. f) else 0.45 +. (0.55 *. f)

let engine (rpm : number) (throttle : bool) ~(tunnel : bool) : unit =
  let hz = 45. +. (110. *. rpm) in
  let open_ = if throttle then 1. else 0.3 in
  let room = if tunnel then 1.6 else 1. in
  Audio.keep_playing "engine"
    (Audio.sawtooth hz |> Audio.low_pass ((400. +. (2400. *. rpm *. open_)) /. room) |> Audio.louder ((0.05 +. (0.04 *. open_)) *. room));
  Audio.keep_playing "engine_low" (Audio.square (hz /. 2.) |> Audio.low_pass 300. |> Audio.louder (0.05 *. room))

(* the car on the road: the tyres squealing as it slides, the grass
 * under it off the road, the wind at speed *)
let road_noise (d : driving) : unit =
  let frac = Float.abs d.car.speed /. top_speed in
  let slide = Float.abs (slip d) in
  if Float.abs d.offset > 1.15 *. road_width && Float.abs d.car.speed > 3. then
    Audio.keep_playing "gravel" (Audio.noise 500. |> Audio.low_pass (200. +. (600. *. frac)) |> Audio.louder 0.15)
  else if (slide > 4. || d.spin > 0) && Float.abs d.car.speed > 25. then
    Audio.keep_playing "squeal" (Audio.triangle 1050. |> Audio.vibrato 11. 0.5 |> Audio.louder (if d.spin > 0 then 0.06 else Float.min 0.06 (0.006 *. slide)));
  if frac > 0.3 then Audio.keep_playing "wind" (Audio.noise 6000. |> Audio.high_pass 1500. |> Audio.louder (0.03 *. frac *. frac))

let beep = Audio.sfx { Sfx.blip with frequency = 440.; sustain = 0.15; volume = 0.3 }
let go_beep = Audio.sfx { Sfx.blip with frequency = 880.; sustain = 0.5; volume = 0.3 }
let shift = Audio.noise 900. |> Audio.lasting 0.07 |> Audio.fading |> Audio.louder 0.12
let crash = Audio.sfx { Sfx.hit with volume = 0.35 }
let final_lap = Audio.after [ Audio.square 660. |> Audio.lasting 0.12; Audio.square 880. |> Audio.lasting 0.3 ] |> Audio.louder 0.2

(* the checkpoints' jingles: the arcade played one of a dozen short
 * ones, cut off before their end; here three, in turn *)
let jingles =
  List.map
    (fun notes -> Audio.abc (Printf.sprintf "X:1\nL:1/16\nQ:1/4=150\nK:C\n%s|\n" notes) |> Audio.louder 0.22)
    [ "c2e2g2c'4"; "g2e2c'2g2e'4"; "d2f2a2d'2c'4" ]

let fanfare =
  Audio.abc {|X:1
L:1/8
Q:1/4=160
K:C
G2 c2 e2 g4 e2 | g6 z2 |
|}
  |> Audio.louder 0.25

let game_over = Audio.after [ Audio.square 392. |> Audio.lasting 0.25; Audio.square 330. |> Audio.lasting 0.25; Audio.square 262. |> Audio.lasting 0.6 ] |> Audio.louder 0.2
let pick = Audio.sfx { Sfx.blip with frequency = 660.; volume = 0.25 }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let drive (computer : computer) (s : model) (r : race) ~(finished : bool) : race =
  let c = courses.(r.course) in
  let k = computer.keyboard in
  let d = r.drive in
  let d' =
    if r.time < 0 then d
    else if finished then
      (* after the line, or out of time, the car steers itself along the
       * road to a stop *)
      let road = plane_heading (Track3d.at c.ribbon (wrap c d.s)).heading in
      let wanted = Float.max (-1.) (Float.min 1. ((angle_diff d.car.heading road /. 15.) +. (d.offset *. 0.08))) in
      steer_car c ~gas:(if d.car.speed > 1. then -0.5 else 0.) ~wanted d
    else
      let gas = if k.kup then 1. else if k.kdown then if d.car.speed > 1. then -1.6 else -0.5 else 0. in
      let wanted = (if k.kleft then 1. else 0.) -. if k.kright then 1. else 0. in
      steer_car c ~gas ~wanted d
  in
  let rivals = if r.time < 0 then r.rivals else drive_rivals c d' r.rivals in
  let d', rivals, bumped = if r.time < 0 then (d', rivals, false) else bump_rivals c d' rivals in
  if gear d' > gear d then Audio.play shift;
  if (d'.bump = 0 && d.bump > 10) || (d'.spin > 0 && d.spin = 0) || (bumped && d.bump > 10) then Audio.play crash;
  (* before the start, the engine revs with the pedal, the car held *)
  let rpm = if r.time < 0 then if k.kup then 0.7 +. (0.2 *. wave 0. 1. 0.3 computer.time) else 0.2 else revs d' in
  engine rpm (k.kup && not finished) ~tunnel:(in_span c.spans [ Tunnel ] (wrap c d'.s) <> None);
  road_noise d';
  let time = r.time + 1 in
  if time < 0 && time mod 60 = 0 then Audio.play beep;
  if time = 0 then Audio.play go_beep;
  let view = if Scene2d.pressed (fun k -> Set_.mem "v" k.keys) s then next_view r.view else r.view in
  { r with drive = d'; rivals; time; view; clock = (if time > 0 && not finished then max 0 (r.clock - 1) else r.clock) }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Select 0) s else s
  | Select i ->
      let n = Array.length courses in
      let left = Scene2d.pressed (fun k -> k.kleft) s and right = Scene2d.pressed (fun k -> k.kright) s in
      if left || right then Audio.play pick;
      if space then (
        Audio.play go_beep;
        Scene2d.go (Racing (new_race i)) s)
      else if left then Scene2d.go (Select ((i + n - 1) mod n)) s
      else if right then Scene2d.go (Select ((i + 1) mod n)) s
      else s
  | Racing r ->
      let c = courses.(r.course) in
      let r' = drive computer s r ~finished:false in
      let lap_done = lap c r'.drive in
      let cp = checkpoint c r'.drive in
      if cp > r.passed && cp >= 1 && lap_done >= c.laps then begin
        (* the goal *)
        let lap_time = r'.time - List.fold_left ( + ) 0 r.lap_times in
        Audio.play fanfare;
        Scene2d.go (Finished { r' with lap_times = lap_time :: r.lap_times; passed = cp }) s
      end
      else if cp > r.passed && cp >= 1 then begin
        (* a checkpoint: time added; at the line, a lap done *)
        let at_line = cp mod c.checkpoints = 0 in
        let lap_time = r'.time - List.fold_left ( + ) 0 r.lap_times in
        let lap_times = if at_line then lap_time :: r.lap_times else r.lap_times in
        let best = at_line && List.for_all (fun t -> lap_time < t) r.lap_times in
        Audio.play (List.nth jingles (cp mod List.length jingles));
        if at_line && lap_done = c.laps - 1 then Audio.play final_lap;
        let message =
          if at_line && lap_done = c.laps - 1 then "FINAL LAP"
          else if best then Printf.sprintf "TIME BONUS  +%d   BEST LAP" c.bonus
          else Printf.sprintf "TIME BONUS  +%d" c.bonus
        in
        { s with scene = Racing { r' with passed = cp; clock = r'.clock + (c.bonus * 60); lap_times; message = (message, r'.time + 150) } }
      end
      else if r'.clock = 0 && r'.time > 0 then (
        Audio.play game_over;
        Scene2d.go (Finished { r' with over = true }) s)
      else { s with scene = Racing r' }
  | Finished r ->
      let s = { s with scene = Finished (drive computer s r ~finished:true) } in
      if space then Scene2d.go (Select r.course) s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* The car: a formula car, as Virtua Racing's were, each part a solid
 * of a few flat faces: the nose tapering to its tip, the tub, the
 * engine cover sloping from the airbox to the gearbox, the sidepods,
 * the two wings with their endplates, the four wheels (octagons: the
 * flat shading shows every face), the fronts turning with the wheel
 * (positive: to the right), the brake lights. Facing -z, the road at
 * y = 0. *)
let car_model ?(livery = rgb 220 30 40) (steer : number) (braking : bool) : shape3d =
  let white = rgb 240 240 240 and dark = rgb 35 35 40 in
  (* a section of the body: [w] half wide at the bottom, narrower at the
   * top, from [y] up [h] (counterclockwise seen from behind) *)
  let sect w h y = [ (-.w, y); (w, y); (w *. 0.75, y +. h); (-.w *. 0.75, y +. h) ] in
  let tyre r w = wheel dark 8 r w in
  let front x = tyre 0.38 0.4 |> rotate3d 0. (-.steer *. 20.) 0. |> move3d x 0.38 (-1.45) in
  let rear x = tyre 0.46 0.55 |> move3d x 0.46 1.3 in
  group3d
    [ loft livery (sect 0.1 0.12 0.3) (sect 0.3 0.35 0.25) (-2.3) (-0.9);
      loft livery (sect 0.3 0.35 0.25) (sect 0.45 0.45 0.2) (-0.9) 0.6;
      loft white (sect 0.45 0.62 0.2) (sect 0.22 0.3 0.3) 0.6 2.;
      loft livery (sect 0.22 0.3 0.15) (sect 0.22 0.3 0.15) (-0.3) 1.1 |> move_x3d 0.62;
      loft livery (sect 0.22 0.3 0.15) (sect 0.22 0.3 0.15) (-0.3) 1.1 |> move_x3d (-0.62);
      box dark 0.42 0.06 0.8 |> move3d 0. 0.66 (-0.2);
      box (rgb 250 210 0) 0.3 0.28 0.34 |> move3d 0. 0.78 0.1;
      box livery 2.1 0.06 0.45 |> move3d 0. 0.18 (-2.25);
      box white 0.05 0.3 0.5 |> move3d (-1.05) 0.25 (-2.25);
      box white 0.05 0.3 0.5 |> move3d 1.05 0.25 (-2.25);
      box livery 1.6 0.08 0.45 |> move3d 0. 1.15 2.;
      box dark 0.06 0.55 0.6 |> move3d (-0.8) 0.95 2.;
      box dark 0.06 0.55 0.6 |> move3d 0.8 0.95 2.;
      box dark 0.1 0.5 0.1 |> move3d 0. 0.85 1.9;
      box (if braking then rgb 255 60 60 else rgb 90 20 20) 0.3 0.12 0.06 |> move3d 0. 0.4 2.05;
      front (-0.95); front 0.95; rear (-0.95); rear 0.95 ]

(* a car's shadow, on the road under it, turned with the car *)
let shadow ((x, y, z) : number * number * number) (heading : number) : shape3d =
  let a = heading *. Float.pi /. 180. in
  let fx = sin a and fz = -.cos a in
  let at along across = (x +. (along *. fx) -. (across *. fz), y +. 0.04, z +. (along *. fz) +. (across *. fx)) in
  polygon3d (rgb 45 50 45) [ at (-2.) (-0.95); at (-2.) 0.95; at 2. 0.95; at 2. (-0.95) ]

(* The sky, the haze and the floor. Camera3d.sky's sky is only 10
 * above the eye, which would cut the mountains off at that height (the
 * sky is a plane, in front of whatever is above it); this one is a
 * ceiling 200 up (over the tallest mountain), from behind the eye to
 * far ahead; the floor, under the land (for beyond its edge), goes as
 * far; and the haze is a slope rising from the floor's far edge to
 * meet the ceiling, which bends down to it -- all within the far plane
 * (2000), and all nearly flat, facing up: flat shading lights a face by
 * its slope, and a steep haze would be grey, darker or lighter with the
 * heading (see Camera3d.sky's comment); the sky is seen from below (see
 * [main]). The haze starts past the mountains: in front of them it
 * would hide their feet and leave their tops floating.
 *
 *     ceiling  ________________________
 *      (200 up)                        \___  (40 up)
 *       eye                           ___/
 *     floor ___________________________/  haze
 *                                  1400  1560 ahead *)
let sky_and_floor ?(ground = ground) (floor : color) (cam : camera) : shape3d list =
  let ex, ey, ez = cam.eye and tx, _, tz = cam.target in
  let d = Float.max 1e-9 (Float.hypot (tx -. ex) (tz -. ez)) in
  let fx = (tx -. ex) /. d and fz = (tz -. ez) /. d in
  let at ahead k y = (ex +. (ahead *. fx) -. (k *. 1200. *. fz), y, ez +. (ahead *. fz) +. (k *. 1200. *. fx)) in
  let top = ey +. 200. and low = ey +. 40. and bend = 1000. and far = 1400. and edge = 1560. in
  let sky = rgb 150 205 250 in
  [ polygon3d floor [ at (-300.) (-1.) ground; at (-300.) 1. ground; at far 1. ground; at far (-1.) ground ];
    polygon3d sky [ at (-300.) (-1.) top; at (-300.) 1. top; at bend 1. top; at bend (-1.) top ];
    polygon3d sky [ at bend (-1.) top; at bend 1. top; at edge 1. low; at edge (-1.) low ];
    polygon3d (rgb 215 232 248) [ at far (-1.) ground; at far 1. ground; at edge 1. low; at edge (-1.) low ] ]

(* the land's tiles within sight of the eye *)
let land_near (i : int) (cam : camera) : shape3d list =
  let ex, _, ez = cam.eye in
  List.filter_map (fun ((x, z), tile) -> if Float.hypot (x -. ex) (z -. ez) < 1300. then Some tile else None) lands.(i)

(* the chunks of road around the car along the lap, and any near the
 * eye (an overpass, the other side of a hairpin) *)
let road_near (i : int) (c : course) (s : number) (cam : camera) : shape3d list =
  let chunk = int_of_float (wrap c s /. Track3d.step c.ribbon) / chunk_size in
  let n = Array.length chunks.(i) in
  let closed = c.laps > 1 in
  let ex, _, ez = cam.eye in
  List.filteri
    (fun k _ ->
      let ahead = if closed then ((k - chunk + 1) mod n + n) mod n else k - chunk + 1 in
      let _, (x, z) = chunks.(i).(k) in
      (ahead >= 0 && ahead < 7) || Float.hypot (x -. ex) (z -. ez) < 160.)
    (Array.to_list chunks.(i))
  |> List.map fst

let text color size str = words color str |> scale size

let time_text (frames : int) : string =
  let frames = max 0 frames in
  Printf.sprintf "%d'%02d\"%02d" (frames / 3600) (frames / 60 mod 60) (frames mod 60 * 100 / 60)

(* the course's map, bottom right, its level under it; the rivals dots,
 * the player a red marker *)
let minimap (screen : screen) (c : course) (d : driving) (rivals : rival list) : shape list =
  let x0, x1, z0, z1 = bounds c in
  let k = 170. /. Float.max (x1 -. x0) (z1 -. z0) in
  let cx = screen.right -. 120. and cy = screen.bottom +. 150. in
  let at px pz = (cx +. ((px -. ((x0 +. x1) /. 2.)) *. k), cy -. ((pz -. ((z0 +. z1) /. 2.)) *. k)) in
  let n = Track3d.segments c.ribbon in
  let dots =
    List.init (n / 6) (fun j ->
        let p = Track3d.at c.ribbon (segment_of c (6 * j)) in
        let x, y = at p.px p.pz in
        square (rgb 230 230 230) 3. |> move x y)
  in
  let others =
    List.map
      (fun r ->
        let p = Track3d.at c.ribbon r.rs in
        let x, y = at p.px p.pz in
        circle (rgb 60 140 255) 3.5 |> move x y)
      rivals
  in
  let x, y = at d.car.x (-.d.car.y) in
  dots @ others @ [ circle red 6. |> move x y; text yellow 2. c.level |> move cx (cy -. 110.) ]

(* the tachometer, an arc on the left: the revs round it, the yellow
 * zone where to change gear, the red one past it, the needle *)
let tachometer (screen : screen) (rpm : number) (gear : int) : shape list =
  let cx = screen.left +. 120. and cy = screen.bottom +. 190. in
  let r = 80. in
  let angle f = 210. -. (240. *. f) in
  let marks =
    List.init 25 (fun k ->
        let f = float_of_int k /. 24. in
        let color = if f > 0.92 then red else if f > 0.78 then yellow else white in
        let a = angle f *. Float.pi /. 180. in
        rectangle color 4. 14. |> rotate (angle f -. 90.) |> move (cx +. (r *. cos a)) (cy +. (r *. sin a)))
  in
  let a = angle (Float.min 1. rpm) in
  let ar = a *. Float.pi /. 180. in
  let needle = rectangle (rgb 255 90 40) 72. 4. |> rotate a |> move (cx +. (36. *. cos ar)) (cy +. (36. *. sin ar)) in
  marks @ [ needle; circle (rgb 40 40 40) 22. |> move cx cy; text white 2.5 (string_of_int gear) |> move cx cy ]

let hud_race (screen : screen) (s : model) (c : course) (r : race) : shape list =
  let top = screen.top and left = screen.left and right = screen.right and bottom = screen.bottom in
  let d = r.drive in
  let lap_now = max 1 (min c.laps (lap c d + 1)) in
  let current = r.time - List.fold_left ( + ) 0 r.lap_times in
  let best = match r.lap_times with [] -> None | l -> Some (List.fold_left min max_int l) in
  let hurry = r.clock < 10 * 60 && r.time > 0 in
  [ text white 2. "TIME" |> move_y (top -. 30.);
    text (if hurry then red else yellow) 7. (string_of_int ((r.clock + 59) / 60)) |> move_y (top -. 80.);
    text white 2. "LAP TIME" |> move (right -. 150.) (top -. 30.);
    text white 2.8 (time_text (max 0 current)) |> move (right -. 150.) (top -. 62.);
    text yellow 2.4 (match best with Some b -> "BEST " ^ time_text b | None -> "") |> move (right -. 150.) (top -. 96.);
    text white 2.4 (Printf.sprintf "SPEED %3.0f km/h" (kmh d.car.speed)) |> move (left +. 170.) (bottom +. 70.);
    text white 2.4 (if c.laps > 1 then Printf.sprintf "LAPS %d/%d" lap_now c.laps else c.name) |> move (left +. 170.) (bottom +. 35.) ]
  @ (if c.rivals > 0 then
       [ text white 2. "POSITION" |> move (left +. 110.) (top -. 30.);
         text yellow 4. (Printf.sprintf "%s/%d" (ordinal (position d r.rivals)) (c.rivals + 1)) |> move (left +. 110.) (top -. 70.) ]
     else [])
  @ tachometer screen (revs d) (gear d)
  @ minimap screen c d r.rivals
  @ (if r.time < 0 then [ text yellow 12. (string_of_int (((-r.time - 1) / 60) + 1)) |> move_y 150. ]
     else if r.time < 60 then [ text (rgb 60 230 90) 12. "GO!" |> move_y 150. ]
     else [])
  @
  let msg, until = r.message in
  if r.time < until then Scene2d.blink 0.3 s [ text yellow 4. msg |> move_y 180. ] else []

(* the cars: the player's, and the rivals within sight, each with its
 * shadow *)
let cars (c : course) (cam : camera) (r : race) ~(braking : bool) : shape3d list =
  let d = r.drive in
  let (x, y, z), heading = world c d in
  let ex, _, ez = cam.eye in
  let player = [ car_model (-.d.wheel) braking |> rotate3d 0. (-.heading) 0. |> move3d x y z; shadow (x, y, z) heading ] in
  let rivals =
    List.concat_map
      (fun rv ->
        let (x, y, z), h = rival_place c rv in
        if Float.hypot (x -. ex) (z -. ez) > 350. then []
        else [ car_model ~livery:rv.livery 0. false |> rotate3d 0. (-.h) 0. |> move3d x y z; shadow (x, y, z) h ])
      r.rivals
  in
  (if r.view = Cockpit then List.filteri (fun k _ -> k = 0) player else player) @ rivals

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let c = courses.(0) in
      let r = new_race 0 in
      let (x, y, z), _ = world c r.drive in
      let cam = Camera3d.orbit ~distance:11. ~height:3.5 ~look:0.8 (spin 12. computer.time) (x, y, z) in
      ( cam,
        sky_and_floor (snd c.grass) cam @ land_near 0 cam @ [ landscapes.(0) ] @ c.animated computer.time @ road_near 0 c r.drive.s cam
        @ cars c cam r ~braking:false
        @ List.map hud
            ([ text (rgb 250 60 40) 7. "TINY VIRTUA RACING" |> move_y 300.;
               text white 2.5 "up: accelerate   down: brake   left/right: steer   v: view" |> move_y 230. ]
            @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 170. ]) )
  | Select i ->
      let c = courses.(i) in
      (* the course on a table, turning, seen from above and aside *)
      let cam = Camera3d.orbit ~distance:110. ~height:105. ~look:0. (spin 10. computer.time) (0., 0., 0.) in
      let cx, cz = miniature_center c in
      let moving = List.map (fun sh -> sh |> move3d (-.cx) 0. (-.cz) |> scale3d miniature_scale) (c.animated computer.time) in
      ( cam,
        sky_and_floor ~ground:(-40.) (rgb 40 80 50) cam @ [ miniatures.(i) ] @ moving
        @ List.map hud
            ([ text white 4. "SELECT COURSE" |> move_y (screen.top -. 60.);
               text (rgb 250 140 30) 7. (Printf.sprintf "<  %s  >" c.name) |> move_y (screen.bottom +. 170.);
               text white 3.
                 (Printf.sprintf "%s   %s   %.1f km" c.level (if c.laps > 1 then Printf.sprintf "%d LAPS" c.laps else "TO THE GOAL") (c.lap_length /. 1000.))
               |> move_y (screen.bottom +. 110.) ]
            @ Scene2d.blink 1. s [ text yellow 3. "left/right: course    space: race" |> move_y (screen.bottom +. 60.) ]) )
  | Racing r | Finished r ->
      let c = courses.(r.course) in
      let d = r.drive in
      let (x, y, z), heading = world c d in
      (* the camera follows the car's heading, not its spin *)
      let heading = if d.spin > 0 then 90. -. d.car.heading else heading in
      let cam = camera_for r.view (Float.abs d.car.speed /. top_speed) (x, y, z) heading in
      (* off the road, and against the rail, the camera shakes with the car *)
      let shake =
        (if Float.abs d.offset > 1.15 *. road_width then 0.12 *. Float.abs d.car.speed /. top_speed else 0.)
        +. if d.bump < 12 then 0.3 *. float_of_int (12 - d.bump) /. 12. else 0.
      in
      let cam =
        let ex, ey, ez = cam.eye in
        { cam with eye = (ex, ey +. (shake *. sin (float_of_int r.time *. 2.1)), ez) }
      in
      let braking = computer.keyboard.kdown || match s.scene with Finished _ -> true | _ -> false in
      let huds =
        match s.scene with
        | Finished r when r.over ->
            [ text red 8. "GAME OVER" |> move_y 250.; text white 3. (Printf.sprintf "%d LAPS DONE" (List.length r.lap_times)) |> move_y 170. ]
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 100. ]
        | Finished r ->
            [ text yellow 8. "GOAL!" |> move_y 250.; text white 4. (Printf.sprintf "TIME %s" (time_text r.time)) |> move_y 160. ]
            @ (match r.lap_times with
              | [] | [ _ ] -> []
              | times -> [ text white 3. (Printf.sprintf "BEST LAP %s" (time_text (List.fold_left min max_int times))) |> move_y 110. ])
            @ (if c.rivals > 0 then [ text white 3. (Printf.sprintf "POSITION %s" (ordinal (position d r.rivals))) |> move_y 70. ] else [])
            @ Scene2d.blink 1. s [ text white 3. "PRESS SPACE" |> move_y 20. ]
        | _ -> hud_race screen s c r
      in
      ( cam,
        sky_and_floor (snd c.grass) cam @ land_near r.course cam @ [ landscapes.(r.course) ] @ c.animated computer.time
        @ road_near r.course c d.s cam @ cars c cam r ~braking @ List.map hud huds )

let app = game3d view update initial_model

(* flat shading, Virtua Racing's look; the back faces drawn too, for the
 * sky (see [sky_and_floor]) and the thin things seen from both sides
 * (the cables, the Ferris wheel, the sails) *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
