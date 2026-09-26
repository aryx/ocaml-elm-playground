(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Fortnite Battle Royale (Epic Games, 2017): sixteen
 * fighters jump out of a flying bus over an island, glide down, loot
 * chests for better guns, and fight until one is left, while a storm
 * closes in and shrinks the island to a point. And they build: a wall,
 * a floor or a ramp of wood, in a second, anywhere -- the one idea
 * that set it apart from the battle royales before it (PUBG, 2017,
 * after Brendan Greene's mods of ARMA 2). W/S/A/D to walk, the arrows
 * (or the mouse) to turn and aim, space to jump (and out of the bus),
 * 1 the gun, 2 a wall, 3 a floor, 4 a ramp, x (or a click) to fire or
 * to build. Be the last one standing: a Victory Royale.
 *
 * Three ideas, each with its section below:
 *
 * 1. *The building grid.* A piece is not put where you point but snapped
 *    to a grid of 4 m cells, 3 m storeys: a floor or a ramp fills a
 *    cell, a wall stands on one of its edges. That is what lets a
 *    player build a tower in two seconds without aiming -- the grid
 *    aims for them ([ghost]: the edge or cell ahead, at the storey of
 *    your feet) -- and what makes the pieces fit each other exactly,
 *    which the next idea needs.
 *
 * 2. *Structural integrity.* Every piece must be held up: by the
 *    ground, or by another piece it shares an edge of the grid with.
 *    Pieces are the nodes of a graph, a shared edge ([segments]) is a
 *    link, and after a piece is shot down, a flood fill from the pieces
 *    touching the ground ([standing]) finds what still stands;
 *    everything it doesn't reach falls:
 *
 *         +----+----+
 *         |    |    |   <- the floors of the second storey: held by
 *         +====+====+      the walls under them, through their edges
 *         |    X    |
 *        =+====+====+=  <- the ground: the fill starts from the pieces
 *                          touching it
 *
 *    Shoot the two walls marked X and |, and the floor above them has
 *    no edge left on anything that stands. TinyTeardown.ml does the
 *    same fill over voxels; here it is over a graph of a few hundred
 *    pieces, which is why a Fortnite tower can be cut down by its
 *    bottom wall. The same rule decides what can be built: a new piece
 *    must touch the ground or a piece ([supported]), so nobody builds in
 *    the air.
 *
 * 3. *The storm.* A battle royale is a map too big for its players,
 *    made small on a timetable: a circle, a wait, a shrink towards a
 *    smaller circle drawn at random inside it, and damage outside it,
 *    stronger at each stage ([stages]). The storm is the game's
 *    director: it decides where the fights will be, and when.
 *
 * The bots play by the same rules and through the same record as the
 * player's keys ([intent]): libs/ai's Bot (a reaction time, a rate of
 * thought, an aim that settles) and Sense (an enemy seen only through a
 * clear line of sight, remembered for a while). Shot, a bot with wood
 * builds itself a wall, as Fortnite players learnt to do before
 * anything else.
 *
 * What it uses: Playground3d (the island cached, the storm's wall
 * translucent, the HUD), Camera3d (forward, sky), the heightmap kit
 * (the island, as TinyComanche3d.ml's), libs/ai's Bot and Sense, Scene2d
 * and Sprite (the minimap). Not Character3d or Physics3d: the collision
 * is the grid's -- a wall is an edge you cannot cross, a floor or a
 * ramp a height to stand on -- as TinyTombRaider.ml's is. Not the
 * brawler kit's Skeleton: the fighters are boxes, their legs swung by a
 * sine.
 *
 * Exercises: the pickaxe, harvesting wood from the trees (and trees
 * that stop you); stone and metal, stronger and slower to build;
 * editing a piece (a window, a door); roofs (a pyramid on a cell);
 * shields, and potions to drink; the camera pulled in front of a wall
 * behind the player (TinyTombRaider.ml's [look]); bots finding a way
 * round the walls (ai/'s Pathfind on the build grid); a map of named
 * places ("Tilted Towers"); spectating the one who beat you; and the
 * real thing, a hundred players over the network (Multiplayer3d, as
 * TinyCyberSled.ml).
 *)
open Playground
open Playground3d

type xyz = number * number * number

let fl = float_of_int

(*****************************************************************************)
(* The island *)
(*****************************************************************************)

(* a heightmap's cell is 8 m: the island is 32 x 32 of them, 256 m
 * across, x east and z south; the heights in metres *)
let cell = 8.
let map : Heightmap.t = Heightmap.generate ~seed:5 ~size:32 ~top:26. ~roughness:0.5
let width = cell *. fl map.size

(* The ground at (x, z): each cell of the grid two triangles, split
 * along its diagonal, and the height read off the one the point is in
 * -- the triangles the island is drawn with, so the feet stand on what
 * is drawn (TinyTombRaider.ml's [on_triangles]). *)
let ground_at (x : number) (z : number) : number =
  let fx = x /. cell and fz = z /. cell in
  let i = int_of_float (Float.floor fx) and j = int_of_float (Float.floor fz) in
  let u = fx -. fl i and v = fz -. fl j in
  let h a b = Heightmap.cell map a b in
  let h00 = h i j and h10 = h (i + 1) j and h11 = h (i + 1) (j + 1) and h01 = h i (j + 1) in
  if u >= v then h00 +. ((h10 -. h00) *. u) +. ((h11 -. h10) *. v) else h00 +. ((h11 -. h01) *. u) +. ((h01 -. h00) *. v)

let on_land (x : number) (z : number) : bool = ground_at x z > map.sea +. 1.

let color_of (kind : Heightmap.kind) (light : int) : color =
  let r, g, b = Heightmap.color kind light in
  rgb r g b

(* a hash, -1 to 1, the same natively and on the web: every random
 * thing of the game, from the trees to a shot's spread *)
let rnd (a : int) (b : int) : number = Heightmap.random 77 a b

(* places on land, from a hash: [n] tries, the ones that land *)
let spots (salt : int) (n : int) (ok : number -> number -> bool) : (number * number) list =
  List.filter_map
    (fun k ->
      let x = width *. (0.5 +. (0.42 *. rnd salt k)) and z = width *. (0.5 +. (0.42 *. rnd (salt + 1) k)) in
      if ok x z then Some (x, z) else None)
    (List.init n Fun.id)

let trees : (number * number) list =
  spots 10 90 (fun x z ->
      match Heightmap.kind map (int_of_float (x /. cell)) (int_of_float (z /. cell)) with
      | Grass | Forest -> true
      | _ -> false)

let chest_spots : (number * number) list = spots 20 60 (fun x z -> on_land x z && ground_at x z < 16.)

(* where the bots mean to land: spread over the island, one each (the
 * player's is never used) *)
let drops : (number * number) array = Array.of_list (spots 70 80 on_land)

(* the grid as triangles, counter-clockwise from above, coloured by
 * their cell's kind and light; the cells all at sea left out, for the
 * sea just under them ([surroundings]) *)
let island_mesh : shape3d list =
  let n = map.size in
  let p i j = (fl i *. cell, Heightmap.cell map i j, fl j *. cell) in
  let quad i j =
    if List.for_all (fun (a, b) -> Heightmap.cell map a b <= map.sea) [ (i, j); (i + 1, j); (i + 1, j + 1); (i, j + 1) ] then []
    else
      let c = color_of (Heightmap.kind map i j) (Heightmap.light map i j) in
      [ polygon3d c [ p i j; p (i + 1) (j + 1); p (i + 1) j ]; polygon3d c [ p i j; p i (j + 1); p (i + 1) (j + 1) ] ]
  in
  List.concat (List.init (n * n) (fun k -> quad (k mod n) (k / n)))

(* a cone of [sides] faces on a circle of radius [r] *)
let cone (color : color) (sides : int) (r : number) (h : number) : shape3d =
  let at k = let a = 2. *. Float.pi *. fl k /. fl sides in (r *. cos a, 0., r *. sin a) in
  group3d (List.init sides (fun k -> polygon3d color [ (0., h, 0.); at (k + 1); at k ]))

let tree_shape ((x, z) : number * number) : shape3d =
  let g = ground_at x z in
  group3d [ box (rgb 96 66 40) 0.5 3. 0.5 |> move_y3d 1.5; cone (rgb 40 110 50) 6 2.2 5. |> move_y3d 2.5 ] |> move3d x g z

(* the island and its trees never change: built once *)
let island : shape3d = cached3d (island_mesh @ List.map tree_shape trees)

(*****************************************************************************)
(* The building grid *)
(*****************************************************************************)

(* A cell is [tile] across, a storey [storey] high. A wall along x
 * stands on the edge z = j of the cell (i, j), from x = i to i + 1; a
 * wall along z on the edge x = i, from z = j to j + 1; a floor fills the
 * cell at its storey's height; a ramp too, rising a storey towards
 * (dx, dz). *)
let tile = 4.
let storey = 3.

type kind = Wall_x | Wall_z | Floor | Ramp of (int * int)
type piece = { kind : kind; i : int; j : int; k : int; hp : number }

let full_hp = 150.
let cost = 10

let same (a : piece) (b : piece) : bool =
  a.i = b.i && a.j = b.j && a.k = b.k
  && match (a.kind, b.kind) with Ramp _, Ramp _ -> true | x, y -> x = y

(* how far up a ramp is at (u, v), its cell's fractions, 0 to 1 *)
let rise ((dx, dz) : int * int) (u : number) (v : number) : number =
  match (dx, dz) with 1, _ -> u | -1, _ -> 1. -. u | _, 1 -> v | _ -> 1. -. v

(* its four corners, in order round it: counter-clockwise from above
 * for a floor or a ramp, so that the light finds its face turned up *)
let corners (p : piece) : xyz list =
  let x0 = fl p.i *. tile and z0 = fl p.j *. tile and y0 = fl p.k *. storey in
  let x1 = x0 +. tile and z1 = z0 +. tile and y1 = y0 +. storey in
  match p.kind with
  | Wall_x -> [ (x0, y0, z0); (x1, y0, z0); (x1, y1, z0); (x0, y1, z0) ]
  | Wall_z -> [ (x0, y0, z0); (x0, y0, z1); (x0, y1, z1); (x0, y1, z0) ]
  | Floor -> [ (x0, y0, z0); (x0, y0, z1); (x1, y0, z1); (x1, y0, z0) ]
  | Ramp d ->
      let at u v = (x0 +. (u *. tile), y0 +. (storey *. rise d u v), z0 +. (v *. tile)) in
      [ at 0. 0.; at 0. 1.; at 1. 1.; at 1. 0. ]

(* The edges of the grid a piece lies along, which are what holds pieces
 * together: ('x', i, j, k) is the segment from the grid's point (i, j,
 * k) one step along x, 'z' along z, 'y' one storey up. Two pieces
 * touch when they share one. A ramp is held by its low edge and its
 * high one, a storey apart. *)
type segment = char * int * int * int

let segments (p : piece) : segment list =
  let i = p.i and j = p.j and k = p.k in
  match p.kind with
  | Wall_x -> [ ('x', i, j, k); ('x', i, j, k + 1); ('y', i, j, k); ('y', i + 1, j, k) ]
  | Wall_z -> [ ('z', i, j, k); ('z', i, j, k + 1); ('y', i, j, k); ('y', i, j + 1, k) ]
  | Floor -> [ ('x', i, j, k); ('x', i, j + 1, k); ('z', i, j, k); ('z', i + 1, j, k) ]
  | Ramp (1, _) -> [ ('z', i, j, k); ('z', i + 1, j, k + 1) ]
  | Ramp (-1, _) -> [ ('z', i + 1, j, k); ('z', i, j, k + 1) ]
  | Ramp (_, 1) -> [ ('x', i, j, k); ('x', i, j + 1, k + 1) ]
  | Ramp _ -> [ ('x', i, j + 1, k); ('x', i, j, k + 1) ]

(* on the ground: one of its lowest corners at or under the island *)
let grounded (p : piece) : bool =
  let cs = corners p in
  let low = List.fold_left (fun m (_, y, _) -> Float.min m y) infinity cs in
  List.exists (fun (x, y, z) -> y = low && y <= ground_at x z +. 0.3) cs

(* The flood fill: from every grounded piece, through shared edges, to
 * every piece it reaches. Returns the pieces still standing, and the
 * ones that fall. *)
let standing (pieces : piece list) : piece list * piece list =
  let a = Array.of_list pieces in
  let by_segment = Hashtbl.create 64 in
  Array.iteri (fun n p -> List.iter (fun s -> Hashtbl.add by_segment s n) (segments p)) a;
  let reached = Array.make (Array.length a) false in
  let rec fill = function
    | [] -> ()
    | n :: rest when reached.(n) -> fill rest
    | n :: rest ->
        reached.(n) <- true;
        fill (List.concat_map (fun s -> Hashtbl.find_all by_segment s) (segments a.(n)) @ rest)
  in
  fill (List.filter (fun n -> grounded a.(n)) (List.init (Array.length a) Fun.id));
  let kept = ref [] and fell = ref [] in
  Array.iteri (fun n p -> if reached.(n) then kept := p :: !kept else fell := p :: !fell) a;
  (List.rev !kept, List.rev !fell)

(* can [p] be built: not already there, and held by the ground or by a
 * standing piece -- nobody builds in the air *)
let supported (pieces : piece list) (p : piece) : bool =
  (not (List.exists (same p) pieces))
  && (grounded p || List.exists (fun q -> List.exists (fun s -> List.mem s (segments q)) (segments p)) pieces)

let cell_of (x : number) (z : number) : int * int = (int_of_float (Float.floor (x /. tile)), int_of_float (Float.floor (z /. tile)))

(* the height of a floor or a ramp at (x, z), if it covers that point *)
let surface (p : piece) (x : number) (z : number) : number option =
  let ci, cj = cell_of x z in
  if ci <> p.i || cj <> p.j then None
  else
    match p.kind with
    | Floor -> Some (fl p.k *. storey)
    | Ramp d -> Some ((fl p.k *. storey) +. (storey *. rise d ((x /. tile) -. fl p.i) ((z /. tile) -. fl p.j)))
    | Wall_x | Wall_z -> None

(* a fighter's size *)
let radius = 0.45
let tall = 1.8
let step_up = 0.6

(* What a fighter at (x, y, z) stands on: the island, or the highest
 * floor or ramp under it that it can step onto. *)
let support (pieces : piece list) (x : number) (z : number) (y : number) : number =
  List.fold_left
    (fun m p -> match surface p x z with Some s when s <= y +. step_up -> Float.max m s | _ -> m)
    (ground_at x z) pieces

(* a floor or a ramp over its head, between [y0] and [y1] *)
let ceiling (pieces : piece list) (x : number) (z : number) (y0 : number) (y1 : number) : bool =
  List.exists (fun p -> match surface p x z with Some s -> s > y0 +. tall -. 0.1 && s < y1 +. tall | None -> false) pieces

(* a wall across the disc of a fighter at (x, y, z): the edge is a line
 * it cannot come nearer than its radius to, from the storey's bottom to
 * its top *)
let walled (pieces : piece list) (x : number) (z : number) (y : number) : bool =
  List.exists
    (fun p ->
      let y0 = fl p.k *. storey in
      y0 < y +. tall && y0 +. storey > y +. step_up
      &&
      let a = fl p.i *. tile and b = fl p.j *. tile in
      match p.kind with
      | Wall_x -> Float.abs (z -. b) < radius && x > a -. radius && x < a +. tile +. radius
      | Wall_z -> Float.abs (x -. a) < radius && z > b -. radius && z < b +. tile +. radius
      | Floor | Ramp _ -> false)
    pieces

(* the side of the grid a heading faces most (Camera3d's heading: 0
 * towards -z, 90 towards +x) *)
let cardinal (heading : number) : int * int =
  match ((int_of_float (Float.round (heading /. 90.)) mod 4) + 4) mod 4 with
  | 0 -> (0, -1)
  | 1 -> (1, 0)
  | 2 -> (0, 1)
  | _ -> (-1, 0)

(* The slots of the hotbar: the gun, then the three pieces. *)
type slot = Gun | Wall | Floor_slot | Ramp_slot

(* What a slot would build for a fighter standing at (x, y, z) and
 * facing [heading]: the grid aims. A wall on the edge of its cell
 * ahead; a floor or a ramp in the cell ahead, the ramp rising away; at
 * the storey its feet are in (a little above the storey's floor counts:
 * the top third of a ramp is already the next storey, so ramps chain),
 * or, on a slope, where that storey floats over the ground, the one
 * below, sunk in the hill. Some piece, and whether it can be built. *)
let ghost (pieces : piece list) (slot : slot) (x : number) (y : number) (z : number) (heading : number) : (piece * bool) option =
  let i, j = cell_of x z in
  let dx, dz = cardinal heading in
  let at k =
    let p kind i j = Some { kind; i; j; k; hp = full_hp } in
    match slot with
    | Gun -> None
    | Wall -> (
        match (dx, dz) with
        | 1, _ -> p Wall_z (i + 1) j
        | -1, _ -> p Wall_z i j
        | _, 1 -> p Wall_x i (j + 1)
        | _ -> p Wall_x i j)
    | Floor_slot -> p Floor (i + dx) (j + dz)
    | Ramp_slot -> p (Ramp (dx, dz)) (i + dx) (j + dz)
  in
  let k = int_of_float (Float.floor ((y +. 1.) /. storey)) in
  match (at k, at (k - 1)) with
  | Some p, _ when List.exists (same p) pieces -> Some (p, false)
  | Some p, _ when supported pieces p -> Some (p, true)
  | Some p, Some q -> if supported pieces q then Some (q, true) else Some (p, false)
  | _ -> None

(*****************************************************************************)
(* Rays: what a shot hits *)
(*****************************************************************************)

let add (ax, ay, az) (bx, by, bz) = (ax +. bx, ay +. by, az +. bz)
let sub (ax, ay, az) (bx, by, bz) = (ax -. bx, ay -. by, az -. bz)
let times s (x, y, z) = (s *. x, s *. y, s *. z)
let dot (ax, ay, az) (bx, by, bz) = (ax *. bx) +. (ay *. by) +. (az *. bz)
let cross (ax, ay, az) (bx, by, bz) = ((ay *. bz) -. (az *. by), (az *. bx) -. (ax *. bz), (ax *. by) -. (ay *. bx))

(* [ray_quad o d quad]: how far along the ray (o, d) it meets the
 * convex, flat quad: the plane first, then the point on the inner side
 * of all four edges *)
let ray_quad (o : xyz) (d : xyz) (quad : xyz list) : number option =
  match quad with
  | a :: b :: _ :: e :: _ ->
      let n = cross (sub b a) (sub e a) in
      let den = dot n d in
      if Float.abs den < 1e-9 then None
      else
        let t = dot n (sub a o) /. den in
        if t <= 0. then None
        else
          let p = add o (times t d) in
          let rec edges = function
            | u :: (v :: _ as rest) -> dot (cross (sub v u) (sub p u)) n >= 0. && edges rest
            | _ -> true
          in
          if edges (quad @ [ a ]) then Some t else None
  | _ -> None

(* [ray_body o d (x, y, z)]: where the ray meets a fighter standing at
 * (x, y, z), an upright cylinder: across, in the plane seen from above,
 * then the height checked at that point *)
let ray_body (o : xyz) (d : xyz) ((x, y, z) : xyz) : number option =
  let ox, oy, oz = o and dx, dy, dz = d in
  let px = ox -. x and pz = oz -. z in
  let a = (dx *. dx) +. (dz *. dz) and b = (px *. dx) +. (pz *. dz) and c = (px *. px) +. (pz *. pz) -. (radius *. radius) in
  let disc = (b *. b) -. (a *. c) in
  if a < 1e-9 || disc < 0. then None
  else
    let t = (-.b -. sqrt disc) /. a in
    let h = oy +. (t *. dy) in
    if t > 0. && h >= y && h <= y +. tall then Some t else None

(* the island along a ray, a metre at a time, until [range] *)
let ray_ground (o : xyz) (d : xyz) (range : number) : number option =
  let rec go t =
    if t > range then None
    else
      let x, y, z = add o (times t d) in
      if y < ground_at x z then Some t else go (t +. 1.)
  in
  go 1.

(*****************************************************************************)
(* The storm *)
(*****************************************************************************)

(* at each stage: the seconds it waits, the seconds it takes to shrink,
 * the radius it shrinks to, the damage outside it per second *)
let stages = [| (40, 20, 80., 1.); (20, 15, 40., 2.); (15, 12, 18., 5.); (10, 10, 5., 8.); (5, 10, 0., 10.) |]

type circle = { cx : number; cz : number; r : number }

type storm = {
  from_ : circle;
  to_ : circle;
  stage : int;
  clock : int; (* frames into the stage *)
}

(* The next circle, inside the last one: its center at random within
 * [from.r - r] of the last center, on land if one of a few tries is. *)
let next_circle (stage : int) (from_ : circle) (r : number) : circle =
  let tries =
    List.init 8 (fun n ->
        let a = Float.pi *. rnd (30 + stage) n and d = (from_.r -. r) *. Float.abs (rnd (40 + stage) n) in
        { cx = from_.cx +. (d *. cos a); cz = from_.cz +. (d *. sin a); r })
  in
  match List.filter (fun c -> on_land c.cx c.cz) tries with c :: _ -> c | [] -> List.hd tries

let first_circle = { cx = width /. 2.; cz = width /. 2.; r = 190. }
let new_storm () : storm = { from_ = first_circle; to_ = next_circle 0 first_circle 80.; stage = 0; clock = 0 }
let last_stage (s : storm) : bool = s.stage >= Array.length stages

(* where the storm's edge is now: waiting, the last circle; shrinking,
 * between the two *)
let circle_now (s : storm) : circle =
  if last_stage s then s.to_
  else
    let wait, shrink, _, _ = stages.(s.stage) in
    let t = Float.max 0. (Float.min 1. (fl (s.clock - (wait * 60)) /. fl (shrink * 60))) in
    let mix a b = a +. ((b -. a) *. t) in
    { cx = mix s.from_.cx s.to_.cx; cz = mix s.from_.cz s.to_.cz; r = mix s.from_.r s.to_.r }

let step_storm (s : storm) : storm =
  if last_stage s then s
  else
    let wait, shrink, _, _ = stages.(s.stage) in
    if s.clock < (wait + shrink) * 60 then { s with clock = s.clock + 1 }
    else
      let stage = s.stage + 1 in
      let to_ = if stage < Array.length stages then (let _, _, r, _ = stages.(stage) in next_circle stage s.to_ r) else s.to_ in
      { from_ = s.to_; to_; stage; clock = 0 }

let damage_now (s : storm) : number =
  let _, _, _, d = stages.(min s.stage (Array.length stages - 1)) in
  d

let outside (c : circle) (x : number) (z : number) : bool = Float.hypot (x -. c.cx) (z -. c.cz) > c.r

(*****************************************************************************)
(* The fighters *)
(*****************************************************************************)

type gun = { name : string; color : color; damage : number; every : int; spread : number }

(* by rarity, grey, green, blue, gold: a chest gives the next one *)
let guns =
  [| { name = "pistol"; color = rgb 170 170 170; damage = 20.; every = 18; spread = 2.5 };
     { name = "rifle"; color = rgb 80 190 80; damage = 23.; every = 10; spread = 2.5 };
     { name = "rare rifle"; color = rgb 70 140 240; damage = 26.; every = 10; spread = 1.8 };
     { name = "legendary SCAR"; color = rgb 240 180 50; damage = 32.; every = 9; spread = 1.2 } |]

let range = 120.

type mode = Bus | Sky | Glide | Ground | Dead of int (* frames since *)

type fighter = {
  id : int; (* 0 is the player *)
  x : number; (* its feet *)
  y : number;
  z : number;
  vy : number; (* metres per frame *)
  heading : number;
  pitch : number; (* where it aims: up, positive, in degrees *)
  mode : mode;
  hp : number;
  wood : int;
  gun : int;
  slot : slot;
  reload : int; (* frames before it can fire or build again *)
  kills : int;
  phase : number; (* its stride, for the legs *)
  blocked : bool; (* ran into a wall last frame *)
  hit_from : (xyz * int) option; (* where the last shot at it came from, and when *)
  drop : number * number; (* where it means to land *)
}

let alive (f : fighter) : bool = match f.mode with Dead _ -> false | _ -> true
let head (f : fighter) : xyz = (f.x, f.y +. 1.6, f.z)

(* the direction it aims, from its heading and pitch *)
let aim (heading : number) (pitch : number) : xyz =
  let fx, fz = Camera3d.forward heading in
  let p = pitch *. Float.pi /. 180. in
  (fx *. cos p, sin p, fz *. cos p)

(* The camera over the shoulder: behind the head, a little to the right
 * and above, looking where the fighter aims -- so the crosshair in the
 * middle of the screen is where the player's shots go. Farther in the
 * air. *)
let eye_of (f : fighter) : xyz =
  let d = aim f.heading f.pitch in
  let fx, fz = Camera3d.forward f.heading in
  let back = match f.mode with Bus -> 16. | Sky | Glide -> 8. | _ -> 4. in
  let x, y, z = add (head f) (add (times (-.back) d) (-.fz *. 0.8, 0.5, fx *. 0.8)) in
  (x, Float.max y (ground_at x z +. 0.3), z)

(* What a fighter wants this frame: filled by the player's keys or by a
 * bot's mind, the same record, so the game can't tell them apart
 * ([Bot.mli]). [face], for a bot, is a point to turn towards; the
 * player turns by [turn] and [tilt] instead. *)
type intent = {
  fwd : number;
  side : number;
  turn : number;
  tilt : number;
  face : xyz option;
  jump : bool;
  fire : bool;
  select : slot option;
}

let idle = { fwd = 0.; side = 0.; turn = 0.; tilt = 0.; face = None; jump = false; fire = false; select = None }

(* the bus: across the island, 70 m up, 16 m/s *)
let bus_from = (-30., 70., 40.)
let bus_to = (286., 70., 220.)
let bus_frames = int_of_float (Float.hypot (286. +. 30.) (220. -. 40.) /. 16. *. 60.)

let bus_at (frame : int) : xyz =
  let t = Float.min 1. (fl frame /. fl bus_frames) in
  add bus_from (times t (sub bus_to bus_from))

let bus_heading : number =
  let x0, _, z0 = bus_from and x1, _, z1 = bus_to in
  Float.atan2 (x1 -. x0) (-.(z1 -. z0)) *. 180. /. Float.pi

let players = 16

let names =
  [| "you"; "Jonesy"; "Ramirez"; "Headhoncho"; "Renegade"; "Spitfire"; "Wildcat"; "Raptor"; "Brite"; "Drift"; "Raven"; "Ghoul";
     "Skull"; "Peely"; "Midas"; "Fishstick" |]

let new_fighter (id : int) : fighter =
  let x, y, z = bus_at 0 in
  let drop = drops.(id mod Array.length drops) in
  { id; x; y; z; vy = 0.; heading = bus_heading; pitch = 0.; mode = Bus; hp = 100.; wood = (if id = 0 then 150 else 40); gun = 0;
    slot = Gun; reload = 0; kills = 0; phase = 0.; blocked = false; hit_from = None; drop }

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type chest = { at : number * number; opened : bool }

type game = {
  frame : int;
  fighters : fighter array;
  minds : (senses, intent) Bot.running array;
  pieces : piece list;
  built : shape3d; (* the pieces, drawn once each time they change *)
  falling : (piece * int) list; (* pieces that lost their support, falling *)
  chests : chest list;
  storm : storm;
  tracers : (xyz * xyz * int) list;
  feed : (string * int) list; (* who eliminated whom, and when *)
}

(* what a bot may know (Sense.mli): the nearest enemy, seen only through
 * a clear line; the rest is what any player knows -- its own body, the
 * storm's next circle, where it means to go *)
and senses = {
  me : fighter;
  enemy : xyz Sense.target;
  safe : circle;
  goal : number * number;
  now : int;
}

(*****************************************************************************)
(* Building, and the pieces drawn *)
(*****************************************************************************)

let wood_color (hp : number) : color =
  let t = 0.55 +. (0.45 *. (hp /. full_hp)) in
  let v c = int_of_float (fl c *. t) in
  rgb (v 236) (v 184) (v 120)

(* thin boxes along a quad's four edges: along x, y or z, or a ramp's
 * slanted sides, tilted *)
let frame (color : color) (cs : xyz list) : shape3d list =
  let edge a b =
    let (ax, ay, az), (bx, by, bz) = (a, b) in
    let m = ((ax +. bx) /. 2., (ay +. by) /. 2., (az +. bz) /. 2.) in
    let len = Float.hypot (Float.hypot (bx -. ax) (by -. ay)) (bz -. az) in
    (m, len)
  in
  match cs with
  | [ a; b; c; d ] ->
      List.map
        (fun (u, v) ->
          let (x, y, z), len = edge u v in
          let ux, uy, uz = sub v u in
          if Float.abs uy < 0.01 && Float.abs uz < 0.01 then box color len 0.18 0.18 |> move3d x y z
          else if Float.abs ux < 0.01 && Float.abs uz < 0.01 then box color 0.18 len 0.18 |> move3d x y z
          else if Float.abs ux < 0.01 && Float.abs uy < 0.01 then box color 0.18 0.18 len |> move3d x y z
          else if Float.abs uz < 0.01 then box color len 0.18 0.18 |> rotate3d 0. 0. (Float.atan2 uy ux *. 180. /. Float.pi) |> move3d x y z
          else box color 0.18 0.18 len |> rotate3d (-.Float.atan2 uy uz *. 180. /. Float.pi) 0. 0. |> move3d x y z)
        [ (a, b); (b, c); (c, d); (d, a) ]
  | _ -> []

(* a piece as its quad, darker as it is shot, in a frame of beams, so it
 * reads as a plank panel from anywhere *)
let piece_shape (p : piece) : shape3d =
  let cs = corners p in
  group3d (polygon3d (wood_color p.hp) cs :: frame (rgb 160 110 60) cs)

let draw_pieces (pieces : piece list) : shape3d = cached3d (List.map piece_shape pieces)

(* the pieces changed: what stands, what falls, drawn again *)
let rebuilt (g : game) (pieces : piece list) : game =
  let kept, fell = standing pieces in
  { g with pieces = kept; built = draw_pieces kept; falling = List.map (fun p -> (p, 0)) fell @ g.falling }

(*****************************************************************************)
(* Shooting *)
(*****************************************************************************)

type hit = Nothing | Island | Piece of piece | Body of int

(* what the ray (o, d) hits first, from [t0] on, leaving out [shooter]:
 * the pieces, the fighters, the island *)
let trace (g : game) (shooter : int) (o : xyz) (d : xyz) (t0 : number) : number * hit =
  let best = ref (range, Nothing) in
  let consider t h = if t >= t0 && t < fst !best then best := (t, h) in
  List.iter (fun p -> Option.iter (fun t -> consider t (Piece p)) (ray_quad o d (corners p))) g.pieces;
  Array.iter
    (fun (f : fighter) ->
      if f.id <> shooter && f.mode = Ground then Option.iter (fun t -> consider t (Body f.id)) (ray_body o d (f.x, f.y, f.z)))
    g.fighters;
  (match ray_ground o d (fst !best) with Some t -> consider t Island | None -> ());
  !best

(* a line of sight: nothing between the two points *)
let clear (g : game) (a : int) (b : int) : bool =
  let fa = g.fighters.(a) and fb = g.fighters.(b) in
  let o = head fa and target = add (fb.x, fb.y, fb.z) (0., 1.2, 0.) in
  let v = sub target o in
  let dist = sqrt (dot v v) in
  let d = times (1. /. dist) v in
  match trace g a o d 0. with t, Body j -> j = b || t >= dist | t, _ -> t >= dist

let eliminate (g : game) (killer : int) (victim : int) (how : string) : game =
  let fs = g.fighters in
  fs.(victim) <- { (fs.(victim)) with mode = Dead 0; hp = 0. };
  if killer >= 0 then fs.(killer) <- { (fs.(killer)) with kills = fs.(killer).kills + 1 };
  { g with feed = ((if killer >= 0 then names.(killer) ^ " eliminated " ^ names.(victim) else names.(victim) ^ " " ^ how), g.frame) :: g.feed }

(* [fire g i o d t0]: fighter [i] shoots along (o, d), the gun's spread
 * added: a piece loses what the gun does, and may fall with what it
 * held; a fighter too, and may be out *)
let fire (g : game) (i : int) (o : xyz) (d : xyz) (t0 : number) : game =
  let f = g.fighters.(i) in
  let gun = guns.(f.gun) in
  let s = gun.spread *. Float.pi /. 180. in
  let d = add d (times s (rnd g.frame (i * 3), rnd g.frame ((i * 3) + 1), rnd g.frame ((i * 3) + 2))) in
  let d = times (1. /. sqrt (dot d d)) d in
  let t, hit = trace g i o d t0 in
  let muzzle = add (head f) (times 0.6 (aim f.heading f.pitch)) in
  let g = { g with tracers = (muzzle, add o (times t d), 5) :: g.tracers } in
  match hit with
  | Nothing | Island -> g
  | Piece p ->
      let pieces = List.filter_map (fun q -> if same q p then (if q.hp > gun.damage then Some { q with hp = q.hp -. gun.damage } else None) else Some q) g.pieces in
      rebuilt g pieces
  | Body j ->
      let v = g.fighters.(j) in
      g.fighters.(j) <- { v with hp = v.hp -. gun.damage; hit_from = Some (head f, g.frame) };
      if v.hp -. gun.damage <= 0. then eliminate g i j "" else g

(* [build g i]: fighter [i] builds what its slot would, if it can pay
 * for it and something holds it *)
let build (g : game) (i : int) : game =
  let f = g.fighters.(i) in
  match ghost g.pieces f.slot f.x f.y f.z f.heading with
  | Some (p, true) when f.wood >= cost ->
      g.fighters.(i) <- { f with wood = f.wood - cost };
      rebuilt g (p :: g.pieces)
  | _ -> g

(*****************************************************************************)
(* Moving *)
(*****************************************************************************)

let walk_speed = 6. /. 60.
let gravity = 22. /. 3600.
let jump_speed = 7.5 /. 60.

(* a move by (dx, dz), or as much of it as clears the walls: along a
 * wall it slides instead of stopping dead *)
let slide (pieces : piece list) (f : fighter) (dx : number) (dz : number) : number * number * bool =
  let ok x z = not (walled pieces x z f.y) in
  let inside v = Float.max 2. (Float.min (width -. 2.) v) in
  let x' = inside (f.x +. dx) and z' = inside (f.z +. dz) in
  if ok x' z' then (x', z', false) else if ok x' f.z then (x', f.z, true) else if ok f.x z' then (f.x, z', true) else (f.x, f.z, true)

(* turned towards [face] by at most 6 degrees a frame (the player's
 * hand turns as fast; a bot's no faster) *)
let turn_to (f : fighter) (face : xyz) : number * number =
  let tx, ty, tz = face and hx, hy, hz = head f in
  let wanted = Float.atan2 (tx -. hx) (-.(tz -. hz)) *. 180. /. Float.pi in
  let diff = Float.rem (wanted -. f.heading +. 540.) 360. -. 180. in
  let pitch = Float.atan2 (ty -. hy) (Float.hypot (tx -. hx) (tz -. hz)) *. 180. /. Float.pi in
  (f.heading +. Float.max (-6.) (Float.min 6. diff), f.pitch +. Float.max (-4.) (Float.min 4. (pitch -. f.pitch)))

(* one frame of a fighter wanting [it] *)
let act (g : game) (i : int) (it : intent) : game =
  let f = g.fighters.(i) in
  let heading, pitch =
    match it.face with Some p -> turn_to f p | None -> (f.heading +. it.turn, f.pitch +. it.tilt)
  in
  let f = { f with heading; pitch = Float.max (-70.) (Float.min 70. pitch); reload = max 0 (f.reload - 1) } in
  let f = match it.select with Some s -> { f with slot = s } | None -> f in
  let fx, fz = Camera3d.forward f.heading in
  (* forward and sideways, in the ground's plane *)
  let wish speed = ((fx *. it.fwd) -. (fz *. it.side)) *. speed, ((fz *. it.fwd) +. (fx *. it.side)) *. speed in
  let f =
    match f.mode with
    | Dead n -> { f with mode = Dead (n + 1) }
    | Bus ->
        let x, y, z = bus_at g.frame in
        if it.jump || g.frame >= bus_frames then { f with x; y; z; mode = Sky; vy = 0. } else { f with x; y; z }
    | Sky | Glide ->
        (* the sky dive, 20 m/s down, and the glider, open 30 m over the
         * ground, 6 m/s down: steered either way *)
        let high = f.y -. ground_at f.x f.z in
        let mode = if high < 30. then Glide else Sky in
        let speed, fall = if mode = Glide then (10. /. 60., 6. /. 60.) else (15. /. 60., 20. /. 60.) in
        let dx, dz = wish speed in
        let x, z, _ = slide [] f dx dz in
        let y = f.y -. fall in
        let floor = support g.pieces x z f.y in
        if y <= floor then { f with x; z; y = floor; mode = Ground; vy = 0. } else { f with x; y; z; mode }
    | Ground ->
        let dx, dz = wish walk_speed in
        let x, z, blocked = slide g.pieces f dx dz in
        let floor = support g.pieces x z f.y in
        let on_ground = f.y <= floor +. 0.05 in
        let vy = if it.jump && on_ground then jump_speed else if on_ground then 0. else f.vy -. gravity in
        (* the head against a floor above stops the rise *)
        let vy = if vy > 0. && ceiling g.pieces x z f.y (f.y +. vy) then 0. else vy in
        let y = f.y +. vy in
        (* the ground under it: followed up a ramp and down a slope, when
         * it isn't going up *)
        let y, vy = if y <= floor || (vy <= 0. && y -. floor < 0.3) then (floor, 0.) else (y, vy) in
        let moved = Float.hypot dx dz in
        { f with x; y; z; vy; blocked; phase = f.phase +. (moved *. 4.) }
  in
  g.fighters.(i) <- f;
  if f.mode <> Ground || not it.fire || f.reload > 0 then g
  else if f.slot = Gun then begin
    g.fighters.(i) <- { f with reload = guns.(f.gun).every };
    (* the player's shot goes from the camera through the crosshair,
     * counted from level with the player's head (so a wall behind it is
     * not in the way); a bot's from its head *)
    let d = aim f.heading f.pitch in
    if i = 0 then let eye = eye_of f in fire g i eye d (dot (sub (head f) eye) d) else fire g i (head f) d 0.
  end
  else begin
    g.fighters.(i) <- { f with reload = 8 };
    build g i
  end

(*****************************************************************************)
(* The bots *)
(*****************************************************************************)

(* the nearest other fighter still in, seen if the line to it is clear,
 * remembered for three seconds *)
let senses_of (was : senses option) ((g, i) : game * int) : senses =
  let me = g.fighters.(i) in
  let target = match was with Some s -> s.enemy | None -> Sense.unknown in
  let others =
    List.filter (fun (f : fighter) -> f.id <> i && f.mode = Ground) (Array.to_list g.fighters)
    |> List.map (fun (f : fighter) -> (Float.hypot (f.x -. me.x) (f.z -. me.z), f))
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let enemy =
    match others with
    | (d, f) :: _ when me.mode = Ground && d < 90. ->
        Sense.update ~sight:55. ~hearing:15. ~distance:d ~clear:(clear g i f.id) ~position:(f.x, f.y +. 1.2, f.z) target
    | _ -> Sense.update ~distance:infinity ~clear:false ~position:(0., 0., 0.) target
  in
  let enemy = Sense.forget ~after:180 enemy in
  (* where to go: a chest not yet opened nearby, else somewhere in the
   * storm's next circle, another every ten seconds -- where the others
   * will be *)
  let chest = List.find_opt (fun (c : chest) -> (not c.opened) && Float.hypot (fst c.at -. me.x) (snd c.at -. me.z) < 60.) g.chests in
  let safe = g.storm.to_ in
  let wander = (safe.cx +. (0.5 *. safe.r *. rnd 60 ((g.frame / 600) + (i * 7))), safe.cz +. (0.5 *. safe.r *. rnd 61 ((g.frame / 600) + (i * 7)))) in
  let goal = match (me.mode, chest) with (Bus | Sky | Glide), _ -> me.drop | _, Some c -> c.at | _, None -> wander in
  { me; enemy; safe; goal; now = g.frame }

let decide (s : senses) : intent =
  let me = s.me in
  let towards (x, z) = Some (x, ground_at x z +. 1.6, z) in
  let far_from (x, z) = Float.hypot (x -. me.x) (z -. me.z) in
  match me.mode with
  (* out of the bus once its drop is within a glide *)
  | Bus -> { idle with jump = far_from me.drop < 70. }
  | Sky | Glide -> { idle with fwd = (if far_from s.goal > 3. then 1. else 0.); face = towards s.goal }
  | Dead _ -> idle
  | Ground -> (
      let recent = match me.hit_from with Some (at, t) when s.now - t < 40 -> Some at | _ -> None in
      match (s.enemy.position, recent) with
      (* shot, with wood: a wall first, towards the shot *)
      | _, Some at when me.wood >= cost && me.slot <> Wall && s.now mod 90 < 45 -> { idle with face = Some at; select = Some Wall; fire = true }
      | Some (ex, ey, ez), _ ->
          let d = Float.hypot (ex -. me.x) (ez -. me.z) in
          let err = Bot.aim_error ~spread:12. ~settle:60. ~seen_for:s.enemy.seen_for ~seed:me.id () in
          (* the error as a point beside the target, sideways *)
          let off = d *. Float.tan (err *. Float.pi /. 180.) in
          let fx, fz = ((ex -. me.x) /. Float.max d 1., (ez -. me.z) /. Float.max d 1.) in
          let strafe = if (s.enemy.seen_for / 70) mod 2 = 0 then 1. else -1. in
          { idle with
            face = Some (ex -. (fz *. off), ey, ez +. (fx *. off));
            fwd = (if not s.enemy.visible then 1. else if d > 35. then 1. else if d < 10. then -1. else 0.);
            side = (if s.enemy.visible then strafe else 0.);
            fire = s.enemy.visible;
            select = Some Gun }
      | None, _ ->
          (* out of the storm's next circle: to its middle; else to the
           * goal *)
          let goal = if Float.hypot (me.x -. s.safe.cx) (me.z -. s.safe.cz) > s.safe.r *. 0.8 then (s.safe.cx, s.safe.cz) else s.goal in
          { idle with fwd = (if far_from goal > 2. then 1. else 0.); face = towards goal; select = Some Gun })

(* every frame, on its body as it is now: a jump when a wall stops it *)
let reflex ((g, i) : game * int) (it : intent) : intent =
  if g.fighters.(i).blocked && it.fwd <> 0. then { it with jump = true } else it

let mind : (game * int, senses, intent) Bot.t = Bot.make ~delay:12 ~rate:4 ~reflex ~sense:senses_of ~decide ()

(*****************************************************************************)
(* A frame of the match *)
(*****************************************************************************)

let new_game () : game =
  { frame = 0; fighters = Array.init players new_fighter; minds = Array.init players (fun _ -> Bot.start idle); pieces = [];
    built = draw_pieces []; falling = []; chests = List.map (fun at -> { at; opened = false }) chest_spots; storm = new_storm ();
    tracers = []; feed = [] }

let left (g : game) : int = Array.fold_left (fun n f -> if alive f then n + 1 else n) 0 g.fighters

(* a fighter on the ground next to a chest opens it: the next gun, and
 * wood *)
let open_chests (g : game) : game =
  let chests =
    List.map
      (fun (c : chest) ->
        if c.opened then c
        else
          match Array.find_opt (fun (f : fighter) -> f.mode = Ground && Float.hypot (f.x -. fst c.at) (f.z -. snd c.at) < 2.) g.fighters with
          | None -> c
          | Some f ->
              g.fighters.(f.id) <- { f with gun = min (Array.length guns - 1) (f.gun + 1); wood = f.wood + 50 };
              { c with opened = true })
      g.chests
  in
  { g with chests }

(* once a second, the storm hurts whoever is outside it *)
let storm_damage (g : game) : game =
  if g.frame mod 60 <> 0 then g
  else
    let c = circle_now g.storm in
    Array.fold_left
      (fun g (f : fighter) ->
        if f.mode = Ground && outside c f.x f.z then begin
          let hp = f.hp -. damage_now g.storm in
          g.fighters.(f.id) <- { f with hp };
          if hp <= 0. then eliminate g (-1) f.id "fell to the storm" else g
        end
        else g)
      g g.fighters

let step (g : game) (player : intent) : game =
  let g = { g with frame = g.frame + 1; fighters = Array.copy g.fighters; minds = Array.copy g.minds } in
  let g =
    Array.fold_left
      (fun g (f : fighter) ->
        if f.id = 0 then act g 0 player
        else
          let it, mind' = Bot.step mind (g, f.id) g.minds.(f.id) in
          g.minds.(f.id) <- mind';
          act g f.id (if alive f then it else idle))
      g g.fighters
  in
  let g = storm_damage (open_chests g) in
  { g with
    storm = step_storm g.storm;
    falling = List.filter_map (fun (p, n) -> if n < 45 then Some (p, n + 1) else None) g.falling;
    tracers = List.filter_map (fun (a, b, n) -> if n > 0 then Some (a, b, n - 1) else None) g.tracers;
    feed = List.filter (fun (_, t) -> g.frame - t < 300) g.feed }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type scene = Title | Playing of game | Over of { place : int; kills : int }
type model = scene Scene2d.t

let key (k : keyboard) (name : string) : bool = Set_.mem name k.keys

let player_intent (computer : computer) (s : model) : intent =
  let k = computer.keyboard and m = computer.mouse in
  let axis a b = (if a then 1. else 0.) -. if b then 1. else 0. in
  let select =
    if key k "1" then Some Gun else if key k "2" then Some Wall else if key k "3" then Some Floor_slot else if key k "4" then Some Ramp_slot
    else None
  in
  { fwd = axis k.kw k.ks;
    side = axis k.kd k.ka;
    turn = (3. *. axis k.kright k.kleft) +. (0.15 *. m.mdx);
    tilt = (2. *. axis k.kup k.kdown) -. (0.15 *. m.mdy);
    face = None;
    jump = Scene2d.pressed (fun k -> k.kspace) s;
    fire = key k "x" || m.mdown;
    select }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = step g (player_intent computer s) in
      let me = g.fighters.(0) in
      (match me.mode with
      | Dead n when n > 90 -> Scene2d.go (Over { place = left g + 1; kills = me.kills }) s
      | Dead _ -> { s with scene = Playing g }
      | _ when left g = 1 -> Scene2d.go (Over { place = 1; kills = me.kills }) s
      | _ -> { s with scene = Playing g })
  | Over _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* Drawing the match *)
(*****************************************************************************)

let skins = [| rgb 60 110 220; rgb 220 90 60; rgb 230 200 70; rgb 90 180 90; rgb 170 90 200; rgb 70 190 200; rgb 230 130 180; rgb 140 140 140 |]

(* A fighter, blocky: legs swung by its stride, a body, a head, and the
 * gun in its hands, of its rarity's colour; lying down when out. *)
let fighter_shape (f : fighter) : shape3d =
  let skin = if alive f then skins.(f.id mod Array.length skins) else rgb 110 110 110 in
  let swing = 35. *. sin f.phase in
  let leg side a = box (rgb 50 50 70) 0.25 0.9 0.28 |> move_y3d (-0.45) |> rotate3d a 0. 0. |> move3d (side *. 0.15) 0.9 0. in
  let body =
    group3d
      [ leg 1. swing;
        leg (-1.) (-.swing);
        box skin 0.6 0.7 0.35 |> move_y3d 1.25;
        box (rgb 230 190 150) 0.35 0.35 0.35 |> move_y3d 1.8;
        box guns.(f.gun).color 0.12 0.12 0.7 |> move3d 0.2 1.3 (-0.4) ]
  in
  let glider = if f.mode = Glide then [ box skin 2.6 0.08 1.2 |> move_y3d 3.2; box (rgb 60 60 60) 0.05 1.4 0.05 |> move_y3d 2.4 ] else [] in
  let body =
    match f.mode with
    | Dead _ -> body |> rotate3d (-90.) 0. 0. |> move_y3d 0.2
    (* diving head first, flat on the air *)
    | Sky -> body |> move_y3d (-1.) |> rotate3d (-75.) 0. 0. |> move_y3d 1.
    | _ -> group3d (body :: glider)
  in
  body |> rotate3d 0. (-.f.heading) 0. |> move3d f.x f.y f.z

let bus_shape ((x, y, z) : xyz) : shape3d =
  group3d
    [ box (rgb 60 120 220) 3. 2.5 9.; box (rgb 200 230 255) 3.05 0.8 8. |> move_y3d 0.4; sphere (rgb 230 80 80) 3. |> move_y3d 5. ]
  |> rotate3d 0. (-.bus_heading) 0. |> move3d x y z

let chest_shape (c : chest) : shape3d =
  let x, z = c.at in
  let y = ground_at x z in
  if c.opened then box (rgb 110 80 30) 1. 0.5 0.7 |> move3d x (y +. 0.25) z
  else group3d [ box (rgb 220 170 40) 1. 0.7 0.7 |> move3d x (y +. 0.35) z; box (rgb 250 230 120) 0.3 0.25 0.1 |> move3d x (y +. 0.5) (z -. 0.36) ]

(* The storm's edge. Fortnite's is a translucent purple wall; no
 * backend here blends 3D faces, so it is a curtain instead: 72 purple
 * beams round the circle, the island seen between them. *)
let storm_wall (c : circle) : shape3d =
  let n = 72 in
  if c.r < 0.5 then group3d []
  else
    group3d
      (List.init n (fun k ->
           let a = 2. *. Float.pi *. fl k /. fl n and w = Float.min 0.8 (c.r *. 0.02) in
           let px = c.cx +. (c.r *. cos a) and pz = c.cz +. (c.r *. sin a) in
           (* a flat beam facing the circle's middle, seen from both sides *)
           let tx = -.sin a *. w and tz = cos a *. w in
           polygon3d (rgb 170 80 240) [ (px -. tx, -5., pz -. tz); (px +. tx, -5., pz +. tz); (px +. tx, 60., pz +. tz); (px -. tx, 60., pz -. tz) ]))

(* a shot's trail, a thin ribbon lying flat, so that it catches the
 * light as the ground does *)
let tracer_shape ((a, b, _) : xyz * xyz * int) : shape3d =
  let dx, _, dz = sub b a in
  let n = Float.max 1e-6 (Float.hypot dx dz) in
  let w = (-.dz /. n *. 0.06, 0., dx /. n *. 0.06) in
  polygon3d (rgb 255 240 150) [ sub a w; sub b w; add b w; add a w ]

(* the ghost of the piece the slot would build: blue if it can be, red
 * if not *)
let ghost_shape (g : game) (f : fighter) : shape3d list =
  match ghost g.pieces f.slot f.x f.y f.z f.heading with
  | Some (p, ok) when f.mode = Ground ->
      let ok = ok && f.wood >= cost in
      frame (if ok then rgb 90 160 255 else rgb 240 80 80) (corners p)
  | _ -> []

let camera_of (f : fighter) : camera =
  let eye = eye_of f in
  camera ~eye ~target:(add eye (times 20. (aim f.heading f.pitch))) ~fov:70. ~near:0.1 ~far:2000. ()

let world_shapes (g : game) : shape3d list =
  [ island; g.built ]
  @ List.map (fun (p, n) -> piece_shape p |> move_y3d (-.gravity *. 0.5 *. fl (n * n))) g.falling
  @ List.map chest_shape g.chests
  @ List.filter_map
      (fun (f : fighter) -> match f.mode with Bus -> None | Dead n when n > 240 -> None | _ -> Some (fighter_shape f))
      (Array.to_list g.fighters)
  @ (if g.frame < bus_frames then [ bus_shape (bus_at g.frame) ] else [])
  @ List.map tracer_shape g.tracers
  @ [ storm_wall (circle_now g.storm) ]

(*****************************************************************************)
(* The HUD *)
(*****************************************************************************)

let text (c : color) (size : number) (s : string) : shape = words c s |> scale size

(* the island from above, a pixel per cell, north up *)
let minimap_pixel = 4.

let minimap : shape =
  let n = map.size in
  let palette =
    List.concat
      (List.mapi
         (fun k kind -> List.init 3 (fun light -> (Char.chr (65 + (k * 3) + light), color_of kind light)))
         Heightmap.kinds)
  in
  let code i j =
    let rec index k = function [] -> 0 | k' :: ks -> if k' = Heightmap.kind map i j then k else index (k + 1) ks in
    Char.chr (65 + (index 0 Heightmap.kinds * 3) + Heightmap.light map i j)
  in
  Sprite.pixels minimap_pixel palette (List.init n (fun j -> String.init n (fun i -> code i j)))

let slot_name = function Gun -> "gun" | Wall -> "wall" | Floor_slot -> "floor" | Ramp_slot -> "ramp"

let clock (frames : int) : string = Printf.sprintf "%d:%02d" (frames / 3600) (frames / 60 mod 60)

let panel (screen : screen) (g : game) : shape list =
  let me = g.fighters.(0) in
  let side = fl map.size *. minimap_pixel in
  let ox = screen.right -. 20. -. (side /. 2.) and oy = screen.top -. 20. -. (side /. 2.) in
  let on_map x z = (ox +. ((x /. width) -. 0.5) *. side, oy -. ((z /. width) -. 0.5) *. side) in
  let ring color (c : circle) =
    List.init 40 (fun k ->
        let a = 2. *. Float.pi *. fl k /. 40. in
        let x, y = on_map (c.cx +. (c.r *. cos a)) (c.cz +. (c.r *. sin a)) in
        circle color 1.5 |> move x y)
  in
  let px, py = on_map me.x me.z in
  let storm = g.storm in
  let stage_line =
    if last_stage storm then "the storm has closed"
    else
      let wait, shrink, _, _ = stages.(storm.stage) in
      if storm.clock < wait * 60 then "storm shrinks in " ^ clock ((wait * 60) - storm.clock)
      else "storm shrinking " ^ clock (((wait + shrink) * 60) - storm.clock)
  in
  let slots =
    List.mapi
      (fun n sl ->
        let x = screen.right -. 330. +. (fl n *. 80.) and y = screen.bottom +. 50. in
        let on = me.slot = sl in
        let color = if sl = Gun then guns.(me.gun).color else rgb 196 142 84 in
        group
          [ rectangle (if on then white else rgb 40 40 50) 70. 56. |> fade 0.7;
            rectangle color 60. 10. |> move_y (-18.);
            text (if on then black else white) 1.3 (Printf.sprintf "%d %s" (n + 1) (slot_name sl)) |> move_y 6. ]
        |> move x y)
      [ Gun; Wall; Floor_slot; Ramp_slot ]
  in
  [ rectangle black (side +. 8.) (side +. 8.) |> move ox oy; minimap |> move ox oy ]
  @ ring (rgb 190 110 250) (circle_now storm)
  @ ring white storm.to_
  @ [ circle yellow 3.5 |> move px py;
      (* the crosshair *)
      rectangle white 16. 2.;
      rectangle white 2. 16.;
      (* health and wood *)
      rectangle (rgb 40 40 40) 304. 24. |> move (screen.left +. 180.) (screen.bottom +. 50.);
      rectangle (rgb 90 210 90) (3. *. Float.max 0. me.hp) 18. |> move (screen.left +. 30. +. (1.5 *. Float.max 0. me.hp)) (screen.bottom +. 50.);
      text white 1.6 (Printf.sprintf "%d" (int_of_float (Float.max 0. me.hp))) |> move (screen.left +. 180.) (screen.bottom +. 50.);
      text (rgb 230 190 130) 1.6 (Printf.sprintf "wood %d    %s" me.wood guns.(me.gun).name) |> move (screen.left +. 180.) (screen.bottom +. 85.);
      text white 1.8 (Printf.sprintf "%d left    %d eliminations    %s" (left g) me.kills stage_line) |> move_y (screen.top -. 30.) ]
  @ List.mapi (fun n (line, _) -> text white 1.3 line |> move (screen.left +. 170.) (screen.top -. 80. -. (fl n *. 24.))) (List.filteri (fun n _ -> n < 5) g.feed)
  @ (match me.mode with
    | Bus -> [ text yellow 2.5 "SPACE: JUMP FROM THE BUS" |> move_y 120. ]
    | Ground when outside (circle_now storm) me.x me.z -> [ rectangle (rgb 150 60 220) screen.width screen.height |> fade 0.15; text (rgb 230 180 255) 2.2 "IN THE STORM" |> move_y 120. ]
    | _ -> [])
  @ slots

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let gold = rgb 240 190 60

(* the sea, just under the island's cells at sea level (not at the same
 * height, or the z-buffer mixes them), the sky and the horizon, all
 * following the camera *)
let surroundings (cam : camera) : shape3d list =
  Camera3d.floor ~color:(color_of Sea 1) ~ground:(map.sea -. 0.2) cam
  :: Camera3d.sky ~sky:(rgb 130 190 250) ~horizon:(color_of Sea 1) ~ground:(map.sea -. 0.2) cam
let help = "wsad walk   arrows or mouse aim   space jump   1 gun 2 wall 3 floor 4 ramp   x or click: fire, build"

(* the title's still: the island from out at sea, the bus coming *)
let titled (lines : shape list) (s : model) (blink : shape list) : camera * shape3d list =
  let g = new_game () in
  let cam = camera ~eye:(40., 60., 300.) ~target:(128., 10., 128.) ~fov:60. ~far:2000. () in
  ( cam,
    [ island; bus_shape (95., 42., 215.) ] @ surroundings cam @ List.map chest_shape g.chests
    @ List.map hud (lines @ Scene2d.blink 1. s blink) )

let view (computer : computer) (s : model) : camera * shape3d list =
  match s.scene with
  | Title ->
      titled
        [ text gold 6. "TINY FORTNITE" |> move_y 300.;
          text white 2.2 "sixteen on an island, a storm closing in, and walls in a second" |> move_y 240.;
          text white 1.6 help |> move_y 205. ]
        s [ text yellow 3. "PRESS SPACE" |> move_y 150. ]
  | Playing g ->
      let me = g.fighters.(0) in
      let cam = camera_of me in
      ( cam,
        world_shapes g @ ghost_shape g me
        @ surroundings cam
        @ List.map hud (panel computer.screen g) )
  | Over { place; kills } ->
      titled
        [ (if place = 1 then text gold 6. "#1 VICTORY ROYALE" else text (rgb 230 110 100) 6. (Printf.sprintf "#%d" place)) |> move_y 250.;
          text white 3. (Printf.sprintf "%d eliminations" kills) |> move_y 180. ]
        s [ text white 2.5 "PRESS SPACE" |> move_y 120. ]

let app = game3d view update (Scene2d.start Title)

(* flat shading, each triangle of the island lit by its slope; the back
 * faces drawn too, for the sky (seen from below, see Camera3d.sky) and
 * for the pieces, one quad seen from both sides *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } ~capture_mouse:true app
