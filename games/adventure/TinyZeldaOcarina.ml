(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of The Legend of Zelda: Ocarina of Time (Nintendo EAD,
 * 1998): a small Hyrule Field under a sky that turns from day to night,
 * a temple at its far end, and inside, a barred room and a Stalfos -- a
 * skeleton with a sword and a shield -- that must fall before the bars
 * lift. Arrows to move, space to swing the sword, and *hold z* to
 * target. Three hearts. At night, Stalchildren climb out of the field.
 *
 * Ocarina brought two things to 3D, and this file is about both.
 *
 * The first is the fight. TinyZelda's Link fought on a grid, seen from
 * above: facing the enemy was a matter of pressing towards it. In 3D
 * that stops being true. Seen from behind, an enemy a few steps away is
 * somewhere in a circle round you, the camera is somewhere else, and
 * the stick pushes you relative to the camera. Circle a swordsman to
 * reach his back and you are no longer facing him when you get there;
 * swing and you hit the air. Ocarina answered with one button, Z on the
 * Nintendo 64 controller's trigger, and every 3D action game since has
 * it:
 *
 *  - Z-targeting, the lock-on ([lock_on], [move_link]): while it is
 *    held, Link faces the enemy whatever he does, and the stick is
 *    re-read in the enemy's frame -- forward is towards it, back is
 *    away, and left and right go *round* it. A step sideways is taken
 *    along the tangent and then pulled back onto the circle, so a key
 *    held is an orbit at arm's length: the polar coordinates of the
 *    enemy, (distance, angle), become the controls.
 *
 *              Stalfos
 *                 X          free: the stick is the camera's, Link
 *              .     .       faces where he runs
 *            .    ^    .
 *           .     |     .    locked: up and down change the
 *           . <-- L --> .    distance, left and right the angle, and
 *            .         .     Link faces X all the way round
 *              .     .
 *                cam
 *
 *  - The camera turned with the controls ([wanted_camera]): locked, it
 *    sits over Link's shoulder on the line from the enemy through him
 *    and looks at the point between the two, so both are always on the
 *    screen and "round it" is always left and right on the screen too.
 *    Free, it trails behind wherever Link has been heading. Held with
 *    nothing to target, z swings it behind Link and he stops turning:
 *    the button meant for fights ended up fixing the camera too, and it
 *    is the same line. The black bars at the top and bottom say you are
 *    locked, as the film's did.
 *
 *  - An enemy that blocks ([shielded]): the Stalfos holds its shield
 *    towards you, and a slash from the front clangs off it. It opens up
 *    twice: after its own chop (the wind-up is long, and the chop leaves
 *    it bent over, shield down -- Frame_data's startup and recovery,
 *    written as a telegraph and an opening), and from behind. It turns
 *    only 1.4 degrees a frame, and Link circling it at arm's length
 *    goes round at more than 2 ([foe_turn]), so the lock-on is what
 *    gets you to its back: the fight is those two numbers.
 *
 * The acquire range is shorter than the keep range ([lock_range],
 * [keep_range]): locked on, you can back away further than you could
 * have locked from, and a fight at the edge of the range does not
 * flicker on and off -- TinyAloneInTheDark's doorway again.
 *
 * The second is the world. Hyrule Field was the first 3D place big
 * enough to *cross*: open land in every direction, the castle, the
 * ranch, the villages and the dungeons at its edges. It felt like one
 * world, and it was not:
 *
 *  - The field is a hub, and the places are other scenes, joined to it
 *    by loading zones ([cross]). A Nintendo 64 had 4 MB, room for one
 *    place at a time, so walking into the temple's door throws the
 *    field away and loads the room, behind a moment of black. Here the
 *    two [area]s do not even share coordinates: the room is not
 *    anywhere on the field, and Link's (x, z) means one or the other.
 *    A door is a pair of places, one in each, and nothing between.
 *
 *              Hyrule Field                 the temple's room
 *        +-----------------------+         +---------------+
 *        |  hills all round      |         |   bars, the   |
 *        |        [door]---------+--load-->+-- way on      |
 *        |   trees    ^          |         |    Stalfos    |
 *        |          Link         |  <-load-+---- door      |
 *        +-----------------------+         +---------------+
 *
 *  - Time that changes what is there ([step_night]). The day turns to
 *    night in a minute, and at night the field is another place:
 *    Stalchildren climb out of the ground round you, and sink back at
 *    dawn. The clock runs only on the field -- in Ocarina it stopped
 *    in the towns and the dungeons, so a place you are in keeps its
 *    time of day -- and the dungeon's own light never changes.
 *
 *  - The light baked in ([field_mesh]): the field is drawn three
 *    times at startup, by day, at dusk and by night, each cached, and
 *    the sky picks one. Nothing is lit per frame: the Nintendo 64's
 *    games mostly painted their light on as colours on the vertices,
 *    and it cost them nothing while the camera moved.
 *
 *  - The edge of the world, hidden: hills all round, too steep to
 *    climb ([walkable]). Ocarina hid the far end of its field in fog as
 *    well; there is none here (Playground3d has none), and the hills do
 *    it alone.
 *
 * Uses: the heightmap kit (gamekits/heightmap/, TinyComanche's: the
 * field's bumps made up by [Heightmap.generate], the ground under a
 * point by [Heightmap.height], the shading by the slope), the brawler
 * kit's Skeleton (Link, the Stalfos and the Stalchildren; the swords
 * and the shield held in their hands, which the arm's joints carry)
 * and Frame_data (the slash's frames and the chop's), Camera3d
 * ([forward], [follow], [orbit], [sky]), Scene2d. Not Hitbox: a
 * sword's reach in 3D is a slice of a circle in front of you, a
 * distance and an angle, the same polar coordinates as the lock-on's.
 * Not Physics3d: Link walks on the ground's height, and does not fall.
 *
 * Exercises: fog, the far end of the field fading into the sky's
 * colour (a triangle's colour mixed by its distance, every frame: it
 * can no longer be cached); Epona, and the field made big enough to
 * need her; a second place off the field (Kakariko), whose door needs
 * nothing but a pair of positions; Link's own shield; the side-hop and
 * the back-flip, which Ocarina gave the jump button while locked; z
 * pressed again to switch between the Stalchildren; a camera that
 * stays out of the hills (here it only keeps above the ground); Link's
 * left hand -- Ocarina's Link held his sword in it.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* Geometry: headings as Camera3d's (0 towards -z, 90 towards +x) *)
(*****************************************************************************)

type body = { x : number; z : number; heading : number; step : number (* the walk's phase *) }

let heading_to (dx : number) (dz : number) : number = atan2 dx (-.dz) *. 180. /. Float.pi

(* [a] less [b], the short way round: -180 to 180 *)
let diff (a : number) (b : number) : number =
  let d = Float.rem (a -. b) 360. in
  if d > 180. then d -. 360. else if d < -180. then d +. 360. else d

let dist (a : body) (b : body) : number = Float.hypot (b.x -. a.x) (b.z -. a.z)

(* how far, in degrees, [b] is off the way [a] faces: a reach, a shield
 * and a cone of vision are all this, under a limit *)
let off_facing (a : body) (b : body) : number = Float.abs (diff (heading_to (b.x -. a.x) (b.z -. a.z)) a.heading)

(*****************************************************************************)
(* Hyrule Field *)
(*****************************************************************************)

(* A heightmap 64 cells across, a cell 1.6 wide, the field's middle at
 * (0, 0): gentle bumps from the kit's generator, and hills rising all
 * round past [rim] from the middle -- the edge of the world. The temple
 * is set into the hills at the north (towards -z). *)
let cells = 64
let cell = 1.6
let rim = 30.
let temple_z = -33.

let field : Heightmap.t =
  let bumps = Heightmap.generate ~seed:5 ~size:cells ~top:1. ~roughness:0.5 in
  let half_grid = float_of_int (cells / 2) in
  let height k =
    let i = k mod cells and j = k / cells in
    let d = Float.hypot ((float_of_int i -. half_grid) *. cell) ((float_of_int j -. half_grid) *. cell) in
    let hills = if d > rim then 16. *. (((d -. rim) /. 18.) ** 2.) else 0. in
    (2.5 *. Heightmap.cell bumps i j) +. hills
  in
  { size = cells; cells = Array.init (cells * cells) height; top = 20.; sea = -1. }

let field_height (x : number) (z : number) : number =
  let half_grid = float_of_int (cells / 2) in
  Heightmap.height field ((x /. cell) +. half_grid) ((z /. cell) +. half_grid)

(* the trees: scattered by the kit's hash, away from the way to the
 * temple, and from the start and the camera behind it *)
let trees : (number * number) list =
  List.init 60 (fun k ->
      let a = 180. *. Heightmap.random 11 k 0 and r = 18. +. (10. *. Heightmap.random 11 k 1) in
      let fx, fz = Camera3d.forward a in
      (r *. fx, r *. fz))
  |> List.filter (fun (x, z) -> not (Float.abs x < 6. && z < 14.))
  |> List.filteri (fun i _ -> i < 14)

(* where Link can walk: below the hills, not in a tree, not into the
 * temple's stone *)
let walkable (x : number) (z : number) : bool =
  field_height x z < 3.4
  && List.for_all (fun (tx, tz) -> Float.hypot (x -. tx) (z -. tz) > 0.8) trees
  && not (Float.abs x < 4.5 && z < temple_z)

(* The field drawn once per light: by day, at dusk, by night. A quad
 * every two cells, grass or the hills' rock, lighter or darker by its
 * slope towards the sun (the kit's [light]); the trees and the temple
 * with it, in the same light. *)
type sky = Day | Dusk | Night

let tint (sky : sky) ((r, g, b) : int * int * int) : color =
  let f (c : int) (k : number) (plus : number) = int_of_float ((float_of_int c *. k) +. plus) in
  match sky with
  | Day -> rgb r g b
  | Dusk -> rgb (f r 0.8 30.) (f g 0.6 0.) (f b 0.55 10.)
  | Night -> rgb (f r 0.28 0.) (f g 0.32 0.) (f b 0.45 24.)

let field_mesh (sky : sky) : shape3d =
  let n = cells / 2 in
  let half_grid = float_of_int (cells / 2) in
  let p i j = ((float_of_int i -. half_grid) *. cell, Heightmap.cell field i j, (float_of_int j -. half_grid) *. cell) in
  let quad k =
    let i = k mod n * 2 and j = k / n * 2 in
    let kind = if Heightmap.cell field (i + 1) (j + 1) > 3.8 then Heightmap.Rock else Heightmap.Grass in
    let color = tint sky (Heightmap.color kind (Heightmap.light field (i + 1) (j + 1))) in
    (* counterclockwise seen from above, +z being towards the viewer:
     * the faces turned up, lit as the top of the ground *)
    [ polygon3d color [ p i j; p (i + 2) (j + 2); p (i + 2) j ]; polygon3d color [ p i j; p i (j + 2); p (i + 2) (j + 2) ] ]
  in
  let tree (x, z) =
    let h = field_height x z in
    [ box (tint sky (100, 70, 44)) 0.5 2.2 0.5 |> move3d x (h +. 1.1) z;
      box (tint sky (40, 110, 50)) 2.4 2. 2.4 |> move3d x (h +. 3.) z ]
  in
  let temple =
    let h = field_height 0. temple_z in
    let stone = tint sky (150, 140, 120) in
    [ box stone 9. 8. 8. |> move3d 0. (h +. 3.) (temple_z -. 4.);
      box (tint sky (120, 110, 96)) 1. 4.4 1. |> move3d (-1.8) (h +. 1.7) (temple_z +. 0.3);
      box (tint sky (120, 110, 96)) 1. 4.4 1. |> move3d 1.8 (h +. 1.7) (temple_z +. 0.3);
      box (rgb 12 10 14) 2.6 3.2 0.3 |> move3d 0. (h +. 1.1) (temple_z +. 0.05) ]
  in
  cached3d (List.concat (List.init (n * n) quad) @ List.concat_map tree trees @ temple)

let day_field = field_mesh Day
let dusk_field = field_mesh Dusk
let night_field = field_mesh Night

(*****************************************************************************)
(* The temple's room *)
(*****************************************************************************)

(* the room is 22 x 22, its own place (see the header): a door in the
 * middle of the south wall, back to the field, and the barred one in
 * the north wall, the way on; a sword reaches 2.7 *)
let half = 11.
let wall_height = 5.
let door_half = 1.5
let door_height = 3.
let corridor = 5.

let room : shape3d =
  let floor = ref [] in
  for i = 0 to 10 do
    for j = 0 to 10 do
      let x = -.half +. 1. +. (2. *. float_of_int i) and z = -.half +. 1. +. (2. *. float_of_int j) in
      let c = if (i + j) mod 2 = 0 then rgb 92 88 96 else rgb 78 74 84 in
      floor := (box c 2. 0.2 2. |> move3d x (-0.1) z) :: !floor
    done
  done;
  let stone = rgb 116 104 92 and dark = rgb 40 36 44 in
  let t = 1. in
  let across = (2. *. half) +. (2. *. t) in
  let up = wall_height /. 2. in
  (* a wall with a door: two pieces either side, and the lintel *)
  let piece = half -. door_half +. t in
  let wall_with_door (z : number) =
    [ box stone piece wall_height t |> move3d (-.door_half -. (piece /. 2.)) up z;
      box stone piece wall_height t |> move3d (door_half +. (piece /. 2.)) up z;
      box stone (2. *. door_half) (wall_height -. door_height) t |> move3d 0. ((wall_height +. door_height) /. 2.) z ]
  in
  (* a passage beyond a door: its floor, its sides, and its far end *)
  let passage (z0 : number) (sign : number) (length : number) (far_end : color) =
    let mid = z0 +. (sign *. length /. 2.) in
    [ box (rgb 60 56 64) (2. *. door_half) 0.2 length |> move3d 0. (-0.1) mid;
      box dark 0.5 door_height length |> move3d (-.door_half -. 0.25) (door_height /. 2.) mid;
      box dark 0.5 door_height length |> move3d (door_half +. 0.25) (door_height /. 2.) mid;
      box far_end (2. *. door_half) door_height 0.5 |> move3d 0. (door_height /. 2.) (z0 +. (sign *. (length +. 0.25))) ]
  in
  cached3d
    (!floor
    @ [ box stone t wall_height across |> move3d (-.half -. (t /. 2.)) up 0.;
        box stone t wall_height across |> move3d (half +. (t /. 2.)) up 0.;
        box dark across 0.5 across |> move_y3d (wall_height +. 0.25) ]
    @ wall_with_door (-.half -. (t /. 2.))
    @ wall_with_door (half +. (t /. 2.))
    (* north, the way on, dark; south, the field's daylight *)
    @ passage (-.half) (-1.) corridor dark
    @ passage half 1. 2. (rgb 200 220 250))

(* a torch in each corner, its flame flickering, so not cached *)
let torches (time : time) : shape3d list =
  List.concat_map
    (fun (i, (x, z)) ->
      let flicker = wave 0.82 1.18 (0.3 +. (0.07 *. float_of_int i)) time in
      [ box (rgb 70 60 50) 0.3 1.4 0.3 |> move3d x 0.7 z;
        box (rgb 50 44 40) 0.6 0.2 0.6 |> move3d x 1.45 z;
        box (rgb 255 150 40) (0.35 *. flicker) (0.5 *. flicker) (0.35 *. flicker) |> move3d x 1.8 z ])
    (List.mapi (fun i p -> (i, p)) [ (-9.8, -9.8); (9.8, -9.8); (-9.8, 9.8); (9.8, 9.8) ])

(* the bars in the north doorway, which slide up into the lintel when
 * [up] reaches 1 *)
let bars (up : number) : shape3d list =
  List.map
    (fun x -> box (rgb 60 60 70) 0.14 door_height 0.14 |> move3d x ((door_height /. 2.) +. (up *. door_height)) (-.half -. 0.5))
    [ -1.2; -0.6; 0.; 0.6; 1.2 ]

(* where a body can stand in the room: the room, the south doorway (the
 * way out), and, once the bars are up, the north corridor *)
let in_room_free (opened : bool) (x : number) (z : number) : bool =
  let m = half -. 0.5 and d = door_half -. 0.5 in
  (Float.abs x <= m && Float.abs z <= m)
  || (Float.abs x <= d && z >= m && z <= half +. 2.)
  || (opened && Float.abs x <= d && z >= -.half -. corridor +. 0.5 && z <= m)

(*****************************************************************************)
(* The places *)
(*****************************************************************************)

type area = Field | Dungeon

let ground (area : area) (x : number) (z : number) : number =
  match area with Field -> field_height x z | Dungeon -> 0.

let free (area : area) (opened : bool) (x : number) (z : number) : bool =
  match area with Field -> walkable x z | Dungeon -> in_room_free opened x z

(* one axis at a time, so that running into a wall slides along it *)
let walk_by (free : number -> number -> bool) (b : body) (dx : number) (dz : number) : body =
  let b = if free (b.x +. dx) b.z then { b with x = b.x +. dx } else b in
  if free b.x (b.z +. dz) then { b with z = b.z +. dz } else b

(*****************************************************************************)
(* The moves *)
(*****************************************************************************)

(* The hitbox is not used: a reach is a distance and an angle
 * ([strike]), not a box. The chop's startup is its telegraph -- long
 * enough to see coming and step back from -- and its recovery the
 * opening it leaves. *)
let no_box : Hitbox.box = { x = 0.; y = 0.; w = 0.; h = 0. }

let slash : Frame_data.move =
  { startup = 4; active = 6; recovery = 10; damage = 1; hitstun = 30; blockstun = 0; hitbox = no_box }

let chop : Frame_data.move =
  { startup = 40; active = 6; recovery = 50; damage = 1; hitstun = 60; blockstun = 0; hitbox = no_box }

let reach = 2.7

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type kind = Stalfos | Stalchild

type foe_state =
  | Rising of int (* frames since it began climbing out of the ground *)
  | Stalk
  | Chop of int (* the chop's frame, from 1 *)
  | Reel of int (* frames left staggering from a hit *)
  | Dying of int (* frames since it began sinking *)
  | Gone

type foe = { id : int; kind : kind; body : body; state : foe_state; hp : int }

type play = {
  area : area;
  link : body;
  hearts : int;
  hurt : int; (* frames of flinching left, and of not being hurt again *)
  swing : int; (* 0, or the slash's frame, from 1 *)
  landed : bool; (* this slash has struck already *)
  stalfos : foe; (* the room's: once down, it stays down *)
  stalchildren : foe list; (* the field's, at night *)
  next_id : int;
  target : int option; (* the foe locked on, by its id *)
  clock : number; (* the time of day, 0 to 1: morning, then night from 0.5 *)
  loading : int; (* frames of black left, crossing a loading zone *)
  cam : camera;
  cam_heading : number; (* the free camera's: the way it looks along the ground *)
  letterbox : number; (* 0 to 1, the bars sliding in *)
  navi : number * number * number; (* the fairy, who flies to what you target *)
  clang : int; (* frames of spark left, on the shield *)
  opened : number; (* the room's bars, 0 down to 1 up *)
  frames : int;
}

type scene = Title | Playing of play | Over of play * bool (* through the door *)
type model = scene Scene2d.t

let new_play () : play =
  let y = field_height 0. 4. in
  { area = Field;
    link = { x = 0.; z = 4.; heading = 0.; step = 0. };
    hearts = 3;
    hurt = 0;
    swing = 0;
    landed = false;
    stalfos = { id = 0; kind = Stalfos; body = { x = 0.; z = -3.; heading = 180.; step = 0. }; state = Stalk; hp = 4 };
    stalchildren = [];
    next_id = 1;
    target = None;
    clock = 0.08;
    loading = 0;
    cam = camera ~eye:(0., y +. 3.4, 10.) ~target:(0., y +. 1., 4.) ();
    cam_heading = 0.;
    letterbox = 0.;
    navi = (0.6, y +. 2., 4.3);
    clang = 0;
    opened = 0.;
    frames = 0 }

let initial_model : model = Scene2d.start Title

(* what can be targeted and hit: not a Stalchild still climbing out,
 * nor anything sinking *)
let alive (f : foe) : bool = match f.state with Rising _ | Dying _ | Gone -> false | _ -> true
let door_open (p : play) : bool = p.opened >= 1.
let free_in (p : play) : number -> number -> bool = free p.area (door_open p)

(* the foes of the place Link is in *)
let foes (p : play) : foe list = match p.area with Dungeon -> [ p.stalfos ] | Field -> p.stalchildren

let set_foe (p : play) (f : foe) : play =
  match f.kind with
  | Stalfos -> { p with stalfos = f }
  | Stalchild -> { p with stalchildren = List.map (fun g -> if g.id = f.id then f else g) p.stalchildren }

let find_foe (p : play) (id : int option) : foe option =
  match id with None -> None | Some id -> List.find_opt (fun f -> f.id = id) (foes p)

(*****************************************************************************)
(* Link *)
(*****************************************************************************)

let speed = 0.11

(* locking on takes the nearest enemy within [lock_range]; once
 * locked, it holds out to [keep_range] (see the header) *)
let lock_range = 10.
let keep_range = 16.

let lock_on (z_held : bool) (p : play) : int option =
  if not z_held then None
  else
    match find_foe p p.target with
    | Some f when alive f && dist p.link f.body < keep_range -> Some f.id
    | _ ->
        let near = List.filter (fun f -> alive f && dist p.link f.body < lock_range) (foes p) in
        let nearest =
          List.fold_left
            (fun best f -> match best with Some b when dist p.link b.body <= dist p.link f.body -> best | _ -> Some f)
            None near
        in
        Option.map (fun f -> f.id) nearest

type stick = { fwd : number; side : number }

let stick_of (keys : keyboard) : stick =
  let b (k : bool) : number = if k then 1. else 0. in
  { fwd = b keys.kup -. b keys.kdown; side = b keys.kright -. b keys.kleft }

(* The three ways the stick is read, [target] being what Link is
 * locked on. [dx, dz] is a step in the world. *)
let move_link (z_held : bool) (target : body option) (s : stick) (p : play) : body =
  let b = p.link in
  let moving = s.fwd <> 0. || s.side <> 0. in
  let stepped (b : body) : body = { b with step = (if moving then b.step +. speed else 0.) } in
  (* the step along [h]'s forward and right *)
  let along (h : number) : number * number =
    let fx, fz = Camera3d.forward h and rx, rz = Camera3d.forward (h +. 90.) in
    ((s.fwd *. fx) +. (s.side *. rx), (s.fwd *. fz) +. (s.side *. rz))
  in
  let free = free_in p in
  match target with
  | _ when p.swing > 0 -> { b with step = 0. } (* planted while swinging *)
  | Some f ->
      (* The lock-on: the stick in the enemy's frame. The step is taken
       * along the tangent and then pulled back onto the circle, whose
       * radius only up and down change. *)
      let dx, dz = along (heading_to (f.x -. b.x) (f.z -. b.z)) in
      let x = b.x +. (speed *. dx) and z = b.z +. (speed *. dz) in
      let r = Float.max 1.4 (dist b f -. (speed *. s.fwd)) in
      let ox = x -. f.x and oz = z -. f.z in
      let l = Float.hypot ox oz in
      let x = f.x +. (ox *. r /. l) and z = f.z +. (oz *. r /. l) in
      let b = walk_by free b (x -. b.x) (z -. b.z) in
      stepped { b with heading = heading_to (f.x -. b.x) (f.z -. b.z) }
  | None when z_held ->
      (* nothing to target: Link keeps facing ahead, and strafes *)
      let dx, dz = along b.heading in
      stepped (walk_by free b (speed *. dx) (speed *. dz))
  | None ->
      (* free: the stick is the camera's, and Link turns to run where it
       * points *)
      let dx, dz = along p.cam_heading in
      if not moving then stepped b
      else
        let l = Float.hypot dx dz in
        let b = walk_by free b (speed *. dx /. l) (speed *. dz /. l) in
        stepped { b with heading = heading_to dx dz }

(* bumped back, away from [from] *)
let knocked (free : number -> number -> bool) (b : body) (from : body) (by : number) : body =
  let a = heading_to (b.x -. from.x) (b.z -. from.z) in
  let fx, fz = Camera3d.forward a in
  walk_by free b (by *. fx) (by *. fz)

(*****************************************************************************)
(* The sword and the shield *)
(*****************************************************************************)

(* the Stalfos' shield is up while it stalks and while it winds up,
 * and covers what is in front of it; a Stalchild has none *)
let shielded (p : play) (f : foe) : bool =
  f.kind = Stalfos
  && (match f.state with Stalk -> true | Chop n -> Frame_data.phase chop n = Startup | _ -> false)
  && off_facing f.body p.link < 70.

type strike = Miss | Blocked | Hit

(* what the slash does to [f] on one of its active frames *)
let strike (p : play) (f : foe) : strike =
  if (not (alive f)) || dist p.link f.body > reach || off_facing p.link f.body > 50. then Miss
  else if shielded p f then Blocked
  else Hit

let step_slash (swing_pressed : bool) (p : play) : play =
  let swing =
    if p.swing > 0 then if p.swing >= Frame_data.length slash then 0 else p.swing + 1
    else if swing_pressed then 1
    else 0
  in
  let p = { p with swing; landed = (if swing = 1 then false else p.landed) } in
  if swing = 0 || p.landed || Frame_data.phase slash swing <> Active then p
  else
    (* the first foe the blade meets *)
    match List.find_opt (fun f -> strike p f <> Miss) (foes p) with
    | None -> p
    | Some f -> (
        match strike p f with
        | Blocked -> { p with landed = true; clang = 10; link = knocked (free_in p) p.link f.body 0.5 }
        | _ ->
            let hp = f.hp - slash.damage in
            let state = if hp <= 0 then Dying 0 else Reel slash.hitstun in
            set_foe { p with landed = true } { f with hp; state; body = knocked (free_in p) f.body p.link 1.0 })

(*****************************************************************************)
(* The foes *)
(*****************************************************************************)

(* degrees a frame: Link circling at arm's length (2.5) goes round at
 * speed / 2.5 radians, 2.5 degrees, so a Stalfos turning at 1.4 falls
 * behind, and its back comes round *)
let foe_turn = 1.4

let turn_towards (rate : number) (from : number) (towards : number) : number =
  from +. Float.max (-.rate) (Float.min rate (diff towards from))

let hurt_link (p : play) (by : body) (knock : number) : play =
  { p with hearts = p.hearts - 1; hurt = chop.hitstun; link = knocked (free_in p) p.link by knock }

(* [walk f heading pace]: [f] turned to [heading], [pace] forward *)
let walk (p : play) (f : foe) (heading : number) (pace : number) : foe =
  let fx, fz = Camera3d.forward heading in
  let b = walk_by (free p.area false) { f.body with heading } (pace *. fx) (pace *. fz) in
  { f with body = { b with step = (if pace > 0. then b.step +. pace else 0.) } }

let step_foe (p : play) (f : foe) : play =
  let l = p.link and b = f.body in
  let towards = heading_to (l.x -. b.x) (l.z -. b.z) in
  match (f.kind, f.state) with
  | _, Reel n -> set_foe p { f with state = (if n <= 1 then Stalk else Reel (n - 1)) }
  | Stalfos, Dying n -> set_foe p { f with state = (if n >= 90 then Gone else Dying (n + 1)) }
  | Stalchild, Dying n -> set_foe p { f with state = (if n >= 60 then Gone else Dying (n + 1)) }
  | _, Gone -> p
  | _, Rising n -> set_foe p { f with state = (if n >= 60 then Stalk else Rising (n + 1)) }
  | Stalfos, Stalk ->
      let f = walk p f (turn_towards foe_turn b.heading towards) (if dist b l > 2.3 then 0.035 else 0.) in
      (* close, and facing Link: the chop *)
      let chops = dist f.body l < 3. && Float.abs (diff towards f.body.heading) < 25. in
      set_foe p { f with state = (if chops then Chop 1 else Stalk) }
  | _, Chop n when n > Frame_data.length chop -> set_foe p { f with state = Stalk }
  | _, Chop n ->
      let phase = Frame_data.phase chop n in
      (* it follows Link while it winds up, slowly: step aside late and
       * the blade comes down beside you *)
      let f = if phase = Startup then walk p f (turn_towards 0.6 b.heading towards) 0. else f in
      let p = set_foe p { f with state = Chop (n + 1) } in
      if phase = Active && p.hurt = 0 && dist f.body l < 3.2 && off_facing f.body l < 50. then hurt_link p f.body 1.5
      else p
  | Stalchild, Stalk ->
      (* it only shambles at you, and hurts by touching *)
      let f = walk p f (turn_towards 4. b.heading towards) (if dist b l > 0.9 then 0.04 else 0.) in
      let p = set_foe p f in
      if p.hurt = 0 && dist f.body l < 1.1 then hurt_link p f.body 1.2 else p

(*****************************************************************************)
(* The day and the night *)
(*****************************************************************************)

(* a day is a minute; night is its second half, less the dawn *)
let day_frames = 3600.
let is_night (clock : number) : bool = clock >= 0.5 && clock < 0.95

let sky_of (clock : number) : sky =
  if clock < 0.42 then Day else if clock < 0.5 || clock >= 0.95 then Dusk else Night

(* On the field only: the clock runs; at night, a Stalchild climbs out
 * of the ground near Link every two seconds, three at most; at dawn,
 * they sink back. The ones gone are forgotten. *)
let step_night (p : play) : play =
  if p.area <> Field then p
  else
    let clock = Float.rem (p.clock +. (1. /. day_frames)) 1. in
    let night = is_night clock in
    let children =
      List.filter_map
        (fun f ->
          match f.state with
          | Gone -> None
          | Dying _ -> Some f
          | _ when not night -> Some { f with state = Dying 0 }
          | _ -> Some f)
        p.stalchildren
    in
    let awake = List.length (List.filter (fun f -> match f.state with Dying _ -> false | _ -> true) children) in
    let p = { p with clock; stalchildren = children } in
    if night && awake < 3 && p.frames mod 120 = 0 then
      let fx, fz = Camera3d.forward (180. *. Heightmap.random 7 p.next_id 0) in
      let x = p.link.x +. (7. *. fx) and z = p.link.z +. (7. *. fz) in
      if not (walkable x z) then p
      else
        let body = { x; z; heading = heading_to (p.link.x -. x) (p.link.z -. z); step = 0. } in
        { p with
          stalchildren = { id = p.next_id; kind = Stalchild; body; state = Rising 0; hp = 1 } :: p.stalchildren;
          next_id = p.next_id + 1 }
    else p

(*****************************************************************************)
(* The camera *)
(*****************************************************************************)

(* The eye kept where it can see. In the room, inside the walls. On the
 * field, above the ground and out of the trees: walking back from
 * Link towards where the eye wants to be, it stops short of the first
 * tree in the way, as a ray cast back from the player would -- else a
 * canopy behind Link fills the screen. *)
let keep_eye (area : area) (from : number * number * number) ((x, y, z) : number * number * number) :
    number * number * number =
  match area with
  | Dungeon ->
      let m = half -. 0.3 in
      (Float.max (-.m) (Float.min m x), y, Float.max (-.m) (Float.min m z))
  | Field ->
      let fx, fy, fz = from in
      let at t = (fx +. (t *. (x -. fx)), fy +. (t *. (y -. fy)), fz +. (t *. (z -. fz))) in
      let in_tree (px, _, pz) = List.exists (fun (tx, tz) -> Float.hypot (px -. tx) (pz -. tz) < 1.8) trees in
      let rec last_clear k = if k = 12 || in_tree (at (float_of_int (k + 1) /. 12.)) then k else last_clear (k + 1) in
      let x, y, z = at (float_of_int (last_clear 0) /. 12.) in
      (x, Float.max y (field_height x z +. 1.), z)

(* where the camera wants to be, and the free camera's heading *)
let wanted_camera (z_held : bool) (moving : bool) (p : play) : camera * number =
  let l = p.link in
  let ly = ground p.area l.x l.z in
  match find_foe p p.target with
  | Some f ->
      (* over Link's shoulder, on the line from the enemy through him,
       * looking at the point between the two *)
      let f = f.body in
      let h = heading_to (f.x -. l.x) (f.z -. l.z) in
      let fx, fz = Camera3d.forward h and rx, rz = Camera3d.forward (h +. 90.) in
      let eye = keep_eye p.area (l.x, ly +. 1.5, l.z) (l.x -. (5. *. fx) +. (1.2 *. rx), ly +. 3., l.z -. (5. *. fz) +. (1.2 *. rz)) in
      let fy = ground p.area f.x f.z in
      (camera ~eye ~target:((l.x +. f.x) /. 2., ((ly +. fy) /. 2.) +. 1.1, (l.z +. f.z) /. 2.) (), h)
  | None ->
      (* trailing: z swings it behind Link at once, running swings it
       * slowly after him *)
      let h =
        if z_held then l.heading
        else if moving then p.cam_heading +. (0.03 *. diff l.heading p.cam_heading)
        else p.cam_heading
      in
      let fx, fz = Camera3d.forward h in
      let eye = keep_eye p.area (l.x, ly +. 1.5, l.z) (l.x -. (6. *. fx), ly +. 3.4, l.z -. (6. *. fz)) in
      (camera ~eye ~target:(l.x, ly +. 1., l.z) (), h)

(*****************************************************************************)
(* The loading zones *)
(*****************************************************************************)

(* Link put down in the other place: the target dropped, the camera cut
 * behind him (never swung across two places), and a moment of black *)
let arrive (area : area) (link : body) (p : play) : play =
  let p = { p with area; link; target = None; swing = 0; letterbox = 0.; cam_heading = link.heading } in
  let cam, _ = wanted_camera false false p in
  let x, _, z = cam.target in
  { p with cam; loading = 16; navi = (x +. 0.6, ground area link.x link.z +. 2., z) }

(* The two doors: the temple's on the field, the room's south one.
 * Each is a place in its own area and the place it leads to in the
 * other: that pair is all a loading zone is. *)
let cross (p : play) : play =
  match p.area with
  | Field when p.link.z < temple_z +. 0.7 && Float.abs p.link.x < 1.3 ->
      arrive Dungeon { x = 0.; z = half -. 0.8; heading = 0.; step = 0. } p
  | Dungeon when p.link.z > half +. 0.8 -> arrive Field { x = 0.; z = temple_z +. 2.5; heading = 180.; step = 0. } p
  | _ -> p

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let step_play (keys : keyboard) (swing_pressed : bool) (p : play) : play =
  if p.loading > 0 then { p with loading = p.loading - 1 }
  else
    let z_held = Set_.mem "z" keys.keys in
    let s = stick_of keys in
    let target = lock_on z_held p in
    let tbody = Option.map (fun f -> f.body) (find_foe p target) in
    let p = { p with target; link = move_link z_held tbody s p } in
    let p = step_slash swing_pressed p in
    let p = List.fold_left step_foe p (foes p) in
    let p = step_night p in
    (* two bodies do not overlap: Link is pushed out of the foes *)
    let p =
      List.fold_left
        (fun p f ->
          let d = dist p.link f.body in
          if alive f && d < 1.2 then { p with link = knocked (free_in p) p.link f.body (1.2 -. d) } else p)
        p (foes p)
    in
    let wanted, cam_heading = wanted_camera z_held (s.fwd <> 0. || s.side <> 0.) p in
    let nx, ny, nz = p.navi in
    let tx, ty, tz =
      match find_foe p p.target with
      | Some f -> (f.body.x, ground p.area f.body.x f.body.z +. 2.9, f.body.z)
      | None ->
          ( p.link.x +. 0.6,
            ground p.area p.link.x p.link.z +. 2. +. (0.15 *. sin (float_of_int p.frames /. 12.)),
            p.link.z +. 0.3 )
    in
    cross
      { p with
        cam = Camera3d.follow 0.12 wanted p.cam;
        cam_heading;
        letterbox = p.letterbox +. (((if p.target <> None then 1. else 0.) -. p.letterbox) *. 0.2);
        navi = (nx +. ((tx -. nx) *. 0.15), ny +. ((ty -. ny) *. 0.15), nz +. ((tz -. nz) *. 0.15));
        hurt = max 0 (p.hurt - 1);
        clang = max 0 (p.clang - 1);
        opened = (if p.stalfos.hp > 0 then p.opened else Float.min 1. (p.opened +. 0.01));
        frames = p.frames + 1 }

let escaped (p : play) : bool = p.area = Dungeon && p.link.z < -.half -. 1.5

let update (computer : computer) (m : model) : model =
  let m = Scene2d.update computer m in
  let space = Scene2d.pressed (fun k -> k.kspace) m in
  match m.scene with
  | Title -> if space then Scene2d.go (Playing (new_play ())) m else m
  | Playing p ->
      let p = step_play computer.keyboard space p in
      if p.hearts <= 0 then Scene2d.go (Over (p, false)) m
      else if escaped p then Scene2d.go (Over (p, true)) m
      else { m with scene = Playing p }
  | Over (p, won) -> if space then Scene2d.go Title m else { m with scene = Over (p, won) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

(* Link: the sword held low in front, pointing ahead *)
let guard : Skeleton.pose =
  { Skeleton.stand with front_arm = Skeleton.limb ~bend:40. 30.; back_arm = Skeleton.limb ~yaw:(-10.) ~bend:60. 20. }

let walking (b : body) (still : Skeleton.pose) : Skeleton.pose =
  if b.step = 0. then still
  else
    let t = sin (b.step *. 4.) in
    { still with
      front_leg = Skeleton.limb ~bend:(14. +. (8. *. t)) (24. *. t);
      back_leg = Skeleton.limb ~bend:(14. -. (8. *. t)) (-24. *. t) }

(* The slash, a horizontal sweep from the right to the left: the arm
 * straight out and swung by its yaw, the torso turning with it. The
 * sweep is the active frames, because the keyframes are the move's
 * own frame data (TinyVirtuaFighter's rule). *)
let link_pose (p : play) : Skeleton.pose =
  if p.swing = 0 then walking p.link guard
  else
    let wind = { guard with turn = -25.; front_arm = Skeleton.limb ~yaw:(-85.) ~bend:10. 85. } in
    let through = { guard with turn = 30.; front_arm = Skeleton.limb ~yaw:60. ~bend:10. 85. } in
    Skeleton.at
      [ (0, guard); (slash.startup, wind); (slash.startup + slash.active, through); (Frame_data.length slash, guard) ]
      p.swing

(* The Stalfos: the shield held out in front of it, the sword ready *)
let stalk : Skeleton.pose =
  { Skeleton.stand with
    lean = 10.;
    front_arm = Skeleton.limb ~bend:50. 30.;
    back_arm = Skeleton.limb ~yaw:(-15.) ~bend:10. 75.;
    front_leg = Skeleton.limb ~bend:20. 10.;
    back_leg = Skeleton.limb ~bend:20. (-10.) }

(* the chop: the sword raised over its head all through the startup,
 * brought down on the active frames, and the recovery spent bent over
 * with the shield dropped -- the opening *)
let raised = { stalk with lean = -10.; front_arm = Skeleton.limb ~bend:20. 170. }
let chopped = { stalk with lean = 25.; front_arm = Skeleton.limb ~bend:5. 75.; back_arm = Skeleton.limb 10. }
let opened_up = { chopped with lean = 30.; front_arm = Skeleton.limb ~bend:5. 40.; back_arm = Skeleton.limb ~yaw:10. 5. }

(* a Stalchild: arms out, hunched, TinyAloneInTheDark's creature *)
let shamble : Skeleton.pose =
  { Skeleton.stand with lean = 16.; front_arm = Skeleton.limb ~yaw:6. 80.; back_arm = Skeleton.limb ~yaw:(-6.) 74. }

let foe_pose (f : foe) : Skeleton.pose =
  match (f.kind, f.state) with
  | Stalchild, _ -> walking f.body shamble
  | _, Stalk -> walking f.body stalk
  | _, Chop n ->
      let s = chop.startup and a = chop.active in
      Skeleton.at
        [ (0, stalk); (s, raised); (s + 2, chopped); (s + a, chopped); (s + a + 10, opened_up);
          (Frame_data.length chop - 8, opened_up); (Frame_data.length chop, stalk) ]
        n
  | _, Reel _ -> { stalk with lean = -25.; back_arm = Skeleton.limb 20. }
  | _ -> { opened_up with lean = 40. }

(* the things held, built from the fist down the forearm (Skeleton's
 * [draw]): a blade and its hilt, and a shield across the forearm *)
let sword (length : number) : shape3d =
  group3d
    [ box (rgb 210 215 230) 0.07 length 0.025 |> move_y3d (-.(length /. 2.) -. 0.1);
      box (rgb 120 90 40) 0.22 0.05 0.07 |> move_y3d (-0.08) ]

let shield : shape3d = box (rgb 130 84 50) 0.75 0.08 0.9 |> move_y3d (-0.05)

(* a dark patch on the room's floor under a figure, to show where it
 * stands (the field's slopes would cut through it) *)
let shadow (b : body) (size : number) : shape3d = box (rgb 58 55 64) size 0.02 size |> move3d b.x 0.01 b.z

let view_foe (p : play) (f : foe) : shape3d list =
  let y = ground p.area f.body.x f.body.z in
  match (f.kind, f.state) with
  | _, Gone -> []
  | Stalfos, state ->
      (* falling apart: it sinks through the floor *)
      let sink = match state with Dying n -> -2.2 *. float_of_int n /. 90. | _ -> 0. in
      [ Skeleton.draw ~front_hand:(sword 1.2) ~back_hand:shield ~body:(rgb 226 220 200) ~back:(rgb 170 165 150)
          ~skin:(rgb 240 236 220) 2.1 f.body.heading (foe_pose f)
        |> move3d f.body.x (y +. sink) f.body.z;
        shadow f.body 0.75 ]
  | Stalchild, state ->
      (* out of the ground, and back into it *)
      let sink =
        match state with
        | Rising n -> -1.4 *. (1. -. (float_of_int n /. 60.))
        | Dying n -> -1.4 *. float_of_int n /. 60.
        | _ -> 0.
      in
      [ Skeleton.draw ~body:(rgb 200 190 170) ~back:(rgb 150 142 128) ~skin:(rgb 230 222 205) 1.3 f.body.heading
          (foe_pose f)
        |> move3d f.body.x (y +. sink) f.body.z ]

(* the sun by day and the moon by night, across the southern sky *)
let sky_shapes (p : play) (cam : camera) : shape3d list =
  let sky, horizon =
    match sky_of p.clock with
    | Day -> (rgb 120 170 230, rgb 200 222 240)
    | Dusk -> (rgb 90 70 120, rgb 240 150 90)
    | Night -> (rgb 10 14 40, rgb 30 36 70)
  in
  let a = Float.rem (p.clock *. 2.) 1. *. Float.pi in
  let light = if is_night p.clock then sphere (rgb 230 230 250) 6. else sphere (rgb 255 230 120) 8. in
  (light |> move3d (-160. *. cos a) (10. +. (120. *. sin a)) 200.) :: Camera3d.sky ~sky ~horizon ~ground:(-1.) cam

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let p = match m.scene with Title -> new_play () | Playing p | Over (p, _) -> p in
  let cam =
    match m.scene with
    | Title -> Camera3d.orbit ~distance:22. ~height:12. ~look:1. (spin 40. computer.time) (0., 2., -8.)
    | _ -> p.cam
  in
  let ly = ground p.area p.link.x p.link.z in
  let link =
    Skeleton.draw ~front_hand:(sword 0.9) ~body:(rgb 40 140 60) ~back:(rgb 30 100 44) ~skin:(rgb 240 200 160) 1.6
      p.link.heading (link_pose p)
    |> move3d p.link.x ly p.link.z
    |> fun s -> if p.hurt > 0 && p.hurt mod 10 < 5 then fade3d 0.4 s else s
  in
  let spark =
    match find_foe p p.target with
    | Some f when p.clang > 0 ->
        let fx, fz = Camera3d.forward f.body.heading in
        [ sphere (rgb 255 240 120) (0.05 *. float_of_int p.clang) |> move3d (f.body.x +. (0.7 *. fx)) 1.3 (f.body.z +. (0.7 *. fz)) ]
    | _ -> []
  in
  let navi =
    let x, y, z = p.navi in
    [ sphere (if p.target <> None then rgb 255 230 80 else rgb 170 220 255) 0.13 |> move3d x y z ]
  in
  let place =
    match p.area with
    | Dungeon -> (room :: shadow p.link 0.55 :: torches computer.time) @ bars p.opened
    | Field ->
        (match sky_of p.clock with Day -> day_field | Dusk -> dusk_field | Night -> night_field) :: sky_shapes p cam
  in
  let world = (link :: place) @ List.concat_map (view_foe p) (foes p) @ spark @ navi in
  let letterbox =
    if p.letterbox < 0.01 then []
    else
      let h = 70. *. p.letterbox in
      [ rectangle black screen.width h |> move_y (screen.top -. (h /. 2.));
        rectangle black screen.width h |> move_y (screen.bottom +. (h /. 2.)) ]
  in
  let targetable = List.exists (fun f -> alive f && dist p.link f.body < lock_range) (foes p) in
  let hud =
    match m.scene with
    | Title ->
        [ rectangle black screen.width 120. |> move_y 300.;
          rectangle black screen.width 260. |> move_y (-290.);
          text (rgb 230 200 90) 6. "TINY ZELDA OCARINA" |> move_y 300.;
          text white 2.5 "arrows: move   space: sword   hold z: target" |> move_y (-230.);
          text white 2.5 "cross the field to the temple, and get through it" |> move_y (-275.) ]
        @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-360.) ]
    | Playing _ when p.loading > 0 -> [ rectangle black screen.width screen.height ]
    | Playing _ ->
        letterbox
        @ List.init 3 (fun i ->
            circle (if i < p.hearts then rgb 220 40 50 else rgb 70 60 60) 14.
            |> move (screen.left +. 40. +. (36. *. float_of_int i)) (screen.top -. 40.))
        @
        if targetable && p.target = None then [ text white 2.5 "hold z to target" |> move_y (screen.bottom +. 40.) ]
        else if p.area = Dungeon && door_open p && p.frames mod 60 < 40 then
          [ text (rgb 230 200 90) 3. "THE WAY IS OPEN" |> move_y (screen.bottom +. 40.) ]
        else []
    | Over (_, won) ->
        [ rectangle black 1000. 220. |> move_y 180.;
          text (if won then rgb 230 200 90 else rgb 200 60 50) 6. (if won then "ONWARD" else "GAME OVER") |> move_y 200. ]
        @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 110. ]
  in
  (cam, world @ List.map Playground3d.hud hud)

let app = game3d view update initial_model

(* flat shading; the back faces drawn too, for the field's sky (seen
 * from below, see Camera3d.sky) *)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat; backface_culling = false } app
