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
 * 1998): one room of a dungeon, its door barred, and a Stalfos -- a
 * skeleton with a sword and a shield -- that must fall before the bars
 * lift. Arrows to move, space to swing the sword, and *hold z* to
 * target. Three hearts.
 *
 * TinyZelda's Link fought on a grid, seen from above: facing the enemy
 * was a matter of pressing towards it. In 3D that stops being true.
 * Seen from behind, an enemy a few steps away is somewhere in a
 * circle round you, the camera is somewhere else, and the stick pushes
 * you relative to the camera. Circle a swordsman to reach his back
 * and you are no longer facing him when you get there; swing and you
 * hit the air. The 3D action games before 1998 mostly gave up and
 * shot. Ocarina of Time answered with one button, Z on the Nintendo 64
 * controller's trigger, and every 3D action game since has it:
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
 *    the button that was meant for fights ended up fixing the camera
 *    too, and it is the same line. The black bars at the top and
 *    bottom say you are locked, as the film's did.
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
 * Uses: the brawler kit's Skeleton (Link and the Stalfos, their swords
 * and the shield held in their hands, which the arm's joints carry)
 * and Frame_data (the slash's frames and the chop's), Camera3d
 * ([forward], [follow]), Scene2d. Not Hitbox: a sword's reach in 3D is
 * a slice of a circle in front of you, a distance and an angle, which
 * are the same polar coordinates as the lock-on's. Not Physics3d.
 *
 * Exercises: Link's own shield (hold a key, and the Stalfos' chop
 * clangs off it too); the side-hop and the back-flip, which Ocarina
 * gave the jump button while locked; several enemies, and z pressed
 * again to switch between them (the nearest to the camera's line
 * first); a camera that sees through walls, or stays out of them
 * (here it is only kept inside the room, [in_room]); Link's
 * left hand -- Ocarina's Link held his sword in it.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The room *)
(*****************************************************************************)

(* the room is 22 x 22, its door in the middle of the north wall
 * (towards -z); a sword reaches 2.7 *)
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
  (* the north wall is two pieces either side of the door, and the
   * lintel over it *)
  let piece = half -. door_half +. t in
  let north = -.half -. (t /. 2.) in
  cached3d
    (!floor
    @ [ box stone across wall_height t |> move3d 0. up (half +. (t /. 2.));
        box stone t wall_height across |> move3d (-.half -. (t /. 2.)) up 0.;
        box stone t wall_height across |> move3d (half +. (t /. 2.)) up 0.;
        box stone piece wall_height t |> move3d (-.door_half -. (piece /. 2.)) up north;
        box stone piece wall_height t |> move3d (door_half +. (piece /. 2.)) up north;
        box stone (2. *. door_half) (wall_height -. door_height) t
        |> move3d 0. ((wall_height +. door_height) /. 2.) north;
        box dark across 0.5 across |> move_y3d (wall_height +. 0.25);
        (* the corridor beyond the door, where the way on is *)
        box (rgb 60 56 64) (2. *. door_half) 0.2 corridor |> move3d 0. (-0.1) (-.half -. (corridor /. 2.));
        box dark 0.5 door_height corridor |> move3d (-.door_half -. 0.25) (door_height /. 2.) (-.half -. (corridor /. 2.));
        box dark 0.5 door_height corridor |> move3d (door_half +. 0.25) (door_height /. 2.) (-.half -. (corridor /. 2.));
        box dark (2. *. door_half) door_height 0.5 |> move3d 0. (door_height /. 2.) (-.half -. corridor -. 0.25) ])

(* a torch in each corner, its flame flickering, so not cached *)
let torches (time : time) : shape3d list =
  List.concat_map
    (fun (i, (x, z)) ->
      let flicker = wave 0.82 1.18 (0.3 +. (0.07 *. float_of_int i)) time in
      [ box (rgb 70 60 50) 0.3 1.4 0.3 |> move3d x 0.7 z;
        box (rgb 50 44 40) 0.6 0.2 0.6 |> move3d x 1.45 z;
        box (rgb 255 150 40) (0.35 *. flicker) (0.5 *. flicker) (0.35 *. flicker) |> move3d x 1.8 z ])
    (List.mapi (fun i p -> (i, p)) [ (-9.8, -9.8); (9.8, -9.8); (-9.8, 9.8); (9.8, 9.8) ])

(* the bars in the doorway, which slide up into the lintel when [up]
 * reaches 1 *)
let bars (up : number) : shape3d list =
  List.map
    (fun x -> box (rgb 60 60 70) 0.14 door_height 0.14 |> move3d x ((door_height /. 2.) +. (up *. door_height)) (-.half -. 0.5))
    [ -1.2; -0.6; 0.; 0.6; 1.2 ]

(* where a body can stand: in the room, or, once the bars are up, in
 * the doorway and down the corridor *)
let free (opened : bool) (x : number) (z : number) : bool =
  let m = half -. 0.5 in
  (Float.abs x <= m && Float.abs z <= m)
  || (opened && Float.abs x <= door_half -. 0.5 && z >= -.half -. corridor +. 0.5 && z <= m)

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

(* one axis at a time, so that running into a wall slides along it *)
let walk_by (opened : bool) (b : body) (dx : number) (dz : number) : body =
  let b = if free opened (b.x +. dx) b.z then { b with x = b.x +. dx } else b in
  if free opened b.x (b.z +. dz) then { b with z = b.z +. dz } else b

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

type foe_state =
  | Stalk
  | Chop of int (* the chop's frame, from 1 *)
  | Reel of int (* frames left staggering from a hit *)
  | Dying of int (* frames since the last hit landed *)
  | Gone

type play = {
  link : body;
  hearts : int;
  hurt : int; (* frames of flinching left, and of not being hurt again *)
  swing : int; (* 0, or the slash's frame, from 1 *)
  landed : bool; (* this slash has struck already *)
  foe : body;
  foe_state : foe_state;
  foe_hp : int;
  locked : bool;
  cam : camera;
  cam_heading : number; (* the free camera's: the way it looks along the ground *)
  letterbox : number; (* 0 to 1, the bars sliding in *)
  navi : number * number * number; (* the fairy, who flies to what you target *)
  clang : int; (* frames of spark left, on the shield *)
  opened : number; (* the door's bars, 0 down to 1 up *)
  frames : int;
}

type scene = Title | Playing of play | Over of play * bool (* through the door *)
type model = scene Scene2d.t

let new_play () : play =
  { link = { x = 0.; z = 6.; heading = 0.; step = 0. };
    hearts = 3;
    hurt = 0;
    swing = 0;
    landed = false;
    foe = { x = 0.; z = -3.; heading = 180.; step = 0. };
    foe_state = Stalk;
    foe_hp = 4;
    locked = false;
    cam = camera ~eye:(0., 3.4, 10.5) ~target:(0., 1., 6.) ();
    cam_heading = 0.;
    letterbox = 0.;
    navi = (0.6, 2., 6.3);
    clang = 0;
    opened = 0.;
    frames = 0 }

let initial_model : model = Scene2d.start Title

let alive (p : play) : bool = match p.foe_state with Dying _ | Gone -> false | _ -> true
let door_open (p : play) : bool = p.opened >= 1.

(*****************************************************************************)
(* Link *)
(*****************************************************************************)

let speed = 0.11

(* locking on takes the enemy within [lock_range]; once locked, it
 * holds out to [keep_range] (see the header) *)
let lock_range = 10.
let keep_range = 16.

let lock_on (z_held : bool) (p : play) : bool =
  z_held && alive p && dist p.link p.foe < if p.locked then keep_range else lock_range

type stick = { fwd : number; side : number }

let stick_of (keys : keyboard) : stick =
  let b (k : bool) : number = if k then 1. else 0. in
  { fwd = b keys.kup -. b keys.kdown; side = b keys.kright -. b keys.kleft }

(* The three ways the stick is read. [dx, dz] is a step in the world. *)
let move_link (z_held : bool) (locked : bool) (s : stick) (p : play) : body =
  let b = p.link in
  let moving = s.fwd <> 0. || s.side <> 0. in
  let stepped (b : body) : body = { b with step = (if moving then b.step +. speed else 0.) } in
  (* the step along [h]'s forward and right *)
  let along (h : number) : number * number =
    let fx, fz = Camera3d.forward h and rx, rz = Camera3d.forward (h +. 90.) in
    ((s.fwd *. fx) +. (s.side *. rx), (s.fwd *. fz) +. (s.side *. rz))
  in
  let opened = door_open p in
  if p.swing > 0 then { b with step = 0. } (* planted while swinging *)
  else if locked then (
    (* The lock-on: the stick in the enemy's frame. The step is taken
     * along the tangent and then pulled back onto the circle, whose
     * radius only up and down change. *)
    let f = p.foe in
    let dx, dz = along (heading_to (f.x -. b.x) (f.z -. b.z)) in
    let x = b.x +. (speed *. dx) and z = b.z +. (speed *. dz) in
    let r = Float.max 1.4 (dist b f -. (speed *. s.fwd)) in
    let ox = x -. f.x and oz = z -. f.z in
    let l = Float.hypot ox oz in
    let x = f.x +. (ox *. r /. l) and z = f.z +. (oz *. r /. l) in
    let b = walk_by opened b (x -. b.x) (z -. b.z) in
    stepped { b with heading = heading_to (f.x -. b.x) (f.z -. b.z) })
  else if z_held then
    (* nothing to target: Link keeps facing ahead, and strafes *)
    let dx, dz = along b.heading in
    stepped (walk_by opened b (speed *. dx) (speed *. dz))
  else
    (* free: the stick is the camera's, and Link turns to run where it
     * points *)
    let dx, dz = along p.cam_heading in
    if not moving then stepped b
    else
      let l = Float.hypot dx dz in
      let b = walk_by opened b (speed *. dx /. l) (speed *. dz /. l) in
      stepped { b with heading = heading_to dx dz }

(* bumped back, away from [from] *)
let knocked (opened : bool) (b : body) (from : body) (by : number) : body =
  let a = heading_to (b.x -. from.x) (b.z -. from.z) in
  let fx, fz = Camera3d.forward a in
  walk_by opened b (by *. fx) (by *. fz)

(*****************************************************************************)
(* The sword and the shield *)
(*****************************************************************************)

(* the shield is up while the Stalfos stalks and while it winds up, and
 * covers what is in front of it *)
let shielded (p : play) : bool =
  (match p.foe_state with Stalk -> true | Chop n -> Frame_data.phase chop n = Startup | _ -> false)
  && off_facing p.foe p.link < 70.

type strike = Miss | Blocked | Hit

(* what the slash does on one of its active frames *)
let strike (p : play) : strike =
  if (not (alive p)) || dist p.link p.foe > reach || off_facing p.link p.foe > 50. then Miss
  else if shielded p then Blocked
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
    let opened = door_open p in
    match strike p with
    | Miss -> p
    | Blocked -> { p with landed = true; clang = 10; link = knocked opened p.link p.foe 0.5 }
    | Hit ->
        let foe_hp = p.foe_hp - slash.damage in
        { p with
          landed = true;
          foe_hp;
          foe_state = (if foe_hp <= 0 then Dying 0 else Reel slash.hitstun);
          foe = knocked false p.foe p.link 1.0 }

(*****************************************************************************)
(* The Stalfos *)
(*****************************************************************************)

(* degrees a frame: Link circling at arm's length (2.5) goes round at
 * speed / 2.5 radians, 2.5 degrees, so a Stalfos turning at 1.4 falls
 * behind, and its back comes round *)
let foe_turn = 1.4

let turn_towards (rate : number) (from : number) (towards : number) : number =
  from +. Float.max (-.rate) (Float.min rate (diff towards from))

let step_foe (p : play) : play =
  let f = p.foe and l = p.link in
  let towards = heading_to (l.x -. f.x) (l.z -. f.z) in
  match p.foe_state with
  | Stalk ->
      let heading = turn_towards foe_turn f.heading towards in
      let pace = if dist f l > 2.3 then 0.035 else 0. in
      let fx, fz = Camera3d.forward heading in
      let f = walk_by false { f with heading } (pace *. fx) (pace *. fz) in
      let f = { f with step = (if pace > 0. then f.step +. pace else 0.) } in
      (* close, and facing Link: the chop *)
      let chops = dist f l < 3. && Float.abs (diff towards heading) < 25. in
      { p with foe = f; foe_state = (if chops then Chop 1 else Stalk) }
  | Chop n when n > Frame_data.length chop -> { p with foe_state = Stalk }
  | Chop n ->
      let phase = Frame_data.phase chop n in
      (* it follows Link while it winds up, slowly: step aside late and
       * the blade comes down beside you *)
      let f = if phase = Startup then { f with heading = turn_towards 0.6 f.heading towards; step = 0. } else f in
      let p = { p with foe = f; foe_state = Chop (n + 1) } in
      if phase = Active && p.hurt = 0 && dist f l < 3.2 && off_facing f l < 50. then
        { p with hearts = p.hearts - chop.damage; hurt = chop.hitstun; link = knocked (door_open p) l f 1.5 }
      else p
  | Reel n -> { p with foe_state = (if n <= 1 then Stalk else Reel (n - 1)) }
  | Dying n -> { p with foe_state = (if n >= 90 then Gone else Dying (n + 1)) }
  | Gone -> p

(*****************************************************************************)
(* The camera *)
(*****************************************************************************)

(* the eye kept inside the room: without it, a camera behind Link
 * backed against a wall is inside the wall *)
let in_room ((x, y, z) : number * number * number) : number * number * number =
  let m = half -. 0.3 in
  (Float.max (-.m) (Float.min m x), y, Float.max (-.m) (Float.min m z))

(* where the camera wants to be, and the free camera's heading *)
let wanted_camera (z_held : bool) (moving : bool) (p : play) : camera * number =
  let l = p.link in
  if p.locked then
    (* over Link's shoulder, on the line from the enemy through him,
     * looking at the point between the two *)
    let f = p.foe in
    let h = heading_to (f.x -. l.x) (f.z -. l.z) in
    let fx, fz = Camera3d.forward h and rx, rz = Camera3d.forward (h +. 90.) in
    let eye = in_room (l.x -. (5. *. fx) +. (1.2 *. rx), 3., l.z -. (5. *. fz) +. (1.2 *. rz)) in
    (camera ~eye ~target:((l.x +. f.x) /. 2., 1.1, (l.z +. f.z) /. 2.) (), h)
  else
    (* trailing: z swings it behind Link at once, running swings it
     * slowly after him *)
    let h =
      if z_held then l.heading
      else if moving then p.cam_heading +. (0.03 *. diff l.heading p.cam_heading)
      else p.cam_heading
    in
    let fx, fz = Camera3d.forward h in
    (camera ~eye:(in_room (l.x -. (6. *. fx), 3.4, l.z -. (6. *. fz))) ~target:(l.x, 1., l.z) (), h)

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let step_play (keys : keyboard) (swing_pressed : bool) (p : play) : play =
  let z_held = Set_.mem "z" keys.keys in
  let s = stick_of keys in
  let locked = lock_on z_held p in
  let p = { p with locked; link = move_link z_held locked s p } in
  let p = step_slash swing_pressed p in
  let p = step_foe p in
  (* two bodies do not overlap: Link is pushed out of the Stalfos *)
  let p =
    if alive p && dist p.link p.foe < 1.2 then { p with link = knocked (door_open p) p.link p.foe (1.2 -. dist p.link p.foe) }
    else p
  in
  let wanted, cam_heading = wanted_camera z_held (s.fwd <> 0. || s.side <> 0.) p in
  let nx, ny, nz = p.navi in
  let tx, ty, tz =
    if p.locked then (p.foe.x, 2.9, p.foe.z)
    else (p.link.x +. 0.6, 2. +. (0.15 *. sin (float_of_int p.frames /. 12.)), p.link.z +. 0.3)
  in
  { p with
    cam = Camera3d.follow 0.12 wanted p.cam;
    cam_heading;
    letterbox = p.letterbox +. (((if p.locked then 1. else 0.) -. p.letterbox) *. 0.2);
    navi = (nx +. ((tx -. nx) *. 0.15), ny +. ((ty -. ny) *. 0.15), nz +. ((tz -. nz) *. 0.15));
    hurt = max 0 (p.hurt - 1);
    clang = max 0 (p.clang - 1);
    opened = (if alive p then p.opened else Float.min 1. (p.opened +. 0.01));
    frames = p.frames + 1 }

let escaped (p : play) : bool = p.link.z < -.half -. 1.5

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

let foe_pose (p : play) : Skeleton.pose =
  match p.foe_state with
  | Stalk -> walking p.foe stalk
  | Chop n ->
      let s = chop.startup and a = chop.active in
      Skeleton.at
        [ (0, stalk); (s, raised); (s + 2, chopped); (s + a, chopped); (s + a + 10, opened_up);
          (Frame_data.length chop - 8, opened_up); (Frame_data.length chop, stalk) ]
        n
  | Reel _ -> { stalk with lean = -25.; back_arm = Skeleton.limb 20. }
  | Dying _ | Gone -> { opened_up with lean = 40. }

(* the things held, built from the fist down the forearm (Skeleton's
 * [draw]): a blade and its hilt, and a shield across the forearm *)
let sword (length : number) : shape3d =
  group3d
    [ box (rgb 210 215 230) 0.07 length 0.025 |> move_y3d (-.(length /. 2.) -. 0.1);
      box (rgb 120 90 40) 0.22 0.05 0.07 |> move_y3d (-0.08) ]

let shield : shape3d = box (rgb 130 84 50) 0.75 0.08 0.9 |> move_y3d (-0.05)

(* a dark patch on the floor under a figure, to show where it stands *)
let shadow (b : body) (size : number) : shape3d = box (rgb 58 55 64) size 0.02 size |> move3d b.x 0.01 b.z

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let p = match m.scene with Title -> new_play () | Playing p | Over (p, _) -> p in
  let cam =
    match m.scene with
    | Title -> Camera3d.orbit ~fov:70. ~distance:9. ~height:4. ~look:1. (spin 30. computer.time) (0., 0., 1.5)
    | _ -> p.cam
  in
  let link =
    Skeleton.draw ~front_hand:(sword 0.9) ~body:(rgb 40 140 60) ~back:(rgb 30 100 44) ~skin:(rgb 240 200 160) 1.6
      p.link.heading (link_pose p)
    |> move3d p.link.x 0. p.link.z
    |> fun s -> if p.hurt > 0 && p.hurt mod 10 < 5 then fade3d 0.4 s else s
  in
  let foe =
    match p.foe_state with
    | Gone -> []
    | state ->
        (* falling apart: it sinks through the floor *)
        let sink = match state with Dying n -> -2.2 *. float_of_int n /. 90. | _ -> 0. in
        [ Skeleton.draw ~front_hand:(sword 1.2) ~back_hand:shield ~body:(rgb 226 220 200) ~back:(rgb 170 165 150)
            ~skin:(rgb 240 236 220) 2.1 p.foe.heading (foe_pose p)
          |> move3d p.foe.x sink p.foe.z;
          shadow p.foe 0.75 ]
  in
  let spark =
    if p.clang = 0 then []
    else
      let fx, fz = Camera3d.forward p.foe.heading in
      [ sphere (rgb 255 240 120) (0.05 *. float_of_int p.clang) |> move3d (p.foe.x +. (0.7 *. fx)) 1.3 (p.foe.z +. (0.7 *. fz)) ]
  in
  let navi =
    let x, y, z = p.navi in
    let c = if p.locked then rgb 255 230 80 else rgb 170 220 255 in
    [ sphere c 0.13 |> move3d x y z ]
  in
  let world = (room :: link :: shadow p.link 0.55 :: foe) @ torches computer.time @ bars p.opened @ spark @ navi in
  let letterbox =
    if p.letterbox < 0.01 then []
    else
      let h = 70. *. p.letterbox in
      [ rectangle black screen.width h |> move_y (screen.top -. (h /. 2.));
        rectangle black screen.width h |> move_y (screen.bottom +. (h /. 2.)) ]
  in
  let hud =
    match m.scene with
    | Title ->
        [ rectangle black screen.width 120. |> move_y 300. |> fade 0.6;
          rectangle black screen.width 260. |> move_y (-290.) |> fade 0.6;
          text (rgb 230 200 90) 6. "TINY ZELDA OCARINA" |> move_y 300.;
          text white 2.5 "arrows: move   space: sword   hold z: target" |> move_y (-230.);
          text white 2.5 "the bars lift when the Stalfos falls" |> move_y (-275.) ]
        @ Scene2d.blink 1. m [ text yellow 4. "PRESS SPACE" |> move_y (-360.) ]
    | Playing _ ->
        letterbox
        @ List.init 3 (fun i ->
            circle (if i < p.hearts then rgb 220 40 50 else rgb 70 60 60) 14.
            |> move (screen.left +. 40. +. (36. *. float_of_int i)) (screen.top -. 40.))
        @
        if p.frames < 400 && not p.locked then [ text white 2.5 "hold z to target" |> move_y (screen.bottom +. 40.) ]
        else if door_open p && p.frames mod 60 < 40 then [ text (rgb 230 200 90) 3. "THE WAY IS OPEN" |> move_y (screen.bottom +. 40.) ]
        else []
    | Over (_, won) ->
        [ rectangle black 1000. 220. |> move_y 180. |> fade 0.6;
          text (if won then rgb 230 200 90 else rgb 200 60 50) 6. (if won then "ONWARD" else "GAME OVER") |> move_y 200. ]
        @ Scene2d.blink 1. m [ text white 3. "PRESS SPACE" |> move_y 110. ]
  in
  (cam, world @ List.map Playground3d.hud hud)

let app = game3d view update initial_model

let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
