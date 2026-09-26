(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Tomb Raider (Toby Gard, Core Design, 1996): a raider
 * in a tomb dug into a cave, an idol on a plinth across a chasm of
 * spikes, a block to push, and a boulder. Left and right turn her, up
 * runs, down steps back, space jumps (up held: a running jump), x is the
 * hands -- hold it to grab a ledge in mid-air, to hang on it (up pulls
 * her up, left and right shimmy), to climb a step as high as she is
 * (with up), to push a block. Take the idol, then get out. (Names and
 * dates from memory, to check.)
 *
 * Core Design had made Rick Dangerous seven years before, boulder and
 * all (TinyRick.ml, whose header points here), and this is that game
 * in three dimensions, slowed down and made deliberate. Three things
 * make it Tomb Raider, and each has its section below:
 *
 * 1. *The level is a grid of heights.* Every room of the original is a
 *    grid of square sectors, 1024 units on a side, and each sector has
 *    a floor and a ceiling, their heights counted in "clicks", a
 *    quarter of a sector. A floor can be tilted, and its four corners
 *    then make two triangles, which is how a grid of squares becomes
 *    ramps, slides and the uneven floor of a cave. Walls are simply
 *    where two floors differ, standing on the edges of the grid.
 *
 *         clicks   what she does with a step that high
 *         1        walks up it without noticing
 *         4        climbs it (x and up): a block, as high as she is
 *         6        jumps up, grabs it (x held) and pulls herself up
 *         8        nothing, without a block to stand on
 *
 *    So the grid is still there, but in the heights, no longer in her
 *    feet: she runs freely, turns by any angle, and the level is
 *    measured in what she can reach. The table is the whole game.
 *
 * 2. *The jump is committed.* She runs and turns freely, but once she
 *    leaves the ground her velocity is fixed: nothing the player does
 *    steers her in the air ([step_air] never reads the arrows), so a
 *    running jump is always exactly as long (2.6 sectors), a standing
 *    one always as short, and a gap of two sectors is a thing you can
 *    be certain about. That is what makes a game this stiff *fair*,
 *    and why its players line up a jump -- a hop back, a run-up --
 *    where Mario's feel it out. Her hands, too, are committed to the
 *    grid: a ledge is a sector's edge, so a grab snaps her square to
 *    it, facing north, south, east or west, whatever angle she arrived
 *    at.
 *
 * 3. *She is a skeleton of rigid pieces.* The original's Lara is not
 *    one mesh bent at the joints (skinning came later) but a dozen
 *    small meshes, one per bone, each moved whole by its joint's angle:
 *    a hierarchy, a thigh carrying a shin carrying a foot. Hers are
 *    here, tapered boxes and pyramids ([hull]), posed by the brawler
 *    kit's Skeleton, whose key poses are interpolated between frames:
 *    a run is two poses swung by a sine, a pull-up is three keyframes.
 *    And yes, the pyramids are what you think, and famous.
 *
 * The walls are what the game was looked at for: room after room of
 * textured stone, in a year when most 3D was flat colours. The textures
 * live in one *page* (games/adventure/tomb.png, a 2 x 2 grid of 64 x 64
 * tiles), and every face takes a square of it by its UV corners, one
 * square of texture per sector of wall -- stretch a tile over a wall
 * three sectors high and the courses come out three times too tall,
 * the giveaway of a level built by someone who never saw the original.
 * scripts/build/make_tomb_atlas.py draws it with fractal noise and
 * crushes it to 16 colours, dithered: the look of the original's
 * photographs of stone, crushed into an 8-bit palette. The cave around
 * the tomb is the same grid made rough: each corner shared only by cave
 * sectors is nudged up or down by a hash of its position ([rough]), so
 * its floor and ceiling are triangles no two alike -- and since it is
 * the same function that draws the floor and answers "how high is the
 * ground here" ([floor_at]), she walks on exactly what is drawn. Where
 * the original textured its caves too, ours is bare rock, a flat colour
 * per triangle ([Rock]): the built tomb and the natural cave then read
 * apart at a glance, and the facets are the look of the era anyway.
 *
 *      corridor ============================ (2 sectors up)
 *                                        || ramp
 *                          platform ___  \\ slide
 *      plinth  block    ^^     ledge |     \\
 *        [I]   [B]      ^^ chasm     |______\\___ cave floor
 *
 * What it uses: playground3d (textured triangles, the texture carried
 * inside the program as TinyMinecraft.ml carries its own), Camera3d
 * (the chase camera, followed smoothly), kit_brawler3d's Skeleton (the
 * poses, keyframes and interpolation; the meshes are hers) and
 * gamekits/puzzle's Push -- the same Push that TinySokoban.ml uses,
 * because a pushable block in a tomb is a Sokoban crate that happens to
 * be drawn in three dimensions. No physics engine: the collision is
 * five points tested against [floor_at] and [ceiling_at].
 *
 * Exercises: the original's diagonal walls (a sector split along its
 * diagonal, half floor, half rock); water, and swimming, which is the
 * same controls in three dimensions; a pull (x and down) for the block;
 * a walk (shift) that stops at edges; the swan dive; the wolves, and
 * her two pistols.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The tomb, a grid of heights *)
(*****************************************************************************)

(* The floor of each sector: its height in clicks (a quarter of a
 * sector), '#' for rock from floor to ceiling, '^' for the bottom of the
 * chasm, two sectors down, and its spikes. A sector is one world unit:
 * column x spans x to x + 1, row z spans z to z + 1. *)
let floors =
  [| "##################";
     "#888888888888888##";
     "#############66###";
     "##########6666666#";
     "##00#000^^0002200#";
     "##00#100^^010000##";
     "##080000^^001000##";
     "##000100^^000010##";
     "##100000^^00000###";
     "###00010^^01000###";
     "###00000^^0000####";
     "####000###00######";
     "##################" |]

(* The tilted ones, and which way they rise: n, e, s, w by two clicks
 * over the sector, a ramp she walks; N, E, S, W by four, too steep to
 * stand on, a slide. The floor's number is the height of its low
 * edge. *)
let slopes =
  [| "..................";
     "..................";
     ".............nn...";
     "..................";
     ".............NN...";
     ".............nn...";
     "..................";
     "..................";
     "..................";
     "..................";
     "..................";
     "..................";
     ".................." |]

(* The ceilings, in half sectors: a digit where the tomb was built, flat,
 * a letter ('a' + n for n half sectors) where it is cave, rough. *)
let ceilings =
  [| "##################";
     "#777777777777777##";
     "#############77###";
     "##########jjjjjjj#";
     "##hh#hhhiiiiijjii#";
     "##hh#hhhiiiiijjj##";
     "##h8hhhhiiiiiiih##";
     "##ghhhhhiiiihhhh##";
     "##gghhhhiiihhhh###";
     "###gghhhiighhhh###";
     "###ggghhiigggg####";
     "####ggg###gg######";
     "##################" |]

let entrance = (1.5, 1.5)
let plinth = (3, 6)
let block_start = (4, 9)
let click = 0.25
let cols = String.length floors.(0)
let rows = Array.length floors

let at_map (m : string array) (cx : int) (cz : int) : char =
  if cx < 0 || cz < 0 || cx >= cols || cz >= rows then '#' else m.(cz).[cx]

let digit (c : char) (zero : char) : number = float_of_int (Char.code c - Char.code zero)

(* the height of a sector's low edge, None in the rock *)
let base (cx : int) (cz : int) : number option =
  match at_map floors cx cz with
  | '#' -> None
  | '^' -> Some (-2.)
  | c -> Some (digit c '0' *. click)

let pit (cx : int) (cz : int) : bool = at_map floors cx cz = '^'
let cave (cx : int) (cz : int) : bool = at_map ceilings cx cz >= 'a'

let ceiling_of (cx : int) (cz : int) : number =
  let c = at_map ceilings cx cz in
  0.5 *. if c >= 'a' then digit c 'a' else digit c '0'

let steep (cx : int) (cz : int) : bool = String.contains "NESW" (at_map slopes cx cz)

(* where a slide takes her: away from the side it rises to *)
let downhill (cx : int) (cz : int) : int * int =
  match at_map slopes cx cz with 'N' -> (0, 1) | 'S' -> (0, -1) | 'E' -> (-1, 0) | _ -> (1, 0)

(* how far the tilt lifts the point (fx, fz) of the sector, both from
 * 0. to 1. *)
let tilt (cx : int) (cz : int) (fx : number) (fz : number) : number =
  let c = at_map slopes cx cz in
  let rise = if c = '.' then 0. else if Char.uppercase_ascii c = c then 4. *. click else 2. *. click in
  rise
  *.
  match Char.lowercase_ascii c with
  | 'n' -> 1. -. fz
  | 's' -> fz
  | 'e' -> fx
  | 'w' -> 1. -. fx
  | _ -> 0.

(* A hash of a corner, from -0.5 to 0.5: the same corner, the same
 * number, whichever sector asks -- which is what keeps the rough cave
 * in one piece. *)
let noise (a : int) (b : int) : number =
  let h = ((a * 374761393) + (b * 668265263)) land 0x3fffffff in
  let h = (h lxor (h lsr 13)) * 1274126177 land 0x3fffffff in
  (float_of_int ((h lsr 7) land 1023) /. 1023.) -. 0.5

(* the open sectors around the corner (vx, vz) *)
let around (vx : int) (vz : int) : (int * int) list =
  List.filter (fun (cx, cz) -> base cx cz <> None) [ (vx - 1, vz - 1); (vx, vz - 1); (vx - 1, vz); (vx, vz) ]

(* A corner is rough when every sector sharing it is cave (and not the
 * chasm, whose walls stay sheer): then it can move without tearing the
 * built parts of the tomb, since they do not use it. *)
let rough (vx : int) (vz : int) : bool =
  match around vx vz with
  | [] -> false
  | l -> List.for_all (fun (cx, cz) -> cave cx cz && not (pit cx cz)) l

(* The height of the floor of sector (cx, cz) at its corner (vx, vz):
 * the grid, the tilt, and the cave's roughness. *)
let corner (cx : int) (cz : int) (vx : int) (vz : int) : number =
  let b = Option.value (base cx cz) ~default:0. in
  b +. tilt cx cz (float_of_int (vx - cx)) (float_of_int (vz - cz)) +. if rough vx vz then 0.08 *. noise vx vz else 0.

(* The ceiling belongs to the corners, not to the sectors: a corner is as
 * high as the lowest ceiling around it, so the cave's roof is one
 * surface, sloping where the map changes, with no steps in it. *)
let ceiling_corner (vx : int) (vz : int) : number =
  let low = List.fold_left (fun m (cx, cz) -> Float.min m (ceiling_of cx cz)) infinity (around vx vz) in
  if low = infinity then 0. else low +. if rough vx vz then 0.6 *. noise (vx + 101) (vz + 57) else 0.

(* A sector's four corners make two triangles, split along the diagonal
 * from (0, 0) to (1, 1); the height of (fx, fz) is on the plane of the
 * one it falls in. This is how the floor is drawn, too ([floor_tris]):
 * what she stands on is what you see. *)
let on_triangles (h00 : number) (h10 : number) (h11 : number) (h01 : number) (fx : number) (fz : number) : number =
  if fx >= fz then h00 +. ((h10 -. h00) *. fx) +. ((h11 -. h10) *. fz)
  else h00 +. ((h11 -. h01) *. fx) +. ((h01 -. h00) *. fz)

let sector (x : number) (z : number) : int * int = (int_of_float (Float.floor x), int_of_float (Float.floor z))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* the way she is going through the air: what the pose shows, and
 * whether it was a jump at all *)
type leap = Up_jump | Stand_jump | Run_jump | Falling

type state =
  | Ground
  (* her velocity, fixed at takeoff: only gravity changes it *)
  | Air of { vx : number; vy : number; vz : number; leap : leap }
  (* hands on a ledge [top] high, on the edge of her sector towards
   * (nx, nz) *)
  | Hang of { nx : int; nz : int; top : number }
  (* pulling herself up or climbing a step: an animation with its two
   * ends settled when it starts *)
  | Climb of { frame : int; span : int; from_ : number * number * number; to_ : number * number * number }
  (* pushing the block one sector towards (nx, nz) *)
  | Push of { frame : int; nx : int; nz : int; from_ : number * number }
  | Slide

(* where she is, her heading (Camera3d's: 0 towards -z, 90 towards +x),
 * her speed on the ground (negative: backwards), the run's phase, for
 * the legs, and the highest point of the jump or fall she is in *)
type lara = { x : number; y : number; z : number; heading : number; speed : number; state : state; phase : number; peak : number }

type game = {
  lara : lara;
  block : int * int;
  idol : bool; (* still on its plinth *)
  boulder : number option; (* its x, rolling west down the corridor *)
  dead : string option;
  out : bool;
  frames : int;
  cam : camera;
}

(* what the player asks for, one frame's worth: the game reads the
 * keyboard into this, and the tests write it *)
type input = { up : bool; down : bool; left : bool; right : bool; jump : bool; action : bool }

let idle = { up = false; down = false; left = false; right = false; jump = false; action = false }

(*****************************************************************************)
(* How high is it here *)
(*****************************************************************************)

(* the block is part of the floor: a sector one higher, flat *)
let floor_at (g : game) (x : number) (z : number) : number option =
  let cx, cz = sector x z in
  match base cx cz with
  | None -> None
  | Some b when (cx, cz) = g.block -> Some (b +. 1.)
  | Some _ ->
      let c = corner cx cz in
      Some (on_triangles (c cx cz) (c (cx + 1) cz) (c (cx + 1) (cz + 1)) (c cx (cz + 1)) (x -. float_of_int cx) (z -. float_of_int cz))

let ceiling_at (x : number) (z : number) : number =
  let cx, cz = sector x z in
  let c = ceiling_corner in
  on_triangles (c cx cz) (c (cx + 1) cz) (c (cx + 1) (cz + 1)) (c cx (cz + 1)) (x -. float_of_int cx) (z -. float_of_int cz)

(* her size, in sectors: the original's Lara is about three quarters
 * of one, and her raised hands reach a little higher *)
let radius = 0.2
let tall = 0.78
let reach = 0.95
let step_up = 0.35 (* a click, and the cave's unevenness: walked over *)

(* Can she stand with her feet at [y] and her middle at (x, z)? Five
 * points, her middle and four around it: none in the rock, none higher
 * than [rise] above her feet, none under a ceiling lower than her head.
 * That is all the collision there is. *)
let feet = [ (0., 0.); (radius, radius); (radius, -.radius); (-.radius, radius); (-.radius, -.radius) ]

let fits ?(rise = step_up) (g : game) (x : number) (z : number) (y : number) : bool =
  List.for_all
    (fun (ox, oz) ->
      let px = x +. ox and pz = z +. oz in
      match floor_at g px pz with
      | None -> false
      | Some f -> f <= y +. rise && ceiling_at px pz >= Float.max f y +. tall)
    feet

(* What she stands on: the highest floor under the five points, of those
 * she can step onto. Not the floor under her middle only, or she would
 * sink into a ledge whose edge she lands on. *)
let support ?(rise = step_up) (g : game) (x : number) (z : number) (y : number) : number =
  List.fold_left
    (fun m (ox, oz) ->
      match floor_at g (x +. ox) (z +. oz) with Some f when f <= y +. rise -> Float.max m f | _ -> m)
    neg_infinity feet

(* a move by (dx, dz), or as much of it as fits: along a wall she slides
 * instead of stopping dead. The bool says she hit something. *)
let move ?rise (g : game) (l : lara) (dx : number) (dz : number) : number * number * bool =
  let ok x z = fits ?rise g x z l.y in
  if ok (l.x +. dx) (l.z +. dz) then (l.x +. dx, l.z +. dz, false)
  else if ok (l.x +. dx) l.z then (l.x +. dx, l.z, true)
  else if ok l.x (l.z +. dz) then (l.x, l.z +. dz, true)
  else (l.x, l.z, true)

(*****************************************************************************)
(* Her hands: the grid comes back *)
(*****************************************************************************)

(* the side of the grid she faces most: a ledge is a sector's edge, so
 * whatever her heading her hands take one of four directions *)
let cardinal (heading : number) : int * int =
  match ((int_of_float (Float.round (heading /. 90.)) mod 4) + 4) mod 4 with
  | 0 -> (0, -1)
  | 1 -> (1, 0)
  | 2 -> (0, 1)
  | _ -> (-1, 0)

let heading_of ((nx, nz) : int * int) : number =
  match (nx, nz) with 0, -1 -> 0. | 1, 0 -> 90. | 0, 1 -> 180. | _ -> 270.

(* The edge in front of her: its direction, its coordinate along it (x
 * for east and west, z for north and south), and the height of what is
 * beyond it, just past the edge -- or None when her hands would not be
 * on it, that is when the sector in front is not the next one, or is
 * rock. *)
type edge = { nx : int; nz : int; at : number; top : number }

let edge_ahead (g : game) (l : lara) : edge option =
  let nx, nz = cardinal l.heading in
  let cx, cz = sector l.x l.z in
  let hx = l.x +. (float_of_int nx *. (radius +. 0.15)) and hz = l.z +. (float_of_int nz *. (radius +. 0.15)) in
  if sector hx hz <> (cx + nx, cz + nz) then None
  else
    let at = if nx <> 0 then float_of_int (cx + max 0 nx) else float_of_int (cz + max 0 nz) in
    (* the height just past the edge, where her fingers go, across from
     * where she stands *)
    let px = if nx <> 0 then at +. (0.05 *. float_of_int nx) else l.x in
    let pz = if nz <> 0 then at +. (0.05 *. float_of_int nz) else l.z in
    Option.map (fun top -> { nx; nz; at; top }) (floor_at g px pz)

(* her middle put [d] from the edge, on her side of it (d < 0: past it) *)
let off_edge (e : edge) (l : lara) (d : number) : number * number =
  if e.nx <> 0 then (e.at -. (float_of_int e.nx *. d), l.z) else (l.x, e.at -. (float_of_int e.nz *. d))

(* up onto what is beyond the edge, if she fits there: a pull-up from a
 * hang, or a climb from the ground *)
let climb_onto (g : game) (l : lara) (e : edge) (span : int) : lara option =
  let x1, z1 = off_edge e l (-0.3) in
  if not (fits g x1 z1 e.top) then None
  else
    let x0, z0 = off_edge e l (radius +. 0.02) in
    Some
      { l with
        heading = heading_of (e.nx, e.nz);
        speed = 0.;
        state = Climb { frame = 0; span; from_ = (x0, l.y, z0); to_ = (x1, e.top, z1) } }

(*****************************************************************************)
(* Her moves *)
(*****************************************************************************)

(* in sectors per frame, at 60 frames a second, and per frame per frame *)
let run = 3.0 /. 60.
let back = 1.0 /. 60.
let gravity = 20. /. 3600.
let jump_up = 5.37 /. 60. (* 0.72 sector high: hands at 1.67, a six-click ledge *)
let run_jump = 4.8 /. 60. (* 2.6 sectors, from a run *)
let stand_jump = 2.2 /. 60. (* 1.2 sectors *)
let turn_rate = 4.
let deadly_fall = 3.2

let forward (l : lara) : number * number = Camera3d.forward l.heading

let takeoff (l : lara) (leap : leap) (speed : number) : lara =
  let fx, fz = forward l in
  let vy = if leap = Falling then 0. else jump_up in
  { l with state = Air { vx = fx *. speed; vy; vz = fz *. speed; leap }; peak = l.y }

(* The sector the block is pushed into must be open floor at the
 * block's own height: Sokoban's rule, one crate at a time. *)
let push (g : game) (l : lara) (e : edge) : lara option =
  let cx, cz = sector l.x l.z in
  let height = base (fst g.block) (snd g.block) in
  let blocked (x, z) = base x z <> height || pit x z in
  match Push.chain ~blocked ~pushable:(fun c -> c = g.block) ~limit:1 (cx, cz) (e.nx, e.nz) with
  | Some [ _ ] ->
      (* squared up to it, in the middle of her sector *)
      let x, z = off_edge e l (radius +. 0.02) in
      let x = if e.nx = 0 then float_of_int cx +. 0.5 else x and z = if e.nz = 0 then float_of_int cz +. 0.5 else z in
      Some { l with x; z; heading = heading_of (e.nx, e.nz); speed = 0.; state = Push { frame = 0; nx = e.nx; nz = e.nz; from_ = (x, z) } }
  | _ -> None

(* on her feet: the hands first, then a jump, then running and turning *)
let step_ground (g : game) (inp : input) (l : lara) : lara =
  let cx, cz = sector l.x l.z in
  let hands =
    if not (inp.action && inp.up) then None
    else
      match edge_ahead g l with
      | Some e when (cx + e.nx, cz + e.nz) = g.block -> (
          match push g l e with Some l -> Some l | None -> if e.top -. l.y <= 1.05 then climb_onto g l e 40 else None)
      | Some e when e.top -. l.y > step_up && e.top -. l.y <= 1.05 -> climb_onto g l e 40
      | _ -> None
  in
  match hands with
  | Some l -> l
  | None when inp.jump ->
      if inp.up && l.speed >= 0.8 *. run then takeoff l Run_jump run_jump
      else if inp.up then takeoff l Stand_jump stand_jump
      else takeoff l Up_jump 0.
  | None when steep cx cz -> { l with state = Slide }
  | None ->
      let heading = l.heading +. (if inp.left then -.turn_rate else if inp.right then turn_rate else 0.) in
      let wanted = if inp.up then run else if inp.down then -.back else 0. in
      let speed = if l.speed < wanted then Float.min wanted (l.speed +. 0.005) else Float.max wanted (l.speed -. 0.008) in
      let l = { l with heading; speed } in
      let fx, fz = forward l in
      let x, z, hit = move g l (fx *. speed) (fz *. speed) in
      let l = { l with x; z; speed = (if hit then speed *. 0.5 else speed); phase = l.phase +. (Float.abs speed *. 5.) } in
      (* the ground under her: followed down a ramp, fallen from off a
       * ledge *)
      let f = support g x z l.y in
      if f >= l.y -. step_up then { l with y = f } else takeoff { l with speed } Falling speed

let step_air (g : game) (inp : input) (l : lara) (vx : number) (vy : number) (vz : number) (leap : leap) : lara =
  let vy = vy -. gravity in
  (* the ceiling stops her rising *)
  let vy = if ceiling_at l.x l.z < l.y +. tall +. vy then Float.min vy 0. else vy in
  let x, z, hit = move ~rise:0.15 g l vx vz in
  let vx, vz = if hit then (0., 0.) else (vx, vz) in
  let y = l.y +. vy in
  let l = { l with x; z; peak = Float.max l.peak y } in
  let grab =
    (* x held, going down, and her hands pass a ledge's height *)
    if not (inp.action && vy < 0.) then None
    else
      match edge_ahead g l with
      | Some e when e.top <= l.y +. reach && e.top >= y +. reach && e.top > y +. step_up && ceiling_at (fst (off_edge e l (-0.3))) (snd (off_edge e l (-0.3))) -. e.top >= tall ->
          let x, z = off_edge e l (radius +. 0.02) in
          Some { l with x; z; y = e.top -. reach; heading = heading_of (e.nx, e.nz); state = Hang { nx = e.nx; nz = e.nz; top = e.top } }
      | _ -> None
  in
  match grab with
  | Some l -> l
  | None ->
      let f = support ~rise:0.15 g x z l.y in
      if y <= f then
        (* landed: a running jump lands running if up is still held *)
        let speed = if inp.up && leap = Run_jump then run else 0. in
        { l with y = f; speed; state = Ground }
      else { l with y; state = Air { vx; vy; vz; leap } }

(* Hanging: x let go drops her, up pulls her up, left and right shimmy
 * along the edge as long as there is an edge at that height to hold *)
let step_hang (g : game) (inp : input) (l : lara) (nx : int) (nz : int) (top : number) : lara =
  if not inp.action then takeoff l Falling 0.
  else if inp.up then
    match edge_ahead g l with Some e -> Option.value (climb_onto g l e 50) ~default:l | None -> l
  else if inp.left || inp.right then
    let side = if inp.right then 1. else -1. in
    let tx = -.float_of_int nz *. side and tz = float_of_int nx *. side in
    let l' = { l with x = l.x +. (tx *. 0.012); z = l.z +. (tz *. 0.012) } in
    let still_ledge =
      match edge_ahead g l' with Some e -> Float.abs (e.top -. top) < 0.1 | None -> false
    in
    (* and she does not shimmy into a wall *)
    let bx, bz = sector (l'.x +. (tx *. radius)) (l'.z +. (tz *. radius)) in
    let body_free = base bx bz <> None in
    if still_ledge && body_free then l' else l
  else l

(* a committed move plays out: where she is is only a function of the
 * frame *)
let ease (t : number) : number = t *. t *. (3. -. (2. *. t))

let step_climb (l : lara) (frame : int) (span : int) (x0, y0, z0) (x1, y1, z1) : lara =
  let frame = frame + 1 in
  let t = float_of_int frame /. float_of_int span in
  (* up first, then over *)
  let up = ease (Float.min 1. (t *. 1.6)) and over = ease (Float.max 0. ((t -. 0.4) /. 0.6)) in
  let l = { l with x = x0 +. ((x1 -. x0) *. over); y = y0 +. ((y1 -. y0) *. up); z = z0 +. ((z1 -. z0) *. over) } in
  if frame >= span then { l with x = x1; y = y1; z = z1; state = Ground }
  else { l with state = Climb { frame; span; from_ = (x0, y0, z0); to_ = (x1, y1, z1) } }

let push_span = 50

let step_slide (g : game) (inp : input) (l : lara) : lara =
  let cx, cz = sector l.x l.z in
  if not (steep cx cz) then { l with state = Ground; speed = run *. 0.6 }
  else
    let dx, dz = downhill cx cz in
    let l = { l with heading = heading_of (dx, dz) } in
    if inp.jump then takeoff l Run_jump run_jump
    else
      let x, z, _ = move g l (float_of_int dx *. 0.055) (float_of_int dz *. 0.055) in
      let f = support g x z l.y in
      if f >= l.y -. step_up then { l with x; z; y = f; phase = l.phase +. 0.1 } else takeoff { l with x; z } Falling 0.055

(*****************************************************************************)
(* The boulder, and the game *)
(*****************************************************************************)

(* It waits in the rock at the corridor's far end until she comes back
 * up with the idol, then rolls west a little slower than she runs: she
 * gets out if she does not stop to think. *)
let boulder_start = 16.5
let boulder_speed = 2.5 /. 60.
let in_corridor (l : lara) : bool = l.z < 2. && l.y > 1.9

let new_lara () : lara =
  { x = fst entrance; y = 2.; z = snd entrance; heading = 90.; speed = 0.; state = Ground; phase = 0.; peak = 2. }

(* The camera behind her and above, looking a little ahead, pulled in
 * wherever the rock or the ceiling would be between it and her: the
 * original's camera, which had to learn to live in corridors. *)
let look (g : game) : camera =
  let l = g.lara in
  let fx, fz = forward l in
  let head = (l.x, l.y +. 0.9, l.z) in
  let wanted = (l.x -. (fx *. 2.), l.y +. 1.25, l.z -. (fz *. 2.)) in
  let lerp (ax, ay, az) (bx, by, bz) t = (ax +. ((bx -. ax) *. t), ay +. ((by -. ay) *. t), az +. ((bz -. az) *. t)) in
  let inside (x, y, z) = match floor_at g x z with Some f -> y > f +. 0.1 && y < ceiling_at x z -. 0.1 | None -> false in
  let rec out_to i = if i >= 20 || not (inside (lerp head wanted (float_of_int (i + 1) /. 20.))) then i else out_to (i + 1) in
  let eye = lerp head wanted (float_of_int (out_to 0) /. 20.) in
  camera ~eye ~target:(l.x +. (fx *. 0.8), l.y +. 0.5, l.z +. (fz *. 0.8)) ~fov:70. ~near:0.05 ()

let new_game () : game =
  let g = { lara = new_lara (); block = block_start; idol = true; boulder = None; dead = None; out = false; frames = 0; cam = camera ~eye:(0., 0., 0.) ~target:(0., 0., -1.) () } in
  { g with cam = look g }

let step_lara (g : game) (inp : input) : game =
  let l = g.lara in
  match l.state with
  | Ground -> { g with lara = step_ground g inp l }
  | Air a -> { g with lara = step_air g inp l a.vx a.vy a.vz a.leap }
  | Hang h -> { g with lara = step_hang g inp l h.nx h.nz h.top }
  | Climb c -> { g with lara = step_climb l c.frame c.span c.from_ c.to_ }
  | Slide -> { g with lara = step_slide g inp l }
  | Push p ->
      let frame = p.frame + 1 in
      let t = ease (float_of_int frame /. float_of_int push_span) in
      let x0, z0 = p.from_ in
      let l = { l with x = x0 +. (float_of_int p.nx *. t); z = z0 +. (float_of_int p.nz *. t); phase = l.phase +. 0.08 } in
      if frame < push_span then { g with lara = { l with state = Push { p with frame } } }
      else { g with lara = { l with state = Ground }; block = (fst g.block + p.nx, snd g.block + p.nz) }

(* what happens where she lands or stands, after she has moved *)
let consequences (before : lara) (g : game) : game =
  let l = g.lara in
  let landed = (match before.state with Air _ -> true | _ -> false) && l.state = Ground in
  let cx, cz = sector l.x l.z in
  let g = if g.idol && l.state = Ground && (cx, cz) = plinth then { g with idol = false } else g in
  let g = if (not g.idol) && g.boulder = None && in_corridor l then { g with boulder = Some boulder_start } else g in
  let g = match g.boulder with Some b -> { g with boulder = Some (Float.max 1.5 (b -. boulder_speed)) } | None -> g in
  if landed && pit cx cz then { g with dead = Some "the spikes" }
  else if landed && before.peak -. l.y > deadly_fall then { g with dead = Some "the fall" }
  else if (match g.boulder with Some b -> in_corridor l && Float.abs (b -. l.x) < 0.5 +. radius | None -> false) then
    { g with dead = Some "the boulder" }
  else if (not g.idol) && in_corridor l && l.x < 1.8 then { g with out = true }
  else g

let step (g : game) (inp : input) : game =
  if g.dead <> None || g.out then g
  else
    let g = consequences g.lara (step_lara { g with frames = g.frames + 1 } inp) in
    { g with cam = Camera3d.follow 0.2 (look g) g.cam }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

type scene = Title | Playing of game | Lost of string | Escaped of int
type model = scene Scene2d.t

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let k = computer.keyboard in
      let inp = { up = k.kup; down = k.kdown; left = k.kleft; right = k.kright; jump = space; action = Set_.mem "x" k.keys } in
      let g = step g inp in
      if g.out then Scene2d.go (Escaped g.frames) s
      else if g.dead <> None then Scene2d.go (Lost (Option.get g.dead)) s
      else { s with scene = Playing g }
  | Lost _ | Escaped _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* The tomb, drawn *)
(*****************************************************************************)

(* The atlas travels inside the program, the way TinyMinecraft's does
 * (the game's dune file turns tomb.png into Tomb_atlas.base64 at build
 * time), so there is no file to find and the browser gets it as a
 * "data:" URL. It is a 2 x 2 grid, drawn by
 * scripts/build/make_tomb_atlas.py:
 *
 *      (0,0) wall stone        (1,0) wall with hieroglyphs
 *      (0,1) floor flagstone   (1,1) the plinth, and the block
 *)
let atlas = embedded_texture ~name:"tomb-atlas" ~base64:Tomb_atlas.base64

(* A polygon of the atlas: each point with its place (u, v), 0. to 1.,
 * in the tile [cell] -- what the original called an object texture.
 * Points in a row are dropped, so a wall's row that ends in a point is
 * a triangle. *)
let textured (cell : int * int) (pts : ((number * number * number) * (number * number)) list) : shape3d list =
  let rec dedup = function
    | (p, _) :: ((q, _) :: _ as rest) when p = q -> dedup rest
    | x :: rest -> x :: dedup rest
    | [] -> []
  in
  let pts = dedup pts in
  let pts = match (pts, List.rev pts) with (p, _) :: _, (q, _) :: _ when p = q -> List.tl pts | _ -> pts in
  if List.length pts < 3 then []
  else
    let u0 = float_of_int (fst cell) *. 0.5 and v0 = float_of_int (snd cell) *. 0.5 in
    [ { alpha = 1.;
        material = matte;
        form = TexturedPolygon3d (atlas, List.map (fun (p, (u, v)) -> (p, (u0 +. (u *. 0.5), v0 +. (v *. 0.5)))) pts) } ]

let fl (i : int) : number = float_of_int i

type xyz = number * number * number

let middle (pts : xyz list) : xyz =
  let n = fl (List.length pts) in
  let sx, sy, sz = List.fold_left (fun (a, b, c) (x, y, z) -> (a +. x, b +. y, c +. z)) (0., 0., 0.) pts in
  (sx /. n, sy /. n, sz /. n)

(* What a face is made of: the tomb's cut stone, a tile of the atlas;
 * or the cave's bare rock, a flat colour per triangle, varied by a hash
 * of where it is, so that the light picks out every facet. The built
 * tomb and the natural cave then read apart at a glance. *)
type surface = Stone of (int * int) | Rock

let rock_color ((x, y, z) : xyz) : color =
  let t = noise (int_of_float (x *. 8.)) (int_of_float ((y *. 6.) +. (z *. 8.))) in
  let v c = int_of_float (float_of_int c *. (1. +. (0.2 *. t))) in
  rgb (v 122) (v 106) (v 86)

let polygon (s : surface) (pts : (xyz * (number * number)) list) : shape3d list =
  match s with
  | Stone cell -> textured cell pts
  | Rock -> (
      match List.map fst pts with
      | [ (ax, ay, az); (bx, by, bz); (cx, cy, cz) ] as tri ->
          (* a triangle with no area has no normal to light it by *)
          let ux = bx -. ax and uy = by -. ay and uz = bz -. az and wx = cx -. ax and wy = cy -. ay and wz = cz -. az in
          let n = Float.abs ((uy *. wz) -. (uz *. wy)) +. Float.abs ((uz *. wx) -. (ux *. wz)) +. Float.abs ((ux *. wy) -. (uy *. wx)) in
          if n < 1e-6 then [] else [ polygon3d (rock_color (middle tri)) tri ]
      | pts -> [ polygon3d (rock_color (middle pts)) pts ])

(* The floor of a sector, as the two triangles [on_triangles] reads
 * heights from, counter-clockwise from above; and the ceiling, the
 * same corners the other way round. *)
let floor_tris (s : surface) (cx : int) (cz : int) : shape3d list =
  let p vx vz = ((fl vx, corner cx cz vx vz, fl vz), (fl (vx - cx), fl (vz - cz))) in
  let a = p cx cz and b = p (cx + 1) cz and c = p (cx + 1) (cz + 1) and d = p cx (cz + 1) in
  polygon s [ a; c; b ] @ polygon s [ a; d; c ]

let ceiling_tris (s : surface) (cx : int) (cz : int) : shape3d list =
  let p vx vz = ((fl vx, ceiling_corner vx vz, fl vz), (fl (vx - cx), fl (vz - cz))) in
  let a = p cx cz and b = p (cx + 1) cz and c = p (cx + 1) (cz + 1) and d = p cx (cz + 1) in
  polygon s [ a; b; c ] @ polygon s [ a; c; d ]

(* An upright face between two corners, as seen from the side it faces:
 * (lx, lz) on the left, (rx, rz) on the right, from the heights [bl],
 * [br] up to [tl], [tr]. Not one stretched picture but one square of
 * texture per sector of height, which is how the original tiled its
 * walls; the last row is cut by the top, sloping with the cave's
 * ceiling. *)
let wall (cell : int * int) (lx, lz) (rx, rz) (bl, br) (tl, tr) : shape3d list =
  let n = int_of_float (Float.ceil (Float.max (tl -. bl) (tr -. br) -. 0.01)) in
  List.concat
    (List.init (max 0 n) (fun i ->
         let i = fl i in
         let l0 = Float.min (bl +. i) tl and l1 = Float.min (bl +. i +. 1.) tl in
         let r0 = Float.min (br +. i) tr and r1 = Float.min (br +. i +. 1.) tr in
         textured cell
           [ ((lx, l1, lz), (0., 1. -. (l1 -. bl -. i)));
             ((lx, l0, lz), (0., 1.));
             ((rx, r0, rz), (1., 1.));
             ((rx, r1, rz), (1., 1. -. (r1 -. br -. i))) ]))

(* The same face in the cave: half a sector a row, each row's middle
 * point pushed back by up to 0.3 into the rock behind, towards (px, pz),
 * so the wall bulges and no two rows alike. Only the middle moves --
 * its corners are shared with the faces around it, its top and bottom
 * with the floor and the ceiling -- and only away from her, so she
 * never walks into rock that the collision does not know about. *)
let rock_wall (lx, lz) (rx, rz) (bl, br) (tl, tr) ((px, pz) : number * number) : shape3d list =
  let n = int_of_float (Float.ceil (Float.max (tl -. bl) (tr -. br) /. 0.5)) in
  let mx = (lx +. rx) /. 2. and mz = (lz +. rz) /. 2. in
  let row k =
    let h b t = b +. ((t -. b) *. fl k /. fl n) in
    let bulge = if k = 0 || k = n then 0. else 0.3 *. (noise ((int_of_float (mx *. 2.)) + (k * 17)) (int_of_float (mz *. 2.)) +. 0.5) in
    ((lx, h bl tl, lz), (mx +. (px *. bulge), (h bl tl +. h br tr) /. 2., mz +. (pz *. bulge)), (rx, h br tr, rz))
  in
  let tri pts = polygon Rock (List.map (fun p -> (p, (0., 0.))) pts) in
  List.concat
    (List.init (max 0 n) (fun k ->
         let l0, m0, r0 = row k and l1, m1, r1 = row (k + 1) in
         tri [ l1; l0; m0 ] @ tri [ l1; m0; m1 ] @ tri [ m1; m0; r0 ] @ tri [ m1; r0; r1 ]))

(* the four sides of a sector: towards (dx, dz), and its two corners as
 * seen from inside, left then right *)
let sides (cx : int) (cz : int) : ((int * int) * (int * int) * (int * int)) list =
  [ ((0, -1), (cx, cz), (cx + 1, cz));
    ((1, 0), (cx + 1, cz), (cx + 1, cz + 1));
    ((0, 1), (cx + 1, cz + 1), (cx, cz + 1));
    ((-1, 0), (cx, cz + 1), (cx, cz)) ]

(* hieroglyphs along the corridor, and on the plinth *)
let wall_tile (cx : int) (cz : int) : int * int = if cz <= 1 || (cx, cz) = plinth then (1, 0) else (0, 0)

(* Everything a sector shows: its floor, its ceiling, a wall on each side
 * where the rock is, and a skirt down to each neighbour lower than it.
 * Each face is drawn once, by the sector that owns it, in the tomb's
 * stone or the cave's rock. *)
let sector_shapes (cx : int) (cz : int) : shape3d list =
  match base cx cz with
  | None -> []
  | Some _ ->
      let natural = cave cx cz && (cx, cz) <> plinth in
      let floor_s = if natural then Rock else if (cx, cz) = plinth then Stone (1, 1) else Stone (0, 1) in
      let spikes =
        if not (pit cx cz) then []
        else
          List.map
            (fun (ox, oz) -> box (rgb 168 168 178) 0.08 0.5 0.08 |> move3d (fl cx +. ox) (-1.75) (fl cz +. oz))
            [ (0.25, 0.25); (0.75, 0.25); (0.5, 0.5); (0.25, 0.75); (0.75, 0.75) ]
      in
      (* [into] is the way to the rock behind the face *)
      let face left right bottoms tops into =
        if natural then rock_wall left right bottoms tops into else wall (wall_tile cx cz) left right bottoms tops
      in
      floor_tris floor_s cx cz @ ceiling_tris (if natural then Rock else Stone (0, 0)) cx cz @ spikes
      @ List.concat_map
          (fun ((dx, dz), (lx, lz), (rx, rz)) ->
            let nx = cx + dx and nz = cz + dz in
            let p (vx, vz) = (fl vx, fl vz) in
            match base nx nz with
            | None ->
                (* the rock: a wall from our floor to the ceiling, seen
                 * from inside *)
                face (p (lx, lz)) (p (rx, rz))
                  (corner cx cz lx lz, corner cx cz rx rz)
                  (ceiling_corner lx lz, ceiling_corner rx rz)
                  (fl dx, fl dz)
            | Some _ ->
                (* a lower neighbour sees our side from its own: its
                 * right is our left *)
                let ol = corner nx nz lx lz and orr = corner nx nz rx rz in
                let ml = corner cx cz lx lz and mr = corner cx cz rx rz in
                if ml >= ol && mr >= orr && ml +. mr > ol +. orr +. 0.01 then
                  face (p (rx, rz)) (p (lx, lz)) (orr, ol) (mr, ml) (fl (-dx), fl (-dz))
                else [])
          (sides cx cz)

let tomb : shape3d =
  cached3d (List.concat (List.init rows (fun cz -> List.concat (List.init cols (fun cx -> sector_shapes cx cz)))))

(* the block, a sector of plinth stone one high, wherever it is *)
let block_shapes (x : number) (z : number) (h : number) : shape3d list =
  let q a b c d = textured (1, 1) [ (a, (0., 0.)); (b, (0., 1.)); (c, (1., 1.)); (d, (1., 0.)) ] in
  let x1 = x +. 1. and z1 = z +. 1. and t = h +. 1. in
  q (x, t, z) (x, t, z1) (x1, t, z1) (x1, t, z)
  @ q (x, t, z1) (x, h, z1) (x1, h, z1) (x1, t, z1)
  @ q (x1, t, z) (x1, h, z) (x, h, z) (x, t, z)
  @ q (x1, t, z1) (x1, h, z1) (x1, h, z) (x1, t, z)
  @ q (x, t, z) (x, h, z) (x, h, z1) (x, t, z1)

(* torches on the cave's walls, for the look of the place *)
let torches : shape3d list =
  List.concat_map
    (fun (x, y, z) ->
      [ box (rgb 58 46 34) 0.08 0.3 0.08 |> move3d x y z; sphere (rgb 252 198 98) 0.1 |> move3d x (y +. 0.2) z ])
    [ (2.08, 1.2, 8.5); (7.5, 1.2, 4.08); (15.92, 1.3, 6.5); (11.5, 2.6, 3.08); (5.5, 1.2, 10.92) ]

(*****************************************************************************)
(* Lara, a skeleton of rigid pieces *)
(*****************************************************************************)

(* a face turned to face away from [c], the middle of the solid it
 * belongs to: every piece of her is convex, so this is all the winding
 * order there is to get right *)
let outward (c : xyz) (pts : xyz list) : xyz list =
  match pts with
  | (ax, ay, az) :: (bx, by, bz) :: (qx, qy, qz) :: _ ->
      let ux = bx -. ax and uy = by -. ay and uz = bz -. az and wx = qx -. ax and wy = qy -. ay and wz = qz -. az in
      let nx = (uy *. wz) -. (uz *. wy) and ny = (uz *. wx) -. (ux *. wz) and nz = (ux *. wy) -. (uy *. wx) in
      let cx, cy, cz = c in
      if (nx *. (ax -. cx)) +. (ny *. (ay -. cy)) +. (nz *. (az -. cz)) < 0. then List.rev pts else pts
  | _ -> pts

(* a rectangle [w] across and [d] deep, at height [y] *)
let ring ?(dz = 0.) (w : number) (d : number) (y : number) : xyz list =
  [ (-.w, y, dz -. d); (w, y, dz -. d); (w, y, dz +. d); (-.w, y, dz +. d) ]

(* The one mesh she is made of: a solid between two rings, tapered as a
 * thigh or a torso is. Six faces; the original's pieces were not much
 * more. *)
let hull (color : color) (top : xyz list) (bottom : xyz list) : shape3d =
  let c = middle (top @ bottom) in
  let t = Array.of_list top and b = Array.of_list bottom in
  let n = Array.length t in
  let sides = List.init n (fun i -> let j = (i + 1) mod n in [ t.(i); b.(i); b.(j); t.(j) ]) in
  group3d (List.map (fun f -> polygon3d color (outward c f)) (top :: bottom :: sides))

(* a piece hanging from its joint: [len] long, [w0] by [d0] at the joint,
 * [w1] by [d1] at the far end *)
let piece (color : color) (w0 : number) (d0 : number) (w1 : number) (d1 : number) (len : number) : shape3d =
  hull color (ring w0 d0 0.) (ring w1 d1 (-.len))

(* a pyramid on a rectangle *)
let pyramid (color : color) (base : xyz list) (apex : xyz) : shape3d =
  let c = middle (apex :: base) in
  let b = Array.of_list base in
  let n = Array.length b in
  group3d
    (polygon3d color (outward c base)
    :: List.init n (fun i -> polygon3d color (outward c [ apex; b.(i); b.((i + 1) mod n) ])))

let skin = rgb 226 176 136
let top_color = rgb 64 138 146
let shorts = rgb 116 86 58
let boots = rgb 78 52 36
let hair = rgb 86 54 30

(* A limb, the hierarchy made plain: the far piece (and what it carries)
 * is built at the origin, bent at its joint, moved to the end of the
 * near piece, and only then does the whole limb turn at the shoulder or
 * the hip -- so the hip's angle carries the shin and the foot with it.
 * [knee] is -1. for a leg, whose joint closes backwards. *)
let limb ?(knee = 1.) (side : number) (l : Skeleton.limb) (near : shape3d) (len : number) (far : shape3d list) : shape3d =
  group3d [ near; group3d far |> rotate3d (knee *. l.bend) 0. 0. |> move_y3d (-.len) ]
  |> rotate3d l.pitch 0. 0.
  |> rotate3d 0. 0. (side *. l.yaw)

(* Her, standing on y = 0 and facing -z, in [pose]; [braid] is how far
 * the plait swings back, in degrees. *)
let lara_mesh (p : Skeleton.pose) (braid : number) : shape3d =
  let hip = 0.45 in
  let leg side (l : Skeleton.limb) =
    limb ~knee:(-1.) side l
      (group3d [ piece skin 0.048 0.052 0.036 0.04 0.2; box (rgb 40 36 34) 0.03 0.1 0.06 |> move3d (side *. 0.05) (-0.08) 0. ])
      0.2
      [ piece skin 0.036 0.04 0.028 0.032 0.18; box boots 0.065 0.07 0.12 |> move3d 0. (-0.2) (-0.025) ]
    |> move3d (side *. 0.055) (hip -. 0.06) 0.
  in
  let arm side (l : Skeleton.limb) =
    limb side l (piece skin 0.03 0.03 0.025 0.025 0.15) 0.15
      [ piece skin 0.025 0.025 0.02 0.02 0.14; box skin 0.035 0.06 0.02 |> move_y3d (-0.16) ]
    |> move3d (side *. 0.13) 0.21 0.
  in
  let breast side =
    let x = side *. 0.048 in
    pyramid top_color
      [ (x -. 0.034, 0.19, -0.056); (x +. 0.034, 0.19, -0.056); (x +. 0.034, 0.12, -0.056); (x -. 0.034, 0.12, -0.056) ]
      (x +. (side *. 0.006), 0.15, -0.115)
  in
  let plait =
    let seg = piece hair 0.018 0.018 0.013 0.013 0.09 in
    group3d [ seg; group3d [ seg; box hair 0.03 0.04 0.03 |> move_y3d (-0.1) ] |> rotate3d (-.braid *. 0.5) 0. 0. |> move_y3d (-0.09) ]
    |> rotate3d (-.braid) 0. 0.
    |> move3d 0. 0.34 0.07
  in
  let trunk =
    group3d
      [ hull top_color (ring 0.085 0.055 0.) (ring 0.12 0.065 0.24);
        breast 1.;
        breast (-1.);
        piece skin 0.028 0.028 0.028 0.028 0.05 |> move_y3d 0.28;
        hull skin (ring 0.05 0.055 0.26) (ring 0.056 0.062 0.36);
        hull hair (ring ~dz:0.01 0.062 0.068 0.32) (ring ~dz:0.012 0.045 0.05 0.39);
        plait;
        arm 1. p.front_arm;
        arm (-1.) p.back_arm ]
    (* the lean carries her arms and her head, since they are inside the
     * group before it turns *)
    |> rotate3d (-.p.lean) 0. 0.
    |> move_y3d hip
  in
  group3d [ hull shorts (ring 0.088 0.058 hip) (ring 0.105 0.066 (hip -. 0.1)); trunk; leg 1. p.front_leg; leg (-1.) p.back_leg ]

(*****************************************************************************)
(* Her poses: what each state looks like *)
(*****************************************************************************)

let lb = Skeleton.limb
let still : Skeleton.pose = { Skeleton.stand with front_arm = lb ~yaw:6. 0.; back_arm = lb ~yaw:6. 0.; front_leg = lb 0.; back_leg = lb 0. }
let arms_up (p : Skeleton.pose) : Skeleton.pose = { p with front_arm = lb ~yaw:4. 178.; back_arm = lb ~yaw:4. 178. }

(* the run: two poses, one leg forward and then the other, swung
 * between by a sine; a leg going back bends at the knee *)
let running (phase : number) : Skeleton.pose =
  let s = sin phase in
  let leg s = lb ~bend:(15. +. (55. *. Float.max 0. (-.s))) (38. *. s) in
  { lean = 12.; turn = 0.; front_leg = leg s; back_leg = leg (-.s); front_arm = lb ~yaw:8. ~bend:50. (-35. *. s); back_arm = lb ~yaw:8. ~bend:50. (35. *. s) }

(* hanging, pulling up, standing: a pull-up is these three keyframes *)
let hanging : Skeleton.pose = arms_up { still with front_leg = lb ~bend:10. 6.; back_leg = lb ~bend:20. (-4.) }
let crouched : Skeleton.pose =
  { still with lean = 35.; front_arm = lb ~yaw:10. ~bend:80. 20.; back_arm = lb ~yaw:10. ~bend:80. 20.; front_leg = lb ~bend:110. 85.; back_leg = lb ~bend:40. 20. }

let pose (l : lara) (holding : bool) : Skeleton.pose =
  match l.state with
  | Ground -> Skeleton.lerp still (running l.phase) (Float.min 1. (Float.abs l.speed /. run))
  | Air a -> (
      let reaching p = if holding then arms_up p else p in
      match a.leap with
      | Up_jump -> arms_up still
      | Run_jump | Stand_jump ->
          reaching { still with lean = 8.; front_leg = lb ~bend:70. 45.; back_leg = lb ~bend:40. (-15.); front_arm = lb ~yaw:20. 150.; back_arm = lb ~yaw:20. 150. }
      | Falling -> reaching { still with front_arm = lb ~yaw:50. 120.; back_arm = lb ~yaw:50. 100.; front_leg = lb ~bend:40. 25.; back_leg = lb ~bend:20. (-10.) })
  | Hang _ -> hanging
  | Climb c -> Skeleton.at [ (0, hanging); (c.span * 2 / 5, crouched); (c.span, still) ] c.frame
  | Push _ ->
      let r = running l.phase in
      { r with lean = 22.; front_arm = lb ~yaw:8. ~bend:30. 80.; back_arm = lb ~yaw:8. ~bend:30. 80. }
  | Slide -> { still with lean = -10.; front_arm = lb ~yaw:50. 30.; back_arm = lb ~yaw:50. 30.; front_leg = lb ~bend:25. 30.; back_leg = lb ~bend:10. 0. }

(* the plait trails behind her running, flies up as she falls *)
let braid (l : lara) : number =
  match l.state with
  | Air a -> Float.min 70. (Float.max 0. (-.a.vy *. 600.)) +. 15.
  | Ground -> 8. +. (Float.abs l.speed *. 500.) +. (4. *. sin (l.phase *. 2.))
  | _ -> 8.

let lara_shape (l : lara) (holding : bool) (dead : bool) : shape3d =
  let m = lara_mesh (pose l holding) (braid l) in
  (* dead, she lies where she fell *)
  let m = if dead then m |> rotate3d (-90.) 0. 0. |> move_y3d 0.06 else m in
  m |> rotate3d 0. (-.l.heading) 0. |> move3d l.x l.y l.z

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let gold = rgb 226 184 68
let text (c : color) (size : number) (s : string) : shape = words c s |> scale size

let view_tomb (g : game) : shape3d list =
  let l = g.lara in
  let bx, bz = g.block in
  (* the block moves with her while she pushes it *)
  let bx, bz =
    match l.state with
    | Push p -> let t = ease (fl p.frame /. fl push_span) in (fl bx +. (fl p.nx *. t), fl bz +. (fl p.nz *. t))
    | _ -> (fl bx, fl bz)
  in
  let px, pz = plinth in
  [ tomb ] @ torches
  @ block_shapes bx bz (Option.value (base (fst g.block) (snd g.block)) ~default:0.)
  @ (if g.idol then [ sphere gold 0.14 |> move3d (fl px +. 0.5) 2.2 (fl pz +. 0.5) ] else [])
  @ (match g.boulder with
    | None -> []
    (* its facets turning show it rolling *)
    | Some b -> [ sphere (rgb 118 108 96) 0.48 |> rotate3d 0. 0. (b *. 115.) |> move3d b 2.48 1.5 ])
  @ [ lara_shape l false (g.dead <> None) ]

let help = "left right turn   up runs   down steps back   space jumps   x: grab, climb, push"

let panel (screen : screen) (g : game) : shape list =
  [ text (rgb 200 190 170) 1.7
      (if g.idol then "find the idol, across the chasm" else "get out -- the boulder is coming")
    |> move_y (screen.top -. 40.);
    text (rgb 130 124 112) 1.4 help |> move_y (screen.bottom +. 34.) ]

(* the title's still: her on the cave floor, at the chasm's edge,
 * looking across it at the plinth *)
let titled (lines : shape list) (s : model) (blink : shape list) : camera * shape3d list =
  let g = new_game () in
  let g = { g with lara = { g.lara with x = 11.2; y = 0.; z = 6.5; heading = 285. } } in
  ( camera ~eye:(13.5, 2.8, 9.4) ~target:(7.5, -0.4, 6.) ~fov:70. (),
    view_tomb g @ List.map hud (lines @ Scene2d.blink 1. s blink) )

let view (computer : computer) (s : model) : camera * shape3d list =
  match s.scene with
  | Title ->
      titled
        [ text gold 6. "TINY TOMB RAIDER" |> move_y 300.;
          text white 2.2 "an idol across a chasm, a block to push, and a boulder" |> move_y 240.;
          text white 1.8 help |> move_y 205. ]
        s [ text yellow 3. "PRESS SPACE" |> move_y 150. ]
  | Playing g -> (g.cam, view_tomb g @ List.map hud (panel computer.screen g))
  | Lost how ->
      titled [ text (rgb 230 110 100) 5. ("KILLED BY " ^ String.uppercase_ascii how) |> move_y 200. ] s
        [ text white 2.5 "PRESS SPACE" |> move_y 120. ]
  | Escaped frames ->
      titled
        [ text gold 6. "OUT, WITH THE IDOL" |> move_y 200.; text white 3. (Printf.sprintf "%d seconds" (frames / 60)) |> move_y 130. ]
        s [ text white 2.5 "PRESS SPACE" |> move_y 60. ]

let app = game3d view update (Scene2d.start Title)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
