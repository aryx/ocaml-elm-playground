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
 * alone in a stone tomb, an idol on a pedestal, and a boulder. Arrows
 * turn and walk, space jumps, x is the hands -- push a block, pull
 * yourself up a ledge, take the idol. Then get out. (Names and dates
 * from memory, to check.)
 *
 * Core Design had made Rick Dangerous seven years before, boulder and
 * all (games/TinyRick.ml, whose header points here), and this is that
 * game in three dimensions, slowed down and made deliberate.
 *
 * The rendering is nothing new here -- boxes, after TinyQuake and
 * TinyDoom3d. Two other things are, and they hold each other up:
 *
 * 1. *The move is committed.* Everywhere else in this repository a
 *    character has a velocity that you steer every frame: TinyMario,
 *    TinySonic, TinyMario64 all integrate an acceleration. The raider
 *    does not. She has a small vocabulary of moves ([kind]), each of
 *    them a fixed number of frames covering a fixed distance, and once
 *    one starts it plays out: you cannot nudge her mid-air. In the
 *    original this came from the animation -- the position was taken
 *    from the animation data, frame by frame ("root motion"), so a
 *    running jump was exactly as long as the artist had drawn it. That
 *    is why Lara feels heavy, and why her players count moves where
 *    Mario's players feel out a velocity.
 *
 * 2. *The level is measured in her moves.* Because a jump is always
 *    the same length, a gap of one square is a thing you can be
 *    certain about: a running jump crosses it, a standing jump does
 *    not, and the tomb is a set of such certainties. Tomb Raider's
 *    world is built out of a 1024-unit block: the raider is one block
 *    wide, a ledge one block up is one she can grab, and the answer to
 *    a room is countable before you try it.
 *
 * Neither half works without the other. Make the jump analog and "two
 * squares" stops meaning anything; keep the jump fixed but build the
 * room freehand and the player has no way to read it. That pair is the
 * whole design, and the reason a game this stiff feels *fair*.
 *
 * And the walls, which is what the game was looked at for: room after
 * room of textured stone, in a year when most 3D was flat colours. The
 * technique here is the era's rather than ours. The tomb's textures
 * live in one *page* (games3d/tomb.png, a 2 x 2 grid of 64 x 64 tiles)
 * and every face takes a sub-rectangle of it by its four UV corners,
 * which is what the original called an object texture. A wall three
 * squares high is three textured squares and not one picture stretched
 * over it ([wall_column]) -- the stretched version is the giveaway of
 * a level built by someone who never saw the original. And the whole
 * page shares one 16-colour palette, dithered into it.
 *
 * That last one is worth knowing, because it is where the "random,
 * almost fractal" look of those walls comes from. It was not the art:
 * Core's artists worked from photographs of stone, cut down by hand to
 * 64 x 64. It was the 8-bit palette the photographs were crushed into,
 * whose dither speckles every surface, and a PlayStation that drew
 * them with affine mapping and no filtering, so they swam as you
 * walked. scripts/build/make_tomb_atlas.py draws ours with fractal
 * noise -- the cheapest way to fake a photograph of stone -- and then
 * crushes it the same way. (From memory, to check.)
 *
 * What we simplify: the original runs freely between the squares and
 * only its jumps are locked to them, where here every move steps from
 * square to square ([Step] is a move like any other, just a short one).
 * So the tomb reads exactly as the original's does, and the raider is
 * coarser than Lara. The other consequence is worth stating because it
 * is the whole trick of the code: a move's *outcome is settled the
 * moment it starts* ([attempt] works it out, [pose] then merely draws
 * the arc), which is what being committed means, and it is why there
 * is no collision detection in this file at all.
 *
 *      . . # #        standing jump: one square, for getting up
 *      @ ^ 0 #        running jump:  two squares, so it crosses the
 *      # # # #        one-square chasm. Count the squares, not pixels.
 *
 * What it uses: playground3d (box, sphere, camera, hud, and
 * TexturedPolygon3d with the texture carried inside the program, as
 * games3d/TinyMinecraft.ml carries its own), Camera3d (behind: the
 * chase camera), and gamekits/puzzle's Push -- the
 * same Push that games/TinySokoban.ml uses, on the tomb's floor grid,
 * because a pushable block in a tomb is a Sokoban crate that happens
 * to be drawn in three dimensions. That is the nicest thing this game
 * demonstrates: the kit is about the idea, not the rendering.
 *
 * Exercises: the ledge vocabulary in full (shimmy along a ledge, drop
 * and re-grab, the running jump *to* a ledge); a pole to swing on; the
 * original's real timing, with analog running between the squares and
 * only the jumps locked; two rooms and a camera that has to decide what
 * to show; the wolves.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The tomb *)
(*****************************************************************************)

(* The floor, a square at a time: a digit is its height in squares,
 * '#' is rock from floor to ceiling, '^' a chasm with spikes at the
 * bottom. Everything the raider can do is measured against these
 * numbers: a square one higher is a ledge to pull up on, a chasm is
 * something a running jump crosses. *)
let floors =
  [ "###############";
    "#0000000000000#";
    "#############0#";
    "###00^000000#0#";
    "###20^00001100#";
    "###00^000011###";
    "###00^000000###";
    "###############" ]

(* what stands on the floor: the raider, the pushable block, the idol *)
let things =
  [ "...............";
    ".@.............";
    "...............";
    "...............";
    "...I...........";
    "....B..........";
    "...............";
    "..............." ]

let cols = String.length (List.hd floors)
let rows = List.length floors
let at_map (m : string list) (cx : int) (cz : int) : char =
  if cx < 0 || cz < 0 || cx >= cols || cz >= rows then '#' else (List.nth m cz).[cx]

(* the height of a square's floor, or None where the rock is solid.
 * The chasm is a floor too, a long way down, with spikes on it. *)
let floor_of (cx : int) (cz : int) : int option =
  match at_map floors cx cz with '#' -> None | '^' -> Some (-3) | c -> Some (Char.code c - Char.code '0')

let spiked (cx : int) (cz : int) : bool = at_map floors cx cz = '^'
let find (m : string list) (c : char) : int * int =
  let rec go cz = function
    | [] -> (1, 1)
    | row :: rest -> ( match String.index_opt row c with Some cx -> (cx, cz) | None -> go (cz + 1) rest)
  in
  go 0 m

(*****************************************************************************)
(* The four ways she can face *)
(*****************************************************************************)

type dir = North | East | South | West

let delta (d : dir) : int * int =
  match d with North -> (0, -1) | East -> (1, 0) | South -> (0, 1) | West -> (-1, 0)

let right_of (d : dir) : dir = match d with North -> East | East -> South | South -> West | West -> North
let left_of (d : dir) : dir = right_of (right_of (right_of d))

(* Camera3d's headings: 0 looks along -z, which is our North *)
let heading_of (d : dir) : number =
  match d with North -> 0. | East -> 90. | South -> 180. | West -> 270.

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

(* where she is: a square, the height she stands at, and the way she
 * faces. Hanging, she is in the square she jumped from, with her hands
 * on the one in front. *)
type place = { cx : int; cz : int; y : int; facing : dir; hanging : bool }

(* her whole vocabulary. Each one is a fixed number of frames over a
 * fixed distance -- see the header. *)
type kind =
  | Turn
  | Step
  | Back
  | Jump of int (* one square (standing) or two (running) *)
  | Grab (* a jump that ended with her hands on a ledge *)
  | Pull_up
  | Let_go
  | Drop of int (* how many squares down *)

let span (k : kind) : int =
  match k with
  | Turn -> 8
  | Step -> 12
  | Back -> 14
  | Jump 1 -> 20
  | Jump _ -> 26
  | Grab -> 22
  | Pull_up -> 22
  | Let_go -> 10
  | Drop n -> 10 + (4 * n)

type doing = { kind : kind; frame : int; from_ : place; to_ : place }

type game = {
  at : place;
  doing : doing option;
  block : int * int; (* the pushable block's square *)
  idol : bool; (* still on its pedestal *)
  boulder : int option; (* how far along the corridor it has rolled *)
  dead : string option;
  out : bool;
  frames : int;
}

type scene = Title | Playing of game | Lost of string | Escaped of int
type model = scene Scene2d.t

let entrance = find things '@'
let idol_at = find things 'I'

let new_game () : game =
  let cx, cz = entrance in
  { at = { cx; cz; y = 0; facing = East; hanging = false };
    doing = None; block = find things 'B'; idol = true; boulder = None; dead = None; out = false; frames = 0 }

(*****************************************************************************)
(* What is in a square *)
(*****************************************************************************)

(* the top of a square: its floor, or the block's top if it is there.
 * None where the rock is solid. *)
let top_of (g : game) (cx : int) (cz : int) : int option =
  match floor_of cx cz with
  | None -> None
  | Some h -> if (cx, cz) = g.block then Some (h + 1) else Some h

let ahead (p : place) (n : int) : int * int =
  let dx, dz = delta p.facing in
  (p.cx + (n * dx), p.cz + (n * dz))

(*****************************************************************************)
(* The moves, and what each of them settles *)
(*****************************************************************************)

(* A move is worked out here, once, and then only drawn: where she ends
 * up is decided before the first frame of it is shown. Being committed
 * is exactly this. *)
let landing (g : game) (p : place) (cx, cz) : (place * kind) option =
  match top_of g cx cz with
  | None -> None (* rock: the move does not happen *)
  | Some h ->
      if h = p.y then Some ({ p with cx; cz }, Step)
      else if h = p.y + 1 then Some ({ p with hanging = true }, Grab) (* a ledge: her hands catch it *)
      else if h < p.y then Some ({ p with cx; cz; y = h }, Drop (p.y - h))
      else None (* too high to do anything with *)

let attempt (g : game) (action : [ `Forward | `Back | `Left | `Right | `Jump | `Hands ]) : doing option =
  let p = g.at in
  let start kind to_ = Some { kind; frame = 0; from_ = p; to_ } in
  if p.hanging then
    (* on a ledge: up to pull up, down to let go, and that is all *)
    match action with
    | `Forward | `Jump ->
        let cx, cz = ahead p 1 in
        start Pull_up { p with cx; cz; y = p.y + 1; hanging = false }
    | `Back -> start Let_go { p with hanging = false }
    | _ -> None
  else
    match action with
    | `Left -> start Turn { p with facing = left_of p.facing }
    | `Right -> start Turn { p with facing = right_of p.facing }
    | `Forward -> (
        match landing g p (ahead p 1) with
        | None -> None
        | Some (to_, kind) -> start kind to_)
    | `Back -> (
        let dx, dz = delta p.facing in
        let behind = (p.cx - dx, p.cz - dz) in
        match top_of g (fst behind) (snd behind) with
        | Some h when h = p.y -> start Back { p with cx = fst behind; cz = snd behind }
        | _ -> None)
    | `Jump ->
        (* a standing jump crosses one square, a running jump two: which
         * one she does is which one the player asked for, by jumping
         * out of a walk or from a stand (see [update_game]) *)
        None
    | `Hands -> (
        (* the block in front, pushed one square, Sokoban's own move *)
        let front = ahead p 1 in
        if front <> g.block then None
        else
          let dx, dz = delta p.facing in
          let blocked (cx, cz) = floor_of cx cz <> Some p.y in
          let pushable (cx, cz) = (cx, cz) = g.block in
          match Push.chain ~blocked ~pushable ~limit:1 (p.cx, p.cz) (dx, dz) with
          | Some [ _ ] -> start Step { p with cx = fst front; cz = snd front }
          | _ -> None)

(* the two jumps, asked for explicitly *)
let jump (g : game) (squares : int) : doing option =
  let p = g.at in
  if p.hanging then None
  else
    match landing g p (ahead p squares) with
    | None -> None
    (* her hands catch a ledge one square up: she ends hanging over the
     * last square she flew across, not over the one she started on *)
    | Some (_, Grab) ->
        let cx, cz = ahead p (squares - 1) in
        Some { kind = Grab; frame = 0; from_ = p; to_ = { p with cx; cz; hanging = true } }
    (* anything else she lands on, including the bottom of a chasm she
     * misjudged *)
    | Some (to_, _) -> Some { kind = Jump squares; frame = 0; from_ = p; to_ }

(*****************************************************************************)
(* The boulder *)
(*****************************************************************************)

(* the corridor it rolls down: the top row, east to west, which is the
 * way out *)
let corridor : (int * int) list = List.init (cols - 2) (fun i -> (cols - 2 - i, 1))
let boulder_delay = 14

(* it is let go one square behind her, and rolls a square every
 * [boulder_delay] frames against her twelve: she gets out if she does
 * not stop to think *)
let boulder_square (g : game) : (int * int) option =
  match g.boulder with
  | None -> None
  | Some n ->
      let i = (n / boulder_delay) - 1 in
      if i < 0 then None else List.nth_opt corridor i

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let finish (g : game) (d : doing) : game =
  let g = { g with at = d.to_; doing = None } in
  (* the block goes with her when she pushes it *)
  let g =
    if d.kind = Step && (d.to_.cx, d.to_.cz) = g.block then
      let dx, dz = delta d.to_.facing in
      { g with block = (g.block |> fun (bx, bz) -> (bx + dx, bz + dz)) }
    else g
  in
  (* the idol, taken by standing on its pedestal *)
  let g = if g.idol && (g.at.cx, g.at.cz) = idol_at then { g with idol = false } else g in
  (* the boulder is let go the moment she is back in the corridor with it *)
  let g = if (not g.idol) && g.boulder = None && g.at.cz = 1 then { g with boulder = Some 0 } else g in
  if spiked g.at.cx g.at.cz then { g with dead = Some "the spikes" }
  else if (match d.kind with Drop n -> n > 3 | _ -> false) then { g with dead = Some "the fall" }
  else if (not g.idol) && (g.at.cx, g.at.cz) = entrance then { g with out = true }
  else g

let update_game (computer : computer) (s : model) (g : game) : game =
  let g = { g with frames = g.frames + 1 } in
  let g = match g.boulder with Some n -> { g with boulder = Some (n + 1) } | None -> g in
  (* crushed *)
  let g =
    if boulder_square g = Some (g.at.cx, g.at.cz) && g.dead = None then { g with dead = Some "the boulder" } else g
  in
  if g.dead <> None || g.out then g
  else
    match g.doing with
    | Some d when d.frame + 1 >= span d.kind -> finish g d
    | Some d -> { g with doing = Some { d with frame = d.frame + 1 } }
    | None ->
        let pressed f = Scene2d.pressed f s in
        let held f = f computer.keyboard in
        let key c = Set_.mem c computer.keyboard.keys in
        let started =
          if pressed (fun k -> k.kspace) then
            (* out of a walk it is a running jump, from a stand a
             * standing one: the only two lengths there are *)
            match jump g (if held (fun k -> k.kup) then 2 else 1) with Some d -> Some d | None -> None
          else if key "x" then attempt g `Hands
          else if held (fun k -> k.kleft) then attempt g `Left
          else if held (fun k -> k.kright) then attempt g `Right
          else if held (fun k -> k.kup) then attempt g `Forward
          else if held (fun k -> k.kdown) then attempt g `Back
          else None
        in
        { g with doing = started }

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer s g in
      if g.out then Scene2d.go (Escaped g.frames) s
      else if g.dead <> None then Scene2d.go (Lost (Option.get g.dead)) s
      else { s with scene = Playing g }
  | Lost _ | Escaped _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* Drawing the move *)
(*****************************************************************************)

(* one square is one world unit *)
let world_of (cx : int) (cz : int) : number * number = (float_of_int cx, float_of_int cz)

let ease (t : number) : number = t *. t *. (3. -. (2. *. t))

(* Where she is now: the move's two ends, mixed. The arc is the only
 * place a jump's *shape* lives -- its length and its timing were
 * settled when it began. *)
let pose (g : game) : Camera3d.pose =
  let place_xyz (p : place) =
    let x, z = world_of p.cx p.cz in
    let y = float_of_int p.y in
    if p.hanging then
      (* hanging, she is over her own square, hands on the next one *)
      let dx, dz = delta p.facing in
      (x +. (0.5 *. float_of_int dx), y +. 0.15, z +. (0.5 *. float_of_int dz))
    else (x, y, z)
  in
  match g.doing with
  | None ->
      let x, y, z = place_xyz g.at in
      { Camera3d.x; y; z; heading = heading_of g.at.facing }
  | Some d ->
      let t = ease (float_of_int d.frame /. float_of_int (span d.kind)) in
      let x0, y0, z0 = place_xyz d.from_ and x1, y1, z1 = place_xyz d.to_ in
      let lift =
        match d.kind with
        | Jump n -> Float.sin (t *. Float.pi) *. (0.35 +. (0.25 *. float_of_int n))
        | Pull_up -> Float.sin (t *. Float.pi) *. 0.1
        | _ -> 0.
      in
      let h0 = heading_of d.from_.facing and h1 = heading_of d.to_.facing in
      (* turning takes the short way round *)
      let dh = Float.rem (h1 -. h0 +. 540.) 360. -. 180. in
      { Camera3d.x = x0 +. ((x1 -. x0) *. t);
        y = y0 +. ((y1 -. y0) *. t) +. lift;
        z = z0 +. ((z1 -. z0) *. t);
        heading = h0 +. (dh *. t) }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let gold = rgb 226 184 68

let text (c : color) (size : number) (s : string) : shape = words c s |> scale size

(* The walls are the thing Tomb Raider was looked at for: room after
 * room of textured stone, in a year when most 3D was flat colours. The
 * atlas travels inside the program, the way TinyMinecraft's does
 * (games3d/dune turns tomb.png into Tomb_atlas.base64 at build time),
 * so there is no file to find and the browser gets it as a "data:"
 * URL. It is a 2 x 2 grid, drawn by scripts/build/make_tomb_atlas.py:
 *
 *      (0,0) wall stone        (1,0) wall with hieroglyphs
 *      (0,1) floor flagstone   (1,1) the plinth, and the block
 *)
let atlas = embedded_texture ~name:"tomb-atlas" ~base64:Tomb_atlas.base64
let atlas_n = 2

let uv ((col, row) : int * int) : number * number * number * number =
  let m = 1. /. float_of_int atlas_n in
  let u0 = float_of_int col *. m and v0 = float_of_int row *. m in
  (u0, v0, u0 +. m, v0 +. m)

(* A quadrilateral of the atlas, its corners given the way the picture
 * reads -- top-left, bottom-left, bottom-right, top-right -- which is
 * also counter-clockwise seen from the front, so the backface culling
 * keeps it. *)
let face (cell : int * int) ((tl, bl, br, tr) : _ * _ * _ * _) : shape3d =
  let u0, v0, u1, v1 = uv cell in
  { alpha = 1.; form = TexturedPolygon3d (atlas, [ (tl, (u0, v0)); (bl, (u0, v1)); (br, (u1, v1)); (tr, (u1, v0)) ]) }

(* the flat top of a square, at height [h] *)
let top_face (cell : int * int) (cx : int) (cz : int) (h : number) : shape3d =
  let x, z = world_of cx cz in
  face cell ((x -. 0.5, h, z -. 0.5), (x -. 0.5, h, z +. 0.5), (x +. 0.5, h, z +. 0.5), (x +. 0.5, h, z -. 0.5))

(* The upright face where two squares meet: on the side of (cx, cz)
 * facing (dx, dz), from [y0] up to [y1]. Every wall in this tomb is one
 * of these, drawn once, by the square that can see it -- the same "one
 * face per shared edge" bookkeeping as a Doom linedef (gamekits/sectors),
 * and the reason the tomb is a few hundred quadrilaterals and not a few
 * thousand. *)
let side_face (cell : int * int) (cx : int) (cz : int) ((dx, dz) : int * int) (y0 : number) (y1 : number) : shape3d =
  let x, z = world_of cx cz in
  let fx = x +. (0.5 *. float_of_int dx) and fz = z +. (0.5 *. float_of_int dz) in
  (* along the face: the direction turned a quarter, so the corners come
   * out counter-clockwise seen from the open side *)
  let rx = float_of_int dz *. 0.5 and rz = float_of_int (-dx) *. 0.5 in
  face cell ((fx -. rx, y1, fz -. rz), (fx -. rx, y0, fz -. rz), (fx +. rx, y0, fz +. rz), (fx +. rx, y1, fz +. rz))

let sides : (int * int) list = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]
let ceiling = 3.

(* A wall is not one stretched picture but one square of it per square
 * of wall, which is how the tomb was built in the first place and how
 * the original drew it: its rooms are grids, and every face of every
 * grid square got a texture of its own. Stretch a single tile over
 * three units instead and the courses come out three times too tall,
 * which is the giveaway of a level built by someone who did not. *)
let wall_column (cell : int * int) (cx : int) (cz : int) (d : int * int) (y0 : number) (y1 : number) : shape3d list =
  let n = int_of_float (Float.round (y1 -. y0)) in
  List.init (max 1 n) (fun i ->
      let a = y0 +. float_of_int i in
      side_face cell cx cz d a (Float.min y1 (a +. 1.)))

(* the ceiling over a square, seen from below: the same corners as a
 * floor, the other way round *)
let ceiling_face (cell : int * int) (cx : int) (cz : int) (h : number) : shape3d =
  let x, z = world_of cx cz in
  face cell ((x -. 0.5, h, z +. 0.5), (x -. 0.5, h, z -. 0.5), (x +. 0.5, h, z -. 0.5), (x +. 0.5, h, z +. 0.5))

(* hieroglyphs along the entrance corridor, and on the wall the idol
 * stands against: the carved tile, wherever it will be looked at *)
let wall_tile (cx : int) (cz : int) : int * int = if cz = 0 || (cx, cz) = (2, 4) then (1, 0) else (0, 0)

let square_shapes (cx : int) (cz : int) : shape3d list =
  match floor_of cx cz with
  | None ->
      (* rock: it shows a wall to each square around it that can be
       * stood in, and nothing else *)
      List.concat_map
        (fun (dx, dz) ->
          match floor_of (cx + dx) (cz + dz) with
          | None -> []
          | Some nh -> wall_column (wall_tile cx cz) cx cz (dx, dz) (float_of_int nh) ceiling)
        sides
  | Some h ->
      let hf = float_of_int h in
      (if spiked cx cz then
         (* the chasm: its bottom, and the spikes on it *)
         (box (rgb 34 30 26) 1. 0.2 1. |> move3d (fst (world_of cx cz)) (hf -. 0.1) (snd (world_of cx cz)))
         :: List.map
              (fun (ox, oz) ->
                let x, z = world_of cx cz in
                box (rgb 168 168 178) 0.1 0.6 0.1 |> move3d (x +. ox) (hf +. 0.3) (z +. oz))
              [ (-0.25, -0.25); (0.25, -0.25); (-0.25, 0.25); (0.25, 0.25); (0., 0.) ]
       else [ top_face (if h >= 2 then (1, 1) else (0, 1)) cx cz hf ])
      (* and the skirt down to any neighbour lower than it: a ledge's
       * face, the pedestal's sides, the walls of the chasm *)
      @ (if spiked cx cz then [] else [ ceiling_face (0, 0) cx cz ceiling ])
      @ List.concat_map
          (fun (dx, dz) ->
            match floor_of (cx + dx) (cz + dz) with
            | Some nh when float_of_int nh < hf -> wall_column (0, 0) cx cz (dx, dz) (float_of_int nh) hf
            | _ -> [])
          sides

let tomb : shape3d =
  cached3d
    (List.concat_map
       (fun cz -> List.concat_map (fun cx -> square_shapes cx cz) (List.init cols Fun.id))
       (List.init rows Fun.id))

(* the pushable block, a square of plinth stone one high *)
let block_shapes (cx : int) (cz : int) : shape3d list =
  let h = match floor_of cx cz with Some n -> float_of_int n | None -> 0. in
  top_face (1, 1) cx cz (h +. 1.) :: List.map (fun d -> side_face (1, 1) cx cz d h (h +. 1.)) sides

(* Torches, for the look of the place, on the chamber's walls -- not in
 * the entrance corridor, which is one square wide: a light there would
 * be closer to the camera than the raider is. *)
let torches : shape3d list =
  List.concat_map
    (fun ((cx, cz), (dx, dz)) ->
      let x, z = world_of cx cz in
      let fx = x +. (0.45 *. float_of_int dx) and fz = z +. (0.45 *. float_of_int dz) in
      [ box (rgb 58 46 34) 0.14 0.3 0.14 |> move3d fx 1.05 fz;
        sphere (rgb 252 198 98) 0.13 |> move3d fx 1.32 fz ])
    [ ((7, 2), (0, 1)); ((2, 3), (1, 0)); ((2, 6), (1, 0)); ((7, 7), (0, -1)); ((12, 6), (-1, 0)) ]

(* the raider herself: boxes, because she is not what this game is
 * about -- her arms go up when she hangs, and that is the whole
 * animation there is *)
let raider_shape (p : Camera3d.pose) (hanging : bool) : shape3d =
  let skin = rgb 224 176 132 and shirt = rgb 90 150 110 and hair = rgb 70 48 34 in
  group3d
    [ box shirt 0.3 0.42 0.22 |> move3d 0. 0.52 0.;
      box (rgb 80 72 64) 0.3 0.2 0.22 |> move3d 0. 0.22 0.;
      sphere skin 0.13 |> move3d 0. 0.86 0.;
      sphere hair 0.14 |> move3d 0. 0.9 0.04;
      box skin 0.09 0.38 0.09 |> move3d (-0.2) (if hanging then 0.72 else 0.5) 0.;
      box skin 0.09 0.38 0.09 |> move3d 0.2 (if hanging then 0.72 else 0.5) 0.;
      box (rgb 60 55 50) 0.11 0.36 0.11 |> move3d (-0.09) 0.05 0.;
      box (rgb 60 55 50) 0.11 0.36 0.11 |> move3d 0.09 0.05 0. ]
  |> rotate3d 0. p.Camera3d.heading 0.
  |> move3d p.Camera3d.x p.Camera3d.y p.Camera3d.z

let view_tomb (g : game) : shape3d list =
  let p = pose g in
  let ix, iz = world_of (fst idol_at) (snd idol_at) in
  [ tomb ] @ torches
  @ block_shapes (fst g.block) (snd g.block)
  @ (if g.idol then [ sphere gold 0.2 |> move3d ix 2.3 iz ] else [])
  @ (match boulder_square g with
    | None -> []
    | Some (cx, cz) ->
        let x, z = world_of cx cz in
        [ sphere (rgb 108 100 90) 0.46 |> move3d x 0.46 z ])
  @ [ raider_shape p g.at.hanging ]

(* behind her and a little above, smoothed: the camera of every
 * third-person game since, and the thing Tomb Raider had to invent
 * because nobody had had to film a character in a corridor before *)
let look (g : game) : camera =
  Camera3d.behind ~back:3.4 ~height:2.2 ~ahead:1.2 ~look:0.6 ~fov:65. (pose g)

let panel (screen : screen) (g : game) : shape list =
  [ text (rgb 200 190 170) 1.7
      (if g.idol then "find the idol" else "get out -- the boulder is behind you")
    |> move_y (screen.top -. 40.);
    text (rgb 130 124 112) 1.5 "arrows turn and walk   space jumps (hold up: a running jump)   x hands"
    |> move_y (screen.bottom +. 34.) ]

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let g = new_game () in
      ( look g,
        view_tomb g
        @ List.map hud
            ([ text (rgb 226 184 68) 6. "TINY TOMB RAIDER" |> move_y 300.;
               text white 2.2 "an idol, a chasm, a block to push, and a boulder" |> move_y 240.;
               text white 2.2 "arrows turn and walk   space jumps   x hands" |> move_y 205. ]
            @ Scene2d.blink 1. s [ text yellow 3. "PRESS SPACE" |> move_y 150. ]) )
  | Playing g -> (look g, view_tomb g @ List.map hud (panel screen g))
  | Lost how ->
      let g = new_game () in
      ( look g,
        view_tomb g
        @ List.map hud
            ([ text (rgb 230 110 100) 5. ("KILLED BY " ^ String.uppercase_ascii how) |> move_y 200. ]
            @ Scene2d.blink 1. s [ text white 2.5 "PRESS SPACE" |> move_y 120. ]) )
  | Escaped frames ->
      let g = new_game () in
      ( look g,
        view_tomb g
        @ List.map hud
            ([ text gold 6. "OUT, WITH THE IDOL" |> move_y 200.;
               text white 3. (Printf.sprintf "%d seconds" (frames / 60)) |> move_y 130. ]
            @ Scene2d.blink 1. s [ text white 2.5 "PRESS SPACE" |> move_y 60. ]) )

let app = game3d view update (Scene2d.start Title)
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
