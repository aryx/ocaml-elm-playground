(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Minecraft (Markus Persson, 2009), after Michael
 * Fogleman's Python/Pyglet clone (fogleman/Minecraft, ~750 lines):
 * walk, jump, fly, and remove and place blocks, in a generated world
 * of ~85000 of them. W/A/S/D to walk, space to jump, Tab to fly, the
 * mouse (or the arrows) to look, left click to remove a block, right
 * click to place one, 1/2/3 to choose which.
 *
 * A voxel world is a grid of cubes, and everything follows from that:
 *
 *  - The world ([world]) is a hash table from a block's integer
 *    position to what it is made of, plus two things kept up to date
 *    beside it: [shown], the blocks with at least one side touching
 *    air ([exposed]) -- a block walled in by six others can never be
 *    seen -- and [sectors], the blocks of each 16 x 16 column, the
 *    world cut into chunks.
 *  - Looking at a block ([hit_test]) is a walk along the sight line,
 *    an eighth of a block at a time, until a cell of the table is
 *    taken: that is how a click knows what to remove, and the cell
 *    just before it is where a new block goes.
 *  - Walking ([collide]) is the same grid again: the player is pushed
 *    back out of any cell their body overlaps by more than a quarter
 *    of a block. No geometry, just table lookups.
 *  - Drawing: one [cached3d] shape per chunk, built once ([chunk_shape],
 *    [rebuild_chunks_around] after an edit), and each block's sides
 *    that touch another block are left out ([hidden_face_culling]).
 *    Rebuilding every block's shape every frame, the way this
 *    project's other 3D games do, measured about 0.2 frames a second;
 *    chunks kept in the GPU's buffers made it a millisecond (see
 *    docs/claude_notes/plans/done/plan_opengl_perf.md).
 *
 * The blocks are textured from one image, an "atlas" of 4 x 4 cells
 * (minecraft.png, the original's), a cell per kind of side: grass on
 * top, dirt below, grass-over-dirt around ([atlas_cells_of_block]).
 * One image, one texture, however many blocks.
 *
 * What the original has and this doesn't: the queue that spreads world
 * generation over several frames (nothing here needs it, see
 * [init_shown]), fog, and sectors shown and hidden as the player
 * moves -- the chunks are all drawn here, and the GPU backends keep
 * them.
 *
 * Uses: playground3d's cached3d, textured polygons and hud. Not the
 * Segments or the sectors kit (a voxel grid is neither), not Camera3d
 * (the camera is the player's eyes).
 *
 * References: fogleman/Minecraft's main.py, which this follows class by
 * class (its Model is the world section below, its Window's movement
 * the player section); "Meshing in a Minecraft Game" (Mikola Lysenko,
 * 0fps.net, 2012) for what to do beyond [hidden_face_culling]:
 * merging neighboring faces into bigger rectangles, "greedy meshing".
 *
 * Exercises: greedy meshing; fog, and drawing only the chunks near the
 * player (the original's show/hide by sector); saving the world to a
 * file; water; a chunk rebuilt in the background rather than in the
 * frame that edits it.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The world: its blocks, and which of them show *)
(*****************************************************************************)


type block = Grass | Sand | Brick | Stone

(* a block position, always integer coordinates (see [normalize] for
 * how an arbitrary-precision player position rounds down to one) *)
type pos = int * int * int

type world = {
  (* every block that exists, world position -> what it is made of *)
  blocks : (pos, block) Hashtbl.t;
  (* the blocks of [blocks] with a side touching air (see [exposed]):
   * the ones worth drawing at all *)
  shown : (pos, block) Hashtbl.t;
  (* [sectorize position] -> every block inside that sector, 16 x 16
   * columns of the world: the chunks a cached3d is built per
   * ([chunk_shape]), and rebuilt for after an edit. The original shows
   * and hides them as the player moves; here they are all drawn, and
   * the GPU backends keep them. *)
  sectors : (pos, pos list ref) Hashtbl.t;
}

let sector_size = 16

let create () : world =
  { blocks = Hashtbl.create 8192; shown = Hashtbl.create 2048; sectors = Hashtbl.create 64 }

(* claude: Python's `//` floors towards negative infinity; OCaml's `/`
 * truncates towards zero -- these disagree for negative operands
 * (e.g. -1 // 16 = -1 in Python, but -1 / 16 = 0 in OCaml), which
 * would otherwise silently put the blocks at negative coordinates in
 * the wrong sector, and so in the wrong chunk. *)
let floor_div (a : int) (b : int) : int =
  let q = a / b and r = a mod b in
  if r <> 0 && (r < 0) <> (b < 0) then q - 1 else q

let sectorize ((x, _y, z) : pos) : pos = (floor_div x sector_size, 0, floor_div z sector_size)

(* accepts a position of arbitrary precision (e.g. the player's
 * floating-point position) and returns the integer block position
 * containing it *)
let normalize ((x, y, z) : float * float * float) : pos =
  (int_of_float (Float.round x), int_of_float (Float.round y), int_of_float (Float.round z))

(* the 6 axis-aligned neighbour directions of a block (the original's
 * FACES: each one is also the side of the block that looks that way) *)
let neighbours : pos list = [ (0, 1, 0); (0, -1, 0); (-1, 0, 0); (1, 0, 0); (0, 0, 1); (0, 0, -1) ]

(* a block is "exposed" (needs to be drawn) if at least one of its 6
 * neighbors is empty (not in [world]) -- a block fully surrounded by
 * other blocks can never be seen, so there is no point drawing it *)
let exposed (m : world) ((x, y, z) : pos) : bool =
  neighbours |> List.exists (fun (dx, dy, dz) -> not (Hashtbl.mem m.blocks (x + dx, y + dy, z + dz)))

let sector_add (m : world) (position : pos) : unit =
  let sector = sectorize position in
  match Hashtbl.find_opt m.sectors sector with
  | Some positions -> positions := position :: !positions
  | None -> Hashtbl.add m.sectors sector (ref [ position ])

let sector_remove (m : world) (position : pos) : unit =
  match Hashtbl.find_opt m.sectors (sectorize position) with
  | Some positions -> positions := List.filter (fun p -> p <> position) !positions
  | None -> ()

let show_block (m : world) (position : pos) : unit = Hashtbl.replace m.shown position (Hashtbl.find m.blocks position)
let hide_block (m : world) (position : pos) : unit = Hashtbl.remove m.shown position

(* check every neighbor of [position] and make sure its shown/hidden
 * state is up to date -- called after a block is added or removed,
 * since that can newly expose or newly hide any of its 6 neighbors *)
let check_neighbors (m : world) ((x, y, z) : pos) : unit =
  neighbours
  |> List.iter (fun (dx, dy, dz) ->
         let key = (x + dx, y + dy, z + dz) in
         if Hashtbl.mem m.blocks key then
           if exposed m key then (if not (Hashtbl.mem m.shown key) then show_block m key)
           else if Hashtbl.mem m.shown key then hide_block m key)

let rec add_block ?(immediate = true) (m : world) (position : pos) (texture : block) : unit =
  if Hashtbl.mem m.blocks position then remove_block ~immediate m position;
  Hashtbl.replace m.blocks position texture;
  sector_add m position;
  if immediate then begin
    if exposed m position then show_block m position;
    check_neighbors m position
  end

and remove_block ?(immediate = true) (m : world) (position : pos) : unit =
  Hashtbl.remove m.blocks position;
  sector_remove m position;
  if immediate then begin
    Hashtbl.remove m.shown position;
    check_neighbors m position
  end

(* line-of-sight search from [position] along [vector]: returns the
 * first solid block hit (and the last empty position just before it,
 * i.e. where a new block would go if placed), or None if nothing is
 * hit within [max_distance] blocks. *)
let hit_test (m : world) ~(position : float * float * float) ~(vector : float * float * float)
    ?(max_distance = 8) () : (pos * pos option) option =
  let steps_per_block = 8 in
  let (vx, vy, vz) = vector in
  let (dx, dy, dz) =
    (vx /. float_of_int steps_per_block, vy /. float_of_int steps_per_block, vz /. float_of_int steps_per_block)
  in
  let rec loop (x, y, z) (previous : pos option) (steps_left : int) : (pos * pos option) option =
    if steps_left <= 0 then None
    else
      let key = normalize (x, y, z) in
      if Some key <> previous && Hashtbl.mem m.blocks key then Some (key, previous)
      else loop (x +. dx, y +. dy, z +. dz) (Some key) (steps_left - 1)
  in
  loop position None (max_distance * steps_per_block)

(*****************************************************************************)
(* World generation *)
(*****************************************************************************)

(* Python's random.randint(a, b) is inclusive of b; OCaml's Random.int
 * n is exclusive of n. *)
let randint (a : int) (b : int) : int = a + Random.int (b - a + 1)
let choice (options : 'a array) : 'a = options.(Random.int (Array.length options))

(* a flat grass-over-stone base, solid stone outer walls, and a
 * scattering of randomly-placed, randomly-colored "hills" -- a
 * line-for-line port of the original's `_initialize`, sizes/counts
 * unchanged. *)
let initialize (m : world) : unit =
  let n = 80 in
  (* half-width/height of the world *)
  for x = -n to n do
    for z = -n to n do
      add_block ~immediate:false m (x, -2, z) Grass;
      add_block ~immediate:false m (x, -3, z) Stone;
      if x = -n || x = n || z = -n || z = n then
        (* outer walls *)
        for dy = -2 to 2 do
          add_block ~immediate:false m (x, dy, z) Stone
        done
    done
  done;
  (* hills *)
  let o = n - 10 in
  for _ = 1 to 120 do
    let a = randint (-o) o (* hill center x *) in
    let b = randint (-o) o (* hill center z *) in
    let c = -1 (* hill base y *) in
    let h = randint 1 6 (* hill height *) in
    let side = ref (randint 4 8) (* 2*side is the hill's side length, tapering as it rises *) in
    let block = choice [| Grass; Sand; Brick |] in
    for y = c to c + h - 1 do
      let s = !side in
      for x = a - s to a + s do
        for z = b - s to b + s do
          let within_hill_radius = ((x - a) * (x - a)) + ((z - b) * (z - b)) <= (s + 1) * (s + 1) in
          let outside_spawn_clearing = (x * x) + (z * z) >= 25 in
          if within_hill_radius && outside_spawn_clearing then add_block ~immediate:false m (x, y, z) block
        done
      done;
      decr side
    done
  done

(* recomputes [shown] for the whole world at once -- the equivalent
 * end state of the original's `process_entire_queue()` after
 * `_initialize`, without needing the incremental queue (see this
 * file's own header comment). *)
let init_shown (m : world) : unit =
  Hashtbl.iter (fun position texture -> if exposed m position then Hashtbl.replace m.shown position texture) m.blocks

let create_world () : world =
  let m = create () in
  initialize m;
  init_shown m;
  m

(*****************************************************************************)
(* The player: looking, walking, falling *)
(*****************************************************************************)

(* Where the player is, where they look, and how they move: walking,
 * jumping, falling, flying, and bumping into blocks -- the original's
 * get_sight_vector, get_motion_vector, _update and collide, same
 * constants, same behavior.
 *
 * Conventions, the original's (OpenGL's): y is up; yaw 0 looks along
 * -z, and a positive yaw turns right (towards +x); pitch is in
 * -90..90, positive looking up. [position] is the player's eyes; their
 * body is [player_height] = 2 blocks tall, from the eyes down.
 *
 *              -z (yaw 0)
 *               ^
 *               |
 *   -x  <-------+------->  +x (yaw 90)
 *               |
 *              +z (yaw 180)
 *)

type player = { position : float * float * float; yaw : float; pitch : float; dy : float; flying : bool }

let initial_player = { position = (0., 0., 0.); yaw = 0.; pitch = 0.; dy = 0.; flying = false }

type input = { forward : int; right : int; jump : bool }

(*****************************************************************************)
(* Constants (the original's) *)
(*****************************************************************************)

let walking_speed = 5. (* blocks per second *)
let flying_speed = 15.
let gravity = 20. (* blocks per second, per second *)

(* the speed that reaches max_jump_height = 1 block: from v^2 = 2 g h
 * (the original derives it the same way, see its comment) *)
let jump_speed = sqrt (2. *. gravity *. 1.)
let terminal_velocity = 50.
let player_height = 2 (* blocks *)

let radians (degrees : float) : float = degrees *. Float.pi /. 180.

(*****************************************************************************)
(* Looking and moving *)
(*****************************************************************************)

(* [m] is 1 looking horizontally, 0 straight up or down: the horizontal
 * part of the unit vector *)
let sight_vector (p : player) : float * float * float =
  let m = cos (radians p.pitch) in
  (cos (radians (p.yaw -. 90.)) *. m, sin (radians p.pitch), sin (radians (p.yaw -. 90.)) *. m)

(* The direction of the player's motion, a unit vector (or zero).
 * Walking: in the horizontal plane, the angle of the keys (forward,
 * right, or a diagonal) added to the yaw. Flying: forward and back
 * follow the pitch too (fly up by looking up), strafing stays
 * horizontal. *)
let motion_vector (p : player) (input : input) : float * float * float =
  if input.forward = 0 && input.right = 0 then (0., 0., 0.)
  else
    (* the original's strafe: [0] is -1 forward, +1 back; [1] -1 left,
     * +1 right; atan2 of them is the keys' angle relative to the yaw:
     * -90 forward, 0 right, 90 back, 180 left *)
    let strafe = atan2 (float_of_int (-input.forward)) (float_of_int input.right) *. 180. /. Float.pi in
    let angle = radians (p.yaw +. strafe) in
    if p.flying then
      let (m, dy) = if input.right <> 0 then (1., 0.) else (cos (radians p.pitch), sin (radians p.pitch)) in
      let dy = if input.forward < 0 then -.dy else dy in
      (cos angle *. m, dy, sin angle *. m)
    else (cos angle, 0., sin angle)

(*****************************************************************************)
(* Collisions *)
(*****************************************************************************)
(* The world is a grid, so collision detection is simple: look at the
 * 6 cells next to the player's (for each of the 2 blocks of their
 * body), and if one is a block and the player overlaps it, push them
 * back out along that axis. [pad]: how much overlap counts; 0 would
 * collide as soon as touching a block, 0.49 would let the player sink
 * into the ground like into tall grass, 0.5 and more would let them
 * fall through it. *)
let pad = 0.25

let collide (world : world) (player : player) : player =
  let (x, y, z) = player.position in
  let p = [| x; y; z |] in
  let (nx, ny, nz) = normalize player.position in
  let np = [| nx; ny; nz |] in
  let dy = ref player.dy in
  neighbours
  |> List.iter (fun (fx, fy, fz) ->
         let face = [| fx; fy; fz |] in
         for i = 0 to 2 do
           if face.(i) <> 0 then begin
             (* how far into the neighboring cell, along this axis *)
             let d = (p.(i) -. float_of_int np.(i)) *. float_of_int face.(i) in
             if d >= pad then begin
               (* each block of the body, from the eyes down *)
               let hit = ref false in
               for body = 0 to player_height - 1 do
                 if not !hit then begin
                   let op = Array.copy np in
                   op.(1) <- op.(1) - body;
                   op.(i) <- op.(i) + face.(i);
                   if Hashtbl.mem world.blocks (op.(0), op.(1), op.(2)) then begin
                     hit := true;
                     p.(i) <- p.(i) -. ((d -. pad) *. float_of_int face.(i));
                     (* the ground or a ceiling: stop falling / rising *)
                     if fy <> 0 then dy := 0.
                   end
                 end
               done
             end
           end
         done);
  { player with position = (p.(0), p.(1), p.(2)); dy = !dy }

(*****************************************************************************)
(* One step *)
(*****************************************************************************)

let substep (world : world) (dt : float) (input : input) (player : player) : player =
  let speed = if player.flying then flying_speed else walking_speed in
  let (mx, my, mz) = motion_vector player input in
  let d = dt *. speed in
  let (dx, dy, dz) = (mx *. d, my *. d, mz *. d) in
  (* gravity: falling faster until the terminal velocity; jumping,
   * slowing down until falling *)
  let (dy, vy) =
    if player.flying then (dy, player.dy)
    else
      let vy = Float.max (player.dy -. (dt *. gravity)) (-.terminal_velocity) in
      (dy +. (vy *. dt), vy)
  in
  let (x, y, z) = player.position in
  collide world { player with position = (x +. dx, y +. dy, z +. dz); dy = vy }

let step (world : world) ~(dt : float) (input : input) (player : player) : player =
  (* a jump only from the ground (or a ceiling...), like the original *)
  let player = if input.jump && player.dy = 0. then { player with dy = jump_speed } else player in
  let dt = Float.min dt 0.2 in
  let substeps = 8 in
  let rec loop n player = if n = 0 then player else loop (n - 1) (substep world (dt /. float_of_int substeps) input player) in
  loop substeps player

(*****************************************************************************)
(* The player, with the engine *)
(*****************************************************************************)

(* claude: physics=engine (?physics=engine in a browser; see
 * Playground.flags), the pattern of StarCollector3d.ml and
 * TinyMarbleMadness.ml. By hand, the default, the player is the
 * original's: [collide] pushes the body out of the blocks next to it,
 * one axis at a time. With the engine it is Character3d,
 * the capsule controller (plan_physics3d_teaching.md phase 9): Quake's
 * loop of trace and slide against the blocks around it, turned into
 * boxes each frame ([solids_near]), a step offset, and a ground check.
 *
 * The sizes are Minecraft's own rather than the Python original's
 * (whose body is the two cells under the eyes, a quarter of a block
 * from the walls): 1.8 blocks tall, 0.6 wide, the eyes at 1.6, and a
 * step of 0.6 -- so a slab would be walked up and a block still has to
 * be jumped, as in the real game. Flying (Tab) stays the original's:
 * it is a way of moving through the world, not physics. *)

type engine = By_hand | Engine

(* from computer.flags, not the command line at load time: a test linking
 * this game has a command line of its own *)
let engine_of (flags : flags) : engine =
  match List.assoc_opt "physics" flags with Some "engine" -> Engine | _ -> By_hand

let eye = 1.6

(* the blocks around the character, as boxes: a block at (i, j, k) is
 * the unit cube centred there *)
let solids_near (w : world) (c : Character3d.t) : Physics3d.body list =
  let ci = int_of_float (Float.round c.x) and cj = int_of_float (Float.round c.y) and ck = int_of_float (Float.round c.z) in
  let found = ref [] in
  for i = ci - 2 to ci + 2 do
    for j = cj - 2 to cj + 3 do
      for k = ck - 2 to ck + 2 do
        if Hashtbl.mem w.blocks (i, j, k) then
          found :=
            (Physics3d.body (box white 1. 1. 1.)
            |> Physics3d.at (float_of_int i) (float_of_int j) (float_of_int k)
            |> Physics3d.immovable)
            :: !found
      done
    done
  done;
  !found

let walker_of (p : player) : Character3d.t =
  let x, y, z = p.position in
  { (Character3d.make ~radius:0.3 ~height:1.8 ~step:0.6 x (y -. eye) z) with vy = p.dy }

(* one tick of walking, by the engine: the same keys and speeds as
 * [step], the same jump, reaching one block *)
let step_engine (w : world) (input : input) (c : Character3d.t) (p : player) : Character3d.t * player =
  let mx, _, mz = motion_vector p input in
  let c =
    Character3d.walk ~gravity ~jump:(if input.jump then jump_speed else 0.) (solids_near w c)
      (mx *. walking_speed, mz *. walking_speed)
      c
  in
  (c, { p with position = (c.x, c.y +. eye, c.z); dy = c.vy })

(*****************************************************************************)
(* The blocks, drawn: the texture atlas and the chunks *)
(*****************************************************************************)


(* the texture atlas, carried inside the program (the game's dune file turns
 * minecraft.png into Texture_atlas.base64 at build time): no file to
 * find at run time, wherever the game is started from, and the browser
 * gets it as a "data:" URL *)
let atlas_src = embedded_texture ~name:"minecraft-atlas" ~base64:Texture_atlas.base64
let atlas_n = 4

(* claude: converts the original's (col, gl_row) cell addressing (see
 * the header comment) to a (u0,v0)-(u1,v1) rect in this
 * project's own v=0-is-top convention: file_row = (atlas_n-1) - gl_row. *)
let uv_rect_of_cell (col, gl_row) =
  let file_row = atlas_n - 1 - gl_row in
  let m = 1. /. float_of_int atlas_n in
  let u0 = float_of_int col *. m and v0 = float_of_int file_row *. m in
  (u0, v0, u0 +. m, v0 +. m)

(* the original's GRASS/SAND/BRICK/STONE tex_coords(top, bottom, side)
 * calls, as plain (col, gl_row) cell tuples -- kept exactly as the
 * original's literal values for easy side-by-side comparison. *)
let atlas_cells_of_block : block -> (int * int) * (int * int) * (int * int) = function
  | Grass -> ((1, 0), (0, 1), (0, 0)) (* top, bottom, side *)
  | Sand -> ((1, 1), (1, 1), (1, 1))
  | Brick -> ((2, 0), (2, 0), (2, 0))
  | Stone -> ((2, 1), (2, 1), (2, 1))

(* claude: box_faces (the shared 6-corners-per-face builder box/cube/
 * textured_cube use) is deliberately kept private to Playground3d.ml
 * -- see its own doc comment there ("exposed as a real primitive
 * rather than leaving box_faces private" is about box itself, not
 * box_faces). Per plan_tiny_minecraft.md's own design decision ("A
 * textured_box_faces helper local to the Minecraft port, not a new
 * library primitive"), this is a small local duplicate rather than a
 * new addition to the shared library -- same corner points/winding as
 * box_faces (CCW as seen from outside, for correct backface culling),
 * just paired with an atlas sub-rect per face instead of textured_cube's
 * single full-image UV rect. *)
let block_faces (size : number) : (number * number * number) list list =
  let h = size /. 2. in
  let p000 = (-.h, -.h, -.h)
  and p001 = (-.h, -.h, h)
  and p010 = (-.h, h, -.h)
  and p011 = (-.h, h, h)
  and p100 = (h, -.h, -.h)
  and p101 = (h, -.h, h)
  and p110 = (h, h, -.h)
  and p111 = (h, h, h) in
  [ [ p010; p011; p111; p110 ] (* +Y, top *)
  ; [ p000; p100; p101; p001 ] (* -Y, bottom *)
  ; [ p100; p110; p111; p101 ] (* +X, side *)
  ; [ p001; p011; p010; p000 ] (* -X, side *)
  ; [ p001; p101; p111; p011 ] (* +Z, side *)
  ; [ p000; p010; p110; p100 ] (* -Z, side *)
  ]

let textured_face (cell : int * int) = function
  | [ p0; p1; p2; p3 ] ->
      let (u0, v0, u1, v1) = uv_rect_of_cell cell in
      { alpha = 1.; material = matte; form = TexturedPolygon3d (atlas_src, [ (p0, (u0, v0)); (p1, (u1, v0)); (p2, (u1, v1)); (p3, (u0, v1)) ]) }
  | _ -> assert false

(* claude: the direction of each of block_faces's faces, in the same
 * order: the neighbor on that side *)
let face_directions : pos list = [ (0, 1, 0); (0, -1, 0); (1, 0, 0); (-1, 0, 0); (0, 0, 1); (0, 0, -1) ]

(* claude: hidden-face culling -- only the faces with no block in front
 * of them, i.e. the ones touching air: a face pressed against a
 * neighbor can never be seen. Most shown blocks have 1 or 2 faces left
 * (the ground: just its top), so this divides the geometry by several.
 * The original Python version doesn't do it (the GPU copes), but it
 * costs nothing (6 lookups per block, once) and every backend gains.
 * Not the same thing as exposed, which decides whether
 * a block has at least one such face, i.e. whether it's in [shown] at
 * all. Set to false to see the difference (-debug logs the vertex
 * counts). The next step, merging neighboring coplanar faces into big
 * rectangles, is "greedy meshing": M. Lysenko, "Meshing in a Minecraft
 * Game", 0fps.net, 2012, which also starts from this culling. *)
let hidden_face_culling = true

let block_shape (m : world) ((x, y, z) : pos) (block : block) : shape3d =
  let (top, bottom, side) = atlas_cells_of_block block in
  let cells = [ top; bottom; side; side; side; side ] in
  let hidden (dx, dy, dz) = hidden_face_culling && Hashtbl.mem m.blocks (x + dx, y + dy, z + dz) in
  List.combine (List.combine (block_faces 1.) cells) face_directions
  |> List.filter_map (fun ((face, cell), dir) -> if hidden dir then None else Some (textured_face cell face))
  |> group3d
  |> move3d (float_of_int x) (float_of_int y) (float_of_int z)

(* claude: the world as chunks, one per sector (a 16x16 column, see
 * sectorize), each a Playground3d.cached3d built once,
 * here: the GPU backends upload each chunk once and on later frames
 * only draw it again (see docs/claude_notes/plan_opengl_perf.md), so
 * view's only work is returning this list. Rebuilding every block's
 * shape in view instead, every frame, took seconds per frame. Chunks
 * rather than one cached3d for the whole world so that an edit only
 * rebuilds the chunks it touches (see rebuild_chunks_around). *)
let chunk_shape (m : world) (sector : pos) : shape3d =
  let positions = match Hashtbl.find_opt m.sectors sector with Some ps -> !ps | None -> [] in
  positions
  |> List.filter_map (fun pos -> Hashtbl.find_opt m.shown pos |> Option.map (fun block -> block_shape m pos block))
  |> cached3d

(*****************************************************************************)
(* The world *)
(*****************************************************************************)
(* claude: the world, and its chunks, are the one mutable part of this
 * game, like the original's Model object: the hash tables above,
 * changed in place by add_block/remove_block, and [chunks],
 * sector -> its cached3d. The rest of the state (the player, what's
 * selected) is an ordinary immutable model, below. *)

let world = create_world ()
let chunks : (pos, shape3d) Hashtbl.t = Hashtbl.create 128

(* claude: the chunks are not all built at once. Building one means
 * walking its blocks and making their faces; drawing it the first time
 * means the backend sending it to the GPU -- about 120 chunks of an
 * 85000-block world, which on the WebGL backend took tens of seconds
 * in one go, a frozen page before anything showed. So a few are built
 * each frame ([build_some], called from [update]) and the world grows
 * in over a couple of seconds, which is what the original's queue does
 * too (see the note on [init_shown]: this is the one thing that queue
 * was for that is worth keeping). *)
let to_build : pos Queue.t = Queue.create ()
let () = Hashtbl.iter (fun sector _ -> Queue.push sector to_build) world.sectors

let build_some () : unit =
  for _ = 1 to 2 do
    match Queue.take_opt to_build with
    | Some sector -> Hashtbl.replace chunks sector (chunk_shape world sector)
    | None -> ()
  done

(* claude: after an edit at [pos], the chunks that may look different:
 * [pos]'s own, and those of its 6 neighbors, whose exposed faces
 * changed (a neighbor can be in the next sector). Each gets a new
 * cached3d; the GPU backends free the old one's buffers by themselves,
 * since view stops returning it (Mesh_cache's sweep). *)
let rebuild_chunks_around ((x, y, z) : pos) : unit =
  (x, y, z) :: List.map (fun (dx, dy, dz) -> (x + dx, y + dy, z + dz)) neighbours
  |> List.map sectorize
  |> List.sort_uniq compare
  |> List.iter (fun sector -> Hashtbl.replace chunks sector (chunk_shape world sector))

(*****************************************************************************)
(* The game *)
(*****************************************************************************)
(* Controls (the original's):
 *  - W/A/S/D: walk; space: jump; Tab: fly or walk (flying, look up or
 *    down to go up or down);
 *  - the mouse (captured: Escape to get it back, a click to capture it
 *    again), or the arrow keys: look around;
 *  - left click: remove the block under the crosshair (not stone);
 *    right click: place one in front of it; 1/2/3: brick, grass, sand. *)

type model = {
  player : player;
  (* with physics=engine, walking: the capsule the engine moves *)
  walker : Character3d.t option;
  (* what a right click places *)
  block : block;
  (* the previous frame's time, buttons and Tab key: to know how much
   * time passed, and to act once per press rather than on every frame
   * a button is held *)
  last_time : number option;
  was_down : bool;
  was_right_down : bool;
  was_tab : bool;
}

let initial_model : model =
  {
    player = initial_player;
    walker = None;
    block = Brick;
    last_time = None;
    was_down = false;
    was_right_down = false;
    was_tab = false;
  }

let inventory : (string * block) list = [ ("1", Brick); ("2", Grass); ("3", Sand) ]

let bool_int (b : bool) : int = if b then 1 else 0

(* -1., 0. or 1., from two opposite keys *)
let axis (plus : bool) (minus : bool) : number = float_of_int (bool_int plus - bool_int minus)

(* claude: where the player looks, turned by how far the mouse moved
 * (mdx/mdy, the mouse being captured, see [main]), 0.15 degree per
 * pixel, the original's on_mouse_motion; and by the arrow keys, 2
 * degrees per frame. Not by where the mouse is (mx/my): that stops at
 * the window's edges, and when the mouse leaves the window. *)
let look (computer : computer) (m : model) : number * number =
  let kb = computer.keyboard in
  let yaw = m.player.yaw +. (0.15 *. computer.mouse.mdx) +. (2. *. axis kb.kright kb.kleft) in
  let pitch = m.player.pitch +. (0.15 *. computer.mouse.mdy) +. (2. *. axis kb.kup kb.kdown) in
  (yaw, Float.max (-89.) (Float.min 89. pitch))

let key_down (computer : computer) (key : string) : bool = Set_.mem key computer.keyboard.keys

(* the block under the crosshair, and the empty cell in front of it *)
let target (m : model) = hit_test world ~position:m.player.position ~vector:(sight_vector m.player) ()

let edit (computer : computer) (m : model) : unit =
  let clicked = computer.mouse.mdown && not m.was_down in
  let right_clicked = computer.mouse.mrdown && not m.was_right_down in
  match target m with
  | Some (block_pos, _) when clicked && Hashtbl.find world.blocks block_pos <> Stone ->
      remove_block world block_pos;
      rebuild_chunks_around block_pos
  | Some (_, Some empty_pos) when right_clicked ->
      add_block world empty_pos m.block;
      rebuild_chunks_around empty_pos
  | _ -> ()

let update (computer : computer) (m : model) : model =
  let kb = computer.keyboard in
  let (yaw, pitch) = look computer m in
  let tab = key_down computer "tab" || key_down computer "Tab" in
  let flying = if tab && not m.was_tab then not m.player.flying else m.player.flying in
  let player = { m.player with yaw; pitch; flying } in
  let block = List.fold_left (fun b (key, block) -> if key_down computer key then block else b) m.block inventory in
  let (Time now) = computer.time in
  let dt = match m.last_time with Some last -> now -. last | None -> 0. in
  let input : input =
    { forward = bool_int kb.kw - bool_int kb.ks; right = bool_int kb.kd - bool_int kb.ka; jump = kb.kspace }
  in
  let walker, player =
    match (engine_of computer.flags, player.flying) with
    | Engine, false ->
        let c = match m.walker with Some c -> c | None -> walker_of player in
        let c, player = step_engine world input c player in
        (Some c, player)
    | _ -> (None, step world ~dt input player)
  in
  let m = { m with player; walker; block } in
  build_some ();
  edit computer m;
  { m with last_time = Some now; was_down = computer.mouse.mdown; was_right_down = computer.mouse.mrdown; was_tab = tab }

(* claude: black edges around the block under the crosshair, the
 * original's draw_focused_block, so you see which block a click will
 * remove. It draws a cube a bit bigger than the block (0.51 instead of
 * 0.5 from its center, so the edges aren't hidden inside its faces) in
 * wireframe mode; the API has no per-shape wireframe (only a
 * whole-scene debug key), so here the 12 edges are thin boxes (a box,
 * unlike a flat polygon, is visible from any side, see
 * Playground3d.box):
 *
 *        +-----------+       4 edges along x (top and bottom,
 *       /|          /|       front and back), 4 along y, 4 along z;
 *      / |         / |       each [thickness] thick, [length] long,
 *     +-----------+  |       centered at +-h or -h on the 2 other axes
 *     |  +--------|--+
 *     | /         | /
 *     |/          |/
 *     +-----------+
 *
 * Not cached: it moves with the view. *)
let outline ((x, y, z) : pos) : shape3d =
  let h = 0.51 and thickness = 0.03 in
  let length = (2. *. h) +. thickness in
  let edge (dx, dy, dz) (ox, oy, oz) = box black dx dy dz |> move3d ox oy oz in
  let corners = [ (-.h, -.h); (-.h, h); (h, -.h); (h, h) ] in
  List.concat_map
    (fun (a, b) ->
      [
        edge (length, thickness, thickness) (0., a, b);
        edge (thickness, length, thickness) (a, 0., b);
        edge (thickness, thickness, length) (a, b, 0.);
      ])
    corners
  |> group3d
  |> move3d (float_of_int x) (float_of_int y) (float_of_int z)

let crosshair : shape3d =
  hud (group [ rectangle black 20. 2.; rectangle black 2. 20. ])

let status (computer : computer) (m : model) : shape3d =
  let (x, y, z) = m.player.position in
  let block = match m.block with Brick -> "brick" | Grass -> "grass" | Sand -> "sand" | Stone -> "stone" in
  let building = Queue.length to_build in
  hud
    (words black
       (Printf.sprintf "%s  (%.0f, %.0f, %.0f)%s%s" block x y z
          (if m.player.flying then "  flying" else "")
          (if building > 0 then Printf.sprintf "  building %d chunks" building else ""))
    |> move (computer.screen.left +. 150.) (computer.screen.top -. 30.))

let view (computer : computer) (m : model) : camera * shape3d list =
  let (x, y, z) = m.player.position and (sx, sy, sz) = sight_vector m.player in
  (* the original's field of view, 65 degrees; far enough for the whole
   * world, which it cuts at 60 blocks behind fog instead *)
  let cam = camera ~eye:(x, y, z) ~target:(x +. sx, y +. sy, z +. sz) ~fov:65. ~far:300. () in
  let targeted = match target m with Some (pos, _) -> [ outline pos ] | None -> [] in
  (cam, Hashtbl.fold (fun _ chunk l -> chunk :: l) chunks [] @ targeted @ [ crosshair; status computer m ])

let app = game3d view update initial_model

(* claude: sharp texels, like the original's GL_NEAREST: bilinear
 * filtering blurs the pixel-art blocks, and blends each atlas cell with
 * its neighbors in the atlas along its borders *)
let main =
  Playground3d_platform.run_app3d ~rendering:{ default_rendering with smooth_textures = false } ~capture_mouse:true
    ~flags:(Playground_platform.flags ()) app
