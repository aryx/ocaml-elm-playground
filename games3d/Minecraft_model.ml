(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Phase 1 of docs/claude_notes/plan_tiny_minecraft.md: a direct,
 * mostly line-for-line port of ~/software-src/game/tiny-minecraft/main.py's
 * `Model` class -- the world data structure (which blocks exist, which
 * are currently visible, sector-based spatial partitioning) and world
 * generation. Deliberately independent of Playground3d/game3d entirely
 * (no rendering at all here), so it can be tested standalone before
 * any graphics are involved -- see Minecraft3d.ml (added in a later
 * phase) for the game3d wiring on top of this.
 *
 * Deliberate simplification vs. the original: dropped the pyglet-
 * specific incremental show/hide queue (self.queue/_enqueue/_dequeue/
 * process_queue/process_entire_queue/show_sector/hide_sector/
 * change_sectors) entirely. That machinery exists in the original
 * purely to spread the cost of building a Batch's vertex lists across
 * multiple frames, so world generation doesn't freeze the game loop --
 * an optimization for a rendering strategy (a persistent, incrementally
 * updated GPU vertex batch) this port doesn't use (yet -- see the
 * plan's "rebuild vs. cache" discussion: Phase 2 rebuilds every shown
 * block's shape3d from scratch every frame, at least initially).
 * Without a persistent batch to keep in sync, there's nothing to
 * defer: world generation here just calls [add_block] for every block
 * (immediate:false, matching the original), then a single [init_shown]
 * pass recomputes [shown] for the whole world at once -- reaching the
 * same end state as the original's `process_entire_queue()` call,
 * without the queue. *)

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

type block = Grass | Sand | Brick | Stone

(* a block position, always integer coordinates (see [normalize] for
 * how an arbitrary-precision player position rounds down to one) *)
type pos = int * int * int

(*****************************************************************************)
(* The world *)
(*****************************************************************************)

type t = {
  (* every block that exists, world position -> its texture/type *)
  world : (pos, block) Hashtbl.t;
  (* the subset of [world] that is currently exposed (see [exposed])
   * and therefore needs to actually be drawn -- see Minecraft3d.ml *)
  shown : (pos, block) Hashtbl.t;
  (* [sectorize position] -> every position in [world] inside that
   * sector -- SECTOR_SIZE-sized spatial buckets, kept for parity with
   * the original (used there to incrementally show/hide sectors as
   * the player moves; not yet exercised by this phase, since rendering
   * so far just walks the whole [shown] table every frame) *)
  sectors : (pos, pos list ref) Hashtbl.t;
}

let sector_size = 16

let create () : t =
  { world = Hashtbl.create 8192; shown = Hashtbl.create 2048; sectors = Hashtbl.create 64 }

(* claude: Python's `//` floors towards negative infinity; OCaml's `/`
 * truncates towards zero -- these disagree for negative operands
 * (e.g. -1 // 16 = -1 in Python, but -1 / 16 = 0 in OCaml), which
 * would silently put negative-coordinate blocks in the wrong sector
 * (sectorize is only used for the not-yet-exercised sectors table in
 * this phase, but getting it right now avoids a latent bug once
 * Phase 3+ actually reads [sectors]). *)
let floor_div (a : int) (b : int) : int =
  let q = a / b and r = a mod b in
  if r <> 0 && (r < 0) <> (b < 0) then q - 1 else q

let sectorize ((x, _y, z) : pos) : pos = (floor_div x sector_size, 0, floor_div z sector_size)

(* accepts a position of arbitrary precision (e.g. the player's
 * floating-point position) and returns the integer block position
 * containing it *)
let normalize ((x, y, z) : float * float * float) : pos =
  (int_of_float (Float.round x), int_of_float (Float.round y), int_of_float (Float.round z))

(* the 6 axis-aligned neighbor directions of a block *)
let faces : pos list = [ (0, 1, 0); (0, -1, 0); (-1, 0, 0); (1, 0, 0); (0, 0, 1); (0, 0, -1) ]

(* a block is "exposed" (needs to be drawn) if at least one of its 6
 * neighbors is empty (not in [world]) -- a block fully surrounded by
 * other blocks can never be seen, so there is no point drawing it *)
let exposed (m : t) ((x, y, z) : pos) : bool =
  faces |> List.exists (fun (dx, dy, dz) -> not (Hashtbl.mem m.world (x + dx, y + dy, z + dz)))

let sector_add (m : t) (position : pos) : unit =
  let sector = sectorize position in
  match Hashtbl.find_opt m.sectors sector with
  | Some positions -> positions := position :: !positions
  | None -> Hashtbl.add m.sectors sector (ref [ position ])

let sector_remove (m : t) (position : pos) : unit =
  match Hashtbl.find_opt m.sectors (sectorize position) with
  | Some positions -> positions := List.filter (fun p -> p <> position) !positions
  | None -> ()

let show_block (m : t) (position : pos) : unit = Hashtbl.replace m.shown position (Hashtbl.find m.world position)
let hide_block (m : t) (position : pos) : unit = Hashtbl.remove m.shown position

(* check every neighbor of [position] and make sure its shown/hidden
 * state is up to date -- called after a block is added or removed,
 * since that can newly expose or newly hide any of its 6 neighbors *)
let check_neighbors (m : t) ((x, y, z) : pos) : unit =
  faces
  |> List.iter (fun (dx, dy, dz) ->
         let key = (x + dx, y + dy, z + dz) in
         if Hashtbl.mem m.world key then
           if exposed m key then (if not (Hashtbl.mem m.shown key) then show_block m key)
           else if Hashtbl.mem m.shown key then hide_block m key)

let rec add_block ?(immediate = true) (m : t) (position : pos) (texture : block) : unit =
  if Hashtbl.mem m.world position then remove_block ~immediate m position;
  Hashtbl.replace m.world position texture;
  sector_add m position;
  if immediate then begin
    if exposed m position then show_block m position;
    check_neighbors m position
  end

and remove_block ?(immediate = true) (m : t) (position : pos) : unit =
  Hashtbl.remove m.world position;
  sector_remove m position;
  if immediate then begin
    Hashtbl.remove m.shown position;
    check_neighbors m position
  end

(* line-of-sight search from [position] along [vector]: returns the
 * first solid block hit (and the last empty position just before it,
 * i.e. where a new block would go if placed), or None if nothing is
 * hit within [max_distance] blocks. *)
let hit_test (m : t) ~(position : float * float * float) ~(vector : float * float * float)
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
      if Some key <> previous && Hashtbl.mem m.world key then Some (key, previous)
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
let initialize (m : t) : unit =
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
let init_shown (m : t) : unit =
  Hashtbl.iter (fun position texture -> if exposed m position then Hashtbl.replace m.shown position texture) m.world

let create_world () : t =
  let m = create () in
  initialize m;
  init_shown m;
  m
