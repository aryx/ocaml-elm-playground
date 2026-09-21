(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* A toy version of Teardown (Dennis Gustafsson, Tuxedo Labs, 2020-22):
 * a heist where the level is made of small voxels and every one of them
 * can be knocked out. Three crates of loot, one in a house, one in a
 * vault with no door, one on top of a water tower; taking the first
 * sets off the alarm, and then you have seconds to take the others and
 * reach the car. You will not make it by the doors: before you touch
 * anything, you make your own way, with a hammer. W/A/S/D to walk, the
 * arrows (or the mouse) to look, x (or a click) to swing the hammer.
 *
 * Three ideas, each written out in the game:
 *
 *  - The level is a grid of voxels a quarter of a metre wide, each one
 *    a material and a colour, and it is drawn with *greedy meshing*
 *    ([mesh]): of each voxel only the faces touching air, and of those,
 *    the neighbours facing the same way in the same material merged
 *    into one rectangle. A wall of 700 voxels is then a handful of
 *    quads, rebuilt only when the hammer changes it. games3d/
 *    TinyMinecraft.ml stops one step before: it drops the hidden faces
 *    but draws every visible one on its own.
 *
 *  - What the hammer takes out is a ball of voxels; what it *breaks* is
 *    decided afterwards, by a flood fill from the ground ([loose]):
 *    everything the fill reaches still stands, and every group of
 *    voxels it doesn't reach has nothing holding it up any more. Knock
 *    out the four legs of the water tower, and the tank is one such
 *    group:
 *
 *          ########          ########   <- not reached from the
 *          #      #          #      #      ground: loose, a body
 *          #      #          #      #
 *                              X  X      <- the hammer
 *          #      #          #      #
 *        ==#======#==      ==#======#==  <- the ground: the fill
 *                                           starts here
 *
 *  - A loose group leaves the grid and becomes a rigid body of
 *    playground3d/Physics3d, the first game to put the engine's whole
 *    second half to work: the group tumbles as it falls (its
 *    orientation a quaternion, its resistance to turning its bounding
 *    box's), hits the ground and the other pieces at any angle (boxes
 *    against boxes, by separating axes), and settles into a heap that
 *    stays still (the solver, and bodies put to sleep). The part of the
 *    level still standing is, to the engine, a few immovable boxes
 *    ([boxes]: the same greedy idea, in volumes instead of faces),
 *    worked out again after each blow.
 *
 * What it gets wrong, on purpose: a loose group collides as its
 * bounding box, so an L-shaped piece lands on its corners' box rather
 * than its own corners; and you walk through the rubble, since the
 * player collides with the grid only.
 *
 * Uses: Playground3d (polygon3d, cached3d, the camera), Physics3d (a
 * world simulated, bodies made from shapes, a ray against them),
 * Camera3d.sky and floor, Scene2d. Not TinyMinecraft's world (its
 * voxels are a hash table of textured blocks; these are a flat array of
 * materials, which the flood fill and the meshing want), nor a voxel
 * kit, for the same reason, until a third game wants voxels.
 *
 * References: Mikola Lysenko, "Meshing in a Minecraft Game" (0fps.net,
 * 2012) for greedy meshing; John Amanatides and Andrew Woo, "A Fast
 * Voxel Traversal Algorithm for Ray Tracing" (Eurographics, 1987) for
 * the hammer's ray through the grid; Dennis Gustafsson's blog and talks
 * on Teardown's engine, whose voxels are ten centimetres, not
 * twenty-five, and ray traced rather than meshed.
 *
 * Exercises: a piece hit by the hammer breaks further (a body turned
 * back into voxels, carved, and filled again); materials of different
 * strength (glass shatters at a touch, steel needs a blowtorch); a
 * vehicle to drive through a wall; the player standing on the rubble
 * (a Physics3d.ray down, instead of a height of zero); a hull for each
 * piece rather than its bounding box.
 *)
open Playground
open Playground3d

(*****************************************************************************)
(* The grid *)
(*****************************************************************************)

type material = Air | Brick | Concrete | Wood

let color_of (m : material) : color =
  match m with
  | Air -> white
  | Brick -> rgb 172 84 62
  | Concrete -> rgb 168 168 160
  | Wood -> rgb 150 108 66

(* a quarter of a metre, and 20 x 4 x 20 metres of them *)
let size = 0.25
let nx = 80
let ny = 16
let nz = 80

(* the grid, flat: one material per cell. A value like the rest of the
 * model -- the hammer makes a new one (100 KB, a copy, once per blow) *)
type grid = material array

let index (i : int) (j : int) (k : int) : int = (((j * nz) + k) * nx) + i
let inside (i : int) (j : int) (k : int) : bool = i >= 0 && i < nx && j >= 0 && j < ny && k >= 0 && k < nz
let get (g : grid) (i : int) (j : int) (k : int) : material = if inside i j k then g.(index i j k) else Air

(* the world: x and z from -10 to 10 metres, y up from the ground *)
let x0 = -.float_of_int nx *. size /. 2.
let z0 = -.float_of_int nz *. size /. 2.

(* where a lattice point is: a cell's corner, (i, j, k) cells from the
 * grid's *)
let corner ((i, j, k) : int * int * int) : number * number * number =
  (x0 +. (float_of_int i *. size), float_of_int j *. size, z0 +. (float_of_int k *. size))

let centre ((i, j, k) : int * int * int) : number * number * number =
  let x, y, z = corner (i, j, k) in
  (x +. (size /. 2.), y +. (size /. 2.), z +. (size /. 2.))

let cell_of ((x, y, z) : number * number * number) : int * int * int =
  let f v o = int_of_float (Float.floor ((v -. o) /. size)) in
  (f x x0, f y 0., f z z0)

(*****************************************************************************)
(* The level *)
(*****************************************************************************)

(* [fill g m (x1, y1, z1) (x2, y2, z2)]: every cell whose centre is in
 * that box of metres made [m] *)
let fill (g : grid) (m : material) ((x1, y1, z1) : number * number * number) ((x2, y2, z2) : number * number * number)
    : unit =
  for j = 0 to ny - 1 do
    for k = 0 to nz - 1 do
      for i = 0 to nx - 1 do
        let x, y, z = centre (i, j, k) in
        if x > x1 && x < x2 && y > y1 && y < y2 && z > z1 && z < z2 then g.(index i j k) <- m
      done
    done
  done

(* four walls half a metre thick, [h] high, and a roof slab on them *)
let building (g : grid) (walls : material) ((x1, z1) : number * number) ((x2, z2) : number * number) (h : number) :
    unit =
  fill g walls (x1, 0., z1) (x2, h, z2);
  fill g Air (x1 +. 0.5, 0., z1 +. 0.5) (x2 -. 0.5, h, z2 -. 0.5);
  fill g Concrete (x1, h, z1) (x2, h +. 0.25, z2)

(* The heist: the house, two rooms with a door each, the loot in the far
 * corner of the second; the vault, no door; the water tower, the loot
 * on its tank. The car waits by the start. *)
let level () : grid =
  let g = Array.make (nx * ny * nz) Air in
  (* the house: in by the south door, north through the wooden
   * partition's door, and back south to the loot *)
  building g Brick (-7., -8.) (1., -2.) 3.;
  fill g Wood (-3.25, 0., -7.5) (-2.75, 3., -2.5);
  fill g Air (-5.5, 0., -2.6) (-4.5, 2.25, -1.9);
  fill g Air (-3.3, 0., -7.4) (-2.7, 2.25, -6.4);
  (* the vault *)
  building g Concrete (3., -8.) (7., -4.) 2.5;
  (* the water tower: a tank on four legs. A flat one: a rim round it
   * would put the crate inside the tank's bounding box, which is what
   * the tank is to a collision once it falls, and the engine would
   * throw the crate out of it (a first version: through the ground) *)
  fill g Concrete (3., 3., 0.) (6., 3.5, 3.);
  List.iter
    (fun (x, z) -> fill g Concrete (x, 0., z) (x +. 0.5, 3., z +. 0.5))
    [ (3., 0.); (5.5, 0.); (3., 2.5); (5.5, 2.5) ];
  g

(* the three crates, and the car *)
let loot_at : (number * number * number) list = [ (-1., 0.2, -3.); (5., 0.2, -6.); (4.5, 3.7, 1.5) ]
let car_at = (7., 8.)

(*****************************************************************************)
(* Greedy meshing: the faces, merged -- the trick of this game, first part *)
(*****************************************************************************)

(* A face as the engine wants it, counterclockwise seen from the side
 * [n] points to (see playground3d's polygon3d) *)
let facing ((nx', ny', nz') : number * number * number) (pts : (number * number * number) list) :
    (number * number * number) list =
  match pts with
  | (ax, ay, az) :: (bx, by, bz) :: (cx, cy, cz) :: _ ->
      let ux, uy, uz = (bx -. ax, by -. ay, bz -. az) and vx, vy, vz = (cx -. ax, cy -. ay, cz -. az) in
      let cx', cy', cz' = ((uy *. vz) -. (uz *. vy), (uz *. vx) -. (ux *. vz), (ux *. vy) -. (uy *. vx)) in
      if (cx' *. nx') +. (cy' *. ny') +. (cz' *. nz') >= 0. then pts else List.rev pts
  | _ -> pts

(* [mesh mat (di, dj, dk) at]: the faces of the cells [mat] says are
 * solid, in a box of di x dj x dk cells, [at] giving each lattice
 * point's place. For each of the three axes, each plane between two
 * layers of cells:
 *
 *   1. a mask: for each cell of the plane, the face there, if one of
 *      its two cells is solid and the other air (its material, and
 *      which way it faces);
 *   2. greedily, the first face left in the mask grown along u as far
 *      as the same face goes, then along v as far as whole rows of it
 *      go; that rectangle is one quad, and its cells are cleared.
 *
 *        mask of a wall's side     the quads
 *        B B B B B B               +-----------+
 *        B B B B B B               |     1     |
 *        B B . . B B               +---+   +---+
 *        B B . . B B               | 2 |   | 3 |
 *
 * Not the fewest rectangles (that problem is hard), but close, and one
 * pass. *)
let mesh (mat : int -> int -> int -> material) ((di, dj, dk) : int * int * int)
    (at : int * int * int -> number * number * number) : shape3d list =
  let dims = [| di; dj; dk |] in
  let quads = ref [] in
  for d = 0 to 2 do
    let u = (d + 1) mod 3 and v = (d + 2) mod 3 in
    let cell (a : int array) = mat a.(0) a.(1) a.(2) in
    let mask = Array.make (dims.(u) * dims.(v)) None in
    for slice = 0 to dims.(d) do
      (* 1. the mask of the plane between layers slice - 1 and slice *)
      for b = 0 to dims.(v) - 1 do
        for a = 0 to dims.(u) - 1 do
          let p = Array.make 3 0 in
          p.(d) <- slice;
          p.(u) <- a;
          p.(v) <- b;
          let q = Array.copy p in
          q.(d) <- slice - 1;
          let front = if slice < dims.(d) then cell p else Air and back = if slice > 0 then cell q else Air in
          mask.((b * dims.(u)) + a) <-
            (match (back, front) with
            | m, Air when m <> Air -> Some (m, 1.)
            | Air, m when m <> Air -> Some (m, -1.)
            | _ -> None)
        done
      done;
      (* 2. the rectangles *)
      for b = 0 to dims.(v) - 1 do
        for a = 0 to dims.(u) - 1 do
          match mask.((b * dims.(u)) + a) with
          | None -> ()
          | Some face ->
              let same a' b' = a' < dims.(u) && b' < dims.(v) && mask.((b' * dims.(u)) + a') = Some face in
              let w = ref 1 in
              while same (a + !w) b do incr w done;
              let h = ref 1 in
              while List.for_all (fun a' -> same a' (b + !h)) (List.init !w (fun n -> a + n)) do incr h done;
              for b' = b to b + !h - 1 do
                for a' = a to a + !w - 1 do mask.((b' * dims.(u)) + a') <- None done
              done;
              let point a' b' =
                let p = Array.make 3 0 in
                p.(d) <- slice;
                p.(u) <- a';
                p.(v) <- b';
                at (p.(0), p.(1), p.(2))
              in
              let m, sign = face in
              let n = Array.make 3 0. in
              n.(d) <- sign;
              quads :=
                polygon3d (color_of m)
                  (facing (n.(0), n.(1), n.(2))
                     [ point a b; point (a + !w) b; point (a + !w) (b + !h); point a (b + !h) ])
                :: !quads
        done
      done
    done
  done;
  !quads

(* the standing level, drawn: built once per blow *)
let draw_level (g : grid) : shape3d = cached3d (mesh (get g) (nx, ny, nz) corner)

(*****************************************************************************)
(* What stands, and what falls -- the trick of this game, second part *)
(*****************************************************************************)

let neighbours = [ (1, 0, 0); (-1, 0, 0); (0, 1, 0); (0, -1, 0); (0, 0, 1); (0, 0, -1) ]

(* the solid cells joined to [seeds] by faces, a breadth-first flood
 * fill; [seen] marks them, and is shared between calls *)
let flood (g : grid) (seen : bool array) (seeds : (int * int * int) list) : (int * int * int) list =
  let queue = Queue.create () and found = ref [] in
  List.iter (fun (i, j, k) -> seen.(index i j k) <- true; Queue.push (i, j, k) queue) seeds;
  while not (Queue.is_empty queue) do
    let ((i, j, k) as c) = Queue.pop queue in
    found := c :: !found;
    List.iter
      (fun (di, dj, dk) ->
        let i', j', k' = (i + di, j + dj, k + dk) in
        if inside i' j' k' && g.(index i' j' k') <> Air && not seen.(index i' j' k') then begin
          seen.(index i' j' k') <- true;
          Queue.push (i', j', k') queue
        end)
      neighbours
  done;
  !found

(* The loose groups: flood from every solid cell on the ground; every
 * solid cell left over was not reached, and the ones joined together
 * are one group. *)
let loose (g : grid) : (int * int * int) list list =
  let seen = Array.make (Array.length g) false in
  let ground = ref [] in
  for k = 0 to nz - 1 do
    for i = 0 to nx - 1 do
      if g.(index i 0 k) <> Air then ground := (i, 0, k) :: !ground
    done
  done;
  ignore (flood g seen !ground);
  let groups = ref [] in
  for j = 0 to ny - 1 do
    for k = 0 to nz - 1 do
      for i = 0 to nx - 1 do
        if g.(index i j k) <> Air && not seen.(index i j k) then groups := flood g seen [ (i, j, k) ] :: !groups
      done
    done
  done;
  !groups

(* The standing level to the physics engine: boxes, grown greedily as
 * the faces were, but in volume -- along i, then whole rows along k,
 * then whole slabs along j. Each cell in exactly one box. *)
let boxes (g : grid) : ((int * int * int) * (int * int * int)) list =
  let taken = Array.make (Array.length g) false in
  let free i j k = inside i j k && g.(index i j k) <> Air && not taken.(index i j k) in
  let found = ref [] in
  for j = 0 to ny - 1 do
    for k = 0 to nz - 1 do
      for i = 0 to nx - 1 do
        if free i j k then begin
          let i2 = ref (i + 1) in
          while free !i2 j k do incr i2 done;
          let row k' = List.for_all (fun i' -> free i' j k') (List.init (!i2 - i) (fun n -> i + n)) in
          let k2 = ref (k + 1) in
          while row !k2 do incr k2 done;
          let slab j' =
            List.for_all
              (fun k' -> List.for_all (fun i' -> free i' j' k') (List.init (!i2 - i) (fun n -> i + n)))
              (List.init (!k2 - k) (fun n -> k + n))
          in
          let j2 = ref (j + 1) in
          while slab !j2 do incr j2 done;
          for j' = j to !j2 - 1 do
            for k' = k to !k2 - 1 do
              for i' = i to !i2 - 1 do taken.(index i' j' k') <- true done
            done
          done;
          found := ((i, j, k), (!i2, !j2, !k2)) :: !found
        end
      done
    done
  done;
  !found

(* the ground, and the standing level as immovable boxes (never drawn:
 * [draw_level] draws the level) *)
(* thick, so that nothing pushed hard is through it in one step *)
let ground : Physics3d.body =
  Physics3d.body (box white 40. 10. 40.) |> Physics3d.at 0. (-5.) 0. |> Physics3d.immovable |> Physics3d.rough 0.9

let statics (g : grid) : Physics3d.body list =
  List.map
    (fun (lo, (i2, j2, k2)) ->
      let x1, y1, z1 = corner lo and x2, y2, z2 = corner (i2, j2, k2) in
      Physics3d.body (box white (x2 -. x1) (y2 -. y1) (z2 -. z1))
      |> Physics3d.at ((x1 +. x2) /. 2.) ((y1 +. y2) /. 2.) ((z1 +. z2) /. 2.)
      |> Physics3d.immovable |> Physics3d.rough 0.9)
    (boxes g)

(* A loose group as a body: its own faces, meshed in a little grid of
 * its own and centred on its middle (Physics3d.body turns a shape about
 * the origin), as heavy as its voxels. *)
let piece (g : grid) (cells : (int * int * int) list) : Physics3d.body =
  let lo f = List.fold_left (fun m c -> min m (f c)) max_int cells in
  let hi f = List.fold_left (fun m c -> max m (f c)) min_int cells in
  let fi (i, _, _) = i and fj (_, j, _) = j and fk (_, _, k) = k in
  let i0 = lo fi and j0 = lo fj and k0 = lo fk in
  let di = hi fi - i0 + 1 and dj = hi fj - j0 + 1 and dk = hi fk - k0 + 1 in
  let mine = Hashtbl.create 64 in
  List.iter (fun (i, j, k) -> Hashtbl.replace mine (i - i0, j - j0, k - k0) g.(index i j k)) cells;
  let mat i j k = try Hashtbl.find mine (i, j, k) with Not_found -> Air in
  let half n = float_of_int n *. size /. 2. in
  let at (i, j, k) = ((float_of_int i *. size) -. half di, (float_of_int j *. size) -. half dj, (float_of_int k *. size) -. half dk) in
  let x, y, z = corner (i0, j0, k0) in
  Physics3d.body (group3d (mesh mat (di, dj, dk) at))
  |> Physics3d.at (x +. half di) (y +. half dj) (z +. half dk)
  |> Physics3d.heavy (float_of_int (List.length cells) *. 30.)
  |> Physics3d.rough 0.8

let crate (x, y, z) : Physics3d.body =
  Physics3d.body (box (rgb 235 190 40) 0.4 0.4 0.4) |> Physics3d.at x y z |> Physics3d.heavy 20. |> Physics3d.rough 0.8

(*****************************************************************************)
(* The hammer: a ray through the grid *)
(*****************************************************************************)

(* The first solid cell along the ray from [o] towards [d] (a unit
 * vector), within [reach] metres, and how far: Amanatides and Woo's
 * walk, one cell at a time, always into the neighbour whose wall the
 * ray crosses first -- so no cell is skipped and none is visited twice.
 *
 *     +---+---+---+
 *     |   |   | 3 |       t_max: how far along the ray the next wall
 *     +---+---+-/-+       of each axis is; the smaller one is crossed,
 *     |   | 1 /2  |       and that axis's t_max grows by t_delta, the
 *     +---+-/-+---+       ray's length across one cell
 *     |   o   |   |
 *)
let cast (g : grid) ((ox, oy, oz) : number * number * number) ((dx, dy, dz) : number * number * number)
    (reach : number) : ((int * int * int) * number) option =
  let i, j, k = cell_of (ox, oy, oz) in
  let axis o d c origin =
    if d > 0. then (1, (origin +. (float_of_int (c + 1) *. size) -. o) /. d, size /. d)
    else if d < 0. then (-1, (origin +. (float_of_int c *. size) -. o) /. d, -.size /. d)
    else (0, infinity, infinity)
  in
  let si, ti, di = axis ox dx i x0 and sj, tj, dj = axis oy dy j 0. and sk, tk, dk = axis oz dz k z0 in
  let rec walk i j k ti tj tk t =
    if t > reach then None
    else if get g i j k <> Air then Some ((i, j, k), t)
    else if ti <= tj && ti <= tk then walk (i + si) j k (ti +. di) tj tk ti
    else if tj <= tk then walk i (j + sj) k ti (tj +. dj) tk tj
    else walk i j (k + sk) ti tj (tk +. dk) tk
  in
  walk i j k ti tj tk 0.

(* a blow: every cell within [radius] of the point knocked out *)
let radius = 0.45

let smash (g : grid) ((x, y, z) : number * number * number) : grid =
  let g = Array.copy g in
  let ci, cj, ck = cell_of (x, y, z) in
  let r = int_of_float (Float.ceil (radius /. size)) in
  for j = cj - r to cj + r do
    for k = ck - r to ck + r do
      for i = ci - r to ci + r do
        let cx, cy, cz = centre (i, j, k) in
        let d2 = ((cx -. x) ** 2.) +. ((cy -. y) ** 2.) +. ((cz -. z) ** 2.) in
        if inside i j k && d2 <= radius *. radius then g.(index i j k) <- Air
      done
    done
  done;
  g

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type player = { x : number; z : number; yaw : number; pitch : number }

(* The world's bodies, in order: the ground, the standing level's
 * boxes, the crates not taken yet, then the pieces. *)
type game = {
  grid : grid;
  drawn : shape3d;
  n_statics : int;
  n_crates : int;
  world : Physics3d.world;
  player : player;
  alarm : int option; (* frames left, once the first crate is taken *)
  swing : int; (* frames until the hammer can swing again *)
  was_down : bool;
  broken : int; (* how many pieces came down *)
}

type scene = Title | Playing of game | Escaped of game | Caught of game
type model = scene Scene2d.t

(* a prepared route, walls down beforehand, is about 9 seconds of
 * walking; the house by its doors alone is 25 metres *)
let alarm_seconds = 20

let rebuild (g : grid) (crates : Physics3d.body list) (pieces : Physics3d.body list) : Physics3d.world * int =
  let statics = ground :: statics g in
  (Physics3d.world (statics @ crates @ pieces), List.length statics)

let new_game () : game =
  let grid = level () in
  let crates = List.map crate loot_at in
  let world, n_statics = rebuild grid crates [] in
  { grid; drawn = draw_level grid; n_statics; n_crates = List.length crates; world;
    player = { x = 0.; z = 7.; yaw = 0.; pitch = 0. }; alarm = None; swing = 0; was_down = false; broken = 0 }

let initial_model : model = Scene2d.start Title

let rec drop n l = if n = 0 then l else match l with [] -> [] | _ :: t -> drop (n - 1) t
let take n l = List.filteri (fun i _ -> i < n) l
let crates (g : game) : Physics3d.body list = take g.n_crates (drop g.n_statics g.world.bodies)
let pieces (g : game) : Physics3d.body list = drop (g.n_statics + g.n_crates) g.world.bodies

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let eye_height = 1.6
let reach = 3.
let radians (d : number) : number = d *. Float.pi /. 180.

let sight (p : player) : number * number * number =
  let y = radians p.yaw and x = radians p.pitch in
  (cos x *. sin y, sin x, -.(cos x *. cos y))

(* the player is a column 0.6 wide and 1.8 high, and stops at any voxel *)
let blocked (g : grid) (x : number) (z : number) : bool =
  let i1, _, k1 = cell_of (x -. 0.3, 0., z -. 0.3) and i2, j2, k2 = cell_of (x +. 0.3, 1.8, z +. 0.3) in
  let hit = ref (Float.abs x > 9.7 || Float.abs z > 9.7) in
  for j = 0 to j2 do
    for k = k1 to k2 do
      for i = i1 to i2 do
        if get g i j k <> Air then hit := true
      done
    done
  done;
  !hit

let walk (computer : computer) (g : grid) (p : player) : player =
  let k = computer.keyboard in
  let axis a b = (if a then 1. else 0.) -. if b then 1. else 0. in
  let yaw = p.yaw +. (0.15 *. computer.mouse.mdx) +. (2.5 *. axis k.kright k.kleft) in
  let pitch = Float.max (-80.) (Float.min 80. (p.pitch +. (0.15 *. computer.mouse.mdy) +. (1.5 *. axis k.kup k.kdown))) in
  let ahead = axis k.kw k.ks and aside = axis k.kd k.ka in
  let y = radians yaw and speed = 4. /. 60. in
  let dx = speed *. ((ahead *. sin y) +. (aside *. cos y)) and dz = speed *. ((aside *. sin y) -. (ahead *. cos y)) in
  let x = if blocked g (p.x +. dx) p.z then p.x else p.x +. dx in
  let z = if blocked g x (p.z +. dz) then p.z else p.z +. dz in
  { x; z; yaw; pitch }

(* A blow at the level: the ball of voxels gone, then the flood fill,
 * and every loose group (but the dust, fewer than four voxels) a new
 * body. The standing boxes are worked out again, and the world with
 * them; the bodies already moving keep their speed. *)
let blow (g : game) (at : number * number * number) : game =
  let grid = smash g.grid at in
  let groups = loose grid in
  let grid = Array.copy grid in
  List.iter (List.iter (fun (i, j, k) -> grid.(index i j k) <- Air)) groups;
  let fresh = List.filter_map (fun cells -> if List.length cells < 4 then None else Some (piece g.grid cells)) groups in
  let world, n_statics = rebuild grid (crates g) (pieces g @ fresh) in
  { g with grid; drawn = draw_level grid; world; n_statics; broken = g.broken + List.length fresh }

(* a blow at a body (a piece or a crate): a shove *)
let shove (g : game) (target : Physics3d.body) ((dx, dy, dz) : number * number * number) : game =
  let bodies =
    List.map
      (fun (b : Physics3d.body) ->
        if b == target then Physics3d.moving (b.vx +. (3. *. dx)) (b.vy +. (3. *. dy) +. 1.) (b.vz +. (3. *. dz)) b
        else b)
      g.world.bodies
  in
  { g with world = Physics3d.world bodies }

let hammer (computer : computer) (g : game) : game =
  let down = computer.mouse.mdown || Set_.mem "x" computer.keyboard.keys in
  let g = { g with was_down = down; swing = max 0 (g.swing - 1) } in
  if not (down && g.swing = 0) then g
  else
    let eye = (g.player.x, eye_height, g.player.z) and dir = sight g.player in
    let g = { g with swing = 18 } in
    let movers = crates g @ pieces g in
    let voxel = cast g.grid eye dir reach in
    let body = Physics3d.ray ~from:eye ~direction:dir movers in
    match (voxel, body) with
    | _, Some (b, t) when t < reach && (match voxel with Some (_, tv) -> t < tv | None -> true) -> shove g b dir
    | Some (_, t), _ ->
        let ex, ey, ez = eye and dx, dy, dz = dir in
        (* a little into the wall, so the ball takes what was hit *)
        let t = t +. (size /. 2.) in
        blow g (ex +. (t *. dx), ey +. (t *. dy), ez +. (t *. dz))
    | None, _ -> g

(* crates near enough are taken, one on the ground or on a fallen
 * tank, not one up a tower; the first one sets off the alarm *)
let take_loot (g : game) : game =
  let near (b : Physics3d.body) = Float.hypot (b.x -. g.player.x) (b.z -. g.player.z) < 1. && b.y < 3. in
  let left = List.filter (fun b -> not (near b)) (crates g) in
  if List.length left = g.n_crates then g
  else
    let world, n_statics = rebuild g.grid left (pieces g) in
    let alarm = match g.alarm with None -> Some (alarm_seconds * 60) | a -> a in
    { g with world; n_statics; n_crates = List.length left; alarm }

let update_game (computer : computer) (g : game) : game =
  let g = { g with player = walk computer g.grid g.player } in
  let g = hammer computer g in
  let g = { g with world = Physics3d.simulate ~gravity:9.8 g.world } in
  let g = take_loot g in
  { g with alarm = Option.map (fun a -> a - 1) g.alarm }

let at_car (g : game) : bool = Float.hypot (g.player.x -. fst car_at) (g.player.z -. snd car_at) < 2.5

let update (computer : computer) (s : model) : model =
  let s = Scene2d.update computer s in
  let space = Scene2d.pressed (fun k -> k.kspace) s in
  match s.scene with
  | Title -> if space then Scene2d.go (Playing (new_game ())) s else s
  | Playing g ->
      let g = update_game computer g in
      if g.n_crates = 0 && at_car g then Scene2d.go (Escaped g) s
      else if g.alarm = Some 0 then Scene2d.go (Caught g) s
      else { s with scene = Playing g }
  | Escaped _ | Caught _ -> if space then Scene2d.go Title s else s

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let sand = rgb 196 184 150

let car : shape3d =
  let x, z = car_at in
  group3d
    ([ box (rgb 200 40 40) 1.8 0.7 3.8 |> move_y3d 0.6; box (rgb 60 70 90) 1.6 0.6 1.9 |> move3d 0. 1.2 0.3 ]
    @ List.map (fun (a, b) -> box (rgb 30 30 30) 0.3 0.6 0.6 |> move3d a 0.3 b) [ (-0.9, -1.2); (0.9, -1.2); (-0.9, 1.2); (0.9, 1.2) ])
  |> move3d x 0. z

let text (color : color) (size : number) (str : string) : shape = words color str |> scale size

let scene_shapes (cam : camera) (g : game) : shape3d list =
  Camera3d.sky ~sky:(rgb 150 190 230) ~horizon:sand ~ground:(-0.05) cam
  @ [ Camera3d.floor ~color:sand ~ground:(-0.01) cam; g.drawn; car ]
  @ List.map Physics3d.draw (crates g @ pieces g)

let hud_shapes (screen : screen) (g : game) : shape list =
  let taken = List.length loot_at - g.n_crates in
  let hammer =
    let a = if g.swing > 10 then -40. else 0. in
    group [ rectangle (rgb 110 80 50) 16. 160. |> move_y (-70.); rectangle (rgb 90 90 95) 90. 40. ]
    |> rotate a |> move (screen.right -. 170.) (screen.bottom +. 170.)
  in
  [ rectangle black 20. 2.; rectangle black 2. 20.; hammer;
    text black 2.4 (Printf.sprintf "LOOT %d / %d" taken (List.length loot_at)) |> move (screen.left +. 130.) (screen.top -. 35.);
    text (rgb 60 60 60) 1.8 "w/a/s/d: walk   arrows: look   x: hammer" |> move_y (screen.bottom +. 25.) ]
  @
  match g.alarm with
  | Some a -> [ text (rgb 220 30 30) 5. (Printf.sprintf "ALARM  %.1f" (float_of_int a /. 60.)) |> move_y (screen.top -. 60.) ]
  | None -> []

let eye_camera (g : game) : camera =
  let sx, sy, sz = sight g.player in
  let ex, ey, ez = (g.player.x, eye_height, g.player.z) in
  camera ~eye:(ex, ey, ez) ~target:(ex +. sx, ey +. sy, ez +. sz) ~fov:70. ~near:0.05 ~far:3000. ()

(* the level before anyone touched it, for the title: built once *)
let untouched : game Lazy.t = lazy (new_game ())

(* the title's view: the whole heist, from above the car *)
let overview : camera = camera ~eye:(-13., 12., 13.) ~target:(1.5, 0., -2.) ~fov:50. ~far:3000. ()

let view (computer : computer) (s : model) : camera * shape3d list =
  let screen = computer.screen in
  match s.scene with
  | Title ->
      let g = Lazy.force untouched in
      ( overview,
        scene_shapes overview g
        @ List.map hud
            ([ text (rgb 40 40 40) 6. "TINY TEARDOWN" |> move_y 330.;
               text (rgb 40 40 40) 2.4 "three crates: the house, the vault, the water tower" |> move_y (-300.);
               text (rgb 40 40 40) 2.4 "take one and the alarm goes: make your way first" |> move_y (-335.) ]
            @ Scene2d.blink 1. s [ text (rgb 200 40 40) 3.5 "PRESS SPACE" |> move_y (-390.) ]) )
  | Playing g ->
      let cam = eye_camera g in
      (cam, scene_shapes cam g @ List.map hud (hud_shapes screen g))
  | Escaped g | Caught g ->
      let won = match s.scene with Escaped _ -> true | _ -> false in
      let cam = eye_camera g in
      ( cam,
        scene_shapes cam g
        @ List.map hud
            [ rectangle black 900. 170. |> move_y 150. |> fade 0.6;
              text (if won then rgb 90 230 90 else rgb 240 70 70) 6. (if won then "ESCAPED" else "CAUGHT") |> move_y 180.;
              text white 2.4
                (if won then
                   Printf.sprintf "%.1f seconds to spare, %d pieces brought down"
                     (float_of_int (Option.value g.alarm ~default:0) /. 60.) g.broken
                 else Printf.sprintf "%d of %d crates" (List.length loot_at - g.n_crates) (List.length loot_at))
              |> move_y 115. ] )

let app = game3d view update initial_model

(* flat shading, which shows the voxels' faces apart; the back faces
 * drawn, for the sky (seen from below, see Camera3d.sky) *)
let main =
  Playground3d_platform.run_app3d
    ~rendering:{ default_rendering with shading = Flat; backface_culling = false }
    ~capture_mouse:true app
