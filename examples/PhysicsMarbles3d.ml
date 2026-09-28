(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
(* Two hundred marbles in a glass box, all bouncing off each other: the
 * broad phase (physics/3d/Broadphase3d.mli, notes_3d_physics.md
 * section 8), and the twin of examples/PhysicsMarbles.ml.
 *
 *   space    the next way of finding the pairs
 *   g        the grid the middle one uses
 *
 * Two hundred marbles make 19,900 pairs and perhaps thirty of them
 * touch. Which are worth testing? The line at the top counts the
 * bounding boxes compared this frame:
 *
 *   - all pairs: 19,900, every frame, for ever;
 *   - a grid: only the marbles sharing a cell, a few hundred;
 *   - sweep and prune: sorted along the axis they are most spread
 *     along, and compared only while their ranges overlap.
 *
 * All three find the same pairs, so the marbles move identically: only
 * the work differs. That is the thing to watch -- switch with space
 * and nothing on screen changes but the number.
 *
 * What the third dimension adds to the 2D version of this example is
 * on the "g" key: the grid is now *cubic*, and a dense one would be a
 * million cells at a hundred a side, so it is hashed and only the
 * cells with something in them exist. The white cells drawn are
 * exactly those.
 *
 * Metres and seconds; the marbles are 12 to 20 cm across, the box 6 m.
 *)
open Playground
open Playground3d

let box_half = 3.
let floor_y = 0.

(* The walls, as planes. Their *drawing* is a wireframe cage and not
 * six slabs: solid walls are opaque, and a box you cannot see into is
 * not much of a demonstration. A plane has no bounding box worth
 * sorting either (it is infinite), so the marbles are bounced against
 * these by hand and the broad phase is left to sort the marbles alone,
 * which is what it is for. *)
let nothing = group3d []

let wall (normal : number * number * number) (d : number) : Physics3d.body =
  Physics3d.body nothing |> Physics3d.hitbox (Hitbox3d.Plane (normal, d)) |> Physics3d.immovable

let walls : Physics3d.body list =
  [ wall (0., 1., 0.) floor_y;
    wall (1., 0., 0.) (-.box_half);
    wall (-1., 0., 0.) (-.box_half);
    wall (0., 0., 1.) (-.box_half);
    wall (0., 0., -1.) (-.box_half) ]

let cage : shape3d =
  let h = box_half and top = 2.5 in
  let rod a b =
    let ax, ay, az = a and bx, by, bz = b in
    box (rgb 150 155 165)
      (Float.max 0.03 (Float.abs (bx -. ax)))
      (Float.max 0.03 (Float.abs (by -. ay)))
      (Float.max 0.03 (Float.abs (bz -. az)))
    |> move3d ((ax +. bx) /. 2.) ((ay +. by) /. 2.) ((az +. bz) /. 2.)
  in
  let corners = [ (-.h, -.h); (h, -.h); (h, h); (-.h, h) ] in
  group3d
    (polygon3d (rgb 105 125 100) [ (-.h, floor_y, -.h); (-.h, floor_y, h); (h, floor_y, h); (h, floor_y, -.h) ]
    :: List.concat
         (List.mapi
            (fun i (x, z) ->
              let nx, nz = List.nth corners ((i + 1) mod 4) in
              [ rod (x, floor_y, z) (x, top, z);
                rod (x, floor_y, z) (nx, floor_y, nz);
                rod (x, top, z) (nx, top, nz) ])
            corners))

let marble_count = 200

(* a lattice of them, thrown in all directions, from the index: no
 * randomness, so every run is the same run *)
let marbles () : Physics3d.body list =
  List.init marble_count (fun i ->
      let r = 0.06 +. (float_of_int (i mod 5) *. 0.01) in
      let x = -2.4 +. (float_of_int (i mod 8) *. 0.68) in
      let z = -2.4 +. (float_of_int (i / 8 mod 8) *. 0.68) in
      let y = 0.5 +. (float_of_int (i / 64) *. 0.7) in
      Physics3d.body (sphere (rgb (90 + (i * 53 mod 150)) (90 + (i * 97 mod 150)) (200 - (i * 31 mod 110))) r)
      |> Physics3d.ball
      |> Physics3d.at x y z
      |> Physics3d.moving
           (float_of_int (i * 37 mod 200) /. 100. -. 1.)
           (float_of_int (i * 53 mod 100) /. 100.)
           (float_of_int (i * 71 mod 200) /. 100. -. 1.)
      |> Physics3d.bouncy 0.7)

type model = {
  marbles : Physics3d.body list;
  method_ : Broadphase3d.method_;
  show_grid : bool;
  space_was_down : bool;
  g_was_down : bool;
}

let initial_model =
  { marbles = marbles (); method_ = Broadphase3d.All_pairs; show_grid = false; space_was_down = false; g_was_down = false }

let next (m : Broadphase3d.method_) : Broadphase3d.method_ =
  let rec after = function
    | x :: (y :: _ as rest) -> if x = m then y else after rest
    | _ -> List.hd Broadphase3d.methods
  in
  after Broadphase3d.methods

let update (computer : computer) (m : model) : model =
  let k = computer.keyboard in
  let space = k.kspace and g = Set_.mem "g" k.keys in
  let method_ = if space && not m.space_was_down then next m.method_ else m.method_ in
  let show_grid = if g && not m.g_was_down then not m.show_grid else m.show_grid in
  let marbles =
    m.marbles
    |> List.map (fun b -> b |> Physics3d.fall 9.8 |> Physics3d.step)
    |> Physics3d.bounce_all ~broad_phase:method_
    |> List.map (fun b -> List.fold_left (fun b w -> Physics3d.bounce_off w b) b walls)
  in
  { marbles; method_; show_grid; space_was_down = space; g_was_down = g }

(*****************************************************************************)
(* View *)
(*****************************************************************************)

let text color size str = words color str |> scale size
let cam = Camera3d.from_far ~fov:38. ~offset:(3.5, 4.5, 8.) (0., 0.8, 0.)

(* the cells the grid actually holds: one wireframe cube each, which is
 * the picture of "hashed, not allocated" *)
let grid_cells (bodies : Physics3d.body list) : shape3d list =
  let boxes = Array.of_list (List.map Physics3d.world_bounds bodies) in
  let cell = Broadphase3d.cell_size boxes in
  if cell <= 0. then []
  else
    let seen = Hashtbl.create 64 in
    Array.iter
      (fun ((x0, y0, z0), (x1, y1, z1)) ->
        let index v = int_of_float (Float.floor (v /. cell)) in
        for cx = index x0 to index x1 do
          for cy = index y0 to index y1 do
            for cz = index z0 to index z1 do
              Hashtbl.replace seen (cx, cy, cz) ()
            done
          done
        done)
      boxes;
    Hashtbl.fold
      (fun (cx, cy, cz) () acc ->
        (* the square the cell sits on: a footprint, which reads as a
         * grid where twelve edges per cell would read as a thicket *)
        let at i = float_of_int i *. cell in
        let x0 = at cx and y = at cy and z0 = at cz in
        let c = rgb 70 70 90 and t = 0.006 in
        let rod w d mx mz = box c w t d |> move3d mx y mz in
        rod cell t (x0 +. (cell /. 2.)) z0
        :: rod cell t (x0 +. (cell /. 2.)) (z0 +. cell)
        :: rod t cell x0 (z0 +. (cell /. 2.))
        :: rod t cell (x0 +. cell) (z0 +. (cell /. 2.))
        :: acc)
      seen []

let view (computer : computer) (m : model) : camera * shape3d list =
  let screen = computer.screen in
  let found = Physics3d.broad_phase m.method_ m.marbles in
  let all = marble_count * (marble_count - 1) / 2 in
  ( cam,
    cage :: List.map Physics3d.draw m.marbles
    @ (if m.show_grid then grid_cells m.marbles else [])
    @ List.map hud
        [ text black 2.2
            (Printf.sprintf "%s: %d boxes compared, of %d pairs" (Broadphase3d.name m.method_) found.Broadphase3d.tests all)
          |> move_y (screen.top -. 45.);
          text (rgb 60 110 70) 2.
            (Printf.sprintf "%d of them touch this frame -- and all three methods find the same ones"
               (List.length found.Broadphase3d.pairs))
          |> move_y (screen.top -. 80.);
          text black 2. "space: the next method    g: the cells the grid keeps" |> move_y (screen.bottom +. 20.) ] )

let app = game3d view update initial_model
let main = Playground3d_platform.run_app3d ~rendering:{ default_rendering with shading = Flat } app
