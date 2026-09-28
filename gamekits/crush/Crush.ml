(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Crush.mli *)

open Playground
open Basics (* float arithmetics *)

(*****************************************************************************)
(* The levels: slices, front to back *)
(*****************************************************************************)

(* '#' a block, 'E' the exit, 'P' where Danny starts (in the front
 * slice, facing the camera's first way) *)
type level = { name : string; hint : string; slices : string list list }

(* the gap is four blocks wide, too far to jump; a bridge three slices
 * back, and a wall behind the far side -- crush to cross, uncrush to
 * get past the wall *)
let gap =
  { name = "the gap"; hint = "crush (c): the bridge behind comes to you. Then uncrush, or the wall behind will too.";
    slices =
      [ [ "..............";
          "..............";
          "..............";
          "..............";
          ".P...........E";
          "####....######";
          "####....######" ];
        [ ".............."; ".............."; ".............."; ".............."; ".............."; ".............."; ".............." ];
        [ ".............."; ".............."; ".............."; ".............."; ".............."; "....####......"; ".............." ];
        [ ".............."; ".............."; "..........#..."; "..........#..."; "..........#..."; "..........#..."; ".............." ] ] }

(* the exit is deep in the level, across a hole in depth: turn the
 * camera (tab) to crush along the other axis, where a floor far to the
 * right fills the hole. Crushed from the front, a block in slice 3
 * hides the exit (a block wins over what is behind it), and where
 * Danny starts there is no room to crush at all. *)
let deep =
  { name = "the deep"; hint = "turn the camera (tab), then crush: the floor far to the right fills the hole.";
    slices =
      [ [ "........"; "........"; "........"; ".P......"; "####...." ];
        [ "........"; "........"; "........"; "........"; "......##" ];
        [ "........"; "........"; "........"; "........"; "......##" ];
        [ "........"; "........"; "........"; ".#......"; "......##" ];
        [ "........"; "........"; "........"; "........"; "####...." ];
        [ "........"; "........"; "........"; ".E......"; "####...." ] ] }

let levels = [| gap; deep |]

(*****************************************************************************)
(* The grid, and the planes cut from it *)
(*****************************************************************************)

let tile = 48.

let nx (l : level) : int = String.length (List.hd (List.hd l.slices))
let ny (l : level) : int = List.length (List.hd l.slices)
let nz (l : level) : int = List.length l.slices

(* the cell at (x, y, z): y the row from the top, z the slice from the
 * front; outside the grid, nothing *)
let cell (l : level) (x : int) (y : int) (z : int) : char =
  if x < 0 || y < 0 || z < 0 || x >= nx l || y >= ny l || z >= nz l then '.' else (List.nth (List.nth l.slices z) y).[x]

(* The camera: 0 looks along z (the plane is x across), 1 along x (the
 * plane is z across). [across] and [deep] are the grid's sizes that
 * way; [at] turns (across, depth) back into the grid's (x, z). *)
let across (l : level) (view : int) : int = if view = 0 then nx l else nz l
let deep_n (l : level) (view : int) : int = if view = 0 then nz l else nx l
let at (view : int) (u : int) (d : int) : int * int = if view = 0 then (u, d) else (d, u)

(* a cell of the crushed plane: a block if any depth has one, else the
 * exit if one does *)
let project (l : level) (view : int) (u : int) (y : int) : char =
  let cells = List.init (deep_n l view) (fun d -> let x, z = at view u d in cell l x y z) in
  if List.mem '#' cells then '#' else if List.mem 'E' cells then 'E' else '.'

(* the plane Danny plays on: one slice, or all of them crushed *)
let plane (l : level) (view : int) (crushed : bool) (depth : int) : Tilemap.t =
  Tilemap.of_strings tile
    (List.init (ny l) (fun y ->
         String.init (across l view) (fun u ->
             if crushed then project l view u y else let x, z = at view u depth in match cell l x y z with 'P' -> '.' | c -> c)))

(*****************************************************************************)
(* The model *)
(*****************************************************************************)

type play = {
  level : int;
  view : int;
  crushed : bool;
  squash : number; (* 1 uncrushed, 0 crushed; in between, the crush drawn *)
  (* Danny: across the plane and up, in the plane's coordinates, and the
   * depth of the slice he is in (the plane's, uncrushed) *)
  u : number;
  y : number;
  depth : int;
  vx : number;
  vy : number;
  ground : bool;
  message : string;
}

let size = (28., 40.)

let enter (i : int) : play =
  let l = levels.(i) in
  let map = plane l 0 false 0 in
  let u, y = match Tilemap.find (Tilemap.of_strings tile (List.hd l.slices)) 'P' with (c, r) :: _ -> Tilemap.center map c r | [] -> (0., 0.) in
  { level = i; view = 0; crushed = false; squash = 1.; u; y; depth = 0; vx = 0.; vy = 0.; ground = false; message = l.hint }

(*****************************************************************************)
(* The rules: crushing, uncrushing, turning *)
(*****************************************************************************)

let solid (c : char) : bool = c = '#'
let current (p : play) : Tilemap.t = plane levels.(p.level) p.view p.crushed p.depth

(* Crush: refused if Danny would be inside a block of the crushed
 * plane. *)
let crush (p : play) : play =
  let crushed = plane levels.(p.level) p.view true p.depth in
  if Tile_move.hits solid crushed size p.u p.y then { p with message = "no room to crush here" }
  else { p with crushed = true; message = "" }

(* Uncrush: back into a slice -- the one of the block under Danny's
 * feet nearest where he was, or, in the air, the one he was in (every
 * depth is free where he is: the crushed plane was). *)
let uncrush (p : play) : play =
  let l = levels.(p.level) in
  let map = current p in
  let u, y = Tilemap.cell map p.u (p.y - (snd size / 2.) - 2.) in
  let supports = List.filter (fun d -> let x, z = at p.view u d in solid (cell l x y z)) (List.init (deep_n l p.view) Fun.id) in
  let depth =
    match List.sort (fun a b -> compare (abs (a -.. p.depth)) (abs (b -.. p.depth))) supports with
    | d :: _ when p.ground -> d
    | _ -> p.depth
  in
  (* the slice must have room for him, if he stood on a block's edge *)
  let depth = if Tile_move.hits solid (plane l p.view false depth) size p.u p.y then p.depth else depth in
  { p with crushed = false; depth; message = "" }

(* Turn the camera a quarter: what was across becomes depth, what was
 * depth across. Only on the ground, uncrushed. *)
let turn (p : play) : play =
  let l = levels.(p.level) in
  let map = current p in
  let u, _ = Tilemap.cell map p.u p.y in
  let view = 1 -.. p.view in
  (* the depth becomes the position across, and the other way round *)
  let new_map = plane l view false u in
  let nu, _ = Tilemap.center new_map p.depth 0 in
  { p with view; depth = u; u = nu; vx = 0.; message = "" }

(*****************************************************************************)
(* Update *)
(*****************************************************************************)

let gravity = 0.8
let jump_speed = 11.
let run_speed = 4.

(* what the player does this frame, which is all [step] needs: the game
 * reads it off the keyboard, the tests make it up *)
type input = { dx : number; jump : bool; step_deep : int; crush_key : bool; turn_key : bool }

let nothing = { dx = 0.; jump = false; step_deep = 0; crush_key = false; turn_key = false }

let step (i : input) (p : play) : play =
  let target = if p.crushed then 0. else 1. in
  if p.squash <> target then
    (* the crush drawn: the depths slide together, or apart; the game waits *)
    { p with squash = (if target < p.squash then Float.max 0. (p.squash - 0.06) else Float.min 1. (p.squash + 0.06)) }
  else if i.crush_key && p.ground then if p.crushed then uncrush p else crush p
  else if i.turn_key && p.ground && not p.crushed then turn p
  else
    let l = levels.(p.level) in
    (* a step in depth, uncrushed, if the next slice has room *)
    let p =
      if i.step_deep <> 0 && not p.crushed then
        let d = p.depth +.. i.step_deep in
        if d >= 0 && d < deep_n l p.view && not (Tile_move.hits solid (plane l p.view false d) size p.u p.y) then { p with depth = d } else p
      else p
    in
    let map = current p in
    let vy = if i.jump && p.ground then jump_speed else Float.max (-14.) (p.vy - gravity) in
    let (u, y), hit_x = Tile_move.move_by solid map size (p.u, p.y) (i.dx * run_speed, 0.) in
    let (u, y), hit_y = Tile_move.move_by solid map size (u, y) (0., vy) in
    { p with u; y; vx = (if hit_x then 0. else i.dx * run_speed); vy = (if hit_y then 0. else vy);
             ground = Tile_move.on_ground solid map size u y }

let fell (p : play) : bool = p.y < -.(float_of_int (ny levels.(p.level)) * tile / 2.) - 100.
let at_exit (p : play) : bool = p.squash = (if p.crushed then 0. else 1.) && Tile_move.hits (fun c -> c = 'E') (current p) size p.u p.y

(* the keys, read into an [input]: [pressed] says whether a key went
 * down this frame (Scene2d.pressed) *)
let read_input (pressed : (keyboard -> bool) -> bool) (k : keyboard) : input =
  { dx = to_x k; jump = pressed (fun k -> k.kspace);
    step_deep = (if pressed (fun k -> k.kup) then 1 else if pressed (fun k -> k.kdown) then -1 else 0);
    crush_key = pressed (fun k -> Set_.mem "c" k.keys); turn_key = pressed (fun k -> Set_.mem "Tab" k.keys) }

type outcome = Going of play | Next_level of play | Finished

let frame (i : input) (p : play) : outcome =
  let p = step i p in
  if fell p then Going { (enter p.level) with message = "you fell -- again" }
  else if at_exit p then if p.level +.. 1 < Array.length levels then Next_level (enter (p.level +.. 1)) else Finished
  else Going p
