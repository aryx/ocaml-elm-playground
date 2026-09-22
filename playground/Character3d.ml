(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)
open Playground
open Playground3d

type t = {
  x : number;
  y : number;
  z : number;
  vy : number;
  grounded : bool;
  ground : number;
  radius : number;
  height : number;
  step : number;
  slope : number;
}

let make ?(radius = 0.3) ?(height = 1.8) ?(step = 0.4) ?(slope = 45.) (x : number) (y : number) (z : number) : t =
  { x; y; z; vy = 0.; grounded = false; ground = 0.; radius; height; step; slope }

type vec = number * number * number

let add (a, b, c) (d, e, f) : vec = (a +. d, b +. e, c +. f)
let scale k (a, b, c) : vec = (k *. a, k *. b, k *. c)
let dot (a, b, c) (d, e, f) = (a *. d) +. (b *. e) +. (c *. f)
let length v = Float.sqrt (dot v v)

let moved (c : t) ((dx, dy, dz) : vec) : t = { c with x = c.x +. dx; y = c.y +. dy; z = c.z +. dz }
let degrees (n : vec) : number = let _, ny, _ = n in Float.acos (Float.min 1. (Float.max (-1.) ny)) *. 180. /. Float.pi

(*****************************************************************************)
(* The trace *)
(*****************************************************************************)

(* a capsule standing on its feet: Physics3d.pill makes one up y, as
 * wide as the box's narrow sides *)
let capsule (c : t) : Physics3d.body =
  Physics3d.body (box white (2. *. c.radius) c.height (2. *. c.radius))
  |> Physics3d.pill
  |> Physics3d.at c.x (c.y +. (c.height /. 2.)) c.z

(* less than this is touching, not overlapping: a character standing on
 * a floor it was put down on is not inside it *)
let skin = 1e-4

(* What the capsule touches: the normal out of the solid, and whether
 * it is a *face* of it (the normal one of a box's own axes) or an edge
 * or a corner. The difference matters because the capsule's bottom is
 * round: coming down on the edge of a step, it touches the corner, and
 * the normal there points from the corner to the sphere's middle --
 * 60 degrees from level, say, on a step it can perfectly well walk
 * onto. Only a face's slope is the ground's slope; an edge is something
 * a round foot rolls over (a first version refused a 0.3 m step with a
 * 0.4 m offset, stopped on its corner for ever). *)
type touch = { normal : vec; flat : bool; point : vec }

let touch_of (s : Physics3d.body) (k : Contact3d.t) : touch =
  let axes = Hitbox3d.face_axes (Physics3d.hitbox_of s) in
  { normal = k.normal; flat = List.exists (fun a -> Float.abs (dot a k.normal) > 0.999) axes; point = k.point }

(* the deepest overlap of [c] with a solid *)
let deepest (solids : Physics3d.body list) (c : t) : (touch * number) option =
  let me = capsule c in
  List.fold_left
    (fun best s ->
      match Physics3d.contact s me with
      | Some (k : Contact3d.t) when k.depth > skin -> (
          match best with Some (_, d) when d >= k.depth -> best | _ -> Some (touch_of s k, k.depth))
      | _ -> best)
    None solids

let trace_touch (solids : Physics3d.body list) (c : t) (d : vec) : number * touch option =
  let len = length d in
  let pieces = max 1 (int_of_float (Float.ceil (len /. (c.radius /. 2.)))) in
  let at f = moved c (scale f d) in
  let rec along k =
    if k > pieces then (1., None)
    else
      let f = float_of_int k /. float_of_int pieces in
      match deepest solids (at f) with
      | None -> along (k + 1)
      | Some _ ->
          (* the overlap starts somewhere in the last piece: bisect *)
          let rec bisect free hit n =
            if n = 0 then (free, hit)
            else
              let mid = (free +. hit) /. 2. in
              if deepest solids (at mid) = None then bisect mid hit (n - 1) else bisect free mid (n - 1)
          in
          let free, hit = bisect (float_of_int (k - 1) /. float_of_int pieces) f 12 in
          (free, Option.map fst (deepest solids (at hit)))
  in
  along 1

let trace (solids : Physics3d.body list) (c : t) (d : vec) : number * vec option =
  let f, hit = trace_touch solids c d in
  (f, Option.map (fun h -> h.normal) hit)

(* out of anything it starts inside: pushed along the deepest overlap's
 * normal, a few times (a character put down in a wall, or squeezed) *)
let unstick (solids : Physics3d.body list) (c : t) : t =
  let rec go c n =
    match deepest solids c with
    | Some (h, depth) when n > 0 -> go (moved c (scale (depth +. skin) h.normal)) (n - 1)
    | _ -> c
  in
  go c 4

(*****************************************************************************)
(* Sliding *)
(*****************************************************************************)

(* too steep to walk up: a face more than [slope] from level (and not a
 * wall, which is simply a wall) *)
let steep (c : t) (h : touch) : bool = h.flat && degrees h.normal > c.slope && degrees h.normal < 89.

(* Quake's loop. [walking], two kinds of contact are walls to the feet
 * -- their normals made level, so that sliding along them never climbs:
 * a steep slope, and an edge. An edge because the round foot would
 * otherwise ride up over any edge lower than its radius, on top of the
 * step offset (a first version climbed a 0.5 m step with an offset of
 * 0.4, rolling over its corner from 0.4 up); climbing is the step's
 * job alone, as in PhysX's controller. A walkable face keeps its
 * normal: sliding along a 30 degree ramp is walking up it. *)
let slide ~(walking : bool) (solids : Physics3d.body list) (c : t) (d : vec) : t * touch list =
  let rec go c d planes k =
    if k = 4 || length d < 1e-9 then (c, planes)
    else
      let f, hit = trace_touch solids c d in
      let c = moved c (scale f d) in
      match hit with
      | None -> (c, planes)
      | Some h ->
          let n =
            let nx, _, nz = h.normal in
            if walking && (steep c h || not h.flat) && Float.hypot nx nz > 1e-9 then scale (1. /. Float.hypot nx nz) (nx, 0., nz)
            else h.normal
          in
          let rest = scale (1. -. f) d in
          let rest = add rest (scale (-.dot rest n) n) in
          go c rest (h :: planes) (k + 1)
  in
  go c d [] 0

let horizontal (c : t) (a : t) : number = Float.hypot (a.x -. c.x) (a.z -. c.z)

(* Quake's SV_WalkMove: the move as it is, and the move lifted by the
 * step offset and set down again; the one that went farther wins. A
 * step lower than the offset is walked up, a higher one is a wall. *)
let walk_across (solids : Physics3d.body list) (c : t) (d : vec) : t =
  let plain, _ = slide ~walking:true solids c d in
  if not c.grounded || c.step <= 0. then plain
  else
    let up, _ = slide ~walking:false solids c (0., c.step, 0.) in
    let across, _ = slide ~walking:true solids up d in
    let lifted = up.y -. c.y in
    let f, hit = trace_touch solids across (0., -.lifted, 0.) in
    let down = moved across (0., -.lifted *. f, 0.) in
    (* and what it came down on no higher than the offset: not the
     * feet's height but the *contact's* -- the round foot can come to
     * rest on the corner of a step higher than the offset, feet still
     * below it (a first version did, and rolled up a 0.5 m step with an
     * offset of 0.4), as PhysX's controller checks it *)
    let lands =
      match hit with
      | Some h -> let _, py, _ = h.point in not (steep c h) && py -. c.y <= c.step +. skin
      | None -> false
    in
    if lands && horizontal c down > horizontal c plain +. 1e-6 then down else plain

(*****************************************************************************)
(* A tick *)
(*****************************************************************************)

let walk ?(gravity = 9.8) ?(jump = 0.) (solids : Physics3d.body list) ((vx, vz) : number * number) (c : t) : t =
  let dt = Physics3d.tick in
  (* only what is near: the solids whose boxes meet the reach of this
   * tick, which is a few metres at most *)
  let reach = (Float.hypot vx vz *. dt) +. c.step +. (Float.abs c.vy *. dt) +. 0.5 in
  let near =
    List.filter
      (fun s ->
        let (x1, y1, z1), (x2, y2, z2) = Physics3d.world_bounds s in
        x2 > c.x -. c.radius -. reach && x1 < c.x +. c.radius +. reach
        && z2 > c.z -. c.radius -. reach && z1 < c.z +. c.radius +. reach
        && y2 > c.y -. reach && y1 < c.y +. c.height +. reach)
      solids
  in
  let c = unstick near c in
  (* across the ground *)
  let c = walk_across near c (vx *. dt, 0., vz *. dt) in
  (* up and down: a jump, or gravity, or (standing) held to the ground *)
  let jumping = c.grounded && jump > 0. in
  let vy = if jumping then jump else if c.grounded then 0. else c.vy -. (gravity *. dt) in
  let c, planes = if c.grounded && not jumping then (c, []) else slide ~walking:false near c (0., vy *. dt, 0.) in
  (* landed on something walkable, or hit a ceiling: the speed up or
   * down is gone. Not on a steep slope: it keeps falling along it, or
   * it would slide down it at one tick's worth of gravity for ever *)
  let vy =
    List.fold_left
      (fun vy h ->
        let _, ny, _ = h.normal in
        if (vy < 0. && ny > 0. && not (steep c h)) || (vy > 0. && ny < 0.) then 0. else vy)
      vy planes
  in
  (* the ground check: a short way down, or a step's height when it
   * was standing, so that it keeps to stairs and slopes going down *)
  let probe = if c.grounded && not jumping then c.step +. 0.01 else 0.02 in
  let f, hit = if vy > 0. then (1., None) else trace_touch near c (0., -.probe, 0.) in
  match hit with
  | Some h when not (steep c h) ->
      let c = if c.grounded && not jumping then moved c (0., -.probe *. f, 0.) else c in
      { c with vy = 0.; grounded = true; ground = (if h.flat then degrees h.normal else 0.) }
  | _ -> { c with vy; grounded = false; ground = 0. }
