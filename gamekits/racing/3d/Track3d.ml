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

(* See Track3d.mli *)

type control = { x : number; y : number; z : number; width : number; bank : number }

let control ?(y = 0.) ?(width = 10.) ?(bank = 0.) (x : number) (z : number) : control =
  { x; y; z; width; bank }

type place = { px : number; py : number; pz : number; heading : number; width : number; bank : number }

(* The samples, one every [step] along the middle. A circuit is
 * [closed]: the loop runs from the last sample back to the first, and
 * every distance wraps. A stage is not: it has one segment fewer than
 * it has samples (the last one leads nowhere), and a distance past its
 * end stays at its end. *)
type t = { step : number; points : place array; closed : bool }

let segments (t : t) : int = if t.closed then Array.length t.points else Array.length t.points - 1
let step (t : t) : number = t.step
let length (t : t) : number = float_of_int (segments t) *. t.step

let radians (d : number) : number = d *. Float.pi /. 180.

(*****************************************************************************)
(* Building *)
(*****************************************************************************)

(* one coordinate of the Catmull-Rom spline through p0..p3, at [u] in
 * [0, 1] between p1 and p2: the curve passes through p1 and p2, and
 * leaves each of them along the line between its neighbors *)
let catmull (p0 : number) (p1 : number) (p2 : number) (p3 : number) (u : number) : number =
  let u2 = u *. u in
  let u3 = u2 *. u in
  0.5
  *. ((2. *. p1)
     +. ((p2 -. p0) *. u)
     +. (((2. *. p0) -. (5. *. p1) +. (4. *. p2) -. p3) *. u2)
     +. ((-.p0 +. (3. *. p1) -. (3. *. p2) +. p3) *. u3))

(* the fine polyline through the controls, [per] points between each
 * pair: the spline for the middle of the road, the width and the lean
 * carried along it (eased the same way, so a course can change them
 * anywhere it puts a control point) *)
let fine (controls : control array) (per : int) : (number * number * number * number * number) array =
  let n = Array.length controls in
  let get (i : int) : control = controls.(((i mod n) + n) mod n) in
  Array.init (n * per) (fun k ->
      let i = k / per and u = float_of_int (k mod per) /. float_of_int per in
      let a = get (i - 1) and b = get i and c = get (i + 1) and d = get (i + 2) in
      ( catmull a.x b.x c.x d.x u,
        catmull a.y b.y c.y d.y u,
        catmull a.z b.z c.z d.z u,
        catmull a.width b.width c.width d.width u,
        catmull a.bank b.bank c.bank d.bank u ))

let build ?(step = 3.) (controls : control list) : t =
  let controls = Array.of_list controls in
  if Array.length controls < 4 then invalid_arg "Track3d.build: at least four control points";
  let fine = fine controls 24 in
  let n = Array.length fine in
  let point (i : int) = fine.(((i mod n) + n) mod n) in
  let dist (i : int) (j : int) : number =
    let x1, _, z1, _, _ = point i and x2, _, z2, _, _ = point j in
    Float.hypot (x2 -. x1) (z2 -. z1)
  in
  (* the spline's own points are not evenly spaced (it goes faster
   * through the straights), so walk it and drop a sample every [step]:
   * then "how far along" is a distance, and every quad is the same
   * length *)
  let total = ref 0. in
  for i = 0 to n - 1 do
    total := !total +. dist i (i + 1)
  done;
  let count = max 4 (int_of_float (Float.round (!total /. step))) in
  let step = !total /. float_of_int count in
  let samples = Array.make count (0., 0., 0., 0., 0.) in
  let i = ref 0 and walked = ref 0. in
  for k = 0 to count - 1 do
    let wanted = float_of_int k *. step in
    while !walked +. dist !i (!i + 1) < wanted && !i < n do
      walked := !walked +. dist !i (!i + 1);
      incr i
    done;
    let x1, y1, z1, w1, b1 = point !i and x2, y2, z2, w2, b2 = point (!i + 1) in
    let leg = dist !i (!i + 1) in
    let f = if leg = 0. then 0. else (wanted -. !walked) /. leg in
    samples.(k) <-
      ( x1 +. ((x2 -. x1) *. f),
        y1 +. ((y2 -. y1) *. f),
        z1 +. ((z2 -. z1) *. f),
        w1 +. ((w2 -. w1) *. f),
        b1 +. ((b2 -. b1) *. f) )
  done;
  let points =
    Array.init count (fun k ->
        let x, y, z, width, bank = samples.(k) in
        let nx, _, nz, _, _ = samples.((k + 1) mod count) in
        (* the heading of the segment leaving this sample: 0 towards
         * -z, 90 towards +x (Camera3d's) *)
        let heading = atan2 (nx -. x) (-.(nz -. z)) *. 180. /. Float.pi in
        { px = x; py = y; pz = z; heading; width; bank })
  in
  { step; points; closed = true }

(* A Road.t is a course written as a table of segments, each with its
 * curve and its height, which is how the arcade's pseudo-3D racers
 * described one (see gamekits/racing/Road.mli). [centerline] already walks
 * it into points in space; all that is missing to make it a ribbon is
 * a width and, if the game wants one, a lean.
 *
 * The lean comes from the curve itself: a road turning right lifts its
 * left-hand side, which is a negative bank here, and since Road eases
 * a curve in and out the lean is eased with it. Real roads are built
 * this way (superelevation); Virtua Racing's were drawn this way.
 *
 * The result is not a circuit: [Road.coast] ends somewhere else than
 * it started, so this ribbon is a stage, and [at] stops at its end
 * rather than wrapping round to the start line. *)
let of_road ?(width = 10.) ?(degrees_per_curve = 0.8) ?(bank_per_curve = 0.) (road : Road.t) : t =
  let centre = Road.centerline degrees_per_curve road in
  let last = Array.length road.segments - 1 in
  let points =
    Array.mapi
      (fun i (p : Road.point) ->
        let curve = road.segments.(min i last).curve in
        { px = p.x; py = p.y; pz = p.z; heading = p.heading; width; bank = -.curve *. bank_per_curve })
      centre
  in
  { step = road.segment_length; points; closed = false }

(*****************************************************************************)
(* Places *)
(*****************************************************************************)

(* degrees from [a] to [b], the short way: headings wrap, and mixing
 * 359 with 1 must give 0, not 180 *)
let angle_diff (a : number) (b : number) : number = Float.rem (Float.rem (b -. a +. 180.) 360. +. 360.) 360. -. 180.

let at (t : t) (s : number) : place =
  let n = Array.length t.points in
  let total = length t in
  (* a circuit wraps round; a stage stops at its end *)
  let s =
    if t.closed then Float.rem (Float.rem s total +. total) total
    else Float.max 0. (Float.min (total -. 0.001) s)
  in
  let k = int_of_float (s /. t.step) in
  let k = max 0 (min (segments t - 1) k) in
  let f = (s -. (float_of_int k *. t.step)) /. t.step in
  let a = t.points.(k) and b = t.points.((k + 1) mod n) in
  let mix (u : number) (v : number) : number = u +. ((v -. u) *. f) in
  { px = mix a.px b.px;
    py = mix a.py b.py;
    pz = mix a.pz b.pz;
    heading = a.heading +. (angle_diff a.heading b.heading *. f);
    width = mix a.width b.width;
    bank = mix a.bank b.bank }

let forward (t : t) (s : number) : number * number =
  let p = at t s in
  (sin (radians p.heading), -.cos (radians p.heading))

(* the way across the road at a place: to the right of the way it is
 * driven, tilted by the lean -- so the outside of a banked corner
 * rises, and a thing standing at an offset stands on the slope *)
let across_at (p : place) (offset : number) : number * number * number =
  let a = radians p.heading and b = radians p.bank in
  let rx = cos a and rz = sin a in
  (p.px +. (offset *. rx *. cos b), p.py +. (offset *. sin b), p.pz +. (offset *. rz *. cos b))

let across (t : t) (s : number) (offset : number) : number * number * number = across_at (at t s) offset

(* [project i x z]: how far along segment [i] (0 to 1, clamped), how far
 * to the right of it the point (x, z) is, and how far the point really
 * is from it.
 *
 * That last one is what [locate] sorts by, and it is not the offset: a
 * segment the point is long past can still show a tiny offset, since
 * the offset is measured across the whole line the segment lies on and
 * not across the segment itself. Sort by the offset instead and a kart
 * on the straight is told it is halfway round the lap, which sends the
 * computer that believes it driving in circles. *)
let project (t : t) (i : int) (x : number) (z : number) : number * number * number =
  let n = segments t in
  let a = t.points.(((i mod n) + n) mod n) and b = t.points.((((i mod n) + n) mod n + 1) mod n) in
  let dx = b.px -. a.px and dz = b.pz -. a.pz in
  let len2 = (dx *. dx) +. (dz *. dz) in
  let u = if len2 = 0. then 0. else (((x -. a.px) *. dx) +. ((z -. a.pz) *. dz)) /. len2 in
  let u = Float.max 0. (Float.min 1. u) in
  let cx = a.px +. (dx *. u) and cz = a.pz +. (dz *. u) in
  (* the sign: to the right of the way it goes. The direction is
   * (dx, dz) and the right is (-dz, dx), so the offset is their dot
   * product with what is left of the point. *)
  let leg = Float.sqrt len2 in
  let offset = if leg = 0. then 0. else (((x -. cx) *. -.dz) +. ((z -. cz) *. dx)) /. leg in
  (u, offset, Float.hypot (x -. cx) (z -. cz))

let locate ?(near = -1.) (t : t) (x : number) (z : number) : number * number =
  let n = segments t in
  let first, last =
    if near < 0. then (0, n - 1)
    else
      (* a circuit comes back near itself, so look near where the thing
       * was: twelve segments either way, which at any sane speed is
       * further than it can have gone. A stage does not come back, so
       * its window is clipped to its two ends rather than wrapped. *)
      let k = int_of_float (Float.rem (Float.rem near (length t) +. length t) (length t) /. t.step) in
      if t.closed then (k - 12, k + 12) else (max 0 (k - 12), min (n - 1) (k + 12))
  in
  let best = ref (Float.infinity, 0., 0.) in
  for i = first to last do
    let u, offset, d = project t i x z in
    let far, _, _ = !best in
    if d < far then
      let s = Float.rem (float_of_int (((i mod n) + n) mod n) +. u) (float_of_int n) *. t.step in
      best := (d, s, offset)
  done;
  let _, s, offset = !best in
  (s, offset)

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let strip (t : t) (color : color) (i : int) (a : number) (b : number) : shape3d =
  let n = Array.length t.points in
  let p = t.points.(((i mod n) + n) mod n) and q = t.points.((((i mod n) + n) mod n + 1) mod n) in
  (* Near left, near right, far right, far left: counterclockwise seen
   * from above, so that the face points *up*. Wind it the other way
   * round and the road still draws -- a game with no lighting and no
   * back-face culling cannot tell -- but a flat-shaded one lights it
   * from underneath and the whole course goes black. *)
  polygon3d color [ across_at p a; across_at p b; across_at q b; across_at q a ]

let wall (t : t) (color : color) (i : int) (offset : number) (height : number) : shape3d =
  let n = Array.length t.points in
  let p = t.points.(((i mod n) + n) mod n) and q = t.points.((((i mod n) + n) mod n + 1) mod n) in
  let x1, y1, z1 = across_at p offset and x2, y2, z2 = across_at q offset in
  polygon3d color [ (x1, y1, z1); (x2, y2, z2); (x2, y2 +. height, z2); (x1, y1 +. height, z1) ]
