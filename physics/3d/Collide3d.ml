(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Collide3d.mli *)

open Hitbox3d

let eps = 1e-9

(*****************************************************************************)
(* Closest points *)
(*****************************************************************************)

let clamp lo hi v = Float.max lo (Float.min hi v)

let closest_on_segment ((a, b) : Vec3.t * Vec3.t) (q : Vec3.t) : Vec3.t =
  let ab = Vec3.sub b a in
  let len2 = Vec3.dot ab ab in
  if len2 < eps then a else Vec3.add a (Vec3.scale (clamp 0. 1. (Vec3.dot (Vec3.sub q a) ab /. len2)) ab)

(* Ericson 5.1.9: the two closest points of two segments. The pair of
 * parameters (s, t) that minimizes |(p1 + s d1) - (p2 + t d2)|, with
 * the parallel case (the denominator vanishing) taken separately. *)
let closest_between_segments ((p1, q1) : Vec3.t * Vec3.t) ((p2, q2) : Vec3.t * Vec3.t) : Vec3.t * Vec3.t =
  let d1 = Vec3.sub q1 p1 and d2 = Vec3.sub q2 p2 in
  let r = Vec3.sub p1 p2 in
  let a = Vec3.dot d1 d1 and e = Vec3.dot d2 d2 and f = Vec3.dot d2 r in
  if a < eps && e < eps then (p1, p2)
  else if a < eps then (p1, Vec3.add p2 (Vec3.scale (clamp 0. 1. (f /. e)) d2))
  else
    let c = Vec3.dot d1 r in
    if e < eps then (Vec3.add p1 (Vec3.scale (clamp 0. 1. (-.c /. a)) d1), p2)
    else
      let b = Vec3.dot d1 d2 in
      let denom = (a *. e) -. (b *. b) in
      let s = if denom > eps then clamp 0. 1. (((b *. f) -. (c *. e)) /. denom) else 0. in
      let t = ((b *. s) +. f) /. e in
      (* t outside [0, 1]: clamp it and redo s for that t *)
      let s, t =
        if t < 0. then (clamp 0. 1. (-.c /. a), 0.)
        else if t > 1. then (clamp 0. 1. ((b -. c) /. a), 1.)
        else (s, t)
      in
      (Vec3.add p1 (Vec3.scale s d1), Vec3.add p2 (Vec3.scale t d2))

(* into the box's own frame, clamped to it, and back *)
let closest_on_box (b : placed) (q : Vec3.t) : Vec3.t =
  match b.shape with
  | Box (hx, hy, hz) ->
      let lx, ly, lz = Quat.rotate (Quat.conjugate b.orientation) (Vec3.sub q b.pos) in
      Vec3.add b.pos (Quat.rotate b.orientation (clamp (-.hx) hx lx, clamp (-.hy) hy ly, clamp (-.hz) hz lz))
  | _ -> b.pos

(* Ericson 5.1.5: the closest point of a triangle, by regions *)
let closest_on_triangle ((a, b, c) : Vec3.t * Vec3.t * Vec3.t) (p : Vec3.t) : Vec3.t =
  let ab = Vec3.sub b a and ac = Vec3.sub c a and ap = Vec3.sub p a in
  let d1 = Vec3.dot ab ap and d2 = Vec3.dot ac ap in
  if d1 <= 0. && d2 <= 0. then a
  else
    let bp = Vec3.sub p b in
    let d3 = Vec3.dot ab bp and d4 = Vec3.dot ac bp in
    if d3 >= 0. && d4 <= d3 then b
    else
      let vc = (d1 *. d4) -. (d3 *. d2) in
      if vc <= 0. && d1 >= 0. && d3 <= 0. then Vec3.add a (Vec3.scale (d1 /. (d1 -. d3)) ab)
      else
        let cp = Vec3.sub p c in
        let d5 = Vec3.dot ab cp and d6 = Vec3.dot ac cp in
        if d6 >= 0. && d5 <= d6 then c
        else
          let vb = (d5 *. d2) -. (d1 *. d6) in
          if vb <= 0. && d2 >= 0. && d6 <= 0. then Vec3.add a (Vec3.scale (d2 /. (d2 -. d6)) ac)
          else
            let va = (d3 *. d6) -. (d5 *. d4) in
            if va <= 0. && d4 -. d3 >= 0. && d5 -. d6 >= 0. then
              Vec3.add b (Vec3.scale ((d4 -. d3) /. (d4 -. d3 +. (d5 -. d6))) (Vec3.sub c b))
            else
              let denom = 1. /. (va +. vb +. vc) in
              Vec3.add a (Vec3.add (Vec3.scale (vb *. denom) ab) (Vec3.scale (vc *. denom) ac))

(*****************************************************************************)
(* Pairs *)
(*****************************************************************************)

let bounds_overlap ((alo, ahi) : Vec3.t * Vec3.t) ((blo, bhi) : Vec3.t * Vec3.t) : bool =
  let (ax, ay, az) = alo and (bx, by, bz) = ahi and (cx, cy, cz) = blo and (dx, dy, dz) = bhi in
  ax <= dx && cx <= bx && ay <= dy && cy <= by && az <= dz && cz <= bz

let spheres ((ca, ra) : Vec3.t * float) ((cb, rb) : Vec3.t * float) : Contact3d.t option =
  let d = Vec3.sub cb ca in
  let len = Vec3.length d in
  if len >= ra +. rb then None
  else
    let normal = if len < eps then (1., 0., 0.) else Vec3.scale (1. /. len) d in
    let depth = ra +. rb -. len in
    Some (Contact3d.make ~normal ~depth ~point:(Vec3.add ca (Vec3.scale (ra -. (depth /. 2.)) normal)))

(* the sphere first, the box second: the normal points from one to the
 * other, and a centre *inside* the box is its own case -- the way out
 * is then the nearest face, which no "closest point" can tell you *)
let sphere_box ((c, r) : Vec3.t * float) (b : placed) : Contact3d.t option =
  match b.shape with
  | Box (hx, hy, hz) ->
      let cl = closest_on_box b c in
      let d = Vec3.sub cl c in
      let len = Vec3.length d in
      if len > eps then
        if len >= r then None
        else Some (Contact3d.make ~normal:(Vec3.scale (1. /. len) d) ~depth:(r -. len) ~point:cl)
      else
        let lx, ly, lz = Quat.rotate (Quat.conjugate b.orientation) (Vec3.sub c b.pos) in
        let faces = [ (hx -. Float.abs lx, (1., 0., 0.), lx); (hy -. Float.abs ly, (0., 1., 0.), ly); (hz -. Float.abs lz, (0., 0., 1.), lz) ] in
        let out, axis, coord = List.fold_left (fun (bo, ba, bc) (o, a, cc) -> if o < bo then (o, a, cc) else (bo, ba, bc)) (List.hd faces) (List.tl faces) in
        let dir = Quat.rotate b.orientation (Vec3.scale (if coord >= 0. then -1. else 1.) axis) in
        Some (Contact3d.make ~normal:dir ~depth:(r +. out) ~point:c)
  | _ -> None

let sphere_capsule ((c, r) : Vec3.t * float) (cap : placed) : Contact3d.t option =
  match cap.shape with
  | Capsule (_, cr) -> spheres (c, r) (closest_on_segment (segment cap) c, cr)
  | _ -> None

let capsules (a : placed) (b : placed) : Contact3d.t option =
  match (a.shape, b.shape) with
  | Capsule (_, ra), Capsule (_, rb) ->
      let pa, pb = closest_between_segments (segment a) (segment b) in
      spheres (pa, ra) (pb, rb)
  | _ -> None

let sphere_triangle ((c, r) : Vec3.t * float) (tri : Vec3.t * Vec3.t * Vec3.t) : Contact3d.t option =
  spheres (c, r) (closest_on_triangle tri c, 0.)

(* The separating axis theorem, in three dimensions: two convex shapes
 * miss each other if and only if some axis sees their shadows apart.
 * For two boxes the axes to try are the 3 + 3 face normals *and* the 9
 * crosses of an edge of each -- see the .mli for what dropping those
 * nine does. *)
let boxes ?(edge_axes = true) (a : placed) (b : placed) : Contact3d.t option =
  let fa = face_axes a and fb = face_axes b in
  let crosses = if edge_axes then List.concat_map (fun u -> List.map (fun v -> Vec3.cross u v) fb) fa else [] in
  let candidates = fa @ fb @ crosses in
  let best = ref None and separated = ref false in
  List.iter
    (fun axis ->
      if (not !separated) && Vec3.length axis > 1e-6 then begin
        let axis = Vec3.normalize axis in
        let alo, ahi = extent a axis and blo, bhi = extent b axis in
        let overlap = Float.min ahi bhi -. Float.max alo blo in
        if overlap <= 0. then separated := true
        else match !best with Some (o, _) when o <= overlap -> () | _ -> best := Some (overlap, axis)
      end)
    candidates;
  if !separated then None
  else
    match !best with
    | None -> None
    | Some (depth, axis) ->
        (* the normal points from a towards b *)
        let normal = if Vec3.dot (Vec3.sub b.pos a.pos) axis < 0. then Vec3.scale (-1.) axis else axis in
        let pa = support a normal and pb = support b (Vec3.scale (-1.) normal) in
        Some (Contact3d.make ~normal ~depth ~point:(Vec3.scale 0.5 (Vec3.add pa pb)))

(* A capsule is a segment with a radius, so the contact is the closest
 * point of that segment to the box -- found by going back and forth
 * between the two shapes, which converges for convex ones. See the
 * .mli for where this is an approximation and what pays for it. *)
let box_capsule (b : placed) (cap : placed) : Contact3d.t option =
  match cap.shape with
  | Capsule (_, r) ->
      let seg = segment cap in
      let rec settle n p = if n = 0 then p else settle (n - 1) (closest_on_segment seg (closest_on_box b p)) in
      let p = settle 4 cap.pos in
      (match sphere_box (p, r) b with Some c -> Some (Contact3d.flip c) | None -> None)
  | _ -> None

(* the plane first: its normal is the contact's, since that is the way
 * out of a half-space *)
let plane_hitbox ((n, d) : Vec3.t * float) (p : placed) : Contact3d.t option =
  let n = Vec3.normalize n in
  let deepest = support p (Vec3.scale (-1.) n) in
  let along = Vec3.dot deepest n -. d in
  if along >= 0. then None
  else Some (Contact3d.make ~normal:n ~depth:(-.along) ~point:(Vec3.add deepest (Vec3.scale (-.along /. 2.) n)))

(* every pair, each dispatched to the test above that knows it, with
 * the contact turned round when the pair arrives the other way up *)
let contact (a : placed) (b : placed) : Contact3d.t option =
  let flipped f = match f with Some c -> Some (Contact3d.flip c) | None -> None in
  match (a.shape, b.shape) with
  | Sphere ra, Sphere rb -> spheres (a.pos, ra) (b.pos, rb)
  | Sphere r, Box _ -> sphere_box (a.pos, r) b
  | Box _, Sphere r -> flipped (sphere_box (b.pos, r) a)
  | Sphere r, Capsule _ -> sphere_capsule (a.pos, r) b
  | Capsule _, Sphere r -> flipped (sphere_capsule (b.pos, r) a)
  | Box _, Box _ -> boxes a b
  | Capsule _, Capsule _ -> capsules a b
  | Box _, Capsule _ -> box_capsule a b
  | Capsule _, Box _ -> flipped (box_capsule b a)
  | Plane (n, d), _ -> plane_hitbox (n, d) b
  | _, Plane (n, d) -> flipped (plane_hitbox (n, d) a)

let touching (a : placed) (b : placed) : bool = contact a b <> None

(*****************************************************************************)
(* Rays *)
(*****************************************************************************)

let ray_sphere ~(from : Vec3.t) ~(direction : Vec3.t) ((c, r) : Vec3.t * float) : float option =
  let u = Vec3.normalize direction in
  let m = Vec3.sub from c in
  let b = Vec3.dot m u and cc = Vec3.dot m m -. (r *. r) in
  if cc > 0. && b > 0. then None
  else
    let disc = (b *. b) -. cc in
    if disc < 0. then None
    else
      let t = -.b -. sqrt disc in
      Some (Float.max 0. t)

let ray_plane ~(from : Vec3.t) ~(direction : Vec3.t) ((n, d) : Vec3.t * float) : float option =
  let u = Vec3.normalize direction in
  let denom = Vec3.dot n u in
  if Float.abs denom < eps then None
  else
    let t = (d -. Vec3.dot n from) /. denom in
    if t < 0. then None else Some t

(* the slab test, in the box's own frame *)
let ray_box ~(from : Vec3.t) ~(direction : Vec3.t) (b : placed) : float option =
  match b.shape with
  | Box (hx, hy, hz) ->
      let inv = Quat.conjugate b.orientation in
      let ox, oy, oz = Quat.rotate inv (Vec3.sub from b.pos) in
      let dx, dy, dz = Quat.rotate inv (Vec3.normalize direction) in
      let slab o d h (lo, hi) =
        if Float.abs d < eps then if o < -.h || o > h then (infinity, neg_infinity) else (lo, hi)
        else
          let t1 = (-.h -. o) /. d and t2 = (h -. o) /. d in
          (Float.max lo (Float.min t1 t2), Float.min hi (Float.max t1 t2))
      in
      let lo, hi = slab oz dz hz (slab oy dy hy (slab ox dx hx (neg_infinity, infinity))) in
      if lo > hi || hi < 0. then None else Some (Float.max 0. lo)
  | _ -> None

(* a swept sphere: the side of the cylinder, then the two caps *)
let ray_capsule ~(from : Vec3.t) ~(direction : Vec3.t) (cap : placed) : float option =
  match cap.shape with
  | Capsule (_, r) ->
      let a, b = segment cap in
      let u = Vec3.normalize direction in
      let d = Vec3.sub b a in
      let m = Vec3.sub from a in
      let dd = Vec3.dot d d and nd = Vec3.dot u d and md = Vec3.dot m d in
      let side =
        (* the quadratic of a ray against an infinite cylinder, kept
         * only where it lands between the two ends *)
        let a_ = dd -. (nd *. nd) in
        let b_ = dd *. Vec3.dot m u -. (nd *. md) in
        let c_ = (dd *. (Vec3.dot m m -. (r *. r))) -. (md *. md) in
        if Float.abs a_ < eps then None
        else
          let disc = (b_ *. b_) -. (a_ *. c_) in
          if disc < 0. then None
          else
            let t = (-.b_ -. sqrt disc) /. a_ in
            let t = Float.max 0. t in
            let along = md +. (t *. nd) in
            if along < 0. || along > dd then None else Some t
      in
      let caps = List.filter_map (fun c -> ray_sphere ~from ~direction (c, r)) [ a; b ] in
      let all = (match side with Some t -> [ t ] | None -> []) @ caps in
      (match all with [] -> None | t :: ts -> Some (List.fold_left Float.min t ts))
  | _ -> None

(* Moller and Trumbore (1997): no plane equation, no precomputation --
 * the barycentric coordinates fall out of one cross product each *)
let ray_triangle ~(from : Vec3.t) ~(direction : Vec3.t) ((a, b, c) : Vec3.t * Vec3.t * Vec3.t) : float option =
  let u = Vec3.normalize direction in
  let e1 = Vec3.sub b a and e2 = Vec3.sub c a in
  let p = Vec3.cross u e2 in
  let det = Vec3.dot e1 p in
  if Float.abs det < eps then None
  else
    let inv = 1. /. det in
    let s = Vec3.sub from a in
    let bu = Vec3.dot s p *. inv in
    if bu < 0. || bu > 1. then None
    else
      let q = Vec3.cross s e1 in
      let bv = Vec3.dot u q *. inv in
      if bv < 0. || bu +. bv > 1. then None
      else
        let t = Vec3.dot e2 q *. inv in
        if t < 0. then None else Some t

let ray ~(from : Vec3.t) ~(direction : Vec3.t) (p : placed) : float option =
  match p.shape with
  | Sphere r -> ray_sphere ~from ~direction (p.pos, r)
  | Box _ -> ray_box ~from ~direction p
  | Capsule _ -> ray_capsule ~from ~direction p
  | Plane (n, d) -> ray_plane ~from ~direction (n, d)
