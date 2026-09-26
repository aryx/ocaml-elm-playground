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
        (* the point is b's deepest, pulled back half the overlap --
         * *not* the midpoint of the two supports, which for a small
         * box on a huge floor lands at a corner of the floor, metres
         * from where they touch, and gives the impulse a lever arm so
         * long that it vanishes. For anything resting, use [manifold]:
         * one point of a face is never the whole story. *)
        let deepest = support b (Vec3.scale (-1.) normal) in
        Some (Contact3d.make ~normal ~depth ~point:(Vec3.add deepest (Vec3.scale (depth /. 2.) normal)))

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
(* Manifolds: a whole face of contact, not one point of it *)
(*****************************************************************************)

(* a box's six faces, each an outward normal and its four corners in
 * order around it *)
let box_faces (p : placed) : (Vec3.t * Vec3.t list) list =
  match (p.shape, face_axes p) with
  | Box (hx, hy, hz), [ ax; ay; az ] ->
      let pairs = [ ((ax, hx), (ay, hy), (az, hz)); ((ay, hy), (az, hz), (ax, hx)); ((az, hz), (ax, hx), (ay, hy)) ] in
      List.concat_map
        (fun ((n, h), (u, hu), (v, hv)) ->
          List.map
            (fun s ->
              let normal = Vec3.scale s n in
              let centre = Vec3.add p.pos (Vec3.scale (s *. h) n) in
              let corner su sv = Vec3.add centre (Vec3.add (Vec3.scale (su *. hu) u) (Vec3.scale (sv *. hv) v)) in
              (normal, [ corner 1. 1.; corner 1. (-1.); corner (-1.) (-1.); corner (-1.) 1. ]))
            [ 1.; -1. ])
        pairs
  | _ -> []

(* Sutherland-Hodgman against one plane, keeping the side where
 * n . p <= d -- the same clipping TinyDescent.ml does
 * through its portals, here in 3D and against a box's sides *)
let clip_by_plane (poly : Vec3.t list) (n : Vec3.t) (d : float) : Vec3.t list =
  let count = List.length poly in
  if count = 0 then []
  else
    List.concat
      (List.mapi
         (fun i p ->
           let q = List.nth poly ((i + 1) mod count) in
           let dp = Vec3.dot n p -. d and dq = Vec3.dot n q -. d in
           let kept = if dp <= 0. then [ p ] else [] in
           let crossing = if dp > 0. = (dq > 0.) then [] else [ Vec3.add p (Vec3.scale (dp /. (dp -. dq)) (Vec3.sub q p)) ] in
           kept @ crossing)
         poly)

(* at most [keep] of them: the deepest first, then whichever is
 * farthest from the ones already taken, so that four points still
 * span the face rather than huddling in one corner *)
let spread_out (keep : int) (points : Contact3d.t list) : Contact3d.t list =
  let rec go taken left =
    if List.length taken >= keep || left = [] then List.rev taken
    else
      let far =
        List.fold_left
          (fun best (c : Contact3d.t) ->
            let d = List.fold_left (fun m (t : Contact3d.t) -> Float.min m (Vec3.length (Vec3.sub c.Contact3d.point t.Contact3d.point))) infinity taken in
            match best with Some (bd, _) when bd >= d -> best | _ -> Some (d, c))
          None left
      in
      match far with
      | None -> List.rev taken
      | Some (_, c) -> go (c :: taken) (List.filter (fun (x : Contact3d.t) -> x != c) left)
  in
  match List.sort (fun (x : Contact3d.t) (y : Contact3d.t) -> compare y.Contact3d.depth x.Contact3d.depth) points with
  | [] -> []
  | deepest :: rest -> go [ deepest ] rest

(* two boxes: the face of one clipped against the sides of the other's,
 * which is what turns "they overlap, here" into "they overlap along
 * this whole face" -- the difference between a box that bounces and a
 * box that can be stacked on *)
let box_manifold (a : placed) (b : placed) (c : Contact3d.t) : Contact3d.t list =
  let n = c.Contact3d.normal in
  let most_aligned faces dir =
    List.fold_left
      (fun best (fn, pts) ->
        let d = Vec3.dot fn dir in
        match best with Some (bd, _, _) when bd >= d -> best | _ -> Some (d, fn, pts))
      None faces
  in
  match (most_aligned (box_faces a) n, most_aligned (box_faces b) (Vec3.scale (-1.) n)) with
  | Some (da, na, pa), Some (db, nb, pb) ->
      (* The reference face is the better aligned with the contact
       * normal, and the *tolerance* matters: with two crates squarely
       * stacked the two faces tie, and floating point breaks the tie
       * differently from one step to the next -- which hands the
       * solver a different set of four points each time, and the pile
       * twitches. Sticking with the first box unless the second is
       * clearly better keeps the manifold the same manifold (Catto
       * does the same, for the same reason). *)
      let flip = db > da +. 0.01 in
      let rn, ref_pts, inc_pts = if flip then (nb, pb, pa) else (na, pa, pb) in
      let count = List.length ref_pts in
      let middle = Vec3.scale (1. /. float_of_int count) (List.fold_left Vec3.add (0., 0., 0.) ref_pts) in
      let clipped =
        List.fold_left
          (fun poly i ->
            let p = List.nth ref_pts i and q = List.nth ref_pts ((i + 1) mod count) in
            let side = Vec3.cross (Vec3.sub q p) rn in
            if Vec3.length side < eps then poly
            else
              (* the plane through that edge, its normal pointing *away*
               * from the face -- taken from the face's middle rather
               * than from the corners' winding, which a box's six
               * faces do not all share *)
              let side = Vec3.normalize side in
              let d = Vec3.dot side p in
              let side, d = if Vec3.dot side middle > d then (Vec3.scale (-1.) side, -.d) else (side, d) in
              clip_by_plane poly side d)
          inc_pts
          (List.init count Fun.id)
      in
      let plane = Vec3.dot rn (List.hd ref_pts) in
      let normal = if flip then Vec3.scale (-1.) rn else rn in
      let points =
        List.filter_map
          (fun p ->
            let depth = plane -. Vec3.dot rn p in
            if depth >= 0. then Some (Contact3d.make ~normal ~depth ~point:p) else None)
          clipped
      in
      if points = [] then [ c ] else spread_out 4 points
  | _ -> [ c ]

let manifold (a : placed) (b : placed) : Contact3d.t list =
  match contact a b with
  | None -> []
  | Some c -> ( match (a.shape, b.shape) with Box _, Box _ -> box_manifold a b c | _ -> [ c ])

(*****************************************************************************)
(* Rays *)
(*****************************************************************************)

(* claude: the arithmetic is graphics/3d/geometry's Ray, shared with the
 * ray tracer, which answers the whole line; physics asks for the first
 * hit in front, 0 when the ray starts inside. A zero direction is no
 * ray: it hits nothing. *)
let first_hit_in_front ((t_in, t_out) : float * float) : float option =
  if t_out < 0. then None else Some (Float.max 0. t_in)

let with_ray ~(from : Vec3.t) ~(direction : Vec3.t) (f : Ray.t -> float option) : float option =
  if Vec3.length direction < eps then None else f (Ray.make from direction)

let ray_sphere ~(from : Vec3.t) ~(direction : Vec3.t) ((c, r) : Vec3.t * float) : float option =
  with_ray ~from ~direction (fun ray -> Option.bind (Ray.sphere ray (c, r)) first_hit_in_front)

let ray_plane ~(from : Vec3.t) ~(direction : Vec3.t) ((n, d) : Vec3.t * float) : float option =
  with_ray ~from ~direction (fun ray ->
      match Ray.plane ray (n, d) with Some t when t >= 0. -> Some t | _ -> None)

(* the slab test, in the box's own frame *)
let ray_box ~(from : Vec3.t) ~(direction : Vec3.t) (b : placed) : float option =
  match b.shape with
  | Box (hx, hy, hz) ->
      let inv = Quat.conjugate b.orientation in
      with_ray ~from:(Quat.rotate inv (Vec3.sub from b.pos)) ~direction:(Quat.rotate inv direction) (fun ray ->
          Option.bind (Ray.box ray ((-.hx, -.hy, -.hz), (hx, hy, hz))) first_hit_in_front)
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

(* Moller and Trumbore (1997), see Ray.triangle *)
let ray_triangle ~(from : Vec3.t) ~(direction : Vec3.t) (tri : Vec3.t * Vec3.t * Vec3.t) : float option =
  with_ray ~from ~direction (fun ray ->
      match Ray.triangle ray tri with Some (t, _, _) when t >= 0. -> Some t | _ -> None)

let ray ~(from : Vec3.t) ~(direction : Vec3.t) (p : placed) : float option =
  match p.shape with
  | Sphere r -> ray_sphere ~from ~direction (p.pos, r)
  | Box _ -> ray_box ~from ~direction p
  | Capsule _ -> ray_capsule ~from ~direction p
  | Plane (n, d) -> ray_plane ~from ~direction (n, d)
