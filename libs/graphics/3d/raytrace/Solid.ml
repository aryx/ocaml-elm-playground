(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Solid.mli *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type pattern = Plain | Checker of int * float
type surface = { color : int; pattern : pattern; material : Material.t }

let color_at (surface : surface) ((x, y, z) : Vec3.t) : int =
  match surface.pattern with
  | Plain -> surface.color
  | Checker (other, size) ->
      let cell v = int_of_float (Float.floor ((v /. size) +. 1e-6)) in
      if (cell x + cell y + cell z) land 1 = 0 then surface.color else other

type primitive = Ball | Cube | Cylinder | Cone | Torus of float | Half_space

type t =
  | Sphere of Vec3.t * float * surface
  | Plane of Vec3.t * float * surface
  | Triangle of { points : Vec3.t * Vec3.t * Vec3.t; normals : Vec3.t * Vec3.t * Vec3.t; surface : surface }
  | Placed of { primitive : primitive; transform : Transform.t; surface : surface }
  | Csg of Csg.op * t * t

let rec has_triangle (s : t) : bool =
  match s with Triangle _ -> true | Csg (_, a, b) -> has_triangle a || has_triangle b | _ -> false

let csg (op : Csg.op) (a : t) (b : t) : t =
  if op <> Csg.Union && (has_triangle a || has_triangle b) then
    invalid_arg "Solid.csg: a triangle has no inside, it cannot be intersected or subtracted";
  Csg (op, a, b)

let rec surface (solid : t) : surface =
  match solid with
  | Sphere (_, _, s) | Plane (_, _, s) -> s
  | Triangle { surface; _ } | Placed { surface; _ } -> surface
  | Csg (_, a, _) -> surface a

let color (leaf : t) (point : Vec3.t) : int =
  match leaf with
  | Placed { transform; surface; _ } -> color_at surface (Transform.inverse_point transform point)
  | _ -> color_at (surface leaf) point

(*****************************************************************************)
(* The unit primitives, in their own space *)
(*****************************************************************************)

let eps = 1e-12

(* where a t^2 + b t + c <= 0: the inside of a quadric along a ray *)
let quadric (a : float) (b : float) (c : float) : (float * float) list =
  if Float.abs a < eps then
    (* a line: b t + c <= 0 *)
    if Float.abs b < eps then if c <= 0. then [ (neg_infinity, infinity) ] else []
    else if b > 0. then [ (neg_infinity, -.c /. b) ]
    else [ (-.c /. b, infinity) ]
  else
    let disc = (b *. b) -. (4. *. a *. c) in
    if disc < 0. then if a > 0. then [] else [ (neg_infinity, infinity) ]
    else
      (* the stable form: no difference of two close numbers *)
      let q = -0.5 *. (b +. (Float.copy_sign (sqrt disc) b)) in
      let r1 = q /. a and r2 = if q = 0. then 0. else c /. q in
      let lo = Float.min r1 r2 and hi = Float.max r1 r2 in
      if a > 0. then [ (lo, hi) ]
        (* outside two roots that are one (a ray through a cone's apex):
         * the halves touch, the whole line *)
      else if lo >= hi then [ (neg_infinity, infinity) ]
      else [ (neg_infinity, lo); (hi, infinity) ]

(* where lo <= o + t d <= hi, on one axis *)
let slab (o : float) (d : float) (lo : float) (hi : float) : (float * float) option =
  if Float.abs d < eps then if o >= lo && o <= hi then Some (neg_infinity, infinity) else None
  else
    let t1 = (lo -. o) /. d and t2 = (hi -. o) /. d in
    Some (Float.min t1 t2, Float.max t1 t2)

let clip (l : (float * float) list) (s : (float * float) option) : (float * float) list =
  match s with
  | None -> []
  | Some (lo, hi) -> List.filter_map (fun (a, b) -> let a = Float.max a lo and b = Float.min b hi in if a <= b then Some (a, b) else None) l

let torus_f (r : float) ((x, y, z) : Vec3.t) : float =
  let k = (x *. x) +. (y *. y) +. (z *. z) +. 1. -. (r *. r) in
  (k *. k) -. (4. *. ((x *. x) +. (z *. z)))

(* The torus's quartic, by bracketing: along the part of the ray in the
 * torus's box, the sign of f at 96 steps, each change of sign closed
 * in on by bisection (50 halvings: to the last bit). Ferrari's formula
 * (1540) solves a quartic exactly, and is famous for losing its
 * precision in the cancellations -- a surface full of holes; this
 * cannot lose a root it has bracketed, and can only miss a tube the
 * ray grazes between two steps. *)
let torus_intervals (r : float) (o : Vec3.t) (d : Vec3.t) : (float * float) list =
  match Ray.box (Ray.make o d) ((-.(1. +. r), -.r, -.(1. +. r)), (1. +. r, r, 1. +. r)) with
  | None -> []
  | Some (t0, t1) ->
      let f t = torus_f r (Vec3.add o (Vec3.scale t d)) in
      let rec bisect a fa b n = if n = 0 then (a +. b) /. 2. else let m = (a +. b) /. 2. in let fm = f m in if (fm < 0.) = (fa < 0.) then bisect m fm b (n - 1) else bisect a fa m (n - 1) in
      let steps = 96 in
      let h = (t1 -. t0) /. float_of_int steps in
      (* the crossings, in order: where f goes negative (in), positive (out) *)
      let crossings = ref [] in
      let prev = ref (f t0) in
      if !prev < 0. then crossings := [ t0 ];
      for i = 1 to steps do
        let t = t0 +. (h *. float_of_int i) in
        let ft = f t in
        if (ft < 0.) <> (!prev < 0.) then crossings := bisect (t -. h) !prev t 50 :: !crossings;
        prev := ft
      done;
      if !prev < 0. then crossings := t1 :: !crossings;
      let rec pairs = function a :: b :: rest -> (a, b) :: pairs rest | _ -> [] in
      pairs (List.rev !crossings)

(* the inside along a ray (o + t d, d a unit vector) of a unit primitive *)
let primitive_intervals (p : primitive) ((ox, oy, oz) as o : Vec3.t) ((dx, dy, dz) as d : Vec3.t) : (float * float) list =
  match p with
  | Ball -> (match Ray.sphere (Ray.make o d) ((0., 0., 0.), 1.) with Some i -> [ i ] | None -> [])
  | Cube -> (match Ray.box (Ray.make o d) ((-1., -1., -1.), (1., 1., 1.)) with Some i -> [ i ] | None -> [])
  | Cylinder ->
      (* x^2 + z^2 <= 1, and -1 <= y <= 1 *)
      clip (quadric ((dx *. dx) +. (dz *. dz)) (2. *. ((ox *. dx) +. (oz *. dz))) ((ox *. ox) +. (oz *. oz) -. 1.)) (slab oy dy (-1.) 1.)
  | Cone ->
      (* x^2 + z^2 <= ((1 - y) / 2)^2, and -1 <= y <= 1; with k = 1 - oy,
       * (1 - y) = k - t dy *)
      let k = 1. -. oy in
      clip
        (quadric ((dx *. dx) +. (dz *. dz) -. (dy *. dy /. 4.)) ((2. *. ((ox *. dx) +. (oz *. dz))) +. (k *. dy /. 2.))
           ((ox *. ox) +. (oz *. oz) -. (k *. k /. 4.)))
        (slab oy dy (-1.) 1.)
  | Torus r -> torus_intervals r o d
  | Half_space -> clip [ (neg_infinity, infinity) ] (slab oy dy neg_infinity 0.)

let primitive_contains (p : primitive) ((x, y, z) as q : Vec3.t) : bool =
  match p with
  | Ball -> Vec3.dot q q <= 1.
  | Cube -> Float.abs x <= 1. && Float.abs y <= 1. && Float.abs z <= 1.
  | Cylinder -> (x *. x) +. (z *. z) <= 1. && Float.abs y <= 1.
  | Cone -> (x *. x) +. (z *. z) <= ((1. -. y) /. 2.) ** 2. && Float.abs y <= 1.
  | Torus r -> torus_f r q <= 0.
  | Half_space -> y <= 0.

(* the outward normal at a point of the surface: of the faces that
 * meet near it, the nearest one's *)
let primitive_normal (p : primitive) ((x, y, z) as q : Vec3.t) : Vec3.t =
  match p with
  | Ball -> q
  | Cube ->
      let ax = Float.abs x and ay = Float.abs y and az = Float.abs z in
      if ax >= ay && ax >= az then (Float.copy_sign 1. x, 0., 0.)
      else if ay >= az then (0., Float.copy_sign 1. y, 0.)
      else (0., 0., Float.copy_sign 1. z)
  | Cylinder ->
      let r = sqrt ((x *. x) +. (z *. z)) in
      if Float.abs (1. -. Float.abs y) < Float.abs (1. -. r) then (0., Float.copy_sign 1. y, 0.) else (x, 0., z)
  | Cone ->
      let r = sqrt ((x *. x) +. (z *. z)) in
      (* the base, or the side: the gradient of x^2 + z^2 - ((1 - y) / 2)^2 *)
      if Float.abs (y +. 1.) < Float.abs (r -. ((1. -. y) /. 2.)) then (0., -1., 0.) else (2. *. x, (1. -. y) /. 2., 2. *. z)
  | Torus r ->
      (* its gradient *)
      let k = (x *. x) +. (y *. y) +. (z *. z) +. 1. -. (r *. r) in
      (4. *. k *. x -. (8. *. x), 4. *. k *. y, 4. *. k *. z -. (8. *. z))
  | Half_space -> (0., 1., 0.)

let primitive_box (p : primitive) : (Vec3.t * Vec3.t) option =
  match p with
  | Ball | Cube | Cylinder | Cone -> Some ((-1., -1., -1.), (1., 1., 1.))
  | Torus r -> Some ((-.(1. +. r), -.r, -.(1. +. r)), (1. +. r, r, 1. +. r))
  | Half_space -> None

(*****************************************************************************)
(* A ray and a solid *)
(*****************************************************************************)

let boundary (t : float) (leaf : t) : t Csg.boundary = { t; leaf; flipped = false }

let rec intervals (ray : Ray.t) (solid : t) : t Csg.interval list =
  let of_ts l = List.map (fun (a, b) -> (boundary a solid, boundary b solid)) l in
  match solid with
  | Sphere (c, r, _) -> (match Ray.sphere ray (c, r) with Some i -> of_ts [ i ] | None -> [])
  | Plane (n, d, _) -> (
      (* the half-space n.p <= d *)
      let denom = Vec3.dot n ray.direction in
      if Float.abs denom < eps then if Vec3.dot n ray.origin <= d then of_ts [ (neg_infinity, infinity) ] else []
      else
        let t = (d -. Vec3.dot n ray.origin) /. denom in
        match denom < 0. with true -> of_ts [ (t, infinity) ] | false -> of_ts [ (neg_infinity, t) ])
  | Triangle { points; _ } -> (match Ray.triangle ray points with Some (t, _, _) -> of_ts [ (t, t) ] | None -> [])
  | Placed { primitive; transform; _ } ->
      (* the ray in the primitive's space; its t there, along a unit
       * direction, is |d'| times the t here *)
      let d' = Transform.inverse_direction transform ray.direction in
      let len = Vec3.length d' in
      let o' = Transform.inverse_point transform ray.origin in
      of_ts (List.map (fun (a, b) -> (a /. len, b /. len)) (primitive_intervals primitive o' (Vec3.scale (1. /. len) d')))
  | Csg (op, a, b) -> Csg.combine op (intervals ray a) (intervals ray b)

let first_of_intervals ~(min_t : float) (l : t Csg.interval list) : t Csg.boundary option =
  let rec go = function
    | [] -> None
    | ((i : t Csg.boundary), (o : t Csg.boundary)) :: rest ->
        if i.t > min_t then Some i else if o.t > min_t then Some o else go rest
  in
  match go l with Some b when b.t < infinity -> Some b | _ -> None

let hit_leaf ~(min_t : float) (ray : Ray.t) (solid : t) : float option =
  let beyond t = if t > min_t then Some t else None in
  match solid with
  | Sphere (c, r, _) -> (
      match Ray.sphere ray (c, r) with
      | None -> None
      | Some (t_in, t_out) -> if t_in > min_t then Some t_in else beyond t_out)
  | Plane (n, d, _) -> Option.bind (Ray.plane ray (n, d)) beyond
  | Triangle { points; _ } -> Option.bind (Ray.triangle ray points) (fun (t, _, _) -> beyond t)
  | Placed _ | Csg _ -> None

let first_hit ?(min_t = 0.) (ray : Ray.t) (solid : t) : t Csg.boundary option =
  match solid with
  | Sphere _ | Plane _ | Triangle _ -> Option.map (fun t -> boundary t solid) (hit_leaf ~min_t ray solid)
  | Placed _ | Csg _ -> first_of_intervals ~min_t (intervals ray solid)

let hit ?(min_t = 0.) (ray : Ray.t) (solid : t) : float option =
  match solid with
  | Sphere _ | Plane _ | Triangle _ -> hit_leaf ~min_t ray solid
  | Placed _ | Csg _ -> Option.map (fun (b : t Csg.boundary) -> b.t) (first_hit ~min_t ray solid)

let normal (solid : t) (ray : Ray.t) (t : float) : Vec3.t =
  match solid with
  | Sphere (c, _, _) -> Vec3.normalize (Vec3.sub (Ray.at ray t) c)
  | Plane (n, _, _) -> Vec3.normalize n
  | Triangle { points = (a, b, c) as points; normals = n0, n1, n2; _ } -> (
      match Ray.triangle ray points with
      | Some (_, u, v) ->
          Vec3.normalize (Vec3.add (Vec3.scale (1. -. u -. v) n0) (Vec3.add (Vec3.scale u n1) (Vec3.scale v n2)))
      (* the ray grazing the edge it was said to hit: the flat normal *)
      | None -> Vec3.normalize (Vec3.cross (Vec3.sub b a) (Vec3.sub c a)))
  | Placed { primitive; transform; _ } ->
      Transform.normal transform (primitive_normal primitive (Transform.inverse_point transform (Ray.at ray t)))
  | Csg _ -> invalid_arg "Solid.normal: a CSG has no surface of its own, its boundary's leaf has"

let rec contains (solid : t) (p : Vec3.t) : bool =
  match solid with
  | Sphere (c, r, _) -> Vec3.length (Vec3.sub p c) <= r
  | Plane (n, d, _) -> Vec3.dot n p <= d
  | Triangle _ -> false
  | Placed { primitive; transform; _ } -> primitive_contains primitive (Transform.inverse_point transform p)
  | Csg (Union, a, b) -> contains a p || contains b p
  | Csg (Inter, a, b) -> contains a p && contains b p
  | Csg (Diff, a, b) -> contains a p && not (contains b p)

let union_box (((ax, ay, az), (bx, by, bz)) : Vec3.t * Vec3.t) (((cx, cy, cz), (dx, dy, dz)) : Vec3.t * Vec3.t) =
  ((Float.min ax cx, Float.min ay cy, Float.min az cz), (Float.max bx dx, Float.max by dy, Float.max bz dz))

let rec bounds (solid : t) : (Vec3.t * Vec3.t) option =
  match solid with
  | Sphere ((x, y, z), r, _) -> Some ((x -. r, y -. r, z -. r), (x +. r, y +. r, z +. r))
  | Plane _ -> None
  | Triangle { points = (ax, ay, az), (bx, by, bz), (cx, cy, cz); _ } ->
      let lo a b c = Float.min a (Float.min b c) and hi a b c = Float.max a (Float.max b c) in
      Some ((lo ax bx cx, lo ay by cy, lo az bz cz), (hi ax bx cx, hi ay by cy, hi az bz cz))
  | Placed { primitive; transform; _ } ->
      (* the primitive's box, its 8 corners carried, boxed again *)
      Option.map
        (fun ((lx, ly, lz), (hx, hy, hz)) ->
          let corners = List.concat_map (fun x -> List.concat_map (fun y -> List.map (fun z -> (x, y, z)) [ lz; hz ]) [ ly; hy ]) [ lx; hx ] in
          let pts = List.map (Transform.point transform) corners in
          List.fold_left (fun b p -> union_box b (p, p)) (List.hd pts, List.hd pts) pts)
        (primitive_box primitive)
  | Csg (Union, a, b) -> (match (bounds a, bounds b) with Some x, Some y -> Some (union_box x y) | _ -> None)
  | Csg (Inter, a, b) -> (
      match (bounds a, bounds b) with
      | Some ((ax, ay, az), (bx, by, bz)), Some ((cx, cy, cz), (dx, dy, dz)) ->
          Some ((Float.max ax cx, Float.max ay cy, Float.max az cz), (Float.min bx dx, Float.min by dy, Float.min bz dz))
      | Some x, None | None, Some x -> Some x
      | None, None -> None)
  | Csg (Diff, a, _) -> bounds a

(*****************************************************************************)
(* Moving solids *)
(*****************************************************************************)

let rec move (offset : Vec3.t) (solid : t) : t =
  match solid with
  | Sphere (c, r, s) -> Sphere (Vec3.add c offset, r, s)
  (* n.(p - offset) = d, i.e. n.p = d + n.offset *)
  | Plane (n, d, s) -> Plane (n, d +. Vec3.dot n offset, s)
  | Triangle ({ points = a, b, c; _ } as tri) ->
      Triangle { tri with points = (Vec3.add a offset, Vec3.add b offset, Vec3.add c offset) }
  | Placed p -> Placed { p with transform = Transform.compose (Transform.translate offset) p.transform }
  | Csg (op, a, b) -> Csg (op, move offset a, move offset b)

let rec transform (tr : Transform.t) (solid : t) : t =
  if Transform.is_translation tr then move (Transform.point tr (0., 0., 0.)) solid
  else
    match solid with
    | Sphere (c, r, surface) ->
        Placed
          { primitive = Ball;
            transform = Transform.compose tr (Transform.compose (Transform.translate c) (Transform.scale (r, r, r)));
            surface }
    | Plane (n, d, s) ->
        (* a point of it carried, its normal by the inverse transpose *)
        let p0 = Vec3.scale (d /. Vec3.dot n n) n in
        let n' = Transform.normal tr n in
        Plane (n', Vec3.dot n' (Transform.point tr p0), s)
    | Triangle { points = a, b, c; normals = n0, n1, n2; surface } ->
        let p = Transform.point tr and n = Transform.normal tr in
        Triangle { points = (p a, p b, p c); normals = (n n0, n n1, n n2); surface }
    | Placed p -> Placed { p with transform = Transform.compose tr p.transform }
    | Csg (op, a, b) -> Csg (op, transform tr a, transform tr b)
