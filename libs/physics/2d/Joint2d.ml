(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Joint2d.mli *)

type kind =
  | Pin of { motor : (float * float) option }
  | Rod of { length : float }
  | Rope of { length : float }
  | Pulley of { ground_a : Vec2.t; ground_b : Vec2.t; length : float }

type t = { a : int; b : int; anchor_a : Vec2.t; anchor_b : Vec2.t; kind : kind }

(* a vector turned by [angle] radians, counterclockwise *)
let rotate (angle : float) ((x, y) : Vec2.t) : Vec2.t =
  let c = cos angle and s = sin angle in
  ((c *. x) -. (s *. y), (s *. x) +. (c *. y))

(* the world point [p] in body [i]'s own frame, and back *)
let to_local (bodies : Body.t array) (angles : float array) (i : int) (p : Vec2.t) : Vec2.t =
  rotate (-.angles.(i)) (Vec2.sub p bodies.(i).pos)

let arm (angles : float array) (i : int) (local : Vec2.t) : Vec2.t = rotate angles.(i) local

let anchors (bodies : Body.t array) (angles : float array) (j : t) : Vec2.t * Vec2.t =
  (Vec2.add bodies.(j.a).pos (arm angles j.a j.anchor_a), Vec2.add bodies.(j.b).pos (arm angles j.b j.anchor_b))

let pin bodies angles a b ~at ?motor () =
  { a; b; anchor_a = to_local bodies angles a at; anchor_b = to_local bodies angles b at; kind = Pin { motor } }

let rod bodies angles a b ~at_a ~at_b () =
  { a; b; anchor_a = to_local bodies angles a at_a; anchor_b = to_local bodies angles b at_b;
    kind = Rod { length = Vec2.length (Vec2.sub at_b at_a) } }

let rope bodies angles a b ~at_a ~at_b ?length () =
  let length = match length with Some l -> l | None -> Vec2.length (Vec2.sub at_b at_a) in
  { a; b; anchor_a = to_local bodies angles a at_a; anchor_b = to_local bodies angles b at_b; kind = Rope { length } }

let pulley bodies angles a b ~at_a ~at_b ~ground_a ~ground_b () =
  let length = Vec2.length (Vec2.sub at_a ground_a) +. Vec2.length (Vec2.sub at_b ground_b) in
  { a; b; anchor_a = to_local bodies angles a at_a; anchor_b = to_local bodies angles b at_b;
    kind = Pulley { ground_a; ground_b; length } }

let length_now bodies angles (j : t) : float =
  let pa, pb = anchors bodies angles j in
  match j.kind with
  | Pin _ -> 0.
  | Rod _ | Rope _ -> Vec2.length (Vec2.sub pb pa)
  | Pulley { ground_a; ground_b; _ } -> Vec2.length (Vec2.sub pa ground_a) +. Vec2.length (Vec2.sub pb ground_b)

(* A row: the speed along it is lin_a . va + ang_a * wa + lin_b . vb +
 * ang_b * wb; the solver wants it to be [target]. [mass] is 1 over the
 * row's resistance: the impulse that changes that speed by 1. *)
type row = {
  ra : int;
  rb : int;
  lin_a : Vec2.t;
  ang_a : float;
  lin_b : Vec2.t;
  ang_b : float;
  mass : float;
  target : float;
  lo : float;
  hi : float;
  mutable total : float;
}

let make_row (bodies : Body.t array) ra rb lin_a ang_a lin_b ang_b ~target ~lo ~hi : row option =
  let a = bodies.(ra) and b = bodies.(rb) in
  let k =
    (Resolve.inverse_mass a *. Vec2.dot lin_a lin_a) +. (Resolve.inverse_inertia a *. ang_a *. ang_a)
    +. (Resolve.inverse_mass b *. Vec2.dot lin_b lin_b) +. (Resolve.inverse_inertia b *. ang_b *. ang_b)
  in
  if k = 0. then None else Some { ra; rb; lin_a; ang_a; lin_b; ang_b; mass = 1. /. k; target; lo; hi; total = 0. }

(* A row keeping the two anchors' distance along [n] at [gap] = 0: the
 * speed along it is n . (vb + wb x rb - va - wa x ra), and in 2D w x r
 * . n is w (r x n), so each body's lever is r x n *)
let along (bodies : Body.t array) (j : t) ((ra_, rb_) : Vec2.t * Vec2.t) (n : Vec2.t) ~(bias : float) ~lo ~hi : row option =
  make_row bodies j.a j.b (Vec2.scale (-1.) n) (-.Vec2.cross ra_ n) n (Vec2.cross rb_ n) ~target:(-.bias) ~lo ~hi

let rows ~(beta : float) ~(dt : float) (bodies : Body.t array) (angles : float array) (j : t) : row list =
  let arm_a = arm angles j.a j.anchor_a and arm_b = arm angles j.b j.anchor_b in
  let pa = Vec2.add bodies.(j.a).pos arm_a and pb = Vec2.add bodies.(j.b).pos arm_b in
  let gap = Vec2.sub pb pa in
  match j.kind with
  | Pin { motor } ->
      (* x and y, each driven towards closing its part of the gap *)
      let axis n = along bodies j (arm_a, arm_b) n ~bias:(beta /. dt *. Vec2.dot gap n) ~lo:neg_infinity ~hi:infinity in
      let turning =
        match motor with
        | None -> None
        | Some (speed, torque) ->
            make_row bodies j.a j.b (0., 0.) (-1.) (0., 0.) 1. ~target:speed ~lo:(-.torque *. dt) ~hi:(torque *. dt)
      in
      List.filter_map Fun.id [ axis (1., 0.); axis (0., 1.); turning ]
  | Rod { length } | Rope { length } ->
      let d = Vec2.length gap in
      if d < 1e-9 then []
      else
        let n = Vec2.scale (1. /. d) gap in
        let stretch = d -. length in
        let rope = match j.kind with Rope _ -> true | _ -> false in
        (* a rope is a row only when taut, and then it may only pull:
         * its impulse, along n from a to b, never positive *)
        if rope && stretch < 0. then []
        else Option.to_list (along bodies j (arm_a, arm_b) n ~bias:(beta /. dt *. stretch) ~lo:neg_infinity ~hi:(if rope then 0. else infinity))
  | Pulley { ground_a; ground_b; length } ->
      let side p g = let v = Vec2.sub p g in let l = Vec2.length v in (l, if l < 1e-9 then (0., 0.) else Vec2.scale (1. /. l) v) in
      let la, ua = side pa ground_a and lb, ub = side pb ground_b in
      let stretch = la +. lb -. length in
      if stretch < 0. then []
      else
        (* the total length grows at ua . (point a's velocity) + ub .
         * (point b's): both bodies on the "plus" side of the row; it
         * may only pull *)
        Option.to_list
          (make_row bodies j.a j.b ua (Vec2.cross arm_a ua) ub (Vec2.cross arm_b ub) ~target:(-.beta /. dt *. stretch)
             ~lo:neg_infinity ~hi:0.)

let solve_row (bodies : Body.t array) (r : row) : unit =
  let a = bodies.(r.ra) and b = bodies.(r.rb) in
  let speed = Vec2.dot r.lin_a a.vel +. (r.ang_a *. a.spin) +. Vec2.dot r.lin_b b.vel +. (r.ang_b *. b.spin) in
  let before = r.total in
  r.total <- Float.max r.lo (Float.min r.hi (before +. (r.mass *. (r.target -. speed))));
  let p = r.total -. before in
  let push (x : Body.t) lin ang =
    { x with vel = Vec2.add x.vel (Vec2.scale (p *. Resolve.inverse_mass x) lin); spin = x.spin +. (p *. Resolve.inverse_inertia x *. ang) }
  in
  bodies.(r.ra) <- push bodies.(r.ra) r.lin_a r.ang_a;
  bodies.(r.rb) <- push bodies.(r.rb) r.lin_b r.ang_b
