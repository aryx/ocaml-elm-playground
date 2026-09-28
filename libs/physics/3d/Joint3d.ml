(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

type kind =
  | Ball of { cone : (Vec3.t * Vec3.t * float) option }
  | Hinge of {
      axis_a : Vec3.t;
      axis_b : Vec3.t;
      ref_a : Vec3.t;
      ref_b : Vec3.t;
      limits : (float * float) option;
      motor : (float * float) option;
    }
  | Distance of { length : float }

type t = { a : int; b : int; anchor_a : Vec3.t; anchor_b : Vec3.t; kind : kind }

(* a world point or direction in a body's own frame, and back *)
let to_local (body : Body3d.t) (p : Vec3.t) : Vec3.t = Quat.rotate (Quat.conjugate body.orientation) (Vec3.sub p body.pos)
let dir_local (body : Body3d.t) (d : Vec3.t) : Vec3.t = Quat.rotate (Quat.conjugate body.orientation) d
let to_world (body : Body3d.t) (p : Vec3.t) : Vec3.t = Vec3.add body.pos (Quat.rotate body.orientation p)
let dir_world (body : Body3d.t) (d : Vec3.t) : Vec3.t = Quat.rotate body.orientation d

(* some direction across [d] *)
let across (d : Vec3.t) : Vec3.t = fst (Resolve3d.tangents d)

let ball (bodies : Body3d.t array) (a : int) (b : int) ~(at : Vec3.t) ?cone () : t =
  let cone =
    Option.map
      (fun (axis, most) ->
        let axis = Vec3.normalize axis in
        (dir_local bodies.(a) axis, dir_local bodies.(b) axis, most))
      cone
  in
  { a; b; anchor_a = to_local bodies.(a) at; anchor_b = to_local bodies.(b) at; kind = Ball { cone } }

let hinge (bodies : Body3d.t array) (a : int) (b : int) ~(at : Vec3.t) ~(axis : Vec3.t) ?limits ?motor () : t =
  let axis = Vec3.normalize axis in
  let r = across axis in
  { a; b; anchor_a = to_local bodies.(a) at; anchor_b = to_local bodies.(b) at;
    kind =
      Hinge
        { axis_a = dir_local bodies.(a) axis; axis_b = dir_local bodies.(b) axis; ref_a = dir_local bodies.(a) r;
          ref_b = dir_local bodies.(b) r; limits; motor } }

let distance (bodies : Body3d.t array) (a : int) (b : int) ~(at_a : Vec3.t) ~(at_b : Vec3.t) () : t =
  { a; b; anchor_a = to_local bodies.(a) at_a; anchor_b = to_local bodies.(b) at_b;
    kind = Distance { length = Vec3.length (Vec3.sub at_b at_a) } }

let anchors (bodies : Body3d.t array) (j : t) : Vec3.t * Vec3.t =
  (to_world bodies.(j.a) j.anchor_a, to_world bodies.(j.b) j.anchor_b)

(* the signed angle from [u] to [v] about the unit [axis] *)
let signed_angle (axis : Vec3.t) (u : Vec3.t) (v : Vec3.t) : float = atan2 (Vec3.dot (Vec3.cross u v) axis) (Vec3.dot u v)

let angle (bodies : Body3d.t array) (j : t) : float =
  match j.kind with
  | Hinge h ->
      let axis = dir_world bodies.(j.a) h.axis_a in
      signed_angle axis (dir_world bodies.(j.a) h.ref_a) (dir_world bodies.(j.b) h.ref_b)
  | _ -> 0.

(*****************************************************************************)
(* Rows *)
(*****************************************************************************)

(* A row pushes along [dir]: at [point] ([Some]), a linear impulse, like
 * a contact's; or ([None]) a turning one, spin against spin. [mass] is
 * 1 / how the pair resists it; [target] the speed asked along it. *)
type row = {
  ra : int;
  rb : int;
  point : Vec3.t option;
  dir : Vec3.t;
  mass : float;
  target : float;
  lo : float;
  hi : float;
  mutable acc : float;
}

let turning_resistance (a : Body3d.t) (b : Body3d.t) (d : Vec3.t) : float =
  Vec3.dot d (Mat3.mul_vec (Resolve3d.inverse_inertia a) d) +. Vec3.dot d (Mat3.mul_vec (Resolve3d.inverse_inertia b) d)

let speed_along (bodies : Body3d.t array) (r : row) : float =
  let a = bodies.(r.ra) and b = bodies.(r.rb) in
  match r.point with
  | Some p -> Vec3.dot (Resolve3d.relative_velocity a b p) r.dir
  | None -> Vec3.dot (Vec3.sub b.spin a.spin) r.dir

let push (bodies : Body3d.t array) (r : row) (j : float) : unit =
  let a = bodies.(r.ra) and b = bodies.(r.rb) in
  match r.point with
  | Some p ->
      let a, b = Resolve3d.apply j r.dir p (a, b) in
      bodies.(r.ra) <- a;
      bodies.(r.rb) <- b
  | None ->
      let l = Vec3.scale j r.dir in
      bodies.(r.ra) <- { a with spin = Vec3.sub a.spin (Mat3.mul_vec (Resolve3d.inverse_inertia a) l) };
      bodies.(r.rb) <- { b with spin = Vec3.add b.spin (Mat3.mul_vec (Resolve3d.inverse_inertia b) l) }

let row ?(lo = neg_infinity) ?(hi = infinity) (bodies : Body3d.t array) (j : t) (point : Vec3.t option) (dir : Vec3.t)
    (target : float) : row option =
  let a = bodies.(j.a) and b = bodies.(j.b) in
  let k = match point with Some p -> Resolve3d.resistance a b p dir | None -> turning_resistance a b dir in
  if k <= 1e-12 then None else Some { ra = j.a; rb = j.b; point; dir; mass = 1. /. k; target; lo; hi; acc = 0. }

let rows ~(beta : float) ~(dt : float) (bodies : Body3d.t array) (j : t) : row list =
  let a = bodies.(j.a) and b = bodies.(j.b) in
  let pa, pb = anchors bodies j in
  let mid = Vec3.scale 0.5 (Vec3.add pa pb) in
  let gap = Vec3.sub pb pa in
  (* the anchors together: along each axis of the world, b's anchor
   * asked to come back towards a's at beta of the gap per step *)
  let together () =
    List.filter_map
      (fun e -> row bodies j (Some mid) e (-.beta /. dt *. Vec3.dot gap e))
      [ (1., 0., 0.); (0., 1., 0.); (0., 0., 1.) ]
  in
  match j.kind with
  | Distance { length } ->
      let d = Vec3.length gap in
      if d < 1e-9 then []
      else
        let dir = Vec3.scale (1. /. d) gap in
        Option.to_list (row bodies j (Some mid) dir (-.beta /. dt *. (d -. length)))
  | Ball { cone } ->
      let limit =
        match cone with
        | None -> []
        | Some (ca, cb, most) ->
            let ca = dir_world a ca and cb = dir_world b cb in
            let c = Float.max (-1.) (Float.min 1. (Vec3.dot ca cb)) in
            let angle = Float.acos c in
            let n = Vec3.cross ca cb in
            if angle <= most || Vec3.length n < 1e-9 then []
            else
              (* past the cone: b may only turn back towards a's axis,
               * along -n, never further out *)
              let n = Vec3.normalize n in
              Option.to_list (row ~hi:0. bodies j None n (-.beta /. dt *. (angle -. most)))
      in
      together () @ limit
  | Hinge h ->
      let axis = dir_world a h.axis_a and axis_b = dir_world b h.axis_b in
      (* the axes lined up: no turning across the hinge, and back
       * towards lined up as far as they have drifted *)
      let off = Vec3.cross axis axis_b in
      let t1, t2 = Resolve3d.tangents axis in
      let lined = List.filter_map (fun t -> row bodies j None t (-.beta /. dt *. Vec3.dot off t)) [ t1; t2 ] in
      let angle = angle bodies j in
      let limits =
        match h.limits with
        | Some (least, _) when angle < least -> Option.to_list (row ~lo:0. bodies j None axis (beta /. dt *. (least -. angle)))
        | Some (_, most) when angle > most -> Option.to_list (row ~hi:0. bodies j None axis (-.beta /. dt *. (angle -. most)))
        | _ -> []
      in
      let motor =
        match h.motor with
        | Some (speed, torque) ->
            let most = torque *. dt in
            Option.to_list (row ~lo:(-.most) ~hi:most bodies j None axis speed)
        | None -> []
      in
      together () @ lined @ limits @ motor

let solve_row (bodies : Body3d.t array) (r : row) : unit =
  let wanted = r.acc +. (r.mass *. (r.target -. speed_along bodies r)) in
  let clamped = Float.max r.lo (Float.min r.hi wanted) in
  push bodies r (clamped -. r.acc);
  r.acc <- clamped
