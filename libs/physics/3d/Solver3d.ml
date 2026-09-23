(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Solver3d.mli *)

type options = {
  iterations : int;
  warm_starting : bool;
  baumgarte : float;
  slop : float;
  bounce_threshold : float;
  matching : float;
}

(* in metres and seconds, where the 2D engine's are in pixels: 5 mm of
 * tolerated overlap, a metre a second under which nothing bounces, and
 * a contact point within 3 cm of last step's is the same one *)
let default =
  { iterations = 10; warm_starting = true; baumgarte = 0.2; slop = 0.005; bounce_threshold = 1.; matching = 0.03 }

type pair = { a : int; b : int; contacts : Contact3d.t list; restitution : float; friction : float }

module Pairs = Map.Make (struct
  type t = int * int

  let compare = compare
end)

(* A contact point's impulses: the normal one as a number, friction as
 * a *vector*. Friction is solved along two tangents chosen from the
 * normal (Resolve3d.tangents), and that choice jumps by a quarter turn
 * when the normal wobbles across a tie -- so remembering two numbers
 * means replaying last step's friction along this step's axes, which
 * kicks a resting pile every few dozen steps. A vector has no such
 * opinion, and is taken apart again in whatever basis the new step
 * picked. *)
type remembered = { at : Vec3.t; normal_impulse : float; friction_impulse : Vec3.t }

type memory = remembered list Pairs.t

let nothing = Pairs.empty

(* one contact point being solved: everything that does not change
 * during the iterations, worked out once, and the impulses so far *)
type point = {
  a : int;
  b : int;
  p : Vec3.t;
  n : Vec3.t;
  t1 : Vec3.t;
  t2 : Vec3.t;
  (* 1 / resistance along each of the three *)
  mass_n : float;
  mass_t1 : float;
  mass_t2 : float;
  (* the separating speed asked for: Baumgarte's, or the bounce's *)
  bias : float;
  friction : float;
  mutable pn : float;
  mutable p1 : float;
  mutable p2 : float;
}

let solve (o : options) ~(dt : float) ?(joints = []) (bodies : Body3d.t array) (pairs : pair list) (memory : memory) :
    Body3d.t array * memory =
  let bodies = Array.copy bodies in
  let apply (pt : point) (impulse : Vec3.t) =
    let a, b = Resolve3d.apply 1. impulse pt.p (bodies.(pt.a), bodies.(pt.b)) in
    bodies.(pt.a) <- a;
    bodies.(pt.b) <- b
  in
  let prepare (pr : pair) (c : Contact3d.t) : point option =
    let a = bodies.(pr.a) and b = bodies.(pr.b) in
    let n = c.Contact3d.normal in
    let t1, t2 = Resolve3d.tangents n in
    let k_n = Resolve3d.resistance a b c.Contact3d.point n in
    if k_n <= 1e-12 then None (* two immovable bodies: nothing to solve *)
    else
      let k1 = Resolve3d.resistance a b c.Contact3d.point t1 and k2 = Resolve3d.resistance a b c.Contact3d.point t2 in
      let vn = Vec3.dot (Resolve3d.relative_velocity a b c.Contact3d.point) n in
      let bounce = if vn < -.o.bounce_threshold then -.pr.restitution *. vn else 0. in
      let bias = Float.max (o.baumgarte /. dt *. Float.max 0. (c.Contact3d.depth -. o.slop)) bounce in
      (* warm starting: this point's impulses at the previous step,
       * found again by where it is *)
      let before =
        if not o.warm_starting then None
        else
          Option.bind
            (Pairs.find_opt (pr.a, pr.b) memory)
            (List.find_opt (fun r -> Vec3.length (Vec3.sub r.at c.Contact3d.point) < o.matching))
      in
      let pn, p1, p2 =
        match before with
        | Some r -> (r.normal_impulse, Vec3.dot r.friction_impulse t1, Vec3.dot r.friction_impulse t2)
        | None -> (0., 0., 0.)
      in
      Some
        { a = pr.a; b = pr.b; p = c.Contact3d.point; n; t1; t2; mass_n = 1. /. k_n;
          mass_t1 = (if k1 > 1e-12 then 1. /. k1 else 0.);
          mass_t2 = (if k2 > 1e-12 then 1. /. k2 else 0.);
          bias; friction = pr.friction; pn; p1; p2 }
  in
  let points = List.concat_map (fun pr -> List.filter_map (prepare pr) pr.contacts) pairs in
  (* the remembered impulses, put back before the first iteration *)
  List.iter
    (fun pt -> apply pt (Vec3.add (Vec3.scale pt.pn pt.n) (Vec3.add (Vec3.scale pt.p1 pt.t1) (Vec3.scale pt.p2 pt.t2))))
    points;
  (* the joints' rows (Joint3d), solved in the same iterations, before
   * the contacts, and no differently: a row is a direction and a speed
   * to reach along it, as a contact point's normal is *)
  let rows = List.concat_map (Joint3d.rows ~beta:o.baumgarte ~dt bodies) joints in
  for _ = 1 to o.iterations do
    List.iter (Joint3d.solve_row bodies) rows;
    points
    |> List.iter (fun pt ->
           let rel () = Resolve3d.relative_velocity bodies.(pt.a) bodies.(pt.b) pt.p in
           (* along the normal, towards the separating speed asked for;
            * what is clamped is the *sum* of this contact's impulses,
            * which may never pull *)
           let was = pt.pn in
           pt.pn <- Float.max 0. (was +. (pt.mass_n *. (pt.bias -. Vec3.dot (rel ()) pt.n)));
           apply pt (Vec3.scale (pt.pn -. was) pt.n);
           (* friction along each tangent, each within mu times the
            * normal impulse: the pyramid of Resolve3d.mli *)
           let limit = pt.friction *. pt.pn in
           let slide t mass current =
             let wanted = current -. (mass *. Vec3.dot (rel ()) t) in
             Float.max (-.limit) (Float.min limit wanted)
           in
           let was = pt.p1 in
           pt.p1 <- slide pt.t1 pt.mass_t1 was;
           apply pt (Vec3.scale (pt.p1 -. was) pt.t1);
           let was = pt.p2 in
           pt.p2 <- slide pt.t2 pt.mass_t2 was;
           apply pt (Vec3.scale (pt.p2 -. was) pt.t2))
  done;
  let remember m pt =
    let r =
      { at = pt.p; normal_impulse = pt.pn;
        friction_impulse = Vec3.add (Vec3.scale pt.p1 pt.t1) (Vec3.scale pt.p2 pt.t2) }
    in
    Pairs.update (pt.a, pt.b) (fun l -> Some (r :: Option.value ~default:[] l)) m
  in
  (bodies, List.fold_left remember Pairs.empty points)

let impulses (memory : memory) (ab : int * int) : float list =
  List.rev_map (fun r -> r.normal_impulse) (Option.value ~default:[] (Pairs.find_opt ab memory))
