(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Solver.mli *)

type options = {
  iterations : int;
  warm_starting : bool;
  baumgarte : float;
  slop : float;
  bounce_threshold : float;
  matching : float;
}

let default = { iterations = 10; warm_starting = true; baumgarte = 0.2; slop = 0.5; bounce_threshold = 50.; matching = 3. }

type pair = { a : int; b : int; contacts : Contact.t list; restitution : float; friction : float }

module Pairs = Map.Make (struct
  type t = int * int

  let compare = compare
end)

(* a contact point's impulses, along the normal and the tangent *)
type remembered = { at : Vec2.t; normal_impulse : float; tangent_impulse : float }
type memory = remembered list Pairs.t

let nothing = Pairs.empty

(* a contact point being solved: what doesn't change during the
 * iterations, computed once, and its accumulated impulses *)
type point = {
  a : int;
  b : int;
  p : Vec2.t;
  n : Vec2.t;
  t : Vec2.t;
  (* 1 / resistance, along n and t *)
  mass_n : float;
  mass_t : float;
  (* the separating speed asked for: Baumgarte's, or the bounce's *)
  bias : float;
  friction : float;
  mutable pn : float;
  mutable pt : float;
}

let solve (o : options) ~(dt : float) (bodies : Body.t array) (pairs : pair list) (memory : memory) : Body.t array * memory =
  let bodies = Array.copy bodies in
  let apply (pt : point) (impulse : Vec2.t) =
    let (a, b) = Resolve.apply 1. impulse pt.p (bodies.(pt.a), bodies.(pt.b)) in
    bodies.(pt.a) <- a;
    bodies.(pt.b) <- b
  in
  let prepare (pr : pair) (c : Contact.t) : point option =
    let a = bodies.(pr.a) and b = bodies.(pr.b) in
    let n = c.normal and t = Vec2.perp c.normal in
    let k_n = Resolve.resistance a b c.point n and k_t = Resolve.resistance a b c.point t in
    (* two immovable bodies: nothing to solve *)
    if k_n = 0. then None
    else
      let vn = Vec2.dot (Resolve.relative_velocity a b c.point) n in
      let bounce = if vn < -.o.bounce_threshold then -.pr.restitution *. vn else 0. in
      let bias = Float.max (o.baumgarte /. dt *. Float.max 0. (c.depth -. o.slop)) bounce in
      (* warm starting: the same point at the previous step, if any *)
      let before =
        if not o.warm_starting then None
        else
          Option.bind (Pairs.find_opt (pr.a, pr.b) memory)
            (List.find_opt (fun r -> Vec2.length (Vec2.sub r.at c.point) < o.matching))
      in
      let (pn, pt) = match before with Some r -> (r.normal_impulse, r.tangent_impulse) | None -> (0., 0.) in
      Some
        { a = pr.a; b = pr.b; p = c.point; n; t; mass_n = 1. /. k_n; mass_t = 1. /. k_t; bias; friction = pr.friction; pn; pt }
  in
  let points = List.concat_map (fun pr -> List.filter_map (prepare pr) pr.contacts) pairs in
  List.iter (fun pt -> apply pt (Vec2.add (Vec2.scale pt.pn pt.n) (Vec2.scale pt.pt pt.t))) points;
  for _ = 1 to o.iterations do
    points
    |> List.iter (fun pt ->
           let rel () = Resolve.relative_velocity bodies.(pt.a) bodies.(pt.b) pt.p in
           (* along the normal: towards the separating speed asked for,
            * the sum of the impulses never pulling *)
           let before = pt.pn in
           pt.pn <- Float.max 0. (before +. (pt.mass_n *. (pt.bias -. Vec2.dot (rel ()) pt.n)));
           apply pt (Vec2.scale (pt.pn -. before) pt.n);
           (* friction: towards no sliding, within mu times the normal
            * impulse (Coulomb) *)
           let before = pt.pt and limit = pt.friction *. pt.pn in
           pt.pt <- Float.max (-.limit) (Float.min limit (before -. (pt.mass_t *. Vec2.dot (rel ()) pt.t)));
           apply pt (Vec2.scale (pt.pt -. before) pt.t))
  done;
  let remember m pt =
    let r = { at = pt.p; normal_impulse = pt.pn; tangent_impulse = pt.pt } in
    Pairs.update (pt.a, pt.b) (fun l -> Some (r :: Option.value ~default:[] l)) m
  in
  (bodies, List.fold_left remember Pairs.empty points)

let impulses (memory : memory) (ab : int * int) : float list =
  List.rev_map (fun r -> r.normal_impulse) (Option.value ~default:[] (Pairs.find_opt ab memory))
