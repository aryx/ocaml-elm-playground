(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

let count = 14
let gravity = (0., -1800.)
let dt = 1. /. 60.

type t = {
  particles : Particles.particle array;
  sticks : Particles.stick list;
  still : int; (* frames without motion *)
  loose : bool; (* let go: falling *)
}

let rest_length (d : float) : float = (1.08 *. d) +. 60.
let sag ~(d : float) ~(length : float) : float = if d <= 0. then length /. 2. else sqrt (3. *. d *. Float.max 0. (length -. d) /. 8.)

let sticks_for (a : Vec2.t) (b : Vec2.t) : Particles.stick list =
  let piece = rest_length (Vec2.length (Vec2.sub b a)) /. float_of_int (count - 1) in
  List.init (count - 1) (fun i -> { Particles.a = i; b = i + 1; length = piece })

let physics (t : t) : t =
  let ps = Particles.step ~drag:0.03 ~accel:gravity ~dt t.particles in
  { t with particles = Particles.relax ~iterations:15 t.sticks ps }

let energy (t : t) : float =
  Array.fold_left (fun e (p : Particles.particle) -> let v = Vec2.sub p.pos p.old in e +. Vec2.dot v v) 0. t.particles

let make (a : Vec2.t) (b : Vec2.t) : t =
  let d = Vec2.length (Vec2.sub b a) in
  let s = sag ~d ~length:(rest_length d) in
  let at i =
    let u = float_of_int i /. float_of_int (count - 1) in
    let x, y = Vec2.add a (Vec2.scale u (Vec2.sub b a)) in
    (x, y -. (4. *. s *. u *. (1. -. u)))
  in
  let particles = Array.init count (fun i -> Particles.particle ~pinned:(i = 0 || i = count - 1) (at i)) in
  let t = ref { particles; sticks = sticks_for a b; still = 0; loose = false } in
  for _ = 1 to 90 do
    t := physics !t
  done;
  !t

let asleep (t : t) : bool = t.still >= 30

let step (t : t) (a : Vec2.t) (b : Vec2.t) : t =
  if t.loose then physics t
  else
    let last = count - 1 in
    let moved = t.particles.(0).pos <> a || t.particles.(last).pos <> b in
    if (not moved) && asleep t then t
    else begin
      let ps = Array.copy t.particles in
      ps.(0) <- { (ps.(0)) with pos = a; old = a };
      ps.(last) <- { (ps.(last)) with pos = b; old = b };
      let t = physics { t with particles = ps; sticks = (if moved then sticks_for a b else t.sticks) } in
      let quiet = Array.for_all (fun (p : Particles.particle) -> Vec2.length (Vec2.sub p.pos p.old) < 0.05) t.particles in
      { t with still = (if quiet && not moved then t.still + 1 else 0) }
    end

let release (t : t) : t = { t with loose = true; particles = Array.map (fun (p : Particles.particle) -> { p with pinned = false }) t.particles }
let points (t : t) : Vec2.t list = Array.to_list (Array.map (fun (p : Particles.particle) -> p.pos) t.particles)

(* Catmull-Rom between p1 and p2, at u in [0, 1] *)
let catmull (p0 : Vec2.t) (p1 : Vec2.t) (p2 : Vec2.t) (p3 : Vec2.t) (u : float) : Vec2.t =
  let u2 = u *. u in
  let u3 = u2 *. u in
  let f a b c d = 0.5 *. ((2. *. b) +. ((c -. a) *. u) +. (((2. *. a) -. (5. *. b) +. (4. *. c) -. d) *. u2) +. (((3. *. b) -. a -. (3. *. c) +. d) *. u3)) in
  let (x0, y0), (x1, y1), (x2, y2), (x3, y3) = (p0, p1, p2, p3) in
  (f x0 x1 x2 x3, f y0 y1 y2 y3)

let ribbon (t : t) ~(width : float) : Vec2.t list =
  let p = Array.map (fun (q : Particles.particle) -> q.pos) t.particles in
  let n = Array.length p in
  let get i = p.(max 0 (min (n - 1) i)) in
  let smooth =
    Array.of_list
      (List.concat (List.init (n - 1) (fun i -> List.init 3 (fun k -> catmull (get (i - 1)) (get i) (get (i + 1)) (get (i + 2)) (float_of_int k /. 3.))))
      @ [ p.(n - 1) ])
  in
  let m = Array.length smooth in
  let normal i =
    let a = smooth.(max 0 (i - 1)) and b = smooth.(min (m - 1) (i + 1)) in
    let dx, dy = Vec2.sub b a in
    let l = Float.max 1e-9 (sqrt ((dx *. dx) +. (dy *. dy))) in
    (-.dy /. l, dx /. l)
  in
  let side sign = List.init m (fun i -> Vec2.add smooth.(i) (Vec2.scale (sign *. width /. 2.) (normal i))) in
  side 1. @ List.rev (side (-1.))
