(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Emitter.mli *)

type recipe = {
  count : int;
  speed : float * float;
  direction : float;
  spread : float;
  life : float * float;
  size : float * float;
  spin : float;
  gravity : float;
  drag : float;
}

type 'a particle = {
  x : float;
  y : float;
  vx : float;
  vy : float;
  angle : float;
  spin : float;
  size : float;
  age : float;
  life : float;
  gravity : float;
  drag : float;
  data : 'a;
}

(* the particles newest first, as a burst adds them; [draws]: how many
 * random numbers were drawn so far, the next one's point for Hash *)
type 'a t = { seed : int; cap : int; draws : int; newest_first : 'a particle list }

let empty ?(cap = 400) ~(seed : int) () : 'a t = { seed; cap; draws = 0; newest_first = [] }

(* the i-th draw from now, in [0, 1] *)
let draw (t : 'a t) (i : int) : float = Hash.unit ~seed:t.seed (t.draws + i)

let between ((lo, hi) : float * float) (u : float) : float = Tween.lerp lo hi u

let born (r : recipe) (data : float -> 'a) (x : float) (y : float) (t : 'a t) (k : int) : 'a particle =
  (* six draws a particle *)
  let u i = draw t ((6 * k) + i) in
  let speed = between r.speed (u 0) in
  let a = (r.direction +. (r.spread *. (u 1 -. 0.5))) *. Float.pi /. 180. in
  {
    x;
    y;
    vx = speed *. cos a;
    vy = speed *. sin a;
    angle = 0.;
    spin = r.spin *. ((2. *. u 2) -. 1.);
    size = between r.size (u 3);
    age = 0.;
    life = between r.life (u 4);
    gravity = r.gravity;
    drag = r.drag;
    data = data (u 5);
  }

let burst (r : recipe) ~(data : float -> 'a) (x : float) (y : float) (t : 'a t) : 'a t =
  let fresh = List.init r.count (born r data x y t) in
  let all = List.rev_append fresh t.newest_first in
  { t with draws = t.draws + (6 * r.count); newest_first = List.filteri (fun i _ -> i < t.cap) all }

(* semi-implicit Euler: the velocity first, then the position with it *)
let move ~(dt : float) (p : 'a particle) : 'a particle =
  let slow = Float.max 0. (1. -. (p.drag *. dt)) in
  let vx = p.vx *. slow and vy = (p.vy +. (p.gravity *. dt)) *. slow in
  { p with vx; vy; x = p.x +. (vx *. dt); y = p.y +. (vy *. dt); angle = p.angle +. (p.spin *. dt); age = p.age +. dt }

let step ~(dt : float) (t : 'a t) : 'a t =
  { t with newest_first = List.filter (fun p -> p.age < p.life) (List.map (move ~dt) t.newest_first) }

let particles (t : 'a t) : 'a particle list = List.rev t.newest_first
