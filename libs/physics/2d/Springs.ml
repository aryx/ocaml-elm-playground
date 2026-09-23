(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Springs.mli *)

type spring = { a : int; b : int; rest : float; k : float; damping : float }

let accelerations (bodies : Body.t array) (springs : spring list) : Vec2.t array =
  let acc = Array.make (Array.length bodies) (0., 0.) in
  springs
  |> List.iter (fun s ->
         let a = bodies.(s.a) and b = bodies.(s.b) in
         let d = Vec2.sub b.pos a.pos in
         let length = Vec2.length d in
         if length > 0. then (
           let u = Vec2.scale (1. /. length) d in
           (* the pull on a towards b (on b towards a, the opposite):
            * Hooke's, and the damping of their speed apart *)
           let f = (s.k *. (length -. s.rest)) +. (s.damping *. Vec2.dot (Vec2.sub b.vel a.vel) u) in
           acc.(s.a) <- Vec2.add acc.(s.a) (Vec2.scale (f /. a.mass) u);
           acc.(s.b) <- Vec2.sub acc.(s.b) (Vec2.scale (f /. b.mass) u)));
  acc

let step ~(gravity : Vec2.t) ~(dt : float) (bodies : Body.t array) (springs : spring list) : Body.t array =
  let acc = accelerations bodies springs in
  bodies
  |> Array.mapi (fun i (b : Body.t) ->
         if b.mass = infinity then b
         else
           let vel = Vec2.add b.vel (Vec2.scale dt (Vec2.add acc.(i) gravity)) in
           { b with vel; pos = Vec2.add b.pos (Vec2.scale dt vel) })

let chain ~(from : Vec2.t) ~(towards : Vec2.t) (n : int) ~(k : float) ~(damping : float) : Body.t array * spring list =
  let at i = Vec2.add from (Vec2.scale (float_of_int i /. float_of_int (n - 1)) (Vec2.sub towards from)) in
  let rest = Vec2.length (Vec2.sub towards from) /. float_of_int (n - 1) in
  ( Array.init n (fun i -> if i = 0 then Body.make ~mass:infinity (at i) else Body.make (at i)),
    List.init (n - 1) (fun i -> { a = i; b = i + 1; rest; k; damping }) )
