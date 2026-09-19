(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Particles.mli *)

type particle = { pos : Vec2.t; old : Vec2.t; pinned : bool }
type stick = { a : int; b : int; length : float }

let particle ?(pinned = false) (pos : Vec2.t) : particle = { pos; old = pos; pinned }
let velocity ~(dt : float) (p : particle) : Vec2.t = Vec2.scale (1. /. dt) (Vec2.sub p.pos p.old)

let step ?(drag = 0.) ~(accel : Vec2.t) ~(dt : float) (particles : particle array) : particle array =
  particles
  |> Array.map (fun p ->
         if p.pinned then p
         else
           let moved = Vec2.scale (1. -. drag) (Vec2.sub p.pos p.old) in
           { p with old = p.pos; pos = Vec2.add (Vec2.add p.pos moved) (Vec2.scale (dt *. dt) accel) })

let relax ~(iterations : int) (sticks : stick list) (particles : particle array) : particle array =
  let ps = Array.copy particles in
  for _ = 1 to iterations do
    sticks
    |> List.iter (fun s ->
           let a = ps.(s.a) and b = ps.(s.b) in
           let d = Vec2.sub b.pos a.pos in
           let length = Vec2.length d in
           if length > 0. then
             (* the excess, as a fraction of the stick, shared by the
              * free ends *)
             let excess = Vec2.scale ((length -. s.length) /. length) d in
             match (a.pinned, b.pinned) with
             | true, true -> ()
             | true, false -> ps.(s.b) <- { b with pos = Vec2.sub b.pos excess }
             | false, true -> ps.(s.a) <- { a with pos = Vec2.add a.pos excess }
             | false, false ->
                 let half = Vec2.scale 0.5 excess in
                 ps.(s.a) <- { a with pos = Vec2.add a.pos half };
                 ps.(s.b) <- { b with pos = Vec2.sub b.pos half })
  done;
  ps

let rope ~(from : Vec2.t) ~(towards : Vec2.t) (n : int) : particle array * stick list =
  let at i = Vec2.add from (Vec2.scale (float_of_int i /. float_of_int (n - 1)) (Vec2.sub towards from)) in
  let length = Vec2.length (Vec2.sub towards from) /. float_of_int (n - 1) in
  (Array.init n (fun i -> particle ~pinned:(i = 0) (at i)), List.init (n - 1) (fun i -> { a = i; b = i + 1; length }))
