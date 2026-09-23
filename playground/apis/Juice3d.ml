(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Juice3d.mli *)

open Playground
open Playground3d

(*****************************************************************************)
(* The effects' clock *)
(*****************************************************************************)

(* a particle in the world, as juice/Emitter.mli's in the plane *)
type particle = {
  x : number;
  y : number;
  z : number;
  vx : number;
  vy : number;
  vz : number;
  angle : number;
  spin : number;
  size : number;
  grow : number; (* its size's factor at the end of its life: < 1 shrinks *)
  age : number;
  life : number;
  gravity : number;
  color : color;
}

type t = {
  (* the clock, the freeze and the flash: Juice's own, with no shake and
   * no particles of its own *)
  juice : Juice.t;
  seed : int;
  trauma : number;
  particles : particle list; (* the newest first *)
  (* how many random numbers were drawn: the next one's index in the
   * hash (juice/Hash.mli), so that the same seed, the same bursts *)
  drawn : int;
}

let none ~(seed : int) : t = { juice = Juice.none ~seed; seed; trauma = 0.; particles = []; drawn = 0 }

let dt = 1. /. 60.

let on (fx : t) : bool = Juice.on fx.juice

(* a particle one frame later: flying, falling, bouncing once off the
 * ground with half its speed *)
let move (p : particle) : particle option =
  let age = p.age +. dt in
  if age >= p.life then None
  else
    let vy = p.vy -. (p.gravity *. dt) in
    let y = p.y +. (vy *. dt) in
    let y, vy = if y < 0. && vy < 0. then (0., -.vy *. 0.5) else (y, vy) in
    Some { p with age; y; vy; x = p.x +. (p.vx *. dt); z = p.z +. (p.vz *. dt); angle = p.angle +. (p.spin *. dt) }

let step (computer : computer) (fx : t) : t =
  let juice = Juice.step computer fx.juice in
  if not (Juice.on juice) then { (none ~seed:fx.seed) with juice }
  else { fx with juice; trauma = Trauma.decay ~dt fx.trauma; particles = List.filter_map move fx.particles }

let now (fx : t) : time = Juice.now fx.juice
let juice (fx : t) : Juice.t = fx.juice

(*****************************************************************************)
(* Effects that last *)
(*****************************************************************************)

let shake (trauma : number) (fx : t) : t = if on fx then { fx with trauma = Trauma.add trauma fx.trauma } else fx
let freeze (frames : int) (fx : t) : t = { fx with juice = Juice.freeze frames fx.juice }
let frozen (fx : t) : bool = Juice.frozen fx.juice
let flash (color : color) (frames : int) (fx : t) : t = { fx with juice = Juice.flash color frames fx.juice }

(* a recipe: how many, how fast (world units a second), how much of it
 * upwards (0 all around, 1 straight up), how long, how big, and the
 * colors *)
type burst = {
  count : int;
  speed : number * number;
  up : number;
  life : number * number;
  size : number * number;
  grow : number;
  gravity : number;
  palette : color list;
}

let sparks : burst =
  { count = 20; speed = (6., 14.); up = 0.3; life = (0.2, 0.5); size = (0.12, 0.25); grow = 0.2; gravity = 15.;
    palette = [ white; yellow; orange ] }

let smoke : burst =
  { count = 4; speed = (0.3, 1.); up = 0.8; life = (0.5, 1.); size = (0.12, 0.25); grow = 2.5; gravity = -1.;
    palette = [ gray; darkGray; rgb 180 180 180 ] }

let debris (c : color) : burst =
  { count = 16; speed = (4., 10.); up = 0.6; life = (0.8, 1.5); size = (0.3, 0.7); grow = 0.5; gravity = 20.;
    palette = [ c; darkGray ] }

let burst ~(at : number * number * number) (b : burst) (fx : t) : t =
  if not (on fx) then fx
  else
    let x, y, z = at in
    let between (lo, hi) i = lo +. ((hi -. lo) *. Hash.unit ~seed:fx.seed (fx.drawn + i)) in
    let n = List.length b.palette in
    let one k =
      (* six numbers per particle *)
      let i = 6 * k in
      let heading = between (0., 2. *. Float.pi) i and speed = between b.speed (i + 1) in
      (* upwards by [up], the rest of the way across *)
      let rise = b.up +. ((1. -. b.up) *. between (-1., 1.) (i + 2)) in
      let across = Float.sqrt (Float.max 0. (1. -. (rise *. rise))) in
      { x; y; z;
        vx = speed *. across *. cos heading; vy = speed *. rise; vz = speed *. across *. sin heading;
        angle = 0.; spin = between (-400., 400.) (i + 3); size = between b.size (i + 4); grow = b.grow;
        age = 0.; life = between b.life (i + 5); gravity = b.gravity;
        color = List.nth b.palette (k mod n) }
    in
    let fresh = List.init b.count one in
    { fx with particles = List.filteri (fun i _ -> i < 300) (fresh @ fx.particles); drawn = fx.drawn + (6 * b.count) }

(*****************************************************************************)
(* Drawing *)
(*****************************************************************************)

let sub (x1, y1, z1) (x2, y2, z2) = (x1 -. x2, y1 -. y2, z1 -. z2)
let add (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)
let times k (x, y, z) = (k *. x, k *. y, k *. z)
let cross (x1, y1, z1) (x2, y2, z2) = ((y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2))

let normalize v =
  let x, y, z = v in
  let n = Float.sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  if n = 0. then v else times (1. /. n) v

(* The shake of Juice.view (juice/Trauma.mli), in the camera's own
 * frame: [dx] along its right, [dy] along its up, [angle] a roll
 * round the way it looks. The eye and the target move together, so
 * the view slides without turning:
 *
 *            up                    the shaken eye
 *             ^                     +- - - - - -> the shaken target
 *             |   dy               ^
 *             +-----> right        | (dx, dy)
 *                                  eye ---------> target
 *)
let camera (fx : t) (cam : camera) : camera =
  if fx.trauma <= 0. then cam
  else
    let (Time clock) = now fx in
    let o = Trauma.offset ~max_offset:0.3 ~max_angle:3. ~seed:fx.seed ~trauma:fx.trauma clock in
    let forward = normalize (sub cam.target cam.eye) in
    let right = normalize (cross forward cam.up) in
    let up = cross right forward in
    let shift = add (times o.dx right) (times o.dy up) in
    let a = o.angle *. Float.pi /. 180. in
    { cam with eye = add cam.eye shift; target = add cam.target shift; up = add (times (cos a) up) (times (sin a) right) }

let particle (p : particle) : shape3d =
  let t = p.age /. p.life in
  cube p.color (p.size *. (1. +. ((p.grow -. 1.) *. t))) |> rotate3d p.angle (p.angle *. 0.7) 0. |> move3d p.x p.y p.z

let view (fx : t) (world : shape3d list) : shape3d list =
  (* Juice.view with nothing to shake: only its flash, if any *)
  world @ List.map particle fx.particles @ List.map hud (Juice.view fx.juice [])
