(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Kepler.mli *)

type elements = { a : float; e : float; inclination : float; mean_longitude : float; perihelion : float; node : float }

let radians d = d *. Float.pi /. 180.
let period (a : float) : float = a ** 1.5

let eccentric_anomaly ~(e : float) (m : float) : float =
  (* Newton's method on f(E) = E - e sin E - M, f'(E) = 1 - e cos E;
   * a handful of iterations for the planets' e < 0.21 *)
  let rec go n x =
    let dx = (x -. (e *. sin x) -. m) /. (1. -. (e *. cos x)) in
    if n = 0 || Float.abs dx < 1e-12 then x -. dx else go (n - 1) (x -. dx)
  in
  go 30 m

let mean_anomaly (el : elements) ~(days : float) : float =
  radians (el.mean_longitude -. el.perihelion) +. (2. *. Float.pi *. days /. (365.25 *. period el.a))

let in_plane (el : elements) ~(days : float) : Vec2.t =
  let e = eccentric_anomaly ~e:el.e (mean_anomaly el ~days) in
  (el.a *. (cos e -. el.e), el.a *. sqrt (1. -. (el.e *. el.e)) *. sin e)

let position (el : elements) ~(days : float) : float * float * float =
  let (x', y') = in_plane el ~days in
  (* the argument of perihelion, the node, the inclination (JPL's
   * rotations) *)
  let w = radians (el.perihelion -. el.node) and o = radians el.node and i = radians el.inclination in
  let cw = cos w and sw = sin w and co = cos o and so = sin o and ci = cos i and si = sin i in
  ( (((cw *. co) -. (sw *. so *. ci)) *. x') +. ((-.(sw *. co) -. (cw *. so *. ci)) *. y'),
    (((cw *. so) +. (sw *. co *. ci)) *. x') +. ((-.(sw *. so) +. (cw *. co *. ci)) *. y'),
    (sw *. si *. x') +. (cw *. si *. y') )
