(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* physics/2d/Kepler and Planets: Kepler's three laws, his equation's
 * worked example, and the Earth on 2000-01-01 *)

let t = Testo.create

let planet name = (List.find (fun (p : Planets.planet) -> p.name = name) Planets.planets).orbit
let distance (x, y, z) = sqrt ((x *. x) +. (y *. y) +. (z *. z))

(* the third law: T = a^1.5; Jupiter's 11.87 years, 4334 days, the fact
 * sheet's 4331 within 0.1% *)
let test_third_law () =
  Alcotest.(check (float 1e-12)) "the Earth: a year" 1. (Kepler.period 1.);
  let jupiter = Kepler.period (planet "Jupiter").a *. 365.25 in
  Alcotest.(check (float 5.)) "Jupiter, 4331 days" 4331. jupiter

(* Kepler.mli's example, and the equation solved whatever e < 1 *)
let test_equation () =
  Alcotest.(check (float 1e-9)) "e = 0: E = M" 1.234 (Kepler.eccentric_anomaly ~e:0. 1.234);
  Alcotest.(check (float 1e-9)) "e = 0.5, M = 90 degrees" 2.0209799381 (Kepler.eccentric_anomaly ~e:0.5 (Float.pi /. 2.));
  let st = Random.State.make [| 29 |] in
  for _ = 1 to 1000 do
    let e = Random.State.float st 0.95 and m = Random.State.float st (2. *. Float.pi) in
    let x = Kepler.eccentric_anomaly ~e m in
    if Float.abs (x -. (e *. sin x) -. m) > 1e-9 then Alcotest.failf "e = %g, M = %g: E = %g off" e m x
  done

(* on 2000-01-01, the Earth is 0.9833 AU from the Sun, at longitude
 * 100.38 degrees (the Sun, seen from it, at 280.38: in Sagittarius);
 * its perihelion comes 2 or 3 days later *)
let test_earth () =
  let earth = planet "Earth" in
  let (x, y, z) = Kepler.position earth ~days:0. in
  Alcotest.(check (float 1e-6)) "0.9833 AU" 0.9833074 (distance (x, y, z));
  Alcotest.(check (float 1e-4)) "at 100.38 degrees" 100.38018 (Float.atan2 y x *. 180. /. Float.pi);
  let nearest =
    List.fold_left
      (fun best d -> if distance (Kepler.position earth ~days:d) < distance (Kepler.position earth ~days:best) then d else best)
      0. (List.init 150 (fun i -> float_of_int i /. 10.))
  in
  if nearest < 1. || nearest > 4. then Alcotest.failf "the perihelion on day %g, not early January" nearest

(* the first law: Mercury between a (1 - e) and a (1 + e); the second:
 * equal areas in equal times (r x v constant); and its tilt *)
let test_first_second_laws () =
  let mercury = planet "Mercury" in
  let days = List.init 880 (fun i -> float_of_int i /. 10.) in
  let rs = List.map (fun d -> Vec2.length (Kepler.in_plane mercury ~days:d)) days in
  let lo = List.fold_left Float.min infinity rs and hi = List.fold_left Float.max 0. rs in
  Alcotest.(check (float 1e-4)) "perihelion, a (1 - e)" (mercury.a *. (1. -. mercury.e)) lo;
  Alcotest.(check (float 1e-4)) "aphelion, a (1 + e)" (mercury.a *. (1. +. mercury.e)) hi;
  let area d = Vec2.cross (Kepler.in_plane mercury ~days:d) (Kepler.in_plane mercury ~days:(d +. 0.01)) in
  let areas = List.map area days in
  let amin = List.fold_left Float.min infinity areas and amax = List.fold_left Float.max 0. areas in
  if (amax -. amin) /. amax > 1e-3 then Alcotest.failf "unequal areas: %g to %g" amin amax;
  let tilt = List.fold_left (fun m d -> let (_, _, z) = Kepler.position mercury ~days:d in Float.max m (Float.abs z)) 0. days in
  if tilt <= 0. || tilt > hi *. sin (mercury.inclination *. Float.pi /. 180.) then Alcotest.failf "Mercury's z up to %g" tilt

let tests =
  Testo.categorize "Kepler"
    [
      t "the third law: Jupiter's 4331 days" test_third_law;
      t "Kepler's equation, by Newton's method" test_equation;
      t "the Earth on 2000-01-01" test_earth;
      t "Mercury: an ellipse, equal areas, tilted" test_first_second_laws;
    ]
