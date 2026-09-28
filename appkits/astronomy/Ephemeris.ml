(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Ephemeris.mli *)

type planet = Mercury | Venus | Mars | Jupiter | Saturn

let planets = [ Mercury; Venus; Mars; Jupiter; Saturn ]

let planet_name (p : planet) : string =
  match p with Mercury -> "Mercury" | Venus -> "Venus" | Mars -> "Mars" | Jupiter -> "Jupiter" | Saturn -> "Saturn"

let rad = Celestial.degrees

(*****************************************************************************)
(* The planets: Kepler's ellipses *)
(*****************************************************************************)

(* Standish's table 1: a (AU), e, I, L, varpi, Omega (degrees) at
 * J2000, then their rates per century. The Earth's is the Earth-Moon
 * barycentre's, 4700 km from the Earth's centre: 6 arcsec of the Sun's
 * position. *)
type elements = { a : float; e : float; i : float; l : float; varpi : float; omega : float }

let elements (p : planet option) : elements * elements =
  match p with
  | Some Mercury ->
      ( { a = 0.38709927; e = 0.20563593; i = 7.00497902; l = 252.25032350; varpi = 77.45779628; omega = 48.33076593 },
        { a = 0.00000037; e = 0.00001906; i = -0.00594749; l = 149472.67411175; varpi = 0.16047689; omega = -0.12534081 } )
  | Some Venus ->
      ( { a = 0.72333566; e = 0.00677672; i = 3.39467605; l = 181.97909950; varpi = 131.60246718; omega = 76.67984255 },
        { a = 0.00000390; e = -0.00004107; i = -0.00078890; l = 58517.81538729; varpi = 0.00268329; omega = -0.27769418 } )
  | None ->
      ( { a = 1.00000261; e = 0.01671123; i = -0.00001531; l = 100.46457166; varpi = 102.93768193; omega = 0.0 },
        { a = 0.00000562; e = -0.00004392; i = -0.01294668; l = 35999.37244981; varpi = 0.32327364; omega = 0.0 } )
  | Some Mars ->
      ( { a = 1.52371034; e = 0.09339410; i = 1.84969142; l = -4.55343205; varpi = -23.94362959; omega = 49.55953891 },
        { a = 0.00001847; e = 0.00007882; i = -0.00813131; l = 19140.30268499; varpi = 0.44441088; omega = -0.29257343 } )
  | Some Jupiter ->
      ( { a = 5.20288700; e = 0.04838624; i = 1.30439695; l = 34.39644051; varpi = 14.72847983; omega = 100.47390909 },
        { a = -0.00011607; e = -0.00013253; i = -0.00183714; l = 3034.74612775; varpi = 0.21252668; omega = 0.20469106 } )
  | Some Saturn ->
      ( { a = 9.53667594; e = 0.05386179; i = 2.48599187; l = 49.95424423; varpi = 92.59887831; omega = 113.66242448 },
        { a = -0.00125060; e = -0.00050991; i = 0.00193609; l = 1222.49362201; varpi = -0.41897216; omega = -0.28867794 } )

(* Newton's method on f(E) = E - e sin E - m, from E = m + e sin m:
 * a handful of steps for the planets' small eccentricities *)
let kepler ~(e : float) (m : float) : float =
  let rec go e_anomaly n =
    let d = (e_anomaly -. (e *. sin e_anomaly) -. m) /. (1. -. (e *. cos e_anomaly)) in
    let e_anomaly = e_anomaly -. d in
    if Float.abs d < 1e-12 || n = 0 then e_anomaly else go e_anomaly (n - 1)
  in
  go (m +. (e *. sin m)) 20

let heliocentric (p : planet option) (jd : float) : float * float * float =
  let t = Celestial.centuries jd in
  let el0, rate = elements p in
  let a = el0.a +. (rate.a *. t) and e = el0.e +. (rate.e *. t) in
  let i = rad (el0.i +. (rate.i *. t)) in
  let l = rad (el0.l +. (rate.l *. t)) in
  let varpi = rad (el0.varpi +. (rate.varpi *. t)) in
  let omega = rad (el0.omega +. (rate.omega *. t)) in
  (* the argument of the perihelion, from the node; and the mean
   * anomaly, from the perihelion *)
  let w = varpi -. omega in
  let m = Float.rem (l -. varpi) (2. *. Float.pi) in
  let ea = kepler ~e m in
  (* on the ellipse, the Sun at the origin, x towards the perihelion *)
  let x' = a *. (cos ea -. e) and y' = a *. sqrt (1. -. (e *. e)) *. sin ea in
  (* rotated by w about the orbit's pole, tilted by i about the node
   * line, turned by omega about the ecliptic's pole *)
  let cw = cos w and sw = sin w and co = cos omega and so = sin omega and ci = cos i and si = sin i in
  ( (((cw *. co) -. (sw *. so *. ci)) *. x') +. ((-.(sw *. co) -. (cw *. so *. ci)) *. y'),
    (((cw *. so) +. (sw *. co *. ci)) *. x') +. ((-.(sw *. so) +. (cw *. co *. ci)) *. y'),
    (sw *. si *. x') +. (cw *. si *. y') )

(* J2000's ecliptic to J2000's equator: a tilt by the obliquity of 2000
 * about the x axis, the equinox *)
let equatorial_of_ecliptic ((x, y, z) : float * float * float) : Celestial.equatorial =
  let eps = Celestial.obliquity Celestial.j2000 in
  Celestial.of_vector (x, (cos eps *. y) -. (sin eps *. z), (sin eps *. y) +. (cos eps *. z))

let planet (p : planet) (jd : float) : Celestial.equatorial =
  let x, y, z = heliocentric (Some p) jd and ex, ey, ez = heliocentric None jd in
  equatorial_of_ecliptic (x -. ex, y -. ey, z -. ez)

let sun (jd : float) : Celestial.equatorial =
  let ex, ey, ez = heliocentric None jd in
  equatorial_of_ecliptic (-.ex, -.ey, -.ez)

(*****************************************************************************)
(* The Moon: a mean motion and its disturbances *)
(*****************************************************************************)

let moon_ecliptic (jd : float) : float * float =
  let t = Celestial.centuries jd in
  let s a b = sin (rad (a +. (b *. t))) in
  let lon =
    218.32 +. (481267.881 *. t)
    +. (6.29 *. s 134.9 477198.85) (* the equation of the centre *)
    -. (1.27 *. s 259.2 (-413335.38)) (* the evection *)
    +. (0.66 *. s 235.7 890534.23) (* the variation *)
    +. (0.21 *. s 269.9 954397.70)
    -. (0.19 *. s 357.5 35999.05) (* the annual equation *)
    -. (0.11 *. s 186.6 966404.05)
  in
  let lat =
    (5.13 *. s 93.3 483202.03) +. (0.28 *. s 228.2 960400.87) -. (0.28 *. s 318.3 6003.18) -. (0.17 *. s 217.6 (-407332.20))
  in
  (Celestial.normalize (rad lon), rad lat)

let moon (jd : float) : Celestial.equatorial =
  let lon, lat = moon_ecliptic jd in
  Celestial.of_ecliptic ~obliquity:(Celestial.obliquity jd) lon lat

(* the phase angle (Sun - Moon - Earth) is nearly 180 deg less the
 * elongation (Sun - Earth - Moon), the Sun being 400 times further *)
let illuminated ~(sun : Celestial.equatorial) (moon : Celestial.equatorial) : float =
  let elongation = Celestial.separation sun moon in
  (1. -. cos elongation) /. 2.
