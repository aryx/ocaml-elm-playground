(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Celestial.mli *)

type equatorial = { ra : float; dec : float }
type horizontal = { az : float; alt : float }

let pi = Float.pi
let degrees (d : float) : float = d *. pi /. 180.
let hours (h : float) (m : float) (s : float) : float = degrees (15. *. (h +. (m /. 60.) +. (s /. 3600.)))
let dms (sign : int) (d : float) (m : float) (s : float) : float = float_of_int sign *. degrees (d +. (m /. 60.) +. (s /. 3600.))
let to_degrees (a : float) : float = a *. 180. /. pi
let to_hours (a : float) : float = to_degrees a /. 15.
let arcseconds (s : float) : float = degrees (s /. 3600.)

let normalize (a : float) : float =
  let a = Float.rem a (2. *. pi) in
  if a < 0. then a +. (2. *. pi) else a

(*****************************************************************************)
(* Time *)
(*****************************************************************************)

(* the Unix epoch, 1970-01-01 at 0h, was JD 2440587.5 (a Julian day
 * starts at noon) *)
let julian_date (t : float) : float = (t /. 86400.) +. 2440587.5
let j2000 = 2451545.0
let centuries (jd : float) : float = (jd -. j2000) /. 36525.

(* Meeus 12.4: the Earth's rotation angle, 360.98564736629 deg a day;
 * the two small terms are the precession's share *)
let gmst (jd : float) : float =
  let t = centuries jd in
  normalize
    (degrees
       (280.46061837 +. (360.98564736629 *. (jd -. j2000)) +. (0.000387933 *. t *. t) -. (t *. t *. t /. 38710000.)))

let lst (jd : float) ~(lon : float) : float = normalize (gmst jd +. lon)

(*****************************************************************************)
(* Frames *)
(*****************************************************************************)

(* Meeus 22.2, its first two terms: 23 deg 26' 21.448 arcsec, less 47 arcsec a
 * century *)
let obliquity (jd : float) : float =
  let t = centuries jd in
  degrees (23. +. (26. /. 60.) +. (21.448 /. 3600.)) -. arcseconds (46.8150 *. t)

let of_vector ((x, y, z) : float * float * float) : equatorial =
  let r = sqrt ((x *. x) +. (y *. y) +. (z *. z)) in
  { ra = normalize (atan2 y x); dec = asin (z /. r) }

let to_vector (p : equatorial) : float * float * float =
  (cos p.dec *. cos p.ra, cos p.dec *. sin p.ra, sin p.dec)

(* the ecliptic is the equator tilted by the obliquity about the
 * equinox's direction, the x axis both share *)
let of_ecliptic ~(obliquity : float) (lon : float) (lat : float) : equatorial =
  let x = cos lat *. cos lon and y = cos lat *. sin lon and z = sin lat in
  of_vector (x, (cos obliquity *. y) -. (sin obliquity *. z), (sin obliquity *. y) +. (cos obliquity *. z))

(* Meeus 21.2 and 21.3: three rotations, by zeta about the pole, theta
 * about the new x axis, z about the new pole *)
let precess (jd : float) (p : equatorial) : equatorial =
  let t = centuries jd in
  let zeta = arcseconds ((2306.2181 *. t) +. (0.30188 *. t *. t) +. (0.017998 *. t *. t *. t)) in
  let z = arcseconds ((2306.2181 *. t) +. (1.09468 *. t *. t) +. (0.018203 *. t *. t *. t)) in
  let theta = arcseconds ((2004.3109 *. t) -. (0.42665 *. t *. t) -. (0.041833 *. t *. t *. t)) in
  let a = cos p.dec *. sin (p.ra +. zeta) in
  let b = (cos theta *. cos p.dec *. cos (p.ra +. zeta)) -. (sin theta *. sin p.dec) in
  let c = (sin theta *. cos p.dec *. cos (p.ra +. zeta)) +. (cos theta *. sin p.dec) in
  { ra = normalize (atan2 a b +. z); dec = asin c }

(* Meeus 13.5 and 13.6, with the azimuth from north rather than from
 * south: the hour angle H is how far the star has gone west of the
 * meridian *)
let to_horizontal ~(lat : float) ~(lst : float) (p : equatorial) : horizontal =
  let h = lst -. p.ra in
  let alt = asin ((sin lat *. sin p.dec) +. (cos lat *. cos p.dec *. cos h)) in
  let az = atan2 (-.(cos p.dec *. sin h)) ((sin p.dec *. cos lat) -. (cos p.dec *. cos h *. sin lat)) in
  { az = normalize az; alt }

let separation (p : equatorial) (q : equatorial) : float =
  let x1, y1, z1 = to_vector p and x2, y2, z2 = to_vector q in
  (* the cross product's length with the dot product: exact for small
   * angles too, unlike acos *)
  let cx = (y1 *. z2) -. (z1 *. y2) and cy = (z1 *. x2) -. (x1 *. z2) and cz = (x1 *. y2) -. (y1 *. x2) in
  atan2 (sqrt ((cx *. cx) +. (cy *. cy) +. (cz *. cz))) ((x1 *. x2) +. (y1 *. y2) +. (z1 *. z2))
