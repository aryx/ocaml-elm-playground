(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * (LGPL) as published by the Free Software Foundation; either version
 * 2 of the License, or (at your option) any later version.
 *)

(* See Planets.mli *)

type planet = { name : string; orbit : Kepler.elements; radius : float }

(* JPL's table 1 at J2000: a, e, I, L, long. peri., long. node *)
let planet name a e inclination mean_longitude perihelion node diameter =
  { name; orbit = { Kepler.a; e; inclination; mean_longitude; perihelion; node }; radius = diameter /. 2. }

let planets =
  [ planet "Mercury" 0.38709927 0.20563593 7.00497902 252.25032350 77.45779628 48.33076593 4879.;
    planet "Venus" 0.72333566 0.00677672 3.39467605 181.97909950 131.60246718 76.67984255 12104.;
    planet "Earth" 1.00000261 0.01671123 (-0.00001531) 100.46457166 102.93768193 0.0 12756.;
    planet "Mars" 1.52371034 0.09339410 1.84969142 (-4.55343205) (-23.94362959) 49.55953891 6792.;
    planet "Jupiter" 5.20288700 0.04838624 1.30439695 34.39644051 14.72847983 100.47390909 142984.;
    planet "Saturn" 9.53667594 0.05386179 2.48599187 49.95424423 92.59887831 113.66242448 120536.;
    planet "Uranus" 19.18916464 0.04725744 0.77263783 313.23810451 170.95427630 74.01692503 51118.;
    planet "Neptune" 30.06992276 0.00859048 1.77004347 (-55.12002969) 44.96476227 131.78422574 49528. ]

let au = 149597870.7
let sun_radius = 695700.
let moon_distance = 384400.
let moon_period = 27.3217
let moon_radius = 3475. /. 2.
