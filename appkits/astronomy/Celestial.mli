(* Celestial: where a star is, in the sky of a place at an instant.

   A star's catalogue gives two angles on the celestial sphere, fixed
   (almost) for centuries: its right ascension (RA, the sky's longitude,
   counted eastward from the spring equinox, in hours: 24h = 360 deg) and
   its declination (Dec, the sky's latitude, from the celestial equator).
   What an observer sees is two other angles: the altitude above the
   horizon and the azimuth (from north, through east). Between the two,
   the Earth turns, and the question is how far it has turned:

         catalogue (RA, Dec, J2000)
            | precess                   the Earth's axis wobbles, 26000 yr
            v
         RA, Dec of date
            | hour angle H = LST - RA   the Earth's rotation, a sidereal day
            v
         (H, Dec)
            | rotate by the latitude    where you stand on it
            v
         (azimuth, altitude)

   The sidereal time is the Earth's rotation measured against the stars
   rather than the Sun: 360.9856 deg a (solar) day, a day of 23h56m4s,
   because while the Earth turns once it also moves 1/365th of its orbit
   and must turn a little more to face the Sun again. That extra degree
   a day is why the winter sky is not the summer sky. The local sidereal
   time (LST) is the RA crossing your meridian right now: at LST 5h30m,
   Orion's belt is due south.

   Precession: the Earth's axis turns like a top's, once in 25800
   years, dragging the equator and the equinox, so the RA and Dec of
   every star slowly change -- today's pole star, Polaris, was 12 deg
   from the pole when the pyramids were built, and Thuban (alpha
   Draconis) was the pole star then.

   Angles are radians throughout; [hours], [degrees] and [dms] build
   them from the astronomers' units. Times are Julian dates (JD): days
   since noon at Greenwich on -4712-01-01 (Julian.mli's day numbers,
   with a fraction), the astronomers' single count of days, J2000.0
   being JD 2451545.0 (2000-01-01 at noon). Universal time throughout;
   the difference with the dynamical time of ephemerides (69 s today,
   but hours in antiquity) is ignored, as are nutation (up to 17 arcsec),
   aberration (20 arcsec) and refraction (half a degree at the horizon).

   Worked examples (Meeus, chapters 12 and 21, the tests): on 1987-04-10
   at 0h UT (JD 2446895.5) the sidereal time at Greenwich was
   13h10m46.3668s; theta Persei, at RA 2h44m11.986s Dec +49 13 42.48 in
   J2000, is at 2h46m10.34s +49 20 57.1 on 2028-11-13.19 (proper motion
   aside).

   References: Jean Meeus, "Astronomical Algorithms" (2nd ed. 1998);
   Jay Lieske et al., "Expressions for the precession quantities based
   upon the IAU (1976) system of astronomical constants", Astronomy and
   Astrophysics 58 (1977). *)

(* both in radians *)
type equatorial = { ra : float; dec : float }

(* radians; the azimuth from north, through east: 0 N, pi/2 E, pi S *)
type horizontal = { az : float; alt : float }

(* [hours h m s], [degrees d] and [dms sign d m s] (sign 1 or -1, since
 * -0 degrees 17' would lose its sign), as radians *)
val hours : float -> float -> float -> float
val degrees : float -> float
val dms : int -> float -> float -> float -> float

(* and back: radians to degrees, and to hours *)
val to_degrees : float -> float
val to_hours : float -> float

(* an angle in [0, 2 pi) *)
val normalize : float -> float

(* the Julian date of Unix seconds (UTC): 0. is JD 2440587.5 *)
val julian_date : float -> float

(* J2000.0, JD 2451545.0 *)
val j2000 : float

(* Julian centuries since J2000.0, the T of every polynomial here *)
val centuries : float -> float

(* the Greenwich mean sidereal time at a Julian date (UT), radians *)
val gmst : float -> float

(* [lst jd ~lon]: the local sidereal time, [lon] radians east *)
val lst : float -> lon:float -> float

(* the obliquity of the ecliptic at a Julian date: the angle between the
 * equator and the Earth's orbit, 23 deg 26' in 2000, slowly shrinking *)
val obliquity : float -> float

(* [of_ecliptic ~obliquity lon lat]: RA and Dec of a point given in
 * ecliptic longitude and latitude, the planets' coordinates *)
val of_ecliptic : obliquity:float -> float -> float -> equatorial

(* [of_vector (x, y, z)] and [to_vector]: equatorial coordinates as a
 * direction, x towards the equinox, z towards the north pole *)
val of_vector : float * float * float -> equatorial
val to_vector : equatorial -> float * float * float

(* [precess jd p]: a J2000 position moved to the equator and equinox of
 * the date [jd] (Lieske's angles zeta, z, theta) *)
val precess : float -> equatorial -> equatorial

(* [to_horizontal ~lat ~lst p]: the altitude and azimuth of [p] for an
 * observer at latitude [lat] when the local sidereal time is [lst] *)
val to_horizontal : lat:float -> lst:float -> equatorial -> horizontal

(* the angle between two directions on the sphere *)
val separation : equatorial -> equatorial -> float
