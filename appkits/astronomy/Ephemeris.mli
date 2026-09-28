(* Ephemeris: where the Sun, the Moon and the planets are.

   The stars stay put on the celestial sphere; the "wanderers" (planetes
   in Greek) move along the ecliptic, the Sun's yearly path, because we
   see them from a planet that orbits too. Kepler (1609) gave each orbit
   as an ellipse with the Sun at a focus, described by six numbers, its
   elements: its size a (in astronomical units), its eccentricity e, its
   inclination I on the ecliptic, where it crosses the ecliptic going
   north (the node, Omega), where it comes nearest the Sun (the
   perihelion, varpi) and where the planet is on it (the mean longitude
   L). Where the planet is at a date is then Kepler's equation,

       M = E - e sin E      (M, the mean anomaly: L - varpi, growing
                             evenly with time; E, the eccentric anomaly,
                             where on the ellipse; solved by Newton)

   and a rotation of the ellipse into place. The elements drift slowly
   (the planets pull on each other), so each comes with a rate:

         elements at J2000 + rates * T  ->  M  -> E  -> (x, y) on the ellipse
         -> rotate by varpi, I, Omega  ->  heliocentric (x, y, z)
         minus the Earth's own         ->  geocentric
         tilt by the obliquity         ->  RA, Dec (J2000)

   and the Sun is where the Earth is not: its geocentric position is
   minus the Earth's heliocentric one. The elements and rates are E.
   Myles Standish's (JPL), good to a minute of arc or so between 1800
   and 2050, and drifting outside those years (a degree for Mars two
   thousand years away). Uranus and Neptune, invisible to the naked
   eye, are left out.

   The Moon is too near and too disturbed by the Sun for an ellipse:
   its longitude is a mean motion plus sine terms, each a disturbance
   with a history (the equation of the centre, 6.29 deg, Kepler's
   ellipse; the evection, 1.27 deg, Ptolemy's; the variation, 0.66 deg,
   Tycho's; the annual equation, 0.19 deg). These are the Astronomical
   Almanac's low precision formulas: 0.3 deg at most, about half the
   Moon's width.

   Worked examples (Meeus, chapters 25, 33 and 47; the tests): on
   1992-10-13 at 0h the Sun is at RA 13h13m31.4s, Dec -7 47 06; on
   1992-12-20 at 0h Venus is at RA 21h04m41.5s, Dec -18 53 17; on
   1992-04-12 at 0h the Moon is at ecliptic longitude 133.16 deg,
   latitude -3.23 deg.

   References: E. Myles Standish, "Keplerian Elements for Approximate
   Positions of the Major Planets" (JPL, table 1); The Astronomical
   Almanac, section D ("low precision formulas for the Moon"); Jean
   Meeus, "Astronomical Algorithms" (2nd ed. 1998). *)

type planet = Mercury | Venus | Mars | Jupiter | Saturn

val planets : planet list
val planet_name : planet -> string

(* [kepler ~e m]: the eccentric anomaly E of the mean anomaly [m]
 * (radians), E - e sin E = m *)
val kepler : e:float -> float -> float

(* the heliocentric position of the planet, or of the Earth ([None]),
 * at a Julian date, in AU, on J2000's ecliptic *)
val heliocentric : planet option -> float -> float * float * float

(* RA and Dec, J2000, at a Julian date, seen from the Earth's centre;
 * [Celestial.precess] gives the date's *)
val planet : planet -> float -> Celestial.equatorial
val sun : float -> Celestial.equatorial

(* the Moon's ecliptic longitude and latitude, of the date *)
val moon_ecliptic : float -> float * float

(* the Moon's RA and Dec, of the date (already precessed) *)
val moon : float -> Celestial.equatorial

(* the fraction of the Moon's disk lit, 0. new, 1. full, from its
 * angle from the Sun (both of the date) *)
val illuminated : sun:Celestial.equatorial -> Celestial.equatorial -> float
