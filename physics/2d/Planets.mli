(* The Solar System's numbers: each planet's orbital elements at J2000
 * (2000-01-01, 12:00), for Kepler.position, and its size; the Moon's
 * orbit and size; the Sun's. Numbers only (the examples choose the
 * colors), shared by examples/SolarSystem.ml and
 * examples3d/SolarSystem3d.ml.
 *
 * The scale of it, the reason every picture of the Solar System lies
 * about it: the Earth is 12,756 km across, 149.6 million km from the
 * Sun (1 AU), which is 1.39 million km across. Drawn with the Earth's
 * orbit 100 pixels wide, the Earth is 0.004 pixels, the Sun 0.5, and
 * Neptune's orbit 3,000 pixels.
 *
 * Sources:
 * - the elements: E. M. Standish, J. G. Williams, "Approximate
 *   Positions of the Planets", JPL, table 1 (1800-2050), at J2000:
 *   https://ssd.jpl.nasa.gov/planets/approx_pos.html (the Earth's are
 *   the Earth-Moon barycenter's);
 * - the diameters, the Moon's distance and month: NASA's Planetary
 *   Fact Sheet, https://nssdc.gsfc.nasa.gov/planetary/factsheet/;
 * - the AU: IAU 2012, 149,597,870.7 km exactly; the Sun's radius:
 *   IAU 2015's nominal 695,700 km. *)

type planet = {
  name : string;
  orbit : Kepler.elements;
  (* its radius, km (half the fact sheet's diameter) *)
  radius : float;
}

(* Mercury to Neptune *)
val planets : planet list

(* km *)
val au : float
val sun_radius : float

(* the Moon: its distance from the Earth (km), its period around it
 * (the sidereal month, days), its radius (km). A circle here: its
 * orbit's e = 0.055, and its 5 degree tilt, left out *)
val moon_distance : float
val moon_period : float
val moon_radius : float
