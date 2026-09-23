(* Orbits by Kepler's laws: where a planet is at any time, in closed
 * form (see notes_2d_physics.md section 6, and Integrate.mli for the
 * other way, step by step).
 *
 * Kepler's three laws (Astronomia Nova, 1609; Harmonices Mundi, 1619),
 * which Newton's gravitation later explained:
 *
 *   1. a planet goes around the Sun on an ellipse, the Sun at a focus;
 *   2. the line from the Sun to it sweeps equal areas in equal times
 *      (faster near the Sun);
 *   3. its period squared is its distance (the semi-major axis a)
 *      cubed: T = a^1.5, in years and astronomical units (AU, the
 *      Earth's distance, 149.6 million km). Jupiter, 5.2 AU away: 11.9
 *      years.
 *
 *                 .-----.
 *             .'    b    '.            a: semi-major axis
 *            /   (Sun)*  P \           e: eccentricity, how flat
 *            \             /           P: perihelion, the closest,
 *             '.         .'               a (1 - e) from the Sun
 *                '-----'
 *
 * Where on the ellipse at time t? The mean anomaly M, the angle a
 * planet would have turned on a circle at constant speed, grows
 * steadily: M = 360 t / T. The eccentric anomaly E, which gives the
 * position, satisfies Kepler's equation
 *
 *     M = E - e sin E
 *
 * which has no closed-form solution for E: solved here by Newton's
 * method, E <- E - (E - e sin E - M) / (1 - e cos E), a few iterations
 * from E = M (Meeus). Example: e = 0.5, M = 90 degrees: E = 2.0210
 * radians (115.8 degrees). Then, in the orbit's plane, the Sun at the
 * origin and the perihelion along x:
 *
 *     x = a (cos E - e)       y = a sqrt (1 - e^2) sin E
 *
 * and three rotations (the argument of perihelion, the inclination,
 * the ascending node) put the orbit in the ecliptic, the plane of the
 * Earth's orbit.
 *
 * Why not integrate, like examples/PhysicsOrbit.ml? At an animation's time
 * warp (a year a second, a century a minute), a step of 1/60 s is 6
 * days of orbit: Mercury, 88 days around, would need far smaller steps,
 * and every integrator drifts over thousands of orbits. The closed form
 * is exact at any time, however far: the right tool when nothing but
 * the Sun pulls. (The planets do pull on each other a little: real
 * ephemerides add those perturbations; the elements' slow drift, per
 * century, in JPL's table below, is their average effect, left out
 * here.)
 *
 * References:
 * - Kepler, Astronomia Nova, 1609 (laws 1 and 2); Harmonices Mundi,
 *   1619 (law 3);
 * - Jean Meeus, Astronomical Algorithms, 1991, chapter 30 ("Equation of
 *   Kepler": Newton's method, and why it converges for e < 1);
 * - E. M. Standish, J. G. Williams, "Approximate Positions of the
 *   Planets", JPL, the elements at J2000 and the rotations to the
 *   ecliptic: https://ssd.jpl.nasa.gov/planets/approx_pos.html *)

(* the orbital elements at an epoch (J2000: 2000-01-01, 12:00), as in
 * JPL's table; the angles in degrees *)
type elements = {
  a : float; (* semi-major axis, AU *)
  e : float; (* eccentricity, 0 a circle *)
  inclination : float; (* the orbit's tilt to the ecliptic *)
  mean_longitude : float; (* where it is, at the epoch *)
  perihelion : float; (* the longitude of its perihelion *)
  node : float; (* the longitude of its ascending node *)
}

(* [period a]: the orbital period in years of a planet at [a] AU from
 * the Sun, a^1.5 (Kepler's third law) *)
val period : float -> float

(* [eccentric_anomaly ~e m]: E from Kepler's equation, M = E - e sin E,
 * by Newton's method (radians) *)
val eccentric_anomaly : e:float -> float -> float

(* [in_plane el ~days]: where the planet is, [days] after the epoch, in
 * its orbit's plane: the Sun at (0, 0), the perihelion along x; AU *)
val in_plane : elements -> days:float -> Vec2.t

(* [position el ~days]: the same, in the ecliptic's coordinates (x
 * towards the vernal equinox, z towards the ecliptic's north); AU *)
val position : elements -> days:float -> float * float * float
